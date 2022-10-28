------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
------------------------------------------------------------------------------

--  Prototype implementation of System.Parallelism package for Ada 202X

with System.LWT; use System.LWT;
with System.LWT.Storage;

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body System.Parallelism is

   Debug_Par : constant Boolean := False;

   procedure Par_Range_Loop
     (Low, High : Longest_Integer;
      Num_Chunks : Natural := 0;    --  0 means no chunk specification present
      Aspects : access Ada.Aspects.Root_Aspect'Class := null;
                                    --  null means no aspects specified
      Loop_Body : access procedure
        (Low, High : Longest_Integer; Chunk_Index : Positive))
   is
      Count : constant Longest_Integer := High - Low + 1;
      Num_Chunks_Longest : constant Longest_Integer :=
        Longest_Integer'Min (Count, Longest_Integer (Num_Chunks));

      type LWT_Data_Extension is new Range_Loop.Range_Loop_Data
        with null record;  --  No more data, but need to override LWT_Body

      Canceling_Exception : Ada.Exceptions.Exception_Occurrence;
      --  If group is canceled due to propagation of an exception
      --  from Loop_Body, this will be initialized to the exception
      --  to be re-raised after waiting for all LWTs to complete.

      overriding
      procedure LWT_Body (Extended_Data : LWT_Data_Extension);

      overriding
      procedure Reclaim_Storage (Extended_Data : access LWT_Data_Extension;
                                 Server_Index : LWT_Server_Index);

      type LWT_Data_Ptr is access all LWT_Data_Extension
        with Storage_Pool => System.LWT.Storage.LWT_Storage_Pool_Obj;
         --  Local access type needed because extension is local.
         --  Use global storage pool designed for re-using LWT data.

      procedure Free is new Ada.Unchecked_Deallocation
        (LWT_Data_Extension, LWT_Data_Ptr);
         --  This will call the deallocate routine for LWT_Storage_Pool

      overriding
      procedure Reclaim_Storage
        (Extended_Data : access LWT_Data_Extension;
         Server_Index : LWT_Server_Index) is
      --  Reclaim storage at end of LWT's life
         Ptr : LWT_Data_Ptr := LWT_Data_Ptr (Extended_Data);
      begin
         Free (Ptr);
      end Reclaim_Storage;

      overriding
      procedure LWT_Body (Extended_Data : LWT_Data_Extension) is
         --  Execute body of loop
      begin
         Loop_Body (Low => Longest_Integer (Extended_Data.First),
                    High => Longest_Integer (Extended_Data.Last),
                    Chunk_Index => Extended_Data.Chunk_Index);
      exception
         when E : others =>
            --  An exception was raised.  Save it if it is the first
            --  canceling event in the group.
            --  In any case, do not propagate it.
            declare
               Success : Boolean := False;
            begin
               if Debug_Par then
                  Put_Line (" About to cancel group due to exception " &
                    Ada.Exceptions.Exception_Name (E));
               end if;
               Extended_Data.Spawning_Group.Cancel_Group (Success);

               if Success then
                  Ada.Exceptions.Save_Occurrence (Canceling_Exception, E);
               end if;

               if Debug_Par then
                  Put_Line (" Tried to cancel group due to exception " &
                    Ada.Exceptions.Exception_Name (E) & ", Success = " &
                    Success'Image);
               end if;
            end;
      end LWT_Body;

   begin  --  Par_Range_Loop
      if Num_Chunks_Longest = 1 then
         --  Chunk count = 1, so don't spawn anything
         if Debug_Par then
            Put_Line ("Par_Range_Loop, Num_Chunks =" &
              Num_Chunks_Longest'Image);
         end if;

         Loop_Body (Low => Low, High => High, Chunk_Index => 1);
      else
         --  Spawn LW threads to execute loop, with chunking
         --  determined implicitly.
         if Debug_Par then
            Put_Line ("Par_Range_Loop, passing off to LWT_Range_Loop, Low =" &
              Low'Image & ", High =" & High'Image);
         end if;

         LWT_Range_Loop
           (LWT_Data_Extension'(Root_Data with
              Num_Chunks => Longest_Integer'Pos (Num_Chunks_Longest),
              First => Longest_Integer'Pos (Low),
              Last => Longest_Integer'Pos (High),
              Chunk_Index => 1),
            Aspects => Aspects);

         --  Propagate saved exception, if any
         Ada.Exceptions.Reraise_Occurrence (Canceling_Exception);
      end if;
   end Par_Range_Loop;

   function Chunk_Index return Positive is
   begin
      --  Should not be called; compiler should make substitution
      raise Program_Error;
      return 1;
   end Chunk_Index;

   procedure Generic_Par_Iterator_Loop
     (Iterator   : in out Inst.Parallel_Iterator'Class;
      Num_Chunks : Natural := 0;    --  0 means no chunk specification present
      Aspects    : access Ada.Aspects.Root_Aspect'Class := null;
                                    --  null means no aspects specified
      Loop_Body : access procedure
        (Iterator : Inst.Parallel_Iterator'Class;
         Chunk_Index : Positive))
   is
      pragma Unreferenced (Aspects);  --  ??? looks unexpected

   --  Parallel "for loop" over a container or generalized iterator
   --  ??? No need to pass in Num_Chunks or use "in out" mode
   --  if we call Split_Into_Chunks *before* calling this routine
   --  (i.e. Iterator.Is_Split would be true before the call).

      Group : aliased LWT_Group;

      Canceling_Exception : Ada.Exceptions.Exception_Occurrence;
      --  If group is canceled due to propagation of an exception
      --  from Loop_Body, this will be initialized to the exception
      --  to be re-raised after waiting for all LWTs to complete.

      Num_Splits : Positive;

      type LWT_Data_Extension is new Root_Data with record
         --  Extend the root data type with data needed by LWT_Body
         Chunk_Index : Positive;
      end record;

      overriding
      procedure LWT_Body (Extended_Data : LWT_Data_Extension);

      overriding
      procedure Reclaim_Storage (Extended_Data : access LWT_Data_Extension;
                                 Server_Index : LWT_Server_Index);

      type LWT_Data_Ptr is access all LWT_Data_Extension
        with Storage_Pool => System.LWT.Storage.LWT_Storage_Pool_Obj;
         --  Local access type needed because extension is local.
         --  Use global storage pool designed for re-using LWT data.

      procedure Free is new Ada.Unchecked_Deallocation
        (LWT_Data_Extension, LWT_Data_Ptr);
         --  This will call the deallocate routine for LWT_Storage_Pool

      overriding
      procedure Reclaim_Storage
        (Extended_Data : access LWT_Data_Extension;
         Server_Index : LWT_Server_Index) is
      --  Reclaim storage at end of LWT's life
         Ptr : LWT_Data_Ptr := LWT_Data_Ptr (Extended_Data);
      begin
         Free (Ptr);
      end Reclaim_Storage;

      overriding
      procedure LWT_Body (Extended_Data : LWT_Data_Extension) is
         --  Just call through to the Loop_Body, after dealing
         --  with cancellation before the call, or as a result
         --  of an exception.
         This_Chunk : constant Positive := Extended_Data.Chunk_Index;
      begin
         if Group.Cancellation_Point then
            return;
         end if;

         --  Now do this chunk
         Loop_Body (Iterator => Iterator,
                    Chunk_Index => This_Chunk);
      exception
         when E : others =>
            --  An exception was raised.  Save it if it is the first
            --  canceling event in the group.
            --  In any case, do not propagate it.
            declare
               Success : Boolean := False;
            begin
               Group.Cancel_Group (Success);
               if Success then
                  Ada.Exceptions.Save_Occurrence (Canceling_Exception, E);
               end if;
            end;
      end LWT_Body;

      Canceled : Boolean := False;

      --  If no chunk specification, then use 2 * Num servers available
      Num_Chunks_To_Use : constant Positive :=
        (if Num_Chunks = 0
         then 2 * Positive (System.LWT.Num_Servers_Available)
         else Num_Chunks);

   begin  --  Generic_Par_Iterator_Loop

      --  Split the iterator
      Iterator.Split_Into_Chunks (Num_Chunks_To_Use);

      --  Find out how many chunks the Iterator was actually split into.
      Num_Splits := Iterator.Chunk_Count;

      if Num_Splits = 1 then
         --  Iterator is not split at all; just do the one and only chunk
         Loop_Body (Iterator => Iterator, Chunk_Index => 1);
      else
         --  Spawn the chunks
         for I in 1 .. Num_Splits loop
            Spawn_LWT (Group,
               LWT_Data_Ptr'(new LWT_Data_Extension'
                    (Root_Data with
                       Chunk_Index => I)));
         end loop;

         --  Wait for all chunks to complete
         Group.Wait_For_Group (Canceled);

         if Canceled then
            --  Propagate saved exception, if any
            Ada.Exceptions.Reraise_Occurrence (Canceling_Exception);
         end if;
      end if;
   end Generic_Par_Iterator_Loop;

end System.Parallelism;
