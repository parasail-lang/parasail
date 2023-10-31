------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
--                                                                          --
--                     Copyright (C) 2012-2023, AdaCore                     --
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
--  Provide Iterate for Hashed_Maps that returns a parallel iterator.

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package body LWT.Hashed_Map_Par_Iterators is
   use Maps;
   use Map_Par_Iterator_Interfaces;
   use Ada.Containers;  --  for Count_Type

   type Chunk_Info is record
      First_Cursor : Cursor;
      --  First cursor of each chunk
      --  Also represents cursor for where to stop for prior chunk.
   end record;

   type Chunk_Info_Array is array (Positive range <>) of Chunk_Info;

   type Chunk_Info_Record (Num_Chunks : Positive) is record
      Data : Chunk_Info_Array (1 .. Num_Chunks);
   end record;

   type Chunk_Info_Record_Ptr is access Chunk_Info_Record;

   type Seq_Iter_Ptr is
     access Maps.Map_Iterator_Interfaces.Forward_Iterator'Class;

   type Map_Par_Iterator 
     (Container : not null access constant Maps.Map;
      Seq_Iter : not null Seq_Iter_Ptr) is
        new Ada.Finalization.Limited_Controlled
          and Map_Par_Iterator_Interfaces.Parallel_Iterator
     with record
      Chunks : Chunk_Info_Record_Ptr := null;
   end record;

   procedure Finalize (Iterator : in out Map_Par_Iterator);

   function First (Object : Map_Par_Iterator) return Cursor;
   function Next (Object : Map_Par_Iterator; Position : Cursor)
     return Cursor;

   function Is_Split (Object : Map_Par_Iterator)
     return Boolean;

   procedure Split_Into_Chunks (Object     : in out Map_Par_Iterator;
                                Max_Chunks : Chunk_Index);

   function Chunk_Count (Object : Map_Par_Iterator)
      return Chunk_Index;

   function First (Object : Map_Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor;

   function Next (Object   : Map_Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor;

   -----

   procedure Finalize (Iterator : in out Map_Par_Iterator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Maps.Map_Iterator_Interfaces.Forward_Iterator'Class, Seq_Iter_Ptr);
      procedure Free is new Ada.Unchecked_Deallocation
        (Chunk_Info_Record, Chunk_Info_Record_Ptr);

      Iter : Seq_Iter_Ptr := Iterator.Seq_Iter;  -- Make copy so can null out.
   begin
      --  Free chunks and seq iterator, if any
      Free (Iter);
      Free (Iterator.Chunks);
   end Finalize;

   function First (Object : Map_Par_Iterator) return Cursor is
   begin
      return Object.Seq_Iter.First;
   end First;

   function Next (Object : Map_Par_Iterator; Position : Cursor)
     return Cursor is
   begin
      return Object.Seq_Iter.Next(Position);
   end Next;

   function Is_Split (Object : Map_Par_Iterator)
     return Boolean is
   begin
      return Object.Chunks /= null;
   end Is_Split;

   procedure Split_Into_Chunks (Object     : in out Map_Par_Iterator;
                                Max_Chunks : Chunk_Index) is
      --  Use user-requested chunk count unless is > Map length
      --  TBD: Might actually limit chunk count to Map_Len/2, say.
      Map_Len : constant Count_Type := Object.Container.Length;
      Num_Chunks : constant Chunk_Index :=
        Chunk_Index'Max (1,
          Chunk_Index'Min (Max_Chunks, Chunk_Index'Base (Map_Len)));
      Items_Per_Small_Chunk : constant Count_Type :=
        Map_Len / Count_Type (Num_Chunks);
      Num_Extra : constant Count_Type :=
        Map_Len - Items_Per_Small_Chunk * Count_Type (Num_Chunks);
        
      --  Compute how many chunks should be of size Items_Per_Small_Chunk
      --  (the others will have one additional item).
      Num_Small_Chunks : constant Chunk_Index'Base :=
        Num_Chunks - Chunk_Index'Base (Num_Extra);

      Next_Cursor : Cursor := Object.Seq_Iter.First;

   begin  --  Split_Into_Chunks

      if Object.Chunks /= null then
         --  Should not have already split the iterator.
         raise Program_Error;
      end if;

      --  Initialize chunk info records with first cursor for each chunk.
      Object.Chunks := new Chunk_Info_Record (Num_Chunks);
      for I in 1 .. Num_Chunks loop
         --  Initialize I'th chunk info with chunk's first cursor position.
         Object.Chunks.Data (I).First_Cursor := Next_Cursor;

         for J in 1 .. (if I <= Num_Small_Chunks
                        then Items_Per_Small_Chunk
                        else Items_Per_Small_Chunk + 1)
         loop
            if not Maps.Has_Element (Next_Cursor) then
               --  Map must have been tampered with
               raise Program_Error;
            end if;
            --  Get next cursor position
            Next_Cursor := Object.Seq_Iter.Next (Next_Cursor);
         end loop;
      end loop;
      if Maps.Has_Element (Next_Cursor) then
         --  Map must have been tampered with
         raise Program_Error;
      end if;
   end Split_Into_Chunks;

   function Chunk_Count (Object : Map_Par_Iterator)
      return Chunk_Index is
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      return Object.Chunks.Num_Chunks;
   end Chunk_Count;

   function First (Object : Map_Par_Iterator;
                   Chunk  : Chunk_Index) return Cursor is
   --  Return cursor to first element of chunk
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      return Object.Chunks.Data (Chunk).First_Cursor;
   end First;

   function Next (Object   : Map_Par_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor is
      Result : Cursor;
   begin
      if Object.Chunks = null then
         raise Program_Error;
      end if;
      --  Get next cursor using sequential iterator
      Result := Object.Seq_Iter.Next (Position);
      if Chunk < Object.Chunks.Num_Chunks
        and then Result = Object.Chunks.Data (Chunk + 1).First_Cursor
      then
         --  Reached end of chunk
         return Maps.No_Element;
      else
         return Result;
      end if;
   end Next;

   function Par_Iterate (Container : Maps.Map)
     return Map_Par_Iterator_Interfaces.Parallel_Iterator'Class is
   --  Return iterator over Container that can be split into chunks
   begin
      return Map_Par_Iterator'(Ada.Finalization.Limited_Controlled with
                               Container => Container'Unchecked_Access,
                               Seq_Iter => new Maps.Map_Iterator_Interfaces.
                                 Forward_Iterator'Class'(Container.Iterate),
                               others => <>);
   end Par_Iterate;

end LWT.Hashed_Map_Par_Iterators;
