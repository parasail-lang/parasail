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

--  Example using Prototype LWT.Parallelism package for Ada 202X
--  Testing exceptions and Task_Id

with LWT.Parallelism; use LWT.Parallelism;
with LWT.Scheduler;
with LWT.Work_Stealing; use LWT.Work_Stealing;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
procedure Excep_WS_Loop is
   pragma Warnings (Off);
   Num_Chunks : constant := 10;

   Control : WS_Parallel (Num_Servers => 6, Options => null);
                              --  use 6 server threads

   type Int_Arr is array (Positive range <>) of Integer;

   Arr : access Int_Arr := new Int_Arr (1 .. 100_000_000);

   Partial_Sums : array (1 .. Num_Chunks) of Longest_Integer := (others => 0);

   Task_Identities : array (1 .. Num_Chunks) of Task_Id;

   Early_Death : exception;

   Total : Longest_Integer := 0;

   --  Given the following Ada 202X syntax:
   --
   --  parallel (Chunk_Index in 1 .. Num_Chunks)
   --                      --  or "pragma Par_Loop (Num_Chunks);"
   --  for I in Arr'Range loop
   --     Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) + Arr (I);
   --  end loop;
   --
   --  ** This loop-body procedure should be created automatically **
   procedure Loop_Body
     (Low, High : Longest_Integer; Chunk_Index : Positive);
   procedure Loop_Body
     (Low, High : Longest_Integer; Chunk_Index : Positive) is
   begin
      Put_Line (" Loop_Body doing chunk" & Chunk_Index'Image);
      --  Record the task identity
      Task_Identities (Chunk_Index) := LWT.Scheduler.Ada_Task_Identity;

      if Chunk_Index = 2 or else Chunk_Index = Num_Chunks - 1 then
         --  Test raising an exception
         Put_Line (" About to raise Early_Death exception.");
         raise Early_Death with "Exception during parallel iteration";
         --  Put_Line (" [not really!]");
      end if;

      for I in Positive (Low) .. Positive (High) loop
         Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) +
           Integer'Pos (Arr (I));
      end loop;
   end Loop_Body;

   Task_Id_Mismatch : Boolean := False;

begin

   --  Initialize array to simple sequence.
   --  NOTE: We use a separate task to use a different processor's cache.
   declare
      task Init;
      task body Init is
      begin
         for I in Arr'Range loop
            Arr (I) := I;
         end loop;
      end Init;
   begin
      null;
   end;

   begin
      --  Now sum the array in parallel
      --  ** Here is where the user's parallel for loop would have appeared **
      Par_Range_Loop (Low => Longest_Integer (Arr'First),
                      High => Longest_Integer (Arr'Last),
                      Num_Chunks => Partial_Sums'Length,
                      Loop_Body => Loop_Body'Access);
   exception
      when E : others =>
         Put_Line ("Exception Information: " &
           Ada.Exceptions.Exception_Information (E));
   end;

   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Put_Line ("  Partial_Sums (" & I'Image & " ) =" &
        Partial_Sums (I)'Image);
      if Task_Identities (I) = Null_Task_Id then
         Put_Line (" Task_Identities (" & I'Image & " ) is Null_Task_Id");
      else
         Put_Line (" Task_Identities (" & I'Image & " ) is " &
           Image (Task_Identities (I)));
         for J in 1 .. I - 1 loop
            if Task_Identities (J) /= Null_Task_Id and then
               Task_Identities (I) /= Task_Identities (J)
            then
               Put_Line (" Task_Identities (" & I'Image &
                " ) /= Task_Identities (" & J'Image & " )");
               Task_Id_Mismatch := True;
            end if;
         end loop;
      end if;

      Total := Total + Partial_Sums (I);
   end loop;

   if not Task_Id_Mismatch then
      Put_Line (" Non-null Task-Ids all match");
   end if;

   Put_Line ("Final total = " & Total'Image);

   pragma Warnings (On);
end Excep_WS_Loop;
