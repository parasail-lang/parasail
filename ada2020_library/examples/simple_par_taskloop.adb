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

--  Example using Prototype System.Parallelism package for Ada 202X

pragma Ada_2020;

with System.Parallelism; use System.Parallelism;
with Interfaces.OpenMP; use Interfaces.OpenMP;
with Ada.Text_IO;
with Generic_Atomic_Max_Min;

procedure Simple_Par_Taskloop is
   --  This tests the use of "taskloop" with no chunk specification
   --  so it will choose the number of chunks automatically.
   --  NOTE: We are still using the chunk index below, which would
   --        *not* normally be meaningful.  We end up keeping track
   --        of the maximum chunk index passed in, so we can do the
   --        final summation over the Partial_Sums.
   Max_Chunks : constant := 1000;

   Control : OMP_Parallel (Num_Threads => 6);  -- use 6 threads
   pragma Unreferenced (Control);

   type Int_Arr is array (Positive range <>) of Integer;
   type Int_Arr_Access is access Int_Arr;

   Arr : constant Int_Arr_Access := new Int_Arr (1 .. 1_000_000_000);

   Partial_Sums : array (1 .. Max_Chunks) of Longest_Integer := (others => 0);

   Total : Longest_Integer := 0;

   type Atomic_Base_Int is range Integer'First .. Integer'Last with Atomic;

   package Max_Min is new Generic_Atomic_Max_Min (Atomic_Base_Int);

   subtype Atomic_Int is Max_Min.Atomic_Type;

   Max_Chunk_Index : aliased Atomic_Int := 1;

   procedure Loop_Body
     (Low, High : Longest_Integer; Chunk_Index : Positive);
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
     (Low, High : Longest_Integer; Chunk_Index : Positive) is
   begin
      pragma Assert (Chunk_Index in Partial_Sums'Range);
      Max_Min.Update_Max (Max_Chunk_Index, Atomic_Int (Chunk_Index));
               --  Remember max value
      for I in Positive (Low) .. Positive (High) loop
         Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) +
           Integer'Pos (Arr (I));
      end loop;
   end Loop_Body;

begin

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

   --  Now sum the array in parallel
   --  ** Here is where the user's parallel for loop would have appeared **
   Par_Range_Loop (Low => Longest_Integer (Arr'First),
                   High => Longest_Integer (Arr'Last),
                   Num_Chunks => 0,  --  Testing taskloop default chunk-count
                   Loop_Body => Loop_Body'Access);

   --  Now do the final summation
   for I in 1 .. Integer (Max_Chunk_Index) loop
      Total := Total + Partial_Sums (I);
   end loop;

   Ada.Text_IO.Put_Line ("Final total = " & Total'Image);

end Simple_Par_Taskloop;
