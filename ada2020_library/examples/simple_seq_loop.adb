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

with System.Parallelism; use System.Parallelism;
with Ada.Text_IO;
procedure Simple_Seq_Loop is
   Num_Chunks : constant := 10;

   type Int_Arr is array (Positive range <>) of Integer;

   pragma Warnings (Off);
   Arr : constant access Int_Arr := new Int_Arr (1 .. 1_000_000_000);
   pragma Warnings (On);

   Partial_Sums : array (1 .. Num_Chunks) of Longest_Integer := (others => 0);

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
      for I in Positive (Low) .. Positive (High) loop
         Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) +
           Integer'Pos (Arr (I));
      end loop;
   end Loop_Body;

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

   --  Now sum the array in parallel
   --  ** Here is where the user's parallel for loop would have appeared **
   Par_Range_Loop (Low => Longest_Integer (Arr'First),
                   High => Longest_Integer (Arr'Last),
                   Num_Chunks => Partial_Sums'Last,
                   Loop_Body => Loop_Body'Access);

   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Ada.Text_IO.Put_Line ("  Partial_Sums (" & I'Image & " ) =" &
        Partial_Sums (I)'Image);
      Total := Total + Partial_Sums (I);
   end loop;

   Ada.Text_IO.Put_Line ("Final total = " & Total'Image);

end Simple_Seq_Loop;
