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

with LWT.Parallelism; use LWT.Parallelism;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with LWT.Vector_Par_Iterators;
with LWT.OpenMP; use LWT.OpenMP;

procedure Vector_Par_Loop is
   Control : OMP_Parallel;
   pragma Unreferenced (Control);

   Num_Chunks : constant := 30;

   package Vec_Ints is
     new Ada.Containers.Vectors
       (Index_Type => Positive, Element_Type => Integer);

   --  Instantiate Ada 202X extensions of Vectors with parallel iterator.
   --  Defines "Par_Iterate" on a vector.
   package Par_Iterators is
     new LWT.Vector_Par_Iterators (Vec_Ints);

   use Vec_Ints, Par_Iterators;

   Vec : Vec_Ints.Vector := To_Vector (1_000_000);

   Partial_Sums : array (1 .. Num_Chunks) of Longest_Integer := (others => 0);

   Total : Longest_Integer := 0;

   --  Given the following Ada 202X syntax:
   --
   --  parallel (Chunk_Index in 1 .. Num_Chunks)
   --                      --  or "pragma Par_Loop (Num_Chunks);"
   --  for E of Vec loop
   --     Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) + E;
   --  end loop;
   --
   --  ** This loop-body procedure should be created automatically **
   procedure Loop_Body
     (Iterator : Vector_Par_Iterator_Interfaces.Parallel_Iterator'Class;
      Chunk_Index : Positive;
      PID : Par_Loop_Id);
   procedure Loop_Body
     (Iterator : Vector_Par_Iterator_Interfaces.Parallel_Iterator'Class;
      Chunk_Index : Positive;
      PID : Par_Loop_Id) is
      pragma Unreferenced (PID);
      Position : Cursor := Iterator.First (Chunk_Index);
   begin
      while Has_Element (Position) loop
         declare
            E : Integer renames Vec (Position);
         begin
            Partial_Sums (Chunk_Index) :=
              Partial_Sums (Chunk_Index) + Longest_Integer (E);
         end;

         Position := Iterator.Next (Position, Chunk_Index);
      end loop;
   end Loop_Body;

begin

   --  Initialize vector to simple sequence
   for I in 1 .. Integer (Vec.Length) loop
      Vec (I) := I;
   end loop;

   --  Now sum the vector in parallel
   --  ** Here is where the user's parallel for loop would have appeared **
   declare
      Iterator : Vector_Par_Iterator_Interfaces.Parallel_Iterator'Class :=
        Par_Iterate (Vec);
   begin
      Iterator.Par_Iterator_Loop
        (Num_Chunks => Partial_Sums'Last,
         Loop_Body => Loop_Body'Access);
   end;

   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Total := Total + Partial_Sums (I);
   end loop;

   Ada.Text_IO.Put_Line ("Final total = " & Total'Image);

end Vector_Par_Loop;
