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
with LWT.Hashed_Map_Par_Iterators;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with LWT.OpenMP; use LWT.OpenMP;

procedure Map_Par_Loop is

   Control : OMP_Parallel (Num_Threads => 6);  -- use 6 threads
   pragma Unreferenced (Control);

   Num_Chunks : constant := 30;
   Num_Elems : constant := 1_000_000;

   package String_Int_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type => Unbounded_String,
        Element_Type => Integer,
        Hash => Hash,
        Equivalent_Keys => "=");

   package Map_Par_Iterators is
     new LWT.Hashed_Map_Par_Iterators (String_Int_Maps);

   use String_Int_Maps, Map_Par_Iterators;

   Map : String_Int_Maps.Map;

   Partial_Sums : array (1 .. Num_Chunks) of Longest_Integer := (others => 0);

   Total : Longest_Integer := 0;

   procedure Loop_Body
     (Iterator : Map_Par_Iterator_Interfaces.Parallel_Iterator'Class;
      Chunk_Index : Positive;
      PID : Par_Loop_Id);
   --  Given the following Ada 202X syntax:
   --
   --  parallel (Chunk_Index in 1 .. Num_Chunks)
   --                      --  or "pragma Par_Loop (Num_Chunks);"
   --  for E of Map loop
   --     Partial_Sums (Chunk_Index) := Partial_Sums (Chunk_Index) + E;
   --  end loop;
   --
   --  ** This loop-body procedure should be created automatically **

   procedure Loop_Body
     (Iterator : Map_Par_Iterator_Interfaces.Parallel_Iterator'Class;
      Chunk_Index : Positive;
      PID : Par_Loop_Id) is
      pragma Unreferenced (PID);
      Position : Cursor := Iterator.First (Chunk_Index);
   begin
      while Has_Element (Position) loop
         declare
            E : Integer renames Map (Position);
         begin
            Partial_Sums (Chunk_Index) :=
              Partial_Sums (Chunk_Index) + Longest_Integer (E);
         end;

         Position := Iterator.Next (Position, Chunk_Index);
      end loop;
   end Loop_Body;

begin

   --  Initialize map to simple mapping from Image => Value
   Put_Line ("Initializing map");
   for I in 1 .. Num_Elems loop
--       if I mod 1000 = 1 then
--          Put_Line ("Initializing Map (" & I'Image & ")");
--       end if;
      Map.Insert (To_Unbounded_String (I'Image), I);
   end loop;

   Put_Line ("Computing partial sums of map in parallel");
   --  Now sum the map elements in parallel
   --  ** Here is where the user's parallel for loop would have appeared **
   declare
      procedure Par_Iterator_Loop is
        new Generic_Par_Iterator_Loop
          (Map_Par_Iterator_Interfaces);

      Iterator : Map_Par_Iterator_Interfaces.Parallel_Iterator'Class :=
        Par_Iterate (Map);
   begin
      Par_Iterator_Loop (Iterator => Iterator,
                         Num_Chunks => Partial_Sums'Last,
                         Loop_Body => Loop_Body'Access);
   end;

   Put_Line ("Final summation of map");
   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Total := Total + Partial_Sums (I);
   end loop;

   Ada.Text_IO.Put_Line ("Final total = " & Total'Image);

end Map_Par_Loop;
