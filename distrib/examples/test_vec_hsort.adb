------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

--  pragma Ada_2020;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Generic_Hyper_Sorting;
with Gen_Qsort;
with Local_Vectors;
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with Ada.Calendar;
with Ada.Unchecked_Conversion;
with System.Parallelism;

procedure Test_Vec_HSort is

   package Flt_Vectors is new Local_Vectors (Float);
   subtype Flt_Vec_Type is Flt_Vectors.Vector;
   use Flt_Vectors;

   function Length (A : Flt_Vec_Type) return Elem_Index is (Num_Elements (A));

   function First (A : Flt_Vec_Type) return Elem_Index is
      pragma Unreferenced (A);
   begin
      return 1;
   end First;

   function Image (Val : Float) return String is (Val'Image);

   function "/" (Left : Float; Right : Integer) return Float is
     (Left / Float (Right));

   function Element
     (A : Flt_Vec_Type; Index : Elem_Index;
      Part : Natural := 0)
     return Float is
      pragma Unreferenced (Part);
   begin
      return A (Index);
   end Element;

   function Copy (A : in out Flt_Vec_Type) return Flt_Vec_Type is (A);

   procedure Assign_Slice
     (From : Flt_Vec_Type;
      From_Part : Positive;
      From_First : Elem_Index;
      To : in out Flt_Vec_Type;
      To_Part : Positive;
      To_First : Elem_Index;
      Count : Elem_Index'Base) is
      pragma Unreferenced (From_Part, To_Part);
   begin
      Flt_Vectors.Assign_Slice (To, To_First,
                    From, From_First, Count);
   end Assign_Slice;

   procedure Sort_One_Part
     (A : in out Flt_Vec_Type;
      First : Elem_Index; Last : Elem_Index'Base;
      Part : Natural := 0) is
      pragma Unreferenced (Part);

      function Before (Left, Right : Elem_Index) return Boolean is
        (A (Left) < A (Right));

      procedure Swap (Left, Right : Elem_Index) is
         Tmp : constant Float := A (Left);
      begin
         Set_Nth_Element (A, Left, A (Right));
         Set_Nth_Element (A, Right, Tmp);
      end Swap;

      procedure Quicksort is new Gen_Qsort (Elem_Index, Before, Swap);
   begin
      Quicksort (First, Last);
   end Sort_One_Part;

   procedure For_Each_Part
     (Num_Parts : Positive;
      Loop_Body : access procedure (Part_Index : Positive)) is

      use System.Parallelism;

      procedure Iter_Loop_Body
        (Low, High : Longest_Integer; Local_Part_Index : Positive) is
         --  NOTE: When doing things in parallel,
         --        Low = High = Local_Part_Index.
         --        When doing things sequentially,
         --        Low = Local_Part_Index = 1, High = Num_Parts.
         pragma Assert (Positive (Low) = Local_Part_Index);
      begin
         for Part_Index in Integer (Low) .. Integer (High) loop
            Loop_Body (Part_Index);
         end loop;
      end Iter_Loop_Body;
   begin
      Par_Range_Loop (1, Longest_Integer (Num_Parts), Num_Parts,
        Loop_Body => Iter_Loop_Body'Access);
   end For_Each_Part;

   package My_Sorter is new
      Generic_Hyper_Sorting
        (Elem_Type => Float,
         Image => Image,
         Elem_First => Float'First,
         Elem_Last => Float'Last,
         Elem_Zero => 0.0,
         Index_Type => Elem_Index,
         Part_Index_Type => Positive,
         Indexable_Type => Flt_Vec_Type,
         Length => Length,
         First => First,
         Element => Element,
         Move => Copy,
         Copy_Slice => Assign_Slice,
         Sort_One_Part => Sort_One_Part,
         For_Each_Part => For_Each_Part);

   Len : constant Elem_Index :=
     (if Argument_Count > 0 then Elem_Index'Value (Argument (1)) else 100);

   Max_Plus_One : constant Float := Float (Len * 2);

   Num_Threads_To_Use : constant Natural :=
     (if Argument_Count > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

   --  For Random
   Gen : Generator;

   type Float_Int is mod 2 ** Float'Size;
   function To_Int is new Ada.Unchecked_Conversion (Float, Float_Int);

   procedure Print_Vec (Vec : Flt_Vec_Type) is
   begin
      for I in 1 .. Num_Elements (Vec) loop
         if I not in 51 .. Num_Elements (Vec) - 50 then
            Put (" " & Vec (I)'Image);
            if I < Num_Elements (Vec) then
               Put (",");
               if I mod 5 = 0 then
                  New_Line;
               end if;
               if I = 50 and then Num_Elements (Vec) > 100 then
                  Put_Line (" ...");
               end if;
            end if;
         end if;
      end loop;

      New_Line;
   end Print_Vec;

   Vec : Flt_Vec_Type := Create (Len, 0.0);
   Vec2 : Flt_Vec_Type;

   Time_Start : array (1 .. 2) of Ada.Calendar.Time;
   Time_End : array (1 .. 2) of Ada.Calendar.Time;

   use Ada.Calendar;

begin

   Reset (Gen, Initiator => Integer (Len));
   Put_Line ("Seed = " & Len'Image);

   for I in 1 .. Len loop
      Set_Nth_Element (Vec, I, Random (Gen) * Max_Plus_One);
   end loop;

   Vec2 := Vec;

   New_Line;
   Put_Line ("Before sort, Vec = ");
   Print_Vec (Vec);

   Time_Start (1) := Ada.Calendar.Clock;
   My_Sorter.Hyper_Qsort (Vec, Num_Parts => Num_Threads_To_Use);
   Time_End (1) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After HQ sort, Vec = ");
   Print_Vec (Vec);

   Time_Start (2) := Ada.Calendar.Clock;
   My_Sorter.Hyper_Qsort (Vec2, Num_Parts => Num_Threads_To_Use * 2);
   Time_End (2) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After 2nd HQ sort, Vec2 = ");
   Print_Vec (Vec2);

   New_Line;
   for I in 1 .. 2 loop
      Put_Line ("Sort" & I'Image & " Time:" &
        Duration'Image (Time_End (I) - Time_Start (I)));
   end loop;

   New_Line;
   Put_Line ("Checking if sorts worked:");

   Put_Line ("Comparing Vec and Vec2 => " &
     Boolean'Image
       (for all I in 1 .. Num_Elements (Vec) => Vec (I) = Vec2 (I)));

   New_Line;
   Put_Line ("Vec is sorted?");
   for I in 1 .. Num_Elements (Vec) - 1 loop
      declare
         Left : constant Float := Vec (I);
         Right : constant Float := Vec (I + 1);
      begin
         if Left > Right then
            Put_Line ("Vec (" & I'Image & " ) > Vec (" &
              Elem_Index'Image (I + 1) & " ):" &
              Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
         end if;
      end;
   end loop;

   New_Line;
   Put_Line ("Vec2 is sorted?");
   for I in 1 .. Num_Elements (Vec2) - 1 loop
      declare
         Left : constant Float := Vec2 (I);
         Right : constant Float := Vec2 (I + 1);
      begin
         if Left > Right then
            Put_Line ("Vec2 (" & I'Image & " ) > Vec2 (" &
              Elem_Index'Image (I + 1) & " ):" &
              Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
            Put_Line (" aka:" & To_Int (Left)'Image & " >" &
              To_Int (Right)'Image);
         end if;
      end;
   end loop;

end Test_Vec_HSort;
