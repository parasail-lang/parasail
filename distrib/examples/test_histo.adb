------------------------------------------------------------------------------
--                              test_histo.adb                              --
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
with Ada.Exceptions;
with Generic_Histograms;

procedure Test_Histo is

   Num_Args : constant Natural := Argument_Count;

   Len : constant Natural :=
     (if Num_Args > 0 then Integer'Value (Argument (1)) else 100);

   Default_Num_Rand : constant := 6;

   Num_Rand : constant Natural :=
     (if Num_Args < 2 then Default_Num_Rand
      else Natural'Value (Argument (2)));

   --  For Random
   Gen : Generator;

   package Flt_Histograms is new Generic_Histograms
     (Value_Type => Float);
      --  Flt_Histograms.Histogram has one discriminant, Size.
      --  The low and high bounds, and the width of each bucket will
      --  automatically shift to accommodate values as they are added.
      --  The bounds will be a multiple of the bucket width.
      --  The widths of the buckets will double as needed to fit all
      --  of the values, so one outlier could cause the buckets to
      --  become very wide.  One could imagine eventually recognizing
      --  outliers somehow, and e.g., limiting bounds to 2 or 3 std. devs.
      --  from the mean.

   use Flt_Histograms;
   Histo : Histogram (Size => 20);
   Histo2 : Histogram (Size => 20);
   Histo3 : Histogram (Size => 20);
   Histo_Identity : constant Histogram := Histo;

begin
   Reset (Gen, Initiator => Len * Num_Rand);

   Put_Line ("Seed =" &
     Integer'Image (Len * Num_Rand));

   for I in 1 .. Len loop
      declare
         Val : Float := 0.0;
      begin
         for I in 1 .. Num_Rand loop
            Val := Val + Random (Gen);
         end loop;
         Histo.Add_Value (Val * 10.0);
         Histo2.Add_Value (Val * 10.0);

         if I /= Len
           and then (I mod (Len / 5) = 0 or else I <= 4)
         then
            Dump_Histo (Histo);

            --  Combine histograms
            Histo3.Combine (Histo2);

            --  Reset histogram (should not be necessary?)
            Histo2 := Histo_Identity;
         end if;
      end;
   end loop;

   --  Do final combination
   Histo3.Combine (Histo2);

   Dump_Histo (Histo);

   if Histo3 /= Histo then
      Put_Line
        ("Histogram created by Combine not same as one using Add_Value:");
      Put_Line ("Differences: ");
      Dump_Diffs (Histo, Histo3);
   else
      Put_Line
        ("Histogram created by Combine matches one using Add_Value.");
   end if;

exception
   when E : others =>
      Put_Line ("Exception " & Ada.Exceptions.Exception_Name (E) &
        "raised: " & Ada.Exceptions.Exception_Information (E));
end Test_Histo;
