------------------------------------------------------------------------------
--  derived from ACATS CXAI017.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--  OBJECTIVE:
--      Check that an implementation supports the functionality defined
--      in procedures Ada.Containers.Generic_Constrained_Array_Sort,
--      Ada.Containers.Generic_Array_Sort and Ada.Containers.Generic_Sort.
--
--  TEST DESCRIPTION:
--      This test verifies that an implementation supports the subprograms
--      contained in packages Ada.Containers.Generic_Array_Sort,
--      Ada.Containers.Generic_Array_Sort and Ada.Containers.Generic_Sort.
--      Each of the subprograms is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
--  CHANGE HISTORY:
--      30 Oct 13   JAC     Initial pre-release version.
--      28 Mar 14   RLB     Created ACATS 4.0 version, renamed test.
--!
--  with Ada.Containers.Generic_Constrained_Array_Sort;
with Par_Constrained_Bitonic_Sort;
--  with Ada.Containers.Generic_Array_Sort;
--  with Ada.Containers.Generic_Sort;
--  with Ada.Containers.Vectors;
--  with Report;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Calendar; use Ada.Calendar;
with LWT.OpenMP; use LWT.OpenMP;

procedure CXAI017_Bitonic is
   pragma Warnings (Off);

   Control : OMP_Parallel (Num_Threads => 9);

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   Num_Tests : constant := 10;
   Num_Random : constant := 20_000_000;  --  TBD: 50_000_000;

   subtype Constrained_Array_Bounds_Type is Natural range 1 .. Num_Tests;

   subtype Con_Ran_Bounds_Type is Positive range 1 .. Num_Random;

   type My_Constrained_Array_Type is array (Constrained_Array_Bounds_Type) of
     My_Float;

   type My_Con_Ran_Array_Type is array (Con_Ran_Bounds_Type) of Float;

   type My_Array_Type is array (Natural range <>) of My_Float;

--    package My_Vectors is new Ada.Containers.Vectors
--      (Index_Type   => Natural,
--       Element_Type => My_Float); -- Default =

   procedure My_Constrained_Array_Sort is new
     Par_Constrained_Bitonic_Sort
     (Index_Type   => Constrained_Array_Bounds_Type,
      Element_Type => My_Float,
      Array_Type   => My_Constrained_Array_Type,
      "<"          => ">");
   --  Sort in reverse order to check is using what specified not simply <

   procedure My_Con_Ran_Array_Sort is new
     Par_Constrained_Bitonic_Sort
     (Index_Type   => Con_Ran_Bounds_Type,
      Element_Type => Float,
      Array_Type   => My_Con_Ran_Array_Type);

   Gen : Generator;

--    procedure My_Array_Sort_1 is new Ada.Containers.Generic_Array_Sort
--      (Index_Type   => Natural,
--       Element_Type => My_Float,
--       Array_Type   => My_Array_Type,
--       "<"          => ">");
   --  Sort in reverse order to check is using what specified not simply <

   --  No fractional parts so that can compare values for equality
   Sorted_Value_Constrained_Array   : constant My_Constrained_Array_Type :=
     (90.0, 89.0, 78.0, 67.0, 56.0, 45.0, 34.0, 23.0, 12.0, 1.0);
   Sorted_Value_Array               : constant My_Array_Type :=
     My_Array_Type (Sorted_Value_Constrained_Array);
   Unsorted_Value_Constrained_Array : constant My_Constrained_Array_Type :=
     (12.0, 90.0, 78.0, 45.0, 56.0, 34.0, 67.0, 89.0, 23.0, 1.0);
     --  (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 90.0, 1.0);
   Unsorted_Value_Array             : constant My_Array_Type :=
     My_Array_Type (Unsorted_Value_Constrained_Array);

   Unsorted_Ran_Con_Array : constant access My_Con_Ran_Array_Type :=
     new My_Con_Ran_Array_Type;
      --  Put on heap to avoid stack overflow

   To_Be_Sorted_Ran_Con_Array : constant access My_Con_Ran_Array_Type :=
     new My_Con_Ran_Array_Type;
      --  We actually do the sorting in this array

--    Sorted_Vector   : My_Vectors.Vector; -- Will initialise later
--    Unsorted_Vector : My_Vectors.Vector; -- Will initialise later

   --  Initialisation determines the length
   Value_Constrained_Array : My_Constrained_Array_Type :=
                               Unsorted_Value_Constrained_Array;
   Value_Array             : My_Array_Type             :=
                               Unsorted_Value_Array;

--    Value_Vector            : My_Vectors.Vector;

   function My_Array_Before (Left, Right : Natural) return Boolean;
   function My_Array_Before (Left, Right : Natural) return Boolean is
   begin

      return Value_Array (Left) > Value_Array (Right);

   end My_Array_Before;

--    function My_Vector_Before (Left, Right : Natural) return Boolean is
--    begin
--
--       return Value_Vector.Element (Index => Left) >
--              Value_Vector.Element (Index => Right);
--
--    end My_Vector_Before;
--
   procedure My_Array_Swap (Left, Right : Natural);
   procedure My_Array_Swap (Left, Right : Natural) is

      Temp : My_Float;

   begin

      Temp                := Value_Array (Left);
      Value_Array (Left)  := Value_Array (Right);
      Value_Array (Right) := Temp;

   end My_Array_Swap;

--    procedure My_Vector_Swap (Left, Right : Natural) is
--    begin
--
--       Value_Vector.Swap
--         (I => Left,
--          J => Right);
--
--    end My_Vector_Swap;
--
--    procedure My_Array_Sort_2 is new Ada.Containers.Generic_Sort
--      (Index_Type   => Natural,
--       Before       => My_Array_Before,
--       Swap         => My_Array_Swap);
--
--    procedure My_Vector_Sort is new Ada.Containers.Generic_Sort
--      (Index_Type   => Natural,
--       Before       => My_Vector_Before,
--       Swap         => My_Vector_Swap);
--
--    use type My_Vectors.Vector;
--
   Start, Now : Ada.Calendar.Time;
   Diff, Total_Diff : Duration;
begin
   Put_Line
     ("CXAI017_Bitonic: " &
      "Check that an implementation supports a parallel version of " &
      "package Ada.Containers.Generic_Constrained_Array_Sort");

   --  Initialise vectors

   for I in Con_Ran_Bounds_Type loop
      Unsorted_Ran_Con_Array (I) := Random (Gen) * 100.0;
   end loop;

   My_Constrained_Array_Sort (Container => Value_Constrained_Array);

   if Value_Constrained_Array /= Sorted_Value_Constrained_Array then

      Put_Line ("**Generic_Constrained_Array_Sort failed to sort");

   end if;

   Total_Diff := 0.0;

   for I in 1 .. 1 loop  --  TBD: 4 loop
      --  Copy to the To_Be_Sorted array
      To_Be_Sorted_Ran_Con_Array.all := Unsorted_Ran_Con_Array.all;

      Start := Ada.Calendar.Clock;
      --  And sort it
      My_Con_Ran_Array_Sort (Container => To_Be_Sorted_Ran_Con_Array.all);
      Now := Ada.Calendar.Clock;
      Diff := Now - Start;
      Put_Line (I'Image & ":" & Diff'Image & " seconds");
      Total_Diff := Total_Diff + Diff;
   end loop;

   for I in To_Be_Sorted_Ran_Con_Array'First ..
     To_Be_Sorted_Ran_Con_Array'Last - 1 loop
      if To_Be_Sorted_Ran_Con_Array (I) > To_Be_Sorted_Ran_Con_Array (I + 1)
      then
         Put_Line ("**Generic_Constrained_Array_Sort failed big sort at" &
           I'Image);

         for I in Unsorted_Ran_Con_Array'Range loop
            if I >= Unsorted_Ran_Con_Array'First + 20 then
               Put (" ...");
               exit;
            end if;
            Put (Unsorted_Ran_Con_Array (I)'Image);
         end loop;
         New_Line;

         for J in Integer'Max (To_Be_Sorted_Ran_Con_Array'First, I - 10) ..
           Integer'Min (To_Be_Sorted_Ran_Con_Array'Last, I + 10)
         loop
            Put (To_Be_Sorted_Ran_Con_Array (J)'Image &
                 (if J = I then "!!" else ""));
         end loop;
         New_Line;

         exit;
      end if;
   end loop;

--    My_Array_Sort_1 (Container => Value_Array);
--
--    if Value_Array /= Sorted_Value_Array then
--
--       Put_Line ("**Generic_Array_Sort failed to sort");
--
--    end if;
--
--
--    -- Re-fill array
--
--    Value_Array := Unsorted_Value_Array;
--
--    My_Array_Sort_2
--      (First => Value_Array'First,
--       Last  => Value_Array'Last);
--
--    if Value_Array /= Sorted_Value_Array then
--
--       Report.Failed ("Generic_Sort failed to sort array");
--
--    end if;
--
--
--    -- Fill vector
--
--    Value_Vector := Unsorted_Vector;
--
--    My_Vector_Sort
--      (First => Value_Vector.First_Index,
--       Last  => Value_Vector.Last_Index);
--
--    if Value_Vector /= Sorted_Vector then
--
--       Report.Failed ("Generic_Sort failed to sort vector");
--
--    end if;
--
--
--    Report.Result;
   Put_Line ("All done");
   Put_Line ("Total time:" & Total_Diff'Image & " seconds");

exception
   when E : others =>
      Put_Line (" exception raised");
   pragma Warnings (On);
end CXAI017_Bitonic;
