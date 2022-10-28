------------------------------------------------------------------------------
--                              Generic_Histograms                          --
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

with Ada.Numerics.Generic_Elementary_Functions;
pragma Elaborate (Ada.Numerics.Generic_Elementary_Functions);
with Ada.Text_IO; use Ada.Text_IO;
pragma Elaborate (Ada.Text_IO);

with Ada.Strings.Fixed;

package body Generic_Histograms is

   Debug : constant Boolean := False;

   package Value_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Value_Type);

   package Value_IO is new Ada.Text_IO.Float_IO (Value_Type);

   function Abs_Next_Power_Of_Two (Diff : Value_Type) return Value_Type;
   --  Round abs (Diff) up to next power of 2

   function Abs_Next_Power_Of_Two (Diff : Value_Type) return Value_Type is
      pragma Assert (Value_Type'Machine_Radix = 2);
         --  TBD: For now presume radix = power of 2
   begin
      return 2.0 ** Value_Type'Exponent (Diff);
   end Abs_Next_Power_Of_Two;

   function Compute_Value_Index
     (Accum : Histogram; Value : Value_Type) return Index_Type;
      --  Compute index as a function of Value and fields of Accum
      --  First_Non_Zero_Bucket, First_Low_Bound, Bucket_Width, and Size.

   function Compute_Value_Index
     (Accum : Histogram; Value : Value_Type) return Index_Type is
   begin
      if Accum.Bucket_Width = 0.0 then
         return Accum.First_Non_Zero_Bucket;
      else
         return (Accum.First_Non_Zero_Bucket + Index_Type'Base
           (Value_Type'Floor ((Value - Accum.First_Low_Bound) /
              Accum.Bucket_Width)) - 1) mod Accum.Size + 1;
      end if;
   end Compute_Value_Index;

   procedure Adjust_For_New_Bounds
     (Accum : in out Histogram; New_Min, New_Max : Value_Type);
   --  Expand the bucket width and adjust other attributes of histogram
   --  as necessary to accommodate new Min/Max values.

   procedure Adjust_For_New_Bounds
     (Accum : in out Histogram; New_Min, New_Max : Value_Type) is
      pragma Assert (New_Min <= Accum.Min_Value);
      pragma Assert (New_Max >= Accum.Max_Value);

      New_Bucket_Width : Value_Type := Accum.Bucket_Width;

   begin  --  Adjust_For_New_Bounds

      if New_Min < Accum.Min_Value
        or else New_Max > Accum.Max_Value
      then
         if Debug then
            Put_Line ("Adjusting for new bounds, New_Min = " & New_Min'Image &
                      ", New_Max = " & New_Max'Image);
         end if;
         --  Need to compute a new bucket width
         if New_Max = New_Min then
            --  All values are the same
            Accum.Bucket_Width := 0.0;
            Accum.First_Low_Bound := New_Min;
            Accum.Num_Buckets_In_Use := 1;
         else
            --  Not all values are the same.
            --  Compute a new bucket width.
            New_Bucket_Width :=
              Abs_Next_Power_Of_Two
                ((New_Max - New_Min) / Value_Type (Accum.Size));

            --  Check if bucket width is big enough.
            if Value_Type'Floor (New_Max / New_Bucket_Width) -
              Value_Type'Floor (New_Min / New_Bucket_Width) >=
                Value_Type (Accum.Size)
            then
               --  Bucket width is too small
               New_Bucket_Width := New_Bucket_Width * 2.0;
            end if;

            --  Bucket width should not decrease
            pragma Assert (New_Bucket_Width >= Accum.Bucket_Width);

            declare
               --  Compute new low bound of first non-zero bucket
               New_First_Low_Bound : constant Value_Type :=
                 New_Bucket_Width * Value_Type'Floor
                                   (New_Min / New_Bucket_Width);

               New_Num_Buckets_In_Use : constant Index_Type := Index_Type
                 (Value_Type'Floor ((New_Max - New_First_Low_Bound) /
                   New_Bucket_Width)) + 1;
               pragma Assert (New_Num_Buckets_In_Use <= Accum.Size);
            begin
               if New_Bucket_Width > Accum.Bucket_Width then
                  --  Need to combine old buckets
                  declare
                     New_Counts : Count_Array (1 .. Accum.Size) :=
                       (others => 0);
                     New_First_Non_Zero_Bucket : constant Index_Type :=
                       (Accum.Size - New_Num_Buckets_In_Use) / 2 + 1;
                        --  Center the values in the Counts array
                  begin
                     Accum.Count_Max := 0;
                     --  Accumulate into new buckets
                     for I in 0 .. Accum.Num_Buckets_In_Use - 1 loop
                        declare
                           Old_Index : constant Index_Type :=
                             (Accum.First_Non_Zero_Bucket + I - 1) mod
                                Accum.Size + 1;
                           Old_Low_Bound : constant Value_Type :=
                             Accum.First_Low_Bound +
                               Value_Type (I) * Accum.Bucket_Width;
                           New_Offset : constant Index_Type'Base :=
                             Index_Type'Base (Value_Type'Floor
                                ((Old_Low_Bound - New_First_Low_Bound) /
                                    New_Bucket_Width));
                           New_Index : constant Index_Type :=
                             (New_First_Non_Zero_Bucket + New_Offset - 1) mod
                                Accum.Size + 1;
                        begin
                           New_Counts (New_Index) :=
                             New_Counts (New_Index) + Accum.Counts (Old_Index);
                           Accum.Count_Max := Count_Type'Max
                             (Accum.Count_Max, New_Counts (New_Index));
                        end;
                     end loop;
                     Accum.Counts := New_Counts;
                     Accum.Bucket_Width := New_Bucket_Width;
                     Accum.First_Non_Zero_Bucket := New_First_Non_Zero_Bucket;
                  end;
               elsif New_Min < Accum.Min_Value then
                  --  Need to compute new non-first-zero index
                  Accum.First_Non_Zero_Bucket :=
                    Compute_Value_Index (Accum, New_Min);
               end if;

               Accum.First_Low_Bound := New_First_Low_Bound;
               Accum.Num_Buckets_In_Use := New_Num_Buckets_In_Use;
            end;
         end if;
         Accum.Min_Value := New_Min;
         Accum.Max_Value := New_Max;
      end if;
   end Adjust_For_New_Bounds;

   procedure Add_Value (Accum : in out Histogram; Value : Value_Type) is
   begin
      if Value not in Accum.Min_Value .. Accum.Max_Value then
         --  Adjust histogram bounds to encompass new value
         Adjust_For_New_Bounds
           (Accum,
            New_Min => Value_Type'Min (Accum.Min_Value, Value),
            New_Max => Value_Type'Max (Accum.Max_Value, Value));
      end if;

      --  Now incorporate new value into histogram
      declare
         Value_Index : constant Index_Type :=
           Compute_Value_Index (Accum, Value);
      begin
         Accum.Counts (Value_Index) := Accum.Counts (Value_Index) + 1;
         Accum.Count_Max :=
           Count_Type'Max (Accum.Count_Max, Accum.Counts (Value_Index));
         Accum.Num_Values := Accum.Num_Values + 1;
         Accum.Sum := Accum.Sum + Value;
         Accum.Sum_Of_Squares := Accum.Sum_Of_Squares + Value * Value;
      end;
   end Add_Value;

   procedure Combine (Left, Right : in out Histogram) is
   begin
      if Right.Min_Value < Left.Min_Value
        or else Right.Max_Value > Left.Max_Value
      then
         --  Might have to widen the buckets
         Adjust_For_New_Bounds
           (Left,
            New_Min => Value_Type'Min (Left.Min_Value, Right.Min_Value),
            New_Max => Value_Type'Max (Left.Max_Value, Right.Max_Value));
      end if;

      --  Copy over the buckets from the Right accum into the Left accum
      for I in 0 .. Right.Num_Buckets_In_Use - 1 loop
         declare
            Right_Low_Bound : constant Value_Type :=
              Right.First_Low_Bound +
                Value_Type (I) * Right.Bucket_Width;
            Right_Index : constant Index_Type :=
              (Right.First_Non_Zero_Bucket + I - 1) mod
                 Right.Size + 1;
            Left_Index : constant Index_Type :=
              Compute_Value_Index (Left, Right_Low_Bound);
         begin
            Left.Counts (Left_Index) :=
              Left.Counts (Left_Index) + Right.Counts (Right_Index);
            Left.Count_Max :=
              Count_Type'Max (Left.Count_Max, Left.Counts (Left_Index));
         end;
      end loop;

      Left.Num_Values := Left.Num_Values + Right.Num_Values;
      Left.Sum := Left.Sum + Right.Sum;
      Left.Sum_Of_Squares := Left.Sum_Of_Squares + Right.Sum_Of_Squares;
   end Combine;

   function Average_Value (Histo : Histogram) return Value_Type is
   begin
      if Histo.Num_Values = 0 then
         return 0.0;
      else
         return Histo.Sum / Value_Type (Histo.Num_Values);
      end if;
   end Average_Value;

   function Standard_Deviation (Histo : Histogram) return Value_Type is
   begin
      if Histo.Num_Values = 0 then
         return 0.0;
      else
         return Value_Elementary_Functions.Sqrt
           (Value_Type (Histo.Num_Values) * Histo.Sum_Of_Squares -
              Histo.Sum * Histo.Sum) / Value_Type (Histo.Num_Values);
      end if;
   end Standard_Deviation;

   function Nth_Bucket (Histo : Histogram; Index : Index_Type)
     return Bucket_Type is
      Index_To_Use : constant Index_Type :=
        (Index + Histo.First_Non_Zero_Bucket - 2) mod Histo.Size + 1;
   begin
      return Bucket_Type'
        (Low_Bound =>
           Histo.First_Low_Bound + Value_Type (Index - 1) * Histo.Bucket_Width,
         Count => Histo.Counts (Index_To_Use));
   end Nth_Bucket;

   function Last_High_Bound (Histo : Histogram) return Value_Type is
   --  The high bound of the last non-zero bucket
   begin
      return Histo.First_Low_Bound +
        Value_Type (Histo.Num_Buckets_In_Use) * Histo.Bucket_Width;
   end Last_High_Bound;

   procedure Normalize (Histo : in out Histogram) is
      --  Shift Counts array so first-non-zero-bucket = 1.
   begin
      if Histo.First_Non_Zero_Bucket /= 1 then
         declare
            Old_Counts : constant Count_Array := Histo.Counts;
         begin
            for I in 1 .. Histo.Size loop
               Histo.Counts (I) := Old_Counts
                 ((Histo.First_Non_Zero_Bucket + I - 2) mod Histo.Size + 1);
            end loop;
            Histo.First_Non_Zero_Bucket := 1;
         end;
      end if;
   end Normalize;

   function "=" (Left, Right : Histogram) return Boolean is
   --  Return True if equal after normalizing
   begin
      if Left.First_Non_Zero_Bucket = Right.First_Non_Zero_Bucket then
         return Predef_Eq (Left, Right);
      else
         declare
            Norm_Left : Histogram := Left;
            Norm_Right : Histogram := Right;
         begin
            Normalize (Norm_Left);
            Normalize (Norm_Right);
            return Predef_Eq (Norm_Left, Norm_Right);
         end;
      end if;
   end "=";

   procedure Dump_Histo
     (Histo : Histogram;
      Max_Stars : Positive := 40;
      Num_Digits : Positive := Value_Type'Digits) is
   --  Dump the histogram using '*' to indicate number of values
   --  in a given bucket, limited to a maximum of "Max_Stars" for
   --  a given bucket.
   --  Display the floating point values with Num_Digits.
   --  Will use scientific notation if values are such that Num_Digits
   --  if the largest value not in 0.1 .. 10 ** (Num_Digits-1).

      Biggest : constant Value_Type :=
        Value_Type'Max (abs (Histo.Min_Value), abs (Histo.Max_Value));

      Use_Scientific_Notation : constant Boolean :=
        Biggest not in 0.1 .. 10.0 ** (Num_Digits - 1);

      Width : constant Positive :=
        (if Use_Scientific_Notation then Num_Digits + 8 else Num_Digits + 4);

      Aft : constant Natural :=
        (if Use_Scientific_Notation
         then Width - 6
         elsif Biggest < 1.0 then Num_Digits
         else Num_Digits - 1 - Natural
           (Value_Type'Floor
             (Value_Elementary_Functions.Log (Biggest, Base => 10.0))));

      subtype Padded_String is String (1 .. Width);

      use Ada.Strings.Fixed;  --  for "N * Character" operator

      function Padded_Image (Val : Value_Type) return Padded_String;
      --  Image of Val, expanded to Width, using scientific notation
      --  if Use_Scientific_Notation is True.

      function Padded_Image (Val : Value_Type) return Padded_String is
         use Value_IO;
      begin
         return Result : Padded_String do
            if Use_Scientific_Notation then
               Put (Result, Val, Aft => Aft);
            else
               Put (Result, Val, Aft => Aft, Exp => 0);
            end if;
         exception
            when others =>
               Result := (others => '*');
         end return;
      end Padded_Image;

   begin  --  Dump_Histo

      --  Display histogram
      Put_Line ("Histogram:");

      if Histo.Num_Values = 0 then
         Put_Line (" No values in histogram.");
         return;  --  Quit now
      end if;

      Put_Line (" Num values:" & Histo.Num_Values'Image &
        ", Min:" & Padded_Image (Histo.Min_Value) &
        ", Max:" & Padded_Image (Histo.Max_Value));

      Put_Line (" Average:" & Padded_Image (Histo.Average_Value) &
        ", Std. dev.:" & Padded_Image (Histo.Standard_Deviation));

      Put_Line (" Bucket width:" & Padded_Image (Histo.Bucket_Width) &
        ", Num buckets:" & Histo.Num_Buckets_In_Use'Image &
        ", Count max:" & Histo.Count_Max'Image);

      for Buck in 1 .. Histo.Num_Buckets_In_Use loop
         declare
            Bucket : Bucket_Type renames Histo (Buck);
            Star_Count : constant Natural :=
              (if Histo.Count_Max <= Max_Stars
               then Bucket.Count
               else Bucket.Count * Max_Stars / Histo.Count_Max);
         begin
            Put_Line ("  >=" & Padded_Image (Bucket.Low_Bound) &
              " " & Star_Count * '*');
         end;
      end loop;

   end Dump_Histo;

   procedure Dump_Diffs (Left, Right : Histogram) is
   --  Show how two histograms differ --  TBD: this is for debugging
   begin
      if Left.Num_Values /= Right.Num_Values then
         Put_Line ("  Num_Values: " & Left.Num_Values'Image &
           Right.Num_Values'Image);
      end if;
      if Left.Count_Max /= Right.Count_Max then
         Put_Line ("  Count_Max: " & Left.Count_Max'Image &
           Right.Count_Max'Image);
      end if;
      if Left.Min_Value /= Right.Min_Value then
         Put_Line ("  Min_Value: " & Left.Min_Value'Image &
           Right.Min_Value'Image);
      end if;
      if Left.Max_Value /= Right.Max_Value then
         Put_Line ("  Max_Value: " & Left.Max_Value'Image &
           Right.Max_Value'Image);
      end if;
      if Left.Sum /= Right.Sum then
         Put_Line ("  Sum: " & Left.Sum'Image &
           Right.Sum'Image);
      end if;
      if Left.Sum_Of_Squares /= Right.Sum_Of_Squares then
         Put_Line ("  Sum_Of_Squares: " & Left.Sum_Of_Squares'Image &
           Right.Sum_Of_Squares'Image);
      end if;
      if Left.First_Low_Bound /= Right.First_Low_Bound then
         Put_Line ("  First_Low_Bound: " & Left.First_Low_Bound'Image &
           Right.First_Low_Bound'Image);
      end if;
      if Left.Bucket_Width /= Right.Bucket_Width then
         Put_Line ("  Bucket_Width: " & Left.Bucket_Width'Image &
           Right.Bucket_Width'Image);
      end if;
      if Left.First_Non_Zero_Bucket /= Right.First_Non_Zero_Bucket then
         Put_Line ("  First_Non_Zero_Bucket: " &
           Left.First_Non_Zero_Bucket'Image &
           Right.First_Non_Zero_Bucket'Image);
      end if;
      if Left.Num_Buckets_In_Use /= Right.Num_Buckets_In_Use then
         Put_Line ("  Num_Buckets_In_Use: " & Left.Num_Buckets_In_Use'Image &
           Right.Num_Buckets_In_Use'Image);
      end if;
      if Left.Counts /= Right.Counts then
         declare
            Norm_Left : Histogram := Left;
            Norm_Right : Histogram := Right;
         begin
            Normalize (Norm_Left);
            Normalize (Norm_Right);
            if Norm_Left.Counts /= Norm_Right.Counts then
               Put_Line ("  Counts: ");
               for I in Norm_Left.Counts'Range loop
                  Put_Line (Norm_Left.Counts (I)'Image &
                    Norm_Right.Counts (I)'Image);
               end loop;
            end if;
         end;
      end if;
   end Dump_Diffs;
end Generic_Histograms;
