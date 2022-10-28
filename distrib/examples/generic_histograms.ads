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

generic
   type Value_Type is digits <>;
package Generic_Histograms is

   subtype Count_Type is Natural;

   subtype Index_Type is Positive;

   type Histogram (Size : Index_Type) is tagged private
     with Constant_Indexing => Nth_Bucket;

   procedure Combine (Left, Right : in out Histogram);

   procedure Add_Value (Accum : in out Histogram; Value : Value_Type);

   function Min_Value (Histo : Histogram) return Value_Type;

   function Max_Value (Histo : Histogram) return Value_Type;

   function Average_Value (Histo : Histogram) return Value_Type;

   function Standard_Deviation (Histo : Histogram) return Value_Type;

   function Num_Values (Histo : Histogram) return Count_Type;

   type Bucket_Type is record
      Low_Bound : Value_Type;
      Count : Count_Type;
   end record;

   function First_Low_Bound (Histo : Histogram) return Value_Type;
   --  The low bound of the first non-zero bucket.

   function Last_High_Bound (Histo : Histogram) return Value_Type;
   --  The high bound of the last non-zero bucket

   function Num_Buckets_In_Use (Histo : Histogram) return Index_Type;
   --  Number of buckets, counting only buckets from leftmost to rightmost
   --  with a non-zero count.  Initially one.

   function Nth_Bucket (Histo : Histogram; Index : Index_Type)
     return Bucket_Type
     with Pre => Index <= Histo.Size;
   --  Return Nth bucket, starting from the first non-zero bucket.

   function Bucket_Width (Histo : Histogram) return Value_Type;
   --  This is zero when there are zero or one
   --  distinct values in the histogram.

   function Count_Max (Histo : Histogram) return Count_Type;
   --  Maximum count in any bucket

   procedure Normalize (Histo : in out Histogram);
      --  Shift Counts array so first-non-zero-bucket = 1.

   procedure Dump_Histo
     (Histo : Histogram;
      Max_Stars : Positive := 40;
      Num_Digits : Positive := Value_Type'Digits);
   --  Dump the histogram using '*' to indicate number of values
   --  in a given bucket, limited to a maximum of "Max_Stars" for
   --  a given bucket.
   --  Display the floating point values with Num_Digits.
   --  Will use scientific notation if values are such that Num_Digits
   --  if the largest value not in 0.1 .. 10 ** (Num_Digits-1).

   procedure Dump_Diffs (Left, Right : Histogram);
   --  Show how two histograms differ --  TBD: this is for debugging

private
   type Count_Array is array (Index_Type range <>) of Count_Type;

   function Predef_Eq (Left, Right : Histogram) return Boolean
     renames "=";
      --  Squirrel away predefined equality for use in defining
      --  overriding of "="

   function "=" (Left, Right : Histogram) return Boolean;
   --  Override "=" to ignore First_Non_Zero_Bucket

   type Histogram (Size : Index_Type) is tagged record
      Num_Values : Count_Type := 0;
      Count_Max : Count_Type := 0;  --  Max value in Counts array

      Min_Value : Value_Type := Value_Type'Last;
      Max_Value : Value_Type := Value_Type'First;
      Sum : Value_Type := 0.0;
      Sum_Of_Squares : Value_Type := 0.0;

      First_Low_Bound : Value_Type := 0.0;
         --  This value is irrelevant when Num_Values = 0
      Bucket_Width : Value_Type := 0.0;
         --  This is zero when number of distinct values is <= 1

      First_Non_Zero_Bucket : Index_Type := Size / 2 + 1;
         --  Index of leftmost non-zero bucket; lower-numbered buckets
         --  are used for wrap-around.
         --  We arbitrarily start in the middle.
      Num_Buckets_In_Use : Index_Type'Base := 0;
         --  Counting only buckets from leftmost bucket
         --  with a non-zero count to rightmost with non-zero count

      Counts : Count_Array (1 .. Size) := (others => 0);
      --  Counts is actually a circular buffer, going First_Non_Zero .. Size,
      --  wrapping around to use 1 .. First_Non_Zero - 1.
   end record;

   function Num_Values (Histo : Histogram) return Count_Type
     is (Histo.Num_Values);

   function Min_Value (Histo : Histogram) return Value_Type
     is (Histo.Min_Value);

   function Max_Value (Histo : Histogram) return Value_Type
     is (Histo.Max_Value);

   function First_Low_Bound (Histo : Histogram) return Value_Type
     is (Histo.First_Low_Bound);

   function Num_Buckets_In_Use (Histo : Histogram) return Index_Type
     is (Histo.Num_Buckets_In_Use);

   function Bucket_Width (Histo : Histogram) return Value_Type
     is (Histo.Bucket_Width);

   function Count_Max (Histo : Histogram) return Count_Type
     is (Histo.Count_Max);

end Generic_Histograms;
