------------------------------------------------------------------------------
--                        L A R G E _ I N T E G E R S                       --
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
--                                                                          --
-- This package is based on the GENERIC LARGE INTEGER HANDLING PACKAGE      --
-- Created by Mats Weber on 24-MAY-1988                                     --
------------------------------------------------------------------------------

generic
   type Slice_Number      is range <>;  -- Type for the value of a slice
   type Arithmetic_Number is range <>;  -- Type for calculations.
      --  Its range must be at least 0 .. (SLICE_NUMBER'LAST + 1)**2 - 1
   type Slice_Index       is range <>;  -- Type for indexing slices
package PSC.Large_Integers is
----------------------

   type Large_Integer (Length : Slice_Index := 0) is private;
   ----------------------------------------------

   procedure Assign (Object : out Large_Integer; Value : Large_Integer);
   procedure Negate (Object : in out Large_Integer);

   function Equal (X, Y : Large_Integer) return Boolean;
   function "=" (X, Y : Large_Integer) return Boolean renames Equal;

   function "<"  (X, Y : Large_Integer) return Boolean;
   function ">"  (X, Y : Large_Integer) return Boolean;
   function "<=" (X, Y : Large_Integer) return Boolean;
   function ">=" (X, Y : Large_Integer) return Boolean;

   function Is_Zero (X : Large_Integer) return Boolean;
   function Is_One  (X : Large_Integer) return Boolean;

   function Is_Negative (X : Large_Integer) return Boolean;
   function Is_Positive (X : Large_Integer) return Boolean;

   function Hash (X : Large_Integer) return Arithmetic_Number;
   --  Return a hash value given a large integer

   function Num_Slices (X : Large_Integer) return Slice_Index;
   --  Return a count of the number of "slices" needed to represent the value,
   --  where a slice is a "digit" in the range 0 .. Slice_Number'Last.
   --  Return one for a value of zero.

   generic
      type Int is range <>;
   function Large_Integer_To_Integer (X : Large_Integer) return Int;

   function Image (X : Large_Integer) return String;

   package Variable_Length_Operations is
   ----------------------------------

      function "+" (X : Large_Integer) return Large_Integer;
      function "-" (X : Large_Integer) return Large_Integer;

      function "abs" (X : Large_Integer) return Large_Integer;

      function "+" (X, Y : Large_Integer) return Large_Integer;
      function "-" (X, Y : Large_Integer) return Large_Integer;
      function "*" (X, Y : Large_Integer) return Large_Integer;
      function "/" (X, Y : Large_Integer) return Large_Integer;

      function "mod" (X, Y : Large_Integer) return Large_Integer;
      function "rem" (X, Y : Large_Integer) return Large_Integer;

      function "**" (X : Large_Integer; Exp : Natural) return Large_Integer;

      function Zero return Large_Integer;
      function One  return Large_Integer;
      function Two  return Large_Integer;
      function Ten  return Large_Integer;

      function Minus_One return Large_Integer;
      function Minus_Two return Large_Integer;

      function First return Large_Integer;
      function Last  return Large_Integer;
         --  Return the smallest and greatest representable LARGE_INTEGERs.

      generic
         type Int is range <>;
      function Integer_To_Large_Integer (X : Int) return Large_Integer;

      function Value (Image : String) return Large_Integer;

   end Variable_Length_Operations;

   generic
      Length : Slice_Index;
   package Fixed_Length_Operations is
   -------------------------------

      subtype Fixed_Length_Large_Integer is Large_Integer (Length);
      ----------------------------------

      function "+" (X : Large_Integer) return Fixed_Length_Large_Integer;
      function "-" (X : Large_Integer) return Fixed_Length_Large_Integer;

      function "abs" (X : Large_Integer) return Fixed_Length_Large_Integer;

      function "+" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "-" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "*" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "/" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;

      function "mod" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;
      function "rem" (X, Y : Large_Integer) return Fixed_Length_Large_Integer;

      function "**" (X : Large_Integer; Exp : Natural)
        return Fixed_Length_Large_Integer;

      function Zero return Fixed_Length_Large_Integer;
      function One  return Fixed_Length_Large_Integer;
      function Two  return Fixed_Length_Large_Integer;
      function Ten  return Fixed_Length_Large_Integer;

      function Minus_One return Fixed_Length_Large_Integer;
      function Minus_Two return Fixed_Length_Large_Integer;

      function First return Fixed_Length_Large_Integer;
      function Last  return Fixed_Length_Large_Integer;
         --  Return the smallest and greatest representable
         --  FIXED_LENGTH_LARGE_INTEGERs.

      generic
         type Int is range <>;
      function Integer_To_Large_Integer (X : Int)
        return Fixed_Length_Large_Integer;

      function Value (Image : String) return Fixed_Length_Large_Integer;

   end Fixed_Length_Operations;

   Overflow,
   Division_By_Zero  : exception;

private

   type Number_Sign is range -1 .. +1;

   subtype Natural_Slice_Number is Slice_Number range 0 .. Slice_Number'Last;

   type Slice_Array is array (Slice_Index range <>) of Natural_Slice_Number;

   type Large_Integer (Length : Slice_Index := 0) is
      record
         Sign  : Number_Sign := 0;
         Value : Slice_Array (0 .. Length) := (others => 0);
      end record;

end PSC.Large_Integers;
