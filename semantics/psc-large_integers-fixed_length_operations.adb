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
separate (PSC.Large_Integers)

package body Fixed_Length_Operations is
------------------------------------

   function Copy_Of
     (X : Large_Integer; Length : Slice_Index) return Large_Integer is
      --  Returns a copy of X with a length equal to LENGTH.
      --  X.LENGTH must be less than or equal to LENGTH.
   begin
      if X.Length > Length then
         raise Overflow;
      else
         return (Length => Length,
                 Sign   => X.Sign,
                 Value  => X.Value & (1 .. Length - X.Length => 0));
      end if;
   end Copy_Of;

   pragma Inline (Copy_Of);

   function "+" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."+"(X), Length);
   end "+";

   function "-" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."-"(X), Length);
   end "-";

   function "abs" (X : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."abs"(X), Length);
   end "abs";

   function "+" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."+"(X, Y), Length);
   end "+";

   function "-" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."-"(X, Y), Length);
   end "-";

   function "*" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."*"(X, Y), Length);
   end "*";

   function "/" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."/"(X, Y), Length);
   end "/";

   function "mod" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."mod"(X, Y), Length);
   end "mod";

   function "rem" (X, Y : Large_Integer) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."rem"(X, Y), Length);
   end "rem";

   function "**"
     (X : Large_Integer; Exp : Natural) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations."**"(X, Exp), Length);
   end "**";

   function Zero return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => 0,
              Value  => (others => 0));
   end Zero;

   function One return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => +1,
              Value  => (0 => 1, others => 0));
   end One;

   function Two  return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => +1,
              Value  => (0 => 2, others => 0));
   end Two;

   function Ten  return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => +1,
              Value  => (0 => 10, others => 0));
   end Ten;

   function Minus_One return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => -1,
              Value  => (0 => 1, others => 0));
   end Minus_One;

   function Minus_Two return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => -1,
              Value  => (0 => 2, others => 0));
   end Minus_Two;

   function First return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => -1,
              Value  => (others => Slice_Number'Last));
   end First;

   function Last return Fixed_Length_Large_Integer is
   begin
      return (Length => Length,
              Sign   => +1,
              Value  => (others => Slice_Number'Last));
   end Last;

   function Integer_To_Large_Integer
     (X : Int) return Fixed_Length_Large_Integer is

      function To_Large_Integer is
        new Variable_Length_Operations.Integer_To_Large_Integer (Int);

   begin
      return Copy_Of (To_Large_Integer (X), Length);
   end Integer_To_Large_Integer;

   function Value (Image : String) return Fixed_Length_Large_Integer is
   begin
      return Copy_Of (Variable_Length_Operations.Value (Image), Length);
   end Value;

end Fixed_Length_Operations;
