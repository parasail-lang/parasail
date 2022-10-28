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

package body PSC.Large_Integers is
---------------------------

   type Digit_Number is range 0 .. 9;

   function Digit_Image (D : Digit_Number) return Character is
      --  Convert a digit to a corresponding character
   begin
      return Character'Val (Character'Pos ('0') + D);
   end Digit_Image;

   function Digit_Value (C : Character) return Digit_Number is
      --  Convert a character to a corresponding digit
   begin
      return Character'Pos (C) - Character'Pos ('0');
   end Digit_Value;

   function Is_Digit (C : Character) return Boolean is
      --  Return True if C corresponds to a digit's image
   begin
      return C in '0' .. '9';
   end Is_Digit;

   Slice_Modulus : constant Arithmetic_Number :=
                    Arithmetic_Number (Slice_Number'Last) + 1;

   function Effective_Length (X : Large_Integer) return Slice_Index is
      pragma Inline (Effective_Length);
   begin
      for K in reverse X.Value'Range loop
         if X.Value (K) /= 0 then
            return K;
         end if;
      end loop;
      return 0;
   end Effective_Length;

   procedure Assign (Object : out Large_Integer; Value : Large_Integer) is
   begin
      if Object'Constrained then
         declare

            Value_Length : constant Slice_Index := Effective_Length (Value);

         begin
            if Value_Length <= Object.Length then
               Object.Sign := Value.Sign;
               Object.Value (0 .. Value_Length) :=
                 Value.Value (0 .. Value_Length);
               Object.Value (Value_Length + 1 .. Object.Length) :=
                 (others => 0);
            else
               raise Overflow;
            end if;
         end;
      else
         Object := Value;
      end if;
   end Assign;

   procedure Negate (Object : in out Large_Integer) is
   begin
      Object.Sign := -Object.Sign;
   end Negate;

   function Equal (X, Y : Large_Integer) return Boolean is
   begin
      if X.Sign = Y.Sign then
         declare

            X_Length : constant Slice_Index := Effective_Length (X);

         begin
            if X_Length = Effective_Length (Y) then
               return X.Value (0 .. X_Length) = Y.Value (0 .. X_Length);
            end if;
         end;
      end if;
      return False;
   end Equal;

   function Hash (X : Large_Integer) return Arithmetic_Number is
   --  Return a hash value given a large integer
      X_Length : constant Slice_Index := Effective_Length (X);
   begin
      return Arithmetic_Number (X_Length) * 61 +
        Arithmetic_Number (X.Value (X_Length)) * (Slice_Modulus - 13) +
        Arithmetic_Number (X.Value (0));
   end Hash;

   function "<" (X, Y : Large_Integer) return Boolean is

      X_Length : constant Slice_Index := Effective_Length (X);
      Y_Length : constant Slice_Index := Effective_Length (Y);

   begin
      if X.Sign /= Y.Sign then
         return X.Sign < Y.Sign;
      end if;
      if X_Length = Y_Length then
         for K in reverse 0 .. X_Length loop
            if X.Value (K) /= Y.Value (K) then
               if X.Sign > 0 then
                  return X.Value (K) < Y.Value (K);
               else
                  return X.Value (K) > Y.Value (K);
               end if;
            end if;
         end loop;
         return False;
      else
         if X.Sign > 0 then
            return X_Length < Y_Length;
         else
            return X_Length > Y_Length;
         end if;
      end if;
   end "<";

   function ">" (X, Y : Large_Integer) return Boolean is
   begin
      return Y < X;
   end ">";

   function "<=" (X, Y : Large_Integer) return Boolean is

      X_Length : constant Slice_Index := Effective_Length (X);
      Y_Length : constant Slice_Index := Effective_Length (Y);

   begin
      if X.Sign /= Y.Sign then
         return X.Sign < Y.Sign;
      end if;
      if X_Length = Y_Length then
         for K in reverse 0 .. X_Length loop
            if X.Value (K) /= Y.Value (K) then
               if X.Sign > 0 then
                  return X.Value (K) < Y.Value (K);
               else
                  return X.Value (K) > Y.Value (K);
               end if;
            end if;
         end loop;
         return True;
      else
         if X.Sign > 0 then
            return X_Length < Y_Length;
         else
            return X_Length > Y_Length;
         end if;
      end if;
   end "<=";

   function ">=" (X, Y : Large_Integer) return Boolean is
   begin
      return Y <= X;
   end ">=";

   function Is_Zero (X : Large_Integer) return Boolean is
   begin
      return X.Sign = 0;
   end Is_Zero;

   function Is_One (X : Large_Integer) return Boolean is
   begin
      return X.Sign = 1
        and then Effective_Length (X) = 0 and then X.Value (1) = 1;
   end Is_One;

   function Is_Negative (X : Large_Integer) return Boolean is
   begin
      return X.Sign = -1;
   end Is_Negative;

   function Is_Positive (X : Large_Integer) return Boolean is
   begin
      return X.Sign = +1;
   end Is_Positive;

   function Large_Integer_To_Integer (X : Large_Integer) return Int is

      X_Length : constant Slice_Index := Effective_Length (X);

      Result   : Int := Int (X.Value (X_Length));

      Modulus  : Int;

   begin
      if Int'Pos (Int'Last) <= Slice_Number'Pos (Slice_Number'Last) then
         --  This indicates should overflow if more than one slice
         Modulus := 1;
      else
         Modulus := Slice_Number'Pos (Slice_Number'Last) + 1;
      end if;

      for K in reverse 0 .. X_Length - 1 loop
         if Result > 0 and then Modulus = 1 then
            --  Overflow since "true" Modulus is > Int'Last.
            raise Constraint_Error;
         end if;
         Result := Modulus * Result + Int (X.Value (K));
      end loop;
      case X.Sign is
         when -1 =>
            return -Result;
         when 0 =>
            return 0;
         when +1 =>
            return Result;
      end case;
   end Large_Integer_To_Integer;

   function Num_Slices (X : Large_Integer) return Slice_Index is
   --  Return a count of the number of "slices" needed to represent the value,
   --  where a slice is a "digit" in the range 0 .. Slice_Number'Last.
   --  Return one for a value of zero.
   begin
      return Effective_Length (X) + 1;
   end Num_Slices;

   function Image (X : Large_Integer) return String is separate;

   package body Variable_Length_Operations is separate;
   ---------------------------------------

   package body Fixed_Length_Operations is separate;
   ------------------------------------

end PSC.Large_Integers;
