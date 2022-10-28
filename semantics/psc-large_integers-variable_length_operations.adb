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

package body Variable_Length_Operations is
---------------------------------------

   function Min (Left, Right : Slice_Index) return Slice_Index
     renames Slice_Index'Min;
   function Max (Left, Right : Slice_Index) return Slice_Index
     renames Slice_Index'Max;

   function Packed (X : Large_Integer) return Large_Integer is

      X_Length : constant Slice_Index := Effective_Length (X);

   begin
      return (Length => X_Length,
              Sign   => X.Sign,
              Value  => X.Value (0 .. X_Length));
   end Packed;

   function To_Large_Integer
     (X : Slice_Array; Sign : Number_Sign) return Large_Integer is
   begin
      if Sign = 0 then
         return Zero;
      else
         for I in reverse X'Range loop
            if X (I) /= 0 then
               return (Length => I,
                       Sign   => Sign,
                       Value  => X (0 .. I));
            end if;
         end loop;
         return Zero;
      end if;
   end To_Large_Integer;

   pragma Inline (Packed, To_Large_Integer);

   function "+" (X : Large_Integer) return Large_Integer is
   begin
      return Packed (X);
   end "+";

   function "-" (X : Large_Integer) return Large_Integer is

      X_Length : constant Slice_Index := Effective_Length (X);

   begin
      return (Length => X_Length,
              Sign   => -X.Sign,
              Value  => X.Value (0 .. X_Length));
   end "-";

   function "abs" (X : Large_Integer) return Large_Integer is

      X_Length : constant Slice_Index := Effective_Length (X);

   begin
      return (Length => X_Length,
              Sign   => abs X.Sign,
              Value  => X.Value (0 .. X_Length));
   end "abs";

   function "+" (X, Y : Large_Integer) return Large_Integer is
   begin
      if X.Sign = 0 then
         return Packed (Y);
      elsif Y.Sign = 0 then
         return Packed (X);
      elsif X.Sign = Y.Sign then
         declare

            X_Length : constant Slice_Index := Effective_Length (X);
            Y_Length : constant Slice_Index := Effective_Length (Y);

            Result   : Slice_Array (0 .. Max (X_Length, Y_Length));
            Sum      : Arithmetic_Number;
            Carry    : Slice_Number range 0 .. 1 := 0;

         begin
            for I in 0 .. Min (X_Length, Y_Length) loop
               Sum := Arithmetic_Number (X.Value (I)) +
                 Arithmetic_Number (Y.Value (I)) + Arithmetic_Number (Carry);
               if Sum < Slice_Modulus then
                  Result (I) := Slice_Number (Sum);
                  Carry := 0;
               else
                  Result (I) := Slice_Number (Sum - Slice_Modulus);
                  Carry := 1;
               end if;
            end loop;
            if X_Length < Y_Length then
               --  to avoid CONSTRAINT_ERROR being raised by the
               --  evaluation of X_LENGTH + 1
               for I in X_Length + 1 .. Y_Length loop
                  if Y.Value (I) = Slice_Number'Last and Carry /= 0 then
                     Result (I) := 0;
                     Carry := 1;
                  else
                     Result (I) := Y.Value (I) + Carry;
                     Carry := 0;
                  end if;
               end loop;
            elsif Y_Length < X_Length then
               --  to avoid CONSTRAINT_ERROR being raised by the
               --  evaluation of Y_LENGTH + 1
               for I in Y_Length + 1 .. X_Length loop
                  if X.Value (I) = Slice_Number'Last and Carry /= 0 then
                     Result (I) := 0;
                     Carry := 1;
                  else
                     Result (I) := X.Value (I) + Carry;
                     Carry := 0;
                  end if;
               end loop;
            end if;
            if Carry = 0 then
               return (Length => Result'Last,
                       Sign   => X.Sign,
                       Value  => Result);
            elsif Result'Last = Slice_Index'Last then
               raise Overflow;
            else
               return (Length => Result'Last + 1,
                       Sign   => X.Sign,
                       Value  => Result & 1);
            end if;
         end;
      else
         return X - (-Y);
      end if;
   end "+";

   function "-" (X, Y : Large_Integer) return Large_Integer is
   begin
      if X.Sign = 0 then
         return -Y;
      elsif Y.Sign = 0 then
         return Packed (X);
      elsif X.Sign = Y.Sign then
         if (X.Sign > 0 and then X >= Y) or (X.Sign < 0 and then X <= Y) then
            declare

               X_Length : constant Slice_Index := Effective_Length (X);
               Y_Length : constant Slice_Index := Effective_Length (Y);

               Result   : Slice_Array (0 .. X_Length);
               Carry    : Slice_Number range 0 .. 1 := 0;

            begin
               for I in 0 .. Min (X_Length, Y_Length) loop
                  if X.Value (I) - Carry >= Y.Value (I) then
                     Result (I) := X.Value (I) - Y.Value (I) - Carry;
                     Carry := 0;
                  else
                     Result (I) := X.Value (I) +
                       Slice_Number (Slice_Modulus -
                         Arithmetic_Number (Y.Value (I)) -
                           Arithmetic_Number (Carry));
                     Carry := 1;
                  end if;
               end loop;
               if Y_Length < X_Length then
                  --  to avoid CONSTRAINT_ERROR being raised by the
                  --  evaluation of Y_LENGTH + 1
                  for I in Y_Length + 1 .. X_Length loop
                     if X.Value (I) = 0 and Carry /= 0 then
                        Result (I) := Slice_Number'Last;
                        Carry := 1;
                     else
                        Result (I) := X.Value (I) - Carry;
                        Carry := 0;
                     end if;
                  end loop;
               end if;
               return To_Large_Integer (Result, Sign => X.Sign);
            end;
         else
            return -(Y - X);
         end if;
      else
         return X + (-Y);
      end if;
   end "-";

   function "*" (X, Y : Large_Integer) return Large_Integer is
   begin
      if X.Sign = 0 or Y.Sign = 0 then
         return Zero;
      else
         declare

            X_Length : constant Slice_Index := Effective_Length (X);
            Y_Length : constant Slice_Index := Effective_Length (Y);

         begin
            if X_Length > Slice_Index'Last - Y_Length then
               raise Overflow;
            end if;
            declare

               Result   : Slice_Array (0 .. X_Length + Y_Length) :=
                            (others => 0);
               Sum      : Arithmetic_Number;
               Carry    : Arithmetic_Number range 0 .. Arithmetic_Number
                            (Slice_Number'Last) := 0;

            begin
               for I in 0 .. X_Length loop
                  for J in 0 .. Y_Length loop
                     Sum := Arithmetic_Number (Result (I + J)) +
                            Arithmetic_Number (X.Value (I)) *
                              Arithmetic_Number (Y.Value (J)) + Carry;
                     Result (I + J) := Slice_Number (Sum mod Slice_Modulus);
                     Carry := Sum / Slice_Modulus;
                  end loop;
                  for J in Y_Length + 1 .. Result'Last - I loop
                     Sum := Arithmetic_Number (Result (I + J)) + Carry;
                     Result (I + J) := Slice_Number (Sum mod Slice_Modulus);
                     Carry := Sum / Slice_Modulus;
                     exit when Carry = 0;
                  end loop;
               end loop;
               if Carry = 0 then
                  return (Length => Result'Last,
                          Sign   => X.Sign * Y.Sign,
                          Value  => Result);
               elsif Result'Last = Slice_Index'Last then
                  raise Overflow;
               else
                  return (Length => Result'Last + 1,
                          Sign   => X.Sign * Y.Sign,
                          Value  => Result & Slice_Number (Carry));
               end if;
            end;
         end;
      end if;
   end "*";

   procedure Divide (X, Y                :     Slice_Array;
                     Quotient, Remainder : out Slice_Array;
                     Remainder_Needed    :     Boolean) is
      --  X, Y, QUOTIENT and REMAINDER must have their lower bound equal to 0.
      --  One must have Y(Y'LAST) /= 0, X'LAST >= Y'LAST and
      --  QUOTIENT'LAST = X'LAST - Y'LAST.
      --  If REMAINDER_NEEDED is true, then one must have
      --  REMAINDER'LAST = Y'LAST.
      --
      --  Algorithm taken from Knuth, "The Art of Computer Programming",
      --  2nd Edition, Volume 2, Section 4.3.1, Algorithm D.

      procedure Multiply
        (X : in out Slice_Array; Factor : Natural_Slice_Number) is
         --  X := X * FACTOR;

         Product  : Arithmetic_Number;
         Carry    : Arithmetic_Number range
                     0 .. Arithmetic_Number (Slice_Number'Last) := 0;

      begin
         for I in X'Range loop
            Product := Arithmetic_Number (X (I)) *
              Arithmetic_Number (Factor) + Carry;
            X (I) := Slice_Number (Product mod Slice_Modulus);
            Carry := Product / Slice_Modulus;
         end loop;
      end Multiply;

      procedure Multiply_And_Subtract (X      : in out Slice_Array;
                                       Factor : Natural_Slice_Number;
                                       Y      : Slice_Array;
                                       Borrow : out Boolean) is
         --  X := X - FACTOR * Y;

         Term   : Arithmetic_Number;
         Carry  : Arithmetic_Number range
                    0 .. Arithmetic_Number (Slice_Number'Last) := 0;

      begin
         for I in X'Range loop
            Term := Arithmetic_Number (Factor) *
                      Arithmetic_Number (Y (Y'First + (I - X'First))) + Carry;
            if Arithmetic_Number (X (I)) >= Term mod Slice_Modulus then
               X (I) := X (I) - Slice_Number (Term mod Slice_Modulus);
               Carry := Term / Slice_Modulus;
            else
               X (I) := X (I) +
                Slice_Number (Slice_Modulus - Term mod Slice_Modulus);
               Carry := Term / Slice_Modulus + 1;
            end if;
         end loop;
         Borrow := Carry /= 0;
      end Multiply_And_Subtract;

      procedure Add (X : in out Slice_Array; Y : Slice_Array) is
         --  X := X + Y;

         Sum    : Arithmetic_Number;
         Carry  : Arithmetic_Number range 0 .. 1 := 0;

      begin
         for I in X'Range loop
            Sum := Arithmetic_Number (X (I)) +
                     Arithmetic_Number (Y (Y'First + (I - X'First))) + Carry;
            if Sum < Slice_Modulus then
               X (I) := Slice_Number (Sum);
               Carry := 0;
            else
               X (I) := Slice_Number (Sum - Slice_Modulus);
               Carry := 1;
            end if;
         end loop;
      end Add;

      procedure Divide (X         : Slice_Array;
                        Divisor   : Natural_Slice_Number;
                        Quotient  : out Slice_Array;
                        Remainder : out Natural_Slice_Number) is
         --  QUOTIENT  := X / DIVISOR;
         --  REMAINDER := X mod DIVISOR;

         Rest : Arithmetic_Number := 0;

      begin
         for I in reverse X'Range loop
            Rest := Rest * Slice_Modulus + Arithmetic_Number (X (I));
            Quotient (Quotient'First + (I - X'First)) :=
              Slice_Number (Rest / Arithmetic_Number (Divisor));
            Rest := Rest mod Arithmetic_Number (Divisor);
         end loop;
         Remainder := Slice_Number (Rest);
      end Divide;

      pragma Inline (Multiply, Multiply_And_Subtract, Add, Divide);

   begin
      if Y'Last = 0 then
         declare

            Remainder_Slice : Natural_Slice_Number;

         begin
            Divide (X, Divisor => Y (0),
                    Quotient => Quotient, Remainder => Remainder_Slice);
            if Remainder_Needed then
               Remainder := (0 => Remainder_Slice);
            end if;
         end;
      else
         if X'Last = Slice_Index'Last or Y'Last = Slice_Index'Last then
            raise Overflow;
         end if;
         declare

            U       : Slice_Array (0 .. X'Last + 1) := X & 0;
            V       : Slice_Array (0 .. Y'Last + 1) := Y & 0;

            V_1     : Natural_Slice_Number renames V (V'Last - 1);
            V_2     : Natural_Slice_Number renames V (V'Last - 2);

            D       : constant Natural_Slice_Number := Slice_Number
                        (Slice_Modulus / (Arithmetic_Number (V_1) + 1));

            Q_Hat,
            R_Hat,
            U_J_J1  : Arithmetic_Number;
            J_In_U  : Slice_Index range U'Range;
            Borrow  : Boolean;

            Trash   : Natural_Slice_Number;

         begin
            if D /= 1 then
               Multiply (U, Factor => D);
               Multiply (V, Factor => D);
            end if;
            for J in reverse 0 .. Quotient'Last loop
               J_In_U := U'Last - (Quotient'Last - J);
               U_J_J1 := Arithmetic_Number (U (J_In_U)) * Slice_Modulus +
                           Arithmetic_Number (U (J_In_U - 1));
               if U (J_In_U) = V_1 then
                  Q_Hat := Slice_Modulus - 1;
                  R_Hat := Arithmetic_Number (U (J_In_U - 1)) +
                             Arithmetic_Number (V_1);
               else
                  Q_Hat := U_J_J1 / Arithmetic_Number (V_1);
                  R_Hat := U_J_J1 mod Arithmetic_Number (V_1);
               end if;
               while R_Hat < Slice_Modulus and then
                     Arithmetic_Number (V_2) * Q_Hat >
                     R_Hat * Slice_Modulus + Arithmetic_Number (U (J_In_U - 2))
               loop
                  Q_Hat := Q_Hat - 1;
                  R_Hat := R_Hat + Arithmetic_Number (V_1);
               end loop;
               Multiply_And_Subtract (X      => U (J_In_U - V'Last .. J_In_U),
                                     Factor => Slice_Number (Q_Hat),
                                     Y      => V,
                                     Borrow => Borrow);
               if Borrow then
                  Q_Hat := Q_Hat - 1;
                  Add (U (J_In_U - V'Last .. J_In_U), V);
               end if;
               Quotient (J) := Slice_Number (Q_Hat);
            end loop;
            if Remainder_Needed then
               Divide (U (0 .. V'Last - 1), D,
                       Quotient => Remainder, Remainder => Trash);
            end if;
         end;
      end if;
   end Divide;

   function "/" (X, Y : Large_Integer) return Large_Integer is

      X_Length  : constant Slice_Index := Effective_Length (X);
      Y_Length  : constant Slice_Index := Effective_Length (Y);

   begin
      if Y.Sign = 0 then
         raise Division_By_Zero;
      elsif X.Sign = 0 or X_Length < Y_Length then
         return Zero;
      else
         declare

            Quotient   : Slice_Array (0 .. X_Length - Y_Length);
            Remainder  : Slice_Array (0 .. -1);

         begin
            Divide (X.Value (0 .. X_Length), Y.Value (0 .. Y_Length),
                    Quotient, Remainder, Remainder_Needed => False);
            return To_Large_Integer (Quotient, Sign => X.Sign * Y.Sign);
         end;
      end if;
   end "/";

   function "mod" (X, Y : Large_Integer) return Large_Integer is

      X_Length  : constant Slice_Index := Effective_Length (X);
      Y_Length  : constant Slice_Index := Effective_Length (Y);

   begin
      if Y.Sign = 0 then
         raise Division_By_Zero;
      elsif X.Sign = 0 then
         return Zero;
      elsif X_Length < Y_Length then
         if X.Sign = Y.Sign then
            return Packed (X);
         else
            return X + Y;
         end if;
      else
         declare

            Quotient   : Slice_Array (0 .. X_Length - Y_Length);
            Remainder  : Slice_Array (0 .. Y_Length);

         begin
            Divide (X.Value (0 .. X_Length), Y.Value (0 .. Y_Length),
                    Quotient, Remainder, Remainder_Needed => True);
            declare

               Result : constant Large_Integer :=
                 To_Large_Integer (Remainder, Sign => X.Sign);

            begin
               if Result.Sign = 0 or X.Sign = Y.Sign then
                  return Result;
               else
                  return Result + Y;
               end if;
            end;
         end;
      end if;
   end "mod";

   function "rem" (X, Y : Large_Integer) return Large_Integer is

      X_Length  : constant Slice_Index := Effective_Length (X);
      Y_Length  : constant Slice_Index := Effective_Length (Y);

   begin
      if Y.Sign = 0 then
         raise Division_By_Zero;
      elsif X.Sign = 0 then
         return Zero;
      elsif X_Length < Y_Length then
         return Packed (X);
      else
         declare

            Quotient   : Slice_Array (0 .. X_Length - Y_Length);
            Remainder  : Slice_Array (0 .. Y_Length);

         begin
            Divide (X.Value (0 .. X_Length), Y.Value (0 .. Y_Length),
                    Quotient, Remainder, Remainder_Needed => True);
            return To_Large_Integer (Remainder, Sign => X.Sign);
         end;
      end if;
   end "rem";

   function "**" (X : Large_Integer; Exp : Natural) return Large_Integer is

      function Square (X : Large_Integer) return Large_Integer is
      begin
         return X * X;
      end Square;

      pragma Inline (Square);

   begin
      if Exp = 0 then
         return One;
      elsif Exp = 1 then
         return Packed (X);
      elsif Exp mod 2 = 0 then
         return Square (X ** (Exp / 2));
      else
         return X * Square (X ** (Exp / 2));
      end if;
   end "**";

   function Zero return Large_Integer is
   begin
      return (Length => 0,
              Sign   => 0,
              Value  => (0 => 0));
   end Zero;

   function One return Large_Integer is
   begin
      return (Length => 0,
              Sign   => +1,
              Value  => (0 => 1));
   end One;

   function Two return Large_Integer is
   begin
      return (Length => 0,
              Sign   => +1,
              Value  => (0 => 2));
   end Two;

   function Ten return Large_Integer is
   begin
      return (Length => 0,
              Sign   => +1,
              Value  => (0 => 10));
   end Ten;

   function Minus_One return Large_Integer is
   begin
      return (Length => 0,
              Sign   => -1,
              Value  => (0 => 1));
   end Minus_One;

   function Minus_Two return Large_Integer is
   begin
      return (Length => 0,
              Sign   => -1,
              Value  => (0 => 2));
   end Minus_Two;

   function First return Large_Integer is
   begin
      return (Length => Slice_Index'Last,
              Sign   => -1,
              Value  => (others => Slice_Number'Last));
   end First;

   function Last return Large_Integer is
   begin
      return (Length => Slice_Index'Last,
              Sign   => +1,
              Value  => (others => Slice_Number'Last));
   end Last;

   function Integer_To_Large_Integer (X : Int) return Large_Integer is

      Modulus        : Int;
      Y             : Int := abs X;
      X_Sign        : Number_Sign;
      Result_Length : Slice_Index := 0;

   begin
      if X = 0 then
         return Zero;
      elsif X > 0 then
         X_Sign := +1;
      else
         X_Sign := -1;
      end if;

      if Int'Pos (Int'Last) <= Slice_Number'Pos (Slice_Number'Last) then
         return (Length => 0,
                 Sign   => X_Sign,
                 Value  => (0 => Slice_Number (abs X)));
      else
         Modulus := Slice_Number'Pos (Slice_Number'Last) + 1;
      end if;

      loop
         Y := Y / Modulus;
         exit when Y = 0;
         if Result_Length < Slice_Index'Last then
            Result_Length := Result_Length + 1;
         else
            raise Overflow;
         end if;
      end loop;
      Y := abs X;
      declare

         Result : Slice_Array (0 .. Result_Length);

      begin
         for I in Result'Range loop
            Result (I) := Slice_Number (Y mod Modulus);
            Y := Y / Modulus;
         end loop;
         return (Length => Result_Length,
                 Sign   => X_Sign,
                 Value  => Result);
      end;
   end Integer_To_Large_Integer;

   function Value (Image : String) return Large_Integer is separate;

end Variable_Length_Operations;
