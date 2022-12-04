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

with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with PSC.Large_Integers;
pragma Elaborate (PSC.Large_Integers);

with PSC.Hash_Tables;
with PSC.Languages;
with PSC.Strings;
package body PSC.Univ_Integers is
   --  Package to implement Univ_Integers using a 64-bit integer,
   --  but with special values for the high 32-bits that signify the
   --  value is not represented by the 64-bit integer, but is rather
   --  represented by a separate "large" integer in a table indexed
   --  by the low 32 bits.
   --  These separate large integers will be hashed so there will be
   --  bounded storage growth.
   --  The sign of the 64-bit integer will give the sign of the overall
   --  value.

   Debug : constant Boolean := False;
   Collect_Stats : constant Boolean := True;

   function To_Unsigned is new Ada.Unchecked_Conversion
     (Univ_Integer, Interpreter.Unsigned_Word_Type);

   Large_Integer_Indicator_Mask : constant Unsigned_Word_Type :=
     16#FFFF_FFFF_0000_0000#;
      --  If (To_Unsigned (Univ) and Large_Integer_Indicator_Mask) in
      --   Positive_Large_Integer_Indicator | Negative_Large_Integer_Indicator
      --  then low 32 bits of absolute value is an index into
      --  a table of "large" values.
      --  We do not use index = 0 so that we can negate a positive
      --  number with high 32 bits = Positive_Large_Integer_Indicator
      --  always match the Negative_Large_Integer_Indicator.

   Positive_Large_Integer_Indicator : constant Unsigned_Word_Type :=
     16#1BAD_FEED_0000_0000#;
      --  Some easily recognizable but unlikely value devoted
      --  to large values.

   Negative_Large_Integer_Indicator : constant Unsigned_Word_Type :=
     Large_Integer_Indicator_Mask - Positive_Large_Integer_Indicator;

   Null_Value_Indicator : constant Unsigned_Word_Type :=
     16#8000_0000_0000_0000#;
      --  This indicates that we *might* have a null value (presuming
      --  low order 32 bits are in fact all zeroes).

   type Slice_Index is range 0 .. 500;
   Bits_Per_Slice : constant := 31;
   type Slice_Number is range -2**Bits_Per_Slice .. +2**Bits_Per_Slice - 1;

   package Univ_Large_Integers is new Large_Integers
     (Slice_Number => Slice_Number,
      Arithmetic_Number => Word_Type,
      --  Its range must be at least 0 .. (Slice_Number'Last + 1)**2 - 1
      Slice_Index => Slice_Index);

   subtype Univ_Large is Univ_Large_Integers.Large_Integer;
   use Univ_Large_Integers, Univ_Large_Integers.Variable_Length_Operations;

   type Univ_Large_CPtr is access constant Univ_Large;
   --  These point at shared large (positive) values.

   type Univ_Large_Ptr is access Univ_Large;
   --  This is used for actually allocating space for copies of large ints
   procedure Free is
     new Ada.Unchecked_Deallocation (Univ_Large, Univ_Large_Ptr);

   type Large_Key is range 0 .. 2**15 - 1;
   --  This type is used as an index into a table of large (positive) values.
   --  This could be enlarged if we decide it is appropriate.
   --  For now we preallocate this table, so we will want to
   --  be sure this is adequate, but not ridiculous.

   Large_Integer_Table : array (Large_Key range 1 .. Large_Key'Last) of
     Univ_Large_CPtr;

   function Same_Large_Integer (Left, Right : Univ_Large_CPtr) return Boolean;
   --  Return True if Left and Right represent the same Large_Integer.

   function Large_Integer_Hash (Univ : Univ_Large_CPtr)
     return Unsigned_Word_Type;
   --  Compute a hash value given a Large_Integer.

   package Large_Integer_Hash_Tables is
     new Hash_Tables
       (Element_Type => Large_Key,
        Key_Type => Univ_Large_CPtr,
        Equiv => Same_Large_Integer,
        Hash_Type => Unsigned_Word_Type,
        Hash => Large_Integer_Hash,
        Collect_Stats => Collect_Stats);

   Large_Integer_Hash_Table : Large_Integer_Hash_Tables.Hash_Table;

   function From_Large is new Large_Integer_To_Integer (Word_Type);

   function To_Large is new Integer_To_Large_Integer (Word_Type);

   Large_Word_Max : constant Univ_Large := To_Large (Word_Type'Last);
   Large_Word_Min : constant Univ_Large := To_Large (-Word_Type'Last);

   function From_Word is new Ada.Unchecked_Conversion
     (Interpreter.Word_Type, Interpreter.Unsigned_Word_Type);

   function To_Word is new Ada.Unchecked_Conversion
     (Unsigned_Word_Type, Word_Type);

   function Same_Large_Integer
     (Left, Right : Univ_Large_CPtr) return Boolean is
   --  Return True if Left and Right represent the same Large_Integer.
   begin
      return Left.all = Right.all;
   end Same_Large_Integer;

   function Large_Integer_Hash (Univ : Univ_Large_CPtr)
     return Unsigned_Word_Type is
   --  Compute a hash value given a Large_Integer.
   begin
      return From_Word (Hash (Univ.all));
   end Large_Integer_Hash;

   function Is_Large_Univ_Integer (Val : Univ_Integer) return Boolean;
   --  Return True if Val is one of the values that is represented
   --  using an index into a table of Large_Integers.
   pragma Inline (Is_Large_Univ_Integer);

   function Is_Large_Univ_Integer (Val : Univ_Integer) return Boolean is
      Indic : constant Unsigned_Word_Type :=
        From_Word (Word_Type (Val)) and Large_Integer_Indicator_Mask;
   begin
      return Indic = Positive_Large_Integer_Indicator
        or else Indic = Negative_Large_Integer_Indicator;
   end Is_Large_Univ_Integer;

   protected Key_Manager is
      --  Keeper of the Keys for Large_Integers

      procedure Get_Key_Locked
        (CPtr : Univ_Large_CPtr; Key : out Large_Key; Is_New : out Boolean);
      --  Get a key for the given large integer.
      --  Is_New is True if this is a new key.

      function Last_Assigned_Key return Large_Key;
      --  Return last assigned key (so far).
   private
      Num_Keys : Large_Key := 0;
   end Key_Manager;

   protected body Key_Manager is
      procedure Get_Key_Locked
        (CPtr : Univ_Large_CPtr; Key : out Large_Key; Is_New : out Boolean) is
         use Large_Integer_Hash_Tables;
         Existing_Elem : Element_Ref;
         New_Key : constant Large_Key := Num_Keys + 1;
      begin
         --  Enter new key associated with given Large Integer,
         --  if not already there
         Enter_Element
           (Large_Integer_Hash_Table, CPtr, New_Key, Existing_Elem);

         if Existing_Elem /= null then
            --  It was actually already there
            Key := Existing_Elem.all;
            Is_New := False;
         else
            --  We added it, so also add to array-based table
            --  and bump the count of keys.
            Large_Integer_Table (New_Key) := CPtr;
            Num_Keys := New_Key;
            Key := New_Key;
            Is_New := True;
         end if;
      end Get_Key_Locked;

      function Last_Assigned_Key return Large_Key is
      begin
         return Num_Keys;
      end Last_Assigned_Key;
   end Key_Manager;

   function Get_Key (CPtr : Univ_Large_CPtr) return Large_Key;
   --  Given a pointer to a Univ_Large, look it up in the
   --  hash table of Large_Integers.  If found, return the Key.
   --  If not found, copy new Large_Integer on the heap,
   --  get a lock, enter into hash table, and point to it from
   --  the array-based table.

   function Get_Key (CPtr : Univ_Large_CPtr) return Large_Key is
      use Large_Integer_Hash_Tables;
      Key_Ptr : constant Element_Ref :=
        Find_Element (Large_Integer_Hash_Table, CPtr);

   begin
      if Key_Ptr /= null then
         return Key_Ptr.all;
      end if;

      declare
         Key : Large_Key;
         Is_New : Boolean;
         Alloc_Ptr : Univ_Large_Ptr := new Large_Integer'(CPtr.all);
      begin
         Key_Manager.Get_Key_Locked (Univ_Large_CPtr (Alloc_Ptr), Key, Is_New);

         if not Is_New then
            --  We didn't need the newly allocated copy after all
            Free (Alloc_Ptr);
         end if;

         return Key;
      end;
   end Get_Key;

   function To_Large_Univ_Integer (Val : Univ_Large) return Univ_Integer;
   --  Convert Val into a "Large" Univ_Integer which is represented
   --  with a special value in the high 32 bits, and an index into
   --  a table of Large_Integers in the low 32 bits.

   function To_Large_Univ_Integer (Val : Univ_Large) return Univ_Integer is
      Abs_Val : aliased Univ_Large := Val;
   begin
      if Is_Negative (Val) then
         Negate (Abs_Val);
         return Univ_Integer
                   (-To_Word (Positive_Large_Integer_Indicator +
                      Unsigned_Word_Type
                        (Get_Key (Abs_Val'Unchecked_Access))));
      else
         return Univ_Integer
                   (To_Word (Positive_Large_Integer_Indicator +
                      Unsigned_Word_Type
                        (Get_Key (Abs_Val'Unchecked_Access))));
      end if;
   end To_Large_Univ_Integer;

   Two_To_64_Val : constant Univ_Integer :=
     To_Large_Univ_Integer (To_Large (2) ** 64);

   function Two_To_64 return Univ_Integer is
   begin
      return Two_To_64_Val;
   end Two_To_64;

   function Univ_To_Univ_Large (Val : Univ_Integer) return Univ_Large;
   --  Convert a Univ_Integer to a Univ_Large
   --  taking into account that it might *already* be represented
   --  as a Univ_Large.

   function Univ_To_Univ_Large (Val : Univ_Integer) return Univ_Large is
   begin
      if not Is_Large_Univ_Integer (Val) then
         --  Not one of the "special" values
         return To_Large (Word_Type (Val));
      else
         --  Already represented as a Univ_Large.
         declare
            --  Isolate the key
            Key : constant Large_Key := Large_Key
              (From_Word (abs Word_Type (Val)) and
                 not Large_Integer_Indicator_Mask);
         begin
            if Word_Type (Val) < 0 then
               return -Large_Integer_Table (Key).all;
            else
               return Large_Integer_Table (Key).all;
            end if;
         end;
      end if;
   end Univ_To_Univ_Large;

   function Num_Slices (Val : Univ_Integer) return Positive;
   --  Return a count of the number of "slices" needed to represent
   --  the Univ_Integer, where a slice is a "digit"
   --  in the range 0 .. 2**Bits_Per_Slice-1.
   --  Return one for a value of zero.

   function Num_Slices (Val : Univ_Integer) return Positive is
   begin
      if Val < 0 then
         return Num_Slices (-Val);
      elsif not Is_Large_Univ_Integer (Val) then
         case Val is
            when 0 .. Univ_Integer (2**Bits_Per_Slice - 1) =>
               return 1;
            when Univ_Integer (2**Bits_Per_Slice) ..
                   Univ_Integer (2**(Bits_Per_Slice * 2) - 1) =>
               return 2;
            when others =>
               return 3;
         end case;
      else
         return Positive (Num_Slices (Univ_To_Univ_Large (Val)));
      end if;
   end Num_Slices;

   function To_Univ_Integer (Val : Univ_Large) return Univ_Integer;
   --  Convert a large value into a Univ_Integer, adding
   --  it to the table of large integers if appropriate.

   function To_Univ_Integer (Val : Univ_Large) return Univ_Integer is
   begin
      if Val <= Large_Word_Max
        and then Val >= Large_Word_Min
      then
         declare
            Result : constant Univ_Integer := Univ_Integer (From_Large (Val));
         begin
            if not Is_Large_Univ_Integer (Result) then
               return Result;
            end if;
         end;

         --  Result is in the "special" zone so need to treat it as though
         --  it were a large value.
      end if;

      return To_Large_Univ_Integer (Val);
   end To_Univ_Integer;

   function From_Word (Val : Word_Type) return Univ_Integer is
   begin
      if not Is_Large_Univ_Integer (Univ_Integer (Val)) then
         --  Word is OK as is
         return Univ_Integer (Val);
      else
         --  Value happens to bump into "special" zone so need
         --  to convert to a "large" Univ_Integer.
         return To_Large_Univ_Integer (To_Large (Word_Type (Val)));
      end if;
   end From_Word;

   function Fits_In_Word (Val : Univ_Integer) return Boolean is
   --  Returns true if Val is in range of Interpreter.Word_Type;
   --  This returns True if Val = Null_Univ_Integer.
   begin
      if not Is_Large_Univ_Integer (Val) then
         return True;
      else
         declare
            Large : Univ_Large renames Univ_To_Univ_Large (Val);
         begin
            return Large <= Large_Word_Max
              and then large >= Large_Word_Min;
         end;
      end if;
   end Fits_In_Word;

   function From_Univ_Integer (Val : Univ_Integer)
   --  Convert Univ_Integer to a Word
     return Interpreter.Word_Type is
   begin
      if not Is_Large_Univ_Integer (Val) then
         --  Simple conversion
         return Word_Type (Val);
      else
         return From_Large (Univ_To_Univ_Large (Val));
      end if;
   end From_Univ_Integer;

   function From_Unsigned_Word (Val : Unsigned_Word_Type)
     return Univ_Integer is
   --  Convert Unsigned_Word_Type into a Univ_Integer, handling the
   --  "special" zone which needs to be treated specially.
      Result_As_Word : constant Word_Type := To_Word (Val);
   begin
      if Result_As_Word >= 0 then
         --  It is positive, so can convert as word
         return From_Word (Result_As_Word);
      else
         --  Need to add back 2**64 to make it positive.
         return From_Word (Result_As_Word) + Two_To_64_Val;
      end if;
   end From_Unsigned_Word;

   function To_Unsigned_Word (Val : Univ_Integer)
     return Interpreter.Unsigned_Word_Type is
   --  Convert Univ_Integer to an Unsigned_Word,
   --  wrapping around if not in 0 .. 2**64 - 1
   begin
      if not Is_Large_Univ_Integer (Val) then
         --  Not in "special" zone so can convert directly
         return To_Unsigned (Val);
      else
         declare
            Residue_64 : constant Univ_Integer := Val mod Two_To_64_Val;
         begin
            if not Is_Large_Univ_Integer (Residue_64) then
               --  We can convert it directly now
               return To_Unsigned (Residue_64);
            else
               --  Break into two 32-bit halves and recombine
               return To_Unsigned (Residue_64 mod Two_To_32) +
                 2**32 * To_Unsigned (Residue_64 / Two_To_32);
            end if;
         end;
      end if;
   end To_Unsigned_Word;

   function Hex_Image
     (Val : Unsigned_Word_Type;
      Underscores_Every : Natural := 4;
      Zero_Pad : Boolean := False) return String
   is
   --  Produce a base-16 image of a 64-bit unsigned number
      package Uns_IO is new Ada.Text_IO.Modular_IO (Unsigned_Word_Type);

      Result         : String (1 .. 40);
      Result_With_Underscores : String (1 .. 40);
      J              : Natural;
      Between_Sharps : Boolean := False;
      Clump          : Natural := Underscores_Every;
   begin
      if Clump = 0 then
         --  No underscores.
         Clump := 1000;
      end if;

      Uns_IO.Put (Result, Val, Base => 16);

      for I in Result'Range loop
         --  Skip leading spaces
         if Result (I) /= ' ' then
            --  Insert underscores
            J := Result_With_Underscores'Last;

            for K in reverse I .. Result'Last loop
               if Result (K) = '#' then
                  if Zero_Pad and Between_Sharps then
                     --  Pad with zeroes
                     for L in Result'Last - K ..
                       Unsigned_Word_Type'Size / 4
                     loop
                        if L mod Clump = 1 then
                           Result_With_Underscores (J) := '_';
                           J := J - 1;
                        end if;
                        Result_With_Underscores (J) := '0';
                        J := J - 1;
                     end loop;
                  end if;
                  Between_Sharps := not Between_Sharps;
               end if;

               if Between_Sharps
                 and then K < Result'Last - 1
                 and then (Result'Last - K) mod Clump = 1
               then
                  --  Time to put in an underscore
                  Result_With_Underscores (J) := '_';
                  J := J - 1;
               end if;

               Result_With_Underscores (J) := Result (K);
               J := J - 1;
            end loop;

            return Result_With_Underscores
                     (J + 1 .. Result_With_Underscores'Last);
         end if;
      end loop;

      raise Program_Error;  --  Can't be all spaces
   end Hex_Image;

   function Image (Val : Univ_Integer) return String is
   begin
      if Fits_In_Word (Val) then
         declare
            Im : String renames Word_Type'Image (From_Univ_Integer (Val));
         begin
            if Val >= 0 then
               --  Remove the leading space
               return Im (Im'First + 1 .. Im'Last);
            else
               return Im;
            end if;
         end;
      else
         return Image (Univ_To_Univ_Large (Val));
      end if;
   end Image;

   function Value (Str : String) return Univ_Integer is
      Len : Natural := Str'Length;
      First : Positive := Str'First;
      Radix : Integer := 10;
      Result : Univ_Integer := 0;

      Decimal_Digit_Offset : constant Natural := Character'Pos ('0');
      UC_Hex_Digit_Offset : constant Integer := Character'Pos ('A') - 10;
      LC_Hex_Digit_Offset : constant Integer := Character'Pos ('a') - 10;

      After_Sharp : Boolean := False;
      Only_Spaces : Boolean := False;
   begin
      if Str = "null" or else Len = 0 then
         return Null_Univ_Integer;
      elsif Str (First) = ' ' then
         loop
            First := First + 1;
            if First > Str'Last then
               return Null_Univ_Integer;
            end if;
            exit when Str (First) /= ' ';
         end loop;
         Len := Str'Last - First + 1;
      end if;

      if Str (First) = '-' then
         return -Value (Str (First + 1 .. Str'Last));

      elsif Len > 2 and then Str (First) = '0' then
         --  Convert to "standard" Ada notation with <radix>#...#
         case Str (First + 1) is
            when 'x' | 'X' =>
               --  Base 16
               return Value ("16#" & Str (First + 2 .. Str'Last) & '#');
            when 'b' | 'B' =>
               --  Base 2
               return Value ("2#" & Str (First + 2 .. Str'Last) & '#');
            when others =>
               null;
         end case;
      end if;

      --  Start with radix = 10, shift radix if we hit a '#'
      for I in Str'Range loop
         declare
            Chr : constant Character := Str (I);
         begin
            if Only_Spaces and then Chr /= ' ' then
               --  Only spaces allowed at the moment
               return Null_Univ_Integer;
            end if;

            case Chr is
               when '0' .. '9' =>
                  if Character'Pos (Chr) - Decimal_Digit_Offset >= Radix then
                     --  Alphabetic digit is too large
                     return Null_Univ_Integer;
                  end if;

                  Result := Result * Univ_Integer (Radix) +
                    Univ_Integer (Character'Pos (Chr) - Decimal_Digit_Offset);

               when 'A' .. 'Z' =>
                  if Character'Pos (Chr) - UC_Hex_Digit_Offset >= Radix then
                     --  Alphabetic digit is too large
                     return Null_Univ_Integer;
                  end if;

                  Result := Result * Univ_Integer (Radix) +
                    Univ_Integer (Character'Pos (Chr) - UC_Hex_Digit_Offset);

               when 'a' .. 'z' =>
                  if Character'Pos (Chr) - LC_Hex_Digit_Offset >= Radix then
                     --  Alphabetic digit is too large
                     return Null_Univ_Integer;
                  end if;

                  Result := Result * Univ_Integer (Radix) +
                    Univ_Integer (Character'Pos (Chr) - LC_Hex_Digit_Offset);

               when '_' =>
                  if I = Str'Last
                    or else Str (I + 1) = '_'
                    or else Str (I + 1) = '#'
                    or else Str (I + 1) = ' '
                    or else I = First
                    or else Str (I - 1) = '#'
                  then
                     --  Bad syntax if starts or ends with '_' or
                     --  has two in a row
                     return Null_Univ_Integer;
                  end if;

               when '#' =>
                  if After_Sharp then
                     --  End of numeral
                     After_Sharp := False;
                     Only_Spaces := True;
                  else
                     if Result not in 2 .. 36 then
                        --  Unsupported radix
                        return Null_Univ_Integer;
                     end if;
                     Radix := Positive (Result);
                     After_Sharp := True;
                     Result := 0;
                  end if;

               when ' ' =>
                  if After_Sharp then
                     --  Expecting a second '#'
                     return Null_Univ_Integer;
                  end if;
                  --  Only spaces allowed from here on
                  Only_Spaces := True;

               when others =>
                  --  Syntax error
                  return Null_Univ_Integer;
            end case;
         end;
      end loop;
      return Result;
   end Value;

   function Hex_Image
     (Val : Univ_Integer;
      Underscores_Every : Natural := 4) return String is
   --  Produce a base-16 image of a Univ_Integer
      Num_Chars      : constant Natural :=
                         Natural (Num_Slices (Val)) *
                           (9 + 8 / Underscores_Every) + 6;
      Result         : String (1 .. Num_Chars);
      Clump          : Natural := Underscores_Every;
      Num_In_Clump   : Natural := 0;
      Is_Negative    : constant Boolean := Val < 0;
      Abs_Val        : Univ_Integer := abs Val;
      Digit_Chars    : constant array (Natural range 0 .. 15) of Character :=
                        ('0', '1', '2', '3', '4', '5', '6', '7',
                         '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Index          : Positive := Result'Last;
   begin
      if Clump = 0 then
         --  No underscores.
         Clump := 10_000;
      end if;

      --  Ends with a '#'
      Result (Index) := '#';
      Index := Index - 1;

      loop
         declare
            Digit : constant Natural := Natural (Abs_Val rem 16);
            --  NOTE: We presume "small" numbers (e.g. < 2**31)
            --        are represented as is, so "16" works for Univ_Integer.
            Digit_Char : constant Character := Digit_Chars (Digit);
         begin
            if Num_In_Clump >= Clump then
               Result (Index) := '_';
               Index := Index - 1;
               Num_In_Clump := 0;
            end if;
            Result (Index) := Digit_Char;
            Index := Index - 1;

            Abs_Val := Abs_Val - Univ_Integer (Digit);
            exit when Abs_Val = 0;

            Abs_Val := Abs_Val / 16;
            Num_In_Clump := Num_In_Clump + 1;
         end;
      end loop;

      --  Put on the "16#" prefix
      Result (Index) := '#';
      Result (Index - 1) := '6';
      Result (Index - 2 .. Index) := "16#";
      Index := Index - 2;

      --  Prefix with '-' if negative
      if Is_Negative then
         Index := Index - 1;
         Result (Index) := '-';
      end if;

      declare
         subtype Starts_At_One is String (1 .. Result'Last - Index + 1);
      begin
         --  Slide result so indexing starts at one.
         return Starts_At_One (Result (Index .. Result'Last));
      end;
   end Hex_Image;

   -------------------------

   function "+" (Right : Univ_Integer) return Univ_Integer is
   begin
      return Right;
   end "+";

   function "-" (Right : Univ_Integer) return Univ_Integer is
   begin
      if Right = Null_Univ_Integer then
         return Right;
      else
         --  We never have to check for the "special" zone when negating.
         return Univ_Integer (-Word_Type (Right));
      end if;
   end "-";

   function "abs" (Right : Univ_Integer) return Univ_Integer is
   begin
      if Right = Null_Univ_Integer then
         --  Null remains Null  --  TBD: Or propagates an exception?
         return Right;
      else
         --  We never have to check for the "special" zone when negating.
         return Univ_Integer (abs Word_Type (Right));
      end if;
   end "abs";

   generic
      with function Word_Op (Left, Right : Word_Type) return Word_Type;
      with function Large_Op (Left, Right : Univ_Large) return Univ_Large;
   function Binary_Univ_Op (Left, Right : Univ_Integer) return Univ_Integer;

   function Binary_Univ_Op (Left, Right : Univ_Integer) return Univ_Integer is
   begin
      if Left = Null_Univ_Integer or else Right = Null_Univ_Integer then
         return Null_Univ_Integer;
      elsif not Is_Large_Univ_Integer (Left)
        and then not Is_Large_Univ_Integer (Right)
      then
         declare
            Result : Word_Type;
         begin
            Result := Word_Op (Word_Type (Left), Word_Type (Right));
            if Result /= Interpreter.Null_Value then
               return From_Word (Result);
            end if;
         exception
            when others =>
               --  Handle overflow by converting both to Large_Integers
               null;
         end;
      end if;

      --  Convert both to Large_Integers and do the operation
      return To_Univ_Integer (Large_Op
        (Univ_To_Univ_Large (Left), Univ_To_Univ_Large (Right)));
   end Binary_Univ_Op;

   function Add_Op is new Binary_Univ_Op ("+", "+");
   pragma Export (Ada, Add_Op, "_psc_univ_add_op");
   function "+" (Left, Right : Univ_Integer) return Univ_Integer
     renames Add_Op;

   function Subtract_Op is new Binary_Univ_Op ("-", "-");
   pragma Export (Ada, Subtract_Op, "_psc_univ_subtract_op");
   function "-" (Left, Right : Univ_Integer) return Univ_Integer
     renames Subtract_Op;

   function Multiply_Op is new Binary_Univ_Op ("*", "*");
   pragma Export (Ada, Multiply_Op, "_psc_univ_multiply_op");
   function "*" (Left, Right : Univ_Integer) return Univ_Integer
     renames Multiply_Op;

   function Divide_Op is new Binary_Univ_Op ("/", "/");
   pragma Export (Ada, Divide_Op, "_psc_univ_divide_op");
   function "/" (Left, Right : Univ_Integer) return Univ_Integer
     renames Divide_Op;

   function Remainder_Op is new Binary_Univ_Op ("rem", "rem");
   pragma Export (Ada, Remainder_Op, "_psc_univ_rem_op");
   function "rem" (Left, Right : Univ_Integer) return Univ_Integer
     renames Remainder_Op;

   function Modulo_Op is new Binary_Univ_Op ("mod", "mod");
   pragma Export (Ada, Modulo_Op, "_psc_univ_mod_op");
   function "mod" (Left, Right : Univ_Integer) return Univ_Integer
     renames Modulo_Op;

   function "**" (Left : Univ_Integer;
                  Right : Natural) return Univ_Integer is
   begin
      if Left = Null_Univ_Integer then
         return Null_Univ_Integer;
      elsif not Is_Large_Univ_Integer (Left)
        and then Right < Word_Type'Size - 1
        and then (abs Left = 2 or else Right <= 15)
      then
         --  Unlikely to overflow
         begin
            return From_Word (Word_Type (Left) ** Right);
         exception
            when others =>
               --  Handle overflow by converting to Univ_Large
               null;
         end;
      end if;

      --  Convert to Univ_Large and do the operation
      return To_Univ_Integer
        (Univ_To_Univ_Large (Left) ** Right);
   end "**";

   function "<" (Left, Right : Univ_Integer) return Boolean is
   begin
      if Left = Right then
         return False;
      elsif not Is_Large_Univ_Integer (Left)
        and then not Is_Large_Univ_Integer (Right)
      then
         return Word_Type (Left) < Word_Type (Right);
      else
         return Univ_To_Univ_Large (Left) < Univ_To_Univ_Large (Right);
      end if;
   end "<";

   function ">" (Left, Right : Univ_Integer) return Boolean is
   begin
      return Right < Left;
   end ">";

   function "<=" (Left, Right : Univ_Integer) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">=" (Left, Right : Univ_Integer) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   procedure Store_Univ_Integer
     (Context : Exec_Context;
      Params : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Univ_Integer) is
   --  Context parameter is ignored
      pragma Unreferenced (Context);
   begin
      Store_Univ_Integer (Params, Offset, Value);
   end Store_Univ_Integer;

   use Ada.Streams;

   --  First stream element ("Head") determines sign, nullness and length.
   --  These constants and functions indicate those properties
   Stream_UI_Zero : constant Stream_Element := 0;
   Stream_UI_Null : constant Stream_Element := 1;

   Stream_UI_Max_Per_Hunk : constant Positive :=
     Positive (Stream_Element'Last / 2);

   Stream_UI_Hunk_Modulus : constant Univ_Large :=
     To_Large (2) ** (Bits_Per_Slice * Stream_UI_Max_Per_Hunk);
   --  When large integer needs more than one hunk in the stream,
   --  this is the amount to multiply a later chunk by before adding
   --  to an earlier chunk.

   Stream_Element_Modulus : constant Univ_Large :=
     To_Large (Stream_Element'Modulus);

   function Stream_UI_Is_Positive (Head : Stream_Element) return Boolean;
   --  Return True if UI is > 0

   function Stream_UI_Is_Negative (Head : Stream_Element) return Boolean;
   --  Return True if UI is < 0

   function Stream_UI_Hunk_Length (Head : Stream_Element) return Natural;
   --  Return number of stream elements in addition to Head in hunk

   function Stream_UI_Is_Positive (Head : Stream_Element) return Boolean is
   begin
      return Head mod 2 = 0;
   end Stream_UI_Is_Positive;

   function Stream_UI_Is_Negative (Head : Stream_Element) return Boolean is
   begin
      return Head mod 2 = 1;
   end Stream_UI_Is_Negative;

   function Stream_UI_Hunk_Length (Head : Stream_Element) return Natural is
   begin
      return Natural (Head / 2);
   end Stream_UI_Hunk_Length;

   procedure Univ_Integer_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Univ_Integer) is
   --  Write out the contents of the Univ_Integer
   --  This starts with a Stream_Element which indicates
   --  whether is null, negative, or positive, and gives a
   --  length in stream elements.  If the length is the max,
   --  then we expect another such Stream_Element after that many stream
   --  elements to give another length.
   begin
      if Item = Zero then
         Write (Stream.all, (1 => Stream_UI_Zero));
         return;  --  All done
      elsif Item = Null_Univ_Integer then
         Write (Stream.all, (1 => Stream_UI_Null));
         return;  --  All done
      end if;

      declare
         Abs_Item : constant Univ_Integer := abs Item;
         Sign_Indicator : constant Stream_Element :=
           Boolean'Pos (Item < 0);
      begin
         if Abs_Item <= Univ_Integer (Stream_Element'Last) then
            --  Handle single-stream-element values as a special case
            Write (Stream.all,
                    (2 * 1 + Sign_Indicator, Stream_Element (Abs_Item)));
            return;  --  All done
         end if;

         --  Write out as a series of "hunks" each containing
         --  at most Stream_Element'Last/2 stream elements.
         declare
            Large_Abs_Item : Univ_Large := Univ_To_Univ_Large (Abs_Item);
            Num_Stream_Elems : Natural :=
              (Num_Slices (Item) * Bits_Per_Slice + Stream_Element'Size - 1) /
                 Stream_Element'Size;
            Hunk_Addon : constant Stream_Element := Sign_Indicator;
         begin
            while Num_Stream_Elems > 0 loop
               declare
                  This_Hunk : constant Natural :=
                    Natural'Min (Stream_UI_Max_Per_Hunk, Num_Stream_Elems);
               begin
                  --  Write out a header element
                  Write (Stream.all,
                    (1 => Stream_Element (This_Hunk) * 2 + Sign_Indicator));
                  for I in 1 .. This_Hunk loop
                     declare
                        Next_Elem : constant Stream_Element := Stream_Element
                          (From_Large
                            (Large_Abs_Item mod Stream_Element_Modulus));
                     begin
                        Write (Stream.all, (1 => Next_Elem));
                        Assign (Large_Abs_Item,
                                Large_Abs_Item / Stream_Element_Modulus);
                     end;
                  end loop;
                  Num_Stream_Elems := Num_Stream_Elems - This_Hunk;
                  if Num_Stream_Elems = 0
                    and then This_Hunk = Stream_UI_Max_Per_Hunk
                  then
                     --  Write an extra head to indicate we are really done
                     Write (Stream.all, (1 => 0));
                  end if;
               end;
            end loop;
         end;
      end;
   end Univ_Integer_Write;

   procedure Univ_Integer_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Univ_Integer) is

      Buf : Stream_Element_Array
        (0 .. Stream_Element_Offset (Stream_UI_Max_Per_Hunk));
      Head : Stream_Element renames Buf (0);
      Last : Stream_Element_Offset;

      function Read_Hunks (Hunk_Head : Stream_Element) return Univ_Large;
      --  Read from stream and convert to Univ_Large;
      --  ignore the sign indicator in Hunk_Head.
      --  Keep reading if is a maximum-length hunk.

      function Read_Hunks (Hunk_Head : Stream_Element) return Univ_Large is
         Len : constant Stream_Element_Offset :=
           Stream_Element_Offset (Stream_UI_Hunk_Length (Hunk_Head));
      begin
         if Len = 0 then
            return Univ_Large_Integers.Variable_Length_Operations.Zero;
         elsif Len = 1 then
            --  Special case for length 1
            Read (Stream.all, Buf (1 .. 1), Last);
            if Last < 1 then
               raise Ada.IO_Exceptions.End_Error;
            end if;
            return To_Large (Word_Type (Buf (1)));
         else
            --  Read in the stream elements, and accumulate in reverse order.
            Read (Stream.all, Buf (1 .. Len), Last);
            if Last < Len then
               raise Ada.IO_Exceptions.End_Error;
            end if;
            declare
               Num_Big_Digits : constant Slice_Index :=
                 (Slice_Index (Len) * Stream_Element'Size +
                    Bits_Per_Slice - 1) / Bits_Per_Slice;
               Large_Result : Univ_Large (Num_Big_Digits);  --  default 0
            begin
               for I in reverse 1 .. Len loop
                  Assign (Large_Result,
                          Large_Result * Stream_Element_Modulus +
                            To_Large (Word_Type (Buf (I))));
               end loop;
               if Natural (Len) < Stream_UI_Max_Per_Hunk then
                  --  We are all done
                  return Large_Result;
               else
                  --  Recurse with following stream hunk and multiply
                  --  returned result by Hunk modulus.
                  Read (Stream.all, Buf (0 .. 0), Last);
                  if Last /= 0 then
                     raise Ada.IO_Exceptions.End_Error;
                  end if;
                  return Large_Result +
                           Read_Hunks (Head) * Stream_UI_Hunk_Modulus;
               end if;
            end;
         end if;
      end Read_Hunks;

   begin  --  Univ_Integer_Read

      Read (Stream.all, Buf (0 .. 0), Last);
      if Last /= 0 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      if Head = Stream_UI_Zero then
         Item := Zero;
      elsif Head = Stream_UI_Null then
         Item := Null_Univ_Integer;
      else
         declare
            Is_Neg : constant Boolean := Stream_UI_Is_Negative (Head);
            Value : constant Univ_Integer :=
              To_Univ_Integer (Read_Hunks (Head));
         begin
            if Is_Neg then
               Item := -Value;
            else
               Item := Value;
            end if;
         end;
      end if;
   end Univ_Integer_Read;

   procedure Dump_Stats is
   --  Dump statistics about Univ_Integer hash table, etc.
   begin
      Large_Integer_Hash_Tables.Dump_Stats;
   end Dump_Stats;

end PSC.Univ_Integers;
