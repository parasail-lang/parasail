------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with System;

with PSC.Languages;
with PSC.Strings;
with PSC.Interpreter; use PSC.Interpreter;
pragma Elaborate (PSC.Interpreter);

package body PSC.Univ_Strings is
   --  Package to represent Univ_String, using U_Strings for
   --  long-lived strings, and using "big" objects for short-lived strings.
   --  Every Univ_String has an associated Stg_Rgn, identified by a
   --  Interpreter.Stg_Rgn_Index

   --  An earlier representation of Large_Null was:
   --       -(2**17 * 2**32 + Rgn_Index*2 + 1)
   --  This was based on a desire to make large nulls distinct from physical
   --  pointers (by being odd) and virtual addresses (by being negative)
   --  and small values (by being in middle of negative range).
   --  Being distinct from small values is optional, but helps in debugging.
   --  NOTE: With this package, we have now shifted large null to be:
   --       -(2**25 * 2**32) + Rgn_Index*2 + 1

   --  For Univ_Strings, we need to "bury" the U_String_Index somewhere,
   --  and ideally, we could also have some special short-string
   --  representations.  Presuming we put the Rgn_Index in the same place,
   --  this makes it easy to adjust Large_Obj_Stg_Rgn_Index to work on
   --  Large_Nulls as well as Univ_Strings.
   --  We need, say, 24 bits for a U_String_Index, plus some bits to
   --  distinguish null, from U_String_Index, from short strings of
   --  various lengths (0-3?).  3 bits is enough for that.
   --  We might expect region indices to grow in size (they are currently
   --  limited to 16 bits), so lets give them 20 bits.  We need 1 bit to make
   --  the value odd, and 1 bit to make it negative, that gives a total of:
   --     24 + 3 + 20 + 1 + 1 = 49 bits.  That still leaves 15 bits to
   --  distinguish from small values (for debugging).
   --  If we give the region index (+ oddness bit) the full lower 32 bits,
   --  then the high 32 bits can hold everything else.
   --  If we give the low 24 bits of that to U_String_Index or
   --  3 chars of a short string, then the high byte (in absolute value)
   --  can be:  2 == null, 3 == U_String_Index, 4-7 == short string
   --  the values 0, 1, 8-127 are reserved for normal "small" values.
   --  In hex, the high bytes are 16#fe# = null, 16#fd# = U_String_Index,
   --  and 16#f9# to 16#fc# = short strings (0..3 characters).
   --  We want to avoid using a high-byte of "0" because we don't want
   --  the whole value to be close to 0, which is clearly a heavily used
   --  part of the overall small-value range.

   --  For the "large" representation of Univ_String, we are currently
   --  going with a Basic_Array and a separate Info word.  The Info word
   --  contains the Hash value, the Length, and an indicator of what is the
   --  component type of the Basic_Array.

   --  type Field is Enum<[#Hash, #Length, #Kind]>
   --     Low 32 bits = hash
   --     Next 24 bits = length (total number of characters)
   --     Next 3 bits select 0 = array of 8-bit chars,
   --                        1 = array of 16-bit chars,
   --                        2 = array of 31-bit chars,
   --                        3 = array of substrs (Max num sub-strs = 16)
   --                        4 = array of substrs (all sub-strs 8*256 chrs)
   --                        5 = array of substrs (all substrs 8*256^2 chrs)
   --                        6 = array of substrs (all substrs 8*256^3 chrs)

   --   Enumeration of kind of array
   type Array_Kind_Enum is
     (Array_8, Array_16, Array_31, Array_Substr,
      Array_2K_Substr, Array_512K_Substr, Array_128M_Substr);

   subtype Array_Substr_Kinds is Array_Kind_Enum
     range Array_Substr .. Array_128M_Substr;

   --   Number of characters per substr
   Array_2K_Multiplier   : constant := 2**11;
   Array_512K_Multiplier : constant := 2**19;
   Array_128M_Multiplier : constant := 2**27;

   --  These will be initialized at most once
   Underlying_Array_Type_Desc : Interpreter.Non_Op_Map_Type_Ptr := null;
   Univ_String_Type_Desc : Interpreter.Non_Op_Map_Type_Ptr := null;
   Array_Substr_Type_Desc : Interpreter.Non_Op_Map_Type_Ptr := null;

   --   Packed representation for Info word in "large" representation
   type Info_Type is record
      Hash : Strings.Hash_Type;
      Len  : Natural range 0 .. 2**24 - 1;
      Kind : Array_Kind_Enum;
      Pad  : Natural range 0 .. 2**5 - 1;
   end record;

   for Info_Type'Bit_Order use System.Low_Order_First;
   for Info_Type'Size use Interpreter.Word_Type'Size;

   for Info_Type use record
      Hash at 0 range  0 .. 31;
      Len  at 4 range  0 .. 23;
      Kind at 4 range 24 .. 26;
      Pad  at 4 range 27 .. 31;
   end record;

   --  Conversion Word_Type to/from Info_Type
   function To_Info is new Ada.Unchecked_Conversion
     (Interpreter.Word_Type, Info_Type);

   function From_Info is new Ada.Unchecked_Conversion
     (Info_Type, Interpreter.Word_Type);

   subtype Univ_String_Index is Interpreter.Indicator_Type;
   use type Univ_String_Index;

   function S2I is new Ada.Unchecked_Conversion (Univ_String, Indicator_Type);

   function String_Indicator (Str : Univ_String) return Indicator_Type;
   --  Return Indicator "buried" in Univ_String
   pragma Inline (String_Indicator);

   U_String_Indicator      : constant Indicator_Type := 3;
   Short_String_Indicator  : constant Indicator_Type := 4;

   U_String_Multiplier     : constant := 2**32;
      --  Amount to multiply U_String_Index to produce Univ_String value

   Short_String_Multiplier : constant := U_String_Multiplier;
      --  Amount to multiply first char of short string to produce Univ_String
   Char_Multiplier         : constant := 2**8;
      --  Additional amount to multiply each subsequent char of short string

   Max_Short_String_Modulus : constant := Char_Multiplier ** Max_Short_String;
      --  Modulus for isolating maximum "short" string.

   Elem_Sizes : constant array (Array_Kind_Enum range Array_8 .. Array_31) of
     Natural := (Array_8 => 8, Array_16 => 16, Array_31 => 31);

   Int8_Per_Word  : constant := Interpreter.Word_Type'Size / 8;
   Int16_Per_Word : constant := Interpreter.Word_Type'Size / 16;
   Int31_Per_Word : constant := Interpreter.Word_Type'Size / 31;

   U_String_Modulus        : constant Univ_String_Index := 2**24;
      --  Modulus for extracting U_String_Index

   procedure Check_Univ_String_Type_Desc;
   --  Make sure Univ_String_Type_Desc is initialized
   pragma Inline (Check_Univ_String_Type_Desc);

   procedure Init_Univ_String_Type_Desc;
   --  Initialize Univ_String_Type_Desc and other type descriptors
   --  (this is not inlined).

   procedure Check_Univ_String_Type_Desc is
   begin
      if Univ_String_Type_Desc = null
        or else Underlying_Array_Type_Desc = null
        or else Array_Substr_Type_Desc = null
      then
         --  Call out-of-line routine to do the work.
         Init_Univ_String_Type_Desc;
      end if;
   end Check_Univ_String_Type_Desc;

   procedure Reset_Univ_String_Type_Desc is
   --  Reset Univ_String_Type_Desc and other type descriptors
   --  as part of switching to a new language.
   begin
      Univ_String_Type_Desc := null;
      Underlying_Array_Type_Desc := null;
      Array_Substr_Type_Desc := null;
   end Reset_Univ_String_Type_Desc;

   procedure Init_Univ_String_Type_Desc is
   --  Initialize Univ_String_Type_Desc and other type descriptors
   begin
      if Univ_String_Type_Desc = null then
         --  Do one-time lookup of Univ_String_Type_Name,
         --  Underlying_Array_Type_Name, and Array_Substr_Type_Name
         Univ_String_Type_Desc :=
           Type_Descriptor_Ops.Get_Type_Desc_By_Name
             (Strings.String_Lookup (Languages.Univ_String_Type_Name));
      end if;

      if Underlying_Array_Type_Desc = null then
         Underlying_Array_Type_Desc :=
           Type_Descriptor_Ops.Get_Type_Desc_By_Name
             (Strings.String_Lookup
               (Languages.Optional_Unsigned_64_Basic_Array_Type_Name));
      end if;

      if Array_Substr_Type_Desc = null then
         Array_Substr_Type_Desc :=
           Type_Descriptor_Ops.Get_Type_Desc_By_Name
             (Strings.String_Lookup
               (Languages.Univ_String_Basic_Array_Type_Name));
      end if;
   end Init_Univ_String_Type_Desc;

   function String_Indicator (Str : Univ_String) return Indicator_Type is
   --  Return Indicator "buried" in Univ_String
   begin
      return Special_Value_Indicator (Word_Type (Str));
   end String_Indicator;

   function Capacity (Str : Univ_String) return Natural;
   --  Return capacity of string, which is guaranteed to be >= Length (Str)
   --  If capacity > Length(Str) then we can squeeze some more characters
   --  into the string.
   --  NOTE: This is only called if Univ_String is using a "large"
   --        representation.

   function Capacity (Str : Univ_String) return Natural is
   begin
      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return 0;

               when U_String_Indicator =>
                  return Length (Str);

               when Short_String_Indicator ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short multi-character string
                  return Max_Short_String;
               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return 0;
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Arr   : constant Word_Type :=
              Fetch_Word (Large_Str, Large_Obj_Header_Size + 0);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
            Str_Kind  : constant Array_Kind_Enum :=
              Str_Info.Kind;
            Str_Len   : constant Natural := Str_Info.Len;

            Arr_Ptr   : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str_Arr);
            Arr_Len   : constant Natural := Natural
              (Word_Type'(Fetch_Word (Arr_Ptr, Large_Obj_Header_Size)));
         begin
            case Str_Kind is
               when Array_8 =>
                  return Arr_Len * Int8_Per_Word;
               when Array_16 =>
                  return Arr_Len * Int16_Per_Word;
               when Array_31 =>
                  return Arr_Len * Int31_Per_Word;
               when Array_2K_Substr   => return 2**19;
               when Array_512K_Substr => return 2**27;
               when Array_128M_Substr => return Integer'Last;
               when Array_Substr      =>
                  --  Find capacity of last non-empty element
                  declare
                     Remaining_Len : Natural := Str_Len;
                  begin
                     for I in 1 .. Arr_Len loop
                        declare
                           Next_Substr : constant Univ_String :=
                             Fetch_Word (Arr_Ptr,
                                         Large_Obj_Header_Size +
                                           Offset_Within_Area (I));
                           Substr_Len  : constant Natural :=
                             Length (Next_Substr);
                        begin
                           if Substr_Len = Remaining_Len then
                              --  This is the last non-empty substr
                              return Capacity (Next_Substr) +
                                Str_Len - Substr_Len;
                           end if;

                           Remaining_Len := Remaining_Len - Substr_Len;
                        end;
                     end loop;
                     --  TBD: Shouldn't get here?
                     return Str_Len;
                  end;
            end case;
         end;
      end if;
   end Capacity;

   -------------------------------
   -- Get_Univ_String_Type_Desc --
   -------------------------------

   function Get_Univ_String_Type_Desc return Interpreter.Non_Op_Map_Type_Ptr is
   --  Return Univ_String type descriptor
   begin
      --  Initialize if necessary
      Check_Univ_String_Type_Desc;

      pragma Assert (Univ_String_Type_Desc /= null);
         --  Should be non-null by now

      return Univ_String_Type_Desc;
   end Get_Univ_String_Type_Desc;

   function Null_Of_Same_Rgn (Str : Univ_String)
     return Interpreter.Word_Type is
   --  Return Null value for Univ_String from same region as Str
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Interpreter.Null_Value;
      end if;

      return Null_For_Stg_Rgn (Stg_Rgn_Of_Large_Obj (Word_Type (Str)));

   end Null_Of_Same_Rgn;

   function Is_Null (Str : Univ_String) return Boolean is
   --  Return True if Str is a null value of Univ_String type.
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Word_Type (Str) = Null_Value;
      end if;

      return Is_Large_Null (Str);
   end Is_Null;

   function Empty_Univ_String (Rgn : Interpreter.Stg_Rgn_Index)
     return Univ_String is
   --  Return an empty univ string for given region
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Univ_String (Strings.Empty_U_String_Index);
      end if;

      return (Univ_String (Rgn) * 2 + Univ_String'(1) -
        Univ_String (Short_String_Indicator * Indicator_Multiplier));
   end Empty_Univ_String;

   function Equal (Left, Right : Univ_String) return Boolean is
   --  Indicate whether two strings are equal (ignore Stg_Rgn_Index)
   --  TBD: Handle wide strings?
      use type Strings.Hash_Type;
   begin
      if Length (Left) /= Length (Right) then
         return False;
      elsif Hash (Left) /= Hash (Right) then
         return False;
      else
         return To_Wide_Wide_String (Left) /= To_Wide_Wide_String (Right);
      end if;
   end Equal;

   function Compare (Left, Right : Univ_String) return Interpreter.Ordering is
   --  Implementation of "=?" for Univ_String
   --  TBD: Handle wide strings?
      Left_Str  : constant Wide_Wide_String := To_Wide_Wide_String (Left);
      Right_Str : constant Wide_Wide_String := To_Wide_Wide_String (Right);
   begin
      if Left_Str < Right_Str then
         return Interpreter.Less;
      elsif Left_Str > Right_Str then
         return Interpreter.Greater;
      else
         return Interpreter.Equal;
      end if;
   end Compare;

   --  Convert to/from U_String
   function To_U_String (Str : Univ_String) return Strings.U_String is
      --  This will create a new U_String if necessary
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Strings.To_U_String (Strings.U_String_Index (Str));
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  Check the indicator
         if String_Indicator (Str) = U_String_Indicator then
            --  We already have a U_String; isolate it
            declare
               Index : constant Strings.U_String_Index :=
                 Strings.U_String_Index
                   (S2I (Str) / U_String_Multiplier mod U_String_Modulus);
            begin
               --  Convert U_String_Index to a U_String and return it.
               return Strings.To_U_String (Index);
            end;
         end if;
      end if;
      --  Need to create a new U_String; convert to string first.
      return Strings.String_Lookup (To_String (Str));
   end To_U_String;

   function From_U_String
              (Str : Strings.U_String;
               Null_For_Rgn : Interpreter.Word_Type) return Univ_String is
   --  Return Univ_String given a U_String and null identifying region
      use type Strings.U_String;
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Univ_String (Strings.Index (Str));
      end if;

      if Str = Strings.Null_U_String then
         return Univ_String (Null_For_Rgn);
      end if;

      declare
         Rgn : constant Interpreter.Stg_Rgn_Index :=
           Interpreter.Large_Obj_Stg_Rgn_Index (Null_For_Rgn);
      begin
         return (Univ_String (Rgn) * 2 + Univ_String'(1) -
           Univ_String (U_String_Indicator * Indicator_Multiplier) +
           Univ_String (Strings.Index (Str)) * U_String_Multiplier);
      end;
   end From_U_String;

   --  Convert to/from regular String (TBD: Wide_String vs. UTF-8?)
   function To_String (Str : Univ_String) return String is
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         if Word_Type (Str) = Null_Value then
            return "null"; --  TBD: or die?
         else
            return Strings.To_String (Strings.To_U_String
              (Strings.U_String_Index (Str)));
         end if;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return "null";  --  TBD: Or die?

               when U_String_Indicator =>
                  return Strings.To_String (To_U_String (Str));

               when Short_String_Indicator =>
                  --  Empty string
                  return "";

               when Short_String_Indicator + 1 =>
                  --  Single character string
                  return (1 => Character'Val
                                 (S2I (Str) / Short_String_Multiplier mod
                                       Char_Multiplier));

               when Short_String_Indicator + 2 ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short multi-character string
                  declare
                     Len : constant Natural :=
                       Natural (Indicator - Short_String_Indicator);
                     Result : String (1 .. Len);
                     Chars : Univ_String_Index :=
                       S2I (Str) / Short_String_Multiplier mod
                                  Max_Short_String_Modulus;
                  begin
                     --  Isolate each character
                     for I in 1 .. Len loop
                        Result (I) :=
                          Character'Val (Chars mod Char_Multiplier);
                        Chars := Chars / Char_Multiplier;
                     end loop;
                     return Result;
                  end;
               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return "";
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Arr   : constant Word_Type :=
              Fetch_Word (Large_Str, Large_Obj_Header_Size + 0);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
            Str_Kind  : constant Array_Kind_Enum :=
              Str_Info.Kind;
            Str_Len   : constant Natural := Str_Info.Len;

            Arr_Ptr   : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str_Arr);
            Arr_Len   : constant Natural := Natural
              (Word_Type'(Fetch_Word (Arr_Ptr, Large_Obj_Header_Size)));

            Result    : String (1 .. Str_Len);
            Next_Word : Word_Type;
         begin
            case Str_Kind is
               when Array_8 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int8_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2 ** 8;
                           begin
                              Result (Index + I) :=
                                Character'Val (Next_Elem);
                              Next_Word :=
                                 (Next_Word - Next_Elem) / 2 ** 8;
                           end;
                        end loop;
                        Index := Index + Int8_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_16 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int16_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2**16;
                           begin
                              Result (Index + I) :=
                                Character'Val (Next_Elem rem 2**8);
                                 --  TBD: Handle wide chars
                              Next_Word := (Next_Word - Next_Elem) / 2**16;
                           end;
                        end loop;
                        Index := Index + Int16_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_31 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int31_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2**31;
                           begin
                              Result (Index + I) :=
                                Character'Val (Next_Elem rem 2**8);
                                 --  TBD: Handle wide chars
                              Next_Word := (Next_Word - Next_Elem) / 2**31;
                           end;
                        end loop;
                        Index := Index + Int31_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_Substr_Kinds =>
                  declare
                     Index : Positive := 1;
                  begin
                     for I in 1 .. Arr_Len loop
                        declare
                           Next_Substr : constant Univ_String :=
                             Fetch_Word (Arr_Ptr,
                                         Large_Obj_Header_Size +
                                           Offset_Within_Area (I));
                           Substr_Len  : constant Natural :=
                             Length (Next_Substr);
                        begin
                           exit when Substr_Len = 0;
                           Result (Index .. Index + Substr_Len - 1) :=
                             To_String (Next_Substr);
                           Index := Index + Substr_Len;
                        end;
                     end loop;
                  end;
            end case;
            return Result;
         end;
      end if;
   end To_String;

   function From_String
     (Str          : String;
      Null_For_Rgn : Interpreter.Word_Type;
      Server_Index : Interpreter.Thread_Server_Index :=
        Interpreter.Current_Server_Index;
      Minimum_Len  : Natural := 0)
     return Univ_String is
   --  Convert to Univ_String from regular String (TBD: Wide_String vs. UTF-8?)
   --  given null for region and server index.
   --  Capacity of returned string will be at least Minimum_Len.
      Str_Len : constant Natural := Str'Length;
      Len     : constant Natural := Natural'Max (Str_Len, Minimum_Len);

      use Interpreter;
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Univ_String (Strings.Index (Strings.String_Lookup (Str)));
      end if;

      pragma Assert (Is_Large_Null (Null_For_Rgn));

      if Len <= Max_Short_String then
         --  Can be represented as a short string
         declare
            Rgn : constant Stg_Rgn_Index :=
              Large_Obj_Stg_Rgn_Index (Null_For_Rgn);
            Base : constant Univ_String :=
              Univ_String (Rgn) * 2 + Univ_String'(1) -
                 Univ_String
                   ((Short_String_Indicator + Indicator_Type (Str_Len)) *
                      Indicator_Multiplier);
         begin
            --  Incorporate the characters of the string
            case Str_Len is
               when 0 =>
                  return Base;
               when 1 =>
                  return Base + Character'Pos (Str (Str'First)) *
                                  Univ_String'(U_String_Multiplier);
               when others =>
                  --  Sum up the characters and add to Base
                  declare
                     Sum : Univ_String := 0;
                  begin
                     for I in reverse 1 .. Natural (Str_Len) loop
                        Sum := Sum * Char_Multiplier +
                          Univ_String'(Character'Pos
                            (Str (Str'First + I - 1)));
                     end loop;
                     return Base + Sum * U_String_Multiplier;
                  end;
            end case;
         end;
      else
         if Underlying_Array_Type_Desc = null
           or else Univ_String_Type_Desc = null
         then
            --  As a fall back we just create a U_String
            return From_U_String (Strings.String_Lookup (Str), Null_For_Rgn);
         end if;

         pragma Assert (not Univ_String_Type_Desc.Is_Small);

         --  Create a large object
         declare
            Word_Len : constant Natural :=   --  TBD: Handle wide characters
              (Len + Int8_Per_Word - 1) / Int8_Per_Word;

            Rgn_Ptr : constant Stg_Rgn_Ptr :=
              Stg_Rgn_Of_Large_Obj (Null_For_Rgn);
            Arr : constant Word_Type := Create_Basic_Array_Obj
              (Array_Type_Desc => Underlying_Array_Type_Desc,
               Array_Len       => Word_Len,
               Stg_Rgn         => Rgn_Ptr,
               Server_Index    => Server_Index,
               Init_Elements   => Len > Str_Len,
               Element_Value   => 0);
            Arr_Ptr : constant Word_Ptr := Virtual_To_Physical_Address (Arr);

            Univ_Str_Obj : constant Word_Type := Create_Large_Obj
              (Type_Desc    => Univ_String_Type_Desc,
               Stg_Rgn      => Rgn_Ptr,
               Server_Index => Server_Index);
            Univ_Str_Ptr : constant Word_Ptr :=
              Virtual_To_Physical_Address (Univ_Str_Obj);

            Index : Positive := Str'First;
            Next_Word : Word_Type;
            Str_Info : constant Info_Type :=
              (Hash => Strings.Hash_String (Str),
               Len  => Str_Len,
               Kind => Array_8,
               Pad  => 0);
         begin
            --  Initialize the basic packed array
            for I in 1 .. Word_Len loop
               Next_Word := 0;
               --  Build up next word of characters, in reverse
               for J in reverse 0 ..
                 Natural'Min (Int8_Per_Word - 1, Str'Last - Index)
               loop
                  if Next_Word >=
                    2 ** (Word_Type'Size - 8 - 1)
                  then
                     --  Will overflow; make it negative
                     Next_Word := Next_Word -
                       2 ** (Word_Type'Size - 8);
                     pragma Assert (Next_Word < 0);
                  end if;

                  Next_Word := Next_Word * (2 ** 8) +
                    Word_Type (Character'Pos (Str (Index + J)));
               end loop;
               --  Store next word
               Store_Word (Arr_Ptr, Large_Obj_Header_Size +
                 Offset_Within_Area (I), Next_Word);
               --  Move on to next word's worth of characters
               Index := Index + Int8_Per_Word;
               exit when Index > Str'Last;
            end loop;

            --  Initialize the large representation for the Univ_String
            Store_Word (Univ_Str_Ptr, Large_Obj_Header_Size + 0, Arr);
            Store_Word (Univ_Str_Ptr, Large_Obj_Header_Size + 1,
              From_Info (Str_Info));

            return Univ_String (Univ_Str_Obj);
         end;
      end if;
   end From_String;

   --  Convert to/from regular Wide_Wide_String
   function To_Wide_Wide_String (Str : Univ_String) return Wide_Wide_String is
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         if Word_Type (Str) = Null_Value then
            return "null"; --  TBD: or die?
         else
            return Strings.To_Wide_Wide_String (Strings.To_U_String
              (Strings.U_String_Index (Str)));
         end if;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return "null";  --  TBD: Or die?

               when U_String_Indicator =>
                  return Strings.To_Wide_Wide_String (To_U_String (Str));

               when Short_String_Indicator =>
                  --  Empty string
                  return "";

               when Short_String_Indicator + 1 =>
                  --  Single character string
                  return (1 => Wide_Wide_Character'Val
                                 (S2I (Str) / Short_String_Multiplier mod
                                       Char_Multiplier));

               when Short_String_Indicator + 2 ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short multi-character string
                  declare
                     Len : constant Natural :=
                       Natural (Indicator - Short_String_Indicator);
                     Result : Wide_Wide_String (1 .. Len);
                     Chars : Univ_String_Index :=
                       S2I (Str) / Short_String_Multiplier mod
                                  Max_Short_String_Modulus;
                  begin
                     --  Isolate each character
                     for I in 1 .. Len loop
                        Result (I) :=
                          Wide_Wide_Character'Val (Chars mod Char_Multiplier);
                        Chars := Chars / Char_Multiplier;
                     end loop;
                     return Result;
                  end;
               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return "";
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Arr   : constant Word_Type :=
              Fetch_Word (Large_Str, Large_Obj_Header_Size + 0);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
            Str_Kind  : constant Array_Kind_Enum :=
              Str_Info.Kind;
            Str_Len   : constant Natural := Str_Info.Len;

            Arr_Ptr   : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str_Arr);
            Arr_Len   : constant Natural := Natural
              (Word_Type'(Fetch_Word (Arr_Ptr, Large_Obj_Header_Size)));

            Result    : Wide_Wide_String (1 .. Str_Len);
            Next_Word : Word_Type;
         begin
            case Str_Kind is
               when Array_8 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int8_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2 ** 8;
                           begin
                              Result (Index + I) :=
                                Wide_Wide_Character'Val (Next_Elem);
                              Next_Word :=
                                 (Next_Word - Next_Elem) / 2 ** 8;
                           end;
                        end loop;
                        Index := Index + Int8_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_16 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int16_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2**16;
                           begin
                              Result (Index + I) :=
                                Wide_Wide_Character'Val (Next_Elem);
                              Next_Word := (Next_Word - Next_Elem) / 2**16;
                           end;
                        end loop;
                        Index := Index + Int16_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_31 =>
                  declare
                     Index : Positive := 1;
                  begin
                     for Word_Index in 1 .. Arr_Len loop
                        Next_Word := Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                        for I in 0 ..
                          Natural'Min (Int31_Per_Word - 1,
                                       Str_Len - Index)
                        loop
                           declare
                              Next_Elem : constant Word_Type :=
                                Next_Word mod 2**31;
                           begin
                              Result (Index + I) :=
                                Wide_Wide_Character'Val (Next_Elem);
                              Next_Word := (Next_Word - Next_Elem) / 2**31;
                           end;
                        end loop;
                        Index := Index + Int31_Per_Word;
                        exit when Index > Str_Len;
                     end loop;
                  end;
               when Array_Substr_Kinds =>
                  declare
                     Index : Positive := 1;
                  begin
                     for I in 1 .. Arr_Len loop
                        declare
                           Next_Substr : constant Univ_String :=
                             Fetch_Word (Arr_Ptr,
                                         Large_Obj_Header_Size +
                                           Offset_Within_Area (I));
                           Substr_Len  : constant Natural :=
                             Length (Next_Substr);
                        begin
                           exit when Substr_Len = 0;
                           Result (Index .. Index + Substr_Len - 1) :=
                             To_Wide_Wide_String (Next_Substr);
                           Index := Index + Substr_Len;
                        end;
                     end loop;
                  end;
            end case;
            return Result;
         end;
      end if;
   end To_Wide_Wide_String;

   function From_Wide_Wide_String
     (Str          : Wide_Wide_String;
      Null_For_Rgn : Interpreter.Word_Type;
      Server_Index : Interpreter.Thread_Server_Index :=
        Interpreter.Current_Server_Index;
      Minimum_Len  : Natural := 0)
     return Univ_String is
   --  Convert to Univ_String from Wide_Wide_String
   --  given null for region and server index.
   --  Capacity of returned string will be at least Minimum_Len.
      Str_Len : constant Natural := Str'Length;
      Len     : constant Natural := Natural'Max (Str_Len, Minimum_Len);
      Max_Ch  : constant Wide_Wide_Character :=
        Strings.Find_Max_Wide_Wide_Char (Str);
      Max_Pos : constant Word_Type := Wide_Wide_Character'Pos (Max_Ch);

      use Interpreter;
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         return Univ_String
           (Strings.Index (Strings.Wide_Wide_String_Lookup (Str)));
      end if;

      pragma Assert (Is_Large_Null (Null_For_Rgn));

      if Len <= Max_Short_String
        and then Max_Pos <= 2 ** 8 - 1
      then
         --  Can be represented as a short string
         declare
            Rgn : constant Stg_Rgn_Index :=
              Large_Obj_Stg_Rgn_Index (Null_For_Rgn);
            Base : constant Univ_String :=
              Univ_String (Rgn) * 2 + Univ_String'(1) -
                 Univ_String
                   ((Short_String_Indicator + Indicator_Type (Str_Len)) *
                      Indicator_Multiplier);
         begin
            --  Incorporate the characters of the string
            case Str_Len is
               when 0 =>
                  return Base;
               when 1 =>
                  return Base + Wide_Wide_Character'Pos (Str (Str'First)) *
                                  Univ_String'(U_String_Multiplier);
               when others =>
                  --  Sum up the characters and add to Base
                  declare
                     Sum : Univ_String := 0;
                  begin
                     for I in reverse 1 .. Natural (Str_Len) loop
                        Sum := Sum * Char_Multiplier +
                          Univ_String'(Wide_Wide_Character'Pos
                            (Str (Str'First + I - 1)));
                     end loop;
                     return Base + Sum * U_String_Multiplier;
                  end;
            end case;
         end;
      else
         if Underlying_Array_Type_Desc = null
           or else Univ_String_Type_Desc = null
         then
            --  As a fall back we just create a U_String
            return From_U_String
                     (Strings.Wide_Wide_String_Lookup (Str), Null_For_Rgn);
         end if;

         pragma Assert (not Univ_String_Type_Desc.Is_Small);

         --  Create a large object, of appropriate array kind
         declare
            Array_Kind : constant Array_Kind_Enum range Array_8 .. Array_31 :=
              (case Max_Pos is
                  when    0 .. 2**8 - 1  => Array_8,
                  when 2**8 .. 2**16 - 1 => Array_16,
                  when others            => Array_31);

            Elem_Size : constant Natural := Elem_Sizes (Array_Kind);

            Elems_Per_Word : constant Natural := Word_Type'Size / Elem_Size;
            --  tbd:stt pragma Assert (Word_Type'Size rem Elem_Size = 0);
               --  tbd:stt  Should be no remainder

            Word_Len : constant Natural :=
              (Len + Elems_Per_Word - 1) / Elems_Per_Word;

            Rgn_Ptr : constant Stg_Rgn_Ptr :=
              Stg_Rgn_Of_Large_Obj (Null_For_Rgn);
            Arr : constant Word_Type := Create_Basic_Array_Obj
              (Array_Type_Desc => Underlying_Array_Type_Desc,
               Array_Len       => Word_Len,
               Stg_Rgn         => Rgn_Ptr,
               Server_Index    => Server_Index,
               Init_Elements   => Len > Str_Len,
               Element_Value   => 0);
            Arr_Ptr : constant Word_Ptr :=
              Virtual_To_Physical_Address (Arr);

            Univ_Str_Obj : constant Word_Type := Create_Large_Obj
              (Type_Desc    => Univ_String_Type_Desc,
               Stg_Rgn      => Rgn_Ptr,
               Server_Index => Server_Index);
            Univ_Str_Ptr : constant Word_Ptr :=
              Virtual_To_Physical_Address (Univ_Str_Obj);

            Index : Positive := Str'First;
            Next_Word : Word_Type;
            Str_Info : constant Info_Type :=
              (Hash => Strings.Hash_Wide_Wide_String (Str),
               Len  => Str_Len,
               Kind => Array_Kind,
               Pad  => 0);
         begin
            --  Initialize the basic packed array
            for I in 1 .. Word_Len loop
               Next_Word := 0;
               --  Build up next word of characters, in reverse
               for J in reverse 0 ..
                 Natural'Min (Elems_Per_Word - 1, Str'Last - Index)
               loop
                  if Next_Word >=
                    2 ** (Word_Type'Size - Elem_Size - 1)
                  then
                     --  Will overflow; make it negative
                     Next_Word := Next_Word -
                       2 ** (Word_Type'Size - Elem_Size);
                     pragma Assert (Next_Word < 0);
                  end if;

                  Next_Word := Next_Word * (2 ** Elem_Size) +
                    Word_Type (Wide_Wide_Character'Pos (Str (Index + J)));
               end loop;
               --  Store next word
               Store_Word (Arr_Ptr, Large_Obj_Header_Size +
                 Offset_Within_Area (I), Next_Word);
               --  Move on to next word's worth of characters
               Index := Index + Elems_Per_Word;
               exit when Index > Str'Last;
            end loop;

            --  Initialize the large representation for the Univ_String
            Store_Word (Univ_Str_Ptr, Large_Obj_Header_Size + 0, Arr);
            Store_Word (Univ_Str_Ptr, Large_Obj_Header_Size + 1,
              From_Info (Str_Info));

            return Univ_String (Univ_Str_Obj);
         end;
      end if;
   end From_Wide_Wide_String;

   --  Convert to/from Word_Type; Stg_Rgn_Index is part of Word_Type rep.
   function To_Word_Type (Str : Univ_String) return Interpreter.Word_Type is
   begin
      --  One is derived from the other so direct conversion is possible.
      return Interpreter.Word_Type (Str);
   end To_Word_Type;

   function From_Word_Type (Word : Interpreter.Word_Type) return Univ_String is
   begin
      --  One is derived from the other so direct conversion is possible.
      return Univ_String (Word);
   end From_Word_Type;

   function Length (Str : Univ_String) return Natural is
   --  Return length of string; Length (Null_Univ_String) returns 0.
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         if Word_Type (Str) = Null_Value then
            return 0;
         else
            return Strings.Length (Strings.To_U_String
              (Strings.U_String_Index (Str)));
         end if;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return 0;

               when U_String_Indicator =>
                  return Strings.Length (To_U_String (Str));

               when Short_String_Indicator ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short string
                  return Natural (Indicator - Short_String_Indicator);

               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return 0;
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
         begin
            return Str_Info.Len;
         end;
      end if;
   end Length;

   function Hash (Str : Univ_String) return Strings.Hash_Type is
   --  Get a hash for the univ string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         if Word_Type (Str) = Null_Value then
            return 0;
         else
            return Strings.Hash (Strings.To_U_String
              (Strings.U_String_Index (Str)));
         end if;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return 0;

               when U_String_Indicator =>
                  return Strings.Hash (To_U_String (Str));

               when Short_String_Indicator ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short string
                  return Strings.Hash_String (To_String (Str));

               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return 0;
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
         begin
            return Str_Info.Hash;
         end;
      end if;
   end Hash;

   function Case_Insensitive_Equal (Left, Right : Univ_String)
     return Boolean is
   --  Return True if Left and Right are equal ignoring case.
      use Ada.Characters.Handling;
      use type Strings.Hash_Type;
   begin
      if Length (Left) /= Length (Right) then
         return False;
      elsif Hash (Left) /= Hash (Right) then
         --  Hash is case insensitive
         return False;
      else
         declare
            Left_Str  : constant String := To_String (Left);
            Right_Str : constant String := To_String (Right);
         begin
            return Left_Str = Right_Str
              or else To_Lower (Left_Str) = To_Lower (Right_Str);
         end;
      end if;
   end Case_Insensitive_Equal;

   procedure Assign_Concat_String
     (Str_Ptr      : Word_Ptr;
      New_String   : String;
      Server_Index : Interpreter.Thread_Server_Index) is
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr
      Str_Word  : constant Word_Type   := Fetch_Nonnull_Word (Str_Ptr, 0);
      Str       : constant Univ_String := Univ_String (Str_Word);
      Added_Len : constant Natural     := New_String'Length;
      Is_Special : Boolean := False;
   begin
      if Added_Len = 0 then
         --  A No-Op
         return;
      end if;

      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         Store_Word (Str_Ptr, 0, Word_Type
           (Strings.Index (Strings.String_Lookup
             (Strings.To_String (Strings.To_U_String
                (Strings.U_String_Index (Str_Word))) & New_String))));
         return;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  Fall through to fall back of creating a new string.
         Is_Special := True;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Arr   : constant Word_Type :=
              Fetch_Word (Large_Str, Large_Obj_Header_Size + 0);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
            Str_Kind  : constant Array_Kind_Enum :=
              Str_Info.Kind;
            Str_Len   : constant Natural := Str_Info.Len;
            Str_Hash  : constant Strings.Hash_Type := Str_Info.Hash;
            Total_Len : constant Natural := Str_Len + Added_Len;

            Arr_Ptr   : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str_Arr);
            Arr_Len   : constant Natural := Natural
              (Word_Type'(Fetch_Word (Arr_Ptr, Large_Obj_Header_Size)));

         begin
            case Str_Kind is
            when Array_8 =>
               declare
                  Limit : constant Natural := Arr_Len * Int8_Per_Word;
                  Space : constant Natural := Limit - Str_Len;
                  First_Part_Hash : Strings.Hash_Type := Str_Hash;
               begin
                  if Space > 0 then
                     --  Additional characters fit in the current array
                     declare
                        Index_In_New_String : Positive := New_String'First;
                        Index_In_Result : Positive := Str_Len + 1;
                        Len_In_Space : constant Positive :=
                          Positive'Min (Space, Added_Len);
                        End_Index  : constant Positive :=
                          New_String'First + Len_In_Space - 1;
                        Next_Word  : Word_Type := 0;
                     begin
                        --  Add new characters into existing array
                        for New_Index in New_String'First .. End_Index loop
                           declare
                              Chr : constant Word_Type :=
                                Character'Pos (New_String (New_Index));
                              Word_Index : constant Positive := 1 +
                                (Index_In_Result - 1) / Int8_Per_Word;
                              Char_Offset_In_Word : constant Natural :=
                                (Index_In_Result - 1) mod Int8_Per_Word;
                              Shift_Amount : constant Natural :=
                                Char_Offset_In_Word * 8;
                              Shifted_Chr : Word_Type;
                           begin
                              if New_Index = New_String'First
                                and then Char_Offset_In_Word /= 0
                              then
                                 --  Read in first word to update
                                 Next_Word := Fetch_Word (Arr_Ptr,
                                   Large_Obj_Header_Size +
                                     Offset_Within_Area (Word_Index));
                              end if;
                              if Char_Offset_In_Word = Int8_Per_Word - 1
                                and then Chr >= 2 ** (8 - 1)
                              then
                                 --  Shift will overflow; negate
                                 Shifted_Chr := (Chr - 2 ** 8) *
                                   2 ** Shift_Amount;
                              else
                                 Shifted_Chr := Chr * 2 ** Shift_Amount;
                              end if;
                              --  Add shifted char to Next_Word
                              Next_Word := Next_Word + Shifted_Chr;

                              if Char_Offset_In_Word = Int8_Per_Word - 1
                                or else New_Index = New_String'Last
                              then
                                 --  Write out next word
                                 Store_Word (Arr_Ptr,
                                   Large_Obj_Header_Size +
                                     Offset_Within_Area (Word_Index),
                                   Next_Word);

                                 --  Re-initialize Next_Word
                                 Next_Word := 0;
                              end if;
                              Index_In_Result := Index_In_Result + 1;
                           end;
                        end loop;
                        --  Update the "hash" value and length
                        First_Part_Hash := Strings.Hash_String
                          (New_String (New_String'First .. End_Index),
                           Prefix => Str_Hash);
                        Store_Word (Large_Str, Large_Obj_Header_Size + 1,
                          From_Info
                            ((Hash => First_Part_Hash,
                              Len  => Str_Len + Len_In_Space,
                              Kind => Array_8,
                              Pad  => 0)));
                     end;
                  end if;
                  if Space < Added_Len then
                     --  Some characters didn't fit
                     --  Convert into array of substrings
                     --  and make new substring effectively double the
                     --  amount of space in the Univ_String, presuming
                     --  it will keep growing.
                     declare
                        Rgn_Ptr : constant Stg_Rgn_Ptr :=
                          Stg_Rgn_Of_Large_Obj (Str);
                        Addon_Arr_Len : constant Natural := Natural'Max
                          (Arr_Len,
                           (Added_Len - Space + Int8_Per_Word - 1) /
                             Int8_Per_Word);
                              --  At least double the size
                        Addon_Char_Arr : constant Word_Type :=
                          Create_Basic_Array_Obj
                            (Array_Type_Desc => Underlying_Array_Type_Desc,
                             Array_Len       => Addon_Arr_Len,
                             Stg_Rgn         => Rgn_Ptr,
                             Server_Index    => Server_Index,
                             Init_Elements   => True,
                             Element_Value   => 0);

                        Addon_Univ_Str_Obj : constant Word_Type :=
                          Create_Large_Obj
                            (Type_Desc    => Univ_String_Type_Desc,
                             Stg_Rgn      => Rgn_Ptr,
                             Server_Index => Server_Index);
                        Addon_Univ_Str_Ptr : constant Word_Ptr :=
                          Virtual_To_Physical_Address (Addon_Univ_Str_Obj);
                        Addon_Info         : constant Info_Type :=
                          (Hash => 0,
                           Len  => 0,
                           Kind => Array_8,
                           Pad  => 0);

                        New_Substr_Arr : constant Word_Type :=
                          Create_Basic_Array_Obj
                            (Array_Type_Desc => Array_Substr_Type_Desc,
                             Array_Len       => 2,
                             Stg_Rgn         => Rgn_Ptr,
                             Server_Index    => Server_Index);
                        New_Substr_Arr_Ptr : constant Word_Ptr :=
                          Virtual_To_Physical_Address (New_Substr_Arr);

                        Total_Univ_Str_Obj : constant Word_Type :=
                          Create_Large_Obj
                            (Type_Desc    => Univ_String_Type_Desc,
                             Stg_Rgn      => Rgn_Ptr,
                             Server_Index => Server_Index);
                        Total_Univ_Str_Ptr : constant Word_Ptr :=
                          Virtual_To_Physical_Address (Total_Univ_Str_Obj);
                        Total_Info         : constant Info_Type :=
                          (Hash => First_Part_Hash,
                           Len  => Limit,
                           Kind => Array_Substr,
                           Pad  => 0);
                     begin
                        --  Fill in contents of Addon_Univ_Str
                        Store_Word (Addon_Univ_Str_Ptr,
                          Large_Obj_Header_Size + 0,
                          Addon_Char_Arr);
                        Store_Word (Addon_Univ_Str_Ptr,
                          Large_Obj_Header_Size + 1,
                          From_Info (Addon_Info));

                        --  Fill in contents of New_Substr_Arr
                        Store_Word (New_Substr_Arr_Ptr,
                          Large_Obj_Header_Size + 1,
                          Str);
                        Store_Word (New_Substr_Arr_Ptr,
                          Large_Obj_Header_Size + 2,
                          Addon_Univ_Str_Obj);

                        --  Fill in contents of Total_Univ_Str
                        Store_Word (Total_Univ_Str_Ptr,
                          Large_Obj_Header_Size + 0,
                          New_Substr_Arr);
                        Store_Word (Total_Univ_Str_Ptr,
                          Large_Obj_Header_Size + 1,
                          From_Info (Total_Info));

                        --  Replace Str with Total_Univ_Str
                        Store_Word (Str_Ptr, 0, Total_Univ_Str_Obj);

                        --  Recurse to fill in characters from remainder
                        --  of New_String
                        Assign_Concat_String (Str_Ptr,
                          New_String
                            (New_String'First + Space .. New_String'Last),
                          Server_Index);
                     end;
                  end if;
                  return;  --  All done  --
               end;
            when Array_16 | Array_31 |
              Array_2K_Substr | Array_512K_Substr | Array_128M_Substr =>
               --  Fall through to create a new string
               --  TBD: Handle these more intelligently
               null;

            when Array_Substr =>
               --  Try to concatenate onto last non-empty substr.
               --  Expand the array if necessary.
               declare
                  Remaining_Len : Natural := Str_Len;
                  Needed_Len    : Natural := Added_Len;
                  Start_Index   : Positive := New_String'First;
               begin
                  for I in 1 .. Arr_Len loop
                     declare
                        Next_Substr : constant Univ_String :=
                          Fetch_Word (Arr_Ptr,
                                      Large_Obj_Header_Size +
                                        Offset_Within_Area (I));
                        Substr_Len  : constant Natural :=
                          Length (Next_Substr);
                     begin
                        if Substr_Len = Remaining_Len then
                           --  We have reached the end of the non-empty
                           --  substrings.  Start looking for spare capacity.
                           declare
                              Cap : constant Natural := Capacity (Next_Substr);
                              Space : constant Natural := Cap - Substr_Len;
                              Substr_Ptr : constant Word_Ptr :=
                                Add (Arr_Ptr, Large_Obj_Header_Size +
                                  Offset_Within_Area (I));
                              End_Index : constant Natural :=
                                Natural'Min (New_String'Last,
                                             Start_Index + Space - 1);
                           begin
                              if Space > 0 then
                                 --  This has some room.
                                 --  Recurse to add to this substring.
                                 Assign_Concat_String (Substr_Ptr,
                                   New_String (Start_Index .. End_Index),
                                   Server_Index);
                                 Start_Index := End_Index + 1;
                                 Needed_Len := New_String'Last - End_Index;

                              elsif Substr_Len = 0 then
                                 --  This is a null entry; fill it.
                                 --  Create a new substring of needed size.
                                 Store_Word (Substr_Ptr, 0,
                                   Word_Type (From_String
                                     (New_String
                                       (Start_Index .. New_String'Last),
                                     Null_Of_Same_Rgn (Str),
                                     Server_Index,
                                     Minimum_Len => Str_Len +
                                       Added_Len - Needed_Len)));
                                       --  Minimum ensures that overall
                                       --  capacity at least doubles.
                                 Needed_Len := 0;
                                 Start_Index := New_String'Last + 1;
                              end if;
                              exit when Needed_Len = 0;
                           end;
                        end if;
                        Remaining_Len := Remaining_Len - Substr_Len;
                     end;
                  end loop;
                  if Needed_Len < Added_Len then
                     --  We increased the length of the String
                     --  Update the "hash" value and length
                     declare
                        First_Part_Hash : constant Strings.Hash_Type :=
                          Strings.Hash_String
                            (New_String (New_String'First .. Start_Index - 1),
                             Prefix => Str_Hash);
                     begin
                        Store_Word (Large_Str, Large_Obj_Header_Size + 1,
                          From_Info
                            ((Hash => First_Part_Hash,
                              Len  => Str_Len + Added_Len - Needed_Len,
                              Kind => Array_Substr,
                              Pad  => 0)));
                     end;
                  end if;
                  if Needed_Len > 0 then
                     --  Need yet more space; double the size of the substr
                     --  array.
                     declare
                        Null_Val : constant Word_Type :=
                          Null_Of_Same_Rgn (Str);
                        New_Substr_Arr : constant Word_Type :=
                          Create_Basic_Array_Obj
                            (Array_Type_Desc => Array_Substr_Type_Desc,
                             Array_Len       => Arr_Len * 2,
                             Stg_Rgn         => Stg_Rgn_Of_Large_Obj (Str),
                             Server_Index    => Server_Index,
                             Init_Elements   => True,
                             Element_Value   => Null_Val);
                        New_Substr_Arr_Ptr : constant Word_Ptr :=
                          Virtual_To_Physical_Address (New_Substr_Arr);
                     begin
                        --  Initialize by moving from old substr array
                        for I in 1 .. Arr_Len loop
                           Interpreter.Store_Word (New_Substr_Arr_Ptr,
                             Large_Obj_Header_Size + Offset_Within_Area (I),
                             Interpreter.Fetch_Word (Arr_Ptr,
                               Large_Obj_Header_Size +
                                 Offset_Within_Area (I)));
                           --  Null out old value
                           Store_Word (Arr_Ptr,
                             Large_Obj_Header_Size + Offset_Within_Area (I),
                             Null_Val);
                        end loop;

                        --  Replace old array with new one
                        Store_Word (Large_Str, Large_Obj_Header_Size,
                          New_Substr_Arr);

                        --  Release old array
                        Release_Large_Obj
                          (Large_Obj_Type_Desc (Str_Arr),
                           Str_Arr,
                           Server_Index);

                        --  Now recurse to finish the job
                        Assign_Concat_String (Str_Ptr,
                          New_String (Start_Index .. New_String'Last),
                          Server_Index);
                     end;
                  end if;
                  return;  --  All done  --
               end;
            end case;
         end;
      end if;

      --  Fallback approach: just create a completely new string and store it.
      Store_Word (Str_Ptr, 0, Word_Type
        (From_String (To_String (Str) & New_String,
                      Null_Of_Same_Rgn (Str),
                      Server_Index)));

      if not Is_Special then
         --  Release original value
         Release_Large_Obj (Large_Obj_Type_Desc (Str), Str, Server_Index);
      end if;

   end Assign_Concat_String;

   procedure Assign_Concat_Wide_Wide_String
     (Str_Ptr      : Interpreter.Word_Ptr;
      New_String   : Wide_Wide_String;
      Server_Index : Interpreter.Thread_Server_Index) is
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr
      Str_Word  : constant Word_Type   := Fetch_Nonnull_Word (Str_Ptr, 0);
      Str       : constant Univ_String := Univ_String (Str_Word);
      Added_Len : constant Natural     := New_String'Length;
      Is_Special : constant Boolean :=
        Str < 0 and then Str mod 2 = 1;

      Max_Ch  : constant Wide_Wide_Character :=
        Strings.Find_Max_Wide_Wide_Char (New_String);
      Max_Pos : constant Word_Type := Wide_Wide_Character'Pos (Max_Ch);
   begin
      if Added_Len = 0 then
         --  A No-Op
         return;
      end if;

      if Max_Pos < 2**8 then
         --  Fits into a narrow string
         declare
            Narrow_Str : String (New_String'Range);
            Ignore_Wide : Boolean := False;
         begin
            Strings.Narrow_String (New_String, Narrow_Str, Ignore_Wide);
            Assign_Concat_String (Str_Ptr, Narrow_Str, Server_Index);
            return;  --  All done  --
         end;
      end if;

      --  Fallback approach: just create a completely new string and store it.
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         Store_Word (Str_Ptr, 0, Word_Type
           (Strings.Index (Strings.Wide_Wide_String_Lookup
             (Strings.To_Wide_Wide_String (Strings.To_U_String
                (Strings.U_String_Index (Str_Word))) & New_String))));
         return;
      end if;

      Store_Word (Str_Ptr, 0, Word_Type
        (From_Wide_Wide_String (To_Wide_Wide_String (Str) & New_String,
                      Null_Of_Same_Rgn (Str),
                      Server_Index)));

      if not Is_Special then
         --  Release original value
         Release_Large_Obj (Large_Obj_Type_Desc (Str), Str, Server_Index);
      end if;
   end Assign_Concat_Wide_Wide_String;

   procedure Assign_Concat_Univ_String
     (Str_Ptr      : Word_Ptr;
      New_String   : Univ_String;
      Server_Index : Interpreter.Thread_Server_Index) is
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr
   begin
      --  TBD: Optimize this if New_String is a U_String, by converting
      --       object into a vector of substrings.
      Assign_Concat_Wide_Wide_String
        (Str_Ptr, To_Wide_Wide_String (New_String), Server_Index);
   end Assign_Concat_Univ_String;

   function Nth_Univ_Character (Str : Univ_String; Index : Positive)
     return Interpreter.Word_Type is
   --  Return Nth univ-character of string
   --  Requires: Index <= Length(Str)
   begin
      --  Make sure Univ_String_Type_Desc is initialized
      Check_Univ_String_Type_Desc;

      if Univ_String_Type_Desc = null
        or else Univ_String_Type_Desc.Is_Small
      then
         --  Using "small" representation
         if Word_Type (Str) = Null_Value then
            return Null_Value;
         else
            return Character'Pos (Strings.To_String (Strings.To_U_String
              (Strings.U_String_Index (Str))) (Index));
         end if;
      end if;

      if Str < 0 and then Str mod 2 = 1 then
         --  See whether is a U_String or a short string
         declare
            Indicator : constant Indicator_Type := String_Indicator (Str);
         begin
            case Indicator is
               when Null_Value_Indicator =>
                  return Null_Value;

               when U_String_Indicator =>
                  return Character'Pos
                    (Strings.To_String (To_U_String (Str)) (Index));

               when Short_String_Indicator =>
                  return Null_Value;

               when Short_String_Indicator + 1 =>
                  --  Single character string
                  if Index = 1 then
                     return Word_Type
                        (S2I (Str) / Short_String_Multiplier mod
                              Char_Multiplier);
                  else
                     return Null_Value;
                  end if;

               when Short_String_Indicator + 2 ..
                 Short_String_Indicator + Max_Short_String =>
                  --  Short multi-character string
                  declare
                     Len : constant Natural :=
                       Natural (Indicator - Short_String_Indicator);
                     Result : String (1 .. Len);
                     Chars : Univ_String_Index :=
                       S2I (Str) / Short_String_Multiplier mod
                                  Max_Short_String_Modulus;
                  begin
                     --  Isolate each character
                     for I in 1 .. Len loop
                        Result (I) :=
                          Character'Val (Chars mod Char_Multiplier);
                        Chars := Chars / Char_Multiplier;
                     end loop;
                     return Character'Pos (Result (Index));
                  end;
               when others =>
                  --  Not a valid representation
                  pragma Assert (False);
                  return Null_Value;
            end case;
         end;
      else
         --  Large object representation
         declare
            use Interpreter;
            Large_Str : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str);
            Str_Arr   : constant Word_Type :=
              Fetch_Word (Large_Str, Large_Obj_Header_Size + 0);
            Str_Info  : constant Info_Type :=
              To_Info (Fetch_Word (Large_Str, Large_Obj_Header_Size + 1));
            Str_Kind  : constant Array_Kind_Enum :=
              Str_Info.Kind;
            Str_Len   : constant Natural := Str_Info.Len;

            Arr_Ptr   : constant Word_Ptr :=
              Virtual_To_Physical_Address (Str_Arr);
            Arr_Len   : constant Natural := Natural
              (Word_Type'(Fetch_Word (Arr_Ptr, Large_Obj_Header_Size)));
         begin
            case Str_Kind is
               when Array_8 =>
                  declare
                     Word_Index : constant Positive :=
                       (Index - 1) / Int8_Per_Word + 1;
                     Within_Word_Offset : constant Natural :=
                       (Index - 1) mod Int8_Per_Word;
                     Divisor : constant Word_Type :=
                       2 ** (8 * Within_Word_Offset);
                     Desired_Word : Word_Type :=
                       Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                  begin
                     if Desired_Word < 0 then
                        --  If negative, we need to make sure division
                        --  has no remainder so equivalent to right shift.
                        return (Desired_Word - Desired_Word mod Divisor) /
                          Divisor mod 2 ** 8;
                     else
                        return Desired_Word / Divisor mod 2 ** 8;
                     end if;
                  end;
               when Array_16 =>
                  declare
                     Word_Index : constant Positive :=
                       (Index - 1) / Int16_Per_Word + 1;
                     Within_Word_Offset : constant Natural :=
                       (Index - 1) mod Int16_Per_Word;
                     Divisor : constant Word_Type :=
                       2 ** (16 * Within_Word_Offset);
                     Desired_Word : Word_Type :=
                       Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                  begin
                     if Desired_Word < 0 then
                        --  If negative, we need to make sure division
                        --  has no remainder so equivalent to right shift.
                        return (Desired_Word - Desired_Word mod Divisor) /
                          Divisor mod 2 ** 16;
                     else
                        return Desired_Word / Divisor mod 2 ** 16;
                     end if;
                  end;
               when Array_31 =>
                  declare
                     Word_Index : constant Positive :=
                       (Index - 1) / Int31_Per_Word + 1;
                     Within_Word_Offset : constant Natural :=
                       (Index - 1) mod Int31_Per_Word;
                     Divisor : constant Word_Type :=
                       2 ** (31 * Within_Word_Offset);
                     Desired_Word : Word_Type :=
                       Fetch_Word
                          (Arr_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area (Word_Index));
                  begin
                     if Desired_Word < 0 then
                        --  If negative, we need to make sure division
                        --  has no remainder so equivalent to right shift.
                        return (Desired_Word - Desired_Word mod Divisor) /
                          Divisor mod 2 ** 31;
                     else
                        return Desired_Word / Divisor mod 2 ** 31;
                     end if;
                  end;
               when Array_Substr_Kinds =>
                  declare
                     Adjusted_Index : Integer := Index;
                  begin
                     for I in 1 .. Arr_Len loop
                        declare
                           Next_Substr : constant Univ_String :=
                             Fetch_Word (Arr_Ptr,
                                         Large_Obj_Header_Size +
                                           Offset_Within_Area (I));
                           Substr_Len  : constant Natural :=
                             Length (Next_Substr);
                        begin
                           if Substr_Len = 0 then
                              return Null_Value;
                           end if;
                           if Adjusted_Index <= Substr_Len then
                              --  Recurse with this substring
                              return Nth_Univ_Character
                                (Next_Substr, Adjusted_Index);
                           end if;
                           Adjusted_Index := Adjusted_Index - Substr_Len;
                        end;
                     end loop;
                     return Null_Value;
                  end;
            end case;
         end;
      end if;
   end Nth_Univ_Character;

   function Str_Lit_In_Rgn
     (Index : Interpreter.Word_Type; Existing_Obj : Interpreter.Word_Type)
      return Univ_String is
   --  Return Univ_String, given U_String_Index (as a Word_Type) and
   --  an existing object determining target region.
   begin
      return From_U_String
        (Strings.To_U_String (Strings.U_String_Index (Index)),
         Null_Of_Same_Rgn (Univ_String (Existing_Obj)));
   end Str_Lit_In_Rgn;

   function Local_Str_Lit
     (Context : Interpreter.Exec_Context_Ptr; Index : Word_Type)
      return Univ_String is
   --  Return Univ_String in local region, given context and U_String_Index
   --  (as a Word_Type)
   begin
      return From_U_String
        (Strings.To_U_String (Strings.U_String_Index (Index)),
         Interpreter.Null_For_Stg_Rgn (Context.Local_Stg_Rgn));
   end Local_Str_Lit;

   function Global_Str_Lit (Index : Word_Type) return Univ_String is
   --  Return Univ_String in global stg rgn, given U_String_Index
   --  (as a Word_Type)
   begin
      Initialize_Global_Data_Stg_Rgn;  --  Make sure global region is init'ed
      return From_U_String
        (Strings.To_U_String (Strings.U_String_Index (Index)),
         Interpreter.Null_For_Stg_Rgn (Interpreter.Global_Data_Stg_Rgn));
   end Global_Str_Lit;

end PSC.Univ_Strings;
