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

with Ada.Streams;
with PSC.Hash_Tables;
with PSC.String_Streams;

pragma Elaborate (PSC.Hash_Tables);
pragma Elaborate (PSC.String_Streams);
package PSC.Strings is
   --  Package to represent each distinct string uniquely.

   type Hash_Type is mod 2 ** 32;

   type U_String is private;
   --  A uniquified string.

   procedure U_String_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : U_String);
   for U_String'Write use U_String_Write;
   --  Write out the contents of the string

   procedure U_String_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_String);
   for U_String'Read use U_String_Read;
   --  Read in the contents of the string an uniquify it

   type U_String_Index is range 0 .. 2**24 - 1;
   --  A unique index for each unique string; limited to 24 bits

   procedure U_String_Index_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : U_String_Index'Base);
   for U_String_Index'Write use U_String_Index_Write;
   --  Write out the contents of the string

   procedure U_String_Index_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_String_Index'Base);
   for U_String_Index'Read use U_String_Index_Read;
   --  Read in the contents of the string and get its index

   Null_U_String : constant U_String;
   Null_U_String_Index : constant U_String_Index := 0;
   Empty_U_String : constant U_String;
   Empty_U_String_Index : constant U_String_Index := 1;

   function String_Lookup
     (S : String; Must_Be_New : Boolean := False) return U_String;
   --  Get the U_String for the given string
   --  If Must_Be_New is True, then return the Null_U_String
   --  if the given string is *already* in the hash table.

   function To_String (U : U_String) return String;
   --  Get back the original string

   function Wide_Wide_String_Lookup
     (S : Wide_Wide_String; Must_Be_New : Boolean := False) return U_String;
   --  Get the U_String for the given wide-wide-string
   --  If Must_Be_New is True, then return the Null_U_String
   --  if the given string is *already* in the hash table.

   function To_Wide_Wide_String (U : U_String) return Wide_Wide_String;
   --  Get back the original wide-wide-string

   function Find_Max_Wide_Wide_Char (S : Wide_Wide_String)
     return Wide_Wide_Character;
   --  Scan wide-wide string S and return character with max character code.

   procedure Narrow_String
     (S : Wide_Wide_String; Narrow : out String; Uses_Wide_Char : out Boolean)
     with Pre => Narrow'Length = S'Length;
   --  Produce a "narrow" version of S by simply removing high bits.
   --  Set Uses_Wide_Char True if any of the characters were non-zero
   --  in their high bits.

   function Length (U : U_String) return Natural;
   --  Return length of U_String.  Length(Null_U_String) = 0.

   function Hash (U : U_String) return Hash_Type;
   --  Get a hash for the unique string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.

   function Index (U : U_String) return U_String_Index;
   --  Return unique index associated with string

   function To_U_String (Index : U_String_Index) return U_String;
   --  Return U_String given its unique index

   function Case_Insensitive_Equal (Left, Right : U_String) return Boolean;
   --  Return True if Left and Right are equal ignoring case.

   function Escaped_Char (Char : Character) return Character;
   --  Return meaning of Char when preceded by a back slash.
   --  NOTE: This does not handle the \#xx_xx# syntax.

   function To_UTF_8 (S : String) return String;
   --  Convert any escaped characters and produce a UTF-8 string.

   function To_UTF_16 (S : String) return Wide_String;
   --  Convert any escaped characters and produce a UTF-16 string

   type U_String_Array is array (Positive range <>) of Strings.U_String;

   function Tokenize (S : String; Seps : String := ' ' & ASCII.HT)
     return U_String_Array;
   --  Break up S into tokens and return as array of U_Strings.
   --  Seps specifies characters that act as separators.

   function Skip_Leading_Spaces (S : String) return String;
   --  Return S stripped of any leading spaces

   function Simple_Name (Full_Name : String) return String;
   --  Return Full_Name stripped of module parameters, if any
   --  and prefix.

   function Contains (S : String; Char : Character) return Boolean;
   --  Return True if S contains Char.

   function Hash_String (S : String; Prefix : Hash_Type := 0) return Hash_Type;
   --  Get a hash for the string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   --  Prefix is hash value of chars preceding S, if we are computing
   --  the "hash" for a concatenation.

   function Hash_Wide_Wide_String
      (S : Wide_Wide_String; Prefix : Hash_Type := 0) return Hash_Type;
   --  Get a hash for the wide-wide-string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   --  Prefix is hash value of chars preceding S, if we are computing
   --  the "hash" for a concatenation.

private

   type U_String_Rec;

   type U_String is access U_String_Rec;

   type Wide_Wide_Str_Ptr is access Wide_Wide_String;

   type U_String_Rec (Len : Natural) is record
      Next : U_String := null;
      pragma Atomic (Next);
      Index : U_String_Index := 0;
      Hash : Hash_Type := 0;
      Str : String (1 .. Len);
      Wide_Wide_Str : Wide_Wide_Str_Ptr := null;
   end record;

   Null_U_String : constant U_String := null;

   Empty_U_String : constant U_String :=
     new U_String_Rec '(Len => 0, Next => null,
                        Index => Empty_U_String_Index,
                        Hash => 0, Str => "", Wide_Wide_Str => null);

end PSC.Strings;
