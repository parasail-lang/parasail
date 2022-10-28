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

with PSC.Strings;
with PSC.Interpreter;
package PSC.Univ_Strings is
   --  Package to represent Univ_String, using U_Strings for
   --  long-lived strings, and using "big" objects for short-lived strings.
   --  Every Univ_String has an associated Stg_Rgn, identified by a
   --  Interpreter.Stg_Rgn_Index

   type Univ_String is private;
   --  A univ string representation, convertible to/from Word_Type

   Max_Short_String : constant := 3;  --  Max chars in "short" string

   function Get_Univ_String_Type_Desc return Interpreter.Non_Op_Map_Type_Ptr;
   --  Return Univ_String type descriptor

   procedure Reset_Univ_String_Type_Desc;
   --  Reset Univ_String_Type_Desc and other type descriptors
   --  as part of switching to a new language.

   function Is_Null (Str : Univ_String) return Boolean;
   --  Return True if Str is a null value of Univ_String type.

   function Null_Of_Same_Rgn (Str : Univ_String) return Interpreter.Word_Type;
   --  Return Null value for Univ_String from same region as Str

   function Empty_Univ_String (Rgn : Interpreter.Stg_Rgn_Index)
     return Univ_String;
   --  Return an empty univ string for given region

   function Equal (Left, Right : Univ_String) return Boolean;
   --  Indicate whether two strings are equal (ignore Stg_Rgn_Index)

   function Compare (Left, Right : Univ_String) return Interpreter.Ordering;
   --  Implementation of "=?" for Univ_String

   --  Convert to/from U_String
   function To_U_String (Str : Univ_String) return Strings.U_String;
   function From_U_String
              (Str : Strings.U_String;
               Null_For_Rgn : Interpreter.Word_Type) return Univ_String;
   --  Return Univ_String given a U_String and null identifying region

   --  Convert to/from regular String (TBD: Wide_String vs. UTF-8?)
   function To_String (Str : Univ_String) return String;

   function From_String
     (Str          : String;
      Null_For_Rgn : Interpreter.Word_Type;
      Server_Index : Interpreter.Thread_Server_Index :=
        Interpreter.Current_Server_Index;
      Minimum_Len  : Natural := 0)
     return Univ_String;
   --  Convert to Univ_String from regular String (TBD: Wide_String vs. UTF-8?)
   --  given null for region and server index.
   --  Capacity of returned string will be at least Minimum_Len.

   --  Convert to/from regular Wide_Wide_String
   function To_Wide_Wide_String (Str : Univ_String) return Wide_Wide_String;

   function From_Wide_Wide_String
     (Str          : Wide_Wide_String;
      Null_For_Rgn : Interpreter.Word_Type;
      Server_Index : Interpreter.Thread_Server_Index :=
        Interpreter.Current_Server_Index;
      Minimum_Len  : Natural := 0)
     return Univ_String;
   --  Convert to Univ_String from Wide_Wide_String
   --  given null for region and server index.
   --  Capacity of returned string will be at least Minimum_Len.

   --  Convert to/from Word_Type; Stg_Rgn_Index is part of Word_Type rep.
   function To_Word_Type (Str : Univ_String) return Interpreter.Word_Type;
   function From_Word_Type (Word : Interpreter.Word_Type) return Univ_String;
   pragma Inline (To_Word_Type, From_Word_Type);

   function Length (Str : Univ_String) return Natural;
   --  Return length of string; Length (Null_Univ_String) returns 0.

   function Hash (Str : Univ_String) return Strings.Hash_Type;
   --  Get a hash for the univ string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.

   function Case_Insensitive_Equal (Left, Right : Univ_String) return Boolean;
   --  Return True if Left and Right are equal ignoring case.

   procedure Assign_Concat_String
     (Str_Ptr      : Interpreter.Word_Ptr;
      New_String   : String;
      Server_Index : Interpreter.Thread_Server_Index);
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr

   procedure Assign_Concat_Wide_Wide_String
     (Str_Ptr      : Interpreter.Word_Ptr;
      New_String   : Wide_Wide_String;
      Server_Index : Interpreter.Thread_Server_Index);
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr

   procedure Assign_Concat_Univ_String
     (Str_Ptr      : Interpreter.Word_Ptr;
      New_String   : Univ_String;
      Server_Index : Interpreter.Thread_Server_Index);
   --  Concatenate New_String onto end of string pointed-to by Str_Ptr

   function Nth_Univ_Character (Str : Univ_String; Index : Positive)
     return Interpreter.Word_Type;
   --  Return Nth univ-character of string
   --  Requires: Index <= Length(Str)

   function Str_Lit_In_Rgn
     (Index : Interpreter.Word_Type; Existing_Obj : Interpreter.Word_Type)
      return Univ_String;
   pragma Export (Ada, Str_Lit_In_Rgn, "_psc_str_lit_in_rgn");
   --  Return Univ_String, given U_String_Index (as a Word_Type) and
   --  an existing object determining target region.

   function Local_Str_Lit
     (Context : Interpreter.Exec_Context_Ptr; Index : Interpreter.Word_Type)
      return Univ_String;
   pragma Export (Ada, Local_Str_Lit, "_psc_local_str_lit");
   --  Return Univ_String in local region, given context and U_String_Index
   --  (as a Word_Type)

   function Global_Str_Lit (Index : Interpreter.Word_Type) return Univ_String;
   pragma Export (Ada, Global_Str_Lit, "_psc_global_str_lit");
   --  Return Univ_String in global stg rgn, given U_String_Index
   --  (as a Word_Type)

private

   type Univ_String is new Interpreter.Word_Type;

end PSC.Univ_Strings;
