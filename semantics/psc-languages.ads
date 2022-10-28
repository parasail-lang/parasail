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

package PSC.Languages is
   --  Language-specific information within the ParaSail "family" of languages

   pragma Elaborate_Body;

   type Language_Enum is (ParaSail, Sparkel, Ada202x, Parython, Javallel);

   subtype Ada_Ish is Language_Enum range Sparkel .. Ada202x;

   type Convention_Enum is
      --  Calling/representation convention for compiled routines
     (Convention_Internal_Default,  --  No convention specified, and not import
      Convention_External_Default,  --  No convention specified, and is import
      Convention_Locking_Default,   --  No convention specified, locking
      Convention_Queuing_Default,   --  No convention specified, queuing
      Convention_ParaSail,          --  #parasail convention specified
      Convention_Sparkel,           --  #sparkel convention specified
      Convention_Parython,          --  #parython convention specified
      Convention_Javallel,          --  #javallel convention specified
      Convention_Ada,               --  #ada convention specified
      Convention_C,                 --  #c convention specified
      Convention_CPP);              --  #cpp convention specified

   procedure Set_Language (Language : Language_Enum);
   --  Set the Language

   function Language return Language_Enum;
   --  Return the current language
   --    (ParaSail, Sparkel, Ada202x, Parython, or Javallel)

   function Language_Name return String;
   --  Return the Language Name ("ParaSail," "Sparkel," "Ada202x," "Parython,"
   --  or "Javallel")

   function Language_Name_LC return String;
   --  Return the Language Name in lower case
   --  ("parasail", "sparkel", "ada202x", "parython", "javallel");

   function Language_Uses_Selection_For_Modules return Boolean;
   --  Return True if language uses selection syntax (e.g. ".") for
   --  selecting from modules (and types).

   function Language_Uses_Parens_For_Indexing return Boolean;
   --  Return True if languages uses parentheses for indexing.

   function Module_Name_Separator return String;
   --  Return character sequence used to separate parts of a module name
   --  (e.g. "::" or ".").

   function Standard_Library_Prefix return String;
   --  Return the prefix to use for the standard library
   --  (e.g. "PSL" or "SSL").

   function Standard_Library_Core_Module return String;
   --  Return the core module to use for the standard library
   --  (e.g. "Core" or "lang").

   function Standard_Library_Containers_Module return String;
   --  Return the container module to use for the standard library
   --  (e.g. "Containers" or "util").

   --  Return language-specific module and operation names
   function Any_Module_Name return String;
   function Assignable_Module_Name return String;
   function Boolean_Module_Name return String;
   function Ordering_Module_Name return String;
   function Exception_Type_Module_Name return String;
   function Univ_Integer_Module_Name return String;
   function Unsigned_64_Module_Name return String;
   function Univ_Real_Module_Name return String;
   function Univ_Character_Module_Name return String;
   function Univ_String_Module_Name return String;
   function Univ_Enumeration_Module_Name return String;
   function Optional_Module_Name return String;
   function Null_Literal_Spelling return String;
   function Default_Integer_Module_Name return String;
   function Default_Float_Module_Name return String;
   function Basic_Array_Module_Name return String;
   function Aliased_Object_Module_Name return String;

   function Debug_Console_Full_Name return String;

   function From_Univ_Op_Name return String;
   function To_Univ_Op_Name return String;
   function Remove_First_Op_Name return String;
   function Remove_Last_Op_Name return String;
   function Remove_Any_Op_Name return String;
   function Compare_Op_Name return String;

   --  Test whether is a module defining a "universal" type
   function Is_Univ_Module_Name (Name : String) return Boolean;

   --  Return full canonical name for certain types of interest,
   --  useful for calls on Get_Type_Desc_By_Name.
   function Univ_Integer_Type_Name return String;
   function Univ_String_Type_Name return String;
   function Operation_Descriptor_Type_Name return String;
   function Optional_Unsigned_64_Basic_Array_Type_Name return String;
   function Univ_String_Basic_Array_Type_Name return String;

end PSC.Languages;
