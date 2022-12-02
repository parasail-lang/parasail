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

package body PSC.Languages is
   --  Language-specific information within the ParaSail "family" of languages

   Current_Language : Language_Enum := ParaSail;

   ----------------  Visible Subprograms  -----------------

   procedure Set_Language (Language : Language_Enum) is
   --  Set the Language
   begin
      Current_Language := Language;
   end Set_Language;

   function Language return Language_Enum is
   --  Return the current language
   --    (ParaSail, Sparkel, Ada202X, Parython, or Javallel)
   begin
      return Current_Language;
   end Language;

   function Language_Name return String is
   --  Return the Language Name ("ParaSail," "Sparkel," "Ada202x," "Parython",
   --  or "Javallel")
   begin
      case Current_Language is
         when ParaSail => return "ParaSail";
         when Sparkel  => return "Sparkel";
         when Ada202x  => return "Ada202x";
         when Parython => return "Parython";
         when Javallel => return "Javallel";
      end case;
   end Language_Name;

   function Language_Name_LC return String is
   --  Return the Language Name in lower case
   --  ("parasail", "sparkel", "ada202x", "parython", "javallel");
   begin
      case Current_Language is
         when ParaSail => return "parasail";
         when Sparkel  => return "sparkel";
         when Ada202x  => return "ada202x";
         when Parython => return "parython";
         when Javallel => return "javallel";
      end case;
   end Language_Name_LC;

   function Language_Uses_Selection_For_Modules return Boolean is
   --  Return True if language uses selection syntax (e.g. ".") for
   --  selecting from modules (and types).
   begin
      case Current_Language is
         when ParaSail => return False;
         when Ada_Ish  => return True;
         when Parython => return True;
         when Javallel => return True;
      end case;
   end Language_Uses_Selection_For_Modules;

   function Language_Uses_Parens_For_Indexing return Boolean is
   --  Return True if languages uses parentheses for indexing.
   begin
      case Current_Language is
         when ParaSail => return False;
         when Ada_Ish  => return True;
         when Parython => return False;
         when Javallel => return False;
      end case;
   end Language_Uses_Parens_For_Indexing;

   function Module_Name_Separator return String is
   --  Return character sequence used to separate parts of a module name
   --  (e.g. "::" or ".").
   begin
      case Current_Language is
         when ParaSail => return "::";
         when Ada_Ish  => return ".";
         when Parython => return ".";
         when Javallel => return ".";
      end case;
   end Module_Name_Separator;

   function Standard_Library_Prefix return String is
   --  Return the prefix to use for the standard library
   --  (e.g. "PSL" or "SSL").
   begin
      case Current_Language is
         when ParaSail => return "PSL";
         when Sparkel  => return "SSL";
         when Ada202x  => return "A2X";
         when Parython => return "PSL";
         when Javallel => return "java";
      end case;
   end Standard_Library_Prefix;

   function Standard_Library_Core_Module return String is
   --  Return the core module to use for the standard library
   --  (e.g. "Core" or "lang").
   begin
      case Current_Language is
         when ParaSail => return "Core";
         when Ada_Ish  => return "Core";
         when Parython => return "Core";
         when Javallel => return "lang";
      end case;
   end Standard_Library_Core_Module;

   function Standard_Library_Containers_Module return String is
   --  Return the container module to use for the standard library
   --  (e.g. "Containers" or "util").
   begin
      case Current_Language is
         when ParaSail => return "Containers";
         when Ada_Ish  => return "Containers";
         when Parython => return "Containers";
         when Javallel => return "util";
      end case;
   end Standard_Library_Containers_Module;

   --  Return language-specific module and operation names
   function Any_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Any";
         when Ada_Ish  => return "Any";
         when Parython => return "Any";
         when Javallel => return "Any";
      end case;
   end Any_Module_Name;

   function Assignable_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Assignable";
         when Ada_Ish  => return "Assignable";
         when Parython => return "Assignable";
         when Javallel => return "Assignable";
      end case;
   end Assignable_Module_Name;

   function Boolean_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Boolean";
         when Ada_Ish  => return "Boolean";
         when Parython => return "Boolean";
         when Javallel => return "Boolean";
      end case;
   end Boolean_Module_Name;

   function Ordering_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Ordering";
         when Ada_Ish  => return "Ordering";
         when Parython => return "Ordering";
         when Javallel => return "Ordering";
      end case;
   end Ordering_Module_Name;

   function Exception_Type_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Exception_Type";
         when Ada_Ish  => return "Exception_Type";
         when Parython => return "Exception_Type";
         when Javallel => return "ExceptionType";
      end case;
   end Exception_Type_Module_Name;

   function Univ_Integer_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Univ_Integer";
         when Ada_Ish  => return "Univ_Integer";
         when Parython => return "Univ_Integer";
         when Javallel => return "UnivInteger";
      end case;
   end Univ_Integer_Module_Name;

   function Unsigned_64_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Unsigned_64";
         when Ada_Ish  => return "Unsigned_64";
         when Parython => return "Unsigned_64";
         when Javallel => return "Unsigned64";
      end case;
   end Unsigned_64_Module_Name;

   function Univ_Real_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Univ_Real";
         when Ada_Ish  => return "Univ_Real";
         when Parython => return "Univ_Real";
         when Javallel => return "UnivReal";
      end case;
   end Univ_Real_Module_Name;

   function Univ_Character_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Univ_Character";
         when Ada_Ish  => return "Univ_Character";
         when Parython => return "Univ_Character";
         when Javallel => return "UnivCharacter";
      end case;
   end Univ_Character_Module_Name;

   function Univ_String_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Univ_String";
         when Ada_Ish  => return "Univ_String";
         when Parython => return "Univ_String";
         when Javallel => return "UnivString";
      end case;
   end Univ_String_Module_Name;

   function Univ_Enumeration_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Univ_Enumeration";
         when Ada_Ish  => return "Univ_Enumeration";
         when Parython => return "Univ_Enumeration";
         when Javallel => return "UnivEnumeration";
      end case;
   end Univ_Enumeration_Module_Name;

   function Optional_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Optional";
         when Ada_Ish  => return "Optional";
         when Parython => return "Optional";
         when Javallel => return "Optional";
      end case;
   end Optional_Module_Name;

   function Null_Literal_Spelling return String is
   begin
      case Current_Language is
         when ParaSail => return "null";
         when Ada_Ish  => return "null";
         when Parython => return "null";
         when Javallel => return "null";
      end case;
   end Null_Literal_Spelling;

   function Default_Integer_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Integer";
         when Ada_Ish  => return "Integer";
         when Parython => return "Integer";
         when Javallel => return "Integer";
      end case;
   end Default_Integer_Module_Name;

   function Default_Float_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Float";
         when Ada_Ish  => return "Float";
         when Parython => return "Float";
         when Javallel => return "Float";
      end case;
   end Default_Float_Module_Name;

   function Basic_Array_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Basic_Array";
         when Ada_Ish  => return "Basic_Array";
         when Parython => return "Basic_Array";
         when Javallel => return "BasicArray";
      end case;
   end Basic_Array_Module_Name;

   function Aliased_Object_Module_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Aliased_Obj";
         when Ada_Ish  => return "Aliased_Obj";
         when Parython => return "Aliased_Obj";
         when Javallel => return "AliasedObj";
      end case;
   end Aliased_Object_Module_Name;

   function Debug_Console_Full_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "PSC::Debugging::Console";
         when Sparkel  => return "SSC.Debugging.Console";
         when Ada202x  => return "A2C.Debugging.Console";
         when Parython => return "PSC.Debugging.Console";
         when Javallel => return "java.util.debugging.Console";
      end case;
   end Debug_Console_Full_Name;

   function From_Univ_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return """from_univ""";
         when Ada_Ish  => return """from_univ""";
         when Parython => return """from_univ""";
         when Javallel => return """from_univ""";
      end case;
   end From_Univ_Op_Name;

   function To_Univ_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return """to_univ""";
         when Ada_Ish  => return """to_univ""";
         when Parython => return """to_univ""";
         when Javallel => return """to_univ""";
      end case;
   end To_Univ_Op_Name;

   function Remove_First_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Remove_First";
         when Ada_Ish  => return "Remove_First";
         when Parython => return "Remove_First";
         when Javallel => return "removeFirst";
      end case;
   end Remove_First_Op_Name;

   function Remove_Last_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Remove_Last";
         when Ada_Ish  => return "Remove_Last";
         when Parython => return "Remove_Last";
         when Javallel => return "removeLast";
      end case;
   end Remove_Last_Op_Name;

   function Remove_Any_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "Remove_Any";
         when Ada_Ish  => return "Remove_Any";
         when Parython => return "Remove_Any";
         when Javallel => return "removeAny";
      end case;
   end Remove_Any_Op_Name;

   function Compare_Op_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "=?";
         when Ada_Ish  => return "=?";
         when Parython => return "=?";
         when Javallel => return "=?";
      end case;
   end Compare_Op_Name;

   --  Test whether is a module defining a "universal" type
   function Is_Univ_Module_Name (Name : String) return Boolean is
   begin
      if Name'Length <= 5
        or else Name (Name'First .. Name'First + 3) /= "Univ"
      then
         --  Cannot be a univ type.
         --  NOTE: This presumes they all start with "Univ";
         --        above test will need revision if that changes.
         return False;
      else
         --  Might be a "univ" type.
         return Name = Univ_String_Module_Name
           or else Name = Univ_Character_Module_Name
           or else Name = Univ_Integer_Module_Name
           or else Name = Univ_Real_Module_Name
           or else Name = Univ_Enumeration_Module_Name;
      end if;
   end Is_Univ_Module_Name;

   --  Return full canonical name for certain types of interest,
   --  useful for calls on Get_Type_Desc_By_Name.

   function Univ_Integer_Type_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "PSL::Core::Univ_Integer";
         when Sparkel  => return "SSL.Core.Univ_Integer.Univ_Integer";
         when Ada202x  => return "A2X.Core.Univ_Integer.Univ_Integer";
         when Parython => return "PSL.Core.Univ_Integer";
         when Javallel => return "java.lang.UnivInteger";
      end case;
   end Univ_Integer_Type_Name;

   function Univ_String_Type_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "PSL::Core::Univ_String";
         when Sparkel  => return "SSL.Core.Univ_String.Univ_String";
         when Ada202x  => return "A2X.Core.Univ_String.Univ_String";
         when Parython => return "PSL.Core.Univ_String";
         when Javallel => return "java.lang.UnivString";
      end case;
   end Univ_String_Type_Name;

   function Operation_Descriptor_Type_Name return String is
   begin
      case Current_Language is
         when ParaSail => return "PSL::Core::Operation_Descriptor";
         when Sparkel  => return "SSL.Core.Operation_Descriptor";
         when Ada202x  => return "A2X.Core.Operation_Descriptor";
         when Parython => return "PSL.Core.Operation_Descriptor";
         when Javallel => return "java.lang.OperationDescriptor";
      end case;
   end Operation_Descriptor_Type_Name;

   function Optional_Unsigned_64_Basic_Array_Type_Name return String is
   begin
      case Current_Language is
         when ParaSail => return
           "PSL::Containers::Basic_Array<PSL::Core::Unsigned_64>";
         when Sparkel  => return
           "SSL.Containers.Basic_Array" &
             "<SSL.Core.Unsigned_64.Unsigned_64>.Basic_Array";
         when Ada202x  => return
           "A2X.Containers.Basic_Array" &
             "<SSL.Core.Unsigned_64.Unsigned_64>.Basic_Array";
         when Parython => return
           "PSL.Containers.Basic_Array<PSL.Core.Unsigned_64>";
         when Javallel => return
           "java.util.BasicArray<java.lang.Unsigned64>";
      end case;
   end Optional_Unsigned_64_Basic_Array_Type_Name;

   function Univ_String_Basic_Array_Type_Name return String is
   begin
      case Current_Language is
         when ParaSail => return
           "PSL::Containers::Basic_Array<PSL::Core::Univ_String>";
         when Sparkel  => return
           "SSL.Containers.Basic_Array" &
           "<SSL.Core.Univ_String.Univ_String>.Basic_Array";
         when Ada202x  => return
           "A2X.Containers.Basic_Array" &
           "<A2X.Core.Univ_String.Univ_String>.Basic_Array";
         when Parython => return
           "PSL.Containers.Basic_Array<PSL.Core.Univ_String>";
         when Javallel => return
           "java.util.BasicArray<java.lang.UnivString>";
      end case;
   end Univ_String_Basic_Array_Type_Name;

end PSC.Languages;
