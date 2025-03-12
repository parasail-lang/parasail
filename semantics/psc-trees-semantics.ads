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
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with PSC.Languages;
with PSC.Strings;
with PSC.Symbols;
with PSC.Interpreter;
with PSC.Source_Positions;
with PSC.Trees.Lists;

with Ada.Text_IO;
package PSC.Trees.Semantics is

   --------- Context for second pass of semantic analysis ---------
   type Context_Enum is (
     No_Context,
     Statement_Context,       --  Context for statement/nested decl
     Class_Agg_Context,       --  Element of class aggregate
     Container_Agg_Context,   --  Element of container aggregate
     Operand_Context,         --  operand presumed to be R/O
     Call_Operand_Context,    --  operand context for call which might be R/W
     Ref_Operand_Context,     --  ref operand context
      --  where we should use "var_indexing" if "indexing" doesn't return a ref
     Mutable_Context,        --  LHS of assignment or optional (ref) var.
      --  where we should use "var_indexing" op for A[I], even if "indexing"
      --  returns a "ref" as well.
     Exit_With_Values_Context,  --  exit loop with A => B or (A => B, C => D)
     Operation_Input_Context, --  while walking outputs of operation
     Operation_Output_Context, --  while walking outputs of operation
     Op_Context,             --  operation of call
     Formal_Op_Context,      --  actual operation name passed in a module
      --  instantiation, for a formal operation param
     Module_Context,         --  module of instantiation
     Module_Formal_Context,  --  formal parameter of module
     Module_Implements_Interfaces_Context,  --  list of implemented interfaces
     Module_Formal_Type_Def_Context,  --  type-def part of formal type
     Module_Actual_Of_Formal_Context, --  actual parameter to formal type
      --  e.g. interface Sorting<Arr is Array<Comparable<>>> is ...
      --  "Comparable<>" is an actual of a formal.
     Module_Actual_Type_Context,  --  actual type parameter of instantiation
     Module_Extends_Interface_Context,  --  Parent part declaration
     Ancestor_Implements_Item_Context,
      --  item from implements section of ancestor module interface
     Ancestor_Item_Context,
      --  item from module interface of ancestor
     Standalone_Item_Context,  --  standalone module or operation
     Interface_Item_Context,  --  item from module interface
     Interface_Implements_Item_Context,   --  item from implements section of
                                          --  module interface
     Local_Class_Item_Context,  --  local item from module class
     Exported_Class_Item_Context,  --  exported item from module class
     Class_Implements_Item_Context,  --  item from implements section of class
     Type_Context,            --  type for object decl, prefix of "::", etc.
     Selector_Prefix_Context, --  obj of obj.selector
     Case_Choice_Context);    --  case choice (inside [])

   subtype Statement_Contexts is Context_Enum range
     No_Context .. Statement_Context;
   --  These contexts do not allow typed results -- must resolve to a statement
   --  or a declaration.

   subtype Operand_Contexts is Context_Enum range
     Operand_Context .. Ref_Operand_Context;
   --  These contexts are used for operands, and determines whether
   --  to prefer "indexing" or "var_indexing"

   subtype Var_Indexing_Contexts is Context_Enum range
     Call_Operand_Context .. Mutable_Context;
   --  These contexts are used when "var_indexing" is to be considered
   --  in addition to "indexing", but preferred only if formal param
   --  turns out to require a "var" actual.

   subtype Var_Indexing_Preferred_Contexts is Context_Enum range
     Ref_Operand_Context .. Mutable_Context;
   --  These contexts are used when "var_indexing" is to be preferred
   --  to "indexing"

   Obj_Decl_Context_Map : constant array (Boolean) of Operand_Contexts :=
     (False => Operand_Context, True => Ref_Operand_Context);
      --  Operand contexts to be used for an object decl,
      --  indexed by whether the obj-decl is declared "ref"

   subtype Operation_Name_Contexts is Context_Enum range
     Op_Context .. Formal_Op_Context;
   --  These contexts are looking for an operation name
   --  and should not stop if they hit the name of an output
   --  implicitly named by the enclosing operation.

   subtype Agg_Contexts is Context_Enum range
     Class_Agg_Context .. Container_Agg_Context;
   --  These contexts allow for a special-case use of "<==" as
   --  a component of an aggregate.

   subtype Operand_Or_Agg_Contexts is Context_Enum range
     Agg_Contexts'First .. Operand_Contexts'Last;
   --  These contexts allow a target type to be provided
   --  to help with overload resolution.

   subtype Ancestor_Item_Contexts is Context_Enum range
     Ancestor_Implements_Item_Context .. Ancestor_Item_Context;
   --  These are interface declarations inherited from ancestors
   --  that must be implemented when this module is "implemented,"
   --  and which in turn can be used to implement some other
   --  module's interface.

   subtype Interface_Item_Contexts is Context_Enum range
     Interface_Item_Context .. Interface_Implements_Item_Context;
   --  These are the ones that must be implemented when
   --  this module is "implemented," and which in turn can be
   --  used to implement some other module's interface.

   subtype Any_Interface_Item_Contexts is Context_Enum range
     Ancestor_Implements_Item_Context .. Interface_Implements_Item_Context;
   --  These are the ones that are potentially visible unless overridden.

   subtype Visible_Interface_Item_Contexts is Context_Enum range
     Ancestor_Item_Context .. Interface_Item_Context;
   --  These are the ones that are always visible unless overridden.

   subtype Module_Parameter_And_Ancestry_Contexts is Context_Enum range
     Module_Formal_Context .. Module_Extends_Interface_Context;
   --  These are also visible from outside module

   subtype Exported_Class_Item_Contexts is Context_Enum range
     Exported_Class_Item_Context .. Class_Implements_Item_Context;
   --  These are the ones that are the "bodies" for the items
   --  declared in the interface.
   --  used to implement some other module's interface.

   --------- Semantic info associated with Tree nodes ---------

   type Semantic_Info is new Root_Semantic_Info with record
      Associated_Symbol : Symbols.Sym_Ptr;
      Nested_Region : Symbols.Region_Ptr;
      Context : Context_Enum := No_Context;
      --  This indicates where a construct occurs
      --  relative to its enclosing module
      --  (interface, class local part, class exported part).
   end record;

   type Sem_Ptr is access all Semantic_Info'Class;

   procedure Give_Copyright (File : Ada.Text_IO.File_Type);
   --  Produce a copyright/license notice on the given File.

   Sem_Error_Count : Natural := 0;
   --  Total number of error messages generated

   procedure Sem_Error
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
     Source_Positions.Null_Source_Position);
   --  Produce an error message on the standard error output.

   procedure Sem_Error (OT : Optional_Tree; S : String);
   --  Produce an error message on the Current_Error file

   procedure Sem_Error (T : Trees.Tree'Class; S : String);
   --  Produce an error message on the Current_Error file

   procedure Sem_Warning
     (S : String;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position);
   --  Produce a warning message on the standard error output.

   procedure Set_Language (Lang : Languages.Language_Enum);
   --  This is called once at the beginning of processing to establish
   --  the language being parsed, within the ParaSail "family" of languages.

   procedure Start_New_Source_File;
   --  This is called each time we start parsing a new source file

   procedure Add_Top_Level_Tree (OT : Optional_Tree; Imports : Lists.List);
   --  Add top-level tree to library, with given Imports list

   function Analyze
     (Command_To_Execute : Strings.U_String_Array :=
     (1 .. 0 => Strings.Null_U_String)) return Natural;
   --  Analyze trees in library
   --  If Command_To_Execute is non-null then start-up the thread servers,
   --  invoke specified operation with given arguments, and shut down the
   --  thread servers.
   --  Return number of compile-time errors (0 if A-OK).

   procedure Analyze_And_Interpret_All
     (Total_Errors : in out Natural;
      Command_Given : Boolean);
   --  If no Command_Given, then
   --     Analyze all modules in the library
   --     If no errors, prompt for commands and execute them.
   --  If there was a Command_Given, then
   --    we will have already analyzed everything and executed the command
   --    during the Parse_All operation.
   --  Shut down the thread servers

   procedure Finish_Type_Descriptor
     (Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False);
   --  Finish up formal object parameters and nested constants
   --  If Return_On_Recursion is True, do not complain about recursion
   --  and simply return immediately.

   function Name_For_Object_Locator
     (Locator : Interpreter.Object_Locator;
      Enclosing_Type : Interpreter.Object_Locator :=
     Interpreter.Null_Object_Locator)
      return String;
   --  Return a string representing the name associated with the
   --  given object locator.  Return "" if none available.
   --  NOTE: Currently only supports Const_Area, Zero_Base, and Type_Area.
   --       Type_Area requires Enclosing_Type to be provided,
   --       which must have a Zero_Base.

   function Type_Sem_Image
     (Type_Sem : Root_Sem_Ptr;
      Use_Short_Form : Boolean := False)
      return String;
   --  Return image of type, presuming Type_Sem designates a type

   function Equiv_Interps (Interp1, Interp2 : Optional_Tree) return Boolean;
   --  Return True if Interp1 and Interp2 are equivalent as far
   --  as overload resolution.

   function Prefix (Name : Optional_Tree) return Optional_Tree;
   --  Return prefix of Qualified_Name, Selection, or Property
   --  Returns Null_Optional_Tree if given an Identifier
   --  Requires: Name is Qualified_Name, Selection, Property, or an Identifier

   function Suffix (Name : Optional_Tree) return Optional_Tree;
   --  Return suffix of Qualified_Name, Selection (Id or Selector), or Property
   --  Return tree itself if given an Identifier
   --  Result should be an Identifier
   --  Requires: Name is Qualified_Name, Selection, Property, or an Identifier

   Routine_Sym_Table : array (Interpreter.Routine_Index) of Symbols.Sym_Ptr;
   --  Table of symbols associated with given routine

   subtype Debug_Index is Integer range 0 .. 9;
   --  0 = all,
   --  1 = Sem pass 1
   --  2 = Sem pass 2
   --  3 = Pre CG
   --  4 = CG
   --  5 = Interpreter threading
   --  6 = Interpreter region management
   --  7 = Interpreter call trace
   --  8 = Interpreter thread statistics
   --  9 = Truly everything, including Matching and Substitution

   procedure Turn_On_Debugging (Which : Debug_Index := 0);
   --  Turn on some or all debugging

   procedure Turn_Off_Debugging (Which : Debug_Index := 0);
   --  Turn off some or all debugging

   --  Whether to create listing of PSVM code for each routine
   List_Routine_Instructions : Boolean := True;

   --  Whether to insert implicit parallel calls into expressions automatically
   Insert_Implicit_Parallel_Calls : Boolean := True;

   --  Whether to insert implicit parallel loops automatically
   Insert_Implicit_Parallel_Loops : Boolean := False;

end PSC.Trees.Semantics;
