------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with PSC.Interpreter;
with PSC.Object_Access;
with PSC.Source_Positions;
with PSC.Symbols;

with PSC.Trees.Lists;

with PSC.Trees.Semantics.Info; use PSC.Trees.Semantics.Info;

private package PSC.Trees.Semantics.Dynamic is

   --  Dynamic Semantics and PSVM Code Generation

   use type Info.Static_Level;

   ------------- Apply pre-codegen actions -------------

   type Visitor_Annotation_Mode_Enum is
     (Normal_Mode,         --  "Normal" walk
      Precondition_Mode,   --  Walk expecting a precondition
      Entry_Temp_Mode,     --  Walk to initialize entry temps
      Postcondition_Mode_No_Check,     --  Walk expecting a postcondition
      Postcondition_Mode_With_Check);  --  Walk expecting a postcondition and
                                       --  check that it uses some exit value

   procedure Pre_Cg
     (Decl : Optional_Tree;
      Read_Write : in out Object_Access.Read_Write_Mapping;
      How_Combined : Object_Access.Read_Write_Combination_Enum :=
        Object_Access.Sequential;
      Mode : Object_Access.Access_Mode_Enum := Object_Access.Read_Access;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode);
   --  Pre code-gen pass for Decl

   procedure Pre_Cg_List
     (Decl_List : Lists.List;
      Read_Write : in out Object_Access.Read_Write_Mapping;
      How_Combined : Object_Access.Read_Write_Combination_Enum :=
        Object_Access.Sequential;
      Mode : Object_Access.Access_Mode_Enum := Object_Access.Read_Access;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode);
   --  Apply Pre_CG to each element in Decl_List

   ------------- Apply Codegen actions -------------

   procedure Code_Gen (R : Symbols.Region_Ptr; Decl : Optional_Tree;
     Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index);
   --  Generate PSVM code for Decl

   procedure Code_Gen_List (R : Symbols.Region_Ptr; Decl_List : Lists.List);
   --  Apply Code_Gen to each element in Decl_List

   ------------- Finish up code generation --------------

   function Run_Time_Type_Info
     (Obj_Type : Type_Sem_Ptr;
      Referring_Module : Module_Sem_Ptr := null;
      Formal_Type : Type_Sem_Ptr := null;
      Source_Pos : Source_Positions.Source_Position :=
     Source_Positions.Null_Source_Position;
      Is_Polymorphic_Type_Id : Boolean := False;
      Polymorphic_Param_Index : Integer := 0;
      For_Val_Param_Or_Const : Boolean := False)
      return Interpreter.Object_Locator;
   --  Return pointer to type information
   --  If Referring_Module is non-null, may assume that Type_Area
   --  is set up to point to a type for the referring module.
   --  If Formal_Type is non-null, create op-map
   --  relative to formal type and locator for op-map type-descriptor
   --  if necessary.
   --  If Is_Polymorphic_Type_Id is True, then we want a locator for
   --  a polymorphic type descriptor.
   --  If Polymorphic_Param_Index is not zero,
   --  specified parameter is polymorphic and its type-id
   --  should be used as the static link.  If it is negative,
   --  param is passed by reference, so an extra level of indirection
   --  is required.
   --  If For_Val_Param_Or_Const is True, then return locator for
   --  the type-desc to use when fetching a value-param or a constant; this
   --  is relevant only if Obj_Type is a formal type, and it has
   --  value parameters or nested constants, in which case we use the nested
   --  index of Obj_Type rather than its formal type index, as that is
   --  where the descriptor for the appropriate ancestor of the actual
   --  type passed for Obj_Type (the one for the ancestor from the same
   --  module as Obj_Type itself, rather than from some module that extends
   --  or implements Obj_Type's module).

   function Build_Type_Op_Map
     (Obj_Type : Type_Sem_Ptr;
      Formal_Type : Type_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position)
      return Interpreter.Type_Descriptor_Ptr;
   --  Build an "op-map" if needed for the given Formal_Type.
   --  Return null if op-map not needed.

   procedure Finish_Cur_Inst_Param_Info;
   --  Fill in Cur_Inst_Param_Info for all type descriptors

   procedure Evaluate_Global_Constants;
   --  Go through compile_time_known constants and evaluate them

   procedure Finish_Global_Constants;
   --  Go through compile_time_known constants again and make
   --  copies, as needed

   procedure Finish_All_Type_Descriptors;
   --  Finish remaining not-completely-finished type descriptors.

   procedure Finish_Type_Descriptor
     (Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False);
   --  Finish up formal object parameters and nested constants
   --  If Return_On_Recursion is True, do not complain about recursion
   --  and simply return immediately.

   ------------- Converters to run-time representation ------------

   function Literal_Value (Lit : String) return Interpreter.Word_Type;
   --  Return value used to represent given literal

   function String_Is_Convertible
     (Lit : String;
      Param_Type_Desc : Interpreter.Type_Descriptor_Ptr)
      return Boolean;
   --  Return True if given Lit can be converted to
   --  parameter with given type desc, using Convert_String.

   function Convert_String
     (Lit : String;
      Param_Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Local_Stg_Rgn : Interpreter.Stg_Rgn_Ptr)
      return Interpreter.Word_Type;
   --  Return value of Str after converting to the
   --  type of the given param.
   --  Requires: String_Is_Convertible(Lit, Param_Type_Desc)

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

end PSC.Trees.Semantics.Dynamic;
