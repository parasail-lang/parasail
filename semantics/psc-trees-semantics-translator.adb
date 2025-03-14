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

with Ada.Unchecked_Conversion;
with System.Storage_Elements;

with PSC.Interpreter.Builtins;
with PSC.Languages;
with PSC.Messages;
with PSC.Per_File_Strings;
with PSC.Strings;
with PSC.String_Streams;
with PSC.Symbols;
with PSC.Trees.Lists;
with PSC.Trees.Semantics.Dynamic;
with PSC.Trees.Semantics.Info;
with PSC.Trees.Semantics.Static;
with PSC.Univ_Strings;

with PSC.Trees.Unary;
with PSC.Trees.Binary;
with PSC.Trees.Identifier;

pragma Elaborate_All (PSC.Interpreter.Builtins);
pragma Elaborate_All (PSC.Strings);
package body PSC.Trees.Semantics.Translator is
   --  Package providing support for writing a translator
   --  from PSVM instructions to a compilable language.

   use Interpreter;
   use Semantics.Info;

   type Decl_Kind_Enum is  --  Enumeration of kinds of decls
     (Module_Kind, Type_Kind, Object_Kind, Operation_Kind);

   type Decl_Context_Enum is  --  Enumeration of where decls can appear
     (Exported_Context, Inherited_Context,
      Local_Context, Implements_Context);

   Library : Lists.List renames Symbols.Library_Region.Stmt_List;

   --  Breakpoint index and table  --

   type Breakpoint_Count is range 0 .. 100;
   subtype Breakpoint_Index is
     Breakpoint_Count range 1 .. Breakpoint_Count'Last;

   --  Routine and line# where a given breakpoint is set.
   type Breakpoint is record
      Op : Routine_Ptr := null;
      Line : Source_Positions.Line_Number := 0;
   end record;

   Breakpoints : array (Breakpoint_Index) of Breakpoint;

   Last_Breakpoint : Breakpoint_Count := 0;

   --------

   Code_Offset_Array_Name : constant Strings.U_String := Strings.String_Lookup
      ("PSL::Containers::Basic_Array" &
          "<PSC::Reflection::Instruction::Code_Offset>");

   Unsigned_64_Array_Name : constant Strings.U_String := Strings.String_Lookup
      ("PSL::Containers::Basic_Array<PSL::Core::Unsigned_64>");

   function Object_Locator_Type_Desc (Instr_Type_Desc : Non_Op_Map_Type_Ptr)
     return Non_Op_Map_Type_Ptr is
      --  Return type descriptor for Object_Locator given the type
      --  descriptor for the Instruction module.
   begin
      --  We presume Object_Locator is type of second component of Instruction
      return Instr_Type_Desc.Components (2).Type_Desc;
   end Object_Locator_Type_Desc;

   ------------ Exported operations ------------
   procedure PFS_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, PFS_Create, "_psc_pfs_create");

   procedure PFS_Get_Local_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, PFS_Get_Local_Index, "_psc_pfs_get_local_index");

   procedure PFS_Num_Elems
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, PFS_Num_Elems, "_psc_pfs_num_elems");

   procedure PFS_Nth_Elem
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, PFS_Nth_Elem, "_psc_pfs_nth_elem");

   procedure PFS_Reset
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, PFS_Reset, "_psc_pfs_reset");

   procedure Num_Library_Items
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Num_Library_Items, "_psc_num_library_items");

   procedure Nth_Library_Item
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Nth_Library_Item, "_psc_nth_library_item");

   procedure Decl_Id
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Id, "_psc_decl_id");

   procedure Decl_Module_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Module_Name, "_psc_decl_module_name");

   procedure Decl_Num_Prior_Homonyms
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Num_Prior_Homonyms,
      "_psc_decl_num_prior_homonyms");

   procedure Decl_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Kind, "_psc_decl_kind");

   procedure Decl_Is_Spec
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Is_Spec, "_psc_decl_is_spec");

   procedure Decl_Spec
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Spec, "_psc_decl_spec");

   procedure Decl_Context
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Context, "_psc_decl_context");

   procedure Decl_Level
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Level, "_psc_decl_level");

   procedure Decl_Location
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Location, "_psc_decl_location");

   procedure Decl_Tree_Of
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Tree_Of, "_psc_decl_tree_of");

   procedure Decl_Component_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Component_Index, "_psc_decl_component_index");

   procedure Decl_Source_Pos
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Source_Pos, "_psc_decl_source_pos");

   procedure Operation_Equiv_To
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Operation_Equiv_To, "_psc_operation_equiv_to");

   procedure Routine_For_Operation
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_For_Operation, "_psc_routine_for_operation");

   procedure Descriptor_For_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Descriptor_For_Type, "_psc_descriptor_for_type");

   procedure Const_Is_Large_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Is_Large_Null, "_psc_const_is_large_null");

   procedure Const_Info_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Info_At_Locator, "_psc_const_info_at_locator");

   procedure Const_Value_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Value_At_Locator, "_psc_const_value_at_locator");
      --  Return streamable value corresponding to given const-area locator

   procedure Const_Value_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Value_Locator, "_psc_const_value_locator");
      --  Return const-area locator associated with streamable value

   procedure Const_Info_For_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Info_For_Value, "_psc_const_info_for_value");

   procedure Const_Value_Init_Stream
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Const_Value_Init_Stream,
      "_psc_const_value_init_stream");

   procedure Global_Const_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Global_Const_Value, "_psc_global_const_value");

   procedure Name_For_Object_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export
     (Ada, Name_For_Object_Locator, "_psc_name_for_object_locator");

   procedure Region_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Kind, "_psc_region_kind");

   procedure Region_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Index, "_psc_region_index");

   procedure Region_Associated_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Associated_Decl, "_psc_region_associated_decl");

   procedure Region_Produces_Nested_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Produces_Nested_Block,
                  "_psc_region_produces_nested_block");

   procedure Region_Num_Items
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Num_Items, "_psc_region_num_items");

   procedure Region_Nth_Item
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Nth_Item, "_psc_region_nth_item");

   procedure Region_Num_Nested_Regions
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Num_Nested_Regions,
      "_psc_region_num_nested_regions");

   procedure Region_Nth_Nested_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Nth_Nested_Region,
      "_psc_region_nth_nested_region");

   procedure Region_Sibling_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Sibling_Region, "_psc_region_sibling_region");

   procedure Region_Num_Trees
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Num_Trees, "_psc_region_num_trees");

   procedure Tree_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Kind, "_psc_tree_kind");

   procedure Tree_Num_Operands
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Num_Operands, "_psc_tree_num_operands");

   procedure Tree_Nth_Operand
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Nth_Operand, "_psc_tree_nth_operand");

   procedure Tree_Pre_Annotation
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Pre_Annotation, "_psc_tree_pre_annotation");

   procedure Tree_Post_Annotation
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Post_Annotation, "_psc_tree_post_annotation");

   procedure Tree_Resolved_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Resolved_Type, "_psc_tree_resolved_type");

   procedure Tree_Decl_Of
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Decl_Of, "_psc_tree_decl_of");

   procedure Tree_Unary_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Unary_Op, "_psc_tree_unary_op");

   procedure Tree_Binary_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Binary_Op, "_psc_tree_binary_op");

   procedure Tree_Source_Pos
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Source_Pos, "_psc_tree_source_pos");

   procedure Tree_Lit_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Lit_Kind, "_psc_tree_lit_kind");

   procedure Tree_Identifier
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Tree_Identifier, "_psc_tree_identifier");

   procedure Region_Nth_Tree
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Region_Nth_Tree, "_psc_region_nth_tree");

   procedure Decl_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Decl_Region, "_psc_decl_region");

   procedure Body_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Body_Region, "_psc_body_region");

   procedure Source_Position_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Source_Position_Create, "_psc_source_position_create");

   procedure Source_Position_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Source_Position_File, "_psc_source_position_file");

   procedure Source_Position_Line
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Source_Position_Line, "_psc_source_position_line");

   procedure Source_Position_Col
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Source_Position_Col, "_psc_source_position_col");

   procedure Instruction_Opcode
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Opcode, "_psc_instruction_opcode");

   procedure Instruction_Source_Pos
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Source_Pos, "_psc_instruction_source_pos");

   procedure Instruction_Skip_Count
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Skip_Count, "_psc_instruction_Skip_Count");

   procedure Instruction_Skip_Counts
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Skip_Counts,
      "_psc_instruction_Skip_Counts");

   procedure Instruction_Level_Diff
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Level_Diff, "_psc_instruction_Level_Diff");

   procedure Instruction_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Params, "_psc_instruction_Params");

   procedure Instruction_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Static_Link,
      "_psc_instruction_Static_Link");

   procedure Instruction_Call_Target
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Call_Target,
      "_psc_instruction_Call_Target");

   procedure Instruction_Target_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Target_Index,
      "_psc_instruction_Target_Index");

   procedure Instruction_Locked_Param_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Locked_Param_Index,
      "_psc_instruction_Locked_Param_Index");

   procedure Instruction_Locked_Param_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Locked_Param_Info,
      "_psc_instruction_Locked_Param_Info");

   procedure Instruction_Locked_Param_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Locked_Param_Is_Var,
      "_psc_instruction_Locked_Param_Is_Var");

   procedure Instruction_Locked_Param_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Locked_Param_Is_By_Ref,
      "_psc_instruction_Locked_Param_Is_By_Ref");

   procedure Instruction_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Code_Block, "_psc_instruction_Code_Block");

   procedure Instruction_Assertion_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Assertion_Str,
      "_psc_instruction_Assertion_Str");

   procedure Instruction_Destination
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Destination,
      "_psc_instruction_Destination");

   procedure Instruction_Dest_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Dest_Name, "_psc_instruction_Dest_Name");

   procedure Instruction_Decl_Obj_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Decl_Obj_Is_By_Ref,
      "_psc_instruction_Decl_Obj_Is_By_Ref");

   procedure Instruction_Decl_Obj_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Decl_Obj_Is_Var,
      "_psc_instruction_Decl_Obj_Is_Var");

   procedure Instruction_Declare_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Declare_Type_Info,
      "_psc_instruction_Declare_Type_Info");

   procedure Instruction_Null_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Null_Type_Info,
      "_psc_instruction_Null_Type_Info");

   procedure Instruction_Local_Addr
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Local_Addr, "_psc_instruction_Local_Addr");

   procedure Instruction_Int_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Int_Value, "_psc_instruction_Int_Value");

   procedure Instruction_Char_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Char_Value, "_psc_instruction_Char_Value");

   procedure Instruction_Real_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Real_Value, "_psc_instruction_Real_Value");

   procedure Instruction_Str_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Str_Value, "_psc_instruction_Str_Value");

   procedure Instruction_Existing_Str_In_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Existing_Str_In_Stg_Rgn,
     "_psc_instruction_Existing_Str_In_Stg_Rgn");

   procedure Instruction_Enum_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Enum_Value, "_psc_instruction_Enum_Value");

   procedure Instruction_Operation_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Operation_Static_Link,
      "_psc_instruction_Operation_Static_Link");

   procedure Instruction_Operation_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Operation_Locator,
      "_psc_instruction_Operation_Locator");

   procedure Instruction_Source
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Source, "_psc_instruction_Source");

   procedure Instruction_Might_Be_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Might_Be_Null,
     "_psc_instruction_Might_Be_Null");

   procedure Instruction_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Type_Info, "_psc_instruction_Type_Info");

   procedure Instruction_Existing_Obj_In_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Existing_Obj_In_Stg_Rgn,
      "_psc_instruction_Existing_Obj_In_Stg_Rgn");

   procedure Instruction_Source_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Source_Type_Info,
      "_psc_instruction_Source_Type_Info");

   procedure Instruction_Ancestor_Lvalue
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Ancestor_Lvalue,
      "_psc_instruction_Ancestor_Lvalue");

   procedure Instruction_Polymorphic_Ancestor_Lvalue
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Polymorphic_Ancestor_Lvalue,
      "_psc_instruction_Polymorphic_Ancestor_Lvalue");

   procedure Instruction_If_Source
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_If_Source, "_psc_instruction_If_Source");

   procedure Instruction_If_Condition
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_If_Condition,
      "_psc_instruction_If_Condition");

   procedure Instruction_Skip_If_False
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Skip_If_False,
      "_psc_instruction_Skip_If_False");

   procedure Instruction_Parallel_Master
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Master,
      "_psc_instruction_Parallel_Master");

   procedure Instruction_Parallel_Control
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Control,
      "_psc_instruction_Parallel_Control");

   procedure Instruction_Parallel_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Static_Link,
      "_psc_instruction_Parallel_Static_Link");

   procedure Instruction_Parallel_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Code_Block,
      "_psc_instruction_Parallel_Code_Block");

   procedure Instruction_Parallel_Call_Target
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Call_Target,
      "_psc_instruction_Parallel_Call_Target");

   procedure Instruction_Parallel_Target_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Target_Index,
      "_psc_instruction_Parallel_Target_Index");

   procedure Instruction_Parallel_Locked_Param_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Locked_Param_Index,
      "_psc_instruction_Parallel_Locked_Param_Index");

   procedure Instruction_Parallel_Locked_Param_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Locked_Param_Info,
      "_psc_instruction_Parallel_Locked_Param_Info");

   procedure Instruction_Parallel_Locked_Param_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Locked_Param_Is_Var,
      "_psc_instruction_Parallel_Locked_Param_Is_Var");

   procedure Instruction_Parallel_Locked_Param_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Locked_Param_Is_By_Ref,
      "_psc_instruction_Parallel_Locked_Param_Is_By_Ref");

   procedure Instruction_Parallel_Is_Queued_Call
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Parallel_Is_Queued_Call,
      "_psc_instruction_Parallel_Is_Queued_Call");

   procedure Instruction_Num_In_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export
     (Ada, Instruction_Num_In_Params, "_psc_instruction_Num_In_Params");

   procedure Instruction_Num_Out_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export
     (Ada, Instruction_Num_Out_Params, "_psc_instruction_Num_Out_Params");

   procedure Instruction_Case_Selector
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Case_Selector,
      "_psc_instruction_Case_Selector");

   procedure Instruction_Case_First
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Case_First, "_psc_instruction_Case_First");

   procedure Instruction_Case_Last
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Case_Last, "_psc_instruction_Case_Last");

   procedure Instruction_Case_Default_Skip
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Case_Default_Skip,
      "_psc_instruction_Case_Default_Skip");

   procedure Instruction_Nested_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Nested_Code_Block,
      "_psc_instruction_Nested_Code_Block");

   procedure Instruction_Nested_Block_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Nested_Block_Region,
      "_psc_instruction_Nested_Block_Region");

   procedure Instruction_Output_Inited_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Output_Inited_Null,
      "_psc_instruction_Output_Inited_Null");

   procedure Instruction_Proved
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Instruction_Proved, "_psc_instruction_Proved");

   procedure New_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, New_Conv_Desc, "_psc_new_conv_desc");

   procedure Null_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Null_Conv_Desc, "_psc_null_conv_desc");

   procedure CD_Convention
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, CD_Convention, "_psc_cd_convention");

   procedure CD_Num_Inputs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, CD_Num_Inputs, "_psc_cd_num_inputs");

   procedure CD_Num_Outputs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, CD_Num_Outputs, "_psc_cd_num_outputs");

   procedure CD_Output_Needs_Init
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, CD_Output_Needs_Init, "_psc_cd_output_needs_init");

   procedure CD_Uses_Queuing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, CD_Uses_Queuing, "_psc_cd_uses_queuing");

   procedure Routine_Uses_Queuing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Uses_Queuing, "_psc_routine_uses_queuing");

   procedure Routine_Uses_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Uses_Stg_Rgn, "_psc_routine_uses_stg_rgn");

   procedure Routine_Num_VM_Regs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Num_VM_Regs, "_psc_routine_num_vm_regs");

   procedure Routine_Num_Instrs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Num_Instrs, "_psc_routine_num_instrs");

   procedure Routine_Nth_Instr
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Nth_Instr, "_psc_routine_nth_instr");

   procedure Routine_Start_Callee_Locals
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Start_Callee_Locals,
      "_psc_routine_start_callee_locals");

   procedure Routine_Nesting_Level
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Nesting_Level, "_psc_routine_nesting_level");

   procedure Routine_Convention
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Convention, "_psc_routine_convention");

   procedure Routine_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Conv_Desc, "_psc_routine_conv_desc");

   procedure Routine_Internal_Precond
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Internal_Precond,
      "_psc_routine_internal_precond");

   procedure Routine_Module_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Module_Name, "_psc_routine_module_name");

   procedure Routine_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Name, "_psc_routine_name");

   procedure Routine_Num_Prior_Homonyms
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Num_Prior_Homonyms,
      "_psc_routine_num_prior_homonyms");

   procedure Routine_Name_With_Overloading_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Name_With_Overloading_Index,
      "_psc_routine_name_with_overloading_index");

   procedure Routine_Built_In_Desig
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Built_In_Desig, "_psc_routine_built_in_desig");

   procedure Routine_Enc_Type_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_Enc_Type_Desc, "_psc_routine_enc_type_desc");

   procedure Routine_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_At_Locator, "_psc_routine_at_locator");

   procedure Routine_At_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Routine_At_Index, "_psc_routine_at_index");

   procedure Set_Breakpoint
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Set_Breakpoint, "_psc_set_breakpoint");

   procedure Clear_Breakpoint
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Clear_Breakpoint, "_psc_clear_breakpoint");

   procedure Num_Breakpoints
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Num_Breakpoints, "_psc_num_breakpoints");

   procedure Nth_Breakpoint_Routine
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Nth_Breakpoint_Routine, "_psc_nth_breakpoint_routine");

   procedure Nth_Breakpoint_Line
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Nth_Breakpoint_Line, "_psc_nth_breakpoint_line");

   procedure Type_Desc_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_At_Locator, "_psc_type_desc_at_locator");

   procedure Type_Desc_At_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_At_Index, "_psc_type_desc_at_index");

   procedure Type_Desc_Has_Op_Map
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Has_Op_Map, "_psc_type_desc_has_op_map");

   procedure Type_Desc_Corresponding_Polymorphic_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Corresponding_Polymorphic_Type,
      "_psc_type_desc_polymorphic_type");

   procedure Type_Desc_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Name, "_psc_type_desc_name");

   procedure Type_Desc_Type_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Type_Decl, "_psc_type_desc_type_decl");

   procedure Type_Desc_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Index, "_psc_type_desc_index");

   procedure Type_Desc_Init_Stream
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Init_Stream, "_psc_type_desc_init_stream");

   procedure Type_Desc_Type_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Type_Kind, "_psc_type_desc_kind");

   procedure Type_Desc_All_Parameters_Known
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_All_Parameters_Known,
      "_psc_type_desc_all_parameters_known");

   procedure Type_Desc_Is_Small
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Small, "_psc_type_desc_is_small");

   procedure Type_Desc_Is_Large
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Large, "_psc_type_desc_is_large");

   procedure Type_Desc_Is_Wrapper
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Wrapper, "_psc_type_desc_is_wrapper");

   procedure Type_Desc_Null_Value_For_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Null_Value_For_Type,
      "_psc_type_desc_null_value");

   procedure Type_Desc_Parent_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Parent_Type, "_psc_type_desc_parent_type");

   procedure Type_Desc_Is_Abstract
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Abstract, "_psc_type_desc_is_abstract");

   procedure Type_Desc_Is_Concurrent
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Concurrent,
      "_psc_type_desc_is_concurrent");

   procedure Type_Desc_Is_Polymorphic
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Is_Polymorphic,
      "_psc_type_desc_is_polymorphic");

   procedure Type_Desc_Enclosing_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Enclosing_Type,
      "_psc_type_desc_enclosing_type");

   procedure Type_Desc_Root_Type_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Type_Desc_Root_Type_Desc,
      "_psc_type_desc_root");

   procedure Op_Map_Actual_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Op_Map_Actual_Type, "_psc_op_map_actual_type");

   procedure Op_Map_Formal_Type_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Op_Map_Formal_Type_Decl,
   "_psc_op_map_formal_type_decl");

   procedure Info_Array_First
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Info_Array_First, "_psc_info_array_first");

   procedure Info_Array_Last
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Info_Array_Last, "_psc_info_array_last");

   procedure Info_Array_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Info_Array_Indexing, "_psc_info_array_indexing");

   ---------------

   function To_Word_Type (Sem : Root_Sem_Ptr) return Word_Type is
   --  Return "small" ParaSail object representation of a sem pointer
   begin
      if Sem = null then
         return Null_Value;
      else
         return From_Unsigned_Word (Unsigned_Word_Type
           (System.Storage_Elements.To_Integer (Sem.all'Address)));
      end if;
   end To_Word_Type;

   function To_Root_Sem_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Root_Sem_Ptr);

   function To_Root_Sem_Ptr (Val : Word_Type) return Root_Sem_Ptr is
   --  Convert ParaSail "small" obj representation back into a sem pointer.
   begin
      if Val = Null_Value then
         return null;
      else
         return To_Root_Sem_Ptr (System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address
              (To_Unsigned_Word (Val))));
      end if;
   end To_Root_Sem_Ptr;

   function To_Word_Type (Routine : Routine_Ptr) return Word_Type is
   --  Return "small" ParaSail object representation of a routine pointer
   begin
      if Routine = null then
         return Null_Value;
      else
         return From_Unsigned_Word (Unsigned_Word_Type
           (System.Storage_Elements.To_Integer (Routine.all'Address)));
      end if;
   end To_Word_Type;

   function To_Routine_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Routine_Ptr);

   function To_Routine_Ptr (Val : Word_Type) return Routine_Ptr is
   --  Convert ParaSail "small" obj representation back into a routine pointer.
   begin
      if Val = Null_Value then
         return null;
      else
         return To_Routine_Ptr (System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address
              (To_Unsigned_Word (Val))));
      end if;
   end To_Routine_Ptr;

   function To_Word_Type (Type_Desc : Type_Descriptor_Ptr) return Word_Type is
   --  Return "small" ParaSail object representation of a type descriptor
   begin
      if Type_Desc = null then
         return Null_Value;
      else
         return From_Unsigned_Word (Unsigned_Word_Type
           (System.Storage_Elements.To_Integer (Type_Desc.all'Address)));
      end if;
   end To_Word_Type;

   function To_Word_Type (Region : Symbols.Region_Ptr) return Word_Type is
   --  Return "small" ParaSail object representation of a Region pointer
      use type Symbols.Region_Ptr;
   begin
      if Region = null then
         return Null_Value;
      else
         return From_Unsigned_Word (Unsigned_Word_Type
           (System.Storage_Elements.To_Integer (Region.all'Address)));
      end if;
   end To_Word_Type;

   function To_Region_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Symbols.Region_Ptr);

   function To_Region_Ptr (Val : Word_Type) return Symbols.Region_Ptr is
   --  Convert ParaSail "small" obj representation back into a Region pointer.
   begin
      if Val = Null_Value then
         return null;
      else
         return To_Region_Ptr (System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address
              (To_Unsigned_Word (Val))));
      end if;
   end To_Region_Ptr;

   function To_Word_Type (Op : Optional_Tree) return Word_Type is
   --  Return "small" ParaSail object representation of a tree pointer
   begin
      if Op = Null_Optional_Tree then
         return Null_Value;
      else
         declare
            T : constant Tree_Ptr := Tree_Ptr_Of(Op);
         begin
            return From_Unsigned_Word (Unsigned_Word_Type
               (System.Storage_Elements.To_Integer (T.all'Address)));
         end;
      end if;
   end To_Word_Type;

   function To_Tree_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Tree_Ptr);

   function To_Optional_Tree (Val : Word_Type) return Optional_Tree is
   --  Convert ParaSail "small" obj representation back into a Optional_Tree.
   begin
      if Val = Null_Value then
         return Null_Optional_Tree;
      else
         return (Ptr => To_Tree_Ptr (System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address
              (To_Unsigned_Word (Val)))));
      end if;
   end To_Optional_Tree;

   function To_Word_Type (Src : PSC.Source_Positions.Source_Position) return Word_Type is
   begin
      return Word_Type (Src.File) * 2**32 +
         Word_Type (Src.Line) * 2**10 +
         Word_Type (Src.Col);
   end To_Word_Type;

   use Type_Descriptor_Ops;

   function Addr_To_Type_Descriptor_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Type_Descriptor_Ptr);

   function To_Type_Desc_Or_Op_Map (Val : Word_Type)
      return Type_Descriptor_Ptr is
   --  Convert ParaSail "small" obj rep back into a Type_Descriptor pointer.
   --  Do *not* skip over the op map, if any
   begin
      if Val = Null_Value
        or else Val mod (Word_Type'Size / System.Storage_Unit) /= 0
      then
         --  Null address, or something not on a word boundary
         return null;
      else
         declare
            Type_Desc : constant Type_Descriptor_Ptr :=
               Addr_To_Type_Descriptor_Ptr
                 (System.Storage_Elements.To_Address
                   (System.Storage_Elements.Integer_Address
                      (To_Unsigned_Word (Val))));
         begin
            if Type_Desc.Magic_Number /= Type_Indicator then
               --  Missing a "magic" indicator
               return null;
            else
               return Type_Desc;
            end if;
         end;
      end if;
   end To_Type_Desc_Or_Op_Map;

   function To_Type_Descriptor_Ptr (Val : Word_Type)
      return Non_Op_Map_Type_Ptr is
   --  Convert ParaSail "small" obj rep back into a Type_Descriptor pointer.
   --  Skip over any op map.
   begin
      if Val = Null_Value then
         return null;
      else
         return Skip_Over_Op_Map
           (Addr_To_Type_Descriptor_Ptr (System.Storage_Elements.To_Address
              (System.Storage_Elements.Integer_Address
                (To_Unsigned_Word (Val)))));
      end if;
   end To_Type_Descriptor_Ptr;

   type Info_Kind_Enum is (Type_Parameters_Kind, Actuals_Of_Formals_Kind,
     Components_Kind, Nested_Types_Kind, Nested_Objs_Kind, Operations_Kind,
     Type_Desc_Stream_Kind, Op_Map_Kind, Routine_Parameters_Kind,
     Routine_Uplevel_Refs_Kind, String_Stream_Kind, Const_Value_Stream_Kind,
     Large_Const_Component_Values);

   package PFS renames Per_File_Strings;

   function To_PFS_Ptr is new Ada.Unchecked_Conversion
     (Word_Ptr, PFS.Per_File_String_Table_Ptr);

   function To_Word_Ptr is new Ada.Unchecked_Conversion
     (PFS.Per_File_String_Table_Ptr, Word_Ptr);

   procedure PFS_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Create() -> Per_File_String_Table
      --    is import (#pfs_create)
      --    //  Create an empty per-file table
   begin
      Store_Word_Ptr (Params, 0, To_Word_Ptr (PFS.Create));
   end PFS_Create;

   procedure PFS_Get_Local_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Get_Local_Index
      --    (var Per_File_String_Table; Univ_String) -> Local_Index
      --    is import (#pfs_get_local_index)
      --  //  Return a local index to be used in LLVM code, as an index
      --  //  into a run-time table which will be initialized when the compiled
      --  //  module is loaded at run-time.
      PFST : constant PFS.Per_File_String_Table_Ptr :=  --  Passed by ref
        To_PFS_Ptr (Word_To_Word_Ptr
          (Content_Of_Physical_Address (Fetch_Word_Ptr (Params, 1))));
      Str : constant Strings.U_String_Index :=
        Strings.Index (Univ_Strings.To_U_String (Univ_Strings.From_Word_Type
                                      (Fetch_Word (Params, 2))));
      Result : PFS.Local_String_Index;
   begin
      Result := PFS.Get_Local_Index (PFST, Str);
      Store_Word (Params, 0, Word_Type (Result));
   end PFS_Get_Local_Index;

   procedure PFS_Num_Elems
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Elems(Per_File_String_Table) -> Local_Index
      --    is import (#pfs_num_elems)
      --  //  Number of strings in the table
      PFST : constant PFS.Per_File_String_Table_Ptr :=
        To_PFS_Ptr (Fetch_Word_Ptr (Params, 1));
   begin
      Store_Word (Params, 0,
        Word_Type (PFS.Num_Strings (PFST)));
   end PFS_Num_Elems;

   procedure PFS_Nth_Elem
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Elem(Per_File_String_Table; Local_Index) -> Univ_String
      --    is import (#pfs_nth_elem)
      --  //  Nth string in the table
      PFST : constant PFS.Per_File_String_Table_Ptr :=
        To_PFS_Ptr (Fetch_Word_Ptr (Params, 1));
      Index : constant PFS.Local_String_Index :=
        PFS.Local_String_Index (Fetch_Word (Params, 2));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word (PFS.Nth_String (PFST, Index), Target));
   end PFS_Nth_Elem;

   procedure PFS_Reset
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Reset (var optional Per_File_String_Table)
      --    is import (#pfs_reset)
      --  //  Set string table back to empty
      PFST : PFS.Per_File_String_Table_Ptr :=  --  Passed by ref
        To_PFS_Ptr (Word_To_Word_Ptr
          (Content_Of_Physical_Address (Fetch_Word_Ptr (Params, 0))));
   begin
      --  Reclaim storage
      PFS.Reset (PFST);

      --  Set word to null
      Store_Word (Fetch_Word_Ptr (Params, 0), 0, Null_Value);
   end PFS_Reset;

   function Create_Element_Info
     (Info : Element_Info;
      Elem_Info_Type : Type_Descriptor_Ptr;
      Target_Stg_Rgn : Stg_Rgn_Ptr;
      Server_Index   : Thread_Server_Index) return Word_Type;
   --  Create Elem_Info object

   procedure Num_Library_Items
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Library_Items(Env : Environment) -> Decl_Index
      --    is import(#num_library_items);

      --  Return count of library items
   begin
      Store_Word
        (Params, 0,
         Word_Type (Lists.Length (Library)));
   end Num_Library_Items;

   procedure Nth_Library_Item
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Library_Item(Env : Environment; Decl_Index) -> Decl
      --    is import(#nth_library_item);

      --  return reference to Nth top-level decl
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Sem_Info (Lists.Nth_Element
           (Library,
            Positive (Fetch_Word (Params, 2))))));
   end Nth_Library_Item;

   procedure Decl_Id
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Id(Decl) -> Univ_String
      --    is import(#decl_id);
      Target : constant Word_Type := Fetch_Word (Params, 0);
      Decl_Sem : Semantic_Info'Class renames
        Sem_Ptr (To_Root_Sem_Ptr (Fetch_Word (Params, 1))).all;
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word (Decl_Sem.Associated_Symbol.Str, Target));
   end Decl_Id;

   procedure Decl_Module_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Module_Name(Decl) -> Univ_String  //  full module name
      --    is import(#decl_module_name);
      Decl_Sem : Semantic_Info'Class renames
        Sem_Ptr (To_Root_Sem_Ptr (Fetch_Word (Params, 1))).all;
      use type Symbols.Sym_Ptr;
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if Decl_Sem.Associated_Symbol /= null then
         declare
            Mod_Name : constant Strings.U_String :=
              Symbols.Enclosing_Module_Full_Name (Decl_Sem.Associated_Symbol);
            use type Strings.U_String;
         begin
            if Mod_Name /= Strings.Null_U_String then
               Store_Word
                 (Params, 0,
                  To_Univ_String_Word (Mod_Name, Target));
               return;  --  All done  --
            end if;
         end;
      end if;

      --  Fall back to storing a null string
      Store_Word
        (Params, 0,
         To_Univ_String_Null (Target));
   end Decl_Module_Name;

   procedure Decl_Num_Prior_Homonyms
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Prior_Homonyms(Decl) -> Overloading_Index
      --    is import(#decl_num_prior_homonyms);
   begin
      Store_Word
        (Params, 0,
         Word_Type (Symbols.Num_Prior_Homonyms
           (Sem_Ptr (To_Root_Sem_Ptr
             (Fetch_Word (Params, 1))).Associated_Symbol)));
   end Decl_Num_Prior_Homonyms;

   procedure Decl_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Kind(Decl) -> Decl_Kind
      --    is import(#decl_kind);
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Kind : Decl_Kind_Enum;
   begin
      --  Determine kind of declaration
      if Decl_Sem in Module_Semantic_Info'Class then
         Kind := Module_Kind;
      elsif Decl_Sem in Type_Semantic_Info'Class then
         Kind := Type_Kind;
      elsif Decl_Sem in Object_Semantic_Info'Class then
         Kind := Object_Kind;
      elsif Decl_Sem in Operation_Semantic_Info'Class then
         Kind := Operation_Kind;
      else
         --  None of the above; return null.
         Store_Word
           (Params, 0,
            Null_Value);
         return;
      end if;

      Store_Word
        (Params, 0,
         Decl_Kind_Enum'Pos (Kind));
   end Decl_Kind;

   procedure Decl_Is_Spec
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Spec(Decl) -> Boolean
      --    is import(#decl_is_spec);
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Is_Spec : Boolean := False;
   begin
      --  Determine whether is a "spec" declaration
      if Decl_Sem in Module_Semantic_Info'Class then
         Is_Spec := Module_Semantic_Info (Decl_Sem).Is_Interface;
      elsif Decl_Sem in Type_Semantic_Info'Class then
         Is_Spec := True;
      elsif Decl_Sem in Object_Semantic_Info'Class then
         Is_Spec := True;
      elsif Decl_Sem in Operation_Semantic_Info'Class then
         Is_Spec := not Operation_Semantic_Info (Decl_Sem).Is_Def
          or else Operation_Semantic_Info (Decl_Sem).Is_Import;
      else
         --  None of the above; return False.
         Is_Spec := False;
      end if;

      Store_Word
        (Params, 0,
         Boolean'Pos (Is_Spec));
   end Decl_Is_Spec;

   procedure Decl_Spec
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Spec(Decl) -> Decl  // Self if spec, else corresponding spec
      --    is import(#decl_spec);
      Decl_Sem_Ptr : Root_Sem_Ptr :=
        To_Root_Sem_Ptr (Fetch_Word (Params, 1));
      Decl_Sem : Root_Semantic_Info'Class renames Decl_Sem_Ptr.all;
      Spec : Root_Sem_Ptr := Decl_Sem_Ptr;
   begin
      --  Determine whether is a "spec" declaration
      if Decl_Sem in Module_Semantic_Info'Class then
         if not Module_Semantic_Info (Decl_Sem).Is_Interface then
            Spec := Root_Sem_Ptr (Module_Semantic_Info (Decl_Sem).Other_Part);
         end if;
      elsif Decl_Sem in Operation_Semantic_Info'Class
        and then Operation_Semantic_Info (Decl_Sem).Spec_Sem /= null
      then
         Spec := Root_Sem_Ptr (Operation_Semantic_Info (Decl_Sem).Spec_Sem);
      end if;

      Store_Word
        (Params, 0,
         To_Word_Type (Spec));
   end Decl_Spec;

   procedure Decl_Context
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Context(Decl) -> Context_Enum
      --    is import(#decl_context);
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Info_Context : Context_Enum := No_Context;
      Context_Map : constant array (Context_Enum) of Decl_Context_Enum :=
        (Module_Implements_Interfaces_Context => Implements_Context,
         Ancestor_Implements_Item_Context => Inherited_Context,
         Ancestor_Item_Context => Inherited_Context,
         Interface_Item_Context => Exported_Context,
         Interface_Implements_Item_Context => Implements_Context,
         Local_Class_Item_Context => Local_Context,
         Exported_Class_Item_Context => Exported_Context,
         Class_Implements_Item_Context => Implements_Context,
         Standalone_Item_Context => Exported_Context,
         others => Local_Context);
   begin
      --  Determine context where declaration appears
      if Decl_Sem in Semantic_Info'Class then
         Info_Context := Semantic_Info (Decl_Sem).Context;
      end if;

      Store_Word
        (Params, 0,
         Decl_Context_Enum'Pos (Context_Map (Info_Context)));
   end Decl_Context;

   procedure Decl_Level
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Level(Decl) -> Static_Level
      --    is import(#decl_level);
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Level : Info.Static_Level := 0;
   begin
      --  Determine level associated with declaration
      if Decl_Sem in Operation_Semantic_Info'Class then
         Level := Operation_Semantic_Info (Decl_Sem).Level;
      elsif Decl_Sem in Object_Semantic_Info'Class then
         Level := Object_Semantic_Info (Decl_Sem).Info.Obj_Level;
      elsif Decl_Sem in Computation_Semantic_Info'Class then
         Level := Computation_Semantic_Info (Decl_Sem).Level;
      end if;

      Store_Word
        (Params, 0,
         Word_Type (Level));
   end Decl_Level;

   procedure Decl_Convention
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Convention(Decl) -> Convention_Enum
      --    is import(#decl_convention)
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Convention : Languages.Convention_Enum :=
                     Languages.Convention_Internal_Default;
   begin
      --  Determine level associated with declaration
      if Decl_Sem in Operation_Semantic_Info'Class then
         Convention := Operation_Semantic_Info (Decl_Sem).Convention;
      end if;

      Store_Word
        (Params, 0,
         Languages.Convention_Enum'Pos (Convention));
   end Decl_Convention;

   procedure Decl_Tree_Of
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Tree_Of(Decl) -> Tree
      --    is import(#decl_tree_of)
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Result_Op : constant Optional_Tree := Decl_Sem.Definition;
   begin
      Store_Word
        (Params, 0, To_Word_Type (Result_Op));
   end Decl_Tree_Of;

   procedure Decl_Location
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Location(Decl) -> Object_Locator
      --    is import(#decl_location);
      --  NOTE: This is actually declared in Object_Locator module
      --        so Static_Link refers to Object_Locator
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Location : Object_Locator := Null_Object_Locator;
   begin
      --  Determine object locator associated with declaration
      if Decl_Sem in Type_Semantic_Info'Class then
         declare
            Type_Sem : Type_Semantic_Info'Class renames
              Type_Semantic_Info'Class (Decl_Sem);
         begin
            if Type_Sem.U_Base_Type /= null then
               Location := Type_Sem.U_Base_Type.Type_Descriptor_Location;
            else
               Location := Type_Sem.Type_Descriptor_Location;
            end if;
         end;
      elsif Decl_Sem in Object_Semantic_Info'Class and then
        Object_Semantic_Info (Decl_Sem).Info /= null
      then
         Location := Object_Semantic_Info (Decl_Sem).Info.Obj_Location;
      elsif Decl_Sem in Operation_Semantic_Info'Class and then
        Operation_Semantic_Info (Decl_Sem).Routine /= null
      then
         Location := (Zero_Base,
           Offset_Within_Area
             (Operation_Semantic_Info (Decl_Sem).Routine.Index),
           No_VM_Obj_Id);
      end if;

      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Location,
            Static_Link,  --  Assumes declared inside of module Object_Locator
            Fetch_Word (Params, 0),
            Context.Server_Index));
         --  Word_Type (Location.Base) * 2**32 + Word_Type (Location.Offset));
   end Decl_Location;

   procedure Decl_Component_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Component_Index(Decl) -> optional Offset_Within_Area
      --   is import(#decl_component_index)
      --   //  Returns offset if this is a component, otherwise null.
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
      Result : Word_Type;
   begin
      --  Determine component index, if any associated with declaration
      --  TBD: Need to worry about Module_Extension_Level as well.
      if Decl_Sem in Object_Semantic_Info'Class
        and then Object_Semantic_Info (Decl_Sem).Component_Index /= 0
      then
         Result := Word_Type (Object_Semantic_Info (Decl_Sem).Component_Index);
      else
         Result := Null_Value;
      end if;
      Store_Word
        (Params, 0, Result);
   end Decl_Component_Index;

   procedure Decl_Source_Pos
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Decl_Source_Pos(Decl) -> Source_Position
      --    is import(#decl_source_pos);
      Decl_Sem : Semantic_Info'Class renames Semantic_Info'Class
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all);
      Source_Pos : constant Source_Positions.Source_Position :=
        Find_Source_Pos (Decl_Sem.Definition);
   begin
      Store_Word
        (Params, 0,
         Word_Type (Source_Pos.File) * 2**32 +
         Word_Type (Source_Pos.Line) * 2**10 +
         Word_Type (Source_Pos.Col));
   end Decl_Source_Pos;

   procedure Operation_Equiv_To
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Operation_Equiv_To(Decl {Kind(Decl) == #operation})
      --    -> optional Decl
      --    is import(#operation_equiv_to)
      Op_Sem : constant Operation_Sem_Ptr := Operation_Sem_Ptr
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)));
   begin
      Store_Word
        (Params, 0, To_Word_Type (Root_Sem_Ptr (Op_Sem.Equiv_To)));
   end Operation_Equiv_To;

   procedure Routine_For_Operation
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_For_Operation(Decl {Kind(Decl) == #operation})
      --    -> Routine
      --    is import(#routine_for_operation);
      Op_Sem : constant Operation_Sem_Ptr := Operation_Sem_Ptr
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Routine_Ptr (Op_Sem.Routine)));
   end Routine_For_Operation;

   procedure Descriptor_For_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Descriptor_For_Type(Decl)
      --    // Return type descriptor for fully-defined type
      --    {Kind(Decl) == #type}
      --    {Base(Location(Decl)) == Zero_Base}
      --    -> Type_Descriptor
      --    is import(#descriptor_for_type);
      Type_Sem : constant Type_Sem_Ptr := Type_Sem_Ptr
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)));
   begin
      if Type_Sem = null or else Type_Sem.U_Base_Type = null
        or else
          Type_Sem.U_Base_Type.Type_Descriptor_Location.Base /= Zero_Base
        or else
          Type_Sem.U_Base_Type.Type_Descriptor_Location.Offset = 0
      then
         --  No type-descriptor to be had; return a null
         Store_Word
           (Params, 0, Null_Value);
      else
         Store_Word
           (Params, 0,
            To_Word_Type (Known_Type_Desc
              (Type_Sem.U_Base_Type.Type_Descriptor_Location)));
      end if;
   end Descriptor_For_Type;

   procedure Const_Is_Large_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Const_Is_Large_Null (Value : Unsigned_64)
      --    -> Boolean
      --    is import (#is_large_null)
      Const_Value : constant Word_Type := Fetch_Word (Params, 1);
   begin
      Store_Word (Params, 0, Boolean'Pos (Is_Large_Null (Const_Value)));
   end Const_Is_Large_Null;

   procedure Const_Info_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Const_Info_At_Locator (Locator : Object_Locator)
      --    {Base(Locator) == Object_Locator::Const_Area}
      --    -> Const_Info
      --    is import (#const_info_at_locator)
      Locator_Int : constant Word_Type :=
        Fetch_Word (Params, 1);
      Locator : constant Object_Locator :=
        Extract_Object_Locator (Locator_Int);
         --  (Base => Area_Base_Indicator (Locator_Int / 2**32),
         --   Offset => Offset_Within_Area (Locator_Int mod 2**16),
         --   VM_Obj_Id => No_VM_Obj_Id);
      Target : constant Word_Type := Fetch_Word (Params, 0);
         --  Get "null" passed in to determine desired region
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
   begin
      if Locator.Base /= Const_Area then
         Store_Word (Params, 0, Null_For_Stg_Rgn (Target_Stg_Rgn));
      else
         declare
            Info : Const_Info
              renames Nth_Element (Compile_Time_Known_Consts,
                                   CTK_Info_Index (Locator.Offset)).Info;

            --  Create the Const_Info object
            Result : constant Word_Type :=
                      Create_Large_Obj
                        (Type_Desc    => Static_Link,
                         Stg_Rgn      => Target_Stg_Rgn,
                         Server_Index => Context.Server_Index);
         begin
            --  Set the "Name" field
            Store_Word
              (Result + Large_Obj_Header_Size,
               To_Univ_String_Word (Info.Name, Target));

            --  Fill in the "Data : Element_Info" field
            Store_Word (Result +
              Large_Obj_Header_Size + Offset_Within_Area'(1),
              Create_Element_Info
                (Info.Data,
                 Static_Link.Components (2).Type_Desc,
                 Target_Stg_Rgn,
                 Context.Server_Index));

            --  Return the result
            Store_Word (Params, 0, Result);
         end;
      end if;
   end Const_Info_At_Locator;

   procedure Const_Value_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  Return streamable value corresponding to given const-area locator
      --  func Const_Value_At_Locator (Locator : Object_Locator)
      --    -> optional Streamable_Value
      --    is import(#const_value_at_locator)

      Locator_Int : constant Word_Type :=
        Fetch_Word (Params, 1);
      Locator : constant Object_Locator :=
        Extract_Object_Locator (Locator_Int);
         --  (Base => Area_Base_Indicator (Locator_Int / 2**32),
         --  Offset => Offset_Within_Area (Locator_Int mod 2**16),
         --  VM_Obj_Id => No_VM_Obj_Id);
   begin
      if Locator.Base /= Const_Area then
         Store_Word (Params, 0, Null_Value);
      else
         --  Represent compile-time values by an offset in Const_Area
         Store_Word (Params, 0, Word_Type (Locator.Offset));
      end if;
   end Const_Value_At_Locator;

   procedure Const_Value_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  Return const-area locator associated with streamable value
      --   func Const_Value_Locator (Streamable_Value) -> Object_Locator
      --     is import(#const_value_locator)
      --   NOTE: Declared in Object_Locator module so has access
      --         to its type descriptor
      Offset_Value : constant Word_Type := Fetch_Nonnull_Word (Params, 1);
   begin
      --  Just take offset and add a "Const_Area" area indicator
      Store_Word (Params, 0,
         Insert_Object_Locator
           (Object_Locator'(Base => Const_Area,
                            Offset => Offset_Within_Area (Offset_Value),
                            VM_Obj_Id => No_VM_Obj_Id),  --  Location,
            Static_Link,  --  Assumes declared in Object_Locator module
            Fetch_Word (Params, 0),
            Context.Server_Index));
   end Const_Value_Locator;

   procedure Const_Info_For_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Const_Info_For_Value (Streamable_Value) -> Const_Info
      --    is import (#const_info_for_value)
      Info : Const_Info renames
        Nth_Element (Compile_Time_Known_Consts,
                     CTK_Info_Index (Fetch_Word (Params, 1))).Info;
      Target : constant Word_Type := Fetch_Word (Params, 0);
         --  Get "null" passed in to determine desired region
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);

      --  Create the Const_Info object
      Result : constant Word_Type :=
                Create_Large_Obj
                  (Type_Desc    => Static_Link,
                   Stg_Rgn      => Stg_Rgn_Of_Large_Obj (Target),
                   Server_Index => Context.Server_Index);
   begin
      --  Set the "Name" field
      Store_Word
        (Result + Large_Obj_Header_Size,
         To_Univ_String_Word (Info.Name, Target));

      --  Fill in the "Data : Element_Info" field
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(1),
        Create_Element_Info
          (Info.Data,
           Static_Link.Components (2).Type_Desc,
           Target_Stg_Rgn,
           Context.Server_Index));

      --  Return the result
      Store_Word (Params, 0, Result);
   end Const_Info_For_Value;

   procedure Const_Value_Init_Stream
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Init_Stream(Val : Streamable_Value; var Per_File_String_Table)
      --    -> Basic_Array<Unsigned_64>
      --  //  Initialize a byte-stream representation of the value
      --  //  Update the Per-File string table as appropriate
      --    is import(#const_value_init_stream)

      --  Get Val parameter
      Info : Const_Info renames
        Nth_Element (Compile_Time_Known_Consts,
                     CTK_Info_Index (Fetch_Word (Params, 1))).Info;
      Target : constant Word_Type := Fetch_Word (Params, 0);

      --  Get per-file string table param (passed by ref)
      PFST : constant PFS.Per_File_String_Table_Ptr :=
        To_PFS_Ptr (Word_To_Word_Ptr (Content_Of_Physical_Address
          (Fetch_Word_Ptr (Params, 2))));

      --  Get type descriptor for result
      Unsigned_64_Array_Type : constant Type_Descriptor_Ptr :=
        Type_Descriptor_Ops.Get_Type_Desc_By_Name (Unsigned_64_Array_Name);
   begin
      --  Generate the stream representation, updating the per-file string tab

      Store_Word (Params, 0,
        Interpreter.Generate_Value_Stream_Rep
          (Info, Unsigned_64_Array_Type, Target, Context.Server_Index, PFST));
   end Const_Value_Init_Stream;

   procedure Global_Const_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Value_Of_Global_Const(Decl)
      --    {Kind(Decl) == #object}
      --    {Base(Location(Decl)) == Const_Area}
      --    -> optional Streamable_Value
      --    is import(#global_const_value)
      Decl_Sem : Root_Semantic_Info'Class renames
        To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all;
   begin
      if Decl_Sem not in Object_Semantic_Info'Class
        or else Object_Semantic_Info (Decl_Sem).Info = null
        or else
          Object_Semantic_Info (Decl_Sem).Info.Obj_Location.Base /= Const_Area
        or else Object_Semantic_Info (Decl_Sem).Info.Obj_Level > 0
      then
         Store_Word (Params, 0, Null_Value);
      else
         --  Represent compile-time values by an offset in Const_Area
         Store_Word (Params, 0, Word_Type
             (Object_Semantic_Info (Decl_Sem).
                Info.Obj_Location.Offset));
      end if;
   end Global_Const_Value;

   procedure Name_For_Object_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Name_For_Object_Locator (Object_Locator) -> Univ_String
      --    is import(#name_for_object_locator)
      --     //  Return a string to be associated with given locator
      --     //  or return the empty string.  Only supports Const_Area so far.

      Locator_Int : constant Word_Type :=
        Fetch_Word (Params, 1);
      Locator : constant Object_Locator :=
        Extract_Object_Locator (Locator_Int);
         --  (Base => Area_Base_Indicator (Locator_Int / 2**32),
         --   Offset => Offset_Within_Area (Locator_Int mod 2**16),
         --   VM_Obj_Id => No_VM_Obj_Id);
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Strings.String_Lookup
              (Dynamic.Name_For_Object_Locator (Locator)), Target));
   end Name_For_Object_Locator;

   procedure Region_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Kind(Region) -> Region_Kind
      --    is import(#region_kind)
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      use Symbols;
   begin
      if Region = null then
         --  Region is null, so No_Region_Kind
         Store_Word
           (Params, 0, Region_Kind_Enum'Pos (No_Region_Kind));
      else
         Store_Word
           (Params, 0, Region_Kind_Enum'Pos (Region.Kind));
      end if;
   end Region_Kind;

   procedure Region_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Index(Region) -> Region_Index
      --    is import(#region_index)
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0,
                  Word_Type (Region.Index));
   end Region_Index;

   procedure Region_At_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Region_At_Index(Region_Index) -> Region
      --    is import(#region_at_index)
      --  //  Return Region given its unique index
      Index : constant Symbols.Region_Index :=
        Symbols.Region_Index (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0,
                  To_Word_Type (Symbols.Region_At_Index (Index)));
   end Region_At_Index;

   procedure Region_Associated_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Associated_Decl(Region) -> Decl
      --    is import(#region_associated_decl);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Result : constant Trees.Root_Sem_Ptr :=
        Region.Associated_Symbol.Sem_Info;
      Decl_Sem : Semantic_Info'Class renames Semantic_Info'Class (Result.all);
      use Symbols;
   begin
      if Region = null or else Region.Associated_Symbol = null
        or else Region.Associated_Symbol.Sem_Info = null
      then
         Store_Word (Params, 0, Null_Value);
      else
         Store_Word (Params, 0,
                     To_Word_Type (Region.Associated_Symbol.Sem_Info));
      end if;
   end Region_Associated_Decl;

   procedure Region_Produces_Nested_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Produces_Nested_Block(Region) -> Boolean
      --    is import(#region_produces_nested_block);
      --  //  Return #true if region becomes a nested block.
      --  //  This is generally true for right-hand-part of "||" and
      --  //  concurrent loop bodies.
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      use Symbols;
      Result : Boolean := False;
   begin
      case Region.Kind is
         when Loop_Body_Region_Kind =>
            --  See whether is a for-loop that
            --  uses a parallel nested block.
            if Region.Associated_Symbol /= null then
               declare
                  Comp_Sem : constant Computation_Sem_Ptr :=
                    Computation_Sem_Ptr (Region.Associated_Symbol.Sem_Info);
               begin
                  if Comp_Sem /= null
                    and then Comp_Sem.all in
                      For_Loop_Construct_Semantic_Info'Class
                  then
                     Result := For_Loop_Construct_Sem_Ptr
                                 (Comp_Sem).Uses_Parallel_Nested_Block;
                  else
                     Result := False;
                  end if;
               end;
            end if;
         when Parallel_Stmt_Region_Kind =>
            Result := True;
         when others =>
            Result := False;
      end case;

      Store_Word (Params, 0, Boolean'Pos (Result));
   end Region_Produces_Nested_Block;

   procedure Region_Num_Items
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Items(Region) -> Decl_Index
      --    is import(#region_num_items);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      use type Symbols.Region_Ptr;
   begin
      if Region = null then
         --  Region is null, so zero items
         Store_Word
           (Params, 0, 0);
      else
         --  Region is not null, so get symbol count
         Store_Word
           (Params, 0,
            Word_Type (Symbols.Num_Symbols_In_Region (Region)));
      end if;
   end Region_Num_Items;

   procedure Region_Nth_Item
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Item(Region, Decl_Index) -> Decl
      --    is import(#region_nth_item);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Result : constant Trees.Root_Sem_Ptr := Symbols.Nth_Symbol_In_Region
         (Region, Symbols.Sym_Index (Fetch_Word (Params, 2))).Sem_Info;
      Decl_Sem : Semantic_Info'Class renames Semantic_Info'Class (Result.all);
      use Symbols;
   begin
      if Decl_Sem.Associated_Symbol /= null then
         Store_Word (Params, 0, To_Word_Type (Result));
      else
         Store_Word (Params, 0, Null_Value);
      end if;
   end Region_Nth_Item;

   procedure Region_Num_Nested_Regions
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Nested_Regions(Region) -> Int
      --    is import(#region_num_nested_regions);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Syms : constant Symbols.Region_Symbols_Ptr := Region.Syms;
      use Symbols;
   begin
      if Syms /= null then
         Store_Word (Params, 0, Word_Type (Num_Symbols (Syms.Local_Symbols)));
      else
         Store_Word (Params, 0, 0);
      end if;
   end Region_Num_Nested_Regions;

   procedure Region_Nth_Nested_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Nested_Region(Region; Int) -> Region
      --    is import(#region_nested_region);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Index : constant Symbols.Symbol_Lists.Elem_Index :=
         Symbols.Symbol_Lists.Elem_Index (Fetch_Word (Params, 2));
      Syms : constant Symbols.Symbol_List := Region.Syms.Local_Symbols;
      use Symbols;
      Nested_Sym : constant Symbols.Sym_Ptr := Nth_Symbol (Syms, Index);
   begin
      --  Return null if this is the "class" part of a module,
      --  unless the interface part was in a different region.
      if Nested_Sym.Kind = Module_Sym_Kind
        and then Nested_Sym.Completion_Of /= null
        and then Nested_Sym.Completion_Of.Enclosing_Region = Region
      then
         Store_Word (Params, 0, Null_Value);
      else
         Store_Word
            (Params, 0, To_Word_Type (Nested_Sym.Nested_Region));
      end if;
   end Region_Nth_Nested_Region;

   procedure Region_Sibling_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Sibling_Region(Region) -> Region
      --    is import(#region_sibling_region);
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Result : constant Symbols.Region_Ptr := Region.Next_Sibling_Region;
   begin
      Store_Word (Params, 0, To_Word_Type (Result));
   end Region_Sibling_Region;

   procedure Region_Num_Trees
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Trees(Region) -> Tree_Index
      --    is import(#region_num_trees)
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Statements : constant Trees.Lists.List := Region.Stmt_List;
   begin
      Store_Word
         (Params, 0,
         Word_Type (Trees.Lists.Length (Statements)));
   end Region_Num_Trees;

   procedure Region_Nth_Tree
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Tree(Region; Tree_Index) -> optional Tree
      --    is import(#region_nth_tree)
      Region : constant Symbols.Region_Ptr :=
        To_Region_Ptr (Fetch_Word (Params, 1));
      Index : constant Integer := Integer (Fetch_Word (Params, 2));
      Statements : constant Trees.Lists.List := Region.Stmt_List;
   begin
      if Index > 0 and then Index <= Trees.Lists.Length (Statements) then
         Store_Word (Params, 0, To_Word_Type
            (Trees.Lists.Nth_Element (Statements, Index)));
      else
         Store_Word (Params, 0, Null_Value);
      end if;
   end Region_Nth_Tree;

   procedure Tree_Kind
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Kind(Tree) -> Tree_Kind
      --    is import(#tree_kind)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      --  Determine kind of tree
      if Not_Null(Op) then
         declare
            Op_Tree : constant Tree'Class := Tree_Of (Op);
         begin
            Store_Word
              (Params, 0,
               Tree_Kind_Enum'Pos (Kind (Op_Tree)));
         end;
      else
         Store_Word (Params, 0, Null_Value);
      end if;
   end Tree_Kind;

   procedure Tree_Num_Operands
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Operands(Tree) -> Tree_Index
      --    is import(#tree_num_operands)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Op = Null_Optional_Tree then
         Store_Word (Params, 0, 0);
      else
         declare
            Op_Tree : constant Tree'Class := Tree_Of (Op);
         begin
            Store_Word
               (Params, 0,
                Word_Type (Num_Operands (Op_Tree)));
         end;
      end if;
   end Tree_Num_Operands;

   procedure Tree_Nth_Operand
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Operand(Tree; Tree_Index) -> optional Tree
      --    is import(#tree_nth_operand)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
      Index : constant Word_Type := Fetch_Word (Params, 2);
   begin
      if Not_Null (Op) and then Index > 0 then
         declare
            Op_Tree : constant Tree'Class := Tree_Of (Op);
         begin
            if Natural(Index) <= Num_Operands(Op_Tree) then
               Store_Word
                  (Params, 0, To_Word_Type 
                   (Nth_Operand (Op_Tree, Positive(Index))));
               return;
            end if;
         end;
      end if;

      Store_Word
         (Params, 0, Null_Value);
   end Tree_Nth_Operand;

   procedure Tree_Pre_Annotation
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Pre_Annotation(Tree) -> optional Tree
      --    is import(#tree_pre_annotation)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Not_Null (Op) then
         declare
            Op_Tree : constant Tree'Class := Tree_Of (Op);
         begin
            Store_Word
              (Params, 0, To_Word_Type (Op_Tree.Pre_Annotation));
         end;
      else
         Store_Word (Params, 0, Null_Value);
      end if;
   end Tree_Pre_Annotation;

   procedure Tree_Post_Annotation
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Post_Annotation(Tree) -> optional Tree
      --    is import(#tree_post_annotation)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Not_Null (Op) then
         declare
            Op_Tree : constant Tree'Class := Tree_Of (Op);
         begin
            Store_Word
              (Params, 0, To_Word_Type (Op_Tree.Post_Annotation));
         end;
      else
         Store_Word (Params, 0, Null_Value);
      end if;
   end Tree_Post_Annotation;

   procedure Tree_Source_Pos
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Source_Pos(Tree) -> optional Source_Position
      --    is import(#tree_source_pos)
      use Source_Positions;
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
      Pos : constant Source_Positions.Source_Position :=
         Source_Pos (Op);
   begin
      if Pos = Source_Positions.Null_Source_Position then
         Store_Word
           (Params, 0, Null_Value);
      else
         Store_Word
           (Params, 0,
            To_Word_Type (Pos));
      end if;
   end Tree_Source_Pos;

   procedure Tree_Resolved_Type
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Resolved_Type(Tree) -> optional Type_Descriptor
      --    is import(#tree_resolved_type)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
      Op_Sem : constant Root_Sem_Ptr := Sem_Info (Op);
   begin
      if Op_Sem /= null and then 
         Op_Sem.all in Operand_Semantic_Info'Class
      then
         declare
            Opnd_Sem : constant Operand_Sem_Ptr :=
               Operand_Sem_Ptr (Op_Sem);
            Res_Type : constant Type_Sem_Ptr :=
               Opnd_Sem.Resolved_Type;
         begin
            if Res_Type /= null then
               Store_Word
               (Params, 0,
                  To_Word_Type (Get_Type_Desc
                  (Context, Res_Type.Type_Descriptor_Location)));
               return;
            end if;
         end;
      end if;

      Store_Word
        (Params, 0, Null_Value);
   end Tree_Resolved_Type;

   procedure Tree_Decl_Of
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Decl_Of(Tree) -> optional Decl
      --    is import(#tree_decl_of)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
      Op_Sem : constant Root_Sem_Ptr := Sem_Info (Op);
   begin
      if Op_Sem /= null and then 
         (Op_Sem.all in Module_Semantic_Info'Class or else
          Op_Sem.all in Type_Semantic_Info'Class or else
          Op_Sem.all in Object_Semantic_Info'Class or else
          Op_Sem.all in Operation_Semantic_Info'Class)
      then
         Store_Word
           (Params, 0, To_Word_Type (Op_Sem));
      else
         Store_Word
           (Params, 0, Null_Value);
      end if;
   end Tree_Decl_Of;

   procedure Tree_Unary_Op
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Unary_Op(Tree {Kind(Tree) == #unary}) -> Unary_Op_Kind
      --    is import(#tree_unary_op)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Not_Null (Op) then
         declare
            Op_Tree : Tree'Class renames Tree_Ptr_Of (Op).all;
         begin
            if Op_Tree in Trees.Unary.Tree then
               declare
                  Un_Tree : constant Trees.Unary.Tree :=
                     Trees.Unary.Tree (Op_Tree);
               begin
                  Store_Word
                     (Params, 0,
                      Trees.Unary.Unary_Operator_Enum'Pos (Un_Tree.Operator));
                  return;
               end;
            end if;
         end;
      end if;

      Store_Word
         (Params, 0, Null_Value);
   end Tree_Unary_Op;

   procedure Tree_Binary_Op
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Binary_Op(Tree {Kind(Tree) == #binary}) -> Binary_Op_Kind
      --    is import(#tree_binary_op)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Not_Null (Op) then
         declare
            Op_Tree : Tree'Class renames Tree_Ptr_Of (Op).all;
         begin
            if Op_Tree in Trees.Binary.Tree'Class then
               declare
                  Bin_Tree : constant Trees.Binary.Tree :=
                     Trees.Binary.Tree (Op_Tree);
                  Index : Word_Type := Trees.Binary.Binary_Operator_Enum'Pos
                     (Bin_Tree.Operator);
               begin
                  Store_Word
                     (Params, 0, Index - 1);
                  return;
               end;
            end if;
         end;
      end if;

      Store_Word
         (Params, 0, Null_Value);
   end Tree_Binary_Op;

   procedure Tree_Lit_Kind
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Lit_Kind(Tree {Kind(Tree) == #identifier}) -> optional Literal_Kind
      --    is import(#tree_lit_kind)
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
      Op_Sem : constant Root_Sem_Ptr := Sem_Info (Op);
   begin
      if Op_Sem /= null and then Op_Sem.all in Literal_Semantic_Info'Class then
         declare
            Lit_Sem : Literal_Sem_Ptr := Literal_Sem_Ptr (Op_Sem);
            Index : Word_Type := Literal_Kind_Enum'Pos (Lit_Sem.Lit_Kind);
         begin
            if Lit_Sem.Lit_Kind /= Not_A_Literal then
               Store_Word
                 (Params, 0, Index - 1);
               return;
            end if;
         end;
      end if;

      Store_Word
         (Params, 0, Null_Value);
   end Tree_Lit_Kind;

   procedure Tree_Identifier
      (Context : in out Exec_Context;
       Params : Word_Ptr;
       Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Identifier(Tree {Kind(Tree) == #identifier}) -> Univ_String
      --    is import(#tree_identifier)
      Target : constant Word_Type := Fetch_Word (Params, 0);
      Op : constant Optional_Tree :=
         To_Optional_Tree (Fetch_Word (Params, 1));
   begin
      if Not_Null (Op) then
         declare
            Op_Tree : Tree'Class renames Tree_Ptr_Of (Op).all;
         begin
            if Op_Tree in Trees.Identifier.Tree then
               declare
                  Ident_Tree : Trees.Identifier.Tree :=
                     Trees.Identifier.Tree (Op_Tree);
                  Word_Str : Word_Type := To_Univ_String_Word
                     (Ident_Tree.Str, Target);
               begin
                  Store_Word (Params, 0, Word_Str);
                  return;
               end;
            end if;
         end;
      end if;

      Store_Word
         (Params, 0, Null_Value);
   end Tree_Identifier;

   procedure Decl_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Decl_Region(Decl) -> optional Region
      --    is import(#decl_region);
      Decl_Sem : Semantic_Info'Class renames Semantic_Info'Class
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all);
      Decl_Sym : constant Symbols.Sym_Ptr := Decl_Sem.Associated_Symbol;
      Region : Symbols.Region_Ptr;
      use Symbols;
   begin
      if Decl_Sym /= null
        and then Decl_Sym.Nested_Region /= null
      then
         --  Get region directly from associated sym if there is one.
         Region := Decl_Sym.Nested_Region;
      elsif Decl_Sem in Type_Semantic_Info'Class then
         --  Get from Associated_Module's sym if this is a type.
         Region := Type_Semantic_Info'Class (Decl_Sem).Associated_Module.
                     Associated_Symbol.Nested_Region;
      end if;

      --  Return region associated with declaration (might be null)
      Store_Word (Params, 0, To_Word_Type (Region));
   end Decl_Region;

   procedure Body_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Body_Region(Decl) -> Region
      --    is import(#body_region);
      Decl_Sem : Semantic_Info'Class renames Semantic_Info'Class
        (To_Root_Sem_Ptr (Fetch_Word (Params, 1)).all);
      Region : Symbols.Region_Ptr := null;
      use type Symbols.Region_Ptr;
   begin
      --  Determine region associated with declaration
      if Decl_Sem in Operation_Semantic_Info'Class and then
        Operation_Semantic_Info (Decl_Sem).Body_Region /= null
      then
         --  Return body region if available
         Region := Operation_Semantic_Info (Decl_Sem).Body_Region;
      --  else leave it null
      end if;

      Store_Word (Params, 0, To_Word_Type (Region));
   end Body_Region;

   procedure Source_Position_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Create(File : Univ_String;
      --    Line : Univ_Integer; Col : Univ_Integer) -> Source_Position
      --    is import(#source_position_create);
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Univ_Strings.To_U_String
           (Univ_Strings.From_Word_Type
             (Fetch_Word (Params, 1)))) * 2**32 +
         Fetch_Word (Params, 2) * 2**10 +
         Fetch_Word (Params, 3));
   end Source_Position_Create;

   procedure Source_Position_File
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func File(Source_Position) -> Univ_String
      --    is import(#source_position_file);
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0, To_Univ_String_Word
          (Strings.U_String_Index (Fetch_Word (Params, 1) / 2**32),
           Target));
   end Source_Position_File;

   procedure Source_Position_Line
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Line(Source_Position) -> Univ_Integer
      --    is import(#source_position_line);
   begin
      Store_Word
        (Params, 0,
         Fetch_Word (Params, 1) / 2**10 mod 2**22);
   end Source_Position_Line;

   procedure Source_Position_Col
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Col(Source_Position) -> Univ_Integer
      --    is import(#source_position_col);
   begin
      Store_Word
        (Params, 0,
         Fetch_Word (Params, 1) mod 2**10);
   end Source_Position_Col;

   procedure Instruction_Opcode
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Opcode(Instruction) -> Opcode_Enum
      --    is import(#instruction_opcode);
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Opcode_Enum'Pos (Instr.Op));
   end Instruction_Opcode;

   procedure Instruction_Source_Pos
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Source_Pos(Instruction) -> Source_Position
      --    is import(#instruction_source_pos);
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      use Source_Positions;
      Line : Line_Number := Instr.Source_Pos.Line;
   begin
      if Line = 0 and then Instr.Source_Pos.End_Line > 0 then
         --  There seems to be a breakpoint set on this line,
         --  so fetch the "true" line number from the End_Line.
         Line := Instr.Source_Pos.End_Line;
      end if;

      Store_Word
        (Params, 0,
         Word_Type (Instr.Source_Pos.File) * 2**32 +
         Word_Type (Line) * 2**10 +
         Word_Type (Instr.Source_Pos.Col));
   end Instruction_Source_Pos;

   procedure Instruction_Skip_Count
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Skip_Count(Instruction) -> Code_Offset
      --    is import(#instruction_Skip_Count)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Skip_Count));
   end Instruction_Skip_Count;

   procedure Instruction_Skip_Counts
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Skip_Counts(Instruction) -> Basic_Array<Code_Offset>
      --    is import(#instruction_Skip_Count)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Null_For_Rgn : constant Word_Type := Fetch_Word (Params, 0);
      Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Null_For_Rgn);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      if Instr.Skip_Counts /= null then
         declare
            Type_Desc : constant Type_Descriptor_Ptr :=
               Type_Descriptor_Ops.Get_Type_Desc_By_Name
                 (Code_Offset_Array_Name);
         begin
            if Type_Desc /= null then
               declare
                  Result : constant Word_Type := Create_Basic_Array_Obj
                     (Type_Desc, Instr.Skip_Counts'Length,
                      Stg_Rgn, Context.Server_Index);
               begin
                  for I in Instr.Skip_Counts'First ..
                                                Instr.Skip_Counts'Last
                  loop
                     Store_Word (Virtual_To_Physical_Address (Result),
                        Large_Obj_Header_Size + Offset_Within_Area (I),
                        Word_Type (Instr.Skip_Counts (I)));
                  end loop;
                  Store_Word (Params, 0, Result);
               end;
            end if;
         end;
      end if;
   end Instruction_Skip_Counts;

   procedure Instruction_Level_Diff
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Level_Diff(Instruction) -> Natural
      --    is import(#instruction_Level_Diff)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Level_Diff));
   end Instruction_Level_Diff;

   procedure Instruction_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Params(Instruction) -> Object_Locator
      --    is import(#instruction_Params)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Params,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --     Word_Type (Instr.Params.Base) * 2**32 +
      --       Word_Type (Instr.Params.Offset));
   end Instruction_Params;

   procedure Instruction_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Static_Link(Instruction) -> Object_Locator
      --    is import(#instruction_Static_Link)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Static_Link,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Static_Link.Base) * 2**32 +
      --     Word_Type (Instr.Static_Link.Offset));
   end Instruction_Static_Link;

   procedure Instruction_Call_Target
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Call_Target(Instruction) -> Object_Locator
      --    is import(#instruction_Call_Target)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Call_Target,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Call_Target.Base) * 2**32 +
      --     Word_Type (Instr.Call_Target.Offset));
   end Instruction_Call_Target;

   procedure Instruction_Target_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Target_Index(Instruction) -> Routine_Index
      --    is import(#instruction_Target_Index)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      if Instr.Op = Call_Op then
         Store_Word
           (Params, 0,
            Word_Type (Instr.Target_Index));
      else
         pragma Assert (Instr.Op = Indirect_Call_Op);
         Store_Word
           (Params, 0, 0);
      end if;
   end Instruction_Target_Index;

   procedure Instruction_Locked_Param_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Locked_Param_Index(Instruction) -> Natural
      --    is import(#instruction_Locked_Param_Index)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Locked_Param_Info.Param_Index));
   end Instruction_Locked_Param_Index;

   procedure Instruction_Locked_Param_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Locked_Param_Info(Instruction) -> Locked_Param_Info_As_Byte_Type
      --    is import(#instruction_Locked_Param_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Info_Value : constant Word_Type range 0 .. 255 :=
        Word_Type (Locked_Param_Info_As_Byte (Instr.Locked_Param_Info));
   begin
      Store_Word
        (Params, 0, Info_Value);
   end Instruction_Locked_Param_Info;

   procedure Instruction_Locked_Param_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Locked_Param_Is_Var(Instruction) -> Boolean
      --    is import(#instruction_Locked_Param_Is_Var)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Locked_Param_Info.Is_Var));
   end Instruction_Locked_Param_Is_Var;

   procedure Instruction_Locked_Param_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Locked_Param_Is_By_Ref(Instruction) -> Boolean
      --    is import(#instruction_Locked_Param_Is_By_Ref)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Locked_Param_Info.Is_By_Ref));
   end Instruction_Locked_Param_Is_By_Ref;

   procedure Fill_In_Code_Block_Descriptor
     (Context : in out Exec_Context;
      Params  : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr;
      Code    : Code_Block_Descriptor) is
   --  Create and fill in a ParaSail Code_Block_Descriptor object

      Target : constant Word_Type := Fetch_Word (Params, 0);
         --  Get "null" passed in to determine desired region

      Target_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);

      Target_Type : constant Non_Op_Map_Type_Ptr := Static_Link;
         --  NOTE: It is important that in the ParaSail interface the
         --        operations using this routine are declared as
         --        operations of Code_Block_Descriptor,
         --        so Context.Enclosing_Type is correct

      Result : constant Word_Type :=
        Create_Large_Obj (Target_Type, Target_Rgn,
                          Context.Server_Index);
   begin
      --  Fill in the code descriptor fields
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(0),
        Word_Type (Code.Pc_Offset));
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(1),
        Boolean'Pos (Code.Uses_Queuing));
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(2),
        Boolean'Pos (Code.Uses_Stg_Rgn));
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(3),
        Word_Type (Code.Local_Area_Length));
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(4),
        Word_Type (Code.Start_Callee_Locals));
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(5),
        Word_Type (Code.Nesting_Level));

      --  Store the result
      Store_Word (Params, 0, Result);
   end Fill_In_Code_Block_Descriptor;

   procedure Instruction_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Code_Block(Instruction) -> optional Code_Block_Descriptor
      --    is import(#instruction_Code_Block)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      case Instr.Op is
         when Check_Nested_Block_Op | Call_Nested_Block_Op =>
            --  Fill in the code descriptor fields
            Fill_In_Code_Block_Descriptor
              (Context, Params, Static_Link, Instr.Code_Block);

         when others =>
            --  Leave the result null
            null;
      end case;
   end Instruction_Code_Block;

   procedure Instruction_Assertion_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Assertion_Str(Instruction) -> optional Univ_String
      --    is import(#instruction_Assertion_Str)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Instr.Assertion_Str, Target));
   end Instruction_Assertion_Str;

   procedure Instruction_Destination
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Destination(Instruction) -> Object_Locator
      --    is import(#instruction_Destination)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Destination,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Destination.Base) * 2**32 +
      --     Word_Type (Instr.Destination.Offset));
   end Instruction_Destination;

   procedure Instruction_Dest_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Dest_Name(Instruction) -> Univ_String
      --    is import(#instruction_Dest_Name)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Instr.Dest_Name, Target));
   end Instruction_Dest_Name;

   procedure Instruction_Decl_Obj_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0, Boolean'Pos (Instr.Is_By_Ref));
   end Instruction_Decl_Obj_Is_By_Ref;

   procedure Instruction_Decl_Obj_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0, Boolean'Pos (Instr.Is_Var));
   end Instruction_Decl_Obj_Is_Var;

   procedure Instruction_Declare_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Declare_Type_Info(Instruction) -> Object_Locator
      --    is import(#instruction_Declare_Type_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Declare_Type_Info,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Declare_Type_Info.Base) * 2**32 +
      --     Word_Type (Instr.Declare_Type_Info.Offset));
   end Instruction_Declare_Type_Info;

   procedure Instruction_Null_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Null_Type_Info(Instruction) -> Object_Locator
      --    is import(#instruction_Null_Type_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Null_Type_Info,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Null_Type_Info.Base) * 2**32 +
      --     Word_Type (Instr.Null_Type_Info.Offset));
   end Instruction_Null_Type_Info;

   procedure Instruction_Local_Addr
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Local_Addr(Instruction) -> Object_Locator
      --    is import(#instruction_Local_Addr)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Local_Addr,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Local_Addr.Base) * 2**32 +
      --     Word_Type (Instr.Local_Addr.Offset));
   end Instruction_Local_Addr;

   procedure Instruction_Int_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Int_Value(Instruction) -> Univ_Integer
      --    is import(#instruction_Int_Value)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Instr.Int_Value);
   end Instruction_Int_Value;

   procedure Instruction_Char_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Char_Value(Instruction) -> Univ_Character
      --    is import(#instruction_Char_Value)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Instr.Char_Value);
   end Instruction_Char_Value;

   procedure Instruction_Real_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Real_Value(Instruction) -> Univ_Real
      --    is import(#instruction_Real_Value)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         From_Univ_Real (Instr.Real_Value));
   end Instruction_Real_Value;

   procedure Instruction_Str_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Str_Value(Instruction) -> Univ_String
      --    is import(#instruction_Str_Value)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Instr.Str_Value, Target));
   end Instruction_Str_Value;

   procedure Instruction_Existing_Str_In_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Existing_Str_In_Stg_Rgn(Instruction) -> Object_Locator
      --    is import(#instruction_Existing_Str_In_Stg_Rgn)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Existing_Str_In_Stg_Rgn,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Existing_Str_In_Stg_Rgn.Base) * 2**32 +
      --     Word_Type (Instr.Existing_Str_In_Stg_Rgn.Offset));
   end Instruction_Existing_Str_In_Stg_Rgn;

   procedure Instruction_Enum_Value
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Enum_Value(Instruction) -> Univ_Enumeration
      --    is import(#instruction_Enum_Value)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Enum_Value));
   end Instruction_Enum_Value;

   procedure Instruction_Operation_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Operation_Static_Link(Instruction) -> Object_Locator
      --    is import(#instruction_Operation_Static_Link)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Operation_Static_Link,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --    Word_Type (Instr.Operation_Static_Link.Base) * 2**32 +
      --      Word_Type (Instr.Operation_Static_Link.Offset));
   end Instruction_Operation_Static_Link;

   procedure Instruction_Operation_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Operation_Locator(Instruction) -> Object_Locator
      --    is import(#instruction_Operation_Locator)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Operation_Locator,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Operation_Locator.Base) * 2**32 +
      --     Word_Type (Instr.Operation_Locator.Offset));
   end Instruction_Operation_Locator;

   procedure Instruction_Source
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Source(Instruction) -> Object_Locator
      --    is import(#instruction_Source)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Source,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Source.Base) * 2**32 +
      --     Word_Type (Instr.Source.Offset));
   end Instruction_Source;

   procedure Instruction_Might_Be_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Might_Be_Null(Instruction) -> Boolean
      --    is import(#instruction_Might_Be_Null)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Might_Be_Null));
   end Instruction_Might_Be_Null;

   procedure Instruction_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Type_Info(Instruction) -> Object_Locator
      --    is import(#instruction_Type_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Type_Info,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Type_Info.Base) * 2**32 +
      --     Word_Type (Instr.Type_Info.Offset));
   end Instruction_Type_Info;

   procedure Instruction_Existing_Obj_In_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Existing_Obj_In_Stg_Rgn(Instruction) -> Object_Locator
      --    is import(#instruction_Existing_Obj_In_Stg_Rgn)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Existing_Obj_In_Stg_Rgn,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --    Word_Type (Instr.Existing_Obj_In_Stg_Rgn.Base) * 2**32 +
      --      Word_Type (Instr.Existing_Obj_In_Stg_Rgn.Offset));
   end Instruction_Existing_Obj_In_Stg_Rgn;

   procedure Instruction_Source_Type_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Source_Type_Info(Instruction) -> Object_Locator
      --    is import(#instruction_Source_Type_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Source_Type_Info,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Source_Type_Info.Base) * 2**32 +
      --     Word_Type (Instr.Source_Type_Info.Offset));
   end Instruction_Source_Type_Info;

   procedure Instruction_Ancestor_Lvalue
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Ancestor_Lvalue(Instruction) -> Boolean
      --    is import(#instruction_Ancestor_Lvalue)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      --  Handle both Select_Ancestor... and Select_Polymorphic_Ancestor...
      case Instr.Op is
         when Select_Ancestor_Part_Op =>
            Store_Word
              (Params, 0,
               Boolean'Pos (Instr.Ancestor_Lvalue));
         when Select_Polymorphic_Ancestor_Part_Op =>
            Store_Word
              (Params, 0,
               Boolean'Pos (Instr.Polymorphic_Ancestor_Lvalue));
         when others =>
            raise Program_Error;
      end case;
   end Instruction_Ancestor_Lvalue;

   procedure Instruction_Polymorphic_Ancestor_Lvalue
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Polymorphic_Ancestor_Lvalue(Instruction) -> Boolean
      --    is import(#instruction_Polymorphic_Ancestor_Lvalue)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Polymorphic_Ancestor_Lvalue));
   end Instruction_Polymorphic_Ancestor_Lvalue;

   procedure Instruction_If_Source
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func If_Source(Instruction) -> Object_Locator
      --    is import(#instruction_If_Source)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.If_Source,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.If_Source.Base) * 2**32 +
      --     Word_Type (Instr.If_Source.Offset));
   end Instruction_If_Source;

   procedure Instruction_If_Condition
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func If_Condition(Instruction) -> Condition_Bit_Mask
      --    is import(#instruction_If_Condition)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Condition_Bit_Mask'Pos (Instr.If_Condition));
   end Instruction_If_Condition;

   procedure Instruction_Skip_If_False
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Skip_If_False(Instruction) -> Code_Offset
      --    is import(#instruction_Skip_If_False)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Skip_If_False));
   end Instruction_Skip_If_False;

   procedure Instruction_Parallel_Master
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Master(Instruction) -> Object_Locator
      --    is import(#instruction_Parallel_Master)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Parallel_Master,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Parallel_Master.Base) * 2**32 +
      --     Word_Type (Instr.Parallel_Master.Offset));
   end Instruction_Parallel_Master;

   procedure Instruction_Parallel_Control
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Control(Instruction) -> Object_Locator
      --    is import(#instruction_Parallel_Control)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Parallel_Control,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Parallel_Control.Base) * 2**32 +
      --     Word_Type (Instr.Parallel_Control.Offset));
   end Instruction_Parallel_Control;

   procedure Instruction_Parallel_Static_Link
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Static_Link(Instruction) -> Object_Locator
      --    is import(#instruction_Parallel_Static_Link)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Parallel_Static_Link,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Parallel_Static_Link.Base) * 2**32 +
      --     Word_Type (Instr.Parallel_Static_Link.Offset));
   end Instruction_Parallel_Static_Link;

   procedure Instruction_Parallel_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Code_Block(Instruction)
      --    -> optional Code_Block_Descriptor
      --    is import(#instruction_Parallel_Code_Block)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      case Instr.Op is
         when Start_Parallel_Op | Start_Handled_Op | Add_Parallel_Op =>
            --  Fill in the code descriptor fields
            Fill_In_Code_Block_Descriptor
              (Context, Params, Static_Link, Instr.Parallel_Code_Block);

         when others =>
            null;
      end case;
   end Instruction_Parallel_Code_Block;

   procedure Instruction_Parallel_Call_Target
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Call_Target(Instruction) -> Object_Locator
      --    is import(#instruction_Parallel_Call_Target)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Parallel_Call_Target,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Parallel_Call_Target.Base) * 2**32 +
      --     Word_Type (Instr.Parallel_Call_Target.Offset));
   end Instruction_Parallel_Call_Target;

   procedure Instruction_Parallel_Target_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Target_Index(Instruction) -> Routine_Index
      --    is import(#instruction_Parallel_Target_Index)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Parallel_Target_Index));
   end Instruction_Parallel_Target_Index;

   procedure Instruction_Parallel_Locked_Param_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Locked_Param_Index(Instruction) -> Natural
      --    is import(#instruction_Parallel_Locked_Param_Index)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Parallel_Locked_Param_Info.Param_Index));
   end Instruction_Parallel_Locked_Param_Index;

   procedure Instruction_Parallel_Locked_Param_Info
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Locked_Param_Info(Instruction)
      --    -> Locked_Param_Info_As_Byte_Type
      --    is import(#instruction_Parallel_Locked_Param_Info)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Info_Value : constant Word_Type range 0 .. 255 := Word_Type
        (Locked_Param_Info_As_Byte (Instr.Parallel_Locked_Param_Info));
   begin
      Store_Word
        (Params, 0, Info_Value);
   end Instruction_Parallel_Locked_Param_Info;

   procedure Instruction_Parallel_Locked_Param_Is_Var
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Locked_Param_Is_Var(Instruction) -> Boolean
      --    is import(#instruction_Parallel_Locked_Param_Is_Var)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Parallel_Locked_Param_Info.Is_Var));
   end Instruction_Parallel_Locked_Param_Is_Var;

   procedure Instruction_Parallel_Locked_Param_Is_By_Ref
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Locked_Param_Is_By_Ref(Instruction) -> Boolean
      --    is import(#instruction_Parallel_Locked_Param_Is_By_Ref)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Parallel_Locked_Param_Info.Is_By_Ref));
   end Instruction_Parallel_Locked_Param_Is_By_Ref;

   procedure Instruction_Parallel_Is_Queued_Call
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parallel_Is_Queued_Call(Instruction) -> Boolean
      --    is import(#instruction_Parallel_Is_Queued_Call)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Instr.Parallel_Locked_Param_Info.Is_Queued_Call));
   end Instruction_Parallel_Is_Queued_Call;

   procedure Instruction_Num_In_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_In_Params(Instruction) -> Natural
      --    is import(#instruction_Num_In_Params)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      if Instr.Op = Indirect_Call_Op then
         Store_Word
           (Params, 0,
            Word_Type (Instr.Indirect_Num_In_Params));
      else
         Store_Word
           (Params, 0,
            Word_Type (Instr.Num_In_Params));
      end if;
   end Instruction_Num_In_Params;

   procedure Instruction_Num_Out_Params
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Out_Params(Instruction) -> Natural
      --    is import(#instruction_Num_Out_Params)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      if Instr.Op = Indirect_Call_Op then
         Store_Word
           (Params, 0,
            Word_Type (Instr.Indirect_Num_Out_Params));
      else
         Store_Word
           (Params, 0,
            Word_Type (Instr.Num_Out_Params));
      end if;
   end Instruction_Num_Out_Params;

   procedure Instruction_Case_Selector
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Case_Selector(Instruction) -> Object_Locator
      --    is import(#instruction_Case_Selector)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Insert_Object_Locator
           (Instr.Case_Selector,
            Object_Locator_Type_Desc (Static_Link),
            Fetch_Word (Params, 0),
            Context.Server_Index));
      --   Word_Type (Instr.Case_Selector.Base) * 2**32 +
      --     Word_Type (Instr.Case_Selector.Offset));
   end Instruction_Case_Selector;

   procedure Instruction_Case_First
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  // func Case_First(Instruction) -> Non_Null_Value
      --  //   is import(#instruction_Case_First)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Instr.Case_First);
   end Instruction_Case_First;

   procedure Instruction_Case_Last
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  // func Case_Last(Instruction) -> Non_Null_Value
      --  //   is import(#instruction_Case_Last)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Instr.Case_Last);
   end Instruction_Case_Last;

   procedure Instruction_Case_Default_Skip
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Case_Default_Skip(Instruction) -> Code_Offset
      --    is import(#instruction_Case_Default_Skip)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Case_Default_Skip));
   end Instruction_Case_Default_Skip;

   procedure Instruction_Nested_Code_Block
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nested_Code_Block(Instruction)
      --    -> optional Code_Block_Descriptor
      --    is import(#instruction_Nested_Code_Block)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
   begin
      case Instr.Op is
         when Begin_Nested_Block_Op =>
            --  Fill in the code descriptor fields
            Fill_In_Code_Block_Descriptor
              (Context, Params, Static_Link, Instr.Nested_Code_Block);

         when others =>
            null;
      end case;
   end Instruction_Nested_Code_Block;

   procedure Instruction_Nested_Block_Region
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nested_Block_Region(Instruction)
      --    -> Region::Region_Index
      --    is import(#instruction_Nested_Block_Region)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      pragma Assert (Instr.Op = Begin_Nested_Block_Op);
   begin
      Store_Word
        (Params, 0,
         Word_Type (Instr.Nested_Block_Region));
   end Instruction_Nested_Block_Region;

   procedure Instruction_Output_Inited_Null
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Output_Inited_Null(Instruction) -> Boolean
      --    is import(#instruction_Output_Inited_Null)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Result : Boolean := False;
   begin
      case Instr.Op is
         when Call_Op | Indirect_Call_Op =>
            Result := Instr.Output_Inited_Null;
         when Start_Parallel_Call_Op | Add_Parallel_Call_Op =>
            Result := Instr.Parallel_Output_Inited_Null;
         when others =>
            Result := False;
      end case;
      Store_Word (Params, 0, Boolean'Pos (Result));
   end Instruction_Output_Inited_Null;

   procedure Instruction_Proved
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Proved(Instruction) -> Boolean is import(#instruction_Proved)
      Instr_Index : constant Word_Type :=
        Fetch_Word (Params, 1);
      Instr : Instruction renames Nth_Instruction
        (Routine_Index (Instr_Index / 2**32),
         Code_Index (Instr_Index mod 2**32));
      Result : Boolean := False;
   begin
      case Instr.Op is
         when Call_Op | Indirect_Call_Op =>
            Result := Instr.Precond_Proved;
         when Start_Parallel_Call_Op | Add_Parallel_Call_Op =>
            Result := Instr.Parallel_Precond_Proved;
         when Return_Op =>
            Result := Instr.Postcond_Proved;
         when Check_Nested_Block_Op =>
            Result := Instr.Assertion_Proved;
         when Check_Not_Null_Op =>
            Result := Instr.Not_Null_Proved;
         when others =>
            Result := False;
      end case;
      Store_Word (Params, 0, Boolean'Pos (Result));
   end Instruction_Proved;

   procedure New_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Conv : constant Languages.Convention_Enum :=
        Languages.Convention_Enum'Val (Fetch_Word (Params, 1));
      Num_Inputs : constant Natural :=
        Natural'Val (Fetch_Word (Params, 2));
      Num_Outputs : constant Natural :=
        Natural'Val (Fetch_Word (Params, 3));
      Output_Needs_Init : constant Boolean :=
        Boolean'Val (Fetch_Word (Params, 4));
      Uses_Queuing : constant Boolean :=
        Boolean'Val (Fetch_Word (Params, 5));
      Result : constant Convention_Descriptor :=
        New_Conv_Desc (Conv,
                       Num_Inputs => Num_Inputs,
                       Num_Outputs => Num_Outputs,
                       Output_Needs_Init => Output_Needs_Init,
                       Uses_Queuing => Uses_Queuing);
   begin
      Store_Word (Params, 0, Convention_Descriptor'Pos (Result));
   end New_Conv_Desc;

   procedure Null_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word (Params, 0,
        Convention_Descriptor'Pos (Interpreter.Null_Conv_Desc));
   end Null_Conv_Desc;

   procedure CD_Convention
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      CD : constant Convention_Descriptor :=
        Convention_Descriptor (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0, Languages.Convention_Enum'Pos (Convention (CD)));
   end CD_Convention;

   procedure CD_Num_Inputs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      CD : constant Convention_Descriptor :=
        Convention_Descriptor (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0, Natural'Pos (Num_Inputs (CD)));
   end CD_Num_Inputs;

   procedure CD_Num_Outputs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      CD : constant Convention_Descriptor :=
        Convention_Descriptor (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0, Natural'Pos (Num_Outputs (CD)));
   end CD_Num_Outputs;

   procedure CD_Output_Needs_Init
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      CD : constant Convention_Descriptor :=
        Convention_Descriptor (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0, Boolean'Pos (Output_Needs_Init (CD)));
   end CD_Output_Needs_Init;

   procedure CD_Uses_Queuing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      CD : constant Convention_Descriptor :=
        Convention_Descriptor (Fetch_Word (Params, 1));
   begin
      Store_Word (Params, 0, Boolean'Pos (Uses_Queuing (CD)));
   end CD_Uses_Queuing;

   procedure Routine_Uses_Queuing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_Uses_Queuing(Routine) -> Boolean
      --    is import(#routine_uses_queuing);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0, Boolean'Pos (Routine.Uses_Queuing));
   end Routine_Uses_Queuing;

   procedure Routine_Uses_Stg_Rgn
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_Uses_Stg_Rgn(Routine) -> Boolean
      --    is import(#routine_uses_stg_rgn);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0, Boolean'Pos
          (Routine.Is_PSVM_Routine  --  Imports never use storage regions
             and then Routine.Code /= null
             and then Routine.Code.Uses_Stg_Rgn));
   end Routine_Uses_Stg_Rgn;

   procedure Routine_Num_VM_Regs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_Num_VM_Regs(Routine) -> VM_Reg_Num
      --    is import(#routine_num_vm_regs);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      Num_VM_Regs : VM_Obj_Unique_Num := 0;
   begin
      if Routine.Is_PSVM_Routine and then Routine.Code /= null then
         Num_VM_Regs := Routine.Code.Num_Locals;
      end if;
      Store_Word
        (Params, 0, Word_Type (Num_VM_Regs));
   end Routine_Num_VM_Regs;

   procedure Routine_Num_Instrs
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Instrs(Routine) -> Code_Length
      --    is import(#routine_num_instrs);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      if not Routine.Is_PSVM_Routine
        or else Routine.Code = null
      then
         --  Imports don't have a Code component.
         --  Abstract routines have a null Code pointer.
         Store_Word
           (Params, 0, 0);
      else
         Store_Word
           (Params, 0,
            Word_Type (Routine.Code.Code_Length));
      end if;
   end Routine_Num_Instrs;

   procedure Routine_Nth_Instr
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nth_Instr(Routine; Code_Index) -> Instruction
      --    is import(#routine_nth_instr);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Routine.Index) * 2**32 +
           Fetch_Word (Params, 2));
   end Routine_Nth_Instr;

   procedure Routine_Start_Callee_Locals
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Frame_Size(Routine) -> Univ_Integer
      --    is import(#routine_start_callee_locals)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
        Word_Type (Routine.Start_Callee_Locals));
   end Routine_Start_Callee_Locals;

   procedure Routine_Internal_Precond
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Internal_Precond(Routine) -> optional Code_Block_Descriptor
      --    is import(#routine_internal_precond)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      if Routine.Boundary_Conditions (Internal_Precondition).Pc_Offset > 0 then
         --  Fill in the code descriptor fields
         Fill_In_Code_Block_Descriptor
           (Context, Params, Static_Link,
            Routine.Boundary_Conditions (Internal_Precondition));
      --  else leave result null
      end if;
   end Routine_Internal_Precond;

   procedure Routine_Nesting_Level
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Nesting_Level(Routine) -> Object_Locator::Code_Nesting_Level
      --    is import(#routine_nesting_level)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
        Word_Type (Routine.Nesting_Level));
   end Routine_Nesting_Level;

   procedure Routine_Convention
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Convention(Routine) -> Decl::Convention_Enum
      --    is import(#routine_convention)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Languages.Convention_Enum'Pos (Routine.Convention));
   end Routine_Convention;

   procedure Routine_Conv_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Conv_Desc(Routine) -> Convention_Descriptor
      --    is import(#routine_conv_desc)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Convention_Descriptor'Pos (Routine.Conv_Desc));
   end Routine_Conv_Desc;

   procedure Routine_Module_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Module_Name(Routine) -> Univ_String
      --    is import(#routine_module_name)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Routine.Full_Module_Name, Target));
   end Routine_Module_Name;

   procedure Routine_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Name(Routine) -> Univ_String
      --    is import(#routine_name)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Routine.Name, Target));
   end Routine_Name;

   procedure Routine_Num_Prior_Homonyms
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Num_Prior_Homonyms(Routine) -> Decl::Overloading_Index
      --    is import(#routine_num_prior_homonyms);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Routine.Num_Prior_Homonyms));
   end Routine_Num_Prior_Homonyms;

   procedure Routine_Name_With_Overloading_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Name_With_Overloading_Index(Routine) -> Univ_String
      --    is import(#routine_name_with_overloading_index)
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Univ_String_Word
           (Routine_Name_With_Overloading_Index (Routine), Target));
   end Routine_Name_With_Overloading_Index;

   procedure Routine_Built_In_Desig
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Built_In_Designator(Routine) -> Univ_String
      --    is import(#routine_built_in_designator);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if not Routine.Is_PSVM_Routine then
         Store_Word
           (Params, 0,
            To_Univ_String_Word (Routine.Built_In_Desig, Target));
      else
         Store_Word
           (Params, 0, To_Univ_String_Null (Target));
      end if;
   end Routine_Built_In_Desig;

   procedure Routine_Enc_Type_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Enc_Type_Desc(Routine) -> optional Type_Descriptor
      --    is import(#routine_enc_type_desc);
      Routine : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
   begin
      if not Routine.Is_PSVM_Routine then
         Store_Word
           (Params, 0,
            To_Word_Type (Type_Descriptor_Ptr'(null)));
      else
         Store_Word
           (Params, 0, To_Word_Type (Routine.Enc_Type_Desc));
      end if;
   end Routine_Enc_Type_Desc;

   procedure Routine_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_At_Locator(Locator : Object_Locator;
      --    Type_Desc : optional Type_Descriptor := null) -> Routine
      --    is import(#routine_at_locator)
      Locator_Int : constant Word_Type :=
        Fetch_Word (Params, 1);
      Locator : constant Object_Locator :=
        Extract_Object_Locator (Locator_Int);
         --  (Base => Area_Base_Indicator (Locator_Int / 2**32),
         --   Offset => Offset_Within_Area (Locator_Int mod 2**16),
         --   VM_Obj_Id => No_VM_Obj_Id);
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 2));
      Index : Routine_Index := 0;
   begin
      case Locator.Base is
         when Zero_Base =>
            --  "Absolute" address of routine
            Index := Routine_Index (Locator.Offset);

         when Type_Area =>
            --  Relative to type area.  If is a formal operation,
            --  then this is relative to the caller's type area.
            --  If this is an operation of the type, then this
            --  is relative to the called routine's type area,
            --  which is given by the static link.

            if Type_Desc = null then
               pragma Assert (False);  --  TBD
               null;
            else
               Index := Nth_Operation_Of_Type
                 (Type_Desc,
                  Index =>
                    Operation_Index
                      (Locator.Offset - Type_Operation_Offsets'First)).Index;
            end if;

         when Enclosing_Type_Areas =>
            --  Nth op of enclosing type area
            pragma Assert (False);  --  TBD
            null;

         when others =>
            --  Address of operation descriptor
            pragma Assert (False);  --  TBD
            null;
      end case;

      Store_Word
        (Params, 0,
         To_Word_Type (Nth_Routine (Index)));
   end Routine_At_Locator;

   procedure Routine_At_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Routine_At_Index(Index : Routine_Index) -> Routine
      --    is import(#routine_at_index)
      Index : constant Routine_Index :=
        Routine_Index (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Nth_Routine (Index)));
   end Routine_At_Index;

   procedure Set_Breakpoint
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Set_Breakpoint(Op : Routine; Line : Univ_Integer)
      --     -> Breakpoint_Index
      --     is import(#set_breakpoint);
      --     //  Set a breakpoint at the given line within the given
      --     //  routine; returns the index identifying the breakpoint.
      --     //  If breakpoint already exists at given location,
      --     //  the old breakpoint number is returned.
      Op : constant Routine_Ptr :=
        To_Routine_Ptr (Fetch_Word (Params, 1));
      use Source_Positions;
      Line_As_Word : constant Word_Type := Fetch_Word (Params, 2);
      Line : Line_Number := 0;
   begin
      if Op = null or else Op.Code = null then
         --  Bad routine provided
         Store_Word (Params, 0, 0);
      end if;

      if Line_As_Word not in 1 .. Word_Type (Line_Number'Last) then
         Messages.Put_Error
           ("Bad Line Number in Set_Breakpoint: " &
             Word_Type'Image (Line_As_Word),
            Null_Source_Position,
            Suppress_Duplicates => False);
         Store_Word (Params, 0, 0);
      end if;

      --  OK, it is safe to do the conversion.
      Line := Line_Number (Line_As_Word);

      for B in 1 .. Last_Breakpoint loop
         if Breakpoints (B).Op = Op
           and then Breakpoints (B).Line = Line
         then
            --  Already a breakpoint at that location
            Store_Word (Params, 0, Word_Type (B));
            return;
         end if;
      end loop;

      --  No existing breakpoint, add it to the table.
      Last_Breakpoint := Last_Breakpoint + 1;
      Breakpoints (Last_Breakpoint) := (Op => Op, Line => Line);

      --  And set the breakpoint in the code
      for I in 1 .. Op.Code.Code_Length loop
         declare
            Instr : Instruction renames Op.Code.Instrs (I);
         begin
            if Instr.Source_Pos.Line = Line then
               --  Breakpoint indicator is line = 0 and End_Line > 0.
               --  End_Col holds the breakpoint index.
               Instr.Source_Pos.End_Line := Line;
               Instr.Source_Pos.Line := 0;
               Instr.Source_Pos.End_Col := Column_Number (Last_Breakpoint);
            end if;
         end;
      end loop;

      --  Return the breakpoint index
      Store_Word (Params, 0, Word_Type (Last_Breakpoint));
   end Set_Breakpoint;

   procedure Clear_Breakpoint
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Clear_Breakpoint(Index : Breakpoint_Index)
      --     is import(#clear_breakpoint);
      --     //  Clear the given breakpoint
      Index_As_Word : constant Word_Type := Fetch_Word (Params, 0);

      use Source_Positions;
   begin
      if Index_As_Word not in 1 .. Word_Type (Last_Breakpoint) then
         --  Bad index
         Messages.Put_Error
           ("Bad Index in Clear_Breakpoint: " &
             Word_Type'Image (Index_As_Word),
            Null_Source_Position,
            Suppress_Duplicates => False);
         return;
      end if;

      declare
         Index : constant Breakpoint_Index := Breakpoint_Index (Index_As_Word);
         Brk : Breakpoint renames Breakpoints (Index);
      begin
         if Brk.Line = 0 then
            Messages.Put_Error
              ("Breakpoint already deleted in Clear_Breakpoint: " &
                Breakpoint_Index'Image (Index),
               Null_Source_Position,
               Suppress_Duplicates => False);
            return;
         end if;

         --  Now clear the breakpoint
         for I in 1 .. Brk.Op.Code.Code_Length loop
            declare
               Instr : Instruction renames Brk.Op.Code.Instrs (I);
            begin
               if Instr.Source_Pos.Line = 0
                 and then Instr.Source_Pos.End_Line = Brk.Line
               then
                  --  Breakpoint indicator is line = 0 and End_Line > 0.
                  --  End_Col holds the breakpoint index.
                  Instr.Source_Pos.Line := Brk.Line;
                  Instr.Source_Pos.End_Line := 0;
                  Instr.Source_Pos.End_Col := 0;
               end if;
            end;
         end loop;

         --  Indicate breakpoint has been cleared
         Brk.Line := 0;
      end;

   end Clear_Breakpoint;

   procedure Num_Breakpoints
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Num_Breakpoints() -> Breakpoint_Index
      --     is import(#num_breakpoints);
      --     //  Returns a count of the number of breakpoints set (including
      --     //  those that were set once, and have since been cleared).
   begin
      Store_Word (Params, 0, Word_Type (Last_Breakpoint));
   end Num_Breakpoints;

   procedure Nth_Breakpoint_Routine
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Nth_Breakpoint_Routine(Index : Breakpoint_Index) -> Routine
      --     is import(#nth_breakpoint_routine);
      --     //  Returns the routine associated with the given breakpoint.
      Index_As_Word : constant Word_Type := Fetch_Word (Params, 1);
   begin
      if Index_As_Word not in 1 .. Word_Type (Last_Breakpoint) then
         --  Bad index
         Messages.Put_Error
           ("Bad Index in Nth_Breakpoint_Routine: " &
             Word_Type'Image (Index_As_Word),
            Source_Positions.Null_Source_Position,
            Suppress_Duplicates => False);

         Store_Word (Params, 0, Null_Value);
         return;
      end if;

      declare
         Index : constant Breakpoint_Index := Breakpoint_Index (Index_As_Word);
         Brk : Breakpoint renames Breakpoints (Index);
      begin
         Store_Word (Params, 0, To_Word_Type (Routine_Ptr (Brk.Op)));
      end;
   end Nth_Breakpoint_Routine;

   procedure Nth_Breakpoint_Line
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Nth_Breakpoint_Line(Index : Breakpoint_Index) -> Univ_Integer
      --     is import(#nth_breakpoint_line);
      --     //  Returns the line# associated with the given breakpoint.
      --     //  Returns 0 if breakpoint has already been cleared.
      Index_As_Word : constant Word_Type := Fetch_Word (Params, 1);
   begin
      if Index_As_Word not in 1 .. Word_Type (Last_Breakpoint) then
         --  Bad index
         Messages.Put_Error
           ("Bad Index in Nth_Breakpoint_Line: " &
             Word_Type'Image (Index_As_Word),
            Source_Positions.Null_Source_Position,
            Suppress_Duplicates => False);

         Store_Word (Params, 0, Null_Value);
         return;
      end if;

      declare
         Index : constant Breakpoint_Index := Breakpoint_Index (Index_As_Word);
         Brk : Breakpoint renames Breakpoints (Index);
      begin
         Store_Word (Params, 0, Word_Type (Brk.Line));
      end;
   end Nth_Breakpoint_Line;

   procedure Type_Desc_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --   func Type_Desc_At_Locator(Locator : Object_Locator)
      --     -> optional Type_Descriptor
      --     is import(#type_desc_at_locator)
      Locator_Int : constant Word_Type :=
        Fetch_Word (Params, 1);
      Locator : constant Object_Locator :=
        Extract_Object_Locator (Locator_Int);
         --  (Base => Area_Base_Indicator (Locator_Int / 2**32),
         --   Offset => Offset_Within_Area (Locator_Int mod 2**16),
         --   VM_Obj_Id => No_VM_Obj_Id);
      Index : Type_Index := 0;
   begin
      case Locator.Base is
         when Zero_Base =>
            --  "Absolute" address of routine
            Index := Type_Index (Locator.Offset);

         when Type_Area | Enclosing_Type_Areas =>

            --  Relative to type area.  If is a formal operation,
            --  then this is relative to the caller's type area.
            --  If this is an operation of the type, then this
            --  is relative to the called routine's type area,
            --  which is given by the static link.

            Store_Word (Params, 0, Null_Value);  --  TBD

            return;

         when others =>
            --  Address of operation descriptor
            Store_Word (Params, 0, Null_Value);  --  TBD
            
            return;
      end case;

      declare
         Result : constant Type_Descriptor_Ptr :=
                    To_Type_Desc_Or_Op_Map (Index);
      begin
         if Result /= null then
            Store_Word (Params, 0, To_Word_Type (Result));
         else
            Store_Word (Params, 0, Null_Value);  --  TBD -- use Large null?
         end if;
      end;
   end Type_Desc_At_Locator;

   procedure Type_Desc_At_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Type_Desc_At_Index(Index : Type_Index)
      --    -> optional Type_Descriptor
      --    is import(#type_desc_at_index)
      Index : constant Type_Index :=
        Type_Index (Fetch_Word (Params, 1));
      Result : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Index);
   begin
      if Result /= null then
         Store_Word (Params, 0, To_Word_Type (Result));
      else
         Store_Word (Params, 0, Null_Value);  --  TBD -- use Large null?
      end if;
   end Type_Desc_At_Index;

   procedure Type_Desc_Has_Op_Map
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Has_Op_Map(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_has_op_map)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Has_Op_Map));
   end Type_Desc_Has_Op_Map;

   procedure Type_Desc_Corresponding_Polymorphic_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Corresponding_Polymorphic_Type(Desc : Type_Descriptor)
      --    -> optional Type_Descriptor+
      --    is import(#type_desc_polymorphic_type)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Type_Desc.Corresponding_Polymorphic_Type_Desc));
   end Type_Desc_Corresponding_Polymorphic_Type;

   procedure Type_Desc_Name
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Name(Desc : Type_Descriptor) -> Univ_String
      --    is import(#type_desc_name)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Nonnull_Word (Params, 1));
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if Type_Desc /= null then
         Store_Word
           (Params, 0,
            To_Univ_String_Word (Type_Desc.Name, Target));
      else
         Store_Word
           (Params, 0, To_Univ_String_Null (Target));
      end if;
   end Type_Desc_Name;

   procedure Type_Desc_Type_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Type_Decl(Desc : Type_Descriptor) -> Decl
      --    is import(#type_desc_type_decl)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Nonnull_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Root_Sem_Ptr (Type_Desc.Type_Sem)));
   end Type_Desc_Type_Decl;

   procedure Type_Desc_Index
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Index(Desc : Type_Descriptor) -> Type_Index
      --    is import(#type_desc_index)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Nonnull_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Word_Type (Type_Desc.Index));
   end Type_Desc_Index;

   procedure Type_Desc_Init_Stream
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Init_Stream(Desc : Type_Descriptor; var Per_File_String_Table)
      --    -> Basic_Array<Unsigned_64>
      --  //  Initialize a byte-stream representation of the type descriptor
      --  //  Update the Per-File string table as appropriate
      --    is import (#type_desc_init_stream)

      Target : constant Word_Type := Fetch_Word (Params, 0);

      --  Get type descriptor parameter
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 1));

      --  Get per-file string table param (passed by ref)
      PFST : constant PFS.Per_File_String_Table_Ptr :=
        To_PFS_Ptr (Word_To_Word_Ptr (Content_Of_Physical_Address
          (Fetch_Word_Ptr (Params, 2))));

      --  Get type descriptor for result
      Unsigned_64_Array_Type : constant Type_Descriptor_Ptr :=
        Type_Descriptor_Ops.Get_Type_Desc_By_Name (Unsigned_64_Array_Name);
   begin
      --  Generate the stream representation, updating the per-file string tab

      Store_Word (Params, 0,
        Interpreter.Type_Descriptor_Ops.Generate_Stream_Rep
          (Type_Desc, Unsigned_64_Array_Type, Target,
           Context.Server_Index, PFST));
   end Type_Desc_Init_Stream;

   --  The following operations will automatically get the underlying
   --  type descriptor if passed an op map.

   procedure Type_Desc_Type_Kind
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Type_Kind(Desc : Type_Descriptor) -> Type_Kind_Enum
      --    is import(#type_desc_kind)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Nonnull_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Type_Kind_Enum'Pos (Type_Desc.Type_Kind));
   end Type_Desc_Type_Kind;

   procedure Type_Desc_All_Parameters_Known
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func All_Parameters_Known(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_all_parameters_known)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.All_Parameters_Known));
   end Type_Desc_All_Parameters_Known;

   procedure Type_Desc_Is_Small
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Small(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_small)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Small));
   end Type_Desc_Is_Small;

   procedure Type_Desc_Is_Large
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Small(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_large)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Large));
   end Type_Desc_Is_Large;

   procedure Type_Desc_Is_Wrapper
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Wrapper(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_wrapper)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Wrapper));
   end Type_Desc_Is_Wrapper;

   procedure Type_Desc_Null_Value_For_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Null_Value_For_Type(Desc : Type_Descriptor)
      --    -> Unsigned_64
      --    is import(#type_desc_null_value)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Type_Desc.Null_Value);
   end Type_Desc_Null_Value_For_Type;

   procedure Type_Desc_Parent_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Parent_Type(Desc : Type_Descriptor) -> optional Type_Descriptor
      --    is import(#type_desc_parent_type)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Type_Desc.Parent_Type));
   end Type_Desc_Parent_Type;

   procedure Type_Desc_Is_Abstract
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Abstract(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_abstract)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Abstract));
   end Type_Desc_Is_Abstract;

   procedure Type_Desc_Is_Concurrent
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Concurrent(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_concurrent)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Concurrent));
   end Type_Desc_Is_Concurrent;

   procedure Type_Desc_Is_Polymorphic
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Is_Polymorphic(Desc : Type_Descriptor) -> Boolean
      --    is import(#type_desc_is_polymorphic)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos (Type_Desc.Is_Polymorphic));
   end Type_Desc_Is_Polymorphic;

   procedure Type_Desc_Enclosing_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Enclosing_Type(Desc : Type_Descriptor)
      --    -> optional Type_Descriptor
      --    is import(#type_desc_enclosing_type)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Type_Desc.Enclosing_Type));
   end Type_Desc_Enclosing_Type;

   procedure Type_Desc_Root_Type_Desc
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Root_Type_Desc_(Desc : Type_Descriptor)
      --    -> optional Type_Descriptor
      --    is import(#type_desc_root)
      Type_Desc : constant Non_Op_Map_Type_Ptr :=
        To_Type_Descriptor_Ptr (Fetch_Word (Params, 1));
   begin
      Store_Word
        (Params, 0,
         To_Word_Type (Type_Desc.Root_Type_Desc));
   end Type_Desc_Root_Type_Desc;

   --  The following operations return null if passed a Type_Descriptor
   --  that does not have an "op map"

   procedure Op_Map_Actual_Type
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Actual_Type(Desc : Type_Descriptor)
      --    -> optional Type_Descriptor
      --    is import(#op_map_actual_type)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 1));
   begin
      if Type_Desc.Has_Op_Map then
         Store_Word
           (Params, 0,
            To_Word_Type (Type_Desc.Actual_Type));
      else
         Store_Word
           (Params, 0,
            Null_Value);
      end if;
   end Op_Map_Actual_Type;

   procedure Op_Map_Formal_Type_Decl
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Formal_Type_Decl(Desc : Type_Descriptor) -> optional Decl
      --    is import(#op_map_formal_type_decl)
      Type_Desc : constant Type_Descriptor_Ptr :=
        To_Type_Desc_Or_Op_Map (Fetch_Word (Params, 1));
   begin
      if Type_Desc.Has_Op_Map then
         Store_Word
           (Params, 0,
            To_Word_Type (Type_Desc.Formal_Type_Sem));
      else
         Store_Word
           (Params, 0,
            Null_Value);
      end if;
   end Op_Map_Formal_Type_Decl;

   subtype Type_Desc_Info_Kinds is Info_Kind_Enum
     range Type_Parameters_Kind .. Op_Map_Kind;

   subtype Type_Info_Kinds is Info_Kind_Enum
     range Type_Parameters_Kind .. Operations_Kind;

   subtype Type_Or_Op_Map_Info_Kinds is Info_Kind_Enum
     range Type_Desc_Stream_Kind .. Op_Map_Kind;

   subtype Routine_Info_Kinds is Info_Kind_Enum
     range Routine_Parameters_Kind .. Routine_Uplevel_Refs_Kind;

   function Create_Element_Info
     (Info : Element_Info;
      Elem_Info_Type : Type_Descriptor_Ptr;
      Target_Stg_Rgn : Stg_Rgn_Ptr;
      Server_Index   : Thread_Server_Index) return Word_Type is
   --  Create Elem_Info object
      Elem_Info : constant Word_Type :=
        Create_Large_Obj (Elem_Info_Type, Target_Stg_Rgn,
                          Server_Index);
   begin
      --  Fill in the Type_Desc/Addr/Value fields
      Store_Word (Elem_Info +
        Large_Obj_Header_Size + Offset_Within_Area'(0),
        To_Word_Type (Info.Type_Desc));
      Store_Word (Elem_Info +
        Large_Obj_Header_Size + Offset_Within_Area'(1),
        Info.Addr);
      Store_Word (Elem_Info +
        Large_Obj_Header_Size + Offset_Within_Area'(2),
        Info.Value);
      return Elem_Info;
   end Create_Element_Info;

   procedure Info_Array_First
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func First(Arr : Info_Array) -> Indexed_By
      --    is import(#info_array_first)
   begin
      --  Just return "1" in all cases
      Store_Word (Params, 0, 1);
   end Info_Array_First;

   procedure Info_Array_Last
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Last(Arr : Info_Array) -> Indexed_By
      --    is import(#info_array_last)
      Info_Array : constant Object_Virtual_Address :=
        Fetch_Word (Params, 1);
      Entity : constant Word_Type :=
        Fetch_Word (Info_Array + Large_Obj_Header_Size);
      Info_Kind : constant Info_Kind_Enum :=
        Info_Kind_Enum'Val
          (Fetch_Word (Info_Array + Large_Obj_Header_Size +
             Offset_Within_Area'(1)));
      Result : Natural := 0;

      use String_Streams;
   begin
      case Info_Kind is
      when Type_Desc_Info_Kinds =>
         --  Info on a type descriptor
         declare
            Type_Desc : Type_Descriptor_Ptr :=
              To_Type_Desc_Or_Op_Map (Entity);
         begin
            if Info_Kind in Type_Info_Kinds then
               --  Get underlying non-op-map type
               Type_Desc := Skip_Over_Op_Map (Type_Desc);
            end if;

            case Type_Desc_Info_Kinds'(Info_Kind) is
            when Type_Parameters_Kind =>
               if Type_Desc.Parameters /= null then
                  --  Only if type-descriptor is fully defined,
                  --  are parameters available.
                  Result := Type_Desc.Num_Parameters;
               end if;
            when Actuals_Of_Formals_Kind =>
               Result := Type_Desc.Num_Actuals_Of_Formals;
            when Components_Kind =>
               Result := Type_Desc.Num_Components;
            when Nested_Types_Kind =>
               Result := Type_Desc.Num_Nested_Types;
            when Nested_Objs_Kind =>
               Result := Type_Desc.Num_Nested_Objs;
            when Operations_Kind =>
               if Type_Desc.Operations /= null then
                  --  Only if type-descriptor is fully defined,
                  --  are operations available.
                  Result := Natural (Type_Desc.Num_Operations);
               end if;
            when Type_Desc_Stream_Kind =>
               declare
                  Stream_Cache : constant Object_Virtual_Address :=
                    Fetch_Word (Info_Array + Large_Obj_Header_Size +
                      Offset_Within_Area'(2));
               begin
                  Result :=
                    Natural (Fetch_Word (Stream_Cache + Large_Obj_Header_Size +
                      Offset_Within_Area'(1)));
               end;
            when Op_Map_Kind =>
               Result := Natural (Type_Desc.Num_Operations);
            end case;
         end;

      when Routine_Info_Kinds =>
         declare
            Routine_Desc : constant Routine_Ptr := To_Routine_Ptr (Entity);
         begin
            case Routine_Info_Kinds'(Info_Kind) is
            when Routine_Parameters_Kind =>
               if Routine_Desc.Parameters = null then
                  Result := 0;
               else
                  Result := Routine_Desc.Parameters'Length;
               end if;
            when Routine_Uplevel_Refs_Kind =>
               if not Routine_Desc.Is_PSVM_Routine
                 or else Routine_Desc.Uplevel_Refs = null
               then
                  --  No uplevel references
                  Result := 0;
               else
                  Result := Routine_Desc.Uplevel_Refs'Length;
               end if;
            end case;
         end;

      when String_Stream_Kind =>
         declare
            Univ_Str : constant Univ_Strings.Univ_String :=
              Univ_Strings.From_Word_Type (Entity);
            --  TBD: This is trying to simulate effect of 'Write
            use Ada.Streams;
            Int_Stream_Len : constant := Integer'Size / Stream_Element'Size;
            use type Strings.U_String;
         begin
            if Univ_Strings.Is_Null (Univ_Str) then
               Result := Int_Stream_Len;
            else
               --  TBD: This presumes character'size = stream_element'size
               pragma Assert (Character'Size =
                 Ada.Streams.Stream_Element'Size);
               Result := Univ_Strings.Length (Univ_Str) + Int_Stream_Len;
            end if;
         end;

      when Const_Value_Stream_Kind =>
         --  Return length of stream representation of global const
         declare
            Global_Const_Info : Const_Info renames
              Nth_Element (Compile_Time_Known_Consts,
                           CTK_Info_Index (Entity)).Info;
            Stream_Cache : constant Object_Virtual_Address :=
              Fetch_Word (Info_Array + Large_Obj_Header_Size +
                Offset_Within_Area'(2));
         begin
            Result := Natural
              (Fetch_Word (Stream_Cache + Large_Obj_Header_Size +
                Offset_Within_Area'(1)));
         end;
      when Large_Const_Component_Values =>
         --  Return number of component values presuming Element_Info
         --  identifies a large constant
         declare
            Element_Type_Desc : constant Type_Descriptor_Ptr :=
              To_Type_Desc_Or_Op_Map (Fetch_Word (Entity +
                Large_Obj_Header_Size + Offset_Within_Area'(0)));
            Element_Value : constant Word_Type :=
              Fetch_Word (Entity +
                Large_Obj_Header_Size + Offset_Within_Area'(2));
         begin
            if Element_Type_Desc.Is_Small then
               Result := 1;
            elsif Is_Large_Null (Element_Value) then
               --  Oh dear, we have a null value
               Result := 0;
            else
               --  Number of values
               Result := Natural (Large_Obj_Size (Element_Value) -
                           Large_Obj_Header_Size);
            end if;
         end;
      end case;

      --  Pass back the result
      Store_Word
        (Params, 0, Word_Type (Result));
   end Info_Array_Last;

   procedure Info_Array_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  op "indexing"(Arr : Info_Array; Index : Indexed_By)
      --    -> Elem_Type
      --    is import(#info_array_indexing)
      Info_Array : constant Object_Virtual_Address :=
        Fetch_Word (Params, 1);

      Entity : constant Word_Type :=
        Fetch_Word (Info_Array + Large_Obj_Header_Size);

      Info_Kind : constant Info_Kind_Enum :=
        Info_Kind_Enum'Val
          (Fetch_Word (Info_Array + Large_Obj_Header_Size +
             Offset_Within_Area'(1)));
      Index : constant Natural :=
        Natural (Fetch_Word (Params, 2));

      Target : constant Word_Type := Fetch_Word (Params, 0);
         --  Get "null" passed in to determine desired region

      Info_Array_Type : constant Non_Op_Map_Type_Ptr := Static_Link;

      Target_Type : constant Type_Descriptor_Ptr :=
        Info_Array_Type.Parameters (1).Data.Type_Desc;

      Result : Word_Type := Target;  --  Initialize to null

      procedure Fill_In_Param_Info (Param_Info : Parameter_Info) is
      --  Fill in a ParaSail Param_Info record given the Ada version
      begin
         --  Create the Param_Info object
         Result := Create_Large_Obj (Type_Desc    => Target_Type,
                                     Stg_Rgn      => Stg_Rgn_Of_Large_Obj
                                                       (Target),
                                     Server_Index => Context.Server_Index);
         --  Set the "Kind" field
         Store_Word
           (Result + Large_Obj_Header_Size,
            Parameter_Kind_Enum'Pos (Param_Info.Kind));

         --  Fill in the "Data : Element_Info" component
         Store_Word (Result +
           Large_Obj_Header_Size + Offset_Within_Area'(1),
           Create_Element_Info
             (Param_Info.Data,
              Target_Type.Components (2).Type_Desc,
              Stg_Rgn_Of_Large_Obj (Target),
              Context.Server_Index));

      end Fill_In_Param_Info;

   begin  --  Info_Array_Indexing

      case Info_Kind is
      when Type_Desc_Info_Kinds =>
         --  Info on a type descriptor
         declare
            Type_Desc : Type_Descriptor_Ptr :=
              To_Type_Desc_Or_Op_Map (Entity);
         begin
            if Info_Kind in Type_Info_Kinds then
               --  Get underlying non-op-map type
               Type_Desc := Skip_Over_Op_Map (Type_Desc);
            end if;

            case Type_Desc_Info_Kinds'(Info_Kind) is
            when Type_Parameters_Kind =>
               if Type_Desc.Parameters = null then
                  --  No parameters at all
                  Messages.Put_Error ("Type " &
                      Strings.To_String (Type_Desc.Name) &
                      " has no parameters.",
                    Interpreter.Execution_Source_Pos);
                  --  Return a null by default
                  raise Constraint_Error;

               elsif Index not in Type_Desc.Parameters'Range then
                  --  Index out of range
                  Messages.Put_Error ("Parameter index out of range:" &
                      Integer'Image (Index) & " not in" &
                      Integer'Image (Type_Desc.Parameters'First) & " .." &
                      Integer'Image (Type_Desc.Parameters'Last),
                    Interpreter.Execution_Source_Pos);
                  --  Return a null by default
                  raise Constraint_Error;
               end if;

               Fill_In_Param_Info (Type_Desc.Parameters (Index));

            when Actuals_Of_Formals_Kind =>
               Fill_In_Param_Info (Type_Desc.Actuals_Of_Formals (Index));

            when Components_Kind =>
               declare
                  Comp_Info : Component_Info
                    renames Type_Desc.Components (Index);
               begin
                  --  Create the result object
                  Result :=
                    Create_Large_Obj (Type_Desc    => Target_Type,
                                      Stg_Rgn      => Stg_Rgn_Of_Large_Obj
                                                        (Target),
                                      Server_Index => Context.Server_Index);

                  --  Set the "Type_Desc" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(0),
                    To_Word_Type (Comp_Info.Type_Desc));

                  --  Set the "Is_By_Ref" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(1),
                    Boolean'Pos (Comp_Info.Is_By_Ref));

                  --  Set the "Is_Optional" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(2),
                    Boolean'Pos (Comp_Info.Is_Optional));
               end;

            when Nested_Types_Kind =>
               --  Get the result
               Result := To_Word_Type (Type_Desc.Nested_Types (Index));

            when Nested_Objs_Kind =>
               declare
                  Info : Const_Info
                    renames Type_Desc.Nested_Objs (Index);
               begin
                  --  Create the Const_Info object
                  Result := Create_Large_Obj
                              (Type_Desc    => Target_Type,
                               Stg_Rgn      => Stg_Rgn_Of_Large_Obj (Target),
                               Server_Index => Context.Server_Index);

                  --  Set the "Name" field
                  Store_Word
                    (Result + Large_Obj_Header_Size,
                     To_Word_Type (Info.Name));

                  --  Fill in the "Data : Element_Info" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(1),
                    Create_Element_Info
                      (Info.Data,
                       Target_Type.Components (2).Type_Desc,
                       Stg_Rgn_Of_Large_Obj (Target),
                       Context.Server_Index));

               end;

            when Operations_Kind =>
               declare
                  Info : Routine_Info
                    renames Type_Desc.Operations (Operation_Index (Index));
               begin
                  --  Create the Routine_Info object
                  Result := Create_Large_Obj
                              (Type_Desc    => Target_Type,
                               Stg_Rgn      => Stg_Rgn_Of_Large_Obj (Target),
                               Server_Index => Context.Server_Index);

                  --  Set the "Index" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(0),
                    Word_Type (Info.Index));

                  --  Set the "Type_Desc" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(1),
                    To_Word_Type (Info.Type_Desc));

                  --  Set the "Action" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(2),
                    Wrapper_Action_Enum'Pos (Info.Action));

                  --  Set the "Op_Index" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(3),
                    Word_Type (Info.Op_Index));

                  --  Set the "Use_Static_Link_For_Type" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(4),
                    Boolean'Pos (Info.Use_Static_Link_For_Type));
               end;

            when Type_Desc_Stream_Kind =>
               declare
                  use Ada.Streams;
                  use String_Streams;
                  Stream_Cache : constant Object_Virtual_Address :=
                    Fetch_Word (Info_Array + Large_Obj_Header_Size +
                      Offset_Within_Area'(2));
                  Stream_Len : constant Stream_Element_Count :=
                    Stream_Element_Count
                      (Fetch_Word (Stream_Cache + Large_Obj_Header_Size +
                         Offset_Within_Area'(1)));
                  Stream_Start : constant Word_Ptr :=
                    Virtual_To_Physical_Address
                      (Stream_Cache + Large_Obj_Header_Size +
                        Offset_Within_Area'(2));
                  Stream_Arr : aliased Stream_Element_Array (1 .. Stream_Len);
                  pragma Import (Ada, Stream_Arr);
                  for Stream_Arr'Address use Stream_Start.all'Address;
               begin
                  --  Get the nth character
                  Result := Stream_Element'Pos
                    (Stream_Arr (Stream_Element_Offset (Index)));
               end;

            when Op_Map_Kind =>
               --  Get the result
               if Type_Desc.Has_Op_Map then
                  Result :=
                    Word_Type (Type_Desc.Op_Map (Operation_Index (Index)));
               else
                  --  Not defined
                  Result := Null_Value;
               end if;

            end case;
         end;

      when Routine_Info_Kinds =>
         declare
            Routine_Desc : constant Routine_Ptr := To_Routine_Ptr (Entity);
         begin
            case Routine_Info_Kinds'(Info_Kind) is
            when Routine_Parameters_Kind =>
               if Index not in Routine_Desc.Parameters'Range then
                  --  Index out of range
                  Messages.Put_Error ("Parameter index out of range:" &
                      Integer'Image (Index) & " not in" &
                      Integer'Image (Routine_Desc.Parameters'First) & " .." &
                      Integer'Image (Routine_Desc.Parameters'Last),
                    Interpreter.Execution_Source_Pos);
                  --  Return a null
                  Store_Word
                    (Params, 0, Target);
                  raise Program_Error;
               end if;
               declare
                  Info : Routine_Param_Info
                    renames Routine_Desc.Parameters (Index);
               begin
                  --  Create the Routine_Param_Info object
                  Result := Create_Large_Obj
                              (Type_Desc    => Target_Type,
                               Stg_Rgn      => Stg_Rgn_Of_Large_Obj (Target),
                               Server_Index => Context.Server_Index);

                  --  0. const Kind                    : Parameter_Kind_Enum
                  --  1. const Type_Info               : Object_Locator
                  --  2. const Is_Operation_Output     : Boolean
                  --  3. const Is_Var                  : Boolean
                  --  4. const Is_Passed_By_Ref        : Boolean
                  --  5. const Is_Optional             : Boolean
                  --  6. const Is_Of_Current_Inst_Type : Boolean
                  --  7. const Is_Declared_Ref         : Boolean
                  --  8. const Name                    : Univ_String
                  --  9. const Type_Name               : Univ_String
                  --  10. const Param_Decl             : Decl

                  --  Set the "Kind" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(0),
                    Parameter_Kind_Enum'Pos (Info.Kind));

                  --  Set the "Type_Info" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(1),
                    Insert_Object_Locator
                      (Info.Type_Info,
                       Target_Type.Components (2).Type_Desc,
                       Target,
                       Context.Server_Index));
                  --   Word_Type (Info.Type_Info.Base) * 2**32 +
                  --     Word_Type (Info.Type_Info.Offset));

                  --  Set the "Is_Operation_Output" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(2),
                    Boolean'Pos (Info.Is_Operation_Output));

                  --  Set the "Is_Var" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(3),
                    Boolean'Pos (Info.Is_Var));

                  --  Set the "Is_Passed_By_Ref" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(4),
                    Boolean'Pos (Info.Is_Passed_By_Ref));

                  --  Set the "Is_Optional" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(5),
                    Boolean'Pos (Info.Is_Optional));

                  --  Set the "Is_Of_Current_Inst_Type" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(6),
                    Boolean'Pos (Info.Is_Of_Current_Inst_Type));

                  --  Set the "Is_Declared_Ref" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(7),
                    Boolean'Pos (Info.Is_Declared_Ref));

                  --  Set the "Name" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(8),
                     To_Univ_String_Word (Info.Name, Target));

                  --  Set the "Type_Name" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(9),
                     To_Univ_String_Word (Info.Type_Name, Target));

                  --  Fill in the "Param_Decl" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(10),
                     To_Word_Type (Sem_Info (Info.Decl)));

               end;
            when Routine_Uplevel_Refs_Kind =>
               declare
                  Info : Uplevel_Info
                    renames Routine_Desc.Uplevel_Refs (Index);
               begin
                  --  Create the Uplevel_Info object
                  Result := Create_Large_Obj
                              (Type_Desc    => Target_Type,
                               Stg_Rgn      => Stg_Rgn_Of_Large_Obj (Target),
                               Server_Index => Context.Server_Index);

                  --  0. const Locator : Object_Locator
                  --  1. const Level   : Code_Nesting_Level
                  --  2. const Mode    : Access_Mode

                  --  Set the "Locator" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(0),
                    Insert_Object_Locator
                      (Info.Locator,
                       Target_Type.Components (1).Type_Desc,
                       Target,
                       Context.Server_Index));
                  --   Word_Type (Info.Locator.Base) * 2**32 +
                  --     Word_Type (Info.Locator.Offset));

                  --  Set the "Level" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(1),
                    Word_Type (Info.Level));

                  --  Set the "Mode" field
                  Store_Word (Result +
                    Large_Obj_Header_Size + Offset_Within_Area'(2),
                    Access_Mode'Pos (Info.Mode));

               end;
            end case;
         end;

      when String_Stream_Kind =>
         declare
            Univ_Str : constant Univ_Strings.Univ_String :=
              Univ_Strings.From_Word_Type (Entity);
            --  TBD: This is trying to simulate effect of 'Write
            use Ada.Streams;
            Int_Stream_Len : constant := Integer'Size / Stream_Element'Size;
            use type Strings.U_String;
         begin
            if Index <= Int_Stream_Len then
               --  First part is the length (or Integer'First if null)
               declare
                  Len : Integer;
                  subtype Int_Stream_Rep is Stream_Element_Array
                    (1 .. Int_Stream_Len);
                  function To_Stream_Rep is new Ada.Unchecked_Conversion
                    (Integer, Int_Stream_Rep);
                  Len_Stream_Rep : Int_Stream_Rep;
               begin
                  if Univ_Strings.Is_Null (Univ_Str) then
                     Len := Integer'First;
                  else
                     Len := Univ_Strings.Length (Univ_Str);
                  end if;

                  Len_Stream_Rep := To_Stream_Rep (Len);

                  Result := Stream_Element'Pos
                    (Len_Stream_Rep (Stream_Element_Offset (Index)));
               end;
            elsif Univ_Strings.Is_Null (Univ_Str) then
               --  Index is out of range
               Result := 0;
               raise Constraint_Error;
            else
               declare
                  Full_Str : String renames Word_To_String (Entity);
                  --  TBD: This presumes character'size = stream_element'size
                  pragma Assert (Character'Size =
                    Ada.Streams.Stream_Element'Size);
               begin
                  Result := Character'Pos
                    (Full_Str (Full_Str'First + Index - Int_Stream_Len - 1));
               end;
            end if;
         end;

      when Const_Value_Stream_Kind =>
         --  Return one element of stream representation of global const
         declare
            use Ada.Streams;
            use String_Streams;
            Global_Const_Info : Const_Info renames
              Nth_Element (Compile_Time_Known_Consts,
                           CTK_Info_Index (Entity)).Info;
            Stream_Cache : constant Object_Virtual_Address :=
              Fetch_Word (Info_Array + Large_Obj_Header_Size +
                Offset_Within_Area'(2));
            Stream_Len : constant Stream_Element_Count :=
              Stream_Element_Count
                (Fetch_Word (Stream_Cache + Large_Obj_Header_Size +
                   Offset_Within_Area'(1)));
            Stream_Start : constant Word_Ptr :=
              Virtual_To_Physical_Address
                (Stream_Cache + Large_Obj_Header_Size +
                  Offset_Within_Area'(2));
            Stream_Arr : aliased Stream_Element_Array (1 .. Stream_Len);
            pragma Import (Ada, Stream_Arr);
            for Stream_Arr'Address use Stream_Start.all'Address;
         begin
            --  Get the nth character
            Result := Stream_Element'Pos
              (Stream_Arr (Stream_Element_Offset (Index)));
         end;
      when Large_Const_Component_Values =>
         --  Return Nth component value presuming Element_Info
         --  identifies a large constant
         declare
            Element_Type_Desc : constant Type_Descriptor_Ptr :=
              To_Type_Desc_Or_Op_Map (Fetch_Word (Entity +
                Large_Obj_Header_Size + Offset_Within_Area'(0)));
            Element_Value : constant Word_Type :=
              Fetch_Word (Entity +
                Large_Obj_Header_Size + Offset_Within_Area'(2));
         begin
            if Element_Type_Desc.Is_Small then
               --  We have only one value (this must be a "wrapper"?)
               Result := Element_Value;
            elsif Is_Large_Null (Element_Value) then
               --  Oh dear, we have a null value; should not get here
               Result := Null_Value;
               raise Program_Error;
            else
               --  Fetch Nth component of constant
               Result := Fetch_Word (Element_Value +
                           Large_Obj_Header_Size +
                           Offset_Within_Area (Index - 1));
            end if;
         end;
      end case;

      --  Pass back the result
      Store_Word
        (Params, 0, Result);
   end Info_Array_Indexing;

begin  --  PSC.Trees.Semantics.Translator;

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#pfs_create"),
      PFS_Create'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#pfs_get_local_index"),
      PFS_Get_Local_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#pfs_num_elems"),
      PFS_Num_Elems'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#pfs_nth_elem"),
      PFS_Nth_Elem'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#pfs_reset"),
      PFS_Reset'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#num_library_items"),
      Num_Library_Items'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#nth_library_item"),
      Nth_Library_Item'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_id"),
      Decl_Id'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_module_name"),
      Decl_Module_Name'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_num_prior_homonyms"),
      Decl_Num_Prior_Homonyms'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_kind"),
      Decl_Kind'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_is_spec"),
      Decl_Is_Spec'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_spec"),
      Decl_Spec'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_context"),
      Decl_Context'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_level"),
      Decl_Level'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_convention"),
      Decl_Convention'Access);
   
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_tree_of"),
      Decl_Tree_Of'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_location"),
      Decl_Location'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_component_index"),
      Decl_Component_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#global_const_value"),
      Global_Const_Value'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_is_large_null"),
      Const_Is_Large_Null'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_info_at_locator"),
      Const_Info_At_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_value_at_locator"),
      Const_Value_At_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_value_locator"),
      Const_Value_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_info_for_value"),
      Const_Info_For_Value'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#const_value_init_stream"),
      Const_Value_Init_Stream'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#source_position_create"),
      Source_Position_Create'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#source_position_file"),
      Source_Position_File'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#source_position_line"),
      Source_Position_Line'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#source_position_col"),
      Source_Position_Col'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_opcode"),
      Instruction_Opcode'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_source_pos"),
      Instruction_Source_Pos'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Skip_Count"),
      Instruction_Skip_Count'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Skip_Counts"),
      Instruction_Skip_Counts'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Level_Diff"),
      Instruction_Level_Diff'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Params"),
      Instruction_Params'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Static_Link"),
      Instruction_Static_Link'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Call_Target"),
      Instruction_Call_Target'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Target_Index"),
      Instruction_Target_Index'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Locked_Param_Index"),
      Instruction_Locked_Param_Index'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Locked_Param_Info"),
      Instruction_Locked_Param_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Locked_Param_Is_Var"),
      Instruction_Locked_Param_Is_Var'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Locked_Param_Is_By_Ref"),
      Instruction_Locked_Param_Is_By_Ref'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Code_Block"),
      Instruction_Code_Block'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Assertion_Str"),
      Instruction_Assertion_Str'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Destination"),
      Instruction_Destination'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Dest_Name"),
      Instruction_Dest_Name'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Decl_Obj_Is_By_Ref"),
      Instruction_Decl_Obj_Is_By_Ref'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Decl_Obj_Is_Var"),
      Instruction_Decl_Obj_Is_Var'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Declare_Type_Info"),
      Instruction_Declare_Type_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Null_Type_Info"),
      Instruction_Null_Type_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Local_Addr"),
      Instruction_Local_Addr'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Int_Value"),
      Instruction_Int_Value'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Char_Value"),
      Instruction_Char_Value'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Real_Value"),
      Instruction_Real_Value'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Str_Value"),
      Instruction_Str_Value'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Existing_Str_In_Stg_Rgn"),
      Instruction_Existing_Str_In_Stg_Rgn'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Enum_Value"),
      Instruction_Enum_Value'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Operation_Static_Link"),
      Instruction_Operation_Static_Link'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Operation_Locator"),
      Instruction_Operation_Locator'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Source"),
      Instruction_Source'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Might_Be_Null"),
      Instruction_Might_Be_Null'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Type_Info"),
      Instruction_Type_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Existing_Obj_In_Stg_Rgn"),
      Instruction_Existing_Obj_In_Stg_Rgn'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Source_Type_Info"),
      Instruction_Source_Type_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Ancestor_Lvalue"),
      Instruction_Ancestor_Lvalue'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Polymorphic_Ancestor_Lvalue"),
      Instruction_Polymorphic_Ancestor_Lvalue'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_If_Source"),
      Instruction_If_Source'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_If_Condition"),
      Instruction_If_Condition'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Skip_If_False"),
      Instruction_Skip_If_False'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Master"),
      Instruction_Parallel_Master'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Control"),
      Instruction_Parallel_Control'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Static_Link"),
      Instruction_Parallel_Static_Link'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Code_Block"),
      Instruction_Parallel_Code_Block'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Call_Target"),
      Instruction_Parallel_Call_Target'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Target_Index"),
      Instruction_Parallel_Target_Index'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Locked_Param_Index"),
      Instruction_Parallel_Locked_Param_Index'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Locked_Param_Info"),
      Instruction_Parallel_Locked_Param_Info'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Locked_Param_Is_Var"),
      Instruction_Parallel_Locked_Param_Is_Var'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Locked_Param_Is_By_Ref"),
      Instruction_Parallel_Locked_Param_Is_By_Ref'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Parallel_Is_Queued_Call"),
      Instruction_Parallel_Is_Queued_Call'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Num_In_Params"),
      Instruction_Num_In_Params'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Num_Out_Params"),
      Instruction_Num_Out_Params'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Case_Selector"),
      Instruction_Case_Selector'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Case_First"),
      Instruction_Case_First'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Case_Last"),
      Instruction_Case_Last'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Case_Default_Skip"),
      Instruction_Case_Default_Skip'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Nested_Code_Block"),
      Instruction_Nested_Code_Block'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Nested_Block_Region"),
      Instruction_Nested_Block_Region'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Proved"),
      Instruction_Proved'Access);
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#instruction_Output_Inited_Null"),
      Instruction_Output_Inited_Null'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#operation_equiv_to"),
      Operation_Equiv_To'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_for_operation"),
      Routine_For_Operation'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#descriptor_for_type"),
      Descriptor_For_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_region"),
      Decl_Region'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#body_region"),
      Body_Region'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#decl_source_pos"),
      Decl_Source_Pos'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#name_for_object_locator"),
      Name_For_Object_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_kind"),
      Region_Kind'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_index"),
      Region_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_at_index"),
      Region_At_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_associated_decl"),
      Region_Associated_Decl'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_produces_nested_block"),
      Region_Produces_Nested_Block'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_num_items"),
      Region_Num_Items'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_nth_item"),
      Region_Nth_Item'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_num_nested_regions"),
      Region_Num_Nested_Regions'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_nth_nested_region"),
      Region_Nth_Nested_Region'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_sibling_region"),
      Region_Sibling_Region'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_num_trees"),
      Region_Num_Trees'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#region_nth_tree"),
      Region_Nth_Tree'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_kind"),
      Tree_Kind'Access);
   
   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_num_operands"),
      Tree_Num_Operands'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_nth_operand"),
      Tree_Nth_Operand'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_pre_annotation"),
      Tree_Pre_Annotation'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_post_annotation"),
      Tree_Post_Annotation'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_resolved_type"),
      Tree_Resolved_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_decl_of"),
      Tree_Decl_Of'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_unary_op"),
      Tree_Unary_Op'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_binary_op"),
      Tree_Binary_Op'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_lit_kind"),
      Tree_Lit_Kind'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_identifier"),
      Tree_Identifier'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#tree_source_pos"),
      Tree_Source_Pos'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#new_conv_desc"),
      New_Conv_Desc'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#null_conv_desc"),
      Null_Conv_Desc'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#cd_convention"),
      CD_Convention'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#cd_num_inputs"),
      CD_Num_Inputs'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#cd_num_outputs"),
      CD_Num_Outputs'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#cd_output_needs_init"),
      CD_Output_Needs_Init'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#cd_uses_queuing"),
      CD_Uses_Queuing'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_num_instrs"),
      Routine_Num_Instrs'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_nth_instr"),
      Routine_Nth_Instr'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_start_callee_locals"),
      Routine_Start_Callee_Locals'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_internal_precond"),
      Routine_Internal_Precond'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_nesting_level"),
      Routine_Nesting_Level'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_convention"),
      Routine_Convention'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_conv_desc"),
      Routine_Conv_Desc'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_module_name"),
      Routine_Module_Name'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_name"),
      Routine_Name'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_num_prior_homonyms"),
      Routine_Num_Prior_Homonyms'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_name_with_overloading_index"),
      Routine_Name_With_Overloading_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_built_in_desig"),
      Routine_Built_In_Desig'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_enc_type_desc"),
      Routine_Enc_Type_Desc'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_uses_queuing"),
      Routine_Uses_Queuing'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_uses_stg_rgn"),
      Routine_Uses_Stg_Rgn'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_num_vm_regs"),
      Routine_Num_VM_Regs'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_at_locator"),
      Routine_At_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#routine_at_index"),
      Routine_At_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#set_breakpoint"), Set_Breakpoint'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#clear_breakpoint"), Clear_Breakpoint'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#num_breakpoints"),
      Num_Breakpoints'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#nth_breakpoint_routine"),
      Nth_Breakpoint_Routine'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#nth_breakpoint_line"),
      Nth_Breakpoint_Line'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_at_locator"),
      Type_Desc_At_Locator'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_at_index"),
      Type_Desc_At_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_has_op_map"),
      Type_Desc_Has_Op_Map'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_polymorphic_type"),
      Type_Desc_Corresponding_Polymorphic_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_name"),
      Type_Desc_Name'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_type_decl"),
      Type_Desc_Type_Decl'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_index"),
      Type_Desc_Index'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_kind"),
      Type_Desc_Type_Kind'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_all_parameters_known"),
      Type_Desc_All_Parameters_Known'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_small"),
      Type_Desc_Is_Small'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_large"),
      Type_Desc_Is_Large'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_wrapper"),
      Type_Desc_Is_Wrapper'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_null_value"),
      Type_Desc_Null_Value_For_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_parent_type"),
      Type_Desc_Parent_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_abstract"),
      Type_Desc_Is_Abstract'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_concurrent"),
      Type_Desc_Is_Concurrent'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_is_polymorphic"),
      Type_Desc_Is_Polymorphic'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_enclosing_type"),
      Type_Desc_Enclosing_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_root"),
      Type_Desc_Root_Type_Desc'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#type_desc_init_stream"),
      Type_Desc_Init_Stream'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#info_array_first"),
      Info_Array_First'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#info_array_last"),
      Info_Array_Last'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#info_array_indexing"),
      Info_Array_Indexing'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#op_map_actual_type"),
      Op_Map_Actual_Type'Access);

   Interpreter.Builtins.Register_Builtin
     (Strings.String_Lookup ("#op_map_formal_type_decl"),
      Op_Map_Formal_Type_Decl'Access);

end PSC.Trees.Semantics.Translator;
