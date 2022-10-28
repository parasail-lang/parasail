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

with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with PSC.Hash_Tables;
with PSC.Interpretations;
with PSC.Interpreter;
with PSC.Object_Access;
with PSC.Strings;         use type PSC.Strings.U_String;
with PSC.String_Streams;
with PSC.Stream_Output;
with PSC.Symbols;         use PSC.Symbols;

with PSC.Trees.Lists;
with PSC.Trees.Visitor;

with PSC.Trees.Annotation;
with PSC.Trees.Assign_Stmt;
with PSC.Trees.Binary;
with PSC.Trees.Block_Stmt;
with PSC.Trees.Case_Construct;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Conditional;
with PSC.Trees.For_Loop_Construct;
with PSC.Trees.Identifier;
with PSC.Trees.Implements_Element;
with PSC.Trees.Invocation;
with PSC.Trees.Iterator;
with PSC.Trees.Module;
with PSC.Trees.Operation;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Param_Decl;
with PSC.Trees.Qualifier;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Reference;
with PSC.Trees.Selection;
with PSC.Trees.Type_Decl;
with PSC.Trees.Unary;
with PSC.Trees.While_Stmt;

with PSC.Trees.Semantics.Debug; use PSC.Trees.Semantics.Debug;
with PSC.Trees.Semantics.Info;  use PSC.Trees.Semantics.Info;

with PSC.Vectors;

pragma Elaborate (Ada.Text_IO);
pragma Elaborate (PSC.Strings);
pragma Elaborate (PSC.Trees.Assign_Stmt);
pragma Elaborate (PSC.Trees.Binary);

private package PSC.Trees.Semantics.Static is

   --  Static Semantics

   use Semantics.Debug;
   use type Info.Static_Level;

   procedure Init_Language_Specific_Info;
   --  This is called once at the beginning of processing after establishing
   --  the language being parsed, within the ParaSail "family" of languages.

   ------------- First pass actions ------------

   procedure First_Pass
     (R : Symbols.Region_Ptr;
      Decl : Optional_Tree;
      Visiting_Operation_Outputs : Boolean := False;
      Processing_Inherited_Decl : Boolean := False);
   --  Apply first pass to Decl

   procedure First_Pass_List
     (R : Symbols.Region_Ptr;
      Decl_List : Lists.List;
      Visiting_Operation_Outputs : Boolean := False;
      Processing_Inherited_Decl : Boolean := False);
   --  Apply First_Pass to each element in Decl_List

   --------- Analysis mode for second pass of semantic analysis --------

   type Analysis_Mode is (Decls_Only, Decls_And_Exprs, Exprs_Only);
   subtype Analyze_Decls is Analysis_Mode range Decls_Only .. Decls_And_Exprs;
   subtype Analyze_Exprs is Analysis_Mode range Decls_And_Exprs .. Exprs_Only;

   --  NOTE: Analysis mode is used to allow forward references in expressions
   --        to declarations that come later.
   --        Also, Analysis mode is used to ensure we analyze interfaces
   --        before we analyze classes.
   --        Generally we analyze a scope first in Decls_Only,
   --        and then in Exprs_Only.  However, for local scopes of operations,
   --        we analyze both at once, allowing operations to be understood
   --        (by the human reader as well) in a single pass.

   --------- Overriding state for second pass of semantic analysis ---------

   type Overriding_State is (Unspecified, Must_Not_Override, Must_Override);

   ------------- Second pass actions ------------

   procedure Second_Pass
     (R : Symbols.Region_Ptr;
      Decl : Optional_Tree;
      Context : Context_Enum := No_Context;
      Formal_Prefix : Optional_Tree := Null_Optional_Tree;
      May_Override : Overriding_State := Unspecified;
      Mode : Analysis_Mode := Decls_And_Exprs;
      Decl_For_Annotations : Optional_Tree := Null_Optional_Tree);
   --  Second pass for Decl

   procedure Second_Pass_List
     (R : Symbols.Region_Ptr;
      Decl_List : Lists.List;
      Context : Context_Enum := No_Context;
      Formal_Prefix : Optional_Tree := Null_Optional_Tree;
      May_Override : Overriding_State := Unspecified;
      Mode : Analysis_Mode := Decls_And_Exprs;
      Decl_For_Annotations : Optional_Tree := Null_Optional_Tree);
   --  Apply Second_Pass to each element in Decl_List

   procedure Analyze_Annotation (T : in out Annotation.Tree);
   --  Analyze annotation, now that other semantic analysis has been performed

   --------- Support routines for Interpretations and Info packages ----------

   function Equiv_Interps (Interp1, Interp2 : Optional_Tree) return Boolean;
   --  Return True if Interp1 and Interp2 are equivalent as far
   --  as overload resolution.

   function Equiv_Tree (Left, Right : Optional_Tree) return Boolean;
   --  Return True if Left and Right are equivalent as far as use
   --  as parameters in a module instantiation.

   function Hash_Tree (OT : Optional_Tree) return Hash_Type;
   --  Return hash of tree, with hash being equal for two Equiv trees

   function Num_Module_Parameters (Mod_Sem : Module_Sem_Ptr) return Natural;
   --  Return number of formals, including any inherited ones.

   function Nth_Module_Parameter
     (Mod_Sem : Module_Sem_Ptr;
      N : Positive)
      return Optional_Tree;
   --  Return Nth module parameter, including ancestor module formals
   --  if any

   function Num_Enclosing_Module_Parameters
     (Decl_Region : Symbols.Region_Ptr)
      return Natural;
   --  Return count of total number of parameters of enclosing modules,
   --  if any.  Return 0 if Decl_Region is Library_Region.

   function Module_Formal_Index
     (Formal_Sym : Symbols.Sym_Ptr)
      return Natural;
   --  Return index for formal of module, based on its Symbol index

   --------------- Routines to look for special operations -------------

   Combine_Move_Op_Str : constant Strings.U_String :=
     Assign_Stmt.Assign_Operator_Designator (Assign_Stmt.Combine_Move_Op);
   --  "<|=" operator

   Index_Set_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""index_set""");
   --  Operator to retrieve set of keys of container

   Indexing_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""indexing""");
   --  Operator for indexing into a container

   Slicing_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""slicing""");
   --  Operator for selecting a slice of a container.

   Empty_Container_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""[]""");
   --  "[]" operator

   Empty_Map_Set_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""{}""");
   --  "{}" operator

   Empty_Op_Strs : constant array
     (Invocation.Container_Aggregate .. Invocation.Map_Set_Aggregate)
       of Strings.U_String :=
         (Invocation.Container_Aggregate => Empty_Container_Op_Str,
          Invocation.Map_Set_Aggregate   => Empty_Map_Set_Op_Str);

   Var_Indexing_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""var_indexing""");
   --  Operator for assigning into an element of a variable container
   --  and growing it as necessary.

   Compare_Op_Str : constant Strings.U_String :=
     Binary.Binary_Operator_Designator (Binary.Compare_Op);
   --  Operator for comparing two values and returning an "ordering"

   Ref_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""ref""");
   --  Operator for turning a "ref object" into a simple "ref"

   Var_Ref_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""var_ref""");
   --  Operator for turning a "ref object" into a simple "ref var"

   End_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""end""");
   --  Operator for finalizing a "ref object"

   Var_End_Op_Str : constant Strings.U_String :=
     Strings.String_Lookup ("""var_end""");
   --  Operator for finalizing a "ref object" to which "var_ref" has been
   --  applied.

   function Return_True
     (Operand_Type : Type_Sem_Ptr;
      Input_Types : Type_Sem_Array;
      Output_Types : Type_Sem_Array)
      return Boolean;
   --  Default for Has_Desired_Signature function

   generic
      Default_Op_Name : Strings.U_String;
      Num_Inputs : Natural;
      Num_Outputs : Natural;
      with function Has_Desired_Signature
        (Operand_Type : Type_Sem_Ptr;
         Input_Types : Type_Sem_Array;
         Output_Types : Type_Sem_Array)
         return Boolean
      is Return_True;
   function Find_Op_For
     (Operand_Type : Type_Sem_Ptr;
      Op_Name : Strings.U_String := Default_Op_Name)
      return Operation_Sem_Ptr;
   --  Return Operation_Sem_Ptr for operation of given Op_Name
   --  for the given type, with the given number of
   --  inputs and outputs, and for which Has_Desired_Signature
   --  returns True.  Return null if none found.

   function First_Input_Is_Operand_Type
     (Operand_Type : Type_Sem_Ptr;
      Input_Types : Type_Sem_Array;
      Output_Types : Type_Sem_Array)
      return Boolean;
   --  Binding for Has_Desired_Signature function
   --  which returns True if first input is of Operand type

   function First_Output_Is_Operand_Type
     (Operand_Type : Type_Sem_Ptr;
      Input_Types : Type_Sem_Array;
      Output_Types : Type_Sem_Array)
      return Boolean;
   --  Binding for Has_Desired_Signature function
   --  which returns True if first output is of Operand type

   function First_Input_Is_Operand_Type_And_Second_Is_Not
     (Operand_Type : Type_Sem_Ptr;
      Input_Types : Type_Sem_Array;
      Output_Types : Type_Sem_Array)
      return Boolean;
   --  Binding for Has_Desired_Signature function
   --  which returns True if first input is of Operand type

   function Find_One_In_One_Out_Op_For
     (Operand_Type : Type_Sem_Ptr;
      Op_Name : Strings.U_String := Strings.Null_U_String)
      return Operation_Sem_Ptr;
   --  Find operation with one input of given type, and one output

   function Find_Index_Set_Op_For
     (Operand_Type : Type_Sem_Ptr;
      Op_Name : Strings.U_String := Index_Set_Op_Str)
      return Operation_Sem_Ptr renames Find_One_In_One_Out_Op_For;
   --  Different op name, everything else the same

   function Find_Indexing_Op_For
     (Operand_Type : Type_Sem_Ptr;
      Op_Name : Strings.U_String := Indexing_Op_Str)
      return Operation_Sem_Ptr;
   --  Return Operation_Sem_Ptr for "indexing" for
   --  the given container type.  If none, return null.
   --  TBD: What to do if there is more than one?

   function Find_Combine_Move_Op_For
     (Operand_Type : Type_Sem_Ptr;
      Op_Name : Strings.U_String := Combine_Move_Op_Str)
      return Operation_Sem_Ptr;
   --  Find "<|=" operation for elements, not whole containers

   function Get_Remove_Func
     (Set_Type : Type_Sem_Ptr;
      Iterator_Dir : Interpreter.Direction := Interpreter.Unordered_Dir)
      return Operation_Sem_Ptr;
   --  Return appropriate Remove_* func to use for
   --  given iterator direction.
   --  Return null if no func found.

   function Get_To_Univ_Op (From : Type_Sem_Ptr; To : Type_Sem_Ptr)
     return Operation_Sem_Ptr;
   --  Return Operation_Sem_Ptr for to-univ operation from
   --  the given non-universal From type to the given univ To type
   --  Return null if no such operation exists in module defining "From" type.

   function Get_From_Univ_Op (From : Type_Sem_Ptr; To : Type_Sem_Ptr)
     return Operation_Sem_Ptr;
   --  Return Operation_Sem_Ptr for to-univ operation from
   --  the given univ From type to the given non-univ To type.
   --  Return null if no such operation exists in module defining "To" type

   function Implicitly_Converts (From, To : Type_Sem_Ptr) return Boolean;
   --  Return True if "From" can be implicitly converted to "To"
   --  using "from_univ" or "to_univ"

   function Get_Ref_Func_Output_Type
     (Ref_Obj_Type : Type_Sem_Ptr)
     return Type_Sem_Ptr;
   --  Return output type of "ref" operation on given ref-obj type
   --  Return null if no "ref" operation found

   --------------- Support for Dynamic semantics package ------------

   function Boolean_Type return Type_Sem_Ptr;
   --  Initialized after second pass.

   function Exception_Type return Type_Sem_Ptr;
   --  Init'ed after second pass.

   function Copy_Resolved_Tree
     (Orig_Tree : Optional_Tree)
      return Optional_Tree;
   --  Make a one-level copy of given tree, along with
   --  a copy of sem info, so can be updated

   function Interface_Part
     (Module_Sem : Module_Sem_Ptr)
      return Module_Sem_Ptr;
   --  Return Interface part, if any of given module.

   function Find_Enclosing_Module_Interface
     (Decl_Region : Symbols.Region_Ptr)
      return Module_Sem_Ptr;
   --  Return semantic info for interface of enclosing module,
   --  or null if none.

   function Find_Enclosing_Operation
     (Decl_Region : Symbols.Region_Ptr)
      return Operation_Sem_Ptr;
   --  Return semantic info for enclosing operation, or null if none.

   function Find_Enclosing_Stmt
     (Decl_Region : Symbols.Region_Ptr;
      Applies_To : Control_Stmt.Exitable_Construct_Enum;
      Id : Optional_Tree)
      return Optional_Tree;
   --  Return tree for enclosing contruct of specified kind and id.
   --  Return Null_Optional_Tree if not found.

   function All_Elems_Are_Refs (Elems : Lists.List) return Boolean;
   --  Return True if all elements in the list are Reference.Trees.
   --  In an annotation, these represent specifications of aspects
   --  of representation, such as "convention => #ada"
   --  and are currently being largely ignored.

   function Inside_Parallel_Construct
     (Decl_Region : Symbols.Region_Ptr;
      Up_Through : Composite_Stmt_Sem_Ptr := null;
      Up_To      : Composite_Stmt_Sem_Ptr := null)
     return Boolean;
   --  Return True if given Decl_Region is for a parallel construct
   --  or is enclosed by one, up through/to specified statement, if any.

   function Sym_Is_By_Ref (Id_Sym : Symbols.Sym_Ptr) return Boolean;
   --  Return True if sym is represented as a "ref" rather than a value.
   --  NOTE: Even if object is declared using "ref", it is represented
   --       as a value instead if the object contains a ref component,
   --       or the object is concurrent.

   function Param_Is_Passed_By_Ref
     (Param_Sem : Param_Sem_Ptr;
      Kind : Param_Decl.Param_Kind;
      Locking : Param_Decl.Param_Locking)
      return Boolean;
   --  Whether parameter is passed by reference.

   function Sym_Is_Declared_Ref (Id_Sym : Symbols.Sym_Ptr) return Boolean;
      --  Return True if sym is declared as a "ref" whether or not it
      --  is represented as a "ref."

   function Sym_Is_Variable (Id_Sym : Symbols.Sym_Ptr;
     Treat_Ref_As_Variable : Boolean := False) return Boolean;
      --  Return True if sym may appear on LHS of assignment
      --  or be passed as a "var" or "ref var" parameter.
      --  If it is simply a "ref" then return True iff Treat_Ref_As_Variable
      --  is True.

   function Sem_Info_Is_For_Variable (Obj_Sem : Sem_Ptr) return Boolean;
   --  Return True if Obj_Sem identifies an entity that is a variable,
   --  i.e. can be used on LHS of assignment or passed as a "var" parameter.

   function Sym_Is_Component
     (Sym : Symbols.Sym_Ptr;
      Usable_In_Aggregate : Boolean := False)
      return Boolean;
   --  Return True if sym refers to a component of a module

   function Type_Is_Concurrent
     (Type_Sem : Type_Sem_Ptr)
     return Boolean;
   --  Return True if type has explicit "concurrent" specified
   --  or is based on a concurrent module.

   function Is_Unlocked_Concurrent_Operand
     (Opnd_Sem : Operand_Sem_Ptr)
      return Boolean;
   --  Return True if operand is of a type that is marked
   --  "concurrent" or is based on a concurrent module.

   function Num_Initial_Value_Iterators
     (For_Loop_Sem : For_Loop_Construct_Sem_Ptr)
      return Natural;
   --  Number of iterators of for-loop that require "next" values
   --  because they don't have one specified.

   function Type_Def_Has_New_Module (Type_Def : Optional_Tree)
     return Boolean;
   --  Return True if Type_Def defines a type by an instance of a newly
   --  defined module (that is, the Prefix of the instantiation is itself
   --  a module definition).
   --  NOTE: This is currently only meaningful for Sparkel

   function Module_Is_Wrapper (Mod_Sem : Module_Sem_Ptr) return Boolean;
   --  Return True if non-concurrent module has only one component
   --  which is *not* a "ref"

   function Type_Is_Wrapper (Obj_Type : Type_Sem_Ptr) return Boolean;
   --  Return True if type is a wrapper.  Generally this
   --  simply passes the buck to "Module_Is_Wrapper," but
   --  if the type is polymorphic, then it is never considered
   --  a wrapper, even if the root type is a wrapper.

   function Type_Wraps_Optional_Component (Obj_Type : Type_Sem_Ptr)
     return Boolean;
   --  Return True if type is a wrapper and only component is optional,
   --  directly or indirectly.

   function Unwrapped_Type (Type_Sem : Type_Sem_Ptr) return Type_Sem_Ptr;
   --  Return underlying type which is not a wrapper type.

   function Known_To_Be_Small (Obj_Type : Type_Sem_Ptr) return Boolean;
   --  If it is known at compile-time that the given type
   --  will always be "small" (i.e. a single 64-bit word or less)
   --  then return True.  If the object might be "large" in
   --  certain instantiations of the module, then return False.

   function Is_Compile_Time_Known (Operand : Optional_Tree;
     Disallow_Concurrent_Types : Boolean := False;
     Diagnose : Boolean := False) return Boolean;
      --  Return True if given operand is compile-time known.
      --  If Disallow_Concurrent_Types is true, then disallow having
      --  any parameters of a concurrent type.
      --  TBD: This is relatively simple for now.
      --      Could get more sophisticated eventually.

   function Num_Components (Mod_Sem : Module_Sem_Ptr) return Natural;
   --  Return count of components of given module usable in a class aggregate.
   --  If the module is an extension of a parent module,
   --  then the nearest ancestor with a constructor is considered the
   --  first component, and then components of proper descendants
   --  of that ancestor form the remaining components.

   function Nth_Component
     (Mod_Sem : Module_Sem_Ptr;
      N : Positive)
      return Optional_Tree;
   --  Return nth component of given module; will be an Obj_Decl.Tree

   function Component_Index
     (Mod_Sem : Module_Sem_Ptr;
      Comp_Decl : Optional_Tree;
      Usable_In_Aggregate : Boolean := True)
      return Natural;
   --  Return index of component if is a component of the module
   --  else return 0 if is an operation or some other non-component
   --  If Usable_In_Aggregate is False, then it will return component
   --  index but it might not match Compoennt_Extension_Level of Mod_Sem.

   function Module_With_Formal_Type
     (Formal_Type : Type_Sem_Ptr)
      return Module_Sem_Ptr;
   --  Return Module that has the given formal type.
   --  Return null if not found.

   function Formal_Type_Index (Formal_Type : Type_Sem_Ptr) return Natural;
   --  Return index for formal type, or 0 if none

   procedure Diagnose_Unknown_Parameters
     (Type_Sem : Type_Sem_Ptr;
      Disallow_Concurrent_Types : Boolean := False;
      Indent : Natural := 0);
   --  Provide more information on why Type_Sem.All_Parameters_Known
   --  is False.

   procedure Dump_Type (Expr_Type : Type_Sem_Ptr);
   pragma Export (Ada, Dump_Type, "dump_type");
   --  Display info about given type.

   function Corresponding_Ancestor_Type
     (Descendant_Type : Type_Sem_Ptr;
      Ancestor_Module : Module_Sem_Ptr)
      return Type_Sem_Ptr;
   --  Follow the Parent_Type chain up from Descendant_Type
   --  until finding a type associated with the given Ancestor_Module

   function Is_Ancestor
     (Ancestor, Descendant : Module_Sem_Ptr)
      return Boolean;
   --  Return True if Ancestor is an ancestor of Descendant,
   --  or equal to Descendant.

   function Signatures_And_Modes_Match
     (Op_Sem1, Op_Sem2 : Operation_Sem_Ptr;
      Extra_Subst1, Extra_Subst2 : Type_Sem_Ptr := null;
      Substitute_Using1 : Type_Region_Ptr := null;
      Substitute_Using2 : Type_Region_Ptr := null;
      Complain_If_Different : Boolean := False;
      Allow_New_Type1 : Boolean := False;
      Allow_New_Type2 : Boolean := False;
      Diagnose : Boolean := False)
      return Boolean;
   --  Return True if two operations have same type
   --  for each parameter, or if a given parameter has
   --  Extra_Subst1/2 as corresponding types (these are
   --  always considered to match (e.g. current insts of
   --  relevant modules).  Also check modes for a match.
   --  Before comparing, substitute the types of the first
   --  operation with the Substitute_Using1 type region, if non null,
   --  and the types of the second operation with the
   --  Substitute_Using2 type region, if non null.
   --  If "Complain_If_Different" is True, then verify that names, defaults,
   --  etc. match, and complain if not, using source pos of Op_Sem2.
   --  If "Allow_New_Type1" is True, then allow type of param in first
   --  signature to be a "new" type relative to param in second.
   --  If "Allow_New_Type2" is True, then allow type of param in second
   --  signature to be a "new" type relative to param in first.

   function Instantiate_Module
     (Mod_Sem : Module_Sem_Ptr;
      Actual_Params : Sem_Info_Array;
      Decl_Region : Symbols.Region_Ptr := null;
      Enclosing_Type : Type_Sem_Ptr := null;
      Is_Formal_Type : Boolean := False;
      Formal_Prefix : Optional_Tree := Null_Optional_Tree;
      Associated_Generic_Op : Symbols.Sym_Ptr := null;
      Source_Pos : Source_Positions.Source_Position :=
     Source_Positions.Null_Source_Position)
      return Type_Sem_Ptr;
   --  Instantiate module with given actual parameters

   --  Information used when doing substitution while
   --  in the middle of doing an instantiation.
   --  See Substitute_Actuals_From_Instantiation for how these are used.
   type Instantiation_Info_Record is record
      Instantiation_Module : Module_Sem_Ptr;
      Instantiation : Optional_Tree;
      Decl_Region : Region_Ptr;
   end record;
   type Instantiation_Info_Ptr is access constant Instantiation_Info_Record;

   function Substitute_Actuals
     (Param_Type : Type_Sem_Ptr;
      Assoc_Type_Region : Type_Region_Ptr;
      Instantiation_Info : Instantiation_Info_Ptr := null;
      Extra_Subst : Param_Mapping_Ptr := null;
      Ancestor_From_Formal_Module : Boolean := False;
      Associated_Operation : Sym_Ptr := null)
      return Type_Sem_Ptr;
   --  If Param_Type is a formal type or depends on one,
   --  substitute in the actuals from the Assoc_Type_Region
   --  to produce the "actual" Param_Type.
   --  If Assoc_Type_Region and Param_Type are from the same Module,
   --  and Assoc_Type_Region is a formal type and Param_Type
   --  is a current instance, then substitute Assoc_Type_Region
   --  for Param_Type.
   --  If Extra_Subst is non-null, then it represents
   --  additional substitutions that are to be performed.
   --  If Ancestor_From_Formal_Module is True, then
   --  return the subtype which is the ancestor of the
   --  substituted type which is from the same module as the
   --  formal type.
   --  If Associated_Operation is non-null, then if its enclosing region
   --  does *not* match the enclosing region of Assoc_Type_Region but *does*
   --  match that of Param_Type, and both Param_Type and Assoc_Type_Region
   --  are "new" types and Param_Type is an ancestor of Assoc_Type_Region,
   --  then substitute Assoc_Type_Region for Param_Type.
   --  match enclosing region of
   --  TBD: We should "or" in Optionality and Constraints.

   function Substitute_Actuals_In_Operand
     (Operand : Optional_Tree;
      Assoc_Type_Region : Type_Region_Ptr;
      Instantiation_Info : Instantiation_Info_Ptr := null;
      Extra_Subst : Param_Mapping_Ptr := null;
      Always_Copy_Tree : Boolean := False)
      return Optional_Tree;
   --  Replace any use of module formal parameters with actual parameters
   --  in the given operand.

   function Substitute_Actuals_In_Nested_Type
     (Nested_Type : Type_Sem_Ptr;
      Assoc_Type_Region : Type_Region_Ptr;
      Is_Formal_Type : Boolean := False;
      Instantiation_Info : Instantiation_Info_Ptr := null;
      Extra_Subst : Param_Mapping_Ptr := null)
      return Type_Sem_Ptr;
   --  Handle a nested type; needs something substituted in one
   --  of the actuals to the instantiation defining the type.
   --  Create a new instantiation of the same module
   --  with the actual substituted.
   --  If Is_Formal_Type is True, then do not complain if
   --  result has unknown parameters, and there is
   --  no enclosing module.

   function Corresponding_Progenitor_Type
     (Starting_Type : Type_Sem_Ptr;
      Progenitor_Module : Module_Sem_Ptr)
      return Type_Sem_Ptr;
   --  If Associated_Module of Starting_Type does not match Progenitor_Module,
   --  search chain of ancestors and tree of implemented interfaces of
   --  Starting_Type to find an implementation of Progenitor_Module, and
   --  return corresponding type.
   --  Return null if no such progenitor found.

   function Op_Map_Type_Desc_Index
      (Obj_Type : Type_Sem_Ptr;
       Formal_Type : Type_Sem_Ptr;
       Source_Pos : Source_Positions.Source_Position)
      return Type_Sem_Vectors.Elem_Index;
   --  Return index for Op-Map for Obj_Type viewed as Formal_Type

   function Type_Has_Val_Params_Or_Consts (Type_Sem : Type_Sem_Ptr)
     return Boolean;
   --  Return True if (formal) type has value parameters or
   --  visible constants which are *not* compile-time known.

   function Is_Extended_Return (OT : Optional_Tree) return Boolean;
   --  Return True if OT is the body of an "extended" return statement,
   --  which is indicated by a "then" operation, where the LHS of the
   --  "then" is the declaration of a "return object" and the RHS
   --  is the handled sequence of statements, if any.

end PSC.Trees.Semantics.Static;
