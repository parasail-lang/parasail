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

with PSC.Hash_Tables;
with PSC.Interpretations;
with PSC.Interpreter;
with PSC.Languages;
with PSC.Object_Access;
with PSC.Strings;         use type PSC.Strings.U_String;
with PSC.String_Streams;
with PSC.Symbols;         use PSC.Symbols;
with PSC.Vectors;

with PSC.Trees.Binary;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Identifier;
with PSC.Trees.Invocation;
with PSC.Trees.Iterator;
with PSC.Trees.Operation;

with PSC.Trees.Lists;

private package PSC.Trees.Semantics.Info is

   subtype Static_Level is Interpreter.Code_Nesting_Level;
   --  Level 0 = level of static constants
   --  Level 1 = level of parameters/locals of a level 0 operation
   --  Level 2 = level of parameters/locals of a block nested in a level 0 op
   --  etc...
   --  NOTE: level does *not* increase when merely entering a nested scope.
   --        Level increases only when going into a nested code block
   --        or a nested operation, so it reflects *code* nesting level,
   --        not region nesting level.

   type Object_Location_Info is record
   --  Location
      Obj_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      Obj_Level : Static_Level := 0;
   end record;

   type Object_Location_Ptr is access all Object_Location_Info;

   ----------- Various Semantic_Info extensions -----------

   subtype Hash_Type is Strings.Hash_Type;

   Max_Num_Blocks : constant := 1000;
   type Block_Index is range 0 .. Max_Num_Blocks;
   --  Block_Index is used to identify nested blocks of an operation.

   type Instr_Loc is record
   --  This is the information on a start/add-parallel or
   --  check/call_block instruction which needs to be fixed
   --  up once we have finished creating a nested block.
   --  This is also used for keeping track of exit_ops which
   --  need to have their skip_count fixed up.
      Block : Block_Index := 0;

      --  NOTE: We initialize this to "1" to avoid error if this
      --        is used as an index into an Instruction_Sequence.
      Instr : Interpreter.Code_Length_Type := 1;
   end record;

   type Instr_Loc_Array is array (Natural range <>) of Instr_Loc;
   --  Invokers(0) is original call on nested block
   --  Invokers(1..Num_Next_Values) are recursive calls inside loop body.

   type Instr_Loc_Array_Ptr is access all Instr_Loc_Array;

   Empty_Instr_Loc_Array : constant Instr_Loc_Array_Ptr :=
     new Instr_Loc_Array (0 .. -1);
   --  Used for an empty instr-loc list.

   subtype Type_Region_Ptr is Interpretations.Type_Region_Ptr;
   --  This is actually a Root_Sem_Ptr, but is convertible to a
   --  Type_Sem_Ptr

   type Type_Semantic_Info;
   type Type_Sem_Ptr is access all Type_Semantic_Info;

   type Type_Sem_Array is array (Positive range <>) of Type_Sem_Ptr;
   --  Used to pass list of types as parameters to
   --  Has_Desired_Signature

   package Type_Sem_Vectors is new PSC.Vectors (Type_Sem_Ptr);
   type Type_Sem_Vector is new Type_Sem_Vectors.Vector;
   --  Used to represent the set of types nested within a module.

   function Elem_Index_Hash (Index : Type_Sem_Vectors.Elem_Index)
      return Hash_Type;
   --  Hash function for index into Type_Sem_Vector

   function Type_Sem_Hash (Type_Sem : Type_Sem_Ptr) return Hash_Type;
   --  Hash function for Type_Sem_Ptr

   package Type_Sem_Target_Tables is new PSC.Hash_Tables
     (Element_Type => Type_Sem_Ptr,
      Key_Type => Type_Sem_Vectors.Elem_Index,
      Equiv => Type_Sem_Vectors."=",
      Hash_Type => Hash_Type,
      Hash => Elem_Index_Hash);
   type Type_Sem_Target_Table is new Type_Sem_Target_Tables.Hash_Table;
   --  Hash table used for recording Formal_Type, if any, associated with
   --  Nested_Type.

   package Type_Sem_Index_Tables is new PSC.Hash_Tables
     (Element_Type => Type_Sem_Vectors.Elem_Index,
      Key_Type => Type_Sem_Ptr,
      Equiv => "=",
      Hash_Type => Hash_Type,
      Hash => Type_Sem_Hash);
   type Type_Sem_Index_Table is new Type_Sem_Index_Tables.Hash_Table;
   --  Hash table used for recording Index in Nested_Types, if any,
   --  associated with a given target type.
   --  Also, for types with all parameters known, used for recording the
   --  Index of op-map type-descriptor, if any, associated with a given
   --  target type.

   Types_With_Descriptors : Type_Sem_Vector;
   --  All types with run-time type descriptors, indexed
   --  by same index as type descriptors.

   type Sem_Info_Array is array (Positive range <>) of Sem_Ptr;
   type Sem_Info_Array_Ptr is access Sem_Info_Array;
   --  Array of Sem_Infos used for actuals of instantiation

   function All_Nulls
     (Actual_Sem_Infos : Sem_Info_Array)
      return Boolean;
   --  Return True if Actual_Sem_Infos is of zero length,
   --  or all elements are null.

   function All_Nulls
     (Actual_Sem_Infos : Sem_Info_Array_Ptr)
      return Boolean;
   --  Return True if Actual_Sem_Infos is null,
   --  or zero length, or all elements are null.

   function Some_Nulls
     (Actual_Sem_Infos : Sem_Info_Array)
      return Boolean;
   --  Return True if Actual_Sem_Infos is
   --  of zero length, or some of its elements are null.

   function Some_Nulls
     (Actual_Sem_Infos : Sem_Info_Array_Ptr)
      return Boolean;
   --  Return True if Actual_Sem_Infos is null,
   --  or zero length, or some of its elements are null.

   package Sem_Info_Vectors is new PSC.Vectors (Sem_Ptr);
   type Sem_Info_Vector is new Sem_Info_Vectors.Vector;
   subtype Sem_Info_Index is Sem_Info_Vectors.Elem_Index;
   --  Used to represent the (global) constants nested within a module.
   --  NOTE: These could be used for any constant whose value
   --       depends only on Module parameters, independent of
   --       where it is declared or used, including anonymous
   --       constants.

   Compile_Time_Known_Const_Table : Sem_Info_Vector;
   --  Used to hold info on compile-time-known constants
   --  These are accessed using "(Const_Area, Index)"

   type Operation_Semantic_Info;
   type Operation_Sem_Ptr is access all Operation_Semantic_Info;

   package Operation_Sem_Vectors is new PSC.Vectors (Operation_Sem_Ptr);
   type Operation_Sem_Vector is new Operation_Sem_Vectors.Vector;
   --  Used to represent the set of operations declared in interface
   --  of module.

   type Module_Semantic_Info;
   type Module_Sem_Ptr is access all Module_Semantic_Info;

   function Treat_As_Type_Indicator (Mod_Sem : Module_Sem_Ptr)
     return String;
   --  Return " (as type)" if Treat_As_Type is True for module; "" otherwise

   function Actual_Parameters_Hash
     (Base_Type : Type_Sem_Ptr)
      return Hash_Type;
   --  Hash on actual parameter U_Types of instantiation

   function Same_Actual_Parameters
     (Base_Type1, Base_Type2 : Type_Sem_Ptr)
      return Boolean;
   --  True if same actual parameter U_Types in instantiations

   function Type_Is_Parameterized (Type_Sem : Type_Sem_Ptr) return Boolean;
   --  Return True if module defining type has parameters or
   --  if Type_Is_Parameterized (Type_Sem.Enclosing_Type).

   package U_Base_Type_Tables is new Hash_Tables (
      Element_Type => Type_Sem_Ptr,
      Key_Type => Type_Sem_Ptr,
      Equiv => Same_Actual_Parameters,
      Hash_Type => Hash_Type,
      Hash => Actual_Parameters_Hash);

   function Base_And_Constraints_Hash
     (Constrained_Type : Type_Sem_Ptr)
      return Hash_Type;
   --  Hash on base type and constraints

   function Same_Base_And_Constraints
     (Constrained_Type1, Constrained_Type2 : Type_Sem_Ptr)
      return Boolean;
   --  True if same base type and constraints

   package U_Type_Tables is new Hash_Tables (
      Element_Type => Type_Sem_Ptr,
      Key_Type => Type_Sem_Ptr,
      Equiv => Same_Base_And_Constraints,
      Hash_Type => Hash_Type,
      Hash => Base_And_Constraints_Hash);

   function Find_U_Base_Type (Some_Type : Type_Sem_Ptr) return Type_Sem_Ptr;
   --  Find unique base type for given type

   function Find_U_Type (Some_Type : Type_Sem_Ptr) return Type_Sem_Ptr;
   --  Find unique type equivalent to given type

   --  This type is used for a count that is bumped each time we
   --  apply "new" to a type based on a given module.
   --  A type is "anonymous" if it has a count of zero.
   type New_Type_Count_Type is range 0 .. 100;
   Anonymous_Type_Indicator : constant New_Type_Count_Type := 0;

   type Module_Semantic_Info is new Semantic_Info with record
   --  This is created for an interface or class defining a module
      Treat_As_Type : Boolean := False;
      --  If True, then this module was created implicitly as part of a
      --  type declaration that creates a "new" type.
      Is_Interface : Boolean := True;
      Is_Abstract : Boolean := False;
      --  A module is "abstract" if its interface is explicitly
      --  declared as abstract.
      Is_Partially_Abstract : Boolean := False;
      --  A module is "partially abstract" if it contains at least one
      --  operation explicitly declared as abstract in its interface,
      --  but it is not itself declared as abstract.
      --  A "partially abstract" type can have instances, but they
      --  cannot be converted to be of a polymorphic type if any of the
      --  needed operations are abstract.  Similarly, a partially-abstract
      --  type cannot be used as the actual in an instantiation, if the
      --  formal type expects operations that are abstract for the type.
      Is_Concurrent : Boolean := False;
      Is_Standalone_Module : Boolean := False;  --  Separately compiled module
      Uses_Queuing : Boolean := False;
      --  True if some operation of concurrent module uses queuing
      Ancestor_Decls_Being_Inherited : Boolean := False;
      --  True if in middle of inheriting ancestor decls.
      --  When True, we suppress adding nested types
      --  that have an associated generic operation.
      Code_Being_Generated : Boolean := False;     --  In middle of
                                                   --  code gen
      Code_Has_Been_Generated : Boolean := False;     --  Done with Code
                                                      --  gen
      Other_Part : Module_Sem_Ptr;     --  Pointer between
                                       --  interface and class
      Parent_Module : Module_Sem_Ptr := null;
      --  Module specified in "extends" clause, if any.
      Component_Extension_Level : Natural := 0;
      --  This indicates number of component extensions that have been
      --  performed, where a "component extension" is an extra level
      --  of indirection due to the addition of hidden components.
      Ancestor_Module_Formals : Lists.List := Lists.Empty_List;
      --  Formals of ancestors of module, if being inherited
      Ancestor_Module_Exports : Lists.List := Lists.Empty_List;
      --  Exports of ancestor interfaces
      Ancestor_Module_Implements : Lists.List := Lists.Empty_List;
      --  Implements operation list of ancestor modules
      Cur_Inst_Sem : Type_Sem_Ptr;  --  default/current instance of
                                    --  module
      Primary_Nested_Type : Type_Sem_Ptr; --  Type to use when module used
                                          --  in a "type" context.
                                          --  This is used in Sparkel for a
                                          --  type declared inside a same-named
                                          --  package.
      Num_Module_Parameters : Integer := -1;
      --  Count of module formals, including
      --  inherited formals, if any.
      Num_Constructors : Integer := -1;
      --  Count of visible operations that
      --  have an output of the cur-inst-type.
      Num_Components_For_Aggregate : Natural := 0;
      --  This keeps a count of components
      --  which are usable in aggregates.
      --  The "ancestor part," if any, counts
      --  as a component.
      Num_Visible_Components : Integer := -1;  --  Count of visible components
      --  (not including inherited ones).
      --  Minus 1 means not yet computed.
      Num_Private_Components : Natural := 0;  --  Count of private components
      --  (not including inherited ones)
      Num_Visible_Ancestor_Components : Natural := 0;
      --  Count of visible inherited components
      Num_Private_Ancestor_Components : Natural := 0;
      --  Count of private inherited components
      Ancestor_Module_With_Constructor : Module_Sem_Ptr := null;
      --  Nearest ancestor that might have private components,
      --  as determined only by looking at interfaces.
      --  If non-null, then a class aggregate must specify a
      --  value for this ancestor as a whole, rather than
      --  simply specifying the visible inherited components.
      Contains_Ref_Component : Boolean := False;
      --  True if there is a "ref" component declared directly in
      --  the module.  This does not worry about "ref" subcomponents,
      --  i.e. refs buried within non-ref components.
      Needs_Polymorphic_Type_Desc : Boolean := False;
      --  True if there is a use of a polymorphic type for a type
      --  derived from this module.
      Num_Inherited_Nested_Types : Natural := 0;
      --  Count of nested types after inheriting those of parent module.
      Nested_Types : Type_Sem_Vector;
      --  Set of nested types defined within a module
      --  Each one needs an "actual" type at run-time in the
      --  enclosing type area.  Unlike parameters, nested types
      --  don't need operation maps since each nested type's
      --  associated module is known.
      Nested_Type_Op_Map_Target : Type_Sem_Target_Table;
      --  If Nested_Type needs an op map, this indicates the
      --  target/formal type for the op map.
      Actuals_Of_Formals : Type_Sem_Vector;
      --  Set of types that are used as actuals within a formal type.
      --  These can be identified by Formal_Type::Formal_Type::Formal...
      --  These need an op-map which maps the actual to the
      --  specified interface.
      Num_Interface_Operations : Interpreter.Operation_Index := 0;
      --  Count of operations declared in interface of module that
      --  have an operand or result of the "current instance" type.
      --  Note that operations that do not have such an operand
      --  or result are not permitted to be abstract, as they are not
      --  provided by other modules implementing the interface.
      Nested_Objects : Sem_Info_Vector;
      --  Set of nested (large) constants defined within a module.
      --  These are evaluated when the Type_Descriptor is created
      --  and referenced using offsets relative to the Type_Area
      --  in the range Type_Nested_Obj_Offsets'Range.
      U_Base_Types : U_Base_Type_Tables.Hash_Table;
      --  Table for keeping track of unique instantiations of this module
      --  (these are all unconstrained types)
      U_Types : U_Type_Tables.Hash_Table;
      --  Table for keeping track of unique types based on this module
      --  (this includes constrained types, but ignores "optionality")
      New_Type_Count : New_Type_Count_Type := 0;
      --  Count is bumped each time a type is declared "new"
      --  This is then copied into the Type_Sem info, to arbitrarily
      --  distinguish this type from others based on the same module.
      --  A value of "0" for the counter is used for the "base base" type,
      --  so first we look up the type with a counter of zero and that
      --  becomes the U_Structure_Base, and then we look up with the "true"
      --  value for the counter, and that gives the U_Base_Type.
   end record;

   type Unique_Type_Index is new Natural;
   --  This counts the number of types created

   Last_Type_Index : Unique_Type_Index := 0;

   function Next_Type_Index return Unique_Type_Index;
   --  Assign a unique type index

   type Type_Semantic_Info is new Semantic_Info with record
   --  This is created for the definition of a type (as an instance
   --  of some module).
      Type_Index : Unique_Type_Index := Next_Type_Index;
      Associated_Module : Module_Sem_Ptr := null;

      New_Type_Counter : New_Type_Count_Type := Anonymous_Type_Indicator;
      --  This will be > 0 if "new" was applied when defining this type

      Func_Type_Op_Sem : Operation_Sem_Ptr := null;  --  If is Func Type
      Enclosing_Type : Type_Sem_Ptr := null;
      --  If this is an instance of a nested module, then this
      --  is the type-sem for an instance of the enclosing module.
      Parent_Type : Type_Sem_Ptr := null;
      --  If non-null, the type is an extension of this type.
      Ancestor_Type_With_Constructor : Type_Sem_Ptr := null;
      --  If non-null, the nearest ancestor that might have
      --  private components.
      Constraint_Annotations : Lists.List;
      --  List of constraints that apply to the type.
      --  TBD: Should be filled in on type produced by Annotation tree.
      Value_Is_Optional : Boolean := False;
      --  Indicates that "optional" was specified.
      Obj_Is_Concurrent : Boolean := False;
      --  Indicates that "concurrent" was specified.
      Is_Polymorphic : Boolean := False;
      --  Indicates that "+" suffix was given for type making it polymorphic.
      Root_Type : Type_Sem_Ptr := null;
      --  Type which is the "root" of the polymorphic type.
      --  Points at self if type is not polymorphic.
      Corresponding_Polymorphic_Type : Type_Sem_Ptr := null;
      --  Polymorphic type corresponding to this (non-polymorphic) type.
      --  Points at self if type is polymorphic.
      Is_Universal : Boolean := False;
      Known_To_Be_Assignable : Boolean := False;
      --  This is false if object conains a ref component.
      --  TBD: Unlocked concurrent objects should not be allowed to
      --      to be assigned.
      Known_To_Be_Small : Boolean := False;
      --  Objects are known to fit completely in a single 64-bit word
      Known_To_Be_Large : Boolean := False;
      --  Objects are known to *not* fit completely in a single 64-bit word
      All_Parameters_Known : Boolean := False;
      --  There are no unknown parameters in this type
      --  so it can be given a type id at compile-time.
      --  This means that either the associated module has no
      --  formal parameters, or all of the actual parameters
      --  are types with All_Parameters_Known.
      --  This also means that the module itself is known, meaning
      --  that operations may be called directly.  This will always
      --  be false for formal types, since at compile time we don't
      --  even know the particular module of the actual type.
      Outermost_Module_Where_Used : Module_Sem_Ptr := null;
      --  Outermost module where this type is used, and if nested or an
      --  actual-of-formal, is the module where Actual_Of_Formal_Index
      --  and Nested_Type_Index are meaningful.  If this is a nested
      --  module, then this might get overwritten later by an
      --  enclosing module, at which point the Actual_Of_Formal_Index
      --  or Nested_Type_Index will have to be reassigned.
      Is_Formal_Type : Boolean := False;
      --  This is a formal type of a module (or operation)
      Actual_Sem_Infos : Sem_Info_Array_Ptr;
      --  Array of sem-infos of actual parameters to instantiation
      Formal_Prefix : Optional_Tree := Null_Optional_Tree;
      --  If non-null, this identifies the actual of formal represented
      --  by this type.
      Associated_Generic_Op : Sym_Ptr := null;
      --  If non-null, this includes the definition of a "generic" type
      --  for the given "generic" operation.
      Actual_Of_Formal_Index : Type_Sem_Vectors.Elem_Index := 0;
      --  This is non-zero for an actual-of-formal type
      Nested_Type_Index : Type_Sem_Vectors.Elem_Index := 0;
      --  This is non-zero if this is a nested type.
      Op_Maps_Needed : Type_Sem_Index_Table;
      --  Table of op-maps needed, keyed by target type
      Type_Descriptor_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  Location of run-time type information.
      --  Should be absolute when all parameters known.
      --  Should be type-area-relative otherwise.
      --  If a formal type, then the op map is at Type_Descriptor_Location+1.

      Full_View : Type_Sem_Ptr := null;
      --  This points to "full" view of type, if this is a private type.

      External_View : Type_Sem_Ptr := null;
      --  This points to the "external" view of type, which might be
      --  declared as a "private" type.

      U_Base_Type : Type_Sem_Ptr;
      --  This points to the unique base type for this type, meaning
      --  that constraints have been stripped off, and it has been
      --  "uniquified" relative to the enclosing module.
      --  The Definition associated with this type is always a module
      --  instantiation.
      --  The Constraint_Annotations is always the empty list on such
      --  a type.

      U_Type : Type_Sem_Ptr;
      --  This points to the unique/underlying type for this type (with
      --  constraints), meaning that it has been "uniquified" relative
      --  to the enclosing module.

      U_Base_Structure : Type_Sem_Ptr;
      --  This points same place as U_Base_Type if New_Type_Counter is zero.
      --  If New_Type_Counter is > 0, then this points to the equivalent
      --  type with New_Type_Counter = 0 (i.e Anonymous_Type_Indicator).
   end record;

   function Sem_Image
     (Sem : access Type_Semantic_Info;
      Use_Short_Form : Boolean := False)
      return String;
   --  Dispatching op to return image of type identified by semantic info

   type Param_Mapping;
   type Param_Mapping_Ptr is access all Param_Mapping;

   type Param_Mapping is record
   --  A mapping used when matching "generic" operations
   --  to augment the mapping inherent in the type associated
   --  with a call on an operation.
      From : Sem_Ptr := null;
      To : Sem_Ptr := null;
      Next : Param_Mapping_Ptr := null;
   end record;

   function Unique_Operation_Id return Natural;
   --  Return a unique operation id and announce it if debugging.

   type Operation_Semantic_Info is new Semantic_Info with record
   --  This is created for the declaration or definition of an operation
      Operation_Kind : Operation.Operation_Kind_Enum :=
        Operation.Func_Operation;
      Func_Type_Sem : Type_Sem_Ptr := null;  --  If is a func type

      Routine : Interpreter.Routine_RW_Ptr;
      Implicit_Enclosing_Module : Module_Sem_Ptr := null;
      --  If non-null, this operation is actually a "generic" operation
      --  and the implicit enclosing module must be instantiated as part
      --  of any call.
      Body_Region : Symbols.Region_Ptr := null;
      Is_Def : Boolean := False;
      Is_Import : Boolean := False;
      Is_Abstract : Boolean := False;
      --  An operation is abstract by default if declared in
      --  an explicitly "abstract" interface, or if it includes
      --  "abstract" in its declaration.
      --  No body may be given for an abstract operation.
      --  A module is abstract if its interface is declared "abstract."
      --  A module is "partially abstract" if one or more of its visible
      --  operations are declared abstract.
      Uses_Queuing : Boolean := False;
      --  This is True if the operation is marked explicitly "queued"
      --  or it has a queued parameter, or it takes an unlocked parameter
      --  of a concurrent type with one or more queued operations,
      --  or it is a top-level operation (we presume these can do almost
      --  anything).
      Has_Locked_Param : Boolean := False;
      --  This is True if the operation has a locked formal parameter.

      Import_Id : Strings.U_String := Strings.Null_U_String;
      --  The identifier string/enumeral given in an import clause

      Convention : Languages.Convention_Enum :=
                     Languages.Convention_Internal_Default;
      --  Convention specified in an operation import clause
      --  (TBD: or Convention attribute).

      Link_Name : Strings.U_String := Strings.Null_U_String;
      --  Link-name, if any, specified in an operation import clause
      --  (TBD: or Link_Name attribute).

      Level : Static_Level := 0;

      Num_Cur_Inst_Inputs : Integer := -1;
      --  This indicates the number of inputs that are
      --  of the "current instance" type.
      --  -1 means that we have not yet computed the number.

      Num_Cur_Inst_Outputs : Natural := 0;
      --  This indicates the number of outputs that are
      --  of the "current instance" type.

      First_Polymorphic_Input : Natural := 0;
      --  If this is a polymorphic operation, this identifies the first input
      --  that is of the polymorphic type for the "current instance" type.
      --  This is zero if this is not a polymorphic operation (because it
      --  has no polymorphic inputs, or it has at least one non-polymorphic
      --  "current-instance" operand).

      Index : Interpreter.Operation_Index := 0;
      --  This is used if the Context is in Any_Interface_Item_Contexts or
      --   Exported_Class_Item_Contexts.
      Equiv_To : Operation_Sem_Ptr := null;
      --  This operation is defined using "is op_name" or "is in type_spec";
      Equiv_From_Type : Type_Sem_Ptr := null;
      --  If non-null, indicates operation is defined in terms of
      --  an operation of a different type.
      Spec_Sem : Operation_Sem_Ptr := null;
      --  This points to the sem info of the corresponding specification,
      --  if this is a body.
      Originating_Module : Module_Sem_Ptr := null;
      --  If this is an inherited operation, this indicates
      --  the module from which the operation was inherited.
      --  Otherwise, it indicates the immediately enclosing module.
      Originating_Operation : Operation_Sem_Ptr := null;
      --  If this is an inherited operation, this identifies
      --  the original operation from which it was inherited.
      --  Otherwise, it points to itself.
      Overridden_By : Operation_Sem_Ptr := null;
      --  If non-null, then this (inherited) operation has been overridden

      Return_Effects_RW : Object_Access.Read_Write_Mapping;
      --  The combined effects of all returns from this operation

      Uplevel_Refs : Object_Access.Read_Write_Mapping;
      --  For nested operations, information on up-level references.

      Uplevel_Ref_Count_At_Call : Natural := Natural'Last;
      --  This is set whenever we process a call on this operation
      --  during the Pre_CG phase, to indicate the number of Uplevel_Refs
      --  at the time of the call.  If the number of Uplevel_Refs increases
      --  after a call, then we will need to re-run Pre_CG.

      Rerun_Pre_CG : Boolean := False;
      --  Set to true if we need to re-run the "Pre_CG" phase on this
      --  operation because the set of uplevel references grew after a call.

      Num_Nested_Blocks : Natural := 0;
      --  Number of nested blocks needed for operation, including
      --  for checks associated with pre- and postconditions.

      Unique_Id : Natural := Unique_Operation_Id;
   end record;

   type Entry_Exit_Info_Type is record
      --  This record is built up during Pre_Cg pass when
      --  analyzing an annotation for a precondition or a postcondition.
      Has_Entry_Value_Of_RO_Input : Boolean := False;
         --  Whether expression includes reference to some read-only input
      Has_Entry_Value_Of_Var : Boolean := False;
         --  Whether expression includes reference to entry value of var
      Num_Entry_Temps : Natural := 0;
         --  Number of temps needed to record computation using an entry value
         --  We need to create a temp when expression using an entry-value
         --  of var is used in a postcondition, at point where it is
         --  combined with an exit value.
      Has_Exit_Value_Of_Var : Boolean := False;
         --  Whether expression includes exit value of an input variable
      Has_Exit_Value_Of_Output : Boolean := False;
         --  Whether expression includes final value of an output
      --  NOTE: A precondition should reference some input, either R/O or var
      --        A postcondition should reference final value of var or output
      --  Invariant: Has_Entry_Value_Of_Var ==>
      --               not Has_Exit_Value_Of_Var and
      --                 not Has_Exit_Value_Of_Output
      --    If Invariant about to be violated, we create an "entry temp"
      --    and set Has_Entry_Value_Of_Var to False.
   end record;

   Null_Entry_Exit_Info : constant Entry_Exit_Info_Type :=
     (Has_Entry_Value_Of_RO_Input => False,
      Has_Entry_Value_Of_Var => False,
      Num_Entry_Temps => 0,
      Has_Exit_Value_Of_Var => False,
      Has_Exit_Value_Of_Output => False);

   type Operand_Semantic_Info is new Semantic_Info with record
   --  Derivatives of this kind of semantic info are created for
   --  Trees that represent names or expressions of some type,
   --  or an operation name.
      Interps : Interpretations.Interp_Tree := null;
      Resolved_Type : Type_Sem_Ptr := null;
      --  If there is an Interps tree, this is the type of the chosen
      --  interpretation.  If there is no Interps tree, then this is
      --  the type associated with this particular interpretation.
      Resolved_Interp : Optional_Tree := Null_Optional_Tree;
      Hash_Value : Hash_Type := 0;
      Target_Polymorphic_Type : Type_Sem_Ptr := null;
      --  If non-null, then this operand is to be "wrapped" to
      --  produce the given target type.
      Entry_Exit_Info : Entry_Exit_Info_Type;
         --  Keeps track of nature of pre/postcondition computation
         --  such as whether it references the entry or exit value of a var
      Entry_Temp_Info : Object_Location_Ptr := null;
         --  If non-null, then an entry temp should be allocated "here"
   end record;

   type Operand_Sem_Ptr is access all Operand_Semantic_Info'Class;

   type Operand_Info is record
   --  This record identifies the particular operand which
   --  is responsible for an interpretation being added to the interp tree.
      Is_Output_Operand : Boolean := False;  --  Input vs. Output
      Position : Natural := 0;  --  Position of operand
      Name : Strings.U_String := Strings.Null_U_String;  --  Name
                                                         --  of
                                                         --  operand
   end record;

   Any_Operation : constant Operand_Info :=
     (Is_Output_Operand => False,
      Position => 0,
      Name => Strings.Null_U_String);
   --  Allows any operation with a matching operation name

   Constructors_Only : constant Operand_Info :=
     (Is_Output_Operand => True,
      Position => 1,
      Name => Strings.Null_U_String);
   --  Allows operations of given operation name that return given type.

   type Sym_Reference_Info is new Operand_Semantic_Info with record
   --  This is created for a reference to a declared entity (a sym)
      Prefix_Type_Region : Type_Region_Ptr := null;
      --  Type region for prefix, if any (used for qualified name)
      Underlying_Sem_Info : Sem_Ptr := null;
      --  This has been substituted with type region if this is
      --  a qualified name identifying a type.
   end record;

   type Sym_Ref_Ptr is access all Sym_Reference_Info'Class;

   type Literal_Kind_Enum is (
     Not_A_Literal,
     Integer_Literal,
     Real_Literal,
     String_Literal,
     Char_Literal,
     Enum_Literal,
     Null_Literal);

   subtype Univ_Literal_Kinds is Literal_Kind_Enum
     range Integer_Literal .. Enum_Literal;
   --  Literals that correspond to the universal types

   type Literal_Semantic_Info is new Operand_Semantic_Info with record
   --  This is created for a reference to some kind of literal
      Lit_Kind : Literal_Kind_Enum := Not_A_Literal;
   end record;

   type Literal_Sem_Ptr is access all Literal_Semantic_Info'Class;

   type Slow_Call_Enum is (
   --  Indicates structure of "slow" calls (i.e. calls on
   --  something other than a built-in) within a computation tree.
   --  NOTE: These are ordered so that 'Max give the correct
   --       value when processing two sequential computations.
     No_Slow_Calls,       --  No slow call in tree
     Slow_Call,           --  A single slow call
     Slow_Call_Sequence,  --  Multiple slow calls, but no parallelism
   --  possible, as each depends on the results
   --  of the prior one.
     Mandatory_Parallel_Call,
   --  A queued call or other call which is required
   --  to be performed on its own TCB after evaluating
   --  all of the parameters.
     Slow_Call_Tree_Needing_Master,
   --  We need a master, since we have some
   --  exploitable parallelism somewhere inside the
   --  tree.  Will turn this slow call into a nested
   --  block so internal parallelism can be used,
   --  given our rule that we never have more than
   --  one master per code block.
   --  TBD: This rule is a bit arbitrary!
     Independent_Slow_Calls);
   --  There are two or more Slow-Call trees to be
   --  eval'ed in parallel, so an enclosing
   --  master is needed.

   type Composite_Stmt_Semantic_Info;
   --  Incomplete type decl so can point at target of exit/continue.

   type Composite_Stmt_Sem_Ptr is access all Composite_Stmt_Semantic_Info'
     Class;

   type Computation_Semantic_Info is new Operand_Semantic_Info with record
   --  This kind of semantic info is created for invocations,
   --  unary, and binary operations, and anything else that might
   --  include nested calls.
      Slow_Calls : Slow_Call_Enum := No_Slow_Calls;
      Op_Sem : Operation_Sem_Ptr := null;
      Parallel_Result_Offset : Interpreter.Offset_Within_Area := 0;
      TCB_VM_Num : Interpreter.VM_Obj_Unique_Num := 0;
      Num_Nested_Blocks : Natural := 0;
      Uses_Queuing : Boolean := False;
      Needs_Anon_Const : Boolean := False;
         --  Set to True if this is computing a large compile-time-known
         --  constant that is needed to compute some other compile-time-known
         --  constant.

      Num_Finalizable_Temps : Natural := 0;
      --  Number of finalizable temps for subtree, including this node.
      Finalizable_Temp_Info : Object_Location_Ptr := null;
      --  Finalizable temp for this node, if any.

      --  This is non-zero if this construct is a parallel statement op
      Enclosing_Master : Interpreter.Offset_Within_Area := 0;

      Level : Static_Level := 0;

      Target_Stmt : Composite_Stmt_Sem_Ptr := null;
      --  Target of exit/continue
   end record;

   type Computation_Sem_Ptr is access all Computation_Semantic_Info'Class;

   type Annotation_Semantic_Info is new Computation_Semantic_Info with record
   --  This kind of semantic info is created for Annotation nodes.
      Decl_For_Annotations : Optional_Tree := Null_Optional_Tree;
      Annotation_Analyzed : Boolean := False;
   end record;

   type Annotation_Sem_Ptr is access all Annotation_Semantic_Info'Class;

   type Composite_Stmt_Semantic_Info is new Computation_Semantic_Info with
      record
         Num_Exits : Natural := 0;
         Num_Exits_Emitted : Natural := 0;
         Exit_Locs : Instr_Loc_Array_Ptr := Empty_Instr_Loc_Array;

         Num_Continues : Natural := 0;
         Num_Continues_Emitted : Natural := 0;
         Continue_Locs : Instr_Loc_Array_Ptr := Empty_Instr_Loc_Array;
         --  These last three are only used for loop statements.

         Exit_Effects_RW : Object_Access.Read_Write_Mapping;
         --  The combined effects of all exits from this composite statement.
      end record;

   type Case_Construct_Semantic_Info is new Composite_Stmt_Semantic_Info with
      record
      --  This kind of semantic info is created for case constructs.
      --  Op_Sem field points to "=?" operator
         Case_Selector : Optional_Tree;
         Case_Selector_Type : Type_Sem_Ptr := null;
         Case_Min : Interpreter.Word_Type := 0;
         Case_Max : Interpreter.Word_Type := 0;
      end record;

   type Case_Construct_Sem_Ptr is access all Case_Construct_Semantic_Info'
     Class;

   type Object_Semantic_Info is new Computation_Semantic_Info with record
   --  This is created for an object declaration
      Info : Object_Location_Ptr := new Object_Location_Info;
      --  NOTE: This provides a level of indirection
      --       so copies of Object_Sem info will still see the info
      --       even if it is not filled in until the Code_Gen phase
      Originating_Module : Module_Sem_Ptr := null;
      --  If this is an inherited component, this indicates
      --  the module from which the component was inherited.
      --  Otherwise, it indicates the immediately enclosing module.
      Component_Index : Natural := 0;
      --  If this is a component, this indicates the index of the
      --  component.  This is used in conjunction with
      --  Originating_Module.Component_Extension_Level to
      --  determine how to access the component.
      Object_Id : Object_Access.Object_Id_Type;
      --  If this is *not* a component, this is a unique id for
      --  the object.
      Object_Ref : Object_Access.Read_Write_Mapping;
      --  If this is a declared-ref object, then it preserves
      --  the read-write mapping of its initializing expression,
      --  rather than being a completely independent object.
   end record;

   type Object_Sem_Ptr is access all Object_Semantic_Info'Class;

   type Param_Semantic_Info is new Object_Semantic_Info with record
   --  This is created for a parameter declaration
      Is_Operation_Output : Boolean := False;
      Is_Implicit_Module_Param : Boolean := False;
      Is_By_Ref_Choice_Param : Boolean := False;
      Is_Var_Choice_Param : Boolean := False;
   end record;

   type Param_Sem_Ptr is access all Param_Semantic_Info'Class;

   type Class_Aggregate_Semantic_Info is new Computation_Semantic_Info with
      record
      --  This is created to represent a class aggregate.
         Target_Type : Type_Sem_Ptr := null;
         --  This records the target type of the class aggregate.
      end record;

   type Class_Agg_Sem_Ptr is access all Class_Aggregate_Semantic_Info'Class;

   type Container_Aggregate_Semantic_Info is new Object_Semantic_Info
   with
      record
      --  This is created to represent a container aggregate.
         Target_Type : Type_Sem_Ptr := null;
         --  This records the target type of the container aggregate.
         Index_Type : Type_Sem_Ptr := null;
         --  Index type for container (after substituting actuals)
         Element_Type : Type_Sem_Ptr := null;
         --  Element type for container (after substituting actuals)

         Aggregate_Kind : Invocation.Aggregate_Kind_Enum :=
           Invocation.Container_Aggregate;
         --  Container_Aggregate vs. Map_Set_Aggregate
      end record;

   type Container_Agg_Sem_Ptr is access all Container_Aggregate_Semantic_Info'
     Class;

   type For_Loop_Construct_Semantic_Info; --  TBD Ada2005: is tagged;

   type For_Loop_Construct_Sem_Ptr is access all
     For_Loop_Construct_Semantic_Info'Class;

   type Iterator_Semantic_Info is new Param_Semantic_Info with record
   --  This kind of semantic info is created for iterators.
      Enclosing_For_Loop : For_Loop_Construct_Sem_Ptr := null;
      --  Loop for which this is an iterator
      Iterator_Kind : Iterator.Iterator_Kind_Enum :=
        Iterator.Iterator_Kind_Enum'First;
      Iterator_Direction : Interpreter.Direction := Interpreter.Unordered_Dir;
      Iterator_Index : Natural := 0;
      --  Index within sequence of iterators for single for-loop
      Iteration_Type : Type_Sem_Ptr := null;
      --  This is the type of the container/set/initial value.
      --  It is redundant with the Resolved_Type for a value iterator,
      --  and redundant with Index_Set_Type for a set iterator.
      Key_Sem : Param_Sem_Ptr := null;
      --  Only used if this is a for-each iterator;
      --  if [key => value] notation then this is a named object;
      --  otherwise this is an anonymous object.
      Index_Set_Type : Type_Sem_Ptr := null;
      --  This is the type of the set over which we destructively iterate.
      --  In a for-each iterator, this is the type of the result
      --  of the "index_set" operator.  In a for-in iterator, this is
      --  the type of the set or interval specified.
      Index_Set_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  This is the location of the index set associated
      --  with a Set_Iterator (for-in) or a Container_Iterator (for-each)
      Container_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  This is the location of the container associated
      --  with a for-each iterator.  We call the indexing
      --  operator to create a ref to each element.
      --  TBD: Element is a variable only if the container is a var
      --      and the indexing op returns a "ref."
      Container_Is_By_Ref : Boolean := True;
      --  This is determined by the first parameter to the "indexing" op
      Loop_Param_Is_By_Ref : Boolean := False;
      --  This is determined by the result parameter of the "indexing" op
      --  for a container iterator, and determined by the use of => or :=
      --  for a value iterator.  It is always false for a set iterator.
      Initial_Value_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  This is the location of the initial value of the
      --  associated loop parameter.
      Initial_Value_Test : Interpreter.Code_Offset := 0;
      --  Instruction needing fixup to skip past call for initial iteration
      Num_Next_Values : Natural := 1;
      --  Number of next values provided in the initial/next/while iterator.
      --  Will be 1 for Set_Iterator and Container_Iterator.
      --  Will be 0 if no next values specified, which implies
      --  the next values must be specified in the "continue" statement.
      Next_Value_Test : Interpreter.Code_Offset := 0;
      --  Instruction needing fixup to skip past call for next iteration
   end record;

   type Iterator_Sem_Ptr is access all Iterator_Semantic_Info'Class;

   type Iterator_Sem_Array is array (Positive range <>) of Iterator_Sem_Ptr;
   type Iterator_Sem_Array_Ptr is access all Iterator_Sem_Array;

   type For_Loop_Construct_Semantic_Info is new Composite_Stmt_Semantic_Info
   with
      record
      --  This kind of semantic info is created for For_Loop constructs.
      --  Op_Sem field points to "=?" operator
         For_Loop_Direction : Interpreter.Direction :=
           Interpreter.Unordered_Dir;
         For_Loop_Master : Interpreter.Offset_Within_Area := 0;
         For_Loop_Level : Static_Level := 0;
         Loop_Body_Block : Block_Index := 0;
         --  Block representing the loop body
         Iterator_Sems : Iterator_Sem_Array_Ptr := null;
         --  Sem info for each iterator
         Loop_Param_Region : Symbols.Region_Ptr;
         --  Region in which loop variables are declared.
         --  Nested_Region field is used for the region of the loop body
         --  itself.
         Is_Potentially_Concurrent : Boolean := False;
         --  True if the loop is explicitly concurrent, or if it
         --  has multiple "next" values, or if there is a continue
         --  statement for the loop that is from inside a concurrent
         --  construct.
         Uses_Parallel_Nested_Block : Boolean := True;
         --  True if a nested block is created for the loop body,
         --  and the loop is controlled using Start/Add/Wait_Parallel.
         --  False if looping is accomplished by simply branching backwards.
         Enclosing_Container_Agg_Sem : Container_Agg_Sem_Ptr := null;
         --  Sem_Info for enclosing container agg, if any
         Loop_Result_Locator : Interpreter.Object_Locator :=
           Interpreter.Null_Object_Locator;
         --  Result of loop in case of map-reduce expr and quantified expr
         Loop_Result_Type : Type_Sem_Ptr := null;
         Loop_Params_Local_Offset : Interpreter.Offset_Within_Area := 0;
         --  Where loop parameters are stored when we do *not* invoke
         --  a parallel nested block for the loop body.

         Loop_Body_RW : Object_Access.Read_Write_Mapping;
         --  Read/Write mapping for loop body, used to detect race conditions
         --  in (potentially) concurrent loops.
      end record;

   type Resolved_Operation_Info is new Computation_Semantic_Info with record
   --  This is created to represent an operation passed as a parameter.
      Assoc_Type_Region : Type_Region_Ptr := null;
      --  This records from what type-region the operation originates.
      --  This is used to substitute into the operation parameter types
      --  if they are, or depend on, formal types of the enclosing module.

      --  TBD: Might want to pull in other components of Call_Semantic_Info
      --       to provide additional information, though some of them
      --       seem to belong on Operation_Semantic_Info more
   end record;

   type Resolved_Op_Ptr is access all Resolved_Operation_Info'Class;

   type Call_Semantic_Info is new Resolved_Operation_Info with record
   --  This is created to represent a call, including unary and
   --  binary operator invocations that are equivalent to calls.
      Equiv_Invocation : Optional_Tree := Null_Optional_Tree;
      --  This is an invocation tree, either the original, or one
      --  created to represent an invocation of an operator,
      --  or to represent an obj.operation(...) reconstructed as
      --  as operation(obj, ...).

      Original_Binary_Operator : Binary.Binary_Operator_Enum_With_Null :=
        Binary.Not_An_Op;
      --  This is the original binary operator, when the Equiv_Invocation
      --  might be an invocation of some other operator (e.g. "=?").

      Generic_Param_Map : Param_Mapping_Ptr := null;
      --  This records extra mappings from "generic" types to
      --  actual types.

      Might_Be_Queued : Boolean := False;
      --  If True, then this call might involve potential queueing,
      --  meaning that it will require a Parallel_Call as part of its
      --  invocation.

      Is_Queued_Call : Boolean := False;
      --  If True, then locked param has Queued_Param locking.

      Locked_Param_Info : Interpreter.Locked_Param_Info_Type;
      --  If Param_Index greater than zero, identifies concurrent object
      --  parameter that is locked during the execution of the call.

      Polymorphic_Param_Index : Integer
        range Integer'First + 1 .. Integer'Last := 0;
         --  Range is symmetric so can compute absolute value w/o overflow
      --  If greater than zero, identifies polymorphic parameter
      --  that controls the selection of the operation body to be invoked.
      --  If less than zero, indicates that the polymorphic parameter
      --  is passed by reference, and so an extra level of indirection
      --  is required.
   end record;

   type Call_Sem_Ptr is access all Call_Semantic_Info'Class;

   type Selection_Semantic_Info is new Computation_Semantic_Info with record
   --  This is created to represent a component selection
      Comp_Decl : Object_Sem_Ptr := null;
   end record;

   type Selection_Sem_Ptr is access all Selection_Semantic_Info'Class;

   --------------- Utility operations on Sem info --------------

   function Types_Match (Type1, Type2 : Type_Sem_Ptr) return Boolean;
   --  Return True if Type1 and Type2 are value-equivalent

   function U_Base_Type_Region
     (Obj_Type : Type_Sem_Ptr)
      return Type_Region_Ptr;
   --  Return unique base type of type
   --  This strips away any constraint on top of a module instantiation,
   --  and returns a unique value for all instantiations with equivalent
   --  parameter types.  Each formal type get its own unique base type.

   function Underlying_Sem_Info (Orig_Sem : Sem_Ptr) return Sem_Ptr;
   --  Return Orig_Sem, unless Orig_Sem is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol.

   function Underlying_Sem_Info (OT : Optional_Tree) return Sem_Ptr;
   --  Return Sem_Info of OT, unless OT is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol.

   function Underlying_Op_Sem_Info (Orig_Sem : Sem_Ptr) return Sem_Ptr;
   --  Return Orig_Sem, unless Orig_Sem is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol,
   --  or in case Orig_Sem is a Resolved_Operation_Info, in which
   --  case we return the underlying Operation_Info.

   function Underlying_Op_Sem_Info (OT : Optional_Tree) return Sem_Ptr;
   --  Return Sem_Info of OT, unless OT is a Sym_Reference_Info,
   --  in which case we return the Sem_Info of the Associated_Symbol,
   --  or in case Sem_Info is a Resolved_Operation_Info, in which
   --  case we return the underlying Operation_Info.

   function Follow_Op_Equiv (Op_Sem : Operation_Sem_Ptr)
     return Operation_Sem_Ptr;
   --  If Op_Sem.Equiv_To /= null, then return Op_Sem at end of chain
   --  of Equiv_To's.

   function Resolved_Type (OT : Optional_Tree) return Type_Sem_Ptr;
   --  Return resolved type for given optional tree, if determined

   function Resolved_Tree (T : Optional_Tree) return Optional_Tree;
   --  Return Resolved interp if present, else return original tree

   procedure Visit_Resolved
     (T : Optional_Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class);
   --  If T has Sem_Info that has a Resolved_Interp, then
   --  visit that instead of T itself.

   function Initial_Value_Operand (Loop_Body : Optional_Tree;
     Num_Found : access Natural) return Optional_Tree;
   --  Return operand of Initial_Value_Op found within loop body.
   --  Bump up Num_Found for each one found; caller should init to zero.
   --  Return Null_Optional_Tree if none found.

   function Contains_Initial_Value_Operand (Loop_Body : Optional_Tree)
     return Boolean;
   --  Return True if Loop_Body contains an initial-value operand

   function Sym_Name (Id : Identifier.Tree) return String;
   --  Return string representation of identifier

   function Type_Image
     (Expr_Type : Type_Sem_Ptr;
      Use_Short_Form : Boolean := True;
      Max_Chars : Positive := 2000)
      return String;
   --  Return image of type

   function Canonical_Type_Name (Expr_Type : Type_Sem_Ptr) return String;
   --  Return a name that is unique and canonical for the given type.

   function Param_Map_Image (Map : Param_Mapping_Ptr) return String;
   --  Return an image in the form "(From => To, From => To, ...)"

   ---------- Universal and other built-in types --------------

   type Univ_Type_Array is array (Literal_Kind_Enum) of Type_Sem_Ptr;

   type Univ_Types_Ptr is access all Univ_Type_Array;

   type Builtin_Types_Rec is record
      Univ_Types : aliased Univ_Type_Array := (others => null);
      --  All of the univeral types

      Integer_Module : Module_Sem_Ptr;  -- Module for built-in Integer
      Integer_64_Module : Module_Sem_Ptr;  -- Module for built-in Integer
      Unsigned_64_Module : Module_Sem_Ptr;  -- Module for built-in Integer
      Float_Module : Module_Sem_Ptr;    -- Module for built-in Float
      Univ_Real_Module : Module_Sem_Ptr;    -- Module for built-in Univ_Real
         --  Some day change this to Univ_Float?
      Basic_Array_Module : Module_Sem_Ptr;  -- Module for built-in Basic_Array
      Aliased_Object_Module : Module_Sem_Ptr;
                                       --  Module for built-in Aliased Object

      --  A "pseudo" module to hold all of the func types
      Func_Type_Module : Module_Sem_Ptr := new Module_Semantic_Info;

      --  Miscellaneous other builtins
      Plastic_Type : Type_Sem_Ptr;
      Any_Type : Type_Sem_Ptr;
      Assignable_Type : Type_Sem_Ptr;
      Exception_Type : Type_Sem_Ptr;
      Boolean_Type : Type_Sem_Ptr;
      Ordering_Type : Type_Sem_Ptr;
      Unsigned_64_Type : Type_Sem_Ptr;
      Integer_64_Type : Type_Sem_Ptr;
      Univ_Real_Type : Type_Sem_Ptr;
   end record;

   type Builtin_Types_Ptr is access Builtin_Types_Rec;

   --  Set of builtin types and modules, one for each language
   Builtin_Types_Array : array (PSC.Languages.Language_Enum)
     of Builtin_Types_Ptr;

   --  Types of the various kinds of literals
   function Univ_Types return Univ_Types_Ptr;
   function Univ_Integer_Type return Type_Sem_Ptr;
   function Univ_Character_Type return Type_Sem_Ptr;
   function Univ_String_Type return Type_Sem_Ptr;
   function Univ_Enumeration_Type return Type_Sem_Ptr;
   function Optional_Type return Type_Sem_Ptr;

   function Univ_Real_Type return Type_Sem_Ptr;
   function Unsigned_64_Type return Type_Sem_Ptr;
   function Integer_64_Type return Type_Sem_Ptr;

   function Integer_Module return Module_Sem_Ptr;
   --  Module for built-in Integer

   function Float_Module return Module_Sem_Ptr;
   --  Module for built-in Float

   function Basic_Array_Module return Module_Sem_Ptr;
   --  Module for built-in Basic_Array

   function Aliased_Object_Module return Module_Sem_Ptr;
   --  Module for built-in Aliased Object

   function Func_Type_Module return Module_Sem_Ptr;
   --  A "pseudo" module to hold all of the func types

end PSC.Trees.Semantics.Info;
