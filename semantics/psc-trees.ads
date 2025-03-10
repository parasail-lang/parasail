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

with Ada.Streams;
with PSC.Languages;
with PSC.Source_Positions;
with Ada.Text_IO.Text_Streams;
package PSC.Trees is

   type Tree_Kind_Enum is
     (Annotation_Kind, Assign_Stmt_Kind, Binary_Kind,
      Block_Stmt_Kind, Case_Construct_Kind, Conditional_Kind,
      Control_Stmt_Kind, For_Loop_Kind, Identifier_Kind,
      Implements_Element_Kind, Invocation_Kind, Iterator_Kind,
      Module_Kind, Obj_Decl_Kind, Operation_Kind, Param_Decl_Kind,
      Property_Kind, Qualified_Name_Kind, Qualifier_Kind,
      Reference_Kind, Selection_Kind, Type_Decl_Kind, Unary_Kind,
      While_Stmt_Kind);

   type Optional_Tree is private;
   --  A possibly empty subtree

   Null_Optional_Tree : constant Optional_Tree;
   --  An empty tree

   --  Ambiguity list data structure
   --  Keeps track of operands which are themselves ambiguous,
   --  and other interpretations ambiguous with this one.
   type Ambiguity_List_Node;
   type Ambiguity_List is access Ambiguity_List_Node;
   type Ambiguity_List_Node is record
      This_Interp : Optional_Tree;
      Ambiguous_Operands : Ambiguity_List;
      Next_Ambig_Interp : Ambiguity_List;
   end record;

   type Root_Semantic_Info is abstract tagged record
   --  Semantic information associated with tree or symbol
      Definition : Optional_Tree;
      Ambiguity : Ambiguity_List := null;
      Is_Plastic : Boolean := False;
         --  If True, an error has already been reported
         --  so no further errors should be reported using
         --  this semantic info.  Allow it to conform
         --  to any sort of entity required (i.e. it is "plastic").
   end record;

   type Root_Sem_Ptr is access all Root_Semantic_Info'Class;
   pragma No_Strict_Aliasing (Root_Sem_Ptr);

   procedure Root_Sem_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Root_Sem_Ptr);
   for Root_Sem_Ptr'Write use Root_Sem_Ptr_Write;
   --  Write out nothing

   procedure Root_Sem_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Root_Sem_Ptr);
   for Root_Sem_Ptr'Read use Root_Sem_Ptr_Read;
   --  Read in nothing, set Item to null

   type Tree is abstract tagged record
      Sem_Info : Root_Sem_Ptr := null;
      Source_Pos : PSC.Source_Positions.Source_Position;
      Language : PSC.Languages.Language_Enum := PSC.Languages.Language;
      Pre_Annotation : Optional_Tree := Null_Optional_Tree;
      Post_Annotation : Optional_Tree := Null_Optional_Tree;
   end record;

   type Tree_Ptr is access all Tree'Class;

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree (defaults to zero)
   --  NOTE: Should include any subtree which might require substitution
   --       as part of module instantiation.

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree
   --  NOTE: Should include any subtree which might require substitution
   --       as part of module instantiation.

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

   function Kind (T : Tree) return Tree_Kind_Enum;
   -- Return tree type as enum

   type Tree_Array is array (Positive range <>) of Optional_Tree;

   function Substitute_Operands
     (T : Tree;
      New_Operands : Tree_Array)
      return Optional_Tree;
   --  Create a new tree given new operands and an existing tree.
   --  The default implementation dispatches to Set_Nth_Operand but
   --  it may be overridden for efficiency.
   --  Requires: New_Operands'Length = Num_Operands(T)

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is abstract;
   --  Produce a human readable display of a subtree, at the given indent
   --  If Use_Short_Form is True, then elide some of the output for
   --  a module or an operation.

   procedure Display_Subtree
     (OT : Optional_Tree;
      On : access Ada.Streams.Root_Stream_Type'Class :=
      Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False);
   --  Produce a human readable display of a subtree, at the given indent
   --  If Use_Short_Form is True, then elide some of the output for
   --  a module or an operation.

   procedure Dump_Subtree (OT : Optional_Tree);
   --  Produce a human readable display of a subtree given an optional tree
   --  on the standard output stream.
   pragma Export (Ada, Dump_Subtree, "dump_subtree");

   function Subtree_Image
     (OT : Optional_Tree;
      Use_Short_Form : Boolean := False;
      Max_Chars : Positive := 2000)
      return String;
   --  Return a text image of the subtree (up to Max_Chars in length)

   function Subtree_Image
     (T : Tree'Class;
      Use_Short_Form : Boolean := False;
      Max_Chars : Positive := 2000)
      return String;
   --  Return a text image of the subtree (up to Max_Chars in length)

   type Root_RO_Tree_Visitor is abstract tagged null record;
   --  A "visitor" object with an action procedure for each kind of tree.
   --  The tree parameter is read-only.

   procedure Visit
     (T : Tree;
      Visitor : in out Root_RO_Tree_Visitor'Class) is abstract;
   --  Call appropriate *_Action procedure on Visitor

   procedure Pre_Visit
     (Visitor : in out Root_RO_Tree_Visitor;
      T : Tree'Class);
   --  A pre-visit routine for RO visitors

   procedure Post_Visit
     (Visitor : in out Root_RO_Tree_Visitor;
      T : Tree'Class);
   --  A post-visit routine for RO visitors

   procedure Visit
     (OT : Optional_Tree;
      Visitor : in out Root_RO_Tree_Visitor'Class);
   --  Call appropriate *_Action procedure on Visitor
   --  given optional tree.

   type Root_RW_Tree_Visitor is abstract tagged null record;
   --  A "visitor" object with an action procedure for each kind of tree.
   --  The tree parameter is read/write.

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is abstract;
   --  Call appropriate RW *_Action procedure on Visitor

   procedure Pre_Visit
     (Visitor : in out Root_RW_Tree_Visitor;
      T : in out Tree'Class);
   --  A pre-visit routine for RW visitors

   procedure Post_Visit
     (Visitor : in out Root_RW_Tree_Visitor;
      T : in out Tree'Class);
   --  A post-visit routine for RW visitors

   procedure Visit
     (OT : in out Optional_Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class);
   --  Call appropriate RW *_Action procedure on Visitor
   --  given optional tree.

   procedure Assign (Left : in out Optional_Tree; Right : Tree'Class);
   --  Copy Right into given "optional" tree.
   --  Reuse existing space if possible.

   function Optional (T : Tree'Class) return Optional_Tree;
   --  Wrap a tree up as an optional tree so can be stored
   --  as an operand.

   function Optional (T : access Tree'Class) return Optional_Tree;
   --  Wrap up a preexisting tree so can be stored as an
   --  an operand.  This is a reference to, not a copy of, the original tree.

   function Is_Null (OT : Optional_Tree) return Boolean;
   --  Return True if OT is empty

   function Not_Null (OT : Optional_Tree) return Boolean;
   --  Return True if OT is not empty

   procedure Set_Source_Pos
     (OT : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position);
   --  Fill in the source position information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).

   procedure Set_End_Source_Pos
     (OT : Optional_Tree;
      End_Source_Pos : Source_Positions.Source_Position);
   --  Fill in the "end" source position information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).

   function Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position;
   --  Return source position information from given tree.
   --  Return Null_Source_Position if Is_Null(OT).

   function Find_Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position;
   --  Walk into tree to try to find a meaningful source position

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position
   is abstract;
   --  Walk into tree to try to find a meaningful source position

   function Find_End_Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position;
   --  Walk into tree to try to find a source position
   --  with the largest End_Line.

   procedure Set_Sem_Info (OT : Optional_Tree; Sem_Info : Root_Sem_Ptr);
   --  Fill in the semantic information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).

   function Sem_Info (OT : Optional_Tree) return Root_Sem_Ptr;
   --  Return semantic information from given tree.
   --  Return null if Is_Null(OT).

   generic
      with procedure Action (T : in out Tree'Class);
   procedure Apply_RW (To : in out Optional_Tree);
   --  Call Action on contained Tree, if any, with read/write access.

   generic
      with procedure Action (T : Tree'Class);
   procedure Apply (To : Optional_Tree);
   --  Call Action on contained Tree, if any.

   generic
      with procedure Pre_Action (T : Tree'Class);
      with procedure Post_Action (T : Tree'Class);
   procedure Apply_To_Subtree (To : Optional_Tree);
   --  Call Pre/Post_Action on entire subtree given pointer to top.

   generic
      with procedure Pre_Action (T : in out Tree'Class);
      with procedure Post_Action (T : in out Tree'Class);
   procedure Apply_To_Subtree_RW (To : in out Optional_Tree);
   --  Call RW Pre/Post_Action on entire subtree given pointer to top.

   generic
      with procedure Action (T : Tree'Class);
   procedure Apply_To_Nth_Operand (T : Tree'Class; N : Positive);
   --  Call Action with Nth operand of given tree, if any.
   --  Requires: Num_Operands(T) >= N

   generic
      with procedure Action (T : in out Tree'Class);
   procedure Apply_To_Nth_Operand_RW
     (T : in out Tree'Class;
      N : Positive);
   --  Call RW Action with Nth operand of given tree, if any.
   --  Requires: Num_Operands(T) >= N

   function Tree_Of (OT : Optional_Tree) return Tree'Class;
   --  Get the tree from an optional tree.
   --  Requires Not_Null(OT);

   function Tree_Ptr_Of (OT : Optional_Tree) return Tree_Ptr;
   --  Get a pointer to the underlying tree from an optional tree.
   --  Requires Not_Null(OT);

   function Sem_Image
     (Sem : access Root_Semantic_Info;
      Use_Short_Form : Boolean := False)
      return String;
   --  Dispatching op to return image of entity identified by semantic info

   function Copy_Tree
     (OT : Optional_Tree)
      return Optional_Tree;
   --  Make a deep copy of given tree, prior to any semantic info
   --  having been added, to use for cases where we have
   --  multiple identifiers in a declaration and the intent
   --  is for it to be equivalent to a sequence of separate
   --  declarations.

private

   type Optional_Tree is record
      Ptr : Tree_Ptr := null;
   end record;

   Null_Optional_Tree : constant Optional_Tree := (Ptr => null);
   --  An empty tree

end PSC.Trees;
