------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with PSC.Trees.Lists;
package PSC.Trees.Invocation is
   --  A tree for calls, indexing, aggregates, and module instantiations

   type Invocation_Kind_Enum is (
     Operation_Call,
     Container_Indexing,
     Class_Aggregate,
     Container_Aggregate,
     Map_Set_Aggregate,
     Module_Instantiation,
     Is_Function_Of,
     Tuple_Type_Definition);

   subtype Aggregate_Kind_Enum is Invocation_Kind_Enum range
     Class_Aggregate .. Map_Set_Aggregate;

   type Tree is new Trees.Tree with record
      Kind : Invocation_Kind_Enum;
      Prefix : Optional_Tree;
      Operands : Lists.List;
      Extends : Optional_Tree;
   end record;

   function Make
     (Kind : Invocation_Kind_Enum;
      Prefix : Optional_Tree;
      Operands : Lists.List;
      Extends : Optional_Tree := Null_Optional_Tree;
      Source_Pos : Source_Positions.Source_Position :=
     Source_Positions.Null_Source_Position)
      return Optional_Tree;
   --  Build up an Invocation tree.

   function Add_Extends
     (Instantiation : Optional_Tree;
      Extends : Optional_Tree)
      return Optional_Tree;
   --  Add "Extends" to an instantiation
   --  Requires: Instantiation
   --  must be an invocation of Kind Module_Instantiation

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

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
      Use_Short_Form : Boolean := False);
   --  Produce a human readable display of a subtree, at the given indent
   --  If Use_Short_Form is True, then elide some of the output for
   --  a module or an operation.

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class);
   --  Call appropriate RO *_Action procedure on Visitor

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class);
   --  Call appropriate RW *_Action procedure on Visitor

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position;
   --  Walk into tree to try to find a meaningful source position

   function Is_Parenthesized_Expression
     (T : Invocation.Tree)
      return Boolean;
   --  Return True if Invocation is a parenthesized expression
   --  (represented by a Class_Aggregate with a single non "=>" operand)

   function Is_Parenthesized_Expression
     (Expr : Optional_Tree)
      return Boolean;
   --  Return True if Expr is a parenthesized expression
   --  (represented by a Class_Aggregate with a single non "=>" operand)

   function Remove_Parentheses (Expr : Optional_Tree) return Optional_Tree;
   --  Return operand after stripping off any parentheses

end PSC.Trees.Invocation;
