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

with PSC.Trees.Lists;
with PSC.Trees.Compound_Stmt;
with PSC.Strings;
pragma Elaborate (PSC.Strings);
with PSC.Interpreter;

package PSC.Trees.For_Loop_Construct is
   --  Tree for for-loop statement and quantified expression

   type For_Loop_Kind_Enum is (
     For_Loop_Statement,
     Existential_Quantified_Expr,
     Univ_Quantified_Expr,
     Container_Comprehension,
     Map_Reduce_Expr);

   subtype Quantified_Expr_Kinds is For_Loop_Kind_Enum range
     Existential_Quantified_Expr .. Univ_Quantified_Expr;

   Forward_Str : constant Strings.U_String :=
     Strings.String_Lookup ("forward");

   Reverse_Str : constant Strings.U_String :=
     Strings.String_Lookup ("reverse");

   Concurrent_Str : constant Strings.U_String :=
     Strings.String_Lookup ("concurrent");

   Direction_Names : constant
     array (Interpreter.Direction) of Strings.U_String :=
     (Interpreter.Unordered_Dir => Strings.Null_U_String,
      Interpreter.Forward_Dir => Forward_Str,
      Interpreter.Reverse_Dir => Reverse_Str,
      Interpreter.Concurrent_Dir => Concurrent_Str);
   --  These are the names used for the different directions
   --  of iteration.

   type Tree is new Compound_Stmt.Tree with record
      Kind : For_Loop_Kind_Enum;
      Prologue : Lists.List;
      Iterators : Lists.List;
      Filter : Optional_Tree;
      Loop_Body : Optional_Tree;
      Direction : Strings.U_String := Strings.Null_U_String;
      Chunk_Spec : Optional_Tree := Null_Optional_Tree;
   end record;

   function Make
     (Source_Pos : Source_Positions.Source_Position;
      Kind : For_Loop_Kind_Enum;
      Iterators : Lists.List;
      Filter : Lists.List;
      Loop_Body : Optional_Tree;
      Direction : Strings.U_String := Strings.Null_U_String;
      Chunk_Spec : Optional_Tree := Null_Optional_Tree;
      End_With_Values : Optional_Tree := Null_Optional_Tree;
      Label : Optional_Tree := Null_Optional_Tree;
      Check_Label : Boolean := True)
      return Optional_Tree;
   --  Build up a For_Loop_Construct

   function Replace_Body
     (Original : Tree; New_Body : Optional_Tree) return Optional_Tree;
   --  Create a For_Loop_Construct identical to Original except for new body.

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

   function Kind (T : Tree) return Tree_Kind_Enum is (For_Loop_Kind);
   -- Return tree type as enum

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

end PSC.Trees.For_Loop_Construct;
