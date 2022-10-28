------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with PSC.Symbols;
package PSC.Trees.Control_Stmt is
   --  Representation for a Flow-of-Control statement

   type Control_Stmt_Enum is (
     Null_Stmt,
     Return_Stmt,
     Continue_Stmt,
     Exit_Stmt);

   type Exitable_Construct_Enum is (
     Operation_Body,
     Loop_Stmt,
     If_Stmt,
     Case_Stmt,
     Block_Stmt,
     Case_Or_Loop_Stmt,  --  for "break" in Javallel
     Any_Labeled_Stmt);  --  for exit/break with Id

   type Region_Kind_Set is array (Symbols.Region_Kind_Enum) of Boolean;

   --  Kind(s) of region expected for given kind of exitable construct
   Associated_Region_Kinds : constant array (Exitable_Construct_Enum)
     of Region_Kind_Set :=
     (Operation_Body =>
        (Symbols.Operation_Body_Region_Kind => True, others => False),
      Loop_Stmt =>
        (Symbols.Loop_Body_Region_Kind => True, others => False),
      If_Stmt =>
        (Symbols.If_Stmt_Region_Kind => True, others => False),
      Case_Stmt =>
        (Symbols.Case_Stmt_Region_Kind => True, others => False),
      Block_Stmt =>
        (Symbols.Block_Stmt_Region_Kind => True, others => False),
      Case_Or_Loop_Stmt =>
        (Symbols.Case_Stmt_Region_Kind | Symbols.Loop_Body_Region_Kind => True,
         others => False),
      Any_Labeled_Stmt =>
        (Symbols.Exitable_Stmt_Region_Kind => True, others => False));

   type Tree is new Trees.Tree with record
      Kind : Control_Stmt_Enum;
      Applies_To : Exitable_Construct_Enum;
      Id : Optional_Tree;
      Values : Optional_Tree;
   end record;

   function Make
     (Kind : Control_Stmt_Enum;
      Applies_To : Exitable_Construct_Enum;
      Id : Optional_Tree;
      Values : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position)
      return Optional_Tree;
   --  Build up a control statement

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

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

end PSC.Trees.Control_Stmt;
