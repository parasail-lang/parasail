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

with PSC.Strings;
package PSC.Trees.Assign_Stmt is

   type Assign_Operator_Enum is (
     Assign_Op,
     Plus_Assign_Op,
     Minus_Assign_Op,
     Times_Assign_Op,
     Divide_Assign_Op,
     Power_Assign_Op,
     Combine_Assign_Op,
     Ampersand_Assign_Op,
     And_Assign_Op,
     Or_Assign_Op,
     Xor_Assign_Op,
     Left_Shift_Assign_Op,
     Right_Shift_Assign_Op,
     Swap_Op,
     Move_Op,
     Combine_Move_Op);

   type Tree is new Trees.Tree with record
      Assign_Operator : Assign_Operator_Enum;
      LHS : Optional_Tree;
      RHS : Optional_Tree;
   end record;

   function Make
     (Assign_Operator : Assign_Operator_Enum;
      LHS : Optional_Tree;
      RHS : Optional_Tree)
      return Optional_Tree;
   --  Build up an assignment statement

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

   function Assign_Operator_Designator
     (Op : Assign_Operator_Enum)
      return Strings.U_String;
   --  Return the designator to use when defining an assign operator.
   --  This includes the "" on either side of the operator symbol.

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

end PSC.Trees.Assign_Stmt;
