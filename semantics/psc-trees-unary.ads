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

with PSC.Trees.Binary;
with PSC.Strings;
package PSC.Trees.Unary is

   type Unary_Operator_Enum is (
     Plus_Op,
     Minus_Op,
     Not_Op,
     Abs_Op,
     Is_Null_Op,
     Not_Null_Op,
     Meaning_Op,
     Magnitude_Op,
     Updated_Value_Op,
     Initial_Value_Op);

   Precedence_Table : constant
     array (Unary_Operator_Enum) of Binary.Precedence_Type :=
     (Plus_Op | Minus_Op | Not_Op | Abs_Op => 95,
      Is_Null_Op | Not_Null_Op => 85,
      Meaning_Op => Binary.Max_Precedence,
      Magnitude_Op => Binary.Max_Precedence,
      Updated_Value_Op => 85,
      Initial_Value_Op => Binary.Max_Precedence);

   type Tree is new Trees.Tree with record
      Operator : Unary_Operator_Enum;
      Operand : Optional_Tree;
   end record;

   function Make
     (Operator : Unary_Operator_Enum;
      Operand : Optional_Tree)
      return Optional_Tree;
   --  Build up a unary tree.

   function Unary_Operator_Image (Op : Unary_Operator_Enum) return String;
   --  Return string name for Op (surrounding "" are omitted).

   function Unary_Operator_Designator
     (Op : Unary_Operator_Enum)
      return Strings.U_String;
   --  Return U_String that corresponds to operator designator used
   --  when declaring the operator (e.g. "to_univ" rather than "[[...]]")
   --  The U_String *includes* the surrounding "".

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

end PSC.Trees.Unary;
