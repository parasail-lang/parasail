------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
package PSC.Trees.Binary is

   type Binary_Operator_Enum_With_Null is (
     Not_An_Op,
     Plus_Op,
     Minus_Op,
     Times_Op,
     Divide_Op,
     Mod_Op,
     Rem_Op,
     Power_Op,
     Compare_Op,
     Less_Op,
     LEQ_Op,
     Equal_Op,
     NEQ_Op,
     GEQ_Op,
     Greater_Op,
     And_Op,
     Or_Op,
     Xor_Op,
     And_Then_Op,
     Or_Else_Op,
     Implies_Op,
     Combine_Op,
     Ampersand_Op,
     In_Op,
     Not_In_Op,
     Meaning_Op,
     Left_Shift_Op,
     Right_Shift_Op,
     Closed_Interval_Op,
     Open_Interval_Op,
     Open_Closed_Interval_Op,
     Closed_Open_Interval_Op,
     Sequential_Stmt_Op,
     Parallel_Stmt_Op,
     Handled_Stmt_Op,
     Next_Stmt_Op,
     Then_Stmt_Op);

   subtype Binary_Operator_Enum is Binary_Operator_Enum_With_Null range
     Binary_Operator_Enum_With_Null'Succ
        (Binary_Operator_Enum_With_Null'First) ..
     Binary_Operator_Enum_With_Null'Last;
   --  Eliminate Not_An_Op from enumeration

   subtype Stmt_Ops is Binary_Operator_Enum range
     Sequential_Stmt_Op .. Then_Stmt_Op;
   --  Operators used to separate/terminate statements

   subtype Independent_Stmt_Ops is Stmt_Ops range
     Parallel_Stmt_Op .. Handled_Stmt_Op;
   --  Statement ops where LHS and RHS get independent declarative regions

   subtype Interval_Ops is Binary_Operator_Enum range
     Closed_Interval_Op .. Closed_Open_Interval_Op;

   subtype Logical_Ops is Binary_Operator_Enum range And_Op .. Implies_Op;

   subtype Short_Circuit_Ops is Binary_Operator_Enum range
     And_Then_Op .. Implies_Op;
   --  These are expressed in terms of the equivalent "if" expression

   subtype Shift_Ops is Binary_Operator_Enum range
     Left_Shift_Op .. Right_Shift_Op;

   subtype Relational_Ops is Binary_Operator_Enum range Less_Op .. Greater_Op;

   Max_Precedence : constant := 110;
   --  Used for things that never need to be parenthesized

   type Precedence_Type is range 1 .. Max_Precedence;  --  Operator precedence

   Precedence_Table : constant
     array (Binary_Operator_Enum) of Precedence_Type :=
     (Power_Op | Meaning_Op => 100,
      Times_Op | Divide_Op | Mod_Op | Rem_Op => 90,
      Interval_Ops => 85,
      Plus_Op | Minus_Op | Ampersand_Op => 80,
      Compare_Op |
      Less_Op    |
      LEQ_Op     |
      Equal_Op   |
      NEQ_Op     |
      GEQ_Op     |
      Greater_Op => 70,
      In_Op | Not_In_Op => 70,
      Shift_Ops => 70,
      And_Op | Or_Op | Xor_Op | And_Then_Op | Or_Else_Op | Implies_Op => 60,
      Combine_Op => 50,
      Sequential_Stmt_Op | Next_Stmt_Op => 40,
      Parallel_Stmt_Op => 30,
      Handled_Stmt_Op | Then_Stmt_Op => 20);

   Assignment_Op_Precedence : constant Precedence_Type := 45;
   --  Higher than statement ops, lower than other things
   --  TBD: This is really only relevant if we start allowing
   --      these inside expressions.

   type Associativity_Enum is (Left_Assoc, Non_Assoc, Right_Assoc);
   --  Associativity of operators

   Associativity_Table : constant
     array (Binary_Operator_Enum) of Associativity_Enum :=
     (Power_Op => Right_Assoc,
      Times_Op | Divide_Op | Mod_Op | Rem_Op => Left_Assoc,
      Interval_Ops => Non_Assoc,
      Plus_Op | Minus_Op | Ampersand_Op => Left_Assoc,
      Compare_Op |
      Less_Op    |
      LEQ_Op     |
      Equal_Op   |
      NEQ_Op     |
      GEQ_Op     |
      Greater_Op => Non_Assoc,
      In_Op | Not_In_Op | Meaning_Op => Non_Assoc,
      Shift_Ops => Non_Assoc,
      And_Op | Or_Op | Xor_Op | And_Then_Op | Or_Else_Op | Implies_Op =>
        Non_Assoc,
      Combine_Op => Left_Assoc,
      Handled_Stmt_Op => Non_Assoc,
      Sequential_Stmt_Op | Next_Stmt_Op => Left_Assoc,
      Parallel_Stmt_Op | Then_Stmt_Op => Left_Assoc);

   type Tree is new Trees.Tree with record
      Operator : Binary_Operator_Enum;
      Left_Operand : Optional_Tree;
      Right_Operand : Optional_Tree;
   end record;

   function Make
     (Operator : Binary_Operator_Enum;
      Left_Operand : Optional_Tree;
      Right_Operand : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree;
   --  Build up a binary tree.

   function Precedence (OT : Optional_Tree) return Precedence_Type;
   --  Return precedence of given tree.
   --  Return Max_Precedence if not a unary or binary operator

   function Binary_Operator_Image (Op : Binary_Operator_Enum) return String;
   --  Return string name for Op (the surrounding "" are omitted).

   function Binary_Operator_Designator
     (Op : Binary_Operator_Enum)
      return Strings.U_String;
   --  Return U_String that corresponds to operator designator used
   --  when declaring the operator.
   --  The U_String *includes* the surrounding "".

   function Is_Interval_Op_Designator
     (Desig : Strings.U_String)
      return Boolean;
   --  Return True if Desig is one of the interval operators
   --  ("..", "<..", etc.)

   function Is_Parallel_Stmt_Op (OT : Optional_Tree) return Boolean;
   --  Return True if OT is a binary node with operator
   --  Binary.Parallel_Stmt_Op.

   function Num_Operands (T : Tree) return Natural;
   --  Return number of operands of given Tree

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree;
   --  Return Nth operand of given Tree

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree);
   --  Set Nth operand of given Tree

   function Kind (T : Tree) return Tree_Kind_Enum;
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

end PSC.Trees.Binary;
