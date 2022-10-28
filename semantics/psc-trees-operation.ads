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
package PSC.Trees.Operation is
   --  Representation for a function, procedure, or operator

   use Lists;

   type Operation_Kind_Enum is (
     Function_Operation,
     Procedure_Operation,
     Func_Operation,
     Proc_Operation,
     Operator_Operation,
     Op_Operation,
     Lambda_Operation,
     Func_Type_Specifier,
     Proc_Type_Specifier,
     Protected_Func_Type,
     Protected_Proc_Type);

   subtype Operation_Specifier is Operation_Kind_Enum
     range Function_Operation .. Op_Operation;

   subtype Func_Proc_Specifier is Operation_Kind_Enum
     range Function_Operation .. Proc_Operation;

   subtype Operation_Type_Specifier is Operation_Kind_Enum
     range Func_Type_Specifier .. Protected_Proc_Type;

   type Tree is new Trees.Tree with record
      Name : Optional_Tree; --  Qualified or Identifier
      Operation_Kind : Operation_Kind_Enum := Function_Operation;
      Import_Clauses : List;
      Operation_Inputs : List;
      Operation_Outputs : List;
      Global_Read_List : List;
      Global_Update_List : List;
      Preconditions : Optional_Tree;
      Postconditions : Optional_Tree;
      Is_Abstract : Boolean := False;
      Is_Optional : Boolean := False;
      Is_Queued : Boolean := False;
      Is_Def : Boolean := False;
      Is_Expression_Function : Boolean := False;
      Is_Import : Boolean := False;
      Op_Equiv : Optional_Tree;
      Op_Location : Optional_Tree;
      Import_Info : List;
      Dequeue_Condition : Optional_Tree;
      Statements : Optional_Tree;
   end record;

   function Make
     (Name : Optional_Tree;
      Operation_Kind : Operation_Kind_Enum;
      Operation_Inputs : List;
      Operation_Outputs : List;
      Global_Read_List : List := Lists.Empty_List;
      Global_Update_List : List := Lists.Empty_List;
      Preconditions : Optional_Tree;
      Postconditions : Optional_Tree;
      Is_Abstract : Boolean := False;
      Is_Optional : Boolean := False;
      Is_Queued : Boolean := False;
      Is_Def : Boolean := False;
      Is_Expression_Function : Boolean := False;
      Is_Import : Boolean := False;
      Dequeue_Condition : Optional_Tree := Null_Optional_Tree;
      Statements : Optional_Tree := Null_Optional_Tree)
      return Optional_Tree;
   --  Build up an operation

   function Add_Op_Default
     (Op_Decl : Optional_Tree;
      Op_Default : Optional_Tree)
      return Optional_Tree;
   --  Add "Default" to an operation
   --  Requires: Op_Decl must be a tree of type
   --  Operation.Tree with Is_Def False.

   function Add_Op_Equiv
     (Op_Decl : Optional_Tree;
      Op_Equiv : Optional_Tree)
      return Optional_Tree;
   --  Add "Op_Equiv" to an operation
   --  Requires: Op_Decl must be a tree of type
   --  Operation.Tree with Is_Def False.

   function Add_Op_Location
     (Op_Decl : Optional_Tree;
      Op_Location : Optional_Tree)
      return Optional_Tree;
   --  Add "Op_Location" to an operation
   --  Requires: Op_Decl must be a tree of type
   --  Operation.Tree with Is_Def False, or with Op_Equiv specified.

   function Add_Import_Info
     (Op_Decl : Optional_Tree;
      Import_Info : List)
      return Optional_Tree;
   --  Add import info to an operation declaration.
   --  Requires: Op_Decl must be a tree of type
   --  Operation.Tree with Is_Def False.

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

end PSC.Trees.Operation;
