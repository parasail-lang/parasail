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

with PSC.Strings;
with PSC.Trees.Identifier;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Lists;
package PSC.Trees.Iterator is
   --  Representation for an iterator of a for-loop

   use Lists;

   type Iterator_Kind_Enum is (
     Set_Iterator,
     Each_Value,
     Each_Key_Value,
     Initial_Next_Value,
     Initial_Value);

   subtype Container_Iterator is Iterator_Kind_Enum range
     Each_Value .. Each_Key_Value;

   subtype Value_Iterator is Iterator_Kind_Enum range
     Initial_Next_Value .. Initial_Value;

   type Tree is new Trees.Tree with record
      Kind : Iterator_Kind_Enum;
      Name : Optional_Tree;
      Is_Ref : Boolean := False;
      Obj_Type : Optional_Tree;
      Obj_Value : Optional_Tree;
      Next_Values : Lists.List := Lists.Empty_List;
      While_Cond : Optional_Tree := Null_Optional_Tree;
      --  "not" added if "until" used instead.
      Key_Name : Optional_Tree := Null_Optional_Tree;
      Direction : Strings.U_String := Strings.Null_U_String;
   end record;

   function Make
     (Kind : Iterator_Kind_Enum;
      Name : Optional_Tree;
      Is_Ref : Boolean;
      Obj_Type : Optional_Tree;
      Obj_Value : Optional_Tree;
      Next_Values : Lists.List := Lists.Empty_List;
      While_Cond : Optional_Tree := Null_Optional_Tree;
      Key_Name : Optional_Tree := Null_Optional_Tree)
      return Optional_Tree;
   --  Build up an iterator of a for-loop

   procedure Add_Direction
     (OT : Optional_Tree;
      Direction : Strings.U_String);
   --  Add a "direction" to the iterator.

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

end PSC.Trees.Iterator;
