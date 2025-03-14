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

with PSC.Trees.Visitor;
with PSC.Stream_Output;
package body PSC.Trees.Implements_Element is

   function Make
     (For_Interfaces : Lists.List;
      Elements : Lists.List)
      return Optional_Tree
   is
   --  Build up an "implements" element.
   begin
      return Optional
               (Tree'(Trees.Tree with
                      For_Interfaces => For_Interfaces,
                      Elements => Elements));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return Lists.Length (T.Elements);
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      return Lists.Nth_Element (T.Elements, N);
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      T.Elements := Lists.Replace_Nth_Element (T.Elements, N, New_Operand);
   end Set_Nth_Operand;

   function Substitute_Operands
     (T : Tree;
      New_Operands : Tree_Array)
      return Optional_Tree
   is
      --  Create a new tree given new operands and an existing tree.
      --  The default implementation dispatches to Set_Nth_Operand but
      --  it may be overridden for efficiency.
      --  Requires: New_Operands'Length = Num_Operands(T)
      New_Tree : Tree := T;
   begin
      New_Tree.Elements := Lists.Make (New_Operands);
      return Optional (New_Tree);
   end Substitute_Operands;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
   begin
      --  Display implements_element at given indent

      if Lists.Length (T.For_Interfaces) > 0 then
         Put_Indent (On, Indent);
         Put (On, "for ");
         Display_List (T.For_Interfaces, On, Separator => ", ");
         New_Line (On);
      end if;

      Display_List
        (T.Elements,
         On,
         Indent => Indent + 2,
         Separator => ";",
         Terminator => ";");

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Implements_Element_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Implements_Element_Action (RW_Tree_Visitor'Class (Visitor), T);
   end Visit;

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position
   is
   --  Walk into tree to try to find a meaningful source position
   begin
      if Lists.Length (T.For_Interfaces) > 0 then
         return Find_Source_Pos (Lists.Nth_Element (T.For_Interfaces, 1));
      elsif Lists.Length (T.Elements) > 0 then
         return Find_Source_Pos (Lists.Nth_Element (T.Elements, 1));
      else
         return Source_Positions.Null_Source_Position;
      end if;
   end Find_Source_Pos;

end PSC.Trees.Implements_Element;
