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
package body PSC.Trees.Selection is

   function Make
     (Prefix : Optional_Tree;
      Selector : Optional_Tree)
      return Optional_Tree
   is
   --  Build up a selection
   begin
      return Optional
               (Tree'(Trees.Tree with Prefix => Prefix, Selector => Selector));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (N <= 2);
      if N = 1 then
         return T.Prefix;
      else
         return T.Selector;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      pragma Assert (N <= 2);
      if N = 1 then
         T.Prefix := New_Operand;
      else
         T.Selector := New_Operand;
      end if;
   end Set_Nth_Operand;

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
      Put_Indent (On, Indent);
      Display_Subtree (T.Prefix, On);
      Put (On, ".");
      Display_Subtree (T.Selector, On);
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Selection_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Selection_Action (RW_Tree_Visitor'Class (Visitor), T);
   end Visit;

   function Find_Source_Pos
     (T : Tree)
      return Source_Positions.Source_Position
   is
      --  Walk into tree to try to find a meaningful source position
      use Source_Positions;
      T_Source_Pos : Source_Position := T.Source_Pos;
   begin
      if T_Source_Pos = Null_Source_Position then
         T_Source_Pos := Find_Source_Pos (T.Selector);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Prefix);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Selection;
