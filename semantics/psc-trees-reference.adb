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

with PSC.Stream_Output;
with PSC.Trees.Lists;
with PSC.Trees.Visitor;
with PSC.Trees.Invocation;
package body PSC.Trees.Reference is

   function Make
     (Key : Optional_Tree;
      Referent : Optional_Tree)
      return Optional_Tree
   is
   --  Build up a Reference tree.
   begin
      return Optional (Tree'(Trees.Tree with Key, Referent));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N <= 2);
   begin
      if N = 1 then
         return T.Key;
      else
         return T.Referent;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= 2);
   begin
      if N = 1 then
         T.Key := New_Operand;
      else
         T.Referent := New_Operand;
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

      procedure Display_Key is
         --  Handle special case where "key" is "[X => blah]"
         --  and display it as "[X : blah]" instead as appropriate
         --  for a "case" statement or expression.
         Key : Trees.Tree'Class renames Tree_Of (T.Key);
         use type Invocation.Invocation_Kind_Enum;
      begin
         if Key in Invocation.Tree
           and then Invocation.Tree (Key).Kind =
                    Invocation.Container_Aggregate
         then
            declare
               Agg : Invocation.Tree renames Invocation.Tree (Key);
            begin
               if Lists.Length (Agg.Operands) = 1 then
                  declare
                     Only_Elem : Trees.Tree'Class renames Tree_Of
                                                            (
                                                            Lists.Nth_Element
                                                              (Agg.Operands,
                                                               1));
                  begin
                     if Only_Elem in Reference.Tree then
                        --  OK, we have the interesting special case
                        declare
                           Ref : Reference.Tree renames Reference.Tree (
                             Only_Elem);
                        begin
                           Put (On, "[");
                           Display_Subtree (Ref.Key, On);
                           Put (On, " : ");
                           Display_Subtree (Ref.Referent, On);
                           Put (On, "]");
                           return;
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
         --  Do the default thing
         Display_Subtree (T.Key, On);
      end Display_Key;

   begin   --  Display_Subtree

      if Indent > 0 then
         --  Assume is a separate statement
         Put_Indent (On, Indent);
         Display_Key;
         Put_Line (On, " =>");
         Display_Subtree (T.Referent, On, Indent => Indent + 2);
      else
         --  Assume is a parameter association
         Display_Key;
         Put (On, " => ");
         Display_Subtree (T.Referent, On);
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Reference_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Reference_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Key);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Referent);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Reference;
