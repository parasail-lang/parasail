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

with PSC.Stream_Output;
with PSC.Trees.Visitor;
package body PSC.Trees.Qualifier is

   function Qualify
     (Qualifiers : Qualifier_Set;
      Operand : Optional_Tree)
      return Optional_Tree
   is
      --  Create/Update a Qualifier tree.
      pragma Assert (Not_Null (Operand));
   begin
      pragma Assert
        (not Qualifiers (Is_Const)
        or else (not Qualifiers (Is_Var)
                and then not Qualifiers (Is_Mutable)));

      pragma Assert
        (not Qualifiers (Is_Abstract)
        or else (not Qualifiers (Is_Optional)
                and then not Qualifiers (Is_Mutable)
                and then not Qualifiers (Is_Var)
                and then not Qualifiers (Is_Const)));

      if Qualifiers = (Qualifier_Enum => False) then
         --  No interesting qualifiers
         return Operand;

      elsif Tree_Of (Operand) in Qualifier.Tree'Class then
         --  "or" in additional qualifiers.
         declare
            Orig_Tree : Qualifier.Tree := Qualifier.Tree (Tree_Of (Operand));
         begin
            Orig_Tree.Qualifiers := Orig_Tree.Qualifiers or Qualifiers;
            return Optional (Orig_Tree);
         end;

      else
         --  Create new qualifier tree.
         return Optional
                  (Tree'(Trees.Tree with Qualifiers => Qualifiers, Operand =>
           Operand));
      end if;
   end Qualify;

   function Qualifiers (OT : Optional_Tree) return Qualifier_Set is
   --  If OT is a Qualifier Tree, return qualifier set.
   --  If not, return (others => False).
   begin
      if Tree_Of (OT) in Qualifier.Tree'Class then
         return Qualifier.Tree (Tree_Of (OT)).Qualifiers;
      else
         return (others => False);
      end if;
   end Qualifiers;

   function Unqualified_Tree (OT : Optional_Tree) return Optional_Tree is
   --  If OT is a Qualifier Tree, return OT.Operand.
   --  If not, return OT.
   begin
      if Tree_Of (OT) in Qualifier.Tree'Class then
         return Qualifier.Tree (Tree_Of (OT)).Operand;
      else
         return OT;
      end if;
   end Unqualified_Tree;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 1;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N = 1);
   begin
      return T.Operand;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N = 1);
   begin
      T.Operand := New_Operand;
   end Set_Nth_Operand;

   function Qualifier_Image (Op : Qualifier_Enum) return String is
   begin
      case Op is
         when Is_Ref =>
            return "ref ";
         when Is_Abstract =>
            return "abstract ";
         when Is_Optional =>
            return "optional ";
         when Is_Not_Null =>
            return "not null ";
         when Is_Mutable =>
            return "mutable ";
         when Is_Concurrent =>
            return "concurrent ";
         when Is_Var =>
            return "var ";
         when Is_Const =>
            return "const ";
         when Is_Polymorphic =>
            return "+";
      end case;
   end Qualifier_Image;

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
      Put_Indent (On, Indent => Indent);
      for I in Qualifier_Enum loop
         if I = Is_Polymorphic then
            --  Display the actual operand before the "+"
            --  for polymorphic type
            Display_Subtree (T.Operand, On);
         end if;
         if T.Qualifiers (I) then
            Put (On, Qualifier_Image (I));
         end if;
      end loop;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Qualifier_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Qualifier_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Operand);
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Qualifier;
