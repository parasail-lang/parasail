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
with PSC.Trees.Unary;
package body PSC.Trees.Iterator is
   --  Representation for an iterator construct

   function Make
     (Kind : Iterator_Kind_Enum;
      Name : Optional_Tree;
      Is_Ref : Boolean;
      Obj_Type : Optional_Tree;
      Obj_Value : Optional_Tree;
      Next_Values : Lists.List := Lists.Empty_List;
      While_Cond : Optional_Tree := Null_Optional_Tree;
      Key_Name : Optional_Tree := Null_Optional_Tree)
      return Optional_Tree
   is
   begin
      --  Make sure the flags make sense
      if Is_Null (Obj_Value) then
         --  Must be a "for all X : T => " quantified expression
         pragma Assert (Kind = Set_Iterator);
         pragma Assert (Not_Null (Obj_Type));
         pragma Assert (Is_Null (While_Cond));
         pragma Assert (Lists.Is_Empty (Next_Values));
         null;
      end if;
      pragma Assert ((Kind = Each_Key_Value) = Not_Null (Key_Name));
      if Kind = Each_Key_Value then
         pragma Assert (Is_Null (Obj_Type));
         null;
      end if;

      pragma Assert
        ((Kind = Initial_Next_Value) = (Lists.Length (Next_Values) > 0));

      return Optional
               (Tree'(Trees.Tree with
                      Kind => Kind,
                      Name => Name,
                      Is_Ref => Is_Ref,
                      Obj_Type => Obj_Type,
                      Obj_Value => Obj_Value,
                      Next_Values => Next_Values,
                      While_Cond => While_Cond,
                      Key_Name => Key_Name,
                      Direction => Strings.Null_U_String));
   end Make;

   procedure Add_Direction
     (OT : Optional_Tree;
      Direction : Strings.U_String) is
   --  Add a "direction" to the iterator.
   begin
      Iterator.Tree (Tree_Ptr_Of (OT).all).Direction := Direction;
   end Add_Direction;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2 + Lists.Length (T.Next_Values);
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N <= Num_Operands (T));
   begin
      case N is
         when 1 =>
            return T.Obj_Value;
         when 2 =>
            return T.While_Cond;
         when others =>
            return Lists.Nth_Element (T.Next_Values, N - 2);
      end case;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= Num_Operands (T));
   begin
      case N is
         when 1 =>
            T.Obj_Value := New_Operand;
         when 2 =>
            T.While_Cond := New_Operand;
         when others =>
            T.Next_Values :=
               Lists.Replace_Nth_Element (T.Next_Values, N - 2, New_Operand);
      end case;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Iterator_Kind;
   end Kind;
 
   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      use type Strings.U_String;
   begin
      --  Display iterator using given indent for continuation lines
      if T.Kind = Each_Key_Value then
         --  Put out "[key => value]"
         Put (On, "[");
         Display_Subtree (T.Key_Name, On);
         Put (On, " => ");
         Display_Subtree (T.Name, On);
         Put (On, "]");
      else
         --  Put out the iterator variable name
         Display_Subtree (T.Name, On);
      end if;

      if Not_Null (T.Obj_Type) then
         --  Put out the type for the iterator variable
         Put (On, " : ");
         Display_Subtree (T.Obj_Type, On);
      end if;

      if Is_Null (T.Obj_Value) then
         --  Universal quantified expression
         return;
      end if;

      case T.Kind is
         when Set_Iterator =>
            Put (On, " in ");
         when Container_Iterator =>
            Put (On, " of ");
         when Value_Iterator =>
            if T.Is_Ref then
               Put (On, " => ");
            else
               Put (On, " := ");
            end if;
      end case;

      Display_Subtree (T.Obj_Value, On);

      if Length (T.Next_Values) > 0 then
         if Indent > 0 then
            New_Line (On, Indent => Indent + 1);
         end if;
         Put (On, " then ");
         Display_List (T.Next_Values, On, Separator => " || ");
      end if;

      if Not_Null (T.While_Cond) then
         declare
            While_Cond : PSC.Trees.Tree'Class renames Tree_Of (T.While_Cond);
            use type Unary.Unary_Operator_Enum;
         begin
            if Indent > 0 then
               New_Line (On, Indent => Indent + 2);
            else
               Put (On, ' ');
            end if;
            if While_Cond in Unary.Tree'Class
              and then Unary.Tree (While_Cond).Operator = Unary.Not_Op
            then
               --  while not X => until X
               Put (On, "until ");
               Display_Subtree (Unary.Tree (While_Cond).Operand, On);
            else
               Put (On, "while ");
               Display_Subtree (T.While_Cond, On);
            end if;
         end;
      end if;

      if T.Direction /= Strings.Null_U_String then
         --  Put out "forward" or "reverse."
         Put (On, " " & Strings.To_String (T.Direction));
      end if;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Iterator_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Iterator_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Name);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Obj_Type);
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Obj_Value);
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.While_Cond);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Iterator;
