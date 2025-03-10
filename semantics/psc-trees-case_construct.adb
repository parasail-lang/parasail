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

with PSC.Languages;
with PSC.Stream_Output;
with PSC.Trees.Visitor;
package body PSC.Trees.Case_Construct is
   --  Used both for "case" statements and expressions

   function Make
     (Source_Pos : Source_Positions.Source_Position;
      Case_Selector : Optional_Tree;
      Case_Alt_List : Lists.List;
      Is_Case_Expr : Boolean := False;
      End_With_Values : Optional_Tree := Null_Optional_Tree;
      Label : Optional_Tree := Null_Optional_Tree;
      Check_Label : Boolean := True)
      return Optional_Tree
   is
   --  Build up a Case_Construct
      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with
                      Case_Selector => Case_Selector,
                      Case_Alt_List => Case_Alt_List,
                      Is_Case_Expr => Is_Case_Expr,
                      End_With_Values => End_With_Values,
                      Label => Label,
                      Check_Label => Check_Label));
   begin
      Set_Source_Pos (Result, Source_Pos);
      return Result;
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 1 +
             Lists.Length (T.Case_Alt_List) +
             Boolean'Pos (Not_Null (T.End_With_Values));
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      Num_Opnds : constant Natural := Num_Operands (T);
      pragma Assert (N <= Num_Opnds);
   begin
      if N = 1 then
         return T.Case_Selector;
      elsif N = Num_Opnds and then Not_Null (T.End_With_Values) then
         return T.End_With_Values;
      else
         return Lists.Nth_Element (T.Case_Alt_List, N - 1);
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      Num_Opnds : constant Natural := Num_Operands (T);
      pragma Assert (N <= Num_Opnds);
   begin
      if N = 1 then
         T.Case_Selector := New_Operand;
      elsif N = Num_Opnds and then Not_Null (T.End_With_Values) then
         T.End_With_Values := New_Operand;
      else
         T.Case_Alt_List :=
            Lists.Replace_Nth_Element (T.Case_Alt_List, N - 1, New_Operand);
      end if;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Case_Construct_Kind;
   end Kind;

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
      Num_Opnds : constant Natural := Num_Operands (T);
      pragma Assert (New_Operands'Length = Num_Opnds);
      Last_Opnd : Natural := New_Operands'Last;
   begin
      New_Tree.Case_Selector := New_Operands (New_Operands'First);
      if Not_Null (T.End_With_Values) then
         New_Tree.End_With_Values := New_Operands (Last_Opnd);
         Last_Opnd := Last_Opnd - 1;
      end if;
      New_Tree.Case_Alt_List :=
         Lists.Make (New_Operands (New_Operands'First + 1 .. Last_Opnd));
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
      if T.Is_Case_Expr then
         --  This is a case expression
         pragma Assert (Is_Null (T.Label));
         pragma Assert (Is_Null (T.End_With_Values));
         Put (On, "(case ");
         Display_Subtree (T.Case_Selector, On);
         case Languages.Language is
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               Put (On, " of ");
            when Languages.Ada_Ish =>
               Put (On, " is ");
         end case;
         if Use_Short_Form then
            Put (On, " ... ");
         else
            Lists.Display_List (T.Case_Alt_List, On, Separator => "; ");
         end if;
         Put (On, ")");

      else
         --  This is a "case" statement
         if Not_Null (T.Label) then
            --  Put out label at indent-1 after a blank line
            New_Line (On, Indent => Indent - 1);
            Put (On, '*');
            Display_Subtree (T.Label, On);
            Put_Line (On, "*");
         end if;
         Put_Indent (On, Indent);
         if Not_Null (T.Case_Selector) then  --  else an excep handler
            Put (On, "case ");
            Display_Subtree (T.Case_Selector, On);
            case Languages.Language is
               when Languages.ParaSail | Languages.Parython |
                 Languages.Javallel =>
                  Put (On, " of");
               when Languages.Ada_Ish =>
                  Put (On, " is");
            end case;
            if Use_Short_Form then
               Put_Line (On, " ...");
               return;
            end if;
            New_Line (On);
         end if;
         Lists.Display_List
           (T.Case_Alt_List,
            On,
            Separator => ";",
            Terminator => ";",
            Indent => Indent + 2);
         Put_Indent (On, Indent);
         if Not_Null (T.Case_Selector) then
            Put (On, "end case");
         end if;
         if Not_Null (T.Label) then
            --  Put out end label
            Put (On, ' ');
            Display_Subtree (T.Label, On);
         end if;
         if Not_Null (T.End_With_Values) then
            New_Line (On, Indent => Indent + 2);
            Put (On, "with ");
            Display_Subtree (T.End_With_Values, On);
         end if;
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Case_Construct_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Case_Construct_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Case_Selector);
         if T_Source_Pos = Null_Source_Position
           and then Lists.Length (T.Case_Alt_List) > 0
         then
            T_Source_Pos :=
               Find_Source_Pos (Lists.Nth_Element (T.Case_Alt_List, 1));
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Label);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Case_Construct;
