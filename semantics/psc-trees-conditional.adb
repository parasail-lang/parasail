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
with PSC.Trees.Visitor;
package body PSC.Trees.Conditional is

   function Make
     (Source_Pos : Source_Positions.Source_Position;
      Kind : Conditional_Kind_Enum;
      Cond : Optional_Tree;
      Then_Part : Optional_Tree;
      Else_Part : Optional_Tree;
      End_With_Values : Optional_Tree := Null_Optional_Tree;
      Label : Optional_Tree := Null_Optional_Tree;
      Check_Label : Boolean := True)
      return Optional_Tree
   is
   --  Build up a conditional
      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with
                      Kind => Kind,
                      Cond => Cond,
                      Then_Part => Then_Part,
                      Else_Part => Else_Part,
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
      return 3;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      case N is
         when 1 =>
            return T.Cond;
         when 2 =>
            return T.Then_Part;
         when 3 =>
            return T.Else_Part;
         when others =>
            pragma Assert (N <= 3);
            return Null_Optional_Tree;
      end case;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      case N is
         when 1 =>
            T.Cond := New_Operand;
         when 2 =>
            T.Then_Part := New_Operand;
         when 3 =>
            T.Else_Part := New_Operand;
         when others =>
            pragma Assert (N <= 3);
            return;
      end case;
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

      procedure Display_Starting_At_Cond (T : Tree) is
      --  Presume Indent and "if " or "elsif " already emitted
      --  Presume caller puts out "end if"
      begin
         Display_Subtree (T.Cond, On);
         if T.Kind in Expr_Parts then
            Put (On, " then ");
            Display_Subtree (T.Then_Part, On);
         else
            if Use_Short_Form then
               Put_Line (On, " then ...");
               return;
            end if;
            Put_Line (On, " then");
            Display_Subtree (T.Then_Part, On, Indent => Indent + 4);
            Put_Line (On, ";");
            Put_Indent (On, Indent);
         end if;

         if Not_Null (T.Else_Part) then
            if T.Kind in Expr_Parts then
               Put (On, " ");
            end if;
            if T.Else_Part.Ptr.all in Conditional.Tree
              and then Is_Null (Conditional.Tree (T.Else_Part.Ptr.all).Label)
            then
               --  We have an "elsif"; recurse
               Put (On, "elsif ");
               Display_Starting_At_Cond
                 (Conditional.Tree (T.Else_Part.Ptr.all));
            else
               --  Put out else part
               if T.Kind in Expr_Parts then
                  Put (On, "else ");
                  Display_Subtree (T.Else_Part, On);
               else
                  Put_Line (On, "else");
                  Display_Subtree (T.Else_Part, On, Indent => Indent + 4);
                  Put_Line (On, ";");
                  Put_Indent (On, Indent);
               end if;
            end if;
         end if;
      end Display_Starting_At_Cond;

   begin
      case T.Kind is
         when Stmt_Parts =>
            --  This is an "if" statement
            if Not_Null (T.Label) then
               --  Put out label at indent-1 after a blank line
               New_Line (On, Indent => Indent - 1);
               Put (On, '*');
               Display_Subtree (T.Label, On);
               Put_Line (On, "*");
            end if;
            Put_Indent (On, Indent);
            Put (On, "if ");
            Display_Starting_At_Cond (T);
            if Use_Short_Form then
               return;
            end if;
            Put (On, "end if");
            if Not_Null (T.Label) then
               Put (On, ' ');
               Display_Subtree (T.Label, On);
            end if;
            if Not_Null (T.End_With_Values) then
               New_Line (On, Indent => Indent + 2);
               Put (On, "with ");
               Display_Subtree (T.End_With_Values, On);
            end if;
         when Quest_Colon =>
            --  This is a "?:"
            pragma Assert (Is_Null (T.Label));
            pragma Assert (Is_Null (T.End_With_Values));
            Display_Subtree (T.Cond, On);
            Put (On, "? ");
            Display_Subtree (T.Then_Part, On);
            Put (On, ": ");

            pragma Assert (Not_Null (T.Else_Part));
            Display_Subtree (T.Else_Part, On);
         when Expr_Parts =>
            --  This is an "if" expression
            Put (On, "(if ");
            Display_Starting_At_Cond (T);
            Put (On, ")");
      end case;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Conditional_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Conditional_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Cond);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Then_Part);
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Label);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Conditional;
