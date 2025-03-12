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
package body PSC.Trees.Block_Stmt is

   function Make
     (Source_Pos : Source_Positions.Source_Position;
      Block_Body : Optional_Tree;
      End_With_Values : Optional_Tree := Null_Optional_Tree;
      Label : Optional_Tree := Null_Optional_Tree;
      Check_Label : Boolean := True)
      return Optional_Tree
   is
   --  Build up a Block_Stmt
      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with
                      Block_Body => Block_Body,
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
      return 0;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (False);
      return Null_Optional_Tree;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      raise Program_Error;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is (Block_Stmt_Kind);

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
      if Not_Null (T.Label) then
         --  Put out label at indent-1 after a blank line
         New_Line (On, Indent => Indent - 1);
         Put (On, '*');
         Display_Subtree (T.Label, On);
         Put_Line (On, "*");
      end if;
      Put_Indent (On, Indent => Indent);
      Put (On, "block ");
      New_Line (On);
      Display_Subtree (T.Block_Body, On, Indent => Indent + 4);
      New_Line (On, Indent => Indent);
      Put (On, "end block");
      if Not_Null (T.Label) then
         Put (On, ' ');
         Display_Subtree (T.Label, On);
      end if;
      if Not_Null (T.End_With_Values) then
         New_Line (On, Indent => Indent + 2);
         Put (On, "with ");
         Display_Subtree (T.End_With_Values, On);
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Block_Stmt_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Block_Stmt_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Block_Body);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Label);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Block_Stmt;
