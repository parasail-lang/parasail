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
with PSC.Trees.Invocation;
package body PSC.Trees.Control_Stmt is
   --  Representation for a Flow-of-Control statement

   function Make
     (Kind : Control_Stmt_Enum;
      Applies_To : Exitable_Construct_Enum;
      Id : Optional_Tree;
      Values : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position)
      return Optional_Tree
   is
      --  Build up a control statement
      Result : Optional_Tree;
   begin
      --  Make sure "Applies_To" and "Id" make sense
      case Kind is
         when Null_Stmt =>
            pragma Assert (Is_Null (Id)); --  no "Id" allowed on a null stmt
            pragma Assert (Is_Null (Values));   --  no "Values" allowed on a
                                                --  null
            null;
         when Return_Stmt =>
            pragma Assert (Is_Null (Id)); --  no "Id" allowed on a return stmt
            pragma Assert (Applies_To = Operation_Body);
            null;
         when Continue_Stmt =>
            pragma Assert (Applies_To = Loop_Stmt);
            null;
         when Exit_Stmt =>
            pragma Assert (Applies_To /= Operation_Body);
            null;
      end case;

      Result :=
         Optional
           (Tree'(Trees.Tree with
                  Kind => Kind,
                  Applies_To => Applies_To,
                  Id => Id,
                  Values => Values));

      Set_Source_Pos (Result, Source_Pos);

      return Result;
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      if Not_Null (T.Values) then
         return 1;
      else
         return 0;
      end if;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N = 1 and then Not_Null (T.Values));
   begin
      return T.Values;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N = 1 and then Not_Null (T.Values));
   begin
      T.Values := New_Operand;
   end Set_Nth_Operand;

   function Exitable_Construct_Image
     (Construct : Exitable_Construct_Enum)
      return String
   is
   begin
      case Construct is
         when Operation_Body =>
            pragma Assert (False);
            return "";
         when Loop_Stmt =>
            return "loop";
         when If_Stmt =>
            return "if";
         when Case_Stmt =>
            return "case";
         when Block_Stmt =>
            return "block";
         when Case_Or_Loop_Stmt =>
            return "(switch or loop)";  -- for Javallel "break"
         when Any_Labeled_Stmt =>
            return "(any labeled stmt)";  -- for Javallel "break"
      end case;
   end Exitable_Construct_Image;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      use type Invocation.Invocation_Kind_Enum;
   begin
      --  Display control stmt at given indent
      Put_Indent (On, Indent);

      case T.Kind is
         when Null_Stmt =>
            Put (On, "null");

         when Return_Stmt =>
            Put (On, "return");
            if Not_Null (T.Values) then
               --  TBD: AdaMagic has trouble if we write
               --   "if Not_Null() and then (Tree_Of(T.Values) not in ...)"
               --   because it calls "init_master" only if Not_Null but
               --   calls finalize-and-pop unconditionally.
               declare
                  Return_Values : Trees.Tree'Class renames Tree_Of (T.Values);
               begin
                  if Return_Values not in Invocation.Tree'Class
                    or else Invocation.Tree (Return_Values).Kind /=
                            Invocation.Class_Aggregate
                  then
                     --  Just returning a simple single value
                     Put (On, ' ');
                     Display_Subtree (T.Values, On);
                     return; --  all done
                  end if;
               end;
            end if;
         --  Fall through to finish the job

         when Continue_Stmt =>
            Put (On, "continue loop");
         when Exit_Stmt =>
            Put (On, "exit ");
            Put (On, Exitable_Construct_Image (T.Applies_To));
      end case;

      --  Put out rest of control statement
      if Not_Null (T.Id) then
         Put (On, ' ');
         Display_Subtree (T.Id, On);
      end if;

      if Not_Null (T.Values) then
         declare
            Indent_For_Values : Natural := 0;
         begin
            if Indent > 0 then
               --  Start a new line if indenting
               declare
                  Value_Tree : Trees.Tree'Class renames Tree_Of (T.Values);
                  use type Invocation.Invocation_Kind_Enum;
               begin
                  New_Line (On, Indent => Indent + 2);
                  if Value_Tree in Invocation.Tree'Class
                    and then Invocation.Tree (Value_Tree).Kind =
                             Invocation.Class_Aggregate
                  then
                     --  Pass an indent into invocation display routine
                     Indent_For_Values := Indent + 5;
                     --  Invocation adds another 3,
                     --  which causes this to line up below "XX" in
                     --  "  with (XX".
                  end if;
               end;
            else
               Put (On, ' ');
            end if;
            Put (On, "with ");
            Display_Subtree (T.Values, On, Indent => Indent_For_Values);
         end;
      end if;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Control_Stmt_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Control_Stmt_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Id);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Values);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Control_Stmt;
