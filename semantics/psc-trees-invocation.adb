------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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
with PSC.Strings;
with PSC.Trees.Visitor;

with PSC.Trees.Assign_Stmt;
with PSC.Trees.Iterator;
with PSC.Trees.Module;
with PSC.Trees.Reference;
package body PSC.Trees.Invocation is

   Open_Char : constant array (Invocation_Kind_Enum) of Character :=
     (Operation_Call | Class_Aggregate | Is_Function_Of |
        Tuple_Type_Definition => '(',
      Container_Indexing | Container_Aggregate => '[',
      Map_Set_Aggregate => '{',
      Module_Instantiation => '<');

   Close_Char : constant array (Invocation_Kind_Enum) of Character :=
     (Operation_Call | Class_Aggregate | Is_Function_Of |
        Tuple_Type_Definition => ')',
      Container_Indexing | Container_Aggregate => ']',
      Map_Set_Aggregate => '}',
      Module_Instantiation => '>');

   Separator_Char : constant array (Invocation_Kind_Enum) of Character :=
     (Operation_Call | Aggregate_Kind_Enum | Is_Function_Of |
      Container_Indexing | Module_Instantiation => ',',
      Tuple_Type_Definition => ';');

   function Make
     (Kind : Invocation_Kind_Enum;
      Prefix : Optional_Tree;
      Operands : Lists.List;
      Extends : Optional_Tree := Null_Optional_Tree;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree
   is
      --  Build up an Invocation tree.
      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with Kind, Prefix, Operands, Extends));
      use type Strings.U_String_Index;
   begin
      if Source_Pos.File /= Strings.Null_U_String_Index then
         --  Fill in the source position information
         Set_Source_Pos (Result, Source_Pos);
      end if;
      return Result;
   end Make;

   function Add_Extends
     (Instantiation : Optional_Tree;
      Extends : Optional_Tree)
      return Optional_Tree
   is
      --  Add "Extends" to an instantiation
      --  Requires: Instantiation
      --  must be an invocation of Kind Module_Instantiation
      pragma Assert (Not_Null (Instantiation));
      pragma Assert (Tree_Of (Instantiation) in Invocation.Tree'Class);
      pragma Assert
        (Invocation.Tree (Tree_Of (Instantiation)).Kind =
         Module_Instantiation);
   begin
      --  Fill in the "Extends" field in place
      Invocation.Tree (Instantiation.Ptr.all).Extends := Extends;
      return Instantiation;
   end Add_Extends;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return Boolean'Pos (Not_Null (T.Prefix)) +
             Lists.Length (T.Operands) +
             Boolean'Pos (Not_Null (T.Extends));
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N <= Num_Operands (T));
      Operand_Pos : constant Natural :=
        N - Boolean'Pos (Not_Null (T.Prefix));
   begin
      if Operand_Pos = 0 then
         return T.Prefix;
      elsif Operand_Pos <= Lists.Length (T.Operands) then
         return Lists.Nth_Element (T.Operands, Operand_Pos);
      else
         return T.Extends;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= Num_Operands (T));
      Operand_Pos : constant Natural :=
        N - Boolean'Pos (Not_Null (T.Prefix));
   begin
      if Operand_Pos = 0 then
         T.Prefix := New_Operand;
      elsif Operand_Pos <= Lists.Length (T.Operands) then
         T.Operands :=
            Lists.Replace_Nth_Element (T.Operands, Operand_Pos, New_Operand);
      else
         T.Extends := New_Operand;
      end if;
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
      pragma Assert (New_Operands'Length = Num_Operands (T));
      Index : Natural := New_Operands'First;
      New_Tree : Tree := T;
   begin
      --  Prefix if any comes first
      if Not_Null (T.Prefix) then
         New_Tree.Prefix := New_Operands (Index);
         Index := Index + 1;
      end if;
      --  Operands are in the middle
      New_Tree.Operands :=
         Lists.Make
           (New_Operands (Index .. Index + Lists.Length (T.Operands) - 1));
      --  "Extends" if any comes last
      if Not_Null (T.Extends) then
         New_Tree.Extends := New_Operands (New_Operands'Last);
      end if;
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
      Indent_For_Operands : Natural := 0;

      Is_First : Boolean := True;
      Separator : constant Character := Separator_Char (T.Kind);

      procedure Display_Element (Elem : Trees.Tree'Class) is
         --  Display one element of list of operands

         function Elem_Is_Iterator return Boolean is
         --  Return True if Elem is an iterator rather than a simple
         --  element or A..B => expr.
         begin
            if Elem in Iterator.Tree then
               --  This is an iterator so we need a "for"
               return True;
            elsif Elem in Reference.Tree
              and then Not_Null (Reference.Tree (Elem).Key)
            then
               --  TBD: AdaMagic bug: if we use "and then"
               --      to check if Tree_Of(Key) is in Iterator.Tree
               --      we die since master isn't always initialized.
               --  This is Key => Expr, check if Key is an iterator
               return Tree_Of (Reference.Tree (Elem).Key) in Iterator.Tree;
            else
               --  Not an iterator
               return False;
            end if;
         end Elem_Is_Iterator;

         Is_Iterator : constant Boolean := Elem_Is_Iterator;

      begin   --  Display_Element

         if Is_First then
            Is_First := False;
         else
            PSC.Stream_Output.Put (On, Separator & ' ');
            if Indent_For_Operands > 0 then
               PSC.Stream_Output.New_Line (On, Indent => Indent_For_Operands);
            end if;
         end if;

         if Is_Iterator then
            --  This is an iterator so we need a "for"
            Put (On, "for ");

            if Elem in Reference.Tree then
               declare
                  Inner_Elem : Trees.Tree'Class renames
                    Tree_Ptr_Of (Reference.Tree (Elem).Referent).all;
               begin
                  if Inner_Elem in Reference.Tree then
                     --  A key was specified to use instead of loop parameter
                     --  So display, e.g. "for I in 1..10, I*2 => I**2"
                     Display_Subtree (Reference.Tree (Elem).Key, On);
                     Put (On, ", ");
                     Display_Subtree (Inner_Elem, On,
                       Use_Short_Form => Use_Short_Form);

                     return;   --  All done  --
                  end if;
               end;
            end if;
         end if;

         Display_Subtree (Elem, On, Use_Short_Form => Use_Short_Form);
      end Display_Element;

      procedure Display_All_Elements is new Lists.Apply_To_List (
         Display_Element);
   --  Display each element of list

   begin --  Display_Subtree

      if Indent > 0 then
         if T.Kind = Operation_Call then
            --  This is the one that might be a statement.
            --  TBD: This isn't a great way to make this distinction.
            Put_Indent (On, Indent);
         end if;
         Indent_For_Operands := Indent + 3;
      end if;

      if Not_Null (T.Prefix) then
         if T.Kind = Module_Instantiation
           and then Tree_Ptr_Of (T.Prefix).all in Module.Tree
         then
            --  Prefix is a module; display it and return
            Display_Subtree (T.Prefix, On, Indent => Indent,
              Use_Short_Form => Use_Short_Form);

            return;  --  Return now

         end if;

         Display_Subtree (T.Prefix, On);

         case T.Kind is
            when Aggregate_Kind_Enum =>
               Put (On, "::");
            when Is_Function_Of =>
               Put (On, " is func");
            when Operation_Call | Container_Indexing |
              Module_Instantiation | Tuple_Type_Definition =>
               null;
         end case;

         if Indent > 0 then
            New_Line (On, Indent => Indent + 2);
         end if;
      end if;

      Put (On, Open_Char (T.Kind));

      Display_All_Elements (T.Operands);

      Put (On, Close_Char (T.Kind));
      if Not_Null (T.Extends) then
         if Indent > 0 then
            New_Line (On, Indent => Indent + 2);
         else
            Put (On, ' ');
         end if;
         Put (On, "extends ");
         Display_Subtree (T.Extends, On);
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Invocation_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Invocation_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Prefix);
      end if;
      if T_Source_Pos = Null_Source_Position
        and then Lists.Length (T.Operands) > 0
      then
         T_Source_Pos := Find_Source_Pos (Lists.Nth_Element (T.Operands, 1));
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

   function Is_Parenthesized_Expression
     (T : Invocation.Tree)
      return Boolean
   is
      --  Return True if Invocation is a parenthesized expression
      --  (represented by a Class_Aggregate with a single non "=>" operand)
      use Invocation;
   begin
      if T.Kind = Class_Aggregate
        and then Lists.Length (T.Operands) = 1
      then
         declare
            Operand : constant Optional_Tree :=
              Lists.Nth_Element (T.Operands, 1);
            Operand_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Operand).all;
         begin
            if Operand_Tree not in Reference.Tree'Class
              and then Operand_Tree not in Assign_Stmt.Tree'Class
            then

               --  Presume is simple parentheses
               return True;
            end if;
         end;
      end if;

      --  Not a parenthesized expression
      return False;
   end Is_Parenthesized_Expression;

   function Is_Parenthesized_Expression
     (Expr : Optional_Tree)
      return Boolean
   is
      --  Return True if Expr is a parenthesized expression
      --  (represented by a Class_Aggregate with a single non "=>" operand)
      Expr_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Expr).all;
   begin
      if Expr_Tree in Invocation.Tree then
         return Is_Parenthesized_Expression (Invocation.Tree (Expr_Tree));
      end if;

      --  Not a parenthesized expression
      return False;
   end Is_Parenthesized_Expression;

   function Remove_Parentheses (Expr : Optional_Tree) return Optional_Tree is
   --  Return operand after stripping off any parentheses
   begin
      if not Is_Parenthesized_Expression (Expr) then
         --  No parentheses to remove
         return Expr;
      else
         --  Remove one level of parens.
         return Remove_Parentheses
                  (Lists.Nth_Element
                      (Invocation.Tree (Tree_Ptr_Of (Expr).all).Operands,
                       1));
      end if;
   end Remove_Parentheses;

end PSC.Trees.Invocation;
