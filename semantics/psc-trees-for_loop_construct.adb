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

with PSC.Messages;
with PSC.Stream_Output;
with PSC.Symbols;
with PSC.Trees.Annotation;
with PSC.Trees.Binary;
with PSC.Trees.Identifier;
with PSC.Trees.Invocation;
with PSC.Trees.Iterator;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Property;
with PSC.Trees.Reference;
with PSC.Trees.Visitor;
package body PSC.Trees.For_Loop_Construct is

   --------  Visible subprograms  ---------

   function Make
     (Source_Pos : Source_Positions.Source_Position;
      Kind : For_Loop_Kind_Enum;
      Iterators : Lists.List;
      Filter : Lists.List;
      Loop_Body : Optional_Tree;
      Direction : Strings.U_String := Strings.Null_U_String;
      Chunk_Spec : Optional_Tree := Null_Optional_Tree;
      End_With_Values : Optional_Tree := Null_Optional_Tree;
      Label : Optional_Tree := Null_Optional_Tree;
      Check_Label : Boolean := True)
      return Optional_Tree
   is
      Is_Chunked : constant Boolean := Not_Null (Chunk_Spec)
        and then Tree_Ptr_Of (Chunk_Spec).all in Iterator.Tree;

      Outer_Loop : For_Loop_Construct.Tree :=
        For_Loop_Construct.Tree'
          (Trees.Tree with
           Kind => Kind,
           Prologue => Lists.Empty_List,
           Iterators => Iterators,        --  Updated below if chunked
           Filter => Null_Optional_Tree,  --  Filled in below
           Loop_Body => Loop_Body,        --  Updated below if chunked
           Direction => Direction,
           Chunk_Spec => Chunk_Spec,
           End_With_Values => End_With_Values,
           Label => Label,
           Check_Label => Check_Label);

      Filtering_Annotation : Optional_Tree := Null_Optional_Tree;

      use type Source_Positions.Source_Position;
      use type Strings.U_String;

      pragma Assert (Source_Pos /= Source_Positions.Null_Source_Position);
      --  We need a "good" source position.

   begin  --  Make

      Outer_Loop.Source_Pos := Source_Pos;

      if not Lists.Is_Empty (Filter) then
         --  Create the filtering annotation
         Filtering_Annotation := Annotation.Make (Filter);
      end if;

      if Is_Chunked then
         --  We have a chunked iteration.
         --  We will create a nested loop, with outer parallel iteration
         --  over the chunk parameter, and inner sequential loop over the
         --  "real" iterator(s).

         --  Replace the Iterators of the outer loop with an iteration
         --  over the chunk index.
         --  But first set direction of chunk spec back to be unspecified.
         Iterator.Tree (Tree_Ptr_Of (Chunk_Spec).all).Direction :=
           PSC.Strings.Null_U_String;

         Outer_Loop.Iterators := Lists.Make ((1 => Chunk_Spec));

         --  Now replace the body with a nested loop
         --  Use "default" direction for this nested loop
         --   (iterators already are "forward").
         Outer_Loop.Loop_Body := Optional (For_Loop_Construct.Tree'
           (Trees.Tree with
            Kind => Kind,
            Prologue => Lists.Empty_List,
            Iterators => Iterators,
            Filter => Filtering_Annotation,
            Loop_Body => Loop_Body,
            Direction => PSC.Strings.Null_U_String,
            Chunk_Spec => Chunk_Spec,
            End_With_Values => End_With_Values,
            Label => Null_Optional_Tree,
            Check_Label => False));

         Set_Source_Pos (Outer_Loop.Loop_Body, Find_Source_Pos (Chunk_Spec));

         if Lists.Length (Iterators) /= 1 then
            Messages.Parser_Error
              ("Only one iterator permitted in chunked for-loop.",
               Find_Source_Pos (Lists.Nth_Element (Iterators, 2)));
         end if;

         --  We want to add another iterator to the outer loop
         --  to iterate through the "split-into-chunks" of the
         --  set iterated over by the inner loop.
         --  We then replace the inner iterator's iterable object
         --  with the loop parameter from this outer iterator.
         declare
            --  New outer iterator loop parameter name
            Set_Chunk_Loop_Str : constant Strings.U_String :=
               Symbols.Generate_Unique_Label
                 (Outer_Loop.Source_Pos, "chunk_of_set_");
            Set_Chunk_Loop_Param : constant Optional_Tree :=
              Identifier.Make (Set_Chunk_Loop_Str, Outer_Loop.Source_Pos);

            --  Original outer (chunk) iterator
            Chunk_Iter : Iterator.Tree renames Iterator.Tree
                     (Tree_Ptr_Of (Chunk_Spec).all);

            --  Make a temporary copy of the Chunk Range to avoid
            --  evaluating it more than once.
            Chunk_Range : constant Optional_Tree := Chunk_Iter.Obj_Value;

            Chunk_Range_Str : constant Strings.U_String :=
              Symbols.Generate_Unique_Label (Find_Source_Pos (Chunk_Range),
                "chunk_range_");

            Chunk_Range_Temp : constant Optional_Tree := Identifier.Make
              (Chunk_Range_Str, Find_Source_Pos (Chunk_Range));

            Chunk_Range_Decl : constant Optional_Tree := Obj_Decl.Make
                (Name => Identifier.Tree (Tree_Ptr_Of (Chunk_Range_Temp).all),
                 Is_Var => False,
                 Is_Const => True,
                 Is_Ref => False,
                 Is_Optional => False,
                 Obj_Type => Null_Optional_Tree,
                 Obj_Value => Chunk_Range);

            Inner_Loop : For_Loop_Construct.Tree renames
              For_Loop_Construct.Tree (Tree_Ptr_Of (Outer_Loop.Loop_Body).all);

            Inner_Iter : constant Optional_Tree :=
                     Lists.Nth_Element (Iterators, 1);
            Inner_Iter_Tree : Iterator.Tree renames Iterator.Tree
                     (Tree_Ptr_Of (Inner_Iter).all);
            Inner_Iter_Name : constant Optional_Tree :=
                     Inner_Iter_Tree.Name;
            Inner_Iter_Name_Tree : Identifier.Tree renames
                     Identifier.Tree (Tree_Ptr_Of (Inner_Iter_Name).all);
            Inner_Iter_Name_Str : constant Strings.U_String :=
                     Inner_Iter_Name_Tree.Str;
            Inner_Iterable_Obj : constant Optional_Tree :=
                     Inner_Iter_Tree.Obj_Value;

            Operand_To_Split : Optional_Tree;

         begin
            --  Depending on kind of inner iterator, we make an
            --  appropriate transformation.
            case Inner_Iter_Tree.Kind is
               when Iterator.Set_Iterator =>
                  --  This is the easy case, since we are iterating over
                  --  a set.  We just need to replace the set with the
                  --  appropriate chunk of the set.
                  Operand_To_Split := Inner_Iter_Tree.Obj_Value;

                  --  Replace Obj_Value with ref to chunk_set_...
                  Inner_Iter_Tree.Obj_Value :=
                    Identifier.Make (Set_Chunk_Loop_Str,
                                     Find_Source_Pos (Operand_To_Split));

               when Iterator.Each_Value
                  | Iterator.Each_Key_Value =>
                  --  Need to extract the index set, split it,
                  --  and then put things back together so the key
                  --  iterates over the proper part of the index set chunk
                  --  and the value is a rename of an indexing into
                  --  the container.
                  --  NOTE: This won't work if it is a for-each iterator
                  --        over a set that doesn't have an "index_set"
                  --        operator (though we could provide one which
                  --        is the identity operator).
                  Operand_To_Split :=
                    Invocation.Make
                      (Kind => Invocation.Operation_Call,
                       Prefix => Identifier.Make
                         ("""index_set""", Find_Source_Pos (Inner_Iter)),
                       Operands => Lists.Make
                               ((1 => Copy_Tree (Inner_Iterable_Obj))));

                  if Is_Null (Inner_Iter_Tree.Key_Name) then
                     --  Make sure the Key has an explicit name
                     Inner_Iter_Tree.Key_Name :=
                       Identifier.Make
                         (Symbols.Generate_Unique_Label
                           (Find_Source_Pos (Inner_Iter), "key_"),
                          Find_Source_Pos (Inner_Iter));
                  end if;

                  --  Turn the inner iterator into a Set_Iterator over the
                  --  loop parameter iterating through the split of the
                  --  original iterable obj, and add the
                  --  definition of the "Value" name to the beginning
                  --  of the body as a ref to an indexing into the
                  --  original iterable obj.
                  Inner_Iter_Tree.Kind := Iterator.Set_Iterator;
                  Inner_Iter_Tree.Name := Inner_Iter_Tree.Key_Name;
                  Inner_Iter_Tree.Key_Name := Null_Optional_Tree;
                  Inner_Iter_Tree.Obj_Value :=
                    Identifier.Make (Set_Chunk_Loop_Str,
                                     Find_Source_Pos (Operand_To_Split));

                  --  Add the "ref" declaration for the "Value" name
                  --  of the [Key => Value] pair.
                  Inner_Loop.Loop_Body := Binary.Make
                    (Binary.Then_Stmt_Op,
                     Left_Operand =>
                       Obj_Decl.Make
                         (Name => Inner_Iter_Name_Tree,
                          Is_Var => False,
                          Is_Const => False,
                          Is_Ref => True,
                          Is_Optional => False,
                          Obj_Type => Null_Optional_Tree,
                          Obj_Value => Invocation.Make
                            (Kind => Invocation.Container_Indexing,
                             Prefix => Inner_Iterable_Obj,
                             Operands => Lists.Make  --  The original Key
                               ((1 => Copy_Tree (Inner_Iter_Tree.Name))))),
                     Right_Operand => Loop_Body);

               when Iterator.Initial_Next_Value
                  | Iterator.Initial_Value =>
                  Messages.Parser_Error
                    ("Iterator in chunked loop must be over a " &
                     "range, array, or container",
                     Find_Source_Pos (Inner_Iter));
            end case;

            --  Add an iterator to the outer loop which iterates over
            --  the Operand_To_Split
            --  NOTE: Rather than using Copy_Tree multiple times,
            --        we make a copy of the Chunk_Spec into a temp and
            --        put the declaration of the temp in the "prologue".
            declare
               Split_Set : constant Optional_Tree :=
                 Invocation.Make
                   (Kind => Invocation.Operation_Call,
                    Prefix => Identifier.Make
                      ("Split", Outer_Loop.Source_Pos),
                    Operands => Lists.Make
                      ((Copy_Tree (Inner_Iterable_Obj),  --  original iterable
                        Property.Make                    --  Chunk-spec'Length
                          (Operand => Copy_Tree (Chunk_Range_Temp),
                           Property_Id =>
                             Identifier.Make
                               ("@Length", Find_Source_Pos (Chunk_Spec)),
                              Source_Pos =>
                                Find_Source_Pos (Chunk_Spec)))));

               Split_Set_Str : constant Strings.U_String :=
                 Symbols.Generate_Unique_Label (Find_Source_Pos (Split_Set),
                   "split_set_");

               Split_Set_Temp : constant Optional_Tree := Identifier.Make
                 (Split_Set_Str, Find_Source_Pos (Split_Set));

               Split_Set_Decl : constant Optional_Tree := Obj_Decl.Make
                 (Name => Identifier.Tree (Tree_Ptr_Of (Split_Set_Temp).all),
                  Is_Var => False,
                  Is_Const => True,
                  Is_Ref => False,
                  Is_Optional => False,
                  Obj_Type => Null_Optional_Tree,
                  Obj_Value => Split_Set);
            begin

               --  Declare the chunk range and the split set
               Outer_Loop.Prologue := Lists.Make
                 ((Chunk_Range_Decl, Split_Set_Decl));

               --  Replace the chunk iterable with the chunk-range temp
               Chunk_Iter.Obj_Value := Chunk_Range_Temp;

               --  Add an iterator over the split set
               Lists.Append (Outer_Loop.Iterators,
                 Iterator.Make
                   (Kind => Iterator.Each_Value,
                    Name => Set_Chunk_Loop_Param,
                    Is_Ref => True,
                    Obj_Type => Null_Optional_Tree,
                    Obj_Value => Copy_Tree (Split_Set_Temp)));

            end;
         end;
      else

         --  No chunking specified, though there might be
         --  an aspect spec.
         --  For now, if Direction is "concurrent", we will
         --  do a "regular" concurrent loop
         --  with a thread per iteration.  Some day we might
         --  have a "default" chunking, or the underlying
         --  LWT scheduler might do something based on the
         --  aspect spec.

         --  Fill in the filter.
         Outer_Loop.Filter := Filtering_Annotation;

         if Direction = Concurrent_Str then
            --  Reset "forward" iterators to null.
            --  so won't complain about specifying direction twice.
            for I in 1 .. Lists.Length (Iterators) loop
               declare
                  Nth_Iter : Iterator.Tree renames
                    Iterator.Tree (Tree_Ptr_Of
                      (Lists.Nth_Element (Iterators, I)).all);
                  use type Strings.U_String;
               begin
                  if Nth_Iter.Direction = Forward_Str then
                     Nth_Iter.Direction := PSC.Strings.Null_U_String;
                  end if;
               end;
            end loop;
         end if;

      end if;

      return Optional (Outer_Loop);
   end Make;

   function Replace_Body
     (Original : Tree; New_Body : Optional_Tree) return Optional_Tree is
   --  Create a For_Loop_Construct identical to Original except for new body.
   begin
      return Optional
               (Tree'(Trees.Tree with
                      Kind => Original.Kind,
                      Prologue => Original.Prologue,
                      Iterators => Original.Iterators,
                      Filter => Original.Filter,
                      Loop_Body => New_Body,
                      Direction => Original.Direction,
                      Chunk_Spec => Original.Chunk_Spec,
                      End_With_Values => Original.End_With_Values,
                      Label => Original.Label,
                      Check_Label => Original.Check_Label));
   end Replace_Body;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return
             Lists.Length (T.Iterators) +
             1 +
             Boolean'Pos (Not_Null (T.End_With_Values)) +
             Boolean'Pos (Not_Null (T.Chunk_Spec)) +
             Lists.Length (T.Prologue);
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      Num_Iters : constant Natural := Lists.Length (T.Iterators);
      pragma Assert (N <= Num_Operands (T));
   begin
      if N <= Num_Iters then
         return Lists.Nth_Element (T.Iterators, N);
      elsif N = Num_Iters + 1 then
         return T.Loop_Body;
      else
         declare
            Adjusted_N : Natural := N - Num_Iters - 1;
         begin
            if Not_Null (T.End_With_Values) then
               Adjusted_N := Adjusted_N - 1;
               if Adjusted_N = 0 then
                  return T.End_With_Values;
               end if;
            end if;
            if Not_Null (T.Chunk_Spec) then
               Adjusted_N := Adjusted_N - 1;
               if Adjusted_N = 0 then
                  return T.Chunk_Spec;
               end if;
            end if;
            return Lists.Nth_Element (T.Prologue, Adjusted_N);
         end;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      Num_Iters : constant Natural := Lists.Length (T.Iterators);
      pragma Assert (N <= Num_Operands (T));
   begin
      if N <= Num_Iters then
         T.Iterators :=
            Lists.Replace_Nth_Element (T.Iterators, N, New_Operand);
      elsif N = Num_Iters + 1 then
         T.Loop_Body := New_Operand;
      else
         declare
            Adjusted_N : Natural := N - Num_Iters - 1;
         begin
            if Not_Null (T.End_With_Values) then
               Adjusted_N := Adjusted_N - 1;
               if Adjusted_N = 0 then
                  T.End_With_Values := New_Operand;
               end if;
            end if;
            if Not_Null (T.Chunk_Spec) then
               Adjusted_N := Adjusted_N - 1;
               if Adjusted_N = 0 then
                  T.Chunk_Spec := New_Operand;
               end if;
            end if;
            T.Prologue :=
              Lists.Replace_Nth_Element (T.Prologue, Adjusted_N, New_Operand);
         end;
      end if;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return For_Loop_Kind;
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
      use PSC.Trees.Iterator;
      use type PSC.Strings.U_String;

      function Each_If_Needed (Iter : Optional_Tree) return String is
      --  Return "each" if Iter is a For-Each style of iterator and
      --  for-loop is not a quantified expr.
      begin
         if T.Kind in Quantified_Expr_Kinds then
            return "";
         else
            case Iterator.Tree (Tree_Of (Iter)).Kind is
               when Each_Value | Each_Key_Value =>
                  return "each ";
               when others =>
                  return "";
            end case;
         end if;
      end Each_If_Needed;

   begin  --  Display_Subtree
      case T.Kind is
         when Univ_Quantified_Expr | Existential_Quantified_Expr |
           Map_Reduce_Expr =>
            --  Quantified expression or map-reduce expression
            pragma Assert (Is_Null (T.Label));
            pragma Assert (Is_Null (T.End_With_Values));

            Put (On, "(for ");
            case T.Kind is
               when Univ_Quantified_Expr =>
                  Put (On, "all ");
               when Existential_Quantified_Expr =>
                  Put (On, "some ");
               when Map_Reduce_Expr => null;
                  if Lists.Length (T.Iterators) = 1 then
                     Put (On, Each_If_Needed
                       (Lists.Nth_Element (T.Iterators, 1)));
                  end if;
               when others =>
                  pragma Assert (False); null;
            end case;
            if Lists.Length (T.Iterators) = 1 then
               Display_Subtree (Lists.Nth_Element (T.Iterators, 1), On);
            else
               Put (On, "(");
               for I in 1 .. Lists.Length (T.Iterators) loop
                  declare
                     Iter : constant Optional_Tree :=
                       Lists.Nth_Element (T.Iterators, I);
                  begin
                     if I > 1 then
                        Put (On, "; ");
                     end if;
                     Put (On, Each_If_Needed (Iter));
                     Display_Subtree (Iter, On);
                  end;
               end loop;
               Put (On, ")");
            end if;
            if Not_Null (T.Filter) then
               Display_Subtree (T.Filter, On);
            end if;
            if T.Direction /= Strings.Null_U_String then
               Put (On, ' ');
               Put (On, Strings.To_String (T.Direction));
            end if;
            Put (On, " => ");
            if Use_Short_Form then
               Put (On, "... ");
            else
               if Indent > 0 then
                  New_Line (On, Indent => Indent + 2);
               end if;
               Display_Subtree (T.Loop_Body, On);
            end if;
            Put (On, ")");

         when Container_Comprehension =>
            --  [for I ... => expr]
            declare
               Loop_Body_Tree : Trees.Tree'Class renames
                 Tree_Ptr_Of (T.Loop_Body).all;
               Iter : constant Optional_Tree :=
                 Lists.Nth_Element (T.Iterators, 1);
            begin
               pragma Assert (Is_Null (T.Label));
               pragma Assert (Is_Null (T.End_With_Values));
               pragma Assert (Lists.Length (T.Iterators) = 1);

               Put (On, "for ");
               Put (On, Each_If_Needed (Iter));
               Display_Subtree (Iter, On);
               if Not_Null (T.Filter) then
                  Display_Subtree (T.Filter, On);
               end if;
               if T.Direction /= Strings.Null_U_String then
                  Put (On, ' ');
                  Put (On, Strings.To_String (T.Direction));
               end if;

               if Loop_Body_Tree in Reference.Tree then
                  --  A key was specified to use instead of loop parameter
                  --  So display, e.g. "for I in 1..10, I*2 => I**2"
                  Put (On, ", ");
               else
                  Put (On, " => ");
               end if;
               if Use_Short_Form then
                  Put (On, "... ");
               else
                  if Indent > 0 then
                     New_Line (On, Indent => Indent + 2);
                  end if;
                  Display_Subtree (T.Loop_Body, On);
               end if;
            end;

         when For_Loop_Statement =>
            --  For-Loop Statement
            if not Lists.Is_Empty (T.Prologue) then
               --  These are declarations and statements that precede the loop
               Lists.Display_List
                 (T.Prologue,
                  On,
                  Indent => Indent,
                  Separator => ";",
                  Terminator => ";");
            end if;
            if Not_Null (T.Label) then
               case Languages.Language is
                  when Languages.ParaSail | Languages.Parython =>
                     --  Put out label at indent-1 after a blank line
                     New_Line (On, Indent => Indent - 1);
                     Put (On, '*');
                     Display_Subtree (T.Label, On);
                     Put_Line (On, "*");
                  when Languages.Ada_Ish | Languages.Javallel =>
                     --  Put out label at same indent after a blank line
                     New_Line (On, Indent => Indent);
                     Display_Subtree (T.Label, On);
                     Put_Line (On, ":");
               end case;
            end if;

            if Languages.Language in Languages.Ada_Ish
              and then T.Direction = Concurrent_Str
            then
               Put_Indent (On, Indent);
               Put_Line (On, "parallel");  --  TBD: Aspect clause
            end if;

            Put_Indent (On, Indent);
            Put (On, "for ");
            if Lists.Length (T.Iterators) > 1 then
               --  We have multiple iterators, surround in "()"
               Put (On, "(");
               Lists.Display_List
                 (T.Iterators,
                  On,
                  Separator => ";",
                  Indent => Indent + 5);
               --  TBD: "each" won't be printed out where appropriate
               Put (On, ")");
            else
               --  Just one iterator, check for need to display "each"
               declare
                  Single_Iterator : constant Optional_Tree :=
                    Lists.Nth_Element (T.Iterators, 1);
               begin
                  Put (On, Each_If_Needed (Single_Iterator));
                  Display_Subtree (Single_Iterator, On);
               end;
            end if;
            if Not_Null (T.Filter) then
               Put (On, ' ');
               Display_Subtree (T.Filter, On);
            end if;
            if T.Direction /= Strings.Null_U_String
              and then
                (Languages.Language not in Languages.Ada_Ish
                   or else T.Direction = Reverse_Str)
            then
               Put (On, ' ');
               Put (On, Strings.To_String (T.Direction));
            end if;
            if Use_Short_Form then
               Put_Line (On, " loop ...");
               return;
            end if;

            Put_Line (On, " loop");
            Display_Subtree (T.Loop_Body, On, Indent => Indent + 4);
            New_Line (On, Indent => Indent);
            Put (On, "end loop");
            if Not_Null (T.Label) then
               Put (On, ' ');
               Display_Subtree (T.Label, On);
            end if;
            if Not_Null (T.End_With_Values) then
               New_Line (On, Indent => Indent + 2);
               Put (On, "with ");
               Display_Subtree (T.End_With_Values, On);
            end if;
      end case;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      For_Loop_Construct_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      For_Loop_Construct_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         if Lists.Length (T.Iterators) > 0 then
            T_Source_Pos :=
               Find_Source_Pos (Lists.Nth_Element (T.Iterators, 1));
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Loop_Body);
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Label);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.For_Loop_Construct;
