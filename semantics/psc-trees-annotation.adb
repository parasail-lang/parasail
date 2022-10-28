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
package body PSC.Trees.Annotation is

   function Make (Annotations : Lists.List;
     Label : Optional_Tree := Null_Optional_Tree) return Optional_Tree is
   --  Build up an Annotation tree.
   begin
      if not Lists.Is_Empty (Annotations) or else
        Not_Null (Label) then
         return Optional (Tree'(Trees.Tree with Annotations => Annotations,
           Label => Label));
      else
         --  Empty annotations => ignore
         return Null_Optional_Tree;
      end if;
   end Make;

   procedure Add_Annotation
     (OT : Optional_Tree;
      Annotations : Lists.List;
      Label : Optional_Tree := Null_Optional_Tree;
      Precedes : Boolean := False) is
      --  Add annotation to tree
      Assoc_Tree : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
   begin
      if Lists.Is_Empty (Annotations) and then
        Is_Null (Label) then
         return;
      elsif Precedes then
         if Not_Null (Assoc_Tree.Pre_Annotation) then
            --  Already has a pre-annotation, add this one to that
            Add_Annotation
              (Assoc_Tree.Pre_Annotation, Annotations, Label, Precedes);
         else
            --  Insert annotation
            Assoc_Tree.Pre_Annotation := Make (Annotations, Label => Label);
         end if;
      else
         if Not_Null (Assoc_Tree.Post_Annotation) then
            --  Already has a post-annotation, add this one to that
            Add_Annotation
              (Assoc_Tree.Post_Annotation,
               Annotations,
               Label,
               Precedes);
         else
            --  Insert annotation
            Assoc_Tree.Post_Annotation := Make (Annotations, Label => Label);
         end if;
      end if;
   end Add_Annotation;

   procedure Add_Annotations
     (OT : Optional_Tree;
      From : Trees.Tree'Class)
   --  Copy annotations from "From" onto OT
   is
      To_Tree : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;

      --  Should not already have any annotations
      pragma Assert (Is_Null (To_Tree.Pre_Annotation) and then
        Is_Null (To_Tree.Post_Annotation));
   begin
      To_Tree.Pre_Annotation := From.Pre_Annotation;
      To_Tree.Post_Annotation := From.Post_Annotation;
   end Add_Annotations;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return Lists.Length (T.Annotations);
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N <= Lists.Length (T.Annotations));
   begin
      return Lists.Nth_Element (T.Annotations, N);
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= Lists.Length (T.Annotations));
   begin
      T.Annotations :=
         Lists.Replace_Nth_Element (T.Annotations, N, New_Operand);
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
      Indent_For_Annotations : Natural := 0;
      Is_First : Boolean := True;
      Inside_Braces : Boolean := False;
      Separator : constant String := "; ";
      Need_Separator : Boolean := False;
   begin
      if Indent > 0 then
         Indent_For_Annotations := Indent + 1;
      end if;

      if Not_Null (T.Label) then
         --  We have a labeled annotation list, put out the label now
         if Indent = 0 then
            Put (On, ' ');
         else
            Put_Indent (On, Indent => Indent);
         end if;
         Put (On, "{> *" & Subtree_Image (T.Label) & "* ");
         if Indent > 0 then
            PSC.Stream_Output.New_Line
              (On, Indent => Indent_For_Annotations);
         end if;
         Inside_Braces := True;
      end if;

      for I in 1 .. Lists.Length (T.Annotations) loop
         declare
            Elem : constant Optional_Tree :=
              Lists.Nth_Element (T.Annotations, I);
            Elem_Tree : Trees.Tree'Class renames
              Tree_Ptr_Of (Elem).all;
         begin
            if Is_Null (T.Label) and then
              Elem_Tree in Trees.Annotation.Tree and then
              Not_Null (Trees.Annotation.Tree (Elem_Tree).Label) then
               --  We have a nested, labeled annotation
               if Inside_Braces then
                  --  We need to finish current list
                  Put (On, " <}");
                  if Indent > 0 then
                     New_Line (On);
                  end if;
                  Inside_Braces := False;
               end if;
               --  Recurse with this annotation
               Display_Subtree (Elem_Tree, On, Indent);

            else
               if not Inside_Braces then
                  if Indent = 0 then
                     Put (On, ' ');
                  else
                     Put_Indent (On, Indent => Indent);
                  end if;
                  Put (On, "{> ");
                  Inside_Braces := True;
                  Need_Separator := False;
               end if;

               if Need_Separator then
                  PSC.Stream_Output.Put (On, Separator);
                  if Indent > 0 then
                     --  If Indent > 0 then each item goes on its own line
                     PSC.Stream_Output.New_Line
                       (On, Indent => Indent_For_Annotations);
                  end if;
               end if;

               Display_Subtree (Elem, On);
               Need_Separator := True;
            end if;
         end;
      end loop;

      if Inside_Braces then
         Put (On, " <}");
         if Indent > 0 then
            New_Line (On);
         end if;
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Annotation_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Annotation_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         if Not_Null (T.Label) then
            T_Source_Pos := Find_Source_Pos (T.Label);
            if T_Source_Pos /= Null_Source_Position then
               return T_Source_Pos;
            end if;
         end if;

         if not Lists.Is_Empty (T.Annotations) then
            T_Source_Pos :=
               Find_Source_Pos (Lists.Nth_Element (T.Annotations, 1));
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Annotation;
