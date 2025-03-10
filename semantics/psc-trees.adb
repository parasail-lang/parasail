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
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with PSC.Stream_Output;
with PSC.Strings;
with PSC.String_Streams;
package body PSC.Trees is

   procedure Root_Sem_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Root_Sem_Ptr) is
   --  Write out nothing
      pragma Unreferenced (Stream);
      pragma Unreferenced (Item);
   begin
      null;
   end Root_Sem_Ptr_Write;

   procedure Root_Sem_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Root_Sem_Ptr) is
   --  Read in nothing, set Item to null
      pragma Unreferenced (Stream);
   begin
      Item := null;
   end Root_Sem_Ptr_Read;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 0;  --  Default is no operands
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (False);  --  Default is no operands
      return Null_Optional_Tree;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   --  NOTE: Cannot declare as a "No_Return" procedure because
   --        then any overridings would similarly have to be No_Return.
   --        That makes it hard to provide an exception-raising "default."
   --        Could declare as "abstract" but then overriding would be required
   --        which would again defeat the desire to provide a default.
   begin
      pragma Assert (False);  --  Default is no operands
      null;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      pragma Assert (False);
      return Annotation_Kind;
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

      New_Tree : Tree'Class := Tree'Class (T);  --  Copy the original tree
      pragma Assert (New_Operands'Length = Num_Operands (New_Tree));
   --  We apply Num_Operands to New_Tree to ensure it is dispatching
   begin
      --  Set each of the operands and then return the result.
      for I in New_Operands'Range loop
         Set_Nth_Operand
           (New_Tree,
            I - New_Operands'First + 1,
            New_Operands (I));  --  This is a dispatching call
      end loop;
      --  Return a new tree
      return Optional (New_Tree);
   end Substitute_Operands;

   procedure Display_Subtree
     (OT : Optional_Tree;
      On : access Ada.Streams.Root_Stream_Type'Class :=
         Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
   --  Produce a human readable display of a subtree, at the given indent
   --  If Use_Short_Form is True, then elide some of the output for
   --  a module or an operation.
   begin
      if OT.Ptr /= null then
         --  Just pass the buck to the dispatching operation
         declare
            Pre_Annot : Optional_Tree := OT.Ptr.Pre_Annotation;
            Post_Annot : Optional_Tree := OT.Ptr.Post_Annotation;
         begin
            if not Use_Short_Form then
               --  Display the pre-annotations
               while Not_Null (Pre_Annot) loop
                  Display_Subtree
                    (Pre_Annot.Ptr.all,
                     On,
                     Indent,
                     Use_Short_Form);
                  Pre_Annot := Pre_Annot.Ptr.Pre_Annotation;
                  if Indent = 0 then
                     PSC.Stream_Output.Put (On, ' ');
                  end if;
               end loop;
            end if;

            Display_Subtree (OT.Ptr.all, On, Indent, Use_Short_Form);

            if not Use_Short_Form then
               --  Display the post-annotations
               while Not_Null (Post_Annot) loop
                  Display_Subtree
                    (Post_Annot.Ptr.all,
                     On,
                     Indent,
                     Use_Short_Form);
                  Post_Annot := Post_Annot.Ptr.Post_Annotation;
               end loop;
            end if;
         end;
      end if;
   end Display_Subtree;

   procedure Dump_Subtree (OT : Optional_Tree) is
   --  Produce a human readable display of a subtree given an optional tree
   --  on the standard output stream.
   begin
      Ada.Text_IO.New_Line (2);
      Display_Subtree
        (OT,
         On => Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output),
         Indent => 0);
      Ada.Text_IO.Put_Line (";");
   end Dump_Subtree;

   function Subtree_Image
     (T : Tree'Class;
      Use_Short_Form : Boolean := False;
      Max_Chars : Positive := 2000)
      return String
   is
      --  Return a text image of the subtree (up to Max_Chars in length)
      use PSC.String_Streams;
      Str : aliased String_Stream
        (Ada.Streams.Stream_Element_Offset (Max_Chars * Character'Size /
                                            Ada.Streams.Stream_Element'Size));
   begin
      --  Put image into the stream
      Display_Subtree
        (T,
         On => Str'Access,
         Indent => 0,
         Use_Short_Form => Use_Short_Form);
      --  Get stream as a string
      return String_Of (Str'Access);
   end Subtree_Image;

   function Subtree_Image
     (OT : Optional_Tree;
      Use_Short_Form : Boolean := False;
      Max_Chars : Positive := 2000)
      return String
   is
   --  Return a text image of the subtree (up to Max_Chars in length)
   begin
      if Is_Null (OT) then
         return "";
      else
         return Subtree_Image (OT.Ptr.all, Use_Short_Form, Max_Chars);
      end if;
   end Subtree_Image;

   procedure Pre_Visit
     (Visitor : in out Root_RO_Tree_Visitor;
      T : Tree'Class) is
   --  A pre-visit routine for RO visitors
   begin
      null;
   end Pre_Visit;

   procedure Post_Visit
     (Visitor : in out Root_RO_Tree_Visitor;
      T : Tree'Class) is
   --  A post-visit routine for RO visitors
   begin
      null;
   end Post_Visit;

   procedure Visit
     (OT : Optional_Tree;
      Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate *_Action procedure on Visitor
   --  given optional tree.
   begin
      if OT.Ptr /= null then
         --  Just pass the buck to the dispatching operations
         Pre_Visit (Visitor, OT.Ptr.all);
         Visit (OT.Ptr.all, Visitor);
         Post_Visit (Visitor, OT.Ptr.all);
      end if;
   end Visit;

   procedure Pre_Visit
     (Visitor : in out Root_RW_Tree_Visitor;
      T : in out Tree'Class) is
   --  A pre-visit routine for RW visitors
   begin
      null;
   end Pre_Visit;

   procedure Post_Visit
     (Visitor : in out Root_RW_Tree_Visitor;
      T : in out Tree'Class) is
   --  A post-visit routine for RW visitors
   begin
      null;
   end Post_Visit;

   procedure Visit
     (OT : in out Optional_Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   --  given optional tree.
   begin
      if OT.Ptr /= null then
         --  Just pass the buck to the dispatching operations
         Pre_Visit (Visitor, OT.Ptr.all);
         Visit (OT.Ptr.all, Visitor);
         Post_Visit (Visitor, OT.Ptr.all);
      end if;
   end Visit;

   procedure Free is new Ada.Unchecked_Deallocation (Tree'Class, Tree_Ptr);

   procedure Assign (Left : in out Optional_Tree; Right : Tree'Class) is
      --  Copy Right into given "optional" tree.
      --  Reuse existing space if possible.
      use Ada.Tags;
   begin
      if Left.Ptr /= null then
         if Left.Ptr'Tag = Right'Tag then
            Left.Ptr.all := Right;
         else
            Free (Left.Ptr);
         end if;
      end if;
      Left.Ptr := new Tree'Class'(Right);
   end Assign;

   function Optional (T : Tree'Class) return Optional_Tree is
   --  Wrap a tree up as an optional tree so can be stored
   --  as an operand.
   begin
      return Optional_Tree'(Ptr => new Tree'Class'(T));
   end Optional;

   function Optional (T : access Tree'Class) return Optional_Tree is
   --  Wrap up a preexisting tree so can be stored as an
   --  an operand.  This is a reference to, not a copy of, the original tree.
   begin
      return Optional_Tree'(Ptr => T.all'Unchecked_Access);
   end Optional;

   function Is_Null (OT : Optional_Tree) return Boolean is
   --  Return True if OT is empty
   begin
      return OT.Ptr = null;
   end Is_Null;

   function Not_Null (OT : Optional_Tree) return Boolean is
   --  Return True if OT is not empty
   begin
      return OT.Ptr /= null;
   end Not_Null;

   procedure Set_Source_Pos
     (OT : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position) is
   --  Fill in the source position information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).
   begin
      if OT.Ptr /= null then
         OT.Ptr.Source_Pos := Source_Pos;
      end if;
   end Set_Source_Pos;

   procedure Set_End_Source_Pos
     (OT : Optional_Tree;
      End_Source_Pos : Source_Positions.Source_Position) is
   --  Fill in the "end" source position information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).
      use type Strings.U_String_Index;
      use type Source_Positions.Line_Number;
   begin
      if OT.Ptr /= null then
         --  Fill in End_Line/End_Col
         if End_Source_Pos.End_Line > End_Source_Pos.Line then
            --  Take the "end" information
            OT.Ptr.Source_Pos.End_Line := End_Source_Pos.End_Line;
            OT.Ptr.Source_Pos.End_Col := End_Source_Pos.End_Col;
         else
            --  Copy the "start" information
            OT.Ptr.Source_Pos.End_Line := End_Source_Pos.Line;
            OT.Ptr.Source_Pos.End_Col := End_Source_Pos.Col;
         end if;
         if OT.Ptr.Source_Pos.File = Strings.Null_U_String_Index then
            --  Fill in the File as well
            OT.Ptr.Source_Pos.File := End_Source_Pos.File;
         end if;
      end if;
   end Set_End_Source_Pos;

   function Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position
   is
   --  Return source position information from given tree.
   --  Return Null_Source_Position if Is_Null(OT).
   begin
      if OT.Ptr /= null then
         return OT.Ptr.Source_Pos;
      else
         return Source_Positions.Null_Source_Position;
      end if;
   end Source_Pos;

   function Find_Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position
   is
      --  Walk into tree to try to find a meaningful source position
      use type Source_Positions.Source_Position;
   begin
      if OT.Ptr = null then
         return Source_Positions.Null_Source_Position;
      elsif OT.Ptr.Source_Pos /= Source_Positions.Null_Source_Position then
         return OT.Ptr.Source_Pos;
      else
         --  Dispatch to try to find source position
         OT.Ptr.Source_Pos := Find_Source_Pos (OT.Ptr.all);
         return OT.Ptr.Source_Pos;
      end if;
   end Find_Source_Pos;

   function Find_End_Source_Pos
     (OT : Optional_Tree)
      return Source_Positions.Source_Position is
   --  Walk into tree to try to find a source position
   --  with the largest End_Line.
      use Source_Positions;
   begin
      if OT.Ptr = null then
         return Null_Source_Position;
      end if;

      declare
         Overall : Source_Position := Find_Source_Pos (OT);
      begin
         if Overall.End_Line < Overall.Line then
            --  Use start as end
            Overall.End_Line := Overall.Line;
            Overall.End_Col  := Overall.Col;
         else
            --  Use end as start
            Overall.Line := Overall.End_Line;
            Overall.Col  := Overall.End_Col;
         end if;

         if Num_Operands (OT.Ptr.all) > 0 then
            --  Recurse to find source position of last operand with one.
            for I in reverse 1 .. Num_Operands (OT.Ptr.all) loop
               declare
                  Result : constant Source_Positions.Source_Position :=
                    Find_End_Source_Pos (Nth_Operand (OT.Ptr.all, I));
               begin
                  if Result.End_Line > Overall.End_Line then
                     return Result;
                  end if;
               end;
            end loop;
         end if;
         --  Fall back on source-pos of tree as a whole, with End_Line = Line
         return Overall;
      end;
   end Find_End_Source_Pos;

   procedure Set_Sem_Info (OT : Optional_Tree; Sem_Info : Root_Sem_Ptr) is
   --  Fill in the semantic information in the given tree,
   --  if Not_Null(OT).  Has no effect if Is_Null(OT).
   begin
      if OT.Ptr /= null then
         OT.Ptr.Sem_Info := Sem_Info;
      end if;
   end Set_Sem_Info;

   function Sem_Info (OT : Optional_Tree) return Root_Sem_Ptr is
   --  Return semantic information from given tree.
   --  Return null if Is_Null(OT).
   begin
      if OT.Ptr /= null then
         return OT.Ptr.Sem_Info;
      else
         return null;
      end if;
   end Sem_Info;

   procedure Apply_RW (To : in out Optional_Tree) is
   --  Call Action on contained Tree, if any, with read/write access.
   begin
      if To.Ptr /= null then
         Action (To.Ptr.all);
      end if;
   end Apply_RW;

   procedure Apply (To : Optional_Tree) is
   --  Call Action on contained Tree, if any.
   begin
      if To.Ptr /= null then
         Action (To.Ptr.all);
      end if;
   end Apply;

   procedure Apply_To_Subtree (To : Optional_Tree) is
   --  Call Pre/Post_Action on entire subtree given pointer to top.
   begin
      if To.Ptr /= null then
         Pre_Action (To.Ptr.all);
         for I in 1 .. Num_Operands (To.Ptr.all) loop
            Apply_To_Subtree (Nth_Operand (To.Ptr.all, I));  --  Recurse!
         end loop;
         Post_Action (To.Ptr.all);
      end if;
   end Apply_To_Subtree;

   procedure Apply_To_Subtree_RW (To : in out Optional_Tree) is
   --  Call RW Pre/Post_Action on entire subtree given pointer to top.
   begin
      if To.Ptr /= null then
         Pre_Action (To.Ptr.all);
         for I in 1 .. Num_Operands (To.Ptr.all) loop
            --  Apply to each subtree
            declare
               Operand : Optional_Tree := Nth_Operand (To.Ptr.all, I);
            --  Get a variable so as to allow calling routine
            --  which requires one (cheating here!)
            begin
               Apply_To_Subtree_RW (Operand);  --  Recurse!
            end;
         end loop;
         Post_Action (To.Ptr.all);
      end if;
   end Apply_To_Subtree_RW;

   procedure Apply_To_Nth_Operand (T : Tree'Class; N : Positive) is
      --  Call Action with Nth operand of given tree, if any.
      --  Requires: Num_Operands(T) >= N
      pragma Assert (Num_Operands (T) >= N);
      Operand : constant Optional_Tree := Nth_Operand (T, N);
   begin
      if Operand.Ptr /= null then
         Action (Operand.Ptr.all);
      end if;
   end Apply_To_Nth_Operand;

   procedure Apply_To_Nth_Operand_RW
     (T : in out Tree'Class;
      N : Positive) is
      --  Call RW Action with Nth operand of given tree, if any.
      --  Requires: Num_Operands(T) >= N
      pragma Assert (Num_Operands (T) >= N);
      Operand : Optional_Tree := Nth_Operand (T, N);
   --  Make this a variable to allow calling the RW action
   begin
      if Operand.Ptr /= null then
         Action (Operand.Ptr.all);
      end if;
   end Apply_To_Nth_Operand_RW;

   function Tree_Of (OT : Optional_Tree) return Tree'Class is
      --  Get the tree from an optional tree.
      --  Requires Not_Null(OT);
      pragma Assert (Not_Null (OT));
   begin
      return OT.Ptr.all;
   end Tree_Of;

   function Tree_Ptr_Of (OT : Optional_Tree) return Tree_Ptr is
      --  Get a pointer to the underlying tree from an optional tree.
      --  Requires Not_Null(OT);
      pragma Assert (Not_Null (OT));
   begin
      return OT.Ptr;
   end Tree_Ptr_Of;

   function Sem_Image
     (Sem : access Root_Semantic_Info;
      Use_Short_Form : Boolean := False)
      return String is
   --  Dispatching op to return image of entity identified by semantic info
   begin
      return "";
   end Sem_Image;

   function Copy_Tree
     (OT : Optional_Tree)
      return Optional_Tree is
   --  Make a deep copy of given tree, prior to any semantic info
   --  having been added, to use for cases where we have
   --  multiple identifiers in a declaration and the intent
   --  is for it to be equivalent to a sequence of separate
   --  declarations.
   begin
      if OT.Ptr = null then
         --  A null tree
         return OT;
      end if;

      declare
         Copied_OT : Optional_Tree := Optional (OT.Ptr.all);
         Copied_Tree : Tree'Class renames Copied_OT.Ptr.all;
      begin
         for I in 1 .. Num_Operands (Copied_Tree) loop
            Set_Nth_Operand (Copied_Tree, I,
                             Copy_Tree (Nth_Operand (Copied_Tree, I)));
         end loop;
         return Copied_OT;
      end;
   end Copy_Tree;

end PSC.Trees;
