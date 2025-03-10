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
with PSC.Languages;
with PSC.Strings;
package body PSC.Trees.Property is

   function Make
     (Operand : Optional_Tree;
      Property_Id : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree
   is
      --  Build up a Property tree.
      pragma Assert (Not_Null (Operand));
      pragma Assert (Not_Null (Property_Id));

      Result : constant Optional_Tree :=
        Optional (Tree'(Trees.Tree with
                        Operand => Operand, Property_Id => Property_Id));
      use type Strings.U_String_Index;
   begin
      if Source_Pos.File /= Strings.Null_U_String_Index then
         --  Fill in the source position information
         Set_Source_Pos (Result, Source_Pos);
      end if;
      return Result;
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N <= 2);
   begin
      case N is
         when 1 =>
            return T.Operand;
         when 2 =>
            return T.Property_Id;
         when others =>
            pragma Assert (False);
            return Null_Optional_Tree;
      end case;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= 2);
   begin
      case N is
         when 1 =>
            T.Operand := New_Operand;
         when 2 =>
            T.Property_Id := New_Operand;
         when others =>
            pragma Assert (False);
            return;
      end case;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Property_Kind;
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
   begin
      case Languages.Language is
         when Languages.Ada_Ish =>
            --  We use properties for Ada attributes
            Display_Subtree (T.Operand, On, Indent => Indent);
            Put (On, "'");
            --  Strip off a leading '@' if any.
            declare
               Prop_Image : constant String := Subtree_Image (T.Property_Id);
            begin
               if Prop_Image'Length > 0
                 and then Prop_Image (Prop_Image'First) = '@'
               then
                  Put (On,
                       Prop_Image (Prop_Image'First + 1 .. Prop_Image'Last));
               else
                  Put (On, Prop_Image);
               end if;
            end;

         when Languages.ParaSail =>
            --  Parasail uses syntax of <entity>#<property> but
            --  we substitute '@' for '#'
            Display_Subtree (T.Operand, On, Indent => Indent);
            Put (On, "#");
            --  Strip off a leading '@' if any.
            declare
               Prop_Image : constant String := Subtree_Image (T.Property_Id);
            begin
               if Prop_Image'Length > 0
                 and then Prop_Image (Prop_Image'First) = '@'
               then
                  Put (On,
                       Prop_Image (Prop_Image'First + 1 .. Prop_Image'Last));
               else
                  Put (On, Prop_Image);
               end if;
            end;

         when Languages.Parython | Languages.Javallel =>
            --  Not really used yet
            Display_Subtree (T.Operand, On, Indent => Indent);
            Put (On, ".");
            Display_Subtree (T.Property_Id, On);
      end case;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Property_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Property_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Property_Id);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Operand);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Property;
