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
with PSC.Strings;
with PSC.Trees.Property;
with PSC.Trees.Visitor;
package body PSC.Trees.Obj_Decl is
   --  Representation for a declaration of an object

   function Make
     (Name : Identifier.Tree;
      Is_Var : Boolean;
      Is_Const : Boolean;
      Is_Ref : Boolean;
      Is_Optional : Boolean;
      Obj_Type : Optional_Tree;
      Obj_Value : Optional_Tree;
      In_Region : Optional_Tree := Null_Optional_Tree;
      Is_Global : Boolean := False;
      Is_Move : Boolean := False)
      return Optional_Tree
   is
   --  Build up an object declaration
      Obj_Value_To_Use : Optional_Tree := Obj_Value;
      Now_Is_Move : Boolean := Is_Move;
   begin
      --  Make sure the flags make sense
      if Is_Null (Obj_Type) then
         --  "optional" requires an explicit type
         pragma Assert (not Is_Optional);
         null;
      end if;

      pragma Assert (not (Is_Var and Is_Const));
      pragma Assert (not (Is_Ref and Is_Move));
      pragma Assert (not (Is_Ref and then Not_Null (In_Region)));
      pragma Assert (not Is_Global or Is_Var);

      --  Check for use of 'Move on Obj_Value
      if not Is_Move and then Not_Null (Obj_Value) then
         declare
            Val_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Obj_Value).all;
         begin
            --  Check to see if we have a 'Move
            if Val_Tree in Property.Tree then
               declare
                  Val_Prop_Id : Identifier.Tree renames Identifier.Tree
                    (Tree_Ptr_Of (Property.Tree (Val_Tree).Property_Id).all);
                  Val_Prop_Str : String renames
                    Strings.To_String (Val_Prop_Id.Str);
               begin
                  if Val_Prop_Str = "@Move" then
                     --  Create a "Move" from the Prefix of the 'Move
                     --  Only used for Ada202X so far, but might be used
                     --  for other languages some day.
                     Obj_Value_To_Use := Property.Tree (Val_Tree).Operand;
                     Now_Is_Move := True;
                  end if;
               end;
            end if;
         end;
      end if;

      --  Create the declaration
      return Optional
               (Tree'(Trees.Tree with
                      Name => Name,
                      Is_Var => Is_Var,
                      Is_Const => Is_Const,
                      Is_Ref => Is_Ref,
                      Is_Optional => Is_Optional,
                      In_Region => In_Region,
                      Is_Global => Is_Global,
                      Obj_Type => Obj_Type,
                      Is_Move => Now_Is_Move,
                      Obj_Value => Obj_Value_To_Use));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      case N is
         when 1 =>
            return T.Obj_Type;
         when 2 =>
            return T.Obj_Value;
         when others =>
            pragma Assert (False);  --  N out of range
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
            T.Obj_Type := New_Operand;
         when 2 =>
            T.Obj_Value := New_Operand;
         when others =>
            pragma Assert (False);  --  N out of range
            null;
      end case;
      return;
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
   begin
      --  Display object declaration at given indent
      Put_Indent (On, Indent);
      if T.Is_Global then
         Put (On, "global ");
      end if;
      if T.Is_Ref then
         Put (On, "ref ");
      end if;
      if T.Is_Var then
         Put (On, "var ");
      elsif T.Is_Const then
         Put (On, "const ");
      end if;

      Identifier.Display_Subtree (T.Name, On);

      if Not_Null (T.In_Region) then
         Put (On, " for ");
         Display_Subtree (T.In_Region, On);
      end if;

      if Not_Null (T.Obj_Type) then
         Put (On, " : ");
         if T.Is_Optional then
            Put (On, "optional ");
         end if;
         Display_Subtree (T.Obj_Type, On);
      end if;

      if Not_Null (T.Obj_Value) then
         if T.Is_Ref then
            Put (On, " => ");
         elsif T.Is_Move then
            Put (On, " <== ");
         else
            Put (On, " := ");
         end if;
         Display_Subtree (T.Obj_Value, On);
      end if;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Obj_Decl_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Obj_Decl_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Identifier.Find_Source_Pos (T.Name);
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Obj_Decl;
