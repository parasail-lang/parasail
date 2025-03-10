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

with Ada.Text_IO; use Ada.Text_IO;

with PSC.Stream_Output;
with PSC.Symbols;
with PSC.Trees.Binary;
with PSC.Trees.Identifier;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Property;
with PSC.Trees.Visitor;
with PSC.Strings; pragma Elaborate (PSC.Strings);
package body PSC.Trees.Assign_Stmt is

   Target_Name_Str : constant Strings.U_String := Strings.String_Lookup ("@");
   --  This is only used in Ada for now.

   function Contains_Target_Name (RHS : Optional_Tree) return Boolean;
   --  Return True if the target name symbol ('@') appears within RHS

   function Replace_Target_Name (RHS : Optional_Tree;
                                 LHS_Ren_Str : Strings.U_String)
     return Optional_Tree;
   --  Return RHS with all appearances of the target name symbol '@'
   --  replaced with an identifier with id LHS_Ren_Str.

   function Make
     (Assign_Operator : Assign_Operator_Enum;
      LHS : Optional_Tree;
      RHS : Optional_Tree)
      return Optional_Tree
   is
   --  Build up an Assign_Stmt tree.
   begin
      if Languages.Language in Languages.Ada_Ish
        and then Contains_Target_Name (RHS)
      then
         --  Put_Line ("Found a target name in " & Subtree_Image (RHS));
         --  Replace LHS and all target name appearances with rename of LHS
         declare
            LHS_Ren_Str : constant Strings.U_String :=
                (Symbols.Generate_Unique_Label
                   (Find_Source_Pos (LHS), "target_name_"));
            LHS_Ren : constant Optional_Tree :=
              Identifier.Make (LHS_Ren_Str, Find_Source_Pos (LHS));
            Updated_RHS : constant Optional_Tree :=
              Replace_Target_Name (RHS, LHS_Ren_Str);
         begin
            --  Use "then" operator to connect renaming
            --  to assignment.
            return Binary.Make (Binary.Then_Stmt_Op,
                     Left_Operand => Obj_Decl.Make
                        (Name =>
                           Identifier.Tree (Tree_Ptr_Of (LHS_Ren).all),
                         Is_Var => False,
                         Is_Const => False,
                         Is_Ref => True,
                         Is_Optional => False,
                         Obj_Type => Null_Optional_Tree,
                         Obj_Value => LHS),
                     Right_Operand => Assign_Stmt.Make (Assign_Operator,
                         LHS => Copy_Tree (LHS_Ren),
                         RHS => Updated_RHS),
                     Source_Pos => Find_Source_Pos (LHS));
         end;
      end if;

      --  Check for use of 'Move on RHS
      declare
         RHS_Tree : Trees.Tree'Class renames Tree_Ptr_Of (RHS).all;
      begin
         --  Check to see if we have a 'Move
         if RHS_Tree in Property.Tree then
            declare
               RHS_Prop_Id : Identifier.Tree renames Identifier.Tree
                 (Tree_Ptr_Of (Property.Tree (RHS_Tree).Property_Id).all);
               RHS_Prop_Str : String renames
                 Strings.To_String (RHS_Prop_Id.Str);
            begin
               if RHS_Prop_Str = "@Move" then
                  --  Create a "Move" from the Prefix of the 'Move
                  --  Only used for Ada202X so far, but might be used
                  --  for other languages some day.
                  return Optional
                     (Tree'(Trees.Tree with Move_Op,
                            LHS => LHS,
                            RHS => Property.Tree (RHS_Tree).Operand));
               end if;
            end;
         end if;
      end;

      --  Nothing special -- just create an assignment
      return Optional (Tree'(Trees.Tree with Assign_Operator, LHS, RHS));
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
      if N = 1 then
         return T.LHS;
      else
         return T.RHS;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N <= 2);
   begin
      if N = 1 then
         T.LHS := New_Operand;
      else
         T.RHS := New_Operand;
      end if;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Assign_Stmt_Kind;
   end Kind;

   function Assign_Operator_Image
     (Assign_Operator : Assign_Operator_Enum)
      return String
   is
   begin
      case Assign_Operator is
      when Assign_Op =>
         return ":=";
      when Plus_Assign_Op =>
         return "+=";
      when Minus_Assign_Op =>
         return "-=";
      when Times_Assign_Op =>
         return "*=";
      when Divide_Assign_Op =>
         return "/=";
      when Power_Assign_Op =>
         return "**=";
      when Combine_Assign_Op =>
         return "|=";
      when Ampersand_Assign_Op =>
         return "&=";
      when And_Assign_Op =>
         return "and=";
      when Or_Assign_Op =>
         return "or=";
      when Xor_Assign_Op =>
         return "xor=";
      when Left_Shift_Assign_Op =>
         return "<<=";
      when Right_Shift_Assign_Op =>
         return ">>=";
      when Swap_Op =>
         return "<=>";
      when Move_Op =>
         return "<==";
      when Combine_Move_Op =>
         return "<|=";
      end case;
   end Assign_Operator_Image;

   function Assign_Operator_Designator
     (Op : Assign_Operator_Enum)
      return Strings.U_String
   is
   --  Return the designator to use when defining an assign operator.
   --  This includes the "" on either side of the operator symbol.
   begin
      return Strings.String_Lookup ('"' & Assign_Operator_Image (Op) & '"');
   end Assign_Operator_Designator;

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
      Put_Indent (On, Indent);
      Display_Subtree (T.LHS, On);
      Put (On, ' ' & Assign_Operator_Image (T.Assign_Operator) & ' ');
      Display_Subtree (T.RHS, On);
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Assign_Stmt_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Assign_Stmt_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.LHS);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.RHS);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

   function Contains_Target_Name (RHS : Optional_Tree) return Boolean is
   --  Return True if the target name symbol ('@') appears within RHS
   begin
      if Not_Null (RHS) then
         declare
            RHS_Tree : Trees.Tree'Class renames Tree_Ptr_Of (RHS).all;
            use type Strings.U_String;
         begin
            if RHS_Tree in Identifier.Tree then
               return Identifier.Tree (RHS_Tree).Str = Target_Name_Str;
            else
               for I in 1 .. Num_Operands (RHS_Tree) loop
                  if Contains_Target_Name (Nth_Operand (RHS_Tree, I)) then
                     return True;
                  end if;
               end loop;
            end if;
         end;
      end if;
      return False;
   end Contains_Target_Name;

   function Replace_Target_Name (RHS : Optional_Tree;
                                 LHS_Ren_Str : Strings.U_String)
     return Optional_Tree is
   --  Return RHS with all appearances of the target name symbol '@'
   --  replaced with an identifier with id LHS_Ren_Str.
   begin
      if Is_Null (RHS) then
         return RHS;
      else
         declare
            RHS_Tree : Trees.Tree'Class renames Tree_Ptr_Of (RHS).all;
            use type Strings.U_String;
         begin
            if RHS_Tree in Identifier.Tree
              and then Identifier.Tree (RHS_Tree).Str = Target_Name_Str
            then
               --  Put_Line ("Replacing target name with " &
               --    Strings.To_String (LHS_Ren_Str));
               return Identifier.Make
                        (LHS_Ren_Str,
                         Source_Pos => Identifier.Tree (RHS_Tree).Source_Pos);
            end if;
            for I in 1 .. Num_Operands (RHS_Tree) loop
               declare
                  Nth_Opnd : constant Optional_Tree :=
                    Nth_Operand (RHS_Tree, I);
                  Updated_Opnd : constant Optional_Tree :=
                    Replace_Target_Name
                      (Nth_Opnd, LHS_Ren_Str);
               begin
                  if Updated_Opnd /= Nth_Opnd then
                     --  Operand must have contained a target name
                     --  so insert it into the (updated) tree.
                     Set_Nth_Operand (RHS_Tree, I, Updated_Opnd);
                  end if;
               end;
            end loop;

            --  Return the (possibly) updated RHS.
            return RHS;
         end;
      end if;
   end Replace_Target_Name;
   --  replaced with an identifier with id LHS_Ren_Str.

end PSC.Trees.Assign_Stmt;
