------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
with PSC.Messages;
package body PSC.Trees.Param_Decl is
   --  Representation for a declaration of an object

   function Make
     (Name : Optional_Tree;   --  Name is optional on param
      Kind : Param_Kind;
      Locking : Param_Locking;
      Is_Optional : Boolean;
      Param_Type : Optional_Tree;      --  Type is actually *not*
                                       --  optional
      Param_Default : Optional_Tree;
      In_Region : Optional_Tree := Null_Optional_Tree;
      Is_Implicit_Module_Param : Boolean := False)
      return Optional_Tree
   is
   --  Build up a parameter declaration
   begin
      --  Make sure the params make sense

      if Not_Null (Param_Default) then
         --  Default only permitted for "regular" in parameter
         if Kind /= Default_Param then
            Messages.Parser_Error
              ("Default not permitted for " &
                 Param_Kind_Image (Kind) & "parameter.");
         end if;
      end if;

      if Is_Optional then
         --  If optional, cannot be locked/queued
         pragma Assert (Locking = Not_Locked);
         null;
      end if;

      return Optional
               (Tree'(Trees.Tree with
                      Name => Name,
                      Kind => Kind,
                      Locking => Locking,
                      Is_Optional => Is_Optional,
                      In_Region => In_Region,
                      Param_Type => Param_Type,
                      Param_Default => Param_Default,
                      Is_Implicit_Module_Param => Is_Implicit_Module_Param));
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
            return T.Param_Type;
         when 2 =>
            return T.Param_Default;
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
            T.Param_Type := New_Operand;
         when 2 =>
            T.Param_Default := New_Operand;
         when others =>
            pragma Assert (False);  --  N out of range
            null;
      end case;
      return;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is (Param_Decl_Kind);

   function Param_Kind_Image
     (Kind : Param_Kind; Locking : Param_Locking := Not_Locked) return String
   --  Return Param_Kind/Locking as it would appear in the source as
   --  a prefix to the parameter name.
   is
   begin
      case Locking is
         when Queued_Param =>
            return "queued " & Param_Kind_Image (Kind);
         when Locked_Param =>
            return "locked " & Param_Kind_Image (Kind);
         when Not_Locked =>
            --  Var vs. Ref vs. Global
            case Kind is
               when Default_Param =>
                  return "";
               when Out_Param =>
                  return "out ";
               when Var_Param =>
                  if Languages.Language in Languages.Ada_Ish then
                     return "in out ";
                  else
                     return "var ";
                  end if;
               when Ref_Param =>
                  if Languages.Language in Languages.Ada_Ish then
                     return "aliased ";
                  else
                     return "ref ";
                  end if;
               when Ref_Const_Param =>
                  if Languages.Language in Languages.Ada_Ish then
                     return "aliased constant ";
                  else
                     return "ref const ";
                  end if;
               when Ref_Out_Param =>
                  return "aliased out ";
               when Ref_Var_Param =>
                  if Languages.Language in Languages.Ada_Ish then
                     return "aliased in out ";
                  else
                     return "ref var ";
                  end if;
               when Global_Param =>
                  return "global ";
               when Global_Out_Param =>
                  return "global out ";
               when Global_Var_Param =>
                  if Languages.Language in Languages.Ada_Ish then
                     return "global in out ";
                  else
                     return "global var ";
                  end if;
            end case;
      end case;

   end Param_Kind_Image;

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
      --  Display parameter declaration at given indent
      Put_Indent (On, Indent);

      if T.Is_Implicit_Module_Param then
         Put (On, "<");
      end if;

      Put (On, Param_Kind_Image (T.Kind, T.Locking));

      if Not_Null (T.Name) then
         Display_Subtree (T.Name, On);
         if Not_Null (T.In_Region) then
            Put (On, " for ");
            Display_Subtree (T.In_Region, On);
         end if;
         if T.Is_Optional or else Not_Null (T.Param_Type) then
            Put (On, " : ");
         end if;
      end if;

      if T.Is_Optional then
         Put (On, "optional ");
      end if;

      Display_Subtree (T.Param_Type, On, Use_Short_Form => Use_Short_Form);

      if Not_Null (T.Param_Default) then
         Put (On, " := ");
         Display_Subtree (T.Param_Default, On);
      end if;

      if T.Is_Implicit_Module_Param then
         Put (On, ">");
      end if;
      if Indent > 0 then
         --  Only if indent > 0 do we assume we have our own line
         Put_Line (On, ";");
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Param_Decl_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Param_Decl_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Name);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Param_Type);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Param_Decl;
