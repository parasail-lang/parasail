------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with PSC.Trees.Visitor;
with PSC.Stream_Output;
with PSC.Strings;
with PSC.Languages;
package body PSC.Trees.Qualified_Name is

   function Make
     (Prefix : Optional_Tree;
      Id : Optional_Tree;
      Uses_Selection_Syntax : Boolean := False)
      return Optional_Tree
   is
   --  Build up a qualified name
   begin
      return Optional (Tree'(Trees.Tree with Prefix => Prefix, Id => Id,
        Uses_Selection_Syntax => Uses_Selection_Syntax));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 2;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
   --  Return Nth operand of given Tree
   begin
      pragma Assert (N <= 2);
      if N = 1 then
         return T.Prefix;
      else
         return T.Id;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
   --  Set Nth operand of given Tree
   begin
      pragma Assert (N <= 2);
      if N = 1 then
         T.Prefix := New_Operand;
      else
         T.Id := New_Operand;
      end if;
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
      use type PSC.Languages.Language_Enum;
   begin
      Put_Indent (On, Indent);
      case Languages.Language is
         when Languages.Ada_Ish =>
            Display_Subtree (T.Prefix, On);
            if T.Uses_Selection_Syntax then
               Put (On, ".");
            else
               Put (On, "'");
            end if;
         when Languages.ParaSail | Languages.Parython =>
            Display_Subtree (T.Prefix, On);
            Put (On, "::");
         when Languages.Javallel =>
            if T.Uses_Selection_Syntax then
               Display_Subtree (T.Prefix, On);
               Put (On, ".");
            else
               Put (On, "(");
               Display_Subtree (T.Prefix, On);
               Put (On, ")");
            end if;
      end case;
      Display_Subtree (T.Id, On);
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Qualified_Name_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Qualified_Name_Action (RW_Tree_Visitor'Class (Visitor), T);
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
            T_Source_Pos := Find_Source_Pos (T.Prefix);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

   function Contains_String
     (OT : Optional_Tree)
      return Boolean
   --  Return True if qualified name (or simple identifier) contains
   --  a string literal somewhere within it.
   is
      T : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
      use type Strings.U_String;
   begin
      if T in Qualified_Name.Tree then
         --  Recurse on components of qualified name
         return Contains_String (Qualified_Name.Tree (T).Prefix)
           or else Contains_String (Qualified_Name.Tree (T).Id);
      elsif T in Identifier.Tree
        and then Identifier.Tree (T).Str /= Strings.Null_U_String
      then
         --  A simple identifier, check first character
         declare
            Str : String renames Strings.To_String (Identifier.Tree (T).Str);
         begin
            return Str (Str'First) = '"';
         end;
      else
         return False;
      end if;

   end Contains_String;

end PSC.Trees.Qualified_Name;
