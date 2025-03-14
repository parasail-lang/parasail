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
with PSC.Trees.Binary;
package body PSC.Trees.Unary is

   function Make
     (Operator : Unary_Operator_Enum;
      Operand : Optional_Tree)
      return Optional_Tree
   is
   --  Build up a unary tree.
   begin
      return Optional
               (Tree'(Trees.Tree with Operator => Operator, Operand =>
        Operand));
   end Make;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return 1;
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      pragma Assert (N = 1);
   begin
      return T.Operand;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      pragma Assert (N = 1);
   begin
      T.Operand := New_Operand;
   end Set_Nth_Operand;

   function Unary_Operator_Image (Op : Unary_Operator_Enum) return String is
   --  Return string name for Op (surrounding "" are omitted).
   begin
      case Op is
         when Plus_Op =>
            return "+";
         when Minus_Op =>
            return "-";
         when Not_Op =>
            return "not";
         when Abs_Op =>
            return "abs";
         when Is_Null_Op =>
            return "is null";
         when Not_Null_Op =>
            return "not null";
         when Meaning_Op =>
            return "[[";
         when Magnitude_Op =>
            return "|";
         when Updated_Value_Op =>
            return "'";
         when Initial_Value_Op =>
            return "<";
      end case;
   end Unary_Operator_Image;

   function Unary_Operator_Designator
     (Op : Unary_Operator_Enum)
      return Strings.U_String
   is
   --  Return U_String that corresponds to operator designator used
   --  when declaring the operator (e.g. "to_univ" rather than "[[...]]")
   --  The U_String *includes* the surrounding "".
   begin
      case Op is
         when Plus_Op     |
              Minus_Op    |
              Not_Op      |
              Abs_Op      |
              Is_Null_Op  |
              Not_Null_Op =>
            return Strings.String_Lookup
                     ('"' & Unary_Operator_Image (Op) & '"');
         when Updated_Value_Op =>
            return Strings.String_Lookup ("""prime""");
         --  TBD: How could this possibly be used?
         when Meaning_Op =>
            return Strings.String_Lookup ("""to_univ""");
         when Magnitude_Op =>
            return Strings.String_Lookup ("""magnitude""");
         when Initial_Value_Op =>
            return Strings.String_Lookup ("""initial_value""");
      end case;
   end Unary_Operator_Designator;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      use type Binary.Precedence_Type;

      --  Whether a space is needed to separate from operand
      Needs_Space : constant array (Unary_Operator_Enum) of Boolean :=
        (Plus_Op | Minus_Op | Meaning_Op | Magnitude_Op |
         Updated_Value_Op | Initial_Value_Op => False,
         Not_Op | Abs_Op | Is_Null_Op | Not_Null_Op => True);

      --  Whether operator comes after rather than before.
      Is_Postfix : constant array (Unary_Operator_Enum) of Boolean :=
        (Plus_Op | Minus_Op | Not_Op | Abs_Op |
         Meaning_Op | Magnitude_Op | Initial_Value_Op => False,
         Is_Null_Op | Not_Null_Op | Updated_Value_Op => True);

      --  Whether operator comes both before and after
      Is_Bracketing : constant array (Unary_Operator_Enum) of Boolean :=
        (Meaning_Op | Magnitude_Op | Initial_Value_Op => True,
         Plus_Op | Minus_Op | Not_Op | Abs_Op | Is_Null_Op | Not_Null_Op |
         Updated_Value_Op => False);

      function Closing_Bracket_Image (Op : Unary_Operator_Enum) return String
      is
      --  Return string used to close a "bracketing" op
         pragma Assert (Is_Bracketing (Op));
      begin
         case Op is
            when Meaning_Op =>
               return "]]";
            when Magnitude_Op =>
               return "|";
            when Initial_Value_Op =>
               return ">";
            when others =>
               pragma Assert (False);
               return "";
         end case;
      end Closing_Bracket_Image;

   begin  --  Display_Subtree

      if not Is_Postfix (T.Operator) then
         Put (On, Unary_Operator_Image (T.Operator));
      end if;

      if Is_Bracketing (T.Operator) then
         --  The "meaning" op ([[x]]) and the "initial value" op
         --  are "bracketing" operators.
         Display_Subtree (T.Operand, On);
         Put (On, Closing_Bracket_Image (T.Operator));

      elsif Binary.Precedence (T.Operand) <
            Precedence_Table (T.Operator)
      then
         --  Operand needs to be parenthesized
         Put (On, '(');
         Display_Subtree (T.Operand, On);
         Put (On, ')');
      else
         --  No parentheses needed
         if Needs_Space (T.Operator)
           and then not Is_Postfix (T.Operator)
         then
            Put (On, ' ');
         end if;
         Display_Subtree (T.Operand, On);
      end if;
      if Is_Postfix (T.Operator) then
         if Needs_Space (T.Operator) then
            Put (On, ' ');
         end if;
         Put (On, Unary_Operator_Image (T.Operator));
      end if;
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Unary_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Unary_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Operand);
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Unary;
