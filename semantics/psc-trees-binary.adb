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

with PSC.Languages;
with PSC.Stream_Output;
with PSC.Trees.Assign_Stmt;
with PSC.Trees.Unary;
with PSC.Trees.Visitor;
with PSC.Strings;
pragma Elaborate (PSC.Strings);
package body PSC.Trees.Binary is

   function Make
     (Operator : Binary_Operator_Enum;
      Left_Operand : Optional_Tree;
      Right_Operand : Optional_Tree;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position)
      return Optional_Tree
   is
   --  Build up a binary tree.
      Result_Tree : Binary.Tree :=
        (Trees.Tree with Operator, Left_Operand, Right_Operand);
   begin
      Result_Tree.Source_Pos := Source_Pos;
      return Optional (Result_Tree);
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
         return T.Left_Operand;
      else
         return T.Right_Operand;
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
         T.Left_Operand := New_Operand;
      else
         T.Right_Operand := New_Operand;
      end if;
   end Set_Nth_Operand;

   function Kind (T : Tree) return Tree_Kind_Enum is
   begin
      return Binary_Kind;
   end Kind;

   function Precedence (OT : Optional_Tree) return Precedence_Type is
   --  Return precedence of given tree.
   --  Return Max_Precedence if not a unary or binary operator
   begin
      if OT.Ptr = null then
         return Max_Precedence;
      elsif OT.Ptr.all in Binary.Tree'Class then
         return Precedence_Table (Binary.Tree'Class (OT.Ptr.all).Operator);
      elsif OT.Ptr.all in Unary.Tree'Class then
         return Unary.Precedence_Table
           (Unary.Tree'Class (OT.Ptr.all).Operator);
      elsif OT.Ptr.all in Assign_Stmt.Tree'Class then
         --  These should essentially always be parenthesized
         return Assignment_Op_Precedence;
      else
         return Max_Precedence;
      end if;
   end Precedence;

   function Binary_Operator_Image (Op : Binary_Operator_Enum) return String is
   --  Return string name for Op (the surrounding "" are omitted).
   begin
      case Op is
      when Plus_Op =>
         return "+";
      when Minus_Op =>
         return "-";
      when Times_Op =>
         return "*";
      when Divide_Op =>
         return "/";
      when Mod_Op =>
         return "mod";
      when Rem_Op =>
         return "rem";
      when Power_Op =>
         return "**";
      when Compare_Op =>
         return Languages.Compare_Op_Name;
      when Less_Op =>
         return "<";
      when LEQ_Op =>
         return "<=";
      when Equal_Op =>
         case Languages.Language is
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               return "==";
            when Languages.Ada_Ish =>
               return "=";
         end case;
      when NEQ_Op =>
         case Languages.Language is
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               return "!=";
            when Languages.Ada_Ish =>
               return "/=";
         end case;
      when GEQ_Op =>
         return ">=";
      when Greater_Op =>
         return ">";
      when In_Op =>
         return "in";
      when Not_In_Op =>
         return "not in";
      when Left_Shift_Op =>
         return "<<";
      when Right_Shift_Op =>
         return ">>";
      when Meaning_Op =>
         return "::[[";  --  Meaning_op prefixed with type name
      when Closed_Interval_Op =>
         return "..";
      when Open_Interval_Op =>
         return "<..<";
      when Open_Closed_Interval_Op =>
         return "<..";
      when Closed_Open_Interval_Op =>
         return "..<";
      when And_Op =>
         return "and";
      when Or_Op =>
         return "or";
      when Xor_Op =>
         return "xor";
      when And_Then_Op =>
         return "and then";
      when Or_Else_Op =>
         return "or else";
      when Implies_Op =>
         return "==>";
      when Combine_Op =>
         return "|";
      when Ampersand_Op =>
         return "&";
      when Sequential_Stmt_Op =>
         return ";";
      when Parallel_Stmt_Op =>
         return "||";
      when Handled_Stmt_Op =>
         return "exception";
      when Next_Stmt_Op =>
         return ";";
      when Then_Stmt_Op =>
         return "then";
      end case;
   end Binary_Operator_Image;

   function Binary_Operator_Designator
     (Op : Binary_Operator_Enum)
      return Strings.U_String
   is
   --  Return U_String that corresponds to operator designator used
   --  when declaring the operator.
   --  The U_String *includes* the surrounding "".
   begin
      case Op is
         when Plus_Op                 |
              Minus_Op                |
              Times_Op                |
              Divide_Op               |
              Mod_Op                  |
              Rem_Op                  |
              Power_Op                |
              Compare_Op              |
              Less_Op                 |
              LEQ_Op                  |
              Equal_Op                |
              NEQ_Op                  |
              GEQ_Op                  |
              Greater_Op              |
              In_Op                   |
              Not_In_Op               |
              Left_Shift_Op           |
              Right_Shift_Op          |
              Closed_Interval_Op      |
              Open_Interval_Op        |
              Open_Closed_Interval_Op |
              Closed_Open_Interval_Op |
              And_Op                  |
              Or_Op                   |
              Xor_Op                  |
              And_Then_Op             |
              Or_Else_Op              |
              Implies_Op              |
              Combine_Op              |
              Ampersand_Op            |
              Sequential_Stmt_Op      |
              Parallel_Stmt_Op        |
              Handled_Stmt_Op         |
              Next_Stmt_Op            |
              Then_Stmt_Op            =>
            return Strings.String_Lookup
                     ('"' & Binary_Operator_Image (Op) & '"');
         when Meaning_Op =>
            return Strings.String_Lookup ("""to_univ""");
      end case;
   end Binary_Operator_Designator;

   Interval_Op_Desigs : constant array (Interval_Ops) of Strings.U_String :=
     (Closed_Interval_Op => Binary_Operator_Designator (Closed_Interval_Op),
      Open_Closed_Interval_Op => Binary_Operator_Designator
                                   (Open_Closed_Interval_Op),
      Closed_Open_Interval_Op => Binary_Operator_Designator
                                   (Closed_Open_Interval_Op),
      Open_Interval_Op => Binary_Operator_Designator (Open_Interval_Op));

   function Is_Interval_Op_Designator
     (Desig : Strings.U_String)
      return Boolean
   is
      --  Return True if Desig is one of the interval operators
      --  ("..", "<..", etc.)
      use type Strings.U_String;
   begin
      for I in Interval_Op_Desigs'Range loop
         if Desig = Interval_Op_Desigs (I) then
            --  Found it
            return True;
         end if;
      end loop;
      --  Not an interval op
      return False;
   end Is_Interval_Op_Designator;

   function Is_Parallel_Stmt_Op (OT : Optional_Tree) return Boolean is
      --  Return True if OT is a binary node with operator
      --  Binary.Parallel_Stmt_Op.
      use type Binary.Binary_Operator_Enum;
   begin
      return Not_Null (OT)
            and then Tree_Ptr_Of (OT).all in Binary.Tree
            and then Binary.Tree (Tree_Ptr_Of (OT).all).Operator =
                     Binary.Parallel_Stmt_Op;
   end Is_Parallel_Stmt_Op;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      Op_Prec : constant Precedence_Type := Precedence_Table (T.Operator);
      Op_Assoc : constant Associativity_Enum :=
        Associativity_Table (T.Operator);
      Left_Prec : constant Precedence_Type := Precedence (T.Left_Operand);
      Right_Prec : constant Precedence_Type := Precedence (T.Right_Operand);
      Left_Needs_Space : constant array (Binary_Operator_Enum) of Boolean :=
        (Meaning_Op         |
         Next_Stmt_Op       |
         Sequential_Stmt_Op |
         Then_Stmt_Op       |
         Handled_Stmt_Op    |
         Parallel_Stmt_Op   => False,
         others => True);
      --  Whether a space is needed to separate from left operand
      --  (for "then" and "||" and a handler we don't need a space
      --  because we start them on a new line).
      Right_Needs_Space : constant array (Binary_Operator_Enum) of Boolean :=
        (Meaning_Op | Stmt_Ops => False,
         others => True);
   --  Whether a space is needed to separate from right operand
   begin
      if T.Operator not in Stmt_Ops then
         Put_Indent (On, Indent);
      end if;

      if Op_Prec > Left_Prec
        or else (Op_Prec = Left_Prec and then Op_Assoc /= Left_Assoc)
      then
         if T.Operator in Stmt_Ops then
            Put_Indent (On, Indent);
            Put_Line (On, "block");
            Display_Subtree
              (T.Left_Operand,
               On,
               Indent => Indent + 2,
               Use_Short_Form => Use_Short_Form);
            New_Line (On, Indent => Indent);
            Put (On, "end block");
         else
            --  Parenthesize left operand
            Put (On, "(");
            Display_Subtree (T.Left_Operand, On);
            Put (On, ")");
         end if;
      elsif T.Operator in Stmt_Ops then
         Display_Subtree
           (T.Left_Operand,
            On,
            Indent => Indent,
            Use_Short_Form => Use_Short_Form);
      else
         Display_Subtree (T.Left_Operand, On);
      end if;

      if Use_Short_Form
        and then T.Operator in Stmt_Ops
        and then Not_Null (T.Left_Operand)
        and then Tree_Ptr_Of (T.Left_Operand).all in Binary.Tree
        and then Binary.Tree (Tree_Ptr_Of (T.Left_Operand).all).Operator in
           Stmt_Ops
      then
         --  Left operand is tree of statements, and has already
         --  emitted the "..." part
         return;  --- quit early ---
      end if;

      if T.Operator = Then_Stmt_Op
        or else T.Operator in Independent_Stmt_Ops
      then
         if Indent >= 2 then
            Put (On, ';');
            New_Line (On, Indent => Indent - 2);
         else
            Put (On, ' ');
         end if;
      elsif Left_Needs_Space (T.Operator) then
         Put (On, ' ');
      end if;
      Put (On, Binary_Operator_Image (T.Operator));

      if Use_Short_Form and then T.Operator in Stmt_Ops then
         Put_Line (On, " ...");
         return;  --- quit early ---
      end if;

      if Right_Needs_Space (T.Operator) then
         Put (On, ' ');
      elsif T.Operator in Stmt_Ops then
         if Indent < 2 then
            --  Unusual case of un-indented statement,
            --  must be inside lambda expression.
            Put (On, ' ');
         else
            New_Line (On);
         end if;
      end if;

      if Op_Prec > Right_Prec
        or else (Op_Prec = Right_Prec and then Op_Assoc /= Right_Assoc)
      then
         if T.Operator in Stmt_Ops then
            Put_Indent (On, Indent);
            Put_Line (On, "block");
            Display_Subtree (T.Right_Operand, On, Indent => Indent + 2);
            New_Line (On, Indent => Indent);
            Put (On, "end block");
         else
            --  Parenthesize Right operand
            Put (On, "(");
            Display_Subtree (T.Right_Operand, On);
            Put (On, ")");
         end if;
      elsif T.Operator in Stmt_Ops then
         Display_Subtree (T.Right_Operand, On, Indent => Indent);
      else
         Display_Subtree (T.Right_Operand, On);
      end if;

      if T.Operator = Meaning_Op then
         --  Finish the meaning op
         Put (On, "]]");
      end if;

   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Binary_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Binary_Action (RW_Tree_Visitor'Class (Visitor), T);
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
         T_Source_Pos := Find_Source_Pos (T.Left_Operand);
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Right_Operand);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Binary;
