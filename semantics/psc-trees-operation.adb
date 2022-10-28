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

with PSC.Languages;
with PSC.Stream_Output;

with PSC.Trees.Binary;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Invocation;
with PSC.Trees.Lists;
with PSC.Trees.Unary;
with PSC.Trees.Visitor;
package body PSC.Trees.Operation is
   --  Representation for a function, procedure, or operator

   function Make
     (Name : Optional_Tree; --  Qualified_Name or Identifier
      Operation_Kind : Operation_Kind_Enum;
      Operation_Inputs : List;
      Operation_Outputs : List;
      Global_Read_List : List := Lists.Empty_List;
      Global_Update_List : List := Lists.Empty_List;
      Preconditions : Optional_Tree;
      Postconditions : Optional_Tree;
      Is_Abstract : Boolean := False;
      Is_Optional : Boolean := False;
      Is_Queued : Boolean := False;
      Is_Def : Boolean := False;
      Is_Expression_Function : Boolean := False;
      Is_Import : Boolean := False;
      Dequeue_Condition : Optional_Tree := Null_Optional_Tree;
      Statements : Optional_Tree := Null_Optional_Tree)
      return Optional_Tree
   is
   --  Build up an operation
      function Is_Single_Expression (Statements : Optional_Tree)
        return Boolean
      --  Return True if Statements is in the form of a single
      --  expression, rather than a "return" statement or
      --  an expression sequence.
      --  This determines whether Is_Expression_Function is set to True.
      is
         Stmt_Tree : Trees.Tree'Class renames
           Tree_Ptr_Of (Invocation.Remove_Parentheses (Statements)).all;
      begin
         if Stmt_Tree in Control_Stmt.Tree then
            --  E.g. "return X"
            return False;
         elsif Stmt_Tree in Binary.Tree
           and then Binary.Tree (Stmt_Tree).Operator in Binary.Stmt_Ops
         then
            --  E.g. "(A; B)"
            return False;
         else
            return True;
         end if;
      end Is_Single_Expression;

   begin  --  Make

      --  Make sure it is well formed
      if not Is_Def then
         pragma Assert (Is_Null (Dequeue_Condition));
         pragma Assert (Is_Null (Statements));
         null;
         pragma Assert (not (Is_Optional and Is_Abstract));
      else
         pragma Assert (not Is_Optional);
         pragma Assert (not Is_Abstract);
         null;
      end if;

      return Optional
               (Tree'(Trees.Tree with
                      Name => Name,
                      Operation_Kind => Operation_Kind,
                      Import_Clauses => Lists.Empty_List,
                      Operation_Inputs => Operation_Inputs,
                      Operation_Outputs => Operation_Outputs,
                      Global_Read_List => Global_Read_List,
                      Global_Update_List => Global_Update_List,
                      Preconditions => Preconditions,
                      Postconditions => Postconditions,
                      Is_Abstract => Is_Abstract,
                      Is_Optional => Is_Optional,
                      Is_Queued => Is_Queued,
                      Is_Def => Is_Def,
                      Is_Expression_Function => Is_Expression_Function
                        or else (Operation_Kind = Lambda_Operation
                          and then Is_Single_Expression (Statements)),
                      Is_Import => Is_Import,
                      Op_Equiv => Null_Optional_Tree,
                      Op_Location => Null_Optional_Tree,
                      Import_Info => Empty_List,
                      Dequeue_Condition => Dequeue_Condition,
                      Statements => Statements));
   end Make;

   function Add_Op_Default
     (Op_Decl : Optional_Tree;
      Op_Default : Optional_Tree)
      return Optional_Tree
   is
      --  Add "Default" to an operation
      --  Requires: Op_Decl must be a tree of type
      --  Operation.Tree with Is_Def False.
      pragma Assert (Not_Null (Op_Decl));
      pragma Assert (Tree_Of (Op_Decl) in Operation.Tree'Class);
      pragma Assert (Operation.Tree (Tree_Of (Op_Decl)).Is_Def = False);
   begin
      --  Fill in the "Statements" field in place to represent a default
      Operation.Tree (Op_Decl.Ptr.all).Statements := Op_Default;
      return Op_Decl;
   end Add_Op_Default;

   function Add_Op_Equiv
     (Op_Decl : Optional_Tree;
      Op_Equiv : Optional_Tree)
      return Optional_Tree
   is
      --  Add "Op_Equiv" to an operation
      --  Requires: Op_Decl must be a tree of type
      --  Operation.Tree with Is_Def False.
      pragma Assert (Not_Null (Op_Decl));
      pragma Assert (Tree_Of (Op_Decl) in Operation.Tree'Class);
      pragma Assert (Operation.Tree (Tree_Of (Op_Decl)).Is_Def = False);
   begin
      --  Fill in the "Op_Equiv" field and set Is_Def true.
      Operation.Tree (Op_Decl.Ptr.all).Op_Equiv := Op_Equiv;
      Operation.Tree (Op_Decl.Ptr.all).Is_Def := True;
      return Op_Decl;
   end Add_Op_Equiv;

   function Add_Op_Location
     (Op_Decl : Optional_Tree;
      Op_Location : Optional_Tree)
      return Optional_Tree
   is
      --  Add "Op_Location" to an operation
      --  Requires: Op_Decl must be a tree of type
      --  Operation.Tree with Is_Def False, or with Op_Equiv specified.
      pragma Assert (Not_Null (Op_Decl));
      pragma Assert (Tree_Of (Op_Decl) in Operation.Tree'Class);
      pragma Assert
        (Operation.Tree (Tree_Of (Op_Decl)).Is_Def = False
        or else Not_Null (Operation.Tree (Tree_Of (Op_Decl)).Op_Equiv));
   begin
      --  Fill in the "Op_Location" field and set Is_Def true.
      Operation.Tree (Op_Decl.Ptr.all).Op_Location := Op_Location;
      Operation.Tree (Op_Decl.Ptr.all).Is_Def := True;
      return Op_Decl;
   end Add_Op_Location;

   function Add_Import_Info
     (Op_Decl : Optional_Tree;
      Import_Info : List)
      return Optional_Tree
   is
      --  Add import info to an operation declaration.
      --  Requires: Op_Decl must be a tree of type
      --  Operation.Tree with Is_Def False.
      pragma Assert (Not_Null (Op_Decl));
      pragma Assert (Tree_Of (Op_Decl) in Operation.Tree'Class);
      pragma Assert (Operation.Tree (Tree_Of (Op_Decl)).Is_Def = False);
   begin
      --  Fill in the "Import_Info" field.
      --  Set Is_Def and Is_Import flags
      Operation.Tree (Op_Decl.Ptr.all).Import_Info := Import_Info;
      Operation.Tree (Op_Decl.Ptr.all).Is_Import := True;
      Operation.Tree (Op_Decl.Ptr.all).Is_Def := True;
      return Op_Decl;
   end Add_Import_Info;

   function Num_Operands (T : Tree) return Natural is
   --  Return number of operands of given Tree
   begin
      return Lists.Length (T.Operation_Inputs) +
             Lists.Length (T.Operation_Outputs) +
             Lists.Length (T.Global_Read_List) +
             Lists.Length (T.Global_Update_List) +
             Boolean'Pos (Not_Null (T.Statements)) +
             Boolean'Pos (Not_Null (T.Op_Equiv)) +
             Boolean'Pos (Not_Null (T.Op_Location));
   end Num_Operands;

   function Nth_Operand (T : Tree; N : Positive) return Optional_Tree is
      --  Return Nth operand of given Tree
      Num_Inputs : constant Natural := Lists.Length (T.Operation_Inputs);
      Num_Outputs : constant Natural := Lists.Length (T.Operation_Outputs);
      Num_Global_Reads : constant Natural :=
        Lists.Length (T.Global_Read_List);
      Num_Global_Updates : constant Natural :=
        Lists.Length (T.Global_Update_List);
      Index : Natural := N;
   begin
      if N <= Num_Inputs then
         return Lists.Nth_Element (T.Operation_Inputs, N);
      else
         Index := Index - Num_Inputs;
         if Index <= Num_Outputs then
            return Lists.Nth_Element (T.Operation_Outputs, Index);
         else
            Index := Index - Num_Outputs;
            if Index <= Num_Global_Reads then
               return Lists.Nth_Element (T.Global_Read_List, Index);
            end if;
            Index := Index - Num_Global_Reads;
            if Index <= Num_Global_Updates then
               return Lists.Nth_Element (T.Global_Update_List, Index);
            end if;
            Index := Index - Num_Global_Updates;
            if Not_Null (T.Statements) then
               Index := Index - 1;
               if Index = 0 then
                  return T.Statements;
               end if;
            end if;
            if Not_Null (T.Op_Equiv) then
               Index := Index - 1;
               if Index = 0 then
                  return T.Op_Equiv;
               end if;
            end if;
            pragma Assert (Not_Null (T.Op_Location));
            pragma Assert (Index = 1);
            return T.Op_Location;
         end if;
      end if;
   end Nth_Operand;

   procedure Set_Nth_Operand
     (T : in out Tree;
      N : Positive;
      New_Operand : Optional_Tree) is
      --  Set Nth operand of given Tree
      Num_Inputs : constant Natural := Lists.Length (T.Operation_Inputs);
      Num_Outputs : constant Natural := Lists.Length (T.Operation_Outputs);
      Num_Global_Reads : constant Natural :=
        Lists.Length (T.Global_Read_List);
      Num_Global_Updates : constant Natural :=
        Lists.Length (T.Global_Update_List);
      Index : Natural := N;
   begin
      if N <= Num_Inputs then
         T.Operation_Inputs :=
            Lists.Replace_Nth_Element (T.Operation_Inputs, N, New_Operand);
      else
         Index := Index - Num_Inputs;
         if Index <= Num_Outputs then
            T.Operation_Outputs :=
               Lists.Replace_Nth_Element
                 (T.Operation_Outputs, Index, New_Operand);
         else
            Index := Index - Num_Outputs;
            if Index <= Num_Global_Reads then
               T.Global_Read_List :=
                  Lists.Replace_Nth_Element
                    (T.Global_Read_List, Index, New_Operand);
               return;
            end if;
            Index := Index - Num_Global_Reads;
            if Index <= Num_Global_Updates then
               T.Global_Update_List :=
                  Lists.Replace_Nth_Element
                    (T.Global_Update_List, Index, New_Operand);
               return;
            end if;
            Index := Index - Num_Global_Updates;
            if Not_Null (T.Statements) then
               Index := Index - 1;
               if Index = 0 then
                  T.Statements := New_Operand;
               end if;
            end if;
            if Not_Null (T.Op_Equiv) then
               Index := Index - 1;
               if Index = 0 then
                  T.Op_Equiv := New_Operand;
               end if;
            end if;
            pragma Assert (Not_Null (T.Op_Location));
            pragma Assert (Index = 1);
            T.Op_Location := New_Operand;
         end if;
      end if;
      return;
   end Set_Nth_Operand;

   function Substitute_Operands
     (T : Tree;
      New_Operands : Tree_Array)
      return Optional_Tree
   is
      --  Create a new tree given new operands and an existing tree.
      --  The default implementation dispatches to Set_Nth_Operand but
      --  it may be overridden for efficiency.
      --  Requires: New_Operands'Length = Num_Operands(T)
      New_Tree : Tree := T;
      Num_Opnds : constant Natural := Num_Operands (T);
      Num_Inputs : constant Natural := Lists.Length (T.Operation_Inputs);
      Num_Outputs : constant Natural := Lists.Length (T.Operation_Outputs);
      Num_Global_Reads : constant Natural :=
        Lists.Length (T.Global_Read_List);
      Num_Global_Updates : constant Natural :=
        Lists.Length (T.Global_Update_List);
      Index : Positive := New_Operands'First;
      pragma Assert (New_Operands'Length = Num_Opnds);
   begin
      --  TBD: Null out preconditions/postconditions for now
      --       since no substitution was performed.
      New_Tree.Preconditions := Null_Optional_Tree;
      New_Tree.Postconditions := Null_Optional_Tree;

      New_Tree.Operation_Inputs :=
         Lists.Make (New_Operands (Index .. Index + Num_Inputs - 1));
      Index := Index + Num_Inputs;
      New_Tree.Operation_Outputs :=
         Lists.Make (New_Operands (Index .. Index + Num_Outputs - 1));
      Index := Index + Num_Outputs;
      New_Tree.Global_Read_List :=
         Lists.Make (New_Operands (Index .. Index + Num_Global_Reads - 1));
      Index := Index + Num_Global_Reads;
      New_Tree.Global_Update_List :=
         Lists.Make (New_Operands (Index .. Index + Num_Global_Updates - 1));
      Index := Index + Num_Global_Updates;
      if Not_Null (T.Statements) then
         New_Tree.Statements := New_Operands (Index);
         Index := Index + 1;
      end if;
      if Not_Null (T.Op_Equiv) then
         New_Tree.Op_Equiv := New_Operands (Index);
         Index := Index + 1;
      end if;
      if Not_Null (T.Op_Location) then
         New_Tree.Op_Location := New_Operands (Index);
         Index := Index + 1;
      end if;
      return Optional (New_Tree);
   end Substitute_Operands;

   procedure Display_Subtree
     (T : Tree;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Produce a human readable display of a subtree, at the given indent
      --  If Use_Short_Form is True, then elide some of the output for
      --  a module or an operation.
      use PSC.Stream_Output;
      Indent_For_Inputs : Natural := 0;
      Param_Separator : Character := ';';
      Num_Inputs : constant Natural := Lists.Length (T.Operation_Inputs);
      Num_Outputs : constant Natural := Lists.Length (T.Operation_Outputs);
      Num_Global_Reads : constant Natural :=
        Lists.Length (T.Global_Read_List);
      Num_Global_Updates : constant Natural :=
        Lists.Length (T.Global_Update_List);
   begin
      --  Display import clauses, if any
      if not Use_Short_Form
        and then not Lists.Is_Empty (T.Import_Clauses)
      then
         Put_Line (On, "import", Indent => Indent);
         Display_List (T.Import_Clauses, On,
           Separator => ",",
           Terminator => ";",
           Indent => Indent + 2);
      end if;

      --  Display operation at given indent
      Put_Indent (On, Indent);
      if T.Is_Abstract then
         Put (On, "abstract ");
      elsif T.Is_Optional then
         Put (On, "optional ");
      end if;
      if T.Is_Queued then
         Put (On, "queued ");
      end if;
      case T.Operation_Kind is
         when Operator_Operation =>
            Put (On, "operator ");
         when Function_Operation =>
            Put (On, "function ");
         when Func_Operation =>
            Put (On, "func ");
         when Func_Type_Specifier =>
            case Languages.Language is
               when Languages.ParaSail | Languages.Parython |
                 Languages.Javallel | Languages.Sparkel =>
                  Put (On, "func ");
               when Languages.Ada202x =>
                  Put (On, "access function ");
            end case;
         when Procedure_Operation =>
            Put (On, "procedure ");
         when Proc_Operation =>
            Put (On, "proc ");
         when Proc_Type_Specifier =>
            case Languages.Language is
               when Languages.ParaSail | Languages.Parython |
                 Languages.Javallel | Languages.Sparkel =>
                  Put (On, "proc ");
               when Languages.Ada202x =>
                  Put (On, "access procedure ");
            end case;
         when Lambda_Operation =>
            Put (On, "lambda ");
            Param_Separator := ',';
         when Op_Operation =>
            Put (On, "op ");
         when Protected_Proc_Type =>
            Put (On, "access protected procedure ");
         when Protected_Func_Type =>
            Put (On, "access protected function ");
      end case;

      Display_Subtree (T.Name, On);

      if (Num_Inputs + Num_Global_Reads + Num_Global_Updates) > 0
        and then ((T.Is_Def and then Not_Null (T.Name)) or else Indent > 0)
      then
         New_Line (On, Indent => Indent + 2);
         Indent_For_Inputs := Indent + 3;
      end if;
      Put (On, "(");
      Display_List
        (T.Operation_Inputs,
         On,
         Separator => Param_Separator & ' ',
         Indent => Indent_For_Inputs,
         Use_Short_Form => Use_Short_Form);

      if Num_Inputs > 0
        and then Num_Global_Reads + Num_Global_Updates > 0
      then
         Put (On, (1 => Param_Separator));
         New_Line (On, Indent_For_Inputs);
      end if;

      if Num_Global_Reads > 0 then
         Put (On, "global ");
         Display_List
           (T.Global_Read_List,
            On,
            Separator => ", ",
            Indent => 0,
            Use_Short_Form => Use_Short_Form);

         if Num_Global_Updates > 0 then
            Put (On, (1 => Param_Separator));
            New_Line (On, Indent_For_Inputs);
         end if;

      end if;

      if Num_Global_Updates > 0 then
         Put (On, "global var ");
         Display_List
           (T.Global_Update_List,
            On,
            Separator => ", ",
            Indent => 0,
            Use_Short_Form => Use_Short_Form);
      end if;

      Put (On, ")");

      if not Use_Short_Form and then Not_Null (T.Preconditions) then
         if Num_Inputs > 1 or else Indent > 0 then
            New_Line (On, Indent => Indent + 2);
         else
            Put (On, ' ');
         end if;
         Display_Subtree (T.Preconditions, On, Indent => Indent + 3);
      end if;

      if T.Operation_Kind = Lambda_Operation then
         case Languages.Language is
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               Put (On, " -> ");
            when Languages.Ada_Ish =>
               Put (On, " is ");
         end case;
         Display_Subtree (T.Statements, On);
         return;   --  All done  --
      end if;

      if Num_Outputs > 0 then
         if (T.Is_Def and then Not_Null (T.Name)) or else Indent > 0 then
            New_Line (On, Indent => Indent + 2);
         else
            Put (On, ' ');
         end if;
         case Languages.Language is
            when Languages.ParaSail | Languages.Parython |
              Languages.Javallel =>
               Put (On, "-> ");
            when Languages.Ada_Ish =>
               Put (On, "return ");
         end case;
         if Num_Outputs > 1
           or else (Indent = 0 and then not T.Is_Def)
         then
            Put (On, "(");
            Display_List (T.Operation_Outputs, On, Separator => "; ",
              Use_Short_Form => Use_Short_Form);
            Put (On, ")");
         else
            Display_List (T.Operation_Outputs, On, Separator => "; ",
              Use_Short_Form => Use_Short_Form);
         end if;
      end if;

      if not Use_Short_Form and then Not_Null (T.Postconditions) then
         if (Num_Inputs + Num_Global_Reads + Num_Global_Updates) > 1
           or else Length (T.Operation_Outputs) > 0
           or else Indent > 0
         then
            New_Line (On, Indent => Indent + 2);
         else
            Put (On, ' ');
         end if;
         Display_Subtree (T.Postconditions, On, Indent => Indent + 3);
      end if;

      if not T.Is_Def then
         if Not_Null (T.Statements) then
            --  Default for operation formal
            Put (On, " is ");
            Display_Subtree (T.Statements, On);
         elsif Indent > 0 then
            --  End with a newline.
            New_Line (On);
         end if;
      elsif Not_Null (T.Op_Equiv) or else Not_Null (T.Op_Location) then
         --  Op-Equiv or Op-Location is set
         Put (On, " is ");
         if Not_Null (T.Op_Equiv) then
            --  Op-Equiv is set
            Display_Subtree (T.Op_Equiv, On);
         end if;
         if Not_Null (T.Op_Location) then
            --  Op-Location is (also) set
            case Languages.Language is
               when Languages.ParaSail | Languages.Parython |
                    Languages.Javallel =>
                  Put (On, " in ");
               when Languages.Ada_Ish =>
                  Put (On, " of ");
            end case;
            Display_Subtree (T.Op_Location, On);
         end if;
      elsif T.Is_Expression_Function then
         --  Expression function
         Put (On, " is ");
         Display_Subtree (T.Statements, On);
      elsif Use_Short_Form then
         Put (On, " is ...");
         New_Line (On);
      else
         --  Is an operation definition
         New_Line (On, Indent => Indent + 1);
         Put (On, "is");
         if T.Is_Import then
            Put (On, " import");
            Put (On, "(");
            Display_List (T.Import_Info, On, Separator => ", ");
            Put (On, ")");
            return;   --- All done ---
         end if;
         if Not_Null (T.Dequeue_Condition) then
            --  Put out the dequeue condition
            declare
               Dq_Cond : PSC.Trees.Tree'Class renames Tree_Of
                                                        (T.Dequeue_Condition);
               use type Unary.Unary_Operator_Enum;
            begin
               Put (On, " queued ");
               if Dq_Cond in Unary.Tree'Class
                 and then Unary.Tree (Dq_Cond).Operator = Unary.Not_Op
               then
                  --  queued until not X => queued while X
                  Put (On, "while ");
                  Display_Subtree (Unary.Tree (Dq_Cond).Operand, On);
               else
                  Put (On, "until ");
                  Display_Subtree (T.Dequeue_Condition, On);
               end if;
               Put_Line (On, " then");
            end;
         else
            New_Line (On);
         end if;

         Display_Subtree (T.Statements, On, Indent => Indent + 2);

         if not T.Is_Import then
            Put_Line (On, ";");
            Put_Indent (On, Indent);
            Put (On, "end ");

            case T.Operation_Kind is
               when Operator_Operation =>
                  Put (On, "operator ");
               when Function_Operation | Protected_Func_Type =>
                  null;  --  Nothing at end of function in Ada
               when Procedure_Operation | Protected_Proc_Type =>
                  null;  --  Nothing at end of procedure in Ada
               when Lambda_Operation | Func_Type_Specifier
                 | Proc_Type_Specifier =>
                  null;  --  Nothing at end of lambda or func-type
               when Func_Operation =>
                  Put (On, "func ");
               when Proc_Operation =>
                  Put (On, "proc ");
               when Op_Operation =>
                  Put (On, "op ");
            end case;

            Display_Subtree (T.Name, On);
         end if;  --  not Import
      end if;  --  whether Is_Def
   end Display_Subtree;

   use PSC.Trees.Visitor;

   procedure Visit (T : Tree; Visitor : in out Root_RO_Tree_Visitor'Class) is
   --  Call appropriate RO *_Action procedure on Visitor
   begin
      Operation_Action (RO_Tree_Visitor'Class (Visitor), T);
   end Visit;

   procedure Visit
     (T : in out Tree;
      Visitor : in out Root_RW_Tree_Visitor'Class) is
   --  Call appropriate RW *_Action procedure on Visitor
   begin
      Operation_Action (RW_Tree_Visitor'Class (Visitor), T);
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
            if Lists.Length (T.Operation_Inputs) > 0 then
               T_Source_Pos :=
                  Find_Source_Pos (Nth_Element (T.Operation_Inputs, 1));
            elsif Lists.Length (T.Global_Read_List) > 0 then
               T_Source_Pos :=
                  Find_Source_Pos (Nth_Element (T.Global_Read_List, 1));
            elsif Lists.Length (T.Global_Update_List) > 0 then
               T_Source_Pos :=
                  Find_Source_Pos (Nth_Element (T.Global_Update_List, 1));
            end if;
         end if;
         if T_Source_Pos = Null_Source_Position then
            T_Source_Pos := Find_Source_Pos (T.Statements);
         end if;
      end if;

      return T_Source_Pos;
   end Find_Source_Pos;

end PSC.Trees.Operation;
