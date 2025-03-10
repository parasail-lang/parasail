------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
--                Originally developed by S. Tucker Taft                    --
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
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with PSC.Hash_Tables;
with PSC.Interpreter;
with PSC.Interpreter.Builtins;
with PSC.Object_Access;
with PSC.Languages;
with PSC.Messages;
with PSC.Strings;                     use type PSC.Strings.U_String;
with PSC.Symbols;                     use PSC.Symbols;

with PSC.Trees.Lists;
with PSC.Trees.Visitor;

with PSC.Trees.Annotation;
with PSC.Trees.Assign_Stmt;
with PSC.Trees.Binary;
with PSC.Trees.Block_Stmt;
with PSC.Trees.Case_Construct;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Conditional;
with PSC.Trees.For_Loop_Construct;
with PSC.Trees.Identifier;
with PSC.Trees.Implements_Element;
with PSC.Trees.Invocation;
with PSC.Trees.Iterator;
with PSC.Trees.Module;
with PSC.Trees.Operation;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Param_Decl;
with PSC.Trees.Property;
with PSC.Trees.Qualifier;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Reference;
with PSC.Trees.Selection;
with PSC.Trees.Type_Decl;
with PSC.Trees.Unary;
with PSC.Trees.While_Stmt;

with PSC.Trees.Semantics.Debug; use PSC.Trees.Semantics.Debug;

with PSC.Trees.Semantics.Static;

with PSC.Univ_Strings;
with PSC.Vectors;

pragma Elaborate (PSC.Strings);

package body PSC.Trees.Semantics.Dynamic is

   --  Dynamic Semantics and PSVM Code Generation

   Checking_Preconditions : Boolean := True;
   --  Whether we are emitting precondition checks.
   --  TBD: Provide a way to control this with an annotation and/or a flag

   Checking_Postconditions : Boolean := True;
   --  Whether we are emitting Check_Not_Null_Op operations
   --  and other postcondition checks.
   --  TBD: Provide a way to control this with an annotation and/or a flag

   Debug_Entry_Exit : constant Boolean := False;

   type Entry_Exit_Enum is
      --  Enumeration of kinds of expressions that might appear in
      --  a precondition or a postcondition.
     (Entry_Computation,   --  Read of Initial value of Var
      Stable_Computation,  --  Read of an Input parameter or other constant
      Exit_Computation);   --  Read of Final value of Var or Output

   Debug_VM_Info : Boolean := True;
   --  Whether to complain if VM_Obj_Id info not filled in

   Use_Faster_Calling_Convention : constant Boolean := True;
   --  Whether to allow a faster calling convention for calling
   --  non-queued operations.

   Avoid_Parallel_Calls : constant Boolean :=
     Use_Faster_Calling_Convention;
   --  Whether to minimize number of parallel calls, so can use
   --  a faster calling convention.

   First_Unfinished_Type : Interpreter.Type_Descriptor_Ptr := null;
   Last_Unfinished_Type : Interpreter.Type_Descriptor_Ptr := null;
   --  Type descriptors that are not yet finished.

   First_Needing_Cur_Inst_Info : Interpreter.Type_Descriptor_Ptr := null;
   Last_Needing_Cur_Inst_Info : Interpreter.Type_Descriptor_Ptr := null;
   --  Other type descriptors that need cur-inst-param info filled in.

   subtype Code_Length_Type is Interpreter.Code_Length_Type;

   --  NOTE: These two "length"s are intentionally *not* constants
   --        so the corresponding objects end up on the secondary stack;
   --        to avoid overflowing any primary stack limits.
   --        Also, this would allow us to have a (TBD) command-line option
   --        to override these limits.
   --  TBD: Code-blocks should be extensible, e.g. use an extensible vector.

   pragma Warnings (Off);
   --  Maximum PSVM instructions per operation
   Max_Code_Length : Code_Length_Type := 30_000;

   --  Maximum PSVM instructions per block within operation
   Max_Block_Length : Code_Length_Type := 15_000;
   pragma Warnings (On);

   --  We allocate code blocks in the heap rather than on the stack
   --  to avoid excessive stack sizes (problem on some hosts).
   procedure Free_Code is new Ada.Unchecked_Deallocation
     (Interpreter.Code_Type, Interpreter.Code_Ptr);

   Access_Map : constant array (Param_Decl.Param_Kind) of
     Object_Access.Access_Mode_Enum :=
       --  Map from Parameter kind to object access mode.
       (Param_Decl.Default_Param => Object_Access.Read_Access,
        Param_Decl.Out_Param => Object_Access.Update_Access,
        Param_Decl.Var_Param => Object_Access.Update_Access,
        Param_Decl.Ref_Param => Object_Access.Ref_Access,
        Param_Decl.Ref_Const_Param => Object_Access.Ref_Const_Access,
        Param_Decl.Ref_Out_Param => Object_Access.Ref_Var_Access,
        Param_Decl.Ref_Var_Param => Object_Access.Ref_Var_Access,
        --  TBD: Should the next two be No_Access since must be concurrent?
        Param_Decl.Global_Param => Object_Access.Read_Access,
        Param_Decl.Global_Out_Param => Object_Access.Update_Access,
        Param_Decl.Global_Var_Param => Object_Access.Update_Access);

   package Anon_Const_Tables is new PSC.Hash_Tables
     (Element_Type => Obj_Sem_Info_Index,
      Key_Type => Optional_Tree,
      Equiv => Static.Equiv_Tree,
      Hash_Type => Hash_Type,
      Hash => Static.Hash_Tree);

   type Anon_Const_Table_Type is new Anon_Const_Tables.Hash_Table;

   Anon_Const_Table : Anon_Const_Table_Type;
   --  Hash table used for keeping track of anonymous constants
   --  that will be pre-computed to avoid recomputing on every occurrence.
   --  Initially will only be used for constants of small and string types.

   type CTK_Annotation_Info is record
      --  Information kept for annotations that can be checked
      --  at compile time, after global constants have been
      --  evaluated.
      CTK_Annotation : Optional_Tree;  --  Used to display message
      CTK_Index : Obj_Sem_Info_Index;
         --  Index into both Compile_Time_Known_Const_Table
         --  and Interpreter.Compile_Time_Known_Consts.
   end record;

   package CTK_Annotation_Vectors is new PSC.Vectors (CTK_Annotation_Info);

   type CTK_Annotation_Vector_Type is new CTK_Annotation_Vectors.Vector;

   CTK_Annotation_Vector : CTK_Annotation_Vector_Type;
   --  Vector of annotations to be checked at compile-time

   ------------- Pre codegen actions ------------

   subtype Postcondition_Mode is Visitor_Annotation_Mode_Enum
     range Postcondition_Mode_No_Check .. Postcondition_Mode_With_Check;
   subtype Pre_Post_Condition_Mode is Visitor_Annotation_Mode_Enum
     range Precondition_Mode .. Postcondition_Mode'Last;

   type Pre_Cg_Visitor is new Visitor.RW_Tree_Visitor with record
      Read_Write : Object_Access.Read_Write_Mapping;
      --  Records kinds of object accesses performed within subtree.
      Mode : Object_Access.Access_Mode_Enum := Object_Access.Read_Access;
      --  Indicates mode of access of object.
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode;
      --  Indicates whether visiting annotation expression
      --  for pre/postcondition.
   end record;

   procedure Pre_Visit
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Trees.Tree'Class);

   procedure Post_Visit
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Trees.Tree'Class);

   procedure Module_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Module.Tree);

   procedure Implements_Element_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Implements_Element.Tree);

   procedure Operation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Operation.Tree);

   procedure Obj_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Obj_Decl.Tree);

   procedure Param_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Param_Decl.Tree);

   procedure Type_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Type_Decl.Tree);

   procedure Unary_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Unary.Tree);

   procedure Binary_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Binary.Tree);

   procedure Annotation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Annotation.Tree);

   procedure Identifier_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Identifier.Tree);

   procedure Property_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Property.Tree);

   procedure Qualified_Name_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Qualified_Name.Tree);

   procedure Assign_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Assign_Stmt.Tree);

   procedure Invocation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Invocation.Tree);

   procedure Block_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Block_Stmt.Tree);

   procedure Case_Construct_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Case_Construct.Tree);

   procedure Control_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Control_Stmt.Tree);

   procedure Conditional_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Conditional.Tree);

   procedure For_Loop_Construct_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out For_Loop_Construct.Tree);

   procedure Iterator_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Iterator.Tree);

   procedure While_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out While_Stmt.Tree);

   procedure Selection_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Selection.Tree);

   -----------

   function Combine_Operand_Calls
     (Left, Right : Slow_Call_Enum)
      return Slow_Call_Enum;
   --  Combine slow-call information from two operands into a single
   --  value

   function Slow_Calls (T : Optional_Tree) return Slow_Call_Enum is
   --  Return slow-call info from given operand
   begin
      if Is_Null (T) then
         --  Null tree, not a slow call
         return No_Slow_Calls;
      else
         declare
            Opnd_Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (T));
         begin
            if Opnd_Sem /= null
              and then Opnd_Sem.all in Computation_Semantic_Info'Class
            then
               --  Computation, retrieve slow calls from there
               return Computation_Semantic_Info (Opnd_Sem.all).Slow_Calls;
            elsif Tree_Ptr_Of (T).all in Reference.Tree then
               --  Treat reference as a special case
               declare
                  Ref : Reference.Tree renames
                   Reference.Tree (Tree_Ptr_Of (T).all);
               begin
                  return Combine_Operand_Calls (Slow_Calls (Ref.Key),
                    Slow_Calls (Ref.Referent));
               end;
            else
               --  No computation info, so presume no slow call
               --  TBD: Might be a bad assumption, e.g. might be
               --      a conditional or quantified expr
               return No_Slow_Calls;
            end if;
         end;
      end if;
   end Slow_Calls;

   function Num_Nested_Blocks (T : Optional_Tree) return Natural is
   --  Return num-nested-blocks info from given operand
   begin
      if Is_Null (T) then
         --  Null tree, no nested blocks
         return 0;
      else
         declare
            Opnd_Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (T));
         begin
            if Opnd_Sem /= null
              and then Opnd_Sem.all in Computation_Semantic_Info'Class
            then
               --  Computation, retrieve num-nested-blocks from there
               return
                 Computation_Semantic_Info (Opnd_Sem.all).Num_Nested_Blocks;
            else
               --  No computation info, so presume no parallel calls
               return 0;
            end if;
         end;
      end if;
   end Num_Nested_Blocks;

   function Uses_Queuing (T : Optional_Tree) return Boolean is
   --  Return uses-queuing info from given operand
   begin
      if Is_Null (T) then
         --  Null tree, no queuing
         return False;
      else
         declare
            Opnd_Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (T));
         begin
            if Opnd_Sem /= null
              and then Opnd_Sem.all in Computation_Semantic_Info'Class
            then
               --  Computation, retrieve uses-queuing from there
               return Computation_Semantic_Info (Opnd_Sem.all).Uses_Queuing;
            else
               --  No computation info, so presume no queuing.
               --  TBD: Might be a bad assumption, e.g. might be
               --      a conditional or quantified expr
               return False;
            end if;
         end;
      end if;
   end Uses_Queuing;

   function Combine_Operand_Calls
     (Left, Right : Slow_Call_Enum)
      return Slow_Call_Enum
   is
   --  Combine slow-call information from two operands into a single
   --  value
   begin
      if Right = No_Slow_Calls then
         return Left;
      elsif Left = No_Slow_Calls then
         return Right;
      else
         --  We have some exploitable parallelism
         return Independent_Slow_Calls;
      end if;
   end Combine_Operand_Calls;

   function Combine_Operand_Calls
     (Operands : Lists.List)
      return Slow_Call_Enum
   is
      --  Combine slow-call information from a list of operands into
      --  a single value
      Result : Slow_Call_Enum := No_Slow_Calls;
   begin
      for I in 1 .. Lists.Length (Operands) loop
         Result :=
            Combine_Operand_Calls
              (Result,
               Slow_Calls (Lists.Nth_Element (Operands, I)));
      end loop;
      return Result;
   end Combine_Operand_Calls;

   function Combine_Operand_Blocks (Operands : Lists.List) return Natural is
      --  Combine nested-block information from a list of operands into
      --  a single value
      Result : Natural := 0;
   begin
      for I in 1 .. Lists.Length (Operands) loop
         Result := Result +
                   Num_Nested_Blocks (Lists.Nth_Element (Operands, I));
      end loop;
      return Result;
   end Combine_Operand_Blocks;

   function Combine_Operand_Queuing (Operands : Lists.List) return Boolean is
      --  Combine uses-queuing information from a list of operands into
      --  a single value
      Result : Boolean := False;
   begin
      for I in 1 .. Lists.Length (Operands) loop
         Result := Result or Uses_Queuing (Lists.Nth_Element (Operands, I));
      end loop;
      return Result;
   end Combine_Operand_Queuing;

   function Combine_Operands_With_Op
     (Is_Slow_Call : Boolean;
      Operand_Calls : Slow_Call_Enum)
      return Slow_Call_Enum
   is
   begin
      if not Is_Slow_Call then
         --  New call is a built-in, so just propagate
         --  operands.
         return Operand_Calls;
      else
         --  This is a slow call, combine with the operand
         case Operand_Calls is
            when No_Slow_Calls =>
               --  We now have a single slow call
               return Slow_Call;
            when Slow_Call | Slow_Call_Sequence =>
               --  We now have a sequence of slow calls, but no
               --  exploitable parallelism yet, since this new slow call
               --  depends on the results of the prior slow call.
               return Slow_Call_Sequence;
            when Mandatory_Parallel_Call       |
                 Slow_Call_Tree_Needing_Master |
                 Independent_Slow_Calls        =>
               --  We have a tree headed by a slow call, which
               --  has internal parallelism.
               return Slow_Call_Tree_Needing_Master;
         end case;
      end if;
   end Combine_Operands_With_Op;

   function Combine_Sequential_Computations
     (Left, Right : Slow_Call_Enum)
      return Slow_Call_Enum
   is
   --  Return combination Slow_Calls where Left and Right are alternatives
   --  (e.g. "then" part and "else" part of an "if")
   --  or sequential rather than being evaluated in parallel.
   begin
      --  We cleverly designed Slow_Call_Enum so 'Max does the right thing.
      return Slow_Call_Enum'Max (Left, Right);
   end Combine_Sequential_Computations;

   function Call_Is_Slow (Comp_Sem : Computation_Sem_Ptr) return Boolean is
   --  Return True if node is a call for which it is worth
   --  spawning a separate pico-thread.

   --  NOTE: At the moment, this is when the call is *not* an "import"
   --       or when the call might involve queuing.
   --  TBD: At some point, there might be more user-specifiable
   --      properties or other means of deciding this.
   begin
      return Comp_Sem /= null
        and then Comp_Sem.Op_Sem /= null
        and then ((not Follow_Op_Equiv (Comp_Sem.Op_Sem).Is_Import
                     and then Insert_Implicit_Parallel_Calls)
          or else (Comp_Sem.all in Call_Semantic_Info'Class
            and then Call_Sem_Ptr (Comp_Sem).Might_Be_Queued));
   end Call_Is_Slow;

   function Call_Uses_Queuing
     (Comp_Sem : Computation_Sem_Ptr)
      return Boolean
   is
   --  Return True if node is a call for which queuing might occur.

   begin
      return Comp_Sem /= null
            and then Comp_Sem.Op_Sem /= null
            and then (Comp_Sem.Op_Sem.Uses_Queuing
      --  TBD:    -- Might_Be_Queued is true too often at the moment.
      --  TBD:    -- should only be set if there is an internal
      --  TBD:    -- precondition.
      --  TBD: or else
      --  TBD: (Comp_Sem.all in Call_Semantic_Info'Class and then
      --  TBD: Call_Sem_Ptr(Comp_Sem).Might_Be_Queued)
        );

   end Call_Uses_Queuing;

   procedure Refer_To_Object
     (Read_Write : in out Object_Access.Read_Write_Mapping;
      Obj_Sem : Object_Sem_Ptr;
      Mode : Object_Access.Access_Mode_Enum;
      Source_Pos : Source_Positions.Source_Position) is
   --  Initialize Read_Write mapping based on a reference
   --  to the given object.
   begin
      if Obj_Sem /= null
        and then not Static.Is_Unlocked_Concurrent_Operand
          (Operand_Sem_Ptr (Obj_Sem))
        and then Static.Sem_Info_Is_For_Variable (Sem_Ptr (Obj_Sem))
      then
         if Obj_Sem.all not in Param_Semantic_Info
           and then Obj_Sem.all not in Iterator_Semantic_Info
           and then Static.Sym_Is_Declared_Ref (Obj_Sem.Associated_Symbol)
         then
            --  Object is a "ref" so we should have saved a read-write
            --  mapping for it.
            Object_Access.Refer_To_Ref
              (Read_Write,
               Obj_Sem.Object_Ref,
               Obj_Sem.Object_Id,
               Mode => Mode,
               Source_Pos => Source_Pos);
         else
            --  Treat by-ref parameters and by-ref loop variables specially
            Object_Access.Refer_To_Object
              (Read_Write,
               Obj_Sem.Object_Id,
               Mode => Mode,
               Source_Pos => Source_Pos);
         end if;
      end if;
   end Refer_To_Object;

   function Num_Finalizable_Temps (T : Optional_Tree) return Natural is
   --  Return num-finalizable-temp info from given operand
   begin
      if Is_Null (T) then
         --  Null tree, no nested blocks
         return 0;
      else
         declare
            Opnd_Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (T));
         begin
            if Opnd_Sem = null then
               --  No semantic info, so no temps
               return 0;
            elsif Opnd_Sem.all in Computation_Semantic_Info'Class then
               --  Computation, retrieve num-finalizable-temps from there,
               --  plus include temp potentially needed for conv to poly type.
               declare
                  Comp_Sem : Computation_Semantic_Info renames
                     Computation_Semantic_Info (Opnd_Sem.all);
               begin
                  return Comp_Sem.Num_Finalizable_Temps +
                      Boolean'Pos (Comp_Sem.Target_Polymorphic_Type /= null);
               end;
            elsif Opnd_Sem.all in Operand_Semantic_Info'Class
              and then
                  Operand_Semantic_Info
                    (Opnd_Sem.all).Target_Polymorphic_Type /= null
            then
               --  A conversion to a polymorphic type might require a
               --  finalizable temp.
               return 1;
            else
               --  No computation or conversion info,
               --  so presume no finalizable temps.
               return 0;
            end if;
         end;
      end if;
   end Num_Finalizable_Temps;

   function Combine_Operand_Temps (Operands : Lists.List) return Natural is
      --  Combine finalizable-temps information from a list of operands into
      --  a single value
      Result : Natural := 0;
   begin
      for I in 1 .. Lists.Length (Operands) loop
         Result := Result +
                   Num_Finalizable_Temps (Lists.Nth_Element (Operands, I));
      end loop;
      return Result;
   end Combine_Operand_Temps;

   function Find_End_Op_For is new Static.Find_Op_For (
      Static.End_Op_Str,
      Num_Inputs => 1,
      Num_Outputs => 0,
      Has_Desired_Signature => Static.First_Input_Is_Operand_Type);

   procedure Pre_Cg_Exit_Effects
     (Values : Optional_Tree;
      Exited_Stmt_Sem : Composite_Stmt_Sem_Ptr) is
   --  Pre-code-gen on assignments to specified objects, for an exit
   --  with values specified with <name> => <value>
   --  or <name> <== <value>

      Values_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Values).all;
      use Assign_Stmt;
   begin
      if Values_Tree in Reference.Tree then
         --  A single assignment
         Pre_Cg (Reference.Tree (Values_Tree).Referent,
                 Exited_Stmt_Sem.Exit_Effects_RW);
         Pre_Cg (Reference.Tree (Values_Tree).Key,
                 Exited_Stmt_Sem.Exit_Effects_RW,
                 Mode => Object_Access.Update_Access);
      elsif Values_Tree in Invocation.Tree then
         --  A list of assignments
         declare
            Values_Agg : Invocation.Tree renames Invocation.Tree (Values_Tree);
            use Invocation;
         begin
            if Values_Agg.Kind /= Class_Aggregate then
               Sem_Error (Values,
                 "Must be parenthesized list of Name => Value pairs");
            else
               --  Do each assignment
               for I in 1 .. Lists.Length (Values_Agg.Operands) loop
                  declare
                     Key_Val : constant Optional_Tree :=
                       Lists.Nth_Element (Values_Agg.Operands, I);
                     Key_Val_Tree : Trees.Tree'Class renames
                       Tree_Ptr_Of (Key_Val).all;
                  begin
                     if Key_Val_Tree in Reference.Tree then
                        --  Name => Value
                        --  Only Key is updated
                        Pre_Cg (Reference.Tree (Key_Val_Tree).Referent,
                                Exited_Stmt_Sem.Exit_Effects_RW);
                        Pre_Cg (Reference.Tree (Key_Val_Tree).Key,
                                Exited_Stmt_Sem.Exit_Effects_RW,
                                Mode => Object_Access.Update_Access);
                     elsif Key_Val_Tree in Assign_Stmt.Tree then
                        --  Name <== Value
                        --  Both Key and Referent are updated
                        Pre_Cg (Reference.Tree (Key_Val_Tree).Referent,
                                Exited_Stmt_Sem.Exit_Effects_RW,
                                Mode => Object_Access.Update_Access);
                        Pre_Cg (Reference.Tree (Key_Val_Tree).Key,
                                Exited_Stmt_Sem.Exit_Effects_RW,
                                Mode => Object_Access.Update_Access);
                     else
                        Sem_Error (Key_Val,
                          "Must be Name => Value or Name <== Value");
                     end if;
                  end;
               end loop;
            end if;
         end;
      else
         Sem_Error (Values, "Must be Name => Value or (N1 => V1, ...)");
      end if;
   end Pre_Cg_Exit_Effects;

   procedure Put_Entry_Exit_Info (Info : Entry_Exit_Info_Type) is
      --  Display entry/exit info on standard output
   begin
      Put_Line ("  Has_Entry_Value_Of_RO_Input => " &
        Boolean'Image (Info.Has_Entry_Value_Of_RO_Input));
      Put_Line ("  Has_Entry_Value_Of_Var => " &
        Boolean'Image (Info.Has_Entry_Value_Of_Var));
      Put_Line ("  Num_Entry_Temps =>" &
        Natural'Image (Info.Num_Entry_Temps));
      Put_Line ("  Has_Exit_Value_Of_Var => " &
        Boolean'Image (Info.Has_Exit_Value_Of_Var));
      Put_Line ("  Has_Exit_Value_Of_Output => " &
        Boolean'Image (Info.Has_Exit_Value_Of_Output));
   end Put_Entry_Exit_Info;

   function Entry_Exit_Info (T : Optional_Tree) return Entry_Exit_Info_Type is
   --  Return Entry_Exit_Info, if any, on tree
      Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (T));
   begin
      if Sem /= null and then Sem.all in Operand_Semantic_Info'Class then
         return Operand_Sem_Ptr (Sem).Entry_Exit_Info;
      else
         return Null_Entry_Exit_Info;
      end if;
   end Entry_Exit_Info;

   procedure Combine_Entry_Exit_Info (Result : in out Entry_Exit_Info_Type;
                                      Operand : Optional_Tree) is
      --  Update Result to incorporate Operand
      Sem : constant Root_Sem_Ptr := Sem_Info (Resolved_Tree (Operand));
   begin
      if Sem /= null and then Sem.all in Operand_Semantic_Info'Class then
         --  Operand has Entry/Exit info
         declare
            Operand_Sem : constant Operand_Sem_Ptr := Operand_Sem_Ptr (Sem);
            Operand_Info : Entry_Exit_Info_Type renames
              Operand_Sem.Entry_Exit_Info;

            --  Indicate that Operand needs a temp if would violate Invariant:
            --    Operand_Info.Has_Entry_Value_Of_Var ==>
            --               (not Result.Has_Exit_Value_Of_Var and
            --                not Result.Has_Exit_Value_Of_Output));
         begin
            if Operand_Info.Has_Entry_Value_Of_Var
              and then Operand_Sem.Entry_Temp_Info = null
              and then (Result.Has_Exit_Value_Of_Var
                          or else
                        Result.Has_Exit_Value_Of_Output)
            then
               --  Need to allocate a temp; set Entry_Temp_Info to non-null.
               Operand_Sem.Entry_Temp_Info := new Object_Location_Info;

               --  Keep count of total number of such temps
               --  Should not already have an entry temp somewhere inside.
               pragma Assert (Operand_Info.Num_Entry_Temps = 0);
               Operand_Info.Num_Entry_Temps := 1;

               --  Make sure the operand didn't violate the invariant
               pragma Assert (not Operand_Info.Has_Exit_Value_Of_Var
                           and then not Operand_Info.Has_Exit_Value_Of_Output);
               if Debug_Pre_Cg or Debug_Entry_Exit then
                  Put_Line ("Entry_Temp needed for " &
                    Subtree_Image (Operand_Sem.Definition));
               end if;
            end if;

            Result.Has_Entry_Value_Of_RO_Input :=
              Result.Has_Entry_Value_Of_RO_Input
                or
              Operand_Info.Has_Entry_Value_Of_RO_Input;

            if Operand_Sem.Entry_Temp_Info = null then
               --  Pass on "Has_Entry_Value_Of_Var" unless we are
               --  allocating a temp on this operand.
               Result.Has_Entry_Value_Of_Var :=
                 Result.Has_Entry_Value_Of_Var or
                   Operand_Info.Has_Entry_Value_Of_Var;
            end if;

            Result.Num_Entry_Temps := Result.Num_Entry_Temps +
              Operand_Info.Num_Entry_Temps;

            --  NOTE: Might violate invariant temporarily here!
            Result.Has_Exit_Value_Of_Var := Result.Has_Exit_Value_Of_Var or
              Operand_Info.Has_Exit_Value_Of_Var;

            Result.Has_Exit_Value_Of_Output :=
              Result.Has_Exit_Value_Of_Output or
                Operand_Info.Has_Exit_Value_Of_Output;
         end;
      end if;
   end Combine_Entry_Exit_Info;

   function Combine_Entry_Exit_Info (L : Lists.List)
     return Entry_Exit_Info_Type is
      --  Return combined Entry_Exit_Info for list,
      --  setting Entry_Temp_Info as appropriate.
      Result : Entry_Exit_Info_Type;
   begin
      for I in 1 .. Lists.Length (L) loop
         Combine_Entry_Exit_Info (Result, Lists.Nth_Element (L, I));
      end loop;
      if Result.Has_Entry_Value_Of_Var
        and then (Result.Has_Exit_Value_Of_Var
                    or
                  Result.Has_Exit_Value_Of_Output)
      then
         --  Do it again, since we apparently need to create a temp on an
         --  earlier operand.
         if Debug_Pre_Cg then
            Put_Line ("Repeating walk of operands to compute Entry_Exit_Info");
         end if;
         Result.Has_Entry_Value_Of_Var := False;
         Result.Num_Entry_Temps := 0;
         for I in 1 .. Lists.Length (L) loop
            Combine_Entry_Exit_Info (Result, Lists.Nth_Element (L, I));
         end loop;

         --  Should not have any entry values now
         pragma Assert (not Result.Has_Entry_Value_Of_Var
                           and then
                        Result.Num_Entry_Temps > 0);
      end if;

      if Debug_Pre_Cg or Debug_Entry_Exit then
         Put ("Entry_Exit_Info for (");
         for I in 1 .. Lists.Length (L) loop
            if I > 1 then
               Put (", ");
            end if;
            Put (Subtree_Image (Lists.Nth_Element (L, I)));
         end loop;
         Put_Line (")");
         Put_Entry_Exit_Info (Result);
      end if;

      return Result;
   end Combine_Entry_Exit_Info;

   function Combine_Entry_Exit_Info (Left, Right : Optional_Tree)
     return Entry_Exit_Info_Type is
      --  Return combined Entry_Exit_Info for Left and Right,
      --  setting Entry_Temp_Info as appropriate.
      Result : Entry_Exit_Info_Type;
   begin
      Combine_Entry_Exit_Info (Result, Left);
      Combine_Entry_Exit_Info (Result, Right);

      if Result.Has_Entry_Value_Of_Var
        and then (Result.Has_Exit_Value_Of_Var
                    or
                  Result.Has_Exit_Value_Of_Output)
      then
         --  Do it again, since we apparently need to create a temp on an
         --  earlier operand.
         if Debug_Pre_Cg then
            Put_Line ("Repeating walk of operands to compute Entry_Exit_Info");
         end if;
         Result.Has_Entry_Value_Of_Var := False;
         Result.Num_Entry_Temps := 0;
         Combine_Entry_Exit_Info (Result, Left);
         Combine_Entry_Exit_Info (Result, Right);

         --  Should not have any entry values now
         pragma Assert (not Result.Has_Entry_Value_Of_Var
                           and then
                        Result.Num_Entry_Temps > 0);
      end if;

      if Debug_Pre_Cg or Debug_Entry_Exit then
         Put_Line ("Entry_Exit_Info for (" &
           Subtree_Image (Left) & ", " & Subtree_Image (Right) & ")");
         Put_Entry_Exit_Info (Result);
      end if;

      return Result;
   end Combine_Entry_Exit_Info;

   procedure Compute_Entry_Exit_Info
     (Obj_Ref : Sym_Ref_Ptr;
      Annotation_Mode : Visitor_Annotation_Mode_Enum) is
      --  Initialize the Entry_Exit_Info in the Obj_Ref info
      Obj_Sem : constant Sem_Ptr := Underlying_Sem_Info (Sem_Ptr (Obj_Ref));
   begin
      if Obj_Sem.all in Param_Semantic_Info
        and then Param_Sem_Ptr (Obj_Sem).Is_Operation_Output
      then
         --  Mentioning an output in postcondition
         if Annotation_Mode not in Postcondition_Mode then
            Sem_Warning ("Referring to func result in a precondition",
              Src_Pos => Find_Source_Pos (Obj_Sem.Definition));
         end if;
         Obj_Ref.Entry_Exit_Info.Has_Exit_Value_Of_Output := True;
      elsif Annotation_Mode in Postcondition_Mode
        and then Obj_Sem.Associated_Symbol /= null
        and then Obj_Sem.Associated_Symbol.Kind in
          Loop_Param_Sym_Kind .. Loop_Key_Param_Sym_Kind
      then
         --  For a loop parameter reference, we copy the information
         --  from the loop parameter semantic info, which was
         --  set prior to walking the loop body.
         --  TBD: We really don't want to combine a loop parameter
         --       ref in an entry-temp computation, so we might
         --       want a separate flag like "never pre-compute"
         --       (though if we can pre-compute the whole loop, that's OK!).
         Obj_Ref.Entry_Exit_Info := Param_Sem_Ptr (Obj_Sem).Entry_Exit_Info;
      elsif Static.Sem_Info_Is_For_Variable (Obj_Sem) then
         --  In ParaSail, mentioning a var means the entry value
         --  unless inside an "updated_value" op (handled by caller)
         --  In Ada, mentioning a var means the exit value
         --  unless inside a "'Old" reference (handled by caller)
         if Languages.Language in Languages.Ada_Ish then
            Obj_Ref.Entry_Exit_Info.Has_Exit_Value_Of_Var := True;
         else
            Obj_Ref.Entry_Exit_Info.Has_Entry_Value_Of_Var := True;
         end if;
      elsif Obj_Sem.all in Param_Semantic_Info then
         --  An RO parameter
         Obj_Ref.Entry_Exit_Info.Has_Entry_Value_Of_RO_Input := True;
      end if;
      if Debug_Pre_Cg or Debug_Entry_Exit then
         Put_Line ("Compute_Entry_Exit_Info for " &
           Subtree_Image (Obj_Sem.Definition));
         Put_Entry_Exit_Info (Obj_Ref.Entry_Exit_Info);
      end if;
   end Compute_Entry_Exit_Info;

   --------

   procedure Pre_Visit
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Trees.Tree'Class) is
   begin
      if Not_Null (T.Pre_Annotation) then
         if Debug_Pre_Cg then
            Put_Line (" Pre_Cg on Pre annotation: " &
              Subtree_Image (T.Pre_Annotation) & " of: " &
              Subtree_Image (T));
         end if;

         Visit (T.Pre_Annotation, Visitor);
         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            if T.Sem_Info /= null
              and then T.Sem_Info.all in Operand_Semantic_Info'Class
            then
               Combine_Entry_Exit_Info
                 (Operand_Sem_Ptr (T.Sem_Info).Entry_Exit_Info,
                  T.Pre_Annotation);
            end if;
         end if;
      end if;
   end Pre_Visit;

   procedure Post_Visit
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Trees.Tree'Class) is
      Num_Annotations : Natural := 0;
   begin
      if Not_Null (T.Pre_Annotation) then
         Num_Annotations := 1;
      end if;

      if Not_Null (T.Post_Annotation) then
         if Debug_Pre_Cg then
            Put_Line (" Pre_Cg on Post annotation: " &
              Subtree_Image (T.Post_Annotation) & " of: " &
              Subtree_Image (T));
         end if;

         Visit (T.Post_Annotation, Visitor);
         Num_Annotations := Num_Annotations + 1;

         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            if T.Sem_Info /= null
              and then T.Sem_Info.all in Operand_Semantic_Info'Class
            then
               Combine_Entry_Exit_Info
                 (Operand_Sem_Ptr (T.Sem_Info).Entry_Exit_Info,
                  T.Post_Annotation);
            end if;
         end if;
      end if;

      if Num_Annotations > 0
        and then T.Sem_Info /= null
        and then T.Sem_Info.all in Computation_Semantic_Info'Class
      then
         --  Add two for each annotation (in case it turns out to be
         --  a postcondition with entry temps).
         Computation_Sem_Ptr (T.Sem_Info).Num_Nested_Blocks :=
           Computation_Sem_Ptr (T.Sem_Info).Num_Nested_Blocks +
           2 * Num_Annotations;
      end if;
   end Post_Visit;

   procedure Module_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Module.Tree) is
      Mod_Sem : Module_Semantic_Info renames
        Module_Semantic_Info (T.Sem_Info.all);
      Mod_Region : constant Symbols.Region_Ptr := Mod_Sem.Nested_Region;

      New_Mod : constant Symbols.Sym_Ptr := Mod_Sem.Associated_Symbol;
      Read_Write : Object_Access.Read_Write_Mapping;
   begin
      if Debug_Pre_Cg then
         Put_Line
           ("Pre codegen code for module " &
            Sym_Name (New_Mod) &
            "; sym_index =" &
            Sym_Index'Image (New_Mod.Index));
      end if;

      if not Lists.Is_Empty (T.Class_Locals) then
         if Debug_Pre_Cg then
            Put_Line
              ("  Pre codegen for Locals for module " &
               Sym_Name (New_Mod) &
               ':');
         end if;
         Pre_Cg_List (T.Class_Locals, Read_Write, Object_Access.Not_Combined);
      end if;

      if not Lists.Is_Empty (T.Module_Exports)
        or else not Lists.Is_Empty (T.Module_New_Exports)
        or else not Lists.Is_Empty (T.Module_Implements)
      then
         if Debug_Pre_Cg then
            Put_Line
              ("  Pre codegen for Exports for module " &
               Sym_Name (New_Mod) &
               ':');
         end if;
         Pre_Cg_List
           (T.Module_Exports,
            Read_Write,
            Object_Access.Not_Combined);
         Pre_Cg_List
           (T.Module_New_Exports,
            Read_Write,
            Object_Access.Not_Combined);
         Pre_Cg_List
           (T.Module_Implements,
            Read_Write,
            Object_Access.Not_Combined);
      end if;

      if Debug_Pre_Cg then
         Put_Line ("End of pre codegen for module " & Sym_Name (New_Mod));
      end if;

   end Module_Action;

   procedure Implements_Element_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Implements_Element.Tree) is
      Read_Write : Object_Access.Read_Write_Mapping;
   begin
      if not Lists.Is_Empty (T.Elements) then
         if Debug_Pre_Cg then
            if not Lists.Is_Empty (T.For_Interfaces) then
               Put ("  Pre codegen for implements section for ");
               for I in 1 .. Lists.Length (T.For_Interfaces) loop
                  if I > 1 then
                     Put (", ");
                  end if;
                  Put
                    (Subtree_Image (Lists.Nth_Element (T.For_Interfaces, I)));
               end loop;
               New_Line;
            else
               Put_Line ("  Pre codegen for implements-for-all section");
            end if;
         end if;
         --  TBD: Put these in separate sub-regions (at least those with
         --      a non-null For_Interfaces list).
         Pre_Cg_List (T.Elements, Read_Write, Object_Access.Not_Combined);
      end if;
   end Implements_Element_Action;

   procedure Operation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Operation.Tree) is
      Op_Sem : constant Operation_Sem_Ptr :=
        Operation_Sem_Ptr (Underlying_Op_Sem_Info (Sem_Ptr (T.Sem_Info)));

      New_Op : constant Symbols.Sym_Ptr := Op_Sem.Associated_Symbol;
      use Interpreter;
      Read_Write : Object_Access.Read_Write_Mapping;
      use type Operation.Operation_Kind_Enum;
      Orig_Uplevel_Ref_Count_At_Call : Integer :=
        Op_Sem.Uplevel_Ref_Count_At_Call;
      Num_Outputs : constant Natural :=
        Lists.Length (T.Operation_Outputs);

      function Op_Conv_Desc return Convention_Descriptor is
         --  Return convention descriptor based on inputs/outputs/...
      begin
         if Num_Outputs = 0 then
            --  No output to worry about
            return New_Conv_Desc (Op_Sem.Convention,
              Num_Inputs =>  Lists.Length (T.Operation_Inputs),
              Num_Outputs => 0,
              Output_Needs_Init => False,
              Uses_Queuing => False);  --  TBD
         else
            --  Figure out whether the output is known to be small
            --  and hence doesn't need to be pre-initialized.
            declare
               Result_Param_Tree : constant Param_Decl.Tree :=
                 Param_Decl.Tree (Tree_Ptr_Of
                   (Lists.Nth_Element
                     (T.Operation_Outputs, 1)).all);
               Result_Param : constant Param_Sem_Ptr :=
                 Param_Sem_Ptr (Result_Param_Tree.Sem_Info);
               Result_Is_By_Ref : constant Boolean :=
                 Static.Param_Is_Passed_By_Ref
                  (Result_Param,
                   Result_Param_Tree.Kind,
                   Result_Param_Tree.Locking);
            begin
               return New_Conv_Desc (Op_Sem.Convention,
                 Num_Inputs =>  Lists.Length (T.Operation_Inputs),
                 Num_Outputs => Num_Outputs,
                 Uses_Queuing => False,  --  TBD
                 Output_Needs_Init =>  --  Needs init if large or optional
                   not Result_Is_By_Ref  --  and *not* passed by ref
                     and then
                   (Result_Param.Resolved_Type = null
                     or else
                       not Static.Known_To_Be_Small
                             (Result_Param.Resolved_Type)
                     or else
                       Result_Param.Resolved_Type.Value_Is_Optional
                     or else
                       Result_Param_Tree.Is_Optional));
                        --  TBD: Only reason to worry about "optional"
                        --       when returning small values in registers
                        --       is if operation itself might be "null"
                        --       since then the representation of null
                        --       might not be known.  Might find a better
                        --       way to deal with "null" operations...
            end;
         end if;
      end Op_Conv_Desc;

   begin  --  Operation_Action

      --  Walk any annotations on the inputs (treated as preconditions)
      if Checking_Preconditions then
         Pre_Cg_List (T.Operation_Inputs, Read_Write,
                      Annotation_Mode => Precondition_Mode);
         Pre_Cg (T.Preconditions, Read_Write,
                 Annotation_Mode => Precondition_Mode);
      end if;

      --  Initialize to false, and then check at end to decide whether
      --  to rerun Pre_CG.
      Op_Sem.Rerun_Pre_CG := False;

      if Op_Sem.Spec_Sem /= null
        and then Op_Sem.Spec_Sem.Uplevel_Ref_Count_At_Call <
                   Orig_Uplevel_Ref_Count_At_Call
      then
         --  Remember spec uplevel-ref count if smaller
         Orig_Uplevel_Ref_Count_At_Call :=
           Op_Sem.Spec_Sem.Uplevel_Ref_Count_At_Call;
      end if;

      if Debug_Pre_Cg then
         Put_Line
           ("Pre codegen for operation " &
            Sym_Name (New_Op) &
            "; sym_index =" &
            Sym_Index'Image (New_Op.Index));

      end if;

      if Op_Sem.Equiv_To /= null then
         --  We may need to "indirect" through the Equiv_To information
         --  since the code-gen phase doesn't follow such indirections.
         while Op_Sem.Equiv_To.Equiv_To /= null loop
            Op_Sem.Equiv_From_Type := Static.Substitute_Actuals
              (Op_Sem.Equiv_To.Equiv_From_Type,
               U_Base_Type_Region (Op_Sem.Equiv_From_Type));
            Op_Sem.Equiv_To := Op_Sem.Equiv_To.Equiv_To;
         end loop;
      elsif T.Is_Def
        or else
          (Op_Sem.Is_Abstract and then Op_Sem.Overridden_By = null)
        or else
          (T.Is_Optional and then Op_Sem.Overridden_By = null
           and then Op_Sem.Body_Region = null)
      then
         --  Create a "routine" now so the routine "index" will be available
         --  by the time we generate calls.

         if T.Is_Import then
            --  Generate code for an imported operation
            declare
               First_Tree : Trees.Tree'Class renames Tree_Ptr_Of
                                                        (Lists.Nth_Element
                                                            (T.Import_Info,
                                                             1)).all;
               Builtin : Routine_Code_Address := null;
               Desig : Strings.U_String := Strings.Null_U_String;
            begin
               if First_Tree in Identifier.Tree then
                  --  Lookup builtin
                  Desig := Identifier.Tree (First_Tree).Str;
                  Builtin := Interpreter.Find_Builtin (Desig);
               end if;

               if Builtin = null then
                  --  Builtin not found, complain
                  Sem_Error
                    (T,
                     "Built in routine for Import(" &
                     Subtree_Image (First_Tree) &
                     ") not found");
               end if;

               Op_Sem.Routine :=
                 new Routine'
                 (Is_PSVM_Routine => False,
                  Index => 0,
                  Name => New_Op.Str,
                  Num_Prior_Homonyms => Natural
                                         (Symbols.Num_Prior_Homonyms (New_Op)),
                  Name_With_Overloading_Index => Strings.Null_U_String,
                  Full_Module_Name => Enclosing_Module_Full_Name (New_Op),
                  Uses_Queuing => Op_Sem.Uses_Queuing,
                  Local_Area_Length => 0,
                  Start_Callee_Locals => 0,
                  Boundary_Conditions => (others =>
                    Null_Code_Block_Descriptor),
                  Parameters => null, --  TBD
                  Nesting_Level => 0,  --   Filled in later
                  Convention => Op_Sem.Convention,
                  Conv_Desc => Op_Conv_Desc,
                  Routine_Addr => Builtin,
                  Built_In_Desig => Desig,
                  Is_Compiled_Routine => False,
                  Is_Nested_Block => False,
                  Internal_Precond_Addr => null);

            end;
         else
            --  Create Routine structure for user-defined ParaSail routine

            --  Create initial routine
            Op_Sem.Routine :=
              new Routine'
              (Is_PSVM_Routine => True,
               Index => 0,
               Name => New_Op.Str,
               Num_Prior_Homonyms => Natural
                                      (Symbols.Num_Prior_Homonyms (New_Op)),
               Name_With_Overloading_Index => Strings.Null_U_String,
               Full_Module_Name => Enclosing_Module_Full_Name (New_Op),
               Uses_Queuing => Op_Sem.Uses_Queuing,
               Local_Area_Length => 0,
               Start_Callee_Locals => 0,
               Boundary_Conditions => (others => Null_Code_Block_Descriptor),
               Parameters => null, --  TBD
               Nesting_Level => 0,  --   Filled in later
               Convention => Op_Sem.Convention,
               Conv_Desc => Op_Conv_Desc,
               Local_Master => 0, --  Filled in later
               Code => null,    --  Filled in after generating code
               Uplevel_Refs => null,  --  TBD
               Enc_Type_Desc => null);  --  Filled in later

         end if;  --  whether is imported

         --  Install new routine in Routine table
         Install_Code (Op_Sem.Routine);

         if Op_Sem.Spec_Sem /= null
           and then Op_Sem.Index = 0
         then
            --  Copy routine onto spec if not an "interface operation"
            Op_Sem.Spec_Sem.Routine := Op_Sem.Routine;
         end if;

         --  Save info for use by later code generation.
         Routine_Sym_Table (Op_Sem.Routine.Index) := New_Op;

      end if; --  if Is_Def

      if T.Is_Def
        and then not T.Is_Import
        and then Op_Sem.Equiv_To = null
      then
         --  We will be generating code for this one

         Pre_Cg (T.Statements, Read_Write);

         --  Combine in the effects of all return statements
         Object_Access.Combine
           (Read_Write,
            Op_Sem.Return_Effects_RW,
            Object_Access.Sequential);

         Object_Access.Extract_Uplevel_Refs
           (Op_Region => Op_Sem.Associated_Symbol.Nested_Region,
            All_Refs => Read_Write,
            Uplevel_Refs => Op_Sem.Uplevel_Refs);
            --  Here we extract any up-level references from the RW mapping.
            --  We will check these references at the point of call.
            --  NOTE: We could skip this for non-nested routines, but
            --        some ParaSail-based languages might have some sort of
            --        globals, so might as well allow for that here.
            --  NOTE: If we have recursion and call-before-body-seen
            --        we iterate until it stabilizes.

         if Op_Sem.Spec_Sem /= null then
            --  Copy on to spec sem as well
            Op_Sem.Spec_Sem.Uplevel_Refs := Op_Sem.Uplevel_Refs;
         end if;

         --  Check to see whether we now have more uplevel refs
         --  than we had at a call.
         loop
            declare
               Uplevel_Ref_Count : Integer :=
                 Object_Access.Num_Objects (Op_Sem.Uplevel_Refs);
            begin
               if Uplevel_Ref_Count > Orig_Uplevel_Ref_Count_At_Call then
                  --  Need to re-do Pre_CG on enclosing operation
                  declare
                     Encl_Op : constant Operation_Sem_Ptr :=
                       Static.Find_Enclosing_Operation
                         (Op_Sem.Associated_Symbol.Enclosing_Region);
                  begin
                     if Encl_Op /= null then
                        --  Set flag to indicate need to rerun.
                        Encl_Op.Rerun_Pre_CG := True;
                     end if;
                     exit; --  No need to re-run now; will re-run as part
                           --  of encloser
                  end;

               elsif Op_Sem.Rerun_Pre_CG
                 or else
                   Uplevel_Ref_Count > Op_Sem.Uplevel_Ref_Count_At_Call
                 or else
                   (Op_Sem.Spec_Sem /= null
                    and then
                      Uplevel_Ref_Count > Op_Sem.Uplevel_Ref_Count_At_Call)
               then
                  --  Need to re-do Pre_Cg on this operation
                  if Debug_Pre_Cg then
                     Put_Line
                       (" Rerunning Pre_CG on operation " &
                        Sym_Name (New_Op));
                  end if;

                  --  Reinitialize re-run flag to False
                  Op_Sem.Rerun_Pre_CG := False;

                  Pre_Cg (T.Statements, Read_Write);
                  --  Loop around to check again

               else
                  --  No need to re-run
                  exit;
               end if;
            end;
         end loop;

         --  TBD: If is lambda expression being passed as parameter,
         --       worry about objects referenced by lambda expression.

         --  Walk any annotations on the outputs (treated as postconditions)
         Pre_Cg_List (T.Operation_Outputs, Read_Write,
                      Annotation_Mode => Postcondition_Mode_With_Check);

         --  Now walk the "overall" postcondition, but
         --  allow for possibility that it is actually a precondition.
         if Num_Outputs > 0 or else Not_Null (T.Preconditions) then
            --  There are outputs, or already has a precondition,
            --  so this should be a "real" postcondition
            Pre_Cg (T.Postconditions, Read_Write,
                    Annotation_Mode => Postcondition_Mode_With_Check);
         else
            --  No outputs and no precondition, so this might be a
            --  precondition.
            --  TBD: Support "pre =>" and "post =>" to disambiguate.
            Pre_Cg (T.Postconditions, Read_Write,
                    Annotation_Mode => Postcondition_Mode_No_Check);
            declare
               Info : Entry_Exit_Info_Type renames
                 Entry_Exit_Info (T.Postconditions);
            begin
               if not Info.Has_Exit_Value_Of_Var
                 and then not Info.Has_Exit_Value_Of_Output
               then
                  --  Turn this into a precondition.
                  T.Preconditions := T.Postconditions;
                  T.Postconditions := Null_Optional_Tree;
               end if;
            end;
         end if;

         Op_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Statements) +
           Num_Nested_Blocks (T.Preconditions) +
           Num_Nested_Blocks (T.Postconditions) +
           Combine_Operand_Blocks (T.Operation_Inputs) +
           Combine_Operand_Blocks (T.Operation_Outputs);

         if Debug_Pre_Cg then
            Put_Line
              (" For operation " &
               Sym_Name (New_Op) &
               " Slow_Calls = " &
               Slow_Call_Enum'Image (Slow_Calls (T.Statements)) &
               ", Num_Nested_Blocks = " &
               Natural'Image (Op_Sem.Num_Nested_Blocks) &
               ", Uses_Queuing = " &
               Boolean'Image (Uses_Queuing (T.Statements)));

            if not Object_Access.Is_Empty (Op_Sem.Uplevel_Refs) then
               Put (" Uplevel Refs for " & Sym_Name (New_Op) & ": ");
               Object_Access.Dump_Read_Write_Mapping (Op_Sem.Uplevel_Refs);
            end if;
         end if;

      end if;  --  if we will be generating code for this operation

   end Operation_Action;

   procedure Obj_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Obj_Decl.Tree) is
   begin
      if T.Sem_Info /= null then
         declare
            Obj_Sem : Object_Semantic_Info renames Object_Semantic_Info (
              T.Sem_Info.all);

            New_Obj : constant Symbols.Sym_Ptr := Obj_Sem.Associated_Symbol;
         begin
            if Debug_Pre_Cg then
               Put_Line
                 ("Pre codegen for object decl " &
                  Sym_Name (New_Obj) &
                  "; sym_index =" &
                  Sym_Index'Image (New_Obj.Index));
            end if;
            Visit_Resolved (T.Obj_Value, Visitor);

            if Static.Sym_Is_Declared_Ref (New_Obj) then
               --  Save rw-mapping associated with Ref for future use.
               Object_Access.Save_Mapping_For_Ref (Obj_Sem.Object_Ref,
                 Visitor.Read_Write, Obj_Sem.Object_Id,
                 Obj_Decl.Find_Source_Pos (T));
            end if;

            --  Propagate slow-calls info
            Obj_Sem.Slow_Calls := Slow_Calls (T.Obj_Value);
            Obj_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Obj_Value);
            Obj_Sem.Uses_Queuing := Uses_Queuing (T.Obj_Value);
         end;
      end if;
   end Obj_Decl_Action;

   procedure Param_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Param_Decl.Tree) is
      Param_Sem : Param_Semantic_Info
        renames Param_Semantic_Info (T.Sem_Info.all);

      New_Param : constant Symbols.Sym_Ptr := Param_Sem.Associated_Symbol;
      Param_Type : constant Type_Sem_Ptr := Param_Sem.Resolved_Type;
      use type Languages.Language_Enum;
      use Param_Decl;
   begin
      if Debug_Pre_Cg then
         Put_Line
           ("Pre codegen for parameter " &
            Sym_Name (New_Param) &
            "; sym_index =" &
            Sym_Index'Image (New_Param.Index));
      end if;

      --  For Ada, we want to mark as "locked" the first input parameter
      --  of an operation if it is not already so marked,
      --  and its type is concurrent, and is the "current instance" of
      --  the enclosing module.
      --  We suppress this for explicitly aliased parameters.
      if Languages.Language = Languages.Ada202x
        and then Param_Sem.Context = Operation_Input_Context
        and then T.Locking = Param_Decl.Not_Locked
        and then T.Kind not in Param_Decl.Ref_Param_Kinds
        and then Natural (New_Param.Index) =
          Lists.Length
            (Operation.Tree (Tree_Ptr_Of
              (New_Param.Enclosing_Region.Associated_Symbol.Definition).all).
                 Operation_Outputs) + 1
        and then Static.Type_Is_Concurrent (Param_Type)
      --  and then Sem_Ptr
      --    (New_Param.Enclosing_Region.Associated_Symbol.Sem_Info).Context =
      --       Module_Context
        and then
          (Static.Num_Components (Param_Type.Associated_Module) >= 2
             or else
           (Static.Num_Components (Param_Type.Associated_Module) = 1
              and then
            not Resolved_Type (Static.Nth_Component
              (Param_Type.Associated_Module, 1)).Known_To_Be_Small))
      then
         --  Ok, we jumped through all of the hoops -- consider this
         --  parameter as "locked"
         T.Locking := Locked_Param;
         if True or else Debug_Pre_Cg then
            Put_Line
              ("  Marking as ""locked"" parameter " &
               Sym_Name (New_Param) &
               " of " &
               Sym_Name (New_Param.Enclosing_Region.Associated_Symbol));
         end if;

      end if;
   end Param_Decl_Action;

   procedure Type_Decl_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Type_Decl.Tree) is
   begin
      if Static.Type_Def_Has_New_Module (T.Type_Definition) then
         --  Process the module nested inside the type definition
         Visit_Resolved
           (Invocation.Tree
             (Tree_Ptr_Of (T.Type_Definition).all).Prefix,
            Visitor);
      end if;
   end Type_Decl_Action;

   procedure Unary_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Unary.Tree) is
      use Unary;
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);
   begin
      Visit_Resolved (T.Operand, Visitor);

      if Comp_Sem = null then
         return;
      end if;

      --  Combine the operand with the operation
      declare
         Is_Slow_Call : constant Boolean := Call_Is_Slow (Comp_Sem);
      begin
         Comp_Sem.Slow_Calls :=
            Combine_Operands_With_Op
              (Is_Slow_Call => Is_Slow_Call,
               Operand_Calls => Slow_Calls (T.Operand));
         if Is_Slow_Call
           and then (Avoid_Parallel_Calls
             or else Comp_Sem.Slow_Calls /= Slow_Call)
         then
            --  This might need a nested block
            Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Operand) + 1;
            if Debug_Pre_Cg then
               Put_Line
                 (" Unary op " &
                  Subtree_Image (Optional (T'Access)) &
                  ", num nested blocks += 1");
            end if;

         end if;
         Comp_Sem.Uses_Queuing := Uses_Queuing (T.Operand);

         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            Comp_Sem.Entry_Exit_Info := Entry_Exit_Info (T.Operand);
            if T.Operator = Updated_Value_Op then
               if Comp_Sem.Entry_Exit_Info.Has_Entry_Value_Of_Var then
                  --  Convert to retrieving exit (i.e. updated) value
                  pragma Assert
                    (not Comp_Sem.Entry_Exit_Info.Has_Exit_Value_Of_Var);

                  Comp_Sem.Entry_Exit_Info.Has_Entry_Value_Of_Var := False;
                  Comp_Sem.Entry_Exit_Info.Has_Exit_Value_Of_Var := True;
               else
                  Sem_Warning
                    ("requesting updated value of something that is not a var",
                     Src_Pos => Find_Source_Pos (T.Operand));
               end if;

               if Visitor.Annotation_Mode = Precondition_Mode then
                  Sem_Warning
                    ("referring to updated value of var in precondition",
                     Src_Pos => Find_Source_Pos (T));
               end if;
            end if;
         end if;
      end;
   end Unary_Action;

   procedure Binary_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Binary.Tree) is
      use Binary;
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);
      Eval_In_Parallel : Boolean := True;
      Right_Visitor : Pre_Cg_Visitor;
   begin
      Visit_Resolved (T.Left_Operand, Visitor);

      Right_Visitor.Annotation_Mode := Visitor.Annotation_Mode;
      Visit_Resolved (T.Right_Operand, Right_Visitor);

      if Comp_Sem = null then
         return;
      end if;

      --  Combine the slow-call, queuing, entry/exit, and nested-block info
      --  from the op and operands
      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Left_Operand) +
                                    Num_Nested_Blocks (T.Right_Operand);

      Comp_Sem.Uses_Queuing := Uses_Queuing (T.Left_Operand)
                              or else Uses_Queuing (T.Right_Operand);

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
         Comp_Sem.Entry_Exit_Info :=
           Combine_Entry_Exit_Info (T.Left_Operand, T.Right_Operand);
      end if;

      case T.Operator is
         when Stmt_Ops =>
            --  Operands evaluated in parallel if Independent_Stmt_Ops
            Eval_In_Parallel := (T.Operator in Independent_Stmt_Ops);

         when Short_Circuit_Ops =>
            Eval_In_Parallel := False;
            if Visitor.Annotation_Mode in Postcondition_Mode
              and then Entry_Exit_Info (T.Right_Operand).Num_Entry_Temps > 0
            then
               --  Trouble: We have an entry temp in the right-operand
               --  of a short-circuit operation within a postcondition.
               Sem_Warning
                 ("Right operand of short-circuit needs early evaluation",
                  Src_Pos => Find_Source_Pos (T.Right_Operand));
            end if;

         when others =>
            --  Default is to evaluate operands in parallel
            Eval_In_Parallel := True;
      end case;

      if Eval_In_Parallel then
         declare
            Is_Slow_Call : constant Boolean := Call_Is_Slow (Comp_Sem);
         begin
            Object_Access.Combine
              (Visitor.Read_Write,
               Right_Visitor.Read_Write,
               Object_Access.Parallel);

            Comp_Sem.Slow_Calls :=
               Combine_Operands_With_Op
                 (Is_Slow_Call => Is_Slow_Call,
                  Operand_Calls =>
                     Combine_Operand_Calls
                       (Slow_Calls (T.Left_Operand),
                        Slow_Calls (T.Right_Operand)));

            --  Compute additional nested blocks needed
            if T.Operator in Independent_Stmt_Ops then
               if Binary.Is_Parallel_Stmt_Op (T.Left_Operand) then
                  --  For A || B || C we only need three blocks, not 4.
                  Comp_Sem.Num_Nested_Blocks := Comp_Sem.Num_Nested_Blocks +
                                                1;
               else
                  --  For A || B we need two blocks
                  Comp_Sem.Num_Nested_Blocks := Comp_Sem.Num_Nested_Blocks +
                                                2;
               end if;

               --  Independent_Stmt_Ops always require a master
               Comp_Sem.Slow_Calls := Mandatory_Parallel_Call;

               if Debug_Pre_Cg then
                  Put_Line
                    (" Binary op " &
                     Subtree_Image
                        (Optional (T'Access),
                         Use_Short_Form => True) &
                     ", num nested blocks += 2 (or 1)");
               end if;
            elsif Is_Slow_Call
              and then (Avoid_Parallel_Calls
                or else Comp_Sem.Slow_Calls /= Slow_Call)
            then
               Comp_Sem.Num_Nested_Blocks := Comp_Sem.Num_Nested_Blocks + 1;
               --  This is pessimistic, since if this is the leftmost
               --  operation, we don't need a nested block.
               if Debug_Pre_Cg then
                  Put_Line
                    (" Binary op " &
                     Subtree_Image
                        (Optional (T'Access),
                         Use_Short_Form => True) &
                     ", num nested blocks += 1");
               end if;
            end if;
         end;
      else
         --  Operands evaluated sequentially
         Object_Access.Combine
           (Visitor.Read_Write,
            Right_Visitor.Read_Write,
            Object_Access.Sequential);

         Comp_Sem.Slow_Calls :=
            Combine_Sequential_Computations
              (Slow_Calls (T.Left_Operand),
               Slow_Calls (T.Right_Operand));
      end if;
   end Binary_Action;

   procedure Annotation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Annotation.Tree) is
      Annotation_Sem : constant Annotation_Sem_Ptr :=
        Annotation_Sem_Ptr (T.Sem_Info);
      Read_Write : Object_Access.Read_Write_Mapping;
   begin
      if Debug_Pre_Cg then
         Put_Line (" Pre_Cg on annotation: " &
           Subtree_Image (T));
         if Annotation_Sem = null then
            Put_Line (" [annotation sem is null]");
         end if;
      end if;

      if Annotation_Sem /= null then
         --  Do static semantic analysis on annotations
         --  now that everything else has been analyzed.
         --  TBD: This is probably no longer necessary since
         --      we now have the Exprs_Only mode for the Second_Pass.
         declare
            Saved_Error_Count : constant Natural := Sem_Error_Count;
         begin
            Static.Analyze_Annotation (T);
            if Sem_Error_Count > Saved_Error_Count then
               Sem_Error (T, "Semantic error found in annotation");
               return;
            end if;
         end;
      end if;

      if Static.All_Elems_Are_Refs (T.Annotations) then
         --  Nothing to do with annotations that are only of the form "X => Y"
         --  TBD: Some day support "pre => XX" and "post => YY"
         return;  --  All done  --
      end if;

      --  Now walk again for pre-cg pass
      --  TBD: Handle nested annotations and
      --      within-annotation declarations.
      Pre_Cg_List (T.Annotations, Read_Write,
                   Annotation_Mode => Visitor.Annotation_Mode);

      if Annotation_Sem /= null then
         Annotation_Sem.Num_Nested_Blocks :=
           Combine_Operand_Blocks (T.Annotations) + 2;
         --  We count 2 for each annotation since for a postcondition
         --  we might have an "entry-temp" block as well
         --  (this doesn't need to be exact, since it is just determining
         --   the size of the nested-block array we pre-allocate).
         --  NOTE: We don't set the "Slow_Calls" or "Queuing" part since
         --       we don't want the contents of annotations to affect
         --       decisions about what should be done in parallel.
         --       In the Code_Gen phase, we do compute the Slow_Calls
         --       information to decide whether we will need a master
         --       inside the nested block used for the check.
         if Combine_Operand_Queuing (T.Annotations) then
            Sem_Error (T, "Queuing not permitted within an annotation");
         end if;

         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            --  Combine the entry/exit info
            declare
               Info : Entry_Exit_Info_Type renames
                        Annotation_Sem.Entry_Exit_Info;
            begin
               Info := Combine_Entry_Exit_Info (T.Annotations);
               if Visitor.Annotation_Mode = Postcondition_Mode_With_Check
                 and then not Info.Has_Exit_Value_Of_Var
                 and then not Info.Has_Exit_Value_Of_Output
               then
                  Sem_Warning ("Only input values in postcondition: " &
                    Subtree_Image (T),
                    Src_Pos => Find_Source_Pos (Annotation_Sem.Definition));
               end if;

               if Debug_Pre_Cg or Debug_Entry_Exit then
                  Put_Line ("Entry-Exit info for " & Subtree_Image (T));
                  Put_Entry_Exit_Info (Info);
                  if Not_Null (Annotation_Sem.Decl_For_Annotations) then
                     Put_Line ("  Associated_Decl:");
                     Display_Subtree
                       (Annotation_Sem.Decl_For_Annotations,
                        Indent => 4,
                        Use_Short_Form => True);
                  end if;
               end if;
            end;
         end if;
      end if;
   end Annotation_Action;

   procedure Identifier_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Identifier.Tree) is
      Id_Sem : constant Sem_Ptr :=
        Underlying_Sem_Info (Sem_Ptr (T.Sem_Info));
   begin
      if Id_Sem /= null
        and then Id_Sem.all in Object_Semantic_Info'Class
        and then not Static.Is_Unlocked_Concurrent_Operand
          (Operand_Sem_Ptr (Id_Sem))
        and then Static.Sem_Info_Is_For_Variable (Id_Sem)
      then
         --  Initialize read/write mapping to represent a reference
         --  to the named object.
         --  TBD: If is name of local operation, worry about objects
         --       referenced by that operation.
         Refer_To_Object (Visitor.Read_Write,
           Object_Sem_Ptr (Id_Sem),
           Mode => Object_Access.Ref_Access,
           Source_Pos => Identifier.Find_Source_Pos (T));
      end if;

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode
        and then Id_Sem /= null
        and then T.Sem_Info.all in Sym_Reference_Info'Class
      then
         --  Initialize Entry_Exit_Info
         Compute_Entry_Exit_Info
           (Sym_Ref_Ptr (T.Sem_Info), Visitor.Annotation_Mode);
      end if;
   end Identifier_Action;

   procedure Property_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Property.Tree) is
      Id_Sem : constant Sem_Ptr :=
        Underlying_Sem_Info (Sem_Ptr (T.Sem_Info));
   begin
      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode
        and then Id_Sem /= null
        and then T.Sem_Info.all in Sym_Reference_Info'Class
      then
         --  Initialize Entry_Exit_Info
         Compute_Entry_Exit_Info
           (Sym_Ref_Ptr (T.Sem_Info), Visitor.Annotation_Mode);
      end if;
   end Property_Action;

   procedure Qualified_Name_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Qualified_Name.Tree) is
      Id_Sem : constant Sem_Ptr :=
        Underlying_Sem_Info (Sem_Ptr (T.Sem_Info));
   begin
      if Id_Sem /= null
        and then Id_Sem.all in Object_Semantic_Info'Class
        and then not Static.Is_Unlocked_Concurrent_Operand
          (Operand_Sem_Ptr (Id_Sem))
        and then Static.Sem_Info_Is_For_Variable (Id_Sem)
      then
         --  Initialize read/write mapping to represent a reference
         --  to the named object.
         --  TBD: If is name of local operation, worry about objects
         --       referenced by that operation.
         Refer_To_Object (Visitor.Read_Write,
           Object_Sem_Ptr (Id_Sem),
           Mode => Object_Access.Ref_Access,
           Source_Pos => Qualified_Name.Find_Source_Pos (T));
      end if;

      --  TBD Visit_Resolved (T.Prefix, Visitor);
   end Qualified_Name_Action;

   procedure Assign_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Assign_Stmt.Tree) is
   begin
      if T.Sem_Info = null
        or else T.Sem_Info.all not in Computation_Semantic_Info'Class
      then
         --  Probably a "move" inside an aggregate;
         --  nothing to do.
         null;
      else
         declare
            Comp_Sem : constant Computation_Sem_Ptr :=
              Computation_Sem_Ptr (T.Sem_Info);
            LHS_Ref : Object_Access.Read_Write_Mapping;
            LHS_Visitor : Pre_Cg_Visitor;
         begin
            --  The "+=", "-=", etc. operators have been converted into calls.
            --  All that we have here are assign, move, and swap.
            --  These evaluate the LHS and RHS in parallel, and then
            --  as a sequential step, assign into the LHS.

            --  Visit LHS and save result
            Visit_Resolved (T.LHS, LHS_Visitor);
            Object_Access.Save_Mapping_For_Ref (LHS_Ref,
              LHS_Visitor.Read_Write, Object_Access.Null_Object_Id,
              Find_Source_Pos (T.LHS));

            --  Visit RHS and combine into LHS assuming parallel execution
            Pre_Cg (T.RHS, LHS_Visitor.Read_Write, Object_Access.Parallel);

            --  Now combine update of LHS into r/w mapping
            Object_Access.Combine
              (LHS_Visitor.Read_Write,  --  TBD: Visitor.Read_Write?
               Addition => LHS_Ref,
               How_Combined => Object_Access.Sequential,
               Mode => Object_Access.Update_Access);

            --  Now combine everything into outer visitor.
            Object_Access.Combine
              (Visitor.Read_Write,
               Addition => LHS_Visitor.Read_Write,
               How_Combined => Object_Access.Sequential,
               Mode => Object_Access.No_Access);

            --  Compute slow-calls and para-block info, presuming LHS
            --  and RHS evaluated in parallel
            Comp_Sem.Slow_Calls :=
               Combine_Operand_Calls (Slow_Calls (T.LHS), Slow_Calls (T.RHS));
            Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.LHS) +
                                          Num_Nested_Blocks (T.RHS);
            Comp_Sem.Uses_Queuing := Uses_Queuing (T.LHS)
                                    or else Uses_Queuing (T.RHS);
         end;
      end if;
   end Assign_Stmt_Action;

   procedure Invocation_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Invocation.Tree) is
      use Invocation;
      Op_Inputs : Lists.List;
      Num_Inputs : Natural := 0;
      Op_Outputs : Lists.List;
      Num_Outputs : Natural := 0;
      Is_Indexing_Op : Boolean := False;
      Is_Slicing_Op : Boolean := False;
      Indexed_Array_RW : Object_Access.Read_Write_Mapping;
      Indexing_RW : Object_Access.Read_Write_Mapping;
      Param_Comp_RW : Object_Access.Read_Write_Mapping;
      Operation_RW : Object_Access.Read_Write_Mapping;
      Uplevel_Refs : Object_Access.Read_Write_Mapping;
      Num_Operands : constant Natural := Lists.Length (T.Operands);
      Num_Agg_Temps_Needed : Natural := 0;
      use Object_Access;
   begin
      if T.Sem_Info.all in Call_Semantic_Info then
         declare
            Op_Sem : Operation_Sem_Ptr := Call_Sem_Ptr (T.Sem_Info).Op_Sem;
         begin
            if Op_Sem /= null then
               --  Get list of operation inputs
               Op_Inputs :=
                 Operation.Tree
                   (Tree_Ptr_Of (Op_Sem.Definition).all).Operation_Inputs;
               Num_Inputs := Lists.Length (Op_Inputs);
               Op_Outputs :=
                 Operation.Tree
                   (Tree_Ptr_Of (Op_Sem.Definition).all).Operation_Outputs;
               Num_Outputs := Lists.Length (Op_Outputs);

               --  Keep track of any uplevel refs of operation
               Uplevel_Refs := Op_Sem.Uplevel_Refs;

               --  Remember number of uplevels at point of call
               Op_Sem.Uplevel_Ref_Count_At_Call :=
                 Object_Access.Num_Objects (Op_Sem.Uplevel_Refs);

               if Num_Inputs >= 2
                 and then Op_Sem.Associated_Symbol /= null
               then
                  --  Check whether is call on "Indexing" or "Slicing"
                  declare
                     Op_Name : constant Strings.U_String :=
                       Op_Sem.Associated_Symbol.Str;
                     use type Strings.U_String;
                  begin
                     if Op_Name = Static.Indexing_Op_Str
                       or else Op_Name = Static.Var_Indexing_Op_Str
                     then
                        --  TBD: Do not allow "var_indexing" once we
                        --       automatically choose "indexing" instead
                        --       when inside a concurrent loop.
                        Is_Indexing_Op := True;
                     elsif Op_Name = Static.Slicing_Op_Str then
                        Is_Slicing_Op := True;
                     end if;
                  end;
               end if;
            end if;
         end;
      elsif T.Sem_Info.all in Container_Aggregate_Semantic_Info
        and then Num_Operands = 1
        and then Tree_Ptr_Of (Lists.Nth_Element (T.Operands, 1)).all in
          For_Loop_Construct.Tree
      then
         --  Check whether container aggregate contains a for-loop that
         --  is potentially concurrent.  If so, make sure the type of the
         --  container is concurrent.
         declare
            Agg_Sem : constant Container_Agg_Sem_Ptr :=
              Container_Agg_Sem_Ptr (T.Sem_Info);
            For_Loop_Tree : For_Loop_Construct.Tree renames
              For_Loop_Construct.Tree
                (Tree_Ptr_Of (Lists.Nth_Element (T.Operands, 1)).all);
            For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr :=
              For_Loop_Construct_Sem_Ptr (For_Loop_Tree.Sem_Info);
         begin
            if For_Loop_Sem.Is_Potentially_Concurrent
              and then not Static.Type_Is_Concurrent (Agg_Sem.Target_Type)
            then
               --  Complain about possible race condition.
               --  TBD: Automatically make the aggregate concurrent.
               Sem_Warning ("Possible race condition: concurrent iterator " &
                 "inside non-concurrent container aggregate",
                 Src_Pos =>
                   For_Loop_Construct.Find_Source_Pos (For_Loop_Tree));
            end if;
         end;
      end if;

      Pre_Cg (T.Prefix, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);

      if not Object_Access.Is_Empty (Uplevel_Refs) then
         --  Incorporate uplevel effects of call
         declare
            Uplevel_Conf_RW : Read_Write_Mapping;
            Uplevel_Result_RW : Read_Write_Mapping;
         begin
            if Debug_Pre_Cg then
               Put_Line (" Combining in uplevel effects of call: " &
                 Subtree_Image (T));
            end if;

            --  Copy so can combine in the uplevel effects
            Object_Access.Copy (From => Uplevel_Refs,
                                To => Uplevel_Conf_RW);
                                 --  Preserve source pos so uplevel
                                 --  conflicts with parameters retain
                                 --  their original point of reference.

            Object_Access.Copy (From => Uplevel_Refs,
                                To => Uplevel_Result_RW,
                                New_Source_Pos => Find_Source_Pos (T));
                                 --  Set new source pos to correspond to
                                 --  point of call

            --  Consider conflicts occurring between uplevels and parameters
            Object_Access.Combine
              (Operation_RW,
               Addition => Uplevel_Conf_RW,
               How_Combined => Object_Access.Uplevel_Refs,
               Mode => Update_Access);

            --  Combine uplevel effects into encloser
            Object_Access.Combine
              (Visitor.Read_Write,
               Addition => Uplevel_Result_RW,
               How_Combined => Sequential,
               Mode => Update_Access);
         end;
      end if;

      --  First make sure that the computations involved in computing the
      --  parameters can be performed in parallel, ignoring the effects of
      --  calling this operation itself.
      --  Second make sure that the parameters being updated are not
      --  aliased with any other parameter.
      --  Finally, compute the R/W mapping to propagate as the result
      --  of the computation.
      for I in 1 .. Num_Operands loop
         declare
            Nth_Actual : constant Optional_Tree :=
              Lists.Nth_Element (T.Operands, I);
            Mode : Access_Mode_Enum := Read_Access;

            Operand_Visitor : Pre_Cg_Visitor;
            Operand_Conf_RW : Object_Access.Read_Write_Mapping;
            Operand_Result_RW : Object_Access.Read_Write_Mapping;
         begin
            --  Visit operand
            Operand_Visitor.Annotation_Mode := Visitor.Annotation_Mode;
            Visit_Resolved (Nth_Actual, Operand_Visitor);

            --  Save two copies of R/W mapping for operand
            Object_Access.Copy
              (From => Operand_Visitor.Read_Write, To => Operand_Conf_RW);
            Object_Access.Copy
              (From => Operand_Visitor.Read_Write, To => Operand_Result_RW);

            --  Check for conflicts presuming parallel evaluation of operands
            Object_Access.Combine
              (Param_Comp_RW,
               Addition => Operand_Visitor.Read_Write,
               How_Combined => Parallel,
               Mode => Param_Computation);

            --  Determine the access mode for this operand
            if Num_Inputs >= I then
               --  Get Nth formal parameter, corresponding to Nth actual
               --  NOTE: We presume that by this time, the actual parameters
               --       have been "canonicalized" into a list of actuals,
               --       with defaults filled in.
               declare
                  Param_Tree : Trees.Tree'Class renames
                    Tree_Ptr_Of (Lists.Nth_Element (Op_Inputs, I)).all;
               begin
                  if Param_Tree in Param_Decl.Tree then
                     Mode := Access_Map (Param_Decl.Tree (Param_Tree).Kind);

                     --  Check if this is a container aggregate passed by
                     --  reference, in which case it will need to use a
                     --  finalizable temp slot.

                     declare
                        Formal_Param : Param_Decl.Tree
                          renames Param_Decl.Tree (Param_Tree);

                        Actual_Sem : Operand_Sem_Ptr :=
                          Operand_Sem_Ptr (Sem_Info
                            (Resolved_Tree (Nth_Actual)));

                        Formal_Sem : constant Param_Sem_Ptr :=
                          Param_Sem_Ptr (Formal_Param.Sem_Info);
                     begin
                        if Actual_Sem.all in
                            Container_Aggregate_Semantic_Info
                        then
                           if Debug_Pre_Cg then
                              Put_Line ("Checking need for agg temp for " &
                                Subtree_Image (Nth_Actual));
                           end if;
                        end if;

                        if Actual_Sem.all in
                            Container_Aggregate_Semantic_Info
                          and then
                            Static.Param_Is_Passed_By_Ref
                              (Formal_Sem,
                               Formal_Param.Kind,
                               Formal_Param.Locking)
                        then
                           --  We have a container aggregate passed by ref.
                           --  Give it a finalizable temp slot.
                           Computation_Sem_Ptr
                             (Actual_Sem).Num_Finalizable_Temps :=
                               Computation_Sem_Ptr
                                 (Actual_Sem).Num_Finalizable_Temps + 1;
                           if Debug_Pre_Cg then
                              Put_Line ("Need temp for by-ref agg " &
                                Subtree_Image (Nth_Actual));
                           end if;
                        end if;
                     end;
                  end if;
               end;
            end if;

            if Is_Indexing_Op or else Is_Slicing_Op then
               if I = 1 then
                  --  Incorporate array into its own read/write mapping
                  --  TBD: Does this result in a ref to the array?
                  Object_Access.Combine
                    (Indexed_Array_RW,
                     Addition => Operand_Result_RW,
                     How_Combined => Within_Operation,
                     Mode => Mode);

               else
                  --  Treat each parameter as another level of indexing
                  Object_Access.Combine
                    (Operation_RW,
                     Addition => Operand_Result_RW,
                     How_Combined => Within_Operation,
                     Mode => Mode);

                  Object_Access.Refer_To_Indexed_Component
                    (Indexing_RW,
                     Enclosing_Object => Indexed_Array_RW,
                     Domain => U_Base_Type_Region (Resolved_Type (Nth_Actual)),
                     Index => Trees.Root_Sem_Ptr (Underlying_Sem_Info
                       (Resolved_Tree (Nth_Actual))),
                     Mode => Object_Access.Ref_Access,
                     Source_Pos => Find_Source_Pos (Nth_Actual));

                  if I < Num_Inputs then
                     --  Copy R/W mapping back so can use for next index
                     Object_Access.Move
                       (From => Indexing_RW, To => Indexed_Array_RW);
                  else
                     --  Combine indexing R/W mapping into final R/W mapping
                     --  TBD: Should we specify a Mode?
                     Object_Access.Combine
                       (Visitor.Read_Write,
                        Addition => Indexing_RW,
                        How_Combined => Sequential);
                  end if;

               end if;
            else
               --  TBD: Should recognize "indivisible" objects which cannot
               --       be updated a component at a time, as it is safe
               --       to have one of these as both an input and an output.
               --       Examples include all "small" objects, as well as
               --       "immutable" objects like univ-strings.

               --  Check for "ref" conflicts
               Object_Access.Combine
                 (Operation_RW,
                  Addition => Operand_Conf_RW,
                  How_Combined => Within_Operation,
                  Mode => Mode);

               --  Combine overall result to propagate to enclosing construct
               Object_Access.Combine
                 (Visitor.Read_Write,
                  Addition => Operand_Result_RW,
                  How_Combined => Sequential,
                  Mode => Mode);
            end if;
         end;
      end loop;

      --  TBD: "Extends" not really used yet.
      Pre_Cg (T.Extends, Visitor.Read_Write, How_Combined => Not_Combined,
        Annotation_Mode => Visitor.Annotation_Mode);

      --  NOTE: If this might be a queued call,
      --        then we must turn it into a parallel call.
      --  Compute combined slow-call and para-block info
      if T.Sem_Info /= null
        and then T.Sem_Info.all in Computation_Semantic_Info'Class
      then
         declare
            Comp_Sem : constant Computation_Sem_Ptr :=
              Computation_Sem_Ptr (T.Sem_Info);
            Is_Slow_Call : constant Boolean := Call_Is_Slow (Comp_Sem);
         begin
            Comp_Sem.Slow_Calls :=
               Combine_Operands_With_Op
                 (Is_Slow_Call => Is_Slow_Call,
                  Operand_Calls => Combine_Operand_Calls (T.Operands));

            Comp_Sem.Num_Nested_Blocks := Combine_Operand_Blocks (T.Operands);

            Comp_Sem.Uses_Queuing := Call_Uses_Queuing (Comp_Sem)
                                    or else Combine_Operand_Queuing
                                               (T.Operands);

            if Is_Slow_Call
              and then (Avoid_Parallel_Calls
                or else Comp_Sem.Slow_Calls /= Slow_Call)
            then
               --  One more nested block needed potentially
               Comp_Sem.Num_Nested_Blocks := Comp_Sem.Num_Nested_Blocks + 1;
               if Debug_Pre_Cg then
                  Put_Line
                    (" Invocation " &
                     Subtree_Image (Optional (T'Access)) &
                     ", num nested blocks += 1");
               end if;
            end if;

            if Comp_Sem.Slow_Calls < Mandatory_Parallel_Call
              and then Comp_Sem.all in Call_Semantic_Info'Class
              and then Call_Sem_Ptr (Comp_Sem).Might_Be_Queued
            then
               --  If we might be queued, then we need a master, since might
               --  be turned into a parallel call.
               Comp_Sem.Slow_Calls := Mandatory_Parallel_Call;
            end if;

            Comp_Sem.Num_Finalizable_Temps :=
              Combine_Operand_Temps (T.Operands);

            for I in 1 .. Num_Outputs loop
               declare
                  Output_Tree : Trees.Tree'Class renames
                    Tree_Ptr_Of (Lists.Nth_Element (Op_Outputs, I)).all;
               begin
                  if Output_Tree in Param_Decl.Tree then
                     --  See whether this output is a "ref object"
                     declare
                        Output_Sem : constant Operand_Sem_Ptr :=
                          Operand_Sem_Ptr (Output_Tree.Sem_Info);
                     begin
                        if Output_Sem /= null
                          and then Output_Sem.Resolved_Type /= null
                          and then Output_Sem.Resolved_Type.
                            Associated_Module.Contains_Ref_Component
                        then
                           --  Is a "ref" object.  Check for "end" ops.
                           declare
                              End_Op_Sem : constant Operation_Sem_Ptr :=
                                Find_End_Op_For (Output_Sem.Resolved_Type);
                              Var_End_Op_Sem : constant Operation_Sem_Ptr :=
                                Find_End_Op_For (Output_Sem.Resolved_Type,
                                  Op_Name => Static.Var_End_Op_Str);
                           begin
                              if End_Op_Sem /= null
                                or else Var_End_Op_Sem /= null
                              then
                                 --  Is a "ref" object with an "end" or
                                 --  "var_end" op; will likely need a temp.
                                 --  TBD: Eventually might want to create a
                                 --       temp for large-object results which
                                 --       are passed as inputs to a call,
                                 --       so we can reclaim them immediately.
                                 Comp_Sem.Num_Finalizable_Temps :=
                                   Comp_Sem.Num_Finalizable_Temps + 1;

                                 --  Create an empty obj-location descriptor,
                                 --  to be filled in during the Code_Gen phase.
                                 if Comp_Sem.Finalizable_Temp_Info = null then
                                    Comp_Sem.Finalizable_Temp_Info :=
                                      new Object_Location_Info;
                                 end if;
                                 if Debug_Code_Gen then
                                    Put_Line (" Finalizable temp needed for " &
                                      Subtree_Image (Comp_Sem.Definition));
                                 end if;
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end;
            end loop;

            --  Combine the entry/exit info if processing a pre- or post-cond
            if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
               Comp_Sem.Entry_Exit_Info :=
                 Combine_Entry_Exit_Info (T.Operands);
            end if;

            if Debug_Pre_Cg then
               Put_Line
                 (" For invocation " &
                  Subtree_Image (Optional (T'Access)) &
                  " Slow_Calls = " &
                  Slow_Call_Enum'Image (Comp_Sem.Slow_Calls) &
                  ", Num_Nested_Blocks = " &
                  Natural'Image (Comp_Sem.Num_Nested_Blocks) &
                  ", Uses_Queuing = " &
                  Boolean'Image (Comp_Sem.Uses_Queuing) &
                  ", Num_Finalizable_Temps =" &
                  Natural'Image (Comp_Sem.Num_Finalizable_Temps) &
                  ", Num_Entry_Temps =" &
                  Natural'Image (Comp_Sem.Entry_Exit_Info.Num_Entry_Temps));
            end if;
         end;
      end if;

   end Invocation_Action;

   procedure Block_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Block_Stmt.Tree) is
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);
   begin
      Visit_Resolved (T.Block_Body, Visitor);
      Pre_Cg (T.End_With_Values, Visitor.Read_Write);

      --  Combine in the exit effects, which effectively happen after
      --  the block is done.
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Comp_Sem.Exit_Effects_RW,
         How_Combined => Object_Access.Sequential);

      --  Propagate worst-case "Slow_Calls" and para-blocks from constituents
      Comp_Sem.Slow_Calls :=
         Combine_Sequential_Computations
           (Slow_Calls (T.Block_Body),
            Slow_Calls (T.End_With_Values));

      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Block_Body) +
                                    Num_Nested_Blocks (T.End_With_Values);

      Comp_Sem.Uses_Queuing := Uses_Queuing (T.Block_Body)
                              or else Uses_Queuing (T.End_With_Values);

      if Debug_Pre_Cg then
         Put_Line
           (" For Block_Stmt " &
            Subtree_Image (Optional (T'Access), Use_Short_Form => True) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Comp_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (Comp_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Comp_Sem.Uses_Queuing));
      end if;
   end Block_Stmt_Action;

   procedure Case_Construct_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Case_Construct.Tree) is
      Case_Sem : constant Case_Construct_Sem_Ptr :=
        Case_Construct_Sem_Ptr (T.Sem_Info);

      Elems : Lists.List;  --  List for entry/exit info
   begin
      if Not_Null (T.Case_Selector) then
         Visit_Resolved (T.Case_Selector, Visitor);
      end if;
      Pre_Cg (T.End_With_Values, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);

      --  Propagate worst-case "Slow_Calls" and para-blocks from constituents
      Case_Sem.Slow_Calls :=
         Combine_Sequential_Computations
           (Slow_Calls (T.Case_Selector),
            Slow_Calls (T.End_With_Values));

      Case_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Case_Selector) +
                                    Num_Nested_Blocks (T.End_With_Values);

      Case_Sem.Uses_Queuing := Uses_Queuing (T.Case_Selector)
                              or else Uses_Queuing (T.End_With_Values);

      for I in 1 .. Lists.Length (T.Case_Alt_List) loop
         declare
            Case_Alt : Reference.Tree renames Reference.Tree (Tree_Ptr_Of
                                                                 (
                                                            Lists.Nth_Element
                                                              (T.Case_Alt_List,
                                                               I)).all);
            Case_Alt_Key_Container : Invocation.Tree
              renames Invocation.Tree (Tree_Ptr_Of (Case_Alt.Key).all);
            Case_Alt_Key : constant Optional_Tree :=
              Lists.Nth_Element (Case_Alt_Key_Container.Operands, 1);
         begin
            --  Visit case-alt key
            Pre_Cg (Case_Alt_Key, Visitor.Read_Write,
              Annotation_Mode => Visitor.Annotation_Mode);

            --  Visit case alternative statements
            Pre_Cg (Case_Alt.Referent, Visitor.Read_Write,
              Annotation_Mode => Visitor.Annotation_Mode);

            if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
               --  Build up list for entry/exit info
               Lists.Append (Elems, Case_Alt_Key);
               Lists.Append (Elems, Case_Alt.Referent);
            end if;

            --  Propagate worst-case "Slow_Calls" and para-blocks
            --  from case alt
            Case_Sem.Slow_Calls :=
               Combine_Sequential_Computations
                 (Case_Sem.Slow_Calls,
                  Combine_Sequential_Computations
                     (Slow_Calls (Case_Alt_Key),
                      Slow_Calls (Case_Alt.Referent)));

            Case_Sem.Num_Nested_Blocks := Case_Sem.Num_Nested_Blocks +
                                          Num_Nested_Blocks (Case_Alt_Key) +
                                          Num_Nested_Blocks
                                             (Case_Alt.Referent);

            Case_Sem.Uses_Queuing := Case_Sem.Uses_Queuing
                                    or else Uses_Queuing (Case_Alt_Key)
                                    or else Uses_Queuing (Case_Alt.Referent);
         end;
      end loop;

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
         --  Compute entry/exit info
         Lists.Append (Elems, T.Case_Selector);
         Lists.Append (Elems, T.End_With_Values);
         Case_Sem.Entry_Exit_Info := Combine_Entry_Exit_Info (Elems);
      end if;

      --  Combine in the exit effects, which effectively happen after
      --  the case statement is done.
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Case_Sem.Exit_Effects_RW,
         How_Combined => Object_Access.Sequential);

      if Debug_Pre_Cg then
         Put_Line
           (" For Case_Construct " &
            Subtree_Image (Optional (T'Access), Use_Short_Form => True) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Case_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (Case_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Case_Sem.Uses_Queuing));
      end if;
   end Case_Construct_Action;

   procedure Control_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Control_Stmt.Tree) is
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);
      use Control_Stmt;
   begin
      case T.Kind is
         when Null_Stmt =>
            --  No code to generate for a "null" statement
            null;
         when Return_Stmt =>
            declare
               Enclosing_Operation : constant Operation_Sem_Ptr :=
                 Comp_Sem.Op_Sem;
            begin
               --  We do *not* include references to the return value(s) in
               --  the read/write map for the Visitor, since these reads
               --  happen after all other threads are finished, so no race
               --  conditions are possible.  Instead, we combine them
               --  into a "Return_Effects_RW" associated with the enclosing
               --  operation where eventually the up-level effects will
               --  be incorporated into the Uplevel_Refs mapping.
               --  TBD: If this is an "extended" return statement,
               --       we have to decide exactly when we wait for the
               --       cancelation of other parallel threads.
               --       If this is a build-in-place function we would
               --       probably want to wait before starting the extended
               --       return since parallel attempts to build in place
               --       would likely fail.  It is possible for a return
               --       statement to fail (or exit) after it begins,
               --       but that is true for a "simple" return as well
               --       (due to an exception that is handled).
               --       Probably best to do the wait first, since
               --       build-in-place is relatively common, at least compared
               --       to a return statement that fails or exits prematurely.
               Pre_Cg (T.Values, Enclosing_Operation.Return_Effects_RW);
            end;

         when Continue_Stmt =>

            declare
               Continued_Stmt_Sem : constant Composite_Stmt_Sem_Ptr :=
                 Comp_Sem.Target_Stmt;
            begin
               if Continued_Stmt_Sem = null then
                  Sem_Error (T, "Target of continue statement not found");
               elsif Not_Null (T.Values) then
                  --  Walk next values
                  declare
                     Loop_Tree : Trees.Tree'Class
                       renames Tree_Ptr_Of (Continued_Stmt_Sem.Definition).all;
                     pragma Assert
                       (Loop_Tree in For_Loop_Construct.Tree);
                     For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr
                        :=
                       For_Loop_Construct_Sem_Ptr (Continued_Stmt_Sem);
                     pragma Assert (For_Loop_Sem.Num_Continues > 0);
                  begin
                     Pre_Cg (T.Values, Visitor.Read_Write);
                     --  TBD: We may need a "mode" here to reflect
                     --       whether value is used as a writable "ref"
                     --       Want to make sure this doesn't conflict
                     --       with other continues.
                  end;
               end if;
            end;

         when Exit_Stmt =>
            declare
               Exited_Stmt_Sem : constant Composite_Stmt_Sem_Ptr :=
                 Comp_Sem.Target_Stmt;
            begin
               if Exited_Stmt_Sem = null then
                  Sem_Error (T, "Target of exit statement not found");
               elsif Not_Null (T.Values) then
                  --  Walk the values to be assigned and combine
                  --  them into the exit effects of exited statement.
                  Pre_Cg_Exit_Effects (T.Values, Exited_Stmt_Sem);
               end if;
            end;
      end case;

      --  Propagate slow-calls and para-block info
      Comp_Sem.Slow_Calls := Slow_Calls (T.Values);
      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Values);
      Comp_Sem.Uses_Queuing := Uses_Queuing (T.Values);
      if Debug_Pre_Cg then
         Put_Line
           (" For control stmt " &
            Subtree_Image (Optional (T'Access)) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Comp_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (Comp_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Comp_Sem.Uses_Queuing));
      end if;
   end Control_Stmt_Action;

   procedure Conditional_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Conditional.Tree) is
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);
   begin
      Visit_Resolved (T.Cond, Visitor);
      --  TBD: Might want to preserve path condition in references
      Pre_Cg (T.Then_Part, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);
      Pre_Cg (T.Else_Part, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);

      --  Combine in the exit effects, which effectively happen after
      --  the conditional is done.
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Comp_Sem.Exit_Effects_RW,
         How_Combined => Object_Access.Sequential);

      --  Propagate worst-case "Slow_Calls" and para-blocks from constituents
      Comp_Sem.Slow_Calls :=
         Combine_Sequential_Computations
           (Slow_Calls (T.Cond),
            Combine_Sequential_Computations
               (Slow_Calls (T.Then_Part),
                Slow_Calls (T.Else_Part)));
      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Cond) +
                                    Num_Nested_Blocks (T.Then_Part) +
                                    Num_Nested_Blocks (T.Else_Part);
      Comp_Sem.Uses_Queuing := Uses_Queuing (T.Cond)
                              or else Uses_Queuing (T.Then_Part)
                              or else Uses_Queuing (T.Else_Part);

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
         --  Combine entry/exit info
         Comp_Sem.Entry_Exit_Info := Combine_Entry_Exit_Info
           (Lists.Make (Tree_Array'(T.Cond, T.Then_Part, T.Else_Part)));
      end if;

      --  Count finalizable temps for conditional expressions
      case T.Kind is
      when Conditional.If_Stmt | Conditional.Elsif_Stmt =>
         null;
      when Conditional.If_Expr | Conditional.Elsif_Expr |
           Conditional.Quest_Colon =>
         --  Count all of the finalizable temps
         Comp_Sem.Num_Finalizable_Temps := Num_Finalizable_Temps (T.Cond) +
           Num_Finalizable_Temps (T.Then_Part) +
           Num_Finalizable_Temps (T.Else_Part);
      end case;

      if Debug_Pre_Cg then
         Put_Line
           (" For conditional " &
            Subtree_Image (Optional (T'Access), Use_Short_Form => True) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Comp_Sem.Slow_Calls) &
            ", Num_Nested_Blocks =" &
            Natural'Image (Comp_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Comp_Sem.Uses_Queuing) &
            ", Num_Finalizable_Temps =" &
            Natural'Image (Comp_Sem.Num_Finalizable_Temps));
      end if;
   end Conditional_Action;

   procedure For_Loop_Construct_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out For_Loop_Construct.Tree) is
      For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr :=
        For_Loop_Construct_Sem_Ptr (T.Sem_Info);
      Iterator_RW : Object_Access.Read_Write_Mapping;
      Loop_Body_RW : Object_Access.Read_Write_Mapping
        renames For_Loop_Sem.Loop_Body_RW;
      Elems : Lists.List;  --  for entry/exit info
      use type Interpreter.Direction;
      use type For_Loop_Construct.For_Loop_Kind_Enum;
   begin
      --  Make sure enclosing_for_loop is still correct in the iterators,
      --  in case we constructed a new for_loop_sem during overload resolution.
      for I in For_Loop_Sem.Iterator_Sems'Range loop
         For_Loop_Sem.Iterator_Sems (I).Enclosing_For_Loop := For_Loop_Sem;
      end loop;

      --  Walk the prologue declarations first, if any
      Pre_Cg_List (T.Prologue, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);

      --  NOTE: The "while" condition in a value iterator is treated
      --        as though it is part of the loop body, for the purposes
      --        of race-condition detection, so when walking a "while"
      --        condition of an iterator, it updates
      --        Iterator_Sem.Enclosing_For_Loop.Loop_Body_RW rather than
      --        the passed-in Iterator_RW.
      Pre_Cg_List (T.Iterators, Iterator_RW,
        Annotation_Mode => Visitor.Annotation_Mode);

      Pre_Cg (T.Filter, Loop_Body_RW,
        Annotation_Mode => Visitor.Annotation_Mode);

      For_Loop_Sem.Num_Nested_Blocks := Combine_Operand_Blocks (T.Iterators);

      if T.Kind /= For_Loop_Construct.For_Loop_Statement then
         --  Combine Num_Finalizable_Temps;
         --  check to see if the body is a reference,
         --  which is used in container aggregates when we are
         --  specifying a key for a map entry.
         declare
            Loop_Body_Tree : Trees.Tree'Class renames
                                Tree_Ptr_Of (T.Loop_Body).all;
         begin
            if Loop_Body_Tree in Reference.Tree then
               declare
                  Ref : Reference.Tree renames Reference.Tree (Loop_Body_Tree);
               begin
                  Pre_Cg (Ref.Key, Loop_Body_RW,
                    Annotation_Mode => Visitor.Annotation_Mode);
                  Pre_Cg (Ref.Referent, Loop_Body_RW,
                    Annotation_Mode => Visitor.Annotation_Mode);
                  For_Loop_Sem.Num_Finalizable_Temps :=
                    Num_Finalizable_Temps (Ref.Key) +
                      Num_Finalizable_Temps (Ref.Referent);

                  For_Loop_Sem.Num_Nested_Blocks :=
                    For_Loop_Sem.Num_Nested_Blocks +
                      Num_Nested_Blocks (Ref.Key) +
                      Num_Nested_Blocks (Ref.Referent);

                  For_Loop_Sem.Slow_Calls :=
                    Combine_Sequential_Computations (Slow_Calls (Ref.Key),
                      Slow_Calls (Ref.Referent));

                  For_Loop_Sem.Uses_Queuing :=
                    Uses_Queuing (Ref.Key) or else Uses_Queuing (Ref.Referent);

                  if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
                     --  Add to list for entry/exit info
                     Lists.Append (Elems, Ref.Key);
                     Lists.Append (Elems, Ref.Referent);
                  end if;
               end;
            else
               Pre_Cg (T.Loop_Body, Loop_Body_RW,
                 Annotation_Mode => Visitor.Annotation_Mode);
               For_Loop_Sem.Num_Finalizable_Temps :=
                 Num_Finalizable_Temps (T.Loop_Body);

               For_Loop_Sem.Num_Nested_Blocks :=
                 For_Loop_Sem.Num_Nested_Blocks +
                   Num_Nested_Blocks (T.Loop_Body);

               For_Loop_Sem.Slow_Calls := Slow_Calls (T.Loop_Body);

               For_Loop_Sem.Uses_Queuing := Uses_Queuing (T.Loop_Body);

               if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
                  --  Add to list for entry/exit info
                  Lists.Append (Elems, T.Loop_Body);
               end if;
            end if;
         end;
      else
         Pre_Cg (T.Loop_Body, Loop_Body_RW,
           Annotation_Mode => Visitor.Annotation_Mode);

         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            --  Add to list for entry/exit info
            Lists.Append (Elems, T.Loop_Body);
         end if;

         For_Loop_Sem.Num_Nested_Blocks :=
           For_Loop_Sem.Num_Nested_Blocks +
             Num_Nested_Blocks (T.Loop_Body);

         For_Loop_Sem.Slow_Calls := Slow_Calls (T.Loop_Body);

         For_Loop_Sem.Uses_Queuing := Uses_Queuing (T.Loop_Body);
      end if;

      if For_Loop_Sem.Is_Potentially_Concurrent then
         --  Is potentially concurrent loop, loop-body RW must only update
         --  variables local to the loop, or elements of an array which are
         --  indexed by a loop variable.
         Object_Access.Check_Concurrent_Loop_Body
           (For_Loop_Sem.Loop_Param_Region, Loop_Body_RW);
      elsif not Insert_Implicit_Parallel_Loops then
         --  Turn off use of parallel nested block
         For_Loop_Sem.Uses_Parallel_Nested_Block := False;
         --  There is an implicit exit for each iterator.
         For_Loop_Sem.Num_Exits := For_Loop_Sem.Num_Exits +
           For_Loop_Sem.Iterator_Sems'Length;
      end if;

      --  Now combine iterator RW and loop body RW into visitor RW
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Iterator_RW,
         How_Combined => Object_Access.Sequential);

      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Loop_Body_RW,
         How_Combined => Object_Access.Sequential);

      Pre_Cg (T.End_With_Values, Visitor.Read_Write,
        Annotation_Mode => Visitor.Annotation_Mode);

      For_Loop_Sem.Num_Nested_Blocks :=
        For_Loop_Sem.Num_Nested_Blocks +
          Num_Nested_Blocks (T.End_With_Values);

      --  Combine in the exit effects, which effectively happen after
      --  the loop is done.
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => For_Loop_Sem.Exit_Effects_RW,
         How_Combined => Object_Access.Sequential);

      --  Determine total number of nested blocks for loop
      For_Loop_Sem.Num_Nested_Blocks :=
        For_Loop_Sem.Num_Nested_Blocks + Num_Nested_Blocks (T.End_With_Values);

      if For_Loop_Sem.Uses_Parallel_Nested_Block then
         --  Add one for loop body itself
         For_Loop_Sem.Num_Nested_Blocks := For_Loop_Sem.Num_Nested_Blocks + 1;
         For_Loop_Sem.Slow_Calls := Mandatory_Parallel_Call;
      else
         --  Propagate worst-case "Slow_Calls" from constituents
         For_Loop_Sem.Slow_Calls :=
           Combine_Sequential_Computations (For_Loop_Sem.Slow_Calls,
             Slow_Calls (T.End_With_Values));

         for I in For_Loop_Sem.Iterator_Sems'Range loop
            For_Loop_Sem.Slow_Calls := Combine_Sequential_Computations
              (For_Loop_Sem.Slow_Calls,
               For_Loop_Sem.Iterator_Sems (I).Slow_Calls);
         end loop;
      end if;

      if Not_Null (T.Filter) then
         --  Include count of nested blocks in filter,
         --  but subtract one because filter is not evaluated out of line.
         pragma Assert (Num_Nested_Blocks (T.Filter) >= 1);
         For_Loop_Sem.Num_Nested_Blocks := For_Loop_Sem.Num_Nested_Blocks +
           Num_Nested_Blocks (T.Filter) - 1;
         For_Loop_Sem.Slow_Calls := Combine_Sequential_Computations
           (For_Loop_Sem.Slow_Calls, Slow_Calls (T.Filter));
      end if;

      For_Loop_Sem.Uses_Queuing := Combine_Operand_Queuing (T.Iterators)
                                  or else For_Loop_Sem.Uses_Queuing
                                  or else Uses_Queuing (T.Filter)
                                          --  NOTE: Filter should not use
                                          --  queuing
                                  or else Uses_Queuing (T.End_With_Values);

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
         --  Compute Entry_Exit_Info
         Lists.Append (Elems, T.Iterators);
         Lists.Append (Elems, T.Filter);
         Lists.Append (Elems, T.End_With_Values);
         For_Loop_Sem.Entry_Exit_Info := Combine_Entry_Exit_Info (Elems);
      end if;

      if Debug_Pre_Cg or Debug_Entry_Exit then
         Put_Line
           (" For for_loop_construct " &
            Subtree_Image (Optional (T'Access), Use_Short_Form => True) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (For_Loop_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (For_Loop_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (For_Loop_Sem.Uses_Queuing) &
            ", Num_Finalizable_Temps =" &
            Natural'Image (For_Loop_Sem.Num_Finalizable_Temps));
         if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
            Put_Entry_Exit_Info (For_Loop_Sem.Entry_Exit_Info);
         end if;
      end if;
   end For_Loop_Construct_Action;

   procedure Iterator_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Iterator.Tree) is
      Iterator_Sem : constant Iterator_Sem_Ptr :=
        Iterator_Sem_Ptr (T.Sem_Info);
      Iterator_Sym : constant Symbols.Sym_Ptr :=
        Iterator_Sem.Associated_Symbol;
      Loop_Var_Is_Declared_Ref : constant Boolean :=
        T.Kind in Iterator.Value_Iterator
          and then Static.Sym_Is_Declared_Ref (Iterator_Sym);
      Initial_Value_Read_Write : Object_Access.Read_Write_Mapping;
      Initial_Value_Modes : constant array (Boolean)
        of Object_Access.Access_Mode_Enum :=
          (Object_Access.Read_Access, Object_Access.Ref_Access);
      Mode_To_Use : constant Object_Access.Access_Mode_Enum :=
        Initial_Value_Modes (Loop_Var_Is_Declared_Ref);
      For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr :=
        Iterator_Sem.Enclosing_For_Loop;
      Initial_Value_Combinations : constant array (Boolean)
        of Object_Access.Read_Write_Combination_Enum :=
          (Object_Access.Sequential, Object_Access.Parallel);
      Combination_To_Use : constant
        Object_Access.Read_Write_Combination_Enum :=
          Initial_Value_Combinations (For_Loop_Sem.Is_Potentially_Concurrent);
   begin
      Pre_Cg (T.Obj_Value, Initial_Value_Read_Write, Mode => Mode_To_Use,
        Annotation_Mode => Visitor.Annotation_Mode);

      Pre_Cg_List (T.Next_Values, Initial_Value_Read_Write,
        How_Combined => Combination_To_Use,
        Mode => Mode_To_Use,
        Annotation_Mode => Visitor.Annotation_Mode);

      if Loop_Var_Is_Declared_Ref then
         --  Save rw-mapping associated with Ref for future use.
         Object_Access.Save_Mapping_For_Ref (Iterator_Sem.Object_Ref,
           Initial_Value_Read_Write, Iterator_Sem.Object_Id,
           Iterator.Find_Source_Pos (T));
      end if;

      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Initial_Value_Read_Write,
         How_Combined => Object_Access.Sequential,
         Mode => Mode_To_Use);

      --  Treat "while" condition as though it is part of the loop body
      --  for the purposes of race-condition detection.
      Pre_Cg (T.While_Cond, Iterator_Sem.Enclosing_For_Loop.Loop_Body_RW,
        Annotation_Mode => Visitor.Annotation_Mode);

      --  Propagate worst-case "Slow_Calls" and para-blocks from constituents
      Iterator_Sem.Slow_Calls :=
         Combine_Sequential_Computations
           (Slow_Calls (T.Obj_Value),
            Combine_Sequential_Computations
               (Combine_Operand_Calls (T.Next_Values),   --  TBD: Is this
                                                         --  right?
                Slow_Calls (T.While_Cond)));
      Iterator_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Obj_Value) +
                                        Combine_Operand_Blocks
                                           (T.Next_Values) +
                                        Num_Nested_Blocks (T.While_Cond);
      Iterator_Sem.Uses_Queuing := Uses_Queuing (T.Obj_Value)
                                  or else Combine_Operand_Queuing
                                             (T.Next_Values)
                                  or else Uses_Queuing (T.While_Cond);

      if Visitor.Annotation_Mode in Pre_Post_Condition_Mode then
         --  Compute Entry_Exit_Info
         declare
            Elems : Lists.List :=
              Lists.Make
                (Tree_Array'(T.Obj_Value, T.While_Cond));
         begin
            Lists.Append (Elems, T.Next_Values);
            Iterator_Sem.Entry_Exit_Info := Combine_Entry_Exit_Info (Elems);
            if Iterator_Sem.Key_Sem /= null then
               --  Copy the entry/exit info down onto the key
               --  where we will retrieve it on use of the key.
               Iterator_Sem.Key_Sem.Entry_Exit_Info :=
                 Iterator_Sem.Entry_Exit_Info;
            end if;
         end;
      end if;

      if Debug_Pre_Cg then
         Put_Line
           (" For iterator " &
            Subtree_Image (Optional (T'Access)) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Iterator_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (Iterator_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Iterator_Sem.Uses_Queuing));
      end if;
   end Iterator_Action;

   procedure While_Stmt_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out While_Stmt.Tree) is
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);
   begin
      Pre_Cg (T.While_Cond, Visitor.Read_Write);
      Pre_Cg (T.Loop_Body, Visitor.Read_Write);
      Pre_Cg (T.End_With_Values, Visitor.Read_Write);

      --  Combine in the exit effects, which effectively happen after
      --  the loop is done.
      Object_Access.Combine
        (Visitor.Read_Write,
         Addition => Comp_Sem.Exit_Effects_RW,
         How_Combined => Object_Access.Sequential);

      --  Propagate worst-case "Slow_Calls" and para-blocks from constituents
      Comp_Sem.Slow_Calls :=
         Combine_Sequential_Computations
           (Slow_Calls (T.While_Cond),
            Combine_Sequential_Computations
               (Slow_Calls (T.Loop_Body),
                Slow_Calls (T.End_With_Values)));
      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.While_Cond) +
                                    Num_Nested_Blocks (T.Loop_Body) +
                                    Num_Nested_Blocks (T.End_With_Values);
      Comp_Sem.Uses_Queuing := Uses_Queuing (T.While_Cond)
                              or else Uses_Queuing (T.Loop_Body)
                              or else Uses_Queuing (T.End_With_Values);
      if Debug_Pre_Cg then
         Put_Line
           (" For while_stmt " &
            Subtree_Image (Optional (T'Access), Use_Short_Form => True) &
            " Slow_Calls = " &
            Slow_Call_Enum'Image (Comp_Sem.Slow_Calls) &
            ", Num_Nested_Blocks = " &
            Natural'Image (Comp_Sem.Num_Nested_Blocks) &
            ", Uses_Queuing = " &
            Boolean'Image (Comp_Sem.Uses_Queuing));
      end if;
   end While_Stmt_Action;

   procedure Selection_Action
     (Visitor : in out Pre_Cg_Visitor;
      T : in out Selection.Tree) is
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);
      Prefix_RW : Object_Access.Read_Write_Mapping;
   begin
      Pre_Cg (T.Prefix, Prefix_RW, Mode => Object_Access.Ref_Access,
        Annotation_Mode => Visitor.Annotation_Mode);

      if not Static.Is_Unlocked_Concurrent_Operand
               (Operand_Sem_Ptr (Comp_Sem))
        and then Static.Sem_Info_Is_For_Variable (Sem_Ptr (Comp_Sem))
      then
         Object_Access.Refer_To_Selected_Component
           (Visitor.Read_Write,
            Enclosing_Object => Prefix_RW,
            Selector =>
              Selection_Sem_Ptr (Comp_Sem).Comp_Decl.Associated_Symbol,
            Mode => Object_Access.Ref_Access,
            Source_Pos => Selection.Find_Source_Pos (T));
      end if;

      --  Propagate slow calls and para-block from prefix.
      --  (selector is just an identifier).
      Comp_Sem.Slow_Calls := Slow_Calls (T.Prefix);
      Comp_Sem.Num_Nested_Blocks := Num_Nested_Blocks (T.Prefix);
      Comp_Sem.Uses_Queuing := Uses_Queuing (T.Prefix);
      Comp_Sem.Entry_Exit_Info := Entry_Exit_Info (T.Prefix);
   end Selection_Action;

   ------------- Code gen actions ------------

   type Block_Info is record    --  nested block info
      Code : Interpreter.Code_Ptr := null;
      Invokers : Instr_Loc_Array_Ptr := Empty_Instr_Loc_Array;
      --  Invokers(0) is original call on nested block
      --  Invokers(1..Num_Next_Values) are recursive calls inside loop body.
      Waiter : Instr_Loc := (Block => 0, Instr => 0);
      Cb : Interpreter.Code_Block_Descriptor :=
        (Pc_Offset => 0,
         Uses_Queuing => False,
         Uses_Stg_Rgn => False,
         Local_Area_Length => 0,
         Start_Callee_Locals => 0,
         Nesting_Level => 1);
   end record;

   type Block_Info_Array is array (Block_Index range <>) of Block_Info;
   type Block_Info_Array_Wrapper (Num_Blocks : Block_Index) is record
   --  Use wrapper so can be aliased
      Num_Blocks_Generated : Block_Index := 0;
      Info : Block_Info_Array (0 .. Num_Blocks);
   end record;

   type Block_Info_Array_Ptr is access all Block_Info_Array_Wrapper;

   type Code_Gen_Visitor is new Visitor.RW_Tree_Visitor with record
      Decl_Region : Symbols.Region_Ptr;
      Num_Instrs : Interpreter.Code_Length_Type := 0;
      Last_Instr_Escapes : Boolean := False;  --  True after return/exit
      Op_Sem : Operation_Sem_Ptr;  --  Operation being compiled.
      New_Routine : Interpreter.Routine_RW_Ptr;
      Is_Lvalue_Context : Boolean := False;
      --  Address of object identified by expression is recorded
      --  in Lvalue_Location; Target_Local_Offset might be used but
      --  is always bumped.
      Lvalue_Location : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  Address recorded here if Is_Lvalue_Context is True
      --  Target_Local_Offset still bumped, even if not used.
      Target_Object : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator;
      --  If non-null, this object determines the region in which
      --  the result is wanted.
      Target_Local_Offset : Interpreter.Offset_Within_Area := 0;
      --  Target of evaluation.  Bumped as a result of "visit" of operand.
      Target_VM_Info : Interpreter.VM_Obj_Id_Type := Interpreter.No_VM_Obj_Id;
      --  Target VM info for evaluation.  Must be set before calling "visit"

      Dest_Name           : Strings.U_String_Index :=
                              Strings.Null_U_String_Index;
      --  Name to be used for Dest_Name in store instruction

      Create_Polymorphic_Obj : Boolean := False;
      --  If True, then upon return from Visit, the Create_Polymorphic_Obj_Op
      --  will be emitted.
      --  By setting this to false, we suppress that action, which is
      --  important when copying an already-polymorphic saved constant.

      Start_Callee_Locals : Interpreter.Offset_Within_Area := 0;
      Local_Master : Interpreter.Offset_Within_Area := 0;
      Master_In_Use : Boolean := False;
      Master_Is_Started : Boolean := False;
      --  Have emitted Start_Parallel*
      Master_Is_Complete : Boolean := False;
      --  Have emitted Wait_For_...

      --  Block indices of nested blocks associated with
      --  current wait-for-parallel instruction.
      First_Awaited_Block : Block_Index := 0;
      Last_Awaited_Block : Block_Index := 0;

      Is_Leftmost : Boolean := False;
      --  Leftmost subtree with slow call
      Gen_Parallel_Invocations_Only : Boolean := False;
      --  This is set to True when we encounter an invocation of an operation
      --  (including an operator or an assignment) where we are allowed
      --  to evaluate the operands in parallel.  We end up walking
      --  the operands twice, once generating parallel invocations,
      --  and then once using the results of the parallel invocations
      --  (after an appropriate wait-for-parallel on the master).
      --  If this is True when emitting code for a construct that
      --  cannot have any sub-operands that are out-of-line calls,
      --  then we just return immediately.  Otherwise we visit those
      --  sub-operands which might have out-of-line calls, and which
      --  are executed unconditionally.  Sub-operands that are not always
      --  executed (such as the right operand of an "and then") are *not*
      --  walked if this flag is True.
      Enclosing_For_Loop : For_Loop_Construct_Sem_Ptr := null;
      --  This is initialized when processing Iterators, and when
      --  immediately inside a for-loop body.

      --  These identify a place where to allocate next finalizable temp.
      --  The offset is bumped each time a finalizable temp is allocated.
      Finalizable_Temp_Level : Static_Level := 0;
      Finalizable_Temp_Offset : Interpreter.Offset_Within_Area := 0;

      Nested_Blocks : Block_Info_Array_Ptr;
      --  The set of nested code blocks within the current operation
      Current_Code : Interpreter.Code_Ptr := null;
      --  The stream of instructions for the current nested block
      Current_Block : Block_Index := 0;
      --  The index of the current nested block; 0 = main block of op.
      Current_Level : Static_Level := 0;
      --  The static nesting level of the current code.

      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode;
      --  Indicates whether visiting annotation expression
      --  for pre/postcondition.
   end record;

   pragma Warnings (Off);  --  Suppress warnings about non-dispatching ops
   procedure Code_Gen
     (Cg_Visitor : in out Code_Gen_Visitor;
      Decl : Optional_Tree;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode);
   --  Generate PSVM code for Decl given visitor

   procedure Code_Gen_List
     (Cg_Visitor : in out Code_Gen_Visitor;
      Decl_List : Lists.List;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode);
   --  Apply Code_Gen to each element in Decl_List using given visitor
   pragma Warnings (On);

   procedure Pre_Visit
     (Visitor : in out Code_Gen_Visitor;
      T : in out Trees.Tree'Class);

   procedure Post_Visit
     (Visitor : in out Code_Gen_Visitor;
      T : in out Trees.Tree'Class);

   procedure Module_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Module.Tree);

   procedure Implements_Element_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Implements_Element.Tree);

   procedure Operation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Operation.Tree);

   procedure Obj_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Obj_Decl.Tree);

   procedure Param_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Param_Decl.Tree);

   procedure Type_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Type_Decl.Tree);

   procedure Unary_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Unary.Tree);

   procedure Binary_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Binary.Tree);

   procedure Annotation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Annotation.Tree);

   procedure Identifier_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Identifier.Tree);

   procedure Property_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Property.Tree);

   procedure Qualified_Name_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Qualified_Name.Tree);

   procedure Assign_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Assign_Stmt.Tree);

   procedure Invocation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Invocation.Tree);

   procedure Block_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Block_Stmt.Tree);

   procedure Case_Construct_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Case_Construct.Tree);

   procedure Control_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Control_Stmt.Tree);

   procedure Conditional_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Conditional.Tree);

   procedure For_Loop_Construct_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out For_Loop_Construct.Tree);

   procedure Iterator_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Iterator.Tree);

   procedure While_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out While_Stmt.Tree);

   procedure Selection_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Selection.Tree);

   --------

   use type Interpreter.Condition_Bit_Mask;

   Low_Bound_Condition : constant
     array (Binary.Interval_Ops) of Interpreter.Condition_Bit_Mask :=
     (Binary.Closed_Interval_Op | Binary.Closed_Open_Interval_Op =>
     Interpreter.Compare_Greater or Interpreter.Compare_Equal,
      Binary.Open_Interval_Op | Binary.Open_Closed_Interval_Op =>
     Interpreter.Compare_Greater);
   --  If_Op Condition to use when comparing value against
   --  low bound of interval.

   High_Bound_Condition : constant
     array (Binary.Interval_Ops) of Interpreter.Condition_Bit_Mask :=
     (Binary.Closed_Interval_Op | Binary.Open_Closed_Interval_Op =>
     Interpreter.Compare_Less or Interpreter.Compare_Equal,
      Binary.Open_Interval_Op | Binary.Closed_Open_Interval_Op =>
     Interpreter.Compare_Less);
   --  If_Op Condition to use when comparing value against
   --  high bound of interval.

   function Get_Known_Type_Descriptor (Obj_Type : Type_Sem_Ptr)
     return Interpreter.Type_Descriptor_Ptr;
   --  Return a type descriptor for a type that has All_Parameters_Known

   function Adjust_For_Level_And_Prefix
     (Current_Level : Static_Level;
      Obj_Location : Interpreter.Object_Locator;
      Obj_Level : Static_Level;
      Decl_Region : Region_Ptr := null;
      Obj_Ref : Sym_Ref_Ptr := null)
      return Interpreter.Object_Locator;
      --  Return object-locator, adjusted if Current_Level > Obj_Level
      --  If Obj_Location is type-area relative, then
      --  check to see region for obj_ref is nearest enclosing module.  If not
      --  adjust for number of nested modules to be skipped over.
      --  If not from an enclosing module, use Prefix_Type to
      --  create a nested constant if necessary.

   pragma Warnings (Off);  --  Suppress warnings about non-dispatching ops
   procedure Emit_Copy_Obj_Or_Word
     (Visitor : in out Code_Gen_Visitor;
      Destination : Interpreter.Object_Locator;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index;
      Source : Interpreter.Object_Locator;
      Target_Object : Interpreter.Object_Locator;
      Opnd_Sem : Operand_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position);
      --  Emit either Copy_Word or Make_Copy_In_Region
      --  depending on whether Target_Object is specified
      --  and whether type might be large.
   pragma Warnings (On);

   procedure Emit
     (Visitor : in out Code_Gen_Visitor;
      Instr : Interpreter.Instruction;
      VM_Checks_Off : Boolean := False) is
      --  Emit an instruction into the code stream
      use Interpreter;

      procedure Check_VM_Info (Locator : Object_Locator) is
      --  Make sure VM info is filled in properly
      begin
         case Locator.VM_Obj_Id.Kind is
            when Local_Kind =>
               pragma Assert (Locator.VM_Obj_Id.Num /= 0);

               --  Should not reference Var before declared
               pragma Assert (Locator.VM_Obj_Id.Num /=
                 Visitor.Current_Code.Most_Recent_Var);

               pragma Assert (Locator.VM_Obj_Id.Is_Var =
                                (Locator.VM_Obj_Id.Indir > 0));
               if Locator.Base in Base_Registers
                 or else (Locator.Base in Phys_Base_Registers
                          and then Locator.Offset = 0)
               then
                  pragma Assert (Locator.VM_Obj_Id.Indir > 0);
                  pragma Assert (Locator.VM_Obj_Id.Is_Var);
                  null;
               end if;
            when Component_Kind =>
               pragma Assert (Locator.VM_Obj_Id.Num /= 0);
               pragma Assert (Locator.VM_Obj_Id.Offset /= 0);
               pragma Assert (Locator.VM_Obj_Id.Is_Var =
                                (Locator.VM_Obj_Id.Indir > 0));
               null;
            when No_VM_Obj_Kind | Param_Kind | Temp_Kind =>
               pragma Assert (Locator.Base /= Local_Area);
               pragma Assert (Locator.Base not in
                                 Enclosing_Local_Areas);
               pragma Assert (Locator.Base not in
                                 Base_Registers);
               pragma Assert (Locator.Base not in
                                 Phys_Base_Registers);
               null;
         end case;
      end Check_VM_Info;

   begin  --  Emit

      Visitor.Last_Instr_Escapes := False;  --  Reset state
      Visitor.Num_Instrs := Visitor.Num_Instrs + 1;
      Visitor.Current_Code.Instrs (Visitor.Num_Instrs) := Instr;

      case Instr.Op is
         when Start_Parallel_Op | Start_Handled_Op | Start_Parallel_Call_Op =>
            --  These instructions use local storage regions or
            --  initialize a local master.  We do some housekeeping for
            --  local masters in initialize/finalize_stg_rgn,
            --  so we need to call them for these instructions.
            Visitor.Current_Code.Uses_Stg_Rgn := True;

         when Store_Large_Local_Null_Op =>
            --  This needs a local storage region if target is this local area
            if Instr.Local_Addr.Base = Local_Area then
               Visitor.Current_Code.Uses_Stg_Rgn := True;
            end if;

         when Store_Local_Null_Op =>
            --  This uses a local storage region if the type might
            --  be large
            if Instr.Null_Type_Info.Base = Zero_Base then
               --  Check whether type is known to be small
               if Instr.Null_Type_Info.Offset /= 0 then
                  declare
                     Type_Desc : constant Type_Descriptor_Ptr :=
                       Type_Descriptor_Ops.To_Type_Desc_Or_Op_Map
                         (Type_Index (Instr.Null_Type_Info.Offset));
                  begin
                     --  Needs storage region if not small
                     if not Type_Desc.Is_Small then
                        Visitor.Current_Code.Uses_Stg_Rgn := True;
                     end if;
                  end;
               end if;
            else
               --  Assume might be large
               Visitor.Current_Code.Uses_Stg_Rgn := True;
            end if;

         when Create_Obj_Op | Create_Polymorphic_Obj_Op
            | Store_Operation_Desc_Op =>
            if Instr.Source = Null_Object_Locator then
               --  Create_Obj uses local storage region if no source
               Visitor.Current_Code.Uses_Stg_Rgn := True;
            end if;

         when Store_Str_Lit_Op =>
            if not Univ_String_Type.Known_To_Be_Small
              and then Instr.Existing_Str_In_Stg_Rgn = Null_Object_Locator
            then
               --  String lit uses local storage region if no target
               Visitor.Current_Code.Uses_Stg_Rgn := True;
            end if;

         when Return_Op | Exit_Op =>
            --  These two instructions cause an "escape" and don't need
            --  to be followed by a return or exit if they come at end
            --  of the routine or nested block.
            --  NOTE: We might consider an unconditional (negative) Skip_Op
            --        as an "escape," but we don't do that yet.
            Visitor.Last_Instr_Escapes := True;

         when Store_Address_Op =>
            --  Check that we aren't storing the address of X into X.
            if Instr.Source = Instr.Destination then
               Sem_Error
                 ("Internal: Store_Address_Op with " &
                    "Source = Destination",
                  Src_Pos => Instr.Source_Pos);
            end if;

            --  Check whether VM_Info is filled in
            if Debug_VM_Info and then not VM_Checks_Off then
               Check_VM_Info (Instr.Destination);
               Check_VM_Info (Instr.Source);

               --  Check that Source is at a level of indirection
               if Instr.Source.VM_Obj_Id.Kind = Local_Kind then
                  pragma Assert (Instr.Source.VM_Obj_Id.Indir > 0);
                  null;
               end if;

            end if;

         when Copy_Word_Op | Assign_Word_Op | Copy_Address_Op =>

            --  Check whether VM_Info is filled in
            if Debug_VM_Info and then not VM_Checks_Off then
               Check_VM_Info (Instr.Destination);
               Check_VM_Info (Instr.Source);
            end if;

            --  Check that Source is not zero based
            if Instr.Source.Base = 0 then
               Sem_Error ("Internal: Source is zero based",
                          Src_Pos => Instr.Source_Pos);
            end if;

         when Declare_Obj_Op =>
            if Debug_VM_Info and then not VM_Checks_Off then
               --  Destination should not be a null locator
               pragma Assert (Instr.Destination.Base /= Zero_Base);
               null;
            end if;

            if Instr.Destination.VM_Obj_Id.Is_Var then
               --  Should be declaring most recently assigned var

               if Debug_VM_Info and then not VM_Checks_Off then
                  pragma Assert
                    (Instr.Destination.VM_Obj_Id.Num =
                       Visitor.Current_Code.Most_Recent_Var
                      or else
                     Visitor.Current_Code.Most_Recent_Var = 0);
                  null;
               end if;

               --  Now mark it as declared
               Visitor.Current_Code.Most_Recent_Var := 0;
            end if;

         when others =>
            --  Other instructions don't need a local storage region
            --  and don't cause an escape.
            null;
      end case;

      if Debug_Code_Gen then
         Put_Line
           (" --> " &
            Opcode_Enum'Image (Instr.Op) &
            " at" &
            Code_Offset'Image (Visitor.Num_Instrs));
      end if;
   end Emit;

   procedure Check_High_Water (Visitor : in out Code_Gen_Visitor) is
      --  Update Start_Callee_Locals to accommodate current
      --  value of Target_Local_Offset.
      use type Interpreter.Offset_Within_Area;
   begin
      --  Remember the high-water mark
      if Visitor.Target_Local_Offset > Visitor.Start_Callee_Locals then
         Visitor.Start_Callee_Locals := Visitor.Target_Local_Offset;
      end if;
   end Check_High_Water;

   procedure Check_And_Set_Local_Offset
     (Visitor : in out Code_Gen_Visitor;
      New_Value : Interpreter.Offset_Within_Area) is
      --  Remember current Target_Local_Offset as a high-water,
      --  and then give it a new value.
      use type Interpreter.Offset_Within_Area;
   begin
      if New_Value > Visitor.Target_Local_Offset then
         --  Bump up now before calling Check_High_Water
         Visitor.Target_Local_Offset := New_Value;
      end if;
      Check_High_Water (Visitor);
      Visitor.Target_Local_Offset := New_Value;
   end Check_And_Set_Local_Offset;

   procedure Check_Postconditions
     (Visitor : in out Code_Gen_Visitor;
      Op_Sem  : Operation_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position) is
   --  Emit a Check_Not_Null_Op if operation is supposed to return a non-null
   --  value.
   --  Emit a Check_Nested_Block if there are other postconditions.
      Op_Def : Operation.Tree renames
        Operation.Tree (Tree_Ptr_Of (Op_Sem.Definition).all);
      Outputs : Lists.List renames Op_Def.Operation_Outputs;
   begin
      --  Generate code to check postconditions.
      --  NOTE: Will first have to walk postconditions looking for
      --       subtrees using Var parameters (without the "'"),
      --       and replace with result of use, which will need to be
      --       computed and saved on entry to the operation.

      if Checking_Postconditions
        and then Not_Null (Op_Def.Postconditions)
      then
         --  Generate code for postcondtion
         Code_Gen (Visitor, Op_Def.Postconditions,
           Annotation_Mode => Postcondition_Mode'Last);
      end if;

      if Checking_Postconditions and then Lists.Length (Outputs) > 0 then
         declare
            use Interpreter;
            First_Output : constant Optional_Tree :=
              Lists.Nth_Element (Outputs, 1);
            First_Out_Param : Param_Decl.Tree renames
              Param_Decl.Tree (Tree_Ptr_Of (First_Output).all);
            Output_Sem : constant Object_Sem_Ptr :=
              Object_Sem_Ptr (Sem_Info (First_Output));
            Output_Is_By_Ref : constant Boolean :=
              Static.Sym_Is_By_Ref (Output_Sem.Associated_Symbol);
            Adjusted_Target : constant Object_Locator :=
              Adjust_For_Level_And_Prefix
                 (Visitor.Current_Level,
                  (Param_Area, 0, No_VM_Obj_Id),
                  Output_Sem.Info.Obj_Level);
            --  Locator for output of operation, handling case where
            --  it requires an up-level reference.
            Enc_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
         begin

            --  Generate postcondition checks, if any
            if Not_Null (First_Out_Param.Pre_Annotation) then
               Code_Gen (Visitor, First_Out_Param.Pre_Annotation,
                 Annotation_Mode => Postcondition_Mode'Last);
            end if;

            if Not_Null (First_Out_Param.Post_Annotation) then
               Code_Gen (Visitor, First_Out_Param.Post_Annotation,
                 Annotation_Mode => Postcondition_Mode'Last);
            end if;

            --  See whether output allowed to be null
            if Output_Sem.Resolved_Type.Value_Is_Optional
              or else First_Out_Param.Is_Optional
              or else Static.Type_Wraps_Optional_Component
                        (Output_Sem.Resolved_Type)
              or else Languages.Language in Languages.Ada_Ish
                        --  Might want to handle elsewhere
            then
               --  Value allowed to be null
               return;
            end if;

            --  if "ref" output then use lvalue context and store addr
            if Output_Is_By_Ref then
               --  TBD: Not checking by-ref optionality yet
               null;
            else
               Emit
                 (Visitor,
                  (Op => Check_Not_Null_Op,
                   Source_Pos => Source_Pos,
                   Destination => Adjusted_Target,
                   Dest_Name => Strings.Index
                     (Output_Sem.Associated_Symbol.Str),
                   Null_Type_Info => Run_Time_Type_Info
                                       (Output_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module),
                   Not_Null_Proved => False));
            end if;
         end;
      end if;
   end Check_Postconditions;

   procedure Allocate_Finalizable_Temps
     (Visitor : in out Code_Gen_Visitor; Num_Temps : Natural) is
   --  Allocate space for finalizable temps by starting at
   --  Target_Local_Offset + 1 and bumping by Num_temps
      use Interpreter;
   begin
      if Num_Temps > 0 then
         --  Should not be set yet
         if Debug_Code_Gen then
            Put_Line (" Allocate_Finalizable_Temps: Num_Temps =" &
              Natural'Image (Num_Temps));
            if Visitor.Finalizable_Temp_Offset /= 0 then
               Put_Line ("  Level/Offset already set to:" &
                 Static_Level'Image (Visitor.Finalizable_Temp_Level) & "," &
                 Offset_Within_Area'Image (Visitor.Finalizable_Temp_Offset));
            end if;
         end if;

         Visitor.Finalizable_Temp_Level := Visitor.Current_Level;
         --  Skip over slot for declared object, if any.
         Visitor.Finalizable_Temp_Offset :=
           Visitor.Target_Local_Offset + 1;
         --  Now skip past finalizable temps.
         Visitor.Target_Local_Offset :=
           Visitor.Finalizable_Temp_Offset +
             Offset_Within_Area (Num_Temps);
      end if;
   end Allocate_Finalizable_Temps;

   procedure Finalize_Result_And_Ref_Operands
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree);
   --  Finalize temps associated with result and ref operands,
   --  and for each ref operand, finalize everything below there.

   procedure Finalize_Non_Ref_Operands
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree);
   --  Walk tree and finalize temps associated with non-ref operands
   --  and for each such operand, finalize everything below there as well.
   --  NOTE: If an operand is "moved" into a container, it does not need
   --        to be further finalized, but any enclosing ref-object from
   --        which it was moved does need finalization.

   function Create_Out_Of_Line_Computation
     (OT : Optional_Tree;
      Decl_Region : Region_Ptr := Symbols.Library_Region;
      Enclosing_Type : Interpreter.Type_Descriptor_Ptr := null;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index)
     return Interpreter.Routine_Ptr is
   --  Create a short routine to compute the value of the given expression.
   --  used as an actual obj in a type instantiation, or equivalent.
      use Operation;
      use Interpreter;

      --  Build up a mini operation and then generate code for it.
      Operand_Sem : constant Operand_Sem_Ptr :=
        Operand_Sem_Ptr (Sem_Info (OT));

      function Result_Type_To_Use (Operand_Sem : Operand_Sem_Ptr)
        return Type_Sem_Ptr is
      --  Return Operand_Sem.Target_Polymorphic_Type if non-null,
      --  else return Operand_Sem.Resolved_Type
      begin
         if Operand_Sem.Target_Polymorphic_Type /= null then
            return Operand_Sem.Target_Polymorphic_Type;
         else
            return Operand_Sem.Resolved_Type;
         end if;
      end Result_Type_To_Use;

      Result_Type : constant Type_Sem_Ptr :=
        Result_Type_To_Use (Operand_Sem);
      Result_Type_Ref : constant Optional_Tree := Identifier.Make
        (Str => Result_Type.Associated_Module.Associated_Symbol.Str);
      Result_Type_Ref_Sem : constant Sym_Ref_Ptr := new Sym_Reference_Info'
        (Root_Semantic_Info with
         Associated_Symbol => Result_Type.Associated_Module.Associated_Symbol,
         Nested_Region => null,
         Context => No_Context,
         Interps => null,
         Resolved_Type => Result_Type,
         Resolved_Interp => Null_Optional_Tree,
         Hash_Value => 0,
         Target_Polymorphic_Type => null,
         Entry_Exit_Info => Null_Entry_Exit_Info,
         Entry_Temp_Info => null,
         Prefix_Type_Region => null,
         Underlying_Sem_Info => Sem_Ptr (Result_Type));
      Mini_Op : constant Optional_Tree :=
        Operation.Make
           (Name => Null_Optional_Tree,   --  TBD: Qualified_Name
                                          --  or Identifier
            Operation_Kind => Operation.Func_Operation,
            Operation_Inputs => Lists.Empty_List,
            Operation_Outputs =>
               Lists.Make
                 ((1 => Param_Decl.Make
                          (Name => Null_Optional_Tree,
                           Kind => Param_Decl.Default_Param,
                           Locking => Param_Decl.Not_Locked,
                           Is_Optional => True,
                           Param_Type => Result_Type_Ref,
                           Param_Default => Null_Optional_Tree))),
            Global_Read_List => Lists.Empty_List,
            Global_Update_List => Lists.Empty_List,
            Preconditions => Null_Optional_Tree,
            Postconditions => Null_Optional_Tree,
            Is_Def => True,
            Statements =>
              Control_Stmt.Make
                (Control_Stmt.Return_Stmt,
                 Applies_To => Control_Stmt.Operation_Body,
                 Id => Null_Optional_Tree,
                 Values => OT,
                 Source_Pos => Find_Source_Pos (OT)));

      Read_Write : Object_Access.Read_Write_Mapping;

      --  Save and then turn off implicit parallelism
      Saved_Insert_Implicit_Parallel_Calls : constant Boolean :=
        Insert_Implicit_Parallel_Calls;
      Saved_Insert_Implicit_Parallel_Loops : constant Boolean :=
        Insert_Implicit_Parallel_Loops;

      Dest_Name_To_Use : Strings.U_String_Index := Dest_Name;
      use type Strings.U_String_Index;
   begin
      --  Fill in result-type reference
      Set_Sem_Info (Result_Type_Ref, Root_Sem_Ptr (Result_Type_Ref_Sem));

      if Debug_Code_Gen then
         Put_Line (" Evaluating tree " & Subtree_Image (OT));
      end if;

      if Dest_Name_To_Use = Strings.Null_U_String_Index then
         --  Provide some sort of name for out-of-line routine.
         Dest_Name_To_Use := Strings.Index (Strings.String_Lookup
           (Subtree_Image (OT)));
      end if;

      --  Turn off implicit parallelism
      Insert_Implicit_Parallel_Calls := False;
      Insert_Implicit_Parallel_Loops := False;

      Static.First_Pass (Decl_Region, Mini_Op);
      Static.Second_Pass (Decl_Region, Mini_Op, Context => Statement_Context);
      Pre_Cg (Mini_Op, Read_Write, Object_Access.Not_Combined);
      Code_Gen (Decl_Region, Mini_Op, Dest_Name => Dest_Name_To_Use);

      --  Restore implicit parallelism, if any
      Insert_Implicit_Parallel_Calls := Saved_Insert_Implicit_Parallel_Calls;
      Insert_Implicit_Parallel_Loops := Saved_Insert_Implicit_Parallel_Loops;

      declare
         Op_Sem : Operation_Semantic_Info
           renames Operation_Semantic_Info (Sem_Info (Mini_Op).all);
         Op_Def : Operation.Tree renames Operation.Tree (Tree_Ptr_Of
                                                            (Mini_Op).all);
      begin
         if Sem_Error_Count > 0 or else Op_Sem.Routine = null then
            Sem_Error
              (OT,
               "Evaluation of compile-time-known constant failed");
            return null;
         else
            return Routine_Ptr (Op_Sem.Routine);
         end if;
      end;
   end Create_Out_Of_Line_Computation;

   function Is_Inside_Parameterless_Computation (Op_Sem : Operation_Sem_Ptr)
     return Boolean is
      --  Return True if Op_Sem is a parameterless anonymous computation
      --  created as part of Evaluate_Tree.
      use Operation;
      use Strings;
   begin
      return Op_Sem.Operation_Kind = Func_Operation
        and then
          (Op_Sem.Associated_Symbol = null
             or else
           Op_Sem.Associated_Symbol.Str = Null_U_String
             or else
           Op_Sem.Associated_Symbol.Str = Empty_U_String
             or else
           To_String (Op_Sem.Associated_Symbol.Str)(1) = '$');
   end Is_Inside_Parameterless_Computation;

   function Evaluate_Tree_Out_Of_Line
     (OT : Optional_Tree;
      Decl_Region : Region_Ptr := Symbols.Library_Region;
      Enclosing_Type : Interpreter.Type_Descriptor_Ptr := null;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index)
     return Interpreter.Word_Type is
   --  Create a short routine and call it to evaluate an expression
   --  used as an actual obj in a type instantiation, or equivalent.
      use Operation;
      use Interpreter;

      --  Build up a mini operation and then generate code for it.
      Computation : constant Routine_Ptr :=
        Create_Out_Of_Line_Computation
          (OT, Decl_Region, Enclosing_Type, Dest_Name);

      Operand_Sem : constant Operand_Sem_Ptr :=
        Operand_Sem_Ptr (Underlying_Sem_Info (OT));

      Result : Word_Type := Null_Value;
   begin
      if Computation /= null then
         declare
            New_Stack_Chunk : Stg_Rgn_Chunk_Ptr :=
              Allocate_Stg_Rgn_Chunk (Min_Size => 5_000,
                Server_Index => Main_Thread_Server_Index);
            --  Get a big enough primary stack

            Initial_Offset : constant Offset_Within_Area := 30;
            New_Local_Area_Obj_Addr : constant Object_Address :=
              (New_Stack_Chunk,
               Offset => New_Stack_Chunk.Last_In_Use + Initial_Offset);
            New_Local_Area : constant Word_Ptr := Object_To_Physical_Address
              (New_Local_Area_Obj_Addr);

            Initial_Tcb_Addr : constant Word_Ptr :=
              Object_To_Physical_Address
                ((New_Stack_Chunk,
                  Offset => New_Local_Area_Obj_Addr.Offset -
                    (Thread_Control_Block_Size + Thread_Master_Size)));
            Initial_Tcb : constant Word_Ptr :=
              Interpreter.Initial_Tcb
                (Target_Routine => Routine_Ptr (Computation),
                 New_Tcb        => Initial_Tcb_Addr,
                 Server_Index   => Main_Thread_Server_Index);
            Initial_Stg_Rgn : constant Stg_Rgn_Ptr :=
                 Get_New_Local_Stg_Rgn
                   (New_Local_Area => New_Local_Area,
                    Server_Index => Main_Thread_Server_Index);
            Context : Exec_Context :=
              (Local_Null => Initial_Stg_Rgn.Null_Value,
               Enclosing_Type => Enclosing_Type,
               Local_Stg_Rgn => Initial_Stg_Rgn,
               Control_Area => Initial_Tcb,
               Server_Index => Main_Thread_Server_Index,
               Open_Master => null,
               Params => Object_To_Physical_Address ((New_Stack_Chunk,
                          Offset => New_Stack_Chunk.Last_In_Use + 1)),
               Local_Area => New_Local_Area,
               Local_Area_Length => New_Stack_Chunk.Chunk_Length -
                                    New_Local_Area_Obj_Addr.Offset,
               Start_Callee_Locals => Computation.Start_Callee_Locals);

            Type_Desc : constant Type_Descriptor_Ptr :=
              Get_Known_Type_Descriptor (Operand_Sem.Resolved_Type);

            Thread_Was_Queued : Boolean;
         begin

            if Debug_Code_Gen then
               Put_Line (" Evaluate_Tree of " & Subtree_Image (OT) &
                 ", initial_tcb at " & Hex_Image (Initial_Tcb));
            end if;

            --  Initialize Output, indicating it should be allocated
            --  in Global_Data_Stg_Rgn
            Store_Word
              (Context,
               (Param_Area, 0, No_VM_Obj_Id),
               Null_For_Type_Or_Stg_Rgn
                  (Type_Desc => Type_Desc,
                   Stg_Rgn => Global_Data_Stg_Rgn));

            --  Execute compiled code
            Execute
              (Routine_Ptr (Computation),
               Start_Pc => 1,
               Context => Context,
               Thread_Was_Queued => Thread_Was_Queued);

            pragma Assert (not Thread_Was_Queued);

            --  Free up the initial tcb/master
            Free_Initial_Tcb (Initial_Tcb,
              Server_Index => Main_Thread_Server_Index);

            --  Return the output,
            --  and also store it in global memory
            --  so we can return an address.
            Result := Fetch_Word (Context, (Param_Area, 0, No_VM_Obj_Id));

            --  Release region allocated for this evaluation
            Release_Stg_Rgn (Context.Local_Stg_Rgn);

            --  Release stack chunk
            Release_Stg_Rgn_Chunk (New_Stack_Chunk,
              Server_Index => Main_Thread_Server_Index);
         end;
      end if;

      return Result;
   end Evaluate_Tree_Out_Of_Line;

   procedure Evaluate_Tree_Or_Create_Routine
     (OT : Optional_Tree;
      Decl_Region : Region_Ptr := Symbols.Library_Region;
      Enclosing_Type : Interpreter.Type_Descriptor_Ptr := null;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index;
      Value : out Interpreter.Word_Type;
      Addr : out Interpreter.Object_Virtual_Address;
      Type_Desc : out Interpreter.Type_Descriptor_Ptr;
      Computation : out Interpreter.Routine_Ptr) is
      --  Evaluate an expression used as an actual obj in a type
      --  instantiation, or equivalent.
      --  If this requires creating a short routine to do this,
      --  create the routine and return with Addr = Null_Virtual_Address

      use Interpreter;

      Operand_Sem : constant Operand_Sem_Ptr :=
        Operand_Sem_Ptr (Underlying_Sem_Info (OT));

      Compute_Out_Of_Line : Boolean := True;
      Expr_Tree    : Trees.Tree'Class renames
                       Tree_Ptr_Of (Operand_Sem.Definition).all;
      Expr_To_Eval : Optional_Tree := OT;

      use type PSC.Languages.Language_Enum;
   begin
      if Expr_Tree.Language /= PSC.Languages.Language then
         --  Switch languages
         Semantics.Set_Language (Expr_Tree.Language);
         if Debug_Code_Gen then
            Ada.Text_IO.Put_Line ("Language now " &
              PSC.Languages.Language_Enum'Image
                (PSC.Languages.Language));
         end if;
      end if;

      --  Initialize the OUT parameters
      Value := Null_Value;
      Addr := Null_Virtual_Address;
      Computation := null;

      if Operand_Sem.all in Literal_Semantic_Info
        and then Expr_Tree in Identifier.Tree
      then
         --  Handle a simple literal
         case Literal_Sem_Ptr (Operand_Sem).Lit_Kind is
            when Null_Literal =>
               --  Create an appropriate null.

               Interpreter.Initialize_Global_Data_Stg_Rgn;
                  --  Create the global data region if necessary

               Value := Null_For_Type_Or_Stg_Rgn
                 (Type_Desc =>
                    Get_Known_Type_Descriptor (Operand_Sem.Resolved_Type),
                  Stg_Rgn => Global_Data_Stg_Rgn);

            when String_Literal =>
               --  Get the value of the string literal
               --  and convert to big representation if appropraite
               Interpreter.Initialize_Global_Data_Stg_Rgn;
                  --  Create the global data region if necessary

               Value := To_Univ_String_Word (Strings.U_String_Index
                           (Literal_Value
                              (Sym_Name (Identifier.Tree (Expr_Tree)))),
                           Null_For_Stg_Rgn (Global_Data_Stg_Rgn));

            when Integer_Literal |
              Real_Literal |
              Char_Literal |
              Enum_Literal =>
               --  Get the value of the non-null, non-string literal
               Value := Literal_Value (Sym_Name (Identifier.Tree (Expr_Tree)));

            when Not_A_Literal =>
               Sem_Error (Operand_Sem.Definition,
                 "Internal: Kind of literal is Not_A_Literal");
               Value := Null_Value;
         end case;
         --  Suppress out-of-line evaluation
         Compute_Out_Of_Line := False;
      elsif Operand_Sem.all in Object_Semantic_Info
        and then Expr_Tree in Obj_Decl.Tree
      then
         --  Check to see if this is a simple constant or reference
         --  to another constant.
         --  Eventually may want to handle simple to/from-univ's that are
         --  mapped to #identity.
         --  Also will want to assign unique-id's to constants and reuse
         --  them appropriately.
         declare
            Obj_Tree : Obj_Decl.Tree renames Obj_Decl.Tree (Expr_Tree);
         begin
            if Obj_Tree.Is_Const and then Not_Null (Obj_Tree.Obj_Value)
              and then
                Tree_Ptr_Of (Resolved_Tree (Obj_Tree.Obj_Value)).all in
                  Identifier.Tree
            then
               --  Recurse with value of constant
               Evaluate_Tree_Or_Create_Routine
                 (Resolved_Tree (Obj_Tree.Obj_Value),
                  Decl_Region, Enclosing_Type, Dest_Name,
                  Value, Addr, Type_Desc, Computation);
               return;
            end if;

            --  Fall through to compute Value
         end;
      end if;

      --  Initialize Type_Desc
      if Operand_Sem.Target_Polymorphic_Type /= null then
         Type_Desc :=
            Get_Known_Type_Descriptor (Operand_Sem.Target_Polymorphic_Type);
      else
         Type_Desc :=
            Get_Known_Type_Descriptor (Operand_Sem.Resolved_Type);
      end if;

      if Compute_Out_Of_Line then
         --  Create the out-of-line routine
         if Debug_Code_Gen then
            Put_Line ("Calling Create_Out_Of_Line_Computation");
         end if;
         Computation := Create_Out_Of_Line_Computation
                   (Expr_To_Eval, Decl_Region, Enclosing_Type, Dest_Name);
         if Debug_Code_Gen then
            Put_Line ("Back from Create_Out_Of_Line_Computation");
         end if;
      else
         --  We were able to evaluate without going out of line.

         --  Create the global data region if necessary
         Interpreter.Initialize_Global_Data_Stg_Rgn;

         --  Allocate address in global data region
         Addr := Allocate_From_Stg_Rgn (Global_Data_Stg_Rgn, 1,
                 Server_Index => Main_Thread_Server_Index);

         --  Store the value there as well
         Store_Word (Addr, Value);
      end if;
   end Evaluate_Tree_Or_Create_Routine;

   procedure Evaluate_Tree
     (OT : Optional_Tree;
      Decl_Region : Region_Ptr := Symbols.Library_Region;
      Enclosing_Type : Interpreter.Type_Descriptor_Ptr := null;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index;
      Value : out Interpreter.Word_Type;
      Addr : out Interpreter.Object_Virtual_Address;
      Type_Desc : out Interpreter.Type_Descriptor_Ptr) is
      --  Evaluate an expression used as an actual obj in a type
      --  instantiation, or equivalent.
      --  If necessary, create a short routine to do this.

      use Interpreter;

      Computation : Routine_Ptr;
   begin
      Evaluate_Tree_Or_Create_Routine
        (OT => OT,
         Decl_Region  => Decl_Region,
         Enclosing_Type => Enclosing_Type,
         Dest_Name => Dest_Name,
         Value => Value,
         Addr => Addr,
         Type_Desc => Type_Desc,
         Computation => Computation);

      if Computation /= null then
         --  Need to go out of line.
         pragma Assert (Addr = Null_Virtual_Address);

         --  Create the global data region if necessary
         Interpreter.Initialize_Global_Data_Stg_Rgn;

         --  Invoke out-of-line routine
         Value := Interpreter.Invoke_Parameterless_Computation
           (Computation, Result_Type => Type_Desc,
            Enclosing_Type => Enclosing_Type);

         --  Allocate address in global data region
         Addr := Allocate_From_Stg_Rgn (Global_Data_Stg_Rgn, 1,
                 Server_Index => Main_Thread_Server_Index);

         --  Store the value there as well
         Store_Word (Addr, Value);
      end if;

   end Evaluate_Tree;

   function Assign_VM_Obj_Id
     (Visitor : Code_Gen_Visitor;
      Needs_Var : Boolean := False;
      Offset : Interpreter.Offset_Within_Area := 0;
      Num_Call_Params : Natural := 0;
      Target_VM_Num : Interpreter.VM_Obj_Unique_Num := 0;
      Only_Param_VM_Num : Interpreter.VM_Obj_Unique_Num := 0)
     return Interpreter.VM_Obj_Id_Type is
   --  Assign a VM_Obj_Id for the given local variable or param or call
   --  Target_VM_Num is used instead of a new VM reg if > 0
   --  Only_Param_VM_Num, if > 0, is used for First_Call_Param_Num
   --  if Num_Call_Params = 1.
      use Interpreter;
      VM_Num_To_Use : VM_Obj_Unique_Num := Target_VM_Num;
   begin

      if Num_Call_Params > 0 then
         --  Assign First_Call_Param_Num for a call
         declare
            First_Call_Param_Num : VM_Obj_Unique_Num;
         begin
            if Only_Param_VM_Num > 0 then
               --  Use specified Reg num for one and only parameter
               pragma Assert (Num_Call_Params = 1);
               First_Call_Param_Num := Only_Param_VM_Num;
            else
               --  Allocate new VM registers for call params
               First_Call_Param_Num :=
                 Visitor.Current_Code.Num_Locals + 1;
               Visitor.Current_Code.Num_Locals :=
                 Visitor.Current_Code.Num_Locals +
                   VM_Obj_Unique_Num (Num_Call_Params);
            end if;
            return VM_Obj_Id_Type'
              (Kind => Local_Kind,
               Is_Var => Needs_Var,
               Level => Visitor.Current_Level,
               Indir => Boolean'Pos (Needs_Var), --  TBD
               Num => Target_VM_Num,
               First_Call_Param_Num => First_Call_Param_Num);
         end;
      elsif Offset > 0 then
         --  A component at given offset
         if VM_Num_To_Use = 0 then
            Visitor.Current_Code.Num_Locals :=
              Visitor.Current_Code.Num_Locals + 1;
               --  Assign VM reg num for local
            VM_Num_To_Use := Visitor.Current_Code.Num_Locals;
         end if;
         return VM_Obj_Id_Type'
           (Kind => Component_Kind,
            Is_Var => Needs_Var,
            Level => Visitor.Current_Level,
            Indir => Boolean'Pos (Needs_Var), --  TBD
            Num => VM_Num_To_Use,
            Offset => Offset);
      else
         --  A local
         if VM_Num_To_Use = 0 then
            Visitor.Current_Code.Num_Locals :=
              Visitor.Current_Code.Num_Locals + 1;
               --  Assign VM reg num for local
            VM_Num_To_Use := Visitor.Current_Code.Num_Locals;
            if Needs_Var then
               --  Remember most recent assigned Var to be sure we declare it.
               Visitor.Current_Code.Most_Recent_Var := VM_Num_To_Use;
            end if;
         end if;
         return VM_Obj_Id_Type'
           (Kind => Local_Kind,
            Is_Var => Needs_Var,
            Level => Visitor.Current_Level,
            Indir => Boolean'Pos (Needs_Var), --  TBD
            Num => VM_Num_To_Use,
            First_Call_Param_Num => 0);
      end if;
   end Assign_VM_Obj_Id;

   procedure Initialize_Lvalue_Location_From_Temp
     (Visitor : in out Code_Gen_Visitor;
      Opnd_Sem : Operand_Sem_Ptr;
      Current_Loc : Interpreter.Object_Locator);
      --  Set loc of result of computation that is to be passed by ref

   procedure Emit_Code_For_Resolved_Tree
     (T : Optional_Tree;
      Visitor : in out Code_Gen_Visitor) is
      --  If T has Sem_Info that has a Resolved_Interp, then
      --  generate code by visiting that instead of T itself.

      --  Also, handle the Target_Polymorphic_Type flag in
      --  Operand_Semantic_Info, if any, unless the Visitor indicates
      --  Gen_Parallel_Invocations_Only or Is_Lvalue_Context.

      --  Also check for the Entry_Temp_Info field being non-null
      --  if in Postcondition_Mode, and return associated temp.
      T_Copy : Optional_Tree := Resolved_Tree (T);
      Tree_Sem : constant Sem_Ptr := Sem_Ptr (Sem_Info (T_Copy));
   begin
      if Tree_Sem = null
        or else Tree_Sem.all not in Operand_Semantic_Info'Class
      then
         --  Generate code the "normal" way
         Visit (T_Copy, Visitor);

      elsif Visitor.Annotation_Mode in Postcondition_Mode
        and then Operand_Sem_Ptr (Tree_Sem).Entry_Temp_Info /= null
      then
         --  In Postcondition mode, we just load the entry temp
         --  rather than recomputing it.
         declare
            use Interpreter;
            Opnd_Sem : constant Operand_Sem_Ptr :=
              Operand_Sem_Ptr (Tree_Sem);
            Info : Object_Location_Info renames Opnd_Sem.Entry_Temp_Info.all;
            Adjusted_Location : constant Interpreter.Object_Locator :=
              Adjust_For_Level_And_Prefix
                (Visitor.Current_Level,
                 Info.Obj_Location,
                 Info.Obj_Level);
         begin
            if Visitor.Gen_Parallel_Invocations_Only then
               --  Nothing to do since this is just an entry-temp load
               null;
            elsif Visitor.Is_Lvalue_Context then
               --  Caller wants address in Lvalue_Location.
               Visitor.Lvalue_Location := Adjusted_Location;
            else
               --  Caller wants "content" in Target_Local_Offset
               Emit_Copy_Obj_Or_Word
                 (Visitor,
                  Destination => (Local_Area,
                                  Visitor.Target_Local_Offset,
                                  Visitor.Target_VM_Info),
                  Dest_Name => Visitor.Dest_Name,
                  Source => Adjusted_Location,
                  Target_Object => Visitor.Target_Object,
                  Opnd_Sem => Opnd_Sem,
                  Source_Pos => Find_Source_Pos (T_Copy));
            end if;
         end;
      elsif not Visitor.Gen_Parallel_Invocations_Only
        and then Operand_Sem_Ptr (Tree_Sem).Target_Polymorphic_Type /= null
      then
         --  We have a target polymorphic type, so we need
         --  to wrap the result in a 2-word object with an
         --  appropriate polymorphic type-descriptor identified
         --  in its header.
         declare
            use Interpreter;

            --  Save state of visitor on entry
            Orig_Target_Offset : constant Offset_Within_Area :=
              Visitor.Target_Local_Offset;
            Orig_Target_Object : constant Object_Locator :=
              Visitor.Target_Object;
            Orig_Lvalue_Context : constant Boolean :=
              Visitor.Is_Lvalue_Context;
            Target_Offset_For_Initial_Value : Offset_Within_Area :=
              Orig_Target_Offset;
            Opnd_Sem : constant Operand_Sem_Ptr := Operand_Sem_Ptr (Tree_Sem);
            pragma Assert (Opnd_Sem.Resolved_Type /= null);
            Enc_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
            Orig_Target_VM_Info : constant VM_Obj_Id_Type :=
              Visitor.Target_VM_Info;
            Poly_Target_VM_Info : VM_Obj_Id_Type := Orig_Target_VM_Info;
            Poly_Obj_Location   : Object_Locator;
            Poly_Target_Needs_Decl : Boolean := False;
         begin
            if Static.Known_To_Be_Small (Opnd_Sem.Resolved_Type)
              and then Orig_Target_Object.Base = Local_Area
              and then Orig_Target_Object.Offset = Orig_Target_Offset
            then
               --  Protect the (large) target object
               --  because initial value is small.

               Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
               Target_Offset_For_Initial_Value := Visitor.Target_Local_Offset;
               Poly_Target_VM_Info :=
                 Assign_VM_Obj_Id (Visitor, Needs_Var => True);
               Poly_Target_Needs_Decl := True;

            elsif not Poly_Target_VM_Info.Is_Var then
               --  We need a VM var for Create_Polymorphic operation.
               --  TBD: Unless we change it to be a function...
               Poly_Target_VM_Info :=
                 Assign_VM_Obj_Id (Visitor, Needs_Var => True);
               Poly_Target_Needs_Decl := True;
            end if;

            Poly_Obj_Location :=
               (Local_Area, Target_Offset_For_Initial_Value,
                Poly_Target_VM_Info);

            if Poly_Target_Needs_Decl then
               --  Need to declare the poly target
               Emit
                 (Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Find_Source_Pos (T_Copy),
                   Destination => Poly_Obj_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Is_By_Ref => False,
                   Is_Var => False,
                   Declare_Type_Info =>
                      Run_Time_Type_Info
                        (Opnd_Sem.Resolved_Type,
                         Referring_Module => Enc_Module)));
            end if;

            --  Set flag indicating we are about to emit a Create_Poly_Obj op
            Visitor.Create_Polymorphic_Obj := True;

            Visitor.Target_VM_Info := Poly_Target_VM_Info;
            Visitor.Is_Lvalue_Context := False;

            Visit (T_Copy, Visitor);  --  Actually generate the code

            Visitor.Is_Lvalue_Context := Orig_Lvalue_Context;

            if Visitor.Create_Polymorphic_Obj then
               --  Flag still set, so we should go ahead and create
               --  the polymorphic object (after resetting the flag).
               Visitor.Create_Polymorphic_Obj := False;

               Emit
                 (Visitor,
                  (Op => Create_Polymorphic_Obj_Op,
                   Source_Pos => Find_Source_Pos (T_Copy),
                   Destination => Poly_Obj_Location,
                   Dest_Name => Visitor.Dest_Name,
                   Source => Orig_Target_Object,
                   Might_Be_Null => True,  --  TBD
                   Type_Info =>
                      Run_Time_Type_Info
                        (Opnd_Sem.Resolved_Type,
                         Referring_Module => Enc_Module,
                         Formal_Type => Opnd_Sem.Target_Polymorphic_Type,
                         Source_Pos => Find_Source_Pos (T),
                         Is_Polymorphic_Type_Id => True)));
            end if;

            if Visitor.Is_Lvalue_Context then
               --  We need to copy into a preallocated (finalizable) temp
               --  and set Visitor.Lvalue_Location from there.
               Initialize_Lvalue_Location_From_Temp
                 (Visitor, Opnd_Sem, Poly_Obj_Location);

            elsif Orig_Target_Offset /= Target_Offset_For_Initial_Value
              or else Orig_Target_VM_Info /= Poly_Target_VM_Info
            then
               --  Need to move (polymorphic) result to original target offset
               Emit
                 (Visitor,
                  (Op => Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (T_Copy),
                   Destination => (Local_Area, Orig_Target_Offset,
                                   Orig_Target_VM_Info),
                   Dest_Name => Visitor.Dest_Name,
                   Source => Poly_Obj_Location,
                   Might_Be_Null => True));

               if Orig_Target_Offset /= Target_Offset_For_Initial_Value then
                  --  Cut back target_local_offset
                  Check_And_Set_Local_Offset
                    (Visitor, Target_Offset_For_Initial_Value);
               end if;
            end if;
         end;

      else

         --  Just generate the code the normal way
         Visit (T_Copy, Visitor);
      end if;
      Check_High_Water (Visitor);
   end Emit_Code_For_Resolved_Tree;

   procedure Emit_Code_And_Finalize_Resolved_Tree
     (T : Optional_Tree;
      Visitor : in out Code_Gen_Visitor) is
      --  Call Emit_Code_For_Resolved_Tree and then
      --  call Finalize_Result_And_Ref_Operands.
   begin
      Emit_Code_For_Resolved_Tree (T, Visitor);
      Finalize_Result_And_Ref_Operands (Visitor, T);
   end Emit_Code_And_Finalize_Resolved_Tree;

   function Name_For_Object_Locator
     (Locator : Interpreter.Object_Locator;
      Enclosing_Type : Interpreter.Object_Locator :=
        Interpreter.Null_Object_Locator)
      return String
   is
      --  Return a string representing the name associated with the
      --  given object locator.  Return "" if none available.
      --  NOTE: Currently only supports Const_Area, Zero_Base, and Type_Area.
      --       Type_Area requires Enclosing_Type to be provided,
      --       which must have a Zero_Base.
      use Interpreter;
      use type CTK_Info_Index;
      use type Strings.U_String;
   begin
      case Locator.Base is
         when Const_Area =>
            if Num_Elements (Interpreter.Compile_Time_Known_Consts) > 0
              and then Nth_Element
                (Interpreter.Compile_Time_Known_Consts,
                   CTK_Info_Index (Locator.Offset)).Info.Data.Addr /=
                       Null_Virtual_Address
              and then Nth_Element
                (Interpreter.Compile_Time_Known_Consts,
                   CTK_Info_Index (Locator.Offset)).Info.Name /=
                       Strings.Null_U_String
            then
               --  Name already computed
               return Strings.To_String
                         (Nth_Element
                            (Interpreter.Compile_Time_Known_Consts,
                             CTK_Info_Index (Locator.Offset)).Info.Name);
            else
               --  Determine appropriate name from const table of sem-ptrs
               declare
                  Const_Sem : constant Sem_Ptr :=
                    Nth_Element
                       (Compile_Time_Known_Const_Table,
                        Obj_Sem_Info_Index (Locator.Offset));
               begin
                  if Const_Sem.all in Sym_Reference_Info'Class then
                     --  Give full name if a qualified name
                     return Subtree_Image (Const_Sem.Definition);
                  elsif Const_Sem.all in Object_Semantic_Info'Class
                    and then Const_Sem.Associated_Symbol /= null
                  then
                     --  Give declared name
                     return Sym_Name (Const_Sem.Associated_Symbol);
                  else
                     --  Give subtree that defined the expression
                     return Subtree_Image (Const_Sem.Definition);
                  end if;
               end;
            end if;
         when Zero_Base =>
            --  TBD
            return "";
         when Type_Area =>
            --  TBD
            return "";
         when others =>
            return "";
      end case;
   end Name_For_Object_Locator;

   procedure Finish_Cur_Inst_Param_Info is
   --  Fill in Cur_Inst_Param_Info for all type descriptors

      use Interpreter;

      procedure Finish_Type_Cur_Inst_Param_Info
        (Type_Desc : Type_Descriptor_Ptr);
      --  Fill in cur-inst param info on Type_Desc

      procedure Finish_Type_Cur_Inst_Param_Info
        (Type_Desc : Type_Descriptor_Ptr) is
      --  Fill in cur-inst param info on Type_Desc
         use Interpreter.Cur_Inst_Params;
      begin
         for I in 1 .. Type_Desc.Num_Operations loop
            declare
               Info    : Routine_Info renames Type_Desc.Operations (I);
               Routine : constant Routine_Ptr := Nth_Routine (Info.Index);
               pragma Assert (Routine.Parameters /= null);
            begin
               --  Build up cur-inst param info
               for J in Routine.Parameters'Range loop
                  if Routine.Parameters (J).Is_Of_Current_Inst_Type then
                     Add_Cur_Inst_Param (Info.Cur_Inst_Param_Info,
                       Param_Offset => Offset_Within_Area (J - 1),
                       Is_By_Ref    => Routine.Parameters (J).Is_Passed_By_Ref,
                       Is_Var       => Routine.Parameters (J).Is_Var,
                       Is_Output    =>
                         Routine.Parameters (J).Is_Operation_Output);
                  end if;
               end loop;
            end;
         end loop;
      end Finish_Type_Cur_Inst_Param_Info;

      Next_Type_Desc : Type_Descriptor_Ptr := First_Needing_Cur_Inst_Info;

   begin  --  Finish_Cur_Inst_Param_Info

      --  Finish cur-inst param info for type descriptors that are
      --  otherwise complete.
      while Next_Type_Desc /= null loop
         declare
            This_Type_Desc : constant Type_Descriptor_Ptr := Next_Type_Desc;
         begin
            Next_Type_Desc := This_Type_Desc.Next_Unfinished;
            Finish_Type_Cur_Inst_Param_Info (This_Type_Desc);
            This_Type_Desc.Next_Unfinished := null;
         end;
      end loop;

      --  Null out this list
      First_Needing_Cur_Inst_Info := null;
      Last_Needing_Cur_Inst_Info := null;

      --  Finish cur-inst param info for type descriptors that are
      --  waiting to evaluate nested objects, etc.
      Next_Type_Desc := First_Unfinished_Type;
      while Next_Type_Desc /= null loop
         Finish_Type_Cur_Inst_Param_Info (Next_Type_Desc);
         Next_Type_Desc := Next_Type_Desc.Next_Unfinished;
      end loop;

   end Finish_Cur_Inst_Param_Info;

   procedure Evaluate_Global_Constants is
      --  Go through compile_time_known constants and evaluate them,
      --  or create a parameterless routine which can do it later.
      use Interpreter;
      Num_Global_Constants : constant Natural :=
        Natural (Num_Elements (Compile_Time_Known_Const_Table));
   begin
      if Num_Global_Constants = 0 then
         --  No global constants
         return;
      end if;

      --  Now evaluate each one and save the result,
      --  or create a parameterless routine to do it.
      for I in 1 .. Num_Global_Constants loop
         declare
            Const_Sem : constant Sem_Ptr :=
              Nth_Element
                 (Compile_Time_Known_Const_Table,
                  Obj_Sem_Info_Index (I));
            New_CTK_Info : constant Computable_Const_Info_Ptr :=
              new Computable_Const_Info;
            New_CTK_Index : CTK_Info_Index;
            Out_Of_Line_Routine : Routine_Ptr;
         begin
            Add_Element (Compile_Time_Known_Consts, New_CTK_Info,
              New_CTK_Index);
            pragma Assert (Natural (New_CTK_Index) = I);

            if Const_Sem.all in Sym_Reference_Info'Class
              and then
               Sym_Ref_Ptr (Const_Sem).Target_Polymorphic_Type = null
            then
               --  These will be handled later
               null;
            elsif Const_Sem.all in Object_Semantic_Info'Class
              and then Tree_Ptr_Of (Const_Sem.Definition).all in
                         Obj_Decl.Tree'Class
            then
               declare
                  Glob_Const : constant Object_Sem_Ptr :=
                    Object_Sem_Ptr (Const_Sem);
                  Initial_Value : constant Optional_Tree :=
                    Obj_Decl.Tree (Tree_Ptr_Of (Glob_Const.Definition).all).
                      Obj_Value;
                  Const_Type_Desc : Type_Descriptor_Ptr;
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       (" Evaluating value of " &
                        Subtree_Image (Resolved_Tree (Initial_Value)));
                  end if;

                  New_CTK_Info.Info.Name :=
                    Strings.String_Lookup
                      (Sym_Full_Name (Glob_Const.Associated_Symbol));

                  Evaluate_Tree_Or_Create_Routine
                    (Resolved_Tree (Initial_Value),
                     Decl_Region =>
                       Glob_Const.Associated_Symbol.Enclosing_Region,
                     Dest_Name => Strings.Index (New_CTK_Info.Info.Name),
                     Value => New_CTK_Info.Info.Data.Value,
                     Addr => New_CTK_Info.Info.Data.Addr,
                     Type_Desc => Const_Type_Desc,
                     Computation => Out_Of_Line_Routine);

                  --  Store the un-wrapped type descriptor so wrappers
                  --  of univ-enum and univ-string are handled properly.
                  New_CTK_Info.Info.Data.Type_Desc :=
                    Interpreter.Type_Descriptor_Ops.Unwrapped_Type_Desc
                      (Const_Type_Desc);

               end;
            else
               declare
                  Initial_Value : constant Optional_Tree :=
                    Const_Sem.Definition;
                  Const_Type_Desc : Type_Descriptor_Ptr;
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       (" Evaluating value of anon-const " &
                        Subtree_Image (Resolved_Tree (Initial_Value)));
                  end if;

                  --  Note that Data.Addr is Null_Virtual_Address until it
                  --  has been computed, so we check for that before using
                  --  a pre-computed constant, once Compile_Time_Known_Consts
                  --  is non-null.

                  Evaluate_Tree_Or_Create_Routine
                    (Resolved_Tree (Initial_Value),
                     Decl_Region => Library_Region,
                     Dest_Name => Strings.Index (Strings.String_Lookup
                       (Subtree_Image (Resolved_Tree (Initial_Value)))),
                     Value => New_CTK_Info.Info.Data.Value,
                     Addr => New_CTK_Info.Info.Data.Addr,
                     Type_Desc => Const_Type_Desc,
                     Computation => Out_Of_Line_Routine);

                  --  Store the un-wrapped type descriptor so wrappers
                  --  of univ-enum and univ-string are handled properly.
                  New_CTK_Info.Info.Data.Type_Desc :=
                    Interpreter.Type_Descriptor_Ops.Unwrapped_Type_Desc
                      (Const_Type_Desc);

                  New_CTK_Info.Info.Name := Strings.Null_U_String;

               end;
            end if;
            exit when Sem_Error_Count > 0;

            if Out_Of_Line_Routine /= null then
               --  Cannot compute yet, save routine and postpone
               pragma Assert (New_CTK_Info.Info.Data.Addr =
                 Null_Virtual_Address);
               New_CTK_Info.Computation := Out_Of_Line_Routine.Index;
            end if;

         end;
      end loop;

   end Evaluate_Global_Constants;

   procedure Finish_Global_Constants is
      --  Go through compile_time_known constants again, completing
      --  computations and making copies, as needed
      use Interpreter;
      Num_Global_Constants : constant Natural :=
        Natural (Num_Elements (Compile_Time_Known_Const_Table));
   begin
      if Num_Global_Constants = 0 then
         --  No global constants
         return;
      end if;

      --  Scan for those that need to be computed
      for I in 1 .. Num_Global_Constants loop
         declare
            Const_Sem : constant Sem_Ptr :=
              Nth_Element
                 (Compile_Time_Known_Const_Table,
                  Obj_Sem_Info_Index (I));
         begin
            if Const_Sem.all not in Sym_Reference_Info'Class
              or else
               Sym_Ref_Ptr (Const_Sem).Target_Polymorphic_Type /= null
            then
               --  Compute constants that are needed and not yet computed.
               --  This includes cases where the original constant is
               --  monomorphic, but the new constant is polymorphic.
               declare
                  CTK_Info : constant Computable_Const_Info_Ptr :=
                    Nth_Element (Compile_Time_Known_Consts,
                                 CTK_Info_Index (I));
               begin
                  if CTK_Info.Computation /= 0
                    and then CTK_Info.Info.Data.Addr = Null_Virtual_Address
                    and then (Const_Sem.all not in
                                Computation_Semantic_Info'Class
                                  or else
                              Computation_Sem_Ptr (Const_Sem).Needs_Anon_Const)
                  then
                     if Debug_Code_Gen then
                        Put_Line
                          (" Invoking deferred computation of " &
                           Subtree_Image (Const_Sem.Definition));
                     end if;
                     --  Invoke out-of-line routine
                     CTK_Info.Info.Data.Value :=
                       Interpreter.Invoke_Parameterless_Computation
                         (Nth_Routine (CTK_Info.Computation),
                          Result_Type => CTK_Info.Info.Data.Type_Desc);

                     --  Allocate address in global data region
                     CTK_Info.Info.Data.Addr :=
                       Allocate_From_Stg_Rgn
                         (Global_Data_Stg_Rgn, 1,
                          Server_Index => Main_Thread_Server_Index);

                     --  Store the value there as well
                     Store_Word (CTK_Info.Info.Data.Addr,
                                 CTK_Info.Info.Data.Value);
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Scan for those that are copies of those in type descriptors
      for I in 1 .. Num_Global_Constants loop
         declare
            Const_Sem : constant Sem_Ptr :=
              Nth_Element
                 (Compile_Time_Known_Const_Table,
                  Obj_Sem_Info_Index (I));
         begin
            if Const_Sem.all in Sym_Reference_Info'Class
              and then
               Sym_Ref_Ptr (Const_Sem).Target_Polymorphic_Type = null
            then
               --  This is a case where we want a copy of the
               --  original constant as is (i.e. *without* having to first
               --  convert it to a polymorphic object).
               declare
                  Const_Ref : constant Sym_Ref_Ptr := Sym_Ref_Ptr (Const_Sem);
                  From_Type_Desc : constant Type_Descriptor_Ptr :=
                    Get_Known_Type_Descriptor
                       (Type_Sem_Ptr (Const_Ref.Prefix_Type_Region));
                  From_Locator : constant Object_Locator :=
                    Object_Sem_Ptr (Const_Ref.Underlying_Sem_Info).Info.
                      Obj_Location;
                  CTK_Info : constant Computable_Const_Info_Ptr :=
                    Nth_Element (Compile_Time_Known_Consts,
                                 CTK_Info_Index (I));
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       (" Copying value of " &
                        Subtree_Image (Const_Ref.Definition));
                  end if;
                  --  Copy info
                  --  TBD:  What about multiple levels of indirection?
                  --        We may need to keep iterating...
                  CTK_Info.Info.Data :=
                     Nth_Type_Area_Element
                       (From_Type_Desc,
                        From_Locator.Offset);

                  --  Provide a more explicit name
                  --  TBD: Use the Sym_Full_Name instead?
                  --       But how does that incorporate the type instance?
                  CTK_Info.Info.Name :=
                     Strings.String_Lookup
                       (Subtree_Image (Const_Ref.Definition));
               end;
            end if;
         end;
      end loop;

      --  Check all compile-time-known annotations and complain if not true.
      for Anx in 1 .. Num_Elements (CTK_Annotation_Vector) loop
         declare
            Annot_Info : constant CTK_Annotation_Info :=
              Nth_Element (CTK_Annotation_Vector, Anx);
            CTK_Info : constant Computable_Const_Info_Ptr :=
              Nth_Element (Compile_Time_Known_Consts,
                CTK_Info_Index (Annot_Info.CTK_Index));
            CTK_Value : constant Word_Type :=
              CTK_Info.Info.Data.Value;
         begin
            if CTK_Value = 0 then
               --  Compile-time-known assertion will fail
               Sem_Warning
                 ("Assertion will fail:",
                  Find_Source_Pos (Annot_Info.CTK_Annotation));
               Messages.Put_Message
                 ("  " &
                  Subtree_Image (Annot_Info.CTK_Annotation,
                                 Use_Short_Form => True),
                  Find_Source_Pos (Annot_Info.CTK_Annotation),
                  Message_Kind => "Info");

            elsif Debug_Code_Gen then
               --  Assertion will pass -- produce debugging message

               Messages.Put_Message
                 ("CTK assertion will succeed: " &
                  Subtree_Image (Annot_Info.CTK_Annotation,
                                 Use_Short_Form => True),
                  Find_Source_Pos (Annot_Info.CTK_Annotation),
                  Message_Kind => "Info");
            end if;
         end;
      end loop;

   end Finish_Global_Constants;

   function Component_Offset
     (Obj_Type : Type_Sem_Ptr;
      Comp_Decl : Optional_Tree;
      Usable_In_Aggregate : Boolean := True)
      return Interpreter.Offset_Within_Area'Base
   is
      --  Return offset of component within object if is a component of
      --  the module else return Offset_Within_Area'Last if is an operation
      --  or some other non-component.
      --  Return 0 if type is a wrapper and this is the only component.
      --  If Usable_In_Aggregate is False, then it will return component
      --  offset but it might not match Component_Extension_Level of Mod_Sem.
      Mod_Sem : constant Module_Sem_Ptr := Obj_Type.Associated_Module;
      Comp_Index : constant Natural :=
        Static.Component_Index (Mod_Sem, Comp_Decl, Usable_In_Aggregate);
      use Interpreter;
   begin
      if Comp_Index = 0 then
         --  Not a component
         if Debug_Code_Gen then
            Put_Line
              (" Component " &
               Subtree_Image (Comp_Decl) &
               " is not found in " &
               Sym_Name (Mod_Sem.Associated_Symbol));
         end if;
         return Offset_Within_Area'Last;
      elsif Static.Type_Is_Wrapper (Obj_Type) then
         --  Is only component of wrapper
         pragma Assert (Comp_Index = 1);
         if Debug_Code_Gen then
            Put_Line
              (" Component " &
               Subtree_Image (Comp_Decl) &
               " is singleton component of wrapper " &
               Type_Image (Obj_Type));
         end if;
         return 0;
      else
         --  Compute offset
         return Large_Obj_Header_Size + Offset_Within_Area (Comp_Index - 1);
      end if;
   end Component_Offset;

   function Nth_Interface_Operation
     (Mod_Sem : Module_Sem_Ptr;
      N : Positive)
      return Operation_Sem_Ptr
   is
      --  Return the "Nth" interface operation of the given module.
      --  Return null if not that many operations.
      Num_Ops : constant Natural :=
        Natural (Mod_Sem.Num_Interface_Operations);
      pragma Assert (N <= Num_Ops);
      Mod_Region : constant Symbols.Region_Ptr := Mod_Sem.Nested_Region;
   begin
      --  Iterate over all of the symbols of the module
      for I in 1 .. Symbols.Num_Symbols_In_Region (Mod_Region) loop
         declare
            Sym_In_Module : constant Symbols.Sym_Ptr :=
              Symbols.Nth_Symbol_In_Region (Mod_Region, I);
         begin
            if Sym_In_Module.Sem_Info /= null
              and then Sym_In_Module.Sem_Info.all in Operation_Semantic_Info'
                 Class
            then
               declare
                  Op_Sem : constant Operation_Sem_Ptr :=
                    Operation_Sem_Ptr (Sym_In_Module.Sem_Info);
               begin
                  if Natural (Op_Sem.Index) = N
                    and then Op_Sem.Context in Any_Interface_Item_Contexts
                    and then Op_Sem.Overridden_By = null
                  then
                     --  Found it
                     return Op_Sem;
                  end if;
               end;
            end if;
            --  Keep looking
         end;
      end loop;

      --  Oops, N interface operations not found
      return null;
   end Nth_Interface_Operation;

   function Nth_Interface_Operation_Name
     (Mod_Sem : Module_Sem_Ptr;
      N : Positive)
      return String
   is
      --  Return the name of the "Nth" interface operation of the given module.
      --  Return "[No op#XX in YY]" if no such operation.
      Op_Sem : constant Operation_Sem_Ptr :=
        Nth_Interface_Operation (Mod_Sem, N);
   begin
      if Op_Sem /= null then
         return Sym_Name (Op_Sem.Associated_Symbol);
      else
         return "[No op #" &
                Positive'Image (N) &
                " in " &
                Sym_Name (Mod_Sem.Associated_Symbol) &
                ']';
      end if;
   end Nth_Interface_Operation_Name;

   function Body_Routine (Op_Sem : Operation_Sem_Ptr)
     return Interpreter.Routine_RW_Ptr is
   --  Return Routine for Op_Sem body's Operation_Sem_Ptr
   begin
      if Op_Sem /= null
        and then Op_Sem.Body_Region /= null
        and then Op_Sem.Body_Region.Associated_Symbol /= null
      then
         return Operation_Sem_Ptr
           (Op_Sem.Body_Region.Associated_Symbol.Sem_Info).Routine;
      else
         return null;
      end if;
   end Body_Routine;

   function Find_Operation_Routine (Op_Sem : Operation_Sem_Ptr)
     return Interpreter.Routine_RW_Ptr is
   --  Return Routine_Ptr for routine associated with given operation,
   --  after following Originating_Operation and Equiv_To links
      use Interpreter;

      Op_To_Use : Operation_Sem_Ptr := Op_Sem.Originating_Operation;
      Routine_To_Use : Interpreter.Routine_RW_Ptr := null;
   begin
      --  Get "Routine" field for inherited and renamed operations.
      if Op_Sem.Context in Ancestor_Item_Contexts
        or else Op_Sem.Equiv_To /= null
      then
         if Op_To_Use /= null
           and then Op_To_Use.Equiv_To /= null
         then
            --  Follow the "Equiv_To" link
            Op_To_Use := Op_To_Use.Equiv_To;
         end if;

         if Op_To_Use = null then
            if Debug_Code_Gen then
               Put_Line (" Inherited/renamed op has null " &
                 "Originating_Operation: ");
               Dump_Subtree (Op_Sem.Definition);
            end if;
         elsif Op_To_Use.Body_Region /= null then
            --  Get routine info from body of original op.
            Routine_To_Use := Operation_Sem_Ptr
              (Op_To_Use.Body_Region.Associated_Symbol.Sem_Info).Routine;

            if Debug_Code_Gen and then Routine_To_Use = null then
               Put_Line
                 (" Inherited/renamed op has null routine: ");
               Dump_Subtree (Op_Sem.Definition);
            end if;
         elsif Op_To_Use.Routine /= null then
            --  Original must have been imported
            Routine_To_Use := Op_To_Use.Routine;
         else
            if Debug_Code_Gen then
               Put_Line
                 (" Inherited/equiv op has null body: ");
               Dump_Subtree (Op_Sem.Definition);
            end if;
         end if;

         if Op_Sem.Context in Ancestor_Item_Contexts then
            --  Remember routine to use for later (in Routine_Locator)
            Op_Sem.Routine := Routine_To_Use;
         end if;

      elsif Op_To_Use /= null then
         --  Not inherited/renamed
         Routine_To_Use := Op_To_Use.Routine;
      end if;

      --  Return routine found by following links
      return Routine_To_Use;

   end Find_Operation_Routine;

   function Find_Operation_Routine_Index (Op_Sem : Operation_Sem_Ptr)
     return Interpreter.Routine_Index is
   --  Return Routine_Index for routine associated with given operation,
   --  after following Originating_Operation and Equiv_To links
      Op_Routine : Interpreter.Routine_RW_Ptr :=
        Find_Operation_Routine (Op_Sem);
      use type Interpreter.Routine_RW_Ptr;
   begin
      if Op_Routine = null and then Op_Sem.Body_Region /= null then
         --  Make one last try to get non-null Routine via body region.
         Op_Routine := Operation_Sem_Ptr
              (Op_Sem.Body_Region.Associated_Symbol.Sem_Info).Routine;
      end if;
      if Op_Routine = null and then Op_Sem.Originating_Operation /= null
        and then Op_Sem.Originating_Operation.Body_Region /= null
      then
         --  Try using Originating_Operation to get to body region
         Op_Routine := Operation_Sem_Ptr (Op_Sem.Originating_Operation.
           Body_Region.Associated_Symbol.Sem_Info).Routine;
      end if;

      if Op_Routine /= null then
         return Op_Routine.Index;
      else
         return 0;
      end if;
   end Find_Operation_Routine_Index;

   procedure Build_Operation_Routine_Info
     (Info : in out Interpreter.Routine_Info;
      Assoc_Module : Module_Sem_Ptr;
      Now_Building : Interpreter.Type_Descriptor_Ptr;
      Local_Op_Sem : Operation_Sem_Ptr;
      Obj_Type : Type_Sem_Ptr;
      Op_Index : Interpreter.Operation_Index) is
      --  Fill in Routine_Info given Operation sem info
      --  for non-overridden operation with non-zero operation index..
      use Interpreter;
      use Interpreter.Cur_Inst_Params;

      pragma Assert (Local_Op_Sem.Overridden_By = null);
      pragma Assert (Op_Index > 0);

      Orig_Op : Operation_Sem_Ptr := Local_Op_Sem.Originating_Operation;
      Type_To_Use : Type_Sem_Ptr := Obj_Type;
      Routine_To_Use : constant Interpreter.Routine_RW_Ptr :=
        Find_Operation_Routine (Local_Op_Sem);
   begin

      if Orig_Op = null then
         Orig_Op := Local_Op_Sem;
      end if;

      if Routine_To_Use /= null
        and then Routine_To_Use.Index /= 0
      then
         --  Found the code for an operation
         if Debug_Code_Gen then
            Put_Line (" Build_Operation_Routine_Info for op #" &
              Operation_Index'Image (Op_Index) & " of type " &
              Type_Image (Obj_Type) & ": " &
              Subtree_Image (Local_Op_Sem.Definition,
                Use_Short_Form => True));
         end if;

         if Info.Index /= 0 then
            --  Already filled in
            Sem_Error
              (Local_Op_Sem.Definition,
               "Duplicate definition of " &
               Sym_Name (Local_Op_Sem.Associated_Symbol) &
               " (operation #" &
               Operation_Index'Image (Local_Op_Sem.Index) &
               ')');
         else
            Info.Index := Routine_To_Use.Index;
            --  Fill in "Op_Index field
            Info.Op_Index := Op_Index;

            if Local_Op_Sem.Context in Ancestor_Item_Contexts then
               --  We have an inherited operation
               --  Set the Type_Desc and "Action" fields
               --  appropriately.
               declare
                  Anc_Mod : constant Module_Sem_Ptr :=
                    Local_Op_Sem.Originating_Module;

               begin
                  --  Get corresponding ancestor type
                  Type_To_Use :=
                    Static.Corresponding_Ancestor_Type
                       (Obj_Type,
                        Anc_Mod);

                  --  Determine special action needed, if any
                  if Anc_Mod.Component_Extension_Level <
                     Assoc_Module.Component_Extension_Level
                    or else
                        (Static.Module_Is_Wrapper (Anc_Mod)
                      and then
                        not Static.Module_Is_Wrapper
                          (Assoc_Module))
                  then
                     --  Set the "action" field if
                     --  appropriate
                     Info.Action := Component_Extension_Action;
                  end if;

               end;
            end if;

            if Orig_Op.Equiv_From_Type /= null then
               --  Operation was a rename of something
               --  else.
               Type_To_Use := Static.Substitute_Actuals
                 (Orig_Op.Equiv_From_Type,
                  U_Base_Type_Region (Type_To_Use));
            end if;

            if Type_To_Use.Root_Type /= Obj_Type.Root_Type then
               --  Fill in Type_Desc to designate ancestor
               Info.Type_Desc :=
                  Known_Type_Desc
                    (Run_Time_Type_Info (Type_To_Use),
                     Now_Building => Now_Building,
                     Src_Pos => Find_Source_Pos (Obj_Type.Definition));

               if Debug_Code_Gen then
                  Put_Line
                    (" Using inherited/renamed routine " &
                     Subtree_Image
                        (Local_Op_Sem.Definition) &
                     " (op #" &
                     Operation_Index'Image
                        (Local_Op_Sem.Index) &
                     ", routine #" &
                     Routine_Index'Image
                        (Routine_To_Use.Index) &
                     ") from " &
                     Type_Image (Type_To_Use) &
                     ", Action = " &
                     Wrapper_Action_Enum'Image (Info.Action));
               end if;
            else
               Info.Type_Desc := Now_Building;
            end if;

            if Orig_Op.Implicit_Enclosing_Module /= null then
               --  Indicate that only caller knows the
               --  correct type desc to use
               Info.Use_Static_Link_For_Type := True;
            end if;
         end if;
      end if;
   end Build_Operation_Routine_Info;

   function Build_Polymorphic_Type_Desc
     (Obj_Type : Type_Sem_Ptr;
      Formal_Type : Type_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position;
      Underlying_Type_Desc : Interpreter.Type_Descriptor_Ptr)
      return Interpreter.Type_Descriptor_Ptr;
   --  See below for comments

   procedure Build_Type_Descriptor (Obj_Type : Type_Sem_Ptr);
   --  Create a type descriptor for Obj_Type.
   --  Put "location" of type desc in Obj_Type.Type_Descriptor_Location.

   procedure Build_Or_Find_Type_Descriptor (Obj_Type : Type_Sem_Ptr) is
   --  First look up type descriptor by name.  If not found,
   --  then build a new one.
      use Interpreter;
   begin
      if Is_Null_Obj_Locator (Obj_Type.Type_Descriptor_Location)
        or else Obj_Type.Type_Descriptor_Location.Base /= Zero_Base
      then
         declare
            Type_Name : constant Strings.U_String :=
              Strings.String_Lookup (Canonical_Type_Name (Obj_Type));
            Existing_Type_Desc : constant Type_Descriptor_Ptr :=
              Type_Descriptor_Ops.Get_Type_Desc_By_Name (Type_Name);
         begin
            if Existing_Type_Desc /= null then
               --  Just copy over the pre-assigned locator
               --  and fill in the type-sem.
               Obj_Type.Type_Descriptor_Location :=
                 Existing_Type_Desc.Location;
               Existing_Type_Desc.Type_Sem := Root_Sem_Ptr (Obj_Type);
            else
               --  Build a new one.
               Obj_Type.Type_Descriptor_Location := Null_Object_Locator;
               Build_Type_Descriptor (Obj_Type);
            end if;
         end;
      end if;
   end Build_Or_Find_Type_Descriptor;

   procedure Fill_In_Nested_Type_Info
     (Obj_Type     : Type_Sem_Ptr;
      Type_Desc    : Interpreter.Type_Descriptor_Ptr) is
      --  Fill in information on nested types
      use Interpreter;
      Assoc_Module : constant Module_Sem_Ptr := Obj_Type.Associated_Module;
      Num_Already_Filled_In : constant Natural := Type_Desc.Num_Nested_Types;
      Existing_Info : constant Type_Desc_Array_Ptr := Type_Desc.Nested_Types;
   begin
      Type_Desc.Num_Nested_Types :=
        Natural (Num_Elements (Assoc_Module.Nested_Types));

      if Num_Already_Filled_In < Type_Desc.Num_Nested_Types then
         --  We need to fill in more
         Type_Desc.Nested_Types :=
           new Type_Desc_Array (1 .. Type_Desc.Num_Nested_Types);

         if Num_Already_Filled_In > 0 then
            --  Already have filled in some; copy those over.
            if Debug_Code_Gen then
               Put_Line (" Finishing info on nested types:");
            end if;
            Type_Desc.Nested_Types (Existing_Info'Range) := Existing_Info.all;
         else
            --  Starting from scratch
            if Debug_Code_Gen then
               Put_Line (" Building info on nested types:");
            end if;
         end if;

         --  (Re-)Copy inherited ones from parent module
         for I in 1 .. Assoc_Module.Num_Inherited_Nested_Types loop
            Type_Desc.Nested_Types (I) :=
              Type_Desc.Parent_Type.Nested_Types (I);
         end loop;

         --  Now find/build type-descs for the new ones
         for I in
              Natural'Max (Num_Already_Filled_In + 1,
                           Assoc_Module.Num_Inherited_Nested_Types + 1) ..
              Type_Desc.Num_Nested_Types
         loop
            declare
               Nested_Type : constant Type_Sem_Ptr :=
                 Nth_Element
                    (Assoc_Module.Nested_Types,
                     Type_Sem_Vectors.Elem_Index (I));
               Resolved_Nested_Type : constant Type_Sem_Ptr :=
                 Static.Substitute_Actuals
                    (Nested_Type,
                     U_Base_Type_Region (Obj_Type));
               Resolved_Type_Desc : Type_Descriptor_Ptr :=
                 Known_Type_Desc
                    (Run_Time_Type_Info (Resolved_Nested_Type),
                     Now_Building => Type_Desc,
                     Src_Pos => Find_Source_Pos (Obj_Type.Definition));
               Op_Map_Target_Ref : constant Type_Sem_Target_Tables.Element_Ref
                 := Find_Element (Assoc_Module.Nested_Type_Op_Map_Target,
                                  Type_Sem_Vectors.Elem_Index (I));
               use type Type_Sem_Target_Tables.Element_Ref;
            begin
               if Op_Map_Target_Ref = null then
                  --  Check for special case of formal type with nested index
                  if Nested_Type.Is_Formal_Type then
                     --  This is a case where we want to find the corresponding
                     --  "progenitor" for use when fetching value params or
                     --  nested objects.
                     if Debug_Code_Gen then
                        Put_Line ("Run_Time_Type_Info found Nested type that" &
                          " is a formal type: " & Type_Image (Nested_Type) &
                          " resolves to " & Type_Image (Resolved_Nested_Type));
                        pragma Assert (Static.Type_Has_Val_Params_Or_Consts
                                       (Nested_Type));
                     end if;
                     declare
                        Progenitor : constant Type_Sem_Ptr :=
                          Static.Corresponding_Progenitor_Type
                            (Resolved_Nested_Type,
                             Nested_Type.Associated_Module);
                     begin
                        if Progenitor /= null
                          and then Progenitor.Root_Type /=
                            Resolved_Nested_Type.Root_Type
                        then
                           --  Get type descriptor for corresponding
                           --  progenitor.
                           Resolved_Type_Desc :=
                             Known_Type_Desc
                               (Run_Time_Type_Info (Progenitor),
                                Now_Building => Type_Desc,
                                Src_Pos =>
                                  Find_Source_Pos (Obj_Type.Definition));
                        end if;
                     end;
                  end if;
                  Type_Desc.Nested_Types (I) := Resolved_Type_Desc;
               else
                  --  Get an "op-map" type descriptor
                  Type_Desc.Nested_Types (I) :=
                    Known_Type_Desc
                      (Run_Time_Type_Info
                         (Resolved_Nested_Type,
                          Formal_Type => Op_Map_Target_Ref.all),
                       Now_Building => Type_Desc,
                       Src_Pos => Find_Source_Pos (Obj_Type.Definition));
                  --  These generally need a polymorphic type as well.
                  if Op_Map_Target_Ref.all.Associated_Module.
                    Needs_Polymorphic_Type_Desc
                  then
                     --  A polymorphic type of the target type
                     --  was created, so create a polymorphic type
                     --  desc for this op-map.
                     declare
                        Op_Map_Poly_Type : constant Type_Descriptor_Ptr :=
                          Build_Polymorphic_Type_Desc
                             (Resolved_Nested_Type,
                              Formal_Type => Op_Map_Target_Ref.all.Root_Type,
                              Source_Pos => Find_Source_Pos
                                (Resolved_Nested_Type.Definition),
                              Underlying_Type_Desc =>
                                Type_Desc.Nested_Types (I));
                     begin
                        if Debug_Code_Gen then
                           Put_Line ("  Created polymorphic type-desc for " &
                             "nested type " & Type_Image (Resolved_Nested_Type)
                             & " viewed as " &
                             Type_Image (Op_Map_Target_Ref.all) &
                             " at location " &
                             Interpreter.Obj_Locator_Image
                               (Op_Map_Poly_Type.Location));
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;
      end if;  --  Has at least one nested type
   end Fill_In_Nested_Type_Info;

   procedure Build_Type_Descriptor (Obj_Type : Type_Sem_Ptr) is
      --  Create a type descriptor for Obj_Type.
      --  Put "location" of type desc in Obj_Type.Type_Descriptor_Location.

      use Interpreter;
      pragma Assert (Obj_Type.Type_Descriptor_Location = Null_Object_Locator);

      --  NOTE: Obj_Type.Associated_Module might be abstract
      --       if we are ultimately building a polymorphic type desc.

      Type_Desc : constant Type_Descriptor_Ptr :=
        new Type_Descriptor (Has_Op_Map => False);
      Assoc_Module : constant Module_Sem_Ptr := Obj_Type.Associated_Module;
      Module_Tree : Module.Tree renames Module.Tree (Tree_Ptr_Of
                                                        (Assoc_Module.
        Definition).all);
      Type_Name : constant Strings.U_String :=
        Strings.String_Lookup (Canonical_Type_Name (Obj_Type));

      --  Mapping from Univ Literal kinds to Type Desc kinds
      Univ_Type_Kinds : constant array (Univ_Literal_Kinds) of Type_Kind_Enum
        := (Integer_Literal => Univ_Integer_Kind,
            Real_Literal    => Univ_Real_Kind,
            String_Literal  => Univ_String_Kind,
            Char_Literal    => Univ_Char_Kind,
            Enum_Literal    => Univ_Enum_Kind);

      Module_Code_Has_Been_Generated : constant Boolean :=
        Assoc_Module.Code_Has_Been_Generated
        and then (Assoc_Module.Other_Part = null
                  or else
                    Assoc_Module.Other_Part.Code_Has_Been_Generated);
      Builtin_Types : constant Builtin_Types_Ptr :=
        Builtin_Types_Array (PSC.Languages.Language);
   begin
      if Debug_Code_Gen then
         Put_Line
           (" Creating type descriptor for " & Strings.To_String (Type_Name));
      end if;
      --  Initialize type descriptor

      Type_Desc.Is_Finished := True;
      --  Presume is finished unless we bump into something
      --  requiring a call on Evaluate_Tree.

      Type_Desc.All_Parameters_Known := Obj_Type.All_Parameters_Known;
      --  Remember whether this type is fully defined

      Type_Desc.Is_Small := Static.Known_To_Be_Small (Obj_Type);

      --  Now can rely on Known_To_Be_Large to be properly initialized:
      Type_Desc.Is_Large := Obj_Type.Known_To_Be_Large;

      Type_Desc.Null_Value := Interpreter.Null_Value;
      --  NOTE: This is updated below if appropriate

      Type_Desc.Is_Abstract := Assoc_Module.Is_Abstract;
      Type_Desc.Is_Partially_Abstract := Assoc_Module.Is_Partially_Abstract;
      Type_Desc.Is_Concurrent := Assoc_Module.Is_Concurrent;

      if Obj_Type.Is_Universal then
         --  Set the Type_Kind to reflect the kind of universal type it is
         for I in Univ_Literal_Kinds loop
            if Obj_Type = Univ_Types (I) then
               Type_Desc.Type_Kind := Univ_Type_Kinds (I);
               exit;
            end if;
         end loop;

         if Type_Desc.Type_Kind = Normal_Kind then
            Sem_Error ("Internal: " & Type_Image (Obj_Type) &
                " doesn't match any univ type.",
              Src_Pos => Find_Source_Pos (Obj_Type.Definition));
         end if;
      elsif Assoc_Module = Basic_Array_Module then
         --  Mark instances of Basic_Array module specially
         Type_Desc.Type_Kind := Basic_Array_Kind;
      elsif Assoc_Module = Aliased_Object_Module then
         --  Mark instances of Aliased Object module specially
         Type_Desc.Type_Kind := Aliased_Object_Kind;
      elsif Builtin_Types = null then
         --  Nothing more to check
         null;
      elsif Assoc_Module = Builtin_Types.Unsigned_64_Module then
         --  Unsigned_64 has its own kind
         Type_Desc.Type_Kind := Unsigned_64_Kind;
      elsif Assoc_Module = Builtin_Types.Integer_64_Module then
         --  Integer_64 has its own kind
         Type_Desc.Type_Kind := Integer_64_Kind;
      end if;

      Type_Desc.Is_Polymorphic := False;
      --  We build the polymorphic type-desc in a separate
      --  routine.

      --  For debugging
      Type_Desc.Name := Type_Name;
      Type_Desc.Type_Sem := Root_Sem_Ptr (Obj_Type);

      --  Install type desc now to get "location" for it
      Install_Type_Info (Type_Desc);

      declare
         use type Type_Sem_Vectors.Elem_Index;
         New_Type_Index : constant Type_Sem_Vectors.Elem_Index :=
           Type_Sem_Vectors.Elem_Index (Type_Desc.Index);
         Type_Sem_Table_Size : constant Type_Sem_Vectors.Elem_Index :=
           Num_Elements (Types_With_Descriptors);
         Index : Type_Sem_Vectors.Elem_Index;
      begin
         --  Add a corresponding element to Types_With_Descriptors
         --  vector
         if New_Type_Index <= Type_Sem_Table_Size then
            --  Already in table; fill in info
            Set_Nth_Element (Types_With_Descriptors,
              New_Type_Index, Obj_Type);
         else
            --  Add mappings to null for missing elements
            while Num_Elements (Types_With_Descriptors) <
              New_Type_Index - 1
            loop
               Add_Element (Types_With_Descriptors, null, Index);
            end loop;

            --  Now add entry for new type desc
            Add_Element (Types_With_Descriptors, Obj_Type, Index);
            pragma Assert (Index = New_Type_Index);
         end if;
      end;

      --  Initialize Type_Decriptor_Loc now to avoid
      --  infinite recursion.
      Obj_Type.Type_Descriptor_Location := Type_Desc.Location;

      --  Info about the components
      Type_Desc.Num_Components := Static.Num_Components (Assoc_Module);

      Type_Desc.Component_Extension_Level :=
        Assoc_Module.Component_Extension_Level;

      Type_Desc.Is_Wrapper := Static.Type_Is_Wrapper (Obj_Type);

      --  Fill in information on interface operations
      Type_Desc.Num_Operations := Assoc_Module.Num_Interface_Operations;

      if Obj_Type.All_Parameters_Known
        and then Type_Desc.Num_Operations > 0
      then
         declare
            Module_Region : constant Symbols.Region_Ptr :=
              Assoc_Module.Nested_Region;
            Found_Equiv : Boolean := False;
         begin

            Type_Desc.Operations :=
              new Routine_Info_Array'
              (1 .. Type_Desc.Num_Operations =>
                (Index => 0,
                 Type_Desc => Type_Desc,
                 Action => No_Action,
                 Op_Index => 0,
                 Use_Static_Link_For_Type => False,
                 Cur_Inst_Param_Info =>
                   Cur_Inst_Params.No_Cur_Inst_Param_Info));

            if Debug_Code_Gen then
               Put_Line (" Building info on operations:");
            end if;
            --  Go through module region and fill in operation table
            for I in 1 .. Symbols.Num_Symbols_In_Region (Module_Region)
            loop
               declare
                  Local_Sym : constant Symbols.Sym_Ptr :=
                    Symbols.Nth_Symbol_In_Region (Module_Region, I);
               begin
                  if Local_Sym.Sem_Info /= null
                    and then Local_Sym.Sem_Info.all in Operation_Semantic_Info
                  then
                     --  We have an operation, see whether it is
                     --  defining a visible operation
                     declare
                        Local_Op_Sem : constant Operation_Sem_Ptr :=
                          Operation_Sem_Ptr (Local_Sym.Sem_Info);
                     begin
                        if Local_Op_Sem.Index > 0
                          and then Local_Op_Sem.Overridden_By = null
                        then
                           --  Build the descriptor of the operation
                           Build_Operation_Routine_Info
                             (Type_Desc.Operations (Local_Op_Sem.Index),
                              Now_Building => Type_Desc,
                              Assoc_Module => Assoc_Module,
                              Local_Op_Sem => Local_Op_Sem,
                              Obj_Type => Obj_Type,
                              Op_Index => Local_Op_Sem.Index);
                        end if;
                     end;
                  end if;
               end;
            end loop;

            if True or else (not Assoc_Module.Is_Abstract
              and then not Assoc_Module.Is_Partially_Abstract)
            then
               --  Make sure all operations are filled in
               --  unless module is abstract.
               for I in 1 .. Type_Desc.Num_Operations loop
                  if Type_Desc.Operations (I).Index = 0 then
                     Sem_Error
                       (Assoc_Module.Definition,
                        "Missing compiled code for " &
                        Nth_Interface_Operation_Name
                           (Assoc_Module,
                            Positive (I)) &
                        " (operation #" &
                        Operation_Index'Image (I) &
                        ')');
                  end if;
               end loop;
            end if;

         end;
      end if;  --  Has at least one interface operation

      if Type_Desc.Num_Components > 0 then
         if Debug_Code_Gen then
            Put_Line (" Building info on components:");
         end if;
         Type_Desc.Components :=
           new Component_Info_Array (1 .. Type_Desc.Num_Components);

         for I in 1 .. Type_Desc.Num_Components loop
            declare
               Comp_Tree : constant Optional_Tree :=
                 Static.Nth_Component (Assoc_Module, I);
               Comp_Sem : constant Object_Sem_Ptr :=
                 Object_Sem_Ptr (Sem_Info (Comp_Tree));
               Comp_Formal_Type : constant Type_Sem_Ptr :=
                 Comp_Sem.Resolved_Type;
               Comp_Actual_Type : Type_Sem_Ptr;
               Comp_Is_By_Ref : constant Boolean :=
                 Comp_Sem.Associated_Symbol /= null
                and then Static.Sym_Is_By_Ref (Comp_Sem.Associated_Symbol);
            begin
               if Obj_Type.All_Parameters_Known then
                  --  Substitute into formal type
                  Comp_Actual_Type :=
                    Static.Substitute_Actuals
                       (Comp_Formal_Type,
                        U_Base_Type_Region (Obj_Type));

                  if not Comp_Actual_Type.All_Parameters_Known then
                     Sem_Error
                       (Comp_Actual_Type.Definition,
                        "Component type not compile-time known");
                     if Debug_Code_Gen then
                        Static.Diagnose_Unknown_Parameters
                          (Comp_Actual_Type,
                           Indent => 2);
                     end if;
                  end if;
               else
                  --  Use formal type as is
                  Comp_Actual_Type := Comp_Formal_Type;
               end if;

               if Comp_Actual_Type.All_Parameters_Known then
                  --  Create full type descriptor for component
                  Type_Desc.Components (I).Type_Desc :=
                     Known_Type_Desc
                       (Run_Time_Type_Info (Comp_Actual_Type),
                        Now_Building => Type_Desc,
                        Src_Pos => Find_Source_Pos (Obj_Type.Definition));
               else
                  --  Create partial type descriptor for component
                  Build_Or_Find_Type_Descriptor (Comp_Actual_Type);

                  Type_Desc.Components (I).Type_Desc :=
                    Interpreter.Type_Descriptor_Ops.To_Type_Desc
                      (Interpreter.Type_Index
                         (Comp_Actual_Type.
                           Type_Descriptor_Location.Offset));
               end if;

               Type_Desc.Components (I).Is_By_Ref := Comp_Is_By_Ref;
               Type_Desc.Components (I).Is_Optional :=
                 Comp_Actual_Type.Value_Is_Optional;
                   --  TBD: Should we "or" with
                   --       Comp_Formal_Type.Value_Is_Optional?
               if Comp_Formal_Type.Known_To_Be_Assignable
                 and then not Comp_Is_By_Ref
                 and then not Comp_Actual_Type.Known_To_Be_Assignable
               then
                  --  Oops, we presume component is assignable
                  --  and it isn't.
                  --  NOTE: This check is redundant with later
                  --       checks on formal/actual match.
                  if Debug_Code_Gen then
                     Put_Line
                       (Type_Image (Comp_Actual_Type) &
                        " must be assignable because component " &
                        Sym_Name (Comp_Sem.Associated_Symbol) &
                        " is assignable");
                  end if;
               end if;
            end;
         end loop;
      end if;  --  Has at least one component

      --  TBD: Fill in Interface_Op_Maps array with op-maps
      --       for each (explicitly) implemented interface.

      if Type_Desc.Is_Small then
         --  If floating point, set Null_Value to Null_Float_Value;
         --  If wrapper, use underlying type Null value.
         if Type_Desc.Is_Wrapper then
            Type_Desc.Null_Value :=
              Type_Desc.Components (1).Type_Desc.Null_Value;
         elsif Obj_Type.U_Base_Type = Univ_Real_Type then
            --  Universal real null
            --  TBD: This might not be correct once we switch
            --      over to using infinite precision univ-real.
            Type_Desc.Null_Value := Interpreter.Null_Float_Value;
         elsif Obj_Type.U_Base_Type = Unsigned_64_Type then
            --  Give it a null value, but don't prevent its use
            Type_Desc.Null_Value := Interpreter.Null_Unsigned_64;
         end if;
      end if;

      --  Fill in info on enclosing type, if any
      if Obj_Type.Enclosing_Type /= null then
         if Debug_Code_Gen then
            Put_Line (" Getting info on enclosing type:");
         end if;
         if Obj_Type.All_Parameters_Known then
            Type_Desc.Enclosing_Type :=
               Known_Type_Desc
                 (Run_Time_Type_Info (Obj_Type.Enclosing_Type),
                  Now_Building => Type_Desc,
                  Src_Pos => Find_Source_Pos (Obj_Type.Definition));
         else
            --  Create if necessary, and then save enclosing type desc
            Build_Or_Find_Type_Descriptor (Obj_Type.Enclosing_Type);

            Type_Desc.Enclosing_Type :=
              Interpreter.Type_Descriptor_Ops.To_Type_Desc
                (Interpreter.Type_Index
                   (Obj_Type.Enclosing_Type.
                     Type_Descriptor_Location.Offset));
         end if;
      end if;

      --  Fill in info on parent type, if any
      if Obj_Type.Parent_Type /= null then
         if Debug_Code_Gen then
            Put_Line (" Getting info on parent type:");
         end if;
         if Obj_Type.Parent_Type.All_Parameters_Known then
            Type_Desc.Parent_Type :=
               Known_Type_Desc
                 (Run_Time_Type_Info (Obj_Type.Parent_Type),
                  Now_Building => Type_Desc,
                  Src_Pos => Find_Source_Pos (Obj_Type.Definition));
         else
            Build_Or_Find_Type_Descriptor (Obj_Type.Parent_Type);

            Type_Desc.Parent_Type :=
              Interpreter.Type_Descriptor_Ops.To_Type_Desc
                (Interpreter.Type_Index
                   (Obj_Type.Parent_Type.
                     Type_Descriptor_Location.Offset));
         end if;
         if Debug_Code_Gen and then Type_Desc.Parent_Type.Index = 0 then
            Static.Diagnose_Unknown_Parameters
              (Obj_Type.Parent_Type,
               Indent => 2);
         end if;
      end if;

      --  Fill in info about the parameters
      Type_Desc.Num_Parameters := Static.Num_Module_Parameters (Assoc_Module);

      if Obj_Type.All_Parameters_Known
        and then Type_Desc.Num_Parameters > 0
        and then Obj_Type.Actual_Sem_Infos /= null
      then
         --  Create "actual" tree for each parameter
         --  Create op-map as well
         Type_Desc.Parameters :=
           new Parameter_Info_Array (1 .. Type_Desc.Num_Parameters);

         if Debug_Code_Gen then
            Put_Line (" Building info on parameters:");
         end if;
         for I in 1 .. Type_Desc.Num_Parameters loop
            declare
               Formal_Tree : constant Optional_Tree :=
                 Static.Nth_Module_Parameter (Assoc_Module, I);
               Formal_Sem : constant Sem_Ptr :=
                 Underlying_Sem_Info (Formal_Tree);
               Actual_Sem : constant Sem_Ptr := Obj_Type.Actual_Sem_Infos (I);
               Actual_Tree : constant Optional_Tree := Actual_Sem.Definition;
            begin
               if Formal_Sem.all in Param_Semantic_Info then
                  --  Defer handling formal obj module parameters
                  Type_Desc.Is_Finished := False;
               elsif Formal_Sem.all in Operation_Semantic_Info then
                  --  TBD: No support for formal op module parameters
                  Sem_Error
                    (Module_Tree,
                     "NYI: Formal operation parameters not supported");
               elsif Actual_Sem.all not in Type_Semantic_Info then
                  Sem_Error
                    (Obj_Type.Definition,
                     "Formal param (" &
                     Subtree_Image (Formal_Tree) &
                     ") is a type, actual (" &
                     Subtree_Image (Actual_Tree) &
                     ") is not");
               else
                  --  We have a type parameter
                  declare
                     Actual_Type_Loc : constant Interpreter.Object_Locator :=
                       Run_Time_Type_Info
                          (Type_Sem_Ptr (Actual_Sem),
                           Formal_Type =>
                              Static.Substitute_Actuals_In_Nested_Type
                                (Type_Sem_Ptr (Formal_Sem),
                                 U_Base_Type_Region (Obj_Type),
                                 Is_Formal_Type => True),
                           Source_Pos => Find_Source_Pos (Actual_Tree));
                     --  Build op-map as necessary
                     Actual_Type_Desc : constant Type_Descriptor_Ptr :=
                       Known_Type_Desc
                          (Actual_Type_Loc,
                           Now_Building => Type_Desc,
                           Src_Pos => Find_Source_Pos (Obj_Type.Definition));
                  begin
                     Type_Desc.Parameters (I).Kind := Interpreter.Formal_Type;
                     Type_Desc.Parameters (I).Data.Type_Desc :=
                       Actual_Type_Desc;
                     Type_Desc.Parameters (I).Data.Addr :=
                       To_Virtual_Address ((null,
                         Offset_Within_Chunk (Actual_Type_Desc.Index)));
                     Type_Desc.Parameters (I).Data.Value :=
                       Null_Virtual_Address;
                  end;
               end if;
            end;
         end loop;
      end if;  --  Has at least one parameter

      --  Fill in info about the actuals-of-formals
      Type_Desc.Num_Actuals_Of_Formals :=
        Natural (Num_Elements (Assoc_Module.Actuals_Of_Formals));

      if Obj_Type.All_Parameters_Known
        and then Type_Desc.Num_Actuals_Of_Formals > 0
      then
         --  Create "actual" tree for each parameter
         --  Create op-map as well
         Type_Desc.Actuals_Of_Formals :=
           new Parameter_Info_Array (1 .. Type_Desc.Num_Actuals_Of_Formals);

         if Debug_Code_Gen then
            Put_Line (" Building info on actuals-of-formals:");
         end if;
         for I in 1 .. Type_Desc.Num_Actuals_Of_Formals loop
            declare
               Actual_Of_Formal : constant Type_Sem_Ptr :=
                 Nth_Element
                    (Assoc_Module.Actuals_Of_Formals,
                     Type_Sem_Vectors.Elem_Index (I));
               Actual_Of_Actual : constant Type_Sem_Ptr :=
                 Static.Substitute_Actuals
                    (Actual_Of_Formal,
                     U_Base_Type_Region (Obj_Type));
               Actual_Of_Actual_Desc : constant Type_Descriptor_Ptr :=
                 Known_Type_Desc
                    (Run_Time_Type_Info
                        (Actual_Of_Actual,
                         Formal_Type => Actual_Of_Formal,
                         Source_Pos => Find_Source_Pos (Obj_Type.Definition)),
                     Now_Building => Type_Desc,
                     Src_Pos => Find_Source_Pos (Obj_Type.Definition));
            begin
               Type_Desc.Actuals_Of_Formals (I) :=
                 (Kind => Interpreter.Formal_Type,
               --  TBD Actual_Of_Formal
                  Data => (Type_Desc => Actual_Of_Actual_Desc,
                           Addr => To_Virtual_Address
                             ((null, Offset_Within_Chunk
                               (Actual_Of_Actual_Desc.Index))),
                           Value => Null_Value));
            end;
         end loop;
      end if;  --  Has at least one actual-of-formal

      if Obj_Type.All_Parameters_Known then
         --  Fill in information on nested types
         Fill_In_Nested_Type_Info (Obj_Type, Type_Desc);
      end if;

      --  See whether there are any nested objects
      Type_Desc.Num_Nested_Objs :=
        Natural (Num_Elements (Assoc_Module.Nested_Objects));

      if Obj_Type.All_Parameters_Known
        and then
          (Type_Desc.Num_Nested_Objs > 0
            or else not Module_Code_Has_Been_Generated
            or else Obj_Type.Generic_Param_Map /= null)
      then
         --  We defer doing Evaluate_Trees until all code
         --  is generated.
         Type_Desc.Is_Finished := False;
      end if;

      if Obj_Type.All_Parameters_Known
        and then Obj_Type.Associated_Module.Needs_Polymorphic_Type_Desc
      then
         --  Create the "self-directed" polymorphic type desc.
         --  NOTE: This might not really be needed.  The more
         --        "interesting" polymorphic type descriptors
         --        are those for nested op-map type descs.
         declare
            Poly_Type : constant Type_Descriptor_Ptr :=
              Build_Polymorphic_Type_Desc
                 (Obj_Type,
                  Formal_Type => Obj_Type,
                  Source_Pos => Find_Source_Pos (Obj_Type.Definition),
                  Underlying_Type_Desc => Type_Desc);
         begin
            if Debug_Code_Gen then
               Put_Line (" Created polymorphic type-desc at location " &
                 Interpreter.Obj_Locator_Image (Poly_Type.Location));
            end if;
         end;
      end if;

      if Debug_Code_Gen then
         Put_Line
           (" Creating type-desc at location " &
            Interpreter.Obj_Locator_Image (Type_Desc.Location) &
            " for " &
            Type_Image (Obj_Type));
      end if;

      if not Type_Desc.Is_Finished then
         --  link onto list of unfinished types
         if Debug_Code_Gen then
            Put_Line
              ("  type-desc for " &
               Type_Image (Obj_Type) &
               " not yet finished.");
         end if;

         --  Add to end of list
         Type_Desc.Prev_Unfinished := Last_Unfinished_Type;
         if Last_Unfinished_Type /= null then
            Last_Unfinished_Type.Next_Unfinished := Type_Desc;
         else
            --  Is also the first on the list
            First_Unfinished_Type := Type_Desc;
         end if;
         Last_Unfinished_Type := Type_Desc;
      else
         if Obj_Type.All_Parameters_Known
           and then Type_Desc.Num_Operations > 0
         then
            --  Put on list to have Cur_Inst_Param_Info filled in.
            if Last_Needing_Cur_Inst_Info /= null then
               Last_Needing_Cur_Inst_Info.Next_Unfinished := Type_Desc;
            else
               --  Is also first on list
               First_Needing_Cur_Inst_Info := Type_Desc;
            end if;
            Last_Needing_Cur_Inst_Info := Type_Desc;
         end if;

         if Debug_Code_Gen then
            Dump_Type_Desc (Type_Desc);
         end if;
      end if;
   end Build_Type_Descriptor;

   function Get_Known_Type_Descriptor (Obj_Type : Type_Sem_Ptr)
     return Interpreter.Type_Descriptor_Ptr is
   --  Return a type descriptor for a type that has All_Parameters_Known
      pragma Assert (Obj_Type.All_Parameters_Known);
   begin
      Build_Or_Find_Type_Descriptor (Obj_Type);

      return Interpreter.Type_Descriptor_Ops.To_Type_Desc
         (Interpreter.Type_Index (Obj_Type.Type_Descriptor_Location.Offset));
   end Get_Known_Type_Descriptor;

   function Build_Parameter_Comparison
     (Decl_Region : Symbols.Region_Ptr; Formal, Actual : Operand_Sem_Ptr)
     return Optional_Tree is
   --  Build and analyze tree representing "Formal == Actual"

      Actual_Src_Pos : constant Source_Positions.Source_Position :=
       Find_Source_Pos (Actual.Definition);

      Compare_Op_Tree : constant Optional_Tree :=
        Identifier.Make (Static.Compare_Op_Str, Actual_Src_Pos);

      Compare_Operands : constant Optional_Tree :=
        Invocation.Make
           (Kind => Invocation.Operation_Call,
            Prefix => Compare_Op_Tree,
            Operands => Lists.Make ((Formal.Definition, Actual.Definition)),
            Source_Pos => Actual_Src_Pos);

      Check_Equiv : constant Optional_Tree :=
        Invocation.Make
           (Kind => Invocation.Operation_Call,
            Prefix =>
               Identifier.Make
                 (Static.To_Bool_Str,
                  Actual_Src_Pos),
            Operands =>
               Lists.Make
                 ((Compare_Operands,
                   Identifier.Make
                      (Static.Cond_Mask_Str (Binary.Equal_Op),
                       Actual_Src_Pos))),
           Source_Pos => Actual_Src_Pos);
   begin
      Static.First_Pass (Decl_Region, Check_Equiv);
      Static.Second_Pass (Decl_Region, Check_Equiv,
                          Context => Operand_Context,
                          Resolve_Expr => True);
      return Check_Equiv;
   end Build_Parameter_Comparison;

   procedure Finish_Type_Descriptor
     (Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False) is
      --  Finish up formal object parameters and nested objects
      --  If Return_On_Recursion is True, do not complain about recursion
      --  and simply return immediately.
      use Interpreter;
      Obj_Type : constant Type_Sem_Ptr := Type_Sem_Ptr (Type_Desc.Type_Sem);
      Assoc_Module : constant Module_Sem_Ptr := Obj_Type.Associated_Module;
   begin
      if Type_Desc.Next_Unfinished = Type_Desc then
         if not Return_On_Recursion then
            --  This is an error unless "Return_On_Recursion" is True.
            Sem_Error
              (Obj_Type.Definition,
               "Recursed while finishing type descriptor for " &
               Type_Image (Obj_Type));
         end if;
         return;
      end if;

      --  Fill in info about the parameters
      if Debug_Code_Gen then
         Put_Line (" Finishing Type Descriptor for " & Type_Image (Obj_Type));
      end if;

      --  Remove from unfinished list, and point back to self
      --  to detect recursion.
      if Type_Desc.Next_Unfinished /= null then
         Type_Desc.Next_Unfinished.Prev_Unfinished :=
           Type_Desc.Prev_Unfinished;
      else
         --  Was last on unfinished list
         Last_Unfinished_Type := Type_Desc.Prev_Unfinished;
      end if;

      if Type_Desc.Prev_Unfinished /= null then
         Type_Desc.Prev_Unfinished.Next_Unfinished :=
           Type_Desc.Next_Unfinished;
      else
         --  Was first on unfinished list
         First_Unfinished_Type := Type_Desc.Next_Unfinished;
      end if;

      --  Now point to self
      Type_Desc.Next_Unfinished := Type_Desc;
      Type_Desc.Prev_Unfinished := Type_Desc;

      --  Update nested-type info in case more nested types were added.
      Fill_In_Nested_Type_Info (Obj_Type, Type_Desc);

      if Type_Desc.Num_Parameters > 0
        and then Obj_Type.Actual_Sem_Infos /= null
      then
         --  Create "actual" tree for each parameter
         if Debug_Code_Gen then
            Put_Line (" Finishing info on parameters:");
         end if;
         for I in 1 .. Type_Desc.Num_Parameters loop
            declare
               Formal_Tree : constant Optional_Tree :=
                 Static.Nth_Module_Parameter (Assoc_Module, I);
               Formal_Sem : constant Sem_Ptr :=
                 Underlying_Sem_Info (Formal_Tree);
            begin
               if Formal_Sem.all in Param_Semantic_Info then
                  --  Now we handle module parameters that are objects
                  declare
                     Substituted_Formal_Type : constant Type_Sem_Ptr :=
                       Static.Substitute_Actuals
                          (Param_Sem_Ptr (Formal_Sem).Resolved_Type,
                           U_Base_Type_Region (Obj_Type));

                     Actual_Sem : constant Sem_Ptr :=
                       Obj_Type.Actual_Sem_Infos (I);
                     Actual_Tree : constant Optional_Tree :=
                       Actual_Sem.Definition;
                     Result_Type_Desc : Type_Descriptor_Ptr;
                     Result_Value : Word_Type := Null_Value;
                     Result_Addr : Object_Virtual_Address :=
                       Null_Virtual_Address;
                  begin
                     if Actual_Sem.all not in Operand_Semantic_Info'Class
                       or else not Types_Match
                                     (Operand_Sem_Ptr (Actual_Sem).
                                        Resolved_Type,
                                      Substituted_Formal_Type)
                     then
                        Sem_Error
                          (Actual_Tree,
                           "Actual for formal " &
                           Subtree_Image (Formal_Sem.Definition) &
                           " not of expected type " &
                           Type_Image
                              (Param_Sem_Ptr (Formal_Sem).Resolved_Type));
                     end if;

                     --  Evaluate actual parameter
                     Evaluate_Tree
                       (Resolved_Tree (Actual_Tree),
                        Enclosing_Type => Type_Desc.Enclosing_Type,
                     --  TBD: or Type_Desc itself?
                        Value => Result_Value,
                        Addr => Result_Addr,
                        Type_Desc => Result_Type_Desc);

                     --  Fill in info
                     Type_Desc.Parameters (I) :=
                       (Kind => Formal_Object,
                        Data => (Type_Desc => Result_Type_Desc,
                                 Addr => Result_Addr,
                                 Value => Result_Value));

                  end;
               end if;

               exit when Sem_Error_Count > 0;
            end;
         end loop;
      end if;

      --  Re-initialize Num_Nested_Objs in case module wasn't quite
      --  finished having code generated when type desc was first created.
      Type_Desc.Num_Nested_Objs :=
        Natural (Num_Elements (Assoc_Module.Nested_Objects));

      if Type_Desc.Num_Nested_Objs > 0 then
         --  Fill in info on nested objects
         Type_Desc.Nested_Objs :=
           new Const_Info_Array (1 .. Type_Desc.Num_Nested_Objs);

         if Debug_Code_Gen then
            Put_Line (" Building info on nested objects:");
         end if;

         for I in 1 .. Type_Desc.Num_Nested_Objs loop
            declare
               Const_Sem : constant Sem_Ptr :=
                 Nth_Element
                    (Assoc_Module.Nested_Objects, Obj_Sem_Info_Index (I));
            begin
               if Const_Sem.all in Sym_Reference_Info'Class then
                  --  We want to copy from another type-desc.
                  --  TBD: Should we do this on a second pass?
                  declare
                     Const_Ref : constant Sym_Ref_Ptr :=
                       Sym_Ref_Ptr (Const_Sem);
                     Substituted_Prefix : constant Type_Sem_Ptr :=
                       Static.Substitute_Actuals
                          (Type_Sem_Ptr (Const_Ref.Prefix_Type_Region),
                           U_Base_Type_Region (Obj_Type),
                           Ancestor_From_Formal_Module => True);
                     From_Type_Desc : constant Type_Descriptor_Ptr :=
                       Known_Type_Desc
                          (Run_Time_Type_Info (Substituted_Prefix),
                           Now_Building => Type_Desc,
                           Src_Pos => Find_Source_Pos (Obj_Type.Definition));
                     From_Locator : constant Object_Locator :=
                       Object_Sem_Ptr (Const_Ref.Underlying_Sem_Info).Info.
                         Obj_Location;
                  begin
                     if Debug_Code_Gen then
                        Put_Line
                          (" Copying value of " &
                           Subtree_Image (Const_Ref.Definition));
                     end if;
                     --  Copy info
                     Type_Desc.Nested_Objs (I).Data :=
                        Nth_Type_Area_Element
                          (From_Type_Desc,
                           From_Locator.Offset);

                     --  Provide a more explicit name for debugging
                     --  TBD: This is not necessarily the link-name
                     --       for the constant
                     Type_Desc.Nested_Objs (I).Name :=
                        Strings.String_Lookup
                          (Strings.To_String (Type_Desc.Name) & "::" &
                            Subtree_Image (Const_Ref.Definition));
                  end;
               else
                  --  This constant is defined by an expression tree;
                  --  Evaluate it now.
                  declare
                     Nested_Obj : constant Object_Sem_Ptr :=
                       Object_Sem_Ptr (Const_Sem);
                     Initial_Value : constant Optional_Tree :=
                       Obj_Decl.Tree (Tree_Ptr_Of
                         (Nested_Obj.Definition).all).Obj_Value;
                     Resolved_Initial_Value : constant Optional_Tree :=
                       Static.Substitute_Actuals_In_Operand
                          (Resolved_Tree (Initial_Value),
                           U_Base_Type_Region (Obj_Type));
                  begin
                     --  Create a unique link-name
                     Type_Desc.Nested_Objs (I).Name :=
                        Strings.String_Lookup
                          (Strings.To_String (Type_Desc.Name) & "::" &
                            Sym_Name (Nested_Obj.Associated_Symbol));

                     Evaluate_Tree
                       (Resolved_Tree (Resolved_Initial_Value),
                        Decl_Region =>
                          Nested_Obj.Associated_Symbol.Enclosing_Region,
                        Enclosing_Type => Type_Desc,
                        Dest_Name => Strings.Index
                          (Type_Desc.Nested_Objs (I).Name),
                        Value => Type_Desc.Nested_Objs (I).Data.Value,
                        Addr => Type_Desc.Nested_Objs (I).Data.Addr,
                        Type_Desc =>
                          Type_Desc.Nested_Objs (I).Data.Type_Desc);

                     exit when Sem_Error_Count > 0;
                  end;
               end if;
            end;
         end loop;
      end if;  --  Has at least one nested object

      if Obj_Type.Generic_Param_Map /= null then
         --  Need to check for full actual <-> formal match
         --  for generic operation parameters.
         declare
            Param_Map_Ptr : Param_Mapping_Ptr :=
              Obj_Type.Generic_Param_Map;
         begin
            while Param_Map_Ptr /= null loop
               if Param_Map_Ptr.From.all in Type_Semantic_Info then
                  --  if From is a type and has parameters, need to check
                  declare
                     From_Type_Sem : constant Type_Sem_Ptr :=
                       Type_Sem_Ptr (Param_Map_Ptr.From);
                     To_Type_Sem : constant Type_Sem_Ptr :=
                       Type_Sem_Ptr (Param_Map_Ptr.To);
                  begin
                     if From_Type_Sem.Associated_Module =
                          To_Type_Sem.Associated_Module
                           --  TBD: Different checks needed when modules differ
                       and then
                        not All_Nulls (From_Type_Sem.Actual_Sem_Infos)
                       and then
                        not All_Nulls (To_Type_Sem.Actual_Sem_Infos)
                     then
                        if Debug_Code_Gen then
                           Put_Line ("Check that " &
                             Type_Image (From_Type_Sem) & " matches " &
                             Type_Image (To_Type_Sem));
                        end if;

                        pragma Assert
                          (From_Type_Sem.Actual_Sem_Infos'Length =
                           To_Type_Sem.Actual_Sem_Infos'Length);

                        for I in From_Type_Sem.Actual_Sem_Infos'Range loop
                           if From_Type_Sem.Actual_Sem_Infos (I).all in
                             Operand_Semantic_Info'Class
                           then
                              declare
                                 Check_Equiv : constant Optional_Tree :=
                                   Build_Parameter_Comparison
                                     (Obj_Type.Generic_Param_Region,
                                      Operand_Sem_Ptr
                                        (From_Type_Sem.Actual_Sem_Infos (I)),
                                      Operand_Sem_Ptr
                                        (To_Type_Sem.Actual_Sem_Infos (I)));
                                  use Interpreter;
                                  Result : Word_Type;
                                  Addr_Of_Result : Object_Virtual_Address;
                                  Type_Of_Result : Type_Descriptor_Ptr;
                              begin
                                 if Debug_Code_Gen then
                                    Put_Line ("About to call Evaluate_Tree");
                                 end if;
                                 Evaluate_Tree
                                   (Check_Equiv,
                                    Obj_Type.Generic_Param_Region,
                                    Enclosing_Type => Type_Desc.Parent_Type,
                                    Value => Result,
                                    Addr => Addr_Of_Result,
                                    Type_Desc => Type_Of_Result);

                                 if Debug_Code_Gen then
                                    Put_Line
                                      ("Back from Evaluate_Tree, Result =" &
                                       Result'Image);
                                 end if;

                                 if Result = 0 then
                                    Sem_Error
                                      (Type_Image (From_Type_Sem) &
                                        " does not match " &
                                       Type_Image (To_Type_Sem),
                                       Src_Pos => Find_Source_Pos
                                         (Obj_Type.Definition));

                                    exit;

                                 end if;
                              end;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;
               Param_Map_Ptr := Param_Map_Ptr.Next;
            end loop;
         end;
      end if;

      --  Mark as finished
      Type_Desc.Is_Finished := True;

      --  Remove pointers to self
      Type_Desc.Next_Unfinished := null;
      Type_Desc.Prev_Unfinished := null;

      if Debug_Code_Gen then
         Dump_Type_Desc (Type_Desc);
      end if;

   end Finish_Type_Descriptor;

   function Build_Type_Op_Map
     (Obj_Type : Type_Sem_Ptr;
      Formal_Type : Type_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position)
      return Interpreter.Type_Descriptor_Ptr
   --  Build an "op-map" if needed for the given Formal_Type.
   --  Return null if op-map not needed.
   is
      function Type_To_Use return Type_Sem_Ptr is
      --  Return Obj_Type if Formal_Type is null
      begin
         if Formal_Type = null then
            return Obj_Type;
         else
            return Formal_Type;
         end if;
      end Type_To_Use;

      Formal_Type_To_Use : constant Type_Sem_Ptr := Type_To_Use;
      Formal_Type_Module : constant Module_Sem_Ptr :=
        Formal_Type_To_Use.Associated_Module;

      use Interpreter;

   begin  --  Build_Type_Op_Map

      if Formal_Type_To_Use.Known_To_Be_Assignable
        and then not Obj_Type.Known_To_Be_Assignable
      then
         Sem_Error
           (Type_Image (Obj_Type) &
            " must be assignable because " &
            Type_Image (Formal_Type_To_Use) &
            " is.",
            Src_Pos => Source_Pos);
      end if;

      if not Obj_Type.All_Parameters_Known then
         Sem_Error
           ("Cannot create op-map for incompletely defined type.",
            Src_Pos => Source_Pos);
      elsif not Static.Is_Ancestor
                  (Ancestor => Formal_Type_Module,
                   Descendant => Obj_Type.Associated_Module)
        and then Formal_Type_Module.Num_Interface_Operations > 0
      then
         --  Create op-map if necessary
         declare
            Formal_Type_Region : constant Symbols.Region_Ptr :=
              Formal_Type_Module.Nested_Region;
            Op_Map : Operation_Index_Array (
              1 .. Formal_Type_Module.Num_Interface_Operations) :=
              (others => 0);
            Need_Op_Map : Boolean := False;
            Progenitor_Type : Type_Sem_Ptr :=
              Static.Corresponding_Progenitor_Type
                 (Obj_Type, Formal_Type_Module);
         begin

            if Progenitor_Type = null then
               --  Fall back on Formal type if Obj_Type does not
               --  explicitly implement the formal module, in case
               --  Formal_Type has any actuals specified.

               if Static.Num_Module_Parameters (Formal_Type_Module) = 0 then
                  --  Switch to the (non-formal) current instance type
                  Progenitor_Type := Formal_Type_Module.Cur_Inst_Sem;
               else
                  Progenitor_Type := Formal_Type_To_Use;
               end if;
            end if;

            if Debug_Code_Gen then
               Put_Line
                 (" Determining op-map, actual type is " &
                  Type_Image (Obj_Type) &
                  ", formal type is " &
                  Type_Image (Formal_Type_To_Use));
            end if;

            --  Go through module region and fill in Op_Map
            for I in
                 1 .. Symbols.Num_Symbols_In_Region (Formal_Type_Region)
            loop
               declare
                  Formal_Op_Sym : constant Symbols.Sym_Ptr :=
                    Symbols.Nth_Symbol_In_Region (Formal_Type_Region, I);
               begin
                  if Formal_Op_Sym.Sem_Info /= null
                    and then Formal_Op_Sym.Sem_Info.all in
                       Operation_Semantic_Info
                  then
                     --  We have an operation, see whether it is
                     --  declaring a visible operation
                     declare
                        Formal_Op_Sem : constant Operation_Sem_Ptr :=
                          Operation_Sem_Ptr (Formal_Op_Sym.Sem_Info);
                     begin
                        if Formal_Op_Sem.Index > 0
                          and then Formal_Op_Sem.Overridden_By = null
                          and then Formal_Op_Sem.Context in
                             Any_Interface_Item_Contexts
                        then
                           --  We have an interface operation
                           --  Look for corresponding operation in actual type
                           declare
                              Actual_Op : Symbols.Sym_Ptr :=
                                Symbols.Lookup_In_Region
                                   (Obj_Type.Associated_Module.Nested_Region,
                                    Formal_Op_Sym.Str);
                              Partial_Matching_Op : Symbols.Sym_Ptr :=
                                Actual_Op;
                              Ignore : Boolean;
                           begin
                              if Debug_Code_Gen
                                and then Actual_Op = null
                              then
                                 Put_Line
                                   ("No decl for " &
                                    Sym_Name (Formal_Op_Sym) &
                                    " in region of " &
                                    Type_Image (Obj_Type));
                              end if;
                              while Actual_Op /= null loop
                                 if Actual_Op.Sem_Info.all in
                                    Operation_Semantic_Info
                                   and then
                                    Operation_Sem_Ptr (Actual_Op.Sem_Info).
                                     Context in Any_Interface_Item_Contexts
                                   and then Operation_Sem_Ptr
                                    (Actual_Op.Sem_Info).Overridden_By = null
                                 then
                                    exit when Static.Signatures_And_Modes_Match
                                       (Operation_Sem_Ptr (Actual_Op.Sem_Info),
                                        Formal_Op_Sem,
                                        Extra_Subst1 =>
                                          Obj_Type.Root_Type,
                                        Extra_Subst2 =>
                                          Progenitor_Type.Root_Type,
                                        Substitute_Using1 =>
                                           U_Base_Type_Region (Obj_Type),
                                        Substitute_Using2 =>
                                           U_Base_Type_Region
                                             (Progenitor_Type),
                                        Allow_New_Type1 => True);

                                    --  Keep looking for op that matches
                                    Partial_Matching_Op := Actual_Op;

                                    if Debug_Code_Gen then
                                       Put_Line
                                         ("Found op but not visible " &
                                          "or sigs don't match; context = " &
                                          Context_Enum'Image
                                             (Operation_Sem_Ptr (
                                         Actual_Op.Sem_Info).Context) &
                                          "; comparing " &
                                          Subtree_Image
                                             (Actual_Op.Definition,
                                              Use_Short_Form => True) &
                                          " vs. " &
                                          Subtree_Image
                                             (Formal_Op_Sem.Definition,
                                              Use_Short_Form => True));
                                       Put_Line
                                         ("  Substitute_Using1 => " &
                                          Type_Image (Obj_Type));
                                       Put_Line
                                         ("  Substitute_Using2 => " &
                                          Type_Image (Formal_Type_To_Use));
                                    end if;
                                 end if;

                                 Actual_Op :=
                                    Symbols.Next_Homonym_In_Region
                                      (Actual_Op);
                              end loop;

                              if Actual_Op /= null then
                                 --  Found a match, remember the index
                                 Op_Map (Formal_Op_Sem.Index) :=
                                   Operation_Sem_Ptr (Actual_Op.Sem_Info).
                                     Index;
                              elsif Formal_Op_Sem.Equiv_To /= null
                                and then Formal_Op_Sem.Equiv_From_Type = null
                                and then
                                  Op_Map (Formal_Op_Sem.Equiv_To.Index) /= 0
                              then
                                 --  Interface specified an equivalence.
                                 --  Use that if not overridden.
                                 --  Copy info from equiv operation
                                 --  TBD: Make second pass to fill in.
                                 Op_Map (Formal_Op_Sem.Index) :=
                                   Op_Map (Formal_Op_Sem.Equiv_To.Index);
                              elsif Operation.Tree
                                (Tree_Ptr_Of (Formal_Op_Sem.Definition).all).
                                  Is_Optional
                              then
                                 if Debug_Code_Gen then
                                    Put_Line ("Using default for optional " &
                                      Nth_Interface_Operation_Name
                                        (Formal_Type_Module,
                                         Positive (Formal_Op_Sem.Index)) &
                                      " (operation #" &
                                      Operation_Index'Image
                                        (Formal_Op_Sem.Index) &
                                      ") in " &
                                      Type_Image (Obj_Type));
                                 end if;
                                 --  Set Op_Map to special indicator used for
                                 --  "optional" operations
                                 Op_Map (Formal_Op_Sem.Index) :=
                                   Optional_Operation_Index;
                              else
                                 Sem_Error
                                   ("No match found for " &
                                    Nth_Interface_Operation_Name
                                       (Formal_Type_Module,
                                        Positive (Formal_Op_Sem.Index)) &
                                    " (operation #" &
                                    Operation_Index'Image
                                       (Formal_Op_Sem.Index) &
                                    ") in " &
                                    Type_Image (Obj_Type),
                                    Src_Pos => Source_Pos);

                                 if Partial_Matching_Op /= null then
                                    --  Give more info on mismatch
                                    Ignore :=
                                       Static.Signatures_And_Modes_Match
                                          (Operation_Sem_Ptr
                                            (Partial_Matching_Op.Sem_Info),
                                           Formal_Op_Sem,
                                           Extra_Subst1 =>
                                             Obj_Type.Root_Type,
                                           Extra_Subst2 =>
                                             Progenitor_Type.Root_Type,
                                           Substitute_Using1 =>
                                              U_Base_Type_Region (Obj_Type),
                                           Substitute_Using2 =>
                                              U_Base_Type_Region
                                                (Progenitor_Type),
                                           Allow_New_Type1 => True,
                                           Diagnose => True);
                                 end if;
                              end if;
                           end;
                        end if;  --  Formal_Op_Sym is an interface operation
                     end;
                  end if; --  Formal_Op_Sym is an operation
               end;
            end loop;

            for I in Op_Map'Range loop
               if Op_Map (I) = 0 then
                  Sem_Error
                    ("Missing definition for " &
                     Nth_Interface_Operation_Name
                        (Formal_Type_Module,
                         Positive (I)) &
                     " (operation #" &
                     Operation_Index'Image (I) &
                     " of " &
                     Type_Image (Formal_Type_To_Use) &
                     ").",
                     Src_Pos => Source_Pos);
               elsif Op_Map (I) /= I then
                  Need_Op_Map := True;
               end if;
            end loop;

            if Need_Op_Map then
               --  See whether we already have an op-map for this combo.
               declare
                  Op_Map_Name : Strings.U_String :=
                     Strings.String_Lookup
                       (Canonical_Type_Name (Obj_Type) & "-->" &
                        Canonical_Type_Name (Formal_Type_To_Use));
                  Existing_Op_Map : constant Type_Descriptor_Ptr :=
                    Type_Descriptor_Ops.Get_Type_Desc_By_Name (Op_Map_Name);
               begin
                  if Existing_Op_Map /= null then
                     --  Just copy over the pre-assigned locator
                     --  and fill in the type-sems.
                     Existing_Op_Map.Type_Sem := Root_Sem_Ptr (Obj_Type);
                     Existing_Op_Map.Formal_Type_Sem :=
                       Root_Sem_Ptr (Formal_Type_To_Use);

                     pragma Assert (Op_Map = Existing_Op_Map.Op_Map.all);

                     return Existing_Op_Map;  --  All done  --
                  end if;
               end;
            end if;

            if Need_Op_Map then
               --  Now use the Op_Map to create an op-map type desc.
               declare
                  Actual_Type_Desc : constant Type_Descriptor_Ptr :=
                    Known_Type_Desc (Obj_Type.Type_Descriptor_Location);
                  Op_Map_Type_Desc : constant Type_Descriptor_Ptr :=
                    new Type_Descriptor'
                    (Has_Op_Map => True,
                     Magic_Number => Type_Indicator,
                     Index => 0,
                     Location => Null_Object_Locator,
                     Corresponding_Polymorphic_Type_Desc => null,
                     Is_Finished => True,
                     Next_Unfinished | Prev_Unfinished => null,
                     Name => Strings.String_Lookup
                       (Canonical_Type_Name (Obj_Type) & "-->" &
                        Canonical_Type_Name (Formal_Type_To_Use)),
                     Type_Sem => Root_Sem_Ptr (Obj_Type),  --  For debugging
                     Num_Operations =>
                       Formal_Type_Module.Num_Interface_Operations,
                     Type_Kind => Actual_Type_Desc.Type_Kind,
                     All_Parameters_Known =>
                       Actual_Type_Desc.All_Parameters_Known,
                     Is_Small => Actual_Type_Desc.Is_Small,
                     Is_Large => Actual_Type_Desc.Is_Large,
                     Is_Wrapper => Actual_Type_Desc.Is_Wrapper,
                     Is_Polymorphic => Actual_Type_Desc.Is_Polymorphic,
                     Null_Value => Actual_Type_Desc.Null_Value,
                     Op_Map => new Operation_Index_Array'(Op_Map),
                     Actual_Type => Actual_Type_Desc,
                     Formal_Type_Sem => Root_Sem_Ptr (Formal_Type_To_Use));
                  --  Formal_Type_Sem is only used for debugging
                  Op_Map_Index : Type_Sem_Vectors.Elem_Index;
                  use type Type_Sem_Vectors.Elem_Index;
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       ("Initializing op-map mapping from " &
                        Type_Image (Obj_Type) &
                        " to " &
                        Type_Image (Formal_Type_To_Use));
                  end if;

                  --  Install op-map type desc now to get "location" for it
                  Install_Type_Info (Op_Map_Type_Desc);

                  --  Add a corresponding element to Types_With_Descriptors
                  --  vector
                  --  TBD: Remember the formal type somewhere?
                  Add_Element
                    (Types_With_Descriptors,
                     Obj_Type,
                     Op_Map_Index);
                  pragma Assert
                    (Op_Map_Index =
                     Type_Sem_Vectors.Elem_Index (Op_Map_Type_Desc.Index));

                  if Debug_Code_Gen then
                     Dump_Type_Desc (Op_Map_Type_Desc);
                  end if;

                  return Op_Map_Type_Desc;
               end;
            end if;  --  If we need an op-map
         end;
      end if;  --  If Formal_Type from different module

      --  Don't need an op-map
      return null;
   end Build_Type_Op_Map;

   function Build_Polymorphic_Type_Desc
     (Obj_Type : Type_Sem_Ptr;
      Formal_Type : Type_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position;
      Underlying_Type_Desc : Interpreter.Type_Descriptor_Ptr)
      return Interpreter.Type_Descriptor_Ptr
   is
      --  Build a polymorphic type descriptor, given
      --  a type descriptor (or op-map) for the underlying
      --  non-polymorphic type descriptor used for Obj_Type
      --  implementing Formal_Type.

      use Interpreter;

      function Name_To_Use return String is
      --  Return Name to use for the polymorphic type (for debugging)
      begin
         if Formal_Type = null
           or else not Underlying_Type_Desc.Has_Op_Map
         then
            if Obj_Type.Is_Polymorphic then
               return Canonical_Type_Name (Obj_Type);
            else
               return Canonical_Type_Name (Obj_Type) & '+';
            end if;
         elsif Formal_Type.Is_Polymorphic then
            return Canonical_Type_Name (Formal_Type) &
                   '(' &
                   Canonical_Type_Name (Obj_Type) &
                   ')';
         else
            return Canonical_Type_Name (Formal_Type) &
                   "+(" &
                   Canonical_Type_Name (Obj_Type) &
                   ')';
         end if;
      end Name_To_Use;

      function Ancestor_Table (Obj_Type : Type_Sem_Ptr)
        return Type_Desc_Array_Ptr is
      --  Return a pointer to an array of type descriptors, one for
      --  each (non-polymorphic) ancestor of Obj_Type, including Obj_Type
      --  itself.
      --  NOTE: Currently this only includes parents, grand parents, etc.
      --        This does not include types that are merely "implemented."
         Num_Ancestors : Positive := 1;
         Obj_Root_Type : constant Type_Sem_Ptr := Obj_Type.Root_Type;
         Ancestor : Type_Sem_Ptr := Obj_Root_Type;
      begin
         --  Count up ancestors
         while Ancestor.Parent_Type /= null loop
            Num_Ancestors := Num_Ancestors + 1;
            Ancestor := Ancestor.Parent_Type;
         end loop;

         --  Build ancestor table
         declare
            Result : constant Type_Desc_Array_Ptr :=
              new Type_Desc_Array (1 .. Num_Ancestors);
         begin
            Ancestor := Obj_Root_Type;
            --  Fill table in reverse order
            for I in reverse 1 .. Num_Ancestors loop
               Result (I) := Get_Known_Type_Descriptor (Ancestor.Root_Type);
               Ancestor := Ancestor.Parent_Type;
            end loop;

            return Result;
         end;
      end Ancestor_Table;

   begin  --  Build_Polymorphic_Type_Desc

      if Underlying_Type_Desc.Corresponding_Polymorphic_Type_Desc =
         null
      then
         --  Need to create a polymorphic version
         declare
            Ancestors : constant Type_Desc_Array_Ptr :=
              Ancestor_Table (Obj_Type);

            Poly_Type_Desc : constant Type_Descriptor_Ptr :=
              new Type_Descriptor'
              (Has_Op_Map => False,
               Magic_Number => Type_Indicator,
               Index => 0,
               Location => Null_Object_Locator,
               Corresponding_Polymorphic_Type_Desc => null,
               Is_Finished => True,
               Next_Unfinished => null,
               Prev_Unfinished => null,
               Num_Operations => 0,
               Name => Strings.String_Lookup (Name_To_Use),
                  --  For debugging
               Type_Sem => Root_Sem_Ptr (Obj_Type),
                  --  For debugging
               Type_Kind => Normal_Kind,
               All_Parameters_Known =>
                 Underlying_Type_Desc.All_Parameters_Known,
               Is_Small => False,
               Is_Large => True,
               Is_Wrapper => False,
               Is_Polymorphic => True,
               Null_Value => Null_Virtual_Address,
               Parent_Type => null,
               Component_Extension_Level => 0,
               Is_Abstract => False,
               Is_Partially_Abstract => False,
               Is_Concurrent => False, --  TBD
               Enclosing_Type => null,  --  TBD
               Root_Type_Desc => null,  -- Filled in below
               Num_Parameters => 0,  --  TBD
               Parameters => null,
               Num_Actuals_Of_Formals => 0,
               Actuals_Of_Formals => null,
               Num_Components => 1,
               Components =>
                 new Component_Info_Array'(1 =>
                   (Type_Desc => Underlying_Type_Desc,
                    Is_By_Ref => False,
                    Is_Optional => True)),  --  TBD: can it be null?
               Num_Nested_Types => Ancestors'Length,
               Nested_Types => Ancestors,
               Num_Nested_Objs => 0,
               Nested_Objs => null,
               Operations => null,  --  TBD
               Num_Interface_Op_Maps => 0,
               Interface_Op_Maps => null);

            Poly_Type_Desc_Index : Type_Sem_Vectors.Elem_Index;
            use type Type_Sem_Vectors.Elem_Index;
         begin
            Poly_Type_Desc.Components (1).Is_By_Ref := False;
            --#stt 1/21/2011 AdaMagic bug workaround

            Install_Type_Info (Poly_Type_Desc);
            --  Add a corresponding element to Types_With_Descriptors
            --  vector
            --  TBD: Remember the formal type somewhere?
            Add_Element
              (Types_With_Descriptors,
               Obj_Type,
               Poly_Type_Desc_Index);
            pragma Assert
              (Poly_Type_Desc_Index =
               Type_Sem_Vectors.Elem_Index (Poly_Type_Desc.Index));
            Underlying_Type_Desc.Corresponding_Polymorphic_Type_Desc :=
              Poly_Type_Desc;

            --  Fill in Root_Type_Desc to point to root type's type desc
            --  if Formal_Type is in fact fully known
            if Formal_Type /= null
              and then not Formal_Type.Is_Formal_Type
              and then Formal_Type.All_Parameters_Known
              and then Underlying_Type_Desc.Has_Op_Map
            then
               Poly_Type_Desc.Root_Type_Desc :=
                 Get_Known_Type_Descriptor (Formal_Type);
            elsif not Underlying_Type_Desc.Has_Op_Map then
               --  Use Underlying_Type_Desc if Formal_Type not known
               --  or Underlying_Type_Desc is a "real" type.
               Poly_Type_Desc.Root_Type_Desc := Underlying_Type_Desc;
            end if;

            if Debug_Code_Gen then
               Dump_Type_Desc (Poly_Type_Desc);
            end if;
         end;
      end if;
      return Underlying_Type_Desc.Corresponding_Polymorphic_Type_Desc;
   end Build_Polymorphic_Type_Desc;

   function Run_Time_Type_Info
     (Obj_Type : Type_Sem_Ptr;
      Referring_Module : Module_Sem_Ptr := null;
      Formal_Type : Type_Sem_Ptr := null;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      Is_Polymorphic_Type_Id : Boolean := False;
      Polymorphic_Param_Index : Integer := 0;
      For_Val_Param_Or_Const : Boolean := False)
      return Interpreter.Object_Locator
   is
      --  Return pointer to type information
      --  If Referring_Module is non-null, may assume that Type_Area
      --  is set up to point to a type for the referring module.
      --  If Formal_Type is non-null, create op-map
      --  relative to formal type and locator for op-map type-descriptor
      --  if necessary.
      --  If Is_Polymorphic_Type_Id is True, then we want a locator for
      --  a polymorphic type descriptor.
      --  If Polymorphic_Param_Index is not zero,
      --  specified parameter is polymorphic and its type-id
      --  should be used as the static link.  If it is negative,
      --  param is passed by reference, so an extra level of indirection
      --  is required.
      --  If For_Val_Param_Or_Const is True, then return locator for
      --  the type-desc to use when fetching a value-param or a constant; this
      --  is relevant only if Obj_Type is a formal type, and it has
      --  value parameters or nested objects, in which case we use the nested
      --  index of Obj_Type rather than its formal type index, as that is
      --  where the descriptor for the appropriate ancestor of the actual
      --  type passed for Obj_Type (the one for the ancestor from the same
      --  module as Obj_Type itself, rather than from some module that extends
      --  or implements Obj_Type's module).

      use type Type_Sem_Vectors.Elem_Index;

      --  Nested_Type_Index needs to be filled in when For_Val_Param_Or_Const
      pragma Assert (not For_Val_Param_Or_Const
         or else not Obj_Type.Is_Formal_Type
         or else Obj_Type.Nested_Type_Index > 0);

      use Interpreter;
      Result : Object_Locator := Null_Object_Locator;
      Type_Name : Strings.U_String;
      Defining_Module : Module_Sem_Ptr;
      --  Module for which Nested_Type_Index or Actual_Of_Formal_Index
      --  are defined.

      function Module_Relative_Type_Loc
        (Def_Module : Module_Sem_Ptr;
         Ref_Kind : Interpreter.Type_Desc_Ref_Kind;
         Offset : Interpreter.Offset_Within_Area)
         return Interpreter.Object_Locator
      is
         --  Return (area base, offset) corresponding to given module
         --  kind of reference, and offset within that subarea,
         --  relative to Referring_Module.

         Def_Module_Interface : constant Module_Sem_Ptr :=
           Static.Interface_Part (Def_Module);
         Type_Area_Offset : constant Offset_Within_Area :=
           Interpreter.Type_Subarea_Start (Ref_Kind) + Offset;
      begin
         if Static.Is_Ancestor
              (Ancestor => Def_Module_Interface,
               Descendant => Referring_Module)
         then
            --  Modules match, return normal "Type_Area" base.
            return (Type_Area, Type_Area_Offset, No_VM_Obj_Id);
         else
            --  Modules don't match, walk up region chain looking for match.
            declare
               Type_Base : Area_Base_Indicator := Enclosing_Type_Areas'First;
               Enc_Region : Symbols.Region_Ptr :=
                 Referring_Module.Nested_Region.Enclosing_Region;
               use Symbols;
               use type Source_Positions.Line_Number;
            begin
               while Enc_Region /= null loop
                  if Enc_Region.Associated_Symbol /= null
                    and then Enc_Region.Associated_Symbol.Kind =
                             Module_Sym_Kind
                  then
                     declare
                        Enc_Module_Interface : constant Module_Sem_Ptr :=
                          Static.Interface_Part
                             (Module_Sem_Ptr (
                          Enc_Region.Associated_Symbol.Sem_Info));
                     begin
                        Type_Base := Type_Base + 1;
                        if Static.Is_Ancestor
                             (Ancestor => Enc_Module_Interface,
                              Descendant => Def_Module_Interface)
                        then
                           --  Found enclosing module that matches
                           --  Def_Module.  Return appropriate up-level
                           --  Type_Area.
                           return (Type_Base, Type_Area_Offset, No_VM_Obj_Id);
                        end if;
                     end;
                  end if;
                  Enc_Region := Enc_Region.Enclosing_Region;
               end loop;
               Sem_Error
                 ("Internal: Defining module " &
                  Sym_Name (Def_Module.Associated_Symbol) &
                  " not enclosing referring module " &
                  Sym_Name (Referring_Module.Associated_Symbol),
                  Source_Pos);
               return (Type_Area, Type_Area_Offset, No_VM_Obj_Id);
            end;
         end if;
      end Module_Relative_Type_Loc;

   begin  --  Run_Time_Type_Info

      if Polymorphic_Param_Index > 0 then
         --  Type comes from type-id of parameter
         return
           (Param_Area,
            Offset_Within_Area (Polymorphic_Param_Index) - 1, No_VM_Obj_Id);
      elsif Polymorphic_Param_Index < 0 then
         --  Type comes from type-id of parameter, which is passed by ref.
         return
           (Enclosing_Param_Areas'First + 1, --  Indicates extra level of indir
            Offset_Within_Area (-Polymorphic_Param_Index) - 1, No_VM_Obj_Id);
      elsif Obj_Type = null then
         if Debug_Code_Gen then
            Put_Line (" No enclosing type area in Run_Time_Type_Info");
         end if;
         return Result;
      elsif Is_Polymorphic_Type_Id
        or else Obj_Type.Is_Polymorphic
        or else (Formal_Type /= null and then Formal_Type.Is_Polymorphic)
      then
         --  Create a type descriptor representing a polymorphic "wrapper"
         declare
            function Get_Non_Poly_Formal return Type_Sem_Ptr is
            --  Return non-polymorphic version of Formal_Type
            begin
               if Formal_Type /= null
                 and then Formal_Type.Is_Polymorphic
               then
                  return Formal_Type.Root_Type;
               else
                  return Formal_Type;
               end if;
            end Get_Non_Poly_Formal;

            Non_Poly_Formal : constant Type_Sem_Ptr := Get_Non_Poly_Formal;

            Root_Type_Info : constant Object_Locator :=
              Run_Time_Type_Info
                 (Obj_Type.Root_Type,
                  Referring_Module,
                  Non_Poly_Formal,
                  Source_Pos);

            use Interpreter;
         begin
            if Root_Type_Info = Null_Object_Locator then
               --  Some prior error
               return Null_Object_Locator;
            elsif Root_Type_Info.Base /= Zero_Base then
               --  Convert base type into polymorphic type locator
               return (Root_Type_Info.Base, Root_Type_Info.Offset +
                 Corresponding_Polymorphic_Type_Offsets'First, No_VM_Obj_Id);
            else
               --  Get (absolute) locator of polymorphic type
               return
                 Build_Polymorphic_Type_Desc
                    (Obj_Type.Root_Type,
                     Non_Poly_Formal,
                     Source_Pos,
                     Known_Type_Desc (Root_Type_Info)).Location;
            end if;
         end;
      elsif Obj_Type.Root_Type /= Obj_Type then
         --  Get the run-time type info for the root type
         return Run_Time_Type_Info
                  (Obj_Type.Root_Type,
                   Referring_Module,
                   Formal_Type,
                   Source_Pos);
      end if;

      Defining_Module := Obj_Type.Outermost_Module_Where_Used;
      --  Module for which Nested_Type_Index and Actual_Of_Formal_Index
      --  are meaningful.

      --  Get saved Locator
      Result := Obj_Type.Type_Descriptor_Location;

      if Is_Null_Obj_Locator (Result)
        or else (not Obj_Type.All_Parameters_Known
                and then (Defining_Module /= Referring_Module
                            or else Result.Base = Zero_Base))
        or else (For_Val_Param_Or_Const and then Obj_Type.Is_Formal_Type)
      then
         --  Locate run-time type info for this base type.
         --  Recompute as appropriate, e.g. if nested module might be involved
         --  and type is not fully defined.

         if not Obj_Type.All_Parameters_Known then
            --  OK, we need to fetch info from enclosing type descriptor.
            if Defining_Module = null or else Referring_Module = null then
               Sem_Error
                 (Obj_Type.Definition,
                  "Parameters to module instantiation " &
                  "not all compile-time known");
               return Null_Object_Locator;
            end if;

            declare
               pragma Assert (Defining_Module /= null);
               pragma Assert (Referring_Module /= null);

               Num_Actuals_Of_Formals : constant Natural :=
                 Natural (Num_Elements (Defining_Module.Actuals_Of_Formals));
               use type Type_Sem_Vectors.Elem_Index;
            begin
               if Obj_Type.Actual_Of_Formal_Index > 0 then
                  if Natural (Obj_Type.Actual_Of_Formal_Index) not in
                       1 .. Num_Actuals_Of_Formals
                  then
                     Sem_Error
                       (Obj_Type.Definition,
                        "Internal: Actual_Of_Formals_Index for " &
                        Type_Image (Obj_Type) &
                        " =" &
                        Natural'Image
                           (Natural (Obj_Type.Actual_Of_Formal_Index)) &
                        "Enclosing module only has" &
                        Natural'Image (Num_Actuals_Of_Formals));
                     return Null_Object_Locator;
                  end if;

                  Result :=
                     Module_Relative_Type_Loc
                       (Defining_Module,
                        Actual_Of_Formal_Reference,
                        Offset_Within_Area (Obj_Type.Actual_Of_Formal_Index));
               elsif Obj_Type.Is_Formal_Type
                 and then not For_Val_Param_Or_Const
               then
                  --  Assign offset in type area based on formal type
                  --  position.
                  declare
                     Assoc_Module : constant Module_Sem_Ptr :=
                       Static.Module_With_Formal_Type (Obj_Type);
                  begin
                     if Static.Formal_Type_Index (Obj_Type) not in
                       1 .. Static.Num_Module_Parameters (Assoc_Module)
                     then
                        Sem_Error (Obj_Type.Definition,
                          "Internal: Formal_Type_Index not " &
                            "properly initialized");
                     else

                        Result :=
                           Module_Relative_Type_Loc
                             (Assoc_Module,
                              Formal_Parameter_Reference,
                              Offset_Within_Area (Static.Formal_Type_Index
                                                     (Obj_Type)));
                     end if;
                  end;
               elsif Obj_Type.Associated_Module.Cur_Inst_Sem = Obj_Type
                 and then Region_Encloses_Region
                   (Encloser => Obj_Type.Associated_Module.Nested_Region,
                    Enclosed => Referring_Module.Nested_Region)
               then
                  --  We have the current instance of some enclosing module,
                  --  set location to the enclosing type area itself.
                  Result :=
                     Module_Relative_Type_Loc
                       (Obj_Type.Associated_Module,
                        Self_Reference,
                        Offset => 0);
               --  TBD: This is a bit confusing, since we don't really
               --      want the type stored at (Type_Area, 0),
               --      we want the Type_Area itself.
               --      We use a "0" offset to indicate that.
               elsif Obj_Type.Func_Type_Op_Sem /= null
                 and then not Obj_Type.Is_Formal_Type
               then
                  --  This is a func type.
                  --  Just return a "magic" locator
                  Result := Unknown_Func_Type_Obj_Locator;
               else
                  --  This is a nested type that depends somehow
                  --  on enclosing module parameters.
                  --  Compute its location in the type area.
                  pragma Assert
                    (Obj_Type.Nested_Type_Index in
                    1 .. Num_Elements (Defining_Module.Nested_Types));

                  Result :=
                     Module_Relative_Type_Loc
                       (Defining_Module,
                        Nested_Type_Reference,
                        Offset_Within_Area (Obj_Type.Nested_Type_Index));
               end if;

               if Result /= Null_Object_Locator then
                  if Debug_Code_Gen then
                     Put_Line
                       (" Using location " &
                        Interpreter.Obj_Locator_Image (Result) &
                        " for " &
                        Type_Image (Obj_Type));
                  end if;
               else
                  Sem_Error
                    (Obj_Type.Definition,
                     "NYI: cannot determine how to get type descriptor");
               end if;

               if Defining_Module = Referring_Module
                 and then not For_Val_Param_Or_Const
               then
                  --  Remember computed result, unless Defining_Module
                  --  and Referring_Module don't match, or
                  --  we are dealing with the special case of
                  --  a val-param or a nested obj.
                  Obj_Type.Type_Descriptor_Location := Result;
               end if;
            end;
         else
            --  We have a fully defined type, create (or find) type descriptor
            Build_Or_Find_Type_Descriptor (Obj_Type);

            Result := Obj_Type.Type_Descriptor_Location;
         end if;  --  whether is fully defined type

      end if;  --  whether we already have a type descriptor

      if Debug_Code_Gen and then Is_Null_Obj_Locator (Result) then
         Put_Line (" *** Returning Null_Obj_Loc from Run_Time_Type_Info ***");
      end if;

      if Formal_Type /= null then
         --  Check whether we need to create an op map;
         --  also verify assignability.
         declare
            Op_Map_Index : constant Type_Sem_Vectors.Elem_Index :=
              Static.Op_Map_Type_Desc_Index
                (Obj_Type,
                 Formal_Type => Formal_Type,
                 Source_Pos => Source_Pos);
         begin
            if Op_Map_Index = 0 then
               --  Some prior error
               null;
            elsif not Obj_Type.All_Parameters_Known then
               --  We cannot create op-map type descriptor right away
               --  since Obj_Type isn't fully defined.
               --  Find the nested-type index for the op-map and return that.

               --  Create a nested-type reference using op-map's index
               pragma Assert
                 (Op_Map_Index in
                 1 .. Num_Elements (Defining_Module.Nested_Types));

               Result :=
                  Module_Relative_Type_Loc
                    (Defining_Module,
                     Nested_Type_Reference,
                     Offset_Within_Area (Op_Map_Index));
            else
               --  Return reference to op-map type descriptor
               Result := (Zero_Base, Offset_Within_Area (Op_Map_Index),
                          No_VM_Obj_Id);
            end if;

         end;

      end if;  --  If we have a Formal_Type

      return Result;
   end Run_Time_Type_Info;

   procedure Finish_All_Type_Descriptors is
      --  Finish remaining not-completely-finished type descriptors.
      use type Interpreter.Type_Descriptor_Ptr;

      Num_Type_Desc_Errors : Natural := Sem_Error_Count;
   begin

      while First_Unfinished_Type /= null loop
         --  Zero out the global count so other operations
         --  do not look like they failed.
         Sem_Error_Count := 0;

         declare
            Type_Desc : constant Interpreter.Type_Descriptor_Ptr :=
              First_Unfinished_Type;
         begin
            Finish_Type_Descriptor (Type_Desc);
         exception
            when E : others =>
               Sem_Error
                 (Type_Desc.Type_Sem.Definition,
                  "Internal: " &
                  Ada.Exceptions.Exception_Name (E) &
                  " raised in");
         end;

         if Sem_Error_Count > 0 then
            --  Keep track of full error count
            Num_Type_Desc_Errors := Num_Type_Desc_Errors + Sem_Error_Count;
         end if;
      end loop;

      --  Restore the full Sem_Error_Count
      Sem_Error_Count := Num_Type_Desc_Errors;

   end Finish_All_Type_Descriptors;

   function Target_Obj_Is_Null (Target : Interpreter.Object_Locator)
     return Boolean is
   --  Return True if Target is a "null" locator
      use Interpreter;
   begin
      return Target.Base = Zero_Base;
   end Target_Obj_Is_Null;

   function Target_Obj_Is_Null (Visitor : Code_Gen_Visitor) return Boolean is
   --  Return True if Visitor.Target_Object is a "null" locator
      use Interpreter;
   begin
      return Visitor.Target_Object.Base = Zero_Base;
   end Target_Obj_Is_Null;

   function Indir_VM_Obj_Id (VM_Info : Interpreter.VM_Obj_Id_Type)
     return Interpreter.VM_Obj_Id_Type is
      --  Just add a level of indirection to an existing VM_Obj_Id
      use Interpreter;
      pragma Assert (VM_Info.Indir = 0);
      pragma Assert (VM_Info.Num /= 0);
      Result : VM_Obj_Id_Type := VM_Info;
   begin
      Result.Indir := 1;
      Result.Is_Var := True;
      if Result.Kind = Local_Kind then
         Result.First_Call_Param_Num := 0;
      end if;
      return Result;
   end Indir_VM_Obj_Id;

   function Param_VM_Obj_Id (Call_VM_Info : Interpreter.VM_Obj_Id_Type;
                             Param_Offset : Natural)
     return Interpreter.VM_Obj_Id_Type is
      --  Return VM_Info for param at given offset,
      --  given VM_Info for the call as a whole.
      use Interpreter;
      pragma Assert (Param_Offset = 0
                     or else Call_VM_Info.First_Call_Param_Num > 0);
                        --  We alway allow calling with Param_Offset = 0,
                        --  even if there were no parameters.
   begin
      return VM_Obj_Id_Type'
        (Kind => Local_Kind,
         Is_Var => False,
         Level => Call_VM_Info.Level,
         Indir => 0,
         Num => Call_VM_Info.First_Call_Param_Num +
                  VM_Obj_Unique_Num (Param_Offset),
         First_Call_Param_Num => 0);
   end Param_VM_Obj_Id;

   function Result_VM_Obj_Id (Call_VM_Info : Interpreter.VM_Obj_Id_Type)
     return Interpreter.VM_Obj_Id_Type is
      --  Return VM_Info for result of call
      --  given VM_Info for the call as a whole.
      use Interpreter;
      pragma Assert (Call_VM_Info.First_Call_Param_Num > 0);
   begin
      return VM_Obj_Id_Type'
        (Kind => Local_Kind,
         Is_Var => Call_VM_Info.Is_Var,
         Level => Call_VM_Info.Level,
         Indir => Call_VM_Info.Indir,
         Num => Call_VM_Info.Num,
         First_Call_Param_Num => 0);
   end Result_VM_Obj_Id;

   procedure Init_Outputs_To_Null
     (Visitor : in out Code_Gen_Visitor;
      Outputs : Lists.List;
      Resolved_Type : Type_Sem_Ptr;
      Call_Sem : Call_Sem_Ptr;
      Output_Inited_Null : out Boolean) is
      --  Initialize outputs of call to appropriate nulls.
      --  Output_Inited_Null is True if at least one output is inited.
      --  TBD: Really need a type for each output!
      --  Visitor.Target_VM_Info.First_Call_Param_Num is first VM
      --  reg to be initialized.
      Num_Outputs : constant Natural := Lists.Length (Outputs);
      use Interpreter;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      Output_VM_Info : VM_Obj_Id_Type :=
        Param_VM_Obj_Id (Visitor.Target_VM_Info, Param_Offset => 0);
         --  Start at first param offset
   begin
      --  Default initialize OUT param.
      if Call_Sem.Op_Sem /= null then
         declare
            Op_Routine : Routine_RW_Ptr := Call_Sem.Op_Sem.Routine;
         begin
            if Op_Routine = null then
               --  Check for routine on body region
               Op_Routine := Body_Routine (Call_Sem.Op_Sem);

               if Op_Routine = null then
                  --  Check for inherited/renamed operation
                  Op_Routine := Find_Operation_Routine (Call_Sem.Op_Sem);
               end if;
            end if;
            if Op_Routine /= null then
               Output_Inited_Null :=
                 Output_Needs_Init (Op_Routine.Conv_Desc);
            else
               --  Sem_Warning ("Cannot find routine in Init_Outputs_To_Null",
               --    Find_Source_Pos (Call_Sem.Definition));
               Output_Inited_Null := False;
            end if;
         end;
      else
         --  NOTE: It is OK to skip providing an init for the output
         --        if we know the type is small, since the only
         --        use for this is "make-copy-in-region" (if non-optional)
         --        and that ignores the init for the output if it is small.
         Output_Inited_Null := False;
      end if;

      for I in 1 .. Num_Outputs loop
         --  get output type and see whether is
         --  "large".
         declare
            Nth_Output_Param : Param_Decl.Tree renames
              Param_Decl.Tree
                (Tree_Ptr_Of (Lists.Nth_Element (Outputs, I)).all);
            Nth_Output_Sem : constant Param_Sem_Ptr :=
              Param_Sem_Ptr (Nth_Output_Param.Sem_Info);
            Nth_Is_By_Ref  : constant Boolean := Static.Sym_Is_By_Ref
                                           (Nth_Output_Sem.Associated_Symbol);
            Nth_Output_Is_Optional : constant Boolean :=
                Nth_Output_Param.Is_Optional
              or else
                Nth_Output_Sem.Resolved_Type.Value_Is_Optional;
         begin
            if I = 1 and then not Nth_Is_By_Ref
                and then
              (Output_Inited_Null
                or else not Static.Known_To_Be_Small (Resolved_Type)
                or else Nth_Output_Is_Optional)
            then
               --  We might need to generate a "large" null
               Output_Inited_Null := True;  -- Output is inited to null

               if not Target_Obj_Is_Null (Visitor)
                 and then not Static.Known_To_Be_Small (Resolved_Type)
               then
                  --  Use region of target
                  if Visitor.Target_Object.Base /= Local_Area
                    or else Visitor.Target_Object.Offset /=
                              Visitor.Target_Local_Offset
                  then
                     --  Only need to do this if target object
                     --  is at a different offset.
                     --  TBD: What about initializing VM reg in this case?
                     Emit
                       (Visitor,
                        (Op => Store_Null_Of_Same_Stg_Rgn_Op,
                         Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                         Destination => (Local_Area,
                                         Visitor.Target_Local_Offset,
                                         Output_VM_Info),
                         Dest_Name => Visitor.Dest_Name,
                         Source => Visitor.Target_Object,
                         Might_Be_Null => True,
                         Type_Info => Run_Time_Type_Info
                                        (Resolved_Type,
                                         Referring_Module => Enc_Module)));
                  elsif Output_VM_Info /= Visitor.Target_Object.VM_Obj_Id then
                     --  Need to copy target object to proper VM register.
                     --  This is a no-op in the interpreter.
                     Emit
                       (Visitor,
                        (Op => Copy_Word_Op,
                         Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                         Destination => (Local_Area,
                                         Visitor.Target_Local_Offset,
                                         Output_VM_Info),
                         Dest_Name => Visitor.Dest_Name,
                         Source => Visitor.Target_Object,
                         Might_Be_Null => True));
                  end if;
               else
                  --  Use local region
                  Emit
                    (Visitor,
                     (Op => Store_Local_Null_Op,
                      Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                      Destination => (Local_Area, Visitor.Target_Local_Offset,
                                      Output_VM_Info),
                      Dest_Name => Visitor.Dest_Name,
                      Null_Type_Info => Run_Time_Type_Info
                                          (Resolved_Type,
                                           Referring_Module => Enc_Module)));
               end if;
            end if;

            --  Advance to next output
            Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
            Output_VM_Info.Num := Output_VM_Info.Num + 1;
         end;
      end loop;
   end Init_Outputs_To_Null;

   function Adjust_For_Level_And_Prefix
     (Current_Level : Static_Level;
      Obj_Location : Interpreter.Object_Locator;
      Obj_Level : Static_Level;
      Decl_Region : Region_Ptr := null;
      Obj_Ref : Sym_Ref_Ptr := null)
      return Interpreter.Object_Locator
   is
      --  Return object-locator, adjusted if Current_Level > Obj_Level
      --  If Obj_Location is type-area relative, then
      --  check to see region for obj_ref is nearest enclosing module.  If not
      --  adjust for number of nested modules to be skipped over.
      --  If not from an enclosing module, use Prefix_Type to
      --  create a nested constant if necessary.

      pragma Assert (Current_Level >= Obj_Level);
      --  No down-level references allowed!

      use Interpreter;
   begin
      if Current_Level = Obj_Level
        and then Obj_Location.Base /= Type_Area
      then
         --  No adjustment needed
         return Obj_Location;
      else
         --  Adjust by difference in static levels
         declare
            Diff : constant Area_Base_Indicator :=
              Area_Base_Indicator (Current_Level - Obj_Level);
         begin
            case Obj_Location.Base is
               when Zero_Base =>
                  --  Absolute
                  return Obj_Location;  --  No change
               when Local_Area =>
                  --  Relative to current local area
                  return
                    (Enclosing_Local_Areas'First + Diff,
                     Obj_Location.Offset,
                     Obj_Location.VM_Obj_Id);
               when Param_Area =>
                  --  Relative to param area of current operation
                  return
                    (Enclosing_Param_Areas'First + Diff,
                     Obj_Location.Offset,
                     No_VM_Obj_Id);
               when Const_Area =>
                  --  Global constant area
                  return Obj_Location;  --  No change
               when Type_Area | Enclosing_Type_Areas =>
                  --  Relative to enclosing type for current operation
                  declare
                     Obj_Region : Symbols.Region_Ptr;
                     Nearest_Enc_Module : constant Module_Sem_Ptr :=
                       Static.Find_Enclosing_Module_Interface (Decl_Region);
                     Enc_Module : Module_Sem_Ptr := Nearest_Enc_Module;
                     Obj_Module : Module_Sem_Ptr;
                     Num_To_Skip : Natural := 0;
                  begin
                     if Obj_Ref = null then
                        return Obj_Location;  --  No change
                     else
                        Obj_Region :=
                          Obj_Ref.Associated_Symbol.Enclosing_Region;
                        Obj_Module :=
                           Static.Find_Enclosing_Module_Interface
                             (Obj_Region);
                     end if;
                     while Enc_Module /= null
                       and then Enc_Module /= Obj_Module
                     loop
                        --  Try to find the matching module
                        Enc_Module :=
                           Static.Find_Enclosing_Module_Interface
                             (Enc_Module.Nested_Region.Enclosing_Region);
                        Num_To_Skip := Num_To_Skip + 1;
                     end loop;
                     if Obj_Ref /= null
                       and then Obj_Ref.Prefix_Type_Region /= null
                     then
                        --  We have an object with a named type as prefix
                        declare
                           Prefix_Type : constant Type_Sem_Ptr :=
                             Type_Sem_Ptr (Obj_Ref.Prefix_Type_Region);
                        begin
                           if Enc_Module = null
                             or else Prefix_Type.Root_Type /=
                                     Prefix_Type.Associated_Module.
                                Cur_Inst_Sem
                           then
                              --  If we have a constant from
                              --  a named type, we need to create a new local
                              --  copy
                              --  of the constant.
                              declare
                                 Constant_Index : Natural;
                                 pragma Assert  --  TBD: Global Var NYI
                                   (not Static.Sem_Info_Is_For_Variable
                                     (Sem_Ptr (Obj_Ref)));
                              begin
                                 if Debug_Code_Gen then
                                    Put_Line
                                      (" Need to create a copy of constant at "
                                       &
                                       Obj_Locator_Image (Obj_Location) &
                                       " of type " &
                                       Type_Image
                                          (Type_Sem_Ptr (
                                      Obj_Ref.Prefix_Type_Region)));
                                 end if;
                                 if Nearest_Enc_Module /= null then
                                    --  Create copy inside module
                                    Add_Element
                                      (Nearest_Enc_Module.Nested_Objects,
                                       Sem_Ptr (Obj_Ref),
                                       Obj_Sem_Info_Index (Constant_Index));

                                    return
                                      (Type_Area,
                                       Type_Nested_Obj_Offsets'First +
                                       Offset_Within_Area (Constant_Index),
                                       No_VM_Obj_Id);
                                 else
                                    --  Create a global constant
                                    Add_Element
                                      (Compile_Time_Known_Const_Table,
                                       Sem_Ptr (Obj_Ref),
                                       Obj_Sem_Info_Index (Constant_Index));

                                    return
                                      (Const_Area,
                                       Offset_Within_Area (Constant_Index),
                                       No_VM_Obj_Id);
                                 end if;
                              end;
                           end if;
                        end;
                     end if;

                     if Enc_Module = null then
                        Sem_Error
                          (Obj_Module.Definition,
                           "Internal: Cannot find enclosing module");
                     end if;
                     if Num_To_Skip > 0 then
                        --  Adjust for number of modules to skip
                        return
                          (Enclosing_Type_Areas'First +
                           Area_Base_Indicator (Num_To_Skip),
                           Obj_Location.Offset,
                           No_VM_Obj_Id);
                     else
                        return Obj_Location;  --  No change
                     end if;
                  end;
               when Base_Registers =>
                  --  Relative to some temp base register
                  --  TBD: Create an ability to make an up-level
                  --      reference to a "ref"
                  pragma Assert (False);
                  return Obj_Location;
               when Phys_Base_Registers =>
                  --  Relative to some temp phys-addr base register
                  --  TBD: Create an ability to make an up-level
                  --      reference to a "ref"
                  pragma Assert (False);
                  return Obj_Location;
               when Enclosing_Param_Areas =>
                  --  Relative to a param area of an enclosing operation
                  return (Obj_Location.Base + Diff, Obj_Location.Offset,
                          No_VM_Obj_Id);
               when Enclosing_Local_Areas =>
                  --  Relative to a local area of an enclosing block or
                  --  operation
                  return (Obj_Location.Base + Diff, Obj_Location.Offset,
                          Obj_Location.VM_Obj_Id);
               when others =>
                  pragma Assert (False);
                  return Obj_Location;
            end case;
         end;
      end if;
   end Adjust_For_Level_And_Prefix;

   function Adjust_For_Level_With_Optional_Temp
     (Current_Visitor : access Code_Gen_Visitor;
      Obj_Location : Interpreter.Object_Locator;
      Obj_Level : Static_Level;
      Obj_Ref : Sym_Ref_Ptr := null;
      Src_Pos : Source_Positions.Source_Position)
      return Interpreter.Object_Locator
   is
      --  Return object-locator, adjusted if
      --  Current_Visitor.Current_Level > Obj_Level.
      --  If Obj_Location involves a level of indirection through a local
      --  and levels don't agree, create a temp to hold up-level
      --  pointer, and bump Current_Visitor.Target_Local_Offset.
      --  Similarly, if Obj_Location is type-relative, but prefix
      --  identifies a type which is not an enclosing type, use
      --  either Store_Type_Related_Obj_Op into a temp if we want its
      --  content, and return locator for temp, or use Store_Address
      --  if we want its address and return indirection through temp
      --  as the result.
      --  Using Store_Type_Related_Obj_Op avoids the level of indirection
      --  for constants; we could use Store_Address for both if we wanted
      --  to simplify things.
      use Interpreter;
      Need_Temp : Boolean := False;
   begin
      case Obj_Location.Base is
      when Any_Base_Register =>
         if Current_Visitor.Current_Level /= Obj_Level then
            --  Need a temp if attempting to use a base register
            --  and levels don't match so not in "current" local area
            --  We need to create a local temp
            declare
               Temp_Base_Reg : constant Offset_Within_Area :=
                 Current_Visitor.Target_Local_Offset;
               Temp_VM_Info : constant VM_Obj_Id_Type :=
                 Assign_VM_Obj_Id (Current_Visitor.all);
               Offset_From_Base : Offset_Within_Area;
               Base : Area_Base_Indicator;
               Source : Object_Locator;
            begin
               --  Compute offset relative to base
               if Obj_Location.Base in Base_Registers then
                  Base := Base_Registers'First;
               else
                  Base := Phys_Base_Registers'First;
               end if;
               Offset_From_Base :=
                 Offset_Within_Area (Obj_Location.Base - Base);
               Source :=
                      Adjust_For_Level_And_Prefix
                        (Current_Visitor.Current_Level,
                         (Local_Area,
                          Offset_From_Base,
                          Assign_VM_Obj_Id (Current_Visitor.all,
                            Target_VM_Num => Obj_Location.VM_Obj_Id.Num)),
                         Obj_Level,
                         Decl_Region => Current_Visitor.Decl_Region,
                         Obj_Ref     => Obj_Ref);

               --  Copy contents of up-level local into temp
               if Obj_Location.Base in Base_Registers then
                  --  Use Copy_Word for copying a Base_Register
                  Emit
                    (Current_Visitor.all,
                     (Copy_Word_Op,
                      Source_Pos => Src_Pos,
                      Destination => (Local_Area, Temp_Base_Reg,
                                      Temp_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Source => Source,
                      Might_Be_Null => True));
               else
                  --  Use Copy_Address for copying a Phys_Base_Register
                  Emit
                    (Current_Visitor.all,
                     (Copy_Address_Op,
                      Source_Pos => Src_Pos,
                      Destination => (Local_Area, Temp_Base_Reg,
                                      Temp_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Source => Source,
                      Might_Be_Null => True));
               end if;

               --  Skip past the temp
               Current_Visitor.Target_Local_Offset :=
                 Current_Visitor.Target_Local_Offset + 1;

               --  Now make the address indirect through the temp
               --  but with the same offset as before.
               return
                 (Base + Area_Base_Indicator (Temp_Base_Reg),
                  Obj_Location.Offset,
                  Assign_VM_Obj_Id (Current_Visitor.all,
                    Offset => Obj_Location.Offset,
                    Target_VM_Num => Temp_VM_Info.Num,
                    Needs_Var => Obj_Location.Offset = 0));
            end;
         end if;
      when Type_Area | Enclosing_Type_Areas =>
         --  Relative to enclosing type for current operation
         if Obj_Ref /= null
           and then Obj_Ref.Prefix_Type_Region /= null
         then
            --  We have an object with a named type as prefix
            declare
               Nearest_Enc_Module : constant Module_Sem_Ptr :=
                 Static.Find_Enclosing_Module_Interface
                   (Current_Visitor.Decl_Region);
               Enc_Module  : Module_Sem_Ptr := Nearest_Enc_Module;
               Obj_Module  : Module_Sem_Ptr;
               Prefix_Type : constant Type_Sem_Ptr :=
                 Type_Sem_Ptr (Obj_Ref.Prefix_Type_Region);
            begin
               Obj_Module :=
                  Static.Find_Enclosing_Module_Interface
                    (Obj_Ref.Associated_Symbol.Enclosing_Region);

               while Enc_Module /= null
                 and then Enc_Module /= Obj_Module
               loop
                  --  Try to find the matching module
                  Enc_Module :=
                     Static.Find_Enclosing_Module_Interface
                       (Enc_Module.Nested_Region.Enclosing_Region);
               end loop;

               if Enc_Module = null
                 or else Prefix_Type.Root_Type /=
                         Prefix_Type.Associated_Module.Cur_Inst_Sem
               then
                  --  We have a global from a named type which is not
                  --  an enclosing type, so we need to either create
                  --  a level of indirection if we want its address,
                  --  or create a temp copy using Store_Type_Related_Obj_Op.
                  declare
                     Temp_Offset : constant Offset_Within_Area :=
                       Current_Visitor.Target_Local_Offset;
                     Temp_Obj_VM_Info : constant VM_Obj_Id_Type :=
                       Assign_VM_Obj_Id (Current_Visitor.all);
                     Is_Glob_Var : constant Boolean :=
                       Static.Sem_Info_Is_For_Variable
                         (Sem_Ptr (Obj_Ref));
                     Dest_Type_Info : constant Object_Locator :=
                       Run_Time_Type_Info
                          (Obj_Ref.Resolved_Type,
                           Referring_Module => Nearest_Enc_Module);
                     Source_Type_Info : constant Object_Locator :=
                       Run_Time_Type_Info
                          (Prefix_Type,
                           Referring_Module => Nearest_Enc_Module,
                           For_Val_Param_Or_Const =>
                               Obj_Location.Offset in
                                 Type_Formal_Parameter_Offsets
                             or else
                               Obj_Location.Offset in
                                  Type_Nested_Obj_Offsets);
                  begin
                     if Debug_Code_Gen then
                        Put_Line ("Creating temp for " &
                          Subtree_Image (Obj_Ref.Definition) &
                          " at (Local_Area," &
                          Offset_Within_Area'Image (Temp_Offset) & ')');
                     end if;

                     Emit
                       (Current_Visitor.all,
                        (Declare_Obj_Op,
                         Source_Pos => Src_Pos,
                         Destination => (Local_Area, Temp_Offset,
                                         Temp_Obj_VM_Info),
                         Dest_Name => Strings.Null_U_String_Index,
                         Is_By_Ref =>
                           Is_Glob_Var and Current_Visitor.Is_Lvalue_Context,
                         Is_Var => Is_Glob_Var,
                         Declare_Type_Info => Run_Time_Type_Info
                           (Obj_Ref.Resolved_Type,
                            Referring_Module => Nearest_Enc_Module)));

                     --  Skip past the temp
                     Current_Visitor.Target_Local_Offset :=
                       Current_Visitor.Target_Local_Offset + 1;

                     if Is_Glob_Var and Current_Visitor.Is_Lvalue_Context then
                        --  Is a type-relative global and we want its address.
                        Emit
                          (Current_Visitor.all,
                           (Store_Type_Related_Addr_Op,
                            Source_Pos => Src_Pos,
                            Destination => (Local_Area, Temp_Offset,
                                            Temp_Obj_VM_Info),
                            Dest_Name => Strings.Null_U_String_Index,
                            Type_Info => Dest_Type_Info,
                            Source => Obj_Location,
                            Might_Be_Null =>
                              Obj_Ref.Resolved_Type.Value_Is_Optional,
                            Source_Type_Info => Source_Type_Info));

                        --  Return locator for indirection via the temp
                        return (Phys_Base_Register (Temp_Offset), 0,
                                  Indir_VM_Obj_Id (Temp_Obj_VM_Info));
                     else
                        --  Create a copy of the type-relative global obj
                        Emit
                          (Current_Visitor.all,
                           (Store_Type_Related_Obj_Op,
                            Source_Pos => Src_Pos,
                            Destination => (Local_Area, Temp_Offset,
                                            Temp_Obj_VM_Info),
                            Dest_Name => Strings.Null_U_String_Index,
                            Type_Info => Dest_Type_Info,
                            Source => Obj_Location,
                            Might_Be_Null =>
                              Obj_Ref.Resolved_Type.Value_Is_Optional,
                            Source_Type_Info => Source_Type_Info));

                        --  Return the locator for the temp
                        return (Local_Area, Temp_Offset, Temp_Obj_VM_Info);
                     end if;
                  end;
               end if;
            end;
         end if;

      when others =>
         --  Nothing special needed
         null;
      end case;

      --  Can use the basic routine
      return Adjust_For_Level_And_Prefix
               (Current_Visitor.Current_Level,
                Obj_Location,
                Obj_Level,
                Decl_Region => Current_Visitor.Decl_Region,
                Obj_Ref     => Obj_Ref);

   end Adjust_For_Level_With_Optional_Temp;

   function Module_Is_Known (Assoc_Type : Type_Sem_Ptr)
     return Boolean is
   --  Determine whether operation's module is known statically.

   --  NOTE (8/17/2012): This is to work around a GNAT bug when this is
   --  computed using "or else".  See [L817-033].

   begin
      if Assoc_Type = null then
         --  Enclosing module known statically if no assoc type.
         return True;
      elsif Assoc_Type.All_Parameters_Known then
         --  Enclosing module known statically if type fully resolved.
         return True;
      elsif Assoc_Type.Root_Type =
        Assoc_Type.Associated_Module.Cur_Inst_Sem
      then
         --  Enclosing module known statically if matches cur-inst type.
         return True;
      elsif Assoc_Type.Is_Formal_Type or else Assoc_Type.Is_Polymorphic then
         --  Not known statically.
         return False;
      else
         --  If not a formal type, and not polymorphic, then we know its
         --  module even though we don't know all the module parameters.
         return True;
      end if;
   end Module_Is_Known;

   function Should_Call_Through_Type_Desc
     (Op_Sem : Operation_Sem_Ptr; Assoc_Type : Type_Sem_Ptr;
      Abstract_Allowed : Boolean;
      Use_Type_Relative_Locator : Boolean := False) return Boolean is
   --  Return True if call should be made through type desc
   --  rather than directly to operation, because
      use Interpreter;
   begin
      --  Call through the type descriptor since might be abstract
      --  or inherited or module not known statically.

      --  TBD: We can't safely resolve inherited operations statically
      --       at the moment, because when calling an inherited operation,
      --       we need to switch to the ancestor's type descriptor.
      --       Routine_Locator would need to return a pair of
      --       routine-locator/type-desc-to-use.

      return Assoc_Type /= null
        and then Op_Sem.Context in Any_Interface_Item_Contexts
        and then Op_Sem.Overridden_By = null
        and then Op_Sem.Index > 0
        and then (Abstract_Allowed
          or else Use_Type_Relative_Locator
          or else
            (Op_Sem.Routine = null and then
             Body_Routine (Op_Sem) = null)
          or else Op_Sem.Context in Ancestor_Item_Contexts
          or else not Module_Is_Known (Assoc_Type));
   end Should_Call_Through_Type_Desc;

   function Routine_Locator
     (Op_Sem : Operation_Sem_Ptr;
      Assoc_Type : Type_Sem_Ptr;
      Current_Level : Static_Level := 0;
      Tree_For_Srcpos : Optional_Tree := Null_Optional_Tree;
      Abstract_Allowed : Boolean := False;
      Use_Type_Relative_Locator : Boolean := False)
      return Interpreter.Object_Locator
   is
      --  Return an object locator for the operation identified
      --  by the operation sem ptr.
      --  If Abstract_Allowed is True, then it is OK if the
      --  the identified routine is abstract.  Otherwise, a reference
      --  to an abstract operation should result in a compile-time error.
      --  Use Tree_For_Srcpos if non-null when producing an error.
      --  Current_Level is used if operation is a parameter.
      --  If Use_Type_Relative_Locator is true, then this will
      --  try to return a locator that is relative to the Assoc_Type.
      use Interpreter;

      function Tree_To_Use_For_Srcpos return Optional_Tree is
      --  Return Tree_For_Srcpos if not null, else Op_Sem.Definition
      begin
         if Not_Null (Tree_For_Srcpos) then
            return Tree_For_Srcpos;
         else
            return Op_Sem.Definition;
         end if;
      end Tree_To_Use_For_Srcpos;

   begin  --  Routine_Locator

      if Op_Sem.Is_Abstract and then not Abstract_Allowed
        and then Module_Is_Known (Assoc_Type)
        and then not Operation.Tree
          (Tree_Ptr_Of (Op_Sem.Definition).all).Is_Optional
      then
         --  Statically known to be an abstract non-optional operation
         Sem_Error (Tree_To_Use_For_Srcpos,
           "Call on abstract operation requires polymorphic operand");
         return Null_Object_Locator;
      end if;

      if Should_Call_Through_Type_Desc
        (Op_Sem, Assoc_Type, Abstract_Allowed,
         Use_Type_Relative_Locator => Use_Type_Relative_Locator)
      then
         --  Call through the type descriptor since might be abstract
         --  or inherited or module not known statically.
         if False and then Op_Sem.Equiv_To /= null
           and then Op_Sem.Equiv_To.Index > 0
         then
            --  We have an equivalence to another operation
            --  TBD: What if the Equiv_From_Type is for an unrelated type?
            return
              (Type_Area,
               Type_Operation_Offsets'First +
                 Offset_Within_Area (Op_Sem.Equiv_To.Index), No_VM_Obj_Id);
         elsif Op_Sem.Index = 0 then
            Sem_Error (Tree_To_Use_For_Srcpos,
              "Internal: Operation of abstract or formal type has zero index");
            return Null_Object_Locator;

         elsif Op_Sem.Implicit_Enclosing_Module /= null then
            --  Get routine from enclosing type area
            return
              (Enclosing_Type_Areas'First + 1,
               Type_Operation_Offsets'First +
                 Offset_Within_Area (Op_Sem.Index), No_VM_Obj_Id);
         else
            return
              (Type_Area,
               Type_Operation_Offsets'First +
                 Offset_Within_Area (Op_Sem.Index), No_VM_Obj_Id);
         end if;
      elsif Op_Sem.Routine /= null and then Op_Sem.Routine.Index > 0 then
         --  We know the routine statically
         return (Zero_Base, Offset_Within_Area (Op_Sem.Routine.Index),
                 No_VM_Obj_Id);

      elsif Body_Routine (Op_Sem) /= null
        and then Body_Routine (Op_Sem).Index > 0
      then
         --  We know the body routine statically
         return (Zero_Base, Offset_Within_Area (Body_Routine (Op_Sem).Index),
                 No_VM_Obj_Id);

      elsif Op_Sem.Context = Operation_Input_Context
        or else (Op_Sem.Func_Type_Sem /= null
          and then (Op_Sem.Context in Statement_Contexts
                    or else Op_Sem.Context = Type_Context))
      then
         --  Formal operation of operation;
         --  return locator for operation descriptor.
         declare
            Param_Sem : constant Object_Sem_Ptr :=
              Object_Sem_Ptr (Op_Sem.Associated_Symbol.Sem_Info);
         begin
            return
              Adjust_For_Level_And_Prefix
                (Current_Level => Current_Level,
                 Obj_Location => Param_Sem.Info.Obj_Location,
                 Obj_Level => Param_Sem.Info.Obj_Level);
         end;
      elsif Op_Sem.Context = Module_Formal_Context then
         Sem_Error (Tree_To_Use_For_Srcpos,
           "NYI: Formal operation of module");
         return Null_Object_Locator;

      else
         --  Check for inheritance/renaming
         declare
            Routine_To_Use : constant Routine_RW_Ptr :=
              Find_Operation_Routine (Op_Sem);
         begin
            if Routine_To_Use /= null and then Routine_To_Use.Index > 0 then
               --  Found routine to use
               return (Zero_Base, Offset_Within_Area (Routine_To_Use.Index),
                       No_VM_Obj_Id);
            else
               Sem_Error (Tree_To_Use_For_Srcpos,
                 "Compiled code not available");
               return Null_Object_Locator;
            end if;
         end;
      end if;
   end Routine_Locator;

   procedure Emit_Copy_Obj_Or_Word
     (Visitor : in out Code_Gen_Visitor;
      Destination : Interpreter.Object_Locator;
      Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index;
      Source : Interpreter.Object_Locator;
      Target_Object : Interpreter.Object_Locator;
      Opnd_Sem : Operand_Sem_Ptr;
      Source_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position) is
      --  Emit either Copy_Word or Make_Copy_In_Region
      --  depending on whether Target_Object is specified
      --  and whether type might be large.
      use Interpreter;

      Type_Of_Obj : constant Type_Sem_Ptr := Opnd_Sem.Resolved_Type;

      function Possibly_Poly_Type_To_Use return Type_Sem_Ptr is
      --  Determine what type object is based on whether
      --  we are copying a pre-computed polymorphic constant
      --  or a non-polymorphic object that is about to be wrapped
      --  by a Create_Polymorphic_Obj operation
      begin
         if Opnd_Sem.Target_Polymorphic_Type /= null
           and then not Visitor.Create_Polymorphic_Obj
         then
            --  Target_Polymorphic_Type is set, but Create_Polymorphic_Obj
            --  is False, so we must be copying a pre-computed
            --  polymorphic object.
            return Opnd_Sem.Target_Polymorphic_Type;
         else
            --  The normal case -- just use resolved type of operand.
            return Type_Of_Obj;
         end if;
      end Possibly_Poly_Type_To_Use;

      Possibly_Poly_Type : constant Type_Sem_Ptr := Possibly_Poly_Type_To_Use;

      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      pragma Assert (Destination.Base /= Zero_Base);
   begin
      if not Target_Obj_Is_Null (Target_Object)
        and then not Static.Known_To_Be_Small (Possibly_Poly_Type)
      then
         --  Make deep copy of object based on target.
         Emit
           (Visitor,
            (Make_Copy_In_Stg_Rgn_Op,
             Source_Pos => Source_Pos,
             Destination => Destination,
             Dest_Name => Dest_Name,
             Source => Source,
             Might_Be_Null => Type_Of_Obj.Value_Is_Optional,
             Existing_Obj_In_Stg_Rgn => Target_Object,
             Type_Info => Run_Time_Type_Info
                            (Possibly_Poly_Type,
                             Referring_Module => Enc_Module)));
      else
         --  Just copy the word containing/identifying the value of object
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Source_Pos,
             Destination => Destination,
             Dest_Name => Dest_Name,
             Source => Source,
             Might_Be_Null => Type_Of_Obj.Value_Is_Optional));
      end if;
   end Emit_Copy_Obj_Or_Word;

   procedure Initialize_Lvalue_Location_From_Temp
     (Visitor : in out Code_Gen_Visitor;
      Opnd_Sem : Operand_Sem_Ptr;
      Current_Loc : Interpreter.Object_Locator) is
      --  Set loc of result of computation that is to be passed by ref
      --  TBD: This currently can happen after substitution
      --       of a module parameter into a constant expression
      --       that iterates over the parameter.  Eventually these
      --       should be replaced with a reference to a normal object.
      use Interpreter;
      Obj_Loc_To_Use : Object_Locator := Current_Loc;
      Finalizable_Temp_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor, Needs_Var => True);
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      if Visitor.Finalizable_Temp_Offset = 0 then
         if Current_Loc.Base = Local_Area
           and then Current_Loc.Offset = Visitor.Target_Local_Offset - 1
         then
            Sem_Error (Opnd_Sem.Definition,
              "Internal: No space allocated for finalizable temp for " &
              Subtree_Image (Opnd_Sem.Definition));
         elsif True or else Debug_Code_Gen then
            Put_Line ("No space allocated for finalizable temps for " &
              Subtree_Image (Opnd_Sem.Definition) & ", current_loc = " &
              Obj_Locator_Image (Current_Loc) & ", target_local_offset = " &
              Offset_Within_Area'Image (Visitor.Target_Local_Offset));
         end if;
      else
         --  Copy result into finalizable temp so we can point
         --  to it from Target_Local_Offset.
         if Debug_Code_Gen then
            Put_Line ("Allocate finalizable temp at " &
              Obj_Locator_Image
                ((Local_Area, Visitor.Finalizable_Temp_Offset,
                  Finalizable_Temp_VM_Info)) &
              " for " & Subtree_Image (Opnd_Sem.Definition));
         end if;

         Emit
           (Visitor,
            (Declare_Obj_Op,
             Source_Pos => Find_Source_Pos (Opnd_Sem.Definition),
             Destination =>
               Adjust_For_Level_And_Prefix
                 (Visitor.Current_Level,
                  Obj_Location =>
                     (Local_Area, Visitor.Finalizable_Temp_Offset,
                      Finalizable_Temp_VM_Info),
                  Obj_Level => Visitor.Finalizable_Temp_Level),
             Dest_Name => Strings.Null_U_String_Index,
             Is_By_Ref => False,
             Is_Var => False,
             Declare_Type_Info => Run_Time_Type_Info
                                 (Opnd_Sem.Resolved_Type,
                                  Referring_Module => Enc_Module)));

         --  Copy result into finalizable temp
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Opnd_Sem.Definition),
             Destination =>
               Adjust_For_Level_And_Prefix
                 (Visitor.Current_Level,
                  Obj_Location =>
                     (Local_Area, Visitor.Finalizable_Temp_Offset,
                      Finalizable_Temp_VM_Info),
                  Obj_Level => Visitor.Finalizable_Temp_Level),
             Dest_Name => Strings.Null_U_String_Index,
             Source => Current_Loc,
             Might_Be_Null => False));

         --  Now create an object-locator that can be passed
         --  as an address.
         Obj_Loc_To_Use := Adjust_For_Level_With_Optional_Temp
           (Visitor'Access,
            (Local_Area, Visitor.Finalizable_Temp_Offset,
             Finalizable_Temp_VM_Info),
            Visitor.Finalizable_Temp_Level,
            Src_Pos => Find_Source_Pos (Opnd_Sem.Definition));

         --  Skip over this finalizable temp
         Visitor.Finalizable_Temp_Offset :=
           Visitor.Finalizable_Temp_Offset + 1;
      end if;
      Visitor.Lvalue_Location := Obj_Loc_To_Use;
   end Initialize_Lvalue_Location_From_Temp;

   procedure Emit_Call
     (Visitor : in out Code_Gen_Visitor;
      Call_Sem : Call_Sem_Ptr;
      Operands : Lists.List;
      Suppress_Derefs : Boolean := False);
   --  See comments below on body

   procedure Assign_Initial_Nested_Block_Info
     (To   : in out Interpreter.Code_Block_Descriptor;
      From : Interpreter.Code_Block_Descriptor) is
   --  Assign Uses_Queuing, Start_Callee_Locals, and Local_Area_Length
   begin
      To.Uses_Queuing := From.Uses_Queuing;
      To.Start_Callee_Locals := From.Start_Callee_Locals;
      To.Local_Area_Length := From.Local_Area_Length;
      To.Nesting_Level := From.Nesting_Level;
   end Assign_Initial_Nested_Block_Info;

   procedure Emit_Nested_Block_Start
     (Orig_Visitor : in out Code_Gen_Visitor;
      Block_Visitor : in out Code_Gen_Visitor;
      Comp_Sem : Computation_Sem_Ptr;
      Block_Region : Region_Ptr;
      New_Code : Interpreter.Code_Ptr;
      Instr_Opcode : Interpreter.Opcode_Enum := Interpreter.Skip_Op;
      Num_Outputs : Natural := 0;
      Block_Inputs : Interpreter.Obj_Locator_Array :=
        (1 .. 0 => Interpreter.Null_Object_Locator);
      Num_Extra_Invokers : Natural := 0;
      Assertion_Str : Strings.U_String := Strings.Null_U_String) is
      --  Emit start of nested block
      --  Allocate space for TCB and outputs.
      --  Generate code to copy Block_Inputs into param area.
      --  Assign number for new nested block
      --  Remember it in computation-sem info for node
      --  Emit code for start/add_parallel or call/check_nested_block
      --  Record index of instruction for later fixup

      --  Initialize Block_Visitor for use within nested block
      use Interpreter;
      New_Tcb_Offset : constant Offset_Within_Area :=
        Orig_Visitor.Target_Local_Offset;
      New_Block_Index : constant Block_Index :=
        Orig_Visitor.Nested_Blocks.Num_Blocks_Generated + 1;
      Info : Block_Info renames Orig_Visitor.Nested_Blocks.Info (
        New_Block_Index);
      TCB_VM_Info : constant VM_Obj_Id_Type :=
         --  NOTE: Not used if this is a Start_Handled_Op.
        Assign_VM_Obj_Id
          (Orig_Visitor,
           Num_Call_Params => Num_Outputs + Block_Inputs'Length,
           Target_VM_Num => Assign_VM_Obj_Id (Orig_Visitor).Num);

      Input_Param_VM_Info : VM_Obj_Id_Type :=
        Param_VM_Obj_Id (TCB_VM_Info, Num_Outputs);

      function Block_Region_Index_To_Use return Block_Region_Index is
         --  Return Block_Region.Index unless Block_Region is same as
         --  Orig_Visitor.Decl_Region, in which case return
         --  No_Block_Region_Index
         use Symbols;
      begin
         if Block_Region = Orig_Visitor.Decl_Region then
            return No_Block_Region_Index;
         else
            return Block_Region_Index (Block_Region.Index);
         end if;
      end Block_Region_Index_To_Use;

   begin  --  Emit_Nested_Block_Start

      --  Allocate new block index
      Orig_Visitor.Nested_Blocks.Num_Blocks_Generated := New_Block_Index;

      --  Initialize nested block code pointer
      Orig_Visitor.Nested_Blocks.Info (New_Block_Index).Code := New_Code;

      if Instr_Opcode = Start_Parallel_Op then
         --  Allocate space for TCB
         Orig_Visitor.Target_Local_Offset := Orig_Visitor.Target_Local_Offset +
           Interpreter.Thread_Control_Block_Size;
      end if;

      --  Remember location of result
      Comp_Sem.Parallel_Result_Offset := Orig_Visitor.Target_Local_Offset;

      --  Remember where TCB is allocated
      Comp_Sem.TCB_VM_Num := TCB_VM_Info.Num;

      --  Leave room for outputs -- will be copied here
      --  by the nested block after the call is complete.
      Orig_Visitor.Target_Local_Offset := Orig_Visitor.Target_Local_Offset +
                                         Offset_Within_Area (Num_Outputs);

      --  Copy in any inputs
      --  Note that for a concurrent loop, we will have inputs
      --  representing the loop indices, but probably no outputs.
      --  For a [for I in ... => ...] aggregate, we may have outputs too.

      for I in Block_Inputs'Range loop
         --  Copy value of input

         Emit
           (Orig_Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
             Destination => (Local_Area, Orig_Visitor.Target_Local_Offset,
                             Input_Param_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Source => Block_Inputs (I),
             Might_Be_Null => False));  --  TBD

         --  Advance to next slot
         Orig_Visitor.Target_Local_Offset :=
           Orig_Visitor.Target_Local_Offset + 1;
         Input_Param_VM_Info.Num := Input_Param_VM_Info.Num + 1;
      end loop;

      Check_High_Water (Orig_Visitor);

      case Instr_Opcode is
         when Start_Parallel_Op | Start_Handled_Op =>
            if Debug_Code_Gen then
               Put_Line
                 (" Initiate parallel/handled block, Master_Is_Started = " &
                  Boolean'Image (Orig_Visitor.Master_Is_Started) &
                  ", Master_Is_Complete = " &
                  Boolean'Image (Orig_Visitor.Master_Is_Complete) &
                  ", Master_In_Use = " &
                  Boolean'Image (Orig_Visitor.Master_In_Use));
            end if;
            pragma Assert (Orig_Visitor.Local_Master > 0);
            if not Orig_Visitor.Master_Is_Started then
               --  First parallel invocation for this master

               --  Initialize First_Awaited_Block
               pragma Assert (Orig_Visitor.First_Awaited_Block = 0);
               Orig_Visitor.First_Awaited_Block := New_Block_Index;

               if Instr_Opcode = Start_Parallel_Op then
                  --  Discriminant has to be static... ;-/
                  Emit
                    (Orig_Visitor,
                     (Start_Parallel_Op,
                      Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
                      Parallel_Master =>
                        (Local_Area, Orig_Visitor.Local_Master, No_VM_Obj_Id),
                      Parallel_Control => (Local_Area, New_Tcb_Offset,
                                           TCB_VM_Info),
                      Parallel_Static_Link => (Local_Area, 0, No_VM_Obj_Id),
                        --  TBD if doubly nested
                      Num_In_Params  => Block_Inputs'Length,
                      Num_Out_Params => Num_Outputs,
                      Parallel_Code_Block => Null_Code_Block_Descriptor));
                        --  will be fixed up
               else
                  Emit
                    (Orig_Visitor,
                     (Start_Handled_Op,
                      Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
                      Parallel_Master =>
                        (Local_Area, Orig_Visitor.Local_Master, No_VM_Obj_Id),
                      Parallel_Control => (Zero_Base, 0, No_VM_Obj_Id),
                      Parallel_Static_Link => (Local_Area, 0, No_VM_Obj_Id),
                        --  TBD if doubly nested
                      Num_In_Params  => Block_Inputs'Length,
                        --  NOTE: There is actually one parameter provided
                        --        when the handler is executed, namely
                        --        the current exception occurrence.
                      Num_Out_Params => Num_Outputs,
                      Parallel_Code_Block => Null_Code_Block_Descriptor));
                        --  will be fixed up
               end if;
            else
               --  Second or later parallel invocation for this master
               Emit
                 (Orig_Visitor,
                  (Add_Parallel_Op,
                   Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
                   Parallel_Master => (Local_Area, Orig_Visitor.Local_Master,
                                       No_VM_Obj_Id),
                   Parallel_Control => (Local_Area, New_Tcb_Offset,
                                        TCB_VM_Info),
                   Parallel_Static_Link => (Local_Area, 0, No_VM_Obj_Id),
                     --  TBD if doubly nested
                   Num_In_Params  => Block_Inputs'Length,
                   Num_Out_Params => Num_Outputs,
                   Parallel_Code_Block => Null_Code_Block_Descriptor));
                     --  will be fixed up
            end if;

            --  We have definitely started the master
            Orig_Visitor.Master_Is_Started := True;
            Orig_Visitor.Master_Is_Complete := False;

            --  Update the Last_Awaited_Block
            Orig_Visitor.Last_Awaited_Block := New_Block_Index;

         when Check_Nested_Block_Op =>
            if Debug_Code_Gen then
               Put_Line (" Initiate nested block for Check_Nested_Block_Op");
            end if;
            Emit
              (Orig_Visitor,
               (Check_Nested_Block_Op,
                Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
                Params => (Local_Area, Comp_Sem.Parallel_Result_Offset,
                           TCB_VM_Info),
                Static_Link => (Local_Area, 0, No_VM_Obj_Id),
            --  TBD if doubly nested
                Code_Block => Null_Code_Block_Descriptor,   --  will be
                                                            --  fixed up
                Assertion_Str => Strings.Index (Assertion_Str),
                Assertion_Proved => False));

         when Skip_Op =>
            --  This is used to indicate we are generating a block of code
            --  which is not invoked directly, but rather as a side effect
            --  of some other operation (e.g. a precondition, a dequeue
            --  condition, a type constraint).
            --  The Block_Visitor.Current_Block provides the index into
            --  the Nested_Blocks array.
            if Debug_Code_Gen then
               Put_Line (" Create nested block without invokers");
            end if;

         --  TBD: when Call_Nested_Block_Op =>
         when others =>
            --  TBD: Handle other instructions
            pragma Assert (False);
            null;
      end case;

      --  Initialize list of invokers
      if Debug_Code_Gen then
         Put_Line
           (" Creating block" &
            Block_Index'Image (New_Block_Index) &
            " with" &
            Natural'Image (Num_Extra_Invokers + 1) &
            " invokers; 0th invoker in block" &
            Block_Index'Image (Orig_Visitor.Current_Block) &
            " at PC" &
            Code_Offset'Image (Orig_Visitor.Num_Instrs));
      end if;

      if Instr_Opcode /= Skip_Op then
         --  One or more invokers
         Info.Invokers := new Instr_Loc_Array (0 .. Num_Extra_Invokers);
         Info.Invokers (0).Block := Orig_Visitor.Current_Block;
         Info.Invokers (0).Instr := Orig_Visitor.Num_Instrs;
      end if;

      --  Initialize the Block_Visitor
      Block_Visitor :=
        (Trees.Visitor.RW_Tree_Visitor with
         Decl_Region => Block_Region,
         Num_Instrs => 0,
         Last_Instr_Escapes => False,
         Op_Sem => Orig_Visitor.Op_Sem,
         New_Routine => Orig_Visitor.New_Routine,
         Is_Lvalue_Context => False,
         Lvalue_Location => Null_Object_Locator,
         Target_Object => Null_Object_Locator, --  Filled in below
         Target_Local_Offset => Local_Area_Local_Data_Offset,
         Target_VM_Info => No_VM_Obj_Id,  --  Caller must fill this in
         Dest_Name => Strings.Null_U_String_Index,
         Create_Polymorphic_Obj => False,
         Start_Callee_Locals => Local_Area_Local_Data_Offset,
         Local_Master => 0,
         Master_In_Use => False,
         Master_Is_Started => False,
         Master_Is_Complete => False,
         First_Awaited_Block => 0,
         Last_Awaited_Block => 0,
         Is_Leftmost => False,
         Gen_Parallel_Invocations_Only => False,
         Enclosing_For_Loop => null,
         Finalizable_Temp_Level => 0,
         Finalizable_Temp_Offset => 0,
         Nested_Blocks => Orig_Visitor.Nested_Blocks,
         Current_Code => New_Code,
         Current_Block => New_Block_Index,
         Current_Level => Orig_Visitor.Current_Level + 1,
         Annotation_Mode => Orig_Visitor.Annotation_Mode);
      --  TBD: might this ever be the same level?
      --      Probably not, even though this block might be
      --      invoked from same level or more nested level
      --      in case of a loop body.

      if Comp_Sem.Slow_Calls in
           Mandatory_Parallel_Call .. Independent_Slow_Calls
      then

         --  Allocate a master for the nested block
         Block_Visitor.Local_Master := Block_Visitor.Target_Local_Offset;
         pragma Assert (Block_Visitor.Local_Master > 0);

         Block_Visitor.Target_Local_Offset :=
           Block_Visitor.Target_Local_Offset + Thread_Master_Size;

         if Debug_Code_Gen then
            Put_Line
              ("Master needed in nested block, allocated at offset" &
               Offset_Within_Area'Image (Block_Visitor.Local_Master));
         end if;

      end if;

      --  Emit a Begin_Nested_Block_Op
      Emit
        (Block_Visitor,
         (Begin_Nested_Block_Op,
          Source_Pos => Find_Source_Pos (Comp_Sem.Definition),
          Nested_Code_Block => Null_Code_Block_Descriptor,
                                                   --  will be fixed up
          Nested_Block_Region => Block_Region_Index_To_Use));

      --  Use same target obj, adjusted to new level, possibly using a temp
      Block_Visitor.Target_Object :=
        Adjust_For_Level_With_Optional_Temp
          (Current_Visitor => Block_Visitor'Access,
           Obj_Location => Orig_Visitor.Target_Object,
           Obj_Level => Orig_Visitor.Current_Level,
           Src_Pos => Find_Source_Pos (Comp_Sem.Definition));

      return;
   end Emit_Nested_Block_Start;

   procedure Emit_Nested_Block_Finish
     (Orig_Visitor : in out Code_Gen_Visitor;
      Block_Visitor : in out Code_Gen_Visitor;
      Uses_Queuing : Boolean) is
      --  Emit statement to exit nested block and then
      --  finish up nested block processing.
      --  Do not cut back stack.
      --  Copy "Code" from Block_Visitor to heap.
      use Interpreter;
      Info : Block_Info renames Orig_Visitor.Nested_Blocks.Info
        (Block_Visitor.Current_Block);
   begin
      --  Finish with an "exit"
      --  TBD: Skip count should be "1" if end has a "with Name => Value"
      --       presuming that first instruction after "wait" skips over the
      --       "end <blah> with Name => Value" assignments.
      --       (this only works for a for-loop; for others, generally
      --       the skip count for an explicit "exit with" will be non-zero
      --       anyway).
      Emit
        (Block_Visitor,
         (Op => Exit_Op,
          Level_Diff => 1,
          Skip_Count => 0,  --  TBD: or "1" to reach "end loop with ..."
          Source_Pos => Source_Positions.Null_Source_Position));

      Check_High_Water (Block_Visitor);

      --  Record size of local area
      Info.Cb.Start_Callee_Locals := Block_Visitor.Start_Callee_Locals;
      Info.Cb.Local_Area_Length := Block_Visitor.Start_Callee_Locals + 10;
      --  TBD

      --  Record queuing use
      Info.Cb.Uses_Queuing := Uses_Queuing;

      --  Set code nesting level
      Info.Cb.Nesting_Level :=
        Code_Nesting_Level'(Block_Visitor.Current_Level - 1);

      --  Set Uses_Stg_Rgn
      Info.Cb.Uses_Stg_Rgn := Block_Visitor.Current_Code.Uses_Stg_Rgn;

      --  Copy code into heap (will be appended to main block
      --  at end of routine).
      Info.Code :=
        new Code_Type'
        (Code_Length => Block_Visitor.Num_Instrs,
         Uses_Stg_Rgn => Block_Visitor.Current_Code.Uses_Stg_Rgn,
         Num_Locals => Block_Visitor.Current_Code.Num_Locals,
         Most_Recent_Var => 0,
         Instrs =>
           Block_Visitor.Current_Code.Instrs (1 .. Block_Visitor.Num_Instrs));

      --  Fixup invocation instructions
      for I in Info.Invokers'Range loop
         if Info.Invokers (I).Block = Orig_Visitor.Current_Block then
            --  This fixup is for instruction of current block
            Assign_Initial_Nested_Block_Info
              (To => Code_Block
                (Orig_Visitor.Current_Code, Info.Invokers (I).Instr).all,
               From => Info.Cb);
         else
            --  This fixup is for instruction of some other block
            declare
               Invoker_Info : Block_Info renames
                 Orig_Visitor.Nested_Blocks.Info (Info.Invokers (I).Block);
            begin
               Assign_Initial_Nested_Block_Info
                 (To => Code_Block
                   (Invoker_Info.Code, Info.Invokers (I).Instr).all,
                  From => Info.Cb);
            end;
         end if;
      end loop;

      if Debug_Code_Gen then
         Put_Line
           ("Done generating code for nested block, len =" &
            Code_Offset'Image (Block_Visitor.Num_Instrs));
      end if;

      Check_High_Water (Orig_Visitor);
   end Emit_Nested_Block_Finish;

   procedure Emit_Nested_Block
     (Orig_Visitor : in out Code_Gen_Visitor;
      Comp_Sem : Computation_Sem_Ptr;
      Block_Region : Region_Ptr;
      Statement_Tree : Optional_Tree;
      Is_Handled_Stmt_Op : Boolean := False) is
      --  Emit invocation of nested block
      --  (used for operands of "||" or "exception").

      --  Allocate space for TCB and return value
      --  Assign number for new nested block
      --  Remember it in computation-sem info for node
      --  Emit code for start/add_parallel
      --  Record index of instruction for later fixup
      --  Inside nested block, evaluate Statement_Tree
      --  Do not cut back stack
      --  Copy "Code" to heap!

      use Interpreter;
      Block_Visitor : Code_Gen_Visitor;
      Max_Length : constant Code_Length_Type := Max_Block_Length;
      Block_Code : Code_Ptr := new Code_Type (Max_Length);
      Opcode_To_Use : constant array (Boolean) of Interpreter.Opcode_Enum :=
        (False => Start_Parallel_Op, True => Start_Handled_Op);
         --  Indexed by Is_Handled_Stmt_Op
   begin
      Emit_Nested_Block_Start
        (Orig_Visitor,
         Block_Visitor,
         Comp_Sem,
         Block_Region,
         Block_Code,
         Instr_Opcode => Opcode_To_Use (Is_Handled_Stmt_Op),
         Num_Outputs => 0);

      --  Now generate code for block
      Emit_Code_And_Finalize_Resolved_Tree (Statement_Tree, Block_Visitor);

      Emit_Nested_Block_Finish
        (Orig_Visitor,
         Block_Visitor,
         Uses_Queuing => Uses_Queuing (Statement_Tree));

      Free_Code (Block_Code);

   end Emit_Nested_Block;

   procedure Emit_Nested_Block_For_Call
     (Orig_Visitor : in out Code_Gen_Visitor;
      Call_Sem : Call_Sem_Ptr;
      Operands : Lists.List) is
      --  Emit invocation of nested block containing a call

      --  Allocate space for TCB and return value
      --  Assign number for new nested block
      --  Remember it in computation-sem info for node
      --  Emit code for start/add_parallel
      --  Record index of instruction for later fixup
      --  Inside nested block, evaluate parameters and do call.
      --  Do not cut back stack
      --  Copy "Code" to heap!

      use Interpreter;
      Op_Sem : constant Operation_Sem_Ptr := Call_Sem.Op_Sem;
      Target_Operation : Operation.Tree renames Operation.Tree (Tree_Ptr_Of
                                                                   (
        Op_Sem.Definition).all);
      Block_Visitor : Code_Gen_Visitor;
      Max_Length : constant Code_Length_Type := Max_Block_Length;
      Block_Code : Code_Ptr := new Code_Type (Max_Length);
      Num_Outputs : constant Natural :=
        Lists.Length (Target_Operation.Operation_Outputs);
      Orig_Target_Object : constant Object_Locator :=
        Orig_Visitor.Target_Object;
   begin
      if Target_Obj_Is_Null (Orig_Visitor) then
         --  No target object.  If there are outputs,
         --  we need to establish a target object
         --  so any large results get allocated in the
         --  proper region.
         if Num_Outputs > 0 then  --  TBD: and at least one is large
            --  Store a large null for the current region
            --  and use that as the target object
            Orig_Visitor.Target_Object :=
              (Local_Area,
               Orig_Visitor.Target_Local_Offset,
               Assign_VM_Obj_Id (Orig_Visitor));
                  --  Assign a VM reg for this target object
            Emit
              (Orig_Visitor,
               (Store_Large_Local_Null_Op,
                Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                Destination => Orig_Visitor.Target_Object,
                Dest_Name => Strings.Null_U_String_Index,
                Local_Addr => (Local_Area, 0, No_VM_Obj_Id)));

            --  Now protect the target
            Orig_Visitor.Target_Local_Offset :=
              Orig_Visitor.Target_Local_Offset + 1;
         end if;
      elsif Orig_Visitor.Target_Object.Base = Local_Area
        and then Orig_Visitor.Target_Object.Offset =
                   Orig_Visitor.Target_Local_Offset
      then
         --  Skip over target object
         Orig_Visitor.Target_Local_Offset :=
           Orig_Visitor.Target_Local_Offset + 1;
      end if;

      --  Allocate TCB, emit start/add_parallel_op, initialize Block_Visitor
      Emit_Nested_Block_Start
        (Orig_Visitor,
         Block_Visitor,
         Computation_Sem_Ptr (Call_Sem),
         Block_Region => Orig_Visitor.Decl_Region,
         New_Code => Block_Code,
         Instr_Opcode => Start_Parallel_Op,
         Num_Outputs => Num_Outputs);

      declare
         Output_Location : constant Offset_Within_Area :=
           Block_Visitor.Target_Local_Offset;
         Output_VM_Info : VM_Obj_Id_Type := No_VM_Obj_Id;
      begin
         --  Now generate code for block:
         if Num_Outputs > 0 then
            --  Establish a VM reg target for Call
            Output_VM_Info := Assign_VM_Obj_Id (Block_Visitor);
            Block_Visitor.Target_VM_Info := Output_VM_Info;
         end if;

         --  Emit the call, inside of the nested block
         Emit_Call
           (Block_Visitor,
            Call_Sem,
            Operands,
            Suppress_Derefs => True);
         --  We suppress dereferences of results returned by reference
         --  since another call on Move_Outputs will be performed
         --  after waiting for the parallel block to complete.

         --  Move result(s) of call to output area
         --  (which is at the end of the TCB).
         for I in 1 .. Num_Outputs loop
            Emit
              (Block_Visitor,
               (Copy_Word_Op,
                Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                Destination => (Param_Area, Offset_Within_Area (I - 1),
                                No_VM_Obj_Id),
                Dest_Name => Strings.Null_U_String_Index,
                Source => (Local_Area,
                           Output_Location + Offset_Within_Area (I - 1),
                           Output_VM_Info),
                Might_Be_Null => True));  --  TBD

            if Num_Outputs > 1 then
               --  Advance to next VM reg
               Output_VM_Info.Num := Output_VM_Info.Num + 1;
            end if;
         end loop;
      end;

      --  Emit "return" and finish up the nested block
      Emit_Nested_Block_Finish
        (Orig_Visitor,
         Block_Visitor,
         Uses_Queuing => Call_Sem.Uses_Queuing);

      --  Restore target object in case it was changed
      Orig_Visitor.Target_Object := Orig_Target_Object;

      Free_Code (Block_Code);

   end Emit_Nested_Block_For_Call;

   function Possibly_Leftmost (Visitor : Code_Gen_Visitor) return String is
   --  Return "(leftmost) " if Is_Leftmost is true
   begin
      if Visitor.Is_Leftmost then
         return "(leftmost) ";
      else
         return "";
      end if;
   end Possibly_Leftmost;

   function Param_Map_To_Sem_Info_Array
     (Generic_Param_Map : Param_Mapping_Ptr)
      return Sem_Info_Array
   is
      --  Convert param-map into an array of sem-infos.
      Map : Param_Mapping_Ptr := Generic_Param_Map;
      Num : Natural := 0;
   begin
      --  Count the number of mappings
      while Map /= null loop
         Num := Num + 1;
         Map := Map.Next;
      end loop;

      --  Copy into sem array in reverse order
      declare
         Actuals : Sem_Info_Array (1 .. Num);
      begin
         Map := Generic_Param_Map;
         while Map /= null loop
            Actuals (Num) := Map.To;
            Num := Num - 1;
            Map := Map.Next;
         end loop;
         return Actuals;
      end;
   end Param_Map_To_Sem_Info_Array;

   procedure Emit_Wait_For_Parallel
     (Visitor : in out Code_Gen_Visitor;
      Source_Pos : Source_Positions.Source_Position) is
   --  Emit wait-for-parallel-op and then update the
   --  flags in the Visitor relating to the master,
   --  and the Waiter field in the associated nested blocks.
      use Interpreter;
   begin
      if Debug_Code_Gen then
         Put_Line
           (" Wait for master at " &
            Offset_Within_Area'Image (Visitor.Local_Master));
      end if;

      pragma Assert (Visitor.Local_Master > 0);

      Emit
        (Visitor,
         (Wait_For_Parallel_Op,
          Source_Pos => Source_Pos,
          Parallel_Master => (Local_Area, Visitor.Local_Master, No_VM_Obj_Id),
          Skip_Counts => null));

      if Visitor.First_Awaited_Block > 0 then
         --  For each nested block associated with this "wait",
         --  update the Waiter field to refer to this instruction.
         for I in Visitor.First_Awaited_Block ..
           Visitor.Last_Awaited_Block loop
            Visitor.Nested_Blocks.Info (I).Waiter :=
              (Block => Visitor.Current_Block,
               Instr => Visitor.Num_Instrs);
         end loop;

         --  Reinitialize first/last awaited block
         Visitor.First_Awaited_Block := 0;
         Visitor.Last_Awaited_Block := 0;
      end if;

      Visitor.Master_Is_Complete := True;
      Visitor.Master_Is_Started := False;
   end Emit_Wait_For_Parallel;

   procedure Emit_Call_Operands
     (Visitor : in out Code_Gen_Visitor;
      Call_Sem : Call_Sem_Ptr;
      Op_Inputs : Lists.List;  --  List of input parameter decls
      Op_Outputs : Lists.List;  --  List of output parameter decls
      Operands : Lists.List;    --  List of actual operands
      Output_Inited_Null : out Boolean) is
      --  Leave room for outputs
      --  Initialize "large" outputs to (large) null.
      --  Evaluate each of the inputs
      --  Pass down target VM reg nums to each operand.
      use Interpreter;
      Orig_Dest_Name : constant Strings.U_String_Index := Visitor.Dest_Name;

      Input_VM_Info : VM_Obj_Id_Type :=
        Param_VM_Obj_Id (Visitor.Target_VM_Info, Lists.Length (Op_Outputs));
         --  Get starting VM reg num
   begin
      if Call_Sem.Finalizable_Temp_Info /= null then
         if Visitor.Finalizable_Temp_Offset = 0 then
            if Debug_Code_Gen then
               Put_Line ("No space allocated for finalizable temps for " &
                 Subtree_Image (Call_Sem.Definition));
            end if;
         else
            Call_Sem.Finalizable_Temp_Info.Obj_Location :=
              (Local_Area, Visitor.Finalizable_Temp_Offset,
               Assign_VM_Obj_Id (Visitor, Needs_Var => True));
            Call_Sem.Finalizable_Temp_Info.Obj_Level :=
              Visitor.Finalizable_Temp_Level;

            Visitor.Finalizable_Temp_Offset :=
              Visitor.Finalizable_Temp_Offset + 1;

            declare
               Enc_Module : constant Module_Sem_Ptr :=
                 Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
            begin
               Emit
                 (Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                   Destination => Call_Sem.Finalizable_Temp_Info.Obj_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Is_By_Ref => False,
                   Is_Var => False,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (Call_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));
            end;

            if Debug_Code_Gen then
               Put_Line ("Allocate finalizable temp at " &
                 Obj_Locator_Image
                   (Call_Sem.Finalizable_Temp_Info.Obj_Location) & " for " &
                   Subtree_Image (Call_Sem.Definition));
            end if;
         end if;
      end if;
      --  Leave room for outputs
      --  Initialize "large" outputs to (large) null.
      --  TBD: Not obvious what we should do for "ref" outputs.
      --      Probably OK to leave uninitialized, but would be
      --      safer to initialize to some kind of null address.
      Init_Outputs_To_Null
        (Visitor,
         Op_Outputs,
         Call_Sem.Resolved_Type,
         Call_Sem,
         Output_Inited_Null);

      --  Remove target object, and dest_name, if any, from Visitor
      Visitor.Target_Object := Null_Object_Locator;
      Visitor.Dest_Name := Strings.Null_U_String_Index;

      --  Evaluate each of the inputs
      for I in 1 .. Lists.Length (Operands) loop
         declare
            Actual_Operand : constant Optional_Tree :=
              Lists.Nth_Element (Operands, I);

            Is_Implicit_Module_Param : Boolean := False;
            --  Skip evaluation if param is passed via enclosing type

            Formal_Param_Tree : Trees.Tree'Class renames
              Tree_Ptr_Of (Lists.Nth_Element (Op_Inputs, I)).all;

            Might_Be_Null : Boolean := True;
         begin
            if Debug_Code_Gen then
               Put_Line
                 ("Generate code for " &
                  Possibly_Leftmost (Visitor) &
                  "actual param = " &
                  Subtree_Image (Actual_Operand));
            end if;

            if Call_Sem.Op_Sem /= null
              and then Formal_Param_Tree in Param_Decl.Tree
            then
               --  A "real" call; decide if is "ref" param
               --  TBD: We are not doing any legality checks on
               --       parameters that are operations, which
               --       might involve checking "global" annotations, etc.
               declare
                  Formal_Param : Param_Decl.Tree
                    renames Param_Decl.Tree (Formal_Param_Tree);

                  Actual_Sem : Operand_Sem_Ptr :=
                    Operand_Sem_Ptr (Sem_Info
                                        (Resolved_Tree (Actual_Operand)));

                  Underlying_Actual_Sem : constant Sem_Ptr :=
                    Underlying_Sem_Info (Sem_Ptr (Actual_Sem));

                  Formal_Sem : constant Param_Sem_Ptr :=
                    Param_Sem_Ptr (Formal_Param.Sem_Info);

                  use type Param_Decl.Param_Kind;
                  use type Param_Decl.Param_Locking;
                  Actual_Is_Unlocked_Concurrent : Boolean;  --  Init'ed below
                  Formal_Is_Unlocked_Concurrent : constant Boolean :=
                    Static.Is_Unlocked_Concurrent_Operand
                       (Operand_Sem_Ptr (Formal_Sem));
                  Actual_Is_Null_Literal : Boolean;  --  Init'ed below
                  use type Param_Decl.Param_Locking;
               begin
                  if Underlying_Actual_Sem /= null
                    and then Underlying_Actual_Sem.all in
                      Operand_Semantic_Info'Class
                  then
                     --  Get underlying info if is still an operand-sem ptr
                     --  (underlying info will actually be operation-sem ptr if
                     --  actual parameter is of a func-type).
                     Actual_Sem := Operand_Sem_Ptr (Underlying_Actual_Sem);
                  end if;

                  Actual_Is_Unlocked_Concurrent :=
                    Static.Is_Unlocked_Concurrent_Operand (Actual_Sem);

                  Actual_Is_Null_Literal :=
                    Actual_Sem.all in Literal_Semantic_Info
                      and then Literal_Sem_Ptr (Actual_Sem).Lit_Kind =
                        Null_Literal;

                  --  Use lvalue context if "ref" parameter
                  Visitor.Is_Lvalue_Context :=
                     Static.Param_Is_Passed_By_Ref
                       (Formal_Sem,
                        Formal_Param.Kind,
                        Formal_Param.Locking);

                  if Visitor.Is_Lvalue_Context
                    and then Actual_Sem.Target_Polymorphic_Type /= null
                  then
                     --  We can't easily create a polymorphic wrapper for
                     --  a by-ref parameter so give up now.
                     --  TBD: Support this someday by implicitly allocating
                     --       temp in pre-codegen phase.
                     Sem_Error
                       (Actual_Operand,
                        "Must be of polymorphic type because passed by ref");
                  end if;

                  --  Indicate whether parameter is locked
                  if Formal_Is_Unlocked_Concurrent then
                     --  The formal is an unlocked concurrent operand.
                     --  The actual must be as well.  No going from locked
                     --  to unlocked.
                     if Debug_Code_Gen then
                        Put_Line (" Formal " &
                          Subtree_Image (Formal_Param_Tree) &
                          " is unlocked in call: " &
                          Subtree_Image (Call_Sem.Definition));
                     end if;
                     if not Actual_Is_Unlocked_Concurrent then
                        Sem_Error
                          (Actual_Operand,
                           "Must not be already locked since formal '" &
                           Sym_Name (Formal_Sem.Associated_Symbol) &
                           "' is unlocked");
                     end if;
                  elsif Actual_Is_Unlocked_Concurrent then
                     --  Actual is unlocked, and formal is locked
                     --  ==> We need to get a lock as part of the call.

                     --  That is, this parameter should be locked because the
                     --  actual is unlocked-concurrent and the
                     --  formal is *not* unlocked-concurrent (i.e. the formal
                     --  is locked/queued or is not concurrent at all).

                     if Debug_Code_Gen then
                        Put_Line (" Actual " &
                          Subtree_Image (Actual_Operand) &
                          " is unlocked in call: " &
                          Subtree_Image (Call_Sem.Definition));
                     end if;

                     if U_Base_Type_Region (Actual_Sem.Resolved_Type) /=
                        Call_Sem.Assoc_Type_Region
                       and then
                         (Actual_Sem.Resolved_Type.Enclosing_Type = null
                          or else U_Base_Type_Region
                            (Actual_Sem.Resolved_Type.Enclosing_Type) /=
                              Call_Sem.Assoc_Type_Region)
                       and then
                         (Call_Sem.Op_Sem.Implicit_Enclosing_Module = null)
                     then
                        --  An "affiliated" operand is one that is of the
                        --  same type as that associated with the call.
                        --  Must be "affiliated" since the "internal"
                        --  preconditions only work as dequeue conditions
                        --  for affiliated operands.
                        Sem_Error
                          (Actual_Operand,
                           "Locking operation must be in module " &
                           "defining type of concurrent object; " &
                           "call for " &
                           Type_Image
                             (Type_Sem_Ptr (Call_Sem.Assoc_Type_Region)) &
                           "; param type is " &
                           Type_Image (Actual_Sem.Resolved_Type));
                     elsif Call_Sem.Locked_Param_Info.Param_Index > 0 then
                        --  TBD: For now, we only allow one locked parameter
                        --      per call.
                        Sem_Error
                          (Actual_Operand,
                           "NYI: Multiple locked or queued " &
                           "parameters in a single call");
                     else
                        if Debug_Code_Gen then
                           Put_Line ("  Getting lock on actual #" &
                             Symbols.Sym_Index'Image
                               (Formal_Sem.Associated_Symbol.Index));
                        end if;

                        --  Remember which parameter is locked/queued
                        Call_Sem.Locked_Param_Info :=
                          (Param_Index    =>
                             Natural (Formal_Sem.Associated_Symbol.Index),
                           Is_Var         =>
                             Static.Sem_Info_Is_For_Variable
                               (Sem_Ptr (Formal_Sem)),
                           Is_By_Ref      =>
                             Static.Param_Is_Passed_By_Ref
                               (Formal_Sem,
                                Formal_Param.Kind,
                                Formal_Param.Locking),
                           Is_Queued_Call =>
                             Formal_Param.Locking =
                               Param_Decl.Queued_Param);
                     end if;
                  end if;

                  if Static.Sem_Info_Is_For_Variable
                       (Sem_Ptr (Formal_Sem))
                  then
                     --  Check whether actual is a variable
                     if not Static.Sem_Info_Is_For_Variable
                              (Sem_Ptr (Actual_Sem))
                     then
                        Sem_Error
                          (Actual_Operand,
                           "Must be a variable because formal " &
                           Sym_Name (Formal_Sem.Associated_Symbol) &
                           " is a variable");
                        --  Prevent further errors
                        Visitor.Is_Lvalue_Context := False;
                     end if;
                  end if;

                  if Actual_Is_Null_Literal
                    and then not Formal_Sem.Resolved_Type.Value_Is_Optional
                    and then Languages.Language not in Languages.Ada_Ish
                  then
                     --  Passing "null" to a non-optional formal;
                     --  Give special warning if using comparison.
                     --  Doesn't really apply to Ada, since you are allowed
                     --  to pass null to equality tests for types that
                     --  have a "null" value.
                     if Call_Sem /= null and then Call_Sem.Op_Sem /= null
                       and then Call_Sem.Op_Sem.Associated_Symbol /= null
                       and then Call_Sem.Op_sem.Associated_Symbol.Str =
                         Static.Compare_Op_Str
                     then
                        Sem_Error
                          ("Use ""X not null"" or ""X is null"" to test " &
                             "whether a value is null",
                           Src_Pos => Find_Source_Pos (Actual_Operand));
                     else
                        --  Give general warning
                        Sem_Warning
                          ("Null value not allowed for non-optional formal " &
                             Sym_Name (Formal_Sem.Associated_Symbol),
                           Src_Pos => Find_Source_Pos (Actual_Operand));
                     end if;
                  end if;

                  Is_Implicit_Module_Param :=
                    Formal_Sem.Is_Implicit_Module_Param;

                  if Call_Sem.Polymorphic_Param_Index /= 0
                    and then abs (Call_Sem.Polymorphic_Param_Index) /=
                             Positive (Formal_Sem.Associated_Symbol.Index)
                    and then Actual_Sem.Resolved_Type.Is_Polymorphic
                    and then not Formal_Sem.Resolved_Type.Is_Polymorphic
                  then
                     --  We have an actual parameter that is polymorphic
                     --  and a formal which isn't, but which is not the
                     --  "controlling" operand.  Check that its type-id
                     --  matches that of the controlling operand.
                     --  TBD: We will do check at run-time for now;
                     --       eventually create check at compile time.
                     --  NOTE: Need to check for *non-polymorphic* actuals
                     --       matching cur-inst-type formals, as run-time
                     --       will attempt to unwrap cur-inst actuals.
                     if Debug_Code_Gen then
                        Put_Line (Source_Positions.Image
                          (Find_Source_Pos (Call_Sem.Definition)) &
                          ": Multiple polymorphic " &
                          """controlling"" operands: " &
                          Subtree_Image (Actual_Sem.Definition));
                     end if;
                  end if;

                  Might_Be_Null := Actual_Sem.Resolved_Type.Value_Is_Optional;
               end;
            end if;

            if Is_Implicit_Module_Param then
               --  A parameter to enclosing implicit module
               if Debug_Code_Gen then
                  Put_Line
                    (" [code gen supressed because Is_Implicit_Module_Param]");
               end if;
            else
               if not Visitor.Is_Lvalue_Context then
                  --  Set up target VM reg num
                  Visitor.Target_VM_Info := Input_VM_Info;
               end if;

               --  Actually evaluate operand
               Emit_Code_For_Resolved_Tree (Actual_Operand, Visitor);

               if Visitor.Is_Lvalue_Context then
                  --  Store address unless already there
                  if Visitor.Target_Local_Offset >
                     Max_Offset_For_Base_Register
                    or else Visitor.Lvalue_Location.Base /=
                      Phys_Base_Register (Visitor.Target_Local_Offset - 1)
                    or else Visitor.Lvalue_Location.Offset /= 0
                    or else Visitor.Lvalue_Location.VM_Obj_Id /=
                      Indir_VM_Obj_Id (Input_VM_Info)
                  then
                     --  Not where we want it yet.
                     Emit
                       (Visitor,
                        (Store_Address_Op,
                         Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                         Destination => (Local_Area,
                                         Visitor.Target_Local_Offset - 1,
                                         Input_VM_Info),
                         Dest_Name => Strings.Null_U_String_Index,
                         Source => Visitor.Lvalue_Location,
                         Might_Be_Null => Might_Be_Null));
                  end if;
               end if;

               --  Move to next VM reg
               Input_VM_Info.Num := Input_VM_Info.Num + 1;
            end if;

            Visitor.Is_Lvalue_Context := False;

            --  Remove target object, if any, from Visitor
            Visitor.Target_Object := Null_Object_Locator;
         end;
      end loop;

      --  Restore Dest_Name in Visitor
      Visitor.Dest_Name := Orig_Dest_Name;
   end Emit_Call_Operands;

   function Static_Link_Type_Region
     (Op_Sem : Operation_Sem_Ptr;
      Assoc_Type_Region : Type_Region_Ptr;
      Polymorphic_Param_Index : Integer := 0;
      Use_Type_Relative_Locator : Boolean := False)
     return Type_Region_Ptr is
   --  Return Assoc_Type_Region unless it corresponds to a "new" type,
   --  in which case we want to return the enclosing type of the
   --  ancestor from which the operation was inherited.
      Originating_Type : constant Type_Sem_Ptr :=
        Type_Sem_Ptr (Assoc_Type_Region);
      use type Interpreter.Operation_Index;
   begin
      if Assoc_Type_Region = null
        or else Op_Sem = null
      then
         --  Incomplete information, so not much to do.
         return Assoc_Type_Region;
      elsif Op_Sem.Equiv_To /= null
        and then Op_Sem.Equiv_From_Type /= null
        and then not Should_Call_Through_Type_Desc
          (Op_Sem, Originating_Type,
           Abstract_Allowed => Polymorphic_Param_Index /= 0,
           Use_Type_Relative_Locator => Use_Type_Relative_Locator)
      then
         --  Use Equiv_From_Type
         return U_Base_Type_Region (Static.Substitute_Actuals
           (Op_Sem.Equiv_From_Type, Assoc_Type_Region));
      elsif Op_Sem.Originating_Module = null then
         --  Not enough information to choose different associated type
         return Assoc_Type_Region;
      else
         --  If a "new" type, then find the Enclosing_Type of the ancestor
         --  that is declared in the same region as Op_Sem.
         --  If not a "new" type, then find the ancestor whose module is where
         --  Op_Sem is declared.
         declare
            Anc : Type_Sem_Ptr := Originating_Type;
         begin
            while Anc /= null loop
               if Anc.New_Type_Counter /= Anonymous_Type_Indicator then
                  --  This is a "new" type; check region of associated symbol
                  if Anc.U_Base_Type.Associated_Symbol /= null
                    and then
                     Anc.U_Base_Type.Associated_Symbol.Enclosing_Region =
                       Op_Sem.Originating_Module.Nested_Region
                  then
                     --  Found desired ancestor, now return its enclosing type
                     return U_Base_Type_Region (Anc.Enclosing_Type);
                  end if;
               elsif Op_Sem.Index > 0 then
                  --  Use original type if calling through type descriptor
                  return Assoc_Type_Region;
               elsif Anc.Associated_Module = Op_Sem.Originating_Module then
                  --  Not a "new" type; check match for module.
                  --  Found desired ancestor, now return ancestor
                  return U_Base_Type_Region (Anc);
               end if;

               --  Keep looking
               Anc := Anc.Parent_Type;
            end loop;
         end;
      end if;

      --  Fall back
      if Debug_Code_Gen then
         Put_Line
           (" Static_Link_Type_Region cannot find region of ancestor of " &
              Type_Image (Originating_Type));
      end if;
      return Assoc_Type_Region;
   end Static_Link_Type_Region;

   function Static_Link_For_Call
     (Op_Sem : Operation_Sem_Ptr;
      Current_Level : Static_Level;
      Assoc_Type_Region : Type_Region_Ptr;
      Referring_Module : Module_Sem_Ptr;
      Polymorphic_Param_Index : Integer := 0;
      Use_Type_Relative_Locator : Boolean := False)
     return Interpreter.Object_Locator is
   --  Return object locator for static link to use in a
   --  Call_Op, Parallel_Call_Op, or Store_Operation_Desc_Op
      use Interpreter;
   begin
      if Assoc_Type_Region /= null then
         --  Compute static link as locator for associated type
         return Run_Time_Type_Info
           (Type_Sem_Ptr (Static_Link_Type_Region (Op_Sem, Assoc_Type_Region,
             Polymorphic_Param_Index,
             Use_Type_Relative_Locator)),
            Referring_Module => Referring_Module,
            Polymorphic_Param_Index =>
              Polymorphic_Param_Index);
      elsif Op_Sem.Context = Operation_Input_Context
        or else (Op_Sem.Func_Type_Sem /= null
          and then (Op_Sem.Context in Statement_Contexts
                    or else Op_Sem.Context = Type_Context))
      then
         --  Call through an operation descriptor.
         --  Set static link to local area
         if Debug_Code_Gen then
            Put_Line (" Static link needed in call on operation desc " &
              Subtree_Image (Op_Sem.Definition, Use_Short_Form => True) &
              " set to Local area");
         end if;

         return (Local_Area, 0, No_VM_Obj_Id);
      else
         --  Compute static link based on level of nesting
         declare
            Static_Link : constant Object_Locator :=
               Adjust_For_Level_And_Prefix
                 (Current_Level => Current_Level,
                  Obj_Location => (Local_Area, 0, No_VM_Obj_Id),
                  Obj_Level => Op_Sem.Level);
         begin
            if Debug_Code_Gen then
               Put_Line (" Static link needed in call on " &
                 Subtree_Image (Op_Sem.Definition, Use_Short_Form => True) &
                 ", current_lev =" & Static_Level'Image
                   (Current_Level) &
                 ", op_sem.level =" & Static_Level'Image
                   (Op_Sem.Level));

               Put_Line (" computed static_link = " &
                 Obj_Locator_Image (Static_Link));
            end if;

            return Static_Link;
         end;
      end if;
   end Static_Link_For_Call;

   procedure Emit_Parallel_Call
     (New_Visitor : in out Code_Gen_Visitor;
      Call_Sem : Call_Sem_Ptr;
      Operands : Lists.List) is
      --  Allocate space for TCB then evaluate parameters as "normal"
      --  Emit a start/add-parallel-call
      --  Do not cut back stack
      use Interpreter;
      Op_Sem : constant Operation_Sem_Ptr := Call_Sem.Op_Sem;
      Target_Operation : Operation.Tree renames
        Operation.Tree (Tree_Ptr_Of (Op_Sem.Definition).all);
      New_Tcb_Offset : Offset_Within_Area;
      Num_Inputs : constant Natural :=
        Lists.Length (Target_Operation.Operation_Inputs);
      Num_Outputs : constant Natural :=
        Lists.Length (Target_Operation.Operation_Outputs);
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (New_Visitor.Decl_Region);
      Call_Target : Object_Locator := Null_Object_Locator; --  Filled in below.
      Static_Link : constant Object_Locator := Static_Link_For_Call
        (Op_Sem => Op_Sem,
         Current_Level => New_Visitor.Current_Level,
         Assoc_Type_Region => Call_Sem.Assoc_Type_Region,
         Referring_Module => Enc_Module,
         Polymorphic_Param_Index => Call_Sem.Polymorphic_Param_Index);
      Output_Inited_Null : Boolean := False;

      VM_Info_For_Params : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (New_Visitor,
          Num_Call_Params => Num_Inputs + Num_Outputs,
          Target_VM_Num => Assign_VM_Obj_Id (New_Visitor).Num);
   begin
      --  Put the first-param information back into the New_Visitor
      if New_Visitor.Target_VM_Info.Kind = No_VM_Obj_Kind then
         --  Initialize all VM info
         New_Visitor.Target_VM_Info := VM_Info_For_Params;
      else
         --  Merely update the First_Call_Param_Num field
         New_Visitor.Target_VM_Info.First_Call_Param_Num :=
           VM_Info_For_Params.First_Call_Param_Num;
      end if;

      if New_Visitor.Target_Object.Base = Local_Area
        and then New_Visitor.Target_Object.Offset =
                   New_Visitor.Target_Local_Offset
      then
         --  Skip over target object
         New_Visitor.Target_Local_Offset :=
           New_Visitor.Target_Local_Offset + 1;
      end if;

      if Num_Outputs = 0
        and then Call_Sem.Num_Finalizable_Temps > 0
      then
         --  Allocate space for finalizable temps, if any
         Allocate_Finalizable_Temps
           (New_Visitor, Call_Sem.Num_Finalizable_Temps);
      end if;

      --  Allocate space for TCB
      New_Tcb_Offset := New_Visitor.Target_Local_Offset;
      New_Visitor.Target_Local_Offset := New_Visitor.Target_Local_Offset +
                                         Interpreter.Thread_Control_Block_Size;

      --  Remember where params/results of parallel call are computed
      Call_Sem.Parallel_Result_Offset := New_Visitor.Target_Local_Offset;

      --  Remember where TCB is allocated
      Call_Sem.TCB_VM_Num := VM_Info_For_Params.Num;

      --  Evaluate parameters, with gen-parallel-only turned off
      New_Visitor.Gen_Parallel_Invocations_Only := False;

      Emit_Call_Operands
        (New_Visitor,
         Call_Sem,
         Op_Inputs => Target_Operation.Operation_Inputs,
         Op_Outputs => Target_Operation.Operation_Outputs,
         Operands => Operands,
         Output_Inited_Null => Output_Inited_Null);

      --  Restore gen-parallel-only flag
      New_Visitor.Gen_Parallel_Invocations_Only := True;
      if Debug_Code_Gen then
         Put_Line
           (" Initiate parallel call, Master_Is_Started = " &
            Boolean'Image (New_Visitor.Master_Is_Started) &
            ", Master_Is_Complete = " &
            Boolean'Image (New_Visitor.Master_Is_Complete) &
            ", Master_In_Use = " &
            Boolean'Image (New_Visitor.Master_In_Use));
      end if;

      --  Compute Call_Target now that we know whether there is a locked param.
      Call_Target :=
        Routine_Locator
          (Op_Sem,
           Type_Sem_Ptr (Call_Sem.Assoc_Type_Region),
           Current_Level => New_Visitor.Current_Level,
           Tree_For_Srcpos => Call_Sem.Definition,
           Abstract_Allowed =>
             Call_Sem.Polymorphic_Param_Index /= 0,
           Use_Type_Relative_Locator =>
             Call_Sem.Locked_Param_Info.Param_Index > 0);

      pragma Assert
        (New_Visitor.Local_Master >= Local_Area_Local_Data_Offset);

      if not New_Visitor.Master_Is_Started then
         --  First parallel call
         Emit
           (New_Visitor,
            (Start_Parallel_Call_Op,
             Source_Pos => Find_Source_Pos (Call_Sem.Definition),
             Parallel_Master => (Local_Area, New_Visitor.Local_Master,
                                 No_VM_Obj_Id),
             Parallel_Control => (Local_Area, New_Tcb_Offset,
                                  VM_Info_For_Params),
             Parallel_Static_Link => Static_Link,
             Num_In_Params  => Num_Inputs,
             Num_Out_Params => Num_Outputs,
             Parallel_Call_Target => Call_Target,
             Parallel_Target_Index =>
               Find_Operation_Routine_Index (Op_Sem),
             Parallel_Locked_Param_Info => Call_Sem.Locked_Param_Info,
             Parallel_Precond_Proved => False,
             Parallel_Output_Inited_Null => Output_Inited_Null));
      else
         --  Second or later parallel call
         Emit
           (New_Visitor,
            (Add_Parallel_Call_Op,
             Source_Pos => Find_Source_Pos (Call_Sem.Definition),
             Parallel_Master => (Local_Area, New_Visitor.Local_Master,
                                 No_VM_Obj_Id),
             Parallel_Control => (Local_Area, New_Tcb_Offset,
                                  VM_Info_For_Params),
             Parallel_Static_Link => Static_Link,
             Num_In_Params  => Num_Inputs,
             Num_Out_Params => Num_Outputs,
             Parallel_Call_Target => Call_Target,
             Parallel_Target_Index =>
               Find_Operation_Routine_Index (Op_Sem),
             Parallel_Locked_Param_Info => Call_Sem.Locked_Param_Info,
             Parallel_Precond_Proved => False,
             Parallel_Output_Inited_Null => Output_Inited_Null));
      end if;

      --  We have definitely started the master
      New_Visitor.Master_Is_Started := True;
      New_Visitor.Master_Is_Complete := False;
   end Emit_Parallel_Call;

   procedure Emit_Call
     (Visitor : in out Code_Gen_Visitor;
      Call_Sem : Call_Sem_Ptr;
      Operands : Lists.List;
      Suppress_Derefs : Boolean := False) is
      --  Emit a call to the routine identified by Call_Sem.Op_Sem
      --  with the given operands.
      --
      --  If Call_Sem.Op_Sem is null, then this is a "pseudo-call" on
      --  the "||" operator, which simply executes its operands in parallel.
      --
      --  If Suppress_Derefs is True, then don't dereference
      --  the output just because it is returned by-ref, and
      --  don't check the Is_Lvalue_Context flag.
      --
      --  This is sensitive to the three Visitor flags,
      --  Gen_Parallel_Invocations_Only, Master_In_Use, and Is_Leftmost.
      --
      --  Visitor.Target_Local_Offset is bumped by the number of outputs
      --  when actually generating code, or by amount of space needed
      --  for parallel calls when Gen_Parallel_Invocations_Only is True.

      Op_Name : constant String :=
        Subtree_Image
           (Invocation.Tree (Tree_Ptr_Of (Call_Sem.Equiv_Invocation).all).
             Prefix);

      Op_Sem : constant Operation_Sem_Ptr := Call_Sem.Op_Sem;
      Is_Slow_Call : constant Boolean :=
        Call_Is_Slow (Computation_Sem_Ptr (Call_Sem));
      Routine_Sym : Symbols.Sym_Ptr := null;
      Num_Inputs : Natural := 0;
      Num_Outputs : Natural := 0;
      Op_Inputs : Lists.List := Lists.Empty_List;
      Op_Outputs : Lists.List := Lists.Empty_List;
      Output_Is_By_Ref : Boolean := False;
      --  Indicates whether output is passed back by ref

      New_Visitor : Code_Gen_Visitor := Visitor;
      Starting_Target_Offset : constant Interpreter.Offset_Within_Area :=
        Visitor.Target_Local_Offset;
      Use_Parallel_Call : Boolean := False;
      Use_Parallel_Block : Boolean := False;
      Turn_Off_Leftmost : Boolean := False;
      Start_Using_Master : Boolean := False;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

      use Interpreter;

      procedure Move_Outputs
        (Call_Target_Offset : Offset_Within_Area;
         Call_Output_VM_Info : VM_Obj_Id_Type := No_VM_Obj_Id;
         Final_Target_Offset : in out Offset_Within_Area) is
         --  Move outputs to their final location
         --  Bump Final_Target_Offset by Num_Outputs
         --  Call_Output_VM_Info, if provided, is the VM_Obj_Id for the
         --  first output.  If not provided, presumption is that
         --  the output(s) are already in the target VM reg(s).

         use type Param_Decl.Param_Kind;

         First_Output_Target : Offset_Within_Area := Final_Target_Offset;
         Dest_VM_Info : VM_Obj_Id_Type := Visitor.Target_VM_Info;
         Source_VM_Info : VM_Obj_Id_Type := Call_Output_VM_Info;
      begin
         if Visitor.Is_Lvalue_Context then
            --  No target VM info when Is_Lvalue_Context is true.
            Dest_VM_Info := Source_VM_Info;
            pragma Assert (Source_VM_Info.Kind /= No_VM_Obj_Kind);
         elsif Source_VM_Info.Kind = No_VM_Obj_Kind then
            --  No change in VM info as part of move
            Source_VM_Info := Dest_VM_Info;
         elsif Dest_VM_Info.Kind = No_VM_Obj_Kind and then Num_Outputs > 0 then
            --  Apparently result is being discarded.
            Dest_VM_Info := Source_VM_Info;
            pragma Assert (Source_VM_Info.Kind /= No_VM_Obj_Kind);
         end if;

         if Output_Is_By_Ref
           and then not Visitor.Is_Lvalue_Context
           and then not Suppress_Derefs
         then
            --  Do an implicit dereference of the single output,
            --  copying it if necessary.
            pragma Assert (Call_Output_VM_Info.Kind /= No_VM_Obj_Kind);
               --  Must specify Call_Output in this case

            Emit_Copy_Obj_Or_Word
              (New_Visitor,
               Destination => (Local_Area, Final_Target_Offset,
                               Dest_VM_Info),
               Dest_Name => Visitor.Dest_Name,
               Source => (Phys_Base_Register (Call_Target_Offset), 0,
                          Indir_VM_Obj_Id (Source_VM_Info)),
               Target_Object => Visitor.Target_Object,
               Opnd_Sem => Operand_Sem_Ptr (Call_Sem),
               Source_Pos => Find_Source_Pos (Call_Sem.Definition));

            --  Protect the output
            Final_Target_Offset := Final_Target_Offset + 1;
         elsif Call_Target_Offset /= Final_Target_Offset
           or else Source_VM_Info /= Dest_VM_Info
         then
            --  Move result(s) to originally specified target
            for I in 1 .. Num_Outputs loop
               Emit
                 (New_Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                   Destination => (Local_Area, Final_Target_Offset,
                                   Dest_VM_Info),
                   Dest_Name => Visitor.Dest_Name,
                   Source => (Local_Area,
                              Call_Target_Offset +
                                Offset_Within_Area (I - 1),
                              Source_VM_Info),
                   Might_Be_Null => True));  --  TBD
               --  Protect the outputs
               Final_Target_Offset := Final_Target_Offset + 1;

               if Num_Outputs > 1 then
                  --  Advance the VM nums
                  Dest_VM_Info.Num := Dest_VM_Info.Num + 1;

                  case Source_VM_Info.Kind is
                     when Local_Kind =>
                        --  Just bump the source VM number
                        Source_VM_Info.Num := Source_VM_Info.Num + 1;
                     when Component_Kind | Param_Kind =>
                        --  Bump the offest of a component/param
                        Source_VM_Info.Offset := Source_VM_Info.Offset + 1;
                     when others =>
                        pragma Assert (False);
                        null;
                  end case;
               end if;
            end loop;
         else
            --  Just bump Target_Local_Offset to protect outputs
            Final_Target_Offset := Final_Target_Offset +
                                   Offset_Within_Area (Num_Outputs);
         end if;

         if Suppress_Derefs then
            --  Don't worry about lvalues or finalizable temps since this is
            --  the "Move_Outputs" at the end of a nested block.
            --  Another call on Move_Outputs will be performed after
            --  waiting for the parallel call to complete.
            null;
         elsif Visitor.Is_Lvalue_Context then
            if not Output_Is_By_Ref then
               Sem_Error
                 (Call_Sem.Definition,
                  "Function used as lvalue does not return by ref");
            else
               --  Set up Lvalue_Location to deref through
               --  the output.
               Visitor.Lvalue_Location :=
                 (Phys_Base_Register (Final_Target_Offset - 1),
                  0,
                  Indir_VM_Obj_Id (Dest_VM_Info));
            end if;
         elsif Call_Sem.Finalizable_Temp_Info /= null
           and then
             Call_Sem.Finalizable_Temp_Info.Obj_Location.Offset /= 0
         then
            --  Copy the output into the finalizable temp.
            pragma Assert (Num_Outputs = 1);  --  TBD: multiple output support

            Emit
              (New_Visitor,
               (Copy_Word_Op,
                Source_Pos => Find_Source_Pos (Call_Sem.Definition),
                Destination =>
                  Adjust_For_Level_And_Prefix
                    (New_Visitor.Current_Level,
                     Obj_Location =>
                        Call_Sem.Finalizable_Temp_Info.Obj_Location,
                     Obj_Level => Call_Sem.Finalizable_Temp_Info.Obj_Level),
                Dest_Name => Strings.Null_U_String_Index,
                Source => (Local_Area, First_Output_Target,
                           Visitor.Target_VM_Info),
                Might_Be_Null => True)); -- TBD

         end if;
      end Move_Outputs;

   begin  --  Emit_Call

      --  NOTE: If this might be a queued call,
      --       then we turn it into a parallel call.

      if Call_Sem.Generic_Param_Map /= null then
         --  Need to instantiate implicit enclosing module
         --  and replace Assoc_Type_Region with this instantiation
         declare
            Implicit_Instance : constant Type_Sem_Ptr :=
              Static.Instantiate_Module
                 (Mod_Sem => Call_Sem.Op_Sem.Implicit_Enclosing_Module,
                  Actual_Params =>
                     Param_Map_To_Sem_Info_Array (Call_Sem.Generic_Param_Map),
                  Decl_Region => Visitor.Decl_Region,
                  Enclosing_Type => Type_Sem_Ptr (Call_Sem.Assoc_Type_Region),
                  Source_Pos => Find_Source_Pos (Call_Sem.Definition));
         begin
            Implicit_Instance.Generic_Param_Map := Call_Sem.Generic_Param_Map;
            Implicit_Instance.Generic_Param_Region := Visitor.Decl_Region;
            --  Preserve generic_param_map in case mapping has two
            --  types with different module params such as
            --  Q<3+2> => Q<2> which need checking.

            if Debug_Code_Gen then
               Put_Line
                 (" Emit_Call with Generic_Param_Map = " &
                  Param_Map_Image (Call_Sem.Generic_Param_Map));
               Put_Line
                 (" Replacing Assoc_Type_Region of " &
                  Type_Image (Type_Sem_Ptr (Call_Sem.Assoc_Type_Region)) &
                  " with " &
                  Type_Image (Implicit_Instance));
            end if;

            Call_Sem.Assoc_Type_Region :=
               U_Base_Type_Region (Implicit_Instance);

            Call_Sem.Generic_Param_Map := null;
            --  TBD: Need to change Op_Sem.Index??
         end;
      end if;

      --  Don't propagate certain fields from Visitor to New_Visitor
      New_Visitor.Is_Lvalue_Context := False;
      New_Visitor.Lvalue_Location := Null_Object_Locator;

      if Op_Sem /= null then
         --  This is a "real" call as opposed to "||" pseudo-call.

         Routine_Sym := Op_Sem.Associated_Symbol;
         --  pragma Assert (Routine_Sym.Kind = Operation_Sym_Kind);

         --  Get copy of inputs/outputs
         declare
            Target_Operation : Operation.Tree
              renames Operation.Tree
                (Tree_Ptr_Of (Op_Sem.Definition).all);
         begin
            Op_Inputs := Target_Operation.Operation_Inputs;
            Num_Inputs := Lists.Length (Op_Inputs);

            Op_Outputs := Target_Operation.Operation_Outputs;
            Num_Outputs := Lists.Length (Op_Outputs);

            --  Decide whether output is passed back by ref
            Output_Is_By_Ref := Num_Outputs = 1
                               and then Static.Sym_Is_By_Ref
                                           (Underlying_Sem_Info
                                               (Lists.Nth_Element
                                                   (Op_Outputs,
                                                    1)).Associated_Symbol);
         end;
      else
         --  This is a "pseudo" call on "||" operator.
         Num_Inputs := Lists.Length (Operands);
      end if;

      if Debug_Code_Gen then
         if Visitor.Gen_Parallel_Invocations_Only then
            Put_Line ("Generate parallel invocations of call on " & Op_Name);
         else
            Put_Line ("Generate code for call on " & Op_Name);
            Put_Line ("# inputs = " & Integer'Image (Num_Inputs));
            Put_Line ("# outputs = " & Integer'Image (Num_Outputs));
            if Op_Sem /= null and then Op_Sem.Originating_Module /= null then
               Put_Line (" Originating_Module = " &
                 Sym_Name (Op_Sem.Originating_Module.Associated_Symbol));
            end if;
         end if;
      end if;

      if Visitor.Master_In_Use then
         --  We are inside a call with multiple slow calls
         --  If this is a slow call, then we need to do the
         --  call in parallel unless we are the leftmost slow call
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use now True.");
         end if;
         if Is_Slow_Call then
            --  Slow call inside master

            pragma Assert (Call_Sem.Slow_Calls /= Independent_Slow_Calls);
            --  Slow call should never be marked as itself
            --  being multiple independent slow calls.

            if Visitor.Is_Leftmost then
               --  Left-most slow call
               Turn_Off_Leftmost := True;
               --  Turn off left-most after processing operands

               --  Just do it inline unless needs master internally
               case Call_Sem.Slow_Calls is
                  when No_Slow_Calls | Independent_Slow_Calls =>
                     --  Shouldn't happen
                     pragma Assert (False);
                     null;

                  when Slow_Call | Slow_Call_Sequence =>
                     --  Can just proceed
                     null;

                  when Slow_Call_Tree_Needing_Master =>
                     --  Parallel block since has internal parallelism
                     --  which will need its own master.
                     Use_Parallel_Block := True;

                  when Mandatory_Parallel_Call =>
                     --  NOTE: We are required to do this
                     --       using a parallel call (e.g. because it
                     --       is queued).
                     Use_Parallel_Call := True;
               end case;
            else
               --  Not left-most
               --  Either a parallel call or a parallel block
               case Call_Sem.Slow_Calls is
                  when No_Slow_Calls | Independent_Slow_Calls =>
                     --  Shouldn't happen
                     pragma Assert (False);
                     null;
                  when Slow_Call =>
                     --  Parallel call since we have a simple call,
                     --  unless we are avoiding parallel calls to
                     --  enable a faster calling convention.
                     if Avoid_Parallel_Calls then
                        --  Parallel call isn't mandatory, and we are
                        --  trying to avoid parallel calls, so use a parallel
                        --  block instead.
                        Use_Parallel_Block := True;
                     else
                        --  OK to do a parallel call because operands
                        --  are themselves simple.
                        Use_Parallel_Call := True;
                     end if;
                  when Mandatory_Parallel_Call =>
                     --  NOTE: We are required to do this
                     --       using a parallel call (e.g. because it
                     --       is queued).
                     Use_Parallel_Call := True;
                  when Slow_Call_Sequence | Slow_Call_Tree_Needing_Master =>
                     --  We need to use a Parallel block to avoid
                     --  slowing down overall process.
                     Use_Parallel_Block := True;
               end case;
            end if;
         else
            --  Fast call inside master, just pass through
            null;
         end if;
      else
         --  Not inside master
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use now False.");
         end if;
         case Call_Sem.Slow_Calls is
            when Slow_Call_Tree_Needing_Master =>
               if Is_Slow_Call then
                  --  This is a slow call,
                  --  time to use the master
                  Start_Using_Master := True;
               --  NOTE: Use_Parallel_Call is False so this call will be
               --       done on this thread.
               else
                  --  This is a fast call,
                  --  just generate the code; we will use the master later
                  null;
               end if;
            when Mandatory_Parallel_Call =>
               if Is_Slow_Call then
                  --  This is a slow call that requires a parallel call,
                  --  time to use the master, and to make a parallel call.
                  --  Currently this is only true for queued calls,
                  --  or locking calls where we might not know whether
                  --  there is an internal precondition.
                  Start_Using_Master := True;
                  Use_Parallel_Call := True;
               else
                  --  This is a fast call,
                  --  just generate the code; we will use the master later
                  null;
               end if;
            when Independent_Slow_Calls =>
               --  Use master
               Start_Using_Master := True;
            when No_Slow_Calls | Slow_Call | Slow_Call_Sequence =>
               --  Don't need a master, just do it.
               null;
         end case;
      end if;

      if Debug_Code_Gen then
         if Start_Using_Master then
            Put_Line (" Start using master");
         end if;
      end if;

      if Start_Using_Master then
         --  Do two passes:
         --  * In first pass we only walk operands that involve slow calls
         --   generating parallel calls/blocks; location of TCB/result of
         --   call, and nested block num if any, is recorded on
         --   node's computation-sem info; we ignore
         --   leftmost slow call on first pass if it doesn't internally
         --   need a master.  When we invoke a parallel block, we
         --   at that point generate code for the nested block.
         --   We need the leftmost flag to be passed down in the case
         --   the operand has independent slow calls, so it will skip
         --   over the leftmost slow call unless it needs its own master.
         --  * In second pass we do the rest of the computation,
         --   waiting on the parallel threads once we run out of other
         --   things to do.  To simplify things, we emit code to evaluate
         --   operands until we get to a slow call which is done in
         --   parallel, at which point we emit an operation to wait for
         --   them all, before we continue code for evaluating operands.
         --   When we emit a parallel_block instruction, we won't know
         --   the final offset to the nested block, so we record the
         --   index of the instruction (and nested block number) in the
         --   nested-block table to allow later fixup.

         --  We need to mark master as being in use
         --  Do first pass and then second pass
         New_Visitor.Master_In_Use := True;
         New_Visitor.Gen_Parallel_Invocations_Only := True;
         New_Visitor.Is_Leftmost := True;
         if Debug_Code_Gen then
            Put_Line (" New_Visitor.Master_In_Use set to True.");
         end if;

      elsif Visitor.Master_In_Use then
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use is True.");
         end if;
         if Visitor.Gen_Parallel_Invocations_Only then
            --  TBD: Do appropriate thing for parallel call/block
            null;
         else
            --  Wait for master if not already waited for
            --  Retrieve result computed in parallel
            null;  --  TBD
         end if;
      else
         --  Not inside a master, not starting a master
         null;  --  Nothing special to do
      end if;

      if New_Visitor.Gen_Parallel_Invocations_Only then
         --  Recurse looking for parallel call/blocks

         if Use_Parallel_Block then
            --  Allocate space for TCB and return value
            --  Assign number for new nested block
            --  Remember it in computation-sem info for node
            --  Emit code for start/add_parallel
            --  Record index of instruction for later fixup
            --  Inside nested block, evaluate parameters and do call.
            --  Do not cut back stack
            --  Copy "Code" to heap!
            if Use_Parallel_Call then
               Put_Line
                 ("Use_Parallel_Call and Use_Parallel_Block both set!");
            end if;
            Emit_Nested_Block_For_Call (New_Visitor, Call_Sem, Operands);
         elsif Use_Parallel_Call then
            --  Allocate space for TCB then evaluate parameters as "normal"
            --  Emit a start/add-parallel-call
            --  Do not cut back stack
            Emit_Parallel_Call (New_Visitor, Call_Sem, Operands);
         else
            --  Recurse into each of the inputs
            if New_Visitor.Target_Object.Base = Local_Area
              and then New_Visitor.Target_Object.Offset =
                         New_Visitor.Target_Local_Offset
            then
               --  Protect target object
               New_Visitor.Target_Local_Offset :=
                 New_Visitor.Target_Local_Offset + 1;
            end if;

            New_Visitor.Target_Object := Null_Object_Locator;
            New_Visitor.Target_VM_Info := No_VM_Obj_Id;
               --  Don't propagate outer targeting to operands.
               --  Target will be reset later

            for I in 1 .. Lists.Length (Operands) loop
               declare
                  Actual_Operand : Optional_Tree :=
                    Lists.Nth_Element (Operands, I);
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       ("Generate parallel invocations for " &
                        Possibly_Leftmost (New_Visitor) &
                        "actual param = " &
                        Subtree_Image (Actual_Operand));
                  end if;
                  --  Do parallel stuff for each operand
                  Emit_Code_For_Resolved_Tree (Actual_Operand, New_Visitor);
               end;
            end loop;

         end if;

         --  Reset the targeting info
         New_Visitor.Target_Object := Visitor.Target_Object;
         New_Visitor.Target_VM_Info := Visitor.Target_VM_Info;

         Check_High_Water (New_Visitor);

         if Start_Using_Master then
            --  Now do the "normal" code-gen actions
            New_Visitor.Gen_Parallel_Invocations_Only := False;
            New_Visitor.Is_Leftmost := True;
            if Debug_Code_Gen then
               Put_Line (" Done with parallel invocations for " & Op_Name);
            end if;
         else
            --  Copy back Target_Local_Offset to preserve allocations
            --  needed for parallel invocations
            Visitor.Target_Local_Offset := New_Visitor.Target_Local_Offset;
            --  Ditto for Finalizable_Temp_Offset
            Visitor.Finalizable_Temp_Offset :=
              New_Visitor.Finalizable_Temp_Offset;
         end if;
      end if;

      if not New_Visitor.Gen_Parallel_Invocations_Only then
         --  Actually generate code

         if Use_Parallel_Call or Use_Parallel_Block then
            --  Wait for completion and move results where expected
            if Call_Sem.Parallel_Result_Offset = 0 then
               if Debug_Code_Gen then
                  Put_Line (" Parallel call/block not performed!");
               end if;
               Use_Parallel_Call := False;
               Use_Parallel_Block := False;
            else
               --  We actually computed it in parallel
               --  See whether we need to wait for master
               if not New_Visitor.Master_Is_Complete then
                  --  Wait for master
                  Emit_Wait_For_Parallel (New_Visitor,
                    Source_Pos => Find_Source_Pos (Call_Sem.Definition));
               end if;

               if Use_Parallel_Call then
                  --  Finalize non-ref operands (for parallel block, this
                  --  happens inside the nested block itself).
                  Finalize_Non_Ref_Operands (New_Visitor,
                    Call_Sem.Definition);
               end if;

               --  Move result(s)
               Move_Outputs
                 (Call_Sem.Parallel_Result_Offset,
                  Call_Output_VM_Info => VM_Obj_Id_Type'(Component_Kind,
                    Is_Var => False, Level => Visitor.Current_Level,
                    Indir => 0, Num => Call_Sem.TCB_VM_Num,
                    Offset => Interpreter.Thread_Control_Block_Size),
                  Final_Target_Offset => Visitor.Target_Local_Offset);

               --  TBD: Copy back target local offset
               --  Visitor.Target_Local_Offset :=
               --    New_Visitor.Target_Local_Offset;
            end if;
         end if;

         if not Use_Parallel_Call and then not Use_Parallel_Block then
            --  Actually generate the call now

            if New_Visitor.Target_Object.Base = Local_Area
              and then New_Visitor.Target_Object.Offset =
                         New_Visitor.Target_Local_Offset
              and then Output_Is_By_Ref
            then
               --  Skip over target object
               New_Visitor.Target_Local_Offset :=
                 New_Visitor.Target_Local_Offset + 1;
            end if;

            if Num_Outputs = 0
              and then Call_Sem.Num_Finalizable_Temps > 0
            then
               --  Allocate space for finalizable temps
               Allocate_Finalizable_Temps
                 (New_Visitor, Call_Sem.Num_Finalizable_Temps);
            end if;

            declare
               function Assign_Output_VM_Obj_Id return VM_Obj_Id_Type is
                  --  Determine whether we need a separate register
                  --  for the output of the call, based on whether
                  --  there needs to be an implicit deref after the call.
               begin
                  if Num_Outputs = 0 then
                     --  No outputs
                     return No_VM_Obj_Id;
                  elsif Visitor.Is_Lvalue_Context then
                     --  There is no target provided, resulting address
                     --  is to be stored in Visitor.Lvalue_Location
                     return Assign_VM_Obj_Id (New_Visitor);
                  elsif Output_Is_By_Ref
                    and then not Suppress_Derefs
                  then
                     --  There needs to be an implicit deref; alloc a VM reg
                     return Assign_VM_Obj_Id (New_Visitor);
                  elsif Visitor.Target_VM_Info.Kind /= No_VM_Obj_Kind then
                     --  Use VM reg of target
                     return Visitor.Target_VM_Info;
                  else
                     --  Assign a VM register for the (discarded) result
                     return Assign_VM_Obj_Id (New_Visitor);
                  end if;
               end Assign_Output_VM_Obj_Id;

               Call_Output_VM_Info : constant VM_Obj_Id_Type :=
                 Assign_Output_VM_Obj_Id;

               Call_Target_Offset : constant Offset_Within_Area :=
                 New_Visitor.Target_Local_Offset;
                  --  Remember where outputs are going
               Output_Inited_Null : Boolean := False;
               VM_Info_For_Params : constant VM_Obj_Id_Type :=
                 Assign_VM_Obj_Id (New_Visitor,
                   Needs_Var => Call_Output_VM_Info.Is_Var,
                   Num_Call_Params => Num_Inputs + Num_Outputs,
                   Target_VM_Num => Call_Output_VM_Info.Num);
            begin
               --  Put the first-param information into the New_Visitor
               New_Visitor.Target_VM_Info := VM_Info_For_Params;

               --  Leave room for outputs and evaluate inputs
               Emit_Call_Operands
                 (New_Visitor,
                  Call_Sem,
                  Op_Inputs => Op_Inputs,
                  Op_Outputs => Op_Outputs,
                  Operands => Operands,
                  Output_Inited_Null => Output_Inited_Null);

               --  Remember the high-water mark
               Check_High_Water (New_Visitor);

               if Op_Sem /= null then
                  --  This is a "real" call.
                  --  Emit the actual call instruction.
                  declare
                     Call_Target : constant Object_Locator :=
                       Routine_Locator
                         (Op_Sem,
                          Type_Sem_Ptr (Call_Sem.Assoc_Type_Region),
                          Current_Level => New_Visitor.Current_Level,
                          Tree_For_Srcpos => Call_Sem.Definition,
                          Abstract_Allowed =>
                            Call_Sem.Polymorphic_Param_Index /= 0,
                          Use_Type_Relative_Locator =>
                            Call_Sem.Locked_Param_Info.Param_Index > 0);
                     Static_Link : constant Object_Locator :=
                       Static_Link_For_Call
                         (Op_Sem => Op_Sem,
                          Current_Level => New_Visitor.Current_Level,
                          Assoc_Type_Region => Call_Sem.Assoc_Type_Region,
                          Referring_Module => Enc_Module,
                          Polymorphic_Param_Index =>
                            Call_Sem.Polymorphic_Param_Index);
                     Target_Index : constant Routine_Index :=
                       Find_Operation_Routine_Index (Op_Sem);
                  begin
                     if Call_Sem.Assoc_Type_Region = null
                       and then Routine_Sym.Enclosing_Region.
                                  Associated_Symbol /= null
                       and then Routine_Sym.Enclosing_Region.Associated_Symbol.
                          Kind = Module_Sym_Kind
                       and then Call_Sem.Op_Sem.Func_Type_Sem = null
                     then
                        --  Enclosed by module, so should have associated type
                        Sem_Error
                          (Call_Sem.Definition,
                           "Internal:" &
                           " Missing type region on call of module operation");
                     end if;

                     if Target_Index = 0 then
                        --  Must be an indirect call
                        Emit
                          (New_Visitor,
                           (Indirect_Call_Op,
                            Source_Pos =>
                              Find_Source_Pos (Call_Sem.Definition),
                            Call_Target => Call_Target,
                            Indirect_Num_In_Params =>
                              Lists.Length (Op_Inputs),
                            Indirect_Num_Out_Params =>
                              Lists.Length (Op_Outputs),
                            Locked_Param_Info => Call_Sem.Locked_Param_Info,
                            Params => (Local_Area, Call_Target_Offset,
                                       VM_Info_For_Params),
                            Static_Link => Static_Link,
                            Precond_Proved => False,
                            Output_Inited_Null => Output_Inited_Null));
                     else
                        --  Normal call
                        Emit
                          (New_Visitor,
                           (Call_Op,
                            Source_Pos =>
                              Find_Source_Pos (Call_Sem.Definition),
                            Call_Target => Call_Target,
                            Target_Index => Target_Index,
                            Locked_Param_Info => Call_Sem.Locked_Param_Info,
                            Params => (Local_Area, Call_Target_Offset,
                                       VM_Info_For_Params),
                            Static_Link => Static_Link,
                            Precond_Proved => False,
                            Output_Inited_Null => Output_Inited_Null));
                     end if;

                     --  Finalize non-ref operands
                     Finalize_Non_Ref_Operands (New_Visitor,
                       Call_Sem.Definition);

                     --  Move outputs to their final resting place
                     Move_Outputs
                       (Call_Target_Offset,
                        Call_Output_VM_Info => Call_Output_VM_Info,
                        Final_Target_Offset => Visitor.Target_Local_Offset);

                     if Num_Outputs = 0
                       and then Call_Sem.Num_Finalizable_Temps > 0
                     then
                        --  Finalize the "ref" operands if the results
                        --  are no longer "live".
                        if Debug_Code_Gen then
                           Put_Line
                             (" Emit_Call: calling Finalize_Result_And_Refs" &
                              " on " & Subtree_Image (Call_Sem.Definition));
                        end if;
                        Finalize_Result_And_Ref_Operands
                          (New_Visitor, Call_Sem.Definition);
                     end if;
                  end;
               end if;
            end;
         end if;
      end if;

      if Visitor.Target_Local_Offset = Starting_Target_Offset then
         --  Make sure we have room for at least one parameter even if
         --  we don't need one, to avoid tripping check in interpreter
         --  for never reaching Start_Callee_Locals in a Local_Area offset.
         Check_And_Set_Local_Offset (New_Visitor, Starting_Target_Offset + 1);
         Check_And_Set_Local_Offset (New_Visitor, Starting_Target_Offset);
      else
         Check_High_Water (New_Visitor);
      end if;

      if Turn_Off_Leftmost then
         --  Next operand is *not* the leftmost slow call
         Visitor.Is_Leftmost := False;
      end if;

      --  Propagate back "up" certain flags
      Visitor.Num_Instrs := New_Visitor.Num_Instrs;
      Visitor.Last_Instr_Escapes := New_Visitor.Last_Instr_Escapes;
      Visitor.Master_Is_Started := New_Visitor.Master_Is_Started;
      Visitor.Master_Is_Complete := New_Visitor.Master_Is_Complete;
      Visitor.Start_Callee_Locals := New_Visitor.Start_Callee_Locals;
      Visitor.Finalizable_Temp_Offset := New_Visitor.Finalizable_Temp_Offset;

      if Visitor.Target_Local_Offset /=
         Offset_Within_Area (Num_Outputs) + Starting_Target_Offset
        and then not Visitor.Gen_Parallel_Invocations_Only
      then
         Put_Line (" *** Target_Local_Offset not bumped properly");
         Put_Line
           (" Initial =" &
            Offset_Within_Area'Image (Starting_Target_Offset) &
            " +" &
            Natural'Image (Num_Outputs) &
            " /=" &
            Offset_Within_Area'Image (Visitor.Target_Local_Offset));
      end if;

   end Emit_Call;

   procedure Emit_Class_Agg
     (Visitor : in out Code_Gen_Visitor;
      Agg_Sem : Class_Agg_Sem_Ptr;
      Operands : Lists.List) is
      --  Emit a class aggregate with the given operands.
      --  This is sensitive to the three Visitor flags,
      --  Gen_Parallel_Invocations_Only, Master_In_Use, and Is_Leftmost.
      --  This bumps Visitor.Target_Local_Offset by one.

      use Interpreter;
      New_Visitor : Code_Gen_Visitor := Visitor;
      Start_Using_Master : Boolean := False;
      Num_Outputs : constant Natural := 1;
      --  Class agg creates a single output
      Num_Comps : constant Natural :=
        Static.Num_Components (Agg_Sem.Resolved_Type.Associated_Module);
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

   begin  --  Emit_Class_Agg

      if Visitor.Is_Lvalue_Context then
         Sem_Error
           (Agg_Sem.Definition,
            "NYI: Aggregate as a target of an assignment");
      end if;

      if Debug_Code_Gen then
         if Visitor.Gen_Parallel_Invocations_Only then
            Put_Line ("Generate parallel invocations of Class Agg");
         else
            Put_Line ("Generate code for Class Agg");
            Put_Line
              ("# inputs = " & Integer'Image (Lists.Length (Operands)));
         end if;
      end if;

      if not Visitor.Master_In_Use then
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use now False.");
         end if;
         --  Not inside master
         case Agg_Sem.Slow_Calls is
            when Slow_Call_Tree_Needing_Master | Mandatory_Parallel_Call =>
               --  Will use the master later
               null;
            when Independent_Slow_Calls =>
               --  Use master
               Start_Using_Master := True;
            when No_Slow_Calls | Slow_Call | Slow_Call_Sequence =>
               --  Don't need a master, just do it.
               null;
         end case;
      end if;

      if Debug_Code_Gen then
         if Start_Using_Master then
            Put_Line (" Start using master");
         end if;
      end if;

      if Start_Using_Master then
         --  Do two passes:
         --  * In first pass we only walk operands that involve slow calls
         --   generating parallel calls/blocks; location of TCB/result of
         --   call, and nested block num if any, is recorded on
         --   node's computation-sem info; we ignore
         --   leftmost slow call on first pass if it doesn't internally
         --   need a master.  When we invoke a parallel block, we
         --   at that point generate code for the nested block.
         --   We need the leftmost flag to be passed down in the case
         --   the operand has independent slow calls, so it will skip
         --   over the leftmost slow call unless it needs its own master.
         --  * In second pass we do the rest of the computation,
         --   waiting on the parallel threads once we run out of other
         --   things to do.  To simplify things, we emit code to evaluate
         --   operands until we get to a slow call which is done in
         --   parallel, at which point we emit an operation to wait for
         --   them all, before we continue code for evaluating operands.
         --   When we emit a parallel_block instruction, we won't know
         --   the final offset to the nested block, so we record the
         --   index of the instruction (and nested block number) in the
         --   nested-block table to allow later fixup.

         --  We need to mark master as being in use
         --  Do first pass and then second pass
         New_Visitor.Master_In_Use := True;
         New_Visitor.Gen_Parallel_Invocations_Only := True;
         New_Visitor.Is_Leftmost := True;
         if Debug_Code_Gen then
            Put_Line (" New_Visitor.Master_In_Use set to True.");
         end if;

         null; --  TBD
      elsif Visitor.Master_In_Use then
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use now True.");
         end if;
         if Visitor.Gen_Parallel_Invocations_Only then
            --  TBD: Do appropriate thing for parallel call/block
            null;
         else
            --  Wait for master if not already waited for
            --  Retrieve result computed in parallel
            null;  --  TBD
         end if;
      else
         if Debug_Code_Gen then
            Put_Line (" Visitor.Master_In_Use now False.");
         end if;
         --  Not inside a master, not starting a master
         null;  --  Nothing special to do
      end if;

      if New_Visitor.Gen_Parallel_Invocations_Only then
         --  Recurse looking for parallel call/blocks

         --  Recurse into each of the inputs
         for I in 1 .. Lists.Length (Operands) loop
            declare
               Actual_Operand : constant Optional_Tree :=
                 Lists.Nth_Element (Operands, I);
            begin
               if Debug_Code_Gen then
                  Put_Line
                    ("Generate parallel invocations for " &
                     Possibly_Leftmost (New_Visitor) &
                     "actual param = " &
                     Subtree_Image (Actual_Operand));
               end if;
               Emit_Code_For_Resolved_Tree (Actual_Operand, New_Visitor);
            end;
         end loop;

         if Start_Using_Master then
            --  Now do the "normal" code-gen actions
            New_Visitor.Gen_Parallel_Invocations_Only := False;
            New_Visitor.Is_Leftmost := True;
            if Debug_Code_Gen then
               Put_Line (" Done with parallel invocations for Class agg");
            end if;
         else
            --  Copy back Target_Local_Offset to preserve allocations
            --  needed for parallel invocations
            Visitor.Target_Local_Offset := New_Visitor.Target_Local_Offset;
            --  Ditto for Finalizable_Temp_Offset
            Visitor.Finalizable_Temp_Offset :=
              New_Visitor.Finalizable_Temp_Offset;
         end if;
      end if;

      if not New_Visitor.Gen_Parallel_Invocations_Only then
         --  Actually generate code
         declare
            New_Obj_Offset : constant Offset_Within_Area :=
              New_Visitor.Target_Local_Offset;
            --  Remember where new object is created

            New_Obj_VM_Info : VM_Obj_Id_Type := Visitor.Target_VM_Info;
               --  Will replace this if it is not a variable.

            Comp_Base_VM_Info : VM_Obj_Id_Type (Local_Kind);
               --  Will point to base of object when assigning components

            Type_Desc_Loc : constant Object_Locator :=
              Run_Time_Type_Info
                 (Agg_Sem.Resolved_Type,
                  Referring_Module => Enc_Module);
            --  Get type descriptor even if a wrapper to
            --  verify that instantiation is legal.
            Assoc_Module : constant Module_Sem_Ptr :=
              Agg_Sem.Resolved_Type.Associated_Module;
         begin
            if not New_Obj_VM_Info.Is_Var then
               New_Obj_VM_Info :=
                 Assign_VM_Obj_Id (New_Visitor, Needs_Var => True);
                  --  Create a variable for new object.  Will copy it into
                  --  target when done.
                  --  TBD: Could optimize further if felt to be important
                  --       by using original target if will only assign once.

               Emit
                 (New_Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
                   Destination => (Local_Area, New_Obj_Offset,
                                   New_Obj_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Is_By_Ref => False,
                   Is_Var => True,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (Agg_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));
            end if;

            if Static.Type_Is_Wrapper (Agg_Sem.Resolved_Type) then
               --  This is a wrapper type, just evaluate the
               --  only operand.
               declare
                  Actual_Operand : Optional_Tree :=
                    Lists.Nth_Element (Operands, 1);
                  Actual_Operand_Tree : Trees.Tree'Class
                    renames Tree_Ptr_Of (Actual_Operand).all;
                  Actual_To_Be_Moved : constant Boolean :=
                    Actual_Operand_Tree in Assign_Stmt.Tree;
                  --  If True, we have a special case of "X <== Value"
                  Only_Comp : constant Optional_Tree :=
                    Static.Nth_Component (Assoc_Module, 1);
                  Only_Comp_Sem : constant Object_Sem_Ptr :=
                    Object_Sem_Ptr (Sem_Info (Only_Comp));
                  Only_Comp_Is_By_Ref : constant Boolean :=
                    Only_Comp_Sem.Associated_Symbol /= null
                   and then Static.Sym_Is_By_Ref
                               (Only_Comp_Sem.Associated_Symbol);
               --  TBD: We don't support this
               begin
                  if Debug_Code_Gen then
                     Put_Line
                       ("Generate code for wrapper of " &
                        Possibly_Leftmost (New_Visitor) &
                        "component = " &
                        Subtree_Image (Actual_Operand));
                  end if;

                  if Only_Comp_Is_By_Ref then
                     Sem_Error
                       (Actual_Operand,
                        "NYI: Wrapper of ""ref"" component");
                  end if;

                  if Actual_To_Be_Moved then
                     declare
                        Actual_Assign_Tree : Assign_Stmt.Tree renames
                          Assign_Stmt.Tree (Actual_Operand_Tree);
                        RHS_Type : constant Type_Sem_Ptr :=
                          Operand_Sem_Ptr (Underlying_Sem_Info
                                              (Resolved_Tree
                                                  (Actual_Assign_Tree.RHS))).
                                                     Resolved_Type;
                     begin
                        if not Static.Sem_Info_Is_For_Variable
                                 (Underlying_Sem_Info
                                     (Actual_Assign_Tree.RHS))
                        then
                           Sem_Error
                             (Actual_Assign_Tree.RHS,
                              "must be a variable");
                        end if;

                        --  Move past new object location
                        New_Visitor.Target_Local_Offset :=
                          New_Visitor.Target_Local_Offset + 1;

                        --  Get location of existing object
                        New_Visitor.Is_Lvalue_Context := True;
                        Emit_Code_For_Resolved_Tree
                          (Actual_Assign_Tree.RHS,
                           New_Visitor);
                        New_Visitor.Is_Lvalue_Context := False;

                        if not Static.Known_To_Be_Small (RHS_Type) then
                           --  Associate with same region as target object
                           Emit
                             (New_Visitor,
                              (Store_Null_Of_Same_Stg_Rgn_Op,
                               Source_Pos => Find_Source_Pos
                                               (Actual_Assign_Tree.RHS),
                               Destination => (Local_Area, New_Obj_Offset,
                                               New_Obj_VM_Info),
                               Dest_Name => New_Visitor.Dest_Name,
                               Source => New_Visitor.Target_Object,
                               Might_Be_Null => True,
                               Type_Info => Run_Time_Type_Info
                                 (RHS_Type, Referring_Module => Enc_Module)));
                        end if;

                        --  Do the move
                        Emit
                          (New_Visitor,
                           (Move_Obj_Op,
                            Source_Pos => Find_Source_Pos
                                            (Actual_Assign_Tree.RHS),
                            Destination => (Local_Area, New_Obj_Offset,
                                            New_Obj_VM_Info),
                            Dest_Name => New_Visitor.Dest_Name,
                            Source => New_Visitor.Lvalue_Location,
                            Might_Be_Null => RHS_Type.Value_Is_Optional,
                            Type_Info => Run_Time_Type_Info
                                           (RHS_Type,
                                            Referring_Module => Enc_Module)));
                     end;
                  else
                     --  Just evaluate the operand into target object's region
                     New_Visitor.Target_VM_Info := New_Obj_VM_Info;
                     Emit_Code_For_Resolved_Tree
                       (Actual_Operand,
                        New_Visitor);
                  end if;

                  if not Only_Comp_Is_By_Ref
                    and then Static.Is_Unlocked_Concurrent_Operand
                                (Operand_Sem_Ptr (Only_Comp_Sem))
                  then
                     --  Make sure object has a lock
                     Emit
                       (New_Visitor,
                        (Create_Lock_For_Obj_Op,
                         Source_Pos => Find_Source_Pos (Actual_Operand),
                         Destination => (Local_Area, New_Obj_Offset,
                                         New_Obj_VM_Info),
                         Dest_Name => New_Visitor.Dest_Name));
                  end if;
               end;
            else
               --  Not just a wrapper, create object and fill it in
               --  Create object of proper type
               Emit
                 (New_Visitor,
                  (Create_Obj_Op,
                   Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
                   Destination => (Local_Area, New_Obj_Offset,
                                   New_Obj_VM_Info),
                   Dest_Name => New_Visitor.Dest_Name,
                   Source => Visitor.Target_Object,
                   Might_Be_Null => False,
                   Type_Info => Type_Desc_Loc));

               New_Visitor.Target_Local_Offset :=
                 New_Visitor.Target_Local_Offset + 1;

               --  Get a VM reg for use as base
               Comp_Base_VM_Info := Assign_VM_Obj_Id (New_Visitor);

               --  Copy object into VM register (this is a no-op in
               --  the interpreter)
               Emit
                 (New_Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
                   Destination =>
                     (Local_Area, New_Obj_Offset, Comp_Base_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source =>
                     (Local_Area, New_Obj_Offset, New_Obj_VM_Info),
                   Might_Be_Null => False));

               --  Remove target object and Dest_Name, if any, from New_Visitor
               New_Visitor.Target_Object := Null_Object_Locator;
               New_Visitor.Dest_Name := Strings.Null_U_String_Index;

               --  Evaluate each of the inputs
               for I in 1 .. Lists.Length (Operands) loop
                  declare
                     Actual_Operand : constant Optional_Tree :=
                       Lists.Nth_Element (Operands, I);
                     Actual_Operand_Tree : Trees.Tree'Class
                       renames Tree_Ptr_Of (Actual_Operand).all;
                     Actual_To_Be_Moved : constant Boolean :=
                       Actual_Operand_Tree in Assign_Stmt.Tree;
                     --  If True, we have a special case of "X <== Value"
                     Comp : constant Optional_Tree :=
                       Static.Nth_Component (Assoc_Module, I);
                     Comp_Location : constant Object_Locator :=
                       (Base_Register (New_Obj_Offset),
                        Component_Offset (Agg_Sem.Resolved_Type, Comp),
                        Assign_VM_Obj_Id (New_Visitor,
                           Target_VM_Num => Comp_Base_VM_Info.Num,
                           Offset =>
                             Component_Offset (Agg_Sem.Resolved_Type, Comp)));
                     Comp_Val_VM_Info : constant VM_Obj_Id_Type :=
                       Assign_VM_Obj_Id (New_Visitor);
                     Comp_Sem : constant Sem_Ptr :=
                       Sem_Ptr (Sem_Info (Comp));
                     Comp_Is_By_Ref : constant Boolean :=
                       Comp_Sem.Associated_Symbol /= null
                      and then Static.Sym_Is_By_Ref
                                  (Comp_Sem.Associated_Symbol);
                     Actual_Might_Be_Null : constant Boolean :=
                       Resolved_Type (Actual_Operand).Value_Is_Optional;
                  begin
                     if Debug_Code_Gen then
                        Put_Line
                          ("Generate code for " &
                           Possibly_Leftmost (New_Visitor) &
                           "component " &
                           Subtree_Image (Comp) &
                           "(sym-kind = " &
                           Symbols.Sym_Kind_Enum'Image
                              (Comp_Sem.Associated_Symbol.Kind) &
                           ") => " &
                           Subtree_Image (Actual_Operand) &
                           ", is_by_ref = " &
                           Boolean'Image (Comp_Is_By_Ref));
                     end if;

                     if Comp_Is_By_Ref or else Actual_To_Be_Moved then
                        --  We want the address
                        New_Visitor.Is_Lvalue_Context := True;
                     else
                        --  Establish target for this component
                        --  NOTE: We use the aggregate as a whole
                        --       as the target object, since that
                        --       is a simpler address, which allows
                        --       it to be up-level referenced.  It is
                        --       OK since it has the right region.
                        New_Visitor.Target_Object :=
                          (Local_Area,
                           New_Obj_Offset,
                           Comp_Base_VM_Info);

                     end if;

                     --  Evaluate into specified VM reg
                     New_Visitor.Target_VM_Info := Comp_Val_VM_Info;

                     Emit_Code_For_Resolved_Tree
                       (Actual_Operand,
                        New_Visitor);

                     if Comp_Is_By_Ref then
                        --  Store address

                        if Actual_To_Be_Moved then
                           Sem_Error
                             (Agg_Sem.Definition,
                              "May not move a value into a ""ref"" component");
                        elsif Static.Sem_Info_Is_For_Variable (Comp_Sem)
                          and then not Static.Sem_Info_Is_For_Variable
                                         (Underlying_Sem_Info
                                             (Actual_Operand))
                        then
                           Sem_Error (Actual_Operand, "must be a variable");
                        end if;

                        Emit
                          (New_Visitor,
                           (Store_Address_Op,
                            Source_Pos => Find_Source_Pos (Actual_Operand),
                            Destination => Comp_Location,
                            Dest_Name => Strings.Null_U_String_Index,
                            Source => New_Visitor.Lvalue_Location,
                            Might_Be_Null => Actual_Might_Be_Null));
                        New_Visitor.Is_Lvalue_Context := False;
                        New_Visitor.Lvalue_Location := Null_Object_Locator;
                     elsif Actual_To_Be_Moved then
                        --  Move operand into component
                        declare
                           RHS_Location : constant Object_Locator :=
                             New_Visitor.Lvalue_Location;
                        begin
                           New_Visitor.Lvalue_Location := Null_Object_Locator;

                           if not Static.Sem_Info_Is_For_Variable
                             (Underlying_Sem_Info
                               (Assign_Stmt.Tree (Actual_Operand_Tree).RHS))
                           then
                              Sem_Error
                                (Assign_Stmt.Tree (Actual_Operand_Tree).RHS,
                                 "must be a variable");
                           end if;

                           if Comp_Location.Base = Zero_Base
                             or else RHS_Location.Base = Zero_Base
                           then
                              Sem_Error (Actual_Operand, "Move not allowed");
                           end if;

                           --  Do the move
                           Emit
                             (New_Visitor,
                              (Move_Obj_Op,
                               Source_Pos => Find_Source_Pos (Actual_Operand),
                               Destination => Comp_Location,
                               Dest_Name => Strings.Null_U_String_Index,
                               Source => RHS_Location,
                               Might_Be_Null => Actual_Might_Be_Null,
                               Type_Info =>
                                  Run_Time_Type_Info
                                    (Operand_Sem_Ptr (Comp_Sem).Resolved_Type,
                                     Referring_Module => Enc_Module)));
                           New_Visitor.Is_Lvalue_Context := False;
                           New_Visitor.Lvalue_Location := Null_Object_Locator;
                        end;

                     else
                        --  Now actually store (copy of) value into component
                        Emit
                          (New_Visitor,
                           (Copy_Word_Op,
                            Source_Pos => Find_Source_Pos (Actual_Operand),
                            Destination => Comp_Location,
                            Dest_Name => Strings.Null_U_String_Index,
                            Source => (Local_Area,
                                       New_Visitor.Target_Local_Offset - 1,
                                       Comp_Val_VM_Info),
                            Might_Be_Null => Actual_Might_Be_Null));

                        New_Visitor.Target_Object := Null_Object_Locator;
                     end if;

                     if not Comp_Is_By_Ref
                       and then Static.Is_Unlocked_Concurrent_Operand
                                   (Operand_Sem_Ptr (Comp_Sem))
                     then
                        --  Make sure component has a lock
                        Emit
                          (New_Visitor,
                           (Create_Lock_For_Obj_Op,
                            Source_Pos => Find_Source_Pos (Actual_Operand),
                            Destination => Comp_Location,
                            Dest_Name => Strings.Null_U_String_Index));
                     end if;

                     --  We don't reset Target_Local_Offset to
                     --  prior value, since we might be computing
                     --  the values in parallel.
                     --  TBD: Actually, we probably could reuse the
                     --      space but not that big a deal at the moment.

                     --  Remove target object, if any, from New_Visitor
                     New_Visitor.Target_Object := Null_Object_Locator;
                  end;

               end loop;

            end if;

            --  Remember the high-water mark
            Check_High_Water (New_Visitor);

            if New_Obj_Offset /= Visitor.Target_Local_Offset
              or else Visitor.Target_VM_Info /= New_Obj_VM_Info
            then
               --  Move object to original location
               Emit
                 (New_Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
                   Destination => (Local_Area, Visitor.Target_Local_Offset,
                                   Visitor.Target_VM_Info),
                   Dest_Name => Visitor.Dest_Name,  --  Use original Dest_Name
                   Source => (Local_Area, New_Obj_Offset, New_Obj_VM_Info),
                   Might_Be_Null => False));
            end if;

            --  Protect the new object
            Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
         end;
      end if;

      --  Propagate back "up" certain flags
      Visitor.Num_Instrs := New_Visitor.Num_Instrs;
      Visitor.Last_Instr_Escapes := New_Visitor.Last_Instr_Escapes;
      Visitor.Master_Is_Started := New_Visitor.Master_Is_Started;
      Visitor.Master_Is_Complete := New_Visitor.Master_Is_Complete;
      Visitor.Start_Callee_Locals := New_Visitor.Start_Callee_Locals;
      Visitor.Finalizable_Temp_Offset := New_Visitor.Finalizable_Temp_Offset;

      Check_High_Water (Visitor);
   end Emit_Class_Agg;

   function Locked_Param_Info_For_Container
     (Container_Type : Type_Sem_Ptr;
      Container_Param_Index : Interpreter.Param_Index_Subtype;
      Container_Is_Var : Boolean)
     return Interpreter.Locked_Param_Info_Type is
   --  Return appropriate locked-param-info if container is of a concurrent
   --  type, otherwise return Null_Locked_Param_Info.
   --  Container_Param_Index identifies the container parameter,
   --  Container_Is_Var indicates whether the container parameter is a "var"
   --  param.
   --  NOTE: We are assuming that "var" params are always passed by reference.
   begin
      if Static.Type_Is_Concurrent (Container_Type) then
         return (Param_Index    => Container_Param_Index,
                 Is_By_Ref      => Container_Is_Var,
                 Is_Var         => Container_Is_Var,
                 Is_Queued_Call => False);
      else
         return Interpreter.Null_Locked_Param_Info;
      end if;
   end Locked_Param_Info_For_Container;

   procedure Emit_Named_Element
     (Visitor : in out Code_Gen_Visitor; Agg_Sem : Container_Agg_Sem_Ptr;
      Key : Optional_Tree;
      Key_Sem : Param_Sem_Ptr := null;
      With_Key_Conversion : Boolean := False;
      Value : Optional_Tree) is
   --  Emit Key => Value element of a container aggregate.
   --  If Key_Sem is non-null, it is used instead of evaluating the Key
   --  If With_Key_Conversion is True, then a call on "to_univ" or
   --  "from_univ" is needed to produce the desired Agg_Sem.Index_Type.

      Var_Indexing_Output_Type : Type_Sem_Ptr := Agg_Sem.Element_Type;
      Var_Indexing_Returns_Ref_Obj : Boolean := False;

      function Container_Key_Gives_Element_Signature
        (Operand_Type : Type_Sem_Ptr;
         Input_Types : Type_Sem_Array;
         Output_Types : Type_Sem_Array)
         return Boolean
      is
      --  Return True if First input is Container,
      --  second is key, and output is element type (or ref-obj for it)
      --  Set Var_Indexing_Returns_Ref_Obj and ..._Output_Type as side-effect
      --  if match is via "ref" operation.
      begin
         if Input_Types (1).Associated_Module =
                Operand_Type.Associated_Module
           and then Types_Match (Input_Types (2), Agg_Sem.Index_Type)
         then
            if Types_Match (Output_Types (1), Agg_Sem.Element_Type) then
               --  Exact match
               return True;
            elsif Types_Match
                     (Static.Get_Ref_Func_Output_Type
                        (Output_Types (1)),
                      Agg_Sem.Element_Type)
            then
               --  Match via "ref" function
               Var_Indexing_Output_Type := Output_Types (1);
               Var_Indexing_Returns_Ref_Obj := True;
               return True;
            end if;
         end if;
         --  Not a match
         return False;
      end Container_Key_Gives_Element_Signature;

      function Find_Var_Indexing_Op_For is new Static.Find_Op_For (
         Static.Var_Indexing_Op_Str,
         Num_Inputs => 2,
         Num_Outputs => 1,
         Has_Desired_Signature => Container_Key_Gives_Element_Signature);

      Var_Indexing_Op_Sem : constant Operation_Sem_Ptr :=
        Find_Var_Indexing_Op_For (Agg_Sem.Resolved_Type);

      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

      use Interpreter;

      Params_Offset : Offset_Within_Area :=
        Visitor.Target_Local_Offset;
         --  Location of parameters being passed to var-indexing operation.

      Value_VM_Info : constant VM_Obj_Id_Type := Assign_VM_Obj_Id (Visitor);

      Elem_Ref_VM_Info : VM_Obj_Id_Type := Assign_VM_Obj_Id (Visitor);
         --  Will be reassigned if var_indexing returns a "ref" obj.

      Indexing_Call_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor,
                          Target_VM_Num => Elem_Ref_VM_Info.Num,
                          Num_Call_Params => 3);

      Ref_Obj_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;

      Ref_Output_VM_Info : VM_Obj_Id_Type (Local_Kind);
         --  Here is where output of "ref" will go.

      Ref_Op_VM_Info : VM_Obj_Id_Type (Local_Kind);
         --  This specifies the VM regs for call on "ref"

      Ref_Obj_VM_Info : VM_Obj_Id_Type (Local_Kind);
         --  This is where the ref obj will be.

      --  Location of container aggregate, adjusted if up level.
      Container_Agg_Loc : constant Object_Locator :=
        Adjust_For_Level_And_Prefix
          (Visitor.Current_Level,
           Obj_Location => Agg_Sem.Info.Obj_Location,
           Obj_Level => Agg_Sem.Info.Obj_Level);

      Key_Type : Type_Sem_Ptr := null;
      Non_Univ_Type : Type_Sem_Ptr := null;

      Key_VM_Info : VM_Obj_Id_Type (Local_Kind);
      Key_Conv_VM_Info : VM_Obj_Id_Type (Local_Kind);

   begin  --  Emit_Named_Element

      --  Key => Value
      --  Turn into Container[Key] := Value;
      --  TBD: Handle case where multiple keys are given.
      --  NOTE: Use "var_indexing" operator.
      --  TBD: Use "var_indexing" in other contexts where container
      --      is a variable, and it is on the LHS or is being
      --      passed as a (ref) variable of optional element type.
      if Var_Indexing_Op_Sem = null then
         Sem_Error
           (Key,
            "Key => Value not supported for " &
            Type_Image (Agg_Sem.Resolved_Type));

         return;   --  Quit now
      end if;

      if Var_Indexing_Returns_Ref_Obj then
         --  Leave room for ref object and for call on "ref"
         Params_Offset := Ref_Obj_Offset + 2;
         Visitor.Target_Local_Offset := Params_Offset;

         --  Here is where output of "ref" will go.
         Ref_Output_VM_Info := Assign_VM_Obj_Id (Visitor);

         --  Assign VM regs for call on "ref"
         Ref_Op_VM_Info :=
           Assign_VM_Obj_Id (Visitor,
                             Target_VM_Num => Ref_Output_VM_Info.Num,
                             Num_Call_Params => 2);

         --  Assign VM reg for ref obj from input param to "ref"
         Ref_Obj_VM_Info :=
           Param_VM_Obj_Id (Ref_Op_VM_Info, Param_Offset => 1);
            --  NOTE: The ref-obj doesn't need to be a VM variable
            --        because it is always passed by copy (even when it
            --        is a ParaSail "var" param), because "ref" objects
            --        cannot be re-assigned.

         --  Establish null target for "var_indexing" operation
         Emit
           (Visitor,
            (Store_Local_Null_Op,
             Source_Pos => Find_Source_Pos (Key),
             Destination => (Local_Area, Params_Offset,
                             Param_VM_Obj_Id (Indexing_Call_VM_Info, 0)),
             Dest_Name => Strings.Null_U_String_Index,
             Null_Type_Info => Run_Time_Type_Info
                                 (Var_Indexing_Output_Type,
                                  Referring_Module => Enc_Module)));
      end if;

      --  Leave room for var-indexing output
      Visitor.Target_Local_Offset := Params_Offset + 1;

      declare
         Var_Indexing_Op_Tree : Operation.Tree renames Operation.Tree
           (Tree_Ptr_Of (Var_Indexing_Op_Sem.Definition).all);

         Container_Is_By_Ref : constant Boolean :=
           Static.Sym_Is_By_Ref (Param_Sem_Ptr
             (Sem_Info
               (Lists.Nth_Element
                 (Var_Indexing_Op_Tree.Operation_Inputs, 1))).
                    Associated_Symbol);

      begin
         if Container_Is_By_Ref then
            --  Pass container by ref
            Emit
              (Visitor,
               (Store_Address_Op,
                Source_Pos => Find_Source_Pos (Key),
                Destination => (Local_Area, Params_Offset + 1,
                                Param_VM_Obj_Id (Indexing_Call_VM_Info, 1)),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Container_Agg_Loc,
                Might_Be_Null => False));
         else
            --  Pass container by copy (probably is polymorphic)
            Emit
              (Visitor,
               (Copy_Word_Op,
                Source_Pos => Find_Source_Pos (Key),
                Destination => (Local_Area, Params_Offset + 1,
                                Param_VM_Obj_Id (Indexing_Call_VM_Info, 1)),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Container_Agg_Loc,
                Might_Be_Null => False));
         end if;
      end;

      Visitor.Target_Local_Offset := Params_Offset + 2;

      if With_Key_Conversion then
         --  Leave room for conversion output
         Visitor.Target_Local_Offset := Params_Offset + 3;

         --  Set up VM info for key conversion
         Key_Conv_VM_Info :=
           Assign_VM_Obj_Id (Visitor,
                             Target_VM_Num => Key_VM_Info.Num,
                             Num_Call_Params => 2);
         Key_VM_Info := Param_VM_Obj_Id (Key_Conv_VM_Info, 1);
      else
         --  Set up VM info for Key when no conversion required
         Key_VM_Info := Param_VM_Obj_Id (Indexing_Call_VM_Info, 2);
      end if;

      if Key_Sem = null then
         --  Evaluate key into input slot for "var_indexing" operation.
         Visitor.Target_VM_Info := Key_VM_Info;
         Emit_Code_For_Resolved_Tree (Key, Visitor);
         Key_Type := Resolved_Type (Key);
      else
         --  Load key given parameter sem info
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Key),
             Destination => (Local_Area,
                             Visitor.Target_Local_Offset,
                             Key_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Source => Key_Sem.Info.Obj_Location,
             Might_Be_Null => Key_Sem.Resolved_Type.Value_Is_Optional));
         Key_Type := Key_Sem.Resolved_Type;
      end if;

      if With_Key_Conversion then
         --  Call To_Univ or From_Univ, as appropriate
         declare
            Conversion_Op : Operation_Sem_Ptr;
         begin
            if Key_Type.Is_Universal then
               --  Convert from universal
               Conversion_Op :=
                 Static.Get_From_Univ_Op
                   (From => Key_Type, To => Agg_Sem.Index_Type);
               Non_Univ_Type := Agg_Sem.Index_Type;
            else
               --  Convert to universal
               Conversion_Op :=
                 Static.Get_To_Univ_Op
                   (From => Key_Type, To => Agg_Sem.Index_Type);
               Non_Univ_Type := Key_Type;
            end if;

            --  Now call the appropriate conversion function

            if not Static.Known_To_Be_Small (Key_Type) then
               --  But first need to pass in a null in case needs a target rgn.
               Emit
                 (Visitor,
                  (Store_Local_Null_Op,
                   Source_Pos => Find_Source_Pos (Key),
                   Destination => (Local_Area, Params_Offset + 2,
                                   Param_VM_Obj_Id (Key_Conv_VM_Info, 0)),
                   Dest_Name => Strings.Null_U_String_Index,
                   Null_Type_Info => Run_Time_Type_Info
                                       (Key_Type,
                                        Referring_Module => Enc_Module)));
            end if;

            Emit
              (Visitor,
               (Call_Op,
                Source_Pos => Find_Source_Pos (Key),
                Call_Target => Routine_Locator
                                 (Conversion_Op, Non_Univ_Type),
                Target_Index =>
                  Find_Operation_Routine_Index (Conversion_Op),
                Params => (Local_Area, Params_Offset + 2, Key_Conv_VM_Info),
                Locked_Param_Info => Locked_Param_Info_For_Container
                  (Container_Type => Agg_Sem.Target_Type,
                   Container_Param_Index => 2,
                   Container_Is_Var => True),
                Static_Link => Run_Time_Type_Info
                                 (Non_Univ_Type,
                                  Referring_Module => Enc_Module),
                Precond_Proved => False,
                Output_Inited_Null =>
                  not Static.Known_To_Be_Small (Key_Type)));
         end;
      end if;

      --  Now call "var_indexing"
      Emit
        (Visitor,
         (Call_Op,
          Source_Pos => Find_Source_Pos (Key),
          Call_Target => Routine_Locator
                           (Var_Indexing_Op_Sem,
                            Agg_Sem.Resolved_Type),
          Target_Index =>
            Find_Operation_Routine_Index (Var_Indexing_Op_Sem),
          Params => (Local_Area, Params_Offset, Indexing_Call_VM_Info),
          Locked_Param_Info => Locked_Param_Info_For_Container
            (Container_Type => Agg_Sem.Target_Type,
             Container_Param_Index => 2,
             Container_Is_Var => True),
          Static_Link => Run_Time_Type_Info
                           (Agg_Sem.Resolved_Type,
                            Referring_Module => Enc_Module),
          Precond_Proved => False,
          Output_Inited_Null => Var_Indexing_Returns_Ref_Obj));

      if Var_Indexing_Returns_Ref_Obj then
         --  We have a "ref" object rather than a simple "ref".
         --  Need to make a copy of it, then call "ref" on it.

         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Key),
             Destination => (Local_Area, Ref_Obj_Offset, Ref_Obj_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Source => (Local_Area, Params_Offset, Elem_Ref_VM_Info),
             Might_Be_Null => False));

         --  Here is where the output of "ref" will go
         Elem_Ref_VM_Info := Ref_Output_VM_Info;

         --  Now call "ref"
         declare
            Ref_Op_Sem : constant Operation_Sem_Ptr :=
              Static.Find_One_In_One_Out_Op_For
                (Var_Indexing_Output_Type, Op_Name => Static.Ref_Op_Str);
         begin
            Emit
              (Visitor,
               (Call_Op,
                Source_Pos => Find_Source_Pos (Key),
                Call_Target => Routine_Locator
                                 (Ref_Op_Sem, Var_Indexing_Output_Type),
                Target_Index =>
                  Find_Operation_Routine_Index (Ref_Op_Sem),
                Params => (Local_Area, Params_Offset - 1, Ref_Op_VM_Info),
                Locked_Param_Info => Null_Locked_Param_Info, --  TBD
                Static_Link => Run_Time_Type_Info
                                 (Var_Indexing_Output_Type,
                                  Referring_Module => Enc_Module),
                Precond_Proved => False,
                Output_Inited_Null => False));
         end;
         Visitor.Target_Local_Offset := Params_Offset;
         Params_Offset := Params_Offset - 1;
      end if;

      --  Now evaluate value with aggregate as a whole
      --  as the target, since it has a simpler address,
      --  and is certain to be of the right region.

      Visitor.Target_Object := Container_Agg_Loc;

      Visitor.Target_VM_Info := Value_VM_Info;
      Emit_Code_For_Resolved_Tree (Value, Visitor);

      Visitor.Target_Object := Null_Object_Locator;

      --  Now assign into returned "ref"
      Emit
        (Visitor,
         (Assign_Word_Op,  --  "Assign_Word" will reclaim target
                           --  if large and non-null.
                           --  That implies we need to pass the type.
          Source_Pos => Find_Source_Pos (Key),
          Destination => (Phys_Base_Register (Params_Offset), 0,
                          Indir_VM_Obj_Id (Elem_Ref_VM_Info)),
          Dest_Name => Strings.Null_U_String_Index,
          Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                     Value_VM_Info),
          Might_Be_Null => True,
          Type_Info => Run_Time_Type_Info
                         (Agg_Sem.Element_Type,
                          Referring_Module => Enc_Module)));

      if Var_Indexing_Returns_Ref_Obj then
         --  Check if there is an "end" operation, and call it on the ref-obj.
         declare
            Ref_Obj_End_Op : constant Operation_Sem_Ptr :=
              Find_End_Op_For (Var_Indexing_Output_Type);
         begin
            if Ref_Obj_End_Op /= null then
               --  Now call "end"
               declare
                  End_Op_VM_Info : constant VM_Obj_Id_Type :=
                    Assign_VM_Obj_Id
                      (Visitor,
                       Num_Call_Params => 1,
                       Only_Param_VM_Num => Ref_Obj_VM_Info.Num);
               begin
                  --  Note that ref-objects are always passed by copy,
                  --  even if they are "var" parameters (yes, I know this
                  --  is a confusing issue).
                  --  This means we don't need to allocate separate registers
                  --  for calling the "end" op; we can just pass the Ref_Obj
                  --  as the one and only parameter:
                  Emit
                    (Visitor,
                     (Call_Op,
                      Source_Pos => Find_Source_Pos (Key),
                      Call_Target => Routine_Locator
                                       (Ref_Obj_End_Op,
                                        Var_Indexing_Output_Type),
                      Target_Index =>
                        Find_Operation_Routine_Index (Ref_Obj_End_Op),
                      Params => (Local_Area, Ref_Obj_Offset, End_Op_VM_Info),
                      Locked_Param_Info => Null_Locked_Param_Info, --  TBD
                      Static_Link => Run_Time_Type_Info
                                       (Var_Indexing_Output_Type,
                                        Referring_Module => Enc_Module),
                      Precond_Proved => False,
                      Output_Inited_Null => False));
               end;
            end if;
         end;
      end if;

      --  Restore target offset
      Check_And_Set_Local_Offset (Visitor, Ref_Obj_Offset);

   end Emit_Named_Element;

   procedure Emit_Positional_Element
     (Visitor : in out Code_Gen_Visitor; Agg_Sem : Container_Agg_Sem_Ptr;
      Operand : Optional_Tree) is
   --  Emit positional element of container aggregate.

      Combiner_Op_Sem : Operation_Sem_Ptr :=
        Static.Find_Combine_Move_Op_For (Agg_Sem.Resolved_Type);

      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

      use Interpreter;

      Params_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;

      --  Location of container aggregate, adjusted if up level.
      Container_Agg_Loc : constant Object_Locator :=
        Adjust_For_Level_And_Prefix
          (Visitor.Current_Level,
           Obj_Location => Agg_Sem.Info.Obj_Location,
           Obj_Level => Agg_Sem.Info.Obj_Level);

      Value_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor, Needs_Var => True);

      Combine_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor,
                          Num_Call_Params => 2);

   begin   --  Emit_Positional_Element

      --  Turn into Container <|= Value;
      if Combiner_Op_Sem = null then
         --  No <|= operator with second param *not* same as first
         --  Look for a fallback where they are the same, to handle
         --  types like JSON_Value where array and component types
         --  are the same.
         Combiner_Op_Sem :=
           Static.Find_Combine_Move_Op_Fallback (Agg_Sem.Resolved_Type);

         if Combiner_Op_Sem = null then
            Sem_Error
              (Operand,
               Strings.To_String (Static.Combine_Move_Op_Str) &
               " positional aggregate not supported by type");
            return;
         end if;
      end if;

      --  Evaluate operand into temp for "<|=" operation with
      --  target object being the empty container.
      Emit
        (Visitor,
         (Declare_Obj_Op,
          Source_Pos => Find_Source_Pos (Operand),
          Destination => (Local_Area, Visitor.Target_Local_Offset,
                          Value_VM_Info),
          Dest_Name => Strings.Null_U_String_Index,
          Is_By_Ref => False,
          Is_Var => True,
          Declare_Type_Info => Run_Time_Type_Info
                              (Agg_Sem.Element_Type,
                               Referring_Module => Enc_Module)));

      Visitor.Target_Object := Container_Agg_Loc;
      Visitor.Target_VM_Info := Value_VM_Info;
      Emit_Code_For_Resolved_Tree (Operand, Visitor);
      Visitor.Target_Object := Null_Object_Locator;

      declare
         Combiner_Op_Tree : Operation.Tree renames Operation.Tree
           (Tree_Ptr_Of (Combiner_Op_Sem.Definition).all);

         Container_Is_By_Ref : constant Boolean :=
           Static.Sym_Is_By_Ref (Param_Sem_Ptr
             (Sem_Info
               (Lists.Nth_Element
                 (Combiner_Op_Tree.Operation_Inputs, 1))).
                    Associated_Symbol);
      begin
         if Container_Is_By_Ref then
            --  Pass container by ref
            Emit
              (Visitor,
               (Store_Address_Op,
                Source_Pos => Find_Source_Pos (Operand),
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Param_VM_Obj_Id (Combine_VM_Info, 0)),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Container_Agg_Loc,
                Might_Be_Null => False));
         else
            --  Pass container by copy (probably is polymorphic)
            Emit
              (Visitor,
               (Copy_Word_Op,
                Source_Pos => Find_Source_Pos (Operand),
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Param_VM_Obj_Id (Combine_VM_Info, 0)),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Container_Agg_Loc,
                Might_Be_Null => False));
         end if;
      end;

      Visitor.Target_Local_Offset :=
        Visitor.Target_Local_Offset + 1;

      --  Pass temp element value by ref
      Emit
        (Visitor,
         (Store_Address_Op,
          Source_Pos => Find_Source_Pos (Operand),
          Destination => (Local_Area, Visitor.Target_Local_Offset,
                          Param_VM_Obj_Id (Combine_VM_Info, 1)),
          Dest_Name => Strings.Null_U_String_Index,
          Source => (Local_Area, Params_Offset, Value_VM_Info),
          Might_Be_Null => True));
      Visitor.Target_Local_Offset :=
        Visitor.Target_Local_Offset + 1;

      --  Now call "<|="
      Emit
        (Visitor,
         (Call_Op,
          Source_Pos => Find_Source_Pos (Operand),
          Call_Target => Routine_Locator
                           (Combiner_Op_Sem,
                            Agg_Sem.Resolved_Type),
          Target_Index =>
            Find_Operation_Routine_Index (Combiner_Op_Sem),
          Params => (Local_Area, Params_Offset + 1, Combine_VM_Info),
          Locked_Param_Info => Locked_Param_Info_For_Container
            (Container_Type => Agg_Sem.Target_Type,
             Container_Param_Index => 1,
             Container_Is_Var => True),
          Static_Link => Run_Time_Type_Info
                           (Agg_Sem.Resolved_Type,
                            Referring_Module => Enc_Module),
          Precond_Proved => False,
          Output_Inited_Null => False));

      Check_And_Set_Local_Offset (Visitor, Params_Offset);

   end Emit_Positional_Element;

   procedure Emit_Container_Agg
     (Visitor : in out Code_Gen_Visitor;
      Agg_Sem : Container_Agg_Sem_Ptr;
      Operands : Lists.List) is
      --  Emit a container aggregate with the given operands.
      --  This is sensitive to the three Visitor flags,
      --  Gen_Parallel_Invocations_Only, Master_In_Use, and Is_Leftmost.
      --  This bumps Visitor.Target_Local_Offset by one.

      use Interpreter;
      Num_Outputs : constant Natural := 1;
      --  Container agg creates a single output
      T : Invocation.Tree
        renames Invocation.Tree (Tree_Ptr_Of (Agg_Sem.Definition).all);
      Num_Operands : constant Natural := Lists.Length (T.Operands);
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      New_Obj_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;
         --  Remember where new object is created

      Orig_Target_VM_Info : constant VM_Obj_Id_Type :=
        Visitor.Target_VM_Info;  --  Remember original VM target reg

      New_Obj_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor, Needs_Var => True);
         --  VM info for new obj

      Assoc_Module : constant Module_Sem_Ptr :=
        Agg_Sem.Resolved_Type.Associated_Module;
      Orig_Dest_Name : constant Strings.U_String_Index := Visitor.Dest_Name;
      Orig_Lvalue_Context : constant Boolean := Visitor.Is_Lvalue_Context;

      function Find_Empty_Container_Op_For is new Static.Find_Op_For (
         Static.Empty_Container_Op_Str,
         Num_Inputs => 0,
         Num_Outputs => 1,
         Has_Desired_Signature => Static.First_Output_Is_Operand_Type);

      function Container_Element_Signature
        (Operand_Type : Type_Sem_Ptr;
         Input_Types : Type_Sem_Array;
         Output_Types : Type_Sem_Array)
         return Boolean
      is
      --  Return True if First input is Container
      --  and second is element type
      begin
         return Input_Types (1).Associated_Module =
                Operand_Type.Associated_Module
               and then Types_Match (Input_Types (2), Agg_Sem.Element_Type);
      end Container_Element_Signature;

      Empty_Container_Op_Sem : constant Operation_Sem_Ptr :=
        Find_Empty_Container_Op_For (Agg_Sem.Resolved_Type);

      Empty_Op_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Visitor,
                          Needs_Var => True,
                          Target_VM_Num => New_Obj_VM_Info.Num,
                          Num_Call_Params => 1);

   begin  --  Emit_Container_Agg

      if Empty_Container_Op_Sem = null then
         Sem_Error
           (T,
            Strings.To_String (Static.Empty_Container_Op_Str) &
            " operator not defined");
         return;
      end if;

      if Visitor.Gen_Parallel_Invocations_Only then
         if Debug_Code_Gen then
            Put_Line
              ("Nothing to do -- " &
               "generate parallel invocations of Container Agg");
         end if;
         return;
      end if;

      if Debug_Code_Gen then
         Put_Line ("Generate code for Container Agg");
         Put_Line ("# inputs = " & Integer'Image (Num_Operands));
      end if;

      --  Clear lvalue context for now
      Visitor.Is_Lvalue_Context := False;

      if not Target_Obj_Is_Null (Visitor) then
         --  Create a "null" of appropriate region
         Emit
           (Visitor,
            (Store_Null_Of_Same_Stg_Rgn_Op,
             Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
             Destination => (Local_Area, New_Obj_Offset,
                             Param_VM_Obj_Id (Empty_Op_VM_Info, 0)),
             Dest_Name => Visitor.Dest_Name,
             Source => Visitor.Target_Object,
             Might_Be_Null => True,
             Type_Info => Run_Time_Type_Info
                            (Agg_Sem.Resolved_Type,
                             Referring_Module => Enc_Module)));
      else
         --  Create a "local" null.
         Emit
           (Visitor,
            (Store_Local_Null_Op,
             Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
             Destination => (Local_Area, New_Obj_Offset,
                             Param_VM_Obj_Id (Empty_Op_VM_Info, 0)),
             Dest_Name => Visitor.Dest_Name,
             Null_Type_Info => Run_Time_Type_Info
                                 (Agg_Sem.Resolved_Type,
                                  Referring_Module => Enc_Module)));
      end if;

      --  Create empty container
      Emit
        (Visitor,
         (Declare_Obj_Op,
          Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
          Destination => (Local_Area, New_Obj_Offset, New_Obj_VM_Info),
          Dest_Name => Strings.Null_U_String_Index,
          Is_By_Ref => False,
          Is_Var => True,
          Declare_Type_Info => Run_Time_Type_Info
                              (Agg_Sem.Resolved_Type,
                               Referring_Module => Enc_Module)));

      Emit
        (Visitor,
         (Call_Op,
          Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
          Call_Target => Routine_Locator
                           (Empty_Container_Op_Sem,
                            Agg_Sem.Resolved_Type),
          Target_Index =>
            Find_Operation_Routine_Index (Empty_Container_Op_Sem),
          Locked_Param_Info => Null_Locked_Param_Info,
          Params => (Local_Area, New_Obj_Offset, Empty_Op_VM_Info),
          Static_Link => Run_Time_Type_Info
                           (Agg_Sem.Resolved_Type,
                            Referring_Module => Enc_Module),
          Precond_Proved => False,
          Output_Inited_Null => True));

      if Static.Type_Is_Concurrent (Agg_Sem.Target_Type)
        and then Num_Operands > 0
      then
         --  Make sure temp aggregate object has a lock
         Emit
           (Visitor,
            (Create_Lock_For_Obj_Op,
             Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
             Destination => (Local_Area, New_Obj_Offset, New_Obj_VM_Info),
             Dest_Name => Visitor.Dest_Name));
      end if;

      Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

      --  Remove target object and Dest_Name, if any, from Visitor
      Visitor.Target_Object := Null_Object_Locator;
      Visitor.Dest_Name := Strings.Null_U_String_Index;

      --  Remember location of new object
      Agg_Sem.Info.Obj_Location :=
        (Local_Area, New_Obj_Offset, New_Obj_VM_Info);
      Agg_Sem.Info.Obj_Level := Visitor.Current_Level;

      --  Evaluate each of the inputs
      for I in 1 .. Num_Operands loop
         declare
            Operand : constant Optional_Tree :=
              Resolved_Tree (Lists.Nth_Element (Operands, I));
            Operand_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Operand).all;
         begin

            --  Make sure we know where we are
            pragma Assert (Visitor.Target_Local_Offset = New_Obj_Offset + 1);

            if Debug_Code_Gen then
               Put_Line
                 ("Generate code for component" &
                  Natural'Image (I) &
                  " = " &
                  Subtree_Image (Operand));
            end if;

            if Operand_Tree in For_Loop_Construct.Tree then
               --  for I in Set [, Key ] => Value

               --  Generate a (forward?) loop.
               --  (TBD: Figure out a way to use a concurrent loop)
               --  On each iteration, either either use "var_indexing"
               --  or "<|=".  If using "var_indexing", key comes from
               --  explicit ", Key =>" part, or if that is not present,
               --  either from the iterator_sem or key_sem object.
               --  Complain if iterator_sem and key_sem are of same type,
               --  if no explicit ", key =>" given, due to ambiguity.

               Emit_Code_For_Resolved_Tree (Operand, Visitor);

            elsif Operand_Tree in Reference.Tree then
               --  Key => Value
               declare
                  Ref_Tree : Reference.Tree renames Reference.Tree (
                    Operand_Tree);
               begin
                  Emit_Named_Element (Visitor, Agg_Sem,
                    Key => Ref_Tree.Key, Value => Ref_Tree.Referent);
               end;
            else
               --  Positional element
               Emit_Positional_Element
                 (Visitor, Agg_Sem, Operand);
            end if;
         end;
      end loop;

      --  Restore Is_Lvalue_Context, and Dest_Name if any
      Visitor.Is_Lvalue_Context := Orig_Lvalue_Context;
      Visitor.Dest_Name := Orig_Dest_Name;

      if Visitor.Is_Lvalue_Context then
         --  Return location of container aggregate
         --  TBD: This currently can happen after substitution
         --       of a module parameter into a constant expression
         --       that iterates over the parameter.  Eventually these
         --       should be replaced with a reference to a normal object.
         Initialize_Lvalue_Location_From_Temp
           (Visitor, Operand_Sem_Ptr (Agg_Sem),
            Agg_Sem.Info.Obj_Location);
      else
         --  Move aggregate to original VM target
         --  (this is a no-op in the interpreter)
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Agg_Sem.Definition),
             Destination => (Local_Area, New_Obj_Offset, Orig_Target_VM_Info),
             Dest_Name => Visitor.Dest_Name,  --  Use original Dest_Name
             Source => Agg_Sem.Info.Obj_Location,
             Might_Be_Null => False));
      end if;

   end Emit_Container_Agg;

   function Literal_Value (Lit : String) return Interpreter.Word_Type is
   --  Return value used to represent given literal
   begin
      case Lit (Lit'First) is
         when '-' | '0' .. '9' =>  --  Int literal or Real literal
            --  Check whether is a "real" value
            for I in Lit'Range loop
               if Lit (I) = '.' then
                  --  Found a ".", must be "real"
                  return Interpreter.From_Univ_Real
                           (Interpreter.Real_Value (Lit));
               end if;
            end loop;
            return Interpreter.Integer_Value (Lit);

         when '#' => --  Enumeration literal
            --  Return u_string index
            return Interpreter.Word_Type
               (Strings.Index (Strings.String_Lookup (Lit)));

         when '"' => --  String literal
            return Interpreter.Word_Type
              (Strings.Index (Strings.Wide_Wide_String_Lookup
                (Strings.Decode_Source_Rep
                  (Lit (Lit'First + 1 .. Lit'Last - 1)))));

         when ''' => --  Character literal
            declare
               Str : constant Wide_Wide_String :=
                 Strings.Decode_Source_Rep
                   (Lit (Lit'First + 1 .. Lit'Last - 1));
            --  Expand escaped characters
            begin
               return Wide_Wide_Character'Pos (Str (Str'First));
            end;

         when others =>
            if Lit = "null" then
               --  Must be "null"
               return Interpreter.Null_Value;
            end if;

            --  Treat everything else as a string literal
            return Interpreter.Word_Type
               (Strings.Index (Strings.String_Lookup (Lit)));
      end case;

   end Literal_Value;

   function String_Is_Convertible
     (Lit : String;
      Param_Type_Desc : Interpreter.Type_Descriptor_Ptr)
      return Boolean
   is
      --  Return True if given Lit can be converted to
      --  parameter with given type desc, using Convert_String.
      use Interpreter;
      Value_Type : Type_Sem_Ptr;
      Param_Type : constant Type_Sem_Ptr :=
        Type_Sem_Ptr (Param_Type_Desc.Type_Sem);
      use Interpreter;
   begin
      if Interpreter.Type_Descriptor_Ops.Unwrapped_Type_Desc
        (Param_Type_Desc).Type_Kind = Univ_String_Kind
      then
         --  Any string is convertible to a string!
         return True;
      end if;

      case Lit (Lit'First) is
         when '-' | '0' .. '9' =>  --  Int literal or Real literal
            Value_Type := Univ_Integer_Type;
            --  Check whether is a "real" value
            for I in Lit'Range loop
               if Lit (I) = '.' then
                  --  Found a ".", must be "real"
                  Value_Type := Univ_Real_Type;
                  exit;
               end if;
            end loop;

         when '#' => --  Enumeration literal
            --  Return u_string index
            Value_Type := Univ_Enumeration_Type;

         when '"' => --  String literal
            Value_Type := Univ_String_Type;

         when ''' => --  Character literal
            Value_Type := Univ_Character_Type;

         when 'a' .. 'z' | 'A' .. 'Z' | '/' =>
            if Lit = "null" then
               --  Null is convertible to anything
               return True;
            end if;

            --  Treat everything else starting with a letter or a slash
            --  as a string. Slash included for absolute filepaths
            Value_Type := Univ_String_Type;

         when others =>
            return False;
      end case;

      if Value_Type.U_Base_Type = Param_Type.U_Base_Type then
         return True;
      elsif Value_Type = Univ_Integer_Type
        and then Param_Type.Associated_Module = Integer_Module
      then
         return True;
      elsif Value_Type = Univ_Real_Type
        and then Param_Type.Associated_Module = Float_Module
      then
         return True;
      end if;

      --  TBD: Handle more types convertible from univ. type
      return False;

   end String_Is_Convertible;

   function Convert_String
     (Lit : String;
      Param_Type_Desc : Interpreter.Type_Descriptor_Ptr;
      Local_Stg_Rgn : Interpreter.Stg_Rgn_Ptr)
      return Interpreter.Word_Type
   is
   --  Return value of Str after converting to the
   --  type of the given param.
   --  Requires: String_Is_Convertible(Lit, Param_Type_Desc)
      use Interpreter;
   begin
      if Lit = "null" then
         return Interpreter.Null_For_Type_Or_Stg_Rgn
           (Type_Desc => Param_Type_Desc,
            Stg_Rgn => Local_Stg_Rgn);
      elsif Interpreter.Type_Descriptor_Ops.Unwrapped_Type_Desc
        (Param_Type_Desc).Type_Kind = Univ_String_Kind
      then
         --  Anything can be converted to univ string
         declare
            Lit_Str : Strings.U_String;
            use Univ_Strings;
         begin
            if Lit (Lit'First) = '"' then
               --  Strip off the '"'s and convert to U_String
               Lit_Str := Strings.To_U_String
                            (Strings.U_String_Index (Literal_Value (Lit)));
            else
               Lit_Str := Strings.String_Lookup (Lit);
            end if;

            --  Convert U_String to large Univ_String representation
            return To_Word_Type
              (From_U_String (Lit_Str, Null_For_Stg_Rgn (Local_Stg_Rgn)));
         end;
      elsif Param_Type_Desc.Is_Small then
         --  Small representation
         return Literal_Value (Lit);
      else
         --  Fall back to returning a "null" value.
         --  TBD: Caller will complain, hopefully
         return Interpreter.Null_For_Stg_Rgn (Local_Stg_Rgn);
      end if;
      --  TBD: Provide additional conversion
   end Convert_String;

   procedure Fixup_Case_Alt_Key_Code
     (Visitor : in out Code_Gen_Visitor;
      Start_Case_Alt, End_Case_Alt_Key : Interpreter.Code_Index) is
      --  Fix up the skip counts in the instructions going from
      --  Start_Case_Alt+1 to End_Case_Alt_Key so they go to
      --  Visitor.Num_Instrs + 1.
      use Interpreter;
   begin
      for I in Start_Case_Alt + 1 .. End_Case_Alt_Key loop
         declare
            Next_Instr : Interpreter.Instruction renames
              Visitor.Current_Code.Instrs (I);
         begin
            case Next_Instr.Op is
               when If_Op =>
                  if Next_Instr.Skip_If_False = 0 then
                     --  Fix up skip count
                     Next_Instr.Skip_If_False := Visitor.Num_Instrs - I;
                  end if;
               when Skip_Op =>
                  if Next_Instr.Skip_Count = 0 then
                     --  Fix up skip count
                     Next_Instr.Skip_Count := Visitor.Num_Instrs - I;
                  end if;
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Fixup_Case_Alt_Key_Code;

   procedure Case_Alt_Key_Code_Gen
     (Visitor : in out Code_Gen_Visitor;
      Case_Sem : Case_Construct_Sem_Ptr;
      Case_Selector_Loc : Interpreter.Object_Locator;
      Case_Alt_Key : Optional_Tree;
      Is_Default_Case : out Boolean) is
      --  Generate sequence of "if_op" instructions needed
      --  to check the case selector against the given case-alt key.
      --  Leave skip-count 0 for those if_op's or skip_op's that
      --  are supposed to jump to next alt.
      --  Code for case alt should immediately follow code generated here.
      --  Is_Default_Case is set True if there is no way for this test to fail
      --  (i.e. it is the "[..]" case).
      use Interpreter;
      Case_Alt_Key_Tree : Trees.Tree'Class
        renames Tree_Ptr_Of (Resolved_Tree (Case_Alt_Key)).all;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      Is_Default_Case := True;  --  Assume is [..] for now

      if Case_Alt_Key_Tree in Binary.Tree then
         --  Might be "|" or ".."
         declare
            Key_Binary_Tree : Binary.Tree renames Binary.Tree (
              Case_Alt_Key_Tree);
            use Binary;
            Skip_Instr : Code_Offset := 0;
            Start_Instr : constant Code_Offset := Visitor.Num_Instrs;
         begin
            case Key_Binary_Tree.Operator is
               when Combine_Op =>
                  --  Success if case-selector matches left or right choice

                  --  Recurse on left choice
                  Case_Alt_Key_Code_Gen
                    (Visitor,
                     Case_Sem,
                     Case_Selector_Loc,
                     Key_Binary_Tree.Left_Operand,
                     Is_Default_Case);

                  Emit
                    (Visitor,
                     (Skip_Op,
                      Source_Pos => Find_Source_Pos (Case_Alt_Key),
                      Skip_Count => -1)); --  will be fixed up
                  Skip_Instr := Visitor.Num_Instrs;
                  --  Skip around second part

                  --  Go here on failure by fixing up "skip" counts
                  Fixup_Case_Alt_Key_Code
                    (Visitor,
                     Start_Instr,
                     Skip_Instr - 1);

                  --  Recurse on right choice
                  Case_Alt_Key_Code_Gen
                    (Visitor,
                     Case_Sem,
                     Case_Selector_Loc,
                     Key_Binary_Tree.Right_Operand,
                     Is_Default_Case);

                  --  Fix up skip amount
                  Visitor.Current_Code.Instrs (Skip_Instr).Skip_Count :=
                    Visitor.Num_Instrs - Skip_Instr;

                  Is_Default_Case := False;  --  Not simply [..]

                  return;   -------  all done  --------

               when Interval_Ops =>
                  --  Success if in specified interval
                  declare
                     Param_Offset : constant Offset_Within_Area :=
                       Visitor.Target_Local_Offset;
                  begin
                     if Not_Null (Key_Binary_Tree.Left_Operand) then
                        declare
                           Low_Test_VM_Info : constant VM_Obj_Id_Type :=
                             Assign_VM_Obj_Id (Visitor);
                           Low_Compare_VM_Info : constant VM_Obj_Id_Type :=
                             Assign_VM_Obj_Id
                               (Visitor, Target_VM_Num => Low_Test_VM_Info.Num,
                                         Num_Call_Params => 3);
                        begin
                           --  Leave room for result
                           Visitor.Target_Local_Offset :=
                             Visitor.Target_Local_Offset + 1;

                           Is_Default_Case := False;  --  Not simply [..]

                           --  Load case selector
                           Emit
                             (Visitor,
                              (Copy_Word_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               Destination => (Local_Area,
                                               Visitor.Target_Local_Offset,
                                               Param_VM_Obj_Id
                                                 (Low_Compare_VM_Info, 1)),
                               Dest_Name => Strings.Null_U_String_Index,
                               Source => Case_Selector_Loc,
                               Might_Be_Null => True));
                           Visitor.Target_Local_Offset :=
                             Visitor.Target_Local_Offset + 1;

                           Visitor.Target_VM_Info :=
                             Param_VM_Obj_Id (Low_Compare_VM_Info, 2);

                           --  Load low bound
                           Emit_Code_For_Resolved_Tree
                             (Key_Binary_Tree.Left_Operand,
                              Visitor);

                           --  Call "=?" operator
                           Emit
                             (Visitor,
                              (Call_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               Call_Target => Routine_Locator
                                                (Case_Sem.Op_Sem,
                                                 Case_Sem.Case_Selector_Type),
                               Target_Index =>
                                 Find_Operation_Routine_Index
                                   (Case_Sem.Op_Sem),
                               Params => (Local_Area, Param_Offset,
                                          Low_Compare_VM_Info),
                               Locked_Param_Info => Null_Locked_Param_Info,
                               Static_Link => Run_Time_Type_Info
                                 (Case_Sem.Case_Selector_Type,
                                  Referring_Module => Enc_Module),
                               Precond_Proved => False,
                               Output_Inited_Null => False));

                           Emit
                             (Visitor,
                              (If_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               If_Source => (Local_Area, Param_Offset,
                                             Low_Test_VM_Info),
                               If_Condition =>
                                 Low_Bound_Condition
                                   (Key_Binary_Tree.Operator),
                               Skip_If_False => 0));
                                             --  will be filled in later

                        end;
                     end if;

                     if Not_Null (Key_Binary_Tree.Right_Operand) then
                        --  Start over with high bound
                        declare
                           High_Test_VM_Info : constant VM_Obj_Id_Type :=
                             Assign_VM_Obj_Id (Visitor);
                           High_Compare_VM_Info : constant VM_Obj_Id_Type :=
                             Assign_VM_Obj_Id (Visitor,
                                               Target_VM_Num =>
                                                 High_Test_VM_Info.Num,
                                               Num_Call_Params => 3);
                        begin
                           Visitor.Target_Local_Offset := Param_Offset + 1;

                           Is_Default_Case := False;  --  Not simply [..]

                           --  Load case selector
                           Emit
                             (Visitor,
                              (Copy_Word_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               Destination => (Local_Area,
                                               Visitor.Target_Local_Offset,
                                               Param_VM_Obj_Id
                                                 (High_Compare_VM_Info, 1)),
                               Dest_Name => Strings.Null_U_String_Index,
                               Source => Case_Selector_Loc,
                               Might_Be_Null => True));
                           Visitor.Target_Local_Offset :=
                             Visitor.Target_Local_Offset + 1;

                           Visitor.Target_VM_Info :=
                             Param_VM_Obj_Id (High_Compare_VM_Info, 2);

                           --  Load high bound
                           Emit_Code_For_Resolved_Tree
                             (Key_Binary_Tree.Right_Operand,
                              Visitor);

                           --  Call "=?" operator
                           Emit
                             (Visitor,
                              (Call_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               Call_Target => Routine_Locator
                                                (Case_Sem.Op_Sem,
                                                 Case_Sem.Case_Selector_Type),
                               Target_Index =>
                                 Find_Operation_Routine_Index
                                   (Case_Sem.Op_Sem),
                               Params => (Local_Area, Param_Offset,
                                          High_Compare_VM_Info),
                               Locked_Param_Info => Null_Locked_Param_Info,
                               Static_Link => Run_Time_Type_Info
                                 (Case_Sem.Case_Selector_Type,
                                  Referring_Module => Enc_Module),
                               Precond_Proved => False,
                               Output_Inited_Null => False));

                           Emit
                             (Visitor,
                              (If_Op,
                               Source_Pos => Find_Source_Pos
                                               (Case_Alt_Key),
                               If_Source => (Local_Area, Param_Offset,
                                             High_Test_VM_Info),
                               If_Condition =>
                                 High_Bound_Condition
                                   (Key_Binary_Tree.Operator),
                               Skip_If_False => 0));
                                                --  will be filled in later

                        end;
                     end if;

                     --  Remember high-water mark, and restore target offset
                     Check_And_Set_Local_Offset (Visitor, Param_Offset);

                     return;   ----- all done -----
                  end;

               when others =>
                  --  Not one of the special operators ("|" or "..").
                  --  Success if equal to given value.
                  null;  --  Fall through to do default action
            end case;
         end;
      end if;

      Is_Default_Case := False;  --  Not simply [..]

      if Case_Sem.Case_Selector_Type.Is_Polymorphic then
         declare
            Choice_Param_Ref_Offset : constant Offset_Within_Area :=
              Visitor.Target_Local_Offset;
            Choice_Param_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Visitor);
            Choice_Not_Null_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Visitor);
            Choice_Param_Type : Type_Sem_Ptr;
            Choice_Param_Str : Strings.U_String := Strings.Null_U_String;
         begin
            --  Case selector is of polymorphic type

            Is_Default_Case := False;  --  Not simply "[..]"

            if Case_Alt_Key_Tree in Param_Decl.Tree then
               --  There is an explicit choice param
               declare
                  Choice_Param_Sem : constant Param_Sem_Ptr :=
                    Param_Sem_Ptr (Sem_Info (Case_Alt_Key));
               begin
                  Choice_Param_Type := Choice_Param_Sem.Resolved_Type;
                  --  Fill in location/level information on choice parameter
                  Choice_Param_Str := Choice_Param_Sem.Associated_Symbol.Str;
                  Choice_Param_Sem.Info.Obj_Location :=
                    (Local_Area, Choice_Param_Ref_Offset,
                     Choice_Param_VM_Info);

                  Choice_Param_Sem.Info.Obj_Level := Visitor.Current_Level;

                  --  Declare choice param
                  Emit
                    (Visitor,
                     (Declare_Obj_Op,
                      Source_Pos => Find_Source_Pos (Case_Alt_Key),
                      Destination => Choice_Param_Sem.Info.Obj_Location,
                      Dest_Name => Strings.Index
                        (Choice_Param_Sem.Associated_Symbol.Str),
                      Is_By_Ref => Choice_Param_Sem.Is_By_Ref_Choice_Param,
                      Is_Var => Choice_Param_Sem.Is_Var_Choice_Param,
                      Declare_Type_Info => Run_Time_Type_Info
                        (Choice_Param_Type,
                         Referring_Module => Enc_Module)));

                  if Choice_Param_Type.Is_Polymorphic then
                     --  Polymorphic choice param is *not* by-ref
                     pragma Assert
                       (not Choice_Param_Sem.Is_By_Ref_Choice_Param);
                     null;
                  else
                     --  Non-polymorphic choice param is by-ref
                     pragma Assert (Choice_Param_Sem.Is_By_Ref_Choice_Param);
                     null;
                  end if;
               end;
            else
               --  No explicit choice param, so we create one
               --  Should be an invocation
               pragma Assert (Case_Alt_Key_Tree in Invocation.Tree'Class);
               Choice_Param_Type :=
                 Resolved_Type (Prefix
                   (Invocation.Tree (Case_Alt_Key_Tree).Prefix));
            end if;

            --  Unwrap polymorphic obj, return a "ref" if not polymorphic
            Emit
              (Visitor,
               (Unwrap_Polymorphic_Obj_Op,
                Source_Pos => Find_Source_Pos (Case_Alt_Key),
                Destination => (Local_Area, Choice_Param_Ref_Offset,
                                Choice_Param_VM_Info),
                Dest_Name => Strings.Index (Choice_Param_Str),
                Source => Case_Selector_Loc,
                Might_Be_Null => True,
                Type_Info => Run_Time_Type_Info
                  (Choice_Param_Type,
                   Referring_Module => Enc_Module),
                Source_Type_Info => Run_Time_Type_Info
                  (Case_Sem.Case_Selector_Type,
                   Referring_Module => Enc_Module)));

            --  Check whether result is null.
            Emit
              (Visitor,
               (Not_Null_Op,
                Source_Pos => Find_Source_Pos (Case_Alt_Key),
                Destination => (Local_Area, Choice_Param_Ref_Offset + 1,
                                Choice_Not_Null_VM_Info),
                Dest_Name => Strings.Null_U_String_Index,
                Source => (Local_Area, Choice_Param_Ref_Offset,
                           Choice_Param_VM_Info),
                Might_Be_Null => True,
                Type_Info => Null_Object_Locator));
                                 --  Means check for null addr

            Visitor.Target_Local_Offset := Choice_Param_Ref_Offset + 2;

            --  Bypass this case alternative if is null
            Emit
              (Visitor,
               (If_Op,
                Source_Pos => Find_Source_Pos (Case_Alt_Key),
                If_Source => (Local_Area, Choice_Param_Ref_Offset + 1,
                              Choice_Not_Null_VM_Info),
                If_Condition => Boolean_Is_True,
                Skip_If_False => 0));  --  will be filled in later

            --  Remember high-water mark and set to after choice param
            Check_And_Set_Local_Offset
              (Visitor, Choice_Param_Ref_Offset + 1);

            return;   ----  All done  ---
         end;
      end if;

      --  Generate code to call compare operation
      --  on Case_Selector and given value
      declare
         Param_Offset : constant Offset_Within_Area :=
           Visitor.Target_Local_Offset;
         If_Test_VM_Info : constant VM_Obj_Id_Type :=
           Assign_VM_Obj_Id (Visitor);
         Compare_Params_VM_Info : constant VM_Obj_Id_Type :=
           Assign_VM_Obj_Id (Visitor,
             Target_VM_Num => If_Test_VM_Info.Num,
             Num_Call_Params => 3);
      begin
         --  Leave room for result
         Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
         --  Load case selector
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_Source_Pos (Case_Alt_Key),
             Destination =>
               (Local_Area, Visitor.Target_Local_Offset,
                Param_VM_Obj_Id
                  (Compare_Params_VM_Info, Param_Offset => 1)),
             Dest_Name => Strings.Null_U_String_Index,
             Source => Case_Selector_Loc,
             Might_Be_Null => True));
         Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
         Visitor.Target_VM_Info :=
           Param_VM_Obj_Id (Compare_Params_VM_Info, Param_Offset => 2);
         --  Load value
         Emit_Code_For_Resolved_Tree (Case_Alt_Key, Visitor);
         --  Call "=?" operator
         Emit
           (Visitor,
            (Call_Op,
             Source_Pos => Find_Source_Pos (Case_Alt_Key),
             Call_Target => Routine_Locator
                              (Case_Sem.Op_Sem,
                               Case_Sem.Case_Selector_Type),
             Target_Index => Find_Operation_Routine_Index (Case_Sem.Op_Sem),
             Params => (Local_Area, Param_Offset, Compare_Params_VM_Info),
             Locked_Param_Info => Null_Locked_Param_Info,
             Static_Link => Run_Time_Type_Info
                              (Case_Sem.Case_Selector_Type,
                               Referring_Module => Enc_Module),
             Precond_Proved => False,
             Output_Inited_Null => False));

         Emit
           (Visitor,
            (If_Op,
             Source_Pos => Find_Source_Pos (Case_Alt_Key),
             If_Source => (Local_Area, Param_Offset, If_Test_VM_Info),
             If_Condition => Compare_Equal,
             Skip_If_False => 0));  --  will be filled in later

         --  Remember high-water mark and restore target offset
         Check_And_Set_Local_Offset (Visitor, Param_Offset);
      end;

   end Case_Alt_Key_Code_Gen;

   procedure Check_Is_Part_Of_Ref
     (T : Optional_Tree; Decl_Region : Region_Ptr;
      Orig_T : Optional_Tree := Null_Optional_Tree) is
      --  Make sure that T is a reference to a part of some ref
      --  declared in Decl_Region.
      Obj : Optional_Tree := Resolved_Tree (T);
      T_Err : Optional_Tree := T;  --  Tree to include in error message
   begin
      if Orig_T /= Null_Optional_Tree then
         T_Err := Orig_T;
      end if;

      loop
         declare
            Obj_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Obj).all;
         begin
            if Obj_Tree in Identifier.Tree then
               --  This should be a reference declared in Decl_Region
               declare
                  Id_Sem : Sem_Ptr := Underlying_Sem_Info (Obj);
                  Id_Def_Tree : Trees.Tree'Class renames
                               Tree_Ptr_Of (Id_Sem.Definition).all;
               begin
                  if Id_Sem = null or else
                    Id_Sem.all not in Object_Semantic_Info'Class
                  then
                     Sem_Error (T_Err, "Must be name of an object");
                     return;
                  elsif Id_Sem.Associated_Symbol.Enclosing_Region.Kind =
                          Module_Region_Kind
                    and then not Static.Sym_Is_Component
                                   (Id_Sem.Associated_Symbol)
                  then
                     --  A global constant or variable is always OK
                     return;
                  elsif not Region_Encloses_Region
                      (Encloser => Decl_Region,
                       Enclosed => Id_Sem.Associated_Symbol.Enclosing_Region)
                  then
                     --  Is a "ref" to an enclosing region.
                     --  This is permitted for a "continue" but not
                     --  for a "return" statement.
                     --  TBD: In a "continue" statement this might create a
                     --       data race but won't create a dangling reference.
                     if Decl_Region.Kind = Operation_Param_Region_Kind then
                        Sem_Error (T_Err, "Must refer to a ref parameter " &
                                      "of innermost operation");
                     end if;
                     return;
                  elsif not Static.Sym_Is_Declared_Ref
                                  (Id_Sem.Associated_Symbol)
                  then
                     Sem_Error (T_Err, "Must not refer to a local object");
                     return;
                  elsif Id_Sem.Associated_Symbol.Enclosing_Region =
                    Decl_Region
                  then
                     --  Is a "ref" in the appropriate region
                     return;
                  elsif Id_Def_Tree in Param_Decl.Tree
                    and then Id_Sem.Associated_Symbol.Enclosing_Region.Kind =
                      Case_Stmt_Region_Kind
                  then
                     --  Is a case alternative ref; follow the chain
                     Obj := Resolved_Tree (Case_Construct.Tree (Tree_Ptr_Of
                              (Id_Sem.Associated_Symbol.Enclosing_Region.
                                Associated_Symbol.Definition).all).
                                  Case_Selector);
                  elsif Id_Def_Tree in Obj_Decl.Tree then
                     --  Is a local declared ref; follow the chain
                     Obj := Resolved_Tree
                              (Obj_Decl.Tree (Id_Def_Tree).Obj_Value);
                  elsif Id_Def_Tree in Iterator.Tree then
                     --  Is a reference to a loop parameter, see what
                     --  it refers to.
                     Obj := Resolved_Tree
                              (Iterator.Tree (Id_Def_Tree).Obj_Value);
                  else
                     --  TBD: What sort of reference is it?
                     Sem_Error (T_Err, "Must not refer to a local ref");
                     return;
                  end if;
               end;
            elsif Obj_Tree in Selection.Tree then
               Obj := Resolved_Tree (Selection.Tree (Obj_Tree).Prefix);
            elsif Obj_Tree in Invocation.Tree then
               --  Each passed in "ref" must also be part of some ref in region
               declare
                  Invoc_Tree : Invocation.Tree renames
                                                  Invocation.Tree (Obj_Tree);
                  Call_Sem : Call_Sem_Ptr;
                  Invoc_Sem : constant Root_Sem_Ptr := Sem_Info (Obj);
                  use Invocation;
               begin
                  if Invoc_Tree.Kind /= Operation_Call
                    or else Invoc_Sem.all not in Call_Semantic_Info
                  then
                     Sem_Error (T_Err, "Must be name of an object");
                     return;
                  end if;

                  Call_Sem := Call_Sem_Ptr (Invoc_Sem);
                  declare
                     Target_Tree : Trees.Tree'Class renames
                        Tree_Ptr_Of (Call_Sem.Op_Sem.Definition).all;
                  begin
                     if Target_Tree not in Operation.Tree then
                        --  TBD: Give up trying to check when is indirect call.
                        return;
                     end if;
                     declare
                        Op_Tree : Operation.Tree renames
                                                  Operation.Tree (Target_Tree);
                        Inputs : constant Lists.List :=
                                            Op_Tree.Operation_Inputs;
                        Outputs : constant Lists.List :=
                                            Op_Tree.Operation_Outputs;
                     begin
                        if Lists.Length (Outputs) /= 1 then
                           --  TBD: Give up checking if not a single output
                           return;
                        elsif Param_Decl.Tree (Tree_Ptr_Of
                          (Lists.Nth_Element (Outputs, 1)).all).Kind not in
                             Param_Decl.Ref_Param_Kinds
                        then
                           Sem_Error (T_Err, "Must be part of a ref");
                           return;
                        end if;

                        --  Check each of the declared-ref inputs
                        for I in 1 .. Lists.Length (Inputs) loop
                           declare
                              Inp : Param_Decl.Tree renames
                                Param_Decl.Tree (Tree_Ptr_Of
                                  (Lists.Nth_Element (Inputs, I)).all);
                           begin
                              if Inp.Kind in Param_Decl.Ref_Param_Kinds then
                                 Check_Is_Part_Of_Ref
                                   (Lists.Nth_Element (Invoc_Tree.Operands, I),
                                    Decl_Region, Orig_T => T_Err);
                              end if;
                           end;
                        end loop;

                        --  All done
                        return;
                     end;
                  end;
               end;
            else
               --  Not rooted at an object?
               Sem_Error (T_Err, "Must be name of an object");
               return;
            end if;
         end;
      end loop;
   end Check_Is_Part_Of_Ref;

   function Iterator_Loop_Param_Sem (Iter_Sem : Iterator_Sem_Ptr)
     return Param_Sem_Ptr is
   --  Return Param_Sem for loop param that is actually passed to
   --  each iteration of a loop.  The other information that might be
   --  needed on each iteration, such as the "V" of [K => V], can
   --  be computed from up-level information and the "official" loop param.
   --  For Set_Iterators and Value_Iterators, this just returns Iter_Sem
   --  converted to Param_Sem_Ptr type.
   --  For Container_Iterators, this returns Iter_Sem.Key_Sem.
      use Iterator;
   begin
      case Iter_Sem.Iterator_Kind is
         when Container_Iterator =>
            return Iter_Sem.Key_Sem;
         when Set_Iterator | Value_Iterator =>
            return Param_Sem_Ptr (Iter_Sem);
      end case;
   end Iterator_Loop_Param_Sem;

   procedure Generate_Next_Value
     (Loop_Visitor : in out Code_Gen_Visitor;
      Iter_Sem : Iterator_Sem_Ptr;
      Next_Value_Index : Natural;
      Next_Value_Location : out Interpreter.Object_Locator;
      Value_Test_Instruction : out Interpreter.Code_Offset;
      Continue_With_Value : Optional_Tree := Null_Optional_Tree;
      Index_Set_Might_Be_Null : Boolean := True) is
      --  Generate next value for iterator and return its location in
      --  Next_Value_Location
      use Interpreter;
      use Iterator;
      T : Iterator.Tree renames Iterator.Tree (Tree_Ptr_Of
                                                  (Iter_Sem.Definition).all);
      Loop_Var_Is_By_Ref : constant Boolean :=
        Static.Sym_Is_By_Ref (Iter_Sem.Associated_Symbol);
      Iterator_Index_Type : Type_Sem_Ptr;
         --  Not used for Value_Iterator

      Index_Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index;
         --  Used for Set_Iterator and for [Key => Val] container iterator

      Value_Dest_Name : constant Strings.U_String_Index :=
        Strings.Index (Iter_Sem.Associated_Symbol.Str);
         --  Used for Container_Iterator and Value_iterator.

      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Loop_Visitor.Decl_Region);
      If_Test_VM_Info : constant VM_Obj_Id_Type :=
        Assign_VM_Obj_Id (Loop_Visitor);
      Next_Value_Init_VM_Info : VM_Obj_Id_Type;

      function Remove_Func_To_Use return Operation_Sem_Ptr
      --  Return Remove_Any/First/Last based on Iterator_Direction;
      --  unless we are inside a container aggregate, in which case
      --  we default to "Remove_First" if it is available,
      --  or if we are inside a concurrent loop with multiple
      --  iterators, in which case we use Remove_First as well.
      is
         use type Interpreter.Direction;
      begin
         if (Iter_Sem.Enclosing_For_Loop.Enclosing_Container_Agg_Sem /= null
               and then
             Iter_Sem.Iterator_Direction = Interpreter.Unordered_Dir)
           or else
            (Iter_Sem.Iterator_Direction = Interpreter.Concurrent_Dir
               and then
             Iter_Sem.Enclosing_For_Loop.Iterator_Sems'Length > 1)
         then
            declare
               Remove_First_Func : constant Operation_Sem_Ptr :=
                 Static.Get_Remove_Func
                    (Iter_Sem.Index_Set_Type, Interpreter.Forward_Dir);
            begin
               if Remove_First_Func /= null then
                  --  Default to "Remove_First" for iterator in container agg
                  --  or iterator in concurrent loop with multiple iterators.
                  return Remove_First_Func;
               end if;
            end;
         end if;

         --  Get specified Remove_* func
         return Static.Get_Remove_Func
           (Iter_Sem.Index_Set_Type, Iter_Sem.Iterator_Direction);
      end Remove_Func_To_Use;

   begin   --  Generate_Next_Value

      --  Generate code to spawn next iteration.
      --  If no more iterations, then skip rest of iterators.

      --  Determine iterator index type
      case T.Kind is
         when Set_Iterator =>
            --  Index and element are one and the same with a set
            Iterator_Index_Type := Iter_Sem.Resolved_Type;
            Index_Dest_Name := Value_Dest_Name;
         when Container_Iterator =>
            --  Index type is the type of the "key"
            Iterator_Index_Type := Iter_Sem.Key_Sem.Resolved_Type;
            if T.Kind = Each_Key_Value then
               --  Name of index is based on key
               Index_Dest_Name :=
                 Strings.Index (Iter_Sem.Key_Sem.Associated_Symbol.Str);
            end if;
         when Value_Iterator =>
            --  No index type defined.
            Iterator_Index_Type := null;
      end case;

      Next_Value_Location := (Local_Area, Loop_Visitor.Target_Local_Offset,
                              No_VM_Obj_Id);

      case T.Kind is
         when Set_Iterator | Container_Iterator =>
            --  Call Remove_First/Last/Any on the index set.
            --  If null, skip other iterators and parallel call.

            --  Init the VM info
            Next_Value_Location.VM_Obj_Id :=
              Assign_VM_Obj_Id (Loop_Visitor, Needs_Var => True);
            Next_Value_Init_VM_Info := Assign_VM_Obj_Id (Loop_Visitor,
              Needs_Var => True,
              Target_VM_Num => Next_Value_Location.VM_Obj_Id.Num,
              Num_Call_Params => 2);

            Emit
              (Loop_Visitor,
               (Declare_Obj_Op,
                Source_Pos => Iterator.Find_Source_Pos (T),
                Destination => (Next_Value_Location.Base,
                                Next_Value_Location.Offset,
                                Next_Value_Init_VM_Info),
                Dest_Name => Index_Dest_Name,
                Is_By_Ref => False,
                Is_Var => False,
                Declare_Type_Info => Run_Time_Type_Info
                                    (Iterator_Index_Type,
                                     Referring_Module => Enc_Module)));

            if not Static.Known_To_Be_Small (Iterator_Index_Type) then
               --  Initialize to null
               Emit
                 (Loop_Visitor,
                  (Store_Null_Of_Same_Stg_Rgn_Op,
                   Source_Pos => Iterator.Find_Source_Pos (T),
                   Destination => (Local_Area,
                                   Loop_Visitor.Target_Local_Offset,
                                   Param_VM_Obj_Id
                                     (Next_Value_Init_VM_Info,
                                      Param_Offset => 0)),
                   Dest_Name => Index_Dest_Name,
                   Source => Adjust_For_Level_And_Prefix
                               (Loop_Visitor.Current_Level,
                                Iter_Sem.Index_Set_Location,
                                Iter_Sem.Enclosing_For_Loop.For_Loop_Level),
                   Might_Be_Null => True,
                   Type_Info => Run_Time_Type_Info
                                  (Iterator_Index_Type,
                                   Referring_Module => Enc_Module)));
            else
               --  Store a "small" null ("local" is irrelevant when small)
               Emit
                 (Loop_Visitor,
                  (Store_Local_Null_Op,
                   Source_Pos => Iterator.Find_Source_Pos (T),
                   Destination => (Local_Area,
                                   Loop_Visitor.Target_Local_Offset,
                                   Param_VM_Obj_Id
                                     (Next_Value_Init_VM_Info,
                                      Param_Offset => 0)),
                   Dest_Name => Index_Dest_Name,
                   Null_Type_Info => Run_Time_Type_Info
                                       (Iterator_Index_Type,
                                        Referring_Module => Enc_Module)));
            end if;

            Loop_Visitor.Target_Local_Offset :=
              Loop_Visitor.Target_Local_Offset + 1;

            --  Pass the index set by reference
            Emit
              (Loop_Visitor,
               (Store_Address_Op,
                Source_Pos => Find_Source_Pos (T.Obj_Value),
                Destination => (Local_Area, Loop_Visitor.Target_Local_Offset,
                                Param_VM_Obj_Id
                                  (Next_Value_Init_VM_Info,
                                   Param_Offset => 1)),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Adjust_For_Level_And_Prefix
                            (Loop_Visitor.Current_Level,
                             Iter_Sem.Index_Set_Location,
                             Iter_Sem.Enclosing_For_Loop.For_Loop_Level),
                Might_Be_Null => Index_Set_Might_Be_Null));

            Loop_Visitor.Target_Local_Offset :=
              Loop_Visitor.Target_Local_Offset + 1;

            --  Call appropriate "Remove_*" func to get index value
            Emit
              (Loop_Visitor,
               (Call_Op,
                Source_Pos => Find_Source_Pos (T.Obj_Value),
                Call_Target =>
                   Routine_Locator
                     (Remove_Func_To_Use, Iter_Sem.Index_Set_Type),
                Target_Index =>
                  Find_Operation_Routine_Index (Remove_Func_To_Use),
                Params => (Next_Value_Location.Base,
                           Next_Value_Location.Offset,
                           Next_Value_Init_VM_Info),
                Locked_Param_Info => Locked_Param_Info_For_Container
                  (Container_Type => Iter_Sem.Index_Set_Type,
                   Container_Param_Index => 2,
                   Container_Is_Var      => True),
                Static_Link => Run_Time_Type_Info
                                 (Iter_Sem.Index_Set_Type,
                                  Referring_Module => Enc_Module),
                Precond_Proved => False,
                Output_Inited_Null => True));

            --  See whether index is null
            Emit
              (Loop_Visitor,
               (Not_Null_Op,
                Source_Pos => Find_Source_Pos (T.Obj_Value),
                Destination => (Local_Area,
                                Loop_Visitor.Target_Local_Offset - 1,
                                If_Test_VM_Info),
                Dest_Name => Strings.Null_U_String_Index,
                Source => Next_Value_Location,
                Might_Be_Null => True,
                Type_Info => Run_Time_Type_Info
                               (Iterator_Index_Type,
                                Referring_Module => Enc_Module)));

            --  Skip around parallel call if is null
            Emit
              (Loop_Visitor,
               (If_Op,
                Source_Pos => Iterator.Find_Source_Pos (T),
                If_Source => (Local_Area,
                              Loop_Visitor.Target_Local_Offset - 1,
                              If_Test_VM_Info),
                If_Condition => Boolean_Is_True,  --  if True
                Skip_If_False => 0));  --  will be fixed up later

            --  Remember instruction needing fixup
            Value_Test_Instruction := Loop_Visitor.Num_Instrs;

            --  Remember the high-water mark and restore local offset
            Check_And_Set_Local_Offset
              (Loop_Visitor,
               Loop_Visitor.Target_Local_Offset - 1);

         when Value_Iterator =>
            --  Evaluate Nth next value, where N is given by
            --  Next_Value_Index (reduced modulo the number
            --  of next values).  TBD: If there are multiple iterators,
            --  then each one had better have the same number of
            --  "next" values, or it needs to be a concurrent loop, so
            --  we can advance one iterator multiple times.
            --  And what if the "while" condition is satisfied for one
            --  of them but not for the other?
            --  That whole iteration is skipped?  But do we advance
            --  the other iterators?
            declare
               Next_Value : Optional_Tree := Null_Optional_Tree;
            begin
               --  Init the VM info for the temp holding the next value
               Next_Value_Location.VM_Obj_Id :=
                 Assign_VM_Obj_Id (Loop_Visitor,
                                   Needs_Var => not Static.Known_To_Be_Small
                                                   (Iter_Sem.Resolved_Type));

               Emit
                 (Loop_Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Iterator.Find_Source_Pos (T),
                   Destination => Next_Value_Location,
                   Dest_Name => Value_Dest_Name,
                   Is_By_Ref => False,
                   Is_Var => False,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (Iter_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));

               case Iter_Sem.Num_Next_Values is
                  when 0 =>
                     if Is_Null (Continue_With_Value) then
                        Sem_Error
                          (T,
                           "Iterator does not specify a ""next"" value");
                     else
                        Next_Value := Continue_With_Value;
                     end if;
                  when 1 =>
                     --  Always use the only next value
                     Next_Value := Lists.Nth_Element (T.Next_Values, 1);
                  when others =>
                     --  Get appropriate next value.
                     if Next_Value_Index > Iter_Sem.Num_Next_Values then
                        Sem_Error
                          (T,
                           "Iterator does not specify enough ""next"" values; "
                           &
                           "expecting" &
                           Natural'Image (Next_Value_Index) &
                           " values");
                     else
                        Next_Value :=
                           Lists.Nth_Element
                             (T.Next_Values,
                              Next_Value_Index);
                     end if;
               end case;

               Value_Test_Instruction := 0;
               --  Will be set if there is a "while" condition

               if Next_Value = Null_Optional_Tree then
                  --  Nothing more to be done; error already reported.
                  return;
               end if;

               if Debug_Code_Gen then
                  Put_Line (" Computing next value for " & Subtree_Image (T));
                  Put_Line
                    (" into location " &
                     Obj_Locator_Image (Next_Value_Location));
               end if;

               if Loop_Var_Is_By_Ref then
                  --  Make sure that the next ref is part of a loop variable
                  Check_Is_Part_Of_Ref
                    (Next_Value,
                     Iter_Sem.Enclosing_For_Loop.Loop_Param_Region);

                  --  By-ref, get address of next object
                  Loop_Visitor.Is_Lvalue_Context := True;
                  Emit_Code_For_Resolved_Tree (Next_Value, Loop_Visitor);
                  Loop_Visitor.Is_Lvalue_Context := False;
                  Emit
                    (Loop_Visitor,
                     (Store_Address_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Destination => Next_Value_Location,
                      Dest_Name => Value_Dest_Name,
                      Source => Loop_Visitor.Lvalue_Location,
                      Might_Be_Null => True));
               else
                  if not Static.Known_To_Be_Small
                           (Iter_Sem.Resolved_Type)
                  then
                     --  Initialize temp to null for region of master/tcb
                     Emit
                       (Loop_Visitor,
                        (Store_Large_Local_Null_Op,
                         Source_Pos => Iterator.Find_Source_Pos (T),
                         Destination => Next_Value_Location,
                         Dest_Name => Value_Dest_Name,
                         Local_Addr =>
                            Adjust_For_Level_And_Prefix
                              (Loop_Visitor.Current_Level,
                               (Local_Area, 0, No_VM_Obj_Id),
                               Iter_Sem.Enclosing_For_Loop.For_Loop_Level)));
                  end if;

                  --  Copy appropriate next value into next-value location.
                  Loop_Visitor.Target_Object := Next_Value_Location;
                  Loop_Visitor.Target_VM_Info :=
                    Next_Value_Location.VM_Obj_Id;
                  Emit_Code_For_Resolved_Tree (Next_Value, Loop_Visitor);
                  Loop_Visitor.Target_Object := Null_Object_Locator;
               end if;

               if Not_Null (T.While_Cond) then
                  --  Iteration is conditional.
                  --  If condition not satisfied, then need to
                  --  jump around rest of iterator initializations
                  --  and parallel-call to initiate next iteration.
                  declare
                     Old_Location : constant Object_Locator :=
                       Iter_Sem.Info.Obj_Location;
                     --  Save old location of loop variable
                     Old_Level : constant Static_Level :=
                       Iter_Sem.Info.Obj_Level;
                  begin
                     --  Adjust location of loop parameter to correspond to
                     --  temp holding next value.
                     Iter_Sem.Info.Obj_Location := Next_Value_Location;
                     Iter_Sem.Info.Obj_Level := Loop_Visitor.Current_Level;

                     Loop_Visitor.Target_VM_Info := If_Test_VM_Info;
                     Emit_Code_For_Resolved_Tree (T.While_Cond, Loop_Visitor);

                     --  Emit an "if" op
                     Emit
                       (Loop_Visitor,
                        (If_Op,
                         Source_Pos => Find_Source_Pos (T.While_Cond),
                         If_Source => (Local_Area,
                                       Loop_Visitor.Target_Local_Offset - 1,
                                       If_Test_VM_Info),
                         If_Condition => Boolean_Is_True,  --  if True
                         Skip_If_False => 0));  --  will be fixed up later
                     Value_Test_Instruction := Loop_Visitor.Num_Instrs;

                     --  Remember the high-water mark and restore local offset
                     Check_And_Set_Local_Offset
                       (Loop_Visitor,
                        Next_Value_Location.Offset + 1);

                     --  Restore location of loop variable to be parameter
                     Iter_Sem.Info.Obj_Location := Old_Location;
                     Iter_Sem.Info.Obj_Level := Old_Level;
                  end;
               end if;
            end;
      end case;
   end Generate_Next_Value;

   procedure Add_Skip_Count
     (Visitor    : in out Code_Gen_Visitor;
      Waiter     : Instr_Loc;
      Skip_Count : Interpreter.Code_Offset) is
   --   Add skip count to list of non-zero skip counts associated with Waiter.
      use Interpreter;
      Instr : Instruction renames Visitor.Nested_Blocks.Info
                                    (Waiter.Block).Code.Instrs (Waiter.Instr);

      procedure Free is new Ada.Unchecked_Deallocation
        (Code_Offset_Array, Code_Offset_Array_Ptr);

   begin
      if Skip_Count /= 0 then
         declare
            Old_Skips : Code_Offset_Array_Ptr := Instr.Skip_Counts;
         begin
            if Old_Skips = null then
               --  First skip count for this waiter.
               Instr.Skip_Counts := new Code_Offset_Array'(1 => Skip_Count);
            else
               for I in Old_Skips'Range loop
                  if Skip_Count = Old_Skips (I) then
                     --  Already in list
                     return;
                  end if;
               end loop;
               --  Not in list yet.  Make a bigger list
               Instr.Skip_Counts :=
                 new Code_Offset_Array (Old_Skips'First .. Old_Skips'Last + 1);

               --  Copy over the old ones
               Instr.Skip_Counts (Old_Skips'Range) := Old_Skips.all;

               --  Add the new one
               Instr.Skip_Counts (Instr.Skip_Counts'Last) := Skip_Count;

               --  Reclaim storage for old list
               Free (Old_Skips);
            end if;
         end;
      end if;
   end Add_Skip_Count;

   procedure Fix_Up_Exits
     (Visitor : in out Code_Gen_Visitor;
      Comp_Sem : Composite_Stmt_Sem_Ptr;
      Target_Instr : Interpreter.Code_Offset) is
      --  Fix up all of the exit statement skip-counts
      use Interpreter;
   begin
      pragma Assert (Comp_Sem.Level /= 0);  --  Make sure level filled in

      if Comp_Sem.Num_Exits_Emitted > 0 then
         --  Fix up skip counts in "local" exits
         --  (which are implemented with skip_ops),
         --  and skip counts in non-local exit-ops.
         for I in 1 .. Comp_Sem.Num_Exits_Emitted loop
            declare
               Next_Exit_Loc : Instr_Loc renames Comp_Sem.Exit_Locs (I);
               Nested_Block_Info : Block_Info renames
                 Visitor.Nested_Blocks.Info (Next_Exit_Loc.Block);
               Instr : Instruction renames Nested_Block_Info.Code.Instrs
                 (Next_Exit_Loc.Instr);
            begin
               case Instr.Op is
                  when Skip_Op =>
                     --  Skip-ops only within block
                     pragma Assert
                       (Next_Exit_Loc.Block = Visitor.Current_Block);

                     --  Fix up this skip_op
                     Instr.Skip_Count :=
                       Visitor.Num_Instrs - Next_Exit_Loc.Instr;

                  when If_Op =>
                     --  If-ops only within block
                     pragma Assert
                       (Next_Exit_Loc.Block = Visitor.Current_Block);

                     --  Fix up this if_op
                     Instr.Skip_If_False :=
                       Visitor.Num_Instrs - Next_Exit_Loc.Instr;

                  when Exit_Op =>
                     --  Exit-ops only used to exit blocks
                     pragma Assert
                       (Next_Exit_Loc.Block > Visitor.Current_Block);

                     --  Find Waiter in current block, and compute
                     --  offset from that.
                     declare
                        Waiter : Instr_Loc :=
                          Nested_Block_Info.Waiter;
                     begin
                        while Waiter.Block /=
                          Visitor.Current_Block
                        loop
                           Waiter :=
                             Visitor.Nested_Blocks.Info (Waiter.Block).Waiter;
                        end loop;

                        --  Set Exit_Op skip count based on Waiter.
                        Instr.Skip_Count := Visitor.Num_Instrs - Waiter.Instr;

                        if Instr.Skip_Count /= 0 then
                           --  Oh dear, we have a non-zero skip count.
                           --  Add it to the list of skip counts hanging
                           --  off the "waiter."
                           Add_Skip_Count (Visitor, Waiter, Instr.Skip_Count);
                        end if;

                     end;

                  when others =>
                     if Next_Exit_Loc.Instr <= 1 and then
                       Next_Exit_Loc.Block = 0
                     then
                        --  NOTE: Must have had a prior error if
                        --        Loc is still (0, 1).
                        Sem_Error (Comp_Sem.Definition,
                          "Internal: Fix_Up_Exits found uninitialized " &
                          "location for Exit statement");
                     else
                        Sem_Error (Comp_Sem.Definition,
                          "Internal: Fix_Up_Exits expected Skip_Op or " &
                          "Exit_Op, found: " & Opcode_Enum'Image (Instr.Op));
                     end if;
               end case;
            end;
         end loop;
      end if;
   end Fix_Up_Exits;

   procedure Fix_Up_Continues
     (Visitor : in out Code_Gen_Visitor;
      Comp_Sem : Composite_Stmt_Sem_Ptr;
      Target_Instr : Interpreter.Code_Offset) is
      --  Fix up all of the continue statement skip-counts
      use Interpreter;
   begin
      pragma Assert (Comp_Sem.Level /= 0);  --  Make sure level filled in

      if Comp_Sem.Num_Continues_Emitted > 0 then
         --  Fix up skip counts in "local" continues
         --  (which are implemented with skip_ops),
         --  and skip counts in non-local exit-ops.
         for I in 1 .. Comp_Sem.Num_Continues_Emitted loop
            declare
               Next_Continue_Loc : Instr_Loc renames
                 Comp_Sem.Continue_Locs (I);
               Nested_Block_Info : Block_Info renames
                 Visitor.Nested_Blocks.Info (Next_Continue_Loc.Block);
               Instr : Instruction renames Nested_Block_Info.Code.Instrs
                 (Next_Continue_Loc.Instr);
            begin
               case Instr.Op is
                  when Skip_Op =>
                     --  Skip-ops only within block
                     pragma Assert
                       (Next_Continue_Loc.Block = Visitor.Current_Block);

                     --  Fix up this skip_op
                     Instr.Skip_Count :=
                       Visitor.Num_Instrs - Next_Continue_Loc.Instr;

                  when Exit_Op =>
                     --  Exit-ops only used to continue a for-loop
                     --  or a while-loop where the continue is inside a
                     --  nested block.
                     if Comp_Sem.all not in
                         For_Loop_Construct_Semantic_Info
                       or else
                         Static.Num_Initial_Value_Iterators
                           (For_Loop_Construct_Sem_Ptr (Comp_Sem)) = 0
                     then
                        --  Fix up continue that crosses blocks
                        --  for a loop other than an initial-value for-loop.
                        pragma Assert
                          (Next_Continue_Loc.Block > Visitor.Current_Block);

                        --  Find Waiter in current block, and compute
                        --  offset from that.
                        declare
                           Waiter : Instr_Loc :=
                             Nested_Block_Info.Waiter;
                        begin
                           while Waiter.Block /=
                             Visitor.Current_Block
                           loop
                              Waiter :=
                                Visitor.Nested_Blocks.Info
                                  (Waiter.Block).Waiter;
                           end loop;

                           --  Set Exit_Op skip count based on Waiter.
                           Instr.Skip_Count :=
                             Visitor.Num_Instrs - Waiter.Instr;

                           if Instr.Skip_Count /= 0 then
                              --  Oh dear, we have a non-zero skip count.
                              --  Add it to the list of skip counts hanging
                              --  off the "waiter."
                              Add_Skip_Count
                                (Visitor, Waiter, Instr.Skip_Count);
                           end if;
                        end;
                     end if;

                  when others =>
                     if Next_Continue_Loc.Instr <= 1 and then
                       Next_Continue_Loc.Block = 0
                     then
                        if Debug_Code_Gen then
                           --  NOTE: This is now normal because
                           --        not all continues get a skip/exit.
                           Put_Line
                             ("Fix_Up_Continues found 0/1 " &
                              "location for Continue statement: " &
                              Subtree_Image (Comp_Sem.Definition));
                        end if;
                     else
                        Sem_Error (Comp_Sem.Definition,
                          "Internal: Fix_Up_Continues expected Skip_Op or " &
                          "Exit_Op, found: " & Opcode_Enum'Image (Instr.Op));
                     end if;
               end case;
            end;
         end loop;
      end if;
   end Fix_Up_Continues;

   procedure Next_For_Loop_Iteration
     (Loop_Visitor : in out Code_Gen_Visitor;
      For_Loop_Sem : For_Loop_Construct_Sem_Ptr;
      Num_Next_Values : Natural := 1;
      Continue_With_Values : Optional_Tree := Null_Optional_Tree) is
      --  Generate the next iteration(s) for the given loop.
      --  If Num_Next_Values is > 1, then this is how many
      --  "next" values to generate, one for each value given
      --  after "then" in an initial/next value iterator.
      --  If Continue_With_Values is non-null, then this is
      --  being called as part of a "continue" statement, and
      --  Continue_With_Values is the canonical class aggregate
      --  to use for generating next values for the simple
      --  value iterators.
      --  Num_Continues_Emitted must be incremented *before*
      --  calling this routine, if Continue_With_Values is non-null.
      use Interpreter;
      Cur_Offset : constant Offset_Within_Area :=
        Loop_Visitor.Target_Local_Offset;
      Loop_Body_Info : Block_Info renames Loop_Visitor.Nested_Blocks.Info
        (For_Loop_Sem.Loop_Body_Block);

      Num_Values_Needed : constant Natural :=
        Static.Num_Initial_Value_Iterators (For_Loop_Sem);
      --  Number of values that need to be specified
      --  after the "continue loop with "

      function Nth_Continue_Value (N : Positive) return Optional_Tree is
      --  Return Nth value from canonicalized list of values from
      --  the "continue loop with ..." statement
      begin
         if Not_Null (Continue_With_Values) then
            return Lists.Nth_Element (Invocation.Tree (Tree_Ptr_Of
              (Resolved_Tree (Continue_With_Values)).all).Operands, N);
         else
            --  No values specified
            return Null_Optional_Tree;
         end if;
      end Nth_Continue_Value;

      function Param_Offset (Iter_Index : Positive)
        return Offset_Within_Area is
         --  return offset in param area to use for given iterator
         --  This is only used when body is a nested block
         pragma Assert
           (For_Loop_Sem.Uses_Parallel_Nested_Block);
         Iter_Sem : constant Iterator_Sem_Ptr :=
           For_Loop_Sem.Iterator_Sems (Iter_Index);
         Loop_Param : constant Param_Sem_Ptr :=
           Iterator_Loop_Param_Sem (Iter_Sem);
      begin
         pragma Assert
           (Loop_Param.Info.Obj_Location.Base = Param_Area);
         return Loop_Param.Info.Obj_Location.Offset;
      end Param_Offset;

   begin  --  Next_For_Loop_Iteration

      if Num_Values_Needed > 0 then
         if Is_Null (Continue_With_Values) then
            --  No next iteration since there are
            --  no values provided.
            --  Without a "continue" statement, this kind of
            --  loop "poops out" after just one iteration.
            --  TBD: Produce a warning for a continue-less loop
            --      with any "initial-value-only" value iterators.

            return;  --  Done with loop  --
         end if;

         if Num_Next_Values > 1 then
            Sem_Error
              (Continue_With_Values,
               "Continue not permitted when an iterator has multiple" &
               " next values specified");
            return;
         end if;
      elsif Not_Null (Continue_With_Values) then
         --  Shouldn't have any values specified in a continue statement
         Sem_Error
           (Continue_With_Values,
            "No iterators need a next value specified in continue stmt");
         return;
      end if;

      --  Loop over each "next" value for each iterator
      for Next_Val_Index in 1 .. Num_Next_Values loop
         declare
            Tcb_Offset : constant Offset_Within_Area := Cur_Offset;
            Create_Tcb_VM_Info : VM_Obj_Id_Type (Local_Kind);
            Add_Parallel_VM_Info : VM_Obj_Id_Type (Local_Kind);
            Invoker_Index : Positive := Next_Val_Index;
            Continue_Value_Index : Natural := 0;
            Next_Value_Locators : Obj_Locator_Array
              (For_Loop_Sem.Iterator_Sems'Range);
         begin
            Loop_Visitor.Target_Local_Offset :=
              Loop_Visitor.Target_Local_Offset + 1;

            --  Generate next values for each iterator
            for Iter_Index in For_Loop_Sem.Iterator_Sems'Range loop
               declare
                  Iter_Sem : constant Iterator_Sem_Ptr :=
                    For_Loop_Sem.Iterator_Sems (Iter_Index);
                  Iter_Tree : Iterator.Tree
                    renames Iterator.Tree
                      (Tree_Ptr_Of (Iter_Sem.Definition).all);
                  Next_Value_Location : Object_Locator;

                  use type Iterator.Iterator_Kind_Enum;
               begin
                  --  This computes the next value
                  if Iter_Tree.Kind = Iterator.Initial_Value then
                     --  Value provided by the "continue" statement
                     Continue_Value_Index := Continue_Value_Index + 1;
                     Generate_Next_Value
                       (Loop_Visitor,
                        Iter_Sem,
                        Next_Val_Index,
                        Next_Value_Location => Next_Value_Location,
                        Value_Test_Instruction => Iter_Sem.Next_Value_Test,
                        Continue_With_Value =>
                          Nth_Continue_Value (Continue_Value_Index));
                  else
                     Generate_Next_Value
                       (Loop_Visitor,
                        Iter_Sem,
                        Next_Val_Index,
                        Next_Value_Location => Next_Value_Location,
                        Value_Test_Instruction => Iter_Sem.Next_Value_Test,
                        Index_Set_Might_Be_Null => False);
                          --  By this time presume index_set is non-null
                          --  NOTE: It might be null, but only if Remove_*
                          --        allows it to be null, since it was last
                          --        updated by Remove_*.
                          --  TBD: Change this to to match optionality
                          --       of Remove_* set parameter.
                  end if;

                  --  Remember next value location in array for later use.
                  --  We don't want to overwrite the loop param yet
                  --  in case it is used to compute the next value for
                  --  other loop params.
                  Next_Value_Locators (Iter_Index) := Next_Value_Location;
               end;
            end loop;

            if For_Loop_Sem.Uses_Parallel_Nested_Block then
               --  Allocate space for TCB out of master's region

               --  But first set up the VM info
               Create_Tcb_VM_Info := Assign_VM_Obj_Id (Loop_Visitor);

               Add_Parallel_VM_Info :=
                 Assign_VM_Obj_Id (Loop_Visitor,
                                   Needs_Var => True,
                                   Target_VM_Num => Create_Tcb_VM_Info.Num,
                                   Num_Call_Params =>
                                     For_Loop_Sem.Iterator_Sems'Length);

               Emit
                 (Loop_Visitor,
                  (Create_Tcb_Op,
                   Source_Pos => Find_Source_Pos (For_Loop_Sem.Definition),
                   Parallel_Master =>
                      Adjust_For_Level_And_Prefix
                        (Loop_Visitor.Current_Level,
                         (Local_Area, For_Loop_Sem.For_Loop_Master,
                          No_VM_Obj_Id),
                         For_Loop_Sem.For_Loop_Level),
                   Parallel_Control =>
                     (Local_Area, Tcb_Offset, Create_Tcb_VM_Info),
                   Parallel_Static_Link => Adjust_For_Level_And_Prefix
                                             (Loop_Visitor.Current_Level,
                                              (Local_Area, 0, No_VM_Obj_Id),
                                              For_Loop_Sem.For_Loop_Level),
                   Num_In_Params  => For_Loop_Sem.Iterator_Sems'Length,
                   Num_Out_Params => 0));

               --  Copy new values into the param area following the TCB
               for I in Next_Value_Locators'Range loop
                  Emit
                    (Loop_Visitor,
                     (Copy_Word_Op,
                      Source_Pos => Find_Source_Pos
                        (For_Loop_Sem.Iterator_Sems (I).Definition),
                      Destination => (Phys_Base_Register (Tcb_Offset),
                                      Thread_Control_Block_Size +
                                        Param_Offset (I),
                                      Param_VM_Obj_Id (Add_Parallel_VM_Info,
                                        Natural (Param_Offset (I)))),
                      Dest_Name => Strings.Null_U_String_Index,
                      Source => Next_Value_Locators (I),
                      Might_Be_Null => True));
               end loop;

               --  Generate call using new set of values
               Emit
                 (Loop_Visitor,
                  (Add_Parallel_Op,
                   Source_Pos => Find_Source_Pos (For_Loop_Sem.Definition),
                   Parallel_Master =>
                      Adjust_For_Level_And_Prefix
                        (Loop_Visitor.Current_Level,
                         (Local_Area, For_Loop_Sem.For_Loop_Master,
                          No_VM_Obj_Id),
                         For_Loop_Sem.For_Loop_Level),
                   Parallel_Control => (Phys_Base_Register (Tcb_Offset), 0,
                                        Add_Parallel_VM_Info),
                                          --  This has a special meaning
                                          --  that the VM reg has already
                                          --  been initialized.
                   Parallel_Static_Link => Adjust_For_Level_And_Prefix
                                             (Loop_Visitor.Current_Level,
                                              (Local_Area, 0, No_VM_Obj_Id),
                                              For_Loop_Sem.For_Loop_Level),
                   Num_In_Params  => For_Loop_Sem.Iterator_Sems'Length,
                   Num_Out_Params => 0,
                   Parallel_Code_Block => Null_Code_Block_Descriptor));
                     --  will be fixed up at end of operation
                     --  and in Emit_Nested_Block_Finish

               if Not_Null (Continue_With_Values) then
                  --  Use the "continues" index as the invoker index.
                  Invoker_Index := For_Loop_Sem.Num_Continues_Emitted;
                  pragma Assert
                    (Invoker_Index <= For_Loop_Sem.Num_Continues);
               end if;

               --  Remember this invoker of nested block
               if Debug_Code_Gen then
                  Put_Line
                    (" Adding invoker #" &
                     Natural'Image (Invoker_Index) &
                     " of block" &
                     Block_Index'Image (For_Loop_Sem.Loop_Body_Block) &
                     ", invoked by block" &
                     Block_Index'Image (Loop_Visitor.Current_Block) &
                     ", PC" &
                     Code_Offset'Image (Loop_Visitor.Num_Instrs));
               end if;
               Loop_Body_Info.Invokers (Invoker_Index).Block :=
                 Loop_Visitor.Current_Block;
               Loop_Body_Info.Invokers (Invoker_Index).Instr :=
                 Loop_Visitor.Num_Instrs;

            else

               --  Not using a parallel nested block for the loop body.

               --  Copy new values into their "official" locations.
               for I in Next_Value_Locators'Range loop
                  Emit
                    (Loop_Visitor,
                     (Copy_Word_Op,
                      Source_Pos => Find_Source_Pos
                        (For_Loop_Sem.Iterator_Sems (I).Definition),
                      Destination =>
                        Iterator_Loop_Param_Sem
                          (For_Loop_Sem.Iterator_Sems (I)).Info.Obj_Location,
                      Dest_Name => Strings.Null_U_String_Index,
                      Source => Next_Value_Locators (I),
                      Might_Be_Null => True));
               end loop;

            end if;  --  Whether using a parallel nested block

            --  Fix up skip counts
            for Iter_Index in For_Loop_Sem.Iterator_Sems'Range loop
               declare
                  Iter_Sem : constant Iterator_Sem_Ptr :=
                    For_Loop_Sem.Iterator_Sems (Iter_Index);
               begin
                  --  Fix up skip count for jump to after call.
                  if Iter_Sem.Next_Value_Test /= 0 then
                     if For_Loop_Sem.Uses_Parallel_Nested_Block then
                        Loop_Visitor.Current_Code.Instrs (
                          Iter_Sem.Next_Value_Test).Skip_If_False :=
                          Loop_Visitor.Num_Instrs - Iter_Sem.Next_Value_Test;
                     else
                        --  Add Next_Value_Test list of exits to be fixed up.
                        For_Loop_Sem.Num_Exits_Emitted :=
                          For_Loop_Sem.Num_Exits_Emitted + 1;

                        For_Loop_Sem.Exit_Locs
                          (For_Loop_Sem.Num_Exits_Emitted) :=
                             (Loop_Visitor.Current_Block,
                              Iter_Sem.Next_Value_Test);
                     end if;
                  end if;
               end;
            end loop;  --  Loop over iterators to fixup skip counts

            Check_And_Set_Local_Offset (Loop_Visitor, Cur_Offset);
         end;
      end loop;  --  Loop over each set of "next" values
   end Next_For_Loop_Iteration;

   procedure Emit_Annotation_List
     (Check_Visitor : in out Code_Gen_Visitor;
      T : Annotation.Tree) is
   --  Emit list of annotations, and-then-ing them together.
   --  Boolean result ends up at original Check_Visitor.Target_Local_Offset,
   --  which is then incremented.

      use Interpreter;

      pragma Assert (Check_Visitor.Annotation_Mode /= Entry_Temp_Mode);
         --  Entry temps handled elsewhere

      Starting_Offset : constant Offset_Within_Area :=
        Check_Visitor.Target_Local_Offset;
      Num_Annotations : constant Natural :=
        Lists.Length (T.Annotations);
      If_Locs : array (1 .. Num_Annotations - 1) of Code_Offset :=
        (others => 0);
      Orig_VM_Info : constant VM_Obj_Id_Type := Check_Visitor.Target_VM_Info;
      Check_VM_Info : VM_Obj_Id_Type := Orig_VM_Info;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Check_Visitor.Decl_Region);
   begin
      if Num_Annotations > 1 and then not Check_VM_Info.Is_Var then
         --  We need a variable as the target
         Check_VM_Info := Assign_VM_Obj_Id (Check_Visitor, Needs_Var => True);

         Emit
           (Check_Visitor,
            (Declare_Obj_Op,
             Source_Pos => Annotation.Find_Source_Pos (T),
             Destination => (Local_Area,
                             Starting_Offset,
                             Check_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Is_By_Ref => False,
             Is_Var => True,
             Declare_Type_Info => Run_Time_Type_Info
                                 (Static.Boolean_Type,
                                  Referring_Module => Enc_Module)));

      end if;

      --  Emit X and then Y and then Z ...
      for I in 1 .. Num_Annotations loop
         declare
            Next_Annotation : constant Optional_Tree :=
              Lists.Nth_Element (T.Annotations, I);
            Annotation_Tree : Trees.Tree'Class
              renames Tree_Ptr_Of (Next_Annotation).all;
         begin
            Check_Visitor.Target_VM_Info := Check_VM_Info;

            if Annotation_Tree in Trees.Annotation.Tree then
               --  We have a nested annotation.
               --  We will turn this into a nested Check,
               --  unless it is the only annotation in this outer list,
               --  in which case we will emit it directly.
               if Num_Annotations = 1 then
                  --  It is the only one, recurse with it
                  Emit_Annotation_List
                    (Check_Visitor, Trees.Annotation.Tree (Annotation_Tree));
                  return;   ------  All done now  ------

               else
                  --  Emit code for it
                  Emit_Code_For_Resolved_Tree
                    (Next_Annotation, Check_Visitor);
                  --  There is no boolean left on the stack, so no need
                  --  to test it.  But if this is the last annotation,
                  --  we need to load a #true.
                  if I = Num_Annotations then
                     --  Load a #true onto the stack
                     Emit
                       (Check_Visitor,
                        (Store_Int_Lit_Op,
                         Source_Pos => Find_Source_Pos (Next_Annotation),
                         Destination => (Local_Area, Starting_Offset,
                                         Check_VM_Info),
                         Dest_Name => Strings.Null_U_String_Index,
                         Int_Value => Boolean'Pos (True)));
                  end if;
               end if;
            elsif Annotation_Tree not in Reference.Tree then
               --  Not a nested annotation list,
               --  and not a "key => ...", so presume it is
               --  a boolean expression.
               --  TBD: Handle "ghost"/annotation-only declarations, etc.
               Emit_Code_For_Resolved_Tree (Next_Annotation, Check_Visitor);
               if I < Num_Annotations then
                  --  Skip to end if this element is #false
                  Emit
                    (Check_Visitor,
                     (If_Op,
                      Source_Pos => Find_Source_Pos (Next_Annotation),
                      If_Source => (Local_Area, Starting_Offset,
                                    Check_VM_Info),
                      If_Condition => Boolean_Is_True,  --  if True
                      Skip_If_False => 0));  --  will be filled in below

                  --  Remember location to be fixed up
                  If_Locs (I) := Check_Visitor.Num_Instrs;
               end if;

               --  Start back at original offset
               Check_And_Set_Local_Offset
                 (Check_Visitor, Starting_Offset);

               if Static.Is_Compile_Time_Known
                    (Resolved_Tree (Next_Annotation),
                     Disallow_Concurrent_Types => True)
                 and then
                  (Annotation_Tree not in Identifier.Tree
                     or else
                   Underlying_Sem_Info (Next_Annotation).all not in
                     Literal_Semantic_Info)
               then
                  --  Is compile-time known annotation, but not a literal,
                  --  so we treat it specially by adding it to list of
                  --  compile-time-known assertions which we will
                  --  check immediately after they are evaluated,
                  --  rather than waiting for run time.
                  declare
                     Anon_Const_Ref : constant Anon_Const_Tables.Element_Ref :=
                       Find_Element (Anon_Const_Table,
                                     Resolved_Tree (Next_Annotation));
                     use type Anon_Const_Tables.Element_Ref;
                     Annotation_Info : CTK_Annotation_Info;
                     Num_Annot_Entries : CTK_Annotation_Vectors.Elem_Index;
                  begin
                     if Anon_Const_Ref = null then
                        --  Should be in table if a constant computation
                        if Debug_Code_Gen then
                           Put_Line ("Compile-time-known annotation not found"
                             & " in Anon_Const_Table: "
                             & Subtree_Image (Next_Annotation));
                        end if;
                     else
                        --  Add an entry into CTK_Annotation_Vector.
                        Annotation_Info.CTK_Annotation := Next_Annotation;
                        --  Save index into anon-const table
                        Annotation_Info.CTK_Index := Anon_Const_Ref.all;
                        Add_Element (CTK_Annotation_Vector, Annotation_Info,
                          Num_Annot_Entries);
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      --  Fix up Skip_If_False fields
      for I in If_Locs'Range loop
         if If_Locs (I) /= 0 then
            Check_Visitor.Current_Code.Instrs (If_Locs (I)).Skip_If_False :=
              Check_Visitor.Num_Instrs - If_Locs (I);
         end if;
      end loop;

      if Num_Annotations = 0 then
         --  Store a #true if is empty annotation list
         Emit
           (Check_Visitor,
            (Store_Int_Lit_Op,
             Source_Pos => Annotation.Find_Source_Pos (T),
             Destination => (Local_Area, Starting_Offset, Orig_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Int_Value => Boolean'Pos (True)));

      elsif Check_VM_Info /= Orig_VM_Info then
         --  We need to copy back to the original VM reg
         Emit
           (Check_Visitor,
            (Copy_Word_Op,
             Source_Pos => Annotation.Find_Source_Pos (T),
             Destination => (Local_Area, Starting_Offset, Orig_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Source => (Local_Area, Starting_Offset, Check_VM_Info),
             Might_Be_Null => False));

      end if;

      --  Protect resulting boolean value
      Check_And_Set_Local_Offset (Check_Visitor, Starting_Offset + 1);

   end Emit_Annotation_List;

   procedure Build_Entry_Temp_List
     (Visitor : in out Code_Gen_Visitor;
      Entry_Temps : out Lists.List;
      Annotations : Lists.List) is
   --  Build list of expressions that need an entry temp within the annotations
   --  Assign locations on stack frame associated with Visitor

      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

      procedure Add_Temps (Ann : Optional_Tree) is
      --  Recursive routine to add entry temps from given annotation (sub) tree
         Resolved : constant Optional_Tree := Resolved_Tree (Ann);
         Sem : constant Root_Sem_Ptr := Sem_Info (Resolved);
      begin
         if Sem /= null and then Sem.all in Operand_Semantic_Info'Class then
            --  See whether entry temp needed here or below this node
            declare
               Opnd_Sem : constant Operand_Sem_Ptr := Operand_Sem_Ptr (Sem);
               Resolved_Type : Type_Sem_Ptr := Opnd_Sem.Resolved_Type;
               use Interpreter;
            begin
               if Opnd_Sem.Entry_Temp_Info /= null then
                  --  Entry temp needed here
                  Lists.Append (Entry_Temps, Resolved);  --  Add to list

                  --  Assign it a location
                  Opnd_Sem.Entry_Temp_Info.Obj_Location :=
                    (Local_Area,
                     Visitor.Target_Local_Offset,
                     No_VM_Obj_Id);  --  Will be filled in later
                  Opnd_Sem.Entry_Temp_Info.Obj_Level := Visitor.Current_Level;
                  Visitor.Target_Local_Offset :=
                    Visitor.Target_Local_Offset + 1;

                  --  Allocate unique ID for temp variable.
                  Opnd_Sem.Entry_Temp_Info.Obj_Location.VM_Obj_Id :=
                    Assign_VM_Obj_Id (Visitor, Needs_Var => True);

                  --  Use polymorphic type if will be converted to
                  --  polymorphic type when expression is evaluated.
                  if Opnd_Sem.Target_Polymorphic_Type /= null then
                     Resolved_Type := Opnd_Sem.Target_Polymorphic_Type;
                  end if;

                  --  Declare it
                  Emit
                    (Visitor,
                     (Declare_Obj_Op,
                      Source_Pos => Find_Source_Pos (Resolved),
                      Destination => Opnd_Sem.Entry_Temp_Info.Obj_Location,
                      Dest_Name => Strings.Null_U_String_Index,
                      Is_By_Ref => False,
                      Is_Var => False,
                      Declare_Type_Info => Run_Time_Type_Info
                                          (Resolved_Type,
                                           Referring_Module => Enc_Module)));

                  if not Static.Known_To_Be_Small (Resolved_Type) then
                     --  Initialize it to appropriate null if might be large,
                     --  so it has the correct region.
                     Emit
                       (Visitor,
                        (Store_Local_Null_Op,
                         Source_Pos => Find_Source_Pos (Resolved),
                         Destination => Opnd_Sem.Entry_Temp_Info.Obj_Location,
                         Dest_Name => Strings.Null_U_String_Index,
                         Null_Type_Info =>
                           Run_Time_Type_Info
                             (Resolved_Type,
                              Referring_Module => Enc_Module)));
                  end if;

               elsif Opnd_Sem.Entry_Exit_Info.Num_Entry_Temps > 0 then
                  declare
                     Res_Tree : Trees.Tree'Class renames
                       Tree_Ptr_Of (Resolved).all;
                  begin
                     --  Recurse on operands of Resolved if entry temps below
                     for I in 1 .. Num_Operands (Res_Tree) loop
                        Add_Temps (Nth_Operand (Res_Tree, I));
                     end loop;
                  end;
               end if;
            end;
         end if;
      end Add_Temps;

   begin  --  Build_Entry_Temp_List

      for I in 1 .. Lists.Length (Annotations) loop
         --  Call recursive routine
         Add_Temps (Lists.Nth_Element (Annotations, I));
      end loop;
   end Build_Entry_Temp_List;

   procedure Emit_Entry_Temp_Initializations
     (Check_Visitor : in out Code_Gen_Visitor;
      Entry_Temps : Lists.List) is
   --  Emit initializations of entry temps
      use Interpreter;
   begin
      for I in 1 .. Lists.Length (Entry_Temps) loop
         --  Initialize the Ith entry temp
         declare
            Entry_Temp : constant Optional_Tree :=
              Lists.Nth_Element (Entry_Temps, I);
            Temp_Sem : constant Operand_Sem_Ptr :=
              Operand_Sem_Ptr (Sem_Info (Entry_Temp));
            Orig_Target_Offset : constant Offset_Within_Area :=
              Check_Visitor.Target_Local_Offset;
            Result_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Check_Visitor);
            Entry_Temp_Location : constant Interpreter.Object_Locator :=
              Adjust_For_Level_And_Prefix
                (Check_Visitor.Current_Level,
                 Obj_Location => Temp_Sem.Entry_Temp_Info.Obj_Location,
                 Obj_Level => Temp_Sem.Entry_Temp_Info.Obj_Level);
         begin
            if Debug_Code_Gen or Debug_Entry_Exit then
               Put_Line ("Initializing Entry Temp #" &
                 Natural'Image (I) & ": " &
                 Subtree_Image (Entry_Temp));
            end if;

            --  Generate code for entry temp expression, in "normal" mode
            Check_Visitor.Target_Object := Entry_Temp_Location;
            Check_Visitor.Target_VM_Info := Result_VM_Info;
            Code_Gen (Check_Visitor, Entry_Temp,
                      Annotation_Mode => Normal_Mode);

            --  Copy into entry temp
            Emit
              (Check_Visitor,
               (Copy_Word_Op,
                Source_Pos => Find_Source_Pos (Entry_Temp),
                Destination => Entry_Temp_Location,
                Dest_Name => Strings.Null_U_String_Index,
                Source => (Local_Area, Orig_Target_Offset, Result_VM_Info),
                Might_Be_Null => Temp_Sem.Resolved_Type.Value_Is_Optional));

            --  Restore target offset
            Check_And_Set_Local_Offset (Check_Visitor, Orig_Target_Offset);
         end;
      end loop;

      --  Reset targeting info
      Check_Visitor.Target_Object := Null_Object_Locator;
      Check_Visitor.Target_VM_Info := No_VM_Obj_Id;
   end Emit_Entry_Temp_Initializations;

   procedure Emit_Block_For_Check
     (Visitor : in out Code_Gen_Visitor;
      Comp_Sem : Computation_Sem_Ptr;
      Ann_Tree : Annotation.Tree) is
      --  Emit nested block for run-time check

      use Interpreter;
      pragma Assert (not Lists.Is_Empty (Ann_Tree.Annotations));
      Check_Visitor : Code_Gen_Visitor;
      Max_Length : constant Code_Length_Type := Max_Block_Length;
      Check_Code : Code_Ptr := new Code_Type (Max_Length);
      Starting_Offset : Offset_Within_Area;
      Result_VM_Info : VM_Obj_Id_Type (Local_Kind);

      Assertion_Str : Strings.U_String := Strings.Null_U_String;

      Entry_Temp_List : Lists.List;

   begin  --  Emit_Block_For_Check

      if Visitor.Annotation_Mode = Entry_Temp_Mode then
         --  Should only be called if there are entry temps
         pragma Assert (Comp_Sem.Entry_Exit_Info.Num_Entry_Temps > 0);

         Build_Entry_Temp_List
           (Visitor, Entry_Temp_List, Ann_Tree.Annotations);
         if Debug_Code_Gen or Debug_Entry_Exit then
            Put_Line
              ("Found" & Natural'Image (Lists.Length (Entry_Temp_List)) &
               " entry temps, expected" &
               Natural'Image (Comp_Sem.Entry_Exit_Info.Num_Entry_Temps) &
               " in: " & Subtree_Image (Ann_Tree));
         end if;

         --  Compute the "slow-calls" info now to determine whether
         --  a master is needed to compute entry temps.
         Comp_Sem.Slow_Calls := Combine_Operand_Calls (Entry_Temp_List);
      else
         --  Compute the "slow-calls" info now to determine whether
         --  a master is needed inside the nested block.
         Comp_Sem.Slow_Calls := Combine_Operand_Calls (Ann_Tree.Annotations);

         --  Initialize Assertion_Str appropriately
         Assertion_Str := Strings.String_Lookup (Subtree_Image (Ann_Tree));
      end if;

      --  Generate Check_Nested_Block_Op call to nested block;
      --  start the nested block; initialize Check_Visitor.
      Emit_Nested_Block_Start
        (Visitor,
         Check_Visitor,
         Comp_Sem,
         Block_Region => Visitor.Decl_Region,
         New_Code => Check_Code,
         Instr_Opcode => Check_Nested_Block_Op,
         Num_Outputs => 1,
         Assertion_Str => Assertion_Str);

      Starting_Offset := Check_Visitor.Target_Local_Offset;

      if Visitor.Annotation_Mode = Entry_Temp_Mode then
         --  Generate assignments to entry temps
         Emit_Entry_Temp_Initializations (Check_Visitor, Entry_Temp_List);

         --  Always set result to #true after init'ing entry temps
         Emit
           (Check_Visitor,
            (Store_Int_Lit_Op,
             Source_Pos => Annotation.Find_Source_Pos (Ann_Tree),
             Destination => (Param_Area, 0, No_VM_Obj_Id),
             Dest_Name => Strings.Null_U_String_Index,
             Int_Value => Boolean'Pos (True)));
      else
         --  Now emit the list of annotations
         Result_VM_Info := Assign_VM_Obj_Id (Check_Visitor);
         Check_Visitor.Target_VM_Info := Result_VM_Info;

         Emit_Annotation_List (Check_Visitor, Ann_Tree);

         --  Copy final boolean result to output at (Param_Area, 0)
         Emit
           (Check_Visitor,
            (Copy_Word_Op,
             Source_Pos => Source_Positions.Null_Source_Position,
             Destination => (Param_Area, 0, No_VM_Obj_Id),
             Dest_Name => Strings.Null_U_String_Index,
             Source => (Local_Area, Starting_Offset, Result_VM_Info),
             Might_Be_Null => False));
      end if;

      --  Emit a "return" and copy code for check into heap
      Emit_Nested_Block_Finish
        (Visitor,
         Check_Visitor,
         Uses_Queuing => Combine_Operand_Queuing (Ann_Tree.Annotations));

      Free_Code (Check_Code);

   end Emit_Block_For_Check;

   procedure Emit_Store_Literal
     (Visitor : in out Code_Gen_Visitor;
      Lit_Sem : Literal_Sem_Ptr;
      Id : String;
      Source_Pos : Source_Positions.Source_Position) is
      --  Emit "Store" for a literal of some sort
      use Interpreter;
   begin
      if Visitor.Is_Lvalue_Context then
         Sem_Error
           (Lit_Sem.Definition,
            "Literal may not be used in an lvalue context");
      end if;
      case Lit_Sem.Lit_Kind is
         when Not_A_Literal =>
            pragma Assert (False);
            null;
         when Integer_Literal =>
            --  Store value of literal
            Emit
              (Visitor,
               (Store_Int_Lit_Op,
                Source_Pos => Source_Pos,
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Visitor.Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Int_Value => Literal_Value (Id)));
         when Real_Literal =>
            --  Store value of literal
            Emit
              (Visitor,
               (Store_Real_Lit_Op,
                Source_Pos => Source_Pos,
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Visitor.Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Real_Value => Interpreter.To_Univ_Real (Literal_Value (Id))));
         when Char_Literal =>
            --  Store value of literal
            Emit
              (Visitor,
               (Store_Char_Lit_Op,
                Source_Pos => Source_Pos,
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Visitor.Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Char_Value => Literal_Value (Id)));
         when String_Literal =>
            --  Store value of literal
            --  Create the string literal in the region of
            --  target object, if any
            Emit
              (Visitor,
               (Store_Str_Lit_Op,
                Source_Pos => Source_Pos,
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Visitor.Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Str_Value =>
                  Strings.Index (To_U_String (Literal_Value (Id))),
                Existing_Str_In_Stg_Rgn => Visitor.Target_Object));
         when Enum_Literal =>
            --  Store value of literal
            Emit
              (Visitor,
               (Store_Enum_Lit_Op,
                Source_Pos => Source_Pos,
                Destination => (Local_Area, Visitor.Target_Local_Offset,
                                Visitor.Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Enum_Value =>
                  Strings.Index (To_U_String (Literal_Value (Id)))));
         when Null_Literal =>
            --  Store a Null_Value
            --  TBD: This is type-specific, should probably be
            --      turned into a call on some imported function.
            declare
               Type_Of_Null : constant Type_Sem_Ptr := Lit_Sem.Resolved_Type;
               Enc_Module : constant Module_Sem_Ptr :=
                 Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
            begin
               if Type_Of_Null = null or else Type_Of_Null = Optional_Type then
                  --  This is a no-op use of "null,"
                  --  probably in an expression function.
                  null;
               elsif Type_Of_Null /= null
                 and then not Static.Known_To_Be_Small (Type_Of_Null)
                 and then not Target_Obj_Is_Null (Visitor)
               then
                  --  Store a possibly large null of an appropriate region
                  if Visitor.Target_Object.Base /= Local_Area
                    or else Visitor.Target_Object.Offset /=
                              Visitor.Target_Local_Offset
                  then
                     --  Only need to do this if target object
                     --  is at a different offset.
                     Emit
                       (Visitor,
                        (Store_Null_Of_Same_Stg_Rgn_Op,
                         Source_Pos => Source_Pos,
                         Destination => (Local_Area,
                                         Visitor.Target_Local_Offset,
                                         Visitor.Target_VM_Info),
                         Dest_Name => Visitor.Dest_Name,
                         Source => Visitor.Target_Object,
                         Might_Be_Null => True,
                         Type_Info => Run_Time_Type_Info
                                        (Type_Of_Null,
                                         Referring_Module => Enc_Module)));
                  end if;
               else
                  --  Store a (local, possibly large) null
                  --  of an appropriate type
                  Emit
                    (Visitor,
                     (Store_Local_Null_Op,
                      Source_Pos => Source_Pos,
                      Destination => (Local_Area,
                                      Visitor.Target_Local_Offset,
                                      Visitor.Target_VM_Info),
                      Dest_Name => Visitor.Dest_Name,
                      Null_Type_Info => Run_Time_Type_Info
                                          (Type_Of_Null,
                                           Referring_Module => Enc_Module)));
               end if;

            end;
      end case;
   end Emit_Store_Literal;

   procedure Emit_Assignment
     (Visitor : in out Code_Gen_Visitor;
      Assign_Operator : Assign_Stmt.Assign_Operator_Enum;
      LHS : Optional_Tree;
      RHS : Optional_Tree) is
      --  Emit an assignment given the operator and the LHS/RHS

      pragma Assert (not Visitor.Is_Lvalue_Context);
      --  Shouldn't already be in an lvalue context

      use Interpreter;
      use type Assign_Stmt.Assign_Operator_Enum;
      LHS_Location : Object_Locator;
      LHS_Type : constant Type_Sem_Ptr :=
        Operand_Sem_Ptr (Sem_Info (LHS)).Resolved_Type;
      --  NOTE: We use LHS type since LHS might be polymorphic while RHS is not

      RHS_Offset : Offset_Within_Area := 0;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      LHS_Sem : constant Sem_Ptr :=
        Underlying_Sem_Info (Resolved_Tree (LHS));
      Num_Temps : constant Natural :=
        Num_Finalizable_Temps (LHS) + Num_Finalizable_Temps (RHS);
      RHS_VM_Info : constant VM_Obj_Id_Type := Assign_VM_Obj_Id (Visitor);
   begin
      --  Allocate space for finalizable temps, if any
      if Num_Temps > 0 then
         Allocate_Finalizable_Temps (Visitor, Num_Temps);

         --  TBD: What if Gen_Parallel_Invocations_Only is True?
         pragma Assert (not Visitor.Gen_Parallel_Invocations_Only);
      end if;

      --  Get address
      --  NOTE: If this involves an operation that returns a reference, then
      --       the reference will be evaluated and stored in a temp and the
      --       "Lvalue_Location" will be an indirection through that temp.
      Visitor.Is_Lvalue_Context := True;
      Visitor.Target_Object := Null_Object_Locator;
      Emit_Code_For_Resolved_Tree (LHS, Visitor);
      Visitor.Is_Lvalue_Context := False;

      LHS_Location := Visitor.Lvalue_Location;
      Visitor.Lvalue_Location := Null_Object_Locator;

      RHS_Offset := Visitor.Target_Local_Offset;

      --  NOTE: We only get here if doing Assign, Move, or Swap:

      if Resolved_Type (LHS) /= null
        and then not Resolved_Type (LHS).Known_To_Be_Assignable
      then
         --  LHS of assignment must be assignable
         Sem_Error (LHS, "LHS not an assignable object");
      end if;

      if not Static.Sem_Info_Is_For_Variable (LHS_Sem) then
         --  LHS of assignment must be a variable
         Sem_Error (LHS, "LHS not a variable");
      end if;

      if Assign_Operator /= Assign_Stmt.Assign_Op then
         if Resolved_Type (RHS) /= null
           and then not Resolved_Type (RHS).Known_To_Be_Assignable
         then
            --  RHS of swap/move must be assignable
            Sem_Error (RHS, "RHS not an assignable object");
         end if;

         if not Static.Sem_Info_Is_For_Variable
                  (Underlying_Sem_Info (Resolved_Tree (RHS)))
         then
            --  RHS of swap/move must be a variable
            Sem_Error (RHS, "RHS not a variable");
         end if;
      end if;

      Visitor.Target_VM_Info := RHS_VM_Info;

      case Assign_Operator is
         when Assign_Stmt.Swap_Op =>
            --  Use a Swap_Obj operation
            declare
               RHS_Location : Object_Locator;
            begin
               --  Get location of RHS
               Visitor.Is_Lvalue_Context := True;
               Visitor.Target_VM_Info := RHS_VM_Info;
               Emit_Code_For_Resolved_Tree (RHS, Visitor);
               Visitor.Is_Lvalue_Context := False;

               RHS_Location := Visitor.Lvalue_Location;
               Visitor.Lvalue_Location := Null_Object_Locator;

               if Visitor.Gen_Parallel_Invocations_Only then
                  return;  -- Return now --
               end if;

               pragma Assert (LHS_Location.Base /= Zero_Base);
               pragma Assert (RHS_Location.Base /= Zero_Base);

               --  Do the swap
               Emit
                 (Visitor,
                  (Swap_Obj_Op,
                   Source_Pos => Find_Source_Pos (LHS),
                   Destination => LHS_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => RHS_Location,
                   Might_Be_Null => Resolved_Type (RHS).Value_Is_Optional,
                   Type_Info => Run_Time_Type_Info
                                  (LHS_Type,
                                   Referring_Module => Enc_Module)));
            end;

         when Assign_Stmt.Assign_Op =>
            if Static.Known_To_Be_Small (LHS_Type) then
               --  Type is known to be a "small" type -- no need for region
               --  info
               --  Get value
               Visitor.Target_VM_Info := RHS_VM_Info;
               Emit_Code_For_Resolved_Tree (RHS, Visitor);

               if Visitor.Gen_Parallel_Invocations_Only then
                  return;  -- Return now --
               end if;

               pragma Assert (LHS_Location.Base /= Zero_Base);

               --  Move value into address
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (LHS),
                   Destination => LHS_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => (Local_Area, RHS_Offset, RHS_VM_Info),
                   Might_Be_Null => True));
            else
               --  Might be "large"
               --  Try to build value in correct region.
               --  If RHS is new object, can build result in target object
               --  (someday) so long as target object not referenced in RHS.
               --  If can't build directly in target object, then build
               --  in correct region, then release target object and
               --  replace with new object.
               --  If RHS is preexisting object which is not a component
               --  of LHS, then reuse as much of LHS as possible (some day),
               --  or as a last resort, make complete copy of RHS in region
               --  of LHS, then release LHS, then move copy onto LHS.
               --
               --  So for now: we create a new object either by calling an
               --  operation or evaluating an aggregate, or by copying an
               --  existing object, then release the LHS, then replace with
               --  the RHS.  If at run-time we determine the type is
               --  actually small, then copying/releasing are trivial.
               --
               --  The target object determines the region.
               --  The source object determines the type.
               --  By setting the Target_Object, we determine the region.
               --  We also indicate that we want a copy of the RHS made.
               --  If there is no Target_Object, then we just get a
               --  (R/O) reference to the existing RHS, if the RHS is large.
               Visitor.Target_Object := LHS_Location;
               Visitor.Target_VM_Info := RHS_VM_Info;
               Emit_Code_For_Resolved_Tree (RHS, Visitor);
               Visitor.Target_Object := Null_Object_Locator;

               if Visitor.Gen_Parallel_Invocations_Only then
                  return;  -- Return now --
               end if;

               pragma Assert (LHS_Location.Base /= Zero_Base);

               --  Release and then overwrite target with new value
               Emit
                 (Visitor,
                  (Assign_Word_Op, --  "Assign_Word" will reclaim target
               --  if large and non-null.
               --  That implies we need to pass the type.
                   Source_Pos => Find_Source_Pos (LHS),
                   Destination => LHS_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => (Local_Area, RHS_Offset, RHS_VM_Info),
                   Might_Be_Null => True,
                   Type_Info => Run_Time_Type_Info
                                  (LHS_Type,
                                   Referring_Module => Enc_Module)));
            end if;

         when Assign_Stmt.Move_Op =>
            declare
               RHS_Location : Object_Locator;
            begin
               --  Get location of RHS
               Visitor.Is_Lvalue_Context := True;
               Visitor.Target_VM_Info := RHS_VM_Info;
               Emit_Code_For_Resolved_Tree (RHS, Visitor);
               Visitor.Is_Lvalue_Context := False;

               RHS_Location := Visitor.Lvalue_Location;
               Visitor.Lvalue_Location := Null_Object_Locator;

               if Visitor.Gen_Parallel_Invocations_Only then
                  return;  -- Return now --
               end if;

               if LHS_Location.Base = Zero_Base
                 or else RHS_Location.Base = Zero_Base
               then
                  Sem_Error (LHS, "Move not allowed");
               end if;

               --  Do the move
               Emit
                 (Visitor,
                  (Move_Obj_Op,
                   Source_Pos => Find_Source_Pos (LHS),
                   Destination => LHS_Location,
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => RHS_Location,
                   Might_Be_Null => Resolved_Type (RHS).Value_Is_Optional,
                   Type_Info => Run_Time_Type_Info
                                  (LHS_Type,
                                   Referring_Module => Enc_Module)));
            end;

         when others =>
            pragma Assert (False);
            null;
      end case;

      --  Finalize any "ref-object" temps (right-hand side first)
      Finalize_Result_And_Ref_Operands (Visitor, RHS);
      Finalize_Result_And_Ref_Operands (Visitor, LHS);

      Check_High_Water (Visitor);
   end Emit_Assignment;

   procedure Emit_Value_Assignments
     (Visitor : in out Code_Gen_Visitor;
      Values : Optional_Tree) is
   --  Emit assignments to specified objects, for an exit
   --  or return with values specified with <name> => <value>
   --  or <name> <== <value>

      Values_Tree : Trees.Tree'Class renames Tree_Ptr_Of (Values).all;
      use Assign_Stmt;
   begin
      if Values_Tree in Reference.Tree then
         --  A single assignment
         Emit_Assignment (Visitor,
           Assign_Operator => Assign_Op,
           LHS => Reference.Tree (Values_Tree).Key,
           RHS => Reference.Tree (Values_Tree).Referent);
      elsif Values_Tree in Invocation.Tree then
         --  A list of assignments
         declare
            Values_Agg : Invocation.Tree renames Invocation.Tree (Values_Tree);
            use Invocation;
         begin
            if Values_Agg.Kind /= Class_Aggregate then
               Sem_Error (Values,
                 "Must be parenthesized list of Name => Value pairs");
            else
               --  Do each assignment
               for I in 1 .. Lists.Length (Values_Agg.Operands) loop
                  declare
                     Key_Val : constant Optional_Tree :=
                       Lists.Nth_Element (Values_Agg.Operands, I);
                     Key_Val_Tree : Trees.Tree'Class renames
                       Tree_Ptr_Of (Key_Val).all;
                  begin
                     if Key_Val_Tree in Reference.Tree then
                        --  Name => Value
                        Emit_Assignment (Visitor,
                          Assign_Operator => Assign_Op,
                          LHS => Reference.Tree (Key_Val_Tree).Key,
                          RHS => Reference.Tree (Key_Val_Tree).Referent);
                     elsif Key_Val_Tree in Assign_Stmt.Tree then
                        --  Name <== Value
                        Emit_Assignment (Visitor,
                          Assign_Operator =>
                            Assign_Stmt.Tree (Key_Val_Tree).Assign_Operator,
                          LHS => Assign_Stmt.Tree (Key_Val_Tree).LHS,
                          RHS => Assign_Stmt.Tree (Key_Val_Tree).RHS);
                     else
                        Sem_Error (Key_Val,
                          "Must be Name => Value or Name <== Value");
                     end if;
                  end;
               end loop;
            end if;
         end;
      else
         Sem_Error (Values, "Must be Name => Value or (N1 => V1, ...)");
      end if;
   end Emit_Value_Assignments;

   function Outermost_Master
     (Visitor : Code_Gen_Visitor;
      Up_Through : Composite_Stmt_Sem_Ptr := null)
     return Interpreter.Object_Locator is
   --  Return locator for outermost master of current operation,
   --  up through the specified composite statement, if any.
      Master_Offset : Interpreter.Offset_Within_Area := 0;
      Master_Level : Static_Level := 0;
      use Interpreter;
   begin
      if Up_Through = null then
         --  This is a "return" statement, so find master for
         --  operation as a whole.
         Master_Offset := Visitor.New_Routine.Local_Master;
         Master_Level :=
           Static.Find_Enclosing_Operation (Visitor.Decl_Region).Level + 1;
      elsif Up_Through.all in For_Loop_Construct_Semantic_Info then
         --  We are exiting a for-loop; these always have their own master
         Master_Offset :=
           For_Loop_Construct_Sem_Ptr (Up_Through).For_Loop_Master;
         Master_Level :=
           For_Loop_Construct_Sem_Ptr (Up_Through).For_Loop_Level;
      else
         --  We are exiting some other kind of statement so we need
         --  to search for outermost master.
         declare
            Encloser : Symbols.Region_Ptr := Visitor.Decl_Region;
            use Symbols;
         begin
            while Encloser /= null loop
               case Encloser.Kind is
                  when No_Region_Kind
                    | Library_Region_Kind
                    | Module_Region_Kind
                    | Operation_Param_Region_Kind
                    | Operation_Body_Region_Kind =>

                     --  Keep looking
                     null;

                  when Parallel_Stmt_Region_Kind =>
                     --  Record the master used if symbol has sem info
                     if Encloser.Associated_Symbol /= null
                       and then Encloser.Associated_Symbol.Sem_Info /= null
                     then
                        declare
                           Parallel_Op_Sem : constant Computation_Sem_Ptr :=
                             Computation_Sem_Ptr
                               (Encloser.Associated_Symbol.Sem_Info);
                        begin
                           Master_Offset := Parallel_Op_Sem.Enclosing_Master;
                           Master_Level := Parallel_Op_Sem.Level;
                        end;
                     end if;

                  when Loop_Body_Region_Kind =>
                     if Encloser.Associated_Symbol /= null then
                        declare
                           Enclosing_Sem : constant Root_Sem_Ptr :=
                             Encloser.Associated_Symbol.Sem_Info;
                           use Interpreter;
                        begin
                           if Enclosing_Sem /= null and then
                             Enclosing_Sem.all in
                               For_Loop_Construct_Semantic_Info
                           then
                              --  We are inside a "for" loop.
                              --  Get its master.
                              declare
                                 For_Loop_Sem : constant
                                   For_Loop_Construct_Sem_Ptr :=
                                     For_Loop_Construct_Sem_Ptr
                                       (Enclosing_Sem);
                              begin
                                 Master_Offset := For_Loop_Sem.For_Loop_Master;
                                 Master_Level := For_Loop_Sem.For_Loop_Level;
                              end;
                           end if;
                        end;
                     end if;
                     --  Keep looking

                  when Loop_Param_Region_Kind
                    | Sequential_Stmt_Region_Kind
                    | Type_Annotation_Region_Kind =>
                     --  Check enclosing regions
                     null;
               end case;

               pragma Assert (Up_Through /= null);  -- Checked above

               exit when Encloser.Associated_Symbol /= null
                 and then Root_Sem_Ptr (Up_Through) =
                   Encloser.Associated_Symbol.Sem_Info;

               --  Keep looking
               Encloser := Encloser.Enclosing_Region;
            end loop;
         end;
      end if;

      if Master_Offset /= 0 then
         --  Return adjusted locator for appropriate master
         return Adjust_For_Level_And_Prefix
           (Visitor.Current_Level,
            Obj_Location => (Local_Area, Master_Offset, No_VM_Obj_Id),
            Obj_Level => Master_Level);
      else
         --  TBD: Perhaps this should be an error
         if Debug_Code_Gen then
            Put_Line (" Outermost_Master returning null");
            if Up_Through /= null then
               Put_Line ("  Up_Through => " &
                 Subtree_Image (Up_Through.Definition));
            end if;
         end if;
         return Interpreter.Null_Object_Locator;
      end if;
   end Outermost_Master;

   procedure Finalize_Non_Ref_Operands
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree) is
   --  Walk tree and finalize temps associated with non-ref operands
   --  and for each such operand, finalize everything below there as well.
   --  NOTE: If an operand is "moved" into a container, it does not need
   --        to be further finalized, but any enclosing ref-object from
   --        which it was moved does need finalization.
      Expr_Tree : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
      Tree_Sem : constant Sem_Ptr := Underlying_Sem_Info (OT);
   begin
      if Tree_Sem /= null
        and then Tree_Sem.all in Call_Semantic_Info'Class
        and then Call_Sem_Ptr (Tree_Sem).Op_Sem /= null
      then
         --  We have a call
         declare
            Call_Sem : constant Call_Sem_Ptr := Call_Sem_Ptr (Tree_Sem);
            Op_Sem : constant Operation_Sem_Ptr := Call_Sem.Op_Sem;
            Call_Tree : Invocation.Tree renames Invocation.Tree
              (Tree_Ptr_Of (Call_Sem.Definition).all);
            Target_Operation : Operation.Tree renames Operation.Tree
              (Tree_Ptr_Of (Op_Sem.Definition).all);
         begin
            --  Walk the operands and the input parameters in parallel
            for I in 1 .. Lists.Length (Call_Tree.Operands) loop
               declare
                  Actual_Operand : constant Optional_Tree :=
                    Lists.Nth_Element (Call_Tree.Operands, I);

                  Formal_Param_Tree : Trees.Tree'Class renames
                    Tree_Ptr_Of (Lists.Nth_Element
                      (Target_Operation.Operation_Inputs, I)).all;
               begin
                  if Formal_Param_Tree not in Param_Decl.Tree'Class
                    or else
                      Param_Decl.Tree'Class (Formal_Param_Tree).Kind
                        not in Param_Decl.Ref_Param_Kinds
                  then
                     --  Not a ref operand; finalize it now
                     Finalize_Result_And_Ref_Operands
                       (Visitor, Actual_Operand);
                  end if;
               end;
            end loop;
         end;
      elsif Expr_Tree in Obj_Decl.Tree
        and then Obj_Decl.Tree (Expr_Tree).Is_Ref
      then
         --  Postpone finalization of initializing value for "ref"
         null;
      else
         --  Finalize each operand now
         for I in 1 .. Num_Operands (Expr_Tree) loop
            Finalize_Result_And_Ref_Operands
              (Visitor, Nth_Operand (Expr_Tree, I));
         end loop;
      end if;
   end Finalize_Non_Ref_Operands;

   procedure Finalize_Ref_Operands
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree) is
   --  Walk tree and finalize temps associated with ref operands,
   --  and for each such operand, finalize everything below there.
      Expr_Tree : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
      Tree_Sem : constant Sem_Ptr := Underlying_Sem_Info (OT);
   begin
      if Tree_Sem /= null
        and then Tree_Sem.all in Call_Semantic_Info'Class
        and then Call_Sem_Ptr (Tree_Sem).Op_Sem /= null
      then
         --  We have a call
         declare
            Call_Sem : constant Call_Sem_Ptr := Call_Sem_Ptr (Tree_Sem);
            Op_Sem : constant Operation_Sem_Ptr := Call_Sem.Op_Sem;
            Call_Tree : Invocation.Tree renames Invocation.Tree
              (Tree_Ptr_Of (Call_Sem.Definition).all);
            Target_Operation : Operation.Tree renames Operation.Tree
              (Tree_Ptr_Of (Op_Sem.Definition).all);
         begin
            --  Walk the operands and the input parameters in parallel
            for I in 1 .. Lists.Length (Call_Tree.Operands) loop
               declare
                  Actual_Operand : constant Optional_Tree :=
                    Lists.Nth_Element (Call_Tree.Operands, I);

                  Formal_Param_Tree : Trees.Tree'Class renames
                    Tree_Ptr_Of (Lists.Nth_Element
                      (Target_Operation.Operation_Inputs, I)).all;
               begin
                  if Formal_Param_Tree in Param_Decl.Tree'Class
                    and then
                      Param_Decl.Tree'Class (Formal_Param_Tree).Kind
                        in Param_Decl.Ref_Param_Kinds
                  then
                     --  Found a "ref" operand; finalize it now
                     if Debug_Code_Gen then
                        Put_Line (" Finalize_Ref_Operands, ref operand: " &
                          Subtree_Image (Actual_Operand));
                     end if;
                     Finalize_Result_And_Ref_Operands
                       (Visitor, Actual_Operand);
                  end if;
               end;
            end loop;
         end;
      elsif Expr_Tree in Binary.Tree then
         declare
            Bin_Tree : Binary.Tree renames Binary.Tree (Expr_Tree);
            use Binary;
         begin
            case Bin_Tree.Operator is
            when Sequential_Stmt_Op | Next_Stmt_Op | Then_Stmt_Op =>
               --  Finalize declarations in reverse order
               Finalize_Result_And_Ref_Operands
                 (Visitor, Bin_Tree.Right_Operand);
               Finalize_Result_And_Ref_Operands
                 (Visitor, Bin_Tree.Left_Operand);
            when others =>
               --  TBD: Any other special finalization required?
               null;
            end case;
         end;
      elsif Expr_Tree in Obj_Decl.Tree
        and then Obj_Decl.Tree (Expr_Tree).Is_Ref
      then
         if Debug_Code_Gen then
            Put_Line (" Finalize_Ref_Operands, Init val is ref: " &
              Subtree_Image (OT));
         end if;
         --  Finalize initializing value
         Finalize_Result_And_Ref_Operands
           (Visitor, Obj_Decl.Tree (Expr_Tree).Obj_Value);
      end if;
   end Finalize_Ref_Operands;

   procedure Finalize_Result
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree) is
   --  Finalize temp, if any, associated with result of computation.
      Expr_Tree : Trees.Tree'Class renames Tree_Ptr_Of (OT).all;
      Tree_Sem : constant Sem_Ptr := Sem_Ptr (Expr_Tree.Sem_Info);
   begin
      if Tree_Sem /= null
        and then Tree_Sem.all in Computation_Semantic_Info'Class
        and then Computation_Sem_Ptr (Tree_Sem).Finalizable_Temp_Info /= null
      then
         --  We have a finalizable temp
         declare
            Comp_Sem : constant Computation_Sem_Ptr :=
              Computation_Sem_Ptr (Tree_Sem);
            Temp_Info : constant Object_Location_Ptr :=
              Comp_Sem.Finalizable_Temp_Info;

            End_Op_Sem : constant Operation_Sem_Ptr :=
              Find_End_Op_For (Comp_Sem.Resolved_Type);
            Var_End_Op_Sem : constant Operation_Sem_Ptr :=
              Find_End_Op_For (Comp_Sem.Resolved_Type,
                Op_Name => Static.Var_End_Op_Str);
            Op_Sem_To_Use : Operation_Sem_Ptr;

            Enc_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);

            use Interpreter;
            Orig_Local_Offset : constant Offset_Within_Area :=
              Visitor.Target_Local_Offset;
            End_Call_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Visitor, Num_Call_Params => 1);
         begin
            if Temp_Info.Obj_Location.Offset = 0 then
               --  Attempt to finalize something that had no space allocated
               Sem_Error
                 (OT,
                  "Finalize_Result: Finalizable_Temp_Info.Obj_Location zero");
            else
               if Debug_Code_Gen then
                  Put_Line ("Finalize_Result: " &
                    "Finalizable_Temp_Info.Obj_Location =" &
                    Obj_Locator_Image (Temp_Info.Obj_Location) &
                    " for " & Subtree_Image (OT));
               end if;

               if Var_End_Op_Sem = null and then End_Op_Sem = null then
                  --  Nothing special to do
                  if Debug_Code_Gen then
                     Put_Line ("Finalize_Result: " &
                       "no ""end"" op found for " & Subtree_Image (OT));
                  end if;
                  return;
               end if;

               if Var_End_Op_Sem /= null then
                  if End_Op_Sem /= null then
                     Sem_Warning ("Finalize_Result: " &
                       "Calling ""var_end"" rather than ""end"" on " &
                       Subtree_Image (OT),
                       Src_Pos => Find_Source_Pos (OT));
                  end if;
                  Op_Sem_To_Use := Var_End_Op_Sem;
               else
                  pragma Assert (End_Op_Sem /= null);
                  Op_Sem_To_Use := End_Op_Sem;
               end if;
               --  Copy Temp to Target_Local_Offset and make the call.
               --  NOTE: We know the ref object is *not* passed by reference
               --        because it contains a ref.
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (OT),
                   Destination => (Local_Area, Orig_Local_Offset,
                                   Param_VM_Obj_Id (End_Call_VM_Info,
                                     Param_Offset => 0)),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source =>
                     Adjust_For_Level_And_Prefix
                       (Visitor.Current_Level,
                        Obj_Location => Temp_Info.Obj_Location,
                        Obj_Level => Temp_Info.Obj_Level),
                   Might_Be_Null => True));  -- TBD

               Check_And_Set_Local_Offset
                 (Visitor, Orig_Local_Offset + 1);

               --  Call appropriate "end" op
               Emit
                 (Visitor,
                  (Call_Op,
                   Source_Pos => Find_Source_Pos (OT),
                   Call_Target =>
                      Routine_Locator
                        (Op_Sem_To_Use,
                         Comp_Sem.Resolved_Type),
                   Target_Index =>
                     Find_Operation_Routine_Index (Op_Sem_To_Use),
                   Params => (Local_Area, Orig_Local_Offset, End_Call_VM_Info),
                   Locked_Param_Info => Null_Locked_Param_Info,
                   Static_Link => Run_Time_Type_Info
                                    (Comp_Sem.Resolved_Type,
                                     Referring_Module => Enc_Module),
                   Precond_Proved => False,
                   Output_Inited_Null => False));

               Visitor.Target_Local_Offset := Orig_Local_Offset;
            end if;
         end;
      end if;
   end Finalize_Result;

   procedure Finalize_Result_And_Ref_Operands
     (Visitor : in out Code_Gen_Visitor; OT : Optional_Tree) is
   --  Finalize temps associated with result and ref operands,
   --  and for each ref operand, finalize ref operands recursively.
   begin
      if Not_Null (OT) then
         --  Just pass the buck to more specific operations.
         declare
            Resolved_OT : constant Optional_Tree := Resolved_Tree (OT);
         begin
            if Debug_Code_Gen then
               Put_Line
                 (" Calling Finalize_Result on " &
                  Subtree_Image (Resolved_Ot));
            end if;
            Finalize_Result (Visitor, Resolved_OT);
            if Debug_Code_Gen then
               Put_Line
                 (" Calling Finalize_Ref_Operands on " &
                  Subtree_Image (Resolved_Ot));
            end if;
            Finalize_Ref_Operands (Visitor, Resolved_OT);
         end;
      end if;
   end Finalize_Result_And_Ref_Operands;

   procedure Identifier_Code_Gen
     (Visitor : in out Code_Gen_Visitor;
      Id_Sem : Operand_Sem_Ptr) is
   --  Generate code for a reference to an identifier or a qualified name.
      Id : String renames Sym_Name (Id_Sem.Associated_Symbol);
      use Interpreter;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      if Id_Sem.all in Literal_Semantic_Info then
         --  A literal should not appear in an Lvalue context.
         pragma Assert (not Visitor.Is_Lvalue_Context);

         Emit_Store_Literal
           (Visitor,
            Literal_Sem_Ptr (Id_Sem),
            Id,
            Source_Pos => Find_Source_Pos (Id_Sem.Definition));
      else
         declare
            Id_Sym : constant Symbols.Sym_Ptr :=
              Operand_Semantic_Info'Class (Id_Sem.all).Associated_Symbol;

            use type Param_Decl.Param_Kind;
         begin
            if Id_Sym = null then
               Sem_Error (Id_Sem.Definition, Id & " is not defined");
            else
               --  OK, we hopefully have an object or a parameter
               case Id_Sym.Kind is
                  when Object_Sym_Kind         |
                       Param_Sym_Kind          |
                       Loop_Param_Sym_Kind     |
                       Loop_Key_Param_Sym_Kind =>
                     declare
                        Obj_Sem : constant Object_Sem_Ptr :=
                          Object_Sem_Ptr
                            (Underlying_Sem_Info (Sem_Ptr (Id_Sem)));
                        Is_By_Ref : constant Boolean :=
                          Static.Sym_Is_By_Ref (Id_Sym);
                        Obj_Location : Object_Locator :=
                          Obj_Sem.Info.Obj_Location;
                        Loc_Is_Adjusted : Boolean := False;

                        Orig_Offset : constant Offset_Within_Area :=
                          Visitor.Target_Local_Offset;
                           --  Save original target-local offset

                        procedure Adjust_Obj_Location is
                           --  Adjust for level of reference,
                           --  if not already done.
                        begin
                           if not Loc_Is_Adjusted then
                              Loc_Is_Adjusted := True;

                              Obj_Location :=
                                Adjust_For_Level_With_Optional_Temp
                                   (Visitor'Access,
                                    Obj_Sem.Info.Obj_Location,
                                    Obj_Sem.Info.Obj_Level,
                                    Obj_Ref => Sym_Ref_Ptr (Id_Sem),
                                    Src_Pos => Find_Source_Pos
                                      (Id_Sem.Definition));
                           end if;
                        end Adjust_Obj_Location;

                     begin
                        if Is_By_Ref then
                           --  Is a "ref" object,
                           --  copy address into target-local-offset.
                           declare
                              Temp_VM_Info : constant VM_Obj_Id_Type :=
                                Assign_VM_Obj_Id (Visitor);
                           begin
                              if Visitor.Target_Object.Base = Local_Area
                                and then Visitor.Target_Object.Offset =
                                           Visitor.Target_Local_Offset
                              then
                                 --  Skip over target object
                                 Visitor.Target_Local_Offset :=
                                   Visitor.Target_Local_Offset + 1;
                              end if;

                              --  Adjust obj-location for current level
                              Adjust_Obj_Location;

                              --  Move address into a known local temp
                              Emit
                                (Visitor,
                                 (Copy_Address_Op,
                                  Source_Pos =>
                                    Find_Source_Pos (Id_Sem.Definition),
                                  Destination =>
                                    (Local_Area,
                                     Visitor.Target_Local_Offset,
                                     Temp_VM_Info),
                                  Dest_Name => Visitor.Dest_Name,
                                  Source => Obj_Location,
                                  Might_Be_Null =>
                                    Obj_Sem.Resolved_Type.Value_Is_Optional));

                              --  Indicate object is through a level of
                              --  indirection
                              Obj_Location :=
                                (Phys_Base_Register
                                  (Visitor.Target_Local_Offset), 0,
                                 Indir_VM_Obj_Id (Temp_VM_Info));

                              --  Remember we used a temp
                              Visitor.Target_Local_Offset :=
                                Visitor.Target_Local_Offset + 1;

                           end;
                        end if;

                        if Target_Obj_Is_Null (Obj_Location) then
                           Sem_Error
                             (Id_Sem.Definition,
                              "Internal: Object not yet assigned a location");
                           return;  --  Return now  --
                        end if;

                        if Is_Inside_Parameterless_Computation
                             (Visitor.Op_Sem)
                          and then not Static.Sem_Info_Is_For_Variable
                                 (Sem_Ptr (Obj_Sem))
                          and then Tree_Ptr_Of (Obj_Sem.Definition).all in
                            Obj_Decl.Tree
                        then
                           --  See whether it is necessary to replace
                           --  reference to object with its value or with
                           --  reference to anonymous global constant.
                           --  This is important when generating code for
                           --  an out-of-line tree evaluation, since we don't
                           --  want to refer to local constants.
                           --  TBD: This is not fully general.
                           declare
                              Obj_Tree : Obj_Decl.Tree renames Obj_Decl.Tree
                                (Tree_Ptr_Of (Obj_Sem.Definition).all);
                              Val : Optional_Tree := Resolved_Tree
                                      (Obj_Tree.Obj_Value);
                           begin
                              if Not_Null (Val)
                                and then
                                  Static.Is_Compile_Time_Known (Val,
                                    Disallow_Concurrent_Types => True)
                              then
                                 if Tree_Ptr_Of (Val).all in
                                   Identifier.Tree
                                 then
                                    --  Constant is a copy of another constant
                                    --  (or a literal).
                                    --  Recurse with initial value
                                    pragma Assert
                                      (Visitor.Target_Local_Offset =
                                         Orig_Offset);
                                       --  Offset should not have changed

                                    Emit_Code_For_Resolved_Tree (Val, Visitor);
                                    return;  --  All done  --

                                 elsif Obj_Sem.Info.Obj_Level > 0 then
                                    --  Is a locally-declared constant.
                                    --  Use a corresponding anon global const.
                                    declare
                                       Anon_Const_Ref : constant
                                         Anon_Const_Tables.Element_Ref :=
                                           Find_Element (Anon_Const_Table,
                                                         Val);
                                       use type Anon_Const_Tables.Element_Ref;
                                       Const_Index : Obj_Sem_Info_Index := 0;
                                       Const_Sem : Sem_Ptr;
                                    begin
                                       if Anon_Const_Ref = null then
                                          --  TBD: Emit an internal error
                                          --       because constant not
                                          --       available?
                                          --  Recurse to compute value
                                          pragma Assert
                                            (Visitor.Target_Local_Offset =
                                               Orig_Offset);
                                             --  Offset should not have changed

                                          Emit_Code_For_Resolved_Tree
                                            (Val, Visitor);
                                          return;  --  All done  --
                                       end if;

                                       --  Copy from anon constant
                                       Const_Index := Anon_Const_Ref.all;
                                       Const_Sem := Nth_Element
                                         (Compile_Time_Known_Const_Table,
                                          Const_Index);

                                       --  Remember that we are using the
                                       --  anonymous constant
                                       Computation_Sem_Ptr (Const_Sem).
                                         Needs_Anon_Const := True;

                                       Obj_Location := (Const_Area,
                                         Offset_Within_Area (Const_Index),
                                         No_VM_Obj_Id);
                                       Loc_Is_Adjusted := True;

                                       --  Suppress any Create_Polymorphic_Obj
                                       --  op that might follow.
                                       --  Indicate to Emit_Copy_Obj_Or_Word
                                       --  that type being copied might be
                                       --  polymorphic.
                                       Visitor.Create_Polymorphic_Obj := False;

                                       --  Fall through to emit load
                                    end;
                                 end if;
                                 --  Fall through for normal load of constant.
                              end if;
                           end;
                        end if;

                        --  Adjust for level of reference
                        Adjust_Obj_Location;

                        if Visitor.Is_Lvalue_Context then
                           --  Caller wants address in Lvalue_Location.

                           Visitor.Lvalue_Location := Obj_Location;
                        else
                           --  Caller wants "content" in
                           --  original Target_Local_Offset.

                           Emit_Copy_Obj_Or_Word
                             (Visitor,
                              Destination => (Local_Area,
                                              Orig_Offset,
                                              Visitor.Target_VM_Info),
                              Dest_Name => Visitor.Dest_Name,
                              Source => Obj_Location,
                              Target_Object => Visitor.Target_Object,
                              Opnd_Sem => Id_Sem,
                              Source_Pos =>
                                Find_Source_Pos (Id_Sem.Definition));

                        end if;

                        --  Mark high water and restore local offset
                        Check_And_Set_Local_Offset
                          (Visitor,
                           Orig_Offset);
                     end;

                  when Component_Sym_Kind =>
                     Sem_Error
                       (Id_Sem.Definition,
                        Id &
                        " is a component and requires ""<obj>."" as a prefix");
                  --  TBD: There are some contexts where components
                  --      can be referred to directly, such as in
                  --      the default value for other components,
                  --      or in the constraint on an object-specific type.

                  when Operation_Sym_Kind =>
                     --  TBD: Passing an operation as a parameter.
                     --       If actual is itself a parameter, this could
                     --       be a simple Copy_Word.
                     --       Otherwise we should use Store_Operation_Desc_Op.
                     declare
                        Resolved_Op : constant Resolved_Op_Ptr :=
                          Resolved_Op_Ptr (Id_Sem);
                        Call_Target : constant Object_Locator :=
                          Routine_Locator
                            (Resolved_Op.Op_Sem,
                             Type_Sem_Ptr (Resolved_Op.Assoc_Type_Region),
                             Current_Level => Visitor.Current_Level,
                             Tree_For_Srcpos => Resolved_Op.Definition,
                             Abstract_Allowed => False,  --  TBD if polymorph.
                             Use_Type_Relative_Locator => True);
                        Static_Link : constant Object_Locator :=
                          Static_Link_For_Call
                            (Op_Sem => Resolved_Op.Op_Sem,
                             Current_Level => Visitor.Current_Level,
                             Assoc_Type_Region =>
                               Resolved_Op.Assoc_Type_Region,
                             Referring_Module => Enc_Module,
                             Polymorphic_Param_Index => 0,
                             Use_Type_Relative_Locator => True);  --  TBD
                     begin
                        Emit (Visitor,
                          (Store_Operation_Desc_Op,
                           Source_Pos => Find_Source_Pos (Id_Sem.Definition),
                           Destination =>
                             (Local_Area, Visitor.Target_Local_Offset,
                              Visitor.Target_VM_Info),
                           Dest_Name => Visitor.Dest_Name,
                           Source => Visitor.Target_Object,
                           Might_Be_Null => True,
                           Operation_Locator => Call_Target,
                           Operation_Static_Link => Static_Link));
                        if Debug_Code_Gen then
                           Put_Line (" Passing operation " & Id &
                             " (type_region = " &
                             Type_Image
                               (Type_Sem_Ptr (Resolved_Op.Assoc_Type_Region))
                             & ") as a parameter");
                        end if;
                     end;

                  when others =>
                     Sem_Error
                       (Id_Sem.Definition,
                        Id &
                        " is neither a parameter nor a local object");
               end case;
            end if;

         end;

      end if;

      --  Unconditionally bump Target_Local_Offset
      Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

   end Identifier_Code_Gen;

   --------

   procedure Pre_Visit
     (Visitor : in out Code_Gen_Visitor;
      T : in out Trees.Tree'Class) is
   begin
      Visit (T.Pre_Annotation, Visitor);
   end Pre_Visit;

   procedure Post_Visit
     (Visitor : in out Code_Gen_Visitor;
      T : in out Trees.Tree'Class) is
   begin
      Visit (T.Post_Annotation, Visitor);
   end Post_Visit;

   procedure Module_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Module.Tree) is
      Mod_Sem : constant Module_Sem_Ptr := Module_Sem_Ptr (T.Sem_Info);
      Mod_Region : constant Symbols.Region_Ptr := Mod_Sem.Nested_Region;

      New_Mod : constant Symbols.Sym_Ptr := Mod_Sem.Associated_Symbol;
   begin
      if Mod_Sem.Code_Has_Been_Generated then
         if Debug_Code_Gen then
            Put_Line
              ("PSVM code for module " &
               Sym_Name (New_Mod) &
               " already generated");
         end if;
         return;
      end if;

      if Mod_Sem.Code_Being_Generated then
         if Debug_Code_Gen then
            Put_Line
              ("PSVM code for module " &
               Sym_Name (New_Mod) &
               " being generated");
         end if;
         return;
      end if;

      Mod_Sem.Code_Being_Generated := True;

      if Debug_Code_Gen then
         Put_Line
           ("Generate PSVM code for module " &
            Sym_Name (New_Mod) &
            "; sym_index =" &
            Sym_Index'Image (New_Mod.Index));
      end if;

      --  Generate code for module itself
      if T.Is_Interface
        and then Static.Num_Module_Parameters (Mod_Sem) > 0
      then
         if Debug_Code_Gen then
            Put_Line
              ("  Generate code for Params for module " &
               Sym_Name (New_Mod) &
               ':');
         end if;
         Code_Gen_List (Mod_Region, Mod_Sem.Ancestor_Module_Formals);
         Code_Gen_List (Mod_Region, T.Module_Formals);
      end if;

      if not Lists.Is_Empty (T.Class_Locals) then
         if Debug_Code_Gen then
            Put_Line
              ("  Generate code for Locals for module " &
               Sym_Name (New_Mod) &
               ':');
         end if;
         Code_Gen_List (Mod_Region, T.Class_Locals);
      end if;

      if not Lists.Is_Empty (T.Module_Exports)
        or else not Lists.Is_Empty (T.Module_New_Exports)
        or else not Lists.Is_Empty (T.Module_Implements)
      then
         if Debug_Code_Gen then
            Put_Line
              ("  Generate code for Exports for module " &
               Sym_Name (New_Mod) &
               ':');
         end if;
         Code_Gen_List (Mod_Region, T.Module_Exports);
         Code_Gen_List (Mod_Region, T.Module_New_Exports);
         Code_Gen_List (Mod_Region, T.Module_Implements);
      end if;

      if Debug_Code_Gen then
         Put_Line ("End of code gen for module " & Sym_Name (New_Mod));
      end if;

      Mod_Sem.Code_Being_Generated := False;
      Mod_Sem.Code_Has_Been_Generated := True;

      if T.Is_Interface and then Mod_Sem.Other_Part /= null
        and then Visitor.Decl_Region = Symbols.Library_Region
      then
         --  Generate code for implementation of top-level module now.
         --  TBD: Not clear that this is particularly useful.
         Module_Action
           (Visitor,
            Module.Tree (Tree_Ptr_Of (Mod_Sem.Other_Part.Definition).all));
      end if;
   end Module_Action;

   procedure Implements_Element_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Implements_Element.Tree) is
   begin
      if not Lists.Is_Empty (T.Elements) then
         if Debug_Code_Gen then
            if not Lists.Is_Empty (T.For_Interfaces) then
               Put ("  Generate code for implements section for ");
               for I in 1 .. Lists.Length (T.For_Interfaces) loop
                  if I > 1 then
                     Put (", ");
                  end if;
                  Put
                    (Subtree_Image (Lists.Nth_Element (T.For_Interfaces, I)));
               end loop;
               New_Line;
            else
               Put_Line ("  Generate code for implements-for-all section");
            end if;
         end if;
         --  TBD: Put these in separate sub-regions (at least those with
         --      a non-null For_Interfaces list).
         Code_Gen_List (Visitor.Decl_Region, T.Elements);
      end if;
   end Implements_Element_Action;

   procedure Operation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Operation.Tree) is
      Op_Sem : constant Operation_Sem_Ptr :=
        Operation_Sem_Ptr (Underlying_Op_Sem_Info (Sem_Ptr (T.Sem_Info)));

      New_Op : constant Symbols.Sym_Ptr := Op_Sem.Associated_Symbol;
      use type Operation.Operation_Kind_Enum;
   begin
      if Debug_Code_Gen then
         Put_Line
           ("Generate code for operation " &
            Sym_Name (New_Op) &
            "; sym_index =" &
            Sym_Index'Image (New_Op.Index));
         Put_Line
           ("Routine context = " &
            Context_Enum'Image (Op_Sem.Context) &
            ", op-index =" &
            Interpreter.Operation_Index'Image (Op_Sem.Index));
      end if;

      --  Fill in level on operation itself to match
      --  its declared level (this is one less than its local variables).
      Op_Sem.Level := Visitor.Current_Level;

      if Op_Sem.Equiv_To /= null then
         --  Share Routine with equivalent operation
         --  TBD: Static link needs to change when calling
         --      a routine from a different module.
         declare
            use Interpreter;
            pragma Assert (Op_Sem.Routine = null);
            Equiv_Op_Sem : constant Operation_Sem_Ptr := Op_Sem.Equiv_To;
            use type Symbols.Region_Ptr;
         begin
            Op_Sem.Routine := Equiv_Op_Sem.Routine;

            if Debug_Code_Gen then
               Put_Line
                 (" Op is equivalent to " &
                  Subtree_Image (Equiv_Op_Sem.Definition));
            end if;

            if Op_Sem.Routine = null then
               --  Try to find body
               if Equiv_Op_Sem.Body_Region /= null then
                  declare
                     Equiv_Op_Body_Sem : constant Operation_Sem_Ptr :=
                       Operation_Sem_Ptr (
                       Equiv_Op_Sem.Body_Region.Associated_Symbol.Sem_Info);
                  begin
                     Op_Sem.Routine := Equiv_Op_Body_Sem.Routine;
                  end;
               end if;

               if Op_Sem.Routine = null then
                  Sem_Error
                    (T,
                     "No compiled code for " &
                     Subtree_Image (Equiv_Op_Sem.Definition));
               end if;
            end if;

            if Debug_Code_Gen and then Op_Sem.Routine /= null then
               Put_Line
                 (" Using code from routine with name " &
                  Strings.To_String (Op_Sem.Routine.Name) &
                  " and routine index" &
                  Interpreter.Routine_Index'Image (Op_Sem.Routine.Index));
            end if;
         end;

      elsif T.Is_Def
        or else (Op_Sem.Is_Abstract and then Op_Sem.Overridden_By = null)
        or else
          (T.Is_Optional and then Op_Sem.Overridden_By = null
           and then Op_Sem.Body_Region = null)
      then
         --  Generate code for a definition (or an abstract/optional op)
         declare
            use Interpreter;
            use type Strings.U_String_Index;

            New_Routine : constant Interpreter.Routine_RW_Ptr :=
              Op_Sem.Routine;
            Routine_Params : constant Routine_Param_Info_Array_Ptr :=
              new Routine_Param_Info_Array (
              1 ..
              Lists.Length (T.Operation_Outputs) +
              Lists.Length (T.Operation_Inputs));

            Cg_Visitor : Code_Gen_Visitor :=
              (Trees.Visitor.RW_Tree_Visitor with
               Decl_Region => Op_Sem.Body_Region,
               Num_Instrs => 0,
               Last_Instr_Escapes => False,
               Op_Sem => Op_Sem,
               New_Routine => New_Routine,
               Is_Lvalue_Context => False,
               Lvalue_Location => Null_Object_Locator,
               Target_Object => Null_Object_Locator,
               Target_Local_Offset => Local_Area_Local_Data_Offset,
               Target_VM_Info => No_VM_Obj_Id,
               Dest_Name => Strings.Null_U_String_Index,
               Create_Polymorphic_Obj => False,
               Start_Callee_Locals => Local_Area_Local_Data_Offset,
               Local_Master => 0,
               Master_In_Use => False,
               Master_Is_Started => False,
               Master_Is_Complete => False,
               First_Awaited_Block => 0,
               Last_Awaited_Block => 0,
               Is_Leftmost => False,
               Gen_Parallel_Invocations_Only => False,
               Enclosing_For_Loop => null,
               Finalizable_Temp_Level => 0,
               Finalizable_Temp_Offset => 0,
               Nested_Blocks => null,
               Current_Code => null, --  Fixed up below
               Current_Block => 0,
               Current_Level => Visitor.Current_Level + 1,
               Annotation_Mode => Normal_Mode);
         --  TBD: Is this always right?
         begin
            New_Routine.Parameters := Routine_Params;
            New_Routine.Nesting_Level :=
              Code_Nesting_Level'(Visitor.Current_Level);
               --  Note that this is one less than Cg_Visitor.Current_Level,
               --  because the "Code_Nesting_Level" is 0 for top-level ops.

            if Cg_Visitor.Decl_Region = null then
               --  Make sure we have a non-null region
               Cg_Visitor.Decl_Region := Op_Sem.Nested_Region;
            end if;

            --  Fill in Location/Level/Info on parameters
            Code_Gen_List (Cg_Visitor, T.Operation_Outputs);
            Code_Gen_List (Cg_Visitor, T.Operation_Inputs);

            --  If has spec, fill in same info on spec in case
            --  pre/postconditions need it.
            if Op_Sem.Spec_Sem /= null then
               declare
                  Spec_Tree : Operation.Tree renames Operation.Tree
                    (Tree_Ptr_Of (Op_Sem.Spec_Sem.Definition).all);
               begin
                  Code_Gen_List (Cg_Visitor, Spec_Tree.Operation_Outputs);
                  Code_Gen_List (Cg_Visitor, Spec_Tree.Operation_Inputs);
               end;
            end if;

            if T.Is_Import then
               --  These are handled during the Pre_CG pass.
               null;
            elsif Op_Sem.Is_Abstract
              or else (T.Is_Optional and then Op_Sem.Body_Region = null)
            then
               --  No code to generate
               null;
            else
               --  Generate code for ParaSail routine
               declare
                  Max_Length : constant Code_Length_Type := Max_Code_Length;
                  Num_Instrs : Code_Length_Type renames Cg_Visitor.Num_Instrs;
                  Routine_Code : Code_Ptr := new Code_Type (Max_Length);
                  Routine_Slow_Calls : constant Slow_Call_Enum :=
                    Slow_Calls (T.Statements);
                  Routine_Nested_Blocks : constant Natural :=
                    Op_Sem.Num_Nested_Blocks +
                    Boundary_Condition_Array'Length;
                  Routine_Queuing : constant Boolean :=
                    Uses_Queuing (T.Statements);
                  Blocks : aliased Block_Info_Array_Wrapper
                    (Block_Index (Routine_Nested_Blocks));
                  Dequeue_Cond_Block : Block_Index := 0;
                  Enc_Module : constant Module_Sem_Ptr :=
                    Static.Find_Enclosing_Module_Interface
                      (Visitor.Decl_Region);
               begin
                  if Visitor.Decl_Region /= Symbols.Library_Region
                    and then Routine_Queuing
                    and then not Op_Sem.Uses_Queuing
                  then
                     Sem_Error
                       (T,
                        "Queuing used in operation but " &
                        "it is not marked as ""queued""");
                  end if;

                  if Enc_Module /= null
                    and then Enc_Module.Cur_Inst_Sem /= null
                  then
                     --  Create if necessary, and then save enclosing type desc
                     Build_Or_Find_Type_Descriptor (Enc_Module.Cur_Inst_Sem);

                     New_Routine.Enc_Type_Desc :=
                       Interpreter.Type_Descriptor_Ops.To_Type_Desc
                         (Interpreter.Type_Index
                            (Enc_Module.Cur_Inst_Sem.
                              Type_Descriptor_Location.Offset));
                  end if;

                  Num_Instrs := 0;

                  --  Point to current code array
                  Cg_Visitor.Current_Code := Routine_Code;

                  if Debug_Code_Gen then
                     Put_Line
                       (" Statements have Slow_Calls of " &
                        Slow_Call_Enum'Image (Routine_Slow_Calls));
                  end if;

                  if Routine_Slow_Calls in
                       Mandatory_Parallel_Call .. Independent_Slow_Calls
                  then
                     --  Allocate a master
                     Cg_Visitor.Local_Master :=
                       Cg_Visitor.Target_Local_Offset;
                     pragma Assert (Cg_Visitor.Local_Master > 0);

                     New_Routine.Local_Master :=
                       Cg_Visitor.Target_Local_Offset;
                     --  Save offset in New_Routine as well, though not
                     --  obvious we need it at "run time" (perhaps for
                     --  thread cancellation?)

                     Cg_Visitor.Target_Local_Offset :=
                       Cg_Visitor.Target_Local_Offset + Thread_Master_Size;

                     if Debug_Code_Gen then
                        Put_Line
                          ("Master needed, allocated at offset" &
                           Offset_Within_Area'Image (Cg_Visitor.Local_Master) &
                           ", # para blocks =" &
                           Natural'Image (Routine_Nested_Blocks));
                     end if;

                  end if;

                  Cg_Visitor.Nested_Blocks := Blocks'Unchecked_Access;
                  Cg_Visitor.Nested_Blocks.Info (0).Code :=
                    Cg_Visitor.Current_Code;

                  --  Generate code for preconditions
                  if Checking_Preconditions then
                     Code_Gen_List (Cg_Visitor, T.Operation_Inputs,
                       Annotation_Mode => Precondition_Mode);
                     Code_Gen (Cg_Visitor, T.Preconditions,
                       Annotation_Mode => Precondition_Mode);
                  end if;

                  if Checking_Postconditions then
                     --  Generate code to initialize entry temps
                     Code_Gen_List (Cg_Visitor, T.Operation_Outputs,
                       Annotation_Mode => Entry_Temp_Mode);
                     Code_Gen (Cg_Visitor, T.Postconditions,
                       Annotation_Mode => Entry_Temp_Mode);
                  end if;

                  --  Build up Instrs
                  Emit_Code_And_Finalize_Resolved_Tree
                    (T.Statements, Cg_Visitor);

                  if Num_Instrs = 0
                    or else not Cg_Visitor.Last_Instr_Escapes
                  then
                     --  Make sure we end with a Return_Op
                     if Checking_Postconditions then
                        Check_Postconditions (Cg_Visitor,
                          Op_Sem,
                          Source_Pos =>
                            Find_End_Source_Pos (Op_Sem.Definition));
                     end if;

                     Emit
                       (Cg_Visitor,
                        (Return_Op,
                         Source_Pos => Find_End_Source_Pos (Op_Sem.Definition),
                         Postcond_Proved => False));
                  end if;

                  New_Routine.Start_Callee_Locals :=
                    Cg_Visitor.Start_Callee_Locals;
                  New_Routine.Local_Area_Length :=
                    New_Routine.Start_Callee_Locals + 10;  --  TBD

                  New_Routine.Uses_Queuing := New_Routine.Uses_Queuing or
                                              Routine_Queuing;

                  if Not_Null (T.Dequeue_Condition) then
                     --  Generate code for dequeue condition
                     declare
                        Dq_Cond_Visitor : Code_Gen_Visitor;
                        Dq_Max_Length : constant Code_Length_Type :=
                          Max_Block_Length;
                        Dq_Cond_Code : Code_Ptr :=
                          new Code_Type (Dq_Max_Length);
                        Dq_Cond_Sem : constant Computation_Sem_Ptr :=
                          Computation_Sem_Ptr (Sem_Info
                                                  (T.Dequeue_Condition));
                        Starting_Offset : Offset_Within_Area := 0;
                        Dq_Cond_VM_Info : VM_Obj_Id_Type (Local_Kind);
                     begin
                        if Debug_Code_Gen then
                           Put_Line
                             (" Generating code for dequeue condition " &
                              Subtree_Image (T.Dequeue_Condition));
                        end if;
                        Emit_Nested_Block_Start
                          (Cg_Visitor,
                           Dq_Cond_Visitor,
                           Dq_Cond_Sem,
                           Block_Region => Cg_Visitor.Decl_Region,
                           New_Code => Dq_Cond_Code);

                        --  Remember the index
                        Dequeue_Cond_Block := Dq_Cond_Visitor.Current_Block;

                        --  Remember where the condition is evaluated
                        Starting_Offset :=
                          Dq_Cond_Visitor.Target_Local_Offset;

                        Dq_Cond_VM_Info := Assign_VM_Obj_Id (Dq_Cond_Visitor);
                        Dq_Cond_Visitor.Target_VM_Info := Dq_Cond_VM_Info;

                        --  Emit code for dequeue condition
                        Emit_Code_For_Resolved_Tree
                          (T.Dequeue_Condition,
                           Dq_Cond_Visitor);

                        --  Copy final boolean result to output at
                        --  (Param_Area, 0)
                        Emit
                          (Dq_Cond_Visitor,
                           (Copy_Word_Op,
                            Source_Pos => Find_Source_Pos
                                            (T.Dequeue_Condition),
                            Destination => (Param_Area, 0, No_VM_Obj_Id),
                            Dest_Name => Strings.Null_U_String_Index,
                            Source => (Local_Area, Starting_Offset,
                                       Dq_Cond_VM_Info),
                            Might_Be_Null => False));

                        --  Finish up
                        Emit_Nested_Block_Finish
                          (Cg_Visitor,
                           Dq_Cond_Visitor,
                           Uses_Queuing => Uses_Queuing (T.Dequeue_Condition));

                        Free_Code (Dq_Cond_Code);
                     end;
                  end if;

                  if Cg_Visitor.Nested_Blocks /= null
                    and then Cg_Visitor.Nested_Blocks.Num_Blocks_Generated > 0
                  then
                     --  Copy the nested blocks onto the end
                     --  and fix up the references
                     for B in
                          1 .. Cg_Visitor.Nested_Blocks.Num_Blocks_Generated
                     loop
                        declare
                           Info : Block_Info renames
                             Cg_Visitor.Nested_Blocks.Info (B);
                        begin
                           --  Remember starting instruction offset
                           Info.Cb.Pc_Offset := Num_Instrs;

                           --  Initialize info in Begin_Nested_Block_Op
                           pragma Assert (Info.Code.Instrs (1).Op =
                             Begin_Nested_Block_Op);
                           Info.Code.Instrs (1).Nested_Code_Block := Info.Cb;

                           --  Copy block's instructions to main code block
                           for I in 1 .. Info.Code.Code_Length loop
                              Emit (Cg_Visitor, Info.Code.Instrs (I),
                                VM_Checks_Off => True);
                           end loop;
                           Info.Code := null;
                        end;
                     end loop;

                     --  Now fix up the references
                     for B in
                          1 .. Cg_Visitor.Nested_Blocks.Num_Blocks_Generated
                     loop
                        declare
                           Info : Block_Info renames
                             Cg_Visitor.Nested_Blocks.Info (B);
                        begin

                           --
                           for I in Info.Invokers'Range loop
                              declare
                                 Invoking_Instr : Code_Length_Type :=
                                   Info.Invokers (I).Instr;
                              begin

                                 exit when Invoking_Instr = 0;  --  All done

                                 --  Adjust invoking instruction index if
                                 --  necessary
                                 if Info.Invokers (I).Block > 0 then
                                    declare
                                       Invoker : Block_Info renames
                                         Cg_Visitor.Nested_Blocks.Info (
                                         Info.Invokers (I).Block);
                                    begin
                                       --  Compute "absolute" instruction index
                                       Invoking_Instr := Invoking_Instr +
                                                         Invoker.Cb.Pc_Offset;
                                    end;
                                 end if;

                                 --  Compute offset from invoking instruction
                                 Code_Block
                                    (Cg_Visitor.Current_Code,
                                     Invoking_Instr).Pc_Offset :=
                                   Info.Cb.Pc_Offset - Invoking_Instr;
                                 Code_Block
                                    (Cg_Visitor.Current_Code,
                                     Invoking_Instr).Uses_Stg_Rgn :=
                                   Info.Cb.Uses_Stg_Rgn;
                              end;
                           end loop;

                        end;
                     end loop;
                  end if;

                  --  Finish building Routine
                  New_Routine.Code :=
                    new Code_Type'
                    (Code_Length => Num_Instrs,
                     Uses_Stg_Rgn => Routine_Code.Uses_Stg_Rgn,
                     Num_Locals => Routine_Code.Num_Locals,
                     Most_Recent_Var => 0,
                     Instrs => Routine_Code.Instrs (1 .. Num_Instrs));

                  if Dequeue_Cond_Block > 0 then
                     --  Dequeue cond becomes the internal precondition
                     if Debug_Code_Gen then
                        Put_Line
                          (" Internal_Precondition has code at" &
                           Code_Offset'Image
                              (Cg_Visitor.Nested_Blocks.Info (
                          Dequeue_Cond_Block).Cb.Pc_Offset));
                     end if;

                     New_Routine.Boundary_Conditions (Internal_Precondition)
                        :=
                       Cg_Visitor.Nested_Blocks.Info (Dequeue_Cond_Block).Cb;
                  end if;

                  if Op_Sem.Operation_Kind = Operation.Lambda_Operation
                    and then New_Op /= null
                    and then Visitor.Decl_Region.Kind /=
                      Operation_Param_Region_Kind
                  then
                     --  Add this symbol to enclosing region so compiler
                     --  can find the Routine to compile it.
                     Add_To_Region (Visitor.Decl_Region, New_Op);
                  end if;

                  Free_Code (Routine_Code);

               end;
            end if;

            if Debug_Code_Gen then
               --  Display routine.
               if Op_Sem.Index > 0 then
                  Put_Line
                    ("Operation #" &
                     Operation_Index'Image (Op_Sem.Index) &
                     " of module " &
                     Sym_Name (New_Op.Enclosing_Region.Associated_Symbol));
               end if;
               Dump_Routine (Routine_Ptr (New_Routine));
            end if;

            if Semantics.List_Routine_Instructions
              and then New_Op.Source_Pos.File /= Strings.Null_U_String_Index
            then
               --  Dump routine to listing file.
               declare
                  Listing_File : Ada.Text_IO.File_Type;
                  Listing_File_Name : constant String :=
                    Strings.To_String (Strings.To_U_String
                      (New_Op.Source_Pos.File)) & ".lst";
               begin
                  begin
                     Open (Listing_File, Append_File, Listing_File_Name);
                  exception
                     when E : others =>
                        Put_Line
                          (Current_Error,
                           "Error: Exception " &
                           Ada.Exceptions.Exception_Name (E) &
                           " when trying to open " &
                           Listing_File_Name &
                           " for append.");
                        raise;
                  end;
                  Set_Output (Listing_File);
                  New_Line;
                  Put_Line
                    (" ----- " & Languages.Language_Name &
                       " Virtual Machine Instructions ---- ");
                  if Op_Sem.Index > 0 then
                     Put_Line
                       ("Operation #" &
                        Operation_Index'Image (Op_Sem.Index) &
                        " of module " &
                        Sym_Name (New_Op.Enclosing_Region.Associated_Symbol));
                  end if;
                  if Visitor.Dest_Name /= Strings.Null_U_String_Index then
                     Put_Line
                       ("Operation used to initialize constant " &
                        Strings.To_String (Strings.To_U_String
                          (Visitor.Dest_Name)));
                  end if;
                  Dump_Routine (Routine_Ptr (New_Routine));
                  Set_Output (Standard_Output);
                  Close (Listing_File);
               exception
                  when E : others =>
                     Put_Line
                       (Current_Error,
                        "Error: Exception " &
                        Ada.Exceptions.Exception_Name (E) &
                        " raised while dumping operation #" &
                        Operation_Index'Image (Op_Sem.Index) &
                        " of module " &
                        Sym_Name (New_Op.Enclosing_Region.Associated_Symbol));
                     Close (Listing_File);
               end;
            end if;

         end;
      end if;

      if T.Operation_Kind = Operation.Lambda_Operation
        and then T.Sem_Info.all in Resolved_Operation_Info
      then
         --  Generate an operation descriptor
         declare
            use Interpreter;
            Call_Target : constant Object_Locator :=
              Routine_Locator
                (Op_Sem,
                 Assoc_Type => null,
                 Current_Level => Visitor.Current_Level,
                 Tree_For_Srcpos => Op_Sem.Definition,
                 Abstract_Allowed => False);
            Static_Link : constant Object_Locator :=
              Static_Link_For_Call
                (Op_Sem => Op_Sem,
                 Current_Level => Visitor.Current_Level,
                 Assoc_Type_Region => null,
                 Referring_Module => null,
                 Polymorphic_Param_Index => 0);
         begin
            Emit (Visitor,
              (Store_Operation_Desc_Op,
               Source_Pos => Operation.Find_Source_Pos (T),
               Destination =>
                 (Local_Area, Visitor.Target_Local_Offset,
                  Visitor.Target_VM_Info),
               Dest_Name => Visitor.Dest_Name,
               Source => Visitor.Target_Object,
               Might_Be_Null => True,
               Operation_Locator => Call_Target,
               Operation_Static_Link => Static_Link));

            Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;
         end;
      end if;
   end Operation_Action;

   procedure Obj_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Obj_Decl.Tree) is
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      if T.Sem_Info /= null then
         declare
            Obj_Sem : constant Object_Sem_Ptr := Object_Sem_Ptr (T.Sem_Info);

            New_Obj : constant Symbols.Sym_Ptr := Obj_Sem.Associated_Symbol;
            Obj_Type : constant Type_Sem_Ptr := Obj_Sem.Resolved_Type;
            Dest_Name : constant Strings.U_String_Index :=
                                   Strings.Index (New_Obj.Str);
            use Interpreter;
         begin

            if Visitor.Current_Code = null then
               --  Must be a local of a module.
               if New_Obj.Kind = Object_Sym_Kind then
                  --  This must be a global object (var or constant).
                  --  Assign it a location in the type descriptor
                  --  or in the global const table if is a compile-time-known
                  --  constant.
                  declare
                     Constant_Index : Natural;
                  begin
                     if Enc_Module = null
                       or else
                         (not Static.Sym_Is_Variable (New_Obj)
                             and then
                           (Enc_Module.Cur_Inst_Sem.All_Parameters_Known
                             or else
                            Static.Is_Compile_Time_Known
                                  (Resolved_Tree (T.Obj_Value),
                                   Disallow_Concurrent_Types => True)))
                     then
                        --  Add to list of compile-time-known constants
                        Add_Element
                          (Compile_Time_Known_Const_Table,
                           Sem_Ptr (Obj_Sem),
                           Obj_Sem_Info_Index (Constant_Index));
                        Obj_Sem.Info.Obj_Location :=
                          (Const_Area,
                           Offset_Within_Area (Constant_Index), No_VM_Obj_Id);
                        Obj_Sem.Info.Obj_Level := Static_Level'First;

                        --  Make sure that we actually compute the value.
                        Obj_Sem.Needs_Anon_Const := True;
                     else
                        --  Add to list of nested objects of enclosing module
                        Add_Element
                          (Enc_Module.Nested_Objects,
                           Sem_Ptr (Obj_Sem),
                           Obj_Sem_Info_Index (Constant_Index));

                        Obj_Sem.Info.Obj_Location :=
                          (Type_Area,
                           Type_Nested_Obj_Offsets'First +
                             Offset_Within_Area (Constant_Index),
                           No_VM_Obj_Id);
                        Obj_Sem.Info.Obj_Level := Visitor.Current_Level;
                        --  TBD: Is this right for a module constant?
                     end if;

                     if Debug_Code_Gen then
                        Put_Line
                          ("Assigning location " &
                           Obj_Locator_Image (Obj_Sem.Info.Obj_Location) &
                           " to global object " &
                           Sym_Name (New_Obj) &
                           "; sym_index =" &
                           Sym_Index'Image (New_Obj.Index));
                     end if;
                     return;
                  end;
               else
                  --  A component; nothing to do at this point
                  pragma Assert (New_Obj.Kind = Component_Sym_Kind);
                  if Debug_Code_Gen then
                     Put_Line
                       ("No code to generate for component decl " &
                        Sym_Name (New_Obj) &
                        "; sym_index =" &
                        Sym_Index'Image (New_Obj.Index));
                  end if;
               end if;
               return;   ------------------
            end if;

            if Debug_Code_Gen then
               Put_Line
                 ("Generate code for object decl " &
                  Sym_Name (New_Obj) &
                  "; sym_index =" &
                  Sym_Index'Image (New_Obj.Index));
            end if;
            --  Generate code for object declaration

            --  Remember location of new object
            Obj_Sem.Info.Obj_Location :=
              (Local_Area,
               Visitor.Target_Local_Offset,
               No_VM_Obj_Id);  --  Will be filled in later
            Obj_Sem.Info.Obj_Level := Visitor.Current_Level;
            --  TBD: Might be more than one word long once we support
            --      operations with multiple outputs.

            pragma Assert (not Visitor.Gen_Parallel_Invocations_Only);
            --  NOTE: Should never get here in statement context

            if T.Is_Const
              and then not T.Is_Ref
              and then not T.Is_Move
              and then Is_Null (T.In_Region)
              and then Not_Null (T.Obj_Value)
              and then Static.Is_Compile_Time_Known (T.Obj_Value,
                            Disallow_Concurrent_Types => True)
            then
               --  Local constant with compile-time-known initial value.
               --  Create a global anon constant and use that.
               declare
                  Local_CTK_Value : constant Optional_Tree :=
                    Resolved_Tree (T.Obj_Value);
                  Local_CTK_Sem : constant Sem_Ptr :=
                    Sem_Ptr (Sem_Info (Local_CTK_Value));
                  Anon_Const_Ref : constant Anon_Const_Tables.Element_Ref :=
                    Find_Element (Anon_Const_Table,
                                  Local_CTK_Value);
                  use type Anon_Const_Tables.Element_Ref;
                  Const_Index : Obj_Sem_Info_Index := 0;
                  Existing_Elem : Anon_Const_Tables.Element_Ref;
               begin
                  if Anon_Const_Ref /= null then
                     --  Already in table
                     Const_Index := Anon_Const_Ref.all;
                  else
                     --  Add to list of compile-time-known constants.
                     Add_Element
                       (Compile_Time_Known_Const_Table,
                        Local_CTK_Sem,
                        Const_Index);
                     --  Add to mapping from tree to global-const index.
                     Enter_Element (Anon_Const_Table, Local_CTK_Value,
                       Const_Index, Existing_Elem);

                  end if;

                  if (Static.Known_To_Be_Small (Obj_Type)
                        or else Static.Unwrapped_Type (Obj_Type) =
                                  Univ_String_Type)
                    and then
                      Local_CTK_Sem.all not in Literal_Semantic_Info'Class
                  then
                     --  We can actually use the anon constant
                     if Local_CTK_Sem.all in
                       Computation_Semantic_Info'Class
                     then
                        Computation_Sem_Ptr
                          (Local_CTK_Sem).Needs_Anon_Const := True;
                     end if;

                     Obj_Sem.Info.Obj_Location :=
                       (Const_Area, Offset_Within_Area (Const_Index),
                        No_VM_Obj_Id);

                     if Debug_Code_Gen then
                        Put_Line
                          ("Using global constant at location " &
                           Obj_Locator_Image (Obj_Sem.Info.Obj_Location) &
                           " for initial value of " &
                           Sym_Name (New_Obj) &
                           "; sym_index =" &
                           Sym_Index'Image (New_Obj.Index));
                     end if;

                     return;  --  All done  --
                  end if;

                  --  NOTE: Needs_Anon_Const not yet set True,
                  --        so might not actually ever compute the
                  --        global constant.
               end;

            end if;

            --  Allocate unique ID for local object; make it a VM variable
            --  if not a "ref," and is a ParaSail var or a large constant.
            Obj_Sem.Info.Obj_Location.VM_Obj_Id :=
              Assign_VM_Obj_Id (Visitor,
                Needs_Var => not Static.Sym_Is_By_Ref (New_Obj)
                  and then (Static.Sym_Is_Variable (New_Obj)
                      or else not Static.Known_To_Be_Small (Obj_Type)));

            Emit
              (Visitor,
               (Declare_Obj_Op,
                Source_Pos => Obj_Decl.Find_Source_Pos (T),
                Destination => Obj_Sem.Info.Obj_Location,
                Dest_Name => Dest_Name,
                Is_By_Ref => Static.Sym_Is_By_Ref (New_Obj),
                Is_Var => Static.Sym_Is_Variable (New_Obj),
                Declare_Type_Info => Run_Time_Type_Info
                                    (Obj_Type,
                                     Referring_Module => Enc_Module)));

            if Static.Known_To_Be_Small (Obj_Type) then
               --  Initialize to null if no initial value specified.
               --  NOTE: This is not really necessary unless declared optional.
               if Is_Null (T.Obj_Value) then
                  --  Initialize to appropriate null
                  Emit
                    (Visitor,
                     (Store_Local_Null_Op,
                      Source_Pos => Obj_Decl.Find_Source_Pos (T),
                      Destination => Obj_Sem.Info.Obj_Location,
                      Dest_Name => Dest_Name,
                      Null_Type_Info => Run_Time_Type_Info
                                          (Obj_Type,
                                           Referring_Module => Enc_Module)));
               end if;
            elsif Not_Null (T.In_Region) then
               --  The "for Obj" clause specifies that we want to
               --  allocate this variable in the same region as Obj.
               if Static.Sym_Is_By_Ref (New_Obj) or else T.Is_Move then
                  Sem_Error
                    (T.In_Region,
                     "May not specify region with ""ref"" or ""<==""");
               end if;

               --  Protect new object
               Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

               --  Get location of existing object
               Visitor.Is_Lvalue_Context := True;
               Emit_Code_For_Resolved_Tree (T.In_Region, Visitor);
               Visitor.Is_Lvalue_Context := False;

               --  Associate with same region as existing object
               --  TBD: Need to be sure that this gets null-ed out
               --      on scope exit to avoid storage leak.
               Emit
                 (Visitor,
                  (Store_Null_Of_Same_Stg_Rgn_Op,
                   Source_Pos => Find_Source_Pos (T.In_Region),
                   Destination => Obj_Sem.Info.Obj_Location,
                   Dest_Name => Dest_Name,
                   Source => Visitor.Lvalue_Location,
                   Might_Be_Null => True,
                   Type_Info => Run_Time_Type_Info
                                  (Obj_Type,
                                   Referring_Module => Enc_Module)));

               Visitor.Lvalue_Location := Null_Object_Locator;

               --  Set back Target_Local_Offset so evaluation of
               --  initial value happens into proper place.
               Visitor.Target_Local_Offset :=
                 Obj_Sem.Info.Obj_Location.Offset;

            elsif not Static.Sym_Is_By_Ref (New_Obj)
              and then not T.Is_Move
            then
               --  Initialize possibly-large object to null for local region
               Emit
                 (Visitor,
                  (Store_Local_Null_Op,
                   Source_Pos => Obj_Decl.Find_Source_Pos (T),
                   Destination => Obj_Sem.Info.Obj_Location,
                   Dest_Name => Dest_Name,
                   Null_Type_Info => Run_Time_Type_Info
                                       (Obj_Type,
                                        Referring_Module => Enc_Module)));
            end if;

            if Not_Null (T.Obj_Value) then
               --  Initialized object

               --  Allocate space for finalizable temps, if any
               Allocate_Finalizable_Temps
                 (Visitor, Num_Finalizable_Temps (T.Obj_Value));

               if Static.Sym_Is_By_Ref (New_Obj) or else T.Is_Move then
                  if Visitor.Target_Local_Offset =
                    Obj_Sem.Info.Obj_Location.Offset
                  then
                     --  Protect new object
                     Visitor.Target_Local_Offset :=
                       Visitor.Target_Local_Offset + 1;
                  end if;

                  --  Get location of existing object
                  Visitor.Is_Lvalue_Context := True;
                  Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);
                  Visitor.Is_Lvalue_Context := False;

                  if T.Is_Move then
                     --  Move contents of existing obj and set existing obj
                     --  null

                     if not Static.Known_To_Be_Small (Obj_Type) then
                        --  Associate with same region as existing object
                        --  TBD: Need to be sure that this gets null-ed out
                        --      on scope exit to avoid storage leak.
                        Emit
                          (Visitor,
                           (Store_Null_Of_Same_Stg_Rgn_Op,
                            Source_Pos => Obj_Decl.Find_Source_Pos (T),
                            Destination => Obj_Sem.Info.Obj_Location,
                            Dest_Name => Dest_Name,
                            Source => Visitor.Lvalue_Location,
                            Might_Be_Null => True,
                            Type_Info => Run_Time_Type_Info
                                           (Obj_Type,
                                            Referring_Module => Enc_Module)));
                     end if;

                     --  Do the move
                     Emit
                       (Visitor,
                        (Move_Obj_Op,
                         Source_Pos => Obj_Decl.Find_Source_Pos (T),
                         Destination => Obj_Sem.Info.Obj_Location,
                         Dest_Name => Dest_Name,
                         Source => Visitor.Lvalue_Location,
                         Might_Be_Null =>
                           Resolved_Type (T.Obj_Value).Value_Is_Optional,
                         Type_Info => Run_Time_Type_Info
                                        (Obj_Type,
                                         Referring_Module => Enc_Module)));
                  else
                     --  Establish a reference
                     pragma Assert (Static.Sym_Is_By_Ref (New_Obj));

                     --  Make sure the reference is where we want it
                     if Visitor.Lvalue_Location.Base /=
                         Phys_Base_Register (Obj_Sem.Info.Obj_Location.Offset)
                       or else Visitor.Lvalue_Location.Offset /= 0
                     then
                        --  Need to store the address of existing object
                        Emit
                          (Visitor,
                           (Store_Address_Op,
                            Source_Pos => Obj_Decl.Find_Source_Pos (T),
                            Destination => Obj_Sem.Info.Obj_Location,
                            Dest_Name => Dest_Name,
                            Source => Visitor.Lvalue_Location,
                            Might_Be_Null =>
                              Resolved_Type (T.Obj_Value).Value_Is_Optional));
                     end if;

                  end if;
               else
                  --  Just evaluate and assign the initial value.
                  declare
                     Result_Offset : constant Offset_Within_Area :=
                       Visitor.Target_Local_Offset;
                     --  NOTE: Target_Local_Offset determines stack offset
                     --        where initial value ends up
                  begin
                     if not Static.Known_To_Be_Small (Obj_Type) then
                        --  Set target, so initial value will be copied
                        --  into proper region.
                        --  We do *not* want to do this unless the object
                        --  is initialized to an appropriate "null" value.
                        --  That doesn't happen for known-small objects.
                        Visitor.Target_Object := Obj_Sem.Info.Obj_Location;
                        --  NOTE: Determines region for copy
                     end if;

                     --  Carry over the target VM reg num
                     Visitor.Target_VM_Info :=
                       Obj_Sem.Info.Obj_Location.VM_Obj_Id;

                     --  Set name for target
                     Visitor.Dest_Name := Dest_Name;

                     Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);

                     if Result_Offset /= Obj_Sem.Info.Obj_Location.Offset then
                        --  Move result to proper offset
                        Emit
                          (Visitor,
                           (Copy_Word_Op,
                            Source_Pos => Obj_Decl.Find_Source_Pos (T),
                            Destination => Obj_Sem.Info.Obj_Location,
                            Dest_Name => Visitor.Dest_Name,
                            Source => (Local_Area, Result_Offset,
                                       Obj_Sem.Info.Obj_Location.VM_Obj_Id),
                            Might_Be_Null => True));
                     end if;

                     --  Reset to indicate no target
                     Visitor.Dest_Name := Strings.Null_U_String_Index;
                     Visitor.Target_Object := Null_Object_Locator;
                     Visitor.Target_VM_Info := No_VM_Obj_Id;

                  end;
               end if;
               if not Static.Sym_Is_By_Ref (New_Obj)
                 and then Static.Is_Unlocked_Concurrent_Operand
                             (Operand_Sem_Ptr (Obj_Sem))
               then
                  --  Make sure object has a lock
                  Emit
                    (Visitor,
                     (Create_Lock_For_Obj_Op,
                      Source_Pos => Obj_Decl.Find_Source_Pos (T),
                      Destination => Obj_Sem.Info.Obj_Location,
                      Dest_Name => Dest_Name));
               end if;

               if not T.Is_Ref then
                  --  Do finalization of initial value now
                  Finalize_Result_And_Ref_Operands (Visitor, T.Obj_Value);
               end if;

            elsif Static.Is_Unlocked_Concurrent_Operand
                     (Operand_Sem_Ptr (Obj_Sem))
            then
               Sem_Error
                 (T,
                  "Concurrent object must be initialized on declaration");
            end if;

            if T.Is_Ref then
               --  Protect the space used by the new object and any
               --  finalizable temps
               Check_And_Set_Local_Offset
                 (Visitor,
                  Obj_Sem.Info.Obj_Location.Offset +
                    Offset_Within_Area
                      (Num_Finalizable_Temps (T.Obj_Value) + 1));
            else
               --  Protect the space used by the new object
               Check_And_Set_Local_Offset
                 (Visitor,
                  Obj_Sem.Info.Obj_Location.Offset + 1);
            end if;

            if Debug_Code_Gen then
               Put_Line
                 ("  Assigned location (Local_Area," &
                  Offset_Within_Area'Image (Obj_Sem.Info.Obj_Location.Offset) &
                  ')');
            end if;
         end;
      end if;
   end Obj_Decl_Action;

   procedure Param_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Param_Decl.Tree) is
      Param_Sem : Param_Semantic_Info
        renames Param_Semantic_Info (T.Sem_Info.all);

      New_Param : constant Symbols.Sym_Ptr := Param_Sem.Associated_Symbol;
      use Interpreter;
      use type Symbols.Sym_Ptr;
      Referring_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      if Visitor.Annotation_Mode /= Normal_Mode then
         --  Nothing to do for this mode
         return;
      end if;

      if New_Param.Enclosing_Region.Associated_Symbol /= null
        and then New_Param.Enclosing_Region.Associated_Symbol.Kind =
                 Module_Sym_Kind
      then
         --  Module parameter
         Param_Sem.Info.Obj_Location :=
           (Type_Area,
            Offset_Within_Area (Static.Module_Formal_Index (New_Param)),
            No_VM_Obj_Id);
         --  First module parameter has offset 1
      elsif Visitor.New_Routine /= null then
         --  Operation parameter
         Param_Sem.Info.Obj_Location :=
           (Param_Area,
            Offset_Within_Area (New_Param.Index) - 1,
            No_VM_Obj_Id);
         --  First operation parameter has offset 0

         --  Fill in parameter info
         Visitor.New_Routine.Parameters (Positive (New_Param.Index)) :=
           Routine_Param_Info'
           (Compiled => False,
            Kind => Formal_Object,
            Type_Info => Run_Time_Type_Info
                           (Param_Sem.Resolved_Type,
                            Referring_Module => Referring_Module),
            Is_Operation_Output => Param_Sem.Is_Operation_Output,
            Is_Var => Static.Sym_Is_Variable (New_Param),
            Is_Passed_By_Ref => Static.Sym_Is_By_Ref (New_Param),
            Is_Optional => Param_Sem.Resolved_Type.Value_Is_Optional,
            Is_Of_Current_Inst_Type => False,  --  Filled in below
            Is_Declared_Ref => Static.Sym_Is_Declared_Ref (New_Param),

         --  for debugging only:
            Name => New_Param.Str,
            Type_Name => Strings.String_Lookup
              (Canonical_Type_Name (Param_Sem.Resolved_Type)),
            Decl => Param_Sem.Definition);

         if Referring_Module /= null
           and then Types_Match
                       (Param_Sem.Resolved_Type,
                        Referring_Module.Cur_Inst_Sem)
           and then not Param_Sem.Resolved_Type.Is_Polymorphic
         then
            --  Remember that this parameter is of the "current-inst" type.
            Visitor.New_Routine.Parameters (Positive (New_Param.Index)).
              Is_Of_Current_Inst_Type := True;
         end if;
      end if;

      Param_Sem.Info.Obj_Level := Visitor.Current_Level;

      if Debug_Code_Gen then
         Put_Line
           ("Generate code for parameter " &
            Sym_Name (New_Param) &
            "; sym_index =" &
            Sym_Index'Image (New_Param.Index) &
            "; location = " &
            Obj_Locator_Image (Param_Sem.Info.Obj_Location));
      end if;

   end Param_Decl_Action;

   procedure Type_Decl_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Type_Decl.Tree) is
      Type_Desc_Loc : constant Interpreter.Object_Locator :=
        Run_Time_Type_Info
           (Type_Sem_Ptr (T.Sem_Info),
            Referring_Module =>
               Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region));
   begin
      null;
   end Type_Decl_Action;

   procedure Unary_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Unary.Tree) is
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);
      use Unary;
      use Interpreter;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      Orig_Target : constant Object_Locator := Visitor.Target_Object;
      Orig_Target_VM_Info : constant VM_Obj_Id_Type :=
        Visitor.Target_VM_Info;
      Temp_VM_Info : VM_Obj_Id_Type (Local_Kind);
   begin

      case T.Operator is
         when Initial_Value_Op =>
            if Visitor.Gen_Parallel_Invocations_Only then
               --  Nothing more to do
               if Debug_Code_Gen then
                  Put_Line ("Skipping initial-value op");
               end if;
               return;
            end if;

            if Visitor.Enclosing_For_Loop = null then
               Sem_Error (T, "Initial-value op only permitted " &
                 "inside a map-reduce expression");
            else
               --  Load current loop result
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Unary.Find_Source_Pos (T),
                   Destination => (Local_Area,
                                   Visitor.Target_Local_Offset,
                                   Orig_Target_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source =>
                     Adjust_For_Level_And_Prefix
                       (Visitor.Current_Level,
                        Obj_Location =>
                          Visitor.Enclosing_For_Loop.Loop_Result_Locator,
                        Obj_Level =>
                          Visitor.Enclosing_For_Loop.For_Loop_Level),
                   Might_Be_Null => True));

               Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

            end if;

         when Plus_Op | Minus_Op | Not_Op | Abs_Op |
           Meaning_Op | Magnitude_Op =>
            --  Emit appropriate call
            if Comp_Sem.Op_Sem /= null then
               Emit_Call
                 (Visitor,
                  Call_Sem_Ptr (Comp_Sem),
                  Lists.Make ((1 => T.Operand)));
            else
               Sem_Error (T, " Unary operator not supported by type");
            end if;

         when Is_Null_Op | Not_Null_Op =>
            --  Emit a test for null
            --  Evaluate the operand; never need to make a copy.
            Temp_VM_Info := Assign_VM_Obj_Id (Visitor);
            Visitor.Target_Object := Null_Object_Locator;
            Visitor.Target_VM_Info := Temp_VM_Info;
            Emit_Code_For_Resolved_Tree (T.Operand, Visitor);
            Visitor.Target_Object := Orig_Target;  --  Restore target

            if Visitor.Gen_Parallel_Invocations_Only then
               --  Nothing more to do
               if Debug_Code_Gen then
                  Put_Line ("Skipping rest of null-checking op");
               end if;
               return;
            end if;

            if T.Operator = Is_Null_Op then
               --  Test whether is null
               Emit
                 (Visitor,
                  (Is_Null_Op,
                   Source_Pos => Unary.Find_Source_Pos (T),
                   Destination => (Local_Area,
                                   Visitor.Target_Local_Offset - 1,
                                   Orig_Target_VM_Info),
                   Dest_Name => Visitor.Dest_Name,
                   Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                              Temp_VM_Info),
                   Might_Be_Null => True,
                   Type_Info =>
                      Run_Time_Type_Info
                        (Operand_Sem_Ptr (Sem_Info (T.Operand)).Resolved_Type,
                         Referring_Module => Enc_Module)));
            else
               --  Test whether not null
               --  NOTE: We have to duplicate the aggregate
               --       because the discriminant needs to be static.
               Emit
                 (Visitor,
                  (Not_Null_Op,
                   Source_Pos => Unary.Find_Source_Pos (T),
                   Destination => (Local_Area,
                                   Visitor.Target_Local_Offset - 1,
                                   Orig_Target_VM_Info),
                   Dest_Name => Visitor.Dest_Name,
                   Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                              Temp_VM_Info),
                   Might_Be_Null => True,
                   Type_Info =>
                      Run_Time_Type_Info
                        (Operand_Sem_Ptr (Sem_Info (T.Operand)).Resolved_Type,
                         Referring_Module => Enc_Module)));
            end if;

         when Updated_Value_Op =>
            if Visitor.Annotation_Mode not in Postcondition_Mode then
               Sem_Warning
                 ("Updated-value (') operator should be in a postcondition",
                  Find_Source_Pos (T));
            end if;
            Emit_Code_For_Resolved_Tree (T.Operand, Visitor);
      end case;

   end Unary_Action;

   procedure Binary_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Binary.Tree) is
      use Binary;
      use Interpreter;
      Comp_Sem : constant Computation_Sem_Ptr :=
        Computation_Sem_Ptr (T.Sem_Info);

      --  Calls should not come through here
      pragma Assert (Comp_Sem.all not in Call_Semantic_Info);
   begin

      if T.Operator in Independent_Stmt_Ops then
         declare
            Master_Was_Started : constant Boolean :=
              Visitor.Master_Is_Started;
         begin
            if Visitor.Gen_Parallel_Invocations_Only then
               --  Should never be true for statement ops
               Sem_Error
                 (T,
                  "Internal: Did not expect " &
                  "Parallel_Invocations_Only for stmts");
            end if;

            --  evaluate each side in a parallel block (even if trivial)

            --  Record level/offset of relevant master
            pragma Assert (Visitor.Local_Master > 0);

            Comp_Sem.Enclosing_Master := Visitor.Local_Master;
            Comp_Sem.Level := Visitor.Current_Level;

            --  Evaluate right operand in parallel block
            --  NOTE: We do this one first so Master_Is_Started will
            --       be True by the time we get into the left operand.
            Emit_Nested_Block
              (Visitor, Comp_Sem, Comp_Sem.Nested_Region, T.Right_Operand,
               Is_Handled_Stmt_Op => T.Operator = Binary.Handled_Stmt_Op);

            if Binary.Is_Parallel_Stmt_Op (T.Left_Operand) then
               --  No need to evaluate left operand in a parallel block since
               --  it will evaluate its two operands in parallel already.
               Emit_Code_For_Resolved_Tree (T.Left_Operand, Visitor);
            else
               --  Evaluate left operand in parallel block
               Emit_Nested_Block (Visitor, Comp_Sem,
                 Comp_Sem.Nested_Region.Next_Sibling_Region, T.Left_Operand);
            end if;

            if not Master_Was_Started then
               --  This is the outermost "||", so
               --  wait for master shared by all of the nested blocks.
               Emit_Wait_For_Parallel (Visitor,
                 Source_Pos => Binary.Find_Source_Pos (T));

            end if;
         end;

      elsif T.Operator in Short_Circuit_Ops then
         --  These are converted into the equivalent "if" expression
         --  X and then Y --->  (if X then Y else #false)
         --  X or else Y --->  (if X then #true else Y)
         --  X ==> Y --->  (if X then Y else #true)
         --    the first two can be optimized to:
         --  X and then Y --> if X then Y else X
         --  X or else Y  --> if X then X else Y

         --  TBD: Use parameterless lambda's for these some day and allow
         --       them to be defined just like regular operators.
         declare
            If_Instr, Skip_Instr : Code_Index;
            Orig_Target_VM_Info : constant VM_Obj_Id_Type :=
              Visitor.Target_VM_Info;
            Result_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Visitor, Needs_Var => True);
            Left_VM_Info : VM_Obj_Id_Type := Result_VM_Info;
            Enc_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
         begin
            Emit
              (Visitor,
               (Declare_Obj_Op,
                Source_Pos => Binary.Find_Source_Pos (T),
                Destination => (Local_Area,
                                Visitor.Target_Local_Offset,
                                Result_VM_Info),
                Dest_Name => Strings.Null_U_String_Index,
                Is_By_Ref => False,
                Is_Var => True,
                Declare_Type_Info => Run_Time_Type_Info
                                    (Comp_Sem.Resolved_Type,
                                     Referring_Module => Enc_Module)));

            if T.Operator = Implies_Op then
               --  For "implies" we use a VM register without indir
               Left_VM_Info := Assign_VM_Obj_Id (Visitor);
            end if;

            Visitor.Target_VM_Info := Left_VM_Info;

            --  Evaluate left operand
            Emit_Code_For_Resolved_Tree (T.Left_Operand, Visitor);

            if Visitor.Gen_Parallel_Invocations_Only then
               --  Nothing more to do
               if Debug_Code_Gen then
                  Put_Line ("Skipping rest of short-circuit op");
               end if;
               Visitor.Target_VM_Info := Orig_Target_VM_Info;
               return;
            end if;

            --  Emit an "if" op
            Emit
              (Visitor,
               (If_Op,
                Source_Pos => Binary.Find_Source_Pos (T),
                If_Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                              Left_VM_Info),
                If_Condition => Boolean_Is_True,  --  if True
                Skip_If_False => 0));  --  will be filled in below

            If_Instr := Visitor.Num_Instrs;

            case Short_Circuit_Ops'(T.Operator) is
               when And_Then_Op | Implies_Op =>
                  --  Evaluate the right operand into same location
                  Visitor.Target_Local_Offset :=
                    Visitor.Target_Local_Offset - 1;
                  Visitor.Target_VM_Info := Result_VM_Info;
                  Emit_Code_For_Resolved_Tree (T.Right_Operand, Visitor);

               when Or_Else_Op =>
                  --  Leave value as is (i.e. #true)
                  null;
            end case;

            --  Skip over else part
            Emit
              (Visitor,
               (Skip_Op,
                Source_Pos => Binary.Find_Source_Pos (T),
                Skip_Count => 0));
            --  Count will be filled in below
            Skip_Instr := Visitor.Num_Instrs;

            --  Fix up "skip_if_false" amount.
            Visitor.Current_Code.Instrs (If_Instr).Skip_If_False :=
              Skip_Instr - If_Instr;

            --  Emit else part
            case Short_Circuit_Ops'(T.Operator) is
               when And_Then_Op =>
                  --  Leave value as is (#false)
                  null;

               when Implies_Op =>
                  --  Store a #true
                  Emit
                    (Visitor,
                     (Store_Int_Lit_Op,
                      Source_Pos => Binary.Find_Source_Pos (T),
                      Destination => (Local_Area,
                                      Visitor.Target_Local_Offset - 1,
                                      Result_VM_Info),
                      Dest_Name => Visitor.Dest_Name,
                      Int_Value => Boolean'Pos (True)));

               when Or_Else_Op =>
                  --  Evaluate the right operand into same location
                  Visitor.Target_Local_Offset :=
                    Visitor.Target_Local_Offset - 1;
                  Visitor.Target_VM_Info := Result_VM_Info;
                  Emit_Code_For_Resolved_Tree (T.Right_Operand, Visitor);
            end case;

            --  Fix up skip around else-part
            Visitor.Current_Code.Instrs (Skip_Instr).Skip_Count :=
              Visitor.Num_Instrs - Skip_Instr;

            --  Restore target info
            Visitor.Target_VM_Info := Orig_Target_VM_Info;

            --  Do a self-to-self copy, but with different VM reg info
            Emit
              (Visitor,
               (Copy_Word_Op,
                Source_Pos => Binary.Find_Source_Pos (T),
                Destination => (Local_Area,
                                Visitor.Target_Local_Offset - 1,
                                Orig_Target_VM_Info),
                Dest_Name => Visitor.Dest_Name,
                Source => (Local_Area,
                           Visitor.Target_Local_Offset - 1,
                           Result_VM_Info),
                Might_Be_Null => False));
         end;
      else
         --  Treat all other operators as equivalent to ";"
         Emit_Code_For_Resolved_Tree (T.Left_Operand, Visitor);

         if Visitor.Gen_Parallel_Invocations_Only then
            --  Nothing more to do
            --  TBD: This isn't quite right.  We want to execute
            --      stuff in parallel that occurs later in the
            --      statement sequence.  Not quite sure how to
            --      make that happen...
            if Debug_Code_Gen then
               Put_Line ("Skipping rest of statement sequence");
            end if;
            return;
         end if;

         Visitor.Target_Object := Null_Object_Locator;
         Emit_Code_For_Resolved_Tree (T.Right_Operand, Visitor);
      end if;
      Check_High_Water (Visitor);
   end Binary_Action;

   procedure Annotation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Annotation.Tree) is
      Annotation_Sem : constant Annotation_Sem_Ptr :=
        Annotation_Sem_Ptr (T.Sem_Info);
      use type Interpreter.Code_Ptr;
   begin
      if Visitor.Current_Code = null
        or else Annotation_Sem = null
        or else Lists.Is_Empty (T.Annotations)
        or else Static.All_Elems_Are_Refs (T.Annotations)
      then
         --  Nothing to do
         return;
      elsif Is_Null (Annotation_Sem.Decl_For_Annotations)
          or else Visitor.Annotation_Mode in Pre_Post_Condition_Mode
      then
         --  Generate "Check_Nested_Block_Op" call as this seems to be
         --  a "simple" statement-level assertion,
         --  or a pre/postcondition.

         if Visitor.Annotation_Mode /= Entry_Temp_Mode
           or else Annotation_Sem.Entry_Exit_Info.Num_Entry_Temps > 0
         then
            --  If in Entry_Temp_Mode, we only generate block if temps needed
            Emit_Block_For_Check
              (Visitor,
               Computation_Sem_Ptr (Annotation_Sem),
               T);
         end if;
      end if;

   end Annotation_Action;

   procedure Identifier_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Identifier.Tree) is
      --  Load value of identifier/literal into Visitor.Target_Local_Offset
      --  and bump Visitor.Target_Local_Offset by 1.
      --  If Visitor.Is_Lvalue_Context is True, then place address
      --  in Visitor.Lvalue_Location and use Target_Local_Offset as a temp
      --  if needed.
      Id : String renames Sym_Name (T);
   begin
      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing to do
         if Debug_Code_Gen then
            Put_Line ("Skipping Identifier/Lit '" & Id & "'");
         end if;
         return;
      end if;

      if Debug_Code_Gen then
         Put_Line ("Generate code for Identifier/Lit '" & Id & "'");
      end if;

      if T.Sem_Info.all not in Operand_Semantic_Info'Class then
         Sem_Error (T, Id & " is neither a parameter nor a local object");
      else
         Identifier_Code_Gen (Visitor, Operand_Sem_Ptr (T.Sem_Info));
      end if;

   end Identifier_Action;

   procedure Property_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Property.Tree) is
      --  Load value of property into Visitor.Target_Local_Offset
      --  and bump Visitor.Target_Local_Offset by 1.
      --  If Visitor.Is_Lvalue_Context is True, then place address
      --  in Visitor.Lvalue_Location and use Target_Local_Offset as a temp
      --  if needed.
      use Property;
   begin
      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing to do
         if Debug_Code_Gen then
            Put_Line ("Skipping Property '" & Subtree_Image (T) & "'");
         end if;
         return;
      end if;

      if Debug_Code_Gen then
         Put_Line ("Generate code for Property '" & Subtree_Image (T) & "'");
      end if;

      if T.Sem_Info.all not in Operand_Semantic_Info'Class then
         Sem_Error (T, Subtree_Image (T) &
           " does not denote an object");
      else
         Identifier_Code_Gen (Visitor, Operand_Sem_Ptr (T.Sem_Info));
      end if;

   end Property_Action;

   procedure Qualified_Name_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Qualified_Name.Tree) is
      use Interpreter;
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
   begin
      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing to do
         if Debug_Code_Gen then
            Put_Line ("Skipping qualified_name '" & Subtree_Image (T) & "'");
         end if;
         return;
      end if;

      if Debug_Code_Gen then
         Put_Line
           ("Generate code for qualified name '" & Subtree_Image (T) & "'");
      end if;

      if T.Sem_Info.all not in Operand_Semantic_Info'Class then
         Sem_Error (T, Subtree_Image (T) &
           " is neither a parameter nor a local object");
      else
         Identifier_Code_Gen (Visitor, Operand_Sem_Ptr (T.Sem_Info));
      end if;

   end Qualified_Name_Action;

   procedure Assign_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Assign_Stmt.Tree) is
   begin
      --  Just pass the buck to Emit_Assignment
      Emit_Assignment (Visitor, T.Assign_Operator,
        LHS => Resolved_Tree (T.LHS), RHS => Resolved_Tree (T.RHS));
   end Assign_Stmt_Action;

   procedure Invocation_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Invocation.Tree) is
      use Invocation;
      use Interpreter;
   begin
      case T.Kind is
         when Operation_Call =>
            --  Emit code for a call
            declare
               Call_Sem : constant Call_Sem_Ptr := Call_Sem_Ptr (T.Sem_Info);
            begin
               if Call_Sem = null or else Call_Sem.Op_Sem = null then
                  return;  --  Nothing to do  --
               end if;

               if Call_Sem.Resolved_Type /= null
                 and then
                   (Static.Known_To_Be_Small (Call_Sem.Resolved_Type)
                      or else
                    Static.Unwrapped_Type (Call_Sem.Resolved_Type) =
                      Univ_String_Type
                      or else
                    Num_Operands (Tree_Ptr_Of (Call_Sem.Definition).all) = 1)
                 and then Static.Is_Compile_Time_Known (Call_Sem.Definition,
                            Disallow_Concurrent_Types => True)
               then
                  --  We have a compile-time-known call with small/string
                  --  result (TBD: Handle other types) or no parameters.
                  --  Add to list of compile-time-known constants to be
                  --  evaluated later.
                  declare
                     Anon_Const_Ref : constant Anon_Const_Tables.Element_Ref :=
                       Find_Element (Anon_Const_Table,
                                     Call_Sem.Definition);
                     use type Anon_Const_Tables.Element_Ref;
                     Const_Index : Obj_Sem_Info_Index := 0;
                     Existing_Elem : Anon_Const_Tables.Element_Ref;
                     Const_Obj_Location : Object_Locator;

                     Inside_Evaluate_Tree : constant Boolean :=
                        --  True if the operation being compiled
                        --  is an anonymous routine created by Evaluate_Tree.
                       Is_Inside_Parameterless_Computation (Visitor.Op_Sem);

                     use type Obj_Sem_Info_Index;
                     use type CTK_Info_Index;
                     Num_Computed_Consts : CTK_Info_Index :=
                       Num_Elements (Compile_Time_Known_Consts);
                  begin
                     if Anon_Const_Ref /= null then
                        --  Already in table
                        Const_Index := Anon_Const_Ref.all;
                     elsif Num_Computed_Consts = 0
                       and then not Inside_Evaluate_Tree
                     then
                        --  Add to list of compile-time-known constants,
                        --  unless we have already begun computing them.
                        Add_Element
                          (Compile_Time_Known_Const_Table,
                           Sem_Ptr (Call_Sem),
                           Const_Index);
                        Enter_Element (Anon_Const_Table, Call_Sem.Definition,
                          Const_Index, Existing_Elem);

                     end if;

                     if Const_Index > 0
                       and then
                         (Num_Computed_Consts = 0
                            or else
                          (Num_Computed_Consts >= CTK_Info_Index (Const_Index)
                             and then
                           Nth_Element (Interpreter.Compile_Time_Known_Consts,
                             CTK_Info_Index (Const_Index)).Info.Data.Addr /=
                                Null_Virtual_Address))
                     then
                        --  Not currently being computed.
                        if Visitor.Gen_Parallel_Invocations_Only then
                           --  Nothing to do
                           if Debug_Code_Gen then
                              Put_Line ("Skipping anon_const " &
                                Subtree_Image (Call_Sem.Definition));
                           end if;
                           return;
                        end if;

                        Const_Obj_Location :=
                          (Const_Area, Offset_Within_Area (Const_Index),
                           No_VM_Obj_Id);

                        --  Make sure that we actually compute the value.
                        Call_Sem.Needs_Anon_Const := True;

                        if Debug_Code_Gen then
                           Put_Line ("Emitting ref to anon_const " &
                             Subtree_Image (Call_Sem.Definition) & " at " &
                             Obj_Locator_Image (Const_Obj_Location));
                        end if;

                        --  Suppress any Create_Polymorphic_Obj op that
                        --  might follow; indicate to Emit_Copy_Obj_Or_Word
                        --  that type being copied might be polymorphic.
                        Visitor.Create_Polymorphic_Obj := False;

                        Emit_Copy_Obj_Or_Word
                          (Visitor,
                           Destination => (Local_Area,
                                           Visitor.Target_Local_Offset,
                                           Visitor.Target_VM_Info),
                           Dest_Name => Visitor.Dest_Name,
                           Source => Const_Obj_Location,
                           Target_Object => Visitor.Target_Object,
                           Opnd_Sem => Operand_Sem_Ptr (Call_Sem),
                           Source_Pos =>
                             Find_Source_Pos (Call_Sem.Definition));

                        --  Bump target offset to indicate object in use.
                        Visitor.Target_Local_Offset :=
                          Visitor.Target_Local_Offset + 1;

                        return;  --  All done  --
                     end if;

                     --  Currently being computed, or too late to add another
                     --  constant, so fall through to make the call.
                  end;
               end if;

               if Debug_Code_Gen then
                  Put_Line
                    ("Call on " &
                     Subtree_Image (T.Prefix) &
                     " with Slow_Calls of " &
                     Slow_Call_Enum'Image (Call_Sem.Slow_Calls));
               end if;
               if Debug_Substitution then
                  Put_Line ("Call_Sem.Op_Sem.Definition: " &
                    Subtree_Image (Call_Sem.Op_Sem.Definition));
               end if;
               Emit_Call (Visitor, Call_Sem, T.Operands);
            end;
         when Container_Indexing =>
            null;
         when Class_Aggregate =>
            declare
               Agg_Sem : constant Sem_Ptr := Sem_Ptr (T.Sem_Info);
            begin
               if Agg_Sem /= null
                 and then Agg_Sem.all in Class_Aggregate_Semantic_Info
               then
                  if Debug_Code_Gen then
                     Put_Line
                       ("Class agg " &
                        Subtree_Image (T) &
                        " with Slow_Calls of " &
                        Slow_Call_Enum'Image
                           (Class_Agg_Sem_Ptr (Agg_Sem).Slow_Calls));
                  end if;
                  if Class_Agg_Sem_Ptr (Agg_Sem).Resolved_Type /= null then
                     Emit_Class_Agg
                       (Visitor,
                        Class_Agg_Sem_Ptr (Agg_Sem),
                        T.Operands);
                  else
                     Sem_Error (T, "Aggregate not resolved");
                  end if;
               elsif Is_Parenthesized_Expression (T) then
                  --  Handle case of simple parentheses
                  Emit_Code_For_Resolved_Tree
                    (Lists.Nth_Element (T.Operands, 1),
                     Visitor);
               end if;
            end;
         when Container_Aggregate | Map_Set_Aggregate =>
            declare
               Agg_Sem : constant Sem_Ptr := Sem_Ptr (T.Sem_Info);
            begin
               if Agg_Sem /= null
                 and then Agg_Sem.all in Container_Aggregate_Semantic_Info
                 and then Container_Agg_Sem_Ptr (Agg_Sem).Resolved_Type /=
                          null
               then
                  if Debug_Code_Gen then
                     Put_Line
                       ("Container agg " &
                        Subtree_Image (T) &
                        " with Slow_Calls of " &
                        Slow_Call_Enum'Image
                           (Container_Agg_Sem_Ptr (Agg_Sem).Slow_Calls));
                  end if;
                  Emit_Container_Agg
                    (Visitor,
                     Container_Agg_Sem_Ptr (Agg_Sem),
                     T.Operands);
               else
                  Sem_Error (T, "Aggregate not resolved");
               end if;
            end;
            null;
         when Module_Instantiation =>
            null;
         when Is_Function_Of =>
            Sem_Error (T, "NYI: ""is func (...)"" specification");
         when Tuple_Type_Definition =>
            Sem_Error (T, "NYI: tuple type definition");
      end case;
   end Invocation_Action;

   procedure Block_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Block_Stmt.Tree) is
      use Interpreter;
      Block_Body_Region : constant Symbols.Region_Ptr :=
        Sem_Ptr (T.Sem_Info).Nested_Region;
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);
   begin
      Comp_Sem.Level := Visitor.Current_Level;

      if Comp_Sem.Num_Exits > 0 then
         --  Initialize the Exit_Locs array
         Comp_Sem.Exit_Locs := new Instr_Loc_Array (1 .. Comp_Sem.Num_Exits);
      end if;

      Visitor.Decl_Region := Block_Body_Region;
      Emit_Code_And_Finalize_Resolved_Tree (T.Block_Body, Visitor);
      Visitor.Decl_Region := Block_Body_Region.Enclosing_Region;

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of ""block""");
         end if;
         return;
      end if;

      --  TBD: Do something with End_With_Values
      if Not_Null (T.End_With_Values) then
         Sem_Error (T.End_With_Values, "NYI: end block with Name => Value");
      end if;

      --  Fix up any exit statements
      Fix_Up_Exits (Visitor, Comp_Sem, Visitor.Num_Instrs);

   end Block_Stmt_Action;

   procedure Case_Construct_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Case_Construct.Tree) is
      use Interpreter;
      Orig_Target_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;
      Case_Selector_Loc : constant Object_Locator :=
        (Local_Area, Orig_Target_Offset, Assign_VM_Obj_Id (Visitor));
      Case_Sem : constant Case_Construct_Sem_Ptr :=
        Case_Construct_Sem_Ptr (T.Sem_Info);
      Num_Alts : constant Natural := Lists.Length (T.Case_Alt_List);
      Skip_Instrs : array (1 .. Num_Alts - 1) of Code_Length_Type;
         --  Location of skip instructions at end of each case-alt
      All_Cases_Escape : Boolean := True;
         --  Indicates whether every case "arm" ends with a return/exit
      Case_Alt_Region : Symbols.Region_Ptr := Case_Sem.Nested_Region;
      Enclosing_Region : constant Symbols.Region_Ptr := Visitor.Decl_Region;
      Orig_Target_Object : constant Object_Locator := Visitor.Target_Object;
      Case_Target_VM_Info : VM_Obj_Id_Type := No_VM_Obj_Id;
      Orig_Target_VM_Info : constant VM_Obj_Id_Type := Visitor.Target_VM_Info;
      Default_Case_Found : Boolean := False;

   begin
      if Is_Null (T.Case_Selector)
        and then
         Case_Sem.Case_Selector_Type = null
      then
         Sem_Error (T, "Missing polymorphic exception type");
         return;
      end if;

      Case_Sem.Level := Visitor.Current_Level;

      if Case_Sem.Num_Exits > 0 then
         --  Initialize the Exit_Locs array
         Case_Sem.Exit_Locs := new Instr_Loc_Array (1 .. Case_Sem.Num_Exits);
      end if;

      --  Evaluate case selector
      --  NOTE: If Target_Object is set up, we want it to apply to the
      --       case-alt expressions, but *not* to case selector
      --       (but selector is likely "small" so usually irrelevant).
      Visitor.Target_Object := Null_Object_Locator;
      Visitor.Target_VM_Info := Case_Selector_Loc.VM_Obj_Id;
      if Not_Null (T.Case_Selector) then
         Emit_Code_For_Resolved_Tree (T.Case_Selector, Visitor);
      else
         --  Load parameter which contains current exception occurrence
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => T.Source_Pos,
             Destination => Case_Selector_Loc,
             Dest_Name => Strings.Index
                           (Strings.String_Lookup ("_cur_exception_")),
             Source => (Param_Area, 0, No_VM_Obj_Id),
             Might_Be_Null => False));
      end if;

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of ""case""");
         end if;
         return;
      end if;

      if T.Is_Case_Expr then
         --  We need a variable to hold the possible case results
         Case_Target_VM_Info :=
           Assign_VM_Obj_Id (Visitor, Needs_Var => True);

         declare
            Enc_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
         begin
            Emit
              (Visitor,
               (Declare_Obj_Op,
                Source_Pos => Case_Construct.Find_Source_Pos (T),
                Destination => (Local_Area, Orig_Target_Offset,
                                Case_Target_VM_Info),
                Dest_Name => Strings.Null_U_String_Index,
                Is_By_Ref => False,
                Is_Var => True,
                Declare_Type_Info => Run_Time_Type_Info
                                    (Case_Sem.Resolved_Type,
                                     Referring_Module => Enc_Module)));
         end;

      end if;

      --  Strategy:
      --  Use "compare" op on each element of X .. Y | Z "tree"
      --  and then emit appropriate sequence of "if" ops.
      --  E.g. if (case_selector >= X and then case_selector <= Y) or else
      --         case_selector == Z.
      --  TBD: Use case_op, which requires evaluating choices at compile-time
      --     and coming up with a min/max.

      for I in 1 .. Num_Alts loop
         declare
            Case_Alt : Reference.Tree renames
              Reference.Tree (Tree_Ptr_Of (Lists.Nth_Element
                (T.Case_Alt_List, I)).all);
            Case_Alt_Key_Container : Invocation.Tree
              renames Invocation.Tree (Tree_Ptr_Of (Case_Alt.Key).all);
            Case_Alt_Key : Optional_Tree :=
              Lists.Nth_Element (Case_Alt_Key_Container.Operands, 1);
            Start_Case_Alt : constant Code_Offset := Visitor.Num_Instrs;
            End_Case_Alt_Key : Code_Offset := 0;
            Actual_Target_Offset : Offset_Within_Area := 0;
            Is_Default_Case : Boolean := False;
         begin
            --  Generate code to compare case-selector against case-alt key.
            Visitor.Decl_Region := Case_Alt_Region;
            Check_And_Set_Local_Offset (Visitor, Orig_Target_Offset + 1);
            Case_Alt_Key_Code_Gen
              (Visitor,
               Case_Sem,
               Case_Selector_Loc,
               Case_Alt_Key,
               Is_Default_Case);

            if Is_Default_Case then
               --  Found a default case
               Default_Case_Found := True;
            end if;

            End_Case_Alt_Key := Visitor.Num_Instrs;

            --  Generate code for case alternative statements
            --  Case_Alt_Key_Code_Gen will have reset Target_Local_Offset
            --  to its original local offset if possible, so result in
            --  right place if this is a case expression.
            --  However, if we have a choice param and we needed to
            --  store a reference to it, then Target_Local_Offset will
            --  not have been reset, and we will need to move the result.
            Actual_Target_Offset := Visitor.Target_Local_Offset;
            if Actual_Target_Offset = Orig_Target_Offset + 1 then
               --  Nothing extra added to stack, cut it back even further
               Check_And_Set_Local_Offset (Visitor, Orig_Target_Offset);
               Actual_Target_Offset := Orig_Target_Offset;
            end if;

            Visitor.Target_Object := Orig_Target_Object;  -- Set up target obj
            Visitor.Target_VM_Info := Case_Target_VM_Info;
            Emit_Code_For_Resolved_Tree (Case_Alt.Referent, Visitor);
            Visitor.Target_Object := Null_Object_Locator;  -- Set back to null

            if not Visitor.Last_Instr_Escapes then
               --  At least one of the alternatives doesn't escape.
               All_Cases_Escape := False;
            end if;

            if T.Is_Case_Expr
              and then Actual_Target_Offset /= Orig_Target_Offset
            then
               --  We need to move the result
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (Case_Alt.Referent),
                   Destination => (Local_Area, Orig_Target_Offset,
                                   Case_Target_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => (Local_Area, Actual_Target_Offset,
                              Case_Target_VM_Info),
                   Might_Be_Null => True));
            end if;

            --  Throw away everything but case selector
            Check_And_Set_Local_Offset (Visitor, Orig_Target_Offset + 1);

            if I /= Num_Alts then
               if not Visitor.Last_Instr_Escapes then
                  --  Need to skip around later alternatives
                  Emit
                    (Visitor,
                     (Skip_Op,
                      Source_Pos => Case_Construct.Find_Source_Pos (T),
                      Skip_Count => 0));
                  Skip_Instrs (I) := Visitor.Num_Instrs;
               else
                  --  No skip needed
                  Skip_Instrs (I) := 0;
               end if;
            end if;

            --  Go through code making up comparison against selector
            --  and fixup all 0 Skip_Counts to go to Visitor.Num_Instrs+1
            Fixup_Case_Alt_Key_Code
              (Visitor,
               Start_Case_Alt,
               End_Case_Alt_Key);

            --  Move on to next region.
            Case_Alt_Region := Case_Alt_Region.Next_Sibling_Region;
         end;
      end loop;

      --  Fixup skip instructions at end of each case-alt
      for I in Skip_Instrs'Range loop
         if Skip_Instrs (I) /= 0 then
            Visitor.Current_Code.Instrs (Skip_Instrs (I)).Skip_Count :=
              Visitor.Num_Instrs - Skip_Instrs (I);
         end if;
      end loop;

      Visitor.Decl_Region := Enclosing_Region;

      --  TBD: Do something with End_With_Values
      if Not_Null (T.End_With_Values) then
         Sem_Error (T.End_With_Values, "NYI: end case with Name => Value");
      end if;

      if Case_Sem.Num_Exits_Emitted > 0 then
         --  Fix up any exit statements
         Fix_Up_Exits
           (Visitor,
            Composite_Stmt_Sem_Ptr (Case_Sem),
            Visitor.Num_Instrs);

         --  We have some exits that jump to this point,
         --  so this doesn't end with an escape.
         All_Cases_Escape := False;
      end if;

      --  A "case" construct end with exits/returns if last instruction
      --  is an escape, there is a default case, and all of the alternatives
      --  end with an escape.
      Visitor.Last_Instr_Escapes := Visitor.Last_Instr_Escapes
        and then All_Cases_Escape
        and then Default_Case_Found;

      if T.Is_Case_Expr then
         --  Copy case result into itself, but with new VM reg info
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Find_End_Source_Pos (Optional (T'Access)),
             Destination => (Local_Area, Orig_Target_Offset,
                             Orig_Target_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Source => (Local_Area, Orig_Target_Offset,
                        Case_Target_VM_Info),
             Might_Be_Null => True));
      end if;

   end Case_Construct_Action;

   procedure Control_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Control_Stmt.Tree) is
      use Control_Stmt;
      use Interpreter;
   begin
      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing to do
         if Debug_Code_Gen then
            Put_Line ("Skipping " & Subtree_Image (T));
         end if;
         return;
      end if;

      case T.Kind is
         when Null_Stmt =>
            --  No code to generate for a "null" statement
            null;
         when Return_Stmt =>
            if Static.Inside_Parallel_Construct (Visitor.Decl_Region) then
               --  See comment in Pre_CG pass about when to cancel parallel
               --  threads when in an "extended" return.  For now
               --  we will cancel before starting the extended return.
               --  We need to be sure that a nested "simple" return doesn't
               --  fail because we have already done the prepare-to-exit.
               Emit
                 (Visitor,
                  (Prepare_To_Exit_Parallel_Op,
                   Source_Pos => Control_Stmt.Find_Source_Pos (T),
                   Parallel_Master => Outermost_Master (Visitor)));
            end if;

            if Not_Null (T.Values) then
               --  Evaluate return value and assign to Param zero
               --  TBD: Handle multiple outputs
               declare
                  Enclosing_Operation : constant Operation_Sem_Ptr :=
                    Static.Find_Enclosing_Operation (Visitor.Decl_Region);
                  Outputs : Lists.List renames
                    Operation.Tree (Tree_Ptr_Of
                                       (Enclosing_Operation.Definition).all).
                                          Operation_Outputs;
                  First_Output : constant Optional_Tree :=
                    Lists.Nth_Element (Outputs, 1);
                  Output_Sem : constant Object_Sem_Ptr :=
                    Object_Sem_Ptr (Sem_Info (First_Output));
                  Output_Is_By_Ref : constant Boolean :=
                    Static.Sym_Is_By_Ref (Output_Sem.Associated_Symbol);
                  Return_Expr : Optional_Tree := T.Values;
                  Return_Stmts : Optional_Tree := Null_Optional_Tree;
               begin
                  if Static.Is_Extended_Return (T.Values) then
                     --  An "extended" return statement
                     --  Associate the declared object with the
                     --  Output parameter (parameter at offset 0).
                     declare
                        Return_Tree : Binary.Tree renames
                          Binary.Tree (Tree_Ptr_Of (T.Values).all);
                        Return_Sem : constant Object_Sem_Ptr :=
                          Object_Sem_Ptr (Sem_Info (Return_Tree.Left_Operand));
                     begin
                        if Return_Sem = null then
                           return;  --  Some prior error
                        end if;
                        --  Return obj is effectively a rename of output parm
                        Return_Sem.Info := Output_Sem.Info;
                        Return_Stmts := Return_Tree.Right_Operand;
                        Return_Expr := Obj_Decl.Tree (Tree_Ptr_Of
                          (Return_Tree.Left_Operand).all).Obj_Value;
                        --  TBF: If no initial value, should do default
                        --       initialization;
                        --       effectively assign from "(others => <>)"
                        --       since all composite objs are "not null"
                        --       in Ada.
                     end;
                  end if;

                  if Not_Null (Return_Expr) then
                     --  Return_Expr is to be assigned to result obj.
                     --  Allocate space for finalizable temps, if any.
                     Allocate_Finalizable_Temps
                       (Visitor, Num_Finalizable_Temps (Return_Expr));

                     declare
                        Result_Offset : constant Offset_Within_Area :=
                          Visitor.Target_Local_Offset;
                        Adjusted_Target : constant Object_Locator :=
                          Adjust_For_Level_And_Prefix
                             (Visitor.Current_Level,
                              (Param_Area, 0, No_VM_Obj_Id),
                              Output_Sem.Info.Obj_Level);
                     --  Locator for output of operation, handling case where
                     --  it requires an up-level reference.
                        Temp_VM_Info : constant VM_Obj_Id_Type :=
                          Assign_VM_Obj_Id (Visitor);
                     begin

                        --  if "ref" output then use lvalue context
                        --  and store addr
                        if Output_Is_By_Ref then
                           --  Make sure that value being returned is part of
                           --  one of the "ref" parameters.
                           Check_Is_Part_Of_Ref
                             (Return_Expr, Enclosing_Operation.Nested_Region);

                           --  A "ref" output is to be produced
                           Visitor.Is_Lvalue_Context := True;

                           --  Set target VM reg
                           Visitor.Target_VM_Info := Temp_VM_Info;

                           --  Evaluate address
                           Emit_Code_For_Resolved_Tree (Return_Expr, Visitor);

                           --  Store address into output location
                           Emit
                             (Visitor,
                              (Store_Address_Op,
                               Source_Pos => Control_Stmt.Find_Source_Pos (T),
                               Destination => Adjusted_Target,
                               Dest_Name => Strings.Index
                                 (Output_Sem.Associated_Symbol.Str),
                               Source => Visitor.Lvalue_Location,
                               Might_Be_Null =>
                                 Resolved_Type
                                   (Return_Expr).Value_Is_Optional));

                           Visitor.Is_Lvalue_Context := False;
                           Visitor.Lvalue_Location := Null_Object_Locator;
                        else
                           --  A "value" output is desired

                           if not Static.Known_To_Be_Small
                                    (Output_Sem.Resolved_Type)
                           then
                              --  Set up a target since it might be large
                              Visitor.Target_Object := Adjusted_Target;
                           end if;

                           --  Set target VM reg
                           Visitor.Target_VM_Info := Temp_VM_Info;

                           --  Evaluate content
                           Emit_Code_For_Resolved_Tree (Return_Expr, Visitor);

                           --  Assign to result operand
                           Emit
                             (Visitor,
                              (Copy_Word_Op,
                               Source_Pos => Control_Stmt.Find_Source_Pos (T),
                               Destination => Adjusted_Target,
                               Dest_Name => Strings.Index
                                 (Output_Sem.Associated_Symbol.Str),
                               Source => (Local_Area, Result_Offset,
                                          Temp_VM_Info),
                               Might_Be_Null => True));

                           --  Restore target to null
                           Visitor.Target_Object := Null_Object_Locator;
                        end if;

                        --  Set high-water and cut back target_local_offset
                        Check_And_Set_Local_Offset (Visitor, Result_Offset);

                        --  Do handled seq of statements, if any
                        if Not_Null (Return_Stmts) then
                           Visit_Resolved (Return_Stmts, Visitor);
                        end if;
                     end;
                  end if;
               end;
            end if;

            --  Return from operation
            if Checking_Postconditions then
               Check_Postconditions (Visitor,
                 Static.Find_Enclosing_Operation (Visitor.Decl_Region),
                 Source_Pos => Control_Stmt.Find_Source_Pos (T));
            end if;

            Emit
              (Visitor,
               (Return_Op, Source_Pos => Control_Stmt.Find_Source_Pos (T),
                         Postcond_Proved => False));

         when Continue_Stmt =>
            --  Generate code for continue statement
            declare
               Loop_To_Continue : constant Optional_Tree :=
                 Static.Find_Enclosing_Stmt
                    (Visitor.Decl_Region,
                     Control_Stmt.Loop_Stmt,
                     T.Id);
               Innermost_Loop   : constant Optional_Tree :=
                 Static.Find_Enclosing_Stmt
                    (Visitor.Decl_Region,
                     Control_Stmt.Loop_Stmt,
                     Id => Null_Optional_Tree);
            begin
               if Is_Null (Loop_To_Continue) then
                  Sem_Error (T, "Target of continue statement not found");
               else
                  declare
                     Loop_Tree : Trees.Tree'Class
                       renames Tree_Ptr_Of (Loop_To_Continue).all;
                     Continued_Stmt_Sem : constant Composite_Stmt_Sem_Ptr :=
                       Composite_Stmt_Sem_Ptr (Loop_Tree.Sem_Info);
                     Innermost_Loop_Sem : constant Composite_Stmt_Sem_Ptr :=
                       Composite_Stmt_Sem_Ptr (Sem_Info (Innermost_Loop));
                     Loop_Sem_To_Exit   : Composite_Stmt_Sem_Ptr :=
                       Continued_Stmt_Sem;  --  Which stmt we exit "to"
                     Is_Parallel_Continue : constant Boolean :=
                       Static.Inside_Parallel_Construct
                         (Visitor.Decl_Region, Up_To => Continued_Stmt_Sem);
                  begin
                     --  Bump count of continues
                     --  (used in Next_For_Loop_Iteration if T.Values non-null)
                     Continued_Stmt_Sem.Num_Continues_Emitted :=
                       Continued_Stmt_Sem.Num_Continues_Emitted + 1;

                     if Innermost_Loop_Sem /= Continued_Stmt_Sem
                       and then Is_Parallel_Continue
                       and then Innermost_Loop_Sem /= null
                     then
                        --  Also bump count for innermost loop
                        Innermost_Loop_Sem.Num_Continues_Emitted :=
                          Innermost_Loop_Sem.Num_Continues_Emitted + 1;

                        --  Add this skip/exit to the innermost loop stmt.
                        Loop_Sem_To_Exit := Innermost_Loop_Sem;
                     end if;

                     if Not_Null (T.Values) then
                        --  Supply next value for iterators
                        declare
                           pragma Assert
                             (Loop_Tree in For_Loop_Construct.Tree);
                           For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr
                              :=
                             For_Loop_Construct_Sem_Ptr (Continued_Stmt_Sem);
                        begin
                           --  Allocate finalizable temps, if any
                           Allocate_Finalizable_Temps (Visitor,
                             Num_Finalizable_Temps (T.Values));

                           Next_For_Loop_Iteration
                             (Loop_Visitor => Visitor,
                              For_Loop_Sem => For_Loop_Sem,
                              Continue_With_Values => T.Values);
                        end;
                     end if;

                     --  Go to end of appropriate loop
                     if Visitor.Current_Level /= Loop_Sem_To_Exit.Level then
                        --  Exit nested block and skip to end of loop body
                        --  and add to list of continues to be fixed up.
                        Emit
                          (Visitor,
                           (Exit_Op,
                            Level_Diff => Natural
                              (Visitor.Current_Level -
                                Loop_Sem_To_Exit.Level),
                            Skip_Count => 0,
                            Source_Pos =>
                              Control_Stmt.Find_Source_Pos (T)));
                     else
                        --  This is a "local" continue of the current loop.
                        --  Emit skip_op to end of loop body
                        --  and add to list of continues to be fixed up.
                        Emit
                          (Visitor,
                           (Skip_Op,
                            Source_Pos => Control_Stmt.Find_Source_Pos (T),
                            Skip_Count => 0));
                     end if;

                     --  Add skip/exit op to list of continues for later fixup
                     Loop_Sem_To_Exit.Continue_Locs
                       (Loop_Sem_To_Exit.Num_Continues_Emitted) :=
                          (Visitor.Current_Block, Visitor.Num_Instrs);

                  end;

               end if;

               --  Remember high-water
               Check_High_Water (Visitor);

            end;

         when Exit_Stmt =>
            declare
               Stmt_To_Exit : constant Optional_Tree :=
                 Static.Find_Enclosing_Stmt
                    (Visitor.Decl_Region,
                     T.Applies_To,
                     T.Id);
            begin
               if Is_Null (Stmt_To_Exit) then
                  Sem_Error (T, "Target of exit statement not found");
               else
                  declare
                     Exited_Stmt_Sem : constant Composite_Stmt_Sem_Ptr :=
                       Composite_Stmt_Sem_Ptr (Sem_Info (Stmt_To_Exit));
                  begin
                     pragma Assert (Exited_Stmt_Sem.Num_Exits > 0);
                     if Static.Inside_Parallel_Construct (Visitor.Decl_Region,
                       Up_Through => Exited_Stmt_Sem)
                     then
                        --  Prepare to exit outermost master within
                        --  exited statement
                        Emit (Visitor,
                          (Prepare_To_Exit_Parallel_Op,
                           Source_Pos => Control_Stmt.Find_Source_Pos (T),
                           Parallel_Master => Outermost_Master
                             (Visitor, Up_Through => Exited_Stmt_Sem)));
                     end if;

                     if Not_Null (T.Values) then
                        --  Emit the appropriate assignments
                        Emit_Value_Assignments (Visitor, T.Values);
                     end if;

                     if Visitor.Current_Level /= Exited_Stmt_Sem.Level then
                        --  Now emit exit op; skip count will be fixed up
                        --  later.
                        Emit (Visitor,
                          (Exit_Op,
                            Source_Pos => Control_Stmt.Find_Source_Pos (T),
                            Level_Diff => Natural
                              (Visitor.Current_Level -
                                 Exited_Stmt_Sem.Level),
                            Skip_Count => 0));
                     else
                        --  This is a "local" exit of composite statement.
                        --  Emit skip_op to go to end of statement,
                        --  and add to list of instructions to be fixed up.
                        Emit
                          (Visitor,
                           (Skip_Op,
                            Source_Pos => Control_Stmt.Find_Source_Pos (T),
                            Skip_Count => 0));

                     end if;

                     --  Add to list of exits to be fixed up.
                     Exited_Stmt_Sem.Num_Exits_Emitted :=
                       Exited_Stmt_Sem.Num_Exits_Emitted + 1;

                     Exited_Stmt_Sem.Exit_Locs
                       (Exited_Stmt_Sem.Num_Exits_Emitted) :=
                          (Visitor.Current_Block, Visitor.Num_Instrs);

                  end;
               end if;

               --  Remember high-water
               Check_High_Water (Visitor);
            end;
      end case;
   end Control_Stmt_Action;

   procedure Conditional_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Conditional.Tree) is
      use Interpreter;
      If_Instr : Code_Index;
      Skip_Instr : Code_Index := Code_Index'Last;
      Else_Instr : Code_Index;
      Then_Escapes : Boolean := False;
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);

      Then_Part_Region : constant Symbols.Region_Ptr :=
        Comp_Sem.Nested_Region; --  Region for the Then part

      Else_Part_Region : constant Symbols.Region_Ptr :=
        Then_Part_Region.Next_Sibling_Region; --  Region for the Else part

      Orig_Target_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;
      Orig_Target        : constant Object_Locator := Visitor.Target_Object;
      Orig_Dest_Name     : constant Strings.U_String_Index :=
                                       Visitor.Dest_Name;
      Orig_Targ_VM_Info  : constant VM_Obj_Id_Type := Visitor.Target_VM_Info;
      Cond_Offset        : Offset_Within_Area := Orig_Target_Offset;
      Cond_VM_Info       : constant VM_Obj_Id_Type :=
                                      Assign_VM_Obj_Id (Visitor);
      Cond_Targ_VM_Info  : VM_Obj_Id_Type := Orig_Targ_VM_Info;
      Cond_Needs_Temp    : Boolean := False;
   begin
      Comp_Sem.Level := Visitor.Current_Level;

      if T.Kind in Conditional.Cond_Expr_Parts then
         --  We have an "if" or "?:" expression
         if Orig_Targ_VM_Info.Kind /= Local_Kind
           or else not Orig_Targ_VM_Info.Is_Var
         then
            --  Need to create a temp
            Cond_Needs_Temp := True;
            Cond_Targ_VM_Info := Assign_VM_Obj_Id (Visitor, Needs_Var => True);
            declare
               Enc_Module : constant Module_Sem_Ptr :=
                 Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
            begin
               Emit
                 (Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Conditional.Find_Source_Pos (T),
                   Destination => (Local_Area, Orig_Target_Offset,
                                   Cond_Targ_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Is_By_Ref => False,
                   Is_Var => True,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (Comp_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));
            end;
         end if;
      end if;

      if Comp_Sem.Num_Exits > 0 then
         --  Initialize the Exit_Locs array
         Comp_Sem.Exit_Locs := new Instr_Loc_Array (1 .. Comp_Sem.Num_Exits);
      end if;

      --  NOTE: If Target_Object is set up, we want it to apply to both
      --       "then" and "else" expressions, but *not* to condition.
      Visitor.Target_Object := Null_Object_Locator;
      Visitor.Dest_Name := Strings.Null_U_String_Index;
      if not Target_Obj_Is_Null (Orig_Target) then
         --  We have a target, bump Cond_Offset just in case
         Cond_Offset := Orig_Target_Offset + 1;
         Visitor.Target_Local_Offset := Cond_Offset;
      end if;
      Visitor.Target_VM_Info := Cond_VM_Info;
      Emit_Code_For_Resolved_Tree (T.Cond, Visitor);  --  Evaluate condition

      --  Reestablish target for then/else part.
      Visitor.Target_Object := Orig_Target;
      Visitor.Dest_Name     := Orig_Dest_Name;

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of ""if""");
         end if;
         return;
      end if;

      --  Emit an "if" op
      Emit
        (Visitor,
         (If_Op,
          Source_Pos => Find_Source_Pos (T.Cond),
          If_Source => (Local_Area, Cond_Offset, Cond_VM_Info),
          If_Condition => Boolean_Is_True,  --  if True
          Skip_If_False => 0));  --  will be filled in below

      If_Instr := Visitor.Num_Instrs;

      --  Emit then part
      Visitor.Decl_Region := Then_Part_Region;

      --  Evaluate the then part into original target offset
      Check_And_Set_Local_Offset (Visitor, Orig_Target_Offset);

      Visitor.Target_VM_Info := Cond_Targ_VM_Info;
      Emit_Code_And_Finalize_Resolved_Tree (T.Then_Part, Visitor);

      --  Remember whether "then" part escapes
      Then_Escapes := Visitor.Last_Instr_Escapes;

      --  Presume else part does not escape
      Visitor.Last_Instr_Escapes := False;

      if Not_Null (T.Else_Part) then
         if not Then_Escapes then
            --  Skip over else part
            Emit
              (Visitor,
               (Skip_Op,
                Source_Pos => Find_Source_Pos (T.Else_Part),
                Skip_Count => 0));
            --  Count will be filled in below
            Skip_Instr := Visitor.Num_Instrs;
         end if;

         --  Remember where else part starts
         Else_Instr := Visitor.Num_Instrs;

         --  Emit else part
         Visitor.Decl_Region := Else_Part_Region;

         --  Evaluate the else part into same location as "then" part
         Check_And_Set_Local_Offset (Visitor, Orig_Target_Offset);
         Visitor.Target_Object := Orig_Target;
         Visitor.Target_VM_Info := Cond_Targ_VM_Info;
         Emit_Code_And_Finalize_Resolved_Tree (T.Else_Part, Visitor);

         if not Then_Escapes then
            --  Fix up skip count
            Visitor.Current_Code.Instrs (Skip_Instr).Skip_Count :=
              Visitor.Num_Instrs - Skip_Instr;
         end if;
      else
         --  No else part
         Else_Instr := Visitor.Num_Instrs;
      end if;

      if not Then_Escapes then
         --  "Then" didn't escape, so whole "if" doesn't escape
         Visitor.Last_Instr_Escapes := False;
      end if;

      --  Fix up "skip_if_false" amount.
      Visitor.Current_Code.Instrs (If_Instr).Skip_If_False :=
        Else_Instr - If_Instr;

      Visitor.Decl_Region := Then_Part_Region.Enclosing_Region;

      --  TBD: Handle "end if with Name => Value"
      if Not_Null (T.End_With_Values) then
         Sem_Error (T.End_With_Values, "NYI: end if with Name => Value");
      end if;

      --  Fix up any exits
      Fix_Up_Exits (Visitor, Comp_Sem, Visitor.Num_Instrs);

      if Cond_Needs_Temp then
         --  Copy from temp into original target
         --  (this is a no-op in the interpreter).
         Emit
           (Visitor,
            (Copy_Word_Op,
             Source_Pos => Conditional.Find_Source_Pos (T),
             Destination => (Local_Area, Orig_Target_Offset,
                             Orig_Targ_VM_Info),
             Dest_Name => Orig_Dest_Name,
             Source => (Local_Area, Orig_Target_Offset,
                        Cond_Targ_VM_Info),
             Might_Be_Null => True));  --  TBD
      end if;
   end Conditional_Action;

   procedure For_Loop_Construct_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out For_Loop_Construct.Tree) is
      use Interpreter;
      use For_Loop_Construct;
      Old_Enclosing_For_Loop : constant For_Loop_Construct_Sem_Ptr :=
        Visitor.Enclosing_For_Loop;
      Old_Target_Obj : constant Object_Locator := Visitor.Target_Object;
      Orig_Target_VM_Info : constant VM_Obj_Id_Type := Visitor.Target_VM_Info;
      Orig_Dest_Name : constant Strings.U_String_Index := Visitor.Dest_Name;
      For_Loop_Sem : constant For_Loop_Construct_Sem_Ptr :=
        For_Loop_Construct_Sem_Ptr (T.Sem_Info);
      Loop_Visitor : aliased Code_Gen_Visitor;
      Max_Length : constant Code_Length_Type := Max_Block_Length;
      Loop_Code : Code_Ptr;
      Loop_Body_Start : Code_Offset := 0;  --  Start of loop when not a blk
      Loop_Body_Region : constant Symbols.Region_Ptr :=
        For_Loop_Sem.Nested_Region;
      Loop_Param_Region : constant Symbols.Region_Ptr :=
        For_Loop_Sem.Loop_Param_Region;
      Num_Iterators : constant Natural := Lists.Length (T.Iterators);
      Loop_Param_Locators : Obj_Locator_Array (1 .. Num_Iterators);
      Loop_Param_VM_Infos : array (1 .. Num_Iterators) of VM_Obj_Id_Type;
      Loop_Result_Local_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;  --  Location of quantified-expr result
      Loop_Result_VM_Info : VM_Obj_Id_Type (Local_Kind);
         --  VM reg info for loop result

      Filter_Loc : Code_Offset := 0;  --  Location of filter "if_op"

      Num_Next_Values : Natural := 0;
      --  We compute this by walking the iterators
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      Num_Continues_With_Values : Natural := 0;
      Num_Extra_Invokers : Natural := 0;
   begin
      --  The loop body becomes a local callable operation.
      --  Each loop variable represents a parameter to the loop body.
      --  For a container element iterator, the "key" is the parameter,
      --  even if it is anonymous.  A ref to the container
      --  and a copy of its index set are referenced up level.
      --  Continue starts a parallel call for
      --  the next iteration and returns.
      --  A concurrent loop starts a parallel call for the
      --  next iteration before performing body.
      --  Master created for threads inside loop body.

      Visitor.Enclosing_For_Loop := For_Loop_Sem;

      --  This is the level where (copy of) any index set resides
      For_Loop_Sem.For_Loop_Level := Visitor.Current_Level;
      For_Loop_Sem.For_Loop_Master := Visitor.Local_Master;

      if For_Loop_Sem.Num_Exits > 0 then
         --  Initialize the Exit_Locs array
         For_Loop_Sem.Exit_Locs :=
           new Instr_Loc_Array (1 .. For_Loop_Sem.Num_Exits);
      end if;

      if For_Loop_Sem.Num_Continues > 0 then
         --  Initialize the Continue_Locs array
         For_Loop_Sem.Continue_Locs :=
           new Instr_Loc_Array (1 .. For_Loop_Sem.Num_Continues);

         if Static.Num_Initial_Value_Iterators (For_Loop_Sem) > 0 then
            --  Any "continue loop" statements must have specified
            --  values as well.
            Num_Continues_With_Values := For_Loop_Sem.Num_Continues;
         end if;
      end if;

      --  Walk the prologue declarations first, if any
      Code_Gen_List (Visitor, T.Prologue);

      case T.Kind is
         when For_Loop_Statement =>
            null;
         when Map_Reduce_Expr =>
            declare
               Num_Found : aliased Natural := 0;
               Initial_Val : constant Optional_Tree :=
                 Initial_Value_Operand
                   (T.Loop_Body, Num_Found'Access);
               pragma Assert (Not_Null (Initial_Val));
            begin
               if Num_Found > 1 then
                  Sem_Error (Initial_Val,
                    "Only one initial-value operand permitted");
               end if;

               --  Used by "<...>" inside loop body
               For_Loop_Sem.Loop_Result_Type := Resolved_Type (Initial_Val);

               --  Prepare to evaluate initial value
               --  (Visitor.Target_Object might be set)
               if Orig_Target_VM_Info.Is_Var then
                  --  Can use original target since it is a variable.
                  Loop_Result_VM_Info := Orig_Target_VM_Info;
               else
                  --  Need to allocate a new variable.
                  Loop_Result_VM_Info :=
                    Assign_VM_Obj_Id (Visitor, Needs_Var => True);
                  Emit
                    (Visitor,
                     (Declare_Obj_Op,
                      Source_Pos => Find_Source_Pos (For_Loop_Sem.Definition),
                      Destination => (Local_Area, Loop_Result_Local_Offset,
                                      Loop_Result_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Is_By_Ref => False,
                      Is_Var => True,
                      Declare_Type_Info => Run_Time_Type_Info
                                          (For_Loop_Sem.Loop_Result_Type,
                                           Referring_Module => Enc_Module)));
               end if;

               --  Save for reference from inside loop using "<...>"
               For_Loop_Sem.Loop_Result_Locator :=
                 (Local_Area, Loop_Result_Local_Offset, Loop_Result_VM_Info);

               --  Evaluate initial value (Visitor.Target_Object might be set)
               Visitor.Target_VM_Info := Loop_Result_VM_Info;
               Emit_Code_For_Resolved_Tree (Initial_Val, Visitor);
               --  Remove target object, if any
               Visitor.Target_Object := Null_Object_Locator;
               Visitor.Target_VM_Info := No_VM_Obj_Id;

               --  Protect result object
               Visitor.Target_Local_Offset := Loop_Result_Local_Offset + 1;

            end;
         when Univ_Quantified_Expr | Existential_Quantified_Expr =>
            --  Quantified expression; initialize result
            --  Store a #true if "for all" and a #false if "for some"

            --  But first remove target object, if any
            Visitor.Target_Object := Null_Object_Locator;

            if Orig_Target_VM_Info.Is_Var then
               --  Can use original target since it is a variable.
               Loop_Result_VM_Info := Orig_Target_VM_Info;
            else
               --  Need to allocate a new variable.
               Loop_Result_VM_Info :=
                 Assign_VM_Obj_Id (Visitor, Needs_Var => True);

               Emit
                 (Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Find_Source_Pos (For_Loop_Sem.Definition),
                   Destination => (Local_Area, Loop_Result_Local_Offset,
                                   Loop_Result_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Is_By_Ref => False,
                   Is_Var => True,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (For_Loop_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));
            end if;

            Emit
              (Visitor,
               (Store_Int_Lit_Op,
                Source_Pos => For_Loop_Construct.Find_Source_Pos (T),
                Destination => (Local_Area, Loop_Result_Local_Offset,
                                Loop_Result_VM_Info),
                Dest_Name => Strings.Null_U_String_Index,
                Int_Value => Boolean'Pos (T.Kind = Univ_Quantified_Expr)));
            Visitor.Target_Local_Offset := Loop_Result_Local_Offset + 1;
         when Container_Comprehension =>
            --  Initial setup done by container-aggregate code
            null;
      end case;

      --  Compute initial values for loop variables
      --  Skip around whole loop if one or more
      --  iteration sequences are empty
      Code_Gen_List (Visitor, T.Iterators);

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of for-loop");
         end if;
         return;
      end if;

      --  Get location of initial values; determine max Num_Next_Values
      for I in Loop_Param_Locators'Range loop
         Loop_Param_Locators (I) :=
           For_Loop_Sem.Iterator_Sems (I).Initial_Value_Location;

         Num_Next_Values :=
            Natural'Max
              (For_Loop_Sem.Iterator_Sems (I).Num_Next_Values,
               Num_Next_Values);
         --  TBD: Not sure what should be rules about Num_Next_Values.
         --      If > 1, should we require they all be the same?
         --      Or may only have exactly one iterator if multiple
         --      "next" values.
      end loop;

      if Num_Continues_With_Values > 0 then
         --  If we ever need to supply a next value, then when we reach
         --  the end of the loop body, we just quit.
         Num_Extra_Invokers := Num_Continues_With_Values;
      else
         --  If there are no "continue" statements that provide values,
         --  then the number of "extra" invokers are determine by the number
         --  of (parallel) "next" values provided for each iterator.
         Num_Extra_Invokers := Num_Next_Values;
      end if;

      if For_Loop_Sem.Uses_Parallel_Nested_Block then
         --  Generate parallel call using loop variables
         Loop_Code := new Code_Type (Max_Length);

         Emit_Nested_Block_Start
           (Visitor,
            Loop_Visitor,
            Computation_Sem_Ptr (For_Loop_Sem),
            For_Loop_Sem.Nested_Region,
            Loop_Code,
            Instr_Opcode => Start_Parallel_Op,
            Num_Outputs => 0,
            Block_Inputs => Loop_Param_Locators,
            Num_Extra_Invokers => Num_Extra_Invokers);
      else
         --  Allocate space for loop parameters in local area and
         --  initialize them.
         For_Loop_Sem.Loop_Params_Local_Offset := Visitor.Target_Local_Offset;

         for I in Loop_Param_Locators'Range loop
            --  Copy initial values into assigned slots
            declare
               Iter_Sem : constant Iterator_Sem_Ptr :=
                 For_Loop_Sem.Iterator_Sems (I);
               Initial_Val_Sem : constant Param_Sem_Ptr :=
                 Iterator_Loop_Param_Sem (Iter_Sem);
                  --  Sem_Ptr for item that is initialized
                  --  (either loop parameter or "key" parameter).
            begin
               Loop_Param_VM_Infos (I) :=
                 Assign_VM_Obj_Id (Visitor, Needs_Var => True);

               Emit
                 (Visitor,
                  (Declare_Obj_Op,
                   Source_Pos => Find_Source_Pos (Iter_Sem.Definition),
                   Destination => (Local_Area, Visitor.Target_Local_Offset,
                                   Loop_Param_VM_Infos (I)),
                   Dest_Name => Strings.Index
                     (Initial_Val_Sem.Associated_Symbol.Str),
                   Is_By_Ref => False,
                   Is_Var => True,
                   Declare_Type_Info => Run_Time_Type_Info
                                       (Initial_Val_Sem.Resolved_Type,
                                        Referring_Module => Enc_Module)));

               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Find_Source_Pos (Iter_Sem.Definition),
                   Destination => (Local_Area, Visitor.Target_Local_Offset,
                                   Loop_Param_VM_Infos (I)),
                   Dest_Name => Strings.Index
                     (Initial_Val_Sem.Associated_Symbol.Str),
                   Source => Loop_Param_Locators (I),
                   Might_Be_Null => True));  --  TBD

               --  Advance to next slot
               Visitor.Target_Local_Offset :=
                 Visitor.Target_Local_Offset + 1;
            end;
         end loop;

         --  Loop visitor is a copy of visitor
         Loop_Visitor := Visitor;

         --  Remember where loop body starts for loops not using nested blk.
         Loop_Body_Start := Loop_Visitor.Num_Instrs;

      end if;

      --  Set the composite-statement level used by "exit" and such
      For_Loop_Sem.Level := Loop_Visitor.Current_Level;
      --  NOTE: This level is that of the loop body block,
      --       since the way we currently implement "exit" is by
      --       jumping over the code which sets up the next iteration.

      --  Remember the block
      For_Loop_Sem.Loop_Body_Block := Loop_Visitor.Current_Block;

      Loop_Visitor.Enclosing_For_Loop := For_Loop_Sem;
      Loop_Visitor.Decl_Region := For_Loop_Sem.Nested_Region;

      --  If there are any container iterators, we need to
      --  create the "ref" to the container element.
      --  We also need to change the various values to be parameters
      --  rather than uplevels
      for I in For_Loop_Sem.Iterator_Sems'Range loop
         declare
            Iter_Sem : constant Iterator_Sem_Ptr :=
              For_Loop_Sem.Iterator_Sems (I);
            Param_Loc : constant Object_Locator :=
              (Param_Area,
               Offset_Within_Area (I - 1),
               No_VM_Obj_Id);
            Non_Param_Loc : constant Object_Locator :=
              (Local_Area,
               For_Loop_Sem.Loop_Params_Local_Offset +
                 Offset_Within_Area (I - 1),
               Loop_Param_VM_Infos (I));
         begin
            case Iter_Sem.Iterator_Kind is
               when Iterator.Value_Iterator | Iterator.Set_Iterator =>
                  --  Set up value location/level (it might be a parameter)
                  Iter_Sem.Info.Obj_Level := Loop_Visitor.Current_Level;
                  if For_Loop_Sem.Uses_Parallel_Nested_Block then
                     Iter_Sem.Info.Obj_Location := Param_Loc;
                     --  Declare as a parameter to the nested block
                     Emit
                       (Loop_Visitor,
                        (Declare_Obj_Op,
                         Source_Pos => Find_Source_Pos (Iter_Sem.Definition),
                         Destination => Iter_Sem.Info.Obj_Location,
                         Dest_Name => Strings.Index
                           (Iter_Sem.Associated_Symbol.Str),
                         Is_By_Ref => False,
                         Is_Var => False,
                         Declare_Type_Info =>
                           Run_Time_Type_Info
                             (Iter_Sem.Resolved_Type,
                              Referring_Module => Enc_Module)));
                  else
                     Iter_Sem.Info.Obj_Location := Non_Param_Loc;
                  end if;

               when Iterator.Container_Iterator =>
                  --  Call "indexing" operator given container (uplevel)
                  --  and key/index (parameter) to produce ref to element
                  declare
                     Indexing_Op : constant Operation_Sem_Ptr :=
                       Static.Find_Indexing_Op_For (Iter_Sem.Iteration_Type);
                     Container_Param : constant Optional_Tree :=
                          (Lists.Nth_Element
                             (Operation.Tree (Tree_Ptr_Of
                               (Indexing_Op.Definition).all).Operation_Inputs,
                              1));
                     Container_Param_Tree : Param_Decl.Tree renames
                        Param_Decl.Tree (Tree_Ptr_Of (Container_Param).all);

                     Adjusted_Container_Location : constant Object_Locator :=
                       Adjust_For_Level_With_Optional_Temp
                          (Loop_Visitor'Access,
                           Iter_Sem.Container_Location,
                           Visitor.Current_Level,
                           Src_Pos => Find_Source_Pos (Iter_Sem.Definition));

                     Indexing_Result_VM_Info : constant VM_Obj_Id_Type :=
                       Assign_VM_Obj_Id (Loop_Visitor,
                         Needs_Var => not Iter_Sem.Loop_Param_Is_By_Ref);

                     Indexing_Call_VM_Info : constant VM_Obj_Id_Type :=
                       Assign_VM_Obj_Id (Loop_Visitor,
                         Needs_Var => Indexing_Result_VM_Info.Is_Var,
                         Target_VM_Num => Indexing_Result_VM_Info.Num,
                         Num_Call_Params => 3);
                  begin

                     --  Set up key location/level (it is now a parameter)
                     Iter_Sem.Key_Sem.Info.Obj_Level :=
                       Loop_Visitor.Current_Level;
                     if For_Loop_Sem.Uses_Parallel_Nested_Block then
                        Iter_Sem.Key_Sem.Info.Obj_Location := Param_Loc;
                     else
                        Iter_Sem.Key_Sem.Info.Obj_Location := Non_Param_Loc;
                     end if;

                     --  Set up element location/level
                     Iter_Sem.Info.Obj_Level := Loop_Visitor.Current_Level;
                     Iter_Sem.Info.Obj_Location :=
                       (Local_Area,
                        Loop_Visitor.Target_Local_Offset,
                        Indexing_Result_VM_Info);
                     --  NOTE: This is where the address of element will be
                     --  stored if Loop_Param_Is_By_Ref

                     if not Iter_Sem.Loop_Param_Is_By_Ref then
                        --  May need to initialize the output if not passing
                        --  by reference.
                        Emit
                          (Loop_Visitor,
                           (Declare_Obj_Op,
                            Source_Pos =>
                              Find_Source_Pos (Iter_Sem.Definition),
                            Destination => Iter_Sem.Info.Obj_Location,
                            Dest_Name => Strings.Index
                              (Iter_Sem.Associated_Symbol.Str),
                            Is_By_Ref => Iter_Sem.Loop_Param_Is_By_Ref,
                            Is_Var => False,
                            Declare_Type_Info => Run_Time_Type_Info
                                                (Iter_Sem.Resolved_Type,
                                                 Referring_Module =>
                                                   Enc_Module)));

                        if not Static.Known_To_Be_Small
                          (Iter_Sem.Resolved_Type)
                        then
                           Emit
                             (Loop_Visitor,
                              (Store_Local_Null_Op,
                               Source_Pos =>
                                 Find_Source_Pos (Iter_Sem.Definition),
                               Destination =>
                                 (Iter_Sem.Info.Obj_Location.Base,
                                  Iter_Sem.Info.Obj_Location.Offset,
                                  Param_VM_Obj_Id (Indexing_Call_VM_Info,
                                    Param_Offset => 0)),
                               Dest_Name => Strings.Index
                                 (Iter_Sem.Associated_Symbol.Str),
                               Null_Type_Info =>
                                  Run_Time_Type_Info
                                    (Iter_Sem.Resolved_Type,
                                     Referring_Module => Enc_Module)));
                        end if;
                     end if;

                     Loop_Visitor.Target_Local_Offset :=
                       Loop_Visitor.Target_Local_Offset + 1;

                     --  Pass the container parameter
                     if Iter_Sem.Container_Is_By_Ref then
                        --  Normal case, where container is passed by ref.
                        Emit
                          (Loop_Visitor,
                           (Store_Address_Op,
                            Source_Pos =>
                              Find_Source_Pos (Iter_Sem.Definition),
                            Destination => (Local_Area,
                                            Loop_Visitor.Target_Local_Offset,
                                            Param_VM_Obj_Id
                                              (Indexing_Call_VM_Info,
                                               Param_Offset => 1)),
                            Dest_Name => Strings.Null_U_String_Index,
                            Source => Adjusted_Container_Location,
                            Might_Be_Null => True)); -- TBD
                     else
                        --  Special case, container is passed by copy.
                        --  Presumably "indexing" provides read-only result
                        --  or type is not assignable.
                        Emit
                          (Loop_Visitor,
                           (Copy_Word_Op,
                            Source_Pos =>
                              Find_Source_Pos (Iter_Sem.Definition),
                            Destination => (Local_Area,
                                            Loop_Visitor.Target_Local_Offset,
                                            Param_VM_Obj_Id
                                              (Indexing_Call_VM_Info,
                                               Param_Offset => 1)),
                            Dest_Name => Strings.Null_U_String_Index,
                            Source => Adjusted_Container_Location,
                            Might_Be_Null => True));  --  TBD
                     end if;

                     Loop_Visitor.Target_Local_Offset :=
                       Loop_Visitor.Target_Local_Offset + 1;

                     if For_Loop_Sem.Uses_Parallel_Nested_Block
                       and then Iter_Sem.Key_Sem.Associated_Symbol /= null
                     then
                        --  Declare the key
                        Emit
                          (Loop_Visitor,
                           (Declare_Obj_Op,
                            Source_Pos =>
                              Find_Source_Pos (Iter_Sem.Definition),
                            Destination => Iter_Sem.Key_Sem.Info.Obj_Location,
                            Dest_Name => Strings.Index
                              (Iter_Sem.Key_Sem.Associated_Symbol.Str),
                            Is_By_Ref => False,
                            Is_Var => False,
                            Declare_Type_Info =>
                              Run_Time_Type_Info
                                (Iter_Sem.Key_Sem.Resolved_Type,
                                  Referring_Module => Enc_Module)));
                     end if;

                     --  Pass the key by value
                     Emit
                       (Loop_Visitor,
                        (Copy_Word_Op,
                         Source_Pos => Find_Source_Pos (Iter_Sem.Definition),
                         Destination => (Local_Area,
                                         Loop_Visitor.Target_Local_Offset,
                                         Param_VM_Obj_Id
                                           (Indexing_Call_VM_Info,
                                            Param_Offset => 2)),
                         Dest_Name => Strings.Null_U_String_Index,
                         Source => Iter_Sem.Key_Sem.Info.Obj_Location,
                         Might_Be_Null =>
                           Iter_Sem.Key_Sem.Resolved_Type.Value_Is_Optional));

                     Loop_Visitor.Target_Local_Offset :=
                       Loop_Visitor.Target_Local_Offset + 1;

                     --  Now call the "indexing" op
                     Emit
                       (Loop_Visitor,
                        (Call_Op,
                         Source_Pos => Find_Source_Pos (Iter_Sem.Definition),
                         Call_Target =>
                            Routine_Locator
                              (Indexing_Op,
                               Iter_Sem.Iteration_Type),
                         Target_Index =>
                           Find_Operation_Routine_Index (Indexing_Op),
                         Params => (Iter_Sem.Info.Obj_Location.Base,
                                    Iter_Sem.Info.Obj_Location.Offset,
                                    Indexing_Call_VM_Info),
                         Locked_Param_Info => Locked_Param_Info_For_Container
                           (Container_Type => Iter_Sem.Iteration_Type,
                            Container_Param_Index => 2,
                            Container_Is_Var => Iter_Sem.Container_Is_By_Ref),
                         Static_Link => Run_Time_Type_Info
                                          (Iter_Sem.Iteration_Type,
                                           Referring_Module => Enc_Module),
                         Precond_Proved => False,
                         Output_Inited_Null =>  --  If large, return-by-value
                           not Iter_Sem.Loop_Param_Is_By_Ref
                             and then
                           not Static.Known_To_Be_Small
                             (Iter_Sem.Resolved_Type)));

                     --  Remember the high-water mark and restore local offset
                     Check_And_Set_Local_Offset
                       (Loop_Visitor,
                        Loop_Visitor.Target_Local_Offset - 2);

                  end;
            end case;
         end;
      end loop;  --  Creating refs to Container[Index] and changing to params

      if For_Loop_Sem.For_Loop_Direction = Interpreter.Concurrent_Dir then
         --  For a concurrent loop, generate next iterations before loop body
         pragma Assert (T.Kind = For_Loop_Statement);

         Next_For_Loop_Iteration
           (Loop_Visitor => Loop_Visitor,
            For_Loop_Sem => For_Loop_Sem,
            Num_Next_Values => Num_Next_Values);
      end if;

      if Not_Null (T.Filter) then
         --  We handle the filtering inside the loop
         --  body, by skipping over the loop body if it
         --  evaluates to false.  But we will still do the
         --  work to determine the next values for the loop variables.

         declare
            Filter_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Loop_Visitor);
         begin
            --  Generate the boolean representing the value of the filter
            Loop_Visitor.Target_VM_Info := Filter_VM_Info;
            Emit_Annotation_List (Loop_Visitor,
              Annotation.Tree (Tree_Ptr_Of (T.Filter).all));

            --  Perform the loop body only if true; skip if false
            Emit
              (Loop_Visitor,
               (If_Op,
                Source_Pos => Find_Source_Pos (T.Filter),
                If_Source => (Local_Area, Loop_Visitor.Target_Local_Offset - 1,
                              Filter_VM_Info),
                If_Condition => Boolean_Is_True,  --  if True
                Skip_If_False => 0));  --  will be filled in below

            --  Remember location of If_Op to be fixed up
            Filter_Loc := Loop_Visitor.Num_Instrs;

            --  Reset Target_Local_Offset
            Check_And_Set_Local_Offset
              (Loop_Visitor, Loop_Visitor.Target_Local_Offset - 1);
         end;
      end if;

      case T.Kind is
         when For_Loop_Statement =>
            --  Just emit the code for the body
            Emit_Code_And_Finalize_Resolved_Tree
              (T.Loop_Body, Loop_Visitor);

         when Map_Reduce_Expr =>
            --  Evaluate body, but set things up so Initial_Value_Op
            --  will fetch current result, and then store result of
            --  expression back into result.

            declare
               Loop_Result : constant Object_Locator :=
                  Adjust_For_Level_And_Prefix
                    (Loop_Visitor.Current_Level,
                     Obj_Location => For_Loop_Sem.Loop_Result_Locator,
                     Obj_Level => For_Loop_Sem.For_Loop_Level);
               Temp_VM_Info : constant VM_Obj_Id_Type :=
                 Assign_VM_Obj_Id (Loop_Visitor);
            begin
               --  Evaluate the body with appropriate target
               Loop_Visitor.Target_Object := Loop_Result;
               Loop_Visitor.Target_VM_Info := Temp_VM_Info;
               Emit_Code_And_Finalize_Resolved_Tree
                 (T.Loop_Body, Loop_Visitor);
               Loop_Visitor.Target_Object := Null_Object_Locator;

               --  Assign result of computation back into loop result
               --  (and free old value if large).
               Emit
                 (Loop_Visitor,
                  (Assign_Word_Op,
                   Source_Pos => Find_Source_Pos (T.Loop_Body),
                   Destination => Loop_Result,
                   Dest_Name => Strings.Null_U_String_Index,
                   Source =>
                     (Local_Area, Loop_Visitor.Target_Local_Offset - 1,
                      Temp_VM_Info),
                   Might_Be_Null => True,
                   Type_Info => Run_Time_Type_Info
                      (For_Loop_Sem.Loop_Result_Type,
                       Referring_Module => Enc_Module)));
            end;

         when Univ_Quantified_Expr | Existential_Quantified_Expr =>
            --  We have a quantified expression.
            --  Check whether value determines val of quantified expression,
            --  and if so, set value of overall quantified expression,
            --  and skip over computation of next value for loop variable.
            --  Emit: if <val_determined> then
            --            Result := <determined_val>;
            --            exit loop;
            --        end if;

            declare
               Temp_VM_Info : constant VM_Obj_Id_Type :=
                 Assign_VM_Obj_Id (Loop_Visitor);
            begin
               --  Evaluate the boolean expression
               Loop_Visitor.Target_VM_Info := Temp_VM_Info;
               Emit_Code_And_Finalize_Resolved_Tree
                 (T.Loop_Body, Loop_Visitor);

               --  Check whether it determines the value
               Emit
                 (Loop_Visitor,
                  (If_Op,
                   Source_Pos => Find_Source_Pos (T.Loop_Body),
                   If_Source =>
                     (Local_Area, Loop_Visitor.Target_Local_Offset - 1,
                      Temp_VM_Info),
                   If_Condition =>
                     If_Condition_Table (T.Kind = Existential_Quantified_Expr),
                   Skip_If_False => 2));  --  Skip store-int-lit and exit

               --  Store a #false if "for all" and #true if "for some"
               Emit
                 (Loop_Visitor,
                  (Store_Int_Lit_Op,
                   Source_Pos => For_Loop_Construct.Find_Source_Pos (T),
                   Destination =>
                     Adjust_For_Level_And_Prefix
                       (Loop_Visitor.Current_Level,
                        Obj_Location => (Local_Area, Loop_Result_Local_Offset,
                                         Loop_Result_VM_Info),
                        Obj_Level => For_Loop_Sem.For_Loop_Level),
                   Dest_Name => Strings.Null_U_String_Index,
                   Int_Value =>
                     Boolean'Pos (T.Kind = Existential_Quantified_Expr)));

               --  Exit the loop
               Emit
                 (Loop_Visitor,
                  (Skip_Op,
                   Source_Pos => Find_Source_Pos (T.Loop_Body),
                   Skip_Count => 0));

               --  Remember location of "exit"
               For_Loop_Sem.Num_Exits_Emitted := 1;
               For_Loop_Sem.Exit_Locs (1) :=
                 (Loop_Visitor.Current_Block, Loop_Visitor.Num_Instrs);
            end;

         when Container_Comprehension =>
            declare
               Body_Tree : Trees.Tree'Class renames
                 Tree_Ptr_Of (T.Loop_Body).all;
               Agg_Sem : constant Container_Agg_Sem_Ptr :=
                 For_Loop_Sem.Enclosing_Container_Agg_Sem;

               --  Only one iterator in container agg "loop"
               Iterator_Sem : constant Iterator_Sem_Ptr :=
                 For_Loop_Sem.Iterator_Sems (1);
            begin
               if Agg_Sem = null or else Iterator_Sem = null then
                  --  Some prior error
                  Sem_Error (T,
                    "Internal: Agg_Sem/Iterator_Sem not defined for " &
                      "container aggregate");
               elsif Body_Tree in Reference.Tree then
                  --  for I in S, Key => Value
                  Emit_Named_Element (Loop_Visitor, Agg_Sem,
                   Key => Reference.Tree (Body_Tree).Key,
                   Value => Reference.Tree (Body_Tree).Referent);
               elsif Agg_Sem.Index_Type /= null then
                  --  "for I in S => Val" or "for [Key=>Elem] of S => Val"
                  --  Use I, Key, or Elem as index.
                  --  If both Key and Elem match, use Key.
                  declare
                     Key_Matches_Exactly : constant Boolean :=
                       Iterator_Sem.Key_Sem /= null and then
                         Types_Match (Agg_Sem.Index_Type,
                           Iterator_Sem.Key_Sem.Resolved_Type);
                     Elem_Matches_Exactly : constant Boolean :=
                       Types_Match (Agg_Sem.Index_Type,
                         Iterator_Sem.Resolved_Type);
                     Key_Matches_By_Conversion : constant Boolean :=
                         Iterator_Sem.Key_Sem /= null
                       and then not Key_Matches_Exactly
                       and then not Elem_Matches_Exactly
                       and then Static.Implicitly_Converts
                         (From => Iterator_Sem.Key_Sem.Resolved_Type,
                          To => Agg_Sem.Index_Type);
                     Elem_Matches_By_Conversion : constant Boolean :=
                         not Key_Matches_Exactly
                       and then not Elem_Matches_Exactly
                       and then Static.Implicitly_Converts
                         (From => Iterator_Sem.Resolved_Type,
                          To => Agg_Sem.Index_Type);
                  begin
                     if Key_Matches_Exactly
                       or else Key_Matches_By_Conversion
                     then
                        --  Use key as index for new aggregate
                        if Elem_Matches_Exactly
                          or else
                            Elem_Matches_By_Conversion
                        then
                           --  TBD: Perhaps this should be ambiguous?
                           if Debug_Code_Gen then
                              Put_Line
                                (" Both key and element match "
                                 & "index type " &
                                 Type_Image (Agg_Sem.Index_Type) &
                                 "; using key");
                           end if;
                        end if;

                        Emit_Named_Element (Loop_Visitor, Agg_Sem,
                          Key => Iterator_Sem.Definition,
                          Key_Sem => Iterator_Sem.Key_Sem,
                          With_Key_Conversion => not Key_Matches_Exactly,
                          Value => T.Loop_Body);
                     elsif Elem_Matches_Exactly
                       or else Elem_Matches_By_Conversion
                     then
                        --  Use element as index for new aggregate
                        Emit_Named_Element (Loop_Visitor, Agg_Sem,
                          Key => Iterator_Sem.Definition,
                          Key_Sem => Param_Sem_Ptr (Iterator_Sem),
                          With_Key_Conversion => not Elem_Matches_Exactly,
                          Value => T.Loop_Body);
                     elsif Iterator_Sem.Key_Sem /= null then
                        Sem_Error (T, "Neither key nor element of iterator"
                          & " matches container index type " &
                          Type_Image (Agg_Sem.Index_Type));
                     else
                        Sem_Error (T, "Iterator parameter " &
                          Sym_Name (Iterator_Sem.Associated_Symbol) &
                          " does not match container index type " &
                          Type_Image (Agg_Sem.Index_Type));
                     end if;
                  end;
               else
                  --  for I in S => Value, use "<|=" operation
                  Emit_Positional_Element (Loop_Visitor, Agg_Sem,
                    Operand => T.Loop_Body);
               end if;
            end;

      end case;

      if Not_Null (T.Filter) then
         --  Fix up filter if-op to jump here if false
         Loop_Visitor.Current_Code.Instrs (Filter_Loc).Skip_If_False :=
           Loop_Visitor.Num_Instrs - Filter_Loc;
      end if;

      if not For_Loop_Sem.Uses_Parallel_Nested_Block
        and then Static.Num_Initial_Value_Iterators (For_Loop_Sem) > 0
      then
         --  If this is not using a nested block, and this loop
         --  requires an explicit "continue" statement to proceed,
         --  exit the loop at this point.
         Emit
           (Loop_Visitor,
            (Skip_Op,
             Source_Pos => Find_Source_Pos (For_Loop_Sem.Definition),
             Skip_Count => 0));

         --  Add to list of exits to be fixed up.
         For_Loop_Sem.Num_Exits_Emitted :=
           For_Loop_Sem.Num_Exits_Emitted + 1;

         For_Loop_Sem.Exit_Locs
           (For_Loop_Sem.Num_Exits_Emitted) :=
              (Loop_Visitor.Current_Block, Loop_Visitor.Num_Instrs);

      end if;

      --  Fix up "continue" statements to jump here.
      Fix_Up_Continues
        (Loop_Visitor,
         Composite_Stmt_Sem_Ptr (For_Loop_Sem),
         Loop_Visitor.Num_Instrs);

      if For_Loop_Sem.For_Loop_Direction /= Interpreter.Concurrent_Dir then
         --  For non-concurrent loops, generate next values after loop body
         Next_For_Loop_Iteration
           (Loop_Visitor => Loop_Visitor,
            For_Loop_Sem => For_Loop_Sem,
            Num_Next_Values => Num_Next_Values);

      end if;

      if not For_Loop_Sem.Uses_Parallel_Nested_Block then
         --  Skip back to start of loop
         Emit
           (Loop_Visitor,
            (Skip_Op,
             Source_Pos => For_Loop_Construct.Find_Source_Pos (T),
             Skip_Count => Loop_Body_Start - Loop_Visitor.Num_Instrs - 1));

      end if;

      --  Fix up "simple" exits to jump here,
      --  bypassing setting up next iteration
      --  TBD: Need to bypass the "end with" assignments
      Fix_Up_Exits
        (Loop_Visitor,
         Composite_Stmt_Sem_Ptr (For_Loop_Sem),
         Loop_Visitor.Num_Instrs);

      if For_Loop_Sem.Uses_Parallel_Nested_Block then
         --  Emit an "exit" and copy code for loop body into heap
         Emit_Nested_Block_Finish
           (Visitor,
            Loop_Visitor,
            Uses_Queuing => For_Loop_Sem.Uses_Queuing);

         Free_Code (Loop_Code);

         if Debug_Code_Gen then
            Put_Line
              (" Wait for master at " &
               Offset_Within_Area'Image (Visitor.Local_Master));
         end if;

         --  Wait for master of the loop
         Emit_Wait_For_Parallel (Visitor,
           Source_Pos => For_Loop_Construct.Find_Source_Pos (T));

         --  TBD: Need to copy-back from Loop_Visitor to Visitor?

      else
         --  Propagate back "up" certain flags
         Visitor.Num_Instrs := Loop_Visitor.Num_Instrs;
         Visitor.Last_Instr_Escapes := Loop_Visitor.Last_Instr_Escapes;
         Visitor.Master_Is_Started := Loop_Visitor.Master_Is_Started;
         Visitor.Master_Is_Complete := Loop_Visitor.Master_Is_Complete;
         Visitor.Start_Callee_Locals := Loop_Visitor.Start_Callee_Locals;
         Visitor.Finalizable_Temp_Offset :=
           Loop_Visitor.Finalizable_Temp_Offset;
      end if;

      --  Fix up skip counts in initial-value check for empty iterators.
      for Iter_Index in For_Loop_Sem.Iterator_Sems'Range loop
         declare
            Iter_Sem : constant Iterator_Sem_Ptr :=
              For_Loop_Sem.Iterator_Sems (Iter_Index);
         begin
            --  Fix up skip count for jump to after loop body.
            if Iter_Sem.Initial_Value_Test /= 0 then
               Visitor.Current_Code.Instrs (Iter_Sem.Initial_Value_Test).
                 Skip_If_False := Visitor.Num_Instrs -
                                  Iter_Sem.Initial_Value_Test;
            end if;
         end;
      end loop;

      --  TBD: Emit_Code_For_Resolved_Tree(T.End_With_Values, Visitor);
      if Not_Null (T.End_With_Values) then
         Sem_Error (T.End_With_Values, "NYI: end loop with Name => Value");
      end if;

      case T.Kind is
         when For_Loop_Statement =>
            --  Remember the high-water mark and restore local offset
            Check_And_Set_Local_Offset
              (Visitor, Loop_Result_Local_Offset);

         when Map_Reduce_Expr |
              Existential_Quantified_Expr | Univ_Quantified_Expr =>
            --  Move result to correct VM reg if necessary
            if Loop_Result_VM_Info /= Orig_Target_VM_Info then
               --  Move result to original VM target
               --  (this is a no-op in the interpreter)
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => For_Loop_Construct.Find_Source_Pos (T),
                   Destination =>
                      (Local_Area, Loop_Result_Local_Offset,
                       Orig_Target_VM_Info),
                   Dest_Name => Orig_Dest_Name,  --  Use original Dest_Name
                   Source =>
                      (Local_Area, Loop_Result_Local_Offset,
                       Loop_Result_VM_Info),
                   Might_Be_Null => (T.Kind = Map_Reduce_Expr)));
            end if;

            --  Remember the high-water mark and point after
            --  quant-expr/map-reduce result
            Check_And_Set_Local_Offset
              (Visitor, Loop_Result_Local_Offset + 1);

         when Container_Comprehension =>
            --  Remember the high-water mark and restore local offset
            Check_And_Set_Local_Offset
              (Visitor, Loop_Result_Local_Offset);

      end case;

      --  Restore enclosing-for-loop and target-object fields
      Visitor.Enclosing_For_Loop := Old_Enclosing_For_Loop;
      Visitor.Target_Object := Old_Target_Obj;
   end For_Loop_Construct_Action;

   procedure Iterator_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Iterator.Tree) is
      Iterator_Sem : constant Iterator_Sem_Ptr :=
        Iterator_Sem_Ptr (T.Sem_Info);

      Iterator_Sym : constant Symbols.Sym_Ptr :=
        Iterator_Sem.Associated_Symbol;
      Loop_Var_Is_By_Ref : constant Boolean :=
        Static.Sym_Is_By_Ref (Iterator_Sym);
      use Interpreter;
      use Iterator;
      Iterator_Obj_Type : constant Type_Sem_Ptr :=
        Iterator_Sem.Iteration_Type;
      Iterator_Element_Type : constant Type_Sem_Ptr :=
        Iterator_Sem.Resolved_Type;
      Iterator_Index_Type : Type_Sem_Ptr;
      --  Type of index in index_set; not used for Value_Iterator
      Enc_Module : constant Module_Sem_Ptr :=
        Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
      Param_Loc : Object_Locator := Null_Object_Locator;
      Loop_Param_Name : constant Strings.U_String_Index :=
                                      Strings.Index (Iterator_Sym.Str);
      Initial_Dest_Name : Strings.U_String_Index :=
        Strings.Null_U_String_Index;
   begin
      --  Evaluate set, container, or initial value for iteration.
      --  If a set or initial value, make sure it is copied.
      --  Skip around whole loop body if set or container is empty
      --  (i.e. Remove_* returns null).

      if T.Kind in Value_Iterator then
         --  Initial value for value iterator should use loop-param name.
         Initial_Dest_Name := Loop_Param_Name;
      end if;

      Iterator_Sem.Info.Obj_Level := Visitor.Current_Level;

      if T.Kind in Container_Iterator then
         --  We initialize this later
         Iterator_Sem.Info.Obj_Location := Null_Object_Locator;
      else
         --  We need a variable into which to evaluate the initial value,
         --  or a copy of the set.
         Iterator_Sem.Info.Obj_Location :=
           (Local_Area,
            Visitor.Target_Local_Offset,
            Assign_VM_Obj_Id (Visitor, Needs_Var => True));
         --  Location where value of Obj_Value expression is stored.
         --  If this is an initial/next/while-style iterator,
         --  this is the initial value for the loop varible, and it
         --  will be moved as necessary to become the Nth parameter
         --  to the loop body routine.
         --  NOTE: We need to use parameters for loop variables,
         --       since each iteration needs its own copy;
         --       it makes sense to use up-levels for container/index set.

         Emit
           (Visitor,
            (Declare_Obj_Op,
             Source_Pos => Iterator.Find_Source_Pos (T),
             Destination => Iterator_Sem.Info.Obj_Location,
             Dest_Name => Initial_Dest_Name,
             Is_By_Ref => False,
             Is_Var => True,
             Declare_Type_Info => Run_Time_Type_Info
                                    (Iterator_Obj_Type, --  TBD: might be a ref
                                     Referring_Module => Enc_Module)));
      end if;

      case T.Kind is
         when Set_Iterator | Value_Iterator =>
            if Loop_Var_Is_By_Ref then
               --  Loop variable is a reference, not a copy of initial value
               --  "for X => blah then ..."
               pragma Assert (T.Kind in Value_Iterator);
               Visitor.Is_Lvalue_Context := True;
               Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);
               Visitor.Is_Lvalue_Context := False;
               Emit
                 (Visitor,
                  (Store_Address_Op,
                   Source_Pos => Iterator.Find_Source_Pos (T),
                   Destination => Iterator_Sem.Info.Obj_Location,
                   Dest_Name => Initial_Dest_Name,
                   Source => Visitor.Lvalue_Location,
                   Might_Be_Null =>
                     Resolved_Type (T.Obj_Value).Value_Is_Optional));
            else
               --  We want a copy of the set or the initial value
               if not Static.Known_To_Be_Small (Iterator_Obj_Type) then
                  --  Initialize temp to null for local region
                  Emit
                    (Visitor,
                     (Store_Local_Null_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Destination => Iterator_Sem.Info.Obj_Location,
                      Dest_Name => Initial_Dest_Name,
                      Null_Type_Info => Run_Time_Type_Info
                                          (Iterator_Obj_Type,
                                           Referring_Module => Enc_Module)));
               end if;

               --  This being non-null means we want a copy
               Visitor.Target_Object := Iterator_Sem.Info.Obj_Location;
               Visitor.Target_VM_Info :=
                 Iterator_Sem.Info.Obj_Location.VM_Obj_Id;
                  --  TBD: Will it work if targ obj pt'ed to by targ VM reg?

               Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);
               Visitor.Target_Object := Null_Object_Locator;
               if Static.Type_Is_Concurrent (Iterator_Obj_Type) then
                  --  Create a lock if object is concurrent
                  Emit
                    (Visitor,
                     (Create_Lock_For_Obj_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Destination => Iterator_Sem.Info.Obj_Location,
                      Dest_Name => Initial_Dest_Name));
               end if;
            end if;

            --  Protect the result
            Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

            if T.Kind in Value_Iterator then
               Iterator_Sem.Initial_Value_Location :=
                 Iterator_Sem.Info.Obj_Location;
            else
               Iterator_Sem.Index_Set_Location :=
                 Iterator_Sem.Info.Obj_Location;
               --  Index and Element are one and the same for a Set_Iterator
               Iterator_Index_Type := Iterator_Element_Type;
            end if;

         when Container_Iterator =>
            --  Evaluate the container as an lvalue if is a variable;
            --  in any case, don't copy it;
            --  we will get our own copy of the "index_set" of the
            --  container and destructively iterate over that.

            if Static.Sem_Info_Is_For_Variable
              (Underlying_Sem_Info (Resolved_Tree (T.Obj_Value)))
            then
               --  Is a variable; just get its address
               Visitor.Is_Lvalue_Context := True;
               Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);
               Visitor.Is_Lvalue_Context := False;
               Iterator_Sem.Container_Location := Visitor.Lvalue_Location;
            else
               --  Is a constant; evaluate it normally and remember location
               if Iterator_Sem.Container_Is_By_Ref then
                  --  We need a var if is passed by ref
                  Visitor.Target_VM_Info := Assign_VM_Obj_Id (Visitor,
                    Needs_Var => True);

                  Emit
                    (Visitor,
                     (Declare_Obj_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Destination => (Local_Area, Visitor.Target_Local_Offset,
                                      Visitor.Target_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Is_By_Ref => False,
                      Is_Var => False,
                      Declare_Type_Info =>
                        Run_Time_Type_Info (Iterator_Obj_Type,
                                            Referring_Module => Enc_Module)));
               else
                  Visitor.Target_VM_Info := Assign_VM_Obj_Id (Visitor);
               end if;

               Iterator_Sem.Container_Location :=
                 (Local_Area,
                  Visitor.Target_Local_Offset,
                  Visitor.Target_VM_Info);
               Emit_Code_For_Resolved_Tree (T.Obj_Value, Visitor);
            end if;

            Iterator_Sem.Index_Set_Location :=
              (Local_Area,
               Visitor.Target_Local_Offset,
               Assign_VM_Obj_Id (Visitor, Needs_Var => True));

            --  Parameter info for call on "index set"
            Param_Loc :=
              (Local_Area,
               Visitor.Target_Local_Offset,
               Assign_VM_Obj_Id (Visitor, Needs_Var => True,
                 Target_VM_Num =>
                   Iterator_Sem.Index_Set_Location.VM_Obj_Id.Num,
                 Num_Call_Params => 2));

            Emit
              (Visitor,
               (Declare_Obj_Op,
                Source_Pos => Iterator.Find_Source_Pos (T),
                Destination =>
                  (Local_Area, Visitor.Target_Local_Offset,
                   Result_VM_Obj_Id (Param_Loc.VM_Obj_Id)),
                Dest_Name => Strings.Null_U_String_Index,
                Is_By_Ref => False,
                Is_Var => False,
                Declare_Type_Info => Run_Time_Type_Info
                                    (Iterator_Sem.Index_Set_Type,
                                     Referring_Module => Enc_Module)));

            Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

            if not Visitor.Gen_Parallel_Invocations_Only then
               --  Pass container to "index_set" function.
               Emit
                 (Visitor,
                  (Copy_Word_Op,
                   Source_Pos => Iterator.Find_Source_Pos (T),
                   Destination =>
                     (Local_Area, Visitor.Target_Local_Offset,
                      Param_VM_Obj_Id
                        (Param_Loc.VM_Obj_Id, Param_Offset => 1)),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => Iterator_Sem.Container_Location,
                   Might_Be_Null =>
                     Resolved_Type (T.Obj_Value).Value_Is_Optional));

               Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

               if not Static.Known_To_Be_Small
                        (Iterator_Sem.Index_Set_Type)
               then
                  --  Initialize index-set output to null for local region
                  Emit
                    (Visitor,
                     (Store_Local_Null_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Destination =>
                        (Param_Loc.Base,
                         Param_Loc.Offset,
                         Param_VM_Obj_Id
                           (Param_Loc.VM_Obj_Id, Param_Offset => 0)),
                      Dest_Name => Strings.Null_U_String_Index,
                      Null_Type_Info => Run_Time_Type_Info
                                          (Iterator_Sem.Index_Set_Type,
                                           Referring_Module => Enc_Module)));
               end if;

               declare
                  Index_Set_Op : constant Operation_Sem_Ptr :=
                    Static.Find_Index_Set_Op_For (Iterator_Obj_Type);
               begin
                  Emit
                    (Visitor,  --  call "index_set"
                     (Call_Op,
                      Source_Pos => Iterator.Find_Source_Pos (T),
                      Call_Target =>
                         Routine_Locator
                           (Index_Set_Op,
                            Iterator_Obj_Type),
                      Target_Index =>
                        Find_Operation_Routine_Index (Index_Set_Op),
                      Params => Param_Loc,
                      Locked_Param_Info => Locked_Param_Info_For_Container
                        (Container_Type => Iterator_Obj_Type,
                         Container_Param_Index => 2,
                         Container_Is_Var      => False),
                      Static_Link => Run_Time_Type_Info
                                       (Iterator_Obj_Type,
                                        Referring_Module => Enc_Module),
                      Precond_Proved => False,
                      Output_Inited_Null =>
                        not Static.Known_To_Be_Small
                              (Iterator_Sem.Index_Set_Type)));
               end;

               --  Remember the high-water mark and restore local offset
               Check_And_Set_Local_Offset
                 (Visitor,
                  Visitor.Target_Local_Offset - 1);

            end if;

            --  Type of "key" is the index type for index_set.
            Iterator_Index_Type := Iterator_Sem.Key_Sem.Resolved_Type;

      end case;

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of iterator");
         end if;
         return;
      end if;

      if Debug_Code_Gen then
         Put_Line
           ("Generate code for iterator " &
            Sym_Name (Iterator_Sym) &
            "; sym_index =" &
            Sym_Index'Image (Iterator_Sym.Index) &
            "; location = " &
            Obj_Locator_Image (Iterator_Sem.Info.Obj_Location) &
            ")");
      end if;

      case T.Kind is
         when Set_Iterator | Container_Iterator =>
            --  Now get the first index from the index set
            pragma Assert (Iterator_Index_Type /= null);

            --  Generate and test the first index
            Generate_Next_Value
              (Visitor,
               Iterator_Sem,
               Next_Value_Index => 0,
               Next_Value_Location => Iterator_Sem.Initial_Value_Location,
               Value_Test_Instruction => Iterator_Sem.Initial_Value_Test);

         when Value_Iterator =>

            if Not_Null (T.While_Cond) then
               --  Iteration is conditional.
               --  If condition not satisfied, then need to
               --  jump around rest of iterator initializations
               --  and actual parallel-call of loop-body routine and following
               --  parallel_wait call.
               declare
                  Cond_VM_Info : constant VM_Obj_Id_Type :=
                    Assign_VM_Obj_Id (Visitor);
               begin
                  Visitor.Target_VM_Info := Cond_VM_Info;
                  Emit_Code_For_Resolved_Tree (T.While_Cond, Visitor);

                  --  Emit an "if" op
                  Emit
                    (Visitor,
                     (If_Op,
                      Source_Pos => Find_Source_Pos (T.While_Cond),
                      If_Source => (Local_Area,
                                    Visitor.Target_Local_Offset - 1,
                                    Cond_VM_Info),
                      If_Condition => Boolean_Is_True,  --  if True
                      Skip_If_False => 0));  --  will be fixed up later
                  Iterator_Sem.Initial_Value_Test := Visitor.Num_Instrs;

                  --  Remember the high-water mark and restore local offset
                  Check_And_Set_Local_Offset
                    (Visitor,
                     Visitor.Target_Local_Offset - 1);
               end;
            end if;

            --  NOTE: We will generate code for computing T.Next_Values
            --       explicitly from inside the loop body

      end case;

   end Iterator_Action;

   procedure While_Stmt_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out While_Stmt.Tree) is
      use Interpreter;
      Comp_Sem : constant Composite_Stmt_Sem_Ptr :=
        Composite_Stmt_Sem_Ptr (T.Sem_Info);
      Test_Instr : Code_Offset := 0;
      Cond_Eval_Instr : constant Code_Index := Visitor.Num_Instrs + 1;
      Loop_Body_Region : constant Symbols.Region_Ptr :=
        Comp_Sem.Nested_Region;
      Orig_Local_Offset : constant Offset_Within_Area :=
        Visitor.Target_Local_Offset;
      Cond_VM_Info : constant VM_Obj_Id_Type := Assign_VM_Obj_Id (Visitor);
   begin
      Comp_Sem.Level := Visitor.Current_Level;

      if Comp_Sem.Num_Exits > 0 then
         --  Initialize the Exit_Locs array
         Comp_Sem.Exit_Locs := new Instr_Loc_Array (1 .. Comp_Sem.Num_Exits);
      end if;

      if Comp_Sem.Num_Continues > 0 then
         --  Initialize the Continue_Locs array
         Comp_Sem.Continue_Locs :=
           new Instr_Loc_Array (1 .. Comp_Sem.Num_Continues);
      end if;

      if Not_Null (T.While_Cond) then
         --  Evaluate condition
         Visitor.Target_VM_Info := Cond_VM_Info;
         Emit_Code_For_Resolved_Tree (T.While_Cond, Visitor);
      end if;

      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing more to do
         if Debug_Code_Gen then
            Put_Line ("Skipping rest of ""while""");
         end if;
         return;
      end if;

      if Not_Null (T.While_Cond) then
         --  Emit an "if" op
         Emit
           (Visitor,
            (If_Op,
             Source_Pos => Find_Source_Pos (T.While_Cond),
             If_Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                           Cond_VM_Info),
             If_Condition => Boolean_Is_True,  --  if True
             Skip_If_False => 0));  --  will be filled in below
         --  Remember the "test" of the condition
         Test_Instr := Visitor.Num_Instrs;
      elsif Comp_Sem.Num_Exits = 0 then
         --  We don't want a disconnected graph, so create the equivalent
         --  of "while True loop ..."
         Emit
           (Visitor,
            (Store_Int_Lit_Op,
             Source_Pos => While_Stmt.Find_Source_Pos (T),
             Destination => (Local_Area, Visitor.Target_Local_Offset,
                             Cond_VM_Info),
             Dest_Name => Strings.Null_U_String_Index,
             Int_Value => 1));  -- #true

         Visitor.Target_Local_Offset := Visitor.Target_Local_Offset + 1;

         Emit
           (Visitor,
            (If_Op,
             Source_Pos => While_Stmt.Find_Source_Pos (T),
             If_Source => (Local_Area, Visitor.Target_Local_Offset - 1,
                           Cond_VM_Info),
             If_Condition => Boolean_Is_True,  --  if True
             Skip_If_False => 0));  --  will be filled in below

         Test_Instr := Visitor.Num_Instrs;
      end if;

      --  Emit loop body
      Visitor.Decl_Region := Loop_Body_Region;
      Emit_Code_And_Finalize_Resolved_Tree (T.Loop_Body, Visitor);
      Visitor.Decl_Region := Loop_Body_Region.Enclosing_Region;

      --  Fix up any continues
      Fix_Up_Continues (Visitor, Comp_Sem, Visitor.Num_Instrs);

      --  Skip back to evaluate the condition
      Emit
        (Visitor,
         (Skip_Op,
          Source_Pos => While_Stmt.Find_Source_Pos (T),
          Skip_Count => Cond_Eval_Instr - Visitor.Num_Instrs - 2));

      if Not_Null (T.While_Cond) or else Comp_Sem.Num_Exits = 0 then
         --  Fix up "skip_if_false" amount in the test.
         Visitor.Current_Code.Instrs (Test_Instr).Skip_If_False :=
           Visitor.Num_Instrs - Test_Instr;
      end if;

      --  TBD: Handle "end loop with Name => Value"
      if Not_Null (T.End_With_Values) then
         Sem_Error (T.End_With_Values, "NYI: end loop with Name => Value");
      end if;

      --  Fix up any exits
      Fix_Up_Exits (Visitor, Comp_Sem, Visitor.Num_Instrs);

      --  Remember high-water mark and reset local offset
      Check_And_Set_Local_Offset (Visitor, Orig_Local_Offset);
   end While_Stmt_Action;

   procedure Selection_Action
     (Visitor : in out Code_Gen_Visitor;
      T : in out Selection.Tree) is
      Prefix_Type : constant Type_Sem_Ptr :=
        Operand_Sem_Ptr (Sem_Info (T.Prefix)).Resolved_Type;
      use Interpreter;
   begin
      if Visitor.Gen_Parallel_Invocations_Only then
         --  Nothing to do
         if Debug_Code_Gen then
            Put_Line ("Skipping selection '" & Subtree_Image (T) & "'");
         end if;
         return;
      end if;
      if Static.Type_Is_Wrapper (Prefix_Type) then
         --  This is a wrapper, just pass target and lvalue-context
         --  through to prefix.
         Emit_Code_For_Resolved_Tree (T.Prefix, Visitor);
      elsif T.Sem_Info.all in Selection_Semantic_Info
        and then Selection_Sem_Ptr (T.Sem_Info).Comp_Decl /= null
      then
         declare
            Sel_Sem : constant Selection_Sem_Ptr :=
              Selection_Sem_Ptr (T.Sem_Info);
            Target : constant Interpreter.Object_Locator :=
              Visitor.Target_Object;
            Comp_Module : constant Module_Sem_Ptr :=
              Static.Interface_Part (Sel_Sem.Comp_Decl.Originating_Module);
            Comp_Offset : constant Offset_Within_Area :=
              Component_Offset
                 (Prefix_Type,
                  Sel_Sem.Comp_Decl.Definition,
                  Usable_In_Aggregate => False);
            Comp_Is_By_Ref : constant Boolean :=
              Static.Sym_Is_By_Ref (Sel_Sem.Comp_Decl.Associated_Symbol);
            Is_Lvalue_Context : constant Boolean := Visitor.Is_Lvalue_Context;
            Result_Offset : constant Offset_Within_Area :=
              Visitor.Target_Local_Offset;
            Comp_Location : Object_Locator :=
              (Base_Register (Result_Offset), Comp_Offset, No_VM_Obj_Id);
            Referring_Module : constant Module_Sem_Ptr :=
              Static.Find_Enclosing_Module_Interface (Visitor.Decl_Region);
            Relevant_Ancestor_Type : Type_Sem_Ptr := Prefix_Type;
            Target_VM_Info : constant VM_Obj_Id_Type :=
              Visitor.Target_VM_Info;
            Prefix_VM_Info : VM_Obj_Id_Type := No_VM_Obj_Id;
            Ancestor_VM_Info : constant VM_Obj_Id_Type :=
              Assign_VM_Obj_Id (Visitor);
            Ancestor_Lvalue : Boolean := False;
         begin
            if Comp_Offset = Offset_Within_Area'Last then
               --  This is the indicator from Component_Offset that this
               --  is not a "selectable" component.
               Sem_Error
                 (T,
                  Subtree_Image (T.Selector) &
                  "is not a selectable component of " &
                  Type_Image (Prefix_Type));
            end if;

            --  We don't want to make a copy of the whole object;
            --  just the selection
            Visitor.Target_Object := Null_Object_Locator;

            --  We don't want ref to whole object, only possibly the component
            Visitor.Is_Lvalue_Context := False;
            Visitor.Target_VM_Info := No_VM_Obj_Id;

            if Prefix_Type.Is_Polymorphic then
               --  Selecting from a polymorphic object.
               --  Need to select correct ancestor for given component.

               if Debug_Code_Gen then
                  Put_Line
                    (" Selecting component of polymorphic obj " &
                     Subtree_Image (T));
                  if Static.Module_Is_Wrapper (Comp_Module) then
                     Put_Line ("  [Module of component is a wrapper]");
                  end if;
               end if;
               Relevant_Ancestor_Type := Static.Corresponding_Ancestor_Type
                 (Prefix_Type.Root_Type, Comp_Module);
               Ancestor_Lvalue := Is_Lvalue_Context
                 and then Static.Type_Is_Wrapper (Relevant_Ancestor_Type);

               Prefix_VM_Info := Assign_VM_Obj_Id (Visitor);

               Visitor.Target_VM_Info := Prefix_VM_Info;

               --  Evaluate Prefix
               Emit_Code_For_Resolved_Tree (T.Prefix, Visitor);

               Emit
                 (Visitor,
                  (Select_Polymorphic_Ancestor_Part_Op,
                   Source_Pos => Selection.Find_Source_Pos (T),
                   Destination =>
                     (Local_Area, Result_Offset, Ancestor_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => (Local_Area, Result_Offset, Prefix_VM_Info),
                   Might_Be_Null => True,  --  TBD
                   Type_Info =>
                      Run_Time_Type_Info
                        (Relevant_Ancestor_Type, Referring_Module),
                   Polymorphic_Ancestor_Lvalue => Ancestor_Lvalue));
            elsif Comp_Module /= Prefix_Type.Associated_Module
              and then Comp_Module.Component_Extension_Level /=
                       Prefix_Type.Associated_Module.Component_Extension_Level
            then
               --  Component-extension level mismatch.
               --  Need to select correct ancestor.

               if Debug_Code_Gen then
                  Put_Line
                    (" Component-extension level mismatch for " &
                     Subtree_Image (T));
                  if Static.Module_Is_Wrapper (Comp_Module) then
                     Put_Line ("  [Module of component is a wrapper]");
                  end if;
               end if;
               Relevant_Ancestor_Type := Static.Corresponding_Ancestor_Type
                 (Prefix_Type, Comp_Module);
               Ancestor_Lvalue := Is_Lvalue_Context
                 and then Static.Type_Is_Wrapper (Relevant_Ancestor_Type);

               Prefix_VM_Info :=
                 Assign_VM_Obj_Id (Visitor, Needs_Var => Ancestor_Lvalue);

               if Ancestor_Lvalue then
                  --  Declare Prefix variable
                  Emit
                    (Visitor,
                     (Declare_Obj_Op,
                      Source_Pos => Selection.Find_Source_Pos (T),
                      Destination =>
                        (Local_Area, Result_Offset, Prefix_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Is_By_Ref => False,
                      Is_Var => False,
                      Declare_Type_Info =>
                         Run_Time_Type_Info
                           (Prefix_Type, Referring_Module)));
               end if;

               Visitor.Target_VM_Info := Prefix_VM_Info;

               --  Evaluate Prefix
               Emit_Code_For_Resolved_Tree (T.Prefix, Visitor);

               Emit
                 (Visitor,
                  (Select_Ancestor_Part_Op,
                   Source_Pos => Selection.Find_Source_Pos (T),
                   Destination =>
                     (Local_Area, Result_Offset, Ancestor_VM_Info),
                   Dest_Name => Strings.Null_U_String_Index,
                   Source => (Local_Area, Result_Offset, Prefix_VM_Info),
                   Might_Be_Null => True,  --  TBD
                   Type_Info =>
                      Run_Time_Type_Info
                        (Relevant_Ancestor_Type, Referring_Module),
                   Source_Type_Info => Run_Time_Type_Info
                                         (Prefix_Type,
                                          Referring_Module),
                   Ancestor_Lvalue => Ancestor_Lvalue));

            else
               --  Simple case; just evaluate the prefix
               Prefix_VM_Info := Ancestor_VM_Info;

               Visitor.Target_VM_Info := Prefix_VM_Info;

               --  Evaluate Prefix
               Emit_Code_For_Resolved_Tree (T.Prefix, Visitor);

            end if;

            if Static.Type_Is_Wrapper (Relevant_Ancestor_Type) then
               --  Relevant ancestor was a wrapper, so component
               --  is at same location as ancestor part.
               if Is_Lvalue_Context then
                  Comp_Location := (Phys_Base_Register (Result_Offset), 0,
                                    Indir_VM_Obj_Id (Ancestor_VM_Info));
               else
                  Comp_Location :=
                    (Local_Area, Result_Offset, Ancestor_VM_Info);
               end if;
            else
               --  Fill in Component_Kind VM Info
               Comp_Location.VM_Obj_Id :=
                 Assign_VM_Obj_Id (Visitor,
                   Target_VM_Num => Ancestor_VM_Info.Num,
                   Offset => Comp_Offset);
            end if;

            if Comp_Is_By_Ref then
               declare
                  By_Ref_Comp_VM_Info : constant VM_Obj_Id_Type :=
                    Assign_VM_Obj_Id (Visitor);
               begin
                  Emit
                    (Visitor,
                     (Copy_Address_Op,
                      Source_Pos => Selection.Find_Source_Pos (T),
                      Destination => (Local_Area, Result_Offset,
                                      By_Ref_Comp_VM_Info),
                      Dest_Name => Strings.Null_U_String_Index,
                      Source => Comp_Location,
                      Might_Be_Null =>
                        Sel_Sem.Resolved_Type.Value_Is_Optional));

                  --  Indicate component is through a level of indirection
                  Comp_Location := (Phys_Base_Register (Result_Offset), 0,
                                    Indir_VM_Obj_Id (By_Ref_Comp_VM_Info));
               end;
            end if;

            if Is_Lvalue_Context then
               --  Form address of component

               Visitor.Lvalue_Location := Comp_Location;

            elsif Comp_Location.Base /= Local_Area
              or else Comp_Location.Offset /= Result_Offset
              or else not Target_Obj_Is_Null (Target)
              or else Comp_Location.VM_Obj_Id /=
                Target_VM_Info
            then
               --  Retrieve content of component

               Emit_Copy_Obj_Or_Word
                 (Visitor,
                  Destination => (Local_Area, Result_Offset,
                                  Target_VM_Info),
                  Source => Comp_Location,
                  Target_Object => Target,
                  Opnd_Sem => Operand_Sem_Ptr (Sel_Sem),
                  Source_Pos => Selection.Find_Source_Pos (T));

            end if;

            --  Restore target object/context
            Visitor.Target_Object := Target;
            Visitor.Is_Lvalue_Context := Is_Lvalue_Context;

            --  Protect result
            Check_And_Set_Local_Offset (Visitor, Result_Offset + 1);
         end;
      else
         Sem_Error (T, "Comp_Decl is null in Selection sem info");
      end if;

   end Selection_Action;

   ------- Visible Subprograms --------

   procedure Pre_Cg
     (Decl : Optional_Tree;
      Read_Write : in out Object_Access.Read_Write_Mapping;
      How_Combined : Object_Access.Read_Write_Combination_Enum :=
        Object_Access.Sequential;
      Mode : Object_Access.Access_Mode_Enum := Object_Access.Read_Access;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode) is
      --  Pre codegen pass for Decl
      use Ada.Text_IO;
      use PSC.Strings;
   begin
      if Is_Null (Decl) then
         --  Ignore empty Decl
         null;
      else
         declare
            Pre_Cg_Decl_Visitor : Pre_Cg_Visitor;
            use type PSC.Languages.Language_Enum;
         begin
            if Tree_Ptr_Of (Decl).Language /= PSC.Languages.Language then
               --  Switch languages
               Semantics.Set_Language (Tree_Ptr_Of (Decl).Language);
               if Debug_Pre_Cg then
                  Ada.Text_IO.Put_Line ("Language now " &
                    PSC.Languages.Language_Enum'Image
                      (PSC.Languages.Language));
               end if;
            end if;

            Pre_Cg_Decl_Visitor.Annotation_Mode := Annotation_Mode;
            Visit_Resolved (Decl, Pre_Cg_Decl_Visitor);

            --  Combine in the R/W mapping
            Object_Access.Combine
              (Read_Write,
               Addition => Pre_Cg_Decl_Visitor.Read_Write,
               How_Combined => How_Combined,
               Mode => Mode);
         end;
      end if;
   end Pre_Cg;

   procedure Pre_Cg_List
     (Decl_List : Lists.List;
      Read_Write : in out Object_Access.Read_Write_Mapping;
      How_Combined : Object_Access.Read_Write_Combination_Enum :=
        Object_Access.Sequential;
      Mode : Object_Access.Access_Mode_Enum := Object_Access.Read_Access;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode) is
   --  Apply Pre_CG to each element in Decl_List
   begin
      for I in 1 .. Lists.Length (Decl_List) loop
         declare
         begin
            Pre_Cg
              (Lists.Nth_Element (Decl_List, I),
               Read_Write,
               How_Combined,
               Mode,
               Annotation_Mode);
         exception
            when E : others =>
               Sem_Error
                 (Lists.Nth_Element (Decl_List, I),
                  "Internal: " &
                  Ada.Exceptions.Exception_Name (E) &
                  " raised in");
         end;
      end loop;
   end Pre_Cg_List;

   procedure Code_Gen (R : Symbols.Region_Ptr; Decl : Optional_Tree;
     Dest_Name : Strings.U_String_Index := Strings.Null_U_String_Index) is
      --  Generate PSVM code for Decl
      use Ada.Text_IO;
      use PSC.Strings;
   begin
      if Is_Null (Decl) then
         --  Ignore empty Decl
         null;
      else
         declare
            Cg_Visitor : Code_Gen_Visitor;
            use type PSC.Languages.Language_Enum;
         begin
            if Tree_Ptr_Of (Decl).Language /= PSC.Languages.Language then
               --  Switch languages
               Semantics.Set_Language (Tree_Ptr_Of (Decl).Language);
               if Debug_Code_Gen then
                  Ada.Text_IO.Put_Line ("Language now " &
                    PSC.Languages.Language_Enum'Image
                      (PSC.Languages.Language));
               end if;
            end if;

            Cg_Visitor.Decl_Region := R;
            Cg_Visitor.Dest_Name := Dest_Name;
            Emit_Code_For_Resolved_Tree (Decl, Cg_Visitor);
         end;
      end if;
   end Code_Gen;

   procedure Code_Gen
     (Cg_Visitor : in out Code_Gen_Visitor;
      Decl : Optional_Tree;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode) is
      --  Generate PSVM code for Decl given visitor
      use Ada.Text_IO;
      use PSC.Strings;
      Orig_Mode : constant Visitor_Annotation_Mode_Enum :=
        Cg_Visitor.Annotation_Mode;  --  Save original annotation mode
   begin
      if Is_Null (Decl) then
         --  Ignore empty Decl
         null;
      else
         if Annotation_Mode /= Normal_Mode then
            --  Set special annotation mode
            Cg_Visitor.Annotation_Mode := Annotation_Mode;
         end if;
         Emit_Code_For_Resolved_Tree (Decl, Cg_Visitor);
         --  Restore annotation mode
         Cg_Visitor.Annotation_Mode := Orig_Mode;
      end if;
   end Code_Gen;

   procedure Code_Gen_List (R : Symbols.Region_Ptr; Decl_List : Lists.List) is
   --  Apply Code_Gen to each element in Decl_List
   begin
      for I in 1 .. Lists.Length (Decl_List) loop
         begin
            Code_Gen (R, Lists.Nth_Element (Decl_List, I));
         exception
            when E : others =>
               Sem_Error
                 (Lists.Nth_Element (Decl_List, I),
                  "Internal: " &
                  Ada.Exceptions.Exception_Name (E) &
                  " raised in");
         end;
      end loop;
   end Code_Gen_List;

   procedure Code_Gen_List
     (Cg_Visitor : in out Code_Gen_Visitor;
      Decl_List : Lists.List;
      Annotation_Mode : Visitor_Annotation_Mode_Enum := Normal_Mode) is
   --  Apply Code_Gen to each element in Decl_List using given visitor
   begin
      for I in 1 .. Lists.Length (Decl_List) loop
         begin
            Code_Gen (Cg_Visitor, Lists.Nth_Element (Decl_List, I),
                      Annotation_Mode);
         exception
            when E : others =>
               Sem_Error
                 (Lists.Nth_Element (Decl_List, I),
                  "Internal: " &
                  Ada.Exceptions.Exception_Name (E) &
                  " raised in");
         end;
      end loop;
   end Code_Gen_List;

begin

   --  Fill in pointers back to Finish_Type_Descriptor and
   --  Name_For_Object_Locator

   Interpreter.Type_Descriptor_Ops.Finish_Type_Descriptor_Ptr :=
     Finish_Type_Descriptor'Access;

   Interpreter.Name_For_Object_Locator_Ptr := Name_For_Object_Locator'Access;

end PSC.Trees.Semantics.Dynamic;
