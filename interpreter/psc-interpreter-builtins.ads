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

--  Package providing support for builtin ParaSail operations

with PSC.Strings;
with PSC.Interpreter.Param_Signatures;
with PSC.Interpreter.Param_Sig;

package PSC.Interpreter.Builtins is

   use Interpreter.Param_Signatures;

   procedure Register_Builtin
     (Desig   : Strings.U_String;
      Builtin : Routine_Code_Address) renames Interpreter.Register_Builtin;
   --  Add Builtin into table of builtin operations under given designator.
   --  Raise Duplicate_Entry if Desig already in table.

   Duplicate_Entry : exception renames Interpreter.Duplicate_Entry;

   procedure Do_Nothing;
   --  Used as default for a formal subprogram
   pragma Inline (Do_Nothing);

   generic
      --  Generic package to introduce a unary builtin routine
      Name : String;  --  Name of operation
      with package Operand is new Param_Sig (<>);
      with package Result  is new Param_Sig (<>);
      with function Op
        (Input : Operand.Param_Type) return Result.Param_Type;
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Unary_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Unary_Builtin;

   generic
      --  Generic package to introduce a binary builtin routine
      Name : String;  --  Name of operation
      with package Left   is new Param_Sig (<>);
      with package Right  is new Param_Sig (<>);
      with package Result is new Param_Sig (<>);
      with function Op
        (Left_Input  : Left.Param_Type;
         Right_Input : Right.Param_Type) return Result.Param_Type;
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Binary_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Binary_Builtin;

   generic
      --  Generic package to introduce a builtin routine with three inputs
      --  and one output
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with package Result is new Param_Sig (<>);
      with function Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type) return Result.Param_Type;
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Ternary_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Ternary_Builtin;

   generic
      --  Generic package to introduce a builtin routine with four inputs
      --  and one output
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with package Fourth is new Param_Sig (<>);
      with package Result is new Param_Sig (<>);
      with function Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type;
         Fourth_Input : Fourth.Param_Type)
         return Result.Param_Type;
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Quarternary_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Quarternary_Builtin;

   generic
      --  Generic package to introduce a builtin routine with one input
      Name : String;  --  Name of operation
      with package Operand is new Param_Sig (<>);
      with procedure Op (Input : Operand.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package One_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end One_Input_Builtin;

   generic
      --  Generic package to introduce a builtin routine with two inputs
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with procedure Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Two_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Two_Input_Builtin;

   generic
      --  Generic package to introduce a builtin routine with three inputs
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with procedure Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Three_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Three_Input_Builtin;

   generic
      --  Generic package to introduce a builtin routine with four inputs
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with package Fourth is new Param_Sig (<>);
      with procedure Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type;
         Fourth_Input : Fourth.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Four_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Four_Input_Builtin;

   generic
      --  Generic package to introduce a builtin routine with five inputs
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with package Fourth is new Param_Sig (<>);
      with package Fifth  is new Param_Sig (<>);
      with procedure Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type;
         Fourth_Input : Fourth.Param_Type;
         Fifth_Input  : Fifth.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Five_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Five_Input_Builtin;

   generic
      --  Generic package to introduce a builtin routine with six inputs
      Name : String;  --  Name of operation
      with package First  is new Param_Sig (<>);
      with package Second is new Param_Sig (<>);
      with package Third  is new Param_Sig (<>);
      with package Fourth is new Param_Sig (<>);
      with package Fifth  is new Param_Sig (<>);
      with package Sixth  is new Param_Sig (<>);
      with procedure Op
        (First_Input  : First.Param_Type;
         Second_Input : Second.Param_Type;
         Third_Input  : Third.Param_Type;
         Fourth_Input : Fourth.Param_Type;
         Fifth_Input  : Fifth.Param_Type;
         Sixth_Input  : Sixth.Param_Type);
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Six_Input_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Six_Input_Builtin;

   generic
      --  Generic package to introduce a builtin "assign-op" routine
      --  which updates the Left operand with result of "Left op Right"
      Name : String;  --  Name of operation
      with package Left  is new Param_Sig (<>);
      with package Right is new Param_Sig (<>);
      with function Op
        (Left_Input  : Left.Param_Type;
         Right_Input : Right.Param_Type) return Left.Param_Type;
      with procedure Invoke_Before_Call is Do_Nothing;
      with procedure Invoke_After_Call is Do_Nothing;
   package Assign_Op_Builtin is
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
   private
      Addr_Of_Builtin : constant Routine_Code_Address :=
        Builtin_Routine'Access;
   end Assign_Op_Builtin;

end PSC.Interpreter.Builtins;
