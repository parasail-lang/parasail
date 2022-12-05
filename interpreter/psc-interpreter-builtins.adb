------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

with PSC.Command_Input;
with PSC.Interpreter.Param_Signatures;
with PSC.Link_Names;
with PSC.Messages;
with PSC.Source_Positions;
with PSC.Strings;
with PSC.Univ_Integers;
with PSC.Univ_Strings;
with PSC.Trees.Lists;
pragma Warnings (Off);
with Ada.Assertions;           --  Is an Ada 2005 unit
with Ada.Calendar.Arithmetic;  --  Is an Ada 2005 unit
pragma Warnings (On);
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Characters.Conversions;

pragma Elaborate (PSC.Interpreter);
pragma Elaborate (PSC.Messages);
pragma Elaborate (PSC.Source_Positions);
pragma Elaborate (PSC.Strings);
pragma Elaborate (Ada.Text_IO);

package body PSC.Interpreter.Builtins is

   use Interpreter.Param_Signatures;

   --------------------------- Local Subprograms  ----------------------------

   procedure Assert_True
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Assert(Boolean, Univ_String).
   --  If Boolean is #false then print string and raise exception.
   pragma Export (Ada, Assert_True, "_psc_assert_true");

   procedure Assign_Concat_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate univ-char onto "universal" string
   pragma Export (Ada, Assign_Concat_Char, "_psc_assign_concat_char");

   procedure Assign_Concat_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate one "universal" string on to end of another
   pragma Export (Ada, Assign_Concat_String, "_psc_assign_concat_string");

   procedure Basic_Array_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Create an array of given length and initial element value
   --    function Create
   --      (Length : Univ_Integer; Val : Element_Type) -> Basic_Array
   pragma Export (Ada, Basic_Array_Create, "_psc_basic_array_create");

   procedure Basic_Array_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Index into a basic array, return ref.
   --    operator "indexing"
   --      (V : ref Basic_Array; Index : Univ_Integer) -> ref Element_Type
   pragma Export (Ada, Basic_Array_Indexing, "_psc_basic_array_indexing");

   procedure Basic_Array_Length
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Return length of basic array
   --    function Length(V : Basic_Array) -> Univ_Integer
   pragma Export (Ada, Basic_Array_Length, "_psc_basic_array_length");

   procedure Bit_Not
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Bit-wise not

   function Unsigned_Compare_Op
     (Left, Right : Unsigned_Word_Type) return Word_Type;
   --  Unsigned-64 version of "=?"
   pragma Export (Ada, Unsigned_Compare_Op, "_psc_uns_compare_op");

   function Unsigned_Exponentiate_Op
     (Left : Unsigned_Word_Type; Right : Word_Type) return Unsigned_Word_Type;
   --  Use Word_Type as exponent for unsigned-word "**" operator
   pragma Export (Ada, Unsigned_Exponentiate_Op, "_psc_uns_exp_op");

   procedure To_String_Unsigned
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, To_String_Unsigned, "_psc_to_string_uns");

   procedure From_String_Unsigned
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, From_String_Unsigned, "_psc_from_string_uns");

   procedure Bool_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "from_univ"(Univ_Enumeration) -> Boolean
   pragma Export (Ada, Bool_From_Univ, "_psc_bool_from_univ");

   procedure Bool_Not
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Boolean not

   procedure Bool_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "to_univ"(Boolean) -> Univ_Enumeration
   pragma Export (Ada, Bool_To_Univ, "_psc_bool_to_univ");

   procedure Clock_Current_Time
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Current_Time(C : Clock) -> Time
   --  NOTE: Clock parameter is ignored; returns wall clock
   pragma Export (Ada, Clock_Current_Time, "_psc_clock_current_time");

   procedure Clock_Delay
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Wait_For_Delay(queued C : Clock; Until : Time)
   --  Wait until the wall clock reads >= Until
   --  NOTE: Clock parameter is ignored
   pragma Export (Ada, Clock_Delay, "_psc_clock_delay");

   procedure Concat_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate "universal" string with univ-char
   pragma Export (Ada, Concat_Char, "_psc_concat_char");

   procedure Concat_Char_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate univ-char with "universal" string
   pragma Export (Ada, Concat_Char_Str, "_psc_concat_char_str");

   procedure Concat_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate "universal" string with string image of int
   pragma Export (Ada, Concat_Int, "_psc_concat_int");

   procedure Concat_Int_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate string image of int with "universal" string
   pragma Export (Ada, Concat_Int_Str, "_psc_concat_int_str");

   procedure Concat_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate "universal" string with string image of real
   pragma Export (Ada, Concat_Real, "_psc_concat_real");

   procedure Concat_Real_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate string image of real with "universal" string
   pragma Export (Ada, Concat_Real_Str, "_psc_concat_real_str");

   procedure Concat_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Concatenate two "universal" strings
   pragma Export (Ada, Concat_String, "_psc_concat_string");

   procedure Direction_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "from_univ"(Univ_Enumeration) -> Direction
   pragma Export (Ada,  Direction_From_Univ, "_psc_direction_from_univ");

   procedure Direction_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "to_univ"(Direction) -> Univ_Enumeration
   pragma Export (Ada, Direction_To_Univ, "_psc_direction_to_univ");

   procedure Fixed_Real_Div
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  op "/"(Left : Fixed; Right : Univ_Real) -> Fixed
   pragma Export (Ada, Fixed_Real_Div, "_psc_fixed_real_div");

   procedure Fixed_Real_Mul
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  op "*"(Left : Fixed; Right : Univ_Real) -> Fixed
   pragma Export (Ada, Fixed_Real_Mul, "_psc_fixed_real_mul");

   procedure From_String_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ string to univ char
   pragma Export (Ada, From_String_Char, "_psc_from_string_char");

   procedure From_String_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ string to univ real
   pragma Export (Ada, From_String_Real, "_psc_from_string_real");

   procedure Identity
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Identity function

   procedure Int_To_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Int_To_Real(Int : Univ_Integer) -> Univ_Real

   procedure From_String_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ string to univ int
   pragma Export (Ada, From_String_Int, "_psc_from_string_int");

   procedure No_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Do nothing; this presumes any outputs are initialized to an appropriate
   --  sort of "null."

   procedure Ordering_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "from_univ"(Univ_Enumeration) -> Ordering
   pragma Export (Ada, Ordering_From_Univ, "_psc_ordering_from_univ");

   procedure Ordering_To_Bool
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "to_bool"(Ordering, Cond_Mask) -> Boolean
   pragma Export (Ada, Ordering_To_Bool, "_psc_ordering_to_bool");

   procedure Ordering_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  "to_univ"(Ordering) -> Univ_Enumeration
   pragma Export (Ada, Ordering_To_Univ, "_psc_ordering_to_univ");

   procedure Print_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print char value of Param 0 as a string
   pragma Export (Ada, Print_Char, "_psc_print_char");

   procedure Print_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print int value of Param 0 as a string
   pragma Export (Ada, Print_Int, "_psc_print_int");

   procedure Print_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print value of Param 0 presuming it is a Univ_String using large rep
   pragma Export (Ada, Print_String, "_psc_print_string");

   procedure Print_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print real value of Param 0 as a string
   pragma Export (Ada, Print_Real, "_psc_print_real");

   procedure Print_Univ_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print univ-enum value of Param 0 as a string
   pragma Export (Ada, Print_Univ_Enum, "_psc_print_univ_enum");

   procedure Println_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print value of Param 0 presuming it is a Univ_String, followed by a
   --  newline, presuming it is using a large rep.
   pragma Export (Ada, Println_String, "_psc_println_string");

   procedure Read_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Read string from standard input; return null at EOF; assume large rep.
   pragma Export (Ada, Read_String, "_psc_read_string");

   procedure Hash_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Hash_Enum, "_psc_hash_enum");

   procedure To_String_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, To_String_Enum, "_psc_to_string_enum");
   --  Convert Univ_Enumeration into Univ_String

   procedure From_String_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, From_String_Enum, "_psc_from_string_enum");
   --  Convert Univ_String into Univ_Enumeration

   procedure Hash_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Hash_String, "_psc_hash_string");

   function Real_Compare_Op
     (Left, Right : Univ_Real) return Word_Type;
   --  Univ-Real version of "=?"
   pragma Export (Ada, Real_Compare_Op, "_psc_real_compare_op");

   function Real_Exponentiate_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real;
   --  Use Word_Type as exponent for univ-real "**" operator

   procedure Real_Fixed_Mul
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  op "*"(Left : Univ_Real; Right : Fixed) -> Fixed
   pragma Export (Ada, Real_Fixed_Mul, "_psc_real_fixed_mul");

   function Real_Int_Divide_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real;
   --  Provide univ-real / int operation

   function Real_Int_Multiply_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real;
   --  Provide univ-real * int operation

   procedure Round_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Round_To_Int(Real : Univ_Real) -> Univ_Integer

   procedure Trunc_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Trunc_To_Int(Real : Univ_Real) -> Univ_Integer

   procedure Floor_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  func Floor_To_Int(Real : Univ_Real) -> Univ_Integer

   procedure String_Compare
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  String "=?" operator
   pragma Export (Ada, String_Compare, "_psc_string_compare");

   procedure String_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  String "indexing" operator (Univ_String, Univ_Integer) -> Univ_Char
   --  TBD: Handle escaped characters properly
   pragma Export (Ada, String_Indexing, "_psc_string_indexing");

   procedure String_Length
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  String Length func (Univ_String) -> Univ_Integer
   --  TBD: Handle escaped characters properly
   pragma Export (Ada, String_Length, "_psc_string_length");

   procedure String_Slicing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  String "slicing" operator (Univ_String, Univ_Integer) -> Univ_Char
   --  TBD: Handle escaped characters properly
   pragma Export (Ada, String_Slicing, "_psc_string_slicing");

   procedure Time_Int_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  op "from_univ"(Univ : Univ_Real) -> Time_Interval
   pragma Export (Ada, Time_Int_From_Univ, "_psc_time_int_from_univ");

   procedure Time_Int_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  op "to_univ"(Val : Time_Interval) -> Univ_Real
   pragma Export (Ada, Time_Int_To_Univ, "_psc_time_int_to_univ");

   function To_Calendar_Time
     (Nano_Count : Word_Type) return Ada.Calendar.Time;

   function To_Nanos (From : Ada.Calendar.Time) return Word_Type;
   --  Return a count of nanos from start of epoch

   procedure To_String_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ-char to univ-string
   pragma Export (Ada, To_String_Char, "_psc_to_string_char");

   procedure To_String_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ int to univ string
   pragma Export (Ada, To_String_Int, "_psc_to_string_int");

   procedure To_String_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Convert univ-real to univ-string
   pragma Export (Ada, To_String_Real, "_psc_to_string_real");

   function Univ_Real_Max
     (Left, Right : Univ_Real) return Univ_Real;
   --  Return Univ_Real'Max(Left, Right) unless Left or Right is null,
   --  in which case return the other.
   pragma Export (Ada, Univ_Real_Max, "_psc_univ_real_max_op");

   function Univ_Real_Min
     (Left, Right : Univ_Real) return Univ_Real;
   --  Return Univ_Real'Min(Left, Right) unless Left or Right is null,
   --  in which case return the other.
   pragma Export (Ada, Univ_Real_Min, "_psc_univ_real_min_op");

   procedure Unordered_Compare
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Unordered "=?" operator (returns #equal or #unordered)
   pragma Export (Ada, Unordered_Compare, "_psc_unordered_compare");

   function Word_Compare (Left, Right : Word_Type) return Word_Type;
   --  Int "=?" operator
   pragma Export (Ada, Word_Compare, "_psc_word_compare_op");

   function Word_Compare_Optional (Left, Right : Word_Type) return Word_Type;
   --  Int "=?" operator allowing nulls (#compare_optional)
   pragma Export (Ada, Word_Compare_Optional, "_psc_word_compare_optional");

   function Word_Image (Word : Word_Type) return String;
   --  Return Word as an image, removing leading blank from Ada 'image;
   --  recognize null value and return "null"

   function Word_Shift_Left (Left, Right : Word_Type) return Word_Type;
   --  Left shift equiv to mult. by power of 2

   function Word_Shift_Right (Left, Right : Word_Type) return Word_Type;
   --  Right shift equiv. to divide by power of 2, but with truncation toward
   --  negative infinity

   function Word_Type_Exponentiate (Left, Right : Word_Type) return Word_Type;
   --  Return Left ** Natural (Right).
   pragma Export (Ada, Word_Type_Exponentiate, "_psc_word_type_exp_op");

   function Word_Type_Max (Left, Right : Word_Type) return Word_Type;
   --  Return Word_Type'Max(Left, Right) unless Left or Right is null,
   --  in which case return the other.
   pragma Export (Ada, Word_Type_Max, "_psc_word_type_max_op");

   function Word_Type_Min (Left, Right : Word_Type) return Word_Type;
   --  Return Word_Type'Min(Left, Right) unless Left or Right is null,
   --  in which case return the other.
   pragma Export (Ada, Word_Type_Min, "_psc_word_type_min_op");

   -------------------------- Visible Subprograms  ---------------------------

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing is
   --  Used as default for a formal subprogram
   begin
      null;
   end Do_Nothing;

   ----------------- Generics for defining builtins -----------

   package body Unary_Builtin is
      --  Body of generic package to introduce a unary builtin routine
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Result.Store_Output
           (Context, Params, 0, Op (Operand.Fetch_Input (Params, 1)));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Unary_Builtin;

   package body Binary_Builtin is
      --  Body of generic package to introduce a binary builtin routine
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Result.Store_Output
           (Context, Params, 0, Op (Left.Fetch_Input (Params, 1),
                Right.Fetch_Input (Params, 2)));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Binary_Builtin;

   package body Ternary_Builtin is
      --  Body of generic package to introduce a
      --  three-input, one-output builtin routine
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Result.Store_Output
           (Context, Params, 0,
            Op (First.Fetch_Input (Params, 1),
                Second.Fetch_Input (Params, 2),
                Third.Fetch_Input (Params, 3)));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Ternary_Builtin;

   package body Quarternary_Builtin is
      --  Body of generic package to introduce a
      --  three-input, one-output builtin routine
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Result.Store_Output
           (Context, Params, 0,
            Op (First.Fetch_Input (Params, 1),
                Second.Fetch_Input (Params, 2),
                Third.Fetch_Input (Params, 3),
                Fourth.Fetch_Input (Params, 4)));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Quarternary_Builtin;

   package body One_Input_Builtin is
      --  Body of generic package to introduce a builtin routine with one input
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (Operand.Fetch_Input (Params, 0));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end One_Input_Builtin;

   package body Two_Input_Builtin is
      --  Body of generic package to define a builtin routine with two inputs
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (First.Fetch_Input (Params, 0),
             Second.Fetch_Input (Params, 1));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Two_Input_Builtin;

   package body Three_Input_Builtin is
      --  Body of generic package to define a builtin routine with three inputs
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (First.Fetch_Input (Params, 0),
             Second.Fetch_Input (Params, 1),
             Third.Fetch_Input (Params, 2));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Three_Input_Builtin;

   package body Four_Input_Builtin is
      --  Body of generic package to define a builtin routine with four inputs
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (First.Fetch_Input (Params, 0),
             Second.Fetch_Input (Params, 1),
             Third.Fetch_Input (Params, 2),
             Fourth.Fetch_Input (Params, 3));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Four_Input_Builtin;

   package body Five_Input_Builtin is
      --  Body of generic package to define a builtin routine with five inputs
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (First.Fetch_Input (Params, 0),
             Second.Fetch_Input (Params, 1),
             Third.Fetch_Input (Params, 2),
             Fourth.Fetch_Input (Params, 3),
             Fifth.Fetch_Input (Params, 4));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Five_Input_Builtin;

   package body Six_Input_Builtin is
      --  Body of generic package to define a builtin routine with six inputs
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Invoke_Before_Call;
         Op (First.Fetch_Input (Params, 0),
             Second.Fetch_Input (Params, 1),
             Third.Fetch_Input (Params, 2),
             Fourth.Fetch_Input (Params, 3),
             Fifth.Fetch_Input (Params, 4),
             Sixth.Fetch_Input (Params, 5));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Six_Input_Builtin;

   package body Assign_Op_Builtin is
      --  Body of generic package to define a builtin "assign-op" routine
      --  which updates the Left operand with result of "Left op Right"
      procedure Builtin_Routine
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
         --  Builtin "op=" operator
         LHS_Addr : constant Word_Ptr :=
           Fetch_Word_Ptr (Params, 0);
         RHS_Val : constant Right.Param_Type :=
           Right.Fetch_Input (Params, 1);
      begin
         Invoke_Before_Call;
         Left.Store_To_Phys_Addr
           (LHS_Addr, 0, Op (Left.Fetch_From_Phys_Addr (LHS_Addr), RHS_Val));
         Invoke_After_Call;
      end Builtin_Routine;
   begin
      --  Register the builtin
      Register_Builtin (Strings.String_Lookup (Name), Addr_Of_Builtin);
   end Assign_Op_Builtin;

   ----------------- Implementations of builtins -----------

   ----------------
   -- Word_Image --
   ----------------

   function Word_Image (Word : Word_Type) return String is
   begin
      if Word = Interpreter.Null_Value then
         return "null";
      else
         declare
            Image : String renames Word_Type'Image (Word);
         begin
            if Word >= 0 then
               --  Skip leading blank
               return Image (Image'First + 1 .. Image'Last);
            else
               return Image;
            end if;
         end;
      end if;
   end Word_Image;

   ---------------
   -- Print_Int --
   ---------------

   procedure Print_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Val : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Ada.Text_IO.Put (Word_Image (Val));
   end Print_Int;

   ----------------
   -- Print_Real --
   ----------------

   procedure Print_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Val : constant Univ_Real := Fetch_Real (Params, 0);
   begin
      Ada.Text_IO.Put (Real_Image (Val));
   end Print_Real;

   ------------------
   -- Print_String --
   ------------------

   procedure Print_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Val : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if Interpreter.Is_Large_Null (Val) then
         Ada.Text_IO.Put ("null");
      else
         --  Expand escaped characters
         declare
            WS : Wide_Wide_String renames Word_To_Wide_Wide_String (Val);
            use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
         begin
            Ada.Text_IO.Put (Encode (WS));
         end;
      end if;
   end Print_String;

   --------------------
   -- Println_String --
   --------------------

   procedure Println_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Print_String (Context, Params, Static_Link);
      Ada.Text_IO.New_Line;
   end Println_String;

   ----------------
   -- Print_Char --
   ----------------

   procedure Print_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Val : constant Word_Type := Fetch_Word (Params, 0);
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
   begin
      if Val = Interpreter.Null_Value then
         Ada.Text_IO.Put ("<null>");
      else
         if Val >= 2**7 then
            Ada.Text_IO.Put (Encode ((1 => Wide_Wide_Character'Val (Val))));
         else
            Ada.Text_IO.Put (Character'Val (Val));
         end if;
      end if;
   end Print_Char;

   ---------------------
   -- Print_Univ_Enum --
   ---------------------

   procedure Print_Univ_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Print univ-enum value of Param 0 as a string
      Val : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if Val = Interpreter.Null_Value then
         Ada.Text_IO.Put ("null");
      else
         --  Convert U_String_Index to String
         Ada.Text_IO.Put (Enum_Word_To_String (Val));
      end if;
   end Print_Univ_Enum;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if PSC.Command_Input.End_Of_File then
         --  Leave a null result
         null;
      else
         declare
            Line : constant String := PSC.Command_Input.Get_Line;
         begin
            Store_Word
              (Params, 0,
               Univ_Strings.To_Word_Type (Univ_Strings.From_String
                 (Line, Target, Context.Server_Index)));
         end;
      end if;
   end Read_String;

   -----------------
   --  Hash_Enum  --
   -----------------

   procedure Hash_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Index : constant Word_Type := Fetch_Word (Params, 1);
   begin
      Store_Word (Params, 0, Word_Type (Strings.Hash (To_U_String (Index))));
   end Hash_Enum;

   --------------------
   -- To_String_Enum --
   --------------------

   procedure To_String_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Convert Univ_Enumeration into Univ_String, convert null into "null"
      Target : constant Word_Type := Fetch_Word (Params, 0);
      Result : Strings.U_String :=
        Strings.To_U_String (Strings.U_String_Index (Fetch_Word (Params, 1)));
      use type Strings.U_String;
   begin
      if Result = Strings.Null_U_String then
         Result := Strings.String_Lookup ("null");
      end if;

      Store_Word (Params, 0, Univ_Strings.To_Word_Type
        (Univ_Strings.From_U_String (Result, Target)));
   end To_String_Enum;

   ----------------------
   -- From_String_Enum --
   ----------------------

   procedure From_String_Enum
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Convert Univ_String into Univ_Enumeration
      use Univ_Strings;
   begin
      Store_Word (Params, 0, Word_Type (Strings.Index
        (To_U_String (From_Word_Type (Fetch_Word (Params, 1))))));
   end From_String_Enum;

   -------------------
   --  Hash_String  --
   -------------------

   procedure Hash_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Str : constant Word_Type := Fetch_Word (Params, 1);
   begin
      Store_Word (Params, 0,
        Word_Type (Univ_Strings.Hash (Univ_Strings.From_Word_Type (Str))));
   end Hash_String;

   -----------------
   -- Assert_True --
   -----------------

   procedure Assert_True
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Bool : constant Boolean :=
               Boolean'Val (Fetch_Word (Params, 0));
      Str  : constant Word_Type := Fetch_Word (Params, 1);
   begin
      if not Bool then
         Messages.Put_Error
           ("Assertion failed: " & Word_To_String (Str),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (Bool);
      end if;
   end Assert_True;

   --  Integer unary "-"
   package Int_Negate is new Unary_Builtin ("#negate",
     Operand => Nonnull_Word_Param,
     Result => Word_Param,
     Op => "-");

   --  Integer unary "abs"
   package Int_Abs is new Unary_Builtin ("""abs""",
     Operand => Nonnull_Word_Param,
     Result => Word_Param,
     Op => "abs");

   generic
      Name : String;
      with function Binary_Op (Left, Right : Word_Type) return Word_Type;
   package Binary_Word_Op is
      package Define_Op is new Binary_Builtin (Name,
        Left   => Nonnull_Word_Param,
        Right  => Nonnull_Word_Param,
        Result => Nonnull_Word_Param,
        Op     => Binary_Op);
   end Binary_Word_Op;
   --  Generic to represent simple binary operation on (non-null) words

   generic
      Name : String;
      with function Binary_Op (Left, Right : Word_Type) return Word_Type;
   package Binary_Optional_Word_Op is
      package Define_Op is new Binary_Builtin (Name,
        Left   => Word_Param,
        Right  => Word_Param,
        Result => Word_Param,
        Op     => Binary_Op);
   end Binary_Optional_Word_Op;
   --  Generic to represent simple binary operation on possibly-null words

   function Saturating_Multiply (Left, Right : Word_Type) return Word_Type is
   --  Return Word_Type'First/Last rather than overflowing
   begin
      return Left * Right;
   exception
      when others =>
         Put_Line ("** Multiplication overflowed: " &
           Word_Type'Image (Left) & " * " & Word_Type'Image (Right));

         if (Left >= 0) = (Right >= 0) then
            return Word_Type'Last;
         else
            return Word_Type'Succ (Word_Type'First);
         end if;
   end Saturating_Multiply;

   package Int_Add      is new Binary_Word_Op ("""+""", "+");
   package Int_Subtract is new Binary_Word_Op ("""-""", "-");
   --  package Int_Multiply is new Binary_Word_Op ("""*""", "*");
   package Int_Multiply is new Binary_Word_Op ("""*""", Saturating_Multiply);
      --  Check for overflow
   package Int_Divide   is new Binary_Word_Op ("""/""", "/");
   package Int_Mod      is new Binary_Word_Op ("""mod""", "mod");
   package Int_Rem      is new Binary_Word_Op ("""rem""", "rem");

   -------------------
   -- Word_Type_Max --
   -------------------

   function Word_Type_Max (Left, Right : Word_Type) return Word_Type is
   begin
      if Left = Null_Value then
         return Right;
      elsif Right = Null_Value then
         return Left;
      else
         return Word_Type'Max (Left, Right);
      end if;
   end Word_Type_Max;

   -------------------
   -- Word_Type_Min --
   -------------------

   function Word_Type_Min (Left, Right : Word_Type) return Word_Type is
   begin
      if Left = Null_Value then
         return Right;
      elsif Right = Null_Value then
         return Left;
      else
         return Word_Type'Min (Left, Right);
      end if;
   end Word_Type_Min;

   package Int_Min is new Binary_Optional_Word_Op ("#min", Word_Type_Min);
   package Int_Max is new Binary_Optional_Word_Op ("#max", Word_Type_Max);

   ----------------------------
   -- Word_Type_Exponentiate --
   ----------------------------

   function Word_Type_Exponentiate (Left, Right : Word_Type) return Word_Type
   is
   begin
      return Left ** Natural (Right);
   end Word_Type_Exponentiate;

   package Int_Exponentiate is new Binary_Word_Op
     ("""**""", Word_Type_Exponentiate);

   procedure Int_Exponentiate_Exported
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Int_Exponentiate_Exported, "_psc_int_exponentiate");

   procedure Int_Exponentiate_Exported
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Int_Exponentiate.Define_Op.Builtin_Routine
        (Context, Params, Static_Link);
   end Int_Exponentiate_Exported;

   generic
      Name : String;
      with function Binary_Op (Left, Right : Word_Type) return Word_Type;
   package Binary_Assign_Op is
   --  Int "op=" operator
      package Define_Op is new Assign_Op_Builtin (Name,
        Left  => Nonnull_Word_Param,
        Right => Nonnull_Word_Param,
        Op    => Binary_Op);
   end Binary_Assign_Op;

   package Int_Assign_Add is new Binary_Assign_Op ("""+=""", "+");
   package Int_Assign_Subtract is new Binary_Assign_Op ("""-=""", "-");
   package Int_Assign_Multiply is new Binary_Assign_Op ("""*=""", "*");
   package Int_Assign_Divide is new Binary_Assign_Op ("""/=""", "/");

   package Int_Assign_Exponentiate is new Binary_Assign_Op
     ("""**=""", Word_Type_Exponentiate);

   procedure Int_Assign_Exponentiate_Exported
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   pragma Export (Ada, Int_Assign_Exponentiate_Exported,
                  "_psc_int_assign_exponentiate");

   procedure Int_Assign_Exponentiate_Exported
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Int_Assign_Exponentiate.Define_Op.Builtin_Routine
        (Context, Params, Static_Link);
   end Int_Assign_Exponentiate_Exported;

   ------------------
   -- Word_Compare --
   ------------------

   function Word_Compare (Left, Right : Word_Type) return Word_Type is
   --  Int "=?" operator
      Result : Ordering;
   begin
      if Left < Right then
         Result := Less;
      elsif Left > Right then
         Result := Greater;
      else
         Result := Equal;
      end if;
      return Ordering'Pos (Result);
   end Word_Compare;

   package Int_Compare is new Binary_Word_Op ("""=?""", Word_Compare);

   ---------------------------
   -- Word_Compare_Optional --
   ---------------------------

   function Word_Compare_Optional (Left, Right : Word_Type) return Word_Type is
   --  A "=?" operator that allows nulls
      Result : Ordering;
   begin
      if Left = Right then
         Result := Equal;
      elsif Left = Null_Value or else Right = Null_Value then
         Result := Unordered;
      elsif Left < Right then
         Result := Less;
      else
         Result := Greater;
      end if;
      return Ordering'Pos (Result);
   end Word_Compare_Optional;

   --  This allows "null" as an operand to "=?" to support Ada-ish languages
   package Int_Compare_Optional is
     new Binary_Optional_Word_Op ("#compare_optional", Word_Compare_Optional);

   ---------------------
   -- Word_Shift_Left --
   ---------------------

   function Word_Shift_Left (Left, Right : Word_Type) return Word_Type is
   --  Left shift equiv to mult. by power of 2
   begin
      return Left * 2 ** Natural (Right);
   end Word_Shift_Left;

   ----------------------
   -- Word_Shift_Right --
   ----------------------

   function Word_Shift_Right (Left, Right : Word_Type) return Word_Type is
      Divisor : constant Word_Type := 2 ** Natural (Right);
   begin
      if Left >= 0 then
         return Left / Divisor;
      else
         --  Truncate toward negative infinity
         return (Left - Divisor + Word_Type'(1)) / Divisor;
      end if;
   end Word_Shift_Right;

   package Int_Shift_Right is new Binary_Word_Op (""">>""", Word_Shift_Right);
   package Int_Shift_Left is new Binary_Word_Op ("""<<""", Word_Shift_Left);

   ------------ Univ_Integer operations ------------

   package Univ_Integer_Ops is
      use PSC.Univ_Integers;

      --  Param Signature for Univ_Integer
      package Univ_Integer_Param is new Param_Sig
        (Param_Type => Univ_Integer,
         Fetch_Input => Fetch_Univ_Integer,
         Store_Output => Store_Univ_Integer,
         Fetch_From_Phys_Addr => Fetch_Univ_Integer,
         Store_To_Phys_Addr => Store_Univ_Integer);

      --  Param Signature for non-null Univ_Integer
      package Nonnull_Univ_Integer_Param is new Param_Sig
        (Param_Type => Univ_Integer,
         Fetch_Input => Fetch_Nonnull_Univ_Integer,
         Store_Output => Store_Univ_Integer,
         Fetch_From_Phys_Addr => Fetch_Univ_Integer,
         Store_To_Phys_Addr => Store_Univ_Integer);

      generic
         Name : String;
         with function Binary_Op (Left, Right : Univ_Integer)
           return Univ_Integer;
      package Binary_Univ_Integer_Op is
         package Define_Op is new Binary_Builtin (Name,
           Left   => Nonnull_Univ_Integer_Param,
           Right  => Nonnull_Univ_Integer_Param,
           Result => Nonnull_Univ_Integer_Param,
           Op     => Binary_Op);
      end Binary_Univ_Integer_Op;
      --  Generic to represent simple binary operation on Univ_Integers

      generic
         Name : String;
         with function Binary_Op (Left, Right : Univ_Integer)
           return Univ_Integer;
      package Binary_Optional_Univ_Integer_Op is
         package Define_Op is new Binary_Builtin (Name,
           Left   => Univ_Integer_Param,
           Right  => Univ_Integer_Param,
           Result => Univ_Integer_Param,
           Op     => Binary_Op);
      end Binary_Optional_Univ_Integer_Op;
      --  Generic to represent binary operation on Univ_Integers allowing null

      package Univ_Add      is
        new Binary_Univ_Integer_Op ("#univ_add", "+");
      package Univ_Subtract is
        new Binary_Univ_Integer_Op ("#univ_subtract", "-");
      package Univ_Multiply is
        new Binary_Univ_Integer_Op ("#univ_multiply", "*");
      package Univ_Divide   is
        new Binary_Univ_Integer_Op ("#univ_divide", "/");
      package Univ_Mod      is
        new Binary_Univ_Integer_Op ("#univ_mod", "mod");
      package Univ_Rem      is
        new Binary_Univ_Integer_Op ("#univ_rem", "rem");

      function Min (Left, Right : Univ_Integer) return Univ_Integer;
      pragma Export (Ada, Min, "_psc_univ_min_op");
      function Max (Left, Right : Univ_Integer) return Univ_Integer;
      pragma Export (Ada, Max, "_psc_univ_max_op");

      package Univ_Min      is
        new Binary_Optional_Univ_Integer_Op ("#univ_min", Min);
      package Univ_Max      is
        new Binary_Optional_Univ_Integer_Op ("#univ_max", Max);

      generic
         Name : String;
         with function Binary_Op (Left, Right : Univ_Integer)
           return Univ_Integer;
      package Binary_Univ_Int_Assign_Op is
      --  Univ_Int word "op=" operator
         package Define_Op is new Assign_Op_Builtin (Name,
           Left  => Nonnull_Univ_Integer_Param,
           Right => Nonnull_Univ_Integer_Param,
           Op    => Binary_Op);
      end Binary_Univ_Int_Assign_Op;

      package Univ_Int_Assign_Add is
        new Binary_Univ_Int_Assign_Op ("#univ_assign_add", "+");
      package Univ_Int_Assign_Subtract is
        new Binary_Univ_Int_Assign_Op ("#univ_assign_subtract", "-");
      package Univ_Int_Assign_Multiply is
        new Binary_Univ_Int_Assign_Op ("#univ_assign_multiply", "*");
      package Univ_Int_Assign_Divide is
        new Binary_Univ_Int_Assign_Op ("#univ_assign_divide", "/");

      procedure Print_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      --  Print int value of Param 0 as a string
      pragma Export (Ada, Print_Univ_Int, "_psc_print_univ_int");

      procedure Univ_Integer_First
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      --  func First() -> Univ_Integer
      --  return Univ_Integer#First
      --  TBD: some day should be -infinity
      pragma Export (Ada, Univ_Integer_First, "_psc_univ_integer_first");

      procedure Univ_Integer_Last
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      --  func Last() -> Univ_Integer
      --  return Univ_Integer#Last
      --  TBD: some day should be +infinity
      pragma Export (Ada, Univ_Integer_Last, "_psc_univ_integer_last");

      procedure From_String_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      pragma Export (Ada, From_String_Univ_Int, "_psc_from_string_univ_int");

      procedure To_String_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      pragma Export (Ada, To_String_Univ_Int, "_psc_to_string_univ_int");

      function Add_Univ_Op
        (Left : Word_Type;
         Right : Univ_Integer)
        return Word_Type;
      pragma Export (Ada, Add_Univ_Op, "_psc_add_univ_op");

      function Univ_Int_Compare_Op
        (Left, Right : Univ_Integer) return Word_Type;
      --  Univ_Int word version of "=?"
      pragma Export (Ada, Univ_Int_Compare_Op, "_psc_univ_compare_op");

      function Univ_Int_Lshift_Op
        (Left : Univ_Integer; Right : Univ_Integer)
         return Univ_Integer;
      pragma Export (Ada, Univ_Int_Lshift_Op, "_psc_univ_lshift_op");

      function Univ_Int_Rshift_Op
        (Left : Univ_Integer; Right : Univ_Integer)
         return Univ_Integer;
      pragma Export (Ada, Univ_Int_Rshift_Op, "_psc_univ_rshift_op");

      function Subtract_Returns_Univ_Op
        (Left : Word_Type;
         Right : Word_Type)
        return Univ_Integer;
      pragma Export (Ada, Subtract_Returns_Univ_Op,
        "_psc_subtract_returns_univ_op");

      function Unsigned_Add_Univ_Op
        (Left : Unsigned_Word_Type;
         Right : Univ_Integer)
        return Unsigned_Word_Type;
      pragma Export (Ada, Unsigned_Add_Univ_Op, "_psc_uns_add_univ_op");

      function Unsigned_Subtract_Returns_Univ_Op
        (Left : Unsigned_Word_Type;
         Right : Unsigned_Word_Type)
        return Univ_Integer;
      pragma Export (Ada, Unsigned_Subtract_Returns_Univ_Op,
        "_psc_uns_subtract_returns_univ_op");

      procedure Unsigned_From_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      pragma Export (Ada, Unsigned_From_Univ, "_psc_uns_from_univ");

      procedure Unsigned_To_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr);
      pragma Export (Ada, Unsigned_To_Univ, "_psc_uns_to_univ");

   end Univ_Integer_Ops;

   package body Univ_Integer_Ops is

      function Min (Left, Right : Univ_Integer) return Univ_Integer is
      begin
         if Left = Null_Univ_Integer then
            return Right;
         elsif Right = Null_Univ_Integer then
            return Left;
         elsif Left < Right then
            return Left;
         else
            return Right;
         end if;
      end Min;

      function Max (Left, Right : Univ_Integer) return Univ_Integer is
      begin
         if Left = Null_Univ_Integer then
            return Right;
         elsif Right = Null_Univ_Integer then
            return Left;
         elsif Left < Right then
            return Right;
         else
            return Left;
         end if;
      end Max;

      function Univ_Int_Exponentiate_Op
        (Left : Univ_Integer; Right : Univ_Integer)
         return Univ_Integer is
      begin
         return Left ** Natural (From_Univ_Integer (Right));
      end Univ_Int_Exponentiate_Op;

      package Univ_Int_Exponentiate is new Binary_Builtin ("#univ_exp",
        Left   => Nonnull_Univ_Integer_Param,
        Right  => Nonnull_Univ_Integer_Param,
        Result => Univ_Integer_Param,
        Op     => Univ_Int_Exponentiate_Op);

      package Univ_Int_Assign_Exponentiate is new Assign_Op_Builtin
        (Name  => "#univ_assign_exp",
         Left  => Nonnull_Univ_Integer_Param,
         Right => Nonnull_Univ_Integer_Param,
         Op    => Univ_Int_Exponentiate_Op);

      function Univ_Int_Lshift_Op
        (Left : Univ_Integer; Right : Univ_Integer)
         return Univ_Integer is
      begin
         return Left * Two ** Natural (From_Univ_Integer (Right));
      end Univ_Int_Lshift_Op;

      package Univ_Int_Lshift_Builtin is new Binary_Builtin ("#univ_lshift",
        Left   => Nonnull_Univ_Integer_Param,
        Right  => Nonnull_Univ_Integer_Param,
        Result => Univ_Integer_Param,
        Op     => Univ_Int_Lshift_Op);

      function Univ_Int_Rshift_Op
        (Left : Univ_Integer; Right : Univ_Integer)
         return Univ_Integer is
      begin
         if Left < Zero then
            --  Want to sign extend on right shift,
            --  so we complement, shift right, and then complement again.
            return -Univ_Int_Rshift_Op (-Left - One, Right) - One;
         else
            return Left / Two ** Natural (From_Univ_Integer (Right));
         end if;
      end Univ_Int_Rshift_Op;

      package Univ_Int_Rshift_Builtin is new Binary_Builtin ("#univ_rshift",
        Left   => Nonnull_Univ_Integer_Param,
        Right  => Nonnull_Univ_Integer_Param,
        Result => Univ_Integer_Param,
        Op     => Univ_Int_Rshift_Op);

      function Univ_Int_Compare_Op
        (Left, Right : Univ_Integer) return Word_Type is
      --  Univ_Int word version of "=?"
         Result : Ordering;
      begin
         if Left = Right then
            Result := Equal;
         elsif Left < Right then
            Result := Less;
         else
            Result := Greater;
         end if;

         return Ordering'Pos (Result);
      end Univ_Int_Compare_Op;

      package Univ_Int_Compare is new Binary_Builtin ("#univ_compare",
        Left   => Nonnull_Univ_Integer_Param,
        Right  => Nonnull_Univ_Integer_Param,
        Result => Nonnull_Word_Param,
        Op     => Univ_Int_Compare_Op);

      ------------------------
      -- Univ_Integer_First --
      ------------------------

      procedure Univ_Integer_First
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      --  TBD: Actually could be even bigger
      begin
         Store_Univ_Integer (Params, 0, -(Two ** 255 - One));
      end Univ_Integer_First;

      -----------------------
      -- Univ_Integer_Last --
      -----------------------

      procedure Univ_Integer_Last
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      --  func Last() -> Univ_Integer
      --  return Univ_Integer#Last
      --  TBD: Actually could be even bigger
      begin
         Store_Univ_Integer (Params, 0, +(Two ** 255 - One));
      end Univ_Integer_Last;

      -----------------
      -- Add_Univ_Op --
      -----------------

      function Add_Univ_Op
        (Left : Word_Type;
         Right : Univ_Integer)
        return Word_Type is
      begin
         return Left + From_Univ_Integer (Right);
      end Add_Univ_Op;

      package Add_Univ is new Binary_Builtin ("#add_univ",
        Left => Word_Param,
        Right => Nonnull_Univ_Integer_Param,
        Result => Word_Param,
        Op => Add_Univ_Op);

      function Subtract_Returns_Univ_Op
        (Left : Word_Type;
         Right : Word_Type)
        return Univ_Integer is
      begin
         return From_Word (Left - Right);
      end Subtract_Returns_Univ_Op;

      package Subtract_Returns_Univ is
        new Binary_Builtin ("#subtract_returns_univ",
          Left => Word_Param,
          Right => Word_Param,
          Result => Univ_Integer_Param,
          Op => Subtract_Returns_Univ_Op);

      package Assign_Add_Univ is
        new Assign_Op_Builtin ("#assign_add_univ",
          Left  => Word_Param,
          Right => Nonnull_Univ_Integer_Param,
          Op    => Add_Univ_Op);

      function Subtract_Univ_Op
        (Left : Word_Type;
         Right : Univ_Integer)
        return Word_Type is
      begin
         return Left - From_Univ_Integer (Right);
      end Subtract_Univ_Op;

      package Assign_Subtract_Univ is
        new Assign_Op_Builtin ("#assign_subtract_univ",
          Left  => Word_Param,
          Right => Nonnull_Univ_Integer_Param,
          Op    => Subtract_Univ_Op);

      --------------------------
      -- Unsigned_Add_Univ_Op --
      --------------------------

      function Unsigned_Add_Univ_Op
        (Left : Unsigned_Word_Type;
         Right : Univ_Integer)
        return Unsigned_Word_Type is
      begin
         return Left + To_Unsigned_Word (Right);
      end Unsigned_Add_Univ_Op;

      package Unsigned_Add_Univ is new Binary_Builtin ("#uns_add_univ",
        Left => Unsigned_Word_Param,
        Right => Nonnull_Univ_Integer_Param,
        Result => Unsigned_Word_Param,
        Op => Unsigned_Add_Univ_Op);

      function Unsigned_Subtract_Returns_Univ_Op
        (Left : Unsigned_Word_Type;
         Right : Unsigned_Word_Type)
        return Univ_Integer is
      begin
         return From_Unsigned_Word (Left - Right);
      end Unsigned_Subtract_Returns_Univ_Op;

      package Unsigned_Subtract_Returns_Univ is
        new Binary_Builtin ("#uns_subtract_returns_univ",
          Left => Unsigned_Word_Param,
          Right => Unsigned_Word_Param,
          Result => Univ_Integer_Param,
          Op => Unsigned_Subtract_Returns_Univ_Op);

      package Unsigned_Assign_Add_Univ is
        new Assign_Op_Builtin ("#uns_assign_add_univ",
          Left  => Unsigned_Word_Param,
          Right => Nonnull_Univ_Integer_Param,
          Op    => Unsigned_Add_Univ_Op);

      function Unsigned_Subtract_Univ_Op
        (Left : Unsigned_Word_Type;
         Right : Univ_Integer)
        return Unsigned_Word_Type is
      begin
         return Left - To_Unsigned_Word (Right);
      end Unsigned_Subtract_Univ_Op;

      package Unsigned_Assign_Subtract_Univ is
        new Assign_Op_Builtin ("#uns_assign_subtract_univ",
          Left  => Unsigned_Word_Param,
          Right => Nonnull_Univ_Integer_Param,
          Op    => Unsigned_Subtract_Univ_Op);

      -----------------------
      -- Integer_From_Univ --
      -----------------------

      procedure Integer_From_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Store_Word
           (Params, 0,
            From_Univ_Integer (Fetch_Univ_Integer (Params, 1)));
      end Integer_From_Univ;

      ---------------------
      -- Integer_To_Univ --
      ---------------------

      procedure Integer_To_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
         use Univ_Strings;
      begin
         Store_Univ_Integer
           (Params, 0,
            From_Word (Fetch_Word (Params, 1)));
      end Integer_To_Univ;

      ------------------------
      -- Unsigned_From_Univ --
      ------------------------

      procedure Unsigned_From_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
      begin
         Store_Unsigned
           (Params, 0,
            To_Unsigned_Word (Fetch_Univ_Integer (Params, 1)));
      end Unsigned_From_Univ;

      ----------------------
      -- Unsigned_To_Univ --
      ----------------------

      procedure Unsigned_To_Univ
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
         use Univ_Strings;
      begin
         Store_Univ_Integer
           (Params, 0,
            From_Unsigned_Word (Fetch_Unsigned (Params, 1)));
      end Unsigned_To_Univ;

      ------------------------
      -- To_String_Univ_Int --
      ------------------------

      procedure To_String_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
         Target : constant Word_Type := Fetch_Word (Params, 0);
            --  Target gives target region for constructed Univ_String
      begin
         Store_Word
           (Params, 0,
            Univ_Strings.To_Word_Type
               (Univ_Strings.From_String
                   (Image (Fetch_Univ_Integer (Params, 1)),
                    Target,
                    Context.Server_Index)));
      end To_String_Univ_Int;

      --------------------------
      -- From_String_Univ_Int --
      --------------------------

      procedure From_String_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr) is
         use Univ_Strings;
      begin
         Store_Univ_Integer
           (Params, 0,
            Value (Word_To_String (Fetch_Word (Params, 1))));
      end From_String_Univ_Int;

      procedure Print_Univ_Int
        (Context : in out Exec_Context;
         Params : Word_Ptr;
         Static_Link : Non_Op_Map_Type_Ptr)
      is
         Val : constant Univ_Integer := Fetch_Univ_Integer (Params, 0);
      begin
         Ada.Text_IO.Put (Image (Val));
      end Print_Univ_Int;

      use PSC.Strings;
   begin
      Register_Builtin
        (String_Lookup ("#to_string_univ_int"), To_String_Univ_Int'Access);
      Register_Builtin
        (String_Lookup ("#from_string_univ_int"), From_String_Univ_Int'Access);
      Register_Builtin
        (String_Lookup ("#print_univ_int"), Print_Univ_Int'Access);
      Register_Builtin
        (String_Lookup ("#uns_from_univ"), Unsigned_From_Univ'Access);
      Register_Builtin
        (String_Lookup ("#uns_to_univ"), Unsigned_To_Univ'Access);

      Register_Builtin
        (String_Lookup ("#integer_from_univ"),
         Integer_From_Univ'Access);
      Register_Builtin
        (String_Lookup ("#integer_to_univ"),
         Integer_To_Univ'Access);

      Register_Builtin
        (String_Lookup ("#univ_integer_first"),
         Univ_Integer_First'Access);
      Register_Builtin
        (String_Lookup ("#univ_integer_last"),
         Univ_Integer_Last'Access);

   end Univ_Integer_Ops;

   ------------ Unsigned/Bit-Wise ------------

   generic
      with function Bit_Op
             (Left, Right : Unsigned_Word_Type) return Unsigned_Word_Type;
   procedure Binary_Bit_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Generic to represent simple binary operation on a word of bits

   procedure Binary_Bit_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         From_Unsigned_Word
            (Bit_Op
                (To_Unsigned_Word
                   (Fetch_Nonnull_Word (Params, 1)),
                 To_Unsigned_Word
                   (Fetch_Nonnull_Word (Params, 2)))));
   end Binary_Bit_Op;

   procedure Bit_And is new Binary_Bit_Op ("and");
   procedure Bit_Or  is new Binary_Bit_Op ("or");
   procedure Bit_Xor is new Binary_Bit_Op ("xor");

   -------------
   -- Bit_Not --
   -------------

   procedure Bit_Not
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         -1 - Fetch_Nonnull_Word (Params, 1));
   end Bit_Not;

   generic
      Name : String;
      with function Binary_Op (Left, Right : Unsigned_Word_Type)
        return Unsigned_Word_Type;
   package Binary_Unsigned_Word_Op is
      package Define_Op is new Binary_Builtin (Name,
        Left   => Unsigned_Word_Param,
        Right  => Unsigned_Word_Param,
        Result => Unsigned_Word_Param,
        Op     => Binary_Op);
   end Binary_Unsigned_Word_Op;
   --  Generic to represent simple binary operation on unsigned words

   package Uns_Add      is new Binary_Unsigned_Word_Op ("#uns_add", "+");
   package Uns_Subtract is new Binary_Unsigned_Word_Op ("#uns_subtract", "-");
   package Uns_Multiply is new Binary_Unsigned_Word_Op ("#uns_multiply", "*");
   package Uns_Divide   is new Binary_Unsigned_Word_Op ("#uns_divide", "/");
   package Uns_Mod      is new Binary_Unsigned_Word_Op ("#uns_mod", "mod");
   package Uns_And      is new Binary_Unsigned_Word_Op ("#uns_and", "and");
   package Uns_Or       is new Binary_Unsigned_Word_Op ("#uns_or", "or");
   package Uns_Xor      is new Binary_Unsigned_Word_Op ("#uns_xor", "xor");
   package Uns_Min      is
     new Binary_Unsigned_Word_Op ("#uns_min", Unsigned_Word_Type'Min);
   package Uns_Max      is
     new Binary_Unsigned_Word_Op ("#uns_max", Unsigned_Word_Type'Max);

   generic
      Name : String;
      with function Binary_Op (Left, Right : Unsigned_Word_Type)
        return Unsigned_Word_Type;
   package Binary_Unsigned_Assign_Op is
   --  Unsigned word "op=" operator
      package Define_Op is new Assign_Op_Builtin (Name,
        Left  => Unsigned_Word_Param,
        Right => Unsigned_Word_Param,
        Op    => Binary_Op);
   end Binary_Unsigned_Assign_Op;

   package Unsigned_Assign_Add is
     new Binary_Unsigned_Assign_Op ("#uns_assign_add", "+");
   package Unsigned_Assign_Subtract is
     new Binary_Unsigned_Assign_Op ("#uns_assign_subtract", "-");
   package Unsigned_Assign_Multiply is
     new Binary_Unsigned_Assign_Op ("#uns_assign_multiply", "*");
   package Unsigned_Assign_Divide is
     new Binary_Unsigned_Assign_Op ("#uns_assign_divide", "/");

   function Unsigned_Exponentiate_Op
     (Left : Unsigned_Word_Type; Right : Word_Type)
      return Unsigned_Word_Type is
   begin
      return Left ** Integer (Right);
   end Unsigned_Exponentiate_Op;

   package Unsigned_Exponentiate is new Binary_Builtin ("#uns_exp",
     Left   => Unsigned_Word_Param,
     Right  => Nonnull_Word_Param,
     Result => Unsigned_Word_Param,
     Op     => Unsigned_Exponentiate_Op);

   package Unsigned_Assign_Exponentiate is new Assign_Op_Builtin
     (Name  => "#uns_assign_exp",
      Left  => Unsigned_Word_Param,
      Right => Nonnull_Word_Param,
      Op    => Unsigned_Exponentiate_Op);

   function Unsigned_Lshift_Op
     (Left : Unsigned_Word_Type; Right : Word_Type)
      return Unsigned_Word_Type is
   begin
      return Unsigned_Word_Type
        (Interfaces.Shift_Left
           (Interfaces.Unsigned_64 (Left), Integer (Right)));
   end Unsigned_Lshift_Op;

   package Unsigned_Lshift_Builtin is new Binary_Builtin ("#uns_lshift",
     Left   => Unsigned_Word_Param,
     Right  => Nonnull_Word_Param,
     Result => Unsigned_Word_Param,
     Op     => Unsigned_Lshift_Op);

   function Unsigned_Rshift_Op
     (Left : Unsigned_Word_Type; Right : Word_Type)
      return Unsigned_Word_Type is
   begin
      return Unsigned_Word_Type
        (Interfaces.Shift_Right
           (Interfaces.Unsigned_64 (Left), Integer (Right)));
   end Unsigned_Rshift_Op;

   package Unsigned_Rshift_Builtin is new Binary_Builtin ("#uns_rshift",
     Left   => Unsigned_Word_Param,
     Right  => Nonnull_Word_Param,
     Result => Unsigned_Word_Param,
     Op     => Unsigned_Rshift_Op);

   function Unsigned_Compare_Op
     (Left, Right : Unsigned_Word_Type) return Word_Type is
   --  Unsigned word version of "=?"
      Result : Ordering;
   begin
      if Left < Right then
         Result := Less;
      elsif Left > Right then
         Result := Greater;
      else
         Result := Equal;
      end if;

      return Ordering'Pos (Result);
   end Unsigned_Compare_Op;

   package Unsigned_Compare is new Binary_Builtin ("#uns_compare",
     Left   => Unsigned_Word_Param,
     Right  => Unsigned_Word_Param,
     Result => Nonnull_Word_Param,
     Op     => Unsigned_Compare_Op);

   package Unsigned_Negate is new Unary_Builtin ("#uns_negate",
     Operand => Unsigned_Word_Param,
     Result => Unsigned_Word_Param,
     Op => "-");

   ------------------------
   -- To_String_Unsigned --
   ------------------------

   procedure To_String_Unsigned
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
         --  Target gives target region for constructed Univ_String
   begin
      Store_Word
        (Params, 0,
         Univ_Strings.To_Word_Type
            (Univ_Strings.From_String
                (Unsigned_Image (Fetch_Unsigned (Params, 1)),
                 Target,
                 Context.Server_Index)));
   end To_String_Unsigned;

   --------------------------
   -- From_String_Unsigned --
   --------------------------

   procedure From_String_Unsigned
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
   begin
      Store_Unsigned
        (Params, 0,
         Unsigned_Value
           (Word_To_String (Fetch_Word (Params, 1))));
   end From_String_Unsigned;

   ------------ Boolean ------------

   generic
      with function Bool_Op (Left, Right : Boolean) return Boolean;
   procedure Binary_Bool_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Generic to represent simple boolean binary operation on words

   procedure Binary_Bool_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         Boolean'Pos
            (Bool_Op
                (Boolean'Val
                    (Fetch_Nonnull_Word (Params, 1)),
                 Boolean'Val
                    (Fetch_Nonnull_Word (Params, 2)))));
   end Binary_Bool_Op;

   procedure Bool_And is new Binary_Bool_Op ("and");
   procedure Bool_Or  is new Binary_Bool_Op ("or");
   procedure Bool_Xor is new Binary_Bool_Op ("xor");

   --------------
   -- Bool_Not --
   --------------

   procedure Bool_Not
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         1 - Fetch_Nonnull_Word (Params, 1));
   end Bool_Not;

   True_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#true"));

   False_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#false"));

   --------------------
   -- Bool_From_Univ --
   --------------------

   procedure Bool_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Index  : constant Strings.U_String_Index :=
                Strings.Index (To_U_String (Fetch_Word (Params, 1)));
      Result : Boolean := False;
      use type Strings.U_String_Index;
   begin
      --  Determine boolean value from index of enumeration literal
      if Index = True_Index then
         Result := True;
      elsif Index = False_Index then
         Result := False;
      else
         Messages.Put_Error
           ("Boolean literal must be #true or #false" &
            ", value = " &
            Strings.To_String (Strings.To_U_String (Index)),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (False);
         null;
      end if;

      Store_Word (Params, 0, Boolean'Pos (Result));
   end Bool_From_Univ;

   ------------------
   -- Bool_To_Univ --
   ------------------

   procedure Bool_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Bool_Pos : constant Word_Type := Fetch_Word (Params, 1);
      Val      : constant Boolean := Boolean'Val (Bool_Pos);
      Result   : Strings.U_String_Index;
   begin
      --  Determine univ-enum value given boolean value
      if Val then
         Result := True_Index;
      else
         Result := False_Index;
      end if;

      Store_Word (Params, 0, Word_Type (Result));
   end Bool_To_Univ;

   ---------------- Ordering and Direction -----------------

   Less_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#less"));

   Equal_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#equal"));

   Greater_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#greater"));

   Unordered_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#unordered"));

   Forward_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#forward"));

   Reverse_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#reverse"));

   Concurrent_Index : Strings.U_String_Index :=
     Strings.Index (Strings.String_Lookup ("#concurrent"));

   ------------------------
   -- Ordering_From_Univ --
   ------------------------

   procedure Ordering_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  "from_univ"(Univ_Enumeration) -> Ordering
      Index   : constant Strings.U_String_Index :=
                  Strings.Index (To_U_String
                    (Fetch_Word (Params, 1)));
      Result  : Ordering := Unordered;
      use type Strings.U_String_Index;
   begin
      --  Determine Ordering value from index of enumeration literal
      if Index = Less_Index then
         Result := Less;
      elsif Index = Equal_Index then
         Result := Equal;
      elsif Index = Greater_Index then
         Result := Greater;
      elsif Index = Unordered_Index then
         Result := Unordered;
      else
         Messages.Put_Error
           ("Ordering literal must be #less, #equal, #greater, " &
            "or #unordered" &
            ", value = " &
            Strings.To_String (Strings.To_U_String (Index)),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (False);
         null;
      end if;

      Store_Word (Params, 0, Ordering'Pos (Result));
   end Ordering_From_Univ;

   ----------------------
   -- Ordering_To_Univ --
   ----------------------

   procedure Ordering_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Ordering_Pos : constant Word_Type :=
                       Fetch_Word (Params, 1);

      Val : constant Ordering := Ordering'Val (Ordering_Pos);
      Map : constant array (Ordering) of Strings.U_String_Index :=
              (Less      => Less_Index,
               Equal     => Equal_Index,
               Greater   => Greater_Index,
               Unordered => Unordered_Index);
   --  Determine univ-enum value given ordering value
   begin
      Store_Word (Params, 0, Word_Type (Map (Val)));
   end Ordering_To_Univ;

   ----------------------
   -- Ordering_To_Bool --
   ----------------------

   procedure Ordering_To_Bool
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Ord       : constant Ordering :=
                    Ordering'Val (Fetch_Word (Params, 1));
      Cond_Mask : constant Condition_Bit_Mask :=
                    Condition_Bit_Mask (Fetch_Word (Params, 2));
      Result    : Boolean := False;
   begin
      Result := (Ordering_Mask (Ord) and Cond_Mask) /= 0;
      --  If any of the desired bits are set, the result is True.

      Store_Word (Params, 0, Boolean'Pos (Result));
   end Ordering_To_Bool;

   -------------------------
   -- Direction_From_Univ --
   -------------------------

   procedure Direction_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Index : constant Strings.U_String_Index :=
                Strings.Index (To_U_String (Fetch_Word (Params, 1)));
      Result : Direction := Unordered_Dir;
      use type Strings.U_String_Index;
   begin
      --  Determine Ordering value from index of enumeration literal
      if Index = Unordered_Index then
         Result := Unordered_Dir;
      elsif Index = Forward_Index then
         Result := Forward_Dir;
      elsif Index = Reverse_Index then
         Result := Reverse_Dir;
      elsif Index = Concurrent_Index then
         Result := Concurrent_Dir;
      else
         Messages.Put_Error
           ("Direction literal " &
            "must be #unordered, #forward, #reverse, or #concurrent" &
            ", value = " &
            Strings.To_String (Strings.To_U_String (Index)),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (False);
         null;
      end if;

      Store_Word (Params, 0, Direction'Pos (Result));
   end Direction_From_Univ;

   -----------------------
   -- Direction_To_Univ --
   -----------------------

   procedure Direction_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  "to_univ"(Direction) -> Univ_Enumeration
      Val : constant Direction :=
        Direction'Val (Fetch_Word (Params, 1));
      Map : constant array (Direction) of Strings.U_String_Index :=
        (Unordered_Dir => Unordered_Index,
         Forward_Dir => Forward_Index,
         Reverse_Dir => Reverse_Index,
         Concurrent_Dir => Concurrent_Index);
   --  Determine univ-enum value given direction value
   begin
      Store_Word (Params, 0, Word_Type (Map (Val)));
   end Direction_To_Univ;

   -----------------------
   -- Unordered_Compare --
   -----------------------

   procedure Unordered_Compare
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Left   : constant Word_Type := Fetch_Word (Params, 1);
      Right  : constant Word_Type := Fetch_Word (Params, 2);
      Result : Ordering;
   begin
      if Left /= Right then
         Result := Unordered;
      else
         Result := Equal;
      end if;

      Store_Word (Params, 0, Ordering'Pos (Result));
   end Unordered_Compare;

   --------------
   -- Identity --
   --------------

   procedure Identity
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         Fetch_Word (Params, 1));
   end Identity;

   -----------
   -- No_Op --
   -----------

   procedure No_Op
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      null;
   end No_Op;

   -------------------
   -- Concat_String --
   -------------------

   procedure Concat_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (Word_To_Wide_Wide_String (Fetch_Word (Params, 1)) &
                 Word_To_Wide_Wide_String (Fetch_Word (Params, 2)),
                Target,
                Context.Server_Index)));
   end Concat_String;

   --------------------------
   -- Assign_Concat_String --
   --------------------------

   procedure Assign_Concat_String
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 0);
   begin
      Univ_Strings.Assign_Concat_Wide_Wide_String
        (Str_Ptr => Target_Ptr,
         New_String => Word_To_Wide_Wide_String (Fetch_Word (Params, 1)),
         Server_Index => Context.Server_Index);
   end Assign_Concat_String;

   ----------------
   -- Concat_Int --
   ----------------

   procedure Concat_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target : constant Word_Type := Fetch_Word (Params, 0);
      use Ada.Characters.Conversions;
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (Word_To_Wide_Wide_String (Fetch_Word (Params, 1)) &
                   To_Wide_Wide_String (Word_Image (Fetch_Word (Params, 2))),
                 Target,
                 Context.Server_Index)));
   end Concat_Int;

   --------------------
   -- Concat_Int_Str --
   --------------------

   procedure Concat_Int_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target : constant Word_Type := Fetch_Word (Params, 0);
      use Ada.Characters.Conversions;
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (To_Wide_Wide_String (Word_Image (Fetch_Word (Params, 1))) &
                   Word_To_Wide_Wide_String (Fetch_Word (Params, 2)),
                 Target,
                 Context.Server_Index)));
   end Concat_Int_Str;

   -------------------
   -- To_String_Int --
   -------------------

   procedure To_String_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         Univ_Strings.To_Word_Type
            (Univ_Strings.From_String
                (Word_Image (Fetch_Word (Params, 1)),
                 Target,
                 Context.Server_Index)));
   end To_String_Int;

   ---------------------
   -- From_String_Int --
   ---------------------

   procedure From_String_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
   begin
      Store_Word
        (Params, 0,
         Integer_Value (Word_To_String (Fetch_Word (Params, 1))));
   end From_String_Int;

   -----------------
   -- Concat_Real --
   -----------------

   procedure Concat_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target : constant Word_Type := Fetch_Word (Params, 0);
      use Ada.Characters.Conversions;
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (Word_To_Wide_Wide_String (Fetch_Word (Params, 1)) &
                   To_Wide_Wide_String (Real_Image (Fetch_Real (Params, 2))),
                 Target,
                 Context.Server_Index)));
   end Concat_Real;

   ---------------------
   -- Concat_Real_Str --
   ---------------------

   procedure Concat_Real_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
      Target : constant Word_Type := Fetch_Word (Params, 0);
      use Ada.Characters.Conversions;
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (To_Wide_Wide_String (Real_Image (Fetch_Real (Params, 1))) &
                   Word_To_Wide_Wide_String (Fetch_Word (Params, 2)),
                 Target,
                 Context.Server_Index)));
   end Concat_Real_Str;

   --------------------
   -- To_String_Real --
   --------------------

   procedure To_String_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         Univ_Strings.To_Word_Type
            (Univ_Strings.From_String
                (Real_Image (Fetch_Real (Params, 1)),
                 Target,
                 Context.Server_Index)));
   end To_String_Real;

   ----------------------
   -- From_String_Real --
   ----------------------

   procedure From_String_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      use Univ_Strings;
   begin
      Store_Word
        (Params, 0,
         From_Univ_Real
            (Real_Value
                (Word_To_String (Fetch_Word (Params, 1)))));
   end From_String_Real;

   -----------------
   -- Concat_Char --
   -----------------

   procedure Concat_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
   begin
      Store_Word
        (Params, 0,
         Univ_Strings.To_Word_Type
            (Univ_Strings.From_Wide_Wide_String
                (Word_To_Wide_Wide_String (Fetch_Word (Params, 1)) &
                   Wide_Wide_Character'Val (Fetch_Nonnull_Word (Params, 2)),
                 Target,
                 Context.Server_Index)));
   end Concat_Char;

   ------------------------
   -- Assign_Concat_Char --
   ------------------------

   procedure Assign_Concat_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target_Ptr : constant Word_Ptr := Fetch_Word_Ptr (Params, 0);
      use Univ_Strings;
      Target_Val : constant Univ_String :=
        From_Word_Type (Content_Of_Physical_Address (Target_Ptr));
   begin
      Univ_Strings.Assign_Concat_Wide_Wide_String
        (Str_Ptr => Target_Ptr,
         New_String =>
           (1 => Wide_Wide_Character'Val (Fetch_Nonnull_Word (Params, 1))),
         Server_Index => Context.Server_Index);
   end Assign_Concat_Char;

   ---------------------
   -- Concat_Char_Str --
   ---------------------

   procedure Concat_Char_Str
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
      use Univ_Strings;
   begin
      Store_Word
        (Params, 0,
         To_Word_Type
            (From_Wide_Wide_String
                (Wide_Wide_Character'Val (Fetch_Nonnull_Word (Params, 1)) &
                   Word_To_Wide_Wide_String (Fetch_Word (Params, 2)),
                 Target,
                 Context.Server_Index)));
   end Concat_Char_Str;

   --------------------
   -- To_String_Char --
   --------------------

   procedure To_String_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      Target : constant Word_Type := Fetch_Word (Params, 0);
      Result : Word_Type;
   begin
      if Target = Null_Value then
         Result :=
            Univ_Strings.To_Word_Type
               (Univ_Strings.From_String
                  ("null",
                    Target,
                    Context.Server_Index));
      else
         Result :=
            Univ_Strings.To_Word_Type
               (Univ_Strings.From_Wide_Wide_String
                   ((1 => Wide_Wide_Character'Val (Fetch_Word (Params, 1))),
                    Target,
                    Context.Server_Index));
      end if;
      Store_Word (Params, 0, Result);
   end To_String_Char;

   ----------------------
   -- From_String_Char --
   ----------------------

   procedure From_String_Char
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Str : Wide_Wide_String
        renames Word_To_Wide_Wide_String (Fetch_Word (Params, 1));
      Result : Word_Type;
   begin
      if Str = "null" then
         Result := Null_Value;
      elsif Str'Length = 3 and then Str (Str'First) = '''
        and then Str (Str'Last) = '''
      then
         Result := Wide_Wide_Character'Pos (Str (Str'First + 1));
      else
         Result := Wide_Wide_Character'Pos (Str (Str'First));
      end if;

      Store_Word
        (Params, 0, Result);
   end From_String_Char;

   ---------------------
   -- String_Indexing --
   ---------------------

   procedure String_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Str_Word : constant Word_Type := Fetch_Word (Params, 1);
      pragma Assert (not Is_Large_Null (Str_Word));
      Index : constant Word_Type := Fetch_Nonnull_Word (Params, 2);
   begin
      Store_Word
        (Params, 0,
           Univ_Strings.Nth_Univ_Character
              (Univ_Strings.From_Word_Type (Str_Word),
              Positive (Index)));
   end String_Indexing;

   --------------------
   -- String_Slicing --
   --------------------

   procedure String_Slicing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Target : constant Word_Type := Fetch_Word (Params, 0);
      Str_Word : constant Word_Type := Fetch_Word (Params, 1);
      pragma Assert (not Is_Large_Null (Str_Word));
      Str : Wide_Wide_String
        renames Word_To_Wide_Wide_String (Str_Word);

      Index_Range : constant Object_Virtual_Address :=
                      Fetch_Word (Params, 2);
      Low_Bound   : constant Integer :=
                      Integer (Content_Of_Virtual_Address
                                (Index_Range + Large_Obj_Header_Size));
      High_Bound  : constant Integer :=
                      Integer (Content_Of_Virtual_Address
                                (Index_Range +
                                 Large_Obj_Header_Size +
                                 Offset_Within_Chunk'(1)));
   begin
      Store_Word
        (Params, 0,
         Univ_Strings.To_Word_Type
            (Univ_Strings.From_Wide_Wide_String
                (Str (Str'First + Low_Bound - 1
                        .. Str'First + High_Bound - 1),
                 Target,
                 Context.Server_Index)));
   end String_Slicing;

   -------------------
   -- String_Length --
   -------------------

   procedure String_Length
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Str : constant Word_Type := Fetch_Word (Params, 1);
      pragma Assert (not Is_Large_Null (Str));
   begin
      Store_Word (Params, 0, Word_Type (Univ_Strings.Length
        (Univ_Strings.From_Word_Type (Str))));
   end String_Length;

   --------------------
   -- String_Compare --
   --------------------

   procedure String_Compare
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Str1 : constant Word_Type := Fetch_Word (Params, 1);
      Str2 : constant Word_Type := Fetch_Word (Params, 2);
      pragma Assert (not Is_Large_Null (Str1));
      pragma Assert (not Is_Large_Null (Str2));
      Result : constant Ordering := Univ_Strings.Compare
        (Univ_Strings.From_Word_Type (Str1),
         Univ_Strings.From_Word_Type (Str2));
   begin
      Store_Word (Params, 0, Ordering'Pos (Result));
   end String_Compare;

   ------------------------
   -- Basic_Array_Create --
   ------------------------

   procedure Basic_Array_Create
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Max_Basic_Array_Len : constant := Offset_Within_Area'Last - 2;
      --  Current maximum Basic_Array, due to limit on large-obj size
      --  being Offset_Within_Area'Last.

      function Check_Len (Len : Word_Type) return Word_Type;
      --  Make sure length is non-negative

      function Check_Len (Len : Word_Type) return Word_Type is
      begin
         if Len < 0 then
            Messages.Put_Error
              ("Basic_Array::Create passed negative length = " &
               Word_Type'Image (Len),
               Src_Pos => Execution_Source_Pos);
            pragma Warnings (Off);
            pragma Assert (Len >= 0);  --  Will fail
            pragma Warnings (On);
            return 0;
         elsif Len > Max_Basic_Array_Len then
            Messages.Put_Error
              ("Basic_Array::Create length =" &
               Word_Type'Image (Len) & " too large, limit =" &
               Offset_Within_Area'Image (Max_Basic_Array_Len) &
               "; Try Big_Array instead",
               Src_Pos => Execution_Source_Pos);
            pragma Warnings (Off);
            pragma Assert (Len <= Max_Basic_Array_Len);  --  Will fail
            pragma Warnings (On);
            return Max_Basic_Array_Len;
         else
            return Len;
         end if;
      end Check_Len;

      Len : constant Word_Type :=
              Check_Len (Fetch_Nonnull_Word (Params, 1));
      Val : constant Word_Type := Fetch_Word (Params, 2);

      Target         : constant Word_Type :=
                         Fetch_Word (Params, 0);
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
      Type_Desc      : constant Non_Op_Map_Type_Ptr := Static_Link;

      pragma Assert
        (Type_Desc.Type_Kind = Normal_Kind
           or else Type_Desc.Type_Kind = Basic_Array_Kind);

      Element_Type : constant Non_Op_Map_Type_Ptr :=
                       Basic_Array_Comp_Type (Type_Desc);

   begin  --  Basic_Array_Create

      --  Mark type so it can be copied and freed correctly.
      Type_Desc.Type_Kind := Basic_Array_Kind;

      --  Return new object
      Store_Word (Params, 0,
        Create_Basic_Array_Obj
          (Type_Desc, Natural (Len), Target_Stg_Rgn, Context.Server_Index,
           Init_Elements => True, Element_Value => Val));
   end Basic_Array_Create;

   --------------------------
   -- Basic_Array_Indexing --
   --------------------------

   procedure Basic_Array_Indexing
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Type_Desc : constant Non_Op_Map_Type_Ptr := Static_Link;
      pragma Assert
        (Type_Desc.Type_Kind = Normal_Kind
           or else Type_Desc.Type_Kind = Basic_Array_Kind);
      --  TBD: How did we get here without first calling create?

      Arr   : constant Word_Type :=
                Content_Of_Physical_Address
                  (Fetch_Word_Ptr (Params, 1));
      Index : constant Offset_Within_Chunk :=
                Offset_Within_Chunk (Fetch_Nonnull_Word (Params, 2));
      Len   : constant Offset_Within_Chunk := Offset_Within_Chunk
                (Content_Of_Virtual_Address (Arr + Large_Obj_Header_Size));

      Element_Type : constant Non_Op_Map_Type_Ptr :=
                       Basic_Array_Comp_Type (Type_Desc);
      --  For Debugging

   begin
      if Is_Large_Null (Arr) then
         Messages.Put_Error
           ("Cannot index into null array",
            Src_Pos => Execution_Source_Pos);
         pragma Assert (not Is_Large_Null (Arr));
      end if;

      if Index not in 1 .. Len then
         Messages.Put_Error
           ("Index must be in 1 .. " &
            Offset_Within_Chunk'Image (Len) &
            ", value = " &
            Offset_Within_Chunk'Image (Index),
            Src_Pos => Execution_Source_Pos);
         pragma Assert (Index in 1 .. Len);
      end if;

      --  Mark type so it can be copied and freed correctly.
      Type_Desc.Type_Kind := Basic_Array_Kind;

      --  TBD: How did we get here without first calling create on same type?

      if not Is_Small (Element_Type) then
         --  For Debugging: Verify that regions match
         pragma Assert
           (Stg_Rgn_Of_Large_Obj (Arr) =
            Stg_Rgn_Of_Large_Obj
               (Content_Of_Virtual_Address
                   (Arr + Large_Obj_Header_Size + Index)));
         null;
      end if;

      --  Pass back address of selected element

      Store_Word_Ptr
        (Params, 0,
         Virtual_To_Physical_Address (Arr + Large_Obj_Header_Size + Index));
   end Basic_Array_Indexing;

   ------------------------
   -- Basic_Array_Length --
   ------------------------

   procedure Basic_Array_Length
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Type_Desc : constant Non_Op_Map_Type_Ptr := Static_Link;
      pragma Assert (Type_Desc.Type_Kind = Basic_Array_Kind);
      --  or else (Type_Desc.Has_Op_Map and then
      --  Type_Desc.Actual_Type.Type_Kind = Basic_Array_Kind));

      Arr : constant Word_Type := Fetch_Word (Params, 1);
      pragma Assert (not Is_Large_Null (Arr));
      Len : constant Word_Type :=
              Content_Of_Virtual_Address (Arr + Large_Obj_Header_Size);
   begin
      Store_Word (Params, 0, Len);
   end Basic_Array_Length;

   -----------------
   -- Real_Negate --
   -----------------

   package Real_Negate is new Unary_Builtin ("#real_negate",
     Operand => Nonnull_Real_Param,
     Result  => Real_Param,
     Op      => "-");

   --------------
   -- Real_Abs --
   --------------

   package Real_Abs is new Unary_Builtin ("#real_abs",
     Operand => Nonnull_Real_Param,
     Result  => Real_Param,
     Op      => "abs");

   -------------------
   -- Univ_Real_Min --
   -------------------

   function Univ_Real_Min (Left, Right : Univ_Real) return Univ_Real is
   begin
      if From_Univ_Real (Left) = Null_Float_Value then
         return Right;
      elsif From_Univ_Real (Right) = Null_Float_Value then
         return Left;
      else
         return Univ_Real'Min (Left, Right);
      end if;
   end Univ_Real_Min;

   -------------------
   -- Univ_Real_Max --
   -------------------

   function Univ_Real_Max (Left, Right : Univ_Real) return Univ_Real is
   begin
      if From_Univ_Real (Left) = Null_Float_Value then
         return Right;
      elsif From_Univ_Real (Right) = Null_Float_Value then
         return Left;
      else
         return Univ_Real'Max (Left, Right);
      end if;
   end Univ_Real_Max;

   --------------------
   -- Binary_Real_Op --
   --------------------

   --  Generic to represent simple binary operation on univ-reals

   generic
      Name : String;
      with function Binary_Op (Left, Right : Univ_Real) return Univ_Real;
   package Binary_Real_Op is
      package Define_Op is new Binary_Builtin (Name,
        Left   => Nonnull_Real_Param,
        Right  => Nonnull_Real_Param,
        Result => Nonnull_Real_Param,
        Op     => Binary_Op);
   end Binary_Real_Op;

   -----------------------------
   -- Binary_Optional_Real_Op --
   -----------------------------

   --  Generic to represent simple binary operation on possibly-null univ-reals

   generic
      Name : String;
      with function Binary_Op (Left, Right : Univ_Real) return Univ_Real;
   package Binary_Optional_Real_Op is
      package Define_Op is new Binary_Builtin (Name,
        Left   => Real_Param,
        Right  => Real_Param,
        Result => Real_Param,
        Op     => Binary_Op);
   end Binary_Optional_Real_Op;

   --------------------
   -- Real Operators --
   --------------------

   package Real_Add      is new Binary_Real_Op ("#real_add", "+");
   package Real_Subtract is new Binary_Real_Op ("#real_subtract", "-");
   package Real_Multiply is new Binary_Real_Op ("#real_multiply", "*");
   package Real_Divide   is new Binary_Real_Op ("#real_divide", "/");

   package Real_Min is                                        --  Allow null
     new Binary_Optional_Real_Op ("#real_min", Univ_Real_Min);
   package Real_Max is                                        --  Allow null
     new Binary_Optional_Real_Op ("#real_max", Univ_Real_Max);

   function Real_Exponentiate_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real is
   begin
      return Left ** Integer (Right);
   end Real_Exponentiate_Op;

   package Real_Exponentiate is new Binary_Builtin ("#real_exp",
     Left   => Nonnull_Real_Param,
     Right  => Nonnull_Word_Param,
     Result => Nonnull_Real_Param,
     Op     => Real_Exponentiate_Op);

   function Real_Int_Multiply_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real is
   begin
      return Left * Univ_Real (Right);
   end Real_Int_Multiply_Op;

   function Real_Int_Divide_Op
     (Left : Univ_Real; Right : Word_Type) return Univ_Real is
   begin
      return Left / Univ_Real (Right);
   end Real_Int_Divide_Op;

   package Real_Int_Multiply is new Binary_Builtin ("#real_int_multiply",
     Left   => Nonnull_Real_Param,
     Right  => Nonnull_Word_Param,
     Result => Nonnull_Real_Param,
     Op     => Real_Int_Multiply_Op);

   package Real_Int_Divide is new Binary_Builtin ("#real_int_divide",
     Left   => Nonnull_Real_Param,
     Right  => Nonnull_Word_Param,
     Result => Nonnull_Real_Param,
     Op     => Real_Int_Divide_Op);

   generic
      Name : String;
      with function Binary_Op (Left, Right : Univ_Real) return Univ_Real;
   package Binary_Real_Assign_Op is
   --  Real "op=" operator
      package Define_Op is new Assign_Op_Builtin (Name,
        Left  => Nonnull_Real_Param,
        Right => Nonnull_Real_Param,
        Op    => Binary_Op);
   end Binary_Real_Assign_Op;

   package Real_Assign_Add is
     new Binary_Real_Assign_Op ("#real_assign_add", "+");
   package Real_Assign_Subtract is
     new Binary_Real_Assign_Op ("#real_assign_subtract", "-");
   package Real_Assign_Multiply is
     new Binary_Real_Assign_Op ("#real_assign_multiply", "*");
   package Real_Assign_Divide is
     new Binary_Real_Assign_Op ("#real_assign_divide", "/");

   package Real_Assign_Exponentiate is new Assign_Op_Builtin
     (Name  => "#real_assign_exp",
      Left  => Nonnull_Real_Param,
      Right => Nonnull_Word_Param,
      Op    => Real_Exponentiate_Op);

   function Real_Compare_Op
     (Left, Right : Univ_Real) return Word_Type
   is
      Result : Ordering;
   begin
      if Left < Right then
         Result := Less;
      elsif Left > Right then
         Result := Greater;
      elsif Left = Right then
         Result := Equal;
      else
         Result := Unordered;
      end if;

      return Ordering'Pos (Result);
   end Real_Compare_Op;

   package Real_Compare is new Binary_Builtin ("#real_compare",
     Left   => Nonnull_Real_Param,
     Right  => Nonnull_Real_Param,
     Result => Nonnull_Word_Param,
     Op     => Real_Compare_Op);

   ------------------------
   -- Time_Int_From_Univ --
   ------------------------

   procedure Time_Int_From_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Real_Val : constant Univ_Real :=
                   Fetch_Nonnull_Real (Params, 1);
   begin
      Store_Word (Params, 0, Word_Type (Real_Val * 10.0 ** 9));
      --  Convert to a count of nanoseconds
   end Time_Int_From_Univ;

   ----------------------
   -- Time_Int_To_Univ --
   ----------------------

   procedure Time_Int_To_Univ
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Nanos    : constant Word_Type := Fetch_Word (Params, 1);
      Real_Val : Univ_Real;
   begin
      if Nanos = Null_Value then
         --  TBD: Should this work for null?
         --  Null int => Null real (NaN)
         Real_Val := Null_Real_Value;
      else
         Real_Val := Univ_Real (Nanos) / 10.0 ** 9;
      end if;
      Store_Real (Params, 0, Real_Val);
   end Time_Int_To_Univ;

   ------------------
   -- Round_To_Int --
   ------------------

   procedure Round_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Real_Val : constant Univ_Real :=
                   Fetch_Nonnull_Real (Params, 1);
   begin
      Store_Word (Params, 0, Word_Type (Real_Val));
      --  Round to int
   end Round_To_Int;

   ------------------
   -- Trunc_To_Int --
   ------------------

   procedure Trunc_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Real_Val : constant Univ_Real :=
                   Univ_Real'Truncation (Fetch_Nonnull_Real (Params, 1));
   begin
      Store_Word (Params, 0, Word_Type (Real_Val));
      --  Trunc to int
   end Trunc_To_Int;

   ------------------
   -- Floor_To_Int --
   ------------------

   procedure Floor_To_Int
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Real_Val : constant Univ_Real :=
                   Univ_Real'Floor (Fetch_Nonnull_Real (Params, 1));
   begin
      Store_Word (Params, 0, Word_Type (Real_Val));
      --  Floor to int
   end Floor_To_Int;

   -----------------
   -- Int_To_Real --
   -----------------

   procedure Int_To_Real
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      Int_Val  : constant Word_Type := Fetch_Word (Params, 1);
      Real_Val : Univ_Real;
   begin
      if Int_Val = Null_Value then
         --  TBD: Should this work for null?
         --  Null int => Null real (NaN)
         Real_Val := Null_Real_Value;
      else
         Real_Val := Univ_Real (Int_Val);
      end if;

      Store_Real (Params, 0, Real_Val);
   end Int_To_Real;

   --------------------
   -- Fixed_Real_Mul --
   --------------------

   procedure Fixed_Real_Mul
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         Word_Type (Univ_Real (Fetch_Nonnull_Word (Params, 1)) *
                    Fetch_Nonnull_Real (Params, 2)));
   end Fixed_Real_Mul;

   --------------------
   -- Real_Fixed_Mul --
   --------------------

   procedure Real_Fixed_Mul
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         Word_Type (Univ_Real (Fetch_Nonnull_Word (Params, 2)) *
                    Fetch_Nonnull_Real (Params, 1)));
   end Real_Fixed_Mul;

   --------------------
   -- Fixed_Real_Div --
   --------------------

   procedure Fixed_Real_Div
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word
        (Params, 0,
         Word_Type (Univ_Real (Fetch_Nonnull_Word (Params, 1)) /
                    Fetch_Nonnull_Real (Params, 2)));
   end Fixed_Real_Div;

   --------------
   -- To_Nanos --
   --------------

   Start_Of_Epoch : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Year => 1970, Month => 1, Day => 1);

   function To_Nanos (From : Ada.Calendar.Time) return Word_Type is
      use Ada.Calendar.Arithmetic;
      Days    : Day_Count;
      Seconds : Duration;
      Leaps   : Leap_Seconds_Count;
   begin
      Difference
        (From,
         Start_Of_Epoch,
         Days => Days,
         Seconds => Seconds,
         Leap_Seconds => Leaps);

      if Debug_Threading then
         Put_Line
           ("Difference returns Days => " &
            Day_Count'Image (Days) &
            ", Seconds => " &
            Duration'Image (Seconds));
      end if;

      declare
         Nanos_Per_Day  : constant Word_Type := 10 ** 9 * 86400;
         Dur_Count      : constant Word_Type :=
                            Word_Type (Seconds / Duration'Small);
         Dur_Multiplier : constant Univ_Real :=
                            Univ_Real (Duration'Small * 10.0 ** 9);

         Result : constant Word_Type :=
                    Word_Type (Days) * Nanos_Per_Day +
                    Word_Type (Univ_Real (Dur_Count) * Dur_Multiplier);
      begin
         if Debug_Threading then
            Put_Line
              ("Seconds in units of Duration'Small = " &
               Word_Type'Image (Dur_Count));
            Put_Line
              ("Multiplier for above = " & Real_Image (Dur_Multiplier));
            Put_Line ("Time in Nanos = " & Word_Type'Image (Result));
         end if;

         return Result;
      end;
   end To_Nanos;

   ----------------------
   -- To_Calendar_Time --
   ----------------------

   function To_Calendar_Time
     (Nano_Count : Word_Type) return Ada.Calendar.Time
   is
      use Ada.Calendar.Arithmetic;
      use Ada.Calendar;

      Nanos_Per_Day : constant Word_Type := 10 ** 9 * 86400;

      Result : constant Ada.Calendar.Time :=
        Start_Of_Epoch +
        Day_Count (Nano_Count / Nanos_Per_Day) +
        Duration (Univ_Real (Nano_Count mod Nanos_Per_Day) / 10.0 ** 9);

   begin
      if Debug_Threading then
         if Nano_Count /= To_Nanos (Result) then
            Put_Line
              (" To_Nanos(To_Calendar_Time(" &
               Word_Type'Image (Nano_Count) &
               ")) = " &
               Word_Type'Image (To_Nanos (Result)));
         end if;
      end if;

      return Result;
   end To_Calendar_Time;

   ------------------------
   -- Clock_Current_Time --
   ------------------------

   procedure Clock_Current_Time
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      Store_Word (Params, 0, To_Nanos (Ada.Calendar.Clock));
   end Clock_Current_Time;

   -----------------
   -- Clock_Delay --
   -----------------

   procedure Clock_Delay
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr)
   is
      --  func Wait_For_Delay(queued C : Clock; Until : Time)
      --  Wait until the wall clock reads >= Until
      --  NOTE: Clock parameter is ignored
      Nano_Count : constant Word_Type :=
                     Fetch_Nonnull_Word (Params, 1);

      use Ada.Calendar.Arithmetic;
      use Ada.Calendar;

      Nanos_Per_Day : constant Word_Type := 10 ** 9 * 86400;
      Delay_Until   : constant Ada.Calendar.Time :=
                        To_Calendar_Time (Nano_Count);
      Can_Be_Queued : Boolean := False;

      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      if Delay_Until <= Now then
         --  No delay necessary
         if Debug_Threading then
            Put_Line (" Returning immediately because time already reached.");
            Put_Line
              (" Delay until " &
               Word_Type'Image (Nano_Count) &
               ", Time now in nanos = " &
               Word_Type'Image (To_Nanos (Now)));
         end if;

         return;
      end if;

      if Context.Control_Area /= null then
         Can_Be_Queued := Tcb_Call_Can_Be_Queued (Context.Control_Area);
      end if;

      if Debug_Threading then
         Put_Line ("Delay until " & Word_Type'Image (Nano_Count));
         if Context.Control_Area /= null then
            Put_Line ("  can-be-queued = " & Boolean'Image (Can_Be_Queued));
         end if;
      end if;

      if not Can_Be_Queued then
         --  Use dequeue condition as a precondition
         Messages.Put_Error
           ("Non-queued delay fails, since time not yet reached",
            Src_Pos => Execution_Source_Pos);
      else
         --  Add tcb to delay queue and set TCB_Was_Queued flag
         Delay_Tcb_Until (Context.Control_Area, Delay_Until);
      end if;
   end Clock_Delay;

   use Strings;
begin
   Register_Builtin (String_Lookup ("#print_int"), Print_Int'Access);
   Register_Builtin (String_Lookup ("#print_real"), Print_Real'Access);
   Register_Builtin (String_Lookup ("#print_char"), Print_Char'Access);

   Register_Builtin
     (String_Lookup ("#print_univ_enum"), Print_Univ_Enum'Access);

   Register_Builtin
     (String_Lookup ("#print_string"), Print_String'Access);
   Register_Builtin
     (String_Lookup ("#println_string"), Println_String'Access);
   Register_Builtin
     (String_Lookup ("#read_string"), Read_String'Access);

   Register_Builtin
     (String_Lookup ("#hash_enum"), Hash_Enum'Access);
   Register_Builtin
     (String_Lookup ("#to_string_enum"), To_String_Enum'Access);
   Register_Builtin
     (String_Lookup ("#from_string_enum"), From_String_Enum'Access);

   Register_Builtin
     (String_Lookup ("#hash_string"), Hash_String'Access);

   Register_Builtin (String_Lookup ("#assert_true"), Assert_True'Access);

   Register_Builtin (String_Lookup ("#identity"), Identity'Access);

   Register_Builtin (String_Lookup ("#bit_and"), Bit_And'Access);
   Register_Builtin (String_Lookup ("#bit_or"), Bit_Or'Access);
   Register_Builtin (String_Lookup ("#bit_xor"), Bit_Xor'Access);
   Register_Builtin (String_Lookup ("#bit_not"), Bit_Not'Access);

   Register_Builtin
     (String_Lookup ("#to_string_uns"),
      To_String_Unsigned'Access);
   Register_Builtin
     (String_Lookup ("#from_string_uns"),
      From_String_Unsigned'Access);

   Register_Builtin (String_Lookup ("""and"""), Bool_And'Access);
   Register_Builtin (String_Lookup ("""or"""), Bool_Or'Access);
   Register_Builtin (String_Lookup ("""xor"""), Bool_Xor'Access);
   Register_Builtin (String_Lookup ("""not"""), Bool_Not'Access);

   Register_Builtin
     (String_Lookup ("#bool_from_univ"),
      Bool_From_Univ'Access);
   Register_Builtin (String_Lookup ("#bool_to_univ"), Bool_To_Univ'Access);

   Register_Builtin
     (String_Lookup ("#ordering_from_univ"),
      Ordering_From_Univ'Access);
   Register_Builtin
     (String_Lookup ("#ordering_to_univ"),
      Ordering_To_Univ'Access);

   Register_Builtin
     (String_Lookup ("#unordered_compare"),
      Unordered_Compare'Access);

   Register_Builtin
     (String_Lookup ("#ordering_to_bool"),
      Ordering_To_Bool'Access);

   Register_Builtin
     (String_Lookup ("#direction_from_univ"),
      Direction_From_Univ'Access);
   Register_Builtin
     (String_Lookup ("#direction_to_univ"),
      Direction_To_Univ'Access);

   Register_Builtin (String_Lookup ("""null"""), No_Op'Access);
   --  Nothing to do, presuming caller fills in output parameter with a null

   Register_Builtin (String_Lookup ("#concat_string"), Concat_String'Access);
   Register_Builtin
     (String_Lookup ("#assign_concat_string"),
      Assign_Concat_String'Access);
   Register_Builtin (String_Lookup ("#concat_int"), Concat_Int'Access);
   Register_Builtin
     (String_Lookup ("#concat_int_str"),
      Concat_Int_Str'Access);
   Register_Builtin (String_Lookup ("#to_string_int"), To_String_Int'Access);
   Register_Builtin
     (String_Lookup ("#from_string_int"),
      From_String_Int'Access);
   Register_Builtin (String_Lookup ("#concat_char"), Concat_Char'Access);
   Register_Builtin
     (String_Lookup ("#assign_concat_char"),
      Assign_Concat_Char'Access);
   Register_Builtin
     (String_Lookup ("#concat_char_str"),
      Concat_Char_Str'Access);
   Register_Builtin
     (String_Lookup ("#to_string_char"),
      To_String_Char'Access);
   Register_Builtin
     (String_Lookup ("#from_string_char"),
      From_String_Char'Access);

   Register_Builtin
     (String_Lookup ("#string_compare"),
      String_Compare'Access);
   Register_Builtin
     (String_Lookup ("#string_indexing"),
      String_Indexing'Access);
   Register_Builtin
     (String_Lookup ("#string_slicing"),
      String_Slicing'Access);
   Register_Builtin (String_Lookup ("#string_length"), String_Length'Access);

   Register_Builtin
     (String_Lookup ("#basic_array_create"),
      Basic_Array_Create'Access);
   Register_Builtin
     (String_Lookup ("#basic_array_indexing"),
      Basic_Array_Indexing'Access);
   Register_Builtin
     (String_Lookup ("#basic_array_length"),
      Basic_Array_Length'Access);

   Register_Builtin (String_Lookup ("#concat_real"), Concat_Real'Access);
   Register_Builtin
     (String_Lookup ("#concat_real_str"),
      Concat_Real_Str'Access);
   Register_Builtin
     (String_Lookup ("#to_string_real"),
      To_String_Real'Access);
   Register_Builtin
     (String_Lookup ("#from_string_real"),
      From_String_Real'Access);

   Register_Builtin (String_Lookup ("#round_to_int"), Round_To_Int'Access);
   Register_Builtin (String_Lookup ("#trunc_to_int"), Trunc_To_Int'Access);
   Register_Builtin (String_Lookup ("#floor_to_int"), Floor_To_Int'Access);
   Register_Builtin (String_Lookup ("#int_to_real"), Int_To_Real'Access);

   Register_Builtin
     (String_Lookup ("#time_int_from_univ"),
      Time_Int_From_Univ'Access);
   Register_Builtin
     (String_Lookup ("#time_int_to_univ"),
      Time_Int_To_Univ'Access);

   Register_Builtin
     (String_Lookup ("#fixed_real_mul"),
      Fixed_Real_Mul'Access);
   Register_Builtin
     (String_Lookup ("#real_fixed_mul"),
      Real_Fixed_Mul'Access);
   Register_Builtin
     (String_Lookup ("#fixed_real_div"),
      Fixed_Real_Div'Access);

   Register_Builtin
     (String_Lookup ("#clock_current_time"),
      Clock_Current_Time'Access);
   Register_Builtin (String_Lookup ("#clock_delay"), Clock_Delay'Access);

end PSC.Interpreter.Builtins;
