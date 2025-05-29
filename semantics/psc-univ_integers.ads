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

with PSC.Interpreter; use PSC.Interpreter;
with Ada.Unchecked_Conversion;
with Ada.Streams;
package PSC.Univ_Integers is
   --  Package to implement Univ_Integers using a 64-bit integer,
   --  but with special values for the high 32-bits that signify the
   --  value is not represented by the 64-bit integer, but is rather
   --  represented by a separate "large" integer in a table indexed
   --  by the low 32 bits.
   --  These separate large integers will be hashed so there will be
   --  bounded storage growth.
   --  The sign of the 64-bit integer will give the sign of the overall
   --  value.

   type Univ_Integer is private;

   Null_Univ_Integer : constant Univ_Integer;

   Zero : constant Univ_Integer;
   One : constant Univ_Integer;
   Two : constant Univ_Integer;
   Ten : constant Univ_Integer;
   Two_To_32 : constant Univ_Integer;

   function Two_To_64 return Univ_Integer;

   Minus_One : constant Univ_Integer;
   Minus_Two : constant Univ_Integer;

   function From_Word (Val : Word_Type) return Univ_Integer;
   --  Convert Word_Type into a Univ_Integer, handling the
   --  "special" zone which needs to be treated specially.

   function "+" (Right : Interpreter.Word_Type) return Univ_Integer
     renames From_Word;

   function Fits_In_Word (Val : Univ_Integer) return Boolean;
   --  Returns true if Val is in range of Interpreter.Word_Type;
   --  This returns True if Val = Null_Univ_Integer.

   function From_Univ_Integer (Val : Univ_Integer)
     return Interpreter.Word_Type;
   --  Convert Univ_Integer to a Word, raise exception if too big

   function To_Word_Type (Val : Univ_Integer)
     return Interpreter.Word_Type;
   --  Unchecked convert Univ_Integer to a Word; never complain.

   function "+" (Right : Univ_Integer) return Interpreter.Word_Type
     renames From_Univ_Integer;

   function From_Unsigned_Word (Val : Unsigned_Word_Type) return Univ_Integer;
   --  Convert Unsigned_Word_Type into a Univ_Integer, handling the
   --  "special" zone which needs to be treated specially.

   function To_Unsigned_Word (Val : Univ_Integer)
     return Interpreter.Unsigned_Word_Type;
   --  Convert Univ_Integer to an Unsigned_Word, wrapping around if > 2**64

   function Univ_Int_To_Float (Val : Univ_Integer)
     return Interpreter.Univ_Real;
   --  Convert Univ_Integer to a 64-bit float

   function GCD (Left, Right : Univ_Integer) return Univ_Integer;
   --  Greatest common divisor (absolute value)

   function Image (Val : Univ_Integer) return String;
   function Value (Str : String) return Univ_Integer;

   function "+" (Right : Univ_Integer) return Univ_Integer;
   function "-" (Right : Univ_Integer) return Univ_Integer;

   function "+" (Left, Right : Univ_Integer) return Univ_Integer;

   function "abs" (Right : Univ_Integer) return Univ_Integer;

   function "-" (Left, Right : Univ_Integer) return Univ_Integer;

   function "*" (Left, Right : Univ_Integer) return Univ_Integer;

   function "/" (Left, Right : Univ_Integer) return Univ_Integer;
   function "rem" (Left, Right : Univ_Integer) return Univ_Integer;
   function "mod" (Left, Right : Univ_Integer) return Univ_Integer;
   function "**" (Left : Univ_Integer;
                  Right : Natural) return Univ_Integer;

   function "<" (Left, Right : Univ_Integer) return Boolean;
   function ">" (Left, Right : Univ_Integer) return Boolean;
   function "<=" (Left, Right : Univ_Integer) return Boolean;
   function ">=" (Left, Right : Univ_Integer) return Boolean;
   --  function "=" inherited as is

   function Hex_Image
     (Val : Unsigned_Word_Type;
      Underscores_Every : Natural := 4;
      Zero_Pad : Boolean := False) return String;
   --  Produce a base-16 image of a 64-bit unsigned number

   function Hex_Image
     (Val : Univ_Integer;
      Underscores_Every : Natural := 4) return String;
   --  Produce a base-16 image of a Univ_Integer

   --  Provide some operations that match what is expected by Param_Sig

   function Fetch_Univ_Integer
     (Params : Word_Ptr; Offset : Offset_Within_Area)
     return Univ_Integer;

   function Fetch_Nonnull_Univ_Integer
     (Params : Word_Ptr; Offset : Offset_Within_Area)
     return Univ_Integer;

   procedure Store_Univ_Integer
     (Context : Exec_Context;
      Params : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Univ_Integer);
   --  Context parameter is ignored

   procedure Store_Univ_Integer
     (Addr : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Univ_Integer);

   procedure Dump_Stats;
   --  Dump statistics about Univ_Integer hash table, etc.

private

   procedure Univ_Integer_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Univ_Integer);
   for Univ_Integer'Write use Univ_Integer_Write;
   --  Write out the contents of the Univ_Integer

   procedure Univ_Integer_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Univ_Integer);
   for Univ_Integer'Read use Univ_Integer_Read;
   --  Read in the stream rep of the Univ_Integer and convert to Univ_Integer

   type Univ_Integer is new Interpreter.Word_Type;

   Null_Univ_Integer : constant Univ_Integer :=
     Univ_Integer (Interpreter.Null_Value);

   Zero : constant Univ_Integer := 0;
   One : constant Univ_Integer := 1;
   Two : constant Univ_Integer := 2;
   Ten : constant Univ_Integer := 10;
   Two_To_32 : constant Univ_Integer := Univ_Integer (2**32);

   Minus_One : constant Univ_Integer := Univ_Integer (-1);
   Minus_Two : constant Univ_Integer := Univ_Integer (-2);

   function Fetch_Univ_Integer
     (Params : Word_Ptr; Offset : Offset_Within_Area)
     return Univ_Integer renames Fetch_Word;

   function Fetch_Nonnull_Univ_Integer
     (Params : Word_Ptr; Offset : Offset_Within_Area)
     return Univ_Integer renames Fetch_Nonnull_Word;

   procedure Store_Univ_Integer
     (Addr : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Univ_Integer) renames Store_Word;

   function To_Word_Type (Val : Univ_Integer)
     return Interpreter.Word_Type is (Interpreter.Word_Type (Val));
   --  Unchecked convert Univ_Integer to a Word; never complain.

   pragma Export (Ada, "**", "_psc_univ_exp_op");

end PSC.Univ_Integers;
