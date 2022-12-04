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

with System.Storage_Elements;
package body PSC.Interpreter.Param_Signatures is
   --  A generic signature package for parameters to builtin operations,
   --  and some particular instances of it for Word_Type and Univ_Real.

   procedure Store_Real
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Univ_Real) is
   --  Store value into Base[Offset]
   --  NOTE: Context parameter is ignored, but is needed to match
   --        signature of Param_Sig generic.
   begin
      Store_Word (Context, Base, Offset, From_Univ_Real (Value));
   end Store_Real;

   procedure Store_Word
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Type)
   is
      pragma Unreferenced (Context);
   begin
      Add (Base, Offset).all := Value;
   end Store_Word;

   function Fetch_Unsigned
          (Params : Word_Ptr; Offset : Offset_Within_Area)
           return Unsigned_Word_Type is
   --  Fetch value from Params[Offset]
   begin
      return To_Unsigned_Word (Fetch_Word (Params, Offset));
   end Fetch_Unsigned;

   procedure Store_Unsigned
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Unsigned_Word_Type) is
   --  Store value into Base[Offset]
   --  NOTE: Context parameter is ignored, but is needed to match
   --        signature of Param_Sig generic.
      pragma Unreferenced (Context);
   begin
      Store_Word (Base, Offset, From_Unsigned_Word (Value));
   end Store_Unsigned;

   procedure Store_Unsigned
     (Addr : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Unsigned_Word_Type) is
   --  Version to match Store_To_Phys_Addr signature
   begin
      Store_Word (Addr, Offset, From_Unsigned_Word (Value));
   end Store_Unsigned;

   --  Functions for fetching/storing System.Address
   function Fetch_Input
     (Params : Word_Ptr; Offset : Offset_Within_Area)
      return System.Address is
      use System.Storage_Elements;
   begin
      return To_Address (Integer_Address (Fetch_Word (Params, Offset)));
   end Fetch_Input;

   procedure Store_Output
       (Context : Exec_Context;
        Params : Word_Ptr;
        Offset : Offset_Within_Area;
        Value : System.Address) is
      use System.Storage_Elements;
   begin
      Store_Word (Params, Offset, Word_Type (To_Integer (Value)));
   end Store_Output;

   function Fetch_From_Phys_Addr
       (Addr : Word_Ptr;
        Offset : Offset_Within_Area)
        return System.Address is
      use System.Storage_Elements;
   begin
      return To_Address (Integer_Address (Fetch_Word (Addr, Offset)));
   end Fetch_From_Phys_Addr;

   procedure Store_To_Phys_Addr
       (Addr : Word_Ptr;
        Offset : Offset_Within_Area;
        Value : System.Address) is
      use System.Storage_Elements;
   begin
      Store_Word (Addr, Offset, Word_Type (To_Integer (Value)));
   end Store_To_Phys_Addr;

   procedure Store_Word_Ptr
          (Context : Exec_Context;
           Params : Word_Ptr;
           Offset : Offset_Within_Area;
           Value : Word_Ptr) is
   --  Procedure for storing a Word_Ptr into the param area
   begin
      Store_Word (Context, Params, Offset, Word_Ptr_To_Word (Value));
   end Store_Word_Ptr;

end PSC.Interpreter.Param_Signatures;
