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

with System;
with PSC.Interpreter.Param_Sig;
package PSC.Interpreter.Param_Signatures is
   pragma Elaborate_Body;
   --  Instances of a generic signature package for parameters to
   --  builtin operations, for Word_Type, Univ_Real, System.Address,
   --  and a Large_Obj.

   procedure Store_Word
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Type);
   --  Store value into Base[Offset]
   --  NOTE: Context parameter is ignored, but is needed to match
   --        signature of Param_Sig generic.

   --  Param Signature for Word_Type
   package Word_Param is new Param_Sig
     (Param_Type => Word_Type,
      Fetch_Input => Fetch_Word,
      Store_Output => Store_Word,
      Fetch_From_Phys_Addr => Fetch_Word,
      Store_To_Phys_Addr => Store_Word);

   --  Param Signature for non-null Word_Type
   package Nonnull_Word_Param is new Param_Sig
     (Param_Type => Word_Type,
      Fetch_Input => Fetch_Nonnull_Word,
      Store_Output => Store_Word,
      Fetch_From_Phys_Addr => Fetch_Word,
      Store_To_Phys_Addr => Store_Word);  --  TBD: Create a Store_Nonnull_Word?

   function Fetch_Unsigned
     (Params : Word_Ptr; Offset : Offset_Within_Area)
      return Unsigned_Word_Type;
   --  Fetch value from Params[Offset]

   procedure Store_Unsigned
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Unsigned_Word_Type);
   --  Store value into Base[Offset]
   --  NOTE: Context parameter is ignored, but is needed to match
   --        signature of Param_Sig generic.

   procedure Store_Unsigned
     (Addr : Word_Ptr;
      Offset : Offset_Within_Area;
      Value : Unsigned_Word_Type);
   --  Version to match Store_To_Phys_Addr signature

   --  Param Signature for Unsigned_Word_Type
   package Unsigned_Word_Param is new Param_Sig
     (Param_Type => Unsigned_Word_Type,
      Fetch_Input => Fetch_Unsigned,
      Store_Output => Store_Unsigned,
      Fetch_From_Phys_Addr => Fetch_Unsigned,
      Store_To_Phys_Addr => Store_Unsigned);

   procedure Store_Real
     (Context : Exec_Context;
      Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Univ_Real);
   --  Store value into Base[Offset]
   --  NOTE: Context parameter is ignored, but is needed to match
   --        signature of Param_Sig generic.

   --  Param Signature for Univ_Real
   package Real_Param is new Param_Sig
     (Param_Type => Univ_Real,
      Fetch_Input => Fetch_Real,
      Store_Output => Store_Real,
      Fetch_From_Phys_Addr => Fetch_Real,
      Store_To_Phys_Addr => Store_Real);

   --  Param Signature for non-null Univ_Real
   package Nonnull_Real_Param is new Param_Sig
     (Param_Type => Univ_Real,
      Fetch_Input => Fetch_Nonnull_Real,
      Store_Output => Store_Real,
      Fetch_From_Phys_Addr => Fetch_Nonnull_Real,
      Store_To_Phys_Addr => Store_Real);

   --  Functions for fetching/storing System.Address
   function Fetch_Input
       (Params : Word_Ptr;
        Offset : Offset_Within_Area)
        return System.Address;

   procedure Store_Output
       (Context : Exec_Context;
        Params : Word_Ptr;
        Offset : Offset_Within_Area;
        Value : System.Address);

   function Fetch_From_Phys_Addr
       (Addr : Word_Ptr;
        Offset : Offset_Within_Area)
        return System.Address;

   procedure Store_To_Phys_Addr
       (Addr : Word_Ptr;
        Offset : Offset_Within_Area;
        Value : System.Address);

   --  Param Signature for System.Address
   package Addr_Param is new Param_Sig
     (Param_Type => System.Address,
      Fetch_Input => Fetch_Input,
      Store_Output => Store_Output,
      Fetch_From_Phys_Addr => Fetch_From_Phys_Addr,
      Store_To_Phys_Addr => Store_To_Phys_Addr);

   --  Procedure for storing a Word_Ptr into the param area
   procedure Store_Word_Ptr
          (Context : Exec_Context;
           Params : Word_Ptr;
           Offset : Offset_Within_Area;
           Value : Word_Ptr);

   --  Param Signature for Word_Ptr
   package Word_Ptr_Param is new Param_Sig
     (Param_Type => Word_Ptr,
      Fetch_Input => Fetch_Word_Ptr,
      Store_Output => Store_Word_Ptr,
      Fetch_From_Phys_Addr => Fetch_Word_Ptr,
      Store_To_Phys_Addr => Store_Word_Ptr);
end PSC.Interpreter.Param_Signatures;
