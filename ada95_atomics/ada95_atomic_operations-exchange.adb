------------------------------------------------------------------------------
--                     A T O M I C  O P E R A T I O N S                     --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
with Interfaces.C;
use Interfaces;
package body Ada95_Atomic_Operations.Exchange is

   use type C.size_t;

   procedure C_Atomic_Exchange (Size : C.Size_T;
     M_Ptr, V_Ptr, R_Ptr : System.Address;
     S_Model : Mem_Model);
   pragma Import (C, C_Atomic_Exchange, "__atomic_exchange");
   --  "Generic" version of atomic exchange.
   --  TBD: Call size-specific versions by using a "case" statement.

   function Atomic_Exchange (Item  : access Atomic_Type;
                             Value : Atomic_Type) return Atomic_Type is
      Value_Copy : aliased Atomic_Type := Value;
      Result : aliased Atomic_Type;
   begin
      C_Atomic_Exchange (Size => Value_Copy'Size / System.Storage_Unit,
        M_Ptr => Item.all'Address,
        V_Ptr => Value_Copy'Address,
        R_Ptr => Result'Address,
        S_Model => Seq_Cst);
      return Result;
   end Atomic_Exchange;

   function C_Atomic_Compare_Exchange
     (Size : C.Size_T;
      M_Ptr, E_Ptr, D_Ptr : System.Address;
      S_Model : Mem_Model; F_Model : Mem_Model) return C.unsigned_char;
   pragma Import
     (C, C_Atomic_Compare_Exchange, "__atomic_compare_exchange");
   --  "Generic" version of atomic exchange.
   --  TBD: Call size-specific versions by using a "case" statement.
   --  TBD2: Should add a "bool" to Interfaces.C since C99 supports it.

   function Atomic_Compare_And_Exchange (Item    : access Atomic_Type;
                                         Prior   : access Atomic_Type;
                                         Desired : Atomic_Type) return Boolean
   is
      Desired_Copy : aliased Atomic_Type := Desired;
      Result : C.unsigned_char;
      use type C.unsigned_char;
   begin
      Result := C_Atomic_Compare_Exchange
        (Size => Desired_Copy'Size / System.Storage_Unit,
         M_Ptr => Item.all'Address,
         E_Ptr => Prior.all'Address,
         D_Ptr => Desired_Copy'Address,
         S_Model => Seq_Cst,
         F_Model => Seq_Cst);
      return C.unsigned_char'Pos (Result) /= 0;
   end Atomic_Compare_And_Exchange;

   function Is_Lock_Free (Item : Atomic_Type) return Boolean is
   begin
      return True;  --  TBD
   end Is_Lock_Free;

end Ada95_Atomic_Operations.Exchange;
