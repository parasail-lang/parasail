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
------------------------------------------------------------------------------
generic
   type Atomic_Type is mod <>; -- TBD: with Atomic;
package System_Atomic_Operations.Unsigned_Arithmetic
  with Pure is

   procedure Atomic_Add (Item  : aliased in out Atomic_Type;
                         Value : Atomic_Type)
     with Convention => Intrinsic;

   procedure Atomic_Subtract (Item  : aliased in out Atomic_Type;
                              Value : Atomic_Type)
     with Convention => Intrinsic;

   function Atomic_Fetch_And_Add
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
     with Convention => Intrinsic;

   function Atomic_Fetch_And_Subtract
     (Item  : aliased in out Atomic_Type;
      Value : Atomic_Type) return Atomic_Type
     with Convention => Intrinsic;

   function Is_Lock_Free (Item : aliased Atomic_Type) return Boolean
     with Convention => Intrinsic;

end System_Atomic_Operations.Unsigned_Arithmetic;
