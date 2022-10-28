------------------------------------------------------------------------------
--                     A T O M I C  O P E R A T I O N S                     --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
generic
   type Atomic_Type is private; --  TBD: with Atomic;
package Ada95_Atomic_Operations.Exchange is
   pragma Preelaborate (Ada95_Atomic_Operations.Exchange);

   function Atomic_Exchange (Item  : access Atomic_Type;
                             Value : Atomic_Type) return Atomic_Type;
   pragma Convention (Intrinsic, Atomic_Exchange);

   function Atomic_Compare_And_Exchange (Item    : access Atomic_Type;
                                         Prior   : access Atomic_Type;
                                         Desired : Atomic_Type) return Boolean;
   pragma Convention (Intrinsic, Atomic_Compare_And_Exchange);

   function Is_Lock_Free (Item : Atomic_Type) return Boolean;
   pragma Convention (Intrinsic, Is_Lock_Free);

end Ada95_Atomic_Operations.Exchange;
