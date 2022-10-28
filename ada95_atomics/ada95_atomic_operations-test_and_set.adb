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
with Ada95_Atomic_Operations.Exchange;
pragma Elaborate (Ada95_Atomic_Operations.Exchange);
package body Ada95_Atomic_Operations.Test_And_Set is

   package Exchange is
     new Ada95_Atomic_Operations.Exchange (Test_And_Set_Type);

   function Atomic_Test_And_Set
     (Item : access Test_And_Set_Flag) return Boolean is
      New_Val : constant Test_And_Set_Type := -1;
   begin
      --  Implement using atomic exchange
      --  TBD: Could use the C primitive test-and-set instead.
      return Exchange.Atomic_Exchange (Item.Flag'Access, New_Val) /= 0;
   end Atomic_Test_And_Set;

   procedure Atomic_Clear
     (Item : access Test_And_Set_Flag) is
   begin
      Item.Flag := 0;
   end Atomic_Clear;

   function Is_Lock_Free
     (Item : Test_And_Set_Flag) return Boolean is
   begin
      return True;  --  TBD
   end Is_Lock_Free;

end Ada95_Atomic_Operations.Test_And_Set;
