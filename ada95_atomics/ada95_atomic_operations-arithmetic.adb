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
package body Ada95_Atomic_Operations.Arithmetic is

   package Exchange is new Ada95_Atomic_Operations.Exchange (Atomic_Type);

   procedure Atomic_Add (Item  : access Atomic_Type;
                         Value : Atomic_Type) is
      Ignore : constant Atomic_Type := Atomic_Fetch_And_Add (Item, Value);
   begin
      return;
   end Atomic_Add;

   procedure Atomic_Subtract (Item  : access Atomic_Type;
                              Value : Atomic_Type) is
      Ignore : constant Atomic_Type := Atomic_Fetch_And_Subtract (Item, Value);
   begin
      return;
   end Atomic_Subtract;

   function Atomic_Fetch_And_Add
     (Item  : access Atomic_Type;
      Value : Atomic_Type) return Atomic_Type is

      Old_Value : aliased Atomic_Type := Item.all;
      New_Value : Atomic_Type := Old_Value + Value;
      use Exchange;
   begin
      --  Keep iterating until the exchange succeeds
      while not Atomic_Compare_And_Exchange
        (Item, Old_Value'Access, New_Value)
      loop
         New_Value := Old_Value + Value;
      end loop;
      return Old_Value;
   end Atomic_Fetch_And_Add;

   function Atomic_Fetch_And_Subtract
     (Item  : access Atomic_Type;
      Value : Atomic_Type) return Atomic_Type is

      Old_Value : aliased Atomic_Type := Item.all;
      New_Value : Atomic_Type := Old_Value - Value;
      use Exchange;
   begin
      --  Keep iterating until the exchange succeeds
      while not Atomic_Compare_And_Exchange
        (Item, Old_Value'Access, New_Value)
      loop
         New_Value := Old_Value - Value;
      end loop;
      return Old_Value;
   end Atomic_Fetch_And_Subtract;

   function Is_Lock_Free (Item : Atomic_Type) return Boolean is
   begin
      return True;  --  TBD
   end Is_Lock_Free;

end Ada95_Atomic_Operations.Arithmetic;
