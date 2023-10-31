------------------------------------------------------------------------------
--                G e n e r i c _ A t o m i c _ M a x _ M i n               --
--                                                                          --
--                     Copyright (C) 2012-2023, AdaCore                     --
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
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------
pragma Ada_2022;

with System.Atomic_Operations.Exchange;
pragma Elaborate (System.Atomic_Operations.Exchange);

package body LWT.Generic_Atomic_Max_Min is
   package Exchanges is new System.Atomic_Operations.Exchange (Atomic_Type);

   procedure Update_Max
     (Max : aliased in out Atomic_Type; New_Val : Atomic_Type) is
   --  Update Max to the Atomic_Type'Max of Max'Old and New_Val.
      Ignore : constant Atomic_Type := Update_Max (Max, New_Val);
   begin
      null;
   end Update_Max;

   function Update_Max
     (Max : aliased in out Atomic_Type; New_Val : Atomic_Type)
     return Atomic_Type is
   --  Update Max to the Atomic_Type'Max of Max'Old and New_Val,
   --  and return prior value of Max.
      Old_Max : aliased Atomic_Type := Max;
   begin
      loop
         exit when Exchanges.Atomic_Compare_And_Exchange
           (Item => Max, Prior => Old_Max,
            Desired => Atomic_Type'Max (Old_Max, New_Val));
      end loop;
      return Old_Max;
   end Update_Max;

   procedure Update_Min
     (Min : aliased in out Atomic_Type; New_Val : Atomic_Type) is
   --  Update Min to the Atomic_Type'Min of Min'Old and New_Val.
      Ignore : constant Atomic_Type := Update_Min (Min, New_Val);
   begin
      null;
   end Update_Min;

   function Update_Min
     (Min : aliased in out Atomic_Type; New_Val : Atomic_Type)
     return Atomic_Type is
   --  Update Min to the Atomic_Type'Min of Min'Old and New_Val,
   --  and return prior value of Min.
      Old_Min : aliased Atomic_Type := Min;
   begin
      loop
         exit when Exchanges.Atomic_Compare_And_Exchange
           (Item => Min, Prior => Old_Min,
            Desired => Atomic_Type'Min (Old_Min, New_Val));
      end loop;
      return Old_Min;
   end Update_Min;

end LWT.Generic_Atomic_Max_Min;
