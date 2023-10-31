------------------------------------------------------------------------------
--                G e n e r i c _ A t o m i c _ M a x _ M i n               --
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
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

pragma Ada_2022;

generic
   type Atomic_Base_Type is (<>);  --  TBD: with Atomic;
package LWT.Generic_Atomic_Max_Min is
   type Atomic_Type is new Atomic_Base_Type with Atomic;
   procedure Update_Max
     (Max : aliased in out Atomic_Type; New_Val : Atomic_Type);
   --  Update Max to the Atomic_Type'Max of Max'Old and New_Val.

   function Update_Max
     (Max : aliased in out Atomic_Type; New_Val : Atomic_Type)
     return Atomic_Type;
   --  Update Max to the Atomic_Type'Max of Max'Old and New_Val,
   --  and return prior value of Max.

   procedure Update_Min
     (Min : aliased in out Atomic_Type; New_Val : Atomic_Type);
   --  Update Min to the Atomic_Type'Min of Min'Old and New_Val.

   function Update_Min
     (Min : aliased in out Atomic_Type; New_Val : Atomic_Type)
     return Atomic_Type;
   --  Update Min to the Atomic_Type'Min of Min'Old and New_Val,
   --  and return prior value of Min.

end LWT.Generic_Atomic_Max_Min;
