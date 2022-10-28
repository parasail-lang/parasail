------------------------------------------------------------------------------
--                              L W T Scheduler                             --
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
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

--  Storage manager for reducing use of heap in LWT threading

with System.Storage_Elements;
with System.Storage_Pools;
package System.LWT.Storage is

   type LWT_Storage_Pool is new System.Storage_Pools.Root_Storage_Pool
     with null record;
      --  Free lists are kept as globals

   procedure Allocate
     (Pool : in out LWT_Storage_Pool;
      Storage_Address : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool : in out LWT_Storage_Pool;
      Storage_Address : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : LWT_Storage_Pool)
     return System.Storage_Elements.Storage_Count;

   LWT_Storage_Pool_Obj : LWT_Storage_Pool;
   --  This storage pool is internally divided by server.
   --  It must only be used when System.LWT.Cur_Server_Index returns > 0.

   procedure Show_Storage_Stats;
   --  Display total number of LWT data records allocated, and number of
   --  times a LWT data record was reused.
end System.LWT.Storage;
