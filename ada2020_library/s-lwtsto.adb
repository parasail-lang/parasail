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

with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System.Storage_Elements;  use System.Storage_Elements;
with System.LWT.Statistics;

package body System.LWT.Storage is

   Debug_Storage : constant Boolean :=
     System.LWT.Statistics.Debug_Storage_Stats;

   --  TBD: False sharing likely with these arrays;
   --       would be better to have single array of records.
   Storage_Reused : array (LWT_Server_Index) of Natural := (others => 0);
   Storage_Alloced : array (LWT_Server_Index) of Natural := (others => 0);

   Default_Data_Size : constant Natural :=
     Root_Data'Size + Integer_Address'Size * 8;
         --  Max bits for per-LWT data to be easily reused.

   Default_Storage_Size : constant Storage_Count :=
     Storage_Count (Default_Data_Size / System.Storage_Unit);

   type Univ_Data;

   type Univ_Data_Ptr is access Univ_Data;
   pragma No_Strict_Aliasing (Univ_Data_Ptr);

   type Univ_Data is record
      --  Type used for free list
      Next : Univ_Data_Ptr;
      Data : Storage_Array (1 .. Default_Storage_Size);
   end record
     with Alignment => Standard'Maximum_Alignment;

   Univ_Data_Storage_Size : constant Storage_Count :=
     Univ_Data'Size / System.Storage_Unit;

   function To_Univ_Data_Ptr is new
     Ada.Unchecked_Conversion (System.Address, Univ_Data_Ptr);

   Free_List : Univ_Data_Ptr := null
     with Thread_Local_Storage;

   procedure Allocate
     (Pool : in out LWT_Storage_Pool;
      Storage_Address : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count) is
      Server_Index : constant LWT_Server_Index :=
        (if Debug_Storage then Cur_Server_Index
         else LWT_Server_Index'First);
      Free_Item : constant Univ_Data_Ptr := Free_List;
   begin
      if Size_In_Storage_Elements <= Univ_Data_Storage_Size
        and then Free_Item /= null
      then
         --  Use item from free list
         Free_List := Free_Item.Next;
         if Debug_Storage then
            Storage_Reused (Server_Index) := Storage_Reused (Server_Index) + 1;
         end if;
         Free_Item.Next := null;
         Storage_Address := Free_Item.all'Address;
      else
         --  Pass the buck to standard storage pool, with size
         --  rounded up.
         System.Storage_Pools.Allocate
           (Univ_Data_Ptr'Storage_Pool, Storage_Address,
            Storage_Count'Max
              (Size_In_Storage_Elements, Univ_Data_Storage_Size),
            Alignment);
         if Debug_Storage then
            Storage_Alloced (Server_Index) :=
              Storage_Alloced (Server_Index) + 1;
         end if;
      end if;
   end Allocate;

   procedure Deallocate
     (Pool : in out LWT_Storage_Pool;
      Storage_Address : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment : System.Storage_Elements.Storage_Count) is
   begin
      if Size_In_Storage_Elements <= Univ_Data_Storage_Size then
         --  Keep track of free items on a per-server free list
         declare
            Unused_Server_Index : constant LWT_Server_Index :=
              Cur_Server_Index;
            --  Cur_Server_Index has side effects.

            Free_Item : constant Univ_Data_Ptr :=
              To_Univ_Data_Ptr (Storage_Address);

         begin
            Free_Item.Next := Free_List;
            Free_List := Free_Item;
         end;
      else
         --  Use normal deallocation routine
         System.Storage_Pools.Deallocate
           (Univ_Data_Ptr'Storage_Pool, Storage_Address,
            Size_In_Storage_Elements, Alignment);
      end if;
   end Deallocate;

   function Storage_Size (Pool : LWT_Storage_Pool)
     return System.Storage_Elements.Storage_Count is
   begin
      --  Pass the buck to the language-provided storage pool
      return System.Storage_Pools.Storage_Size (Univ_Data_Ptr'Storage_Pool);
   end Storage_Size;

   procedure Show_Storage_Stats is
   --  Display total number of LWT data records allocated, and number of
   --  times a LWT data record was reused.
      Total_Storage_Reused, Total_Storage_Alloced : Natural := 0;
      use Ada.Text_IO;
   begin
      if not Debug_Storage then
         --  Nothing to show
         return;
      end if;
      Put_Line ("Storage statistics:");
      for I in 1 .. Max_Server_Index loop
         Total_Storage_Reused := Total_Storage_Reused + Storage_Reused (I);
         Total_Storage_Alloced := Total_Storage_Alloced + Storage_Alloced (I);
      end loop;
      Put_Line (" Total Storage Alloc'ed =" &
        Natural'Image (Total_Storage_Alloced));
      Put_Line (" Total Storage Reused =" &
        Natural'Image (Total_Storage_Reused));
      New_Line;
   end Show_Storage_Stats;

end System.LWT.Storage;
