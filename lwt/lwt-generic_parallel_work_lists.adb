------------------------------------------------------------------------------
--                     Generic_Parallel_Work_Lists                          --
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

with Ada.Unchecked_Deallocation;
with LWT.Statistics; use LWT.Statistics;
with LWT.Scheduler;
with LWT.Storage;
package body LWT.Generic_Parallel_Work_Lists is
   --  This generic package supports a parallel procedural iterator
   --  that calls a loop body multiple times, potentially in parallel
   --  (limited to Num_Chunks logical threads running in parallel at a time)
   --  providing a work item which the loop body can process and possibly
   --  generate new work items, which can be added onto the work list.
   --  The Max_Chunks parameter to the Loop_Body indicates
   --  the maximum number of threads that the Loop_Body should itself
   --  spawn, to avoid creating too many logical threads.

   procedure Par_Iterate_Work_List
     (Initial_Item : Work_Item; Num_Chunks : Positive;
      Loop_Body : not null access procedure
        (Item : Work_Item;          --  Item to be processed.
         Max_Chunks : Positive;     --  Max new threads to spawn.
         Add_Work_Item : not null access procedure (New_Item : Work_Item))) is
                                    --  Routine to call to add new work item;
                                    --  may be called from any thread.
      --  with Parallel_Iterator;   --  Might call Loop_Body in parallel.

      Group : aliased LWT.Scheduler.LWT_Group;
      --  All spawned threads will use this group

      type LWT_Data_Extension is new LWT.Scheduler.Root_Data with record
         --  Extend the root data type with Work_Item
         Item : Work_Item;
      end record;

      overriding
      procedure LWT_Body (Extended_Data : LWT_Data_Extension);

      overriding
      procedure Reclaim_Storage
        (Extended_Data : access LWT_Data_Extension;
         Server_Index : LWT.Scheduler.LWT_Server_Index);

      type LWT_Data_Ptr is access all LWT_Data_Extension
        with Storage_Pool => LWT.Storage.LWT_Storage_Pool_Obj;
         --  Local access type needed because extension is local.
         --  Use global storage pool designed for re-using LWT data.

      procedure Free is new Ada.Unchecked_Deallocation
        (LWT_Data_Extension, LWT_Data_Ptr);
         --  This will call the deallocate routine for LWT_Storage_Pool

      procedure Add_Work_Item (New_Item : Work_Item);
      procedure Add_Work_Item (New_Item : Work_Item) is
      --  Either execute the Loop_Body on the given Item,
      --  or spawn a new LWT to do that, depending on whether
      --  current server has enough LWTs on its queue.
      --  Also arrange for any following LWTs to be executed as well.
      --  If Do_First is True, then first LWT in sequence is not spawned.
      begin
         if Num_Chunks = 1
           or else
             (not LWT.Scheduler.Need_More_LWTs
                and then Recursion_Level < Recursion_Max)
         then
            --  No need to spawn a new LWT
            Recursion_Level := Recursion_Level + 1;

            Num_Bypassed_LWT_Initiations :=
              Num_Bypassed_LWT_Initiations + 1;

            --  Invoke loop body directly
            Loop_Body (New_Item,
              Max_Chunks => 1,  --  TBD: Use something more intelligent
              Add_Work_Item => Add_Work_Item'Access);

            Recursion_Level := Recursion_Level - 1;

         else

            --  We need more LWTs, so spawn a LWT which will
            --  then spawn any following LWTs as well as doing the
            --  work for one LWT directly.
            LWT.Scheduler.Spawn_LWT (Group, LWT_Data_Ptr'
              (new LWT_Data_Extension'
                 (LWT.Scheduler.Root_Data with New_Item)));
         end if;
      end Add_Work_Item;

      procedure LWT_Body (Extended_Data : LWT_Data_Extension) is
         --  Invoke the user-provided Loop_Body on the Item within
         --  the Extended_Data
      begin
         Loop_Body
           (Extended_Data.Item,
              Max_Chunks => Num_Chunks,  --  TBD: This should get smaller
              Add_Work_Item => Add_Work_Item'Access);
      end LWT_Body;

      procedure Reclaim_Storage
        (Extended_Data : access LWT_Data_Extension;
         Server_Index : LWT.Scheduler.LWT_Server_Index) is
      --  Reclaim storage at end of LWT's life
         Ptr : LWT_Data_Ptr := LWT_Data_Ptr (Extended_Data);
      begin
         Free (Ptr);
      end Reclaim_Storage;

      Canceled : Boolean;
   begin  --  Par_Iterate_Work_List

      --  Invoke Loop_Body on initial item, just passing along Num_Chunks
      Loop_Body (Initial_Item, Num_Chunks, Add_Work_Item'Access);

      Group.Wait_For_Group (Canceled);
         --  Wait for LWTs to complete
   end Par_Iterate_Work_List;

end LWT.Generic_Parallel_Work_Lists;
