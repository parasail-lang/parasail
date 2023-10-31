------------------------------------------------------------------------------
--                          L W T . S C H E D U L E R                       --
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

--  Package defining an interface to a light-weight scheduler for Ada 202X
pragma Ada_2022;

with System.Atomic_Operations.Integer_Arithmetic;

package body LWT.Scheduler is
   --  A light-weight thread scheduler

   use LWT_Groups;

   Sequential_Plugin : aliased LWT_Scheduler_Plugin;
   --  This is the plugin that provides a default sequential model.

   Cur_Plugin : not null LWT_Scheduler_Ptr := Sequential_Plugin'Access;
   --  This pointer is used to reach the scheduler-specific routines.
   --  This could be put into thread-local storage if we wanted to permit
   --  multiple light-weight schedulers in the same partition.

   ----------------
   -- LWT Groups --
   ----------------

   function Create_Group
     (Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr is
   --  Create a LWT group specific to current plugin
   begin
      return Cur_Plugin.Create_Group (Aspects);
   end Create_Group;

   package body LWT_Groups is
      --  Put LWT_Group and its operations into a sub-package
      --  to avoid complaints about operations dispatching on multiple
      --  tagged types.

      function Cancellation_Point (Group : aliased LWT_Group) return Boolean is
      --  Return true if given group has been canceled.
      --  Upon return, if result is True, return from the LWT body procedure
      --  immediately; otherwise, continue processing.
      begin
         --  Just pass the buck to underlying group.
         return Group.Sched_Group.Cancellation_Point;
      end Cancellation_Point;

      procedure Wait_For_Group
        (Group : aliased in out LWT_Group;
         Canceled : out Boolean) is
      --  Wait for all sub-LWTs of given Group to complete
      --  Upon return, if Canceled is True the caller
      --  should perform any actions that might
      --  have been requested by the canceling LWT,
      --  presumably recorded in a variable visible to the initiator
      --  of the LWT group.
      begin
         --  Just pass the buck to underlying group.
         Group.Sched_Group.Wait_For_Group (Canceled);

         --  Remember that Wait_For_Group returned successfully.
         Group.Has_Been_Awaited := True;
      end Wait_For_Group;

      procedure Cancel_Group
        (Group : aliased in out LWT_Group;
         Success : out Boolean) is
      --  Cancel the specified group.
      --  If Success is True on return, this was the first LWT to request
      --  cancellation, and it now has a chance to set up for some special
      --  action when the LWT group completes (e.g. return from the
      --  enclosing subprogram) by setting some variable visible to
      --  the initiator of the LWT group.
      --  In any case, upon return from this call the LWT should return
      --  from the LWT body procedure.
      begin
         Group.Sched_Group.Cancel_Group (Success);
      end Cancel_Group;

      procedure Finalize (Group : in out LWT_Group) is
      --  If the group has been awaited, just finish the LWT_Sched_Group.
      --  If not, cancel the group, await the group, and then finish it.
         Success : Boolean;
         Canceled : Boolean;
      begin
         if not Group.Has_Been_Awaited then
            begin
               --  Cancel group because Wait_For_Group was bypassed
               Group.Sched_Group.Cancel_Group (Success);
               Group.Sched_Group.Wait_For_Group (Canceled);
               Group.Has_Been_Awaited := True;
            exception
               when others =>
                  --  Some exception propagated; we still want to finish group.
                  Group.Sched_Group.Finish_Group;
                  raise;
            end;
         end if;
         --  Now finish the group
         Group.Sched_Group.Finish_Group;
      end Finalize;

      function Sched_Group (Group : LWT_Group_Ptr) return LWT_Sched_Group_Ptr
      --  Return LWT-scheduler-specific group associated with Group.
      is
      begin
         return Group.Sched_Group;
      end Sched_Group;

   end LWT_Groups;

   --------------------
   -- LW Thread Data --
   --------------------

   procedure Spawn_LWT
     (Group : aliased in out LWT_Group;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null) is
   --  Spawn a light-weight thread, on given Group, with given Data
   --  and optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.
   begin
      --  Fill in the group info
      Data.Group := Group'Unchecked_Access;

      --  Now pass the buck to the underlying LWT scheduler unless canceled
      if not Group.Cancellation_Point then
         Cur_Plugin.Spawn_LWT (Data, Aspects);
      end if;
   end Spawn_LWT;

   function Spawning_Group (Data : Root_Data'Class) return LWT_Group_Ptr is
   --  Group in which the LWT was spawned.
   begin
      return Data.Group;
   end Spawning_Group;

   procedure LWT_Range_Loop
     (Data : Range_Loop.Range_Loop_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null) is
   --  Execute a light-weight-threading-based "range" loop,
   --  given optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.
   --  An LWT group is created for the LWTs spawned, and awaited
   --  before returning from LWT_Range_Loop.
   begin
      --  Pass the buck to the underlying LWT scheduler
      Cur_Plugin.LWT_Range_Loop (Data, Aspects);
   end LWT_Range_Loop;

   function Need_More_LWTs return Boolean is
   --  Indicates that we need more LWTs to keep all servers busy.
   begin
      --  Just pass the buck to the underlying LWT scheduler
      return Cur_Plugin.Need_More_LWTs;
   end Need_More_LWTs;

   function Num_Servers_Available return LWT_Server_Index is
   --  Return number of servers available to current Ada task
   --  for serving LWTs.
   begin
      --  Just pass the buck to the underlying LWT scheduler
      return Cur_Plugin.Num_Servers_Available;
   end Num_Servers_Available;

   --------------------
   -- Server Indices --
   --------------------

   type Atomic_Server_Index is
     range LWT_Server_Index_Base'First .. LWT_Server_Index_Base'Last
     with Atomic;

   package Inc_Server_Index is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_Server_Index);

   Cur_Server_Index_Obj : LWT_Server_Index_Base := 0
     with Thread_Local_Storage;  --  This is 0 until asked for

   Max_Server_Index_Obj : aliased Atomic_Server_Index := 1;
   --  Count of all servers whose server-index has ever been requested.
   --  Environment task is assigned server-index 1 at elaboration time.

   function Cur_Server_Index return LWT_Server_Index_Base is
   --  Return the unique server index of the task currently executing.
   --  It assigns an index if the caller does not currently have
   --  such an index assigned to it.
      Result : LWT_Server_Index_Base := Cur_Server_Index_Obj;
   begin
      if Result = 0 then
         --  Has never been assigned
         declare
            use Inc_Server_Index;

            Pre_Value : constant Atomic_Server_Index :=
              Atomic_Fetch_And_Add (Max_Server_Index_Obj, 1);
         begin
            Result := LWT_Server_Index_Base (Pre_Value) + 1;
            Cur_Server_Index_Obj := Result;
         end;
      end if;
      return Result;
   end Cur_Server_Index;

   function Max_Server_Index return LWT_Server_Index is
   --  Returns the server index of the last server assigned an index
   begin
      return LWT_Server_Index_Base (Max_Server_Index_Obj);
   end Max_Server_Index;

   function Ada_Task_Identity return Ada.Task_Identification.Task_Id is
   --  Returns the Ada Task_Id associated with the current LWT server
   begin
      --  Just pass the buck to the current plugin
      return Cur_Plugin.Ada_Task_Identity;
   end Ada_Task_Identity;

   ---------------------------
   -- Install/Remove Plugin --
   ---------------------------

   protected Protected_Plugin is
      --  Make sure plugin is updated atomically
      procedure Install (Scheduler : not null LWT_Scheduler_Ptr);

      procedure Remove (Scheduler : not null LWT_Scheduler_Ptr);
   end Protected_Plugin;

   protected body Protected_Plugin is
      procedure Install (Scheduler : not null LWT_Scheduler_Ptr) is
      begin
         if Cur_Plugin /= Sequential_Plugin'Access then
            --  Fail if not currently the Sequential plugin
            raise Program_Error with "Duplicate LWT scheduler";
         end if;
         Cur_Plugin := Scheduler;
      end Install;

      procedure Remove (Scheduler : not null LWT_Scheduler_Ptr) is
      begin
         if Cur_Plugin /= Scheduler then
            --  Fail if trying to remove wrong plugin
            raise Program_Error with "Mismatch when removing LWT scheduler";
         end if;
         Cur_Plugin := Sequential_Plugin'Access;
      end Remove;
   end Protected_Plugin;

   procedure Install_Scheduler_Plugin (Scheduler : not null LWT_Scheduler_Ptr)
   --  It is an error if there is already a scheduler other than the default.
   --  It is implementation-defined whether the scheduler is specific to
   --  an Ada task, or is partition-wide.
   is
   begin
      Protected_Plugin.Install (Scheduler);
   end Install_Scheduler_Plugin;

   procedure Remove_Scheduler_Plugin (Scheduler : not null LWT_Scheduler_Ptr)
   --  It is an error if the specified scheduler does not match the
   --  current scheduler.  After such a call, we revert to the sequential
   --  scheduler.
   --  It is implementation-defined whether the scheduler is specific to
   --  an Ada task, or is partition-wide.
   is
   begin
      Protected_Plugin.Remove (Scheduler);
   end Remove_Scheduler_Plugin;

   ----------------------------
   -- Sequential "Scheduler" --
   ----------------------------

   procedure Spawn_LWT
     (Plugin : in out LWT_Scheduler_Plugin;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null) is
   --  Spawn a light-weight thread, with given Data on Spawning_Group (Data),
   --  and optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.
   begin
      --  Just execute the function and return
      LWT_Body (Data.all);
   end Spawn_LWT;

   procedure LWT_Range_Loop
     (Plugin : in out LWT_Scheduler_Plugin;
      Data : Range_Loop.Range_Loop_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null) is
   --  Execute a light-weight-threading-based "range" loop,
   --  given optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.
   --  An LWT group is created for the LWTs spawned, and awaited
   --  before returning from LWT_Range_Loop.
      Canceled : Boolean := False;
      Group : aliased LWT_Group;
      Loop_Data : Range_Loop.Range_Loop_Data'Class := Data;
   begin
      Loop_Data.Group := Group'Unchecked_Access;
      --  Just execute the body with full range
      Range_Loop.LWT_Body (Loop_Data);

      --  Clean up the group
      Wait_For_Group (Group, Canceled);
   end LWT_Range_Loop;

   function Need_More_LWTs (Plugin : LWT_Scheduler_Plugin) return Boolean is
   --  Indicates that we need more LWTs to keep all servers busy.
   begin
      --  We never need more LWTs when executing sequentially
      return False;
   end Need_More_LWTs;

   function Num_Servers_Available
     (Plugin : LWT_Scheduler_Plugin) return LWT_Server_Index is
   --  Return number of servers available to current Ada task
   --  for serving LWTs.
   begin
      --  Only one server available for serving LWTs.
      return 1;
   end Num_Servers_Available;

   function Ada_Task_Identity (Plugin : LWT_Scheduler_Plugin)
     return Ada.Task_Identification.Task_Id is
   --  Returns the Ada Task_Id associated with the current LWT server
   begin
      --  Just call the "normal" Current_Task function
      return Ada.Task_Identification.Current_Task;
   end Ada_Task_Identity;

   ---------------------------------
   -- Sequential Scheduler Groups --
   ---------------------------------

   type LWT_Sequential_Group is new LWT_Sched_Group with record
      Canceled : Boolean := False;
   end record;

   overriding
   function Cancellation_Point (Group : LWT_Sequential_Group) return Boolean
     is (Group.Canceled);

   overriding
   procedure Wait_For_Group
     (Group : in out LWT_Sequential_Group;
      Canceled : out Boolean);

   overriding
   procedure Cancel_Group
     (Group : in out LWT_Sequential_Group;
      Success : out Boolean);

   overriding
   procedure Finish_Group
     (Group : not null access LWT_Sequential_Group);

   overriding
   procedure Wait_For_Group
     (Group : in out LWT_Sequential_Group;
      Canceled : out Boolean) is
   begin
      Canceled := Group.Canceled;
   end Wait_For_Group;

   overriding
   procedure Cancel_Group
     (Group : in out LWT_Sequential_Group;
      Success : out Boolean) is
   begin
      Success := not Group.Canceled;
      Group.Canceled := True;
   end Cancel_Group;

   function Create_Group
     (Plugin : in out LWT_Scheduler_Plugin;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr is
   --  Create a scheduler-specific LWT group for sequential "scheduler"
   begin
      return new LWT_Sequential_Group;
   end Create_Group;

   overriding
   procedure Finish_Group
     (Group : not null access LWT_Sequential_Group) is
   begin
      null;  --  TBD: Reclaim space for group
   end Finish_Group;

begin
   --  Assign server index 1 to the environment task
   Cur_Server_Index_Obj := 1;
end LWT.Scheduler;
