------------------------------------------------------------------------------
--                  L W T . S C H E D U L E R . O P E N M P                 --
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
------------------------------------------------------------------------------

pragma Ada_2022;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Dynamic_Priorities;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C; use Interfaces;
with LWT.OpenMP; use LWT.OpenMP;

with System.Atomic_Operations.Integer_Arithmetic;
pragma Elaborate (System.Atomic_Operations.Integer_Arithmetic);
with System.Atomic_Operations.Test_And_Set;

with System.Tasking; use System.Tasking;

package body LWT.Scheduler.OpenMP is
   --  This package implements a plug-in LWT scheduler for OpenMP.

   use LWT_Groups;

   Debug_OpenMP : constant Boolean := False;

   procedure Call_LWT_Body (Data : System.Address)
     with Convention => C;

   type Taskloop_Data is record
      Start_Index, End_Index : C.long;
      Range_Group : LWT_Group_Ptr;
      Range_Data : access constant Range_Loop.Range_Loop_Data'Class;
   end record;

   type Taskloop_Data_Ptr is access all Taskloop_Data;

   procedure Call_LWT_Range_Loop_Body (Data : System.Address)
     with Convention => C;

   --------------------------------------
   -- Plugin primitive operation specs --
   --------------------------------------

   type OpenMP_Plugin_Type is
     new LWT.Scheduler.LWT_Scheduler_Plugin with null record;

   overriding
   procedure Spawn_LWT
     (Plugin : in out OpenMP_Plugin_Type;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null);
   --  Spawn a light-weight thread, with given Data on Spawning_Group (Data),
   --  and optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.

   overriding
   procedure LWT_Range_Loop
     (Plugin : in out OpenMP_Plugin_Type;
      Data : Range_Loop.Range_Loop_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null);
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

   overriding
   function Need_More_LWTs (Plugin : OpenMP_Plugin_Type) return Boolean;
   --  Indicates that we need more LWTs to keep all servers busy.

   overriding
   function Num_Servers_Available
     (Plugin : OpenMP_Plugin_Type) return LWT_Server_Index;
   --  Return number of servers available to current Ada task
   --  for serving LWTs.

   overriding
   function Ada_Task_Identity (Plugin : OpenMP_Plugin_Type)
     return Ada.Task_Identification.Task_Id;
   --  Returns the Ada Task_Id associated with the current LWT server

   overriding
   function Create_Group
     (Plugin : in out OpenMP_Plugin_Type;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr;
   --  Create a scheduler-specific LWT group

   ------------------------
   -- OpenMP Task Groups --
   ------------------------

   --  Taskgroups are not externally exposed by the GOMP API.
   --  Starting a taskgroup applies to the current OMP task,
   --  and records the new taskgoup in the OMP task, while
   --  setting the new taskgroup to point to the parent task group.
   --  When a task group is awaited, it must be the innermost task group.
   --  When a task group is canceled, if it is not the innermost task group,
   --  we need to daisy chain the cancelation, by setting a flag to
   --  propagate the cancelation until the group ID matches.

   type OMP_Task_Group is tagged;
   type OMP_Task_Group_Ptr is access all OMP_Task_Group;

   type OMP_Task_Group is new LWT_Sched_Group with record
      Canceled_Flag : aliased
        System.Atomic_Operations.Test_And_Set.Test_And_Set_Flag;
      Has_Been_Awaited : Boolean := False;
      Has_Been_Canceled : Boolean := False;
      Prev_Task_Group : OMP_Task_Group_Ptr := null;
   end record;

   overriding
   function Cancellation_Point (Group : OMP_Task_Group) return Boolean;
   --  Return true if given group has been canceled.
   --  Upon return, if result is True, return from the LWT body procedure
   --  immediately; otherwise, continue processing.

   overriding
   procedure Wait_For_Group
     (Group : in out OMP_Task_Group;
      Canceled : out Boolean);
   --  Wait for all sub-LWTs of given Group to complete
   --  Upon return, if Canceled is True the caller
   --  should perform any actions that might
   --  have been requested by the canceling LWT,
   --  presumably recorded in a variable visible to the initiator
   --  of the LWT group.

   overriding
   procedure Cancel_Group
     (Group : in out OMP_Task_Group;
      Success : out Boolean);
   --  Cancel the specified group.
   --  If Success is True on return, this was the first LWT to request
   --  cancellation, and it now has a chance to set up for some special
   --  action when the LWT group completes (e.g. return from the
   --  enclosing subprogram) by setting some variable visible to
   --  the initiator of the LWT group.
   --  In any case, upon return from this call the LWT should return
   --  from the LWT body procedure.

   overriding
   procedure Finish_Group (Group : not null access OMP_Task_Group);
   --  Finish the group; called after having awaited all of the sub-LWTs

   -----------------------------------------------
   -- OMP Task Group primitive operation bodies --
   -----------------------------------------------

   procedure Free_Group is
     new Ada.Unchecked_Deallocation (OMP_Task_Group, OMP_Task_Group_Ptr);
   --  TBD: At some point, perhaps we should use a server-local free list

   --  Task group for innermost task group active on current server
   Cur_Task_Group : OMP_Task_Group_Ptr := null
     with Thread_Local_Storage;

   overriding
   function Cancellation_Point (Group : OMP_Task_Group) return Boolean is
   --  Return true if given group has been canceled.
   --  Upon return, if result is True, return from the LWT body procedure
   --  immediately; otherwise, continue processing.
   begin
      --  NOTE: Currently we don't call the GOMP "cancellation point" API,
      --        but instead use the Group.Has_Been_Canceled flag.
      --        This enables us to support some of the cancellation
      --        semantics even when the environment variable
      --        OMP_CANCELLATION isn't set.
      return Group.Has_Been_Canceled;
   end Cancellation_Point;

   overriding
   procedure Wait_For_Group
     (Group : in out OMP_Task_Group;
      Canceled : out Boolean) is
   --  Wait for all sub-LWTs of given Group to complete
   --  Upon return, if Canceled is True the caller
   --  should perform any actions that might
   --  have been requested by the canceling LWT,
   --  presumably recorded in a variable visible to the initiator
   --  of the LWT group.

      pragma Assert (Group'Unchecked_Access = Cur_Task_Group);
      --  May only wait for innermost task group.
   begin
      if Debug_OpenMP then
         Put_Line ("Wait for Group, thread num =" & omp_get_thread_num'Image);
      end if;

      --  Wait for tasks in task group
      GOMP_taskgroup_end;

      if Debug_OpenMP then
         Put_Line ("Group is complete, thread num =" &
           omp_get_thread_num'Image);
      end if;

      --  Set flag to know that we actually awaited the group.
      Group.Has_Been_Awaited := True;

      --  Indicate whether group was canceled
      Canceled := Group.Has_Been_Canceled;

      --  Restore innermost task group pointer
      Cur_Task_Group := Group.Prev_Task_Group;

      if Cur_Task_Group /= null and then Cur_Task_Group.Has_Been_Canceled then
         --  Cancel the outer group
         declare
            Ignore : constant C.char :=
               GOMP_cancel (which => GOMP_CANCEL_TASKGROUP,
                            do_cancel => C.char'Val (1));
         begin
            null;
         end;
      end if;
   end Wait_For_Group;

   overriding
   procedure Cancel_Group
     (Group : in out OMP_Task_Group;
      Success : out Boolean) is
   --  Cancel the specified group.
   --  If Success is True on return, this was the first LWT to request
   --  cancellation, and it now has a chance to set up for some special
   --  action when the LWT group completes (e.g. return from the
   --  enclosing subprogram) by setting some variable visible to
   --  the initiator of the LWT group.
   --  In any case, upon return from this call the LWT should return
   --  from the LWT body procedure.
      use System.Atomic_Operations.Test_And_Set;
      Inner_Group : OMP_Task_Group_Ptr := Cur_Task_Group;
      use type C.int;
   begin
      if omp_get_cancellation = 0 then
         Put_Line (" Warning: OpenMP requires OMP_CANCELLATION environment");
         Put_Line ("          variable to be set to ""true"" to support");
         Put_Line ("          propagating exceptions or transfering control");
         Put_Line ("          out of a parallel construct.");
      end if;

      --  Loop canceling all nested groups until reach "Group"
      while Inner_Group /= null loop
         --  This is the first test-and-set of the canceled flag if False.
         Success := not Atomic_Test_And_Set (Inner_Group.Canceled_Flag);

         --  In any case, Inner_Group now has been canceled.
         Inner_Group.Has_Been_Canceled := True;

         if Success and then Inner_Group = Cur_Task_Group then
            --  Can only "actually" cancel the innermost group in OpenMP
            --  We will kill off the outer groups after waiting for the
            --  inner groups (see Wait_For_Group).
            declare
               Ignore : constant C.char :=
                  GOMP_cancel (which => GOMP_CANCEL_TASKGROUP,
                               do_cancel => C.char'Val (1));
            begin
               null;
            end;
         end if;

         exit when Inner_Group = Group'Unchecked_Access;
                        --  All done when we reach Group

         --  Go to next enclosing group
         Inner_Group := Inner_Group.Prev_Task_Group;
      end loop;
   end Cancel_Group;

   overriding
   procedure Finish_Group (Group : not null access OMP_Task_Group) is
   --  Finish the group; called after having awaited all of the sub-LWTs
      Writable_Copy : OMP_Task_Group_Ptr := Group.all'Unchecked_Access;
   begin
      pragma Assert (Group.Has_Been_Awaited);

      --  Nothing more to do except reclaim OMP_Task_Group structure.
      Free_Group (Writable_Copy);
   end Finish_Group;

   -----------------------------------------------
   -- Routines to call into Ada LWT_Body from C --
   -----------------------------------------------

   procedure Call_LWT_Body (Data : System.Address) is
   --  Function that simply dispatches to the LWT_Body operation.
      LWT_Data : constant Root_Data_Ptr := To_Root_Data_Ptr (Data);
      Old_Task_Group : constant OMP_Task_Group_Ptr := Cur_Task_Group;
      New_Task_Group : constant OMP_Task_Group_Ptr :=
        OMP_Task_Group_Ptr (Sched_Group (LWT_Data.Group));
   begin
      if New_Task_Group.Has_Been_Canceled then
         --  Don't perform the LWT_Body if group has been canceled.
         if Debug_OpenMP then
            Put_Line ("Call_LWT_Body skipped due to cancellation");
         end if;
         return;
      end if;

      if Debug_OpenMP then
         Put_Line ("Call_LWT_Body, thread num =" & omp_get_thread_num'Image);
      end if;

      --  Set Cur_Task_Group while executing LWT_Body
      Cur_Task_Group := New_Task_Group;

      LWT_Body (LWT_Data.all);

      --  Restore Cur_Task_Group
      Cur_Task_Group := Old_Task_Group;
   end Call_LWT_Body;

   function To_Taskloop_Data_Ptr is
     new Ada.Unchecked_Conversion
           (System.Address, Taskloop_Data_Ptr);

   procedure Call_LWT_Range_Loop_Body (Data : System.Address) is
   --  Function that simply dispatches to the LWT_Body operation
   --  from a LWT_Range_Loop operation.
      TL_Data : Taskloop_Data renames To_Taskloop_Data_Ptr (Data).all;
      Range_Data : Range_Loop.Range_Loop_Data'Class := TL_Data.Range_Data.all;
      use type C.long;
      Old_Task_Group : constant OMP_Task_Group_Ptr := Cur_Task_Group;
      New_Task_Group : constant OMP_Task_Group_Ptr :=
        OMP_Task_Group_Ptr (Sched_Group (TL_Data.Range_Group));
   begin
      if New_Task_Group.Has_Been_Canceled then
         --  Don't perform the LWT_Body if group has been canceled.
         if Debug_OpenMP then
            Put_Line
              ("Call_LWT_Range_Loop_Body skipped due to cancellation");
         end if;
         return;
      end if;

      --  Set up group info
      Range_Data.Group := TL_Data.Range_Group;

      --  Compute chunk index from start position relative to overall start
      Range_Data.Chunk_Index :=
        1 + (C.long'Pos (TL_Data.Start_Index) -
              Long_Integer'Pos (Range_Data.First)) /
          C.long'Pos (TL_Data.End_Index - TL_Data.Start_Index);

      if Debug_OpenMP then
         Put_Line ("Call_LWT_Range_Loop_Body, thread num =" &
           omp_get_thread_num'Image &
           ", Start =" & TL_Data.Start_Index'Image &
           ", End =" & TL_Data.End_Index'Image &
           ", Chunk =" & Range_Data.Chunk_Index'Image);
      end if;

      Range_Data.First := C.long'Pos (TL_Data.Start_Index);
      Range_Data.Last := C.long'Pos (TL_Data.End_Index - 1);

      --  Set Cur_Task_Group while executing LWT_Body
      Cur_Task_Group := New_Task_Group;

      --  Now make the dispatching call if not canceled
      if not New_Task_Group.Has_Been_Canceled then
         Range_Loop.LWT_Body (Range_Data);
      end if;

      --  Restore Cur_Task_Group
      Cur_Task_Group := Old_Task_Group;
   end Call_LWT_Range_Loop_Body;

   ---------------------------------------
   -- Plugin primitive operation bodies --
   ---------------------------------------

   overriding
   function Create_Group
     (Plugin : in out OpenMP_Plugin_Type;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr is
   --  Create a scheduler-specific LWT group
      Result : constant OMP_Task_Group_Ptr := new OMP_Task_Group;
                     --  NOTE: Important to allocate as OMP_Task_Group_Ptr
                     --        in case it has its own storage pool.
                     --  TBD: Check free list instead of allocating
   begin
      --  Link task group into chain of task groups
      Result.Prev_Task_Group := Cur_Task_Group;
      Cur_Task_Group := Result;

      --  Now actually start the group inside the OpenMP run-time
      GOMP_taskgroup_start;

      --  Return non-null pointer to task group structure
      return LWT_Sched_Group_Ptr (Result);
   end Create_Group;

   overriding
   procedure Spawn_LWT
     (Plugin : in out OpenMP_Plugin_Type;
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
      use type C.long;
   begin
      if OMP_Task_Group_Ptr (Sched_Group (Data.Group)).Has_Been_Canceled then
         --  Don't spawn the LWT_Body if group has been canceled.
         if Debug_OpenMP then
            Put_Line ("Spawn_LWT skipped due to cancellation");
         end if;
         return;
      end if;

      if Debug_OpenMP then
         Put_Line ("Spawn_LWT, thread num =" & omp_get_thread_num'Image);
      end if;

      GOMP_task
        (fn => Call_LWT_Body'Access,
         data => Data.all'Address,
         cpyfn => null,
         arg_size => Data.all'Size / C.long (System.Storage_Unit),
         arg_align => Data.all'Alignment,
         if_clause => C.char'Val (1),
         flags => 0,
         depend => null,
         priority => C.int (Ada.Dynamic_Priorities.Get_Priority));
   end Spawn_LWT;

   overriding
   procedure LWT_Range_Loop
     (Plugin : in out OpenMP_Plugin_Type;
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
      use type C.long;
      Group : aliased LWT_Group;
      TL_Data : Taskloop_Data :=
        (Start_Index | End_Index => 0,
         Range_Group => Group'Unchecked_Access,
         Range_Data => Data'Unchecked_Access);
      Canceled : Boolean := False;
   begin
      if Debug_OpenMP then
         Put_Line ("LWT_Range_Loop, thread num =" &
           omp_get_thread_num'Image &
           ", First = " & Data.First'Image &
           ", Last = " & Data.Last'Image);
      end if;

      --  TBD: Make use of Aspects if non-null

      GOMP_taskloop
        (fn => Call_LWT_Range_Loop_Body'Access,
         data => TL_Data'Address,
         cpyfn => null,
         arg_size => TL_Data'Size / C.long (System.Storage_Unit),
         arg_align => TL_Data'Alignment,
         flags => GOMP_TASK_FLAG_NOGROUP  --  group created externally
               or GOMP_TASK_FLAG_IF,
         num_tasks => C.unsigned_long (Data.Num_Chunks),
         priority => C.int (Ada.Dynamic_Priorities.Get_Priority),
         start_index => C.long (Data.First),
         end_index => C.long (Data.Last) + 1,
         step => 1);

      --  Wait for group explicitly to indicate normal completion
      Group.Wait_For_Group (Canceled);
   end LWT_Range_Loop;

   overriding
   function Need_More_LWTs (Plugin : OpenMP_Plugin_Type) return Boolean is
   --  Indicates that we need more LWTs to keep all servers busy.
   begin
      return True;  --  TBD: no need for more LWTs if we already have 2*servers
   end Need_More_LWTs;

   overriding
   function Num_Servers_Available
     (Plugin : OpenMP_Plugin_Type) return LWT_Server_Index is
   --  Return number of servers available to current Ada task
   --  for serving LWTs.
   begin
      return LWT_Server_Index (omp_get_num_threads);
   end Num_Servers_Available;

   Associated_Ada_Task : System.Tasking.Task_Id := null
     with Thread_Local_Storage;
   --  If not null, then this identifies the Ada Task from which
   --  this LWT server was spawned.

   function To_Task_Id is new Ada.Unchecked_Conversion
     (System.Tasking.Task_Id, Ada.Task_Identification.Task_Id);
   function To_Task_Id is new Ada.Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, System.Tasking.Task_Id);

   overriding
   function Ada_Task_Identity (Plugin : OpenMP_Plugin_Type)
     return Ada.Task_Identification.Task_Id is
   --  Returns the Ada Task_Id associated with the current LWT server
      use Ada.Task_Identification;
   begin
      --  Return the thread-local value
      if Associated_Ada_Task = null then
         --  Never set means this must be a "normal" Ada task
         return Current_Task;
      else
         --  This must be an LWT server task, which will piggy-back
         --  on the Ada task from which it was spawned
         return To_Task_Id (Associated_Ada_Task);
      end if;
   end Ada_Task_Identity;

   procedure Set_Ada_Task_Identity
     (Identity : Ada.Task_Identification.Task_Id) is
   --  Set the Ada task identity to be associated with the current LWT server
   begin
      Associated_Ada_Task := To_Task_Id (Identity);
   end Set_Ada_Task_Identity;

   ----------------------------------
   -- Install/Remove OpenMP Plugin --
   ----------------------------------

   OpenMP_Plugin : aliased OpenMP_Plugin_Type;
   --  This is the plugin object for the OpenMP LWT scheduler

   type Atomic_Int is new Integer with Atomic;

   package Int_Counters is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_Int);

   Num_Parallel_Regions : aliased Atomic_Int := 0;

   --  These two routines keep track of the number of active OpenMP
   --  parallel regions.  While there is at least one active, we establish
   --  OpenMP as the LWT scheduler.  These are called as a side-effect
   --  of initializing/finalizing an Interfaces.OpenMP.OMP_Parallel object.
   --  It is implementation-defined whether there is one LWT scheduler
   --  plug-in per Ada task, or one per program partition.
   procedure Incr_OpenMP_Parallel_Regions is
      use Int_Counters;
      Pre_Value : constant Atomic_Int :=
        Atomic_Fetch_And_Add (Num_Parallel_Regions, 1);
   begin
      if Pre_Value = 0 then
         --  Time to install the scheduler
         LWT.Scheduler.Install_Scheduler_Plugin (OpenMP_Plugin'Access);
      end if;
   end Incr_OpenMP_Parallel_Regions;

   procedure Decr_OpenMP_Parallel_Regions is
      use Int_Counters;
      Pre_Value : constant Atomic_Int :=
        Atomic_Fetch_And_Subtract (Num_Parallel_Regions, 1);
   begin
      if Pre_Value = 1 then
         --  Time to remove the scheduler
         LWT.Scheduler.Remove_Scheduler_Plugin (OpenMP_Plugin'Access);
      end if;
   end Decr_OpenMP_Parallel_Regions;

end LWT.Scheduler.OpenMP;
