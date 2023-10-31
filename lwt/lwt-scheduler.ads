------------------------------------------------------------------------------
--                        L W T . S C H E D U L E R                         --
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

with Ada.Unchecked_Conversion;
with LWT.Aspects;
with Ada.Task_Identification;
with System;

private with Ada.Finalization;

package LWT.Scheduler is
   --  A light-weight thread scheduler

   --  Unique indices for heavyweight server LWTs
   Max_LWT_Servers : constant := 5_000;

   type LWT_Server_Index_Base is range 0 .. 2 * Max_LWT_Servers;
   subtype LWT_Server_Index is LWT_Server_Index_Base
     range 1 .. Max_LWT_Servers;
   --  Unique index to identify an LWT server. Each LWT server has its own
   --  "local" LWT queue but can steal from other LWT queues.
   --  NOTE: We need to be able to represent 2 * Max_LWT because we
   --        use a wrap-around computation when deciding which server to
   --        steal from.

   ----------------
   -- LWT Groups --
   ----------------

   type LWT_Sched_Group_Ptr is private;
   --  A pointer to a scheduler-specific group

   function Create_Group
     (Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr;
   --  Create a LWT group specific to current plugin

   package LWT_Groups is
      --  Put LWT_Group and its operations into a sub-package
      --  to avoid complaints about operations dispatching on multiple
      --  tagged types.

      type LWT_Group
        (Aspects : access LWT.Aspects.Root_Aspect'Class := null)
         is tagged limited private;
         --  Type used to keep track of set of light-weight threads
         --  all finishing at the same point.
         --  LWT_Group is a controlled type that is just a wrapper
         --  around a pointer to a scheduler-specific group structure.
         --  Upon initialization, the underlying group structure is created.
         --  Upon finalization, if Wait_For_Group has not been called,
         --  the group will be canceled and awaited.  In any case,
         --  the underlying group structure will be finalized.

      type LWT_Group_Ptr is access all LWT_Group;

      function Cancellation_Point (Group : aliased LWT_Group) return Boolean;
      --  Return true if given group has been canceled.
      --  Upon return, if result is True, return from the LWT body procedure
      --  immediately; otherwise, continue processing.

      procedure Wait_For_Group
        (Group : aliased in out LWT_Group;
         Canceled : out Boolean);
      --  Wait for all sub-LWTs of given Group to complete
      --  Upon return, if Canceled is True the caller
      --  should perform any actions that might
      --  have been requested by the canceling LWT,
      --  presumably recorded in a variable visible to the initiator
      --  of the LWT group.

      procedure Cancel_Group
        (Group : aliased in out LWT_Group;
         Success : out Boolean);
      --  Cancel the specified group.
      --  If Success is True on return, this was the first LWT to request
      --  cancellation, and it now has a chance to set up for some special
      --  action when the LWT group completes (e.g. return from the
      --  enclosing subprogram) by setting some variable visible to
      --  the initiator of the LWT group.
      --  In any case, upon return from this call the LWT should return
      --  from the LWT body procedure.

      function Sched_Group (Group : LWT_Group_Ptr) return LWT_Sched_Group_Ptr;
      --  Return LWT-scheduler-specific group associated with Group.

   private
      type LWT_Group
        (Aspects : access LWT.Aspects.Root_Aspect'Class := null)
         is new Ada.Finalization.Limited_Controlled with
      record
         --  Wrapper type used to keep track of set of light-weight threads
         --  all finishing at the same point.
         --  LWT_Group is a controlled type.  Upon finalization,
         --  if Wait_For_Group has not been called, the members of
         --  the group will be canceled if possible and then awaited.
         Sched_Group : LWT_Sched_Group_Ptr := Create_Group (Aspects);
         Has_Been_Awaited : Boolean := False;
      end record;

      procedure Initialize (Group : in out LWT_Group) is null;
      --  The LWT_Sched_Group is filled in by default initialization

      procedure Finalize (Group : in out LWT_Group);
      --  If the group has been awaited, just finish the LWT_Sched_Group.
      --  If not, cancel the group, await the group, and then finish it.

   end LWT_Groups;

   subtype LWT_Group is LWT_Groups.LWT_Group;

   --------------------
   -- LW Thread Data --
   --------------------

   type LWT_Sched_Data is abstract tagged limited null record;
   --  This is per-LWT data that can be used by the LWT Scheduler

   type LWT_Sched_Data_Ptr is access all LWT_Sched_Data'Class;

   type Root_Data is abstract tagged record
      --  This type is extended to provide additional data
      --  for use by the LWT_Body procedure.
      Group      : LWT_Groups.LWT_Group_Ptr;
      Sched_Data : LWT_Sched_Data_Ptr;
   end record;

   type Root_Data_Ptr is access all Root_Data'Class;

   function To_Root_Data_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Root_Data_Ptr);

   function Spawning_Group
     (Data : Root_Data'Class) return LWT_Groups.LWT_Group_Ptr;
   --  Group in which the LWT was spawned.

   procedure LWT_Body (Data : Root_Data) is abstract;
   --  Override this to define the body for a LW thread

   procedure Reclaim_Storage (Data : access Root_Data;
                              Server_Index : LWT_Server_Index) is null;
   --  Override to provide a routine to reclaim storage designated by Data.
   --  Caller of Reclaim_Storage will make no further use of Data
   --  after the call.
   --  Server_Index identifies the server that is reclaiming the space.

   procedure Spawn_LWT
     (Group : aliased in out LWT_Group;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null);
   --  Spawn a light-weight thread, on given Group, with given Data
   --  and optional aspect specification.
   --  "Actual" access type of Data must outlive call,
   --  since it will be added to a list hanging off the Group.
   --  But it can't be a type declared in this package because the
   --  tagged type extension presumably occurs locally to the place
   --  where LWT_Body is to be defined, and LWT_Body will generally
   --  need up-level access to data surrounding the parallel construct.

   package Range_Loop is
      --  Extend type for use with LWT_Range_Loop operation.
      --  NOTE: This is in a sub-package to reduce ambiguity with
      --        primitive operations of Root_Data.
      type Range_Loop_Data is abstract new Root_Data with record
         Num_Chunks : Natural := 0;   --  Maximum number of chunks
         First, Last : Long_Integer;  --  Bounds of range for parallel loop
         Chunk_Index : Positive;      --  Chunk index, starting at 1
      end record;
   end Range_Loop;

   procedure LWT_Range_Loop
     (Data : Range_Loop.Range_Loop_Data'Class;
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

   function Need_More_LWTs return Boolean;
   --  Indicates that we need more LWTs to keep all servers busy.

   function Num_Servers_Available return LWT_Server_Index;
   --  Return number of servers available to current Ada task
   --  for serving LWTs.

   function Cur_Server_Index return LWT_Server_Index_Base;
   --  Return the unique server index of the task currently executing.
   --  It assigns an index if the caller does not currently have
   --  such an index assigned to it.

   function Max_Server_Index return LWT_Server_Index;
   --  Returns the server index of the last server assigned an index

   function Ada_Task_Identity return Ada.Task_Identification.Task_Id;
   --  Returns the Ada Task_Id associated with the current LWT server

private

   --------------------------
   -- LWT Scheduler Plugin --
   --------------------------

   type LWT_Scheduler_Plugin is tagged limited null record;
   --  This type's primitives defines the interface to the LWT Scheduler
   --  This type is *not* abstract because the default definitions of
   --  these operations provide a trivial sequential "scheduler."

   type LWT_Scheduler_Ptr is access all LWT_Scheduler_Plugin'Class;
   --  Must provide a non-null pointer to LWT scheduler object whose
   --  operations implement the various scheduling operations.

   procedure Install_Scheduler_Plugin (Scheduler : not null LWT_Scheduler_Ptr);
   --  It is an error if there is already a scheduler other than the default.
   --  It is implementation-defined whether the scheduler is specific to
   --  an Ada task, or is partition-wide.

   procedure Remove_Scheduler_Plugin (Scheduler : not null LWT_Scheduler_Ptr);
   --  It is an error if the specified scheduler does not match the
   --  current scheduler.  After such a call, we revert to the sequential
   --  scheduler.
   --  It is implementation-defined whether the scheduler is specific to
   --  an Ada task, or is partition-wide.

   procedure Spawn_LWT
     (Plugin : in out LWT_Scheduler_Plugin;
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

   procedure LWT_Range_Loop
     (Plugin : in out LWT_Scheduler_Plugin;
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

   function Need_More_LWTs (Plugin : LWT_Scheduler_Plugin) return Boolean;
   --  Indicates that we need more LWTs to keep all servers busy.

   function Num_Servers_Available
     (Plugin : LWT_Scheduler_Plugin) return LWT_Server_Index;
   --  Return number of servers available to current Ada task
   --  for serving LWTs.

   function Ada_Task_Identity (Plugin : LWT_Scheduler_Plugin)
     return Ada.Task_Identification.Task_Id;
   --  Returns the Ada Task_Id associated with the current LWT server

   -----------------------------------
   -- Scheduler-specific LWT Groups --
   -----------------------------------

   type LWT_Sched_Group is abstract tagged limited null record;
   --  LWT Scheduler extends this to represent its LWT-group structure,
   --  and defines the operations that manipulate groups,
   --  called from the corresponding procedures of the LWT_Group type.

   type LWT_Sched_Group_Ptr is not null access all LWT_Sched_Group'Class;

   function Create_Group
     (Plugin : in out LWT_Scheduler_Plugin;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr;
   --  Create a scheduler-specific LWT group

   function Cancellation_Point (Group : LWT_Sched_Group) return Boolean
     is abstract;
   --  Return true if given group has been canceled.
   --  Upon return, if result is True, return from the LWT body procedure
   --  immediately; otherwise, continue processing.

   procedure Wait_For_Group
     (Group : in out LWT_Sched_Group;
      Canceled : out Boolean) is abstract;
   --  Wait for all sub-LWTs of given Group to complete
   --  Upon return, if Canceled is True the caller
   --  should perform any actions that might
   --  have been requested by the canceling LWT,
   --  presumably recorded in a variable visible to the initiator
   --  of the LWT group.

   procedure Cancel_Group
     (Group : in out LWT_Sched_Group;
      Success : out Boolean) is abstract;
   --  Cancel the specified group.
   --  If Success is True on return, this was the first LWT to request
   --  cancellation, and it now has a chance to set up for some special
   --  action when the LWT group completes (e.g. return from the
   --  enclosing subprogram) by setting some variable visible to
   --  the initiator of the LWT group.
   --  In any case, upon return from this call the LWT should return
   --  from the LWT body procedure.

   procedure Finish_Group
     (Group : not null access LWT_Sched_Group) is abstract;
   --  Finish the group; called after having awaited all of the sub-LWTs

end LWT.Scheduler;
