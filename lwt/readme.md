
# Light-Weight Threading Library for Ada 2022

This is a light-weight threading library that provides many of
the features defined for Ada 2022, without depending on support
for the new "parallel" loop or block syntax.

The ParaSail interpreter/compiler has its own version of this
library built into the code in interpreter/psc-interpreter.adb.
At some point we will shift it to use the code in this "lwt" library.

This library is organized as follows:

- lwt.ads   LWT root package
- lwt-scheduler.ads  LWT Scheduler, the primary API for using light-weight
threading
- lwt-openmp.ads  OpenMP interface.  Declaring an object of type OMP_Parallel
indicates that the current task (if declared in a task body) or the environment task (if declared in the main subprogram) wants OpenMP to manage the light-weight threads.
- lwt-work_stealing.ads   Work Stealing Scheduler interface.  Declaring an object of type WS_Parallel indicates
- lwt-work_stealing.ads  Work-Stealing Scheduler interface.  Declaring an object of type WS_Parallel
indicates that the current task (if declared in a task body) or the environment task (if declared in the main subprogram) wants the Work-Stealing scheduler to manage the light-weight threads.
- lwt-parallelism.ads   Some useful generic packages for creating parallel loops.
- lwt-vector_par_iterator.ads   Provides an implementation of the Parallel_Iterator_Interface'Class for Ada.Containers.Vectors
- lwt-hashed_map_par_iterator.ads   Provides an implementation of the Parallel_Iterator_Interface'Class for Ada.Containers.Hashed_Maps
- examples/*.adb  Several examples of using the LWT library, making use of both the OpenMP and the Work-Stealing schedulers.  "gprbuild *.adb" in the examples directory will build all of the executables and put them in the "obj" subdirectory.

Here is the spec for the LWT.Scheduler package:
```ada
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
      . . .
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

   . . .  -- discussed in Plugin section
end LWT.Scheduler;
```

## Scheduler Plugin

## Installing from Source

Todo

### Building on a Unix-like system

Todo

### Building on Windows

Todo

#### MinGW

?

#### Specifying an ABI

?

### Configure and Make

Todo

## Building Documentation

Todo

## Notes

Todo

## Getting Help

The ParaSail community congregates in a few places:

* [Stack Overflow] - Direct questions about using the language.
* [Gitter] - General discussion and broader questions.

[Stack Overflow]: https://stackoverflow.com/questions/tagged/parasail
[Gitter]: https://gitter.im/parasail-lang/community


## Contributing

If you are interested in contributing to the ParaSail project, please take a look
at the [Getting Started][getting_started] section of the parasail-dev-guide.



## License

ParaSail is primarily distributed under the terms of the GPL 3 Copyright,
with the special exception to permit incorporating the ParaSail library
without limitations.
