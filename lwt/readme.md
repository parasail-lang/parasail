
# Light-Weight Threading Library for Ada 2022

This is a light-weight threading library that provides many of
the features defined for Ada 2022, without depending on support
for the new "parallel" loop or block syntax.

The ParaSail interpreter/compiler has its own version of this
library built into the code in interpreter/psc-interpreter.adb.
At some point we will shift it to use the code in this "lwt" library.

This library is organized as follows:

| file | Description |
| --- | --- |
| lwt.ads | LWT root package
| lwt-scheduler.ads  | LWT Scheduler, the primary API for using light-weight threading
| lwt-openmp.ads  | OpenMP interface.  Declaring an object of type OMP_Parallel indicates that the current task (if declared in a task body) or the environment task (if declared in the main subprogram) wants OpenMP to manage the light-weight threads.
| lwt-work_stealing.ads  | Work-Stealing Scheduler interface.  Declaring an object of type WS_Parallel indicates that the current task (if declared in a task body) or the environment task (if declared in the main subprogram) wants the Work-Stealing scheduler to manage the light-weight threads.
| lwt-parallelism.ads   | Some useful generic packages for creating parallel loops, plus a package Parallel_Iterator_Interfaces which provides the parallel iteration capability added to the Ada.Iterator_Interfaces by Ada 2022.
| lwt-vector_par_iterator.ads   | Provides an implementation of the Parallel_Iterator'Class for Ada.Containers.Vectors
| lwt-hashed_map_par_iterator.ads   | Provides an implementation of the Parallel_Iterator'Class for Ada.Containers.Hashed_Maps
| examples/*.adb  | Several examples of using the LWT library, making use of both the OpenMP and the Work-Stealing schedulers.  `gprbuild *.adb` in the examples directory will build all of the executables and put them in the "obj" subdirectory.
| common.gpr and examples/prj.gpr | Project files for building lwt library and examples
| lwt-scheduler-*.ad{s,b} | Plug-in schedulers
| other lwt-*.ad{s,b} | Other aspects of implementation of the various interfaces.

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
   . . .
end LWT.Scheduler;
```

## Scheduler Plugins

There are two scheduler plug-ins, plus a default sequential scheduler that
handles the light weight threads (by executing them in sequence!) in the absence of any other scheduler.

### OpenMP Scheduler Plug-in

The files lwt-scheduler-openmp.ad{s,b} performs light-weight scheduling by using the GNU OpenMP implementation.  Simply by mentioning "LWT.OpenMP" in a "with clause" and declaring a local object of type
LWT.OpenMP.OMP_Parallel you can specify the maximum number of "heavy-weight" server threads to be devoted to the Ada task in which the declaration occurs.
Here is the declaration of the LWT.OpenMP package:
```ada
package LWT.OpenMP is

   --  Types specific to the OpenMP LWT scheduler plug-in.

   type OMP_Parallel (Num_Threads : Natural := 0) is limited private;
   --  Declaring an instance of this type in a declare block is equivalent
   --  to a #pragma omp parallel followed by a #pragma omp single,
   --  with the end of the declare block corresponding to the end
   --  of the C structured-block of the omp parallel directive.
   --  This will also establish OpenMP as the plug-in for LWT scheduling.

   type OMP_Taskloop is new LWT.Aspects.Root_Aspect with record
      Grainsize : Natural := 0;
      Collapse : Positive := 1;
   end record;
   --  This record type can be used in the optional aspect specification
   --  associated with a parallel loop, to specify the grainsize and/or
   --  the nested-loop collapse count.
   --  Note that the num_tasks parameter to the OpenMP taskloop comes
   --  from the chunk specification, if any.
   --  This is ignored if it is used on a parallel loop that is not in
   --  the context of an OMP_Parallel region.
   --  e.g.:
   --         parallel (Num_Chunks) with OMP_Taskloop (Collapse => 2)
   --         for I in 1 .. 1000 loop
   --            for J in 1 .. 2000 loop
   --               ...
   --            end loop;
   --         end loop;

   ----------------
   -- OpenMP API --
   ----------------

   procedure omp_set_num_threads (num_threads : C.int)
     with Import, Convention => C, External_Name => "omp_set_num_threads";

   function omp_get_num_threads return C.int
     with Import, Convention => C, External_Name => "omp_get_num_threads";

   function omp_get_max_threads return C.int
     with Import, Convention => C, External_Name => "omp_get_max_threads";

   function omp_get_thread_num return C.int
     with Import, Convention => C, External_Name => "omp_get_thread_num";

   function omp_get_num_procs return C.int
     with Import, Convention => C, External_Name => "omp_get_num_procs";

   function omp_in_parallel return C.int
     with Import, Convention => C, External_Name => "omp_in_parallel";

   procedure omp_set_dynamic (num_threads : C.int)
     with Import, Convention => C, External_Name => "omp_set_dynamic";

   function omp_get_dynamic return C.int
     with Import, Convention => C, External_Name => "omp_get_dynamic";

   function omp_get_cancellation return C.int
     with Import, Convention => C, External_Name => "omp_get_cancellation";

   procedure omp_set_nested (num_threads : C.int)
     with Import, Convention => C, External_Name => "omp_set_nested";

   function omp_get_nested return C.int
     with Import, Convention => C, External_Name => "omp_get_nested";

   --  void omp_set_schedule(omp_sched_t kind, int chunk_size);
   --  void omp_get_schedule(omp_sched_t *kind, int *chunk_size);

   function omp_get_thread_limit return C.int
     with Import, Convention => C, External_Name => "omp_get_thread_limit";

   function omp_get_supported_active_levels return C.int
     with Import, Convention => C,
          External_Name => "omp_get_supported_active_levels";

   procedure omp_set_max_active_levels (num_threads : C.int)
     with Import, Convention => C,
          External_Name => "omp_set_max_active_levels";

   function omp_get_max_active_levels return C.int
     with Import, Convention => C,
          External_Name => "omp_get_max_active_levels";

   function omp_get_level return C.int
     with Import, Convention => C, External_Name => "omp_get_level";

   function omp_get_ancestor_thread_num (level : C.int) return C.int
     with Import, Convention => C,
          External_Name => "omp_get_ancestor_thread_num";

   function omp_get_team_size (level : C.int) return C.int
     with Import, Convention => C, External_Name => "omp_get_team_size";

   function omp_get_active_level return C.int
     with Import, Convention => C, External_Name => "omp_get_active_level";

   function omp_in_final return C.int
     with Import, Convention => C, External_Name => "omp_in_final";

   --  omp_proc_bind_t omp_get_proc_bind(void);

   function omp_get_num_places return C.int
     with Import, Convention => C, External_Name => "omp_get_num_places";

   --  int omp_get_place_num_procs(int place_num);
   --  void omp_get_place_proc_ids(int place_num, int *ids);

   function omp_get_place_num return C.int
     with Import, Convention => C, External_Name => "omp_get_place_num";

   function omp_get_partition_num_places return C.int
     with Import, Convention => C,
          External_Name => "omp_get_partition_num_places";

   --  void omp_get_partition_place_nums(int *place_nums);
   --  void omp_set_affinity_format(const char *format);
   --  size_t omp_get_affinity_format(char *buffer, size_t size);
   --  void omp_display_affinity(const char *format);
   --  size_t omp_capture_affinity(
   --    char *buffer,
   --    size_t size,
   --    const char *format);

   procedure omp_set_default_device (num_threads : C.int)
     with Import, Convention => C,
          External_Name => "omp_set_default_device";

   function omp_get_default_device return C.int
     with Import, Convention => C,
          External_Name => "omp_get_default_device";

   function omp_get_num_devices return C.int
     with Import, Convention => C, External_Name => "omp_get_num_devices";

   function omp_get_device_num return C.int
     with Import, Convention => C, External_Name => "omp_get_device_num";

   function omp_get_num_teams return C.int
     with Import, Convention => C, External_Name => "omp_get_num_teams";

   function omp_get_team_num return C.int
     with Import, Convention => C, External_Name => "omp_get_team_num";

   function omp_is_initial_device return C.int
     with Import, Convention => C, External_Name => "omp_is_initial_device";

   function omp_get_initial_device return C.int
     with Import, Convention => C,
          External_Name => "omp_get_initial_device";

   function omp_get_max_task_priority return C.int
     with Import, Convention => C,
          External_Name => "omp_get_max_task_priority";

   --  int omp_pause_resource(
   --    omp_pause_resource_t kind,
   --    int device_num);
   --  int omp_pause_resource_all(omp_pause_resource_t kind);

   type fn_ptr is access procedure (Data : System.Address)
     with Convention => C;
   type cpyfn_ptr is access procedure (From, To : System.Address)
     with Convention => C;
   type depend_ptr is access all C.int
     with Convention => C;
   type depend_ptr_ptr is access all depend_ptr
     with Convention => C;

   procedure GOMP_taskgroup_start
     with Import, Convention => C, External_Name => "GOMP_taskgroup_start";

   procedure GOMP_taskgroup_end
     with Import, Convention => C, External_Name => "GOMP_taskgroup_end";

   --  GOMP_task/GOMP_taskloop* flags argument.
   type GOMP_task_flag_type is new C.unsigned;
   GOMP_TASK_FLAG_UNTIED    : constant GOMP_task_flag_type := 2 ** 0;
   GOMP_TASK_FLAG_FINAL     : constant GOMP_task_flag_type := 2 ** 1;
   GOMP_TASK_FLAG_MERGEABLE : constant GOMP_task_flag_type := 2 ** 2;
   GOMP_TASK_FLAG_DEPEND    : constant GOMP_task_flag_type := 2 ** 3;
   GOMP_TASK_FLAG_PRIORITY  : constant GOMP_task_flag_type := 2 ** 4;
   GOMP_TASK_FLAG_UP        : constant GOMP_task_flag_type := 2 ** 8;
   GOMP_TASK_FLAG_GRAINSIZE : constant GOMP_task_flag_type := 2 ** 9;
   GOMP_TASK_FLAG_IF        : constant GOMP_task_flag_type := 2 ** 10;
   GOMP_TASK_FLAG_NOGROUP   : constant GOMP_task_flag_type := 2 ** 11;

   procedure GOMP_task
     (fn : fn_ptr; data : System.Address; cpyfn : cpyfn_ptr;
      arg_size : C.long; arg_align : C.long; if_clause : C.char;
      flags : GOMP_task_flag_type; depend : depend_ptr_ptr; priority : C.int)
     with Import, Convention => C, External_Name => "GOMP_task";

   procedure GOMP_taskloop
     (fn : fn_ptr; data : System.Address; cpyfn : cpyfn_ptr;
      arg_size : C.long; arg_align : C.long;
      flags : GOMP_task_flag_type; num_tasks : C.unsigned_long;
      priority : C.int; start_index, end_index, step : C.long)
     with Import, Convention => C, External_Name => "GOMP_taskloop";

   type unsigned_long_long is mod 2 ** Long_Long_Integer'Size;

   procedure GOMP_taskloop_ull
     (fn : fn_ptr; data : System.Address; cpyfn : cpyfn_ptr;
      arg_size : C.long; arg_align : C.long;
      flags : C.unsigned; num_tasks : C.unsigned_long; priority : C.int;
      start_index, end_index, step : unsigned_long_long)
     with Import, Convention => C, External_Name => "GOMP_taskloop_ull";

   procedure GOMP_parallel_start
     (fn : fn_ptr; data : System.Address; num_threads : C.unsigned)
     with Import, Convention => C, External_Name => "GOMP_parallel_start";

   procedure GOMP_parallel_end
     with Import, Convention => C, External_Name => "GOMP_parallel_end";

   procedure GOMP_barrier
     with Import, Convention => C, External_Name => "GOMP_barrier";

   type GOMP_cancel_kind is new C.int;

   GOMP_CANCEL_PARALLEL : constant GOMP_cancel_kind := 1;
   GOMP_CANCEL_LOOP : constant GOMP_cancel_kind := 2;
   GOMP_CANCEL_FOR : constant GOMP_cancel_kind := GOMP_CANCEL_LOOP;
   GOMP_CANCEL_DO : constant GOMP_cancel_kind := GOMP_CANCEL_LOOP;
   GOMP_CANCEL_SECTIONS : constant GOMP_cancel_kind := 4;
   GOMP_CANCEL_TASKGROUP : constant GOMP_cancel_kind := 8;

   --  function GOMP_cancellation_point (which : GOMP_cancel_kind)
   --    return C.char
   --    with Import, Convention => C,
   --         External_Name => "GOMP_cancellation_point";
   --  NOTE: Currently we don't call this, but instead use
   --        the Group.Has_Been_Canceled flag.

   function GOMP_cancel
     (which : GOMP_cancel_kind; do_cancel : C.char) return C.char
     with Import, Convention => C, External_Name => "GOMP_cancel";

private
   . . .
end LWT.OpenMP;
```
Note that this package includes a relatively complete interface to OpenMP features, though in general, for portability, you should avoid calling them directly.
Declaring an object of type OMP_Parallel and specifying Num_Threads you will automatically be using OpenMP for managing any of the light-weight threads you crreate with the various other capabilities of the LWT library.

### Work-Stealing Scheduler Plug-in

As an alternative to OpenMP, we provide a work-stealing-based scheduler plug-in,
written entirely in Ada, which in many circumstances is more
efficient than OpenMP.
The plugin is in lwt-scheduler-work_stealing.ad{s,b}.  To activate it,
mention "LWT.Work_Stealing" in a with clause and then declare an object
of type WS_Parallel in your main subprogram or task body.

Here is the package spec for LWT.Work_Stealing:

```ada
package LWT.Work_Stealing is
   --  Types specific to the work-stealing-based LWT scheduler plug-in.

   type WS_Options is new LWT.Aspects.Root_Aspect with record
      Num_LWTs_Needed_Per_Server : Natural := 0;
      --  Number of LWTs needed per active server to allow
      --  work stealing to work efficiently.
      --  By default it will be Num_Servers + 1
      Max_Steal_Iterations : Natural := 0;
      --  Number of times we try to steal a LWT
      --  By default it will be Num_Servers
      Min_LWTs_Before_Stealing : Positive := 1;
      --  A server should have at least this many LWTs before
      --  we steal from it; default is one.
      Enough_LWTs_Before_Stealing : Natural := 0;
      --  A server that has this many LWTs is always OK to steal from.
      --  By default it will be Num_Servers
      Debug_Statistics : Boolean := False;
      --  Whether to collect LW threading statistics
      Debug_Storage_Stats : Boolean := False;
      --  Whether to collect storage statistics
   end record;
   --  This record type can be used in the optional aspect specification
   --  associated with a parallel loop, to specify various work-stealing
   --  options.  These override the task-wide defaults during the
   --  execution of the parallel loop.
   --  This is ignored if it is used on a parallel loop that is not in
   --  the context of an WS_Parallel region.
   --  e.g.:
   --         parallel (Num_Chunks) with WS_Options (Debug_Statistics)
   --         for I in 1 .. 1000 loop
   --            for J in 1 .. 2000 loop
   --               ...
   --            end loop;
   --         end loop;

   --  This will establish Work_Stealing as the plug-in for LWT scheduling.
   --  Declaring an instance of this type in a declare block causes
   --  the specified number of servers (or some default number of servers
   --  if Num_Servers is zero) to be used for scheduling LW threads
   --  using work-stealing, up through the end of the declare block.
   --  This also allows the user to specify additional tuning parameters.
   type WS_Parallel
     (Num_Servers : Natural := 0;
      --  Number of servers to devote to this task's work-stealing;
      --  by default, will use the number of physical cores as a guide.
      Options : access WS_Options := null) is limited private;

private
   . . .
end LWT.Work_Stealing;
```

As you can see, the WS_Parallel type has two discriminants, the first
sets a limit on the number of server threads to devote to the current task
or environment task (depending on where you declare the WS_Parallel object).
The second discriminant is a possibly null reference to a set of options to
control the behavior of the work-stealing scheduler.  There is no need
to specify anything for Options, so:
```ada
   Control : WS_Parallel (Num_Servers => 6, Options => null);
```
is a reasonable way to set up the main program or task to have at
most six server threads devoted to managing its light-weight threads.

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
with the special exception to permit incorporating into your code components of the ParaSail library without limitations.
