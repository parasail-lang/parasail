------------------------------------------------------------------------------
--                    I N T E R F A C E S . O P E N M P                     --
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
------------------------------------------------------------------------------

with System;

with LWT.Aspects;

with Interfaces.C; use Interfaces;

private with Ada.Finalization;
private with Ada.Task_Identification;

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

   type OMP_Parallel_Data is record
      Prio                : C.int := 0;
      Associated_Ada_Task : aliased Ada.Task_Identification.Task_Id;
   end record;
   --  This type is used for the non-master threads spawned as a result
   --  of the OpenMP parallel directive.  All these threads do
   --  is set their (Ada) priority to the given value, remember the
   --  associated Ada task identity, and suspend
   --  waiting to be restarted when there are OpenMP tasks to serve.

   type OMP_Parallel (Num_Threads : Natural := 0) is
     new Ada.Finalization.Limited_Controlled with
   record
      Data : aliased OMP_Parallel_Data;
   end record;

   overriding
   procedure Initialize (Obj : in out OMP_Parallel);
   --  Initializing an OMP_Parallel object starts an OpenMP parallel region
   --  and creates a team of servers for serving light-weight threads
   --  (aka OpenMP tasks).

   overriding
   procedure Finalize (Obj : in out OMP_Parallel);
   --  Finalizing an OMP_Parallel object ends the parallel region, and
   --  releases all but the master OpenMP thread of the team of servers.

end LWT.OpenMP;
