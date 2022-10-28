pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package omp_h is


   OMP_H : constant := 1;  --  omp.h:26

  -- These two structures get edited by the libgomp build process to 
  --   reflect the shape of the two types.  Their internals are private
  --   to the library.   

   type omp_lock_t_u_x_array is array (0 .. 3) of aliased unsigned_char;
   type omp_lock_t is record
      u_x : aliased omp_lock_t_u_x_array;  -- omp.h:37
   end record;
   pragma Convention (C_Pass_By_Copy, omp_lock_t);  -- omp.h:38

   --  skipped anonymous struct anon_0

   type omp_nest_lock_t_u_x_array is array (0 .. 15) of aliased unsigned_char;
   type omp_nest_lock_t is record
      u_x : aliased omp_nest_lock_t_u_x_array;  -- omp.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, omp_nest_lock_t);  -- omp.h:44

   --  skipped anonymous struct anon_1

   subtype omp_sched_t is unsigned;
   omp_sched_static : constant omp_sched_t := 1;
   omp_sched_dynamic : constant omp_sched_t := 2;
   omp_sched_guided : constant omp_sched_t := 3;
   omp_sched_auto : constant omp_sched_t := 4;  -- omp.h:47

   procedure omp_set_num_threads (arg1 : int);  -- omp.h:62
   pragma Import (C, omp_set_num_threads, "omp_set_num_threads");

   function omp_get_num_threads return int;  -- omp.h:63
   pragma Import (C, omp_get_num_threads, "omp_get_num_threads");

   function omp_get_max_threads return int;  -- omp.h:64
   pragma Import (C, omp_get_max_threads, "omp_get_max_threads");

   function omp_get_thread_num return int;  -- omp.h:65
   pragma Import (C, omp_get_thread_num, "omp_get_thread_num");

   function omp_get_num_procs return int;  -- omp.h:66
   pragma Import (C, omp_get_num_procs, "omp_get_num_procs");

   function omp_in_parallel return int;  -- omp.h:68
   pragma Import (C, omp_in_parallel, "omp_in_parallel");

   procedure omp_set_dynamic (arg1 : int);  -- omp.h:70
   pragma Import (C, omp_set_dynamic, "omp_set_dynamic");

   function omp_get_dynamic return int;  -- omp.h:71
   pragma Import (C, omp_get_dynamic, "omp_get_dynamic");

   procedure omp_set_nested (arg1 : int);  -- omp.h:73
   pragma Import (C, omp_set_nested, "omp_set_nested");

   function omp_get_nested return int;  -- omp.h:74
   pragma Import (C, omp_get_nested, "omp_get_nested");

   procedure omp_init_lock (arg1 : access omp_lock_t);  -- omp.h:76
   pragma Import (C, omp_init_lock, "omp_init_lock");

   procedure omp_destroy_lock (arg1 : access omp_lock_t);  -- omp.h:77
   pragma Import (C, omp_destroy_lock, "omp_destroy_lock");

   procedure omp_set_lock (arg1 : access omp_lock_t);  -- omp.h:78
   pragma Import (C, omp_set_lock, "omp_set_lock");

   procedure omp_unset_lock (arg1 : access omp_lock_t);  -- omp.h:79
   pragma Import (C, omp_unset_lock, "omp_unset_lock");

   function omp_test_lock (arg1 : access omp_lock_t) return int;  -- omp.h:80
   pragma Import (C, omp_test_lock, "omp_test_lock");

   procedure omp_init_nest_lock (arg1 : access omp_nest_lock_t);  -- omp.h:82
   pragma Import (C, omp_init_nest_lock, "omp_init_nest_lock");

   procedure omp_destroy_nest_lock (arg1 : access omp_nest_lock_t);  -- omp.h:83
   pragma Import (C, omp_destroy_nest_lock, "omp_destroy_nest_lock");

   procedure omp_set_nest_lock (arg1 : access omp_nest_lock_t);  -- omp.h:84
   pragma Import (C, omp_set_nest_lock, "omp_set_nest_lock");

   procedure omp_unset_nest_lock (arg1 : access omp_nest_lock_t);  -- omp.h:85
   pragma Import (C, omp_unset_nest_lock, "omp_unset_nest_lock");

   function omp_test_nest_lock (arg1 : access omp_nest_lock_t) return int;  -- omp.h:86
   pragma Import (C, omp_test_nest_lock, "omp_test_nest_lock");

   function omp_get_wtime return double;  -- omp.h:88
   pragma Import (C, omp_get_wtime, "omp_get_wtime");

   function omp_get_wtick return double;  -- omp.h:89
   pragma Import (C, omp_get_wtick, "omp_get_wtick");

   procedure omp_set_schedule (arg1 : omp_sched_t; arg2 : int);  -- omp.h:91
   pragma Import (C, omp_set_schedule, "omp_set_schedule");

   procedure omp_get_schedule (arg1 : access omp_sched_t; arg2 : access int);  -- omp.h:92
   pragma Import (C, omp_get_schedule, "omp_get_schedule");

   function omp_get_thread_limit return int;  -- omp.h:93
   pragma Import (C, omp_get_thread_limit, "omp_get_thread_limit");

   procedure omp_set_max_active_levels (arg1 : int);  -- omp.h:94
   pragma Import (C, omp_set_max_active_levels, "omp_set_max_active_levels");

   function omp_get_max_active_levels return int;  -- omp.h:95
   pragma Import (C, omp_get_max_active_levels, "omp_get_max_active_levels");

   function omp_get_level return int;  -- omp.h:96
   pragma Import (C, omp_get_level, "omp_get_level");

   function omp_get_ancestor_thread_num (arg1 : int) return int;  -- omp.h:97
   pragma Import (C, omp_get_ancestor_thread_num, "omp_get_ancestor_thread_num");

   function omp_get_team_size (arg1 : int) return int;  -- omp.h:98
   pragma Import (C, omp_get_team_size, "omp_get_team_size");

   function omp_get_active_level return int;  -- omp.h:99
   pragma Import (C, omp_get_active_level, "omp_get_active_level");

   function omp_in_final return int;  -- omp.h:101
   pragma Import (C, omp_in_final, "omp_in_final");

end omp_h;
