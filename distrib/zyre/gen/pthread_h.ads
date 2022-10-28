pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with System;
with x86_64_linux_gnu_bits_pthreadtypes_h;
limited with time_h;
with stddef_h;
limited with x86_64_linux_gnu_bits_sched_h;
with Interfaces.C.Strings;
with x86_64_linux_gnu_bits_setjmp_h;
with x86_64_linux_gnu_bits_types_h;

package pthread_h is

   --  unsupported macro: PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE
   --  unsupported macro: PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
   --  unsupported macro: PTHREAD_MUTEX_INITIALIZER { { 0, 0, 0, 0, 0, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_RECURSIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ERRORCHECK_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP { { 0, 0, 0, 0, PTHREAD_MUTEX_ADAPTIVE_NP, __PTHREAD_SPINS, { 0, 0 } } }
   --  unsupported macro: PTHREAD_RWLOCK_INITIALIZER { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } }
   --  unsupported macro: PTHREAD_RWLOCK_WRITER_NONRECURSIVE_INITIALIZER_NP { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP } }
   --  unsupported macro: PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED
   --  unsupported macro: PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
   --  unsupported macro: PTHREAD_SCOPE_SYSTEM PTHREAD_SCOPE_SYSTEM
   --  unsupported macro: PTHREAD_SCOPE_PROCESS PTHREAD_SCOPE_PROCESS
   --  unsupported macro: PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE
   --  unsupported macro: PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
   --  unsupported macro: PTHREAD_COND_INITIALIZER { { 0, 0, 0, 0, 0, (void *) 0, 0, 0 } }
   --  unsupported macro: PTHREAD_CANCEL_ENABLE PTHREAD_CANCEL_ENABLE
   --  unsupported macro: PTHREAD_CANCEL_DISABLE PTHREAD_CANCEL_DISABLE
   --  unsupported macro: PTHREAD_CANCEL_DEFERRED PTHREAD_CANCEL_DEFERRED
   --  unsupported macro: PTHREAD_CANCEL_ASYNCHRONOUS PTHREAD_CANCEL_ASYNCHRONOUS
   --  unsupported macro: PTHREAD_CANCELED ((void *) -1)
   PTHREAD_ONCE_INIT : constant := 0;  --  /usr/include/pthread.h:228

   PTHREAD_BARRIER_SERIAL_THREAD : constant := -1;  --  /usr/include/pthread.h:235
   --  arg-macro: procedure pthread_cleanup_push (routine, arg)
   --    do { __pthread_cleanup_class __clframe (routine, arg)
   --  arg-macro: procedure pthread_cleanup_pop (execute)
   --    __clframe.__setdoit (execute); } while (0)
   --  arg-macro: procedure pthread_cleanup_push_defer_np (routine, arg)
   --    do { __pthread_cleanup_class __clframe (routine, arg); __clframe.__defer ()
   --  arg-macro: procedure pthread_cleanup_pop_restore_np (execute)
   --    __clframe.__restore (); __clframe.__setdoit (execute); } while (0)

  -- Copyright (C) 2002-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

  -- Detach state.   
  -- Mutex types.   
  -- For compatibility.   
  -- Robust mutex or not flags.   
  -- Mutex protocols.   
  -- Mutex initializers.   
  -- Read-write lock types.   
  -- Define __PTHREAD_RWLOCK_INT_FLAGS_SHARED to 1 if pthread_rwlock_t
  --   has the shared field.  All 64-bit architectures have the shared field
  --   in pthread_rwlock_t.   

  -- Read-write lock initializers.   
  -- Scheduler inheritance.   
  -- Scope handling.   
  -- Process shared or private flag.   
  -- Conditional variable handling.   
  -- Cleanup buffers  
  -- Function to call.   
   type u_pthread_cleanup_buffer;
   type u_pthread_cleanup_buffer is record
      uu_routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:203
      uu_arg : System.Address;  -- /usr/include/pthread.h:204
      uu_canceltype : aliased int;  -- /usr/include/pthread.h:205
      uu_prev : access u_pthread_cleanup_buffer;  -- /usr/include/pthread.h:206
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pthread.h:201

  -- Its argument.   
  -- Saved cancellation type.  
  -- Chaining of cleanup functions.   
  -- Cancellation  
  -- Single execution handling.   
  -- Value returned by 'pthread_barrier_wait' for one of the threads after
  --   the required number of threads have called this function.
  --   -1 is distinct from 0 and all errno constants  

  -- Create a new thread, starting with execution of START-ROUTINE
  --   getting passed ARG.  Creation attributed come from ATTR.  The new
  --   handle is stored in *NEWTHREAD.   

   function pthread_create
     (uu_newthread : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;
      uu_start_routine : access function (arg1 : System.Address) return System.Address;
      uu_arg : System.Address) return int  -- /usr/include/pthread.h:244
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_create";

  -- Terminate calling thread.
  --   The registered cleanup handlers are called via exception handling
  --   so we cannot mark this function with __THROW. 

   procedure pthread_exit (uu_retval : System.Address)  -- /usr/include/pthread.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_exit";

  -- Make calling thread wait for termination of the thread TH.  The
  --   exit status of the thread is stored in *THREAD_RETURN, if THREAD_RETURN
  --   is not NULL.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pthread_join (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_thread_return : System.Address) return int  -- /usr/include/pthread.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_join";

  -- Check whether thread TH has terminated.  If yes return the status of
  --   the thread in *THREAD_RETURN, if THREAD_RETURN is not NULL.   

   function pthread_tryjoin_np (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_thread_return : System.Address) return int  -- /usr/include/pthread.h:266
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_tryjoin_np";

  -- Make calling thread wait for termination of the thread TH, but only
  --   until TIMEOUT.  The exit status of the thread is stored in
  --   *THREAD_RETURN, if THREAD_RETURN is not NULL.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pthread_timedjoin_np
     (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_thread_return : System.Address;
      uu_abstime : access constant time_h.timespec) return int  -- /usr/include/pthread.h:274
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_timedjoin_np";

  -- Indicate that the thread TH is never to be joined with PTHREAD_JOIN.
  --   The resources of TH will therefore be freed immediately when it
  --   terminates, instead of waiting for another thread to perform PTHREAD_JOIN
  --   on it.   

   function pthread_detach (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t) return int  -- /usr/include/pthread.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_detach";

  -- Obtain the identifier of the current thread.   
   function pthread_self return x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t  -- /usr/include/pthread.h:286
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_self";

  -- Compare two thread identifiers.   
   function pthread_equal (uu_thread1 : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_thread2 : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t) return int  -- /usr/include/pthread.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_equal";

  -- Thread attribute handling.   
  -- Initialize thread attribute *ATTR with default attributes
  --   (detachstate is PTHREAD_JOINABLE, scheduling policy is SCHED_OTHER,
  --    no user-provided stack).   

   function pthread_attr_init (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t) return int  -- /usr/include/pthread.h:298
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_init";

  -- Destroy thread attribute *ATTR.   
   function pthread_attr_destroy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t) return int  -- /usr/include/pthread.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_destroy";

  -- Get detach state attribute.   
   function pthread_attr_getdetachstate (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_detachstate : access int) return int  -- /usr/include/pthread.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getdetachstate";

  -- Set detach state attribute.   
   function pthread_attr_setdetachstate (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_detachstate : int) return int  -- /usr/include/pthread.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setdetachstate";

  -- Get the size of the guard area created for stack overflow protection.   
   function pthread_attr_getguardsize (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_guardsize : access stddef_h.size_t) return int  -- /usr/include/pthread.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getguardsize";

  -- Set the size of the guard area created for stack overflow protection.   
   function pthread_attr_setguardsize (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_guardsize : stddef_h.size_t) return int  -- /usr/include/pthread.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setguardsize";

  -- Return in *PARAM the scheduling parameters of *ATTR.   
   function pthread_attr_getschedparam (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_param : access x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/pthread.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getschedparam";

  -- Set scheduling parameters (priority, etc) in *ATTR according to PARAM.   
   function pthread_attr_setschedparam (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/pthread.h:332
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setschedparam";

  -- Return in *POLICY the scheduling policy of *ATTR.   
   function pthread_attr_getschedpolicy (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_policy : access int) return int  -- /usr/include/pthread.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getschedpolicy";

  -- Set scheduling policy in *ATTR according to POLICY.   
   function pthread_attr_setschedpolicy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_policy : int) return int  -- /usr/include/pthread.h:342
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setschedpolicy";

  -- Return in *INHERIT the scheduling inheritance mode of *ATTR.   
   function pthread_attr_getinheritsched (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_inherit : access int) return int  -- /usr/include/pthread.h:346
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getinheritsched";

  -- Set scheduling inheritance mode in *ATTR according to INHERIT.   
   function pthread_attr_setinheritsched (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_inherit : int) return int  -- /usr/include/pthread.h:351
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setinheritsched";

  -- Return in *SCOPE the scheduling contention scope of *ATTR.   
   function pthread_attr_getscope (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_scope : access int) return int  -- /usr/include/pthread.h:357
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getscope";

  -- Set scheduling contention scope in *ATTR according to SCOPE.   
   function pthread_attr_setscope (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_scope : int) return int  -- /usr/include/pthread.h:362
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setscope";

  -- Return the previously set address for the stack.   
   function pthread_attr_getstackaddr (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_stackaddr : System.Address) return int  -- /usr/include/pthread.h:366
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getstackaddr";

  -- Set the starting address of the stack of the thread to be created.
  --   Depending on whether the stack grows up or down the value must either
  --   be higher or lower than all the address in the memory block.  The
  --   minimal size of the block must be PTHREAD_STACK_MIN.   

   function pthread_attr_setstackaddr (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_stackaddr : System.Address) return int  -- /usr/include/pthread.h:374
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setstackaddr";

  -- Return the currently used minimal stack size.   
   function pthread_attr_getstacksize (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_stacksize : access stddef_h.size_t) return int  -- /usr/include/pthread.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getstacksize";

  -- Add information about the minimum stack size needed for the thread
  --   to be started.  This size must never be less than PTHREAD_STACK_MIN
  --   and must also not exceed the system limits.   

   function pthread_attr_setstacksize (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t; uu_stacksize : stddef_h.size_t) return int  -- /usr/include/pthread.h:386
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setstacksize";

  -- Return the previously set address for the stack.   
   function pthread_attr_getstack
     (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;
      uu_stackaddr : System.Address;
      uu_stacksize : access stddef_h.size_t) return int  -- /usr/include/pthread.h:392
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getstack";

  -- The following two interfaces are intended to replace the last two.  They
  --   require setting the address as well as the size since only setting the
  --   address will make the implementation on some architectures impossible.   

   function pthread_attr_setstack
     (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;
      uu_stackaddr : System.Address;
      uu_stacksize : stddef_h.size_t) return int  -- /usr/include/pthread.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setstack";

  -- Thread created with attribute ATTR will be limited to run only on
  --   the processors represented in CPUSET.   

   function pthread_attr_setaffinity_np
     (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access constant x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/pthread.h:407
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_setaffinity_np";

  -- Get bit set in CPUSET representing the processors threads created with
  --   ATTR can run on.   

   function pthread_attr_getaffinity_np
     (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/pthread.h:414
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_attr_getaffinity_np";

  -- Get the default attributes used by pthread_create in this process.   
   function pthread_getattr_default_np (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t) return int  -- /usr/include/pthread.h:420
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getattr_default_np";

  -- Set the default attributes to be used by pthread_create in this
  --   process.   

   function pthread_setattr_default_np (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t) return int  -- /usr/include/pthread.h:425
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setattr_default_np";

  -- Initialize thread attribute *ATTR with attributes corresponding to the
  --   already running thread TH.  It shall be called on uninitialized ATTR
  --   and destroyed with pthread_attr_destroy when no longer needed.   

   function pthread_getattr_np (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_attr_t) return int  -- /usr/include/pthread.h:431
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getattr_np";

  -- Functions for scheduling control.   
  -- Set the scheduling parameters for TARGET_THREAD according to POLICY
  --   and *PARAM.   

   function pthread_setschedparam
     (uu_target_thread : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_policy : int;
      uu_param : access constant x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/pthread.h:440
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setschedparam";

  -- Return in *POLICY and *PARAM the scheduling parameters for TARGET_THREAD.  
   function pthread_getschedparam
     (uu_target_thread : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_policy : access int;
      uu_param : access x86_64_linux_gnu_bits_sched_h.sched_param) return int  -- /usr/include/pthread.h:445
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getschedparam";

  -- Set the scheduling priority for TARGET_THREAD.   
   function pthread_setschedprio (uu_target_thread : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_prio : int) return int  -- /usr/include/pthread.h:451
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setschedprio";

  -- Get thread name visible in the kernel and its interfaces.   
   function pthread_getname_np
     (uu_target_thread : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t) return int  -- /usr/include/pthread.h:457
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getname_np";

  -- Set thread name visible in the kernel and its interfaces.   
   function pthread_setname_np (uu_target_thread : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_name : Interfaces.C.Strings.chars_ptr) return int  -- /usr/include/pthread.h:462
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setname_np";

  -- Determine level of concurrency.   
   function pthread_getconcurrency return int  -- /usr/include/pthread.h:469
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getconcurrency";

  -- Set new concurrency level to LEVEL.   
   function pthread_setconcurrency (uu_level : int) return int  -- /usr/include/pthread.h:472
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setconcurrency";

  -- Yield the processor to another thread or process.
  --   This function is similar to the POSIX `sched_yield' function but
  --   might be differently implemented in the case of a m-on-n thread
  --   implementation.   

   function pthread_yield return int  -- /usr/include/pthread.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_yield";

  -- Limit specified thread TH to run only on the processors represented
  --   in CPUSET.   

   function pthread_setaffinity_np
     (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access constant x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/pthread.h:485
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setaffinity_np";

  -- Get bit set in CPUSET representing the processors TH can run on.   
   function pthread_getaffinity_np
     (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t;
      uu_cpusetsize : stddef_h.size_t;
      uu_cpuset : access x86_64_linux_gnu_bits_sched_h.cpu_set_t) return int  -- /usr/include/pthread.h:490
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getaffinity_np";

  -- Functions for handling initialization.   
  -- Guarantee that the initialization function INIT_ROUTINE will be called
  --   only once, even if pthread_once is executed several times with the
  --   same ONCE_CONTROL argument. ONCE_CONTROL must point to a static or
  --   extern variable initialized to PTHREAD_ONCE_INIT.
  --   The initialization functions might throw exception which is why
  --   this function is not marked with __THROW.   

   function pthread_once (uu_once_control : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_once_t; uu_init_routine : access procedure) return int  -- /usr/include/pthread.h:505
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_once";

  -- Functions for handling cancellation.
  --   Note that these functions are explicitly not marked to not throw an
  --   exception in C++ code.  If cancellation is implemented by unwinding
  --   this is necessary to have the compiler generate the unwind information.   

  -- Set cancelability state of current thread to STATE, returning old
  --   state in *OLDSTATE if OLDSTATE is not NULL.   

   function pthread_setcancelstate (uu_state : int; uu_oldstate : access int) return int  -- /usr/include/pthread.h:517
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setcancelstate";

  -- Set cancellation state of current thread to TYPE, returning the old
  --   type in *OLDTYPE if OLDTYPE is not NULL.   

   function pthread_setcanceltype (uu_type : int; uu_oldtype : access int) return int  -- /usr/include/pthread.h:521
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setcanceltype";

  -- Cancel THREAD immediately or at the next possibility.   
   function pthread_cancel (uu_th : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t) return int  -- /usr/include/pthread.h:524
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cancel";

  -- Test for pending cancellation for the current thread and terminate
  --   the thread as per pthread_exit(PTHREAD_CANCELED) if it has been
  --   cancelled.   

   procedure pthread_testcancel  -- /usr/include/pthread.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_testcancel";

  -- Cancellation handling with integration into exception handling.   
   --  skipped anonymous struct anon_111

   type anon_112 is record
      uu_cancel_jmp_buf : aliased x86_64_linux_gnu_bits_setjmp_h.uu_jmp_buf;  -- /usr/include/pthread.h:538
      uu_mask_was_saved : aliased int;  -- /usr/include/pthread.h:539
   end record
   with Convention => C_Pass_By_Copy;
   type uu_pthread_unwind_buf_t_array3823 is array (0 .. 0) of aliased anon_112;
   type uu_pthread_unwind_buf_t_array3824 is array (0 .. 3) of System.Address;
   type uu_pthread_unwind_buf_t is record
      uu_cancel_jmp_buf : aliased uu_pthread_unwind_buf_t_array3823;  -- /usr/include/pthread.h:540
      uu_pad : uu_pthread_unwind_buf_t_array3824;  -- /usr/include/pthread.h:541
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pthread.h:542

  -- No special attributes by default.   
  -- Structure to hold the cleanup handler information.   
   type uu_pthread_cleanup_frame is record
      uu_cancel_routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:553
      uu_cancel_arg : System.Address;  -- /usr/include/pthread.h:554
      uu_do_it : aliased int;  -- /usr/include/pthread.h:555
      uu_cancel_type : aliased int;  -- /usr/include/pthread.h:556
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/include/pthread.h:551

  -- Class to handle cancellation handler invocation.   
   package Class_uu_pthread_cleanup_class is
      type uu_pthread_cleanup_class is limited record
         uu_cancel_routine : access procedure (arg1 : System.Address);  -- /usr/include/pthread.h:564
         uu_cancel_arg : System.Address;  -- /usr/include/pthread.h:565
         uu_do_it : aliased int;  -- /usr/include/pthread.h:566
         uu_cancel_type : aliased int;  -- /usr/include/pthread.h:567
      end record
      with Import => True,
           Convention => CPP;

      function New_uu_pthread_cleanup_class (uu_fct : access procedure (arg1 : System.Address); uu_arg : System.Address) return uu_pthread_cleanup_class;  -- /usr/include/pthread.h:570
      pragma CPP_Constructor (New_uu_pthread_cleanup_class, "_ZN23__pthread_cleanup_classC1EPFvPvES0_");

      procedure Delete_uu_pthread_cleanup_class (this : access uu_pthread_cleanup_class)  -- /usr/include/pthread.h:572
      with Import => True, 
           Convention => CPP, 
           External_Name => "_ZN23__pthread_cleanup_classD1Ev";

      --  skipped func __setdoit

      --  skipped func __defer

      --  skipped func __restore
   end;
   use Class_uu_pthread_cleanup_class;
  -- Install a cleanup handler: ROUTINE will be called with arguments ARG
  --   when the thread is canceled or calls pthread_exit.  ROUTINE will also
  --   be called with arguments ARG when the matching pthread_cleanup_pop
  --   is executed with non-zero EXECUTE argument.
  --   pthread_cleanup_push and pthread_cleanup_pop are macros and must always
  --   be used in matching pairs at the same nesting level of braces.   

  -- Remove a cleanup handler installed by the matching pthread_cleanup_push.
  --   If EXECUTE is non-zero, the handler function is called.  

  -- Install a cleanup handler as pthread_cleanup_push does, but also
  --   saves the current cancellation type and sets it to deferred
  --   cancellation.   

  -- Remove a cleanup handler as pthread_cleanup_pop does, but also
  --   restores the cancellation type that was in effect when the matching
  --   pthread_cleanup_push_defer was called.   

  -- Function called to call the cleanup handler.  As an extern inline
  --   function the compiler is free to decide inlining the change when
  --   needed or fall back on the copy which must exist somewhere
  --   else.   

  -- Install a cleanup handler: ROUTINE will be called with arguments ARG
  --   when the thread is canceled or calls pthread_exit.  ROUTINE will also
  --   be called with arguments ARG when the matching pthread_cleanup_pop
  --   is executed with non-zero EXECUTE argument.
  --   pthread_cleanup_push and pthread_cleanup_pop are macros and must always
  --   be used in matching pairs at the same nesting level of braces.   

  -- Remove a cleanup handler installed by the matching pthread_cleanup_push.
  --   If EXECUTE is non-zero, the handler function is called.  

  -- Install a cleanup handler as pthread_cleanup_push does, but also
  --   saves the current cancellation type and sets it to deferred
  --   cancellation.   

  -- Remove a cleanup handler as pthread_cleanup_pop does, but also
  --   restores the cancellation type that was in effect when the matching
  --   pthread_cleanup_push_defer was called.   

  -- Install a cleanup handler: ROUTINE will be called with arguments ARG
  --   when the thread is canceled or calls pthread_exit.  ROUTINE will also
  --   be called with arguments ARG when the matching pthread_cleanup_pop
  --   is executed with non-zero EXECUTE argument.
  --   pthread_cleanup_push and pthread_cleanup_pop are macros and must always
  --   be used in matching pairs at the same nesting level of braces.   

  -- Remove a cleanup handler installed by the matching pthread_cleanup_push.
  --   If EXECUTE is non-zero, the handler function is called.  

  -- Install a cleanup handler as pthread_cleanup_push does, but also
  --   saves the current cancellation type and sets it to deferred
  --   cancellation.   

  -- Remove a cleanup handler as pthread_cleanup_pop does, but also
  --   restores the cancellation type that was in effect when the matching
  --   pthread_cleanup_push_defer was called.   

  -- Internal interface to initiate cleanup.   
  -- Function used in the macros.   
   --  skipped func __sigsetjmp

  -- Mutex handling.   
  -- Initialize a mutex.   
   function pthread_mutex_init (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t; uu_mutexattr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t) return int  -- /usr/include/pthread.h:760
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_init";

  -- Destroy a mutex.   
   function pthread_mutex_destroy (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:765
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_destroy";

  -- Try locking a mutex.   
   function pthread_mutex_trylock (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:769
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_trylock";

  -- Lock a mutex.   
   function pthread_mutex_lock (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:773
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_lock";

  -- Wait until lock becomes available, or specified time passes.  
   function pthread_mutex_timedlock (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t; uu_abstime : access constant time_h.timespec) return int  -- /usr/include/pthread.h:778
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_timedlock";

  -- Unlock a mutex.   
   function pthread_mutex_unlock (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:784
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_unlock";

  -- Get the priority ceiling of MUTEX.   
   function pthread_mutex_getprioceiling (uu_mutex : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t; uu_prioceiling : access int) return int  -- /usr/include/pthread.h:789
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_getprioceiling";

  -- Set the priority ceiling of MUTEX to PRIOCEILING, return old
  --   priority ceiling value in *OLD_CEILING.   

   function pthread_mutex_setprioceiling
     (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;
      uu_prioceiling : int;
      uu_old_ceiling : access int) return int  -- /usr/include/pthread.h:796
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_setprioceiling";

  -- Declare the state protected by MUTEX as consistent.   
   function pthread_mutex_consistent (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:804
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_consistent";

   function pthread_mutex_consistent_np (uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:807
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutex_consistent_np";

  -- Functions for handling mutex attributes.   
  -- Initialize mutex attribute object ATTR with default attributes
  --   (kind is PTHREAD_MUTEX_TIMED_NP).   

   function pthread_mutexattr_init (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t) return int  -- /usr/include/pthread.h:817
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_init";

  -- Destroy mutex attribute object ATTR.   
   function pthread_mutexattr_destroy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t) return int  -- /usr/include/pthread.h:821
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_destroy";

  -- Get the process-shared flag of the mutex attribute ATTR.   
   function pthread_mutexattr_getpshared (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_pshared : access int) return int  -- /usr/include/pthread.h:825
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_getpshared";

  -- Set the process-shared flag of the mutex attribute ATTR.   
   function pthread_mutexattr_setpshared (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_pshared : int) return int  -- /usr/include/pthread.h:831
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_setpshared";

  -- Return in *KIND the mutex kind attribute in *ATTR.   
   function pthread_mutexattr_gettype (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_kind : access int) return int  -- /usr/include/pthread.h:837
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_gettype";

  -- Set the mutex kind attribute in *ATTR to KIND (either PTHREAD_MUTEX_NORMAL,
  --   PTHREAD_MUTEX_RECURSIVE, PTHREAD_MUTEX_ERRORCHECK, or
  --   PTHREAD_MUTEX_DEFAULT).   

   function pthread_mutexattr_settype (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_kind : int) return int  -- /usr/include/pthread.h:844
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_settype";

  -- Return in *PROTOCOL the mutex protocol attribute in *ATTR.   
   function pthread_mutexattr_getprotocol (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_protocol : access int) return int  -- /usr/include/pthread.h:849
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_getprotocol";

  -- Set the mutex protocol attribute in *ATTR to PROTOCOL (either
  --   PTHREAD_PRIO_NONE, PTHREAD_PRIO_INHERIT, or PTHREAD_PRIO_PROTECT).   

   function pthread_mutexattr_setprotocol (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_protocol : int) return int  -- /usr/include/pthread.h:856
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_setprotocol";

  -- Return in *PRIOCEILING the mutex prioceiling attribute in *ATTR.   
   function pthread_mutexattr_getprioceiling (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_prioceiling : access int) return int  -- /usr/include/pthread.h:861
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_getprioceiling";

  -- Set the mutex prioceiling attribute in *ATTR to PRIOCEILING.   
   function pthread_mutexattr_setprioceiling (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_prioceiling : int) return int  -- /usr/include/pthread.h:867
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_setprioceiling";

  -- Get the robustness flag of the mutex attribute ATTR.   
   function pthread_mutexattr_getrobust (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_robustness : access int) return int  -- /usr/include/pthread.h:873
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_getrobust";

   function pthread_mutexattr_getrobust_np (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_robustness : access int) return int  -- /usr/include/pthread.h:877
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_getrobust_np";

  -- Set the robustness flag of the mutex attribute ATTR.   
   function pthread_mutexattr_setrobust (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_robustness : int) return int  -- /usr/include/pthread.h:883
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_setrobust";

   function pthread_mutexattr_setrobust_np (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutexattr_t; uu_robustness : int) return int  -- /usr/include/pthread.h:887
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_mutexattr_setrobust_np";

  -- Functions for handling read-write locks.   
  -- Initialize read-write lock RWLOCK using attributes ATTR, or use
  --   the default values if later is NULL.   

   function pthread_rwlock_init (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t; uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t) return int  -- /usr/include/pthread.h:899
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_init";

  -- Destroy read-write lock RWLOCK.   
   function pthread_rwlock_destroy (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:904
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_destroy";

  -- Acquire read lock for RWLOCK.   
   function pthread_rwlock_rdlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:908
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_rdlock";

  -- Try to acquire read lock for RWLOCK.   
   function pthread_rwlock_tryrdlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:912
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_tryrdlock";

  -- Try to acquire read lock for RWLOCK or return after specfied time.   
   function pthread_rwlock_timedrdlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t; uu_abstime : access constant time_h.timespec) return int  -- /usr/include/pthread.h:917
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_timedrdlock";

  -- Acquire write lock for RWLOCK.   
   function pthread_rwlock_wrlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:923
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_wrlock";

  -- Try to acquire write lock for RWLOCK.   
   function pthread_rwlock_trywrlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:927
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_trywrlock";

  -- Try to acquire write lock for RWLOCK or return after specfied time.   
   function pthread_rwlock_timedwrlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t; uu_abstime : access constant time_h.timespec) return int  -- /usr/include/pthread.h:932
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_timedwrlock";

  -- Unlock RWLOCK.   
   function pthread_rwlock_unlock (uu_rwlock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlock_t) return int  -- /usr/include/pthread.h:938
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlock_unlock";

  -- Functions for handling read-write lock attributes.   
  -- Initialize attribute object ATTR with default values.   
   function pthread_rwlockattr_init (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t) return int  -- /usr/include/pthread.h:945
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_init";

  -- Destroy attribute object ATTR.   
   function pthread_rwlockattr_destroy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t) return int  -- /usr/include/pthread.h:949
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_destroy";

  -- Return current setting of process-shared attribute of ATTR in PSHARED.   
   function pthread_rwlockattr_getpshared (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t; uu_pshared : access int) return int  -- /usr/include/pthread.h:953
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_getpshared";

  -- Set process-shared attribute of ATTR to PSHARED.   
   function pthread_rwlockattr_setpshared (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t; uu_pshared : int) return int  -- /usr/include/pthread.h:959
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_setpshared";

  -- Return current setting of reader/writer preference.   
   function pthread_rwlockattr_getkind_np (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t; uu_pref : access int) return int  -- /usr/include/pthread.h:964
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_getkind_np";

  -- Set reader/write preference.   
   function pthread_rwlockattr_setkind_np (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_rwlockattr_t; uu_pref : int) return int  -- /usr/include/pthread.h:970
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_rwlockattr_setkind_np";

  -- Functions for handling conditional variables.   
  -- Initialize condition variable COND using attributes ATTR, or use
  --   the default values if later is NULL.   

   function pthread_cond_init (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t; uu_cond_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t) return int  -- /usr/include/pthread.h:979
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_init";

  -- Destroy condition variable COND.   
   function pthread_cond_destroy (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t) return int  -- /usr/include/pthread.h:984
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_destroy";

  -- Wake up one thread waiting for condition variable COND.   
   function pthread_cond_signal (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t) return int  -- /usr/include/pthread.h:988
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_signal";

  -- Wake up all threads waiting for condition variables COND.   
   function pthread_cond_broadcast (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t) return int  -- /usr/include/pthread.h:992
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_broadcast";

  -- Wait for condition variable COND to be signaled or broadcast.
  --   MUTEX is assumed to be locked before.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pthread_cond_wait (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t; uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t) return int  -- /usr/include/pthread.h:1000
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_wait";

  -- Wait for condition variable COND to be signaled or broadcast until
  --   ABSTIME.  MUTEX is assumed to be locked before.  ABSTIME is an
  --   absolute time specification; zero is the beginning of the epoch
  --   (00:00:00 GMT, January 1, 1970).
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function pthread_cond_timedwait
     (uu_cond : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_cond_t;
      uu_mutex : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_mutex_t;
      uu_abstime : access constant time_h.timespec) return int  -- /usr/include/pthread.h:1011
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_cond_timedwait";

  -- Functions for handling condition variable attributes.   
  -- Initialize condition variable attribute ATTR.   
   function pthread_condattr_init (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t) return int  -- /usr/include/pthread.h:1019
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_init";

  -- Destroy condition variable attribute ATTR.   
   function pthread_condattr_destroy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t) return int  -- /usr/include/pthread.h:1023
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_destroy";

  -- Get the process-shared flag of the condition variable attribute ATTR.   
   function pthread_condattr_getpshared (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t; uu_pshared : access int) return int  -- /usr/include/pthread.h:1027
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_getpshared";

  -- Set the process-shared flag of the condition variable attribute ATTR.   
   function pthread_condattr_setpshared (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t; uu_pshared : int) return int  -- /usr/include/pthread.h:1033
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_setpshared";

  -- Get the clock selected for the condition variable attribute ATTR.   
   function pthread_condattr_getclock (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t; uu_clock_id : access x86_64_linux_gnu_bits_types_h.uu_clockid_t) return int  -- /usr/include/pthread.h:1038
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_getclock";

  -- Set the clock selected for the condition variable attribute ATTR.   
   function pthread_condattr_setclock (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_condattr_t; uu_clock_id : x86_64_linux_gnu_bits_types_h.uu_clockid_t) return int  -- /usr/include/pthread.h:1044
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_condattr_setclock";

  -- Functions to handle spinlocks.   
  -- Initialize the spinlock LOCK.  If PSHARED is nonzero the spinlock can
  --   be shared between different processes.   

   function pthread_spin_init (uu_lock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_spinlock_t; uu_pshared : int) return int  -- /usr/include/pthread.h:1055
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_spin_init";

  -- Destroy the spinlock LOCK.   
   function pthread_spin_destroy (uu_lock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_spinlock_t) return int  -- /usr/include/pthread.h:1059
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_spin_destroy";

  -- Wait until spinlock LOCK is retrieved.   
   function pthread_spin_lock (uu_lock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_spinlock_t) return int  -- /usr/include/pthread.h:1063
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_spin_lock";

  -- Try to lock spinlock LOCK.   
   function pthread_spin_trylock (uu_lock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_spinlock_t) return int  -- /usr/include/pthread.h:1067
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_spin_trylock";

  -- Release spinlock LOCK.   
   function pthread_spin_unlock (uu_lock : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_spinlock_t) return int  -- /usr/include/pthread.h:1071
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_spin_unlock";

  -- Functions to handle barriers.   
  -- Initialize BARRIER with the attributes in ATTR.  The barrier is
  --   opened when COUNT waiters arrived.   

   function pthread_barrier_init
     (uu_barrier : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrier_t;
      uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrierattr_t;
      uu_count : unsigned) return int  -- /usr/include/pthread.h:1079
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrier_init";

  -- Destroy a previously dynamically initialized barrier BARRIER.   
   function pthread_barrier_destroy (uu_barrier : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrier_t) return int  -- /usr/include/pthread.h:1085
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrier_destroy";

  -- Wait on barrier BARRIER.   
   function pthread_barrier_wait (uu_barrier : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrier_t) return int  -- /usr/include/pthread.h:1089
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrier_wait";

  -- Initialize barrier attribute ATTR.   
   function pthread_barrierattr_init (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrierattr_t) return int  -- /usr/include/pthread.h:1094
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrierattr_init";

  -- Destroy previously dynamically initialized barrier attribute ATTR.   
   function pthread_barrierattr_destroy (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrierattr_t) return int  -- /usr/include/pthread.h:1098
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrierattr_destroy";

  -- Get the process-shared flag of the barrier attribute ATTR.   
   function pthread_barrierattr_getpshared (uu_attr : access constant x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrierattr_t; uu_pshared : access int) return int  -- /usr/include/pthread.h:1102
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrierattr_getpshared";

  -- Set the process-shared flag of the barrier attribute ATTR.   
   function pthread_barrierattr_setpshared (uu_attr : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_barrierattr_t; uu_pshared : int) return int  -- /usr/include/pthread.h:1108
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_barrierattr_setpshared";

  -- Functions for handling thread-specific data.   
  -- Create a key value identifying a location in the thread-specific
  --   data area.  Each thread maintains a distinct thread-specific data
  --   area.  DESTR_FUNCTION, if non-NULL, is called with the value
  --   associated to that key when the key is destroyed.
  --   DESTR_FUNCTION is not called if the value associated is NULL when
  --   the key is destroyed.   

   function pthread_key_create (uu_key : access x86_64_linux_gnu_bits_pthreadtypes_h.pthread_key_t; uu_destr_function : access procedure (arg1 : System.Address)) return int  -- /usr/include/pthread.h:1122
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_key_create";

  -- Destroy KEY.   
   function pthread_key_delete (uu_key : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_key_t) return int  -- /usr/include/pthread.h:1127
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_key_delete";

  -- Return current value of the thread-specific data slot identified by KEY.   
   function pthread_getspecific (uu_key : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_key_t) return System.Address  -- /usr/include/pthread.h:1130
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getspecific";

  -- Store POINTER in the thread-specific data slot identified by KEY.  
   function pthread_setspecific (uu_key : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_key_t; uu_pointer : System.Address) return int  -- /usr/include/pthread.h:1133
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_setspecific";

  -- Get ID of CPU-time clock for thread THREAD_ID.   
   function pthread_getcpuclockid (uu_thread_id : x86_64_linux_gnu_bits_pthreadtypes_h.pthread_t; uu_clock_id : access x86_64_linux_gnu_bits_types_h.uu_clockid_t) return int  -- /usr/include/pthread.h:1139
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_getcpuclockid";

  -- Install handlers to be called when a new process is created with FORK.
  --   The PREPARE handler is called in the parent process just before performing
  --   FORK. The PARENT handler is called in the parent process just after FORK.
  --   The CHILD handler is called in the child process.  Each of the three
  --   handlers can be NULL, meaning that no handler needs to be called at that
  --   point.
  --   PTHREAD_ATFORK can be called several times, in which case the PREPARE
  --   handlers are called in LIFO order (last added with PTHREAD_ATFORK,
  --   first called before FORK), and the PARENT and CHILD handlers are called
  --   in FIFO (first added, first called).   

   function pthread_atfork
     (uu_prepare : access procedure;
      uu_parent : access procedure;
      uu_child : access procedure) return int  -- /usr/include/pthread.h:1156
   with Import => True, 
        Convention => C, 
        External_Name => "pthread_atfork";

  -- Optimizations.   
end pthread_h;
