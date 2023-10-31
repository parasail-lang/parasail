------------------------------------------------------------------------------
--                           L W T . O P E N M P                            --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification; use Ada.Task_Identification;

with LWT.Scheduler.OpenMP;

package body LWT.OpenMP is

   use LWT.Scheduler;

   pragma Linker_Options ("-lgomp");

   Debug_OpenMP : constant Boolean := True;

   procedure Parallel_Wrapper (C_Data : System.Address)
     with Convention => C;
   --  Wrapper that sets priority and executes GOMP_barrier

   procedure Parallel_Wrapper (C_Data : System.Address) is
      Data : OMP_Parallel_Data with Address => C_Data, Import;

      use Ada.Dynamic_Priorities;

      My_Index : constant LWT_Server_Index := Cur_Server_Index;
      --  This call has a side-effect of assigning a unique index
      --  if not already assigned.

      My_Prio : constant System.Priority := C.int'Pos (Data.Prio);

   begin
      --  ??? Should register this "foreign thread" as an Ada task explicitly

      --  Set Ada notion of priority
      Set_Priority (My_Prio);

      --  Remember task identity
      LWT.Scheduler.OpenMP.Set_Ada_Task_Identity (Data.Associated_Ada_Task);

      if Debug_OpenMP then
         Put_Line ("Starting server" & My_Index'Image & " at prio" &
           My_Prio'Image);
      end if;

      --  and wait to be used to execute some task
      GOMP_barrier;

      if Debug_OpenMP then
         Put_Line ("Releasing server" & My_Index'Image & " at prio" &
           My_Prio'Image);
      end if;

      --  Set identity back to null
      LWT.Scheduler.OpenMP.Set_Ada_Task_Identity (Null_Task_Id);
   end Parallel_Wrapper;

   overriding
   procedure Initialize (Obj : in out OMP_Parallel) is
      use Ada.Dynamic_Priorities;
   begin
      --  call GOMP_parallel_start (stub_fun, Obj.Num_Threads)
      --  with stub function for non-master threads
      --  that does what "single" would do for a non-master,
      --  namely call GOMP_barrier.
      --  see https://gcc.gnu.org/onlinedocs/libgomp/

      if Debug_OpenMP then
         Put_Line ("Init OMP_Parallel (" & Obj.Num_Threads'Image & ")");
      end if;

      LWT.Scheduler.OpenMP.Incr_OpenMP_Parallel_Regions;
      --  Install OpenMP as the LWT scheduler.

      --  Get the initial Ada priority and pass it along to the
      --  other threads in the "team"
      Obj.Data.Prio := C.int'Val (Get_Priority);

      --  Get Ada task identity of current LWT server, and pass it along.
      Obj.Data.Associated_Ada_Task := LWT.Scheduler.Ada_Task_Identity;

      GOMP_parallel_start
        (fn          => Parallel_Wrapper'Access,
         data        => Obj.Data'Address,
         num_threads => C.unsigned (Obj.Num_Threads));
   end Initialize;

   overriding
   procedure Finalize (Obj : in out OMP_Parallel) is
   begin
      --  call GOMP_barrier and then GOMP_parallel_end
      if Debug_OpenMP then
         Put_Line ("Finalize OMP_Parallel");
      end if;

      GOMP_barrier;
      GOMP_parallel_end;

      LWT.Scheduler.OpenMP.Decr_OpenMP_Parallel_Regions;
      --  Remove OpenMP as the LWT scheduler, if this is the
      --  last remaining active parallel region.
   end Finalize;

end LWT.OpenMP;
