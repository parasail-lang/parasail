------------------------------------------------------------------------------
--                    L W T . W O R K _ S T E A L I N G                     --
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

with LWT.Aspects;

private with Ada.Finalization;
limited private with LWT.Scheduler.Work_Stealing;

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
   use Ada.Finalization;

   type WS_Parallel
     (Num_Servers : Natural := 0;
      --  Number of servers to devote to this task's work-stealing;
      --  by default, will use the number of physical cores as a guide.
      Options : access WS_Options := null) is new Limited_Controlled with
   record
      --  Information about team of servers created for this parallel region
      Team_Info : access LWT.Scheduler.Work_Stealing.Server_Team_Info;
   end record;

   procedure Initialize (Obj : in out WS_Parallel);
   procedure Finalize (Obj : in out WS_Parallel);

end LWT.Work_Stealing;
