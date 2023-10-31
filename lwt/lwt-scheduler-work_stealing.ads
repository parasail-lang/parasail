------------------------------------------------------------------------------
--              L W T . S C H E D U L E R .W O R K _ S T E A L I N G        --
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

with LWT.Work_Stealing;

private with Ada.Task_Identification;

package LWT.Scheduler.Work_Stealing is
   --  This package implements a work-stealing-based plug-in LWT scheduler

   procedure Show_Stats (Clear : Boolean := False);
   --  Show statistics, and clear the accumulators if Clear is True

   procedure Show_Servers;
   --  Show number of servers and other information about servers.

   type Server_Team_Info is limited private;
   --  Information about team of servers created for a given parallel region

   type Server_Team_Ptr is access all Server_Team_Info;
   --  Pointer to team info

   --  These two routines keep track of the number of active WS
   --  parallel regions.  While there is at least one active, we establish
   --  WS as the LWT scheduler.  These are called as a side-effect
   --  of init'ing/finalizing an Interfaces.Work_Stealing.WS_Parallel object.
   --  It is implementation-defined whether there is one LWT scheduler
   --  plug-in per Ada task, or one per program partition.

   procedure Incr_WS_Parallel_Regions
     (Obj : LWT.Work_Stealing.WS_Parallel;
      Team_Info : not null Server_Team_Ptr);

   procedure Decr_WS_Parallel_Regions
     (Obj : LWT.Work_Stealing.WS_Parallel;
      Team_Info : not null Server_Team_Ptr);

private

   Max_WS_Servers : constant Natural := 100;
      --  Max number servers for work stealing.
      --  NOTE: This is an arbitrary maximum; it could be much bigger.

   Max_Team_Size : constant := Max_WS_Servers;
   --  A "team" is a set of servers started at the same time.

   type Team_Member_Index is new Integer range 1 .. Max_Team_Size;

   type Server_Index_Array is
     array (Team_Member_Index range <>) of LWT_Server_Index_Base;

   type Server_Index_Array_Ptr is access Server_Index_Array;

   type LWT_Count_Base is range 0 .. Integer'Last;
   subtype LWT_Count is
     LWT_Count_Base range 0 .. LWT_Count_Base'Last;
   --  Number of active sub LWTs of a given Group

   type Atomic_LWT_Count is new LWT_Count'Base with Atomic;

   protected type Team_Manager (Team_Info : access Server_Team_Info) is
      --  This protected object manages the creating and
      --  stealing of LW ("pico") LWTs within a given team.
      --  TBD: Finer-grained locking might be useful someday.

      procedure Wake_Team;
      --  Num_LWTs_On_Team_Deques just went from zero to one,
      --  so there might be some servers waiting for work to do.

      --  NYI: Premature Exit
      --  entry Prepare_To_Exit
      --    (Group    : WS_Group_Ptr;
      --     Server_Index     : LWT_Server_Index;
      --     Exiting_Tcb      : LWT_Ptr;
      --     Succeeded        : out Boolean);
      --  Prepare to exit given Group.
      --  Set Succeeded to indicate whether the prepare-to-exit
      --  succeeded.

      entry Wait_For_Work (Server_Index : LWT_Server_Index);
      --  A server of the team found nothing to do; wait for new work

      entry Shut_Down_Team;
      --  Shut down the team serving the current parallel region

      procedure Shutting_Down (Team_Member : Team_Member_Index);
      --  Called when a team member is shutting down

   private

      entry Wait_For_Members_To_Shut_Down;
      --  Requeue on this entry after shutting down the team
      --  to wait for all servers of team to complete.

      --  NOTE: Some private data has been moved to enclosing Team_Info

      Num_Shut_Down : Natural := 0;

   end Team_Manager;

   type Server_Team_Info is limited record
      Enclosing_Team_Info : Server_Team_Ptr := null;
      Enclosing_Team_Member : Team_Member_Index := Team_Member_Index'Last;
      Team_Array : Server_Index_Array_Ptr := null;
      Team_Is_Shut_Down  : Boolean := False with Atomic;
      --  Set True when team's parallel region is ending.

      Associated_Ada_Task : aliased Ada.Task_Identification.Task_Id;
      --  Identify of Ada task from which team is spawned.

      Num_LWTs_On_Team_Deques : aliased Atomic_LWT_Count;
      --  Number of LWTs on any team-member's deque.

      Num_Members_Waiting_For_Work : aliased Atomic_LWT_Count;
      --  Number of team members that are waiting on Wait_For_Work

      Manager : Team_Manager (Server_Team_Info'Access);
      --  Protected object used for synchronizing team
   end record;

end LWT.Scheduler.Work_Stealing;
