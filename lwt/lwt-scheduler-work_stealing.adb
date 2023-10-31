------------------------------------------------------------------------------
--                              L W T Scheduler                             --
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
--                                                                          --
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

--  Package defining a light-weight scheduler for Ada 202X
--  based on work stealing
pragma Ada_2022;

with System.Atomic_Operations.Integer_Arithmetic;
pragma Elaborate (System.Atomic_Operations.Integer_Arithmetic);

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Unchecked_Deallocation;

with LWT.Generic_Synchronized_Deques;
pragma Elaborate_All (LWT.Generic_Synchronized_Deques);

with LWT.Generic_Atomic_Max_Min;
pragma Elaborate_All (LWT.Generic_Atomic_Max_Min);

with LWT.Scheduler;
pragma Elaborate (LWT.Scheduler);

with Ada.Dynamic_Priorities;

with System.Multiprocessors;
with LWT.Statistics; use LWT.Statistics;
--  with LWT.Storage;  --  TBD: not needed until GNAT bug fixed
with System.Storage_Elements;

package body LWT.Scheduler.Work_Stealing is
   --  A light-weight LWT scheduler based on work stealing.
   --  Each server task has its own private LIFO queue.
   --  Each server task also has a FIFO queue that is shared across servers.
   --  LW threads are moved to the shared FIFO queue when some
   --  server is looking for work to do, and current server has some
   --  LWTs needing service.

   Debug_LWT : constant Boolean := False;
   Debug_Stealing : constant Boolean := Debug_LWT or else False;
   Debug_Kill : constant Boolean := False;

   Yield_When_Spawning_LWT : constant Boolean := False;
   --  If True, will Delay 0.0 after adding an LW thread to the deque
   --  NOTE: This is not necessary on an RTS with "true" multi-tasking

   Associated_Ada_Task_Ptr : access Ada.Task_Identification.Task_Id := null
     with Thread_Local_Storage;
   --  This keeps track of the Ada task with which a given LWT server
   --  is associated.

   type Atomic_Int is new Integer with Atomic;
   package Int_Counters is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_Int);

   package LWT_Counters is
     new System.Atomic_Operations.Integer_Arithmetic (Atomic_LWT_Count);

   type Atomic_Base_Boolean is new Boolean with Atomic;

   package Boolean_Max_Min is
     new Generic_Atomic_Max_Min (Atomic_Base_Boolean);

   subtype Atomic_Boolean is Boolean_Max_Min.Atomic_Type;
   use all type Atomic_Boolean;

   --  --  WS LWT Groups  --  --

   type WS_Group is tagged;
   type WS_Group_Ptr is access all WS_Group;

   --  Index used to uniquely identify a shared Group
   type Shared_Group_Index is range 0 .. 2**15 - 1;

   --  Zero is used to indicate an unshared Group
   Unshared_Group : constant Shared_Group_Index := 0;

   type WS_Group is new LWT_Sched_Group with record
      Enclosing_Group : WS_Group_Ptr := null;
         --  Enclosing Group, if any.
         --  Field will be initialized as a side-effect of
         --  calling Init_Owning_Server.
      Owning_Server : LWT_Server_Index_Base := Cur_Server_Index;
         --  Server that declared the WS_Group

      Shared_Index : Shared_Group_Index := Unshared_Group;
         --  If > 0, then this is a shared Group, and this is its
         --  unique index.  Being shared means that some of the sub-LWTs
         --  of this Group are available for stealing, and hence might
         --  be executed by a different server.

      Dependent_Count : aliased Atomic_LWT_Count := 0;
         --  (Atomic) Count of dependents, which is either:
         --    for an unshared Group -- count of all sub-LWTs;
         --    for a shared Group    -- count of shared sub-LWTs and
         --       number of servers with at least one unshared sub-LWT
         --       associated with this Group.

      --  Waiting_Server : LWT_Server_Index_Base := 0;
         --  Which server, if any, is waiting for Group
         --  TBD: Not used currently

      --  Is_Being_Awaited : Boolean := False;
         --  If True, then some server is waiting for the sub-LWTs of this
         --  Group to complete.
         --  TBD: Not used currently

      Canceled : aliased Atomic_Boolean := False;
         --  Set to True when group is canceled
   end record;

   overriding
   function Cancellation_Point (Group : WS_Group) return Boolean;
   --  Return true if given group has been canceled.
   --  Upon return, if result is True, return from the LWT body procedure
   --  immediately; otherwise, continue processing.

   overriding
   procedure Wait_For_Group
     (Group : in out WS_Group;
      Canceled : out Boolean);
   --  Wait for all sub-LWTs of given Group to complete
   --  Upon return, if Canceled is True the caller
   --  should perform any actions that might
   --  have been requested by the canceling LWT,
   --  presumably recorded in a variable visible to the initiator
   --  of the LWT group.

   overriding
   procedure Cancel_Group
     (Group : in out WS_Group;
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
   procedure Finish_Group (Group : not null access WS_Group);
   --  Finish the group; called after having awaited all of the sub-LWTs

   --  An enumeration of the possible states of an LWT
   type LWT_State_Enum is
     (Unknown_State, Initial_State,
      On_Server_Queue, On_Lock_Queue, On_Delay_Queue,
      Removed_From_Queue, Ready_To_Run, Running,
      Waiting_For_Group, Completed, Final_State);

   type WS_Data is tagged;
   type WS_Data_Ptr is access all WS_Data'Class;

   type WS_Data is new LWT_Sched_Data with record
      --  Work_Stealing-relevant data
      Data : Root_Data_Ptr := null;
      State : LWT_State_Enum := Unknown_State;
      Is_Shared : Boolean := False;
      Group : WS_Group_Ptr;
      Next : WS_Data_Ptr := null;
      Prev : WS_Data_Ptr := null;
   end record;

   ------------------------
   -- LWT server info --
   ------------------------

   type Longest_Natural is range 0 .. System.Max_Int;

   Server_Stack_Size : constant := 500_000;

   --  Minimum number of servers that should be available for
   --  actively executing code.

   --  TBD: Enough_LWTs_Per_Server : constant := 2;
   --  Minimum waiting LWTs needed to be sure we have enough to support
   --  reasonable work stealing.
   --  When Length (Waiting_LWTs) is at or above this value, we will
   --  generally just run code directly rather than spawning it as a new
   --  LWT.

   subtype LWT_Ptr is WS_Data_Ptr;
   --  A LW thread, while waiting to be selected for execution,
   --  is represented by a pointer to a WS_Data object.

   --  Instantiate to produce a synchronized deque for LWT ptrs
   package Sync_LWT_Deques is new Generic_Synchronized_Deques
     (LWT_Ptr, Empty => null);

   subtype LWT_Deque_Head is Sync_LWT_Deques.Deque;
   use Sync_LWT_Deques;

   procedure Add_To_Deque
     (Deque   : in out LWT_Deque_Head;
      New_Tcb : LWT_Ptr);
   --  Add Tcb to end of deque

   procedure Spawn_LWT_On_Server
     (Group : WS_Group_Ptr;
      Server_Index  : LWT_Server_Index;
      New_Tcb       : LWT_Ptr);
   --  Spawn LWT, using server's own deque

   procedure Get_LWT_From_Server_Deque
     (Server_Index : LWT_Server_Index;
      Tcb_To_Run   : out LWT_Ptr);
   --  Get a LWT from the given server's queue.
   --  Return null Tcb_To_Run if there isn't one.

   procedure Steal_LWT
     (Server_Index        : LWT_Server_Index;
      Tcb_To_Run          : out LWT_Ptr;
      Waiting_For_Group  : WS_Group_Ptr := null);
   --  Steal a LWT from some other server's queue.
   --  Return null Tcb_To_Run if there isn't one.

   procedure Get_LWT
     (Server_Index : LWT_Server_Index;
      Tcb_To_Run   : out LWT_Ptr);
   --  Get a LWT that is waiting to be executed.
   --  Look first on specified server's queue, and
   --  then try to steal from other servers.

   procedure Bump_Active_LWT_Count;
   --  If keeping statistics, increment Num_Active and
   --  see whether we are hitting a new maximum, and keep
   --  other statistics on number waiting.

   procedure Decr_Active_LWT_Count;
   --  If keeping statistics, decrement Num_Active.

   procedure Finish_Sub_LWT
     (Server_Index : LWT_Server_Index;
      Finished_Tcb : LWT_Ptr);
   --  Indicate LWT is finished

   --  Suppress warnings about not being a dispatching op
   pragma Warnings (Off, "not dispatching");

   procedure Add_Sub_LWT (Server_Index : LWT_Server_Index;
                            Group : in out WS_Group;
                            LWT : LWT_Ptr);
   --  Add LWT to set of Sub_LWTs of given Group

   procedure Finalize_Sub_LWT (Server_Index : LWT_Server_Index;
                                 Group : in out WS_Group;
                                 LWT : LWT_Ptr);
   --  Remove LWT from set of Sub_LWTs of given Group

--    procedure Set_Waiting_Server
--      (Group : in out WS_Group;
--       Server_Index : LWT_Server_Index;
--       Dependent_Count : out LWT_Count);
   --  Indicate server wants to be notified when LWT count
   --  goes to zero (unless already zero).
   --  TBD: Not currently used

   pragma Warnings (On, "not dispatching");

   procedure Steal_LWT_Helper
     (Server_Index        : LWT_Server_Index;
      Stolen_Tcb          : out LWT_Ptr;
      Waiting_For_Group  : WS_Group_Ptr := null);
   --  Steal from some other server's queue, but steal
   --  the "oldest" rather than the "youngest" LWT.

   --  -- Layout of info for each server --  --

   type Server_Info is record
   --  Here is all the information we have for each server

      Cur_Active_LWT : LWT_Ptr := null;
      --  Active LWT, if any

      Cur_Group : WS_Group_Ptr := null;
      --  Group currently being awaited, if any

      Innermost_Group : WS_Group_Ptr := null;
      --  Innermost Group owned by this server.

      --  Deque of waiting LWTs, one for each server;
      --  available for stealing.
      Waiting_LWTs : LWT_Deque_Head;

      --  Info on team of which server is a part
      Cur_Team : Server_Team_Ptr := null;
      Team_Member : Team_Member_Index := Team_Member_Index'Last;

      --  Per-server statistics
      Num_Waiting_LWT_Initiations : Natural := 0;
      Max_Waiting_LWTs    : LWT_Count_Base := 0;
      Num_Waiting_Summed     : Longest_Natural := 0;
      Num_LWTs_In_Process          : Natural := 0;
      Max_LWTs_In_Process          : Natural := 0;
      Num_Bypassed_LWT_Initiations : Natural := 0;

   end record;

   Main_LWT_Server_Index : constant LWT_Server_Index := Cur_Server_Index;

   Server_Info_Array : array (LWT_Server_Index) of Server_Info;
   --  All of the info for each server.
   --  NOTE: We use an array of records rather than multiple arrays
   --        to minimize the likelihood of "false" sharing.
   --  TBD:  Ideally the component size of Server_Info should be a
   --        multiple of the cache-line size.

   task type LWT_Server (Prio : System.Priority;
                         Cur_Team : not null Server_Team_Ptr;
                         Team_Member : Team_Member_Index) is
      --  One of these is created for each server process, each of
      --  which serves a queue of LWTs.
      pragma Storage_Size (Server_Stack_Size);
      pragma Priority (Prio);
   end LWT_Server;

   ---------------------
   -- Image Functions --
   ---------------------

   function Hex_Image
     (Addr              : System.Address;
      Underscores_Every : Natural := 4) return String;
   --  Convert an address to a string

   function LWT_Image (LWT : LWT_Ptr) return String;
   --  Convert a LWT pointer to a string

   function Group_Image (Group : WS_Group_Ptr) return String;
   --  Convert a Group pointer to a string

   procedure Dump_One_LWT
     (Tcb    : LWT_Ptr;
      Indent : Natural := 0);
   --  Dump information on given LWT
   pragma Export (Ada, Dump_One_LWT, "dump_tcb");

   procedure Dump_LWT_State
     (Label        : String := "Dump_LWT_State");
   --  Dump state of server deques, etc.
   pragma Export (Ada, Dump_LWT_State, "dump_LWT_state");

   -------------------------
   -- LWT_Management_Data --
   -------------------------

   package LWT_Management_Data is
      --  NOTE: This is statistical data collected on LWT activity.

      Num_Parallel_Regions : aliased Atomic_Int := 0;

      -- Statistics --
      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      Num_Active : aliased Atomic_LWT_Count := 1;
      Max_Active : LWT_Count := 0;

      --  This counts number of Groups with the parent LWT
      --  waiting on them.
      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      --  Num_Waiting_For_Sub_LWTs : Integer := 0;
      --  Max_Waiting_For_Sub_LWTs : Natural := 0;
      --  TBD: Not currently used

      Num_LWT_Steals : Longest_Natural := 0;
      Max_Steal_Iteration_Count : Natural := 0;
      Steal_Iteration_Count_Summed : Longest_Natural := 0;
      Num_Steal_Failures : Natural := 0;

      Num_Active_Summed_Over_Initiations : Longest_Natural := 0;
      Num_Waiting_Summed_Over_Initiations : Longest_Natural := 0;

      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      Num_Active_Groups : Integer := 0;
      Max_Active_Groups : Natural := 0;

      Num_Groups : Natural := 0;

      Max_Sub_LWTs_Per_Group : LWT_Count := 0;

      type Prot_Op_Enum is
        (E_Wake_Servers,
         E_Wait_For_Work,
         E_Shut_Down);

      Num_Prot_Ops : array (Prot_Op_Enum) of Natural := (others => 0);

   end LWT_Management_Data;

   use LWT_Management_Data;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Addr              : System.Address;
      Underscores_Every : Natural := 4) return String
   --  Convert an address to a string
   is
      package Addr_IO is
        new Ada.Text_IO.Modular_IO (System.Storage_Elements.Integer_Address);

      Result         : String (1 .. 40);
      Result_With_Underscores : String (1 .. 40);
      J              : Natural;
      Between_Sharps : Boolean := False;
      Modulus        : Natural := Underscores_Every;
   begin
      if Modulus = 0 then
         --  No underscores.
         Modulus := 100;
      end if;

      Addr_IO.Put
        (Result, System.Storage_Elements.To_Integer (Addr), Base => 16);

      for I in Result'Range loop
         if Result (I) /= ' ' then
            --  Insert underscores
            J := Result_With_Underscores'Last;

            for K in reverse I .. Result'Last loop
               if Result (K) = '#' then
                  Between_Sharps := not Between_Sharps;
               end if;

               if Between_Sharps
                 and then K < Result'Last - 1
                 and then (Result'Last - K) mod Modulus = 1
               then
                  --  Time to put in an underscore
                  Result_With_Underscores (J) := '_';
                  J := J - 1;
               end if;

               Result_With_Underscores (J) := Result (K);
               J := J - 1;
            end loop;

            return Result_With_Underscores
                     (J + 1 .. Result_With_Underscores'Last);
         end if;
      end loop;

      raise Program_Error;  --  Can't be all spaces
   end Hex_Image;

   ------------------
   -- LWT_Image --
   ------------------

   function LWT_Image (LWT : LWT_Ptr) return String is
   --  Convert a LWT pointer to a string
   begin
      if LWT = null then
         return " null";
      else
         return Hex_Image (LWT.all'Address);
      end if;
   end LWT_Image;

   ------------------
   -- Group_Image --
   ------------------

   function Group_Image (Group : WS_Group_Ptr) return String is
   --  Convert a Group pointer to a string
   begin
      if Group = null then
         return " null";
      else
         return Hex_Image (Group.all'Address);
      end if;
   end Group_Image;

   ------------------------------
   -- WS_Group Operations --
   ------------------------------

   procedure Add_Sub_LWT (Server_Index : LWT_Server_Index;
                            Group : in out WS_Group;
                            LWT : LWT_Ptr) is
   --  Add LWT to set of Sub_LWTs of given Group
      pragma Unreferenced (Server_Index, LWT);
      Num_Dependents : constant LWT_Count := 1 + LWT_Count
        (LWT_Counters.Atomic_Fetch_And_Add (Group.Dependent_Count, 1));
   begin
      if Debug_Statistics then
         --  Statistics
         --  TBD: There are race conditions here ...
         if Num_Dependents = 1 then
            --  A newly-active Group
            Num_Active_Groups := Num_Active_Groups + 1;
            if Num_Active_Groups > Max_Active_Groups then
               Max_Active_Groups := Num_Active_Groups;
            end if;
         end if;
         if Num_Dependents > Max_Sub_LWTs_Per_Group then
            --  The busiest Group
            Max_Sub_LWTs_Per_Group := Num_Dependents;
         end if;
      end if;
   end Add_Sub_LWT;

   procedure Finalize_Sub_LWT (Server_Index : LWT_Server_Index;
                               Group : in out WS_Group;
                               LWT : LWT_Ptr) is
   --  Remove LWT from set of Sub_LWTs of given Group
   --  Caller has gotten a "lock" if Group is shared
      pragma Assert (Group.Dependent_Count > 0);
   begin
      --  Reclaim the storage associated with the Sub_LWT first
      --  TBD: Should be no race condition here, but better to reclaim
      --       storage before parent LWT awaiting Group might wake up.
      Reclaim_Storage (LWT.Data, Server_Index);

      --  Now indicate Group has one less Sub_LWT
      if LWT_Counters.Atomic_Fetch_And_Subtract
           (Group.Dependent_Count, 1) = 1
      then
         --  Was last dependent
--          if Group.Waiting_Server > 0 then
--             --  Notify waiting server
--             --  TBD: Group_Is_Complete (Waiting_Server);
--             Group.Waiting_Server := 0;
--             Group.Is_Being_Awaited := False;
--          end if;
         if Debug_Statistics then
            --  Statistics
            Num_Active_Groups := Num_Active_Groups - 1;
            --  TBD: This is a race condition
         end if;
      end if;

   end Finalize_Sub_LWT;

--    procedure Set_Waiting_Server
--      (Group : in out WS_Group;
--       Server_Index : LWT_Server_Index;
--       Dependent_Count : out LWT_Count) is
--    --  Indicate server wants to be notified when LWT count
--    --  goes to zero (unless already zero).
--
--       --  No other server should be waiting on this Group.
--       pragma Assert (Group.Waiting_Server = 0);
--    begin
--       Dependent_Count := LWT_Count (Group.Dependent_Count);
--       if Dependent_Count > 0 then
--          --  Remember server to be woken up
--          Group.Waiting_Server := Server_Index;
--          Group.Is_Being_Awaited := True;
--       end if;
--    end Set_Waiting_Server;
--
   ------------------
   -- Add_To_Deque --
   ------------------

   procedure Add_To_Deque
     (Deque   : in out LWT_Deque_Head;
      New_Tcb : LWT_Ptr) is
      --  Add Tcb to end of Deque
      Prior_Count : LWT_Count;
      Cur_Team : Server_Team_Info renames
        Server_Info_Array (Cur_Server_Index).Cur_Team.all;
   begin
      Push (Deque, New_Tcb);

      --  Keep total count for all deques.
      Prior_Count := LWT_Count
        (LWT_Counters.Atomic_Fetch_And_Add
          (Cur_Team.Num_LWTs_On_Team_Deques, 1));

      if Prior_Count = 0
        and then Cur_Team.Num_Members_Waiting_For_Work > 0
      then
         --  Just went from zero to one.  Time to
         --  wake up servers waiting for work to do.
         Cur_Team.Manager.Wake_Team;
      end if;

      --  Indicate LWT is on server queue
      New_Tcb.State := On_Server_Queue;
   end Add_To_Deque;

   -------------------------
   -- Spawn_LWT_On_Server --
   -------------------------

   procedure Spawn_LWT_On_Server
     (Group : WS_Group_Ptr;
      Server_Index  : LWT_Server_Index;
      New_Tcb       : LWT_Ptr)
   --  Spawn LWT, adding to server's own deque
   is
      Info         : Server_Info renames Server_Info_Array (Server_Index);
   begin
      New_Tcb.Group := Group;

      --  Initialize links
      New_Tcb.Prev := null;
      New_Tcb.Next := null;
      --  chain of waiting LWTs
      --  TBD: Might use links some day to find LWTs to kill

      if Debug_LWT then
         Put_Line
           ("Spawn_LWT for Server" &
            LWT_Server_Index'Image (Server_Index) &
            ", Group " & Group_Image (Group) &
            ", TCB at " &
            LWT_Image (New_Tcb));
      end if;

      --  Add new Tcb to Group
      Add_Sub_LWT (Server_Index, Group.all, New_Tcb);

      --  Add to server's queue of LWTs
      Add_To_Deque (Info.Waiting_LWTs, New_Tcb);

      if Debug_Statistics then
         --  Accumulate statistics
         if LWT_Count (Length (Info.Waiting_LWTs)) >
           Info.Max_Waiting_LWTs
         then
            Info.Max_Waiting_LWTs :=
              LWT_Count (Length (Info.Waiting_LWTs));
         end if;
      end if;

      if Debug_LWT then
         Dump_LWT_State
           (Label        => "Spawn_LWT for server" &
              LWT_Server_Index'Image (Server_Index));
      end if;

      --  Yield processor so LWT can be served
      --  NOTE: This is not necessary on an RTS with "true" multi-tasking
      if Yield_When_Spawning_LWT then
         if Debug_LWT then
            Put_Line (" About to delay 0.0 after spawning LWT " &
              "for Group " & Group_Image (Group));
            Flush;
         end if;
         delay 0.0;
      end if;
   end Spawn_LWT_On_Server;

   procedure Bump_Active_LWT_Count is
   --  If keeping statistics, increment Num_Active and
   --  see whether we are hitting a new maximum, and keep
   --  other statistics on number waiting.
   begin
      if Debug_Statistics then
         --  Accumulate statistics
         declare
            Cur_Team : Server_Team_Info renames
              Server_Info_Array (Cur_Server_Index).Cur_Team.all;
            Now_Active : constant LWT_Count := 1 + LWT_Count
              (LWT_Counters.Atomic_Fetch_And_Add (Num_Active, 1));
            Now_Waiting : constant LWT_Count'Base :=
              LWT_Count'Base (Cur_Team.Num_LWTs_On_Team_Deques);
         begin
            if Now_Active > Max_Active then
               Max_Active := Now_Active;
               if Debug_LWT then
                  Put_Line
                    ("Get_LWT -- new Max_Active =" &
                     Max_Active'Image);
               end if;
            end if;

            if Now_Active > 0 then
               Num_Active_Summed_Over_Initiations :=
                 Num_Active_Summed_Over_Initiations +
                   Longest_Natural (Now_Active);
            end if;

            if Now_Waiting > 0 then
               Num_Waiting_Summed_Over_Initiations :=
                 Num_Waiting_Summed_Over_Initiations +
                   Longest_Natural (Now_Waiting);
            end if;
         end;
      end if;
   end Bump_Active_LWT_Count;

   procedure Decr_Active_LWT_Count is
   --  If keeping statistics, decrement Num_Active.
   begin
      if Debug_Statistics then
         LWT_Counters.Atomic_Subtract (Num_Active, 1);  -- Decr Num_Active
      end if;
   end Decr_Active_LWT_Count;

   ----------------------------------
   -- Get_LWT_From_Server_Deque --
   ----------------------------------

   procedure Get_LWT_From_Server_Deque
     (Server_Index : LWT_Server_Index;
      Tcb_To_Run   : out LWT_Ptr) is
   --  Get a LWT from the given server's deque.
   --  Return null Tcb_To_Run if there isn't one.
      Info : Server_Info renames Server_Info_Array (Server_Index);
      Cur_Team : Server_Team_Info renames Info.Cur_Team.all;
   begin
      if Debug_LWT then
         Put_Line
           ("Get_LWT_From_Server_Deque for server" &
            LWT_Server_Index'Image (Server_Index));
         Flush;
      end if;

      Pop (Info.Waiting_LWTs, Tcb_To_Run);

      if Tcb_To_Run /= null then

         --  Keep total count for all deques.
         LWT_Counters.Atomic_Subtract (Cur_Team.Num_LWTs_On_Team_Deques, 1);

         --  Remember most recent active LWT for this server
         Info.Cur_Active_LWT := Tcb_To_Run;

         --  Indicate no longer on server queue
         Tcb_To_Run.State := Ready_To_Run;

         if Debug_Statistics then
            --  Accumulate statistics
            Info.Num_Waiting_LWT_Initiations :=
              Info.Num_Waiting_LWT_Initiations + 1;

            Info.Num_Waiting_Summed :=
              Info.Num_Waiting_Summed +
                Longest_Natural (Length (Info.Waiting_LWTs));
         end if;

         if Debug_LWT then
            declare
               Group : constant WS_Group_Ptr :=
                 Tcb_To_Run.Group;
            begin
               Put_Line
                 ("Get_LWT_From_Server_Deque for TCB at " &
                  LWT_Image (Tcb_To_Run) &
                  ", Group " &
                  Group_Image (Group));
            end;
         end if;
      end if;
   end Get_LWT_From_Server_Deque;

   ----------------
   -- Get_LWT --
   ----------------

   procedure Get_LWT
     (Server_Index : LWT_Server_Index;
      Tcb_To_Run   : out LWT_Ptr) is
   --  Get a LWT that is waiting to be executed.
   --  Look first on specified server's queue.
   begin
      --  First check for a LWT on this server's deque
      Get_LWT_From_Server_Deque (Server_Index, Tcb_To_Run);

      if Tcb_To_Run = null then
         --  No waiting LWTs on server's own queue, try to steal a LWT
         Steal_LWT (Server_Index, Tcb_To_Run);
      end if;
   end Get_LWT;

   ----------------------
   -- Finish_Sub_LWT --
   ----------------------

   procedure Finish_Sub_LWT
     (Server_Index : LWT_Server_Index;
      Finished_Tcb : LWT_Ptr)
   is
      Group : constant WS_Group_Ptr := Finished_Tcb.Group;
      Info : Server_Info renames Server_Info_Array (Server_Index);
   begin
      --  Indicate that LWT is finished
      --  Used for Sub_LWTs of both shared and unshared Groups.
      --  No lock needed.
      Finished_Tcb.State := Final_State;

      Info.Cur_Active_LWT := null;

      Finalize_Sub_LWT (Server_Index, Group.all, Finished_Tcb);

      if Debug_LWT
        or else (Debug_Kill
                  and then Group.Dependent_Count = 0)
      then
         Put_Line
           ("Finish_Sub_LWT for TCB at " &
            LWT_Image (Finished_Tcb) &
            ", server" & LWT_Server_Index'Image (Server_Index) &
            ", Group " & Group_Image (Group) &
            ", count on Group now" &
            Group.Dependent_Count'Image &
            ", num LWTs on all deques now" &
            Info.Cur_Team.Num_LWTs_On_Team_Deques'Image);
         Flush;
      end if;
   exception
      when Storage_Error =>
         --  Not much to do here
         raise;

      when E : others =>
         Put_Line (Standard_Error,
           "Finish_Sub_LWT: " &
            Ada.Exceptions.Exception_Name (E) &
            " raised.");
   end Finish_Sub_LWT;

   --------------------
   -- Need_More_LWTs --
   --------------------

   function Need_More_LWTs return Boolean;
   --  Indicates that we need more LWTs to keep all servers busy.

   function Need_More_LWTs return Boolean is
   --  Indicates that we need more LWTs to keep all servers busy.
      Cur_Team_Ptr : constant Server_Team_Ptr :=
        Server_Info_Array (Cur_Server_Index).Cur_Team;
   begin
      if Cur_Team_Ptr = null then
         return False;
      end if;

      declare
         Cur_Team : Server_Team_Info renames Cur_Team_Ptr.all;
         Num_Queued_LWTs : constant LWT_Count'Base :=
           LWT_Count'Base (Cur_Team.Num_LWTs_On_Team_Deques);
      begin
         return Num_Queued_LWTs <
             LWT_Count'Base (Statistics.Num_LWTs_Needed_Per_Server)
           or else
             (Num_Queued_LWTs <
                LWT_Count (Max_Server_Index) *
                              LWT_Count (Statistics.Num_LWTs_Needed_Per_Server)
                 and then
              Statistics.Num_Bypassed_LWT_Initiations > 2);
      end;
      --  TBD: Might need a more sophisticated way to decide!
      --       This is currently a heuristic to require more LWTs
      --       once we have managed to bypass at least one LWT initiation.
   end Need_More_LWTs;

   -------------------------
   -- Steal_LWT_Helper --
   -------------------------

   procedure Steal_LWT_Helper
     (Server_Index        : LWT_Server_Index;
      Stolen_Tcb          : out LWT_Ptr;
      Waiting_For_Group  : WS_Group_Ptr := null)
   is
      --  Steal from some other server's queue, but steal
      --  the "oldest" rather than the "youngest" LWT.
      Local_Info : Server_Info renames Server_Info_Array (Server_Index);
      Cur_Team : Server_Team_Info renames Local_Info.Cur_Team.all;
      Num_Team_Members : constant Team_Member_Index :=
        Cur_Team.Team_Array'Length;
   begin
      for Iteration in 1 .. Statistics.Max_Steal_Iterations loop
         declare
            Server_With_Max : LWT_Server_Index'Base := 0;
            Min_LWTs_Allowed : constant Natural :=
              Natural'Max
                (0, Statistics.Min_LWTs_Before_Stealing - Iteration);
            Max_LWTs_On_Server : Natural := Min_LWTs_Allowed;
            Some_Steal_Failed : Boolean := False;
         begin
            for I in 1 .. Num_Team_Members - 1 loop
               --  Find the "next" server with sufficient waiting LWTs

               if Waiting_For_Group /= null and then
                 Waiting_For_Group.Dependent_Count = 0
               then
                  --  Group is now done
                  Stolen_Tcb := null;
                  return;
               end if;

               declare
                  Other_Member : constant Team_Member_Index :=
                    (Local_Info.Team_Member + I - 1) mod Num_Team_Members + 1;
                  Other_Server : constant LWT_Server_Index_Base :=
                    Cur_Team.Team_Array (Other_Member);
                  Num_LWTs_On_Server : constant Natural :=
                    (if Other_Server = 0 then 0  --  Not filled in yet
                     else
                       Length (Server_Info_Array (Other_Server).Waiting_LWTs));
               begin
                  if Num_LWTs_On_Server > Max_LWTs_On_Server then
                     --  Remember server with max
                     Max_LWTs_On_Server := Num_LWTs_On_Server;
                     Server_With_Max := Other_Server;

                     exit when Num_LWTs_On_Server >=
                       Statistics.Enough_LWTs_Before_Stealing;
                        --  Found server with "enough" -- steal now
                  end if;
               end;
            end loop;

            --  Verify that nothing magically turned up on current
            --  server's own deque
            Get_LWT_From_Server_Deque (Server_Index, Stolen_Tcb);

            if Stolen_Tcb /= null then
               --  That is a bit of a surprise, but apparently
               --  the last time we looked some other LWT was in the
               --  middle of stealing from us.

               if Debug_Stealing then
                  Put_Line
                    (" While stealing, a LWT just showed up on server" &
                    Server_Index'Image & " deque");
               end if;

               --  Keep total count for all deques.
               LWT_Counters.Atomic_Subtract
                 (Cur_Team.Num_LWTs_On_Team_Deques, 1);

               return;
            end if;

            if Server_With_Max /= 0 then
               --  Found server with "enough" or at least with "min"
               declare
                  Steal_Failed : Boolean;
               begin
                  Steal (Server_Info_Array (Server_With_Max).Waiting_LWTs,
                    Stolen_Tcb, Steal_Failed);

                  if Stolen_Tcb /= null then
                     --  We found a server with a waiting LWT

                     --  Keep total count for all deques.
                     LWT_Counters.Atomic_Subtract
                       (Cur_Team.Num_LWTs_On_Team_Deques, 1);

                     if Debug_LWT or Debug_Stealing then
                        Put_Line
                          ("Steal_LWT from server" &
                           LWT_Server_Index'Image (Server_With_Max) &
                           " for server" &
                           LWT_Server_Index'Image (Server_Index) &
                           " on iteration" & Iteration'Image);
                     end if;

                     if Debug_Statistics then
                        --  Statistics
                        Num_LWT_Steals := Num_LWT_Steals + 1;

                        if Iteration > Max_Steal_Iteration_Count then
                           Max_Steal_Iteration_Count := Iteration;
                        end if;

                        Steal_Iteration_Count_Summed :=
                          Steal_Iteration_Count_Summed +
                            Longest_Natural (Iteration);
                     end if;

                     return;   -- All done --

                  end if;
                  --  Remember whether some steal failed
                  Some_Steal_Failed := Some_Steal_Failed or Steal_Failed;
               end;
            end if;

            if Waiting_For_Group /= null and then
              Waiting_For_Group.Dependent_Count = 0
            then
               --  Group is now done
               Stolen_Tcb := null;
               return;
            end if;

            --  Nothing to steal, delay a bit
            if not Cur_Team.Team_Is_Shut_Down then
               declare
                  --  Compute an appropriate delay amount
                  Delay_Amount : constant Duration :=
                    Statistics.Steal_Failure_Delay *
                      (Iteration * 3 + Natural (Local_Info.Team_Member)) / 4;
--                     (if Some_Steal_Failed and then Iteration = 1 then 0.0
--                      else Statistics.Steal_Failure_Delay *
--                       (Iteration * 3 + Natural (Server_Index)) / 4);
               begin
                  if Debug_LWT or Debug_Stealing then
                     Put_Line (" Server" & Server_Index'Image &
                       " has nothing to steal (Min-allowed =" &
                       Min_LWTs_Allowed'Image & "), will delay" &
                       Delay_Amount'Image & ";" &
                       Cur_Team.Num_LWTs_On_Team_Deques'Image &
                       " LWTs on all deques");
                     Dump_LWT_State (Label =>
                       "Nothing to steal");
                  end if;
                  delay Delay_Amount;
               end;
            end if;

            --  Don't loop around after delaying when waiting for a Group
            --  unless some steal failed due to a synchronization collision.
            exit when Waiting_For_Group /= null;
--               and then not Some_Steal_Failed;
         end;
      end loop;

      if Debug_Statistics then
         if Waiting_For_Group = null then
            Num_Steal_Failures := Num_Steal_Failures + 1;
         end if;
      end if;

      if Debug_Stealing or Debug_LWT then
         Put_Line (" Nothing to steal in Steal_LWT_Helper; LWT tree:");
         Dump_LWT_State (Label =>
           "Current LWT tree in Steal_LWT" &
              (if Waiting_For_Group /= null
               then " while waiting for Group " &
                       Group_Image (Waiting_For_Group)
               else ""));
      end if;

      Stolen_Tcb := null;
   end Steal_LWT_Helper;

   ------------------
   -- Steal_LWT --
   ------------------

   procedure Steal_LWT
     (Server_Index        : LWT_Server_Index;
      Tcb_To_Run          : out LWT_Ptr;
      Waiting_For_Group  : WS_Group_Ptr := null)
   is
      Info          : Server_Info renames Server_Info_Array (Server_Index);
   begin
      --  Steal from some other server's queue, but steal
      --  the "oldest" rather than the "youngest" LWT.
      Steal_LWT_Helper
        (Server_Index,
         Tcb_To_Run,
         Waiting_For_Group => Waiting_For_Group);

      if Tcb_To_Run = null then
         return;
      end if;

      --  Remember most recent active LWT for this server
      Info.Cur_Active_LWT := Tcb_To_Run;

      if Debug_LWT then
         declare
            Group : constant WS_Group_Ptr :=
              Tcb_To_Run.Group;
         begin
            Put_Line
              ("Get_LWT for TCB at " &
               LWT_Image (Tcb_To_Run) &
               ", Group " &
               Group_Image (Group));
         end;
      end if;

      --  Indicate no longer on server queue
      Tcb_To_Run.State := Ready_To_Run;

   end Steal_LWT;

   ----------------------
   -- Wait_For_LWTs --
   ----------------------

   procedure Wait_For_LWTs
     (Group    : WS_Group_Ptr;
      Server_Index     : LWT_Server_Index);
   --  Wait for Group Sub_LWT count to go to zero;
   --  service other runnable LWTs while waiting.

   procedure Wait_For_LWTs
     (Group    : WS_Group_Ptr;
      Server_Index     : LWT_Server_Index) is
      Info : Server_Info renames Server_Info_Array (Server_Index);

   begin
      --  Wait for Group Sub_LWT count to go to zero;
      --  service other runnable LWTs while waiting.

      --  Indicate LWT state
      --  TBD: Tcb_Waiting.State := Waiting_For_Group;

      if Info.Innermost_Group /= Group then
         --  Must have never awaited the innermost Group,
         --  perhaps because an exception was raised.  Mark
         --  this as the innermost Group.
         if Debug_LWT or Debug_Stealing then
            Put_Line ("Innermost Group never awaited: " &
                      Group_Image (Info.Innermost_Group));
         end if;

         --  This is now the innermost Group that is still active
         Info.Innermost_Group := Group;
      end if;

      --  LWT not considered active while waiting for Sub_LWTs
      Decr_Active_LWT_Count;  --  Decr Num_Active

      --  Wait for LWTs, or get one to serve
      while Group.Dependent_Count > 0 loop
         --  Group has unfinished Sub_LWTs

         declare
            Tcb_To_Run : LWT_Ptr := null;
         begin

            --  See if there are any LWTs on this server's deque
            Get_LWT_From_Server_Deque (Server_Index, Tcb_To_Run);

            if Tcb_To_Run = null then
               --  There are no LWTs on this server's deque,
               --  so use the Team_Manager.
               if Debug_Stealing then
                  Put_Line (" No local LWTs while server" &
                    Server_Index'Image & " waiting for Group " &
                    Group_Image (Group));
               end if;

               exit when Group.Dependent_Count = 0;
                  --  Group complete

               --  No LWTs on own deque, so try to steal

               if Debug_Stealing then
                  Put_Line (" About to steal while Server" &
                    Server_Index'Image & " is waiting for Group " &
                    Group_Image (Group) & " with" &
                    Group.Dependent_Count'Image &
                    " Sub_LWTs still active");
               end if;

               Steal_LWT
                 (Server_Index,
                  Tcb_To_Run,
                  Waiting_For_Group => Group);
            end if;

            if Tcb_To_Run /= null then
               --  We have a LWT to serve while waiting for Group
               declare
                  WS_Group_Of_LWT : constant WS_Group_Ptr :=
                    Tcb_To_Run.Group;
               begin
                  if Debug_LWT then
                     Put_Line
                       ("Wait_For_LWTs (Group " &
                        Group_Image (Group) &
                        "): serving a TCB at " &
                        LWT_Image (Tcb_To_Run) &
                        ", LWT's Group =" &
                        Group_Image (WS_Group_Of_LWT));
                  end if;

                  pragma Assert
                    (WS_Group_Of_LWT.Dependent_Count > 0);

                  --  Perform processing for LWT
                  Bump_Active_LWT_Count;  --  Bump Num_Active

                  Tcb_To_Run.State := Running;

                  if not WS_Group_Of_LWT.Cancellation_Point then
                     --  Do not start LWT if group has been canceled.
                     begin  --  catch exceptions
                        LWT_Body (Tcb_To_Run.Data.all);
                     exception
                        when E : others =>
                           Put_Line (Standard_Error,
                             "Internal: " &
                              "Wait_For_LWTs: " &
                              Ada.Exceptions.Exception_Name (E) &
                              " raised.");
                           --  Fall through to finish LWT
                           --  TBD: Cancel group
                     end;
                  end if;

                  Tcb_To_Run.State := Completed;

                  pragma Assert
                    (WS_Group_Of_LWT.Dependent_Count > 0);

               end;

               Decr_Active_LWT_Count;  --  Decr Num_Active

               --  Indicate we are done with LWT.
               Finish_Sub_LWT (Server_Index, Tcb_To_Run);
            end if;
         end;
      end loop;
      --  LWTs associated with given Group now complete

--       if Debug_Statistics then
--          --  Statistics
--          if Group.Is_Being_Awaited then
--             Num_Waiting_For_Sub_LWTs :=
--               Num_Waiting_For_Sub_LWTs - 1;
--          end if;
--       end if;

      if Debug_LWT or Debug_Kill then
         Put_Line
           (" For server" &
            Server_Index'Image &
            ", Group " &
            Group_Image (Group) &
            " is complete, Num_Active_Groups now" &
            Num_Active_Groups'Image);

         if Debug_LWT then
            Dump_LWT_State (Label =>
              "Current LWT tree in ");
         end if;
      end if;

      --  LWT considered active again after waiting for Sub_LWTs
      if Debug_Statistics then
         declare
            Now_Active : constant LWT_Count := 1 + LWT_Count
              (LWT_Counters.Atomic_Fetch_And_Add (Num_Active, 1));
         begin
            if Now_Active > Max_Active then
               Max_Active := Now_Active;
               if Debug_LWT then
                  Put_Line
                    ("Get_LWT -- new Max_Active =" &
                     Max_Active'Image);
               end if;
            end if;
         end;
      end if;

      if Info.Innermost_Group /= Group then
         --  We check again to see if the Innermost_Group field
         --  was updated and not reset, again
         --  perhaps because an exception was raised.
         if Debug_LWT or Debug_Stealing then
            Put_Line ("Innermost Group never awaited (2): " &
                      Group_Image (Info.Innermost_Group));
         end if;

      end if;

      --  Set new innermost Group (might be null)
      Info.Innermost_Group := Group.Enclosing_Group;

      --  Indicate LWT is running again
      --  TBD: Tcb_Waiting.State := Running;
   end Wait_For_LWTs;

   ------------------
   -- Team_Manager --
   ------------------

   protected body Team_Manager is
      --  This protected object manages the creating and
      --  serving of LW ("pico") threads within a given team.
      --  TBD: Finer-grained locking might be useful someday.

      --  Suppress warnings about use of globals in barriers
      pragma Warnings (Off, "potentially unsynchronized barrier");

      --  ---------- visible operations ---------------  --

      ------------------
      -- Wake_Servers --
      ------------------

      procedure Wake_Team is
      --  Num_LWTs_On_Team_Deques just went from zero to one,
      --  so there might be some servers waiting for work to do.
      begin
         if Debug_Statistics then
            Num_Prot_Ops (E_Wake_Servers) :=
              Num_Prot_Ops (E_Wake_Servers) + 1;
         end if;
      end Wake_Team;

      -------------------
      -- Wait_For_Work --
      -------------------

      entry Wait_For_Work (Server_Index : LWT_Server_Index)
      --  A server found there was nothing to do; wait for new work
        when Team_Info.Team_Is_Shut_Down
          or else Team_Info.Num_LWTs_On_Team_Deques > 0
      is
         pragma Unreferenced (Server_Index);
      begin
         --  Nothing to do other than wake up calling server
         if Debug_Statistics then
            Num_Prot_Ops (E_Wait_For_Work) :=
              Num_Prot_Ops (E_Wait_For_Work) + 1;
         end if;
      end Wait_For_Work;

      --------------------
      -- Shut_Down_Team --
      --------------------

      entry Shut_Down_Team when Standard.True is
      --  Shut down the team serving the current parallel region
         pragma Assert (Team_Info.Num_LWTs_On_Team_Deques = 0);
         --  Shouldn't be any LWTs waiting
      begin
         if Debug_Statistics then
            Num_Prot_Ops (E_Shut_Down) := Num_Prot_Ops (E_Shut_Down) + 1;
         end if;
         Team_Info.Team_Is_Shut_Down := True;
         requeue Wait_For_Members_To_Shut_Down;
      end Shut_Down_Team;

      -------------------
      -- Shutting_Down --
      -------------------

      procedure Shutting_Down (Team_Member : Team_Member_Index) is
      --  Called when a team member is shutting down
         pragma Unreferenced (Team_Member);
      begin
         --  Keep count of those shutting down
         Num_Shut_Down := Num_Shut_Down + 1;
      end Shutting_Down;

      -----------------------------------
      -- Wait_For_Members_To_Shut_Down --
      -----------------------------------

      entry Wait_For_Members_To_Shut_Down
        when Team_Info.Team_Array = null
          or else Num_Shut_Down >= Team_Info.Team_Array'Length - 1
      is
      --  Requeue on this entry after shutting down the team
      --  to wait for all servers of team to complete.
      begin
         null;
      end Wait_For_Members_To_Shut_Down;

      pragma Warnings (On, "potentially unsynchronized barrier");

   end Team_Manager;

   ----------------
   -- LWT_Server --
   ----------------

   task body LWT_Server is
      --  One of these is created for each server process, each of
      --  which serves a queue of LWTs.
      My_Index        : constant LWT_Server_Index := Cur_Server_Index;

      Info : Server_Info renames Server_Info_Array (My_Index);

      Exception_Count : Natural := 0;
      Exception_Limit : constant := 3;  --  Don't allow too many exceptions

   begin --  LWT_Server

      --  Remember Ada task identity
      Associated_Ada_Task_Ptr := Cur_Team.Associated_Ada_Task'Unchecked_Access;

      --  Fill in team info
      Info.Team_Member := Team_Member;
      Info.Cur_Team := Cur_Team;

      while not Cur_Team.Team_Is_Shut_Down loop
         --  Keep trying until we are shut down

         loop  --  Now loop getting LWTs and servicing them
            declare
               Tcb_To_Run : LWT_Ptr := null;
            begin
               --  Get a LWT to serve
               Get_LWT (My_Index, Tcb_To_Run);

               exit when Tcb_To_Run = null;
               --  Null indicates Shut_Down or nothing to steal

               begin  --  catch exceptions

                  --  TCB should be included in its Group's count
                  pragma Assert
                    (Tcb_To_Run.Group.Dependent_Count > 0);

                  --  Perform processing for LWT
                  Bump_Active_LWT_Count;  --  Bump Num_Active

                  Tcb_To_Run.State := Running;
                  if not Tcb_To_Run.Group.Cancellation_Point then
                     --  Run the body, unless group has been canceled.
                     LWT_Body (Tcb_To_Run.Data.all);
                  end if;
                  Tcb_To_Run.State := Completed;

                  pragma Assert
                    (Tcb_To_Run.Group.Dependent_Count > 0);

                  --  We completed some processing exception free, so reset
                  --  counter.
                  Exception_Count := 0;

               exception
                  when E : others =>
                     Put_Line (Standard_Error,
                       "Internal: " &
                        "LWT_Server" &
                        LWT_Server_Index'Image (My_Index) &
                        ": " &
                        Ada.Exceptions.Exception_Information (E));

                     Exception_Count := Exception_Count + 1;
                     if Exception_Count > Exception_Limit then
                        Put_Line (Standard_Error,
                          "Internal: " &
                           "LWT_Server" &
                           LWT_Server_Index'Image (My_Index) &
                           ": Shutting down because of" &
                           Natural'Image (Exception_Count) &
                           " consecutive exceptions.");

                        Decr_Active_LWT_Count;  --  Decr Num_Active

                        exit;  --  exit LWT server  ---
                     end if;

                     --  No need to propagate after reporting problem.
                     --  Fall through to finish LWT that raised an exception
               end;

               Decr_Active_LWT_Count;  --  Decr Num_Active

               --  Add per-server statistics into server-info array
               Server_Info_Array (My_Index).Num_Bypassed_LWT_Initiations :=
                 Server_Info_Array (My_Index).Num_Bypassed_LWT_Initiations +
                 Statistics.Num_Bypassed_LWT_Initiations;

               Statistics.Num_Bypassed_LWT_Initiations := 0;
                  --  Reset LWT-local storage version

               --  Indicate we are done with LWT.
               Finish_Sub_LWT (My_Index, Tcb_To_Run);
            end;

         end loop;

         --  Things seem pretty quiet -- wait for work and loop around

         LWT_Counters.Atomic_Add (Cur_Team.Num_Members_Waiting_For_Work, 1);
         Cur_Team.Manager.Wait_For_Work (My_Index);
         LWT_Counters.Atomic_Subtract
           (Cur_Team.Num_Members_Waiting_For_Work, 1);
      end loop;

      if Debug_LWT or Debug_Stealing then
         Put_Line (" Server" & My_Index'Image & " is exiting;" &
           Cur_Team.Num_LWTs_On_Team_Deques'Image & " LWTs on team deques");
      end if;

      --  Indicate server is shutting down
      Cur_Team.Manager.Shutting_Down (Team_Member);

      --  Null out Ada task identity
      Associated_Ada_Task_Ptr := null;
   end LWT_Server;

   type LWT_Server_Ptr is access LWT_Server;

   ---------------
   -- Spawn_LWT --
   ---------------

   procedure Spawn_LWT
     (Group : in out WS_Group;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null);

   procedure Spawn_LWT
     (Group : in out WS_Group;
      Data : access Root_Data'Class;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null) is
      pragma Unreferenced (Aspects);

      Cur_Server : constant LWT_Server_Index := Cur_Server_Index;
      New_LWT : constant LWT_Ptr := new WS_Data'
        (Data => Data.all'Unchecked_Access,
         State => Initial_State,
         Is_Shared => False,
         Group => Group'Unchecked_Access,
         Next | Prev => null);
   begin
      --  TBD: Use Aspects to control any options

      --  Point at WS_Data
      Data.Sched_Data := LWT_Sched_Data_Ptr (New_LWT);

      --  Spawn LWT on current server
      Spawn_LWT_On_Server (Group'Unchecked_Access, Cur_Server, New_LWT);
   end Spawn_LWT;

   overriding
   function Cancellation_Point (Group : WS_Group) return Boolean is
   begin
      return Boolean (Group.Canceled);
   end Cancellation_Point;
   --  Return true if given group has been canceled.
   --  Upon return, if result is True, return from the LWT body procedure
   --  immediately; otherwise, continue processing.

   overriding
   procedure Cancel_Group
     (Group : in out WS_Group;
      Success : out Boolean) is
   --  Cancel the specified group.
   --  If Success is True on return, this was the first LWT to request
   --  cancellation, and it now has a chance to set up for some special
   --  action when the LWT group completes (e.g. return from the
   --  enclosing subprogram) by setting some variable visible to
   --  the initiator of the LWT group.
   --  In any case, upon return from this call the LWT should return
   --  from the LWT body procedure.
   begin
      if Debug_LWT then
         Put_Line (" Before Cancel_Group, Canceled = " & Group.Canceled'Image);
      end if;
      --  Set the Group.Canceled flag True,
      --  and set Success to true if prior value was False.
      Success :=
        not Boolean (Boolean_Max_Min.Update_Max (Group.Canceled, True));
      if Debug_LWT then
         Put_Line (" After Cancel_Group, Success = " & Success'Image &
           ", Canceled = " & Group.Canceled'Image);
      end if;

      if Success then
         --  Cancel all inner groups as well
         declare
            Info : Server_Info renames Server_Info_Array (Cur_Server_Index);
            Inner_Group : WS_Group_Ptr := Info.Cur_Active_LWT.Group;
         begin
            while Inner_Group /= Group'Unchecked_Access
              and then Inner_Group /= null
            loop
               --  We just set it to True, and we don't care whether or
               --  not we are the first to do so.
               Inner_Group.Canceled := True;

               --  Go to the next outer group
               Inner_Group := Inner_Group.Enclosing_Group;
               --  TBD: We are canceling all of the groups enclosing
               --       the current LWT up to the specified group,
               --       but we aren't worrying about "sibling" groups
               --       at this point.
            end loop;
         end;
      end if;
   end Cancel_Group;

   ---------------------
   -- Wait_For_Group --
   ---------------------

   overriding
   procedure Wait_For_Group
     (Group : in out WS_Group;
      Canceled : out Boolean) is
   --  Wait for all sub-LWTs of given Group to complete
   --  Upon return, if Canceled is True the caller
   --  should perform any actions that might
   --  have been requested by the canceling LWT,
   --  presumably recorded in a variable visible to the initiator
   --  of the LWT group.

      Cur_Server : constant LWT_Server_Index := Cur_Server_Index;
   begin
      --  Wait for LWTs of Group to finish
      Wait_For_LWTs (Group'Unchecked_Access, Cur_Server);

      --  Should be no remaining Sub_LWTs upon return
      pragma Assert (Group.Dependent_Count = 0);

      Canceled := Boolean (Group.Canceled);
   end Wait_For_Group;

   overriding
   procedure Finish_Group (Group : not null access WS_Group) is
   --  Finish the group; called after having awaited all of the sub-LWTs
      Group_Ptr : WS_Group_Ptr := WS_Group_Ptr (Group);
      pragma Unreferenced (Group_Ptr);
   begin
      null;  --  TBD: Unchecked-deallocate the group
   end Finish_Group;

   ------------------------------
   -- Debugging and Statistics --
   ------------------------------

   ---------------------
   -- Dump_One_LWT --
   ---------------------

   procedure Dump_One_LWT
     (Tcb    : LWT_Ptr;
      Indent : Natural := 0)
   is
      Group : constant WS_Group_Ptr :=
                        Tcb.Group;
      Ind_Str : constant String (1 .. Indent) := (others => ' ');
   begin
      Put_Line (Ind_Str & " TCB = " & LWT_Image (Tcb));
      Put (Ind_Str &
        "  State = " & LWT_State_Enum'Image (Tcb.State));
      New_Line;

      Put_Line (Ind_Str &
        "  Group = " & Group_Image (Group));
      Flush;
   end Dump_One_LWT;

   -----------------------
   -- Dump_LWT_State --
   -----------------------

   procedure Dump_LWT_State
     (Label        : String := "Dump_LWT_State")
   is
      --  Num_Free_Groups  : Natural := 0;

      Cur_Server : constant LWT_Server_Index := Cur_Server_Index;
      Cur_Server_Info : Server_Info renames Server_Info_Array (Cur_Server);
      Cur_Team : Server_Team_Info renames Cur_Server_Info.Cur_Team.all;
   begin
      if Label /= "" then
         Put_Line (' ' & Label & ':');
      end if;

      Put_Line ("Num_Active_LWTs =" & Num_Active'Image);
      Put ("Cur_Team.Num_LWTs_On_Team_Deques =" &
        Cur_Team.Num_LWTs_On_Team_Deques'Image & "(");
      for I in Server_Info_Array'First .. Max_Server_Index loop
         Put (Length (Server_Info_Array (I).Waiting_LWTs)'Image);
      end loop;
      Put_Line (")");

      Put_Line
        ("Num_Parallel_Regions =" & Num_Parallel_Regions'Image);

      Put_Line
        ("Num_Active_Groups =" & Num_Active_Groups'Image);

      if not Debug_LWT then
         Put_Line ("Dump_LWT_State (shortened) finished.");
         Flush;
         return;
      end if;

      for I in Server_Info_Array'First .. Max_Server_Index loop
         declare
            Info    : Server_Info renames Server_Info_Array (I);
            Current : constant LWT_Ptr := Info.Cur_Active_LWT;

         begin
            if I = Main_LWT_Server_Index
              or else Current /= null
              or else
                not Is_Empty (Info.Waiting_LWTs)
            then
               --  We have one or more LWTs associated
               --  with this server
               if I = Cur_Server then
                  Put ("*Current* ");
               end if;
               Put_Line ("Server" & LWT_Server_Index'Image (I) & ':');

               if Current /= null then
                  Put_Line (" Active LWT:");
                  Dump_One_LWT (Current);
               elsif I = Main_LWT_Server_Index then
                  Put_Line (" Main LWT");
               else
                  Put_Line (" No active LWT");
               end if;

               --  Now dump queue(s) of waiting LWTs
               declare
                  Deque_Elements : constant Element_Vector :=
                    All_Elements (Info.Waiting_LWTs);
                  Max_To_Show : constant := 5;  -- only show 5 tcbs
               begin
                  if Deque_Elements'Length > 0 then
                     Put_Line
                       (" Waiting LWTs:");
                  end if;

                  for I in Deque_Elements'Range loop
                     if I > Max_To_Show then
                        Put_Line (" ... [omitting" &
                          Natural'Image
                           (Deque_Elements'Last - I + 1) & "]");
                        exit;
                     end if;
                     Dump_One_LWT (Deque_Elements (I));
                  end loop;
               end;
            end if;
         end;
      end loop;
      Put_Line ("Dump_LWT_State finished.");
      Flush;
   exception
      when Storage_Error =>
         --  Not much to do here
         raise;

      when E : others =>
         Flush;
         Put_Line ("Exception raised in Dump_LWT_State: " &
           Ada.Exceptions.Exception_Information (E));
         Flush;
   end Dump_LWT_State;

   ------------------
   -- Show_Servers --
   ------------------

   procedure Show_Servers is
   --  Show number of servers and other information about servers.
   begin
      Put_Line (" Max_LWT_Servers :" & Max_LWT_Servers'Image);
      Put_Line (" Max_Server_Index :" & Max_Server_Index'Image);
   end Show_Servers;

   ----------------
   -- Show_Stats --
   ----------------

   procedure Show_Stats (Clear : Boolean := False) is
      Total_Waiting_LWT_Initiations    : Longest_Natural := 0;
      Overall_Max_Waiting_LWTs : LWT_Count_Base := 0;
      Overall_Summed_Unstolen_Waiting      : Longest_Natural := 0;
      Total_Bypassed_LWT_Initiations    : Longest_Natural := 0;
   begin
      --  Aggregate the statistics over the servers
      for I in 1 .. Max_Server_Index loop
         declare
            Info : Server_Info renames Server_Info_Array (I);
         begin
            Total_Waiting_LWT_Initiations :=
              Total_Waiting_LWT_Initiations +
                Longest_Natural (Info.Num_Waiting_LWT_Initiations);

            Total_Bypassed_LWT_Initiations :=
              Total_Bypassed_LWT_Initiations +
                Longest_Natural (Info.Num_Bypassed_LWT_Initiations);

            if Info.Max_Waiting_LWTs >
              Overall_Max_Waiting_LWTs
            then
               Overall_Max_Waiting_LWTs :=
                 Info.Max_Waiting_LWTs;
            end if;

            Overall_Summed_Unstolen_Waiting :=
              Overall_Summed_Unstolen_Waiting +
                Info.Num_Waiting_Summed;
         end;
      end loop;

      New_Line;
      declare
         Sum_Num_Prot_Ops : Natural := 0;
      begin
         for NP of Num_Prot_Ops loop
            Sum_Num_Prot_Ops := Sum_Num_Prot_Ops + NP;
         end loop;
         Put_Line ("Num protected ops:" & Natural'Image (Sum_Num_Prot_Ops));
         for E in Num_Prot_Ops'Range loop
            Put_Line (" " & Prot_Op_Enum'Image (E) & ":" &
              Natural'Image (Num_Prot_Ops (E)));
         end loop;
      end;

      Put_Line ("LWT Statistics:");

      if Num_LWT_Steals = 0 then
         --  Prevent a divide-by-zero error if statistics reset
         Num_LWT_Steals := 1;
      end if;

      if Total_Waiting_LWT_Initiations = 0 then
         --  Prevent a divide-by-zero error if statistics reset
         Total_Waiting_LWT_Initiations := 1;
      end if;

      declare
         Average_Waiting : constant Long_Float :=
           Long_Float (Num_Waiting_Summed_Over_Initiations) /
             Long_Float (Num_LWT_Steals + Total_Waiting_LWT_Initiations);
         Average_Steal_Iterations : constant Long_Float :=
           Long_Float (Steal_Iteration_Count_Summed) /
             Long_Float (Num_LWT_Steals);
         Average_Unstolen_Waiting : constant Long_Float :=
           Long_Float (Overall_Summed_Unstolen_Waiting) /
             Long_Float (Total_Waiting_LWT_Initiations);
         Average_Active : constant Long_Float :=
           Long_Float (Num_Active_Summed_Over_Initiations) /
             Long_Float (Num_LWT_Steals + Total_Waiting_LWT_Initiations);
      begin
         Put_Line
           (" Max_Waiting_LWTs (on any one server's queue):" &
            LWT_Count'Image (Overall_Max_Waiting_LWTs));
         Put (" Average waiting LWTs at initiation: ");
         Ada.Long_Float_Text_IO.Put (Average_Waiting,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put (" Average waiting unstolen LWTs: ");
         Ada.Long_Float_Text_IO.Put (Average_Unstolen_Waiting,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put_Line (" Max_Active (LWTs):" & Max_Active'Image);
         Put (" Average active LWTs: ");
         Ada.Long_Float_Text_IO.Put (Average_Active,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put (" Max and Average iterations before successful steal: " &
           Max_Steal_Iteration_Count'Image & ", ");
         Ada.Long_Float_Text_IO.Put (Average_Steal_Iterations,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
      end;
      Put_Line
        (" Num_LWT_Steals :" &
         Num_LWT_Steals'Image & " out of" &
         Longest_Natural'Image (Total_Waiting_LWT_Initiations) &
         " +" & Num_LWT_Steals'Image &
         " (U+S) LWT initiations =" &
         Longest_Natural'Image (Num_LWT_Steals * 100 /
           (Total_Waiting_LWT_Initiations +
             Num_LWT_Steals))
         & "%");
      Put_Line
        (" Num_Steal_Failures :" & Num_Steal_Failures'Image);

      Put_Line
        (" Max_Active_Groups :" & Natural'Image (Max_Active_Groups));
      Put_Line
        (" Max_Sub_LWTs_Per_Group :" &
         LWT_Count'Image (Max_Sub_LWTs_Per_Group));
      --  Put_Line
      --    (" Max_Waiting_For_Sub_LWTs :" &
      --     Natural'Image (Max_Waiting_For_Sub_LWTs));
      Put_Line
        (" Num_Groups :" &
         Natural'Image (Num_Groups));
      Put_Line
        (" Num_Bypassed_LWT_Initiations :" &
           Longest_Natural'Image (Total_Bypassed_LWT_Initiations) &
           " (" &
           Longest_Natural'Image (Total_Bypassed_LWT_Initiations * 100 /
             (Total_Bypassed_LWT_Initiations +
              Total_Waiting_LWT_Initiations +
              Num_LWT_Steals)) & "%)");
      New_Line;

      if Clear then
         --  Reset statistics
         Max_Active := LWT_Count_Base (Num_Active);
         Num_Waiting_Summed_Over_Initiations := 0;
         Num_Active_Summed_Over_Initiations := 0;
         --  Max_Waiting_For_Sub_LWTs := Num_Waiting_For_Sub_LWTs;
         if Num_Active_Groups > 0 then
            Max_Active_Groups := Num_Active_Groups;
         else
            Max_Active_Groups := 0;
         end if;
         Num_LWT_Steals := 0;
         Max_Sub_LWTs_Per_Group := 0;

         --  Reset the statistics for each server
         for I in 1 .. Max_Server_Index loop
            declare
               Info : Server_Info renames Server_Info_Array (I);
            begin
               Info.Num_Waiting_LWT_Initiations := 0;
               Info.Max_Waiting_LWTs    := 0;
               Info.Num_Waiting_Summed     := 0;
               Info.Num_Bypassed_LWT_Initiations := 0;
            end;
         end loop;

      end if;
   end Show_Stats;

   --  --  Plugin primitive operation specs  --  --

   type WS_Plugin_Type is
     new LWT.Scheduler.LWT_Scheduler_Plugin with null record;

   overriding
   procedure Spawn_LWT
     (Plugin : in out WS_Plugin_Type;
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
     (Plugin : in out WS_Plugin_Type;
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
   function Need_More_LWTs (Plugin : WS_Plugin_Type) return Boolean;
   --  Indicates that we need more LWTs to keep all servers busy.

   overriding
   function Num_Servers_Available
     (Plugin : WS_Plugin_Type) return LWT_Server_Index;
   --  Return number of servers available to current Ada task
   --  for serving LWTs.

   overriding
   function Ada_Task_Identity (Plugin : WS_Plugin_Type)
     return Ada.Task_Identification.Task_Id;
   --  Returns the Ada Task_Id associated with the current LWT server

   overriding
   function Create_Group
     (Plugin : in out WS_Plugin_Type;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr;
   --  Create a scheduler-specific LWT group

   --  --  Plugin primitive operation bodies  --  --

   overriding
   procedure Spawn_LWT
     (Plugin : in out WS_Plugin_Type;
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
   begin
      Spawn_LWT
        (Group => WS_Group_Ptr (LWT_Groups.Sched_Group (Data.Group)).all,
         Data => Data,
         Aspects => Aspects);
   end Spawn_LWT;

   overriding
   procedure LWT_Range_Loop
     (Plugin : in out WS_Plugin_Type;
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

      type Loop_Data_Ptr is access all Range_Loop.Range_Loop_Data'Class;
      --  TBD:  with Storage_Pool => LWT.Storage.LWT_Storage_Pool_Obj;
         --  Local access type needed because Data might be of
         --  a type that is a local extension of Range_Loop_Data.
         --  Use global storage pool designed for re-using LWT data.
         --  TBD: GNAT chokes on the above declaration when a storage-pool
         --       is specified.

      --  procedure Free is new Ada.Unchecked_Deallocation
      --    (Range_Loop.Range_Loop_Data'Class, Loop_Data_Ptr);
         --  This will call the deallocate routine for LWT_Storage_Pool
         --  TBD: But not until the GNAT bug is fixed...

      Canceled : Boolean := False;
      Num_Chunks : Long_Integer := Long_Integer (Data.Num_Chunks);
      Length : constant Long_Integer := Data.Last - Data.First + 1;
      Group : aliased LWT_Group;
   begin
      if Num_Chunks = 0 then
         Num_Chunks := Long_Integer (Max_Server_Index);
      end if;
      if Num_Chunks > Length then
         Num_Chunks := Length;
      end if;
      if Num_Chunks <= 1 or else not Need_More_LWTs then
         --  Just execute loop body with full range
         --  after filling in the group info.
         declare
            Loop_Data : Range_Loop.Range_Loop_Data'Class := Data;
         begin
            Loop_Data.Group := Group'Unchecked_Access;
            Range_Loop.LWT_Body (Loop_Data);
         end;
      else
         declare
            Chunk_Size : constant Long_Integer := Length / Num_Chunks;
            Extra : constant Long_Integer := Length mod Num_Chunks;
            First : Long_Integer := Data.First;
         begin
            for I in 1 .. Num_Chunks loop
               declare
                  Chunk_Data : constant Loop_Data_Ptr :=
                    new Range_Loop.Range_Loop_Data'Class'(Data);
               begin
                  Chunk_Data.First := First;
                  if I <= Extra then
                     --  Use the ceiling of the chunk size
                     Chunk_Data.Last := First + Chunk_Size;
                  else
                     --  Use the floor of the chunk size
                     Chunk_Data.Last := First + Chunk_Size - 1;
                  end if;
                  Chunk_Data.Chunk_Index := Positive (I);

                  --  Remember where to start the next chunk
                  First := Chunk_Data.Last + 1;

                  --  Quit early if group has been canceled.
                  exit when Group.Cancellation_Point;

                  --  Actually spawn the LWT for this chunk
                  --  TBD: If I = Num_Chunks we could just call LWT_Body
                  if Debug_LWT then
                     Put_Line ("LWT_Range_Loop, spawning for First =" &
                       Chunk_Data.First'Image & ", Last =" &
                       Chunk_Data.Last'Image & ", Chunk_Index =" &
                       Chunk_Data.Chunk_Index'Image);
                  end if;

                  Spawn_LWT
                    (Group, Chunk_Data.all'Unchecked_Access, Aspects);
               end;
            end loop;
         end;
      end if;
      --  Now wait for all of the LW threads to complete
      Group.Wait_For_Group (Canceled);
   end LWT_Range_Loop;

   overriding
   function Need_More_LWTs (Plugin : WS_Plugin_Type) return Boolean is
   --  Indicates that we need more LWTs to keep all servers busy.
   begin
      return Need_More_LWTs;
   end Need_More_LWTs;

   overriding
   function Num_Servers_Available
     (Plugin : WS_Plugin_Type) return LWT_Server_Index is
   --  Return number of servers available to current Ada task
   --  for serving LWTs.
      Server_Index : constant LWT_Server_Index := Cur_Server_Index;
      Info : Server_Info renames Server_Info_Array (Server_Index);
   begin
      if Info.Cur_Team = null
        or else Info.Cur_Team.Team_Array = null
      then
         --  Not part of a team, or team-array not yet allocated
         return 1;
      else
         --  Get team size
         return Info.Cur_Team.Team_Array'Length;
      end if;
   end Num_Servers_Available;

   overriding
   function Ada_Task_Identity (Plugin : WS_Plugin_Type)
     return Ada.Task_Identification.Task_Id is
   --  Returns the Ada Task_Id associated with the current LWT server
      --  NOTE: We don't call "Cur_Server_Index" since this function
      --        might be called for an Ada task that has nothing
      --        to do with LWT scheduling, and we don't want to
      --        assign it a unique server index.
   begin
      if Associated_Ada_Task_Ptr = null then
         --  This thread is just a "normal" Ada task.
         return Ada.Task_Identification.Current_Task;
      else
         return Associated_Ada_Task_Ptr.all;
      end if;
   end Ada_Task_Identity;

   overriding
   function Create_Group
     (Plugin : in out WS_Plugin_Type;
      Aspects : access LWT.Aspects.Root_Aspect'Class := null)
     return LWT_Sched_Group_Ptr is
   --  Create a scheduler-specific LWT group
      Cur_Server : constant LWT_Server_Index := Cur_Server_Index;
      Info       : Server_Info renames Server_Info_Array (Cur_Server);
      New_Group  : constant WS_Group_Ptr := new WS_Group'
        (Enclosing_Group => Info.Innermost_Group,
         Owning_Server => Cur_Server_Index,
         Shared_Index => Unshared_Group,
         Dependent_Count => 0,
         --  Waiting_Server => 0,
         --  Is_Being_Awaited => False,
         Canceled => False);
   begin
      --  The innermost Group field is just needed to properly create the
      --  chain of enclosing Groups.
      --  When a Group is completed, it will be removed from the chain
      --  and replaced by its encloser.
      Info.Innermost_Group := New_Group;

      if Debug_Statistics then
         --  Statistics
         --  TBD: This is a race condition
         Num_Groups := Num_Groups + 1;
      end if;

      return LWT_Sched_Group_Ptr (New_Group);
   end Create_Group;

   --  --  Install/Remove WS Plugin  --  --

   WS_Plugin : aliased WS_Plugin_Type;
   --  This is the plugin object for the WS LWT scheduler

   --  These two routines keep track of the number of active WS
   --  parallel regions.  While there is at least one active, we establish
   --  WS as the LWT scheduler.  These are called as a side-effect
   --  of init'ing/finalizing an LWT.Scheduler.Work_Stealing.WS_Parallel obj.
   --  It is implementation-defined whether there is one LWT scheduler
   --  plug-in per Ada task, or one per program partition.

   procedure Incr_WS_Parallel_Regions
     (Obj : LWT.Work_Stealing.WS_Parallel;
      Team_Info : not null Server_Team_Ptr) is
      use Int_Counters;
      Pre_Value : constant Atomic_Int :=
        Atomic_Fetch_And_Add (Num_Parallel_Regions, 1);
      Num_Servers : Natural := Obj.Num_Servers;
         --  Number of servers, over and above those created for enclosing
         --  parallel regions.

      Server_Index : constant LWT_Server_Index := Cur_Server_Index;
      Info : Server_Info renames Server_Info_Array (Server_Index);

   begin  --  Incr_WS_Parallel_Regions

      --  Remember prior team info
      Team_Info.Enclosing_Team_Info := Info.Cur_Team;
      Team_Info.Enclosing_Team_Member := Info.Team_Member;

      if Pre_Value = 0 then
         --  Time to install the scheduler
         LWT.Scheduler.Install_Scheduler_Plugin (WS_Plugin'Access);
      end if;

      if Num_Servers = 0 then
         --  Choose a number of servers based on number of CPUs
         if Natural (System.Multiprocessors.Number_Of_CPUs) > Max_WS_Servers
         then
            Num_Servers := Max_WS_Servers - Natural (Max_Server_Index) + 1;
         else
            Num_Servers := Natural (System.Multiprocessors.Number_Of_CPUs) -
                             Natural (Max_Server_Index) + 1;
         end if;
      end if;

      if Num_Servers <= 1 then
         --  Nothing more to do; leave Team_Array null
         return;
      end if;

      --  Start the servers, at same priority as current task
      declare
         Prio : constant System.Priority :=
           Ada.Dynamic_Priorities.Get_Priority;
         Team_Size : constant Team_Member_Index :=
           Team_Member_Index (Num_Servers);
         New_Team_Array : constant Server_Index_Array_Ptr :=
           new Server_Index_Array'(1 .. Team_Size => 0);
      begin
         New_Team_Array (1) := Server_Index;
            --  Fill in server index for master of team.

         Team_Info.Team_Array := New_Team_Array;

         --  Record associated Ada task identity.
         if Info.Cur_Team /= null then
            --  Get from current team
            Team_Info.Associated_Ada_Task := Info.Cur_Team.Associated_Ada_Task;
         else
            --  Must be a "true" Ada task
            Team_Info.Associated_Ada_Task :=
              Ada.Task_Identification.Current_Task;
         end if;

         --  Fill in new team info
         Info.Cur_Team := Team_Info;

         for I in 2 .. Team_Size loop
            declare
               --  Create a server which will start serving threads;
               --  Give it a priority and team info.
               Ignore : LWT_Server_Ptr :=
                 new LWT_Server
                   (Prio, Cur_Team => Team_Info, Team_Member => I);
                  --  TBD: Reclaim storage for task object
            begin
               null;  --  Nothing more to do
            end;
         end loop;
      end;
   end Incr_WS_Parallel_Regions;

   procedure Decr_WS_Parallel_Regions
     (Obj : LWT.Work_Stealing.WS_Parallel;
      Team_Info : not null Server_Team_Ptr) is

      pragma Unreferenced (Obj);

      use Int_Counters;

      Pre_Value : constant Atomic_Int :=
        Atomic_Fetch_And_Subtract (Num_Parallel_Regions, 1);

      Server_Index : constant LWT_Server_Index := Cur_Server_Index;
      Info : Server_Info renames Server_Info_Array (Server_Index);

      procedure Free is new Ada.Unchecked_Deallocation
        (Server_Index_Array, Server_Index_Array_Ptr);

   begin  --  Decr_WS_Parallel_Regions

      if Team_Info.Team_Array /= null then
         --  Shut down the servers that were started for this region.
         Team_Info.Manager.Shut_Down_Team;

         --  Restore the Cur_Team/Team_Member
         Info.Cur_Team := Team_Info.Enclosing_Team_Info;
         Info.Team_Member := Team_Info.Enclosing_Team_Member;

         --  Reclaim storage for team array
         Free (Team_Info.Team_Array);
      end if;

      if Pre_Value = 1 then
         --  Time to remove the scheduler
         LWT.Scheduler.Remove_Scheduler_Plugin (WS_Plugin'Access);

         if Debug_Statistics then
            --  Display some statistics
            Show_Stats (Clear => True);
         end if;
      end if;

   end Decr_WS_Parallel_Regions;

end LWT.Scheduler.Work_Stealing;
