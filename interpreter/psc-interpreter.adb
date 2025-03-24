------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

--  Package defining the virtual machine instructions for ParaSail, and an
--  interpreter for them.

pragma Style_Checks (All_Checks);
--  Turn off subprogram ordering, not used for this unit because of
--  elaboration order requirements.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Task_Attributes;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;
with System.Storage_Elements;

with PSC.Hash_Tables;
with PSC.Interpreter.Locks;
with PSC.Languages;
with PSC.Messages;
with PSC.Per_File_Strings;
with PSC.Strings;
with PSC.String_Streams;
with PSC.Trees;
with PSC.Trees.Obj_Decl;
with PSC.Univ_Integers;
with PSC.Univ_Strings;
with PSC.Vectors;

pragma Elaborate (Ada.Text_IO);
pragma Elaborate (Ada.Task_Attributes);
pragma Elaborate (PSC.Hash_Tables);
pragma Elaborate (PSC.Strings);
pragma Elaborate (PSC.Trees);
pragma Elaborate (PSC.Vectors);

package body PSC.Interpreter is

   use Type_Descriptor_Ops;
   use PSC.Interpreter.Locks;

   Never_Deallocate            : constant Boolean := False;
   Internal_Consistency_Checks : constant Boolean := False;
   Debug_Kill                  : constant Boolean := False;
   Debug_Delay                 : constant Boolean := False;
   Debug_Stack                 : constant Boolean := False;

   Yield_When_Spawning_Thread  : constant Boolean := False;
   --  Set to True to cause a delay 0.0 to be invoked whenever a new
   --  thread is spawned. On a mono-processor, this will get more of the
   --  server tasks active sooner by creating a "round-robin" effect.

   ------------------------------------------------------------
   -- ParaSail Routine Table (both Interpreted and Compiled) --
   ------------------------------------------------------------

   package Routine_Vectors is new PSC.Vectors (Routine_RW_Ptr);
   type Routine_Vector is new Routine_Vectors.Vector;
   Routine_Table : Routine_Vector;
   --  Global table of code.

   subtype Routine_Elem_Index is Routine_Vectors.Elem_Index;
   use type Routine_Elem_Index;

   type Global_Operation_Id is record
      --  A unique identifier for an operation of a module
      Module_Name    : Strings.U_String_Index := 0;
      Operation_Name : Strings.U_String_Index := 0;
         --  Operation_Name includes overloading index (as "#XX")
      Is_Builtin     : Boolean := False;
   end record;

   function Name_With_Overloading_Index
     (Simple_Name : Strings.U_String; Num_Prior_Homonyms : Natural)
     return Strings.U_String;
   --  Returns Simple_Name if Num_Prior_Homonyms is 0;
   --  otherwise concatenates "#XX" where XX = Num_Prior_Homonyms + 1.

   procedure Compute_Routine_Name_With_Overloading_Index
     (Routine : Routine_RW_Ptr);
   --  Computes Routine.Name_With_Overloading_Index
   --  by calling Routine_Name_With_Overloading_Index if not already filled in.

   function Hash_Global_Operation_Id (Id : Global_Operation_Id)
     return Strings.Hash_Type;
   --  Hash for global operation Id

   --  Mapping from Global operation Id to the index in the Routine table
   package Id_To_Routine_Maps is new PSC.Hash_Tables
     (Element_Type => Routine_Index,
      Key_Type     => Global_Operation_Id,
      Hash_Type    => Strings.Hash_Type,
      Hash         => Hash_Global_Operation_Id);

   --  The actual Id => Routine map
   Id_To_Routine_Map : Id_To_Routine_Maps.Hash_Table;

   function Get_Routine_By_Global_Id (Global_Id : Global_Operation_Id)
     return Routine_RW_Ptr;
   --  Look up routine in Id_To_Routine_Map.
   --  If not there, create it as a compiled routine, but with a
   --  null Routine_Addr pointer.

   type Routine_Context is record
   --  This is the information needed when calling a routine.
      Code        : Routine_RW_Ptr;
         --  This is Read/Write, but don't write outside of
         --  Find_Routine_Context. It may fail.
         --  It must be RW to avoid memory allocation at run-time
      Static_Link : Word_Ptr;

      --  If this is non-null, then one or more outputs will need
      --  to be wrapped as this polymorphic type.
      Polymorphic_Output_Type : Type_Descriptor_Ptr := null;
   end record;

   Compare_Op_Str : Strings.U_String := Strings.Null_U_String;
   --  Operator for comparing two values and returning an "ordering"
   pragma Atomic (Compare_Op_Str);  --  Avoid race conditions on fetch/store

   Identity_Desig : constant String := "#identity";

   Identity_Builtin_Str : constant Strings.U_String :=
      Strings.String_Lookup (Identity_Desig);
   --  Builtin op for "identity" -- we don't want to "common" all
   --  uses of this, because it provides no useful information.

   Debug_Console_Routine : Routine_Ptr := null;
   --  Will be set to null in Install_Code if we install code whose
   --  full name matches Languages.Debug_Console_Full_Name.

   type Single_Step_Indicator is range -3 .. 100;
      --  Indicator returned from invocation of debugger console
      --  to indicate what should happen upon return:
   Stop_Execution : constant Single_Step_Indicator := -3;
      --  Exit the program upon return from the debugger.
   Single_Step_Into : constant Single_Step_Indicator := -2;
      --  Return to the debugger as soon as leaving the current line
      --  including by calling another routine or returning.
   Single_Step_Over : constant Single_Step_Indicator := -1;
      --  Return to the debugger as soon as leaving the current line,
      --  but "step over" any calls.
   Continue_Execution : constant Single_Step_Indicator := 0;
      --  Continue until hitting a breakpoint or an assertion failure.
   subtype Step_Out is Single_Step_Indicator
     range 1 .. Single_Step_Indicator'Last;
      --  Stop upon exiting the specified number of stack frames.

   type Debugger_Reason is range -8 .. Single_Step_Indicator'Last;
      --  Indicates the reason the debugger console was invoked,
      --  passed as a parameter to Invoke_Debug_Console.
   No_Reason : Debugger_Reason := 0;
      --  Should not occur
   Step_Over_Finished : Debugger_Reason := -1;
      --  A Step-Over just completed
   Step_Into_Finished : Debugger_Reason := -2;
      --  A Step-Into just completed
   Step_Out_Finished : Debugger_Reason := -3;
      --  A Step-Out just complerted
   Step_Over_Exited_Frame : Debugger_Reason := -4;
      --  A Step-Over exited the frame (e.g. did a "return")
   Step_Into_Exited_Frame : Debugger_Reason := -5;
      --  A Step-Into exited the frame (e.g. did a "return")
   Assertion_Failure : Debugger_Reason := -6;
      --  ParaSail Assertion failed
   Null_Check_Failure : Debugger_Reason := -7;
      --  Null check failed
   Internal_Failure : Debugger_Reason := -8;
      --  Internal failure within interpreter
   subtype Breakpoint_Encountered is Debugger_Reason
     range 1 .. Debugger_Reason'Last;
      --  Index of breakpoint reached

   ----------------------------------------------------------------
   -- Built-in-operation registration table and associated types --
   ----------------------------------------------------------------

   type Builtin_Entry;
   type Builtin_Entry_Ptr is access Builtin_Entry;

   type Builtin_Entry is record
      Desig   : Strings.U_String;
      Builtin : Routine_Code_Address;
      Next    : Builtin_Entry_Ptr;
   end record;

   use type Strings.U_String;
   use type Strings.Hash_Type;

   Builtin_Table_Size : constant := 109;  --  medium size prime number

   subtype Table_Index is
     Strings.Hash_Type range 0 .. Builtin_Table_Size - 1;

   Builtins_Table : array (Table_Index) of Builtin_Entry_Ptr;

   function To_Non_Op_Map_Type_Desc is new Ada.Unchecked_Conversion
     (Word_Ptr, Non_Op_Map_Type_Ptr);
   --  Convert physical addr of type descriptor to the corresponding non-op-map
   --  type descriptor.
   --  NOTE: This converter is used for static links that might *not*
   --        be type descriptors, in certain situations, so care must
   --        be taken to ensure value is really a type descriptor
   --        before using it as such (e.g. check for magic number).

   ------------------------
   -- Thread server info --
   ------------------------

   package Thread_Server_Info is

      type Thread_Count_Base is range 0 .. Integer'Last;
      subtype Thread_Count is
        Thread_Count_Base range 0 .. 2 ** 15 - 1;
      --  Number of active sub threads of a given master

      type Longest_Natural is range 0 .. System.Max_Int;

      Server_Stack_Size : constant := 10_000_000;
      --  Size of stack (in bytes, aka "storage elements")
      --  for each work-stealing "server" task.

      Max_Bypassed_Init_Recursion : constant := 15;
      --  No more than 15 levels of recursion when bypassing
      --  thread initiation.

      Num_Initial_Thread_Servers : constant := 1;
      --  Number of Ada tasks initially started up to serve threads
      --  (in addition to the "main" server thread).

      Minimum_Live : constant := 1;
      --  Minimum number of servers that should be available for
      --  actively executing code.

      Enough_Unshared_Threads : constant := 4;
      --  Minimum number of unshared threads needed for work-stealing
      --  to work reasonably.

      type Master_Index is
        range 0 .. 2 ** 15 - 1;
      --  Unique index of active master; used as index into Master_Extras
      --  table.

      type Master_Reuse_Counter is mod 2**16;
      --  This counts how often a given master record is reused.
      --  It wraps around if necessary.

      type Server_State;
      type Server_State_Ptr is access all Server_State;

      type Server_State is record
      --  We keep this information on each server (for debugging)
         Code               : Routine_Ptr;
         Context            : Exec_Context_RW_Ptr;
         Pc                 : Code_Offset := 0;
         Start_Pc           : Code_Offset := 0;  --  > 1 if nested block
         Src_Pos            : Source_Positions.Source_Position :=
                                Source_Positions.Null_Source_Position;
         Locked_Param_Index : Natural := 0;
         Step_Indicator     : Single_Step_Indicator := Continue_Execution;
         Stopped_At_Line    : Source_Positions.Line_Number := 0;
         Prev_State         : Server_State_Ptr := null;
      end record;

      Null_Server_State : constant Server_State :=
        (Code               => null,
         Context            => null,
         Pc                 => 0,
         Start_Pc           => 0,
         Src_Pos            => Source_Positions.Null_Source_Position,
         Locked_Param_Index => 0,
         Step_Indicator     => Continue_Execution,
         Stopped_At_Line    => 0,
         Prev_State         => null);

      type Thread_Category is
        (Locked_Thread, Queuing_Thread, Nonqueuing_Thread);
      --  We keep these three categories separated
      --  because they are handled differently by the scheduler.
      --  NOTE: The order between Queuing_Thread and Nonqueuing_Thread
      --      represents a preference.  We choose queuing threads first
      --      because one reason for queuing is to put a timer on a long
      --      calculation.  If we keep choosing the nonqueuing threads,
      --      we will never get around to setting the timer.

      subtype Thread_Queue_Head is Word_Ptr;
      --  Per-server thread queue is represented by a word_ptr

      type Thread_Deque_Head is record
         --  LIFO queuing is used when getting a thread on the
         --  server's own queue.
         --  FIFO queuing is used when stealing a thread from another server.
         First_Thread : Thread_Queue_Head;  --  Oldest thread
         Last_Thread  : Thread_Queue_Head;  --  Youngest thread
         Count        : Thread_Count_Base := 0;  --  Number of threads on deque
      end record;

      type Thread_Deque_Triplet is
        array (Thread_Category) of Thread_Deque_Head;
      --  Triplet of Deque heads, one for case where a lock is held,
      --  one for case where queuing is possible, and one for
      --  non-queuing threads.

      Last_Category_Table : constant array (Boolean) of Thread_Category :=
        (False => Thread_Category'Last,
         True => Locked_Thread);
      --  Indicates what is the last category that should be considered.
      --  Indexed by Has_Lock.  When waiting for master,
      --  queueing threads can be considered only if they are subthreads.

      Category_Table : constant array (Boolean, Boolean) of Thread_Category :=
        (False => (Nonqueuing_Thread, Queuing_Thread),
         True => (Locked_Thread, Locked_Thread));
      --  Indicates what category to assign to thread based
      --  on whether it is holding a lock, and whether it uses queuing.

      --  Information kept in master about whether it is acting as
      --  an exception handler.
      type Handler_Kind_Enum is
        (Not_A_Handler, PSVM_Handler, Compiled_Handler);

      type Handler_Info_Rec
        (Kind : Handler_Kind_Enum := Not_A_Handler) is record
         case Kind is
            when Not_A_Handler => null;

            when PSVM_Handler =>
               Handler_Start    : Code_Index := Code_Index'First;
               --  PC where Handler_Block starts

               Handler_Routine  : Routine_Ptr := null;
               --  Pointer to routine enclosing handler

               Handler_Block    : Code_Block_Ptr := null;
               --  Descriptor for nested code block containing handler

            when Compiled_Handler =>
               Handler_Desc     : Convention_Descriptor := Null_Conv_Desc;
               Handler_Addr     : Routine_Code_Address := null;
         end case;
      end record;

      --  Exception used to climb up to a compiled handler.
      Propagate_To_Compiled_Handler : exception;

      ---- Layout of "extra" info for each master ----
      type Master_Extra_Rec is record
         --  This is "extra" information maintained centrally on each
         --  task master, rather than in the master record itself.

         Enclosing_Master : Master_Index           := 0;
         --  Enclosing master, if any, of given master
         --  NOTE: This field is used to link "free" extra master records
         --        together when master is not in use.

         Master_Address   : Word_Ptr := null;
         --  Virtual Address of master with given index.
         --  This is null when associated master index is not in use.

         Subthread_Count  : Thread_Count           := 1;
         --  Count of subthreads of master
         --  NOTE: When set to Uninit_Thread_Count, master has had no threads
         --        spawned on it yet, and needs further initialization.

         Lock_Held        : Lock_Obj_Index         := 0;
         --  Indicates which lock, if any, is held by the given master.

         Num_Subordinates : Thread_Count_Base      := 0;
         --  Count of number of waiting threads that are direct subthreads of
         --  given master, plus number of masters with at least one
         --  waiting subthread.
         --  Master_Extras(0).Num_Subordinates is used
         --  for the total number of waiting threads.

         Is_Being_Awaited : Boolean                := False;
         --  Keeps track of whether master is currently being awaited
         --  (used for statistics only).

         Master_Is_Shared : Boolean                := False;
         --  If True, then Master is visible to multiple server tasks,
         --  and master should only be manipulated when in a protected action.
         pragma Atomic (Master_Is_Shared);

         Master_Never_Shared : Boolean             := False;
         --  If True, then this Master should never be shared with another
         --  server, because it is part of a Map/Reduce operation which
         --  has an unprotected reduction accumulator.

         Owned_By_Server : Thread_Server_Index'Base
           range 0 .. Thread_Server_Index'Last := 0;
         --  Server index of server that created (and now "owns") the master.

         Innermost_Shared_Master : Master_Index    := 0;
         --  If non-zero, this is the index of this master if this master
         --  is shared, or of the innermost enclosing master if this master
         --  is not shared.

         Reuse_Count      : Master_Reuse_Counter := 0;
         --  This is bumped each time this master record is taken off
         --  the free list, so that the combination of the master index
         --  and this count is unique (unless this wraps around).

         Exiting_Tcb      : Word_Ptr := null;
         --  If innermost shared master has the "Exit_Requested" flag set,
         --  then this will be the address of the Tcb that initiated the exit,
         --  and hence is the only sub-thread of the master that should
         --  be allowed to proceed.

         State_Of_Master  : aliased Server_State;
         --  The state of the server at the time when the master
         --  is initialized.

         Handler_Info     : Handler_Info_Rec := (Kind => Not_A_Handler);
         --  Info on handler, if any

         Excep_Occurrence : aliased Word_Type := Null_Virtual_Address;
         --  Exception occurrence is non-null if an exception was raised
      end record;

      type Master_Extra_Array_Type is
        array (Master_Index) of aliased Master_Extra_Rec;
      --  This keeps track of the extra information for each master.

      type Master_Extra_Ptr is access all Master_Extra_Rec;

      ---- Layout of info for each server ----

      type Server_Info is record
      --  Here is all the information we have for each server
         Current_State      : aliased Server_State;
         --  Current state of given server (mostly for debugging)

         Last_Src_Pos       : Source_Positions.Source_Position :=
           Source_Positions.Null_Source_Position;
         --  Source-Pos just prior to return (used for Check_Nested_Block_Op)

         Last_Active_Thread : Thread_Queue_Head := null;
         --  Most recent active thread
         --  NOTE: This is mostly for debugging

         --  Doubly-linked Deque of waiting threads, one for each server,
         --  and one for each "category" (normal, locked, queuing);
         --  available for stealing.
         Shared_Threads     : Thread_Deque_Triplet :=
           (others => (Count => 0, others => null));

         --  Doubly-linked Deque of waiting threads, one for each server;
         --  not available for stealing.
         Unshared_Threads   : Thread_Deque_Head :=
           (Count => 0, others => null);

         --  Per-server statistics
         Num_Unshared_Thread_Initiations : Natural := 0;
         Num_Bypassed_Thread_Initiations : Natural := 0;
         Bypassed_Init_Recursion_Level   : Natural := 0;
         Max_Waiting_Unshared_Threads    : Thread_Count_Base := 0;
         Num_Waiting_Unshared_Summed     : Longest_Natural := 0;
         Num_Threads_In_Process          : Natural := 0;
         Max_Threads_In_Process          : Natural := 0;

         Lowest_Stack                    : System.Storage_Elements.
                                             Integer_Address :=
                                             System.Storage_Elements.
                                               Integer_Address'Last;

         Innermost_Stg_Rgn  : Stg_Rgn_Ptr := null;
         --  Innermost storage region of given server.

         Free_Stg_Rgns      : Stg_Rgn_Ptr := null;
         --  List of regions no longer in use

         Free_Rgn_Chunks    : Stg_Rgn_Chunk_Ptr := null;
         --  List of region chunks no longer associated with any region

         Free_Master        : Master_Index := 0;
         --  List of master indices no longer in use for any active master

         Free_Lock_Obj      : Lock_Obj_Index := 0;
         --  List of lock-obj indices no longer in use for any concurrent obj

         Cur_Master_Extra   : Master_Extra_Ptr := null;
         --  This is only used while waiting on a member of the
         --  Get_Thread_Or_Wait_For_Threads_Internal entry family.

      end record;

      Server_Info_Array : array (Thread_Server_Index) of Server_Info;
      --  All of the info for each server.
      --  NOTE: We use an array of records rather than multiple arrays
      --        to minimize the likelihood of "false" sharing.
      --  TBD:  Ideally the component size of Server_Info should be a
      --        multiple of the cache-line size.

      package Server_Index_Attribute is new Ada.Task_Attributes (
         Attribute     => Thread_Server_Index,
         Initial_Value => Main_Thread_Server_Index);
      --  Keeps track of current server index

      Num_Dynamically_Allocated_Thread_Servers : Natural := 0;

      Max_Dynamically_Allocated_Thread_Servers : Natural := 6;
      pragma Atomic (Max_Dynamically_Allocated_Thread_Servers);
      --  Only allow a few dynamically allocated thread servers
      --  (can be overridden with the "-servers" command-line flag
      --   or "servers" command during interactive use).

      task type Thread_Server is
         --  One of these is created for each server process, each of
         --  which serves a queue of pico-threads.
         pragma Storage_Size (Server_Stack_Size);
      end Thread_Server;

      task type Server_Creator is
         --  Task which creates thread servers dynamically
         --  NOTE: We can't do this inside the Thread_Manager
         --       since thread creation isn't permitted
         --       inside of a protected action (because
         --       waiting for activation, or anything, isn't
         --       permitted).
         pragma Priority (System.Priority'First);
         --  Lowest priority
         --  TBD: We might want this to be highest priority
         --      except we don't want it to create new threads when
         --      we already have several that exist but haven't quite
         --      made it to getting a thread to serve.
         entry Start;
      end Server_Creator;

      type Server_Creator_Ptr is access Server_Creator;
      --  We use a pointer and a task type, even though we will
      --  only create one of these, so we can postpone creating
      --  the thread until we are ready to start it up.

      function Caller_Of (State : Server_State) return Server_State_Ptr;
      --  Return pointer to server state for caller of given stack frame.
      --  Normally this is just State.Prev_State, but if we are crossing
      --  pico-thread boundaries, we need to retrieve the state from
      --  the enclosing master.

      procedure Dump_One_Thread
        (Tcb    : Word_Ptr;
         Indent : Natural := 0);
      --  Dump information on given thread
      pragma Export (Ada, Dump_One_Thread, "dump_tcb");

      procedure Dump_Stack
        (State            : Server_State;
         Num_Stack_Frames : Positive := Positive'Last;
         Depth            : Natural := 0;
         Skip_First       : Boolean := False;
         Use_Cur_Err      : Boolean := False);
      --  Dump current state of a single server thread
      --  If Skip_First is True, don't produce error message on first frame
      --  in stack dump.
      --  If Use_Cur_Err is true, set output to Current_Error and then
      --  restore to Standard_Output at end.
      pragma Export (Ada, Dump_Stack, "dump_stack");

      procedure Dump_Masters;
      --  Dump the state of the masters.
      pragma Export (Ada, Dump_Masters, "dump_masters");

   end Thread_Server_Info;
   use Thread_Server_Info;

   ------- Type Table -------

   package Type_Vectors is new PSC.Vectors (Type_Descriptor_Ptr);
   type Type_Vector is new Type_Vectors.Vector;
   Type_Table : Type_Vector;
   --  Global table of type descriptors.

   subtype Type_Elem_Index is Type_Vectors.Elem_Index;
   use type Type_Elem_Index;

   use type Strings.U_String;

   --  Table mapping type name to type descriptor
   package Type_Name_Maps is new PSC.Hash_Tables
     (Element_Type => Type_Descriptor_Ptr,
      Key_Type     => Strings.U_String,
      Hash_Type    => Strings.Hash_Type,
      Hash         => Strings.Hash);

   Type_Name_Map : Type_Name_Maps.Hash_Table;

   --  Table mapping old type-descriptor indices to new type descriptors

   function Type_Index_Hash (Index : Type_Index) return Strings.Hash_Type;
   --  Hash for Type_Index type

   package Reconstructed_Type_Desc_Map is new PSC.Hash_Tables
     (Element_Type => Type_Descriptor_Ptr,
      Key_Type     => Type_Index,
      Hash_Type    => Strings.Hash_Type,
      Hash         => Type_Index_Hash);

   type Buffered_Desc_Reader
     (Data     : access String_Streams.Stream_Rep_Array;
      Type_Map : access Reconstructed_Type_Desc_Map.Hash_Table;
      Strings  : access String_Table_Type;
      Pass     : Natural)  --  Used to communicate which pass we are in
     is new Per_File_Strings.Buffered_Reader_With_Strings (Data, Strings)
   with record
      Is_Incomplete : Boolean := False;   --  Set to True if contains objects
                                          --  which cannot be read in yet.
   end record;

   -------- Stg_Rgn and Chunk tables --------

   Num_Stg_Rgns : Stg_Rgn_Index := 0;

   Chunk_Group_Size : constant := 2**10;
   type Index_In_Chunk_Group is range 0 .. Chunk_Group_Size - 1;
   type Chunk_Group_Index is range 0 .. Chunk_Index'Last / Chunk_Group_Size;

   type Chunk_Group is array (Index_In_Chunk_Group) of Stg_Rgn_Chunk_Ptr;
   type Chunk_Group_Ptr is access Chunk_Group;

   Chunk_Group_Table : array (Chunk_Group_Index) of Chunk_Group_Ptr;
   --  Global table of groups of region chunks.

   --  Range of addresses covered by allocated storage chunks
   Lowest_Virt_Addr    : Object_Virtual_Address :=
                           Object_Virtual_Address'Last;
   Highest_Virt_Addr   : Object_Virtual_Address :=
                           Object_Virtual_Address'First;

   --  Range of addresses covered by stack-based objects
   Lowest_Stack_Addr    : Object_Virtual_Address :=
                           Object_Virtual_Address'Last;
   Highest_Stack_Addr   : Object_Virtual_Address :=
                           Object_Virtual_Address'First;

   Min_Chunk_Size : constant Offset_Within_Chunk := 1024;
   --  8K chunks

   --  Statistics
   Stg_Rgn_Stats : array (Boolean, Boolean) of Natural :=
     (others => (others => 0));
   --  First index is True = By_Owner, False = not By_Owner
   --  Second index is True = Reusing, False = not Reusing a block

   --------  Descriptor for formal operation  ---------

   subtype Static_Chunk_Index is Chunk_Index range 0 .. 2 ** 16 - 1;
   --  NOTE: We are assuming stack chunk id stays < 2**16

   Operation_Descriptor_Type_Desc : Type_Descriptor_Ptr := null;

   -------- Lock object table --------

   ------------------
   -- Object_Locks --
   ------------------

   package Object_Locks is
      type Object_Lock is limited private;

      type Dequeue_Routine_Ptr is
        access procedure
          (Tcb_To_Dequeue : Word_Ptr;
           Thread_Was_Queued : out Boolean);

      procedure Dequeue_Tcb
        (Lock              : in out Object_Lock;
         Server_Index      : Thread_Server_Index;
         Tcb_To_Dequeue    : Word_Ptr;
         Thread_Was_Queued : out Boolean);
      --  Dequeue Tcb from lock, either using the normal dequeue routine,
      --  or the specified "special" dequeue routine, if any. If Thread was
      --  in fact queued (it might have been dequeued before the lock was
      --  acquired), then Thread_Was_Queued is True.

      procedure Dump_Lock_Queue
        (Lock : Object_Lock);
      --  Show the queue on the lock, if any

      procedure Locked_Call
        (Lock                  : in out Object_Lock;
         Target_Routine        : Routine_Ptr;
         New_Context           : in out Exec_Context;
         Server_Index          : Thread_Server_Index;
         Locked_Param_Info     : Locked_Param_Info_Type;
         Locked_Obj_Lock_Index : Lock_Obj_Index;
         Thread_Was_Queued     : out Boolean);
      --  Call the given routine under control of a lock on the specified
      --  parameter. If Thread_Was_Queued is True on return, then call was
      --  not performed but instead associated TCB was queued

      procedure Set_Dequeue_Routine
        (Lock            : in out Object_Lock;
         Dequeue_Routine : Dequeue_Routine_Ptr);
      --  Specify that lock has a special dequeue routine.

   private
      type Object_Lock is limited record
         Actual_Lock : Simple_Lock;
         Queue       : Word_Ptr := null;
         --  Address of first waiting: TCB
         Queue_Last  : Word_Ptr := null;
         --  Address of last waiting: TCB

         Dequeue_Routine : Dequeue_Routine_Ptr := null;
         --  Special dequeue routine, if any
      end record;
   end Object_Locks;

   use Object_Locks;

   type Object_Lock_Ptr is access Object_Lock;

   type Lock_Obj_Info is record
      Lock  : Object_Lock_Ptr := null;
      Index : Lock_Obj_Index := 0;
      --  Index associated with lock obj when allocated.
      --  Index of next free lock obj when released.
   end record;

   Lock_Obj_Table :
     array (Lock_Obj_Index range 1 .. Lock_Obj_Index'Last) of Lock_Obj_Info;
   --  Global table of lock objects. Note that we use an array rather than a
   --  growable vector to avoid race conditions associated with getting nth
   --  element of vector just when the vector is being expanded.

   Enclosing_Lock :
     array (Lock_Obj_Index range 1 .. Lock_Obj_Index'Last) of Lock_Obj_Index
      := (others => 0);
   --  Enclosing lock, if any, held by TCB holding given lock

   Lock_Exit_Requested : array (Lock_Obj_Index range 1 .. Lock_Obj_Index'Last)
      of Boolean := (others => False);
   --  If True, then on release of the lock, exit should be requested for tcb.

   Lock_Obj_Limit : Lock_Obj_Index := 0;
   --  Current maximum value for Lock_Obj_Index -- For debugging

   Large_Obj_Next_Block_Offset : constant Offset_Within_Area := 1;
   --  "Next_Block" is only used when on the reclaimed-block list

   --  Reclaimable blocks

   subtype Reclaimable_Block_Ptr is Object_Virtual_Address;

   Min_Reclaimable_Block_Size : constant Offset_Within_Area :=
     Large_Obj_Next_Block_Offset + 1;
   --  Blocks must be at least this large to be reclaimed

   subtype Hash_Value is Strings.Hash_Type;

   type Reclaimable_Blocks is
     array (Hash_Value range <>) of Reclaimable_Block_Ptr;

   Initial_Modulus : constant := 13;
   --  A prime that has a P-1 length 2**N mod P cycle
   --  NOTE: Others include 11, 19, 29, 37, 53, 59, 61, 67, 83, 101.
   --        These are good choices for the hash since it is quite
   --        common that allocation sizes are doubled as an expandable
   --        vector grows.  By choosing a prime with a maximum length
   --        cycle, we minimize the chance of hash collisions associated
   --        with doublings.

   subtype High_Bound_Subtype is Hash_Value range 0 .. Hash_Value'Last - 1;
      --  Limit high bound subtype so "High_Bound + 1" won't wrap to zero.

   type Reclamation_Info_Record (Modulus_Minus_One : High_Bound_Subtype) is
     record
      Reclamation_Table : Reclaimable_Blocks (0 .. Modulus_Minus_One) :=
        (others => Null_Virtual_Address);
   end record;

   --  Special offsets within local area
   Local_Area_Static_Link_Offset : constant Offset_Within_Area := 0;
   --  Offset to pointer to enclosing local area/type area
   Local_Area_Param_Ptr_Offset   : constant Offset_Within_Area := 1;
   --  Offset to pointer to params
   Local_Area_Stg_Rgn_Ptr_Offset : constant Offset_Within_Area := 2;
   --  Offset to pointer to local region (null if no local region created)

   pragma Assert
     (Local_Area_Stg_Rgn_Ptr_Offset < Local_Area_Local_Data_Offset);
   --  Local data must start after region pointer

   --------------- Local Subprograms ------------------

   function Addr_To_Large_Obj_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Large_Obj_Header_Ptr);

--   function To_Large_Obj_Ptr is
--     new Ada.Unchecked_Conversion (Word_Ptr, Large_Obj_Header_Ptr);

   function Addr_To_Word_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Word_Ptr);

   function Addr_To_Word is
     new Ada.Unchecked_Conversion
       (System.Storage_Elements.Integer_Address, Word_Type);

   function Word_To_Addr is
     new Ada.Unchecked_Conversion
       (Word_Type, System.Storage_Elements.Integer_Address);

   function To_Routine_Code_Address is
     new Ada.Unchecked_Conversion
       (Nested_Blk_Address, Routine_Code_Address);

   function Word_Ptr_To_Routine_Code_Address is
     new Ada.Unchecked_Conversion
       (Word_Ptr, Routine_Code_Address);

   function Routine_Code_Address_To_Word is
     new Ada.Unchecked_Conversion
       (Routine_Code_Address, Word_Type);

   function To_Nested_Blk_Address is
     new Ada.Unchecked_Conversion
       (Routine_Code_Address, Nested_Blk_Address);

   function Allocate_Local_Area
     (Context           : in out Exec_Context;
      Local_Area_Length : Offset_Within_Area) return Word_Ptr;
   --  Allocate/Find space for local area of routine about to be called
   --  TBD: How do we release the storage?

   function Basic_Allocate_From_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Server_Index  : Thread_Server_Index) return Word_Type;
   --  Allocate object of given size within given region.
   --  Space is uninitialized.
   --  Caller gets a lock if needed.
   pragma Inline (Basic_Allocate_From_Stg_Rgn);

   procedure Borrow_Unshared_Stg_Rgn_Chunk
     (Enclosing_Stg_Rgn : Stg_Rgn_Ptr;
      Min_Size : Offset_Within_Area;
      Borrowed_Chunk : out Stg_Rgn_Chunk_Ptr);
      --  Borrow a chunk from enclosing region that has at least
      --  the given amount of space.
      --  Borrowed_Chunk is null if region has no appropriate chunk.
      --  Caller must get a lock, if needed.

   procedure Return_Unshared_Stg_Rgn_Chunk
     (Chunk             : Stg_Rgn_Chunk_Ptr;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr);
   --  Return chunk back to enclosing region
   --  Caller must get a lock, if needed.

   procedure Assign_Word
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Source      : Object_Locator;
      Type_Info   : Object_Locator);
   --  We want to release the old value of the destination before overwriting
   --  it with source.

   procedure Call_Compiled_Routine
     (Context     : in out Exec_Context;
      Params      : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr;
      Code_Addr   : Routine_Code_Address;
      Conv_Desc   : Convention_Descriptor);
   --  Call through Code_Addr passing parameters
   --  from Params vector according to Conv_Desc.

   procedure Check_Is_Large (Large_Obj_Value : Word_Type);
   --  Make sure Large_Obj_Value is a reasonable large-object value

   procedure Check_Not_Null
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Dest_Name   : Strings.U_String;
      Type_Info   : Object_Locator;
      Src_Pos     : Source_Positions.Source_Position);
   --  We want to check that destination is not null.

   procedure Check_Static_Chain (Static_Link : Word_Ptr);
   --  Make sure that the static chain ends with a type or a null address.

   function Content_Of_Address (Address : Object_Address) return Word_Type;
   --  Return word at given address
   --  Requires: Address.Enclosing_Chunk /= null

   procedure Create_Obj
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info               : Object_Locator);
   --  If object is large then create object of appropriate size in region
   --  determined by existing object, and initialize its subcomponents to
   --  nulls of appropriate kinds and regions.
   --  If object is small then create null of appropriate kind.
   --  If object is a wrapper, then recurse on component type.

   function Create_Operation_Desc
     (Context               : in out Exec_Context;
      Operation_Locator     : Object_Locator;
      Operation_Static_Link : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Conv_Desc             : Convention_Descriptor := Null_Conv_Desc;
      SL_Addr               : Word_Type := 0;
      Target_Addr           : Word_Type := 0;
      Existing_Obj_Addr     : Word_Type := 0)
         return Object_Virtual_Address;
   --  Create operation descriptor given a locator and static link for the
   --  operation.

   function Create_Operation_Desc_Exported
     (Context                      : in out Exec_Context;
      Operation_Locator_Base       : Area_Base_Indicator;
      Operation_Locator_Offset     : Offset_Within_Area;
      Operation_Static_Link_Base   : Area_Base_Indicator;
      Operation_Static_Link_Offset : Offset_Within_Area;
      Conv_Desc                    : Convention_Descriptor;
      SL_Addr                      : Word_Ptr;
      Target_Addr                  : Word_Ptr;
      Existing_Obj                 : Word_Type)
         return Object_Virtual_Address;
   --  Calls Create Operation_Desc
   pragma Export (Ada, Create_Operation_Desc_Exported,
      "_psc_create_operation_desc");

   procedure Execute_Call_Op
     (Context                 : in out Exec_Context;
      Params                  : Word_Ptr;
      Static_Link             : Word_Ptr;
      Target_Routine          : Routine_Ptr;
      Locked_Param_Info       : Locked_Param_Info_Type;
      Polymorphic_Output_Type : Type_Descriptor_Ptr := null);
   --  Execute Call_Op instruction

   procedure Exit_Program
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  Exit the ParaSail program

   procedure Find_Routine_Context
     (Context           : in out Exec_Context;
      Routine_Locator   : Object_Locator;
      Static_Link       : Object_Locator;
      Params            : Word_Ptr;
      Result            : in out Routine_Context;
      Orig_Param_Infos  : Routine_Cur_Inst_Param_Info :=
         No_Cur_Inst_Param_Info;
      SL_Addr           : Word_Ptr := null;
      Op_Desc_Virt_Addr : Object_Virtual_Address :=
         Null_Virtual_Address);
   --  Find context (code + static link) for routine given an Object_Locator
   --  If Orig_Param_Infos is non null, then use that to decide whether a
   --  parameter is of the "cur-inst" type.
   --  NOTE: This is important for operations like Hash because
   --        when defined for Hashable the result type is not cur-inst type,
   --        but when looking at the definition for Hash(Univ_Integer)
   --        the result *does* seem to be of the cur-inst type.
   --  NOTE: Result must be initialized with a writable Routine inside

   procedure Finish_Type_Descriptor
     (Type_Desc : Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False);
      --  Finish up formal object parameters and nested objects
      --  If Return_On_Recursion is True, do not complain about recursion
      --  and simply return immediately.

   function Get_Static_Link
     (Context   : in out Exec_Context;
      Static_Link_Locator : Object_Locator) return Word_Ptr;
   --  Get pointer to enclosing local area or enclosing type

   procedure Invoke_Debug_Console (Context : in out Exec_Context;
                                   Reason : Debugger_Reason);
   --  Invoke the debugging console.
   --  Pause other servers while the console is executing.

   function Local_Stg_Rgn (Context : Exec_Context) return Stg_Rgn_Ptr;
   --  Return pointer to local region

   procedure Make_Copy_In_Stg_Rgn
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Source                  : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info : Object_Locator);
   --  We want to make copy of source if large in region determined by
   --  Existing_Obj_In_Stg_Rgn

   procedure Move_Obj
     (Context   : in out Exec_Context;
      LHS       : Object_Locator;
      RHS       : Object_Locator;
      Type_Info : Object_Locator);
   --  We want to release the old values of the destination before overwriting
   --  it, and set source to null without releasing its old value, unless a
   --  region change is necessary.

   function New_Stg_Rgn_Chunk_Index return Chunk_Index;
   --  Get a new region-chunk index

   procedure Num_Stack_Frames
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Return count of number of stack frames for current thread.
   pragma Export (Ada, Num_Stack_Frames, "_psc_num_stack_frames");

   procedure Nth_Stack_Frame
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Return nth stack frame for current thread.
   pragma Export (Ada, Nth_Stack_Frame, "_psc_nth_stack_frame");

   procedure Nth_Frame_Type_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Return type relative to nth stack frame for current thread.
   --  func Nth_Frame_Type_At_Locator (Frame_Num, Type_Locator)
   --    -> Type_Descriptor
   pragma Export (Ada, Nth_Frame_Type_At_Locator,
     "_psc_nth_frame_type_at_locator");

   procedure Obj_Element_Info_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Data : out Element_Info;
      Obj_Info_Incomplete : in out Boolean);
   --  Read in the Element_Info for an object
   --  Set Obj_Info_Incomplete to True if only filled in Type_Desc;
   --  otherwise leave it as it came in.

   procedure Obj_Element_Info_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Data : Element_Info);
   --  Write out the Element_Info for an object
   --  We need Per-File string table to write these out

   procedure Peek_At_Address
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
      --  Return value at given address + offset
   pragma Export (Ada, Peek_At_Address, "_psc_peek_at_address");

   function Replace_Special_Value_Stg_Rgn
     (Val : Word_Type; New_Rgn : Stg_Rgn_Ptr)
     return Word_Type;
      --  Return special value, with region index replaced with that of
      --  newly specified region.

   procedure Runtime_Message
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr);
   --  Print a message as part of some kind of runtime failure or warning.
   --  Print value of Param 0 as a string and (someday) give a traceback.
   pragma Export (Ada, Runtime_Message, "_psc_runtime_message");

   function Select_Ancestor_Part
     (Context            : in out Exec_Context;
      Source_Obj         : Word_Type;
      Ancestor_Type_Desc : Type_Descriptor_Ptr;
      Source_Type_Desc   : Type_Descriptor_Ptr;
      Is_Passed_By_Ref   : Boolean) return Word_Type;
   --  We want to select ancestor part of type Ancestor_Type_Desc of an object
   --  (value in Source_Obj, of type Source_Type_Desc), and return ancestor
   --  object as a Word_Type.
   --  If Is_Passed_By_Ref is True, then "ref" to ancestor part will be placed
   --  in Destination, and, unless polymorphic, Source_Obj also contains ref
   --  to original object.
   --  NOTE: A ref is produced by taking a physical address and converting
   --        it to Word_Type using Word_Ptr_To_Word.
   pragma Export (Ada, Select_Ancestor_Part, "_psc_select_ancestor_part");

   procedure Select_Polymorphic_Ancestor_Part
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Source      : Object_Locator;
      Type_Info   : Object_Locator;
      Polymorphic_Ancestor_Lvalue : Boolean);
   --  We want to select ancestor part of polymorphic object (identified by
   --  Source), of type specified by Type_Info, and place (non-polymorphic)
   --  object in Destination.

   function Shallow_Copy_Large_Obj
     (Context   : in out Exec_Context;
      Stack_Val : Word_Type) return Word_Type;
   --  Copy top level of large stack-resident object into local stg_rgn

   function Get_Large_Obj_Type_Descriptor
      (Obj : Word_Type) return Non_Op_Map_Type_Ptr;
   --  For selection of polymorphic ancestor part in compiled code
   pragma Export (Ada, Get_Large_Obj_Type_Descriptor,
      "_psc_get_large_obj_type_descriptor");

   procedure Share_Lock
     (Large_Obj : Object_Virtual_Address);
   --  Share lock between polymorphic obj and contained obj or obj and its
   --  ancestor part. Caller must ensure that Obj is large as is its first
   --  component.

   procedure Spawn_Parallel_Thread
     (Context      : in out Exec_Context;
      Master_Addr  : Word_Ptr;
      New_Tcb      : Word_Ptr;
      Static_Link  : Word_Ptr;
      Routine      : Routine_Ptr;
      Tcb_Is_Local : Boolean;   -- Whether tcb address is in Local_Area
      Is_Start_Op  : Boolean;   -- Whether is Start_Par vs. Add_Par
      Nested_Block : Code_Block_Ptr := null;
      Start_Pc     : Code_Index := Code_Index'First;
      Locked_Param_Info : Locked_Param_Info_Type := Null_Locked_Param_Info;
      Master_Never_Shared : Boolean := False);
      --  Execute the Start/Add_Parallel_[Call_]Op instructions
      --  Nested_Block is null if this is invoked for compiled code
      --  or is a Start/Add_Parallel_Call_Op.
      --  Master_Never_Shared is True if this master should be executed
      --  all on the same server.

   function Stg_Rgn_Of_Existing_Large_Obj
     (Context                 : in out Exec_Context;
      Existing_Obj_In_Stg_Rgn : Object_Locator) return Stg_Rgn_Ptr;
   --  Return region associated with existing large obj, unless locator is
   --  null, in which case return local region.

   procedure Store_Local_Null
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Type_Info   : Object_Locator);
   --  We want to store a "large" null if the run-time type info implies that
   --  is necessary; the large null should be for the "local" region.
   --  We want to store the right "kind" of null if small.

   procedure Store_Null_Of_Same_Stg_Rgn
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info               : Object_Locator);
   --  We want to store a "large" null if the run-time type info implies that
   --  is necessary Stg_Rgn comes from Existing_Obj_In_Stg_Rgn if large.
   --  We want to store the right "kind" of null if small.

   procedure Stream_To_Basic_Array
     (Stream : in out String_Streams.Buffered_Stream'Class;
      Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
      Target : in out Word_Type;
      Server_Index : Thread_Server_Index);
   --  Convert buffered stream into a basic array, updating Target
   --  to refer to basic array.  Basic array is allocated in region
   --  of Target.

   procedure Swap_Obj
     (Context   : in out Exec_Context;
      LHS       : Object_Locator;
      RHS       : Object_Locator;
      Type_Info : Object_Locator);
   --  We want to release the old values of the destination and source before
   --  overwriting them if necessary.

   function To_Word_Ptr is
     new Ada.Unchecked_Conversion (System.Address, Word_Ptr);

   function To_Word_Ptr is
     new Ada.Unchecked_Conversion (Type_Descriptor_Ptr, Word_Ptr);

   procedure Wait_For_Open_Master (Context : in out Exec_Context);
   --  Wait for the Open_Master, then set Open_Master to null.

   ----------------
   -- Convention --
   ----------------

   Num_Inp_Width : constant := 8;
      --  Number of bits for count of input params within Conv Desc

   Num_Out_Width : constant := 8;
      --  Number of bits for count of output params within Conv Desc

   pragma Assert (Conv_Width + Num_Inp_Width + Num_Out_Width + 2 <
                    Convention_Descriptor'Size);
      --  Make sure the three fields + two Booleans fit within Conv Desc

   function Convention (Conv_Desc : Convention_Descriptor)
     return Languages.Convention_Enum is
      --  Extract Convention from Convention_Descriptor
   begin
      return Languages.Convention_Enum'Val (Conv_Desc mod 2 ** Conv_Width);
   end Convention;

   -------------------
   -- New_Conv_Desc --
   -------------------

   function New_Conv_Desc
     (Convention : Languages.Convention_Enum;
      Num_Inputs : Natural; Num_Outputs : Natural;
      Output_Needs_Init : Boolean;
      Uses_Queuing : Boolean)
     return Convention_Descriptor is
      --  Create convention descriptor given convention,
      --  number of inputs, outputs, whether output needs init,
      --  and whether routine uses queuing.
   begin
      return (((Boolean'Pos (Output_Needs_Init) * 2 +
                Boolean'Pos (Uses_Queuing)) * 2 ** Num_Out_Width +
               Convention_Descriptor (Num_Outputs)) * 2 ** Num_Inp_Width +
              Convention_Descriptor (Num_Inputs)) * 2 ** Conv_Width +
             Languages.Convention_Enum'Pos (Convention);
   end New_Conv_Desc;

   --  Convention used for imported subprograms without a specified convention
   External_Default_Conv_Desc : constant Convention_Descriptor :=
     New_Conv_Desc
        (Convention => Languages.Convention_External_Default,
         Num_Inputs => 1, --  TBD: Not known
         Num_Outputs => 1, --  TBD: Not known
         Output_Needs_Init => False,  --  TBD: Not known
         Uses_Queuing => False);  --  TBD: Not known

   --  We use a special convention for nested blocks
   --  which means there are no incoming parameters,
   --  and the output represents the "outcome" of the nested
   --  block.
   Nested_Block_Conv_Desc : constant Convention_Descriptor :=
     New_Conv_Desc
        (Convention => Languages.Convention_Nested_Block,
         Num_Inputs => 0,
         Num_Outputs => 1,
         Output_Needs_Init => False,
         Uses_Queuing => False);

   --  Convention descriptor appropriate for the no-op used in
   --  case of a mismatched dispatching "=?" which
   --  is defined to return #unordered.
   Mismatched_Compare_Conv_Desc : constant Convention_Descriptor :=
     New_Conv_Desc
        (Convention => Languages.Convention_External_Default,
         Num_Inputs => 2, Num_Outputs => 1,
         Output_Needs_Init => False,
         Uses_Queuing => False);

   --------------------------
   -- Delay_Queue_Handling --
   --------------------------

   package Delay_Queue_Handling is

      type Delay_Queue_Entry;
      type Delay_Queue_Entry_Ptr is access Delay_Queue_Entry;

      type Delay_Queue_Entry is record
         Tcb_Addr     : Word_Ptr;
         Delay_Until  : Ada.Calendar.Time;
         Server_Index : Thread_Server_Index;
         Next         : Delay_Queue_Entry_Ptr;
      end record;

      protected Delay_Queue is

         procedure Add_Tcb
           (Tcb_Addr     : Word_Ptr;
            Delay_Until  : Ada.Calendar.Time;
            Server_Index : Thread_Server_Index);
         --  Add a TCB to the delay queue

         procedure Remove_Tcb
           (Tcb_Addr          : Word_Ptr;
            Thread_Was_Queued : out Boolean);
         --  Remove a TCB from the delay queue, if there. Return
         --  Thread_Was_Queued to indicate that thread was in fact
         --  still queued.

         procedure Service_Queue
           (Thread_To_Wake_Up : out Word_Ptr;
            Next_Wakeup_Time  : out Ada.Calendar.Time;
            Shut_Down_Now     : out Boolean;
            Queue_Is_Empty    : out Boolean);
         --  Return with thread that is now ready. If none, return time when to
         --  next wakeup.

         entry Wait_For_Change;
         --  Wait for a change in the next wakeup time.

         procedure Dump_Delay_Queue;
         --  Dump current state of delay queue

         procedure Pause_Delay_Queue;
         --  Pause the servicing of the delay queue

         procedure Resume_Delay_Queue;
         --  Resume the servicing of the delay queue,
         --  and adjust all times to account for the length of the pause.

         procedure Shut_Down;
         --  Shut down the delay queue
      private

         Delay_Queue_Head   : Delay_Queue_Entry_Ptr;
         Free_List          : Delay_Queue_Entry_Ptr;

         Shut_Down_Now      : Boolean := False;
         Is_New_Wakeup_Time : Boolean := False;
         Clock_At_Pause     : Ada.Calendar.Time;
      end Delay_Queue;

      procedure Delay_Tcb_Until
        (Tcb_Addr    : Word_Ptr;
         Delay_Until : Ada.Calendar.Time);
      --  Add TCB to the delay queue, to wake up when the wall clock reaches
      --  Delay_Until.

      procedure Remove_From_Delay_Queue
        (Tcb_Addr          : Word_Ptr;
         Thread_Was_Queued : out Boolean);
      --  Special dequeue routine for the delay queue

   end Delay_Queue_Handling;
   use Delay_Queue_Handling;

   -----------------------
   -- Locked_And_Queued --
   -----------------------

   --  This local package contains the implementation of "locked" and "queued"
   --  parameters

   package Locked_And_Queued is

      function Dequeue_Condition_Satisfied
        (Context        : in out Exec_Context;
         Calling_Tcb    : Word_Ptr;
         Server_Index   : Thread_Server_Index;
         Called_Routine : Routine_Ptr := null)
         return Boolean;
      --  Return True if target of call by given TCB has no internal
      --  precondition/dequeue condition or it evaluates to #true.
      --  NOTE: Calling_TCB has indicator of routine it was created to execute.
      --        If this TCB was created to perform this parallel call, then the
      --        Calling_TCB's routine will match Called_Routine.
      --        If this TCB was created earlier, perhaps to execute a nested
      --        block, then the TCB's called routine will in general not match
      --        Called_Routine.

      procedure Execute_Nested_Block
        (Context            : in out Exec_Context;
         Instructions       : Routine_Ptr;
         Params_Address     : Word_Ptr;
         Static_Link        : Word_Ptr;
         Code_Block         : Code_Block_Descriptor;
         Server_Index       : Thread_Server_Index;
         Base_For_Pc_Offset : Code_Index := Code_Index'First);
      --  Execute a nested code block

      procedure Allocate_Lock_Obj (Lock_Obj : out Lock_Obj_Index;
        Server_Index : Thread_Server_Index);
      --  Get a unique index for lock obj

      procedure Release_Lock_Obj (Lock_Obj : Lock_Obj_Index;
        Obj_Stg_Rgn : Stg_Rgn_Ptr; Server_Index : Thread_Server_Index);
      --  Release lock object for use by other objects.
      --  Obj_Stg_Rgn is the storage region where the object resides;
      --  Server_Index is the index of the server doing the release.

   end Locked_And_Queued;
   use Locked_And_Queued;

   -------------------------
   -- PSVM_Thread_Support --
   -------------------------

   package PSVM_Thread_Support is

      procedure Execute_Locked_Call_Op
        (Target_Routine     : Routine_Ptr;
         New_Context        : in out Exec_Context;
         Locked_Param_Info  : Locked_Param_Info_Type;
         Server_Index       : Thread_Server_Index;
         Thread_Was_Queued  : out Boolean);
      --  Execute the call under a lock on the given parameter.
      --  Use the internal precondition as a dequeue condition.

      procedure Execute_For_Thread
        (New_Tcb           : Word_Ptr;
         Server_Index      : Thread_Server_Index;
         New_Local_Area    : Word_Ptr;
         Thread_Was_Queued : out Boolean;
         Already_Locked    : Boolean := False);
      --  Execute code specified in thread control block and then mark thread
      --  as complete. If Thread_Was_Queued upon return, then thread is
      --  *not* complete and instead is waiting for a dequeue condition to
      --  be satisfied.

      procedure Execute_Compiled_Code_Immediately
        (Context : in out Exec_Context; New_Tcb : Word_Ptr);
      --  Execute compiled code specified in thread control block,
      --  piggybacking on existing context rather than spawning a new thread.
      --  No locking param allowed.

      function Prepare_To_Exit
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Exiting_Tcb      : Word_Ptr;
         Raising_Excep    : Boolean := False) return Boolean;
      --  Prepare to exit specified master. If the prepare-to-exit on the
      --  master succeeds, then return True. If it fails because some other
      --  picothread has already performed a prepare-to-exit on the master or
      --  some enclosing master, then return False. If returning False, outcome
      --  has been set on enclosing master(s) so appropriate "abrupt" exit will
      --  occur.

      function Exit_Was_Requested
        (Thread_Master : Word_Ptr;
         Current_Tcb   : Word_Ptr) return Boolean;
         --  Return True if Exit has been requested for given master/tcb

      procedure Spawn_Thread
        (Context       : in out Exec_Context;
         Thread_Master : Word_Ptr;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr);
      --  Add a thread to queue of threads waiting for service

      procedure Finish_Thread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr);
      --  Indicate that processing for given thread is complete

      procedure Get_Unshared_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr;
         Subthread_Of_Master : Master_Index := 0);
      --  Get a thread from the given server's unshared queue.
      --  Return null Tcb_To_Run if there isn't one.
      --  If Subthread_Of_Master is > 0, then don't return a "queuing" Tcb
      --  unless it is a subthread of the specified master.

      procedure Get_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr);
      --  Get a thread that is waiting to be executed.
      --  Look first on specified server's queue.

      procedure Wait_For_Threads
        (Context          : in out Exec_Context;
         Thread_Master    : Word_Ptr;
         Holding_Lock_Obj : Lock_Obj_Index;
         New_Local_Area   : Word_Ptr);
      --  Wait for threads associated with given thread master;
      --  while not done, serve other threads.
      --  Return when all subthreads of master are done.

   end PSVM_Thread_Support;
   use PSVM_Thread_Support;

   -----------------------
   -- Thread_Scheduling --
   -----------------------

   package Thread_Scheduling is

      Uninit_Thread_Count : constant Thread_Count := Thread_Count'Last;
      --  Indicates that thread count is not meaningful; used in a master
      --  before it has been fully initialized.

      type Thread_Count_Array_Base is
        array (Master_Index range <>) of Thread_Count;
      subtype Thread_Count_Array is Thread_Count_Array_Base (Master_Index);
      --  Table of counts of number of still active sub-threads of
      --  given master

      -- Statistics --
      Max_Waiting_Shared_Threads : Thread_Count_Base := 0;
      Num_Waiting_Shared_Summed  : Longest_Natural := 0;

      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      Num_Active : Integer := 1;  --  Main thread is active from the start
      Max_Active : Natural := 1;

      Num_Masters : Natural := 0;
      Num_Shared_Masters : Natural := 0;

      --  This counts number of masters with the parent thread
      --  waiting on them.
      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      Num_Waiting_For_Subthreads : Integer := 0;
      Max_Waiting_For_Subthreads : Natural := 0;

      Num_Thread_Steals : Natural := 0;

      Num_Shared_Thread_Initiations      : Natural := 0;
      Num_Active_Summed_Over_Initiations : Longest_Natural := 0;

      --  NOTE: We use Integer here in case of a race condition producing a
      --        negative value.
      Num_Active_Masters : Integer := 0;
      Max_Active_Masters : Natural := 0;

      Max_Subthreads_Per_Master : Thread_Count := 0;

      type Lock_Subordinate_Count_Array is
        array (Lock_Obj_Index) of Thread_Count_Base;
      --  Type used to keep Count of number of waiting threads holding the
      --  given lock, or number of sub-locks with at least one waiting thread.

      ---- Layout of thread master ----
      type Master_Outcome_Enum is (Normal_Outcome,
                                   Return_From_Operation_Outcome,
                                   Exit_Outcome);

      type Master_Info_Rec is record
         Index                 : Master_Index := 0;
         Outcome               : Master_Outcome_Enum := Normal_Outcome;
         Exit_Level_Diff       : Natural range 0 .. 10 := 0;
         Exit_Skip_Count       : Code_Offset := 0;
         Master_Exit_Requested : Boolean := False;
      end record;

      pragma Pack (Master_Info_Rec);

      type Thread_Master_Rec is record
         Header          : Large_Obj_Header;
         --  Stg_Rgn is used when allocating

         --  TCBs
         Info            : Master_Info_Rec;
         First_Subthread : Word_Ptr := null;
      end record;

      type Thread_Master_Ptr is access all Thread_Master_Rec;
      for Thread_Master_Ptr'Storage_Size use 0;

      for Thread_Master_Rec use record
         --  Make sure header at front
         Header at 0 range 0 .. Word_Size * Large_Obj_Header_Size - 1;
      end record;

      pragma Assert
        ((Thread_Master_Rec'Size + Word_Size - 1) / Word_Size <=
             Thread_Master_Size);

      ---- Layout of Thread control block ----

      --  An enumeration of the possible states of a picothread
      type Thread_State_Enum is
        (Initial_State, Unknown_State, Running, Waiting_For_Master,
         On_Server_Unshared_Queue, On_Server_Shared_Queue,
         On_Lock_Queue, On_Delay_Queue, Final_State);

      subtype On_Server_Queue is Thread_State_Enum
        range On_Server_Unshared_Queue .. On_Server_Shared_Queue;

      type Tcb_Info_Rec (For_Compiled_Routine : Boolean := False) is record

         --  Current state of thread
         Thread_State : Thread_State_Enum := Initial_State;

         Call_Can_Be_Queued : Boolean := False;
         --  Indicates that this is a locking call with
         --     Start_PC = Code_Index'First,
         --  so call may be queued.
         --  This is currently used only on a call on an imported routine.
         --  If this flag is false, then the effective dequeue condition of the
         --  imported routine must already have been checked before the call.

         Thread_Was_Queued : Boolean := False;
         --  Indicates that thread was queued by an imported routine.
         --  This can only be set if Call_Can_Be_Queued is set.

         Uses_Queuing : Boolean := False;
         --  Indicates that thread uses queuing internally, directly or
         --  indirectly. A thread with this flag on should not be served while
         --  waiting for a master, since it could cause indefinite suspension.

         Thread_Exit_Requested : Boolean := False;
         --  This indicates that this thread should exit as soon as possible.

         For_Nested_Block     : Boolean := False;
         --  This indicates this is for a nested block

         Locked_Param_Info  : Locked_Param_Info_Type;

         --  The above fields occupy 16 + 5 + 1 + 1 + 1 + 1 + 3 + 1 = 29

         case For_Compiled_Routine is
         when True =>
            --  Used for compiled code
            Conv_Desc             : Convention_Descriptor := Null_Conv_Desc;
            Code_Addr             : Routine_Code_Address := null;
            Internal_Precond_Addr : Nested_Blk_Address := null;

         when False =>
            --  Used for interpreted code
            Routine_Id          : Routine_Index := 0;
            Start_Pc            : Code_Index := Code_Index'First;

            Local_Area_Length   : Offset_Within_Area range 0 .. 2047 := 0;
            Start_Callee_Locals : Offset_Within_Area range 0 .. 2047 := 0;
            --  These last two are only used if Start_PC > Code_Index'First
         end case;

      end record;

      pragma Pack (Tcb_Info_Rec);

      type Tcb_Rec is record
         Header         : Large_Obj_Header;
         --  NOTE : Header.Size includes the size of the parameter list

         Self_Address   : Object_Virtual_Address := Null_Virtual_Address;
         --  We store a TCB's own address when it is allocated dynamically

         Info           : Tcb_Info_Rec;
         Master_Ptr     : Word_Ptr := null;
         --  This normally points to the master of the thread.
         --  However, when one thread spawns another thread, it
         --  temporarily points to the master of the spawned thread.
         --  It is set back to the original master after it completes
         --  waiting on the master.

         Next_Subthread : Word_Ptr := null;
         --  Link on singly-threaded chain hanging off of master

         Next_Waiting   : Word_Ptr := null;
         --  "Next" link on doubly-threaded chain on server queue

         Prev_Waiting   : Word_Ptr := null;
         --  "Prev" link on doubly-threaded chain on server queue

         Static_Link    : Word_Ptr := null;
         --  Parameter list follows
      end record;

      type Tcb_Ptr is access all Tcb_Rec;
      for Tcb_Ptr'Storage_Size use 0;

      for Tcb_Rec use record
         --  Make sure header at front
         Header at 0 range 0 .. Word_Size * Large_Obj_Header_Size - 1;
      end record;

      Tcb_Param_List_Offset : constant Offset_Within_Area :=
                                Thread_Control_Block_Size;
      --  Offset to beginning of parameter list
      --  We presume param list starts right after TCB

      pragma Assert
        ((Tcb_Rec'Size + Word_Size - 1) / Word_Size <=
             Thread_Control_Block_Size);

      Max_Tcb_Size_Including_Params : constant Offset_Within_Area :=
                                        Tcb_Param_List_Offset + 20;
      --  "20" for number of parameters seems very generous.
      --  NOTE: This is just a sanity check when we are passed
      --        a TCB which is supposed to have its size already
      --        initialized.

      ---------------------------  Tcb operations  ---------------------------

      function Addr_To_Tcb_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Tcb_Ptr);

      procedure Add_To_Deque
        (Deque   : in out Thread_Deque_Head;
         New_Tcb : Word_Ptr;
         Is_Shared : Boolean);
      --  Add Tcb to end of deque; Is_Shared indicates whether queue is shared

      procedure Remove_From_Deque
        (Deque             : in out Thread_Deque_Head;
         Tcb_To_Be_Removed : Word_Ptr);
      --  Remove Tcb from deque

      procedure Initialize_Tcb
        (Tcb_Addr            : Word_Ptr;
         Tcb_Is_Local        : Boolean;   -- Whether tcb is in Local_Area
         Routine             : Routine_Ptr;
         Static_Link         : Word_Ptr;
         Locked_Param_Info   : Locked_Param_Info_Type :=
                                 Null_Locked_Param_Info;
         Current_Lock_Index  : Lock_Obj_Index := 0;
         Nested_Block        : Code_Block_Ptr := null;
         Start_Pc            : Code_Index := Code_Index'First);

      function Is_Subthread
        (Waiting_Tcb : Word_Ptr;
         Master      : Master_Index) return Boolean;
      --  Return True if given thread is subthread of given master,
      --  directly or indirectly.

      procedure Finish_Subthread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr);

      function Next_Subthread
        (Tcb_Addr : Word_Ptr)
        return Word_Ptr;

      function Next_Waiting_Tcb
        (Tcb_Addr : Word_Ptr)
        return Word_Ptr;

      function Prev_Waiting_Tcb
        (Tcb_Addr : Word_Ptr)
        return Word_Ptr;

      procedure Set_Next_Subthread
        (Tcb_Addr : Word_Ptr;
         Next     : Word_Ptr);

      procedure Set_Next_Waiting_Tcb
        (Tcb_Addr : Word_Ptr;
         Next     : Word_Ptr);

      procedure Set_Prev_Waiting_Tcb
        (Tcb_Addr : Word_Ptr;
         Prev     : Word_Ptr);

      procedure Set_Tcb_Call_Can_Be_Queued
        (Tcb_Addr      : Word_Ptr;
         Can_Be_Queued : Boolean);

      procedure Set_Tcb_Exit_Requested
        (Tcb_Addr       : Word_Ptr;
         Exit_Requested : Boolean);

      procedure Set_Tcb_Master_Ptr
        (Tcb_Addr    : Word_Ptr;
         Master_Addr : Word_Ptr);

      procedure Set_Tcb_Size
        (Tcb_Addr : Word_Ptr;
         Size     : Offset_Within_Area);

      procedure Set_Tcb_Start_Pc
        (Tcb_Addr : Word_Ptr;
         Start_Pc : Code_Index);

      procedure Set_Tcb_State
        (Tcb_Addr  : Word_Ptr;
         New_State : Thread_State_Enum);

      procedure Set_Tcb_Uses_Queuing
        (Tcb_Addr     : Word_Ptr;
         Uses_Queuing : Boolean);

      procedure Set_Tcb_Was_Queued
        (Tcb_Addr   : Word_Ptr;
         Was_Queued : Boolean);

      function Tcb_Call_Can_Be_Queued
        (Tcb_Addr : Word_Ptr)
        return Boolean;

      function Tcb_Code_Addr
        (Tcb_Addr : Word_Ptr)
        return Routine_Code_Address;

      function Tcb_Conv_Desc
        (Tcb_Addr : Word_Ptr)
        return Convention_Descriptor;

      function Tcb_Exit_Requested
        (Tcb_Addr : Word_Ptr)
         return Boolean;

      function Tcb_For_Compiled_Routine
        (Tcb_Addr : Word_Ptr) return Boolean;

      function Tcb_For_Nested_Block
        (Tcb_Addr : Word_Ptr) return Boolean;

      function Tcb_Internal_Precond_Addr
        (Tcb_Addr : Word_Ptr)
        return Nested_Blk_Address;

      function Tcb_Local_Area_Length
        (Tcb_Addr : Word_Ptr)
         return Offset_Within_Area;

      function Tcb_Locked_Param_Index
        (Tcb_Addr : Word_Ptr)
         return Natural;

      function Tcb_Locked_Param_Info
        (Tcb_Addr : Word_Ptr)
         return Locked_Param_Info_Type;

      function Tcb_Master_Ptr
        (Tcb_Addr : Word_Ptr)
         return Word_Ptr;

      function Tcb_Routine_Index
        (Tcb_Addr : Word_Ptr)
         return Routine_Index;

      function Tcb_Size
        (Tcb_Addr : Word_Ptr)
         return Offset_Within_Area;

      function Tcb_Start_Callee_Locals
        (Tcb_Addr : Word_Ptr)
         return Offset_Within_Area;

      function Tcb_Start_Pc
        (Tcb_Addr : Word_Ptr)
         return Code_Index;

      function Tcb_State
        (Tcb_Addr : Word_Ptr)
        return Thread_State_Enum;

      function Tcb_Static_Link
        (Tcb_Addr : Word_Ptr)
         return Word_Ptr;

      function Tcb_Uses_Queuing
        (Tcb_Addr : Word_Ptr)
         return Boolean;

      function Tcb_Was_Queued
        (Tcb_Addr : Word_Ptr)
        return Boolean;

      function Word_To_Tcb_Ptr
        (Tcb_Word_Addr : Word_Ptr) return Tcb_Ptr;
      --  Return pointer to TCB at given address
      pragma Export (Ada, Word_To_Tcb_Ptr, "word_to_tcb");
      pragma Inline (Word_To_Tcb_Ptr);

      --------- Thread Master access operations ---------

      function Exit_Level_Diff
        (Master_Addr : Word_Ptr)
         return Natural;

      function Exit_Skip_Count
        (Master_Addr : Word_Ptr)
         return Code_Offset;

      function First_Subthread
        (Master_Addr : Word_Ptr)
         return Word_Ptr;

      function Index_Of_Master
        (Master : Word_Ptr)
         return Master_Index;

      procedure Initialize_Master
        (Master_Address          : Word_Ptr;
         Local_Stg_Rgn_Index     : Stg_Rgn_Index;
         Server_Index            : Thread_Server_Index;
         Initial_Subthread_Count : Thread_Count := Uninit_Thread_Count;
         Spawning_Tcb            : Word_Ptr := null;
         Never_Shared            : Boolean := False);
      --  Do one-time initialization of thread master

      procedure Initialize_Handler
        (Master_Address : Word_Ptr;
         Routine        : Routine_Ptr;
         Handler_Block  : Code_Block_Ptr;
         Handler_Start  : Code_Index);
      --  Initialize master that has an exception handler

      procedure Release_Master
        (Index        : Master_Index;
         Server_Index : Thread_Server_Index);
      --  Release the master index and the associated "extra" master rec

      function Master_Exit_Requested
        (Master_Addr : Word_Ptr)
         return Boolean;

      function Master_Outcome
        (Master_Addr : Word_Ptr)
         return Master_Outcome_Enum;

      procedure Set_First_Subthread
        (Master_Addr : Word_Ptr;
         First       : Word_Ptr);

      procedure Set_Index_Of_Master
        (Master : Word_Ptr;
         Index  : Master_Index);

      procedure Set_Enclosing_Master_Outcome
        (Context         : in out Exec_Context;
         Outcome         : Master_Outcome_Enum;
         Exit_Level_Diff : Natural := 0;
         Exit_Skip_Count : Code_Offset := 0);
      --  Set "outcome" field of enclosing master
      --  Should not be called with "Normal" outcome (or Exit_Outcome
      --  with Exit_Level_Diff = 1 and Skip_Count = 0).

      procedure Set_Master_Exit_Requested
        (Master_Addr : Word_Ptr;
         Exit_Requested : Boolean);

      function To_Master_Ptr
        (Master_Word_Addr : Word_Ptr)
        return Thread_Master_Ptr;
      --  Return pointer to Master at given address

      function To_Master_Ptr
        (Master_Addr : Object_Address)
        return Thread_Master_Ptr;
      --  Return pointer to Master at given address

      procedure Add_To_Master
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr);
      --  Link TCB onto chain off master

   end Thread_Scheduling;
   use Thread_Scheduling;

   -------------------------
   -- Thread_Manager_Data --
   -------------------------

   package Thread_Manager_Data is
      --  NOTE: This is data managed by the Thread_Manager.
      --        We are putting it in a non-protected package
      --        to make debugging easier.

      Num_Waiting_Shared_Threads : Thread_Count_Base := 0;
      --  Total number of threads waiting on any shared server queue

      Num_Nonqueuing_Threads : Thread_Count_Base := 0;
      --  Number of waiting threads that don't use queuing.

      Is_Shut_Down  : Boolean := False;
      pragma Atomic (Is_Shut_Down);
      --  Set True when ParaSail run-time is being shut down.

      --  Extra information on masters
      Master_Extras : Master_Extra_Array_Type;

      Num_Lock_Subordinates : Lock_Subordinate_Count_Array := (others => 0);
      --  Count of number of waiting threads holding the given lock,
      --  or number of sub-locks with at least one waiting thread.
      --  Num_Lock_Subordinates(0) is count of threads not holding a lock
      --  and number of locks with at least one waiting thread.

      Num_Shared_Threads_Needed : Thread_Count := 0;
      pragma Atomic (Num_Shared_Threads_Needed);
      --  If this is greater than zero, then this many threads should be
      --  moved to the shared queue.

      Num_Servers_Waiting_For_Masters : Natural := 0;
      --  Number of servers currently on one of the
      --  Get_Thread_Or_Wait_For_Threads_Internal queues.

      Max_Servers_Waiting_For_Masters : Natural := 0;
      --  Maximum number of servers ever waiting.

      Solo_Server : Thread_Server_Index_Base := 0;
      --  This is set non-zero when a server is asking all other servers
      --  to pause their processing, e.g., to permit the debug
      --  console to interact with the user without anything else running.

      Pause_Nesting : Natural := 0;
      --  If non-zero, we are in the middle of a pause.
      --  This counts how many nested pauses we are inside of.

      Last_Master : Master_Index := 0;
      --  Index of last assigned master index.

      Last_Server_Index : Thread_Server_Index := Main_Thread_Server_Index;
      --  Index of last server thread created

      Last_Lock_Obj : Lock_Obj_Index := 0;
      --  Index of last lock object created.
   end Thread_Manager_Data;

   --------------------
   -- Thread_Manager --
   --------------------

   protected Thread_Manager is
      --  This protected object manages the creating and
      --  serving of "pico" threads.
      --  TBD: Finer-grained locking might be useful someday.

      procedure Next_Lock_Obj (Lock_Obj : out Lock_Obj_Index);
      --  Get next unique index for lock obj

      procedure Finish_Thread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr);
      --  Indicate that processing for given thread is complete

      procedure Next_Master_Index (Index : out Master_Index);
      --  Return next unique index for a master

      procedure Get_Server_Index (Index : out Thread_Server_Index);
      --  Get a unique index for a thread server

      entry Get_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr);
      --  Get a thread that is waiting to be executed.
      --  Look first on specified server's queue.

      entry Get_Thread_Or_Wait_For_Threads
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Tcb_Waiting      : Word_Ptr;
         Tcb_To_Run       : out Word_Ptr);
      --  Wait for threads associated with given thread master;
      --  if not done, get a thread to serve;
      --  if done, set Tcb_To_Run to null.

      procedure Increase_Shared_Threads
        (Server_Index : Thread_Server_Index);
      --  Attempt to move threads from server's unshared deque
      --  to the shared deque of threads.

      entry Prepare_To_Exit
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Exiting_Tcb      : Word_Ptr;
         Succeeded        : out Boolean;
         Raising_Excep    : Boolean := False);
      --  Prepare to exit given master.
      --  Set Succeeded to indicate whether the prepare-to-exit
      --  succeeded.

      procedure Set_Max_Dynamically_Allocated_Servers (Max : Natural);
      --  Set new value for max. number of dynamically allocated servers

      procedure Shut_Down;
      --  Return zero to all waiting threads

      entry Spawn_New_Server
        (Num_Live      : out Natural;
         Shut_Down_Now : out Boolean);
      --  Entry that returns when we need a new server task
      --  TBD: We aren't really counting number of active/live servers.
      --      Instead we are just making sure we have some waiting
      --      for new threads to be created.

      procedure Add_To_Shared_Master
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr);
         --  Link TCB onto chain off shared master

      procedure Spawn_Shared_Thread
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr);
      --  Add a thread to queue of threads waiting for service

      entry Pause_Other_Servers (Server_Index : Thread_Server_Index);
      --  Pause other servers; return when they have all stopped
      --  servicing their queues.

      entry Resume_Other_Servers (Server_Index : Thread_Server_Index);
      --  Allow other servers to resume servicing their queues.

      procedure Dump_Locks;
      --  Dump the state of the locks.

      procedure Dump_Thread_State;
      --  Show state of threads

      procedure Dump_Thread_Tree
        (Inner_Master : Master_Index := 1;
         Outer_Master : Master_Index := 0;
         Label        : String := "";
         Indent       : Natural := 2);
      --  Display all subthreads of Inner_Master
      --  and any masters enclosing Inner_Master up to,
      --  but not including, Outer_Master.

   private

      entry Get_Thread_Or_Wait_For_Threads_Internal (Thread_Server_Index)
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Tcb_Waiting      : Word_Ptr;
         Tcb_To_Run       : out Word_Ptr);
      --  Wait for threads of given server's current thread master.
      --  if not done, get a thread to serve;
      --  if done, set Tcb_To_Run to null.

      --  NOTE: Private data has been moved to external package
      --        "Thread_Manager_Data" to ease debugging.
      --        It logically belongs here.

      entry Wait_For_Other_Servers_To_Pause
        (Server_Index : Thread_Server_Index);
      --  Wait for other servers to pause; return when they have all stopped
      --  servicing their queues.

      entry Wait_For_End_Of_Pause (Server_Index : Thread_Server_Index);
      --  Wait until Solo_Server is 0, and then requeue back on
      --  Pause_Other_Servers

   end Thread_Manager;

   use Thread_Manager_Data;  --  TBD: This should be inside Thread_Manager

   -----------
   -- Debug --
   -----------

   package Debug is

      function Area_Base_Image (Area : Area_Base_Indicator) return String;
      --  Return string representation for area base

      function Chunk_Addr (Chunk : Stg_Rgn_Chunk_Ptr) return Word_Ptr;
      --  Return start addr of chunk
      pragma Export (Ada, Chunk_Addr, "chunk_addr");

      function Code_Block_Image
        (Code_Block : Code_Block_Descriptor) return String;
      --  Return image of info in Code_Block_Descriptor

      function Direction_Image (Dir : Direction) return String;
      --  Return "unordered," "forward," "reverse," or "concurrent."

      procedure Dump_Chunk (Chunk : Stg_Rgn_Chunk_Ptr);
      --  Display information in chunk
      pragma Export (Ada, Dump_Chunk, "dump_chunk");

      procedure Dump_Code
        (Code : Code_Ptr);
      --  Display instructions of Code_Type for debugging purposes.

      procedure Dump_Dq;
      --  Dump the delay queue
      pragma Export (Ada, Dump_Dq, "dump_dq");

      procedure Dump_Locks;
      --  Dump state of locks
      pragma Export (Ada, Dump_Locks, "dump_locks");

      procedure Dump_Obj_Locator
        (Locator : Object_Locator);
      pragma Export (Ada, Dump_Obj_Locator, "dump_obj_locator");

      procedure Dump_Obj_With_Indent
        (Value     : Word_Type;
         Type_Desc : Type_Descriptor_Ptr := null;
         Indent    : Natural := 0);
      --  Dump contents of object with given indent

      procedure Dump_One_Instruction (Instr : Instruction;
                                      Use_Message_Format : Boolean := False);
      --  Dump one instruction, either in an instruction format,
      --  or in the "message" format that is understood by "vim -q" and
      --  other similar tools.

      procedure Dump_Param
        (Param          : Routine_Param_Info;
         Use_Short_Form : Boolean := False);
      --  Display information about one parameter

      procedure Dump_Param_Decls
        (Code : Routine_Ptr);
      --  Dump declarations of parameters

      procedure Dump_Param_Values
        (Code        : Routine_Ptr;
         Context     : in out Exec_Context;
         On_Entering : Boolean);

      procedure Dump_Routine (Code : Routine_Ptr);
      --  Display instructions of routine for debugging purposes.

      procedure Dump_Routine_Info (Info : Routine_Info;
        Current_Op_Index  : Operation_Index := 0;
        Current_Type_Desc : Type_Descriptor_Ptr := null);
      --  Display a Routine_Info for debugging purposes.

      procedure Dump_State;
      --  Dump the state of the interpreter
      pragma Export (Ada, Dump_State, "dump_state");

      procedure Dump_Type_Element_Data
        (Data   : Element_Info;
         Indent : String := "");
      --  Dump the data associated with a nested object or parameter

      procedure Dump_Type_Desc
        (Type_Desc : Type_Descriptor_Ptr);
      --  Produce a human-readable display of the contents of a type descriptor
      pragma Export (Ada, Dump_Type_Desc, "dump_type_desc");

      function Hex_Image
        (Addr              : Object_Virtual_Address;
         Underscores_Every : Natural := 4) return String;
      --  Produce hex image of given address, with underscores
      --  inserted periodically

      function Hex_Image
        (Addr              : Word_Ptr;
         Underscores_Every : Natural := 4) return String;
      --  Produce hex image of given address, with underscores
      --  inserted periodically

      function Hex_Image
        (Addr              : Routine_Code_Address;
         Underscores_Every : Natural := 4) return String;
      --  Produce hex image of given address, with underscores
      --  inserted periodically

      function Integer_Value (Img : String) return Word_Type;
      --  Convert image to a value.
      --  Allow 0xFF, 0b01, 16#ff#, etc.
      --  Recognize "null" and return Null_Value
      --  TBD: Handle bases > 16

      function Obj_Address_Image (Addr : Object_Address) return String;
      --  Return image of given object-address as a pair

      function Obj_Locator_Image
        (Locator : Object_Locator) return String;

      function Real_Image (Val : Univ_Real) return String;
      --  Return Val as an image, removing leading blank from
      --  Ada 'image; recognize null value and return "null"

      function Real_Value (Img : String) return Univ_Real;
      --  Return Img as a Univ_Real value
      --  Recognize "null" and return Null_Real_Value

      function Type_Sem_Image
        (Type_Sem       : Trees.Root_Sem_Ptr;
         Use_Short_Form : Boolean := False)
         return String;
      --  Return an image of a type, given its type semantic info

      function Virt_To_Large_Obj_Ptr
        (Large_Obj_Virt_Addr : Object_Virtual_Address)
         return Large_Obj_Header_Ptr;
      --  Convert virt. addr of large object to a pointer to header type
      pragma Export (Ada, Virt_To_Large_Obj_Ptr, "virt_to_obj");

      function To_Large_Obj_Ptr
        (Large_Obj_Virt_Addr : Object_Virtual_Address)
         return Large_Obj_Header_Ptr renames Virt_To_Large_Obj_Ptr;

   end Debug;
   use Debug;

   --------------------------
   -- Stg_Rgn_Manager_Type --
   --------------------------

   protected type Stg_Rgn_Manager_Type is
      --  Type used to manage allocation/deallocation from associated
      --  storage region.

      procedure Allocate_From_Stg_Rgn
        (Stg_Rgn       : Stg_Rgn_Ptr;
         Size_In_Words : Offset_Within_Area;
         Obj_Addr      : out Word_Type;
         Server_Index  : Thread_Server_Index);
      --  This attempts to reuse storage freed by Deallocate_From_Stg_Rgn.
      --  Initialize allocated space with region/size/null-type.

      procedure Deallocate_From_Stg_Rgn
        (Stg_Rgn         : Stg_Rgn_Ptr;
         Storage_Address : Object_Virtual_Address;
         Server_Index    : Thread_Server_Index);
      --  This adds the given storage to a list indexed by the size.
      --  Requires: Object has size embedded in its header.

      procedure Borrow_Stg_Rgn_Chunk
        (Enclosing_Stg_Rgn : Stg_Rgn_Ptr;
         Min_Size : Offset_Within_Area;
         Borrowed_Chunk : out Stg_Rgn_Chunk_Ptr);
      --  Borrow a chunk from enclosing region that has at least
      --  the given amount of space.
      --  Borrowed_Chunk is null if region has no appropriate chunk.
      --  Mark/Depth-of-mark will be set appropriately.

      procedure Return_Stg_Rgn_Chunk
        (Chunk             : Stg_Rgn_Chunk_Ptr;
         Enclosing_Stg_Rgn : Stg_Rgn_Ptr);
      --  Return chunk back to enclosing region

      procedure Perform_Shared_Stg_Rgn_Action
        (Shared_Rgn : Stg_Rgn_Ptr;
         Action : in out Stg_Rgn_Action_Type'Class;
         Server_Index : Thread_Server_Index);
      --  Perform the given action while inside the manager of the
      --  given shared-stg-rgn manager.

   end Stg_Rgn_Manager_Type;

   procedure Invoke_Shared_Stg_Rgn_Action
     (Shared_Rgn : Stg_Rgn_Ptr;
      Action : in out Stg_Rgn_Action_Type'Class;
      Server_Index : Thread_Server_Index) is
   --  Invoke the given action while inside the manager of the
   --  given shared-stg-rgn manager.
   begin
      --  Just pass the buck to the protected operation
      Shared_Rgn.Manager.Perform_Shared_Stg_Rgn_Action
        (Shared_Rgn, Action, Server_Index);
   end Invoke_Shared_Stg_Rgn_Action;

   ---------------------
   -- Stg_Rgn_Manager --
   ---------------------

   protected Stg_Rgn_Manager is
      --  This creates storage regions and storage-region chunks

      procedure Create_Stg_Rgn
        (New_Stg_Rgn       : out Stg_Rgn_Ptr);
      --  Return a region that can be used as a local region.

      procedure Get_Stg_Rgn_Chunk_Index
        (Index : out Chunk_Index);
      --  Get a unique region chunk index

      procedure Initialize_Global_Stack_Chunk;
      --  Initialize the global stack chunk if not yet initialized

      function Num_Stg_Rgn_Chunks return Natural;
      --  Return count of region chunks allocated.

   private

      Last_Chunk_Index : Chunk_Index := 1;  --  Main program uses chunk "1"

   end Stg_Rgn_Manager;

   --------------- Local Subprograms ------------------

   -------------------------
   -- Allocate_Local_Area --
   -------------------------

   function Allocate_Local_Area
     (Context           : in out Exec_Context;
      Local_Area_Length : Offset_Within_Area) return Word_Ptr is
   begin
      return null;  --  TBD
   end Allocate_Local_Area;

   -----------------
   -- Assign_Word --
   -----------------

   procedure Assign_Word
     (Context     : in out Exec_Context;
      Type_Desc   : Type_Descriptor_Ptr;
      Destination : Word_Ptr;
      New_Value   : Word_Type) is
   begin
      if not Is_Small (Type_Desc) then
         declare
            Old_Value : constant Word_Type := --  Retrieve old value
              Fetch_Word (Destination, 0);
         begin
            Check_Is_Large (New_Value);

            if not Is_Special_Large_Value (Old_Value) then
               --  Preserve the former lock, if any
               declare
                  Old_Lock : constant Lock_Obj_Index :=
                    Large_Obj_Lock_Obj (Old_Value);
               begin
                  if Old_Lock /= 0
                    and then Large_Obj_Lock_Obj (New_Value) = 0
                  then
                     pragma Assert (Old_Lock in 1 .. Lock_Obj_Limit);
                     Set_Large_Obj_Lock_Obj (Old_Value, 0);
                     Set_Large_Obj_Lock_Obj (New_Value, Old_Lock);
                  end if;
               end;
            end if;

            if Large_Obj_On_Stack (New_Value)
              and then
               (not Large_Obj_On_Stack (Old_Value)
                  or else Stg_Rgn_Of_Large_Obj (New_Value) /=
                          Stg_Rgn_Of_Large_Obj (Old_Value))
            then
               --  Overwrite destination with shallow copy of new value
               --  since old value is not also on the stack, or is
               --  associated with a different region.
               Store_Word (Destination, 0,
                 Shallow_Copy_Large_Obj (Context, New_Value));
            else
               --  Overwrite destination
               Store_Word (Destination, 0, New_Value);
            end if;

            --  Now release storage of prior value
            --  NOTE: We do it in this order so we never
            --       have an object pointing at freed storage.
            --       Also, it means we could reclaim the storage
            --       in the background (e.g. in a separate thread).
            Release_Large_Obj (Type_Desc, Old_Value,
              Server_Index => Context.Server_Index);
         end;
      else
         --  No need to reclaim storage; just overwrite destination
         Store_Word (Destination, 0, New_Value);
      end if;
   end Assign_Word;

   procedure Assign_Word
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Source      : Object_Locator;
      Type_Info   : Object_Locator)
   is
      Type_Desc : constant Type_Descriptor_Ptr :=
                    Get_Type_Desc (Context, Type_Info);
      New_Value : constant Word_Type := Fetch_Word (Context, Source);
   begin
      --  Just pass the buck to version that takes Word_Type/Word_Ptr
      Assign_Word (Context, Type_Desc,
        Locator_To_Physical_Address (Context, Destination),
        New_Value);
   end Assign_Word;

   ---------------------------
   -- Call_Compiled_Routine --
   ---------------------------

   procedure Call_Compiled_Routine
     (Context     : in out Exec_Context;
      Params      : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr;
      Code_Addr   : Routine_Code_Address;
      Conv_Desc   : Convention_Descriptor) is
   --  Call through Code_Addr passing parameters
   --  from Params vector according to Conv_Desc.
      use Languages;
      pragma Assert (Conv_Desc /= Null_Conv_Desc);

      subtype Word_Array_Max is Word_Array (Offset_Within_Area);
      type Word_Array_Max_Ptr is access all Word_Array_Max;
      pragma No_Strict_Aliasing (Word_Array_Max_Ptr);

      function To_Word_Array is
        new Ada.Unchecked_Conversion (Word_Ptr, Word_Array_Max_Ptr);
   begin
      --  Move parameters into registers according to Conv_Desc
      case Convention (Conv_Desc) is
      when Convention_Internal_Default
         --  No convention specified, and not import
        | Convention_Locking_Default
         --  No convention specified, locking
        |  Convention_ParaSail
         --  #parasail convention specified
        |  Convention_Sparkel
         --  #sparkel convention specified
        |  Convention_Parython
         --  #parython convention specified
        |  Convention_Javallel =>
         --  #javallel convention specified

         --  Pass everything in registers

         if Num_Outputs (Conv_Desc) = 0 then
            --  There is no output
            case Num_Inputs (Conv_Desc) is
            when 0 =>
               declare
                  type Proc_Ptr is access procedure
                    (Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)(Context, Static_Link);
               end;
            when 1 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0), Context, Static_Link);
               end;
            when 2 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Context, Static_Link);
               end;
            when 3 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2),
                     Context, Static_Link);
               end;
            when 4 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3, Inp_4 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Context, Static_Link);
               end;
            when 5 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3, Inp_4, Inp_5 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Context, Static_Link);
               end;
            when others =>
               --  NYI
               raise Too_Many_Inputs;
            end case;

         elsif Output_Needs_Init (Conv_Desc) then
            --  The output needs initialization
            case Num_Inputs (Conv_Desc) is
            when 0 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Context, Static_Link, Param_Array (0)));
               end;
            when 1 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1), Context, Static_Link,
                     Param_Array (0)));
               end;
            when 2 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Context, Static_Link,
                     Param_Array (0)));
               end;
            when 3 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Context, Static_Link,
                     Param_Array (0)));
               end;
            when 4 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Context, Static_Link,
                     Param_Array (0)));
               end;
            when 5 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4, Inp_5 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr;
                     Inited_Output : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Param_Array (5),
                     Context, Static_Link,
                     Param_Array (0)));
               end;
            when others =>
               --  NYI
               raise Too_Many_Inputs;
            end case;
         else
            --  The output does not need initialization
            case Num_Inputs (Conv_Desc) is
            when 0 =>
               declare
                  type Func_Ptr is access function
                    (Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Context, Static_Link));
               end;
            when 1 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1), Context, Static_Link));
               end;
            when 2 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Context, Static_Link));
               end;
            when 3 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Context, Static_Link));
               end;
            when 4 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Context, Static_Link));
               end;
            when 5 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4, Inp_5 : Word_Type;
                     Context : in out Exec_Context;
                     Static_Link : Non_Op_Map_Type_Ptr)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Param_Array (5),
                     Context, Static_Link));
               end;
            when others =>
               --  NYI
               raise Too_Many_Inputs;
            end case;
         end if;

      when Convention_External_Default =>
         --  No convention specified, and is import
            Code_Addr (Context, Params, Static_Link);

      when Convention_Queuing_Default =>
         --  No convention specified, queuing
            Code_Addr (Context, Params, Static_Link);

      when Convention_Nested_Block =>
         --  Compiled nested blocks return an indication of outcome
         --  (multi-level exit or return).
         declare
            function Import_NBO is new Ada.Unchecked_Conversion
                  (Nested_Block_Outcome_As_Int, Nested_Block_Outcome);
            Nested_Block : constant Nested_Blk_Address :=
              To_Nested_Blk_Address (Code_Addr);
            Block_Outcome : constant Nested_Block_Outcome :=
              Import_NBO (Nested_Block.all
                 (Context, Params, Static_Link));
         begin
            if Block_Outcome.Level <=
               Nested_Block_Return_Outcome_Level
            then
               --  A "return"
               Set_Enclosing_Master_Outcome (Context,
                 Outcome         => Return_From_Operation_Outcome);
            elsif Block_Outcome.Level > 0
              or else Block_Outcome.Skip > 0
            then
               --  A multi-level "exit", or a non-zero "skip" count
               Set_Enclosing_Master_Outcome (Context,
                 Outcome         => Exit_Outcome,
                 Exit_Level_Diff => Natural (Block_Outcome.Level + 1),
                 Exit_Skip_Count => Block_Outcome.Skip);
            end if;
         end;

      when Convention_Ada
         --  #ada convention specified
        |  Convention_C
         --  #c convention specified
        |  Convention_CPP =>
         --  #cpp convention specified

         --  Pass parameters in registers, but no context, inited output, etc.
         if Num_Outputs (Conv_Desc) = 0 then
            --  There is no output
            case Num_Inputs (Conv_Desc) is
            when 0 =>
               declare
                  type Proc_Ptr is access procedure;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr).all;
               end;
            when 1 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1 : Word_Type);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0));
               end;
            when 2 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2 : Word_Type);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1));
               end;
            when 3 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3 : Word_Type);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2));
               end;
            when 4 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3, Inp_4 : Word_Type);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2),
                     Param_Array (3));
               end;
            when 5 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Proc_Ptr is access procedure
                    (Inp_1, Inp_2, Inp_3, Inp_4, Inp_5 : Word_Type);
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Proc_Ptr);
               begin
                  Conv (Code_Addr)
                    (Param_Array (0),
                     Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4));
               end;
            when others =>
               --  NYI
               raise Too_Many_Inputs;
            end case;

         else
            --  There is an output
            case Num_Inputs (Conv_Desc) is
            when 0 =>
               declare
                  type Func_Ptr is access function
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr).all);
               end;
            when 1 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1 : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1)));
               end;
            when 2 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2 : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2)));
               end;
            when 3 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3 : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3)));
               end;
            when 4 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4 : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4)));
               end;
            when 5 =>
               declare
                  Param_Array : Word_Array_Max renames
                    To_Word_Array (Params).all;
                  type Func_Ptr is access function
                    (Inp_1, Inp_2, Inp_3, Inp_4, Inp_5 : Word_Type)
                    return Word_Type;
                  function Conv is new Ada.Unchecked_Conversion
                                          (Routine_Code_Address, Func_Ptr);
               begin
                  Store_Word (Params, 0, Conv (Code_Addr)
                    (Param_Array (1),
                     Param_Array (2),
                     Param_Array (3),
                     Param_Array (4),
                     Param_Array (5)));
               end;
            when others =>
               --  NYI
               raise Too_Many_Inputs;
            end case;
         end if;
      end case;
   end Call_Compiled_Routine;

   --------------------
   -- Check_Is_Large --
   --------------------

   procedure Check_Is_Large (Large_Obj_Value : Word_Type) is
   begin
      if Virt_Is_Phys then
         if (Large_Obj_Value mod Word_SU_Size = 0
             and then
               (Large_Obj_Value in Lowest_Virt_Addr .. Highest_Virt_Addr
                  or else
                Large_Obj_Value in Lowest_Stack_Addr .. Highest_Stack_Addr))
           or else Is_Special_Large_Value (Large_Obj_Value)
         then
            return;  --  OK
         end if;
      elsif Large_Obj_Value > Chunk_Divisor
        or else Is_Special_Large_Value (Large_Obj_Value)
      then
         return;  --  OK
      end if;

      --  We have a bad address for a large object
      Messages.Put_RT_Error
        ("Internal: Large object has null region chunk:" &
           Hex_Image (Large_Obj_Value),
         Src_Pos => Execution_Source_Pos);
      raise Program_Error;
   end Check_Is_Large;

   --------------------
   -- Check_Not_Null --
   --------------------

   procedure Check_Not_Null
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Dest_Name   : Strings.U_String;
      Type_Info   : Object_Locator;
      Src_Pos     : Source_Positions.Source_Position) is
   --  We want to check that destination is not null.
   begin
      if Doing_Run_Time_Checks then
         declare
            Val : constant Word_Type := Fetch_Word (Context, Destination);
            Type_Desc : constant Non_Op_Map_Type_Ptr :=
              Get_Type_Desc (Context, Type_Info);
         begin
            if Is_Null_Value (Val, Type_Desc)
              and then
                (Val /= Null_Unsigned_64 or else not Type_Desc.Is_Small)
            then
               --  Failed a not-null check
               --  and not the Unsigned-64 type.
               Messages.Put_RT_Error
                 (Strings.To_String (Dest_Name),
                  Src_Pos => Src_Pos,
                  Message_Kind => "Null value not permitted here");

               --  TBD: Should we continue or raise an exception?
               --      For now we will dump the stack and keep going
               Dump_Stack
                 (Server_Info_Array (Context.Server_Index).Current_State,
                  Use_Cur_Err => True);

               --  Invoke the debugging console if available.
               Invoke_Debug_Console (Context, Reason => Null_Check_Failure);

            end if;
         end;
      end if;
   end Check_Not_Null;

   ------------------------
   -- Check_Static_Chain --
   ------------------------

   procedure Check_Static_Chain (Static_Link : Word_Ptr) is
      Encloser : Word_Ptr := Static_Link;
   begin
      if not Internal_Consistency_Checks then
         --  Not doing these checks
         return;
      end if;

      while Encloser /= null loop
         --  Keep looking up the static chain
         declare
            Next : constant Word_Type := Fetch_Word (Encloser, 0);
         begin
            if Next = 0 then
               --  OK, no type descriptor
               return;
            elsif Next = Type_Indicator then
               --  We have a type
               exit;
            else
               --  Keep looking
               Encloser := Word_To_Word_Ptr (Next);
            end if;
         end;
      end loop;

      --  Encloser now is a type descriptor.
      pragma Assert
        (Encloser = null
           or else Type_Elem_Index (To_Type_Desc (Encloser).Index)
                     in 0 .. Num_Elements (Type_Table));
   end Check_Static_Chain;

   ------------------------
   -- Context_Of_Address --
   ------------------------

   function Content_Of_Address (Address : Object_Address) return Word_Type is
   begin
      if Address.Enclosing_Chunk = null then
         Messages.Put_RT_Error
           ("Attempting to select a component of a null object, " &
            "offset =" &
            Offset_Within_Chunk'Image (Address.Offset),
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return Address.Enclosing_Chunk.Data (Address.Offset);
   end Content_Of_Address;

   ----------------
   -- Create_Obj --
   ----------------

   procedure Create_Obj
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info               : Object_Locator)
   is
      Type_Desc : constant Type_Descriptor_Ptr :=
                    Unwrapped_Type_Desc (Get_Type_Desc (Context, Type_Info));
   begin
      if Is_Small (Type_Desc) then
         --  Store appropriate kind of "null"
         Store_Word (Context, Destination, Null_For_Type (Type_Desc));
      else
         --  Create large object appropriately initialized
         --  and store into destination
         Store_Word
           (Context,
            Destination,
            Create_Large_Obj
               (Type_Desc,
                Stg_Rgn_Of_Existing_Large_Obj
                   (Context,
                    Existing_Obj_In_Stg_Rgn),
                Context.Server_Index));
      end if;
   end Create_Obj;

   procedure Create_Object_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      Destination  : Word_Ptr;
      Existing_Obj : Word_Ptr)
   is
      Stg_Region : Stg_Rgn_Ptr := null;
      Type_Desc  : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
   begin
      if Is_Small (Type_Desc) then
         --  Store appropriate kind of "null"
         Store_Word (Destination, 0, Null_For_Type (Type_Desc));
      else
         if Existing_Obj = null then
            Stg_Region := Local_Stg_Rgn (Context);
         else
            Stg_Region := Stg_Rgn_Of_Large_Obj (Fetch_Word (Existing_Obj, 0));
         end if;
         --  Create large object appropriately initialized
         --  and store into destination
         Store_Word
           (Destination, 0,
            Create_Large_Obj
               (Type_Desc,
                Stg_Region,
                Context.Server_Index));
      end if;
   end Create_Object_Exported;

   ---------------------------
   -- Create_Operation_Desc --
   ---------------------------

   function Create_Operation_Desc
     (Context               : in out Exec_Context;
      Operation_Locator     : Object_Locator;
      Operation_Static_Link : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Conv_Desc             : Convention_Descriptor := Null_Conv_Desc;
      SL_Addr               : Word_Type := 0;
      Target_Addr           : Word_Type := 0;
      Existing_Obj_Addr     : Word_Type := 0)
         return Object_Virtual_Address
   is
      function Get_Op_Desc_Type_Desc return Type_Descriptor_Ptr;
      function Get_Op_Desc_Type_Desc return Type_Descriptor_Ptr is
      begin
         --  Only needs to be retrieved the first time
         if Operation_Descriptor_Type_Desc = null then
            Operation_Descriptor_Type_Desc :=
               Get_Type_Desc_By_Name
                  (Strings.String_Lookup
                     (Languages.Operation_Descriptor_Type_Name));
         end if;
         pragma Assert (Operation_Descriptor_Type_Desc /= null);
         return Operation_Descriptor_Type_Desc;
      end Get_Op_Desc_Type_Desc;

      Null_Routine_Cpy : aliased Routine := Null_Routine.all;
      --  This Unchecked_Access is OK because it's only passed to
      --  Find_Routine_Context which doesn't store it.
      Operation_Routine_Ctx : Routine_Context :=
         (Null_Routine_Cpy'Unchecked_Access, null, null);
      Index : Routine_Index := 0;
      Static_Link : Word_Type := SL_Addr;
      Target_Addr_To_Use : Word_Type := Target_Addr;
      Conv_Desc_To_Use : Convention_Descriptor := Conv_Desc;
      Op_Desc_Type_Desc : constant Type_Descriptor_Ptr :=
         Get_Op_Desc_Type_Desc;

      function Stg_Rgn_To_Use return Stg_Rgn_Ptr;
         --  Determine what storage region to use
      function Stg_Rgn_To_Use return Stg_Rgn_Ptr is
      begin
         if Existing_Obj_Addr /= 0 then
            return Stg_Rgn_Of_Large_Obj (Existing_Obj_Addr);
         elsif not Is_Null_Obj_Locator (Existing_Obj_In_Stg_Rgn) then
            return Stg_Rgn_Of_Large_Obj
              (Fetch_Word (Context, Existing_Obj_In_Stg_Rgn));
         else
            return Local_Stg_Rgn (Context);
         end if;
      end Stg_Rgn_To_Use;

      --  Create the Large_Object
      Op_Desc : constant Object_Virtual_Address :=
         Create_Large_Obj (Op_Desc_Type_Desc, Stg_Rgn_To_Use,
            Context.Server_Index);

      --  Convert to physical for writing to fields
      Op_Desc_Phys : constant Word_Ptr :=
         Virtual_To_Physical_Address (Op_Desc);
   begin
      if Target_Addr = 0 then
         Find_Routine_Context
          (Context, Operation_Locator,
           Static_Link => Operation_Static_Link,
           Params      => null,
           SL_Addr     =>
             Word_To_Word_Ptr (SL_Addr),
           Result => Operation_Routine_Ctx);
         if not Operation_Routine_Ctx.Code.Is_PSVM_Routine then
            --  Get address if is a non-interpreted routine
            Target_Addr_To_Use := Routine_Code_Address_To_Word
                             (Operation_Routine_Ctx.Code.Routine_Addr);
         end if;
         Index := Operation_Routine_Ctx.Code.Index;
         Static_Link := Word_Ptr_To_Word (Operation_Routine_Ctx.Static_Link);
         Conv_Desc_To_Use := Operation_Routine_Ctx.Code.Conv_Desc;
      end if;

      --  Set fields of object
      Store_Word (Op_Desc_Phys, Large_Obj_Header_Size, Target_Addr_To_Use);
      Store_Word (Op_Desc_Phys, Large_Obj_Header_Size + 1, Word_Type (Index));
      Store_Word (Op_Desc_Phys, Large_Obj_Header_Size + 2, Static_Link);
      Store_Word (Op_Desc_Phys, Large_Obj_Header_Size + 3,
        Word_Type (Conv_Desc_To_Use));

      --  Return the virtual address
      return Op_Desc;
   end Create_Operation_Desc;

   function Create_Operation_Desc_Exported
     (Context                      : in out Exec_Context;
      Operation_Locator_Base       : Area_Base_Indicator;
      Operation_Locator_Offset     : Offset_Within_Area;
      Operation_Static_Link_Base   : Area_Base_Indicator;
      Operation_Static_Link_Offset : Offset_Within_Area;
      Conv_Desc                    : Convention_Descriptor;
      SL_Addr                      : Word_Ptr;
      Target_Addr                  : Word_Ptr;
      Existing_Obj                 : Word_Type)
         return Object_Virtual_Address is
   begin
      return Create_Operation_Desc (Context,
         (Operation_Locator_Base, Operation_Locator_Offset, No_VM_Obj_Id),
         (Operation_Static_Link_Base, Operation_Static_Link_Offset,
          No_VM_Obj_Id),
         Null_Object_Locator, Conv_Desc,
         Word_Ptr_To_Word (SL_Addr), Word_Ptr_To_Word (Target_Addr),
         Existing_Obj_Addr => Existing_Obj);
   end Create_Operation_Desc_Exported;

   ----------------------------------
   -- Cur_Inst_Params package body --
   ----------------------------------

   package body Cur_Inst_Params is
      --  Info on "cur-inst" params of a routine

      function To_Routine_Param_Info_Array (Info : Routine_Cur_Inst_Param_Info)
         return Routine_Param_Info_Array is
         Result : Routine_Param_Info_Array (1 .. Info'Length);
      begin
         for I in Info'First .. Info'Last loop
            Result (I) :=
               (Compiled => True,
                Is_Passed_By_Ref    => Info (I).Is_By_Ref,
                Is_Var              => Info (I).Is_Var,
                Is_Operation_Output => Info (I).Is_Output);
         end loop;
         return Result;
      end To_Routine_Param_Info_Array;

      function Has_Cur_Inst_Param_Info (Info : Routine_Cur_Inst_Param_Info)
        return Boolean is
      --  Return True if Info has info on at least one cur-inst param
      begin
         return Info (Info'First).Param_Offset /= Not_A_Param;
      end Has_Cur_Inst_Param_Info;

      function Has_Cur_Inst_Output (Info : Routine_Cur_Inst_Param_Info)
        return Boolean is
      --  Return True if Info has info on at least one cur-inst output
      begin
         return Info (Info'First).Is_Output;
      end Has_Cur_Inst_Output;

      function Has_Cur_Inst_Input (Info : Routine_Cur_Inst_Param_Info)
        return Boolean is
      --  Return True if Info has info on at least one cur-inst input
      begin
         for I in Info'Range loop
            exit when Info (I).Param_Offset = Not_A_Param;
            if not Info (I).Is_Output then
               return True;  --  Found an input
            end if;
         end loop;

         --  No cur-inst inputs
         return False;
      end Has_Cur_Inst_Input;

      procedure Get_First_Cur_Inst_Input
        (Info : Routine_Cur_Inst_Param_Info;
         Param_Offset : out Offset_Within_Area;
         Is_By_Ref    : out Boolean) is
      --  For first cur-inst input, get offset and whether is passed by-ref.
      --  Param_Offset = Not_A_Param if there are no cur-inst inputs.
      begin
         for I in Info'Range loop
            exit when Info (I).Param_Offset = Not_A_Param;
            if not Info (I).Is_Output then
               Param_Offset := Info (I).Param_Offset;
               Is_By_Ref    := Info (I).Is_By_Ref;
               return;  --  All done
            end if;
         end loop;

         --  No cur-inst inputs
         Param_Offset := Not_A_Param;
         Is_By_Ref    := False;
      end Get_First_Cur_Inst_Input;

      procedure Iterate_Cur_Inst_Params (Info : Routine_Cur_Inst_Param_Info) is
      --  Iterate through the Routine_Cur_Inst_Param_Info calling Action
      --  for each "cur-inst" param.
      begin
         for I in Info'Range loop
            declare
               Next_Param : constant One_Param_Info := Info (I);
            begin
               exit when Next_Param.Param_Offset = Not_A_Param;

               --  Call Action with next cur-inst parameter
               Action (Next_Param.Param_Offset,
                       Is_By_Ref => Next_Param.Is_By_Ref,
                       Is_Var    => Next_Param.Is_Var,
                       Is_Output => Next_Param.Is_Output);
            end;
         end loop;
      end Iterate_Cur_Inst_Params;

      procedure Add_Cur_Inst_Param
        (Info         : in out Routine_Cur_Inst_Param_Info;
         Param_Offset : Offset_Within_Area;
         Is_By_Ref    : Boolean;
         Is_Var       : Boolean;
         Is_Output    : Boolean) is
      --  This is called repeatedly to build up a Routine_Cur_Inst_Param_Info
      --  for cur-inst params.
      --  The exception Too_Many_Cur_Inst_Params is raised if the limit on
      --  Cur-inst params is reached.
         pragma Assert (Param_Offset <= Max_Param_Offset);
      begin
         for I in Info'Range loop
            pragma Assert (Param_Offset /= Info (I).Param_Offset);
            --  No duplicates allowed

            if Param_Offset < Info (I).Param_Offset then
               --  This is where it goes
               if Info (I).Param_Offset /= Not_A_Param then
                  --  Shift the rest down

                  if Info (Info'Last).Param_Offset /= Not_A_Param then
                     --  Ran out of room
                     raise Too_Many_Cur_Inst_Params;
                  end if;

                  for J in reverse I + 1 .. Info'Last loop
                     Info (J) := Info (J - 1);
                  end loop;
               end if;

               --  Fill in the I'th slot
               Info (I) := (Param_Offset,
                            Is_By_Ref => Is_By_Ref,
                            Is_Var    => Is_Var,
                            Is_Output => Is_Output);

               return;  --  All done
            end if;
         end loop;

         raise Too_Many_Cur_Inst_Params;
      end Add_Cur_Inst_Param;

      ----------------------------------------------
      -- Cur_Inst_Param_Info streaming operations --
      ----------------------------------------------

      type Byte_Count is mod 2**8;

      type One_Param_Info_Byte is mod 2 ** 8;
      type Byte_Base_Array is array (Byte_Count range <>)
        of One_Param_Info_Byte;
      for Byte_Base_Array'Component_Size use 8;

      subtype Byte_Array is Byte_Base_Array (1 .. Max_Cur_Inst_Params);

      function To_Byte_Array is new Ada.Unchecked_Conversion
        (Routine_Cur_Inst_Param_Info, Byte_Array);

      function From_Byte_Array is new Ada.Unchecked_Conversion
        (Byte_Array, Routine_Cur_Inst_Param_Info);

      procedure Cur_Inst_Param_Info_Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item : Routine_Cur_Inst_Param_Info) is
      --  Write out the contents of the routine cur-inst-param info array.
         use Ada.Streams;
         Num_Params : Byte_Count := 0;
         Rep : constant Byte_Array := To_Byte_Array (Item);
      begin
         --  Determine how many cur-inst params are present
         for I in reverse 1 .. Max_Cur_Inst_Params loop
            if Item (I).Param_Offset /= Not_A_Param then
               Num_Params := Byte_Count (I);
               exit;
            end if;
         end loop;
         --  Write out count and then equivalent byte array
         Byte_Count'Write (Stream, Num_Params);
         Byte_Base_Array'Write (Stream, Rep (1 .. Num_Params));
      end Cur_Inst_Param_Info_Write;

      procedure Cur_Inst_Param_Info_Read
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Item : out Routine_Cur_Inst_Param_Info) is
      --  Read in the contents of the routine cur-inst-param info array.
         use Ada.Streams;
         Rep : Byte_Array := To_Byte_Array (No_Cur_Inst_Param_Info);
         Num_Params : constant Byte_Count := Byte_Count'Input (Stream);
      begin
         --  Read in equiv byte array
         Byte_Base_Array'Read (Stream, Rep (1 .. Num_Params));
         --  And convert to cur-inst info array
         Item := From_Byte_Array (Rep);
      end Cur_Inst_Param_Info_Read;

   end Cur_Inst_Params;

   --------------------------
   -- Find_Routine_Context --
   --------------------------

   procedure Find_Routine_Context
     (Context           : in out Exec_Context;
      Routine_Locator   : Object_Locator;
      Static_Link       : Object_Locator;
      Params            : Word_Ptr;
      Result            : in out Routine_Context;
      Orig_Param_Infos  : Routine_Cur_Inst_Param_Info :=
         No_Cur_Inst_Param_Info;
      SL_Addr           : Word_Ptr := null;
      Op_Desc_Virt_Addr : Object_Virtual_Address :=
         Null_Virtual_Address)
   is
      Index                   : Routine_Index := 0;
      Static_Link_Addr        : Word_Ptr := null;
      Polymorphic_Input_Type  : Type_Descriptor_Ptr := null;
      Polymorphic_Output_Type : Type_Descriptor_Ptr := null;
      Is_Operation_Desc       : Boolean := False;

      type Poly_Match_Result is
        (Params_Match, Compare_Op_Mismatch, Params_Mismatch);
      --  "Params_Match" means all polymorphic params match
      --  "Compare_Op_Mismatch" means there is a mismatch, but op is "=?"
      --  "Params_Mismatch" means there is a mismatch, and that is an error.

      function To_Routine_RW_Ptr is
         new Ada.Unchecked_Conversion (Source => Routine_Ptr,
                                       Target => Routine_RW_Ptr);

      function Unwrap_Poly_Params
        (Polymorphic_Input_Type : Type_Descriptor_Ptr;
         Code                   : Routine_Ptr;
         Param_Infos            : Routine_Cur_Inst_Param_Info)
         return Poly_Match_Result;
      --  This is a dispatching call.
      --  We need to see whether any other inputs are of the current-inst type.
      --  If so, we need to "unwrap" them and verify that their underlying type
      --  is the same. If not, we fail unless the operation is "=?", in which
      --  case we return #unordered.

      function Unwrap_Poly_Params
        (Polymorphic_Input_Type : Type_Descriptor_Ptr;
         Code                   : Routine_Ptr;
         Param_Infos            : Routine_Cur_Inst_Param_Info)
         return Poly_Match_Result
      is
         First_Input           : Boolean := True;
         First_Underlying_Type : constant Type_Descriptor_Ptr :=
                                   Polymorphic_Input_Type
                                     .Components (1).Type_Desc;

         Result                : Poly_Match_Result := Params_Match;

         procedure Unwrap_Param (Param_Offset : Offset_Within_Area;
           Is_By_Ref : Boolean; Is_Var : Boolean; Is_Output : Boolean);
         --  Unwrap one param

         procedure Unwrap_Param (Param_Offset : Offset_Within_Area;
           Is_By_Ref : Boolean; Is_Var : Boolean; Is_Output : Boolean) is
         --  Unwrap one param
         begin
            if not Is_Output and then Result = Params_Match then
               if First_Input then
                  --  Ignore first input as that one was already unwrapped
                  First_Input := False;
               else
                  --  Unwrap parameter, make sure is of same underlying type

                  declare
                     Param_Addr : constant Word_Ptr :=
                                    Add (Params, Param_Offset);
                     Poly_Param : Word_Type :=
                                    Content_Of_Physical_Address (Param_Addr);
                  begin
                     if Debug_Calls then
                        Put_Line
                          (" Unwrapping param at offset" &
                           Offset_Within_Area'Image (Param_Offset) &
                           " in call on " &
                           Strings.To_String (Code.Name));
                     end if;

                     if Is_By_Ref then
                        --  Was passed by reference
                        Poly_Param :=
                          Content_Of_Physical_Address
                            (Word_To_Word_Ptr (Poly_Param));
                     end if;

                     if Is_Large_Null (Poly_Param) then
                        --  Create null of same type as controlling param
                        Store_Word (Param_Addr, 0,
                          Null_For_Type_Or_Stg_Rgn
                            (Polymorphic_Input_Type.Components (1).Type_Desc,
                             Stg_Rgn_Of_Large_Obj (Poly_Param),
                             Is_By_Ref));

                     else
                        declare
                           Poly_Param_Type : constant Type_Descriptor_Ptr :=
                                              Large_Obj_Type_Desc (Poly_Param);
                           Underlying_Type : constant Type_Descriptor_Ptr :=
                                              Poly_Param_Type.Components (1)
                                                .Type_Desc;
                           Addr_Of_Value : constant Object_Virtual_Address :=
                                            Poly_Param + Large_Obj_Header_Size;
                           Underlying_Value : constant Word_Type :=
                                                Content_Of_Virtual_Address
                                                  (Addr_Of_Value);
                           use type Strings.U_String;
                        begin
                           Share_Lock (Poly_Param);  --  Share lock if any
                           if Debug_Calls then
                              Put_Line
                                (" Poly param value:");
                              Dump_Obj_With_Indent
                                (Poly_Param,
                                 Poly_Param_Type,
                                 Indent => 2);
                              Put_Line
                                (" Underlying value:");
                              Dump_Obj_With_Indent
                                (Underlying_Value,
                                 Underlying_Type,
                                 Indent => 2);
                           end if;

                           --  Store underlying value (or its addr) back in
                           --  its place

                           if not Is_By_Ref then
                              Store_Word (Param_Addr, 0, Underlying_Value);
                           else
                              --  Passed by ref
                              Store_Word_Ptr (Param_Addr, 0,
                                Virtual_To_Physical_Address (Addr_Of_Value));
                           end if;

                           if Underlying_Type /= First_Underlying_Type then

                              --  Type mismatch. Either an error or return
                              --  #unordered if a call on "=?"
                              declare
                                 Compare_Op : Strings.U_String :=
                                   Compare_Op_Str;
                                 --  NOTE: Compare_Op_Str is atomic to
                                 --        avoid a race condition here.
                              begin
                                 if Compare_Op = Strings.Null_U_String then
                                    --  Initialize Compare_Op_Str on first use
                                    Compare_Op := Strings.String_Lookup
                                      ('"' & Languages.Compare_Op_Name & '"');
                                    Compare_Op_Str := Compare_Op;
                                 end if;

                                 if Code.Name = Compare_Op then
                                    --  Initialize result to unordered
                                    --  and return a routine that does nothing.
                                    Store_Word
                                      (Params, 0, Ordering'Pos (Unordered));
                                    Result := Compare_Op_Mismatch;
                                 else
                                    Messages.Put_RT_Error
                                      ("Type mismatch " &
                                         Type_Sem_Image
                                           (First_Underlying_Type.Type_Sem) &
                                         " vs. " &
                                         Type_Sem_Image
                                           (Underlying_Type.Type_Sem),
                                       Src_Pos => Execution_Source_Pos);
                                    Result := Params_Mismatch;
                                 end if;

                                 --  Might as well return now.
                                 return;
                              end;
                           end if;
                        end;
                     end if;  --  Whether is null
                  end;
               end if;  --  Whether is first input
            end if;  --  Whether is cur-inst input
         end Unwrap_Param;

         procedure Unwrap_Params is
           new Iterate_Cur_Inst_Params (Unwrap_Param);

      begin  --  Unwrap_Poly_Params

         Unwrap_Params (Param_Infos);

         --  Return indicator of match vs. mismatch
         return Result;
      end Unwrap_Poly_Params;

   begin  --  Find_Routine_Context

      if Static_Link.Base = Param_Area
        or else Static_Link.Base = Enclosing_Param_Areas'First + 1
      then

         --  This is a special case where the type-id of a polymorphic
         --  parameter is determining the static link.
         --  We need to fetch the type-id of the polymorphic param, and put the
         --  underlying value back in place of the param.
         --  "Enclosing_Param_Areas'First+1" is a special indicator that the
         --  parameter is passed by reference.

         declare
            Addr_In_Param_Area : constant Word_Ptr :=
                                   Add (Params, Static_Link.Offset);
            Poly_Param         : Word_Type :=
                                   Content_Of_Physical_Address
                                     (Addr_In_Param_Area);
         begin
            if Static_Link.Base /= Param_Area then
               --  Was passed by reference
               Poly_Param := Content_Of_Physical_Address
                                (Word_To_Word_Ptr (Poly_Param));
            end if;

            if Is_Large_Null (Poly_Param) then
               Messages.Put_RT_Error
                 ("Null polymorphic object not allowed here",
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
            end if;

            declare
               pragma Assert (not Is_Large_Null (Poly_Param));
               Poly_Param_Type : constant Type_Descriptor_Ptr :=
                                   Large_Obj_Type_Desc (Poly_Param);
               Underlying_Type : constant Type_Descriptor_Ptr :=
                                   Poly_Param_Type.Components (1).Type_Desc;

               Addr_Of_Underlying_Value : constant Word_Ptr :=
                                Virtual_To_Physical_Address
                                  (Poly_Param + Large_Obj_Header_Size);
               Underlying_Value         : constant Word_Type :=
                                            Content_Of_Physical_Address
                                              (Addr_Of_Underlying_Value);
            begin
               if not Is_Small (Underlying_Type) then
                  Share_Lock (Poly_Param);  --  Share lock if any
               end if;

               if Debug_Calls then
                  Put_Line
                    (" Extracting type and value from polymorphic param:");
                  Put_Line ("  Original value:");
                  Dump_Obj_With_Indent
                    (Poly_Param,
                     Poly_Param_Type,
                     Indent => 2);
                  Put_Line ("  Extracted value:");
                  Dump_Obj_With_Indent
                    (Underlying_Value,
                     Underlying_Type,
                     Indent => 2);
               end if;

               --  Store underlying value (or its addr) back in its place
               if Static_Link.Base = Param_Area then
                  Store_Word (Addr_In_Param_Area, 0, Underlying_Value);
               else
                  --  Passed by ref
                  Store_Word_Ptr
                    (Addr_In_Param_Area, 0, Addr_Of_Underlying_Value);
               end if;

               --  Get static link based on polymorphic obj type-id
               Static_Link_Addr :=
                  Get_Static_Link (Context, Underlying_Type.Location);

               --  Remember polymorphic input type
               Polymorphic_Input_Type := Poly_Param_Type;
            end;
         end;
      elsif SL_Addr = null then
      --  Get static link address given its locator (Interpreted code)
         Static_Link_Addr :=
           Get_Static_Link (Context, Static_Link);
      else
      --  Compiled code
         Static_Link_Addr := SL_Addr;
      end if;

      case Routine_Locator.Base is
         when Zero_Base =>
            --  "Absolute" address of routine
            Index := Routine_Index (Routine_Locator.Offset);

         when Type_Area | Enclosing_Type_Areas =>

            --  Relative to type area.  If is a formal operation,
            --  then this is relative to the caller's type area.
            --  If this is an operation of the type, then this
            --  is relative to the called routine's type area,
            --  which is given by the static link.

            if Routine_Locator.Offset in Type_Operation_Offsets then
               declare
                  Callee_Type_Area_Or_Map : constant Type_Descriptor_Ptr :=
                    Get_Enclosing_Type_Or_Op_Map
                       (Static_Link_Addr,
                        Type_Base => Routine_Locator.Base);

                  Op_Index : constant Operation_Index :=
                    Operation_Index
                      (Routine_Locator.Offset - Type_Operation_Offsets'First);

                  Info : constant Routine_Info :=
                           Nth_Operation_Of_Type
                              (Callee_Type_Area_Or_Map, Op_Index);
                  Code : constant Routine_Ptr := Nth_Routine (Info.Index);

                  Param_Infos : Routine_Cur_Inst_Param_Info :=
                                  Info.Cur_Inst_Param_Info;
               begin
                  if Debug_Calls then
                     Put (" Target routine info:");
                     Dump_Routine_Info (Info);
                  end if;

                  if Has_Cur_Inst_Param_Info (Orig_Param_Infos) then
                     --  Use Orig_Param_Infos since they are provided.
                     Param_Infos := Orig_Param_Infos;

                  elsif Polymorphic_Input_Type /= null then
                     --  Get param infos from poly type's operations
                     --  if available.
                     declare
                        Root_Type : constant Type_Descriptor_Ptr :=
                          Root_Of_Polymorphic_Type (Polymorphic_Input_Type);
                     begin
                        if Root_Type /= null then
                           declare
                              Poly_Info : constant Routine_Info :=
                                Nth_Operation_Of_Type
                                  (Root_Of_Polymorphic_Type
                                    (Polymorphic_Input_Type),
                                   Op_Index);
                           begin
                              Param_Infos := Poly_Info.Cur_Inst_Param_Info;
                           end;
                        end if;
                     end;
                  end if;

                  --  Get routine-index for nth operation of callee type.

                  Index := Info.Index;

                  if Info.Type_Desc /= null
                    and then not Info.Use_Static_Link_For_Type
                  then
                     Static_Link_Addr :=
                       Get_Static_Link (Context, Info.Type_Desc.Location);
                  end if;

                  case Info.Action is
                     when No_Action =>
                        --  No special action, unless this is a call with a
                        --  polymorphic controlling parameter.

                        if Polymorphic_Input_Type /= null then
                           --  We need to see whether any other inputs are
                           --  of the current-inst type. If so, we need to
                           --  "unwrap" them and verify that their underlying
                           --  type is the same. If not, we fail unless the
                           --  operation is "=?", in which case we return
                           --  #unordered.

                           case Unwrap_Poly_Params
                                 (Polymorphic_Input_Type, Code, Param_Infos)
                           is
                              when Params_Match =>
                                 --  Everything is fine
                                 null;
                              when Compare_Op_Mismatch =>
                                 --  Return a no-op
                                 Result.Code.Conv_Desc :=
                                   Mismatched_Compare_Conv_Desc;
                                 return;
                              when Params_Mismatch =>
                                 --  Fatal error
                                 raise Program_Error;
                           end case;
                        end if;
                     when Component_Extension_Action =>

                        --  This operation was inherited from an ancestor
                        --  potentially at a different "component-extension"
                        --  level. Call Select_Ancestor_Part on each
                        --  "current-inst" input to the operation.

                        declare
                           Source_Type_Desc : constant Non_Op_Map_Type_Ptr :=
                             Skip_Over_Op_Map (Callee_Type_Area_Or_Map);
                           --  This is the type of the actual "cur-inst" inputs
                           --  We need to get the "Info.Type_Desc" ancestor
                           --  part.
                        begin
                           if Polymorphic_Input_Type /= null then

                              --  First unwrap the polymorphic params. We need
                              --  to see whether any other inputs are of the
                              --  current-inst type. If so, we need to "unwrap"
                              --  them and verify that their underlying type
                              --  is the same. If not, we fail unless the
                              --  operation is "=?", in which case we return
                              --  #unordered.

                              case Unwrap_Poly_Params
                                (Polymorphic_Input_Type, Code, Param_Infos) is
                                 when Params_Match =>
                                    --  Everything is fine
                                    null;
                                 when Compare_Op_Mismatch =>
                                    --  Return a no-op
                                    Result.Code.Conv_Desc :=
                                      Mismatched_Compare_Conv_Desc;
                                    return;
                                 when Params_Mismatch =>
                                    --  Fatal error
                                    raise Program_Error;
                              end case;
                           end if;

                           if Source_Type_Desc.Component_Extension_Level /=
                                Info.Type_Desc.Component_Extension_Level
                             or else Info.Type_Desc.Is_Wrapper
                           then
                              --  We have a component-extension level mismatch
                              --  or a target that is a wrapper.
                              declare
                                 procedure Select_Ancestor
                                   (Param_Offset : Offset_Within_Area;
                                    Is_By_Ref    : Boolean;
                                    Is_Var       : Boolean;
                                    Is_Output    : Boolean);

                                 procedure Select_Ancestor
                                   (Param_Offset : Offset_Within_Area;
                                    Is_By_Ref    : Boolean;
                                    Is_Var       : Boolean;
                                    Is_Output    : Boolean) is
                                 --  Select ancestor part of parameter

                                    Param_Address : constant Word_Ptr :=
                                      Add (Params, Param_Offset);
                                 begin
                                    if not Is_Output then
                                    --  Select ancestor part
                                       if Debug_Calls then
                                          Put_Line
                                            (" For param at" &
                                             Offset_Within_Area'Image
                                              (Param_Offset) &
                                             " in call on " &
                                             Strings.To_String (Code.Name) &
                                             ", selecting " &
                                             Type_Sem_Image
                                                (Info.Type_Desc.Type_Sem) &
                                             " ancestor of " &
                                             Type_Sem_Image
                                                (Source_Type_Desc.Type_Sem));
                                       end if;

                                       Store_Word (Params, Param_Offset,
                                          Select_Ancestor_Part
                                            (Context,
                                             Source_Obj =>
                                                Content_Of_Physical_Address
                                                  (Param_Address),
                                             Ancestor_Type_Desc =>
                                               Info.Type_Desc,
                                             Source_Type_Desc =>
                                               Source_Type_Desc,
                                             Is_Passed_By_Ref => Is_By_Ref));
                                    end if;
                                 end Select_Ancestor;

                                 procedure Select_Ancestors is new
                                   Iterate_Cur_Inst_Params
                                     (Select_Ancestor);
                              begin
                                 --  Select all of the ancstors
                                 Select_Ancestors (Param_Infos);
                              end;
                           end if;

                           if Info.Use_Static_Link_For_Type then
                              --  Use Parent_Type as static link if non-null
                              declare
                                 Generic_Op_Type : constant Non_Op_Map_Type_Ptr
                                   := Get_Enclosing_Type (Static_Link_Addr);
                              begin
                                 if Generic_Op_Type.Parent_Type /= null then
                                    --  We assume that Parent_Type for an
                                    --  implicit module is an instance of the
                                    --  implicit module of the original
                                    --  generic operation we are inheriting.
                                    --  TBD: Parent_Type is therefore a bit of
                                    --      a misnomer in this case.
                                    Static_Link_Addr :=
                                       Get_Static_Link
                                         (Context,
                                          Generic_Op_Type.Parent_Type.
                                            Location);
                                 end if;
                              end;
                           end if;
                        end;

                     when Polymorphic_Type_Action =>
                        --  Handle polymorphic type action, which is the case
                        --  where we instantiate a module with a polymorphic
                        --  type, and the module calls one of the operations of
                        --  the formal type. This needs to unwrap the cur-inst
                        --  inputs, make sure their type-ids match, call the
                        --  operation, and then re-wrap any cur-inst outputs.
                        --  We need enough info to be able to find the cur-inst
                        --  inputs/outputs, which is provided by "stub" routine
                        --  infos. No actual code is needed in the stub, since
                        --  once we determine the "true" type-id by unwrapping
                        --  one of the inputs, we get the real routine info
                        --  from there.

                        --  Need to find polymorphic input(s) and recurse with
                        --  them.

                        if Debug_Calls then
                           Put_Line (" wrapper action on " &
                             Strings.To_String (Code.Name) & " = " &
                             Wrapper_Action_Enum'Image (Info.Action) &
                             ", op index = " &
                             Operation_Index'Image (Info.Op_Index));
                        end if;

                        declare
                           Param_Offset : Offset_Within_Area;
                           Is_By_Ref    : Boolean;
                           Param_Base   : Area_Base_Indicator := Param_Area;
                        begin
                           Get_First_Cur_Inst_Input (Param_Infos,
                             Param_Offset, Is_By_Ref);

                           if Param_Offset /= Not_A_Param then
                              --  Found first cur-inst input.
                              if Debug_Calls then
                                 Put_Line (" recursing with " &
                                   Obj_Locator_Image
                                     ((Type_Area,
                                       Type_Operation_Offsets'First +
                                         Offset_Within_Area
                                           (Info.Op_Index),
                                       No_VM_Obj_Id)) & ", "
                                   & Obj_Locator_Image
                                       ((Param_Area, Param_Offset,
                                         No_VM_Obj_Id)));
                              end if;

                              --  Recurse with input's type as the static link,
                              --  and with original op index

                              if Is_By_Ref then
                                 --  Use by-ref indicator
                                 Param_Base := Enclosing_Param_Areas'First + 1;
                              end if;

                              Find_Routine_Context
                                (Context          => Context,
                                 Routine_Locator  =>
                                   (Type_Area,
                                    Type_Operation_Offsets'First +
                                      Offset_Within_Area (Info.Op_Index),
                                    No_VM_Obj_Id),
                                 Static_Link      =>
                                   (Param_Base, Param_Offset, No_VM_Obj_Id),
                                 Params           => Params,
                                 Orig_Param_Infos => Param_Infos,
                                 Result           => Result);
                              return;
                           end if;
                        end;

                        --  TBD: Currently need at least one cur-inst input.
                        --       Eventually can presume we will have an
                        --       extra parameter for operations with no
                        --       cur-inst inputs.
                        Messages.Put_RT_Error
                          ("NYI: call on operation with no cur-inst input" &
                             " when instantiated with polymorphic type",
                           Src_Pos => Execution_Source_Pos);

                  end case;

                  if Polymorphic_Input_Type /= null then

                     --  We may need to wrap an output. Scan to see if we have
                     --  any cur-inst outputs.

                     if Has_Cur_Inst_Output (Param_Infos) then
                        --  Found at least one current-inst output
                        Polymorphic_Output_Type := Polymorphic_Input_Type;
                     end if;
                  end if;
               end;
            else
               --  This is a formal operation parameter to the caller's type.
               Is_Operation_Desc := True;
            end if;

         when others =>
            --  Address of operation descriptor
            Is_Operation_Desc := True;
      end case;

      if Is_Operation_Desc then
         declare
            Op_Desc : Object_Virtual_Address;
            Op_Desc_Phys : Word_Ptr;

            Conv_Desc : Convention_Descriptor;

            Routine_Addr : Word_Ptr;
         begin
            if Op_Desc_Virt_Addr = Null_Virtual_Address then
               Op_Desc := Fetch_Word (Context, Routine_Locator);
            else
               Op_Desc := Op_Desc_Virt_Addr;
            end if;
            Op_Desc_Phys := Virtual_To_Physical_Address (Op_Desc);

            --  Fetch contents of Operation_Descriptor Object
            Routine_Addr := Word_To_Word_Ptr
               (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size));
            Static_Link_Addr := Word_To_Word_Ptr
               (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 2));
            Conv_Desc := Convention_Descriptor
               (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 3));

            if Routine_Addr /= null then
               --  Result.Code should be initialized as the Null_Routine
               --  Create a Routine on the fly with this address
               --  instead of using the index
               Result.Code.Routine_Addr :=
                  Word_Ptr_To_Routine_Code_Address (Routine_Addr);
               Result.Code.Is_Compiled_Routine := True;
               Result.Code.Conv_Desc := Conv_Desc;
               Result.Static_Link := Static_Link_Addr;
               Result.Polymorphic_Output_Type := Polymorphic_Output_Type;
               return;
               --  TBD: queueing for Store_Operation_Descriptor
            end if;
            Index := Routine_Index
               (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 1));

            if Debug_Calls then
               Put_Line (" Calling through op desc with Routine = " &
                 Strings.To_String (Nth_Routine (Index).Name) & " (#" &
                 Routine_Index'Image (Index) &
                 "), static_link = " &
                 Hex_Image (Static_Link_Addr));
            end if;
         end;
      end if;

      if Debug_Calls and then Polymorphic_Output_Type /= null then
         Put_Line ("  Returning routine ctxt with polymorphic output type:" &
           Type_Desc_Name_And_Num
             (Polymorphic_Output_Type, Use_Short_Form => True));
      end if;

      Result.Code := To_Routine_RW_Ptr (Nth_Routine (Index));
      Result.Static_Link := Static_Link_Addr;
      Result.Polymorphic_Output_Type := Polymorphic_Output_Type;
   end Find_Routine_Context;

   ----------------------
   -- Finalize_Stg_Rgn --
   ----------------------

   procedure Finalize_Stg_Rgn
     (Context : in out Exec_Context;
      Local_Area : Word_Ptr) is
   --  Finalize the storage region associated with the given local area.
   --  Context is updated to point to enclosing region.
   --  Restore the saved value of Open_Master.
   --  NOTE: This should be called at the end of a compiled
   --        ParaSail routine or nested block if Code.Uses_Stg_Rgn is true.
      Old_Rgn : Stg_Rgn_Ptr := Context.Local_Stg_Rgn;
      Encl_Rgn : constant Stg_Rgn_Ptr := Old_Rgn.Enclosing_Stg_Rgn;
   begin
      --  Point to enclosing region
      Context.Local_Stg_Rgn := Encl_Rgn;

      --  Restore Local_Null
      if Encl_Rgn /= null then
         Context.Local_Null := Encl_Rgn.Null_Value;
      else
         Context.Local_Null := Null_Virtual_Address;
      end if;

      --  Open_Master should be null at this point.
      --  TBD: Or should we await open master?
      pragma Assert (Context.Open_Master = null);

      --  Restore Open_Master to value on entry.
      Context.Open_Master := Old_Rgn.Saved_Open_Master;

      if Local_Area /= null then
         --  Null out the region information in the local area
         --  (after verifying that it matches the current value).
         pragma Assert
           (Fetch_Word (Local_Area, Local_Area_Stg_Rgn_Ptr_Offset) =
              Word_Type (Old_Rgn.Index));

         Store_Word (Local_Area, Local_Area_Stg_Rgn_Ptr_Offset, 0);
      end if;

      --  Release this region for future use
      Release_Stg_Rgn (Old_Rgn);
   end Finalize_Stg_Rgn;

   ------------------
   -- Find_Builtin --
   ------------------

   function Find_Builtin (Desig : Strings.U_String)
     return Routine_Code_Address is
      Index : constant Table_Index :=
                Strings.Hash (Desig) mod Builtin_Table_Size;
      Ptr   : Builtin_Entry_Ptr := Builtins_Table (Index);
   begin
      while Ptr /= null loop
         if Ptr.Desig = Desig then
            --  Return existing builtin
            return Ptr.Builtin;
         end if;
         Ptr := Ptr.Next;
      end loop;

      --  not found
      return null;
   end Find_Builtin;

   ----------------------------
   -- Finish_Type_Descriptor --
   ----------------------------

   procedure Finish_Type_Descriptor
     (Type_Desc : Type_Descriptor_Ptr;
      Return_On_Recursion : Boolean := False) is
      --  Finish up formal object parameters and nested objects
      --  If Return_On_Recursion is True, do not complain about recursion
      --  and simply return immediately.
   begin
      if Finish_Type_Descriptor_Ptr /= null then
         --  Just pass the buck to the version in the compiler
         Finish_Type_Descriptor_Ptr.all (Type_Desc, Return_On_Recursion);
      else
         --  Cannot do it without the compiler linked in.
         Messages.Put_RT_Error ("Need compiler to finish type descriptor " &
             Strings.To_String (Type_Desc.Name),
           Src_Pos => Execution_Source_Pos);
      end if;
   end Finish_Type_Descriptor;

   -------------------------------
   -- Generate_Value_Stream_Rep --
   -------------------------------

   function Generate_Value_Stream_Rep (Info : Const_Info;
     Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
     Target : Word_Type;
     Server_Index : Thread_Server_Index;
     PFST : Per_File_Strings.Per_File_String_Table_Ptr := null)
     return Word_Type is
   --  Create a basic array in region associated with Target and
   --  fill it in with a stream representation of the compile-time constant.
      use type Stream_Element_Array_Ptr;
      use Per_File_Strings;

      Result : Word_Type := Target;
   begin  --  Generate_Value_Stream_Rep

      --  Write out the stream and save it
      if PFST /= null then
         --  Add strings to per-file-string table instead of streaming them
         declare
            use String_Streams;
            Local_Stream : aliased Buffered_Stream_With_PFS (PFST);
         begin
            PSVM_Object_Output (Local_Stream'Access,
              Info.Data.Type_Desc, Info.Data.Value);

            --  Retrieve the stream
            Stream_To_Basic_Array
              (Local_Stream, Univ_Int_Array_Type, Target => Result,
                Server_Index => Server_Index);
         end;
      else
         --  Write strings into the stream
         declare
            use String_Streams;
            Local_Stream : aliased Buffered_Stream;
         begin
            PSVM_Object_Output (Local_Stream'Access,
              Info.Data.Type_Desc, Info.Data.Value);

            --  Retrieve the stream
            Stream_To_Basic_Array
              (Local_Stream, Univ_Int_Array_Type, Target => Result,
                Server_Index => Server_Index);
         end;
      end if;

      --  Return basic array
      return Result;
   end Generate_Value_Stream_Rep;

   ------------------------------
   -- Hash_Global_Operation_Id --
   ------------------------------

   function Hash_Global_Operation_Id (Id : Global_Operation_Id)
     return Strings.Hash_Type is
   --  Hash for global operation Id
      use type Strings.Hash_Type;
   begin
      return Strings.Hash_Type (Id.Module_Name) * 7 +
        Strings.Hash_Type (Id.Operation_Name);
   end Hash_Global_Operation_Id;

   ------------------------------------
   -- Initialize_Global_Data_Stg_Rgn --
   ------------------------------------

   procedure Initialize_Global_Data_Stg_Rgn is
   --  Initialize Global_Data_Stg_Rgn and Global_Const_Stg_Rgn if not yet done
   begin
      if Global_Data_Stg_Rgn = null then
         --  Create the global data region
         Global_Data_Stg_Rgn :=
            Get_New_Local_Stg_Rgn
              (New_Local_Area => Null_Object_Address,
               Server_Index => Main_Thread_Server_Index);
      end if;
      if Global_Const_Stg_Rgn = null then
         --  Create the global const region
         Global_Const_Stg_Rgn :=
            Get_New_Local_Stg_Rgn
              (New_Local_Area => Null_Object_Address,
               Server_Index => Main_Thread_Server_Index);

         --  Mark as a constant region
         Global_Const_Stg_Rgn.Is_Constant_Stg_Rgn := True;
         Global_Const_Stg_Rgn.Shared_Part.Is_Constant_Stg_Rgn := True;
      end if;
   end Initialize_Global_Data_Stg_Rgn;

   -----------------------------------
   -- Initialize_Global_Stack_Chunk --
   -----------------------------------

   procedure Initialize_Global_Stack_Chunk is
   --  Initialize the global stack chunk, if not yet initialized.
   begin
      --  Just pass the buck to protected object
      Stg_Rgn_Manager.Initialize_Global_Stack_Chunk;
   end Initialize_Global_Stack_Chunk;

   ----------------------------------
   -- Invoke_Per_File_Initializers --
   ----------------------------------

   procedure Invoke_Per_File_Initializers is
   --  Execute all of the per-file initializer functions
      Ptr : Per_File_Init_Ptr := Per_File_Initializer_List;
   begin
      while Ptr /= null loop
         --  Call the per-file initializer
         Ptr.To_Do.all;
         --  and go on to the next one
         Ptr := Ptr.Next;
      end loop;
   end Invoke_Per_File_Initializers;

   -------------------------
   -- Is_Null_Obj_Locator --
   -------------------------

   function Is_Null_Obj_Locator (Locator : Object_Locator) return Boolean is
   --  Return True if Locator is a null object locator
   begin
      return Locator.Base = Zero_Base and then Locator.Offset = 0;
   end Is_Null_Obj_Locator;

   -------------------
   -- Local_Stg_Rgn --
   -------------------

   function Local_Stg_Rgn (Context : Exec_Context) return Stg_Rgn_Ptr is
   begin
      return Context.Local_Stg_Rgn;
   end Local_Stg_Rgn;

   -------------------------------
   -- Locked_Param_Info_As_Byte --
   -------------------------------

   function Locked_Param_Info_As_Byte
     (Locked_Param_Info : Locked_Param_Info_Type)
       return Locked_Param_Info_As_Byte_Type is
   --  Convert locked-param-info record into a byte
   begin
      return Locked_Param_Info_As_Byte_Type
        (Locked_Param_Info.Param_Index) +
          Boolean'Pos (Locked_Param_Info.Is_Var)         * 2**5 +
          Boolean'Pos (Locked_Param_Info.Is_By_Ref)      * 2**6 +
          Boolean'Pos (Locked_Param_Info.Is_Queued_Call) * 2**7;
   end Locked_Param_Info_As_Byte;

   ---------------------------------
   -- Locked_Param_Info_From_Byte --
   ---------------------------------

   function Locked_Param_Info_From_Byte
     (Info_As_Byte : Locked_Param_Info_As_Byte_Type)
       return Locked_Param_Info_Type;
   --  Convert byte back into locked-param-info record

   function Locked_Param_Info_From_Byte
     (Info_As_Byte : Locked_Param_Info_As_Byte_Type)
       return Locked_Param_Info_Type is
   --  Convert byte back into locked-param-info record
   begin
      return Locked_Param_Info_Type'
        (Param_Index    => Natural (Info_As_Byte mod 32),
         Is_Var         => Boolean'Val (Info_As_Byte / 2**5 mod 2),
         Is_By_Ref      => Boolean'Val (Info_As_Byte / 2**6 mod 2),
         Is_Queued_Call => Boolean'Val (Info_As_Byte / 2**7 mod 2));
   end Locked_Param_Info_From_Byte;

   --------------------------
   -- Make_Copy_In_Stg_Rgn --
   --------------------------

   procedure Make_Copy_In_Stg_Rgn
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Source                  : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info               : Object_Locator)
   is
      Type_Desc     : constant Type_Descriptor_Ptr :=
                        Get_Type_Desc (Context, Type_Info);
      Value_To_Copy : constant Word_Type := Fetch_Word (Context, Source);
   begin
      if Is_Small (Type_Desc) then
         Store_Word (Context, Destination, Value_To_Copy);
      else
         declare
            Copy_Of_Obj : constant Word_Type :=
              Copy_Large_Obj
                 (Type_Desc,
                  Value_To_Copy,
                  Stg_Rgn_Of_Large_Obj
                     (Fetch_Word (Context, Existing_Obj_In_Stg_Rgn)),
                  Context.Server_Index);
         begin
            Store_Word (Context, Destination, Copy_Of_Obj);
         end;
      end if;
   end Make_Copy_In_Stg_Rgn;

   --------------------------
   -- Make_Copy_In_Stg_Rgn --
   --------------------------

   procedure Make_Copy_In_Stg_Rgn_Exported
     (Context                 : in out Exec_Context;
      Type_Info               : Type_Descriptor_Ptr;
      Destination             : Word_Ptr;
      Source                  : Word_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Ptr)
   is
      Value_To_Copy : constant Word_Type := Fetch_Word (Source, 0);
      Stg_Region    : Stg_Rgn_Ptr := null;
      Type_Desc     : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
   begin
      if Is_Small (Type_Desc) then
         Store_Word (Destination, 0, Value_To_Copy);
      else
         if Existing_Obj_In_Stg_Rgn = null then
            Stg_Region := Local_Stg_Rgn (Context);
         else
            Stg_Region := Stg_Rgn_Of_Large_Obj
              (Fetch_Word (Existing_Obj_In_Stg_Rgn, 0));
         end if;

         declare
            Copy_Of_Obj : constant Word_Type :=
              Copy_Large_Obj
                 (Type_Desc,
                  Value_To_Copy,
                  Stg_Region,
                  Context.Server_Index);
         begin
            Store_Word (Destination, 0, Copy_Of_Obj);
         end;
      end if;
   end Make_Copy_In_Stg_Rgn_Exported;

   --------------
   -- Move_Obj --
   --------------

   procedure Move_Obj
     (Context   : in out Exec_Context;
      LHS       : Object_Locator;
      RHS       : Object_Locator;
      Type_Info : Object_Locator)
   is
      Type_Desc     : constant Type_Descriptor_Ptr :=
                        Get_Type_Desc (Context, Type_Info);
      LHS_Ptr       : constant Word_Ptr := Locator_To_Physical_Address
                                    (Context, LHS);
      RHS_Ptr       : constant Word_Ptr := Locator_To_Physical_Address
                                    (Context, RHS);
   begin
      --  Pass the buck to version that takes pointers
      Move_Object (Context, Type_Desc, LHS_Ptr, RHS_Ptr);
   end Move_Obj;

   -----------------
   -- Move_Object --
   -----------------

   procedure Move_Object
     (Context   : in out Exec_Context;
      Type_Info : Type_Descriptor_Ptr;
      LHS_Ptr   : Word_Ptr;
      RHS_Ptr   : Word_Ptr) is

      LHS_Value     : constant Word_Type := Fetch_Word (LHS_Ptr, 0);
      RHS_Value     : constant Word_Type := Fetch_Word (RHS_Ptr, 0);
      Type_Desc     : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
      Type_Is_Small : constant Boolean := Is_Small (Type_Desc);

      procedure Finish_Large_Move (RHS_Copy : Word_Type);
         --  Do the common steps at the end of a large-value move,
         --  namely, storing the copy into the LHS, storing null into the RHS,
         --  and releasing the prior value of the LHS.

      procedure Finish_Large_Move (RHS_Copy : Word_Type) is
      begin
         --  Do the move
         Store_Word (LHS_Ptr, 0, RHS_Copy);

         --  Store large null for region of RHS obj
         Store_Word
           (RHS_Ptr, 0,
            Null_For_Stg_Rgn (Stg_Rgn_Of_Large_Obj (RHS_Value)));

         --  Now release the storage
         --  NOTE: We do it in this order so we never
         --       have an object pointing at freed storage.
         --       Also, it means we could reclaim the storage
         --       in the background (e.g. in a separate thread).
         Release_Large_Obj (Type_Desc, LHS_Value,
           Server_Index => Context.Server_Index);

      end Finish_Large_Move;

   begin  --  Move_Object

      if not Type_Is_Small then
         Check_Is_Large (LHS_Value);
         Check_Is_Large (RHS_Value);
      end if;

      if Type_Is_Small then
         --  No need to reclaim storage; just move value and set
         --  RHS to null.
         Store_Word (LHS_Ptr, 0, RHS_Value);
         Store_Word (RHS_Ptr, 0, Null_For_Type (Type_Desc));

      elsif Stg_Rgn_Of_Large_Obj (LHS_Value) =
            Stg_Rgn_Of_Large_Obj (RHS_Value)
      then
         --  Stg_Rgns match, do a shallow copy if RHS on stack and LHS isn't,
         --  and then move value into LHS and set RHS to appropriate
         --  null for region.
         if Large_Obj_On_Stack (RHS_Value)
           and then not Large_Obj_On_Stack (LHS_Value)
         then
            Finish_Large_Move (Shallow_Copy_Large_Obj (Context, RHS_Value));
         else
            Finish_Large_Move (RHS_Value);
         end if;

      elsif Is_Special_Large_Value (RHS_Value) then
         --  RHS is a "special" large object; just need to change the region
         Finish_Large_Move
           (Replace_Special_Value_Stg_Rgn (RHS_Value,
             Stg_Rgn_Of_Large_Obj (LHS_Value)));

      else
         --  Need to copy RHS and free old value
         declare
            RHS_Copy : constant Word_Type :=
                         Copy_Large_Obj
                           (Type_Desc,
                            RHS_Value,
                            Stg_Rgn_Of_Large_Obj (LHS_Value),
                            Context.Server_Index);
            RHS_Lock : constant Lock_Obj_Index :=
                         Large_Obj_Lock_Obj (RHS_Value);
         begin
            if RHS_Lock /= 0 then
               --  Move the lock to RHS_Copy
               pragma Assert (RHS_Lock in 1 .. Lock_Obj_Limit);
               Set_Large_Obj_Lock_Obj (RHS_Value, 0);
               Set_Large_Obj_Lock_Obj (RHS_Copy, RHS_Lock);
            else
               pragma Assert
                 (Large_Obj_Lock_Obj (RHS_Copy) in 0 .. Lock_Obj_Limit);
               null;
            end if;

            --  Do the move
            Finish_Large_Move (RHS_Copy);

            --  Now release the storage of RHS.
            --  NOTE: We do it in this order so we never
            --       have an object pointing at freed storage.
            --       Also, it means we could reclaim the storage
            --       in the background (e.g. in a separate thread).
            Release_Large_Obj (Type_Desc, RHS_Value,
              Server_Index => Context.Server_Index);
         end;
      end if;
   end Move_Object;

   ---------------------------------
   -- Name_With_Overloading_Index --
   ---------------------------------

   function Name_With_Overloading_Index
     (Simple_Name : Strings.U_String; Num_Prior_Homonyms : Natural)
     return Strings.U_String is
   --  Returns Simple_Name if Num_Prior_Homonyms is 0;
   --  otherwise concatenates "#XX" where XX = Num_Prior_Homonyms + 1.
   begin
      if Num_Prior_Homonyms = 0 then
         return Simple_Name;
      else
         --  Concatenate "#XX" where XX = Num_Prior_Homonyms + 1
         declare
            Overloading_Index : String :=
              Natural'Image (Num_Prior_Homonyms + 1);
         begin
            --  Replace leading ' ' with '#'
            Overloading_Index (Overloading_Index'First) := '#';

            --  Return concatenation
            return Strings.String_Lookup
              (Strings.To_String (Simple_Name) & Overloading_Index);
         end;
      end if;
   end Name_With_Overloading_Index;

   -----------------------
   -- New_Stg_Rgn_Chunk --
   -----------------------

   function New_Stg_Rgn_Chunk_Index return Chunk_Index is
      Result : Chunk_Index;
   begin
      --  Get a unique chunk index
      Stg_Rgn_Manager.Get_Stg_Rgn_Chunk_Index (Result);

      return Result;
   end New_Stg_Rgn_Chunk_Index;

   ----------------------
   -- Obj_Locator_Read --
   ----------------------

   procedure Obj_Locator_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Object_Locator) is
   --  Read the Base and Offset, fill in No_VM_Obj_Id for VM_Obj_Id.
   begin
      Area_Base_Indicator'Read (Stream, Item.Base);
      Offset_Within_Area'Read (Stream, Item.Offset);
      Item.VM_Obj_Id := No_VM_Obj_Id;
   end Obj_Locator_Read;

   -----------------------
   -- Obj_Locator_Write --
   -----------------------

   procedure Obj_Locator_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : in Object_Locator) is
   --  Write the Base and Offset, ignore VM_Obj_Id
   begin
      Area_Base_Indicator'Write (Stream, Item.Base);
      Offset_Within_Area'Write (Stream, Item.Offset);
   end Obj_Locator_Write;

   ------------------------------------
   -- Operation_Index_Array_Ptr_Read --
   ------------------------------------

   procedure Operation_Index_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Operation_Index_Array_Ptr) is
   --  Read in the contents of the operation-index array
      Len : constant Operation_Index := Operation_Index'Input (Stream);
      Index_Arr : Operation_Index_Array (1 .. Len);
   begin
      Operation_Index_Array'Read (Stream, Index_Arr);
      if Stream.all in Buffered_Desc_Reader then
         --  We have a map, these are worth saving
         --  NOTE: We need to do the 'Reads anyway so everything
         --        matches up correctly.
         Item := new Operation_Index_Array'(Index_Arr);
      else
         Item := null;
      end if;
   end Operation_Index_Array_Ptr_Read;

   -------------------------------------
   -- Operation_Index_Array_Ptr_Write --
   -------------------------------------

   procedure Operation_Index_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Operation_Index_Array_Ptr) is
   --  Write out the contents of the operation-index array
   begin
      Operation_Index'Write (Stream, Item'Length);
      Operation_Index_Array'Write (Stream, Item.all);
   end Operation_Index_Array_Ptr_Write;

   ------------------------------------
   -- Op_Map_Type_Array_Ptr_Read --
   ------------------------------------

   procedure Op_Map_Type_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Op_Map_Type_Array_Ptr) is
   --  Read in the contents of the op-map array
      Len : constant Op_Map_Count := Op_Map_Count'Input (Stream);
      Op_Map_Arr : Op_Map_Type_Array (1 .. Len);
   begin
      Op_Map_Type_Array'Read (Stream, Op_Map_Arr);
      if Stream.all in Buffered_Desc_Reader then
         --  We have a map, these are worth saving
         --  NOTE: We need to do the 'Reads anyway so everything
         --        matches up correctly.
         Item := new Op_Map_Type_Array'(Op_Map_Arr);
      else
         Item := null;
      end if;
   end Op_Map_Type_Array_Ptr_Read;

   -------------------------------------
   -- Op_Map_Type_Array_Ptr_Write --
   -------------------------------------

   procedure Op_Map_Type_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Op_Map_Type_Array_Ptr) is
   --  Write out the contents of the op-map array
   begin
      if Item /= null then
         Op_Map_Count'Write (Stream, Item'Length);
         Op_Map_Type_Array'Write (Stream, Item.all);
      else
         Op_Map_Count'Write (Stream, 0);
      end if;
   end Op_Map_Type_Array_Ptr_Write;

   ------------------------
   -- Phys_Base_Register --
   ------------------------

   function Phys_Base_Register
     (Local_Area_Offset : Offset_Within_Area) return Area_Base_Indicator is
   --  Return an Area_Base_Indicator for the phys-address base register
   --  represented by the local at the given offset within the local area.
      pragma Assert (Local_Area_Offset <= Max_Offset_For_Base_Register);
   begin
      return Phys_Base_Registers'First +
        Area_Base_Indicator (Local_Area_Offset);
   end Phys_Base_Register;

   ----------------------
   -- Register_Builtin --
   ----------------------

   procedure Register_Builtin
     (Desig   : Strings.U_String;
      Builtin : Routine_Code_Address)
   is
      Index : constant Table_Index :=
                Strings.Hash (Desig) mod Builtin_Table_Size;
   begin
      --  Check that there are no duplicates
      if Find_Builtin (Desig) /= null then
         Messages.Put_Message
           ("Duplicate definition: " & Strings.To_String (Desig),
            Src_Pos => Source_Positions.Null_Source_Position,
            Message_Kind => "Fatal error");
         Messages.Number_Of_Errors := Messages.Number_Of_Errors + 1;
         raise Duplicate_Entry;
      end if;

      --  Now enter it into the table
      Builtins_Table (Index) :=
        new Builtin_Entry'
        (Desig => Desig,
         Builtin => Builtin,
         Next => Builtins_Table (Index));
   end Register_Builtin;

   -----------------------
   -- Release_Large_Obj --
   -----------------------

   procedure Release_Large_Obj
     (Type_Desc          : Type_Descriptor_Ptr;
      Old_Value          : Word_Type;
      Server_Index       : Thread_Server_Index;
      Old_Stg_Rgn        : Stg_Rgn_Ptr := null;
      Enclosing_Lock_Obj : Lock_Obj_Index := 0)
   is
      pragma Assert (not Is_Small (Type_Desc));
   begin
      Check_Is_Large (Old_Value);

      if Debug_Stg_Rgns then
         if Old_Stg_Rgn /= null then
            --  Make sure that regions match
            pragma Assert (Old_Stg_Rgn = Stg_Rgn_Of_Large_Obj (Old_Value));
            null;
         end if;
      end if;

      if not Is_Special_Large_Value (Old_Value) then

         --  Release each component

         declare
            Old_Value_Stg_Rgn : constant Stg_Rgn_Ptr :=
                                  Stg_Rgn_Of_Large_Obj (Old_Value);
            Old_Value_Lock    : constant Lock_Obj_Index :=
                                  Large_Obj_Lock_Obj (Old_Value);
            Old_Type_Desc     : constant Type_Descriptor_Ptr :=
                                  Large_Obj_Type_Desc (Old_Value);
         begin
            if Old_Value_Stg_Rgn.Is_Constant_Stg_Rgn then
               return;  --  Nothing to release
            end if;

            if Old_Type_Desc.Type_Kind = Basic_Array_Kind then
               --  Release array components
               declare
                  Arr_Comp_Type : constant Type_Descriptor_Ptr :=
                    Basic_Array_Comp_Type (Old_Type_Desc);
               begin
                  if not Is_Small (Arr_Comp_Type) then
                     --  Need to iterate over array of large objects,
                     --  freeing each one.
                     declare
                        Len : constant Word_Type :=
                                Content_Of_Virtual_Address
                                   (Old_Value + Large_Obj_Header_Size);
                        --  TBD: This "len" is a bit of a waste as it can
                        --       be computed from the size by subtracting hdr
                        --       size.
                     begin
                        for I in 1 .. Offset_Within_Area (Len) loop
                           declare
                              Offset : constant Offset_Within_Area :=
                                         Large_Obj_Header_Size + I;
                           begin
                              --  Release each array component
                              Release_Large_Obj
                                (Arr_Comp_Type,
                                 Content_Of_Virtual_Address
                                   (Old_Value + Offset),
                                 Server_Index,
                                 Old_Stg_Rgn => Old_Value_Stg_Rgn);
                           end;
                        end loop;
                     end;
                  end if;
               end;
            else
               --  Release each (non-array) component
               declare
                  Old_Stg_Rgn_To_Pass : Stg_Rgn_Ptr := Old_Stg_Rgn;
               begin
                  if Old_Stg_Rgn_To_Pass = null
                    and then not Old_Type_Desc.Is_Polymorphic
                  then
                     --  Set Old_Stg_Rgn_To_Pass if not yet set.
                     --  However, if this is a "top-level" polymorphic
                     --  object, don't require the regions to match.
                     Old_Stg_Rgn_To_Pass := Old_Value_Stg_Rgn;
                  end if;

                  for I in 1 .. Old_Type_Desc.Num_Components loop
                     declare
                        Comp_Type : constant Non_Op_Map_Type_Ptr :=
                          Skip_Over_Op_Map
                             (Old_Type_Desc.Components (I).Type_Desc);
                     begin
                        if not Old_Type_Desc.Components (I).Is_By_Ref
                          and then not Is_Small (Comp_Type)
                        then
                           --  Recurse
                           Release_Large_Obj
                             (Comp_Type,
                              Content_Of_Virtual_Address
                                 (Old_Value +
                                  Large_Obj_Header_Size +
                                  Offset_Within_Area (I - 1)),
                              Server_Index,
                              Old_Stg_Rgn => Old_Stg_Rgn_To_Pass,
                              Enclosing_Lock_Obj => Old_Value_Lock);
                        end if;
                     end;
                  end loop;
               end;
            end if;

            --  Now release object itself and its lock
            if Old_Value_Lock /= 0 and then
              Old_Value_Lock /= Enclosing_Lock_Obj  --  Allow lock sharing
            then
               pragma Assert (Old_Value_Lock in 1 .. Lock_Obj_Limit);
               Release_Lock_Obj (Old_Value_Lock,
                 Old_Value_Stg_Rgn, Server_Index);
            end if;

            if not Large_Obj_On_Stack (Old_Value) then
               --  Do not release objects on stack
               --  as they might be declared inside a loop
               --  and hence reused on next iteration of loop.
               Deallocate_From_Stg_Rgn
                 (Old_Value_Stg_Rgn, Old_Value, Server_Index);
            end if;
         end;
      end if;
   end Release_Large_Obj;

   ------------------------
   -- Routine_Index_Read --
   ------------------------

   procedure Routine_Index_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Routine_Index'Base) is
   --  Read in (module-id, operation-id) and look up in Id->Routine map
      use Per_File_Strings;
   begin
      if Stream.all in Buffered_Reader_With_Strings'Class then
         --  Presume we have an Operation_Id of (mod-index, oper-index)
         declare
            String_Tab : Local_String_Map_Type renames
              Buffered_Reader_With_Strings'Class (Stream.all).Strings.all;
            Routine_Local_Id : constant Operation_Id :=
              Operation_Id'Input (Stream);
            --  Convert local str index for module name to global index
            Mod_Index : constant Strings.U_String_Index :=
              Strings.U_String_Index
                (String_Tab (Routine_Local_Id.Module_Name) mod 2**32);
            --  Convert local str index for operation name to global index
            Op_Index : constant Strings.U_String_Index :=
              Strings.U_String_Index
                (String_Tab (abs Routine_Local_Id.Operation_Name) mod 2**32);
            Is_Builtin : constant Boolean :=
                              Routine_Local_Id.Operation_Name < 0;
            --  Get routine, given its global op id
            Routine : constant Routine_RW_Ptr :=
              Get_Routine_By_Global_Id
                ((Module_Name => Mod_Index,
                  Operation_Name => Op_Index,
                  Is_Builtin => Routine_Local_Id.Operation_Name < 0));
         begin
            --  And return its index
            Item := Routine.Index;
         end;
      else
         --  No string table, so just do default read of routine index
         Basic_Routine_Index'Read (Stream, Basic_Routine_Index (Item));
      end if;
   end Routine_Index_Read;

   -------------------------
   -- Routine_Index_Write --
   -------------------------

   procedure Routine_Index_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Routine_Index'Base) is
   --  Write out a Routine_Index as a pair (module-id, operation-id)
      use Per_File_Strings;
   begin
      if Stream.all in Buffered_Stream_With_PFS'Class then
         --  We have a per-file stream, so convert to module-id/op-id pair
         declare
            PFST : constant Per_File_String_Table_Ptr :=
              Buffered_Stream_With_PFS'Class
                (Stream.all).PFS.all'Unchecked_Access;
            Routine : constant Routine_Ptr := Nth_Routine (Item);
            Module_Index : constant Local_String_Index :=
              Get_Local_Index (PFST, Strings.Index (Routine.Full_Module_Name));
               --  Get local index assigned to full-module name
            Operation_Index : Local_String_Index_Base;
         begin
            if not Routine.Is_PSVM_Routine
              and then Routine.Built_In_Desig /= Strings.Null_U_String
            then
               --  For built-ins, we use "desig" instead of operation name
               --  and negate the Operation_Index.
               Operation_Index :=
                 -Get_Local_Index (PFST,
                   Strings.Index (Routine.Built_In_Desig));
            else
               --  For others, we add in overloading index to make it unique
               Operation_Index :=
                 Get_Local_Index (PFST,
                   Strings.Index
                     (Routine_Name_With_Overloading_Index (Routine)));
                  --  Get local index assigned to name-with-overloading-index
            end if;

            --  Write out as an Operation_Id pair
            Operation_Id'Write (Stream, (Module_Name => Module_Index,
                                         Operation_Name => Operation_Index));
         end;
      else
         --  No choice but to just write it out as is.
         --  (Convert to Basic_Routine_Index to get default 'Write.)
         Basic_Routine_Index'Write (Stream, Basic_Routine_Index (Item));
      end if;
   end Routine_Index_Write;

   -----------------------------------------
   -- Routine_Name_With_Overloading_Index --
   -----------------------------------------

   function Routine_Name_With_Overloading_Index
     (Routine : Routine_Ptr)
     return Strings.U_String is
   --  Returns Routine.Name if Routine.Num_Prior_Homonyms is 0;
   --  otherwise concatenates "#XX" where XX = Num_Prior_Homonyms + 1.
      use type Strings.U_String;
   begin
      if Routine.Name_With_Overloading_Index /= Strings.Null_U_String then
         --  Already filled in
         return Routine.Name_With_Overloading_Index;
      end if;

      --  Compute the name with overloading string index
      return Name_With_Overloading_Index
        (Routine.Name, Routine.Num_Prior_Homonyms);
   end Routine_Name_With_Overloading_Index;

   --------------------------
   -- Select_Ancestor_Part --
   --------------------------

   function Select_Ancestor_Part
     (Context            : in out Exec_Context;
      Source_Obj         : Word_Type;
      Ancestor_Type_Desc : Type_Descriptor_Ptr;
      Source_Type_Desc   : Type_Descriptor_Ptr;
      Is_Passed_By_Ref   : Boolean) return Word_Type
   is
      pragma Assert (not Source_Type_Desc.Has_Op_Map);
      Source_Obj_To_Use : Word_Type := Source_Obj;
   begin
      if Source_Type_Desc.Is_Polymorphic then
         --  Extract "true" source type from polymorphic object
         declare
            Poly_Type : constant Type_Descriptor_Ptr :=
                          Large_Obj_Type_Desc (Source_Obj_To_Use);
            pragma Assert (Poly_Type.Is_Polymorphic);

            Underlying_Type : constant Type_Descriptor_Ptr :=
                                Poly_Type.Components (1).Type_Desc;
            pragma Assert (not Underlying_Type.Has_Op_Map);

            --  Should be an extension of Ancestor type, so shouldn't need an
            --  op-map.
            --  TBD: This might not be true, unless front end makes an effort
            --      to align op#s with parent type.
            Underlying_Obj : Word_Type;
         begin
            if not Is_Small (Underlying_Type) then
               Share_Lock (Source_Obj_To_Use);  -- Share lock if any
            end if;

            if Debug_Calls then
               Put_Line
                 (" Selecting polymorphic ancestor part " &
                  "starting with obj of polymorphic type " &
                  Type_Sem_Image (Poly_Type.Type_Sem) & ":");
               Dump_Obj_With_Indent
                 (Source_Obj_To_Use, Poly_Type, Indent => 2);
            end if;

            --  Pass the underlying object to recursive call on
            --  Select_Ancestor_Part

            if Is_Passed_By_Ref then
               --  Compute "ref" to underlying obj
               Underlying_Obj :=
                 Word_Ptr_To_Word (Virtual_To_Physical_Address
                   (Source_Obj_To_Use + Large_Obj_Header_Size));
            else
               --  Compute value of underlying obj
               Underlying_Obj :=
                  Content_Of_Virtual_Address
                    (Source_Obj + Large_Obj_Header_Size);
            end if;

            --  Recurse with "true" type of polymorphic object
            return Select_Ancestor_Part
              (Context,
               Source_Obj => Underlying_Obj,
               Ancestor_Type_Desc => Ancestor_Type_Desc,
               Source_Type_Desc => Underlying_Type,
               Is_Passed_By_Ref => Is_Passed_By_Ref);

         end;
      else
         --  Non-polymorphic object, Source_Type_Desc is correct;
         --  check for pass by ref.

         if Is_Passed_By_Ref then
            --  Follow level of indirection
            Source_Obj_To_Use := Fetch_Word (Word_To_Word_Ptr (Source_Obj), 0);
         end if;

         declare
            Ancestor_Type_Level : constant Natural :=
              Ancestor_Type_Desc.Component_Extension_Level;

            Ancestor_Is_Wrapper : constant Boolean :=
              Ancestor_Type_Desc.Is_Wrapper;

            Ances_Obj      : Word_Type := Source_Obj_To_Use;
            Ances_Obj_Addr : Word_Ptr := null;
            Next_Desc      : Type_Descriptor_Ptr := Source_Type_Desc;
         begin
            if Debug_Calls then
               Put_Line
                 (" Selecting ancestor part of type " &
                  Type_Sem_Image (Ancestor_Type_Desc.Type_Sem) &
                  " given type " &
                  Type_Sem_Image (Source_Type_Desc.Type_Sem));
            end if;

            if Is_Passed_By_Ref then
               Ances_Obj_Addr := Word_To_Word_Ptr (Source_Obj);
            end if;

            --  When component extension level changes, we do a level
            --  of indirection, unless child type is a wrapper of parent.

            --  When extension level stays the same, we do a level of
            --  indirection only if parent is a wrapper and child type is
            --  not, and parent has no parent, or the
            --  grand-parent has no components

            --  Example:
            --     interface A0, visible component V  (A0 is wrapper of V)
            --       (level 0)
            --     interface A extends A0, private components A1 and A2
            --       (level 0)
            --     interface B extends A adds visible components B1 and B2
            --       (level 1)
            --     interface C extends B, no new components (C is wrapper of B)
            --       (level 1)
            --     interface D extends C, adds private component D1
            --       (level 1)
            --     interface E extends D, no new components (E is wrapper of D)
            --       (level 2)

            --  Representation of an E object is identical to D.
            --  Representation of a D object: <Anc-obj-of-type A>, B1, B2, D1
            --  Representation of a C object is identical to B.
            --  Representation of a B object: <Parent-obj-of-type A>, B1, B2
            --  Representation of an A obj: <V, A1, A2>
            --  Representation of an A0 obj: Identical to V

            --  If we inherit routine from A0 into E, we need to do various
            --  levels of indir going from "E" object to an "A0" obj

            --  Do appropriate number of levels of indirection
            while Next_Desc.Component_Extension_Level > Ancestor_Type_Level
              or else Next_Desc.Is_Wrapper /= Ancestor_Is_Wrapper
            loop
               declare
                  Next_Parent_Desc : constant Type_Descriptor_Ptr :=
                                       Next_Desc.Parent_Type;
               begin
                  --  NOTE: Here we are applying wrapper checks
                  --  TBD: Do this in the compiler eventually and store
                  --      a "physical" component level.
                  if not Next_Desc.Is_Wrapper
                    and then
                        (Next_Desc.Component_Extension_Level >
                         Next_Parent_Desc.Component_Extension_Level
                      or else
                        (Next_Parent_Desc.Is_Wrapper
                          and then
                            (Next_Parent_Desc.Parent_Type = null
                               or else
                             Next_Parent_Desc.Parent_Type.Num_Components = 0)))
                  then
                     --  Insert a level of indirection
                     if not Is_Small (Next_Parent_Desc) then
                        Share_Lock (Ances_Obj);  --  But first, share the lock
                     end if;

                     Ances_Obj_Addr :=
                        Virtual_To_Physical_Address
                          (Ances_Obj + Large_Obj_Header_Size);
                     Ances_Obj :=
                        Content_Of_Physical_Address (Ances_Obj_Addr);
                  else
                     if Debug_Calls then
                        Put_Line
                          (" Suppressing level of indir for " &
                           Type_Sem_Image (Next_Parent_Desc.Type_Sem));
                     end if;
                  end if;

                  Next_Desc := Next_Parent_Desc;
               end;
            end loop;

            if Debug_Calls then
               Put_Line (" Selected ancestor obj is:");
               Dump_Obj_With_Indent (Ances_Obj, Ancestor_Type_Desc,
                 Indent => 2);
            end if;

            if Is_Passed_By_Ref then
               --  Return a "ref"
               return Word_Ptr_To_Word (Ances_Obj_Addr);
            else
               --  Return a "value"
               return Ances_Obj;
            end if;

         end;
      end if;
   end Select_Ancestor_Part;

   --------------------------------------
   -- Select_Polymorphic_Ancestor_Part --
   --------------------------------------

   procedure Select_Polymorphic_Ancestor_Part
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Source      : Object_Locator;
      Type_Info   : Object_Locator;
      Polymorphic_Ancestor_Lvalue : Boolean)
   is
      Ancestor_Type_Desc : constant Type_Descriptor_Ptr :=
                             Get_Type_Desc (Context, Type_Info);

      Poly_Obj   : constant Word_Type := Fetch_Word (Context, Source);
      Poly_Type  : constant Type_Descriptor_Ptr :=
                     Large_Obj_Type_Desc (Poly_Obj);
      pragma Assert (Poly_Type.Is_Polymorphic);

   begin
      --  Pass the buck to Select_Ancestor_Part which checks
      --  for Is_Polymorphic case.
      Store_Word (Context, Destination,
         Select_Ancestor_Part
           (Context,
            Source_Obj => Poly_Obj,
            Ancestor_Type_Desc => Ancestor_Type_Desc,
            Source_Type_Desc => Poly_Type,
            Is_Passed_By_Ref => Polymorphic_Ancestor_Lvalue));
   end Select_Polymorphic_Ancestor_Part;

   function Get_Large_Obj_Type_Descriptor (Obj : Word_Type)
      return Non_Op_Map_Type_Ptr is
   begin
      return Large_Obj_Type_Desc (Obj);
   end Get_Large_Obj_Type_Descriptor;

   ----------------------------
   -- Shallow_Copy_Large_Obj --
   ----------------------------

   function Shallow_Copy_Large_Obj
     (Context   : in out Exec_Context;
      Stack_Val : Word_Type) return Word_Type is
   --  Copy top level of large stack-resident object into local stg_rgn

      Obj_Header : Large_Obj_Header := To_Large_Obj_Ptr (Stack_Val).all;
      pragma Assert (Obj_Header.On_Stack);

      Obj_Size : constant Offset_Within_Area := Obj_Header.Size;
      New_Obj : constant Word_Type :=
        Allocate_From_Stg_Rgn
          (Stg_Rgn_Table (Obj_Header.Stg_Rgn), Obj_Size, Context.Server_Index);
         --  Allocate in same region as stack-resident object

      Old_Obj_Ptr : constant Word_Ptr := Word_To_Word_Ptr (Stack_Val);
      New_Obj_Ptr : constant Word_Ptr := Word_To_Word_Ptr (New_Obj);
   begin
      --  Set the new obj header with On_Stack off.
      Obj_Header.On_Stack := False;
      To_Large_Obj_Ptr (New_Obj).all := Obj_Header;

      --  Copy the rest of the object
      for Offset in Large_Obj_Header_Size .. Obj_Size - 1 loop
         Store_Word (New_Obj_Ptr, Offset, Fetch_Word (Old_Obj_Ptr, Offset));
      end loop;

      return New_Obj;
   end Shallow_Copy_Large_Obj;

   ----------------
   -- Share_Lock --
   ----------------

   procedure Share_Lock (Large_Obj : Object_Virtual_Address) is
      Enclosing_Lock : constant Lock_Obj_Index :=
                         Large_Obj_Lock_Obj (Large_Obj);
   begin
      if Enclosing_Lock /= 0 then
         --  Make sure that first component also has same lock
         declare
            First_Comp : constant Object_Virtual_Address :=
              Content_Of_Virtual_Address (Large_Obj + Large_Obj_Header_Size);
            pragma Assert (not Is_Small
              (Large_Obj_Type_Desc (Large_Obj).Components (1).Type_Desc));
         begin
            if not Is_Special_Large_Value (First_Comp)
              and then Large_Obj_Lock_Obj (First_Comp) = 0
            then
               Set_Large_Obj_Lock_Obj (First_Comp, Enclosing_Lock);
            end if;
         end;
      end if;
   end Share_Lock;

   -----------------------------------
   -- Stg_Rgn_Of_Existing_Large_Obj --
   -----------------------------------

   function Stg_Rgn_Of_Existing_Large_Obj
     (Context                 : in out Exec_Context;
      Existing_Obj_In_Stg_Rgn : Object_Locator) return Stg_Rgn_Ptr is
   begin
      if Is_Null_Obj_Locator (Existing_Obj_In_Stg_Rgn) then
         return Local_Stg_Rgn (Context);
      else
         return Stg_Rgn_Of_Large_Obj
                  (Fetch_Word (Context, Existing_Obj_In_Stg_Rgn));
      end if;
   end Stg_Rgn_Of_Existing_Large_Obj;

   function Stg_Rgn_Of_Existing_Large_Obj_Exported
     (Context                 : Exec_Context;
      Existing_Obj_In_Stg_Rgn : Word_Ptr) return Stg_Rgn_Ptr is
   --  Same thing but uses Word_Ptr instead of Object_Locator
   begin
      if Existing_Obj_In_Stg_Rgn = null then
         return Local_Stg_Rgn (Context);
      else
         return Stg_Rgn_Of_Large_Obj
                  (Fetch_Word (Existing_Obj_In_Stg_Rgn, 0));
      end if;
   end Stg_Rgn_Of_Existing_Large_Obj_Exported;

   ----------------------
   -- Store_Local_Null --
   ----------------------

   procedure Store_Local_Null
     (Context     : in out Exec_Context;
      Destination : Object_Locator;
      Type_Info   : Object_Locator)
   is
      Type_Desc : constant Type_Descriptor_Ptr :=
                    Get_Type_Desc (Context, Type_Info);
   begin
      if Is_Small (Type_Desc) then
         --  Store appropriate kind of "null"
         Store_Word (Context, Destination, Null_For_Type (Type_Desc));
      else
         --  Store large null for local region
         pragma Assert (Context.Local_Null /= Null_Virtual_Address);
         Store_Word
           (Context,
            Destination,
            Context.Local_Null);
      end if;
   end Store_Local_Null;

   --------------------------------
   -- Store_Null_Of_Same_Stg_Rgn --
   --------------------------------

   procedure Store_Null_Of_Same_Stg_Rgn
     (Context                 : in out Exec_Context;
      Destination             : Object_Locator;
      Existing_Obj_In_Stg_Rgn : Object_Locator;
      Type_Info               : Object_Locator)
   is
      Type_Desc : constant Type_Descriptor_Ptr :=
                    Get_Type_Desc (Context, Type_Info);
   begin
      if Is_Small (Type_Desc) then
         --  Store appropriate kind of "null"
         Store_Word (Context, Destination, Null_For_Type (Type_Desc));
      else
         --  Store large null for region determined by Existing obj
         Store_Word
           (Context,
            Destination,
            Null_For_Stg_Rgn
               (Stg_Rgn_Of_Large_Obj
                  (Fetch_Word (Context, Existing_Obj_In_Stg_Rgn))));
      end if;
   end Store_Null_Of_Same_Stg_Rgn;

   -----------------------------------------
   -- Store_Null_Of_Same_Stg_Rgn_Exported --
   -----------------------------------------

   procedure Store_Null_Of_Same_Stg_Rgn_Exported
     (Context                 : Exec_Context;
      Type_Info               : Type_Descriptor_Ptr;
      Destination             : Word_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Ptr)
   is
   begin
      if Is_Small (Type_Info) then
         --  Store appropriate kind of "null"
         Store_Word (Destination, 0, Null_For_Type (Type_Info));
      else
         --  Store large null for region determined by Existing obj
         Store_Word
           (Destination, 0,
            Null_For_Stg_Rgn
               (Stg_Rgn_Of_Large_Obj
                  (Fetch_Word (Existing_Obj_In_Stg_Rgn, 0))));
      end if;
   end Store_Null_Of_Same_Stg_Rgn_Exported;

   --------------
   -- Swap_Obj --
   --------------

   procedure Swap_Obj
     (Context   : in out Exec_Context;
      LHS       : Object_Locator;
      RHS       : Object_Locator;
      Type_Info : Object_Locator)
   is
      Type_Desc     : constant Type_Descriptor_Ptr :=
                        Get_Type_Desc (Context, Type_Info);
      LHS_Value     : Word_Type := Fetch_Word (Context, LHS);
      RHS_Value     : Word_Type := Fetch_Word (Context, RHS);
      Type_Is_Small : constant Boolean := Is_Small (Type_Desc);
   begin
      if Type_Is_Small then
         --  Just swap the small values
         Store_Word (Context, LHS, RHS_Value);
         Store_Word (Context, RHS, LHS_Value);
      else
         --  Swap large values; see if in same region
         Check_Is_Large (LHS_Value);
         Check_Is_Large (RHS_Value);

         if Stg_Rgn_Of_Large_Obj (LHS_Value) /=
                 Stg_Rgn_Of_Large_Obj (RHS_Value)
         then
            --  Need to copy both and free both old values
            declare
               LHS_Copy : constant Word_Type :=
                 Copy_Large_Obj
                    (Type_Desc,
                     LHS_Value,
                     Stg_Rgn_Of_Large_Obj (RHS_Value),
                     Context.Server_Index);
               RHS_Copy : constant Word_Type :=
                 Copy_Large_Obj
                    (Type_Desc,
                     RHS_Value,
                     Stg_Rgn_Of_Large_Obj (LHS_Value),
                     Context.Server_Index);
            begin
               --  Do the swap
               Store_Word (Context, LHS, RHS_Copy);
               Store_Word (Context, RHS, LHS_Copy);

               --  Now release the storage
               --  NOTE: We do it in this order so we never
               --       have an object pointing at freed storage.
               --       Also, it means we could reclaim the storage
               --       in the background (e.g. in a separate thread).
               Release_Large_Obj (Type_Desc, LHS_Value,
                 Server_Index => Context.Server_Index);
               Release_Large_Obj (Type_Desc, RHS_Value,
                 Server_Index => Context.Server_Index);
            end;
         else
            --  Check whether one is on the stack while the other isn't.
            --  If so we need to do a shallow copy of the one on the stack,
            --  in case we are inside a loop and the stack-based value would
            --  need to be re-used.
            if Large_Obj_On_Stack (LHS_Value) then
               if not Large_Obj_On_Stack (RHS_Value) then
                  --  LHS is on the stack but RHS is not; make a shallow copy
                  LHS_Value := Shallow_Copy_Large_Obj (Context, LHS_Value);
               end if;
            elsif Large_Obj_On_Stack (RHS_Value) then
               --  RHS is on the stack, and LHS is not
               RHS_Value := Shallow_Copy_Large_Obj (Context, RHS_Value);
            end if;

            --  No need to reclaim storage; just swap values
            Store_Word (Context, LHS, RHS_Value);
            Store_Word (Context, RHS, LHS_Value);
         end if;

      end if;
   end Swap_Obj;

   --------------------------
   -- Swap_Object_Exported --
   --------------------------

   procedure Swap_Object_Exported
     (Context   : in out Exec_Context;
      Type_Info : Type_Descriptor_Ptr;
      LHS_Ptr   : Word_Ptr;
      RHS_Ptr   : Word_Ptr)
   is
      LHS_Value     : constant Word_Type := Fetch_Word (LHS_Ptr, 0);
      RHS_Value     : constant Word_Type := Fetch_Word (RHS_Ptr, 0);
      Type_Desc     : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
      Type_Is_Small : constant Boolean := Is_Small (Type_Desc);
   begin
      if not Type_Is_Small then
         Check_Is_Large (LHS_Value);
         Check_Is_Large (RHS_Value);
      end if;

      if not Type_Is_Small
        and then (Stg_Rgn_Of_Large_Obj (LHS_Value) /=
                    Stg_Rgn_Of_Large_Obj (RHS_Value)
                       or else
                  Large_Obj_On_Stack (LHS_Value)
                       or else
                  Large_Obj_On_Stack (RHS_Value))
      then
         --  Need to copy both and free both old values
         declare
            LHS_Copy : constant Word_Type :=
              Copy_Large_Obj
                 (Type_Desc,
                  LHS_Value,
                  Stg_Rgn_Of_Large_Obj (RHS_Value),
                  Context.Server_Index);
            RHS_Copy : constant Word_Type :=
              Copy_Large_Obj
                 (Type_Desc,
                  RHS_Value,
                  Stg_Rgn_Of_Large_Obj (LHS_Value),
                  Context.Server_Index);
         begin
            --  Do the swap
            Store_Word (LHS_Ptr, 0, RHS_Copy);
            Store_Word (RHS_Ptr, 0, LHS_Copy);

            --  Now release the storage
            --  NOTE: We do it in this order so we never
            --       have an object pointing at freed storage.
            --       Also, it means we could reclaim the storage
            --       in the background (e.g. in a separate thread).
            Release_Large_Obj (Type_Desc, LHS_Value,
              Server_Index => Context.Server_Index);
            Release_Large_Obj (Type_Desc, RHS_Value,
              Server_Index => Context.Server_Index);
         end;
      else
         --  No need to reclaim storage; just swap values
         Store_Word (LHS_Ptr, 0, RHS_Value);
         Store_Word (RHS_Ptr, 0, LHS_Value);
      end if;
   end Swap_Object_Exported;

   ----------------------
   -- To_Size_In_Words --
   ----------------------

   function To_Size_In_Words
     (Size_In_Bits : Natural) return Offset_Within_Area is
   begin
      --  Round up
      return Offset_Within_Area ((Size_In_Bits + (Word_Type'Size - 1)) /
                                 Word_Type'Size);
   end To_Size_In_Words;

   ------------------------
   -- To_Virtual_Address --
   ------------------------

   function To_Virtual_Address
     (Address : Object_Address) return Object_Virtual_Address is
   begin
      if Address.Enclosing_Chunk = null then

         --  Null chunk used for null address, and for "absolute" address

         if Address.Offset = 0 then
            return Null_Virtual_Address;
         else
            --  Absolute address
            return Object_Virtual_Address (0) + Address.Offset;
         end if;
      else
         return Address.Enclosing_Chunk.Starting_Virtual_Address +
                Address.Offset;
      end if;
   end To_Virtual_Address;

   -------------------------- Local protected bodies -------------------------

   --------------------
   -- Thread_Manager --
   --------------------

   protected body Thread_Manager is
      --  This protected object manages the creating and
      --  serving of "pico" threads.
      --  TBD: Finer-grained locking might be useful someday.

      --  Suppress warnings about use of globals in barriers
      pragma Warnings (Off, "potentially unsynchronized barrier");

      ---------- Local operations -----------

      procedure Add_To_Shared_Deque
        (Server_Index        : Thread_Server_Index;
         New_Tcb             : Word_Ptr);
      --  Add New_Tcb to appropriate shared deque, and bump various
      --  counts of number of waiting shared threads, etc.

      procedure Flush_Master
        (Master       : Word_Ptr;
         Server_Index : Thread_Server_Index);
      --  Remove all subthreads from their queues

      procedure Remove_Tcb
        (Tcb_To_Be_Removed : Word_Ptr;
         Server_Index      : Thread_Server_Index;
         Category          : Thread_Category);
      --  Remove thread from appropriate deque

      procedure Steal_Thread
        (Server_Index        : Thread_Server_Index;
         Holding_Lock_Obj    : Lock_Obj_Index;
         Stolen_Tcb          : out Word_Ptr;
         Subthread_Of_Master : Master_Index := 0);
      --  Steal from some other server's queue, but steal
      --  the "oldest" rather than the "youngest" thread.
      --  If Holding_Lock_Obj > 0, then only look at threads
      --  that are holding the same lock.

      procedure Update_Innermost_Shared_Master
        (Start_With_Master       : Master_Index;
         Innermost_Shared_Master : Master_Index);
      --  Update Innermost_Shared_Master in the master info for the
      --  given "Start_With" master, and all of its submasters which are
      --  not already so marked.

      function Thread_Is_Holding_Lock
        (Waiting_Tcb : Word_Ptr;
         Lock        : Lock_Obj_Index) return Boolean;
      --  Return True if given thread is holding the given lock,
      --  somewhere in its chain of held locks.

      -------------------------
      -- Add_To_Shared_Deque --
      -------------------------

      procedure Add_To_Shared_Deque
        (Server_Index : Thread_Server_Index;
         New_Tcb      : Word_Ptr) is
      --  Add New_Tcb to appropriate shared deque, and bump various
      --  counts of number of waiting shared threads, etc.

         Lock_Held_By_Tcb : constant Lock_Obj_Index :=
           Large_Obj_Lock_Obj (New_Tcb);
         Lock_Is_Held     : constant Boolean :=
           Lock_Held_By_Tcb > 0;
         Category         : constant Thread_Category :=
           Category_Table
             (Lock_Is_Held, Tcb_Uses_Queuing (New_Tcb));
      begin
         --  Add to end of chain
         Add_To_Deque
           (Server_Info_Array (Server_Index).Shared_Threads (Category),
            New_Tcb,
            Is_Shared => True);

         --  One more waiting thread
         Num_Waiting_Shared_Threads := Num_Waiting_Shared_Threads + 1;

         if not Tcb_Uses_Queuing (New_Tcb) then
            --  And one more that doesn't use queuing
            Num_Nonqueuing_Threads := Num_Nonqueuing_Threads + 1;
         end if;

         declare
            Lock_Held : Lock_Obj_Index := Lock_Held_By_Tcb;
         begin
            --  Increment Num_Lock_Subordinates for Lock_Held
            --  and all enclosing locks.
            loop
               Num_Lock_Subordinates (Lock_Held) :=
                 Num_Lock_Subordinates (Lock_Held) + 1;
               exit when Lock_Held = 0
                        or else Num_Lock_Subordinates (Lock_Held) > 1;
               Lock_Held := Enclosing_Lock (Lock_Held);
            end loop;
         end;

         declare
            Ancestor : Master_Index :=
              Index_Of_Master (Tcb_Master_Ptr (New_Tcb));
         begin
            --  Increment Num_Master_Subordinates for Master and all enclosing
            --  masters.
            loop
               Master_Extras (Ancestor).Num_Subordinates :=
                 Master_Extras (Ancestor).Num_Subordinates + 1;
               exit when Ancestor = 0
                        or else Master_Extras (Ancestor).Num_Subordinates > 1;
               Ancestor := Master_Extras (Ancestor).Enclosing_Master;
            end loop;
         end;

         --  Accumulate statistics
         if Num_Waiting_Shared_Threads > Max_Waiting_Shared_Threads then
            Max_Waiting_Shared_Threads := Num_Waiting_Shared_Threads;
            if Debug_Threading then
               Put_Line
                 ("Add_To_Shared_Deque -- new Max_Waiting_Shared_Threads =" &
                  Thread_Count'Image (Max_Waiting_Shared_Threads));
            end if;
         end if;

      end Add_To_Shared_Deque;

      ------------------
      -- Flush_Master --
      ------------------

      procedure Flush_Master
        (Master       : Word_Ptr;
         Server_Index : Thread_Server_Index)
      is
         use Thread_Manager_Data;
         Subthread : Word_Ptr := First_Subthread (Master);
         Master_Extra : Master_Extra_Rec renames
           Master_Extras (Index_Of_Master (Master));
      begin
         --  Remove all of the threads on the master from server deques.
         while Subthread /= null loop
            if not Master_Extra.Master_Is_Shared then
               --  Unshared master, remove from unshared deque.
               Remove_From_Deque
                 (Server_Info_Array (Server_Index).Unshared_Threads,
                  Subthread);
            else
               --  Shared master, remove from appropriate category deque.
               Remove_Tcb
                 (Subthread,
                  Server_Index,
                  Category_Table
                    (Large_Obj_Lock_Obj (Subthread) > 0,
                     Tcb_Uses_Queuing (Subthread)));
            end if;
            Subthread := Next_Subthread (Subthread);
         end loop;
      end Flush_Master;

      ----------------
      -- Remove_Tcb --
      ----------------

      procedure Remove_Tcb
        (Tcb_To_Be_Removed : Word_Ptr;
         Server_Index      : Thread_Server_Index;
         Category          : Thread_Category)
      is
         --  Remove thread from appropriate deque
      begin
         Remove_From_Deque
           (Deque             =>
              Server_Info_Array (Server_Index).Shared_Threads (Category),
            Tcb_To_Be_Removed => Tcb_To_Be_Removed);
      end Remove_Tcb;

      ---------------------------
      -- Return_Waiting_Thread --
      ---------------------------

      procedure Return_Waiting_Thread
        (Server_Index        : Thread_Server_Index;
         Holding_Lock_Obj    : Lock_Obj_Index;
         Tcb_To_Run          : out Word_Ptr;
         Subthread_Of_Master : Master_Index := 0)
      is
         use Thread_Manager_Data;
         pragma Assert (Num_Lock_Subordinates (Holding_Lock_Obj) > 0);

         Last_Category : constant Thread_Category :=
                           Last_Category_Table (Holding_Lock_Obj > 0);
         Info          : Server_Info renames Server_Info_Array (Server_Index);
      begin

         Tcb_To_Run := null;
            --  Avoid warning about potentially uninitialized OUT parameter

         for Category in Thread_Category'First .. Last_Category loop
            --  Look at the queues in order,
            --  first looking for TCBs holding a lock.

            Tcb_To_Run := Info.Shared_Threads (Category).Last_Thread;

            if Subthread_Of_Master > 0
              and then Category = Queuing_Thread
            then
               --  Keep looking for TCB that is subthread of given master
               while Tcb_To_Run /= null
                 and then not Is_Subthread (Tcb_To_Run, Subthread_Of_Master)
               loop
                  Tcb_To_Run := Prev_Waiting_Tcb (Tcb_To_Run);
               end loop;

            elsif Holding_Lock_Obj > 0 then
               --  Scan looking for TCB holding the correct lock
               while Tcb_To_Run /= null
                 and then not Thread_Is_Holding_Lock
                                (Tcb_To_Run,
                                 Holding_Lock_Obj)
               loop
                  Tcb_To_Run := Prev_Waiting_Tcb (Tcb_To_Run);
               end loop;

            end if;

            if Tcb_To_Run /= null then
               --  Remove Tcb_To_Run from the server's deque
               Remove_Tcb (Tcb_To_Run, Server_Index, Category);

               exit;
            end if;
         end loop;

         if Tcb_To_Run = null then
            --  Steal from some other server's queue, but steal
            --  the "oldest" rather than the "youngest" thread.
            Steal_Thread
              (Server_Index,
               Holding_Lock_Obj,
               Tcb_To_Run,
               Subthread_Of_Master => Subthread_Of_Master);

            if Tcb_To_Run = null then
               --  TBD: This shouldn't happen
               return;
            end if;

            --  Statistics
            Num_Thread_Steals := Num_Thread_Steals + 1;
         end if;

         --  One less waiting thread
         Num_Waiting_Shared_Threads := Num_Waiting_Shared_Threads - 1;

         if not Tcb_Uses_Queuing (Tcb_To_Run) then
            --  And one less that doesn't use queuing
            Num_Nonqueuing_Threads := Num_Nonqueuing_Threads - 1;
         end if;

         declare
            Lock_Held : Lock_Obj_Index := Large_Obj_Lock_Obj (Tcb_To_Run);
         begin
            --  Decrement Num_Lock_Subordinates for Lock_Held
            --  and all enclosing locks.
            loop
               Num_Lock_Subordinates (Lock_Held) :=
                 Num_Lock_Subordinates (Lock_Held) - 1;
               exit when Lock_Held = 0
                        or else Num_Lock_Subordinates (Lock_Held) > 0;
               Lock_Held := Enclosing_Lock (Lock_Held);
            end loop;
         end;

         declare
            Ancestor       : Master_Index :=
              Index_Of_Master (Tcb_Master_Ptr (Tcb_To_Run));
         begin
            --  Decrement Num_Subordinates for master of TCB
            --  and all enclosing masters.
            loop
               Master_Extras (Ancestor).Num_Subordinates :=
                 Master_Extras (Ancestor).Num_Subordinates - 1;
               exit when Ancestor = 0
                        or else Master_Extras (Ancestor).Num_Subordinates > 0;
               Ancestor := Master_Extras (Ancestor).Enclosing_Master;
            end loop;
         end;

         --  Remember most recent active thread for this server
         Info.Last_Active_Thread := Tcb_To_Run;
         --  NOTE: This is mostly for debugging

         if Debug_Threading then
            declare
               Thread_Master : constant Word_Ptr :=
                 Tcb_Master_Ptr (Tcb_To_Run);
               Index : constant Master_Index :=
                 Index_Of_Master (Thread_Master);
            begin
               Put_Line
                 ("Get_Thread for TCB at " &
                  Hex_Image (Tcb_To_Run) &
                  ", master" &
                  Master_Index'Image (Index));

               if Index <= 0 then
                  Put_Line ("Get_Thread -- Master_Index <= 0");
                  raise Program_Error;
               end if;
            end;
         end if;

         --  Indicate no longer on server queue
         Set_Tcb_State (Tcb_To_Run, Unknown_State);

         --  Accumulate statistics
         Num_Active := Integer'Max (0,
           Integer (Last_Server_Index - Main_Thread_Server_Index + 1)
             - Get_Thread'Count - Num_Servers_Waiting_For_Masters);

         if Num_Active > Max_Active then
            Max_Active := Num_Active;
            if Debug_Threading then
               Put_Line
                 ("Get_Thread -- new Max_Active =" &
                  Natural'Image (Max_Active));
            end if;
         end if;

         Num_Shared_Thread_Initiations := Num_Shared_Thread_Initiations + 1;

         if Num_Active > 0 then
            Num_Active_Summed_Over_Initiations :=
              Num_Active_Summed_Over_Initiations +
                Longest_Natural (Num_Active);
         end if;

         if Num_Waiting_Shared_Threads > 0 then
            Num_Waiting_Shared_Summed :=
              Num_Waiting_Shared_Summed +
                Longest_Natural (Num_Waiting_Shared_Threads);
         end if;
      end Return_Waiting_Thread;

      ------------------
      -- Steal_Thread --
      ------------------

      procedure Steal_Thread
        (Server_Index        : Thread_Server_Index;
         Holding_Lock_Obj    : Lock_Obj_Index;
         Stolen_Tcb          : out Word_Ptr;
         Subthread_Of_Master : Master_Index := 0)
      is
         --  Steal from some other server's queue, but steal
         --  the "oldest" rather than the "youngest" thread.
         --  If Holding_Lock_Obj > 0, then only look at threads
         --  that are holding the same lock.
         use Thread_Manager_Data;
         pragma Assert (Num_Lock_Subordinates (Holding_Lock_Obj) > 0);

         Last_Category : constant Thread_Category :=
           Last_Category_Table (Holding_Lock_Obj > 0);

         Num_Active_Servers : constant Thread_Server_Index :=
           Last_Server_Index - Main_Thread_Server_Index + 1;
      begin
         for I in 1 .. Num_Active_Servers - 1 loop
            declare
               Other_Server : constant Thread_Server_Index :=
                 (Server_Index + I - Main_Thread_Server_Index) mod
                   Num_Active_Servers + Main_Thread_Server_Index;
            --  Choose the "next" server with a waiting thread
            begin

               for Category in Thread_Category'First .. Last_Category loop
                  --  Try all categories that are applicable.
                  --  Remove oldest thread from queue, if any
                  Stolen_Tcb :=
                    Server_Info_Array (Other_Server).
                      Shared_Threads (Category).First_Thread;

                  if Subthread_Of_Master > 0
                    and then Category = Queuing_Thread
                  then
                     --  Keep looking for TCB that is subthread of master
                     while Stolen_Tcb /= null
                       and then not Is_Subthread
                                      (Stolen_Tcb,
                                       Subthread_Of_Master)
                     loop
                        Stolen_Tcb := Next_Waiting_Tcb (Stolen_Tcb);
                     end loop;

                  elsif Holding_Lock_Obj > 0 then
                     --  Check whether it is holding proper lock
                     while Stolen_Tcb /= null
                       and then not Thread_Is_Holding_Lock
                                      (Stolen_Tcb,
                                       Holding_Lock_Obj)
                     loop

                        --  Keep looking for thread holding proper lock
                        Stolen_Tcb := Next_Waiting_Tcb (Stolen_Tcb);
                     end loop;
                  end if;

                  if Stolen_Tcb /= null then
                     --  We found a server with a waiting thread
                     --  Unlink thread from queue

                     Remove_Tcb (Stolen_Tcb, Other_Server, Category);

                     if Debug_Threading then
                        Put
                          ("Steal_Thread from server" &
                           Thread_Server_Index'Image (Other_Server) &
                           " for server" &
                           Thread_Server_Index'Image (Server_Index) &
                           ", Holding_Lock_Obj =" &
                           Lock_Obj_Index'Image (Holding_Lock_Obj));
                        if Subthread_Of_Master > 0 then
                           Put_Line (", Subthread_Of_Master =" &
                             Master_Index'Image (Subthread_Of_Master));
                        else
                           New_Line;
                        end if;
                     end if;

                     return;   -- All done --

                  end if;

               end loop;
            end;
         end loop;

         Messages.Put_RT_Error
           ("Internal: Steal_Thread found no threads to steal" &
            ", Holding_Lock_Obj = " &
            Lock_Obj_Index'Image (Holding_Lock_Obj) &
            ", Subthread_Of_Master = " &
            Master_Index'Image (Subthread_Of_Master),
            Src_Pos => Source_Positions.Null_Source_Position);
         --  There was supposed to be at least one waiting thread
         --  TBD: What if none of the threads has the right lock?

         if Debug_Threading then
            Dump_Masters;
            Thread_Manager.Dump_Locks;
            Put_Line (" Current thread tree in Steal_Thread:");
            Dump_Thread_Tree (Label =>
              "Current thread tree in Steal_Thread");
         end if;

         Stolen_Tcb := null;
      end Steal_Thread;

      ----------------------------
      -- Thread_Is_Holding_Lock --
      ----------------------------

      function Thread_Is_Holding_Lock
        (Waiting_Tcb : Word_Ptr;
         Lock        : Lock_Obj_Index) return Boolean
      is
         use Thread_Manager_Data;
         Lock_By_Tcb : Lock_Obj_Index := Large_Obj_Lock_Obj (Waiting_Tcb);
      begin
         if Lock = 0 then
            --  Any thread will do
            return True;
         end if;

         while Lock_By_Tcb /= 0 loop
            if Lock_By_Tcb = Lock then
               --  We have a match
               return True;
            end if;

            --  Check enclosing lock
            Lock_By_Tcb := Enclosing_Lock (Lock_By_Tcb);
         end loop;

         --  Thread not holding given lock
         return False;
      end Thread_Is_Holding_Lock;

      ------------------------------------
      -- Update_Innermost_Shared_Master --
      ------------------------------------

      procedure Update_Innermost_Shared_Master
        (Start_With_Master       : Master_Index;
         Innermost_Shared_Master : Master_Index) is
      --  Update Innermost_Shared_Master in the master info for the
      --  given "Start_With" master, and all of its submasters which are
      --  not already so marked.
         Starter_Extra : Master_Extra_Rec renames
           Master_Extras (Start_With_Master);
      begin
         if Starter_Extra.Innermost_Shared_Master /=
           Innermost_Shared_Master
         then
            if Debug_Threading then
               Put_Line (" Update master" &
                 Master_Index'Image (Start_With_Master) &
                 " to refer to shared master" &
                 Master_Index'Image (Innermost_Shared_Master));
            end if;

            --  Does not yet have correct innermost shared master
            declare
               Starter_Addr : constant Word_Ptr :=
                 Master_Extras (Start_With_Master).Master_Address;
               Subthread    : Word_Ptr :=
                 First_Subthread (Starter_Addr);
            begin
               --  Update innermost-shared-master indicator
               Starter_Extra.Innermost_Shared_Master :=
                 Innermost_Shared_Master;

               --  Now propagate to all submasters
               while Subthread /= null loop
                  declare
                     Submaster : Master_Index :=
                       Index_Of_Master (Tcb_Master_Ptr (Subthread));
                  begin
                     --  See whether this thread has any submasters
                     while Submaster /= Start_With_Master
                       and then Submaster /= 0
                     loop
                        declare
                           Submaster_Extra : Master_Extra_Rec
                             renames Master_Extras (Submaster);
                        begin
                           if not Submaster_Extra.Master_Is_Shared then
                              --  Submaster is not itself shared, so update
                              --  its innermost shared master indicator.
                              Update_Innermost_Shared_Master
                                (Submaster, Innermost_Shared_Master);
                           end if;

                           --  Continue with next submaster, if any
                           Submaster := Submaster_Extra.Enclosing_Master;
                        end;
                     end loop;

                     --  Now move on to next subthread
                     Subthread := Next_Subthread (Subthread);
                  end;
               end loop;
            end;
         end if;
      end Update_Innermost_Shared_Master;

      function Master_Is_Ready (Server_Index : Thread_Server_Index)
        return Boolean is
      --  The entry barrier for Get_Thread_Or_Wait_For_Threads_Internal.
      --  Return True if master associated with given server is ready
      --  to be served, either because all of its subthreads are done,
      --  or because there is a ready thread that can safely be run on the same
      --  server that is waiting for the given master.
         Master_Extra : Master_Extra_Rec renames
           Server_Info_Array (Server_Index).Cur_Master_Extra.all;
      begin
         if Solo_Server /= 0 and then Solo_Server /= Server_Index then
            --  We are supposed to pause to let Solo_Server do its thing
            --  (e.g. support an interactive debugging console).
            return False;
         end if;

         return Master_Extra.Subthread_Count = 0
           or else ((Get_Thread'Count = 0 or else Solo_Server /= 0)
            and then
              Num_Lock_Subordinates
                (Master_Extra.Lock_Held) > 0
            and then
              (Master_Extra.Lock_Held > 0
               or else Num_Nonqueuing_Threads > 0
               or else Master_Extra.Num_Subordinates > 0));
      end Master_Is_Ready;

      entry Wait_For_End_Of_Pause (Server_Index : Thread_Server_Index)
        when Solo_Server = 0 or else Is_Shut_Down is
      --  Wait until Solo_Server is 0, and then requeue back on
      --  Pause_Other_Servers
      begin
         if not Is_Shut_Down then
            --  Try again
            requeue Pause_Other_Servers;
         end if;
      end Wait_For_End_Of_Pause;

      entry Wait_For_Other_Servers_To_Pause
        (Server_Index : Thread_Server_Index)
      --  Wait for other servers to pause; return when they have all stopped
      --  servicing their queues.
        when Is_Shut_Down
          or else
            Thread_Server_Index_Base (Get_Thread'Count +
            Wait_For_End_Of_Pause'Count +
            Num_Servers_Waiting_For_Masters) >=
              Last_Server_Index - Main_Thread_Server_Index
      is
      begin
         if not Is_Shut_Down then
            if Debug_Threading then
               Put_Line ("All but server" &
                 Thread_Server_Index'Image (Server_Index) & " have paused.");
            end if;
            Delay_Queue.Pause_Delay_Queue;  --  Now stop the delay queue
         end if;
      end Wait_For_Other_Servers_To_Pause;

      ------------ visible operations -----------------

      procedure Next_Lock_Obj (Lock_Obj : out Lock_Obj_Index) is
         use Thread_Manager_Data;
      begin
         --  Need to allocate a new lock obj
         Last_Lock_Obj := Last_Lock_Obj + 1;
         Lock_Obj := Last_Lock_Obj;
         Lock_Obj_Limit := Last_Lock_Obj;  --  For debugging
         Lock_Obj_Table (Lock_Obj).Index := Lock_Obj;
         Lock_Obj_Table (Lock_Obj).Lock := new Object_Lock;
         if Debug_Threading then
            Put_Line
              (" Allocating lock #" & Lock_Obj_Index'Image (Lock_Obj));
            Flush;
         end if;
      end Next_Lock_Obj;

      -------------------
      -- Finish_Thread --
      -------------------

      procedure Finish_Thread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr)
      is
         use Thread_Manager_Data;
         Thread_Master : constant Word_Ptr :=
                           Tcb_Master_Ptr (Finished_Tcb);
         Index     : constant Master_Index  := Index_Of_Master (Thread_Master);
      begin
         --  Hand off to common code for finishing a subthread
         Finish_Subthread (Server_Index, Finished_Tcb);

         if Debug_Threading then
            Put_Line
              ("Finish_Thread for TCB at " &
               Hex_Image (Finished_Tcb) &
               ", server" & Thread_Server_Index'Image (Server_Index) &
               ", master" & Master_Index'Image (Index) &
               ", count now " &
               Thread_Count'Image (Master_Extras (Index).Subthread_Count) &
               ", Num_Active now " &
               Natural'Image (Num_Active) &
               ", Num_Shared_Threads_Needed =" &
               Thread_Count'Image (Num_Shared_Threads_Needed));
            Flush;
         end if;
      exception
         when Storage_Error =>
            --  Not much to do here
            raise;

         when E : others =>
            Messages.Put_RT_Error
              ("Finish_Thread: " &
               Ada.Exceptions.Exception_Name (E) &
               " raised.",
               Src_Pos => Execution_Source_Pos (Server_Index));
      end Finish_Thread;

      -----------------------
      -- Next_Master_Index --
      -----------------------

      procedure Next_Master_Index (Index : out Master_Index) is
      --  Return next unique index for a master
      begin
         --  Assign a new master index
         Last_Master := Last_Master + 1;
         Index := Last_Master;
      end Next_Master_Index;

      ----------------------
      -- Get_Server_Index --
      ----------------------

      procedure Get_Server_Index (Index : out Thread_Server_Index) is
         use Thread_Manager_Data;
      begin
         Last_Server_Index := Last_Server_Index + 1;
         Index := Last_Server_Index;
      end Get_Server_Index;

      ----------------
      -- Get_Thread --
      ----------------

      entry Get_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr)
         when Is_Shut_Down
           or else
             (Solo_Server = 0
                and then
              (Num_Waiting_Shared_Threads > 0
                or else Thread_Manager_Data.Num_Shared_Threads_Needed = 0))
      is
         use Thread_Manager_Data;
      begin
         if Is_Shut_Down then
            --  Indicate server should shut itself down
            Tcb_To_Run := null;
            return;
         end if;

         if Num_Waiting_Shared_Threads = 0
           and then Num_Shared_Threads_Needed = 0
         then
            --  Indicate we need at least one shared thread
            Num_Shared_Threads_Needed := Thread_Count
              (Get_Thread'Count + Num_Servers_Waiting_For_Masters + 1);

            --  Now try again
            requeue Get_Thread with abort;
         end if;

         --  Get first thread from queue
         Return_Waiting_Thread
           (Server_Index,
            Holding_Lock_Obj => 0,
            Tcb_To_Run => Tcb_To_Run);

      exception
         when E : others =>
            Messages.Put_RT_Error
              ("Get_Thread: " &
               Ada.Exceptions.Exception_Name (E) &
               " raised.",
               Src_Pos => Execution_Source_Pos (Server_Index));
      end Get_Thread;

      ------------------------------------
      -- Get_Thread_Or_Wait_For_Threads --
      ------------------------------------

      entry Get_Thread_Or_Wait_For_Threads
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Tcb_Waiting      : Word_Ptr;
         Tcb_To_Run       : out Word_Ptr) when True
      is
         use Thread_Manager_Data;

         --  Simply requeue on appropriate member of entry family
         Index : constant Master_Index := Index_Of_Master (Thread_Master);
         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
         Info : Server_Info renames Server_Info_Array (Server_Index);
      begin
         if Debug_Threading then
            Put_Line
              ("Get_Thread_Or_Wait_For_Threads for server" &
               Thread_Server_Index'Image (Server_Index) & ", master" &
               Master_Index'Image (Index) &
               ", count now " &
               Thread_Count'Image (Master_Extra.Subthread_Count));
            Flush;
         end if;

         if Master_Extra.Subthread_Count = 0 then
            --  Master is already complete; no need for a requeue.
            Tcb_To_Run := null;

            --  Statistics
            Num_Active_Masters := Num_Active_Masters - 1;
            if Master_Extra.Is_Being_Awaited then
               Num_Waiting_For_Subthreads :=
                 Num_Waiting_For_Subthreads - 1;
            end if;

            return;  --  Return now  --
         end if;

         if Master_Extra.Master_Is_Shared then
            --  If master is unshared, we already checked for
            --  unshared threads before calling this entry.
            --  However, if master is shared, we call this entry without
            --  first checking for unshared threads, so we do that now.
            Get_Unshared_Thread (Server_Index, Tcb_To_Run,
              Subthread_Of_Master => Index);

            if Tcb_To_Run /= null then
               return;  --  Return now  --
            end if;
         end if;

         --  Statistics
         if not Master_Extra.Is_Being_Awaited then
            --  This is the first time we are waiting for this master
            Master_Extra.Is_Being_Awaited := True;
            Num_Waiting_For_Subthreads := Num_Waiting_For_Subthreads + 1;
            if Num_Waiting_For_Subthreads > Max_Waiting_For_Subthreads then
               Max_Waiting_For_Subthreads := Num_Waiting_For_Subthreads;
               if Debug_Threading then
                  Put_Line
                    ("Get_Thread_Or_Wait_For_Threads -- " &
                     "new Max_Waiting_For_Subthreads =" &
                     Natural'Image (Max_Waiting_For_Subthreads));
                  Flush;
               end if;
            end if;
         end if;

         --  Keep a count of number of waiters
         --  NOTE: This could become wrong if an entry call were interrupted,
         --        but currently we don't interrupt entry calls, we instead
         --        change something which causes their barrier to become true.
         Num_Servers_Waiting_For_Masters :=
           Num_Servers_Waiting_For_Masters + 1;

         if Num_Servers_Waiting_For_Masters > Max_Servers_Waiting_For_Masters
         then
            --  We have a new "max"
            Max_Servers_Waiting_For_Masters := Num_Servers_Waiting_For_Masters;
         end if;

         if Num_Shared_Threads_Needed = 0
           and then
             Num_Waiting_Shared_Threads <
              Thread_Count (Num_Servers_Waiting_For_Masters + Get_Thread'Count)
         then
            --  We need at least one shared thread to serve for each waiter.
            Num_Shared_Threads_Needed := Thread_Count'Max (1, Thread_Count
              (Get_Thread'Count + Num_Servers_Waiting_For_Masters) -
                Num_Waiting_Shared_Threads);
         end if;

         --  Verify that Owned_By_Server is properly initialized
         pragma Assert (Master_Extra.Owned_By_Server = Server_Index);

         --  Set current master in Server Info
         Info.Cur_Master_Extra := Master_Extra'Access;

         --  Do the requeue
         requeue Get_Thread_Or_Wait_For_Threads_Internal
           (Server_Index) with abort;
      end Get_Thread_Or_Wait_For_Threads;

      ---------------------------------------------
      -- Get_Thread_Or_Wait_For_Threads_Internal --
      ---------------------------------------------

      entry Get_Thread_Or_Wait_For_Threads_Internal
        (for Serv_Index in Thread_Server_Index)
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Tcb_Waiting      : Word_Ptr;
         Tcb_To_Run       : out Word_Ptr)
      --  Wait for threads of given server's current thread master.
      --  if not done, get a thread to serve;
      --  if done, set Tcb_To_Run to null.

        when Master_Is_Ready (Serv_Index)
      is
         use Thread_Manager_Data;
         Index : constant Master_Index := Index_Of_Master (Thread_Master);
         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
      begin
         --  Decrement count of servers waiting on this entry family.
         Num_Servers_Waiting_For_Masters :=
           Num_Servers_Waiting_For_Masters - 1;

         --  Null out Cur_Master_Extra (to prevent dangling refs)
         Server_Info_Array (Serv_Index).Cur_Master_Extra := null;

         if Master_Extra.Subthread_Count /= 0 then
         --  Get a waiting thread and return
         --  TBD: Would be better to find thread
         --      associated with this master.
            begin
               Return_Waiting_Thread
                 (Server_Index,
                  Holding_Lock_Obj,
                  Tcb_To_Run,
                  Subthread_Of_Master => Index);

               if Tcb_To_Run = null
                 and then Master_Extra.Subthread_Count /= 0
               then
                  --  Trouble; no thread found.  Need to flush the master.
                  Messages.Put_RT_Error
                    ("Internal: No thread found by " &
                     "Return_Waiting_Thread -- flushing master.",
                     Src_Pos => Execution_Source_Pos (Server_Index));

                  Flush_Master (Thread_Master, Server_Index);
               end if;

               return;
            end;
         end if;

         --  All threads of master now complete.

         if Index /= 0 then

            --  Statistics
            Num_Waiting_For_Subthreads := Num_Waiting_For_Subthreads - 1;

            Num_Active_Masters := Num_Active_Masters - 1;

            if Debug_Threading or Debug_Kill then
               Put_Line
                 ("Get_Thread_Or_Wait_For_Threads_Internal for server" &
                  Thread_Server_Index'Image (Server_Index) &
                  ", master" &
                  Master_Index'Image (Index) &
                  " is complete, Num_Active_Masters now " &
                  Natural'Image (Num_Active_Masters));

               if Debug_Threading then
                  Dump_Masters;
                  Thread_Manager.Dump_Locks;
                  Dump_Thread_Tree (Label =>
                    "Current thread tree in GTOWFTI");
               end if;
            end if;
         end if;

         --  Indicate that thread master is complete
         Tcb_To_Run := null;

      end Get_Thread_Or_Wait_For_Threads_Internal;

      -----------------------------
      -- Increase_Shared_Threads --
      -----------------------------

      procedure Increase_Shared_Threads
        (Server_Index : Thread_Server_Index) is
      --  Attempt to move threads from server's unshared deque
      --  to the shared deque of threads.
         use Thread_Manager_Data;
         Num_Needed : Thread_Count := Num_Shared_Threads_Needed;
         Info : Server_Info renames Server_Info_Array (Server_Index);
      begin
         --  Set to zero now to prevent additional calls from other servers
         Num_Shared_Threads_Needed := 0;

         while Num_Needed > 0
           and then Info.Unshared_Threads.Count > 0
         loop
            --  Move oldest unshared thread to the shared thread queue
            --  while marking the master (and all of its enclosing masters)
            --  as shared (if not already so marked).
            --  Any other threads associated with the same master must also
            --  be moved to the shared queue to avoid synchronization issues.
            declare
               Tcb_To_Move   : Word_Ptr :=
                 Info.Unshared_Threads.First_Thread; --  Oldest unshared thread

               Master_Of_TCB : constant Master_Index :=
                 Index_Of_Master (Tcb_Master_Ptr (Tcb_To_Move));
               Encloser      : Master_Index := Master_Of_TCB;
            begin
               --  Mark all enclosing masters as shared.
               while Encloser /= 0 loop
                  declare
                     Encloser_Extra : Master_Extra_Rec renames
                       Master_Extras (Encloser);
                  begin
                     exit when Encloser_Extra.Master_Is_Shared
                       or else Encloser_Extra.Master_Never_Shared;
                        --  exit if already marked shared, or if not
                        --  allowed to be marked shared.

                     if Debug_Threading then
                        Put_Line (" Increase_Shared_Threads: marking master" &
                          Master_Index'Image (Encloser) & " as shared");
                     end if;

                     --  Mark encloser as shared.
                     Encloser_Extra.Master_Is_Shared := True;

                     --  Update indicator of innermost shared master
                     Update_Innermost_Shared_Master
                       (Start_With_Master       => Encloser,
                        Innermost_Shared_Master => Encloser);

                     --  Continue walking up the chain of masters
                     Encloser := Encloser_Extra.Enclosing_Master;
                  end;
               end loop;

               --  Now move oldest thread, and any other thread on the unshared
               --  deque that is associated with a newly shared master,
               --  to the appropriate shared deque.
               while Tcb_To_Move /= null loop
                  declare
                     Next_Oldest      : constant Word_Ptr :=
                       Next_Waiting_Tcb (Tcb_To_Move);
                  begin
                     if Debug_Threading then
                        Put_Line (" Increase_Shared_Threads: moving tcb " &
                          Hex_Image (Tcb_To_Move) & " to shared deque");
                     end if;

                     --  Remove tcb from unshared deque
                     Remove_From_Deque (Info.Unshared_Threads, Tcb_To_Move);

                     --  Add to end of proper shared deque
                     Add_To_Shared_Deque (Server_Index, Tcb_To_Move);

                     --  See whether next oldest thread is also on a shared
                     --  master.
                     --  NOTE: A nested "continue" statement could cause
                     --        a thread to be spawned on an enclosing master.

                     Tcb_To_Move := Next_Oldest;

                     --  Skip over subthreads of non-shared masters
                     while Tcb_To_Move /= null
                       and then not Master_Extras (Index_Of_Master
                          (Tcb_Master_Ptr (Tcb_To_Move))).Master_Is_Shared
                     loop
                        --  Skip past thread of an unshared master
                        Tcb_To_Move := Next_Waiting_Tcb (Tcb_To_Move);
                     end loop;

                     --  Tcb_To_Move is null, or another thread to be moved
                  end;
               end loop;

               --  Add_To_Shared_Deque bumped Num_Waiting_Shared_Threads so
               --  now we recompute number of shared threads needed.
               Num_Needed :=
                 Thread_Count'Max (0, Thread_Count
                   (Get_Thread'Count + Num_Servers_Waiting_For_Masters) -
                     Num_Waiting_Shared_Threads);
            end;
         end loop;

         --  Now update the global
         Num_Shared_Threads_Needed := Num_Needed;
      end Increase_Shared_Threads;

      ---------------------
      -- Prepare_To_Exit --
      ---------------------

      entry Prepare_To_Exit
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Exiting_Tcb      : Word_Ptr;
         Succeeded        : out Boolean;
         Raising_Excep    : Boolean := False) when True
      --  Prepare to exit given master.
      --  Set Succeeded to indicate whether the prepare-to-exit
      --  succeeded.
      is
         use Thread_Manager_Data;
         Target_Master_Index : constant Master_Index :=
                                 Index_Of_Master (Thread_Master);
         Encloser            : Master_Index :=
                                 Index_Of_Master
                                   (Tcb_Master_Ptr (Exiting_Tcb));
         Target_Master_Extra : Master_Extra_Rec renames
                                 Master_Extras (Target_Master_Index);
         Lock_Held_By_Target : constant Lock_Obj_Index :=
                                 Target_Master_Extra.Lock_Held;

         procedure Mark_Inside_Lock
           (Inner_Lock : Lock_Obj_Index;
            Outer_Lock : Lock_Obj_Index);
         --  Set Lock_Exit_Requested on lock just inside of Outer_Lock

         procedure Kill_Thread_Tree
           (Inner_Master : Master_Index;
            Outer_Master : Master_Index;
            Quit_If_Master_Already_Exiting : Boolean := False);
         --  Request exit for all subthreads of Inner_Master
         --  and any masters enclosing Inner_Master up to,
         --  but not including, Outer_Master.
         --  Don't touch "Exiting_Tcb" itself.
         --  If thread is waiting on a queued call because its
         --  dequeue condition is not satisfied, wake it up.
         --  If thread is holding a lock that is not held by
         --  its master, then do *not* touch it, though somehow
         --  mark it so that when it comes out of the locked
         --  call, it will exit.  Or we mark the lock object
         --  itself with such an indicator.
         --  If Quit_If_Master_Already_Exiting is True,
         --  then set Succeeded to False and return early
         --  if Master_Exit_Requested already true for one
         --  of the masters in the chain.

         ----------------------
         -- Mark_Inside_Lock --
         ----------------------

         procedure Mark_Inside_Lock
           (Inner_Lock : Lock_Obj_Index;
            Outer_Lock : Lock_Obj_Index)
         is
            pragma Assert (Inner_Lock /= Outer_Lock);
            Inside_Lock : Lock_Obj_Index := Inner_Lock;
         begin
            while Enclosing_Lock (Inside_Lock) /= Outer_Lock loop
               --  Look for lock immediately inside Outer_Lock
               Inside_Lock := Enclosing_Lock (Inside_Lock);
               pragma Assert (Inside_Lock /= 0);
            end loop;

            Lock_Exit_Requested (Inside_Lock) := True;
         end Mark_Inside_Lock;

         ----------------------
         -- Kill_Thread_Tree --
         ----------------------

         procedure Kill_Thread_Tree
           (Inner_Master : Master_Index;
            Outer_Master : Master_Index;
            Quit_If_Master_Already_Exiting : Boolean := False)
         is
            Inner : Master_Index := Inner_Master;
         begin
            while Inner /= Outer_Master loop
               declare
                  Inner_Extra : Master_Extra_Rec renames Master_Extras (Inner);
                  Inner_Addr  : constant Word_Ptr :=
                    Inner_Extra.Master_Address;
                  Subthread : Word_Ptr;
               begin
                  if Master_Exit_Requested (Inner_Addr) then
                     --  This master has already been exited
                     if Quit_If_Master_Already_Exiting then
                        --  Indicate some other thread has already
                        --  requested an exit.
                        Succeeded := False;

                        return;  --  Quit early  --

                     end if;

                  elsif Inner_Extra.Lock_Held /= Lock_Held_By_Target then
                     --  This master is inside a different lock,
                     --  so we don't want to force it to exit.
                     --  However, we will mark the lock.

                     Mark_Inside_Lock
                       (Inner_Lock => Inner_Extra.Lock_Held,
                        Outer_Lock => Lock_Held_By_Target);

                     if Debug_Threading then
                        Put_Line ("  Not killing master" &
                          Master_Index'Image (Inner) &
                          "; holding lock" &
                          Lock_Obj_Index'Image
                            (Inner_Extra.Lock_Held));
                     end if;
                  else
                     --  This master hasn't been handled yet

                     --  Attempt to kill off the threads below this master
                     if Debug_Threading then
                        Put_Line (" Killing off subthreads of master" &
                          Master_Index'Image (Inner));
                     end if;

                     Subthread := First_Subthread (Inner_Addr);

                     while Subthread /= null loop

                        --  Prevent infinite loop
                        pragma Assert
                          (Subthread /= Next_Subthread (Subthread));

                        if Subthread = Exiting_Tcb
                          and then not Raising_Excep
                        then
                           --  Don't kill off the exiting TCB
                           --  unless raising an exception.
                           Subthread := Next_Subthread (Subthread);

                        elsif Tcb_Exit_Requested (Subthread) then
                           if Debug_Threading then
                              Put_Line ("  Already killed: subthread at " &
                                Hex_Image (Subthread));
                           end if;

                           Subthread := Next_Subthread (Subthread);

                        else
                           declare
                              Master_Of_Subthread : Master_Index :=
                                Index_Of_Master (Tcb_Master_Ptr (Subthread));
                              Next : constant Word_Ptr :=
                                Next_Subthread (Subthread);
                              Lock_Held_By_Subthread : constant Lock_Obj_Index
                                := Large_Obj_Lock_Obj (Subthread);
                           begin
                              if Lock_Held_By_Subthread /= Lock_Held_By_Target
                                and then not Tcb_Was_Queued (Subthread)
                              then
                                 --  Subthread holds a different lock,
                                 --  so don't request an exit.
                                 --  Mark the enclosing lock unless subthread
                                 --  is queued on the lock.
                                 Mark_Inside_Lock
                                   (Inner_Lock => Lock_Held_By_Subthread,
                                    Outer_Lock => Lock_Held_By_Target);

                                 if Debug_Threading then
                                    Put_Line ("  Not killing master" &
                                      Master_Index'Image (Inner) &
                                      " subthread at " &
                                      Hex_Image (Subthread) &
                                      "; holding lock" &
                                      Lock_Obj_Index'Image
                                        (Lock_Held_By_Subthread));
                                 end if;
                              else
                                 --  Request exit of subthread.
                                 Set_Tcb_Exit_Requested (Subthread, True);

                                 if Debug_Threading or Debug_Kill then
                                    Put_Line ("  Killing master" &
                                      Master_Index'Image (Inner) &
                                      " subthread at " &
                                      Hex_Image (Subthread));
                                 end if;
                              end if;

                              if Master_Of_Subthread /= Inner then
                                 --  Recursively walk its inner masters
                                 if Debug_Threading then
                                    Put_Line ("  Has inner master of" &
                                      Master_Index'Image
                                        (Master_Of_Subthread));
                                 end if;

                                 Kill_Thread_Tree
                                   (Inner_Master => Master_Of_Subthread,
                                    Outer_Master => Inner);
                              end if;

                              --  Wake up task if currently doing a queued call
                              if Tcb_Was_Queued (Subthread) then
                                 --  Remove from queue associated with lock.
                                 if Debug_Threading or Debug_Kill then
                                    Put_Line ("  Subthread is queued on lock" &
                                      Lock_Obj_Index'Image
                                        (Large_Obj_Lock_Obj (Subthread)));
                                 end if;

                                 declare
                                    Thread_Was_Queued : Boolean;
                                    Where_Queued : constant Object_Lock_Ptr :=
                                      Lock_Obj_Table
                                        (Large_Obj_Lock_Obj (Subthread)).Lock;
                                 begin
                                    Object_Locks.Dequeue_Tcb
                                      (Where_Queued.all,
                                       Server_Index,
                                       Subthread,
                                       Thread_Was_Queued);

                                    if Thread_Was_Queued then
                                       --  Finish thread if it was queued.
                                       Thread_Manager.Finish_Thread
                                         (Server_Index, Subthread);
                                    end if;
                                 end;
                              end if;

                              Subthread := Next;
                           end;
                        end if;
                     end loop;
                  end if;

                  --  Go up to next master
                  --  Prevent infinite loop
                  pragma Assert (Inner /= Inner_Extra.Enclosing_Master);

                  Inner := Inner_Extra.Enclosing_Master;
               end;
            end loop;
         end Kill_Thread_Tree;

      begin  --  Prepare_To_Exit

         if Debug_Threading or Debug_Kill then
            Put_Line (" Preparing to exit, with target master =" &
              Master_Index'Image (Target_Master_Index) &
              ", current master =" & Master_Index'Image (Encloser));
         end if;

         --  Walk up chain of masters until we find the specified one.
         --  Mark each master as being exited, and then walk back down
         --  the subthreads of the master, other than ones waiting on
         --  a master in the chain (TBD: is this necessary?).

         if Tcb_Exit_Requested (Exiting_Tcb) then
            --  Some other subthread beat us to it
            Succeeded := False;

            if Debug_Threading or Debug_Kill then
               Put_Line (" Prepare_To_Exit: thread at " &
                 Hex_Image (Exiting_Tcb) & " already killed");
            end if;
         else
            --  Assume success for now
            Succeeded := True;

            Kill_Thread_Tree
              (Inner_Master => Encloser,
               Outer_Master => Target_Master_Extra.Enclosing_Master,
               Quit_If_Master_Already_Exiting => True);
         end if;

         if Debug_Threading then
            Flush;
         end if;

      end Prepare_To_Exit;

      -------------------------------------------
      -- Set_Max_Dynamically_Allocated_Servers --
      -------------------------------------------

      procedure Set_Max_Dynamically_Allocated_Servers (Max : Natural) is
      --  Set new value for max. number of dynamically allocated servers
      begin
         Max_Dynamically_Allocated_Thread_Servers := Max;
      end Set_Max_Dynamically_Allocated_Servers;

      ---------------
      -- Shut_Down --
      ---------------

      procedure Shut_Down is
         use Thread_Manager_Data;
         pragma Assert (Num_Waiting_Shared_Threads = 0
                          or else Solo_Server /= 0);
         --  Shouldn't be any threads waiting
      begin
         Is_Shut_Down := True;
      end Shut_Down;

      ----------------------
      -- Spawn_New_Server --
      ----------------------

      entry Spawn_New_Server
        (Num_Live      : out Natural;
         Shut_Down_Now : out Boolean)
           when Is_Shut_Down
             or else
               (Get_Thread'Count < Minimum_Live
                and then
                  Num_Dynamically_Allocated_Thread_Servers <
                    Max_Dynamically_Allocated_Thread_Servers)
             or else --  Handle case when we exhaust existing servers
                     --  because they will only serve non-queuing threads.
               (Get_Thread'Count = 0
                and then Num_Waiting_Shared_Threads > 0
                and then Num_Nonqueuing_Threads = 0
                and then Num_Dynamically_Allocated_Thread_Servers in
                  Max_Dynamically_Allocated_Thread_Servers ..
                    Max_Thread_Servers - 10)
      is
      begin
         Num_Live := Get_Thread'Count;  --  TBD: Not really "live" count
         Shut_Down_Now := Is_Shut_Down;
      end Spawn_New_Server;

      --------------------------
      -- Add_To_Shared_Master --
      --------------------------

      procedure Add_To_Shared_Master
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr) is
         --  Link TCB onto chain off shared master
      begin
         --  Just pass the buck to the unshared version, with a lock
         Add_To_Master (Thread_Master, Server_Index, New_Tcb);
      end Add_To_Shared_Master;

      -------------------------
      -- Spawn_Shared_Thread --
      -------------------------

      procedure Spawn_Shared_Thread
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr)
      is
         use Thread_Manager_Data;
         pragma Assert (not Is_Shut_Down);

         Index            : Master_Index := Index_Of_Master (Thread_Master);
         pragma Assert (Index /= 0);

         Master_Extra     : Master_Extra_Rec renames Master_Extras (Index);
         Info             : Server_Info renames
                              Server_Info_Array (Server_Index);
      begin
         if Debug_Threading then
            Put_Line
              ("Spawn_Shared_Thread for Server" &
               Thread_Server_Index'Image (Server_Index) &
               ", TCB at " &
               Hex_Image (New_Tcb));
         end if;

         --  Check for multi-thread exit.
         if Tcb_Exit_Requested (Spawning_Tcb)
           or else Master_Exit_Requested (Thread_Master)
         then
            --  Rather than spawning a new thread we will either simply exit,
            --  if we are spawning a sibling thread, or if we are the "master"
            --  thread, then do a "wait" on the Thread_Master, and then exit.

            if Debug_Threading then
               Put_Line
                 (" Exit requested for Tcb/Master at " &
                  Hex_Image (Spawning_Tcb) & "/" & Hex_Image (Thread_Master)
                  & " (#" & Master_Index'Image
                    (Index_Of_Master (Thread_Master)) & ")");
            end if;

            return;   ----  return now  ----
         end if;

         --  Link TCB onto chain off master
         --  NOTE: We now do this inside the protected object since
         --        there is a potential race condition when spawning the
         --        next iteration of a concurrent initial/next/while loop.
         Add_To_Master (Thread_Master, Server_Index, New_Tcb);

         if not Master_Extra.Master_Is_Shared then
            --  Mark master shared if not already
            Master_Extra.Master_Is_Shared := True;
            Master_Extra.Innermost_Shared_Master := Index;
         end if;

         --  Add to queue of waiting threads
         --  NOTE: We used to put these in FIFO order
         --       but now we use LIFO order, with the
         --       assumption that "smaller" threads are
         --       put on the thread later than "bigger" ones,
         --       and we want to process the smaller threads
         --       quickly so the master waiting for them
         --       can continue.

         Add_To_Shared_Deque (Server_Index, New_Tcb);

         if Num_Shared_Threads_Needed > 1 then
            --  We added one shared thread, but more are needed.
            if Info.Unshared_Threads.Count > 0 then
               --  More shared threads needed, and this server
               --  has some unshared ones
               Increase_Shared_Threads (Server_Index);
            else
               --  Recompute num shared threads needed
               Num_Shared_Threads_Needed :=
                 Thread_Count'Max (1, Thread_Count
                   (Get_Thread'Count + Num_Servers_Waiting_For_Masters) -
                     Num_Waiting_Shared_Threads);
            end if;
         else
            --  We provided one new shared thread
            Num_Shared_Threads_Needed := 0;
         end if;

         if Master_Extra.Subthread_Count > Max_Subthreads_Per_Master then
            Max_Subthreads_Per_Master := Master_Extra.Subthread_Count;
            if Debug_Threading then
               Put_Line
                 ("Spawn_Shared_Thread -- new " &
                  "Max_Subthreads_Per_Master =" &
                  Thread_Count'Image (Max_Subthreads_Per_Master));
            end if;
         end if;

         if Debug_Threading then
            Put_Line
              ("Spawn_Shared_Thread for master with index" &
               Master_Index'Image (Index));

            Dump_Masters;
            Thread_Manager.Dump_Locks;
            Thread_Manager.Dump_Thread_Tree (Label =>
              "Thread tree after Spawn_Shared_Thread");
         end if;

      end Spawn_Shared_Thread;

      -------------------------
      -- Pause_Other_Servers --
      -------------------------

      entry Pause_Other_Servers (Server_Index : Thread_Server_Index)
      --  Pause other servers; return when they have all stopped
      --  servicing their queues.
        when True is
      begin
         if Solo_Server /= 0 and then Solo_Server /= Server_Index then
            requeue Wait_For_End_Of_Pause;
         end if;

         Pause_Nesting := Pause_Nesting + 1;
         if Pause_Nesting > 1 then
            pragma Assert (Solo_Server = Server_Index);
            return;  --  all done  --
         end if;

         --  Indicate current server is the only one that should
         --  continue running.
         Solo_Server := Server_Index;

         requeue Wait_For_Other_Servers_To_Pause;

         --  We will set the atomic variable Solo_Server
         --  and then requeue on an entry that will wait until
         --  all of the other servers have paused.
         --  We will then clear the Pause variable, so
         --  the debug console can proceed (or we could remember the
         --  server index, and only allow that server to proceed).
         --  Presumably we also don't want this server to start serving
         --  other threads on its queue -- how do we prevent that?
         --     By taking them all off?
      end Pause_Other_Servers;

      --------------------------
      -- Resume_Other_Servers --
      --------------------------

      entry Resume_Other_Servers (Server_Index : Thread_Server_Index)
      --  Allow other servers to resume servicing their queues.
        when True is
      begin
         --  We will reset the atomic variable Solo_Server
         --  and allow all of the other servers to proceed.
         pragma Assert (Solo_Server = Server_Index);
         Pause_Nesting := Pause_Nesting - 1;
         if Pause_Nesting > 0 then
            return;  --  nothing more to do  --
         end if;

         --  Restart the delay queue
         Delay_Queue.Resume_Delay_Queue;

         --  Allow other servers to proceed
         Solo_Server := 0;

         if not Is_Shut_Down then
            if Debug_Threading then
               Put_Line ("All servers have resumed.");
            end if;
         end if;
      end Resume_Other_Servers;

      ----------------
      -- Dump_Locks --
      ----------------

      procedure Dump_Locks is
      begin
         if Last_Lock_Obj > 0 then
            Put_Line (" Locks:");

            for I in 1 .. Last_Lock_Obj loop
               if Lock_Obj_Table (I).Index = I then
                  Put_Line ("  " &
                    Lock_Obj_Index'Image (I) &
                    ": enclosing lock =" &
                    Lock_Obj_Index'Image (Enclosing_Lock (I)) &
                    "; num_subordinates =" &
                    Thread_Count'Image (Num_Lock_Subordinates (I)));
                  Object_Locks.Dump_Lock_Queue (Lock_Obj_Table (I).Lock.all);
               end if;
            end loop;
         end if;
         Put_Line (" Num_Lock_Subordinates (0) =" &
           Thread_Count'Image (Num_Lock_Subordinates (0)));
         Flush;
      end Dump_Locks;

      -----------------------
      -- Dump_Thread_State --
      -----------------------

      procedure Dump_Thread_State is
         use Thread_Manager_Data;
         Num_Free_Masters  : Natural := 0;

         Cur_Server : constant Thread_Server_Index := Current_Server_Index;

      begin
         Dump_Masters;

         Put_Line ("Num_Active_Threads =" & Natural'Image (Num_Active));

         Put_Line
           ("Num_Waiting_Shared_Threads =" &
            Thread_Count'Image (Num_Waiting_Shared_Threads));

         Put_Line
           ("Num_Nonqueuing_Threads =" &
            Thread_Count'Image (Num_Nonqueuing_Threads));

         Dump_Thread_Tree (Label =>
           "Current thread tree in Dump_Thread_State");

         --  Count the free masters
         for I in 1 .. Last_Master loop
            if Master_Extras (I).Master_Address = null then
               Num_Free_Masters := Num_Free_Masters + 1;
            end if;
         end loop;

         Put_Line ("Last_Master =" & Master_Index'Image (Last_Master));

         Put_Line
           ("Num_Active_Masters =" & Natural'Image (Num_Active_Masters));

         Put_Line ("Num_Free_Masters =" & Natural'Image (Num_Free_Masters));

         for I in 1 .. Last_Master loop
            declare
               Master_Extra : Master_Extra_Rec renames Master_Extras (I);
            begin
               if Master_Extra.Master_Address /= null then
                  Put_Line
                    ("Master" &
                     Master_Index'Image (I) &
                     " has" &
                     Thread_Count'Image (Master_Extra.Subthread_Count) &
                     " waiting threads, " &
                     Thread_Count'Image
                        (Num_Lock_Subordinates (Master_Extra.Lock_Held)) &
                     " lock subordinates, " &
                     Thread_Count'Image (Master_Extra.Num_Subordinates) &
                     " master subordinates.");
               end if;
            end;
         end loop;

         for I in Server_Info_Array'First .. Last_Server_Index loop
            declare
               Info    : Server_Info renames Server_Info_Array (I);
               Current : constant Word_Ptr :=
                 Info.Last_Active_Thread;

            begin
               if I = Main_Thread_Server_Index
                 or else Current /= null
                 or else
                   Info.Shared_Threads (Locked_Thread).First_Thread /=
                     null
                 or else
                   Info.Shared_Threads (Nonqueuing_Thread).First_Thread /=
                     null
                 or else
                   Info.Shared_Threads (Queuing_Thread).First_Thread /=
                     null
               then
                  --  We have one or more threads associated
                  --  with this server
                  if I = Cur_Server then
                     Put ("*Current* ");
                  end if;
                  Put_Line ("Server" & Thread_Server_Index'Image (I) & ':');

                  if Current /= null then
                     Put_Line (" Active thread:");
                     Dump_One_Thread (Current);
                  elsif I = Main_Thread_Server_Index then
                     Put_Line (" Main Thread");
                  else
                     Put_Line (" No active thread");
                  end if;

                  if I = Main_Thread_Server_Index
                    or else Current /= null
                  then
                     Put_Line (" Active thread stack:");
                     Dump_Stack (Info.Current_State);
                  end if;

                  --  Now dump queue(s) of waiting threads
                  for Category in Info.Shared_Threads'Range loop
                     declare
                        Next : Word_Ptr :=
                          Info.Shared_Threads (Category).First_Thread;
                     begin
                        if Next /= null then
                           Put_Line
                             (" Waiting threads with Category = " &
                              Thread_Category'Image (Category) &
                              ":");
                        else
                           Put_Line
                             (" No waiting threads with Category = " &
                              Thread_Category'Image (Category) &
                              "");
                        end if;

                        while Next /= null loop
                           Dump_One_Thread (Next);

                           Next := Next_Waiting_Tcb (Next);
                        end loop;
                     end;
                  end loop;
               end if;
            end;
         end loop;
         Put_Line ("Dump_Thread_State finished.");
         Flush;
      exception
         when Storage_Error =>
            --  Not much to do here
            raise;

         when E : others =>
            Flush;
            Put_Line ("Exception raised in Dump_Thread_State: " &
              Ada.Exceptions.Exception_Information (E));
            Flush;
      end Dump_Thread_State;

      ----------------------
      -- Dump_Thread_Tree --
      ----------------------

      procedure Dump_Thread_Tree
        (Inner_Master : Master_Index := 1;
         Outer_Master : Master_Index := 0;
         Label        : String := "";
         Indent       : Natural := 2)
      is
      --  Display all subthreads of Inner_Master
      --  and any masters enclosing Inner_Master up to,
      --  but not including, Outer_Master.

         procedure Dump_One_Master
           (Encloser : Master_Index;
            Indent   : Natural);
         --  Display all of the subthreads of Encloser recursively

         procedure Dump_One_Master
           (Encloser : Master_Index;
            Indent   : Natural) is
         --  Display all of the subthreads of Encloser recursively
            Encloser_Extra : Master_Extra_Rec renames
                               Master_Extras (Encloser);
            Encloser_Addr  : constant Word_Ptr :=
                               Encloser_Extra.Master_Address;
            Subthread      : Word_Ptr :=
                               First_Subthread (Encloser_Addr);
            Ind_Str : constant String (1 .. Indent) := (others => ' ');

            function Is_Local return String;
            --  Return " (local to server X)" if Encloser master is unshared

            function Is_Local return String is
            begin
               if not Encloser_Extra.Master_Is_Shared then
                  return " (local to server" &
                    Thread_Server_Index'Image
                      (Encloser_Extra.Owned_By_Server) & ")";
               else
                  return "";
               end if;
            end Is_Local;

         begin  --  Dump_One_Master

            --  Display this master
            Put_Line (Ind_Str & "-- Master" &
              Master_Index'Image (Encloser) & Is_Local & " --");
            while Subthread /= null loop
               declare
                  Submaster : Master_Index :=
                    Index_Of_Master (Tcb_Master_Ptr (Subthread));

                  Next      : constant Word_Ptr :=
                           Next_Subthread (Subthread);
               begin
                  --  Dump this thread
                  Dump_One_Thread (Subthread, Indent);

                  --  Dump each of its inner masters, if any
                  while Submaster /= Encloser
                    and then Submaster /= 0
                    and then Master_Extras (Submaster).Master_Address /= null
                  loop
                     Dump_One_Master (Submaster, Indent => Indent + 2);
                     Submaster := Master_Extras (Submaster).Enclosing_Master;
                  end loop;

                  --  Prevent infinite loop
                  pragma Assert (Subthread /= Next, "loop in subthread list");

                  Subthread := Next;
               end;
            end loop;
         end Dump_One_Master;

         Encloser : Master_Index := Inner_Master;

      begin  --  Display_Thread_Tree

         if Label /= "" then
            Put_Line (' ' & Label & ':');
         end if;

         --  Find master immediately inside of Outer_Master
         while Master_Extras (Encloser).Enclosing_Master /= Outer_Master loop
            --  Prevent infinite loop
            pragma Assert
              (Encloser /= Master_Extras (Encloser).Enclosing_Master,
               "loop in enclosing master chain");

            exit when Master_Extras (Encloser).Enclosing_Master = 0;

            Encloser := Master_Extras (Encloser).Enclosing_Master;
         end loop;

         --  Display all of the subthreads of Encloser recursively
         Dump_One_Master (Encloser, Indent);

         if Outer_Master = 0 and then Inner_Master = 1 then
            --  This is a special case where we want to dump entire tree;
            --  we dump all masters with Enclosing_Master = 0,
            --  i.e. the "top-level" masters.
            for Index in 1 .. Last_Master loop
               declare
                  Master_Extra : Master_Extra_Rec
                    renames Master_Extras (Index);
               begin
                  if Master_Extra.Enclosing_Master = 0
                    and then Index /= Encloser  --  Already dumped this one
                    and then
                      Master_Extra.Master_Address /= null
                  then
                     --  This is a "top level" master, other than Encloser.
                     Dump_One_Master (Index, Indent);
                  end if;
               end;
            end loop;
         end if;

         Flush;
      end Dump_Thread_Tree;

      pragma Warnings (On, "potentially unsynchronized barrier");

   end Thread_Manager;

   ---------------------
   -- Stg_Rgn_Manager --
   ---------------------

   protected body Stg_Rgn_Manager is
      --  This manages the creation of storage regions and stg-rgn chunks

      ---------------------------
      -- Create_Stg_Rgn --
      ---------------------------

      procedure Create_Stg_Rgn
        (New_Stg_Rgn       : out Stg_Rgn_Ptr)
      is
         --  Return a region that can be used as a local region.
         Result : constant Stg_Rgn_Ptr := new Stg_Rgn;
      begin
         Result.Shared_Part := new Stg_Rgn;

         --  Assign a new region index.
         Num_Stg_Rgns := Num_Stg_Rgns + 1;
         Result.Index := Num_Stg_Rgns;
         Result.Shared_Part.Index := Num_Stg_Rgns;

         --  Need to compute a "null" for this region
         --  NOTE: We reserve a special negative chunk # for large nulls
         Result.Null_Value := Large_Null_Chunk * Chunk_Divisor +
                              Word_Type (Result.Index) * 2 + Word_Type'(1);

         Result.Shared_Part.Null_Value := Result.Null_Value;

         --  Initialize stg_rgn table
         Stg_Rgn_Table (Result.Index) := Result;

         New_Stg_Rgn := Result;
      end Create_Stg_Rgn;

      -----------------------------
      -- Get_Stg_Rgn_Chunk_Index --
      -----------------------------

      procedure Get_Stg_Rgn_Chunk_Index (Index : out Chunk_Index) is
      --  Get a unique region chunk index
      begin
         Last_Chunk_Index := Last_Chunk_Index + 1;
         Index := Last_Chunk_Index;

         if Chunk_Group_Table
           (Chunk_Group_Index (Index / Chunk_Group_Size)) = null
         then
            --  Need to allocate a new chunk group
            Chunk_Group_Table (Chunk_Group_Index (Index / Chunk_Group_Size)) :=
              new Chunk_Group;
         end if;
      end Get_Stg_Rgn_Chunk_Index;

      -----------------------------------
      -- Initialize_Global_Stack_Chunk --
      -----------------------------------

      procedure Initialize_Global_Stack_Chunk is
      --  Initialize the global stack chunk, if not yet initialized.
      begin
         if Global_Stack_Chunk = null then
            if Chunk_Group_Table (0) = null then
               --  Have not allocated any other chunks yet
               --  so need to create first Chunk_Group
               Chunk_Group_Table (0) := new Chunk_Group;
            end if;

            Global_Stack_Chunk :=
              new Interpreter.Stg_Rgn_Chunk (Chunk_Length => 10_000);

            Interpreter.Install_Chunk
              (Interpreter.Global_Stack_Chunk, Index => 1);
         end if;
      end Initialize_Global_Stack_Chunk;

      ------------------------
      -- Num_Stg_Rgn_Chunks --
      ------------------------

      function Num_Stg_Rgn_Chunks return Natural is
      --  Return count of region chunks allocated.
      begin
         return Natural (Last_Chunk_Index);
      end Num_Stg_Rgn_Chunks;

   end Stg_Rgn_Manager;

   --------------------------
   -- Stg_Rgn_Manager_Type --
   --------------------------

   protected body Stg_Rgn_Manager_Type is

      --  This synchronizes allocation/deallocation within regions.

      --------------------------
      -- Borrow_Stg_Rgn_Chunk --
      --------------------------

      procedure Borrow_Stg_Rgn_Chunk
        (Enclosing_Stg_Rgn : Stg_Rgn_Ptr;
         Min_Size : Offset_Within_Area;
         Borrowed_Chunk : out Stg_Rgn_Chunk_Ptr) is
         --  Borrow a chunk from enclosing region that has at least
         --  the given amount of space.
         --  Borrowed_Chunk is null if region has no appropriate chunk.
      begin
         --  Just pass the buck to common code
         Borrow_Unshared_Stg_Rgn_Chunk
           (Enclosing_Stg_Rgn, Min_Size, Borrowed_Chunk);
      end Borrow_Stg_Rgn_Chunk;

      --------------------------
      -- Return_Stg_Rgn_Chunk --
      --------------------------

      procedure Return_Stg_Rgn_Chunk
        (Chunk             : Stg_Rgn_Chunk_Ptr;
         Enclosing_Stg_Rgn : Stg_Rgn_Ptr) is
      --  Return chunk back to enclosing region
      begin
         --  Just pass the buck to common code
         Return_Unshared_Stg_Rgn_Chunk (Chunk, Enclosing_Stg_Rgn);
      end Return_Stg_Rgn_Chunk;

      ---------------------------
      -- Allocate_From_Stg_Rgn --
      ---------------------------

      procedure Allocate_From_Stg_Rgn
        (Stg_Rgn       : Stg_Rgn_Ptr;
         Size_In_Words : Offset_Within_Area;
         Obj_Addr      : out Word_Type;
         Server_Index  : Thread_Server_Index)
      is
         --  This attempts to reuse storage freed by Deallocate_From_Stg_Rgn.
         --  Initialize allocated space with region/size/null-type.
      begin
         --  Just pass the buck to the common code
         Allocate_From_Unshared_Stg_Rgn
           (Stg_Rgn, Size_In_Words, Obj_Addr, Server_Index);
      end Allocate_From_Stg_Rgn;

      -----------------------------
      -- Deallocate_From_Stg_Rgn --
      -----------------------------

      procedure Deallocate_From_Stg_Rgn
        (Stg_Rgn         : Stg_Rgn_Ptr;
         Storage_Address : Object_Virtual_Address;
         Server_Index    : Thread_Server_Index)
      is
         --  This adds the given storage to a list indexed by the size.
         --  Requires: Object has size embedded in its header.
      begin
         --  Just pass the buck to common code
         Deallocate_From_Unshared_Stg_Rgn
           (Stg_Rgn, Storage_Address, Server_Index);
      end Deallocate_From_Stg_Rgn;

      -----------------------------------
      -- Perform_Shared_Stg_Rgn_Action --
      -----------------------------------

      procedure Perform_Shared_Stg_Rgn_Action
        (Shared_Rgn : Stg_Rgn_Ptr;
         Action : in out Stg_Rgn_Action_Type'Class;
         Server_Index : Thread_Server_Index) is
      --  Perform the given action while inside the manager of the
      --  given shared-stg-rgn manager.
      begin
         --  Do the action while holding the lock on the shared stg rgn.
         Do_Stg_Rgn_Action (Action, Shared_Rgn, Server_Index);
      end Perform_Shared_Stg_Rgn_Action;

   end Stg_Rgn_Manager_Type;

   --------------- Local Task ------------------

   task type Delay_Server;
   type Delay_Server_Ptr is access Delay_Server;

   task body Delay_Server is
      Shut_Down_Now    : Boolean := False;
      Next_Wakeup_Time : Ada.Calendar.Time;
      Now              : Ada.Calendar.Time;
      Queue_Is_Empty   : Boolean := False;

      use type Ada.Calendar.Time;
   begin
      loop
         if Debug_Threading then
            Delay_Queue.Dump_Delay_Queue;
         end if;

         loop
            declare
               Thread_To_Wake_Up : Word_Ptr;
            begin
               Delay_Queue.Service_Queue
                 (Thread_To_Wake_Up,
                  Next_Wakeup_Time,
                  Shut_Down_Now,
                  Queue_Is_Empty => Queue_Is_Empty);

               exit when Thread_To_Wake_Up = null;

               --  Wake up the thread (by finishing it!)

               --  Only tcbs of a shared master should be put on delay queue
               pragma Assert (Master_Extras (Index_Of_Master
                 (Tcb_Master_Ptr (Thread_To_Wake_Up))).Master_Is_Shared);

               Finish_Thread
                 (Server_Index => Delay_Queue_Server_Index,
               --  TBD: should keep track of TCB's original server
                  Finished_Tcb => Thread_To_Wake_Up);

               --  Loop around to get another thread, or the
               --  next wakeup time.
            end;

         end loop;

         exit when Shut_Down_Now;
         --  Quit if system shutting down

         if Queue_Is_Empty then
            --  Just wait for something to be queued
            --  Doing it this way will allow the system
            --  to detect a deadlock.
            Delay_Queue.Wait_For_Change;
            if Debug_Threading or Debug_Delay then
               Put_Line
                 (" Delay queue now has something on it " &
                  "(or shutting down)");
            end if;
         else
            --  Wait for something new to be queued while
            --  delaying until the next wakeup time.

            if Debug_Threading or Debug_Delay then
               Now := Ada.Calendar.Clock;
               Put_Line
                 (" Next_Wakeup_Time = Now + " &
                  Duration'Image (Next_Wakeup_Time - Now));
            end if;

            select
               Delay_Queue.Wait_For_Change;
               --  Wait until wakeup time changes
               if Debug_Threading then
                  Put_Line (" Delay queue wakeup time change");
               end if;
            or
            --  while waiting for next wakeup time to arrive
               delay until Next_Wakeup_Time;

               if (Debug_Threading or Debug_Delay)
                 and then not Shut_Down_Now
               then
                  Now := Ada.Calendar.Clock;
                  Put_Line
                    (" Reached next wakeup time = Now - " &
                     Duration'Image (Now - Next_Wakeup_Time));
               end if;
            end select;
         end if;
      end loop;

      if Debug_Threading then
         Put_Line (" Shutting down delay queue.");
      end if;
   end Delay_Server;

   ---------------------------- Local package bodies --------------------------

   --------------------------
   -- Delay_Queue_Handling --
   --------------------------

   package body Delay_Queue_Handling is

      --  Constants used by Pause/Resume_Delay_Queue
      Max_Pause_Amount_In_Hours : constant := 8;
      Max_Pause_Duration : constant Duration :=
        Max_Pause_Amount_In_Hours * 3600.0;

      protected body Delay_Queue is

         -------------
         -- Add_Tcb --
         -------------

         procedure Add_Tcb
           (Tcb_Addr     : Word_Ptr;
            Delay_Until  : Ada.Calendar.Time;
            Server_Index : Thread_Server_Index)
         is
            New_Entry  : Delay_Queue_Entry_Ptr := Free_List;
            Cur_Entry  : Delay_Queue_Entry_Ptr := Delay_Queue_Head;
            Prev_Entry : Delay_Queue_Entry_Ptr := null;
            Num_Before : Natural := 0;
            use type Ada.Calendar.Time;
         begin
            if New_Entry = null then
               New_Entry := new Delay_Queue_Entry;
            else
               Free_List := New_Entry.Next;
            end if;

            while Cur_Entry /= null
              and then Delay_Until >= Cur_Entry.Delay_Until
            loop
               Prev_Entry := Cur_Entry;
               Cur_Entry := Cur_Entry.Next;
               Num_Before := Num_Before + 1;
            end loop;

            --  Found where it should go
            New_Entry.all :=
              (Tcb_Addr => Tcb_Addr,
               Delay_Until => Delay_Until,
               Server_Index => Server_Index,
               Next => Cur_Entry);

            if Prev_Entry = null then
               --  We have a new first entry
               Delay_Queue_Head := New_Entry;
               Is_New_Wakeup_Time := True;
            else
               Prev_Entry.Next := New_Entry;
            end if;

            if Debug_Threading or Debug_Delay then
               Put_Line
                 (" Adding TCB at " &
                  Hex_Image (Tcb_Addr) &
                  " to delay queue with New_Wakeup_Time now " &
                  Boolean'Image (Is_New_Wakeup_Time));
               declare
                  Num_After : Natural := 0;
               begin
                  while Cur_Entry /= null loop
                     Num_After := Num_After + 1;
                     Cur_Entry := Cur_Entry.Next;
                  end loop;
                  Put_Line (" " & Natural'Image (Num_Before) & " before and" &
                    Natural'Image (Num_After) & " after.");
               end;
            end if;

            --  Indicate thread is on delay queue
            Set_Tcb_State (Tcb_Addr, On_Delay_Queue);

         end Add_Tcb;

         ----------------------
         -- Dump_Delay_Queue --
         ----------------------

         procedure Dump_Delay_Queue is
            --  Dump current state of delay queue
            Dq_Ptr : Delay_Queue_Entry_Ptr := Delay_Queue_Head;
         begin
            while Dq_Ptr /= null loop
               declare
                  Next : constant Delay_Queue_Entry_Ptr := Dq_Ptr.Next;
                  use Ada.Calendar;
               begin
                  Put_Line
                    (" Found TCB on delay queue at " &
                     Hex_Image (Dq_Ptr.Tcb_Addr) &
                     " with wakeup = Now + " &
                     Duration'Image (Dq_Ptr.Delay_Until - Ada.Calendar.Clock) &
                     ", server_index = " &
                     Thread_Server_Index'Image (Dq_Ptr.Server_Index));

                  --  Should still be marked as queued
                  pragma Assert (Tcb_Was_Queued (Dq_Ptr.Tcb_Addr));

                  Dq_Ptr := Next;
               end;
            end loop;

         end Dump_Delay_Queue;

         ----------------
         -- Remove_Tcb --
         ----------------

         procedure Remove_Tcb
           (Tcb_Addr : Word_Ptr;
            Thread_Was_Queued : out Boolean)
         is
            --  Remove Tcb from delay queue
            Prev_Elem  : Delay_Queue_Entry_Ptr := null;
            Queue_Elem : Delay_Queue_Entry_Ptr := Delay_Queue_Head;
         begin
            while Queue_Elem /= null loop
               if Queue_Elem.Tcb_Addr = Tcb_Addr then
                  --  Found it; carve it out of the delay queue
                  if Prev_Elem = null then
                     --  Was first on the delay queue
                     Delay_Queue_Head := Queue_Elem.Next;
                  else
                     Prev_Elem.Next := Queue_Elem.Next;
                  end if;

                  --  Mark thread as complete
                  if Debug_Threading or Debug_Delay then
                     Put_Line
                       (" Removed TCB from delay queue at " &
                        Hex_Image (Tcb_Addr));
                  end if;

                  Thread_Was_Queued := True;

                  --  Put delay queue entry on the free list
                  Queue_Elem.Next := Free_List;
                  Free_List := Queue_Elem;

                  --  Indicate no longer on delay queue
                  Set_Tcb_State (Tcb_Addr, Unknown_State);

                  return;    --  And return  --
               end if;

               --  Keep looking
               Prev_Elem := Queue_Elem;
               Queue_Elem := Queue_Elem.Next;
            end loop;

            --  Its delay must have already expired
            Thread_Was_Queued := False;

         end Remove_Tcb;

         -------------------
         -- Service_Queue --
         -------------------

         procedure Service_Queue
           (Thread_To_Wake_Up : out Word_Ptr;
            Next_Wakeup_Time  : out Ada.Calendar.Time;
            Shut_Down_Now     : out Boolean;
            Queue_Is_Empty    : out Boolean) is
         --  Return with thread that is now ready.
         --  If none, return time when to next wakeup.
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            use type Ada.Calendar.Time;
         begin
            if Debug_Threading then
               Put_Line (" Servicing delay queue");
            end if;

            --  Compute the default next wakeup time
            Next_Wakeup_Time := Now +
                                (Ada.Calendar.Day_Duration'Last -
                                 Duration'Small);

            Thread_To_Wake_Up := null;
            Queue_Is_Empty := True;

            Shut_Down_Now := Delay_Queue.Shut_Down_Now;

            --  Initialize this to False; will be set true
            --  when an Add_TCB comes in with an earlier wakeup time.
            Is_New_Wakeup_Time := False;

            if Delay_Queue_Head /= null
              and then
                (Delay_Queue_Head.Delay_Until <= Now
                   or else
                 Shut_Down_Now)
            then
               --  Found a ready thread
               declare
                  Next : constant Delay_Queue_Entry_Ptr :=
                           Delay_Queue_Head.Next;
               begin
                  --  Thread is now complete
                  if Debug_Threading or Debug_Delay then
                     Put_Line
                       (" Found ready TCB on delay queue at " &
                        Hex_Image (Delay_Queue_Head.Tcb_Addr));
                  end if;

                  Thread_To_Wake_Up := Delay_Queue_Head.Tcb_Addr;

                  --  Put delay queue entry on the free list
                  Delay_Queue_Head.Next := Free_List;
                  Free_List := Delay_Queue_Head;

                  --  Update the first queue
                  Delay_Queue_Head := Next;

                  return;  --  Return now with Thread_To_Wake_Up set
               end;
            end if;

            --  No ready threads.

            if Delay_Queue_Head /= null then
               --  Set the next wakeup time
               Next_Wakeup_Time := Delay_Queue_Head.Delay_Until;
               Queue_Is_Empty := False;
            else
               if Debug_Threading or Debug_Delay then
                  Put_Line (" Nothing on the delay queue.");
               end if;
            end if;

            if Debug_Threading or Debug_Delay then
               Put_Line
                 (" Next_Wakeup_Time = Now + " &
                  Duration'Image (Next_Wakeup_Time - Now));
            end if;
         end Service_Queue;

         ---------------------
         -- Wait_For_Change --
         ---------------------

         entry Wait_For_Change when Is_New_Wakeup_Time or Shut_Down_Now is
         begin
            --  Delay_Server will loop around and call Service_Queue
            Is_New_Wakeup_Time := False;
            if Debug_Threading or Debug_Delay then
               Put_Line (" We have a new wakeup time.");
            end if;
         end Wait_For_Change;

         ---------------
         -- Shut_Down --
         ---------------

         procedure Shut_Down is
         begin
            Shut_Down_Now := True;
         end Shut_Down;

         -----------------------
         -- Pause_Delay_Queue --
         -----------------------

         procedure Pause_Delay_Queue is
         --  Pause the servicing of the delay queue
            Waiter : Delay_Queue_Entry_Ptr := Delay_Queue_Head;
            use type Ada.Calendar.Time;
         begin
            --  Remember when the pause occurred
            Clock_At_Pause := Ada.Calendar.Clock;

            if Debug_Delay and then Waiter /= null then
               Put_Line ("Adding" &
                 Natural'Image (Max_Pause_Amount_In_Hours) &
                 " hours to all delay-queue entries");
            end if;

            while Waiter /= null loop
               Waiter.Delay_Until := Waiter.Delay_Until + Max_Pause_Duration;
               Waiter := Waiter.Next;
            end loop;

            --  We want to allow the debug console to invoke things that
            --  need the delay queue, so the best thing is probably to
            --  go through and change all of the times on the items already
            --  on the queue to be much later.
            --  Then, when we get to "resume" we will subtract the
            --  amount added, adjusted by how long the clock was "stopped."
            --  We should not call Pause_Delay_Queue if already paused.
         end Pause_Delay_Queue;

         ------------------------
         -- Resume_Delay_Queue --
         ------------------------

         procedure Resume_Delay_Queue is
         --  Resume the servicing of the delay queue,
         --  and adjust all times to account for the length of the pause.

            --  Compute how much to re-adjust the queued items,
            --  so that delays continue to proceed by the same amount
            --  ignoring the time spent while paused.
            use type Ada.Calendar.Time;
            Actual_Pause_Duration : constant Duration :=
              Ada.Calendar.Clock - Clock_At_Pause;
            Adjustment : constant Duration :=
              Max_Pause_Duration - Actual_Pause_Duration;
            Waiter : Delay_Queue_Entry_Ptr := Delay_Queue_Head;
         begin
            if Shut_Down_Now then
               --  Quit immediately
               return;
            end if;

            if Waiter /= null then
               if Debug_Delay then
                  Put_Line
                    ("Adjusting delay-queue entries for pause which took" &
                     Duration'Image (Actual_Pause_Duration) & " seconds");
               end if;
            end if;

            while Waiter /= null loop
               Waiter.Delay_Until := Waiter.Delay_Until - Adjustment;
               Waiter := Waiter.Next;
            end loop;

            if Debug_Delay and then Delay_Queue_Head /= null then
               Dump_Delay_Queue;
            end if;

            Is_New_Wakeup_Time := True;

         end Resume_Delay_Queue;

      end Delay_Queue;

      ---------------------
      -- Delay_Tcb_Until --
      ---------------------

      procedure Delay_Tcb_Until
        (Tcb_Addr    : Word_Ptr;
         Delay_Until : Ada.Calendar.Time)
      is
         Clock_Lock : constant Object_Lock_Ptr :=
                        Lock_Obj_Table (Large_Obj_Lock_Obj (Tcb_Addr)).Lock;
      begin
         if Debug_Threading then
            declare
               Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               use type Ada.Calendar.Time;
            begin
               Put_Line
                 (" Add to TCB delay queue, to wakeup in " &
                  Duration'Image (Delay_Until - Now) &
                  " seconds.");
            end;
         end if;

         --  Set up special dequeue routine for the clock lock
         Set_Dequeue_Routine
           (Clock_Lock.all, Remove_From_Delay_Queue'Access);
         Set_Tcb_Was_Queued (Tcb_Addr, True);

         Delay_Queue.Add_Tcb (Tcb_Addr, Delay_Until, Current_Server_Index);
      end Delay_Tcb_Until;

      -----------------------------
      -- Remove_From_Delay_Queue --
      -----------------------------

      procedure Remove_From_Delay_Queue
        (Tcb_Addr          : Word_Ptr;
         Thread_Was_Queued : out Boolean) is
      begin
         --  Just pass the buck to the protected subprogram
         Delay_Queue.Remove_Tcb (Tcb_Addr, Thread_Was_Queued);
      end Remove_From_Delay_Queue;

   end Delay_Queue_Handling;

   --------------------------
   -- Large_Obj_Header_Ops --
   --------------------------

   package body Large_Obj_Header_Ops is

      ------------------------
      -- Large_Obj_Lock_Obj --
      ------------------------

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Virtual_Address) return Lock_Obj_Index is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj;
      end Large_Obj_Lock_Obj;

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Address) return Lock_Obj_Index is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj;
      end Large_Obj_Lock_Obj;

      function Large_Obj_Lock_Obj
        (Large_Obj_Addr : Word_Ptr) return Lock_Obj_Index is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj;
      end Large_Obj_Lock_Obj;

      --------------------------
      -- Large_Obj_Next_Block --
      --------------------------

      function Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Virtual_Address) return Object_Virtual_Address
      is
      begin
         return Content_Of_Virtual_Address
                  (Large_Obj_Addr + Large_Obj_Next_Block_Offset);
      end Large_Obj_Next_Block;

      ------------------------
      -- Large_Obj_On_Stack --
      ------------------------

      function Large_Obj_On_Stack
        (Addr : Object_Virtual_Address) return Boolean is
      --  Return True if given non-null large obj is residing on stack
      begin
         return not Is_Special_Large_Value (Addr)
           and then To_Large_Obj_Ptr (Addr).On_Stack;
      end Large_Obj_On_Stack;

      --------------------
      -- Large_Obj_Size --
      --------------------

      function Large_Obj_Size
        (Large_Obj_Addr : Object_Address) return Offset_Within_Chunk is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Size;
      end Large_Obj_Size;

      function Large_Obj_Size
        (Large_Obj_Addr : Object_Virtual_Address) return Offset_Within_Chunk is
      begin
         if Is_Special_Large_Value (Large_Obj_Addr) then
            return 1;
         else
            return To_Large_Obj_Ptr (Large_Obj_Addr).Size;
         end if;
      end Large_Obj_Size;

      function Large_Obj_Size
        (Large_Obj_Addr : Word_Ptr) return Offset_Within_Chunk is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Size;
      end Large_Obj_Size;

      -----------------------------
      -- Large_Obj_Stg_Rgn_Index --
      -----------------------------

      function Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Object_Virtual_Address) return Stg_Rgn_Index is
      begin
         if Large_Obj_Addr < 0 and then Large_Obj_Addr mod 2 = 1 then
            --  We have a special large value -- extract region index from it
            pragma Assert (Is_Special_Large_Value (Large_Obj_Addr));
            return Stg_Rgn_Index
              (To_Unsigned_Word (Large_Obj_Addr) mod Chunk_Divisor / 2);
         else
            return To_Large_Obj_Ptr (Large_Obj_Addr).Stg_Rgn;
         end if;
      end Large_Obj_Stg_Rgn_Index;

      function Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Word_Ptr) return Stg_Rgn_Index is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Stg_Rgn;
      end Large_Obj_Stg_Rgn_Index;

      -------------------------
      -- Large_Obj_Type_Info --
      -------------------------

      function Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Virtual_Address) return Type_Index is
      begin
         if Is_Special_Large_Value (Large_Obj_Addr) then
            return 0;
         else
            return To_Large_Obj_Ptr (Large_Obj_Addr).Type_Info;
         end if;
      end Large_Obj_Type_Info;

      function Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Address) return Type_Index is
      begin
         return To_Large_Obj_Ptr (Large_Obj_Addr).Type_Info;
      end Large_Obj_Type_Info;

      -------------------------------------
      -- Large_Obj_Type_Info_Is_In_Range --
      -------------------------------------

      function Large_Obj_Type_Info_Is_In_Range
        (Large_Obj_Addr : Object_Virtual_Address) return Boolean is
      begin
         return
           Is_Special_Large_Value (Large_Obj_Addr)
             or else
               To_Large_Obj_Ptr (Large_Obj_Addr).Type_Info
                 in 1 .. Type_Index (Num_Elements (Type_Table));
      end Large_Obj_Type_Info_Is_In_Range;

      --------------------------
      -- Set_Large_Obj_Header --
      --------------------------

      procedure Set_Large_Obj_Header
        (Large_Obj_Addr : Object_Address;
         Size           : Offset_Within_Area;
         Stg_Rgn_Id     : Stg_Rgn_Index;
         Type_Id        : Type_Index;
         Lock_Obj       : Lock_Obj_Index := 0;
         On_Stack       : Boolean := False)
      is
         Header : Large_Obj_Header
                    renames To_Large_Obj_Ptr (Large_Obj_Addr).all;
      begin
         Header.Stg_Rgn := Stg_Rgn_Id;
         Header.Size := Size;
         Header.Type_Info := Type_Id;
         Header.Lock_Obj := Lock_Obj;
         Header.On_Stack := On_Stack;
      end Set_Large_Obj_Header;

      procedure Set_Large_Obj_Header
        (Large_Obj_Addr : Object_Virtual_Address;
         Size           : Offset_Within_Area;
         Stg_Rgn_Id     : Stg_Rgn_Index;
         Type_Id        : Type_Index;
         Lock_Obj       : Lock_Obj_Index := 0;
         On_Stack       : Boolean := False)
      is
         Header : Large_Obj_Header
                    renames To_Large_Obj_Ptr (Large_Obj_Addr).all;
      begin
         Header.Stg_Rgn := Stg_Rgn_Id;
         Header.Size := Size;
         Header.Type_Info := Type_Id;
         Header.Lock_Obj := Lock_Obj;
         Header.On_Stack := On_Stack;
      end Set_Large_Obj_Header;

      ----------------------------
      -- Set_Large_Obj_Lock_Obj --
      ----------------------------

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Virtual_Address;
         Lock_Obj       : Lock_Obj_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj := Lock_Obj;
      end Set_Large_Obj_Lock_Obj;

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Object_Address;
         Lock_Obj       : Lock_Obj_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj := Lock_Obj;
      end Set_Large_Obj_Lock_Obj;

      procedure Set_Large_Obj_Lock_Obj
        (Large_Obj_Addr : Word_Ptr;
         Lock_Obj       : Lock_Obj_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Lock_Obj := Lock_Obj;
      end Set_Large_Obj_Lock_Obj;

      ------------------------------
      -- Set_Large_Obj_Next_Block --
      ------------------------------

      procedure Set_Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Address;
         Next_Block     : Object_Virtual_Address) is
      begin
         Large_Obj_Addr.Enclosing_Chunk.Data
           (Large_Obj_Addr.Offset +
              Large_Obj_Next_Block_Offset) := Next_Block;
      end Set_Large_Obj_Next_Block;

      procedure Set_Large_Obj_Next_Block
        (Large_Obj_Addr : Object_Virtual_Address;
         Next_Block     : Object_Virtual_Address) is
      begin
         Store_Word (Large_Obj_Addr + Large_Obj_Next_Block_Offset, Next_Block);
      end Set_Large_Obj_Next_Block;

      ----------------------------
      -- Set_Large_Obj_On_Stack --
      ----------------------------

      procedure Set_Large_Obj_On_Stack
        (Large_Obj_Addr : Object_Virtual_Address;
         On_Stack       : Boolean) is
      --  Indicate whether given object resides on stack rather than in stg rgn
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).On_Stack := On_Stack;
      end Set_Large_Obj_On_Stack;

      ------------------------
      -- Set_Large_Obj_Size --
      ------------------------

      procedure Set_Large_Obj_Size
        (Large_Obj_Addr : Object_Address;
         Size           : Offset_Within_Area) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Size := Size;
      end Set_Large_Obj_Size;

      procedure Set_Large_Obj_Size
        (Large_Obj_Addr : Object_Virtual_Address;
         Size           : Offset_Within_Area) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Size := Size;
      end Set_Large_Obj_Size;

      ---------------------------------
      -- Set_Large_Obj_Stg_Rgn_Index --
      ---------------------------------

      procedure Set_Large_Obj_Stg_Rgn_Index
        (Large_Obj_Addr : Object_Virtual_Address;
         Stg_Rgn_Id     : Stg_Rgn_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Stg_Rgn := Stg_Rgn_Id;
      end Set_Large_Obj_Stg_Rgn_Index;

      -----------------------------
      -- Set_Large_Obj_Type_Info --
      -----------------------------

      procedure Set_Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Address;
         Type_Id        : Type_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Type_Info := Type_Id;
      end Set_Large_Obj_Type_Info;

      procedure Set_Large_Obj_Type_Info
        (Large_Obj_Addr : Object_Virtual_Address;
         Type_Id        : Type_Index) is
      begin
         To_Large_Obj_Ptr (Large_Obj_Addr).Type_Info := Type_Id;
      end Set_Large_Obj_Type_Info;

      ----------------------
      -- To_Large_Obj_Ptr --
      ----------------------

      function To_Large_Obj_Ptr
        (Large_Obj_Addr : Object_Address) return Large_Obj_Header_Ptr is
      begin
         return Addr_To_Large_Obj_Ptr
                  (Large_Obj_Addr.Enclosing_Chunk
                    .Data (Large_Obj_Addr.Offset)'Address);
      end To_Large_Obj_Ptr;

   end Large_Obj_Header_Ops;

   -----------------------
   -- Locked_And_Queued --
   -----------------------

   package body Locked_And_Queued is

      -----------------------
      -- Allocate_Lock_Obj --
      -----------------------

      procedure Allocate_Lock_Obj (Lock_Obj : out Lock_Obj_Index;
        Server_Index : Thread_Server_Index) is
         use Thread_Manager_Data;
         Info : Server_Info renames Server_Info_Array (Server_Index);
      begin
         if Info.Free_Lock_Obj /= 0 then
            --  Reuse existing lock
            declare
               New_Lock_Info : Lock_Obj_Info renames
                                  Lock_Obj_Table (Info.Free_Lock_Obj);
            begin
               --  Remove from linked list
               Lock_Obj            := Info.Free_Lock_Obj;
               Info.Free_Lock_Obj  := New_Lock_Info.Index;

               --  Re-initialize lock info to point back to itself
               New_Lock_Info.Index := Lock_Obj;

               if Debug_Threading then
                  Put_Line
                    (" Reusing lock #" & Lock_Obj_Index'Image (Lock_Obj));
               end if;
            end;
         else
            --  Need to allocate a new lock obj
            Thread_Manager.Next_Lock_Obj (Lock_Obj);
         end if;
      end Allocate_Lock_Obj;

      ---------------------------------
      -- Dequeue_Condition_Satisfied --
      ---------------------------------

      function Dequeue_Condition_Satisfied
        (Context : in out Exec_Context;
         Calling_Tcb : Word_Ptr;
         Server_Index : Thread_Server_Index;
         Called_Routine : Routine_Ptr := null)
         return Boolean
      is
      --  NOTE: Calling_TCB has indicator of routine it was created to execute.
      --        If this TCB was created to perform this parallel call, then the
      --        Calling_TCB's routine will match Called_Routine.
      --        If this TCB was created earlier, perhaps to execute a nested
      --        block, then the TCB's called routine will in general not match
      --        Called_Routine.
         Tcb_Associated_Routine : Routine_Ptr;
         Start_Pc       : constant Code_Offset := Tcb_Start_Pc (Calling_Tcb);
         Caller_Params  : constant Word_Ptr :=
                            Add (Calling_Tcb, Tcb_Param_List_Offset);
      begin
         if Tcb_For_Compiled_Routine (Calling_Tcb) then
            --  Queueing for compiled routines is handled differently.
            --  We have two addresses, one for the called routine itself,
            --  and one for the dequeue condition (aka "internal precond").
            if Tcb_Internal_Precond_Addr (Calling_Tcb) = null then
               --  No internal precond, so no dequeue condition.
               return True;
            end if;

            if Called_Routine /= null then
               if Called_Routine.Is_PSVM_Routine
                 or else not Called_Routine.Is_Compiled_Routine
                 or else Called_Routine.Routine_Addr /=
                   Tcb_Code_Addr (Calling_Tcb)
               then
                  --  If called routine is in PSVM, or if
                  --  code addresses don't match, then this wasn't
                  --  a queued call in the first place.
                  return True;
               end if;
            end if;

            --  OK, we need to invoke the internal precondition
            declare
               Enclosing_Locals : Word_Array (0 .. 2) :=
                 (Local_Area_Static_Link_Offset =>
                    Word_Ptr_To_Word (To_Word_Ptr (Context.Enclosing_Type)),
                  Local_Area_Param_Ptr_Offset   =>
                    Word_Ptr_To_Word (Caller_Params),
                  Local_Area_Stg_Rgn_Ptr_Offset => 0);

               New_Static_Link : constant Type_Descriptor_Ptr :=
                 To_Type_Desc_Or_Op_Map
                   (Enclosing_Locals
                     (Enclosing_Locals'First)'Unchecked_Access);

               Params : aliased Word_Type := 0;  --  Will be filled in
               New_Context : Exec_Context :=
                 (Local_Null          => Context.Local_Null,
                  Enclosing_Type      => Context.Enclosing_Type,
                  Local_Stg_Rgn       => Context.Local_Stg_Rgn,
                  Control_Area        => Context.Control_Area,
                  Open_Master         => null,
                  Server_Index        => Server_Index,
                  Params              => Params'Unchecked_Access,
                  Local_Area          => null,
                  Local_Area_Length   => 0,
                  Start_Callee_Locals => 0);

               --  Evaluate the nested block representing the internal precond
               Outcome : constant Nested_Block_Outcome_As_Int :=
                 Tcb_Internal_Precond_Addr (Calling_Tcb)
                   (New_Context, Params'Unchecked_Access, New_Static_Link);
            begin
               pragma Assert (Outcome = 0);  --  Should always be "normal"

               --  Dequeue condition is satisfied if non-zero value returned.
               return (Params /= 0);
            end;
         end if;

         Tcb_Associated_Routine := Nth_Routine
                           (Tcb_Routine_Index (Calling_Tcb));

         if Called_Routine /= null
           and then Called_Routine.Boundary_Conditions (Internal_Precondition).
              Pc_Offset > 0
         then
            --  Called routine has a dequeue condition.
            --  It should only be called with a parallel call.
            pragma Assert (Called_Routine = Tcb_Associated_Routine);
            null;
         end if;

         if Tcb_Associated_Routine.Boundary_Conditions
           (Internal_Precondition).Pc_Offset > 0
         then
            --  We have a dequeue condition
            --  NOTE: Internal_Precondition is used for dequeue condition.
            --       And for non-concurrent modules, the internal precondition
            --       is treated as a dequeue condition when passed a
            --       concurrent instance of a type based on the module.
            declare
               pragma Assert (Start_Pc = Code_Index'First);
               pragma Assert
                 (Called_Routine = null
                    or else Called_Routine = Tcb_Associated_Routine);
               --  Should be a parallel call
               --  of the operation with a
               --  dequeue condition.
               --  Should be
               Params : constant Word_Ptr :=
                 Locator_To_Physical_Address
                    (Context,
                     (Local_Area, Local_Area_Local_Data_Offset, No_VM_Obj_Id));
            --  Use new caller's local area for param area for
            --  calling internal precondition as a nested block.
            begin
               if Debug_Threading then
                  Put_Line
                    (" evaluating dequeue condition for thread at " &
                     Hex_Image (Calling_Tcb));
               end if;

               --  Initialize link to param area
               --  TBD: This duplicates code in Execute, but we need
               --      to do it here because it may be pointing at the
               --      "wrong" parameters when this routine is called
               --      at the end of a locked operation.
               Store_Word (Context.Local_Area,
                 Local_Area_Param_Ptr_Offset,
                 Word_Ptr_To_Word (Caller_Params));

               Execute_Nested_Block
                 (Context,
                  Instructions => Tcb_Associated_Routine,
                  Params_Address => Params,
                  Static_Link =>
                    Locator_To_Physical_Address
                      (Context, (Local_Area, 0, No_VM_Obj_Id)),
                  Code_Block =>
                    Tcb_Associated_Routine.Boundary_Conditions
                      (Internal_Precondition),
                  Server_Index => Server_Index);

               --  Return result of evaluating the dequeue condition
               if Debug_Threading then
                  Put_Line
                    (" dequeue condition for thread at " &
                     Hex_Image (Calling_Tcb) &
                     " evaluates to " &
                     Boolean'Image
                       (Content_Of_Physical_Address (Params) /= 0));
               end if;

               return Content_Of_Physical_Address (Params) /= 0;
            end;
         else
            return True;
         end if;
      end Dequeue_Condition_Satisfied;

      --------------------------
      -- Execute_Nested_Block --
      --------------------------

      procedure Execute_Nested_Block
        (Context            : in out Exec_Context;
         Instructions       : Routine_Ptr;
         Params_Address     : Word_Ptr;
         Static_Link        : Word_Ptr;
         Code_Block         : Code_Block_Descriptor;
         Server_Index       : Thread_Server_Index;
         Base_For_Pc_Offset : Code_Index := Code_Index'First)
      is
         New_Local_Area : constant Word_Ptr :=
           Add (Context.Local_Area, Context.Start_Callee_Locals);
         --  TBD: This presumes that caller local area includes room for
         --       longest callee's local area. Clearly doesn't work for
         --       recursion! Once we start allocating dynamically we need
         --       to worry about reaching the end of the chunk, and more
         --       generally whether space after end of current local area
         --       is in use for other threads.

         New_Local_Rgn : constant Stg_Rgn_Ptr :=
            Get_New_Local_Stg_Rgn (New_Local_Area => New_Local_Area,
                                   Server_Index => Server_Index);

         New_Context : Exec_Context :=
           (Local_Null          => New_Local_Rgn.Null_Value,
            Enclosing_Type      => Context.Enclosing_Type,
            Local_Stg_Rgn       => New_Local_Rgn,
            Control_Area        => Context.Control_Area,
            Open_Master         => null,
            Server_Index        => Server_Index,
            Params              => Params_Address,
            Local_Area          => New_Local_Area,
            Local_Area_Length   => Code_Block.Local_Area_Length,
            Start_Callee_Locals => Code_Block.Start_Callee_Locals);

         Thread_Was_Queued : Boolean;
      begin --  Execute_Nested_Block

         --  Initialize static link
         Init_Static_Link (New_Local_Area, Static_Link);

         Check_Static_Chain (Static_Link);

         --  Recurse to execute the block
         Execute
           (Instructions      => Instructions,
            Start_Pc          => Base_For_Pc_Offset + Code_Block.Pc_Offset,
            Context           => New_Context,
            Thread_Was_Queued => Thread_Was_Queued,
            Server_Index      => Server_Index);

         pragma Assert (not Thread_Was_Queued);

         Release_Stg_Rgn (New_Context.Local_Stg_Rgn);

         --  TBD: If we allocated a new chunk for callee's locals,
         --      we need to reclaim it or at least not lose track of it.
      end Execute_Nested_Block;

      ----------------------
      -- Release_Lock_Obj --
      ----------------------

      procedure Release_Lock_Obj (Lock_Obj : Lock_Obj_Index;
        Obj_Stg_Rgn : Stg_Rgn_Ptr; Server_Index : Thread_Server_Index) is
      --  Release lock object for use by other objects.
      --  Obj_Stg_Rgn is the storage region where the object resides;
      --  Server_Index is the index of the server doing the release.
         use Thread_Manager_Data;
         Existing_Lock_Info : Lock_Obj_Info renames Lock_Obj_Table (Lock_Obj);
         Info               : Server_Info renames
                                Server_Info_Array (Server_Index);
      begin
         pragma Assert (Existing_Lock_Info.Index = Lock_Obj);
         --  Should be pointing at itself

         pragma Assert (not Lock_Exit_Requested (Lock_Obj));
         --  This flag should have been cleared.

         --  Set dequeue routine back to null
         Object_Locks.Set_Dequeue_Routine (Existing_Lock_Info.Lock.all, null);

         --  Link into front of server's chain of free locks
         Existing_Lock_Info.Index := Info.Free_Lock_Obj;
         Info.Free_Lock_Obj       := Lock_Obj;
         if Debug_Threading then
            Put_Line (" Release lock #" & Lock_Obj_Index'Image (Lock_Obj));
         end if;
      end Release_Lock_Obj;

   end Locked_And_Queued;
   use Locked_And_Queued;

   ------------------
   -- Object_Locks --
   ------------------

   package body Object_Locks is

      -----------------
      -- Dequeue_Tcb --
      -----------------

      procedure Dequeue_Tcb
        (Lock              : in out Object_Lock;
         Server_Index      : Thread_Server_Index;
         Tcb_To_Dequeue    : Word_Ptr;
         Thread_Was_Queued : out Boolean) is
      begin
         --  Presume it was not in fact still queued
         Thread_Was_Queued := False;

         --  Do the dequeue while holding the lock
         Acquire (Lock.Actual_Lock);

         if Tcb_Was_Queued (Tcb_To_Dequeue) then
            --  Still on the queue, dequeue it now
            if Debug_Threading or Debug_Kill then
               Put_Line (" Dequeuing Tcb at " &
                 Hex_Image (Tcb_To_Dequeue));
            end if;

            if Lock.Dequeue_Routine /= null then
               --  Use special dequeue routine, which will dequeue
               --  the tcb from the appropriate internal queue(s).
               Lock.Dequeue_Routine (Tcb_To_Dequeue, Thread_Was_Queued);
            else
               --  Remove tcb from lock's queue of waiting callers.
               declare
                  Prev_Tcb      : constant Word_Ptr :=
                                    Prev_Waiting_Tcb (Tcb_To_Dequeue);
                  Following_Tcb : constant Word_Ptr :=
                                    Next_Waiting_Tcb (Tcb_To_Dequeue);
               begin
                  if Prev_Tcb = null then
                     --  Was first item on the queue
                     Lock.Queue := Following_Tcb;
                  else
                     Set_Next_Waiting_Tcb (Prev_Tcb, Following_Tcb);
                  end if;

                  if Following_Tcb = null then
                     --  Was last item
                     Lock.Queue_Last := Prev_Tcb;
                  else
                     Set_Prev_Waiting_Tcb (Following_Tcb, Prev_Tcb);
                  end if;
               end;

               Thread_Was_Queued := True;

               --  Indicate thread no longer on lock queue
               Set_Tcb_State (Tcb_To_Dequeue, Unknown_State);
            end if;

         end if;

         --  Release the lock
         Release (Lock.Actual_Lock);

      exception
         when others =>
            Release (Lock.Actual_Lock);

            raise;
      end Dequeue_Tcb;

      ---------------------
      -- Dump_Lock_Queue --
      ---------------------

      procedure Dump_Lock_Queue (Lock : Object_Lock) is
         Tcb_On_Queue : Word_Ptr := Lock.Queue;
      begin
         while Tcb_On_Queue /= null loop
            Put_Line ("  Tcb queued on lock: " & Hex_Image (Tcb_On_Queue));
            Tcb_On_Queue := Next_Waiting_Tcb (Tcb_On_Queue);
         end loop;
      end Dump_Lock_Queue;

      -----------------
      -- Locked_Call --
      -----------------

      procedure Locked_Call
        (Lock                  : in out Object_Lock;
         Target_Routine        : Routine_Ptr;
         New_Context           : in out Exec_Context;
         Server_Index          : Thread_Server_Index;
         Locked_Param_Info     : Locked_Param_Info_Type;
         Locked_Obj_Lock_Index : Lock_Obj_Index;
         Thread_Was_Queued     : out Boolean)
      is
         Lock_Was_On_Variable : Boolean;

         Calling_Tcb          : constant Word_Ptr :=
                                  New_Context.Control_Area;

         Enclosing_Lock_Index : Lock_Obj_Index := 0;
         Start_Pc             : Code_Offset := 0;

         procedure Mark_Master_Shared;
            --  Mark master as "shared" so any further changes to it
            --  are performed under a lock (unless master is not allowed
            --  to be shared).

         procedure Mark_Master_Shared is
            Master_Of_Queued_Tcb : Master_Index :=
              Index_Of_Master (Tcb_Master_Ptr (Calling_Tcb));
            Master_Extra         : Master_Extra_Rec renames Master_Extras
              (Master_Of_Queued_Tcb);
         begin
            if not Master_Extra.Master_Never_Shared then
               --  OK to mark master as shared
               Master_Extra.Master_Is_Shared := True;
               Master_Extra.Innermost_Shared_Master :=
                 Master_Of_Queued_Tcb;
            end if;
         end Mark_Master_Shared;

      begin  --  Locked_Call

         Lock_Was_On_Variable := Locked_Param_Info.Is_Var;

         Thread_Was_Queued := False;  --  Presume by default not queued.

         --  Get the lock
         --  NOTE: We used to do the whole locked ParaSail operation by
         --       calling a protected subprogram, but this caused trouble
         --       because we encountered nested entry calls as part
         --       of waiting for a master.  Eventually we should probably
         --       go back to using a protected subprogram for efficiency,
         --       and use suspension objects as a way to signal completion
         --       in cases where that is necessary.
         Acquire (Lock.Actual_Lock);

         if Calling_Tcb /= null then
            --  Get enclosing lock
            Enclosing_Lock_Index := Large_Obj_Lock_Obj (Calling_Tcb);

            --  Remember new lock in TCB
            Set_Large_Obj_Lock_Obj (Calling_Tcb, Locked_Obj_Lock_Index);

            --  Get start PC.  Queuing only allowed if Start_PC = 1
            Start_Pc := Tcb_Start_Pc (Calling_Tcb);
         end if;

         --  Record enclosing lock, if any
         Enclosing_Lock (Locked_Obj_Lock_Index) := Enclosing_Lock_Index;

         if Debug_Threading then
            Thread_Manager.Dump_Locks;
         end if;

         --  Check if there is an internal precondition,
         --  i.e., a dequeue condition.
         --  If so, and it evaluates to false, then put call
         --  on a queue and return, at which point we will
         --  go and find another pico-thread to serve.

         if Calling_Tcb /= null
           and then not Dequeue_Condition_Satisfied
                          (New_Context,
                           Calling_Tcb,
                           Server_Index => Server_Index,
                           Called_Routine => Target_Routine)
         then
            --  Dequeue condition is not satisified.
            --  Queue the calling TCB.

            pragma Assert (Start_Pc = Code_Index'First);

            --  Indicate thread is on a lock queue
            Set_Tcb_State (Calling_Tcb, On_Lock_Queue);

            --  Add to end of chain.
            Set_Prev_Waiting_Tcb (Calling_Tcb, Lock.Queue_Last);

            --  Null out the "next" pointer
            Set_Next_Waiting_Tcb (Calling_Tcb, null);

            if Lock.Queue_Last = null then
               --  This is the "first" (and only) thread for this server
               Lock.Queue := Calling_Tcb;
            else
               --  Set the "Next" link of the old Last thread
               Set_Next_Waiting_Tcb (Lock.Queue_Last, Calling_Tcb);
            end if;

            Lock.Queue_Last := Calling_Tcb;

            Thread_Was_Queued := True;
            Set_Tcb_Was_Queued (Calling_Tcb, True);

            --  Mark master as "shared" so any further changes to it
            --  are performed under a lock.
            Mark_Master_Shared;

         else

            --  Process this call and any others that become ready
            --  as a result of this call.
            if Debug_Calls or Debug_Threading then
               if Calling_Tcb /= null then
                  Put (" TCB at " & Hex_Image (Calling_Tcb) & " in");
               end if;

               Put_Line
                 (" locked call (inside protected op) on " &
                  Strings.To_String (Target_Routine.Name));
            end if;

            if not Target_Routine.Is_PSVM_Routine then
               --  This is a built-in routine.
               --  Call it.
               --  NOTE: We are assuming the built-in operation checks the
               --       internal precondition if not doing static checking.
               --       We are also assuming it does queuing internally
               --       and sets the TCB_Was_Queued flag if it queues
               --       the TCB rather than finishing the operation.
               if Debug_Calls or Debug_Threading then
                  Put_Line
                    ("   at " & Hex_Image (Target_Routine.Routine_Addr));
               end if;

               declare
                  Can_Be_Queued : constant Boolean :=
                    (Start_Pc = Code_Index'First);
               --  Can only be queued if this operation is all the TCB
               --  is doing; i.e. it was invoked using *Parallel_Call_Op
               begin
                  if Calling_Tcb /= null then
                     Set_Tcb_Call_Can_Be_Queued (Calling_Tcb, Can_Be_Queued);
                     Set_Tcb_Was_Queued (Calling_Tcb, False);
                     --  Initialize flags appropriately for calling an
                     --  imported operation with a lock.
                  end if;

                  Call_Compiled_Routine
                    (New_Context,
                     New_Context.Params,
                     New_Context.Enclosing_Type,
                     Target_Routine.Routine_Addr,
                     Target_Routine.Conv_Desc);

                  if Calling_Tcb /= null then
                     Thread_Was_Queued := Tcb_Was_Queued (Calling_Tcb);
                     --  Check for internal queuing by imported routine.

                     if Thread_Was_Queued then
                        --  Mark master as shared since the thread will
                        --  likely be woken up by some other server task.
                        Mark_Master_Shared;
                     end if;

                  end if;

                  if Debug_Calls or Debug_Threading then
                     if Calling_Tcb /= null then
                        Put (" TCB at " & Hex_Image (Calling_Tcb));
                     end if;

                     Put_Line
                       (" returning from locked call on " &
                        Strings.To_String (Target_Routine.Name) & " at " &
                        Hex_Image (Target_Routine.Routine_Addr) &
                        ", imported routine #" &
                        Routine_Index'Image (Target_Routine.Index) &
                        ", TCB_Was_Queued = " &
                        Boolean'Image (Thread_Was_Queued));
                     Put_Line
                       (" (param area, 0) =" &
                        Hex_Image
                           (Fetch_Word (New_Context,
                                          (Param_Area, 0, No_VM_Obj_Id))));
                  end if;
               end;
            else
               --  Recurse to execute the operation
               Execute
                 (Instructions => Target_Routine,
                  Start_Pc => Code_Index'First,
                  Context => New_Context,
                  Thread_Was_Queued => Thread_Was_Queued,
                  Server_Index => Server_Index);

               pragma Assert (not Thread_Was_Queued);

               if Calling_Tcb /= null then
                  Set_Tcb_Was_Queued (Calling_Tcb, False);
               end if;

               if Debug_Calls or Debug_Threading then
                  if Calling_Tcb /= null then
                     Put (" TCB at " & Hex_Image (Calling_Tcb));
                  end if;

                  Put_Line
                    (" returning from locked call on " &
                     Strings.To_String (Target_Routine.Name) &
                     ", TCB_Was_Queued = " &
                     Boolean'Image (Thread_Was_Queued));
                  Dump_Param_Decls (Target_Routine);
                  Dump_Param_Values
                    (Target_Routine,
                     New_Context,
                     On_Entering => False);
               end if;
            end if;

            if Lock_Was_On_Variable and then not Thread_Was_Queued then
               --  Original call was not queued, and lock was on a
               --  "var" param.
               --  Scan queue until it is empty or until everything
               --  has been found to have an unsatisfied dequeue condition.
               --  TBD: We might want to do a "Finish_Thread" on the
               --      original Calling_TCB before scanning the queue.
               --      Possible reasons not to: we are holding a lock
               --      so we want to give priority to work that needs
               --      to be done while the lock is held.  But presuming
               --      there are other servers, they might be idle.
               --      Another reason: we would need to prevent caller
               --      of Locked_Call from calling Finish_Thread.
               while Lock.Queue /= null loop
                  --  There is at least one Queued TCB
                  --  Look at its dequeue condition
                  declare
                     Next_Tcb : Word_Ptr := Lock.Queue;
                  begin
                     if Debug_Calls or Debug_Threading then
                        Put_Line
                          (" scanning queue (inside protected op) " &
                           "after call on " &
                           Strings.To_String (Target_Routine.Name));
                     end if;

                     --  Look for a satisfied dequeue condition.
                     while Next_Tcb /= null
                       and then not Dequeue_Condition_Satisfied
                                      (New_Context,
                                       Next_Tcb,
                                       Server_Index => Server_Index)
                     loop
                        Next_Tcb := Next_Waiting_Tcb (Next_Tcb);
                     end loop;

                     exit when Next_Tcb = null;  --  Nothing ready to execute

                     --  Found a satisfied dequeue condition
                     --  Remove call from queue and execute it
                     declare
                        Prev_Tcb : constant Word_Ptr :=
                          Prev_Waiting_Tcb (Next_Tcb);
                        Following_Tcb : constant Word_Ptr :=
                          Next_Waiting_Tcb (Next_Tcb);
                        New_Local_Area : constant Word_Ptr :=
                          Add (New_Context.Local_Area,
                           New_Context.Start_Callee_Locals);

                        New_Thread_Was_Queued : Boolean := False;

                     begin
                        if Prev_Tcb = null then
                           --  Was first item on the queue
                           Lock.Queue := Following_Tcb;
                        else
                           Set_Next_Waiting_Tcb (Prev_Tcb, Following_Tcb);
                        end if;
                        if Following_Tcb = null then
                           --  Was last item
                           Lock.Queue_Last := Prev_Tcb;
                        else
                           Set_Prev_Waiting_Tcb (Following_Tcb, Prev_Tcb);
                        end if;

                        --  Clear the Tcb_Was_Queued flag
                        Set_Tcb_Was_Queued (Next_Tcb, False);

                        if not Tcb_Exit_Requested (Next_Tcb) then
                           --  Perform processing for Next_TCB
                           --  NOTE: TCBs on the queue already have the
                           --        lock-obj field filled in properly.
                           Execute_For_Thread
                             (Next_Tcb,
                              Server_Index,
                              New_Local_Area,
                              New_Thread_Was_Queued,
                              Already_Locked => True);

                           pragma Assert (not New_Thread_Was_Queued);
                           --  We already checked its dequeue condition
                        end if;

                        --  Indicate thread is complete.
                        --  NOTE: No need to restore the lock-obj field,
                        --       since thread is now complete.
                        Finish_Thread (Server_Index, Next_Tcb);
                     exception
                        when E : others =>
                           Messages.Put_RT_Error
                             ("Internal: " &
                              "Locked_Call serving queued caller: " &
                              Ada.Exceptions.Exception_Name (E) &
                              " raised.",
                              Src_Pos => Execution_Source_Pos (Server_Index));

                           --  Finish thread that raised an exception.
                           Finish_Thread
                             (Server_Index,
                              Next_Tcb);
                     end;

                     --  Loop around to scan queue again
                  end;
               end loop;
            end if;
         end if;  --  Whether call was queued or processed

         if Calling_Tcb /= null then
            --  Restore lock in TCB unless it was queued
            if not Thread_Was_Queued then
               Set_Large_Obj_Lock_Obj (Calling_Tcb, Enclosing_Lock_Index);
            else
               --  NOTE: If it was queued, it had better not
               --  be holding a lock!
               if Enclosing_Lock_Index /= 0 then
                  Messages.Put_RT_Error
                    ("Internal: Lock already held when " &
                     "calling queued operation",
                     Src_Pos => Execution_Source_Pos (Server_Index));
               end if;
            end if;
         end if;

         --  Indicate lock not held currently.
         Enclosing_Lock (Locked_Obj_Lock_Index) := 0;

         if Lock_Exit_Requested (Locked_Obj_Lock_Index) then
            --  Tcb holding lock should be killed
            pragma Assert (not Thread_Was_Queued);
            pragma Assert (Calling_Tcb /= null);

            if Debug_Threading then
               Put_Line (" lock exit requested for lock" &
                 Lock_Obj_Index'Image (Locked_Obj_Lock_Index) &
                 ", killing tcb at " &
                 Hex_Image (Calling_Tcb));
            end if;

            Lock_Exit_Requested (Locked_Obj_Lock_Index) := False;
            Set_Tcb_Exit_Requested (Calling_Tcb, True);
         end if;

         --  Release the lock
         Release (Lock.Actual_Lock);

         if Debug_Threading then
            Thread_Manager.Dump_Locks;
         end if;
      exception
         when others =>
            --  Indicate lock not held currently.
            Enclosing_Lock (Locked_Obj_Lock_Index) := 0;

            Release (Lock.Actual_Lock);

            raise;
      end Locked_Call;

      -------------------------
      -- Set_Dequeue_Routine --
      -------------------------

      procedure Set_Dequeue_Routine
        (Lock            : in out Object_Lock;
         Dequeue_Routine : Dequeue_Routine_Ptr) is
      begin
         Lock.Dequeue_Routine := Dequeue_Routine;
      end Set_Dequeue_Routine;

   end Object_Locks;

   -------------------------
   -- PSVM_Thread_Support --
   -------------------------

   package body PSVM_Thread_Support is

      --  Local operations  --

      procedure Spawn_Unshared_Thread
        (Context       : in out Exec_Context;
         Thread_Master : Word_Ptr;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr);
      --  Spawn thread which is not available for stealing
      --  on an unshared master

      ---------------------------
      -- Spawn_Unshared_Thread --
      ---------------------------

      procedure Spawn_Unshared_Thread
        (Context       : in out Exec_Context;
         Thread_Master : Word_Ptr;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr) is
         --  Spawn thread which is not immediately available for stealing,
         --  since there is no call for shared threads.

         --  NOTE: We presume caller has made this check
         --        (we don't want to repeat it since is an atomic object)
         --  pragma Assert (Num_Shared_Threads_Needed = 0);

         Server_Index : constant Thread_Server_Index := Context.Server_Index;
         Info         : Server_Info renames Server_Info_Array (Server_Index);
         Index        : constant Master_Index :=
           Index_Of_Master (Thread_Master);
         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);

      begin  --  Spawn_Unshared_Thread

         if Debug_Threading then
            Put_Line
              ("Spawn_Unshared_Thread for Server" &
               Thread_Server_Index'Image (Server_Index) &
               ", TCB at " &
               Hex_Image (New_Tcb));
         end if;

         if not Exit_Was_Requested (Thread_Master, Spawning_Tcb) then
            --  Only spawn a new thread when the spawner isn't exiting

            if Info.Unshared_Threads.Count >= Enough_Unshared_Threads
              and then Tcb_For_Compiled_Routine (New_Tcb)
              and then Tcb_Locked_Param_Index (New_Tcb) = 0
              and then Info.Bypassed_Init_Recursion_Level <
                Max_Bypassed_Init_Recursion
            then
               --  We can just run the code, rather than creating a thread
               if Debug_Threading then
                  Put_Line
                    ("Can execute immediately instead of spawning on master" &
                       Master_Index'Image (Index));
               end if;

               --  Avoid too many levels of recursion
               Info.Bypassed_Init_Recursion_Level :=
                 Info.Bypassed_Init_Recursion_Level + 1;

               --  Execute code associated with TCB now
               Execute_Compiled_Code_Immediately (Context, New_Tcb);

               --  Restore recursion count
               Info.Bypassed_Init_Recursion_Level :=
                 Info.Bypassed_Init_Recursion_Level - 1;

               return;  --  All done
            end if;

            if Master_Extra.Master_Is_Shared then
               --  Add new Tcb to shared master
               Thread_Manager.Add_To_Shared_Master
                 (Thread_Master, Server_Index, New_Tcb);
            else
               --  Add new Tcb to (unshared) master
               Add_To_Master (Thread_Master, Server_Index, New_Tcb);
            end if;

            --  Add to unshared queue
            Add_To_Deque (Info.Unshared_Threads, New_Tcb,
              Is_Shared => False);

            --  Accumulate statistics
            if Info.Unshared_Threads.Count >
              Info.Max_Waiting_Unshared_Threads
            then
               Info.Max_Waiting_Unshared_Threads :=
                 Info.Unshared_Threads.Count;
            end if;

            if Debug_Threading then
               Put_Line
                 ("Spawn_Unshared_Thread for master" &
                  Master_Index'Image (Index));

               Dump_Masters;
               Thread_Manager.Dump_Locks;
               Thread_Manager.Dump_Thread_Tree
                 (Inner_Master => Index,
                  Outer_Master => Master_Extra.Innermost_Shared_Master,
                  Label        => "Unshared Thread Tree for server" &
                    Thread_Server_Index'Image (Server_Index));
            end if;

         end if;
      end Spawn_Unshared_Thread;

      --  Visible operations  --

      ----------------------------
      -- Execute_Locked_Call_Op --
      ----------------------------

      procedure Execute_Locked_Call_Op
        (Target_Routine     : Routine_Ptr;
         New_Context        : in out Exec_Context;
         Locked_Param_Info  : Locked_Param_Info_Type;
         Server_Index       : Thread_Server_Index;
         Thread_Was_Queued  : out Boolean)
      is
         Locked_Obj_Addr : Object_Virtual_Address :=
           Content_Of_Physical_Address
              (Locator_To_Physical_Address
                  (New_Context,
                   (Param_Area,
                    Offset_Within_Area (Locked_Param_Info.Param_Index) - 1,
                    No_VM_Obj_Id)));
         Locked_Obj_Value : Word_Type := Locked_Obj_Addr;
      begin

         if Locked_Param_Info.Is_By_Ref then
            --  Passed by ref ==> extra level of indirection
            Locked_Obj_Value := Content_Of_Physical_Address
                            (Word_To_Word_Ptr (Locked_Obj_Addr));
         end if;

         if Debug_Calls or Debug_Threading then
            Put_Line
              ("Locked call on " & Strings.To_String (Target_Routine.Name));
            Put (" locked param: (" & Natural'Image
                 (Locked_Param_Info.Param_Index));
            if Locked_Param_Info.Is_Var then
               Put (", Is_Var");
            end if;
            if Locked_Param_Info.Is_By_Ref then
               Put (", Is_By_Ref");
            end if;
            Put (", at " & Hex_Image (Locked_Obj_Value));
            Put_Line (")");
         end if;

         if Target_Routine.Is_PSVM_Routine then
            --  Check static chain only if not an import,
            --  since we don't bother initializing the static
            --  link in the local area for an imported routine.
            Check_Static_Chain (New_Context.Local_Area);
         end if;

         declare
            Locked_Obj_Lock_Index : constant Lock_Obj_Index :=
              Large_Obj_Lock_Obj (Locked_Obj_Value);
         begin
            if Locked_Obj_Lock_Index = 0 then
               --  Weird.  No lock on object
               Messages.Put_RT_Error
                 ("Internal: Locked call on " &
                  Strings.To_String (Target_Routine.Name) &
                  " with Locked_Param_Index =" &
                  Integer'Image (Locked_Param_Info.Param_Index) &
                  " but object at " &
                  Hex_Image (Locked_Obj_Value) &
                  " does not have a lock",
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
            else
               --  Get lock object and make the locked call on it
               declare
                  Locked_Obj_Lock : constant Object_Lock_Ptr :=
                    Lock_Obj_Table (Locked_Obj_Lock_Index).Lock;
               begin
                  --  Make the locked call
                  Locked_Call
                    (Locked_Obj_Lock.all,
                     Target_Routine,
                     New_Context,
                     Server_Index,
                     Locked_Param_Info,
                     Locked_Obj_Lock_Index,
                     Thread_Was_Queued);

                  if Locked_Param_Info.Is_Var
                    and then Locked_Param_Info.Is_By_Ref
                  then
                     --  by-ref variable, object might have moved
                     declare
                        New_Locked_Obj_Value : constant Word_Type :=
                          Content_Of_Physical_Address
                            (Word_To_Word_Ptr (Locked_Obj_Addr));
                     begin
                        if New_Locked_Obj_Value /= Locked_Obj_Value
                          and then Large_Obj_Lock_Obj (New_Locked_Obj_Value) /=
                                      Locked_Obj_Lock_Index
                        then
                           --  Restore the lock index lost by the call
                           --  as a side effect of a whole-object assignment.
                           if Debug_Threading then
                              Put_Line (" Locked index of " &
                                Hex_Image (Locked_Obj_Value) & " was " &
                                Lock_Obj_Index'Image
                                  (Locked_Obj_Lock_Index) & "; new obj " &
                                Hex_Image (New_Locked_Obj_Value) & " has " &
                                  Lock_Obj_Index'Image
                                    (Large_Obj_Lock_Obj
                                      (New_Locked_Obj_Value)));
                           end if;

                           Set_Large_Obj_Lock_Obj
                             (New_Locked_Obj_Value, Locked_Obj_Lock_Index);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end Execute_Locked_Call_Op;

      ------------------------
      -- Execute_For_Thread --
      ------------------------

      procedure Execute_For_Thread
        (New_Tcb           : Word_Ptr;
         Server_Index      : Thread_Server_Index;
         New_Local_Area    : Word_Ptr;
         Thread_Was_Queued : out Boolean;
         Already_Locked    : Boolean := False)
      --  Execute code specified in thread control block and then mark thread
      --  as complete. If Thread_Was_Queued upon return, then thread is
      --  *not* complete and instead is waiting for a dequeue condition to
      --  be satisfied.
      is
         Target_Routine : Routine_Ptr;
         Params         : constant Word_Ptr :=
                            Add (New_Tcb, Tcb_Param_List_Offset);
         Static_Link    : constant Word_Ptr := Tcb_Static_Link (New_Tcb);

         Thread_Master        : constant Word_Ptr :=
                                  Tcb_Master_Ptr (New_Tcb);
         Thread_Master_Index  : constant Master_Index :=
                                  Index_Of_Master (Thread_Master);
         Locked_Param         : constant Locked_Param_Info_Type :=
                                  Tcb_Locked_Param_Info (New_Tcb);

         New_Context : Exec_Context :=
           (Local_Null          => Null_Virtual_Address, -- init'ed below
            Enclosing_Type      => Get_Enclosing_Type (Static_Link),
            Local_Stg_Rgn       => null,  --  Initialized below
            Control_Area        => New_Tcb,
            Open_Master         => null,
            Server_Index        => Server_Index,
            Params              => Params,
            Local_Area          => New_Local_Area,
            Local_Area_Length   => Tcb_Local_Area_Length (New_Tcb),
            Start_Callee_Locals => Tcb_Start_Callee_Locals (New_Tcb));

         Compiled_Routine : aliased Routine (Is_PSVM_Routine => False);
         Is_Compiled : constant Boolean := Tcb_For_Compiled_Routine (New_Tcb);
         Info        : Server_Info renames Server_Info_Array (Server_Index);
         Orig_Depth  : constant Natural := Info.Num_Threads_In_Process;
      begin
         if not Is_Compiled then
            Target_Routine := Nth_Routine (Tcb_Routine_Index (New_Tcb));

            --  Allocate a new local storage region
            New_Context.Local_Stg_Rgn := Get_New_Local_Stg_Rgn
                (New_Local_Area => New_Local_Area,
                 Server_Index => Server_Index);

            New_Context.Local_Null := New_Context.Local_Stg_Rgn.Null_Value;

            Check_Static_Chain (Static_Link);

         end if;

         --  Initialize static link
         --  TBD: "Execute" gets static link out of this location.
         --       Better would be to pass it as a separate parameter,
         --       though it would have to be passed through Execute_Locked_Call
         --       as well.
         Init_Static_Link (New_Local_Area, Static_Link);

         Thread_Was_Queued := False;  --  By default, assume not queued

         --  Presume thread will start running
         Set_Tcb_State (New_Tcb, Running);

         if Thread_Master_Index <= 0 then
            Messages.Put_RT_Error
              ("Internal: " &
               "Execute_For_Thread: Master_Index <= 0 " &
               "for TCB at " &
               Hex_Image (New_Tcb),
               Src_Pos => Execution_Source_Pos (Server_Index));
            raise Program_Error;
         end if;

         if Debug_Threading then
            Put_Line
              ("Execute_For_Thread: by server" &
               Thread_Server_Index'Image (Server_Index) &
               " for TCB at " &
               Hex_Image (New_Tcb) &
               ", master" &
               Master_Index'Image (Thread_Master_Index));
         end if;

         Info.Num_Threads_In_Process := Orig_Depth + 1;

         if Info.Num_Threads_In_Process > Info.Max_Threads_In_Process then
            --  Number of simultaneous threads in process on server just
            --  hit a new maximum.
            Info.Max_Threads_In_Process := Info.Num_Threads_In_Process;

            if Debug_Stack then
               Put_Line ("--> Server" &
                 Thread_Server_Index'Image (Server_Index) & " now has" &
                 Natural'Image (Info.Num_Threads_In_Process) &
                 " threads in process (tcb:" & Hex_Image (New_Tcb) & ").");
            end if;
         end if;

         if Locked_Param.Param_Index > 0 and then not Already_Locked then
            if Debug_Calls or Debug_Threading then
               Put_Line
                 (" with lock on param #" &
                  Natural'Image (Locked_Param.Param_Index));
            end if;

            if Is_Compiled then
               Compiled_Routine.Routine_Addr := Tcb_Code_Addr (New_Tcb);
               Compiled_Routine.Conv_Desc := Tcb_Conv_Desc (New_Tcb);
               Compiled_Routine.Internal_Precond_Addr :=
                  Tcb_Internal_Precond_Addr (New_Tcb);
               Compiled_Routine.Is_Compiled_Routine := True;
               Target_Routine := Compiled_Routine'Unchecked_Access;
            end if;

               --  Compiled code will create its own local stg rgn if necessary
            Execute_Locked_Call_Op
              (Target_Routine,
               New_Context,
               Locked_Param_Info  => Locked_Param,
               Server_Index       => Server_Index,
               Thread_Was_Queued  => Thread_Was_Queued);

         elsif Is_Compiled then
            if Debug_Threading then
               Put_Line (" about to use call compiled code to invoke tcb " &
                 Hex_Image (New_Tcb) & ", server index" &
                 Thread_Server_Index'Image (Server_Index));
            end if;

            begin  --  exception handler
               --  do a convention-specific call on compiled code
               Call_Compiled_Routine
                 (Context     => New_Context,
                  Params      => New_Context.Params,
                  Static_Link => To_Non_Op_Map_Type_Desc (Static_Link),
                  Code_Addr   => Tcb_Code_Addr (New_Tcb),
                  Conv_Desc   => Tcb_Conv_Desc (New_Tcb));
            exception
               when Propagate_To_Compiled_Handler =>
                  declare
                     Handler_Info : Handler_Info_Rec renames
                       Master_Extras (Index_Of_Master
                         (Tcb_Master_Ptr (New_Tcb))).Handler_Info;
                  begin
                     if Handler_Info.Kind = Not_A_Handler then
                        raise;
                     end if;
                     --  This exception has served its purpose.
                  end;
            end;
         else
            Execute
              (Instructions      => Target_Routine,
               Start_Pc          => Tcb_Start_Pc (New_Tcb),
               Context           => New_Context,
               Thread_Was_Queued => Thread_Was_Queued,
               Server_Index      => Server_Index);
         end if;

         --  Restore thread depth
         --  NOTE: We do this even if the thread was queued, since
         --        a queued thread doesn't actually occupy any stack space.
         Info.Num_Threads_In_Process := Orig_Depth;

         if not Is_Compiled then
            pragma Assert (New_Context.Local_Stg_Rgn /= null);
            Release_Stg_Rgn (New_Context.Local_Stg_Rgn);
         end if;

      exception
         when E : others =>

            --  Restore thread depth
            Info.Num_Threads_In_Process := Orig_Depth;

            Messages.Put_RT_Error
              ("Execute_For_Thread: " &
               Ada.Exceptions.Exception_Name (E) &
               " raised.",
               Src_Pos => Execution_Source_Pos (Server_Index));

            Dump_Stack (Server_Info_Array (Server_Index).Current_State,
              Num_Stack_Frames => 1, Use_Cur_Err => True);

            --  Invoke the debugging console if available.
            Invoke_Debug_Console (New_Context, Reason => Internal_Failure);

            raise;
      end Execute_For_Thread;

      procedure Execute_Compiled_Code_Immediately
        (Context : in out Exec_Context; New_Tcb : Word_Ptr) is
      --  Execute compiled code specified in thread control block,
      --  piggybacking on existing context rather than spawning a new thread.
      --  No locking param allowed.

         --  No locks allowed; code is compiled
         pragma Assert (Tcb_Locked_Param_Index (New_Tcb) = 0);
         pragma Assert (Tcb_For_Compiled_Routine (New_Tcb));

         Old_Tcb     : constant Word_Ptr := Context.Control_Area;
         Static_Link : constant Word_Ptr := Tcb_Static_Link (New_Tcb);
         Params      : constant Word_Ptr :=
                            Add (New_Tcb, Tcb_Param_List_Offset);

      begin

         Context.Control_Area := New_Tcb;

         --  Call compiled code using appropriate calling convention
         Call_Compiled_Routine
           (Context, Params,
            To_Non_Op_Map_Type_Desc (Static_Link),
            Tcb_Code_Addr (New_Tcb), Tcb_Conv_Desc (New_Tcb));

         --  Restore Control_Area
         Context.Control_Area := Old_Tcb;

         if Debug_Statistics then
            --  Statistics on number of bypassed thread inits
            declare
               Info : Server_Info renames
                 Server_Info_Array (Context.Server_Index);
            begin
               Info.Num_Bypassed_Thread_Initiations :=
                 Info.Num_Bypassed_Thread_Initiations + 1;
            end;
         end if;

      exception
         when others =>
            --  Restore Control_Area and re-raise
            Context.Control_Area := Old_Tcb;
            raise;

      end Execute_Compiled_Code_Immediately;

      function Exit_Was_Requested
        (Thread_Master : Word_Ptr;
         Current_Tcb   : Word_Ptr) return Boolean is
         --  Return True if Exit has been requested for given master/tcb
      begin
         if Tcb_Exit_Requested (Current_Tcb) then
            --  Tcb has been directly requested to exit
            return True;
         elsif Is_Shut_Down then
            return True;
         else
            --  Check innermost shared master for indicator
            declare
               Index        : constant Master_Index :=
                 Index_Of_Master (Thread_Master);
               Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
               Shared_Index : constant Master_Index :=
                 Master_Extra.Innermost_Shared_Master;
            begin
               if Shared_Index /= 0 then
                  --  We are inside a shared master; check for flag there
                  declare
                     Shared_Master : Master_Extra_Rec renames
                       Master_Extras (Shared_Index);
                  begin
                     --  Return True if Exit has been requested for threads
                     --  below this shared master, and Current_Tcb is
                     --  not the thread that initiated the exit request.
                     return Master_Exit_Requested
                         (Shared_Master.Master_Address)
                       and then
                         Current_Tcb /= Shared_Master.Exiting_Tcb;
                  end;
               else
                  --  Not inside a shared master, so Tcb_Exit_Requested is
                  --  all that we need to check.
                  return False;
               end if;
            end;
         end if;
      end Exit_Was_Requested;

      -------------------
      -- Finish_Thread --
      -------------------

      procedure Finish_Thread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr) is
      --  Indicate that processing for given thread is complete
         Master_Extra : Master_Extra_Rec renames
           Master_Extras (Index_Of_Master (Tcb_Master_Ptr (Finished_Tcb)));
      begin
         if not Master_Extra.Master_Is_Shared then
            --  Unshared master; no need for a protected operation
            Finish_Subthread (Server_Index, Finished_Tcb);
         else
            --  Just pass the buck to the Thread_Manager
            Thread_Manager.Finish_Thread (Server_Index, Finished_Tcb);
         end if;
      end Finish_Thread;

      -------------------------
      -- Get_Unshared_Thread --
      -------------------------

      procedure Get_Unshared_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr;
         Subthread_Of_Master : Master_Index := 0) is
      --  Get a thread from the given server's unshared queue.
      --  Return null Tcb_To_Run if there isn't one.
      --  If Subthread_Of_Master is > 0, then don't return a "queuing" Tcb
      --  unless it is a subthread of the specified master.
         Info : Server_Info renames Server_Info_Array (Server_Index);
      begin
         if Debug_Threading then
            if Subthread_Of_Master > 0 then
               Put_Line
                 ("Get_Unshared_Thread for server" &
                  Thread_Server_Index'Image (Server_Index) &
                  " that is subthread of master" &
                  Master_Index'Image (Subthread_Of_Master));
            else
               Put_Line
                 ("Get_Unshared_Thread for server" &
                  Thread_Server_Index'Image (Server_Index));
            end if;
            Flush;
         end if;

         Tcb_To_Run := Info.Unshared_Threads.Last_Thread;

         if Subthread_Of_Master > 0 then
            --  Check whether Tcb uses queueing, and if so
            --  keep looking for TCB that is subthread of given master.
            while Tcb_To_Run /= null
              and then Tcb_Uses_Queuing (Tcb_To_Run)
              and then not Is_Subthread (Tcb_To_Run, Subthread_Of_Master)
            loop
               Tcb_To_Run := Prev_Waiting_Tcb (Tcb_To_Run);
            end loop;
         end if;

         if Tcb_To_Run /= null then
            --  Remove Tcb_To_Run from the server's unshared deque
            Remove_From_Deque
              (Deque             => Info.Unshared_Threads,
               Tcb_To_Be_Removed => Tcb_To_Run);

            --  Remember most recent active thread for this server
            Info.Last_Active_Thread := Tcb_To_Run;
            --  NOTE: This is mostly for debugging

            --  Indicate no longer on server queue
            Set_Tcb_State (Tcb_To_Run, Unknown_State);

            --  Accumulate statistics
            Info.Num_Unshared_Thread_Initiations :=
              Info.Num_Unshared_Thread_Initiations + 1;

            Info.Num_Waiting_Unshared_Summed :=
              Info.Num_Waiting_Unshared_Summed +
                Longest_Natural (Info.Unshared_Threads.Count);

            if Debug_Threading then
               declare
                  Thread_Master : constant Word_Ptr :=
                    Tcb_Master_Ptr (Tcb_To_Run);
                  Index : constant Master_Index :=
                    Index_Of_Master (Thread_Master);
               begin
                  Put_Line
                    ("Get_Unshared_Thread for TCB at " &
                     Hex_Image (Tcb_To_Run) &
                     ", master" &
                     Master_Index'Image (Index));

                  if Index <= 0 then
                     Put_Line ("Get_Unshared_Thread -- Master_Index <= 0");
                     raise Program_Error;
                  end if;
               end;
            end if;
         end if;

      end Get_Unshared_Thread;

      ----------------
      -- Get_Thread --
      ----------------

      procedure Get_Thread
        (Server_Index : Thread_Server_Index;
         Tcb_To_Run   : out Word_Ptr) is
      --  Get a thread that is waiting to be executed.
      --  Look first on specified server's queue.
      begin
         --  First check for an unshared thread
         Get_Unshared_Thread (Server_Index, Tcb_To_Run);

         if Tcb_To_Run = null then
            --  No unshared threads, pass the buck to the Thread_Manager
            Thread_Manager.Get_Thread (Server_Index, Tcb_To_Run);
         end if;
      end Get_Thread;

      ---------------------
      -- Prepare_To_Exit --
      ---------------------

      function Prepare_To_Exit
        (Thread_Master    : Word_Ptr;
         Server_Index     : Thread_Server_Index;
         Holding_Lock_Obj : Lock_Obj_Index;
         Exiting_Tcb      : Word_Ptr;
         Raising_Excep    : Boolean := False) return Boolean
      --  Prepare to exit specified master. If the prepare-to-exit on the
      --  master succeeds, then return True. If it fails because some other
      --  picothread has already performed a prepare-to-exit on the master or
      --  some enclosing master, then return False. If returning False, outcome
      --  has been set on enclosing master(s) so appropriate "abrupt" exit will
      --  occur.
      is
         Succeeded : Boolean := False;
      begin
         if Debug_Threading then
            Put_Line (" Thread tree before Prepare_To_Exit:");
            Thread_Manager.Dump_Thread_Tree (Label =>
              "Thread tree before Prepare_To_Exit");
         end if;

         --  Prepare to exit the given master
         Thread_Manager.Prepare_To_Exit
           (Thread_Master,
            Server_Index,
            Holding_Lock_Obj,
            Exiting_Tcb,
            Succeeded,
            Raising_Excep);

         --  Prepare-to-exit either succeeded or failed.

         if Debug_Threading and then Succeeded then
            Thread_Manager.Dump_Thread_Tree (Label =>
              "Thread tree after successful Prepare_To_Exit");
         end if;

         --  Return success indicator.
         return Succeeded;
      end Prepare_To_Exit;

      ------------------
      -- Spawn_Thread --
      ------------------

      procedure Spawn_Thread
        (Context       : in out Exec_Context;
         Thread_Master : Word_Ptr;
         New_Tcb       : Word_Ptr;
         Spawning_Tcb  : Word_Ptr)
      is
         Lock_Held_By_Tcb : constant Lock_Obj_Index :=
                              Large_Obj_Lock_Obj (New_Tcb);
         Lock_Is_Held     : constant Boolean := Lock_Held_By_Tcb > 0;
         Index            : constant Master_Index :=
           Index_Of_Master (Thread_Master);
         Master_Extra     : Master_Extra_Rec renames Master_Extras (Index);
      begin
         --  Finish initialization of control areas
         Set_Tcb_Master_Ptr (New_Tcb, Thread_Master);
         if Debug_Threading then
            Put_Line
              (" Spawn thread for master" & Master_Index'Image (Index));
         end if;

         --  Initialize links
         Set_Next_Waiting_Tcb (New_Tcb, null);
         Set_Prev_Waiting_Tcb (New_Tcb, null);
         --  chain of waiting threads

         if Master_Extra.Subthread_Count = Uninit_Thread_Count then
            --  This is the first thread for this master
            pragma Assert (Master_Extra.Enclosing_Master <= Last_Master);

            --  Copy the Innermost_Shared_Master indicator from encloser
            --  TBD: Why don't we do this in Initialize_Master?
            Master_Extra.Innermost_Shared_Master :=
              Master_Extras (Master_Extra.Enclosing_Master).
                Innermost_Shared_Master;

            if Debug_Threading then
               Put_Line (" Spawning first thread on master" &
                 Master_Index'Image (Index) & ", enclosed by master" &
                 Master_Index'Image (Master_Extra.Enclosing_Master));
            end if;

         end if;

         if (not Master_Extra.Master_Is_Shared
               or else
             not Tcb_Uses_Queuing (New_Tcb))
           and then Thread_Manager_Data.Num_Shared_Threads_Needed = 0
         then
            --  Create an unshared thread
            Spawn_Unshared_Thread (Context, Thread_Master, New_Tcb,
              Spawning_Tcb => Spawning_Tcb);
         else
            --  Pass the buck to the thread manager to create a shared thread
            Thread_Manager.Spawn_Shared_Thread
              (Thread_Master,
               Context.Server_Index,
               New_Tcb,
               Spawning_Tcb => Spawning_Tcb);
         end if;

         if Tcb_Exit_Requested (Spawning_Tcb) then
            --  An exit has been requested, so we should
            --  wait for other threads of same master, if we
            --  are the enclosing thread rather than a sibling thread,
            --  and then in any case, finish the current thread.
            --  NOTE: A sibling might not have the same master,
            --        since it might be due to a continue from
            --        a nested block, so we can't use the
            --        master of spawner to decide whether or not
            --        this is a sibling thread.
            if Debug_Threading then
               Put_Line
                 (" Exit requested for thread at " &
                    Hex_Image (Server_Info_Array
                      (Context.Server_Index).Last_Active_Thread));
            end if;
         end if;

         --  Yield processor so thread can be served (unless holding a lock).
         --  NOTE: This would not be necessary in a true multiprocessor.
         if Yield_When_Spawning_Thread and then not Lock_Is_Held then
            if Debug_Threading then
               Put_Line (" About to delay 0.0 after spawning thread " &
                 "for master" & Master_Index'Image (Index));
               Flush;
            end if;
            delay 0.0;
         end if;
      end Spawn_Thread;

      ----------------------
      -- Wait_For_Threads --
      ----------------------

      procedure Wait_For_Threads
        (Context          : in out Exec_Context;
         Thread_Master    : Word_Ptr;
         Holding_Lock_Obj : Lock_Obj_Index;
         New_Local_Area   : Word_Ptr) is

         Server_Index : constant Thread_Server_Index := Context.Server_Index;
         Tcb_Waiting : constant Word_Ptr := Context.Control_Area;
         Index : constant Master_Index := Index_Of_Master (Thread_Master);
         pragma Assert (Index /= 0);

         Master_Extra    : Master_Extra_Rec renames Master_Extras (Index);
         Subthread_Count : constant Thread_Count :=
           Master_Extra.Subthread_Count;
      begin
         --  Statistics
         Num_Masters := Num_Masters + 1;

         if Subthread_Count = Uninit_Thread_Count then
            --  We are waiting on a master before any subthreads were spawned
            if Debug_Threading then
               Put_Line ("Wait_For_Threads: Master" &
                 Master_Index'Image (Index) & " Subthread_Count is uninit'ed"
                 & "; returning immediately");
               Flush;
            end if;

            --  Restore Tcb_Waiting's master if has been updated to point
            --  to master being awaited (set in Initialize_Master).
            if Tcb_Master_Ptr (Tcb_Waiting) = Thread_Master then
               Set_Tcb_Master_Ptr (Tcb_Waiting,
                 Master_Extras
                   (Master_Extra.Enclosing_Master).Master_Address);

               if Debug_Threading then
                  Put_Line ("Wait_For_Threads: Resetting master to" &
                    Master_Index'Image
                      (Index_Of_Master (Tcb_Master_Ptr (Tcb_Waiting))));
                  Flush;
               end if;
            end if;

         else
            --  Wait for master subthread count to go to zero;
            --  service other runnable threads while waiting.

            --  Indicate thread state
            Set_Tcb_State (Tcb_Waiting, Waiting_For_Master);

            loop
               declare
                  Tcb_To_Run : Word_Ptr := null;
                  Thread_Was_Queued : Boolean := False;
               begin

                  --  Wait for threads, or get one to serve
                  if not Master_Extra.Master_Is_Shared then
                     --  The master is not shared, first see if we are done
                     --  with master.  If not, look for an unshared thread.

                     if Master_Extra.Subthread_Count = 0 then
                        --  Statistics
                        Num_Active_Masters := Num_Active_Masters - 1;
                        if Master_Extra.Is_Being_Awaited then
                           Num_Waiting_For_Subthreads :=
                             Num_Waiting_For_Subthreads - 1;
                        end if;

                        exit;  --  Done with master
                     end if;

                     --  Master has unfinished subthreads

                     --  See if there are any unshared threads;
                     --  make sure they are subthreads of given master
                     --  if they use queuing.
                     Get_Unshared_Thread (Server_Index, Tcb_To_Run,
                       Subthread_Of_Master => Index);
                  end if;

                  if Tcb_To_Run = null then
                     --  There are no unshared threads, or the master is
                     --  shared, so use the Thread_Manager.
                     Thread_Manager.Get_Thread_Or_Wait_For_Threads
                       (Thread_Master,
                        Server_Index,
                        Holding_Lock_Obj,
                        Tcb_Waiting,
                        Tcb_To_Run);

                     exit when Tcb_To_Run = null;
                     --  Master is complete
                  end if;

                  --  We have a thread to serve while waiting for master

                  if Debug_Threading then
                     declare
                        Thread_Master_Of_Thread : constant
                          Word_Ptr :=
                            Tcb_Master_Ptr (Tcb_To_Run);
                        Master_Index_Of_Thread : constant Master_Index :=
                          Index_Of_Master (Thread_Master_Of_Thread);
                     begin
                        Ada.Text_IO.Put_Line
                          ("Wait_For_Threads (master" &
                           Master_Index'Image (Index) &
                           "): serving a TCB at " &
                           Hex_Image (Tcb_To_Run) &
                           ", thread's master =" &
                           Master_Index'Image (Master_Index_Of_Thread));
                     end;
                  end if;

                  if not Tcb_Exit_Requested (Tcb_To_Run) then
                     --  Perform processing for thread
                     Execute_For_Thread
                       (Tcb_To_Run,
                        Server_Index,
                        New_Local_Area,
                        Thread_Was_Queued);
                  end if;

                  if not Thread_Was_Queued then
                     --  Indicate we are done with thread.
                     Finish_Thread (Server_Index, Tcb_To_Run);
                  end if;
               exception
                  when E : others =>
                     Messages.Put_RT_Error
                       ("Internal: " &
                        "Wait_For_Threads: " &
                        Ada.Exceptions.Exception_Name (E) &
                        " raised.",
                        Src_Pos => Execution_Source_Pos (Server_Index));

                     if Tcb_To_Run /= null
                       and then not Thread_Was_Queued
                     then
                        --  Indicate we are done with thread
                        --  which raised an exception.
                        Finish_Thread
                          (Server_Index, Tcb_To_Run);
                     end if;

                     if Tcb_To_Run = null then
                        --  OK to propagate if all threads are complete.
                        raise;
                     end if;
               end;
            end loop;
            --  Threads associated with given master now complete

            --  Restore Tcb_Waiting's master, after verifying it still
            --  points to master being awaited (set in Initialize_Master).
            pragma Assert (Tcb_Master_Ptr (Tcb_Waiting) = Thread_Master);
            Set_Tcb_Master_Ptr (Tcb_Waiting,
              Master_Extras
                (Master_Extra.Enclosing_Master).Master_Address);

            --  Indicate thread is running again
            Set_Tcb_State (Tcb_Waiting, Running);

         end if;

         if Master_Extra.Handler_Info.Kind /= Not_A_Handler
           and then
            Master_Extra.Excep_Occurrence /= Null_Virtual_Address
         then
            Messages.Put_Warning
              ("Exception raised, about to invoke handler.",
               Src_Pos => Execution_Source_Pos);
            Dump_Obj (Master_Extra.Excep_Occurrence);

            case Master_Extra.Handler_Info.Kind is
               when Compiled_Handler =>
                  Call_Compiled_Routine
                    (Context     => Context,
                     Params      =>
                       Master_Extra.Excep_Occurrence'Access,
                     Static_Link =>
                       To_Non_Op_Map_Type_Desc (Context.Local_Area),
                     Code_Addr   =>
                       Master_Extra.Handler_Info.Handler_Addr,
                     Conv_Desc   =>
                       Master_Extra.Handler_Info.Handler_Desc);

               when PSVM_Handler =>
                  Execute_Nested_Block
                    (Context            => Context,
                     Instructions       =>
                       Master_Extra.Handler_Info.Handler_Routine,
                     Params_Address     =>
                       Master_Extra.Excep_Occurrence'Access,
                     Static_Link        => Context.Local_Area,
                     Code_Block         =>
                       Master_Extra.Handler_Info.Handler_Block.all,
                     Server_Index       => Server_Index,
                     Base_For_Pc_Offset =>
                       Master_Extra.Handler_Info.Handler_Start -
                         Master_Extra.Handler_Info.Handler_Block.Pc_Offset);

               when Not_A_Handler =>
                  --  Should not occur (precluded above).
                  null;
            end case;

            --  Reset information
            Master_Extra.Handler_Info := (Kind => Not_A_Handler);
            Master_Extra.Excep_Occurrence := Null_Virtual_Address;
         end if;

         --  Statistics
         if Master_Extra.Master_Is_Shared then
            Num_Shared_Masters := Num_Shared_Masters + 1;
         end if;

         --  Zero out the stored master index
         Set_Index_Of_Master (Thread_Master, 0);

         --  Free up master index and extra info
         Release_Master (Index, Server_Index);
      end Wait_For_Threads;

   end PSVM_Thread_Support;

   -----------------------
   -- Thread_Scheduling --
   -----------------------

   package body Thread_Scheduling is

      function To_Tcb_Ptr (Tcb_Addr : Object_Address) return Tcb_Ptr;
      --  Return pointer to TCB at given address
      pragma Inline (To_Tcb_Ptr);

      function To_Tcb_Ptr (Tcb_Addr : Object_Address) return Tcb_Ptr is
      begin
         return Addr_To_Tcb_Ptr
           (Tcb_Addr.Enclosing_Chunk.Data (Tcb_Addr.Offset)'Address);
      end To_Tcb_Ptr;

      function To_Tcb_Ptr
        (Tcb_Virt_Addr : Word_Ptr) return Tcb_Ptr
        renames Word_To_Tcb_Ptr;

      ---------------------------  Tcb operations  ---------------------------

      ------------------
      -- Add_To_Deque --
      ------------------

      procedure Add_To_Deque
        (Deque   : in out Thread_Deque_Head;
         New_Tcb : Word_Ptr;
         Is_Shared : Boolean) is
         --  Add Tcb to end of deque;
         --  Is_Shared indicates whether queue is shared
      begin
         Set_Prev_Waiting_Tcb
           (New_Tcb, Deque.Last_Thread);

         --  Null out the "next" pointer
         Set_Next_Waiting_Tcb (New_Tcb, null);

         if Deque.Last_Thread = null then
            --  This is the "first" (and only) thread on the Deque
            Deque.First_Thread := New_Tcb;
         else
            --  Set the "Next" link of the old Last thread
            Set_Next_Waiting_Tcb (Deque.Last_Thread, New_Tcb);
         end if;

         Deque.Last_Thread := New_Tcb;

         --  Keep count of threads on deque.
         Deque.Count       := Deque.Count + 1;

         --  Indicate thread is on server queue
         if Is_Shared then
            Set_Tcb_State (New_Tcb, On_Server_Shared_Queue);
         else
            Set_Tcb_State (New_Tcb, On_Server_Unshared_Queue);
         end if;
      end Add_To_Deque;

      --------------------
      -- Initialize_Tcb --
      --------------------

      procedure Initialize_Tcb
        (Tcb_Addr            : Word_Ptr;
         Tcb_Is_Local        : Boolean;   -- Whether tcb is in Local_Area
         Routine             : Routine_Ptr;
         Static_Link         : Word_Ptr;
         Locked_Param_Info   : Locked_Param_Info_Type :=
                                 Null_Locked_Param_Info;
         Current_Lock_Index  : Lock_Obj_Index := 0;
         Nested_Block        : Code_Block_Ptr := null;
         Start_Pc            : Code_Index := Code_Index'First)
      is
         Tcb : constant Tcb_Ptr := To_Tcb_Ptr (Tcb_Addr);
      begin
         --  Initialize TCB size field *unless* it is dynamically allocated.
         if Tcb_Is_Local then
            --  If TCB not through a level of indirection
            --  then its size field needs to be set to zero
            --  to indicate it should *not* be reclaimed.
            --  TBD: This is probably not the right long-term solution!
            Tcb.Header :=
              (Size      => 0,
               Stg_Rgn   => 0,
               Type_Info => 0,
               Lock_Obj  => Current_Lock_Index,
               On_Stack  => True);
            Tcb.Self_Address := Null_Virtual_Address;
         else
            --  TCB's Size should already be initialized,
            --  to be something in the range Thread_Control_Block_Size
            --  up to TCB-size + 10 or so (maximum # of parameters)
            pragma Assert
              (Tcb.Header.Size in
                 Thread_Control_Block_Size .. Max_Tcb_Size_Including_Params);
            pragma Assert (Tcb.Self_Address /= Null_Virtual_Address);
            Tcb.Header.Lock_Obj := Current_Lock_Index;
         end if;

         if Nested_Block /= null then
            --  Length/Callee-start comes from Nested_Block info
            Tcb.Info :=
              (For_Compiled_Routine  => False,
               For_Nested_Block      => True,
               Routine_Id            => Routine.Index,
               Start_Pc              => Start_Pc,
               Locked_Param_Info     => Locked_Param_Info,
               Local_Area_Length     => Nested_Block.Local_Area_Length,
               Start_Callee_Locals   => Nested_Block.Start_Callee_Locals,
               Call_Can_Be_Queued |
               Thread_Was_Queued     => False,
               Uses_Queuing          => Nested_Block.Uses_Queuing,
               Thread_Exit_Requested => False,
               Thread_State          => Initial_State);

         elsif Routine.Is_PSVM_Routine
            or else not Routine.Is_Compiled_Routine
         then
            --  Length/Callee-start comes from target routine
            Tcb.Info :=
              (For_Compiled_Routine  => False,
               For_Nested_Block      => False,
               Routine_Id            => Routine.Index,
               Start_Pc              => Code_Index'First,
               Locked_Param_Info     => Locked_Param_Info,
               Local_Area_Length     => Routine.Local_Area_Length,
               Start_Callee_Locals   => Routine.Start_Callee_Locals,
               Call_Can_Be_Queued    => Locked_Param_Info.Param_Index > 0
                                          and then not Routine.Is_PSVM_Routine,
               Thread_Was_Queued     => False,
               Uses_Queuing          => Routine.Uses_Queuing,
               Thread_Exit_Requested => False,
               Thread_State          => Initial_State);
         else
            --  Compiled code
            Tcb.Info :=
              (For_Compiled_Routine  => True,
               For_Nested_Block      => Routine.Is_Nested_Block,
               Code_Addr             => Routine.Routine_Addr,
               Conv_Desc             => Routine.Conv_Desc,
               Internal_Precond_Addr => Routine.Internal_Precond_Addr,
               Locked_Param_Info     => Locked_Param_Info,
               Call_Can_Be_Queued    => Locked_Param_Info.Param_Index > 0
                                          and then not Routine.Is_PSVM_Routine,
               Thread_Was_Queued     => False,
               Uses_Queuing          => Routine.Uses_Queuing,
               Thread_Exit_Requested => False,
               Thread_State          => Initial_State);
         end if;

         Tcb.Static_Link := Static_Link;
         Tcb.Master_Ptr := null;
         Tcb.Next_Subthread := null;
         --  Link on singly-threaded chain hanging off of master

         Tcb.Next_Waiting := null;
         --  "Next" link on doubly-threaded chain on server queue
         Tcb.Prev_Waiting := null;
         --  "Prev" link on doubly-threaded chain on server queue
      end Initialize_Tcb;

      ------------------
      -- Is_Subthread --
      ------------------

      function Is_Subthread
        (Waiting_Tcb : Word_Ptr;
         Master      : Master_Index) return Boolean
      is
         Ancestor : Master_Index :=
                      Index_Of_Master (Tcb_Master_Ptr (Waiting_Tcb));
      begin
         if Master = 0 then
            --  Any thread will do
            return True;
         end if;

         while Ancestor /= 0 loop
            if Ancestor = Master then
               --  We have a match
               return True;
            end if;

            --  Check enclosing master
            Ancestor := Master_Extras (Ancestor).Enclosing_Master;
         end loop;

         --  Thread not a subthread
         return False;
      end Is_Subthread;

      ----------------------
      -- Finish_Subthread --
      ----------------------

      procedure Finish_Subthread
        (Server_Index : Thread_Server_Index;
         Finished_Tcb : Word_Ptr)
      is
         Thread_Master : constant Word_Ptr :=
                           Tcb_Master_Ptr (Finished_Tcb);
         Index     : constant Master_Index  := Index_Of_Master (Thread_Master);
         Subthread : Word_Ptr := First_Subthread (Thread_Master);

         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
      begin
         if Index <= 0 then
            Messages.Put_RT_Error
              ("Internal: Finish_Subthread: " &
               "Master_Index <= 0 for TCB at " &
               Hex_Image (Finished_Tcb),
               Src_Pos => Execution_Source_Pos (Server_Index));
            Thread_Manager.Dump_Thread_State;
            raise Program_Error;
         end if;

         --  Indicate that thread is finished
         Set_Tcb_State (Finished_Tcb, Final_State);

         Server_Info_Array (Server_Index).Last_Active_Thread :=
           null;
         --  NOTE: This is mostly used for debugging

         if Subthread = Finished_Tcb then
            --  Finished_Tcb was first on subthread list;
            --  skip over it.
            Set_First_Subthread (Thread_Master, Next_Subthread (Finished_Tcb));
         else
            --  Look for Finished_Tcb in middle of list
            loop
               pragma Assert (Subthread /= null);
               declare
                  Next : constant Word_Ptr :=
                    Next_Subthread (Subthread);
               begin
                  exit when Next = Finished_Tcb;

                  Subthread := Next;
               end;
            end loop;

            --  Carve Finished_Tcb out of list
            Set_Next_Subthread
              (Subthread, Next_Subthread (Finished_Tcb));
         end if;

         Master_Extra.Subthread_Count := Master_Extra.Subthread_Count - 1;
         --  By decrementing this, we notify the master that
         --  one more of its dependent threads is finished.
         --  When this gets to zero, the master is complete
         --  and Wait_For_Threads will return.

         if Tcb_Size (Finished_Tcb) > 0 then
            --  Size field > 0 in TCB means TCB was dynamically allocated.
            --  Release it back to the appropriate region.
            Deallocate_From_Stg_Rgn
              (Stg_Rgn_Of_Large_Obj (Thread_Master),
               To_Tcb_Ptr (Finished_Tcb).Self_Address,
               Server_Index);
         end if;

         if Debug_Threading
           or else (Debug_Kill and then Master_Extra.Subthread_Count = 0)
         then
            Put_Line
              ("Finish_Subthread for TCB at " &
               Hex_Image (Finished_Tcb) &
               ", server" & Thread_Server_Index'Image (Server_Index) &
               ", master" & Master_Index'Image (Index) &
               ", count now " &
               Thread_Count'Image (Master_Extra.Subthread_Count));
            if not Master_Extra.Master_Is_Shared then
               Put_Line (" Master" & Master_Index'Image (Index) &
                 " is unshared.");
            end if;
            Flush;
         end if;
      exception
         when Storage_Error =>
            --  Not much to do here
            raise;

         when E : others =>
            Messages.Put_RT_Error
              ("Finish_Subthread: " &
               Ada.Exceptions.Exception_Name (E) &
               " raised.",
               Src_Pos => Execution_Source_Pos (Server_Index));
      end Finish_Subthread;

      --------------------
      -- Next_Subthread --
      --------------------

      function Next_Subthread
        (Tcb_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Next_Subthread;
      end Next_Subthread;

      ----------------------
      -- Next_Waiting_Tcb --
      ----------------------

      function Next_Waiting_Tcb
        (Tcb_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Next_Waiting;
      end Next_Waiting_Tcb;

      ----------------------
      -- Prev_Waiting_Tcb --
      ----------------------

      function Prev_Waiting_Tcb
        (Tcb_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Prev_Waiting;
      end Prev_Waiting_Tcb;

      -----------------------
      -- Remove_From_Deque --
      -----------------------

      procedure Remove_From_Deque
        (Deque             : in out Thread_Deque_Head;
         Tcb_To_Be_Removed : Word_Ptr) is
      --  Remove Tcb from deque
         use Thread_Manager_Data;
         Next  : constant Word_Ptr :=
           Next_Waiting_Tcb (Tcb_To_Be_Removed);
         Prev  : constant Word_Ptr :=
           Prev_Waiting_Tcb (Tcb_To_Be_Removed);
      begin
         if Next = null then
            --  Was last TCB
            Deque.Last_Thread := Prev;
         else
            Set_Prev_Waiting_Tcb (Next, Prev);
         end if;

         if Prev = null then
            --  Was first TCB
            Deque.First_Thread := Next;
         else
            Set_Next_Waiting_Tcb (Prev, Next);
         end if;

         --  Keep count of length of Deque
         Deque.Count := Deque.Count - 1;

         --  Set to point at itself so Remove_From_Deque is idempotent.
         Set_Prev_Waiting_Tcb (Tcb_To_Be_Removed, Tcb_To_Be_Removed);
         Set_Next_Waiting_Tcb (Tcb_To_Be_Removed, Tcb_To_Be_Removed);

         --  Indicate thread no longer on server queue
         Set_Tcb_State (Tcb_To_Be_Removed, Unknown_State);
      end Remove_From_Deque;

      ------------------------
      -- Set_Next_Subthread --
      ------------------------

      procedure Set_Next_Subthread
        (Tcb_Addr : Word_Ptr;
         Next     : Word_Ptr)
      is
         pragma Assert (Next /= Tcb_Addr);  --  Avoid creating a loop
      begin
         To_Tcb_Ptr (Tcb_Addr).Next_Subthread := Next;
      end Set_Next_Subthread;

      --------------------------
      -- Set_Next_Waiting_Tcb --
      --------------------------

      procedure Set_Next_Waiting_Tcb
        (Tcb_Addr : Word_Ptr;
         Next     : Word_Ptr) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Next_Waiting := Next;
      end Set_Next_Waiting_Tcb;

      --------------------------
      -- Set_Prev_Waiting_Tcb --
      --------------------------

      procedure Set_Prev_Waiting_Tcb
        (Tcb_Addr : Word_Ptr;
         Prev     : Word_Ptr) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Prev_Waiting := Prev;
      end Set_Prev_Waiting_Tcb;

      --------------------------------
      -- Set_Tcb_Call_Can_Be_Queued --
      --------------------------------

      procedure Set_Tcb_Call_Can_Be_Queued
        (Tcb_Addr      : Word_Ptr;
         Can_Be_Queued : Boolean) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Call_Can_Be_Queued := Can_Be_Queued;
      end Set_Tcb_Call_Can_Be_Queued;

      ----------------------------
      -- Set_Tcb_Exit_Requested --
      ----------------------------

      procedure Set_Tcb_Exit_Requested
        (Tcb_Addr       : Word_Ptr;
         Exit_Requested : Boolean) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Thread_Exit_Requested := Exit_Requested;
      end Set_Tcb_Exit_Requested;

      ------------------------
      -- Set_Tcb_Master_Ptr --
      ------------------------

      procedure Set_Tcb_Master_Ptr
        (Tcb_Addr    : Word_Ptr;
         Master_Addr : Word_Ptr)
      is
         pragma Assert (Master_Addr /= null);
      begin
         To_Tcb_Ptr (Tcb_Addr).Master_Ptr := Master_Addr;
      end Set_Tcb_Master_Ptr;

      ------------------
      -- Set_Tcb_Size --
      ------------------

      procedure Set_Tcb_Size
        (Tcb_Addr : Word_Ptr;
         Size     : Offset_Within_Area) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Header.Size := Size;
      end Set_Tcb_Size;

      ----------------------
      -- Set_Tcb_Start_Pc --
      ----------------------

      procedure Set_Tcb_Start_Pc
        (Tcb_Addr : Word_Ptr;
         Start_Pc : Code_Index) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Start_Pc := Start_Pc;
      end Set_Tcb_Start_Pc;

      -------------------
      -- Set_Tcb_State --
      -------------------

      procedure Set_Tcb_State
        (Tcb_Addr  : Word_Ptr;
         New_State : Thread_State_Enum) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Thread_State := New_State;
      end Set_Tcb_State;

      --------------------------
      -- Set_Tcb_Uses_Queuing --
      --------------------------

      procedure Set_Tcb_Uses_Queuing
        (Tcb_Addr     : Word_Ptr;
         Uses_Queuing : Boolean) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Uses_Queuing := Uses_Queuing;
      end Set_Tcb_Uses_Queuing;

      ------------------------
      -- Set_Tcb_Was_Queued --
      ------------------------

      procedure Set_Tcb_Was_Queued
        (Tcb_Addr   : Word_Ptr;
         Was_Queued : Boolean) is
      begin
         To_Tcb_Ptr (Tcb_Addr).Info.Thread_Was_Queued := Was_Queued;
      end Set_Tcb_Was_Queued;

      ----------------------------
      -- Tcb_Call_Can_Be_Queued --
      ----------------------------

      function Tcb_Call_Can_Be_Queued
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Call_Can_Be_Queued;
      end Tcb_Call_Can_Be_Queued;

      -------------------
      -- Tcb_Code_Addr --
      -------------------

      function Tcb_Code_Addr
        (Tcb_Addr : Word_Ptr) return Routine_Code_Address is
         pragma Assert (Tcb_For_Compiled_Routine (Tcb_Addr));
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Code_Addr;
      end Tcb_Code_Addr;

      -------------------
      -- Tcb_Conv_Desc --
      -------------------

      function Tcb_Conv_Desc
        (Tcb_Addr : Word_Ptr) return Convention_Descriptor is
         pragma Assert (Tcb_For_Compiled_Routine (Tcb_Addr));
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Conv_Desc;
      end Tcb_Conv_Desc;

      ------------------------
      -- Tcb_Exit_Requested --
      ------------------------

      function Tcb_Exit_Requested
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return Is_Shut_Down or else
           (Tcb_Addr /= null and then
             To_Tcb_Ptr (Tcb_Addr).Info.Thread_Exit_Requested);
      end Tcb_Exit_Requested;

      ------------------------------
      -- Tcb_For_Compiled_Routine --
      ------------------------------

      function Tcb_For_Compiled_Routine
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return Tcb_Addr /= null and then
           To_Tcb_Ptr (Tcb_Addr).Info.For_Compiled_Routine;
      end Tcb_For_Compiled_Routine;

      --------------------------
      -- Tcb_For_Nested_Block --
      --------------------------

      function Tcb_For_Nested_Block
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return Tcb_Addr /= null and then
           To_Tcb_Ptr (Tcb_Addr).Info.For_Nested_Block;
      end Tcb_For_Nested_Block;

      -------------------------------
      -- Tcb_Internal_Precond_Addr --
      -------------------------------

      function Tcb_Internal_Precond_Addr
        (Tcb_Addr : Word_Ptr) return Nested_Blk_Address is
         pragma Assert (Tcb_For_Compiled_Routine (Tcb_Addr));
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Internal_Precond_Addr;
      end Tcb_Internal_Precond_Addr;

      ---------------------------
      -- Tcb_Local_Area_Length --
      ---------------------------

      function Tcb_Local_Area_Length
        (Tcb_Addr : Word_Ptr) return Offset_Within_Area is
      begin
         if Tcb_For_Compiled_Routine (Tcb_Addr) then
            return 0;
         else
            return To_Tcb_Ptr (Tcb_Addr).Info.Local_Area_Length;
         end if;
      end Tcb_Local_Area_Length;

      ----------------------------
      -- Tcb_Locked_Param_Index --
      ----------------------------

      function Tcb_Locked_Param_Index
        (Tcb_Addr : Word_Ptr) return Natural is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Locked_Param_Info.Param_Index;
      end Tcb_Locked_Param_Index;

      ---------------------------
      -- Tcb_Locked_Param_Info --
      ---------------------------

      function Tcb_Locked_Param_Info
        (Tcb_Addr : Word_Ptr) return Locked_Param_Info_Type is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Locked_Param_Info;
      end Tcb_Locked_Param_Info;

      --------------------
      -- Tcb_Master_Ptr --
      --------------------

      function Tcb_Master_Ptr
        (Tcb_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Master_Ptr;
      end Tcb_Master_Ptr;

      -----------------------
      -- Tcb_Routine_Index --
      -----------------------

      function Tcb_Routine_Index
        (Tcb_Addr : Word_Ptr) return Routine_Index is
      begin
         if Tcb_For_Compiled_Routine (Tcb_Addr) then
            return 0;
         else
            return To_Tcb_Ptr (Tcb_Addr).Info.Routine_Id;
         end if;
      end Tcb_Routine_Index;

      --------------
      -- Tcb_Size --
      --------------

      function Tcb_Size
        (Tcb_Addr : Word_Ptr) return Offset_Within_Area is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Header.Size;
      end Tcb_Size;

      -----------------------------
      -- Tcb_Start_Callee_Locals --
      -----------------------------

      function Tcb_Start_Callee_Locals
        (Tcb_Addr : Word_Ptr) return Offset_Within_Area is
      begin
         if Tcb_For_Compiled_Routine (Tcb_Addr) then
            return 0;
         else
            return To_Tcb_Ptr (Tcb_Addr).Info.Start_Callee_Locals;
         end if;
      end Tcb_Start_Callee_Locals;

      ---------------
      -- Tcb_State --
      ---------------

      function Tcb_State
        (Tcb_Addr : Word_Ptr) return Thread_State_Enum is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Thread_State;
      end Tcb_State;

      ---------------------
      -- Tcb_Static_Link --
      ---------------------

      function Tcb_Static_Link
        (Tcb_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Static_Link;
      end Tcb_Static_Link;

      ------------------
      -- Tcb_Start_Pc --
      ------------------

      function Tcb_Start_Pc
        (Tcb_Addr : Word_Ptr) return Code_Index is
      begin
         if Tcb_For_Compiled_Routine (Tcb_Addr) then
            return Code_Index'First;
         else
            return To_Tcb_Ptr (Tcb_Addr).Info.Start_Pc;
         end if;
      end Tcb_Start_Pc;

      ----------------------
      -- Tcb_Uses_Queuing --
      ----------------------

      function Tcb_Uses_Queuing
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Uses_Queuing;
      end Tcb_Uses_Queuing;

      --------------------
      -- Tcb_Was_Queued --
      --------------------

      function Tcb_Was_Queued
        (Tcb_Addr : Word_Ptr) return Boolean is
      begin
         return To_Tcb_Ptr (Tcb_Addr).Info.Thread_Was_Queued;
      end Tcb_Was_Queued;

      -----------------------  Thread Master operations  ----------------------

      function Addr_To_Master_Ptr is
        new Ada.Unchecked_Conversion (System.Address, Thread_Master_Ptr);

      function Word_To_Master_Ptr
        (Master_Word_Addr : Word_Ptr) return Thread_Master_Ptr;
      --  Return pointer to Master at given address
      pragma Export (Ada, Word_To_Master_Ptr, "word_to_master");
      --  For debugging

      -------------------
      -- Add_To_Master --
      -------------------

      procedure Add_To_Master
        (Thread_Master : Word_Ptr;
         Server_Index  : Thread_Server_Index;
         New_Tcb       : Word_Ptr) is
         --  Link TCB onto chain off master

         Index         : constant Master_Index :=
           Index_Of_Master (Thread_Master);
         Master_Extra  : Master_Extra_Rec renames Master_Extras (Index);
      begin
         Set_Next_Subthread (New_Tcb, First_Subthread (Thread_Master));
         Set_First_Subthread (Thread_Master, New_Tcb);

         if Master_Extra.Subthread_Count = Uninit_Thread_Count then
            --  This is the first thread for this master
            --  NOTE: We use "Uninit_Thread_Count" rather than "0"
            --        because 0 is used to signal that one or more threads
            --        were spawned and have now finished, so cannot be used
            ---       to indicate that this is first use of the master.

            --  Initialize active thread count and lock held
            Master_Extra.Subthread_Count := 1;
            Master_Extra.Lock_Held := Large_Obj_Lock_Obj (New_Tcb);

            --  Statistics
            Num_Active_Masters := Num_Active_Masters + 1;
            if Num_Active_Masters > Max_Active_Masters then
               Max_Active_Masters := Num_Active_Masters;
               if Debug_Threading then
                  Put_Line
                    ("Spawn_Threads -- new Max_Active_Masters =" &
                     Natural'Image (Max_Active_Masters));
               end if;
            end if;

         else
            --  Bump active thread count
            Master_Extra.Subthread_Count := Master_Extra.Subthread_Count + 1;

            if Debug_Threading then
               Put_Line
                 ("Spawn_Thread adding thread for master" &
                  Master_Index'Image (Index) &
                  ", thread count now " &
                  Thread_Count'Image (Master_Extra.Subthread_Count));
            end if;
         end if;
      end Add_To_Master;

      ---------------------
      -- Exit_Level_Diff --
      ---------------------

      function Exit_Level_Diff
        (Master_Addr : Word_Ptr) return Natural is
      begin
         return To_Master_Ptr (Master_Addr).Info.Exit_Level_Diff;
      end Exit_Level_Diff;

      ---------------------
      -- Exit_Skip_Count --
      ---------------------

      function Exit_Skip_Count
        (Master_Addr : Word_Ptr) return Code_Offset is
      begin
         return To_Master_Ptr (Master_Addr).Info.Exit_Skip_Count;
      end Exit_Skip_Count;

      ---------------------
      -- First_Subthread --
      ---------------------

      function First_Subthread
        (Master_Addr : Word_Ptr) return Word_Ptr is
      begin
         return To_Master_Ptr (Master_Addr).First_Subthread;
      end First_Subthread;

      ---------------------
      -- Index_Of_Master --
      ---------------------

      function Index_Of_Master
        (Master : Word_Ptr) return Master_Index is
      begin
         return To_Master_Ptr (Master).Info.Index;
      exception
         when E : others =>
            Messages.Put_RT_Error
              ("Index_Of_Master: " &
                 Ada.Exceptions.Exception_Name (E) &
                 " raised.",
               Src_Pos => Execution_Source_Pos);
            return 0;
      end Index_Of_Master;

      ------------------------
      -- Initialize_Handler --
      ------------------------

      procedure Initialize_Handler
        (Master_Address : Word_Ptr;
         Routine        : Routine_Ptr;
         Handler_Block  : Code_Block_Ptr;
         Handler_Start  : Code_Index)
      is
         Master_Ptr   : constant Thread_Master_Ptr :=
                          To_Master_Ptr (Master_Address);
         Index        : constant Master_Index := Master_Ptr.Info.Index;
         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
         Info         : Server_Info renames
                          Server_Info_Array (Master_Extra.Owned_By_Server);
      begin
         if Handler_Block = null then
            --  Compiled handler
            Master_Extra.Handler_Info :=
              (Compiled_Handler, Routine.Conv_Desc, Routine.Routine_Addr);
            if Debug_Threading then
               Put_Line
                 (" Initialize_Handler" & Master_Index'Image (Index) & " at " &
                    Hex_Image (Master_Address) & ", compiled handler " &
                    Hex_Image (Routine.Routine_Addr));
            end if;
         else
            --  PSVM handler
            Master_Extra.Handler_Info :=
              (PSVM_Handler, Handler_Start, Routine, Handler_Block);
            if Debug_Threading then
               Put_Line
                 (" Initialize_Handler" & Master_Index'Image (Index) & " at " &
                    Hex_Image (Master_Address) & ", handler " &
                    Code_Block_Image (Handler_Block.all));
            end if;
         end if;
      end Initialize_Handler;

      -----------------------
      -- Initialize_Master --
      -----------------------

      procedure Initialize_Master
        (Master_Address          : Word_Ptr;
         Local_Stg_Rgn_Index     : Stg_Rgn_Index;
         Server_Index            : Thread_Server_Index;
         Initial_Subthread_Count : Thread_Count := Uninit_Thread_Count;
         Spawning_Tcb            : Word_Ptr := null;
         Never_Shared            : Boolean := False)
      is
         Master_Ptr : constant Thread_Master_Ptr :=
                        To_Master_Ptr (Master_Address);
         Index      : Master_Index := 0;
         Info       : Server_Info renames Server_Info_Array (Server_Index);
      begin
         Master_Ptr.Header :=
           (Size => 0,
            Stg_Rgn => Local_Stg_Rgn_Index,
            Type_Info => 0,
            Lock_Obj => 0,
            On_Stack => True);

         --  Assign the thread master an index
         if Info.Free_Master /= 0 then
            --  Reuse an old master index
            --  NOTE: Free list is linked through the "enclosing-master" field.
            Index            := Info.Free_Master;
            Info.Free_Master := Master_Extras (Index).Enclosing_Master;

            --  Reset Enclosing_Master field to zero
            Master_Extras (Index).Enclosing_Master := 0;
         else
            --  Allocate a new master index
            Thread_Manager.Next_Master_Index (Index);
         end if;

         declare
            Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
         begin
            Master_Ptr.Info :=
              (Index => Index,
               Outcome => Normal_Outcome,
               Exit_Level_Diff => 0,
               Exit_Skip_Count => 0,
               Master_Exit_Requested => False);

            Master_Ptr.First_Subthread := null;

            --  Initialize the master address so we can get back.
            Master_Extra.Master_Address  := Master_Address;

            --  Initialize subthread count as specified by caller.
            Master_Extra.Subthread_Count := Initial_Subthread_Count;

            --  Initialize Owned-By field
            Master_Extra.Owned_By_Server := Server_Index;

            --  Indicate whether can be shared
            Master_Extra.Master_Never_Shared := Never_Shared;

            --  Save state of server at point when master is created
            Master_Extra.State_Of_Master := Info.Current_State;

            --  Initialize other fields (NOTE: this should not be needed)
            Master_Extra.Exiting_Tcb      := null;
            Master_Extra.Handler_Info     := (Kind => Not_A_Handler);
            Master_Extra.Excep_Occurrence := Null_Virtual_Address;

            --  Initialize enclosing master
            if Spawning_Tcb /= null then
               Master_Extra.Enclosing_Master :=
                 Index_Of_Master (Tcb_Master_Ptr (Spawning_Tcb));
               if not Never_Shared
                 and then Master_Extra.Enclosing_Master /= 0
               then
                  --  Prevent sharing if enclosing master doesn't permit
                  --  sharing.
                  Master_Extra.Master_Never_Shared :=
                    Master_Extras
                      (Master_Extra.Enclosing_Master).Master_Never_Shared;
               end if;

               --  Temporarily update the master of the spawner to be
               --  this new master; we will reset it when the master is
               --  awaited.
               Set_Tcb_Master_Ptr (Spawning_Tcb, Master_Address);
            end if;

            if Debug_Threading then
               Put_Line
                 (" Initialize_Master" & Master_Index'Image (Index) & " at " &
                    Hex_Image (Master_Address) & ", encloser" &
                    Master_Index'Image (Master_Extra.Enclosing_Master) &
                    ", server" & Thread_Server_Index'Image (Server_Index) &
                    ", region" &
                    Stg_Rgn_Index'Image (Local_Stg_Rgn_Index) &
                    ", never_shared " &
                    Boolean'Image (Master_Extra.Master_Never_Shared));
            end if;
         end;
      end Initialize_Master;

      ---------------------------
      -- Master_Exit_Requested --
      ---------------------------

      function Master_Exit_Requested
        (Master_Addr : Word_Ptr) return Boolean is
      begin
         return To_Master_Ptr (Master_Addr).Info.Master_Exit_Requested;
      end Master_Exit_Requested;

      --------------------
      -- Master_Outcome --
      --------------------

      function Master_Outcome
        (Master_Addr : Word_Ptr) return Master_Outcome_Enum is
      begin
         return To_Master_Ptr (Master_Addr).Info.Outcome;
      end Master_Outcome;

      --------------------
      -- Release_Master --
      --------------------

      procedure Release_Master
        (Index        : Master_Index;
         Server_Index : Thread_Server_Index) is
      --  Release the master index and the associated "extra" master rec

         pragma Assert (Index /= 0);

         Master_Extra : Master_Extra_Rec renames Master_Extras (Index);
         Info         : Server_Info renames Server_Info_Array (Server_Index);
         Reuse_Count  : constant Master_Reuse_Counter :=
           Master_Extra.Reuse_Count;
      begin
         if Debug_Threading then
            Put_Line (" Release_Master" & Master_Index'Image (Index) & " at " &
              Hex_Image (Master_Extra.Master_Address) & " for server" &
              Thread_Server_Index'Image (Server_Index));
         end if;

         --  Reset the fields of the master "extra" record
         Master_Extra :=
           (Enclosing_Master => 0,
            Master_Address   => null,
            Subthread_Count  => Uninit_Thread_Count,
            Lock_Held        => 0,
            Num_Subordinates => 0,
            Is_Being_Awaited => False,
            Master_Is_Shared => False,
            Master_Never_Shared     => False,
            Owned_By_Server         => 0,
            Innermost_Shared_Master => 0,
            Reuse_Count             => 0,
            Exiting_Tcb             => null,
            State_Of_Master         => Null_Server_State,
            Handler_Info            => (Kind => Not_A_Handler),
            Excep_Occurrence        => Null_Virtual_Address);

         --  Now bump the reuse count (might wrap around).
         Master_Extra.Reuse_Count := Reuse_Count + 1;

         --  Link onto the chain of free master indices,
         --  linked through the "enclosing-master" field.
         Master_Extra.Enclosing_Master := Info.Free_Master;
         Info.Free_Master := Index;
      end Release_Master;

      ----------------------------------
      -- Set_Enclosing_Master_Outcome --
      ----------------------------------

      procedure Set_Enclosing_Master_Outcome
        (Context         : in out Exec_Context;
         Outcome         : Master_Outcome_Enum;
         Exit_Level_Diff : Natural := 0;
         Exit_Skip_Count : Code_Offset := 0) is
         --  Set "outcome" field of enclosing master
         --  Should not be called with "Normal" outcome (or Exit_Outcome
         --  with Exit_Level_Diff = 1 and Skip_Count = 0).
         pragma Assert (Outcome /= Normal_Outcome
           and then
             (Outcome /= Exit_Outcome
              or else Exit_Level_Diff > 1
              or else Exit_Skip_Count > 0));

         --  NOTE: If at least one iteration of a loop ends with a "continue"
         --        of an outer loop, then when all iterations are done,
         --        the thread enclosing the loop will immediately exit.
         --  TBD: There is a race condition if the same concurrent loop has
         --       two or more iterations that "continue" different outer loops.
         --       There is also a race condition with sequential loops, where
         --       an iteration does an "Add_Parallel_Op" to create an iteration
         --       which quickly runs to completion with a return or
         --       multi-level exit before the "normal" exit occurs.
         --       We now prevent this by not calling this routine with
         --       "normal" exits.

         Enclosing_Master_Address : constant Word_Ptr :=
           Tcb_Master_Ptr (Context.Control_Area);
         Master_Ptr : constant Thread_Master_Ptr :=
           To_Master_Ptr (Enclosing_Master_Address);
      begin
         Master_Ptr.Info.Outcome := Outcome;
         Master_Ptr.Info.Exit_Level_Diff := Exit_Level_Diff;
         Master_Ptr.Info.Exit_Skip_Count := Exit_Skip_Count;
      end Set_Enclosing_Master_Outcome;

      -------------------------
      -- Set_First_Subthread --
      -------------------------

      procedure Set_First_Subthread
        (Master_Addr : Word_Ptr;
         First       : Word_Ptr) is
      begin
         To_Master_Ptr (Master_Addr).First_Subthread := First;
      end Set_First_Subthread;

      -------------------------
      -- Set_Index_Of_Master --
      -------------------------

      procedure Set_Index_Of_Master
        (Master : Word_Ptr;
         Index  : Master_Index) is
      begin
         To_Master_Ptr (Master).Info.Index := Index;
      end Set_Index_Of_Master;

      -------------------------------
      -- Set_Master_Exit_Requested --
      -------------------------------

      procedure Set_Master_Exit_Requested
        (Master_Addr    : Word_Ptr;
         Exit_Requested : Boolean) is
      begin
         To_Master_Ptr (Master_Addr).Info.Master_Exit_Requested
           := Exit_Requested;
      end Set_Master_Exit_Requested;

      -------------------
      -- To_Master_Ptr --
      -------------------

      function To_Master_Ptr
        (Master_Word_Addr : Word_Ptr) return Thread_Master_Ptr
        renames Word_To_Master_Ptr;

      function To_Master_Ptr
        (Master_Addr : Object_Address) return Thread_Master_Ptr is
      begin
         return Addr_To_Master_Ptr
           (Master_Addr.Enclosing_Chunk.Data (Master_Addr.Offset)'Address);
      end To_Master_Ptr;

      ------------------------
      -- Word_To_Master_Ptr --
      ------------------------

      function Word_To_Master_Ptr
        (Master_Word_Addr : Word_Ptr) return Thread_Master_Ptr
      is
      begin
         return Addr_To_Master_Ptr (Master_Word_Addr.all'Address);
      end Word_To_Master_Ptr;

      ---------------------
      -- Word_To_Tcb_Ptr --
      ---------------------

      function Word_To_Tcb_Ptr
        (Tcb_Word_Addr : Word_Ptr) return Tcb_Ptr
      is
      begin
         return Addr_To_Tcb_Ptr (Tcb_Word_Addr.all'Address);
      end Word_To_Tcb_Ptr;

   end Thread_Scheduling;

   --------------------------------
   -- Type_Descriptor_Operations --
   --------------------------------

   package body Type_Descriptor_Ops is

      -------------------------
      -- Generate_Stream_Rep --
      -------------------------

      function Generate_Stream_Rep (Type_Desc : Type_Descriptor_Ptr;
        Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
        Target : Word_Type;
        Server_Index : Thread_Server_Index;
        PFST : Per_File_Strings.Per_File_String_Table_Ptr := null)
        return Word_Type is
      --  Generate the stream representation for the type descriptor
      --  and return it in a basic array allocated in the region
      --  determined by Target.
      --  Update the Per-file String table, if provided.
         use type Stream_Element_Array_Ptr;
         use Per_File_Strings;

         Result : Word_Type := Target;
      begin  --  Generate_Stream_Rep

         if Type_Desc = null then
            --  Nothing to do
            return Target;
         end if;

         --  Write out the stream and save it
         if PFST /= null then
            --  Add strings to per-file-string table instead of streaming them
            declare
               use String_Streams;
               Local_Stream : aliased Buffered_Stream_With_PFS (PFST);
            begin
               --  NOTE: Type_Desc.Stream_Rep is null at this point so
               --        won't be writing a non-null access value to stream.
               Type_Descriptor'Output (Local_Stream'Access, Type_Desc.all);

               --  Retrieve the stream
               Stream_To_Basic_Array
                 (Local_Stream, Univ_Int_Array_Type, Target => Result,
                   Server_Index => Server_Index);
            end;
         else
            --  Write strings into the stream
            declare
               use String_Streams;
               Local_Stream : aliased Buffered_Stream;
            begin
               --  NOTE: Type_Desc.Stream_Rep is null at this point so
               --        won't be writing a non-null access value to stream.
               Type_Descriptor'Output (Local_Stream'Access, Type_Desc.all);

               --  Retrieve the stream
               Stream_To_Basic_Array
                 (Local_Stream, Univ_Int_Array_Type, Target => Result,
                   Server_Index => Server_Index);
            end;
         end if;

         --  Return the basic array
         return Result;
      end Generate_Stream_Rep;

      ------------------------
      -- Get_Enclosing_Type --
      ------------------------

      function Get_Enclosing_Type
        (Static_Link : Word_Ptr;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Non_Op_Map_Type_Ptr is
      begin
         return
           Skip_Over_Op_Map
             (Get_Enclosing_Type_Or_Op_Map (Static_Link, Type_Base));
      end Get_Enclosing_Type;

      ----------------------------------
      -- Get_Enclosing_Type_Or_Op_Map --
      ----------------------------------

      function Get_Enclosing_Type_Or_Op_Map
        (Static_Link : Word_Ptr;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Type_Descriptor_Ptr
      is
         Encloser : Word_Ptr := Static_Link;
         Result   : Type_Descriptor_Ptr;
      begin
         while Encloser /= null loop
            --  Keep looking up the static chain
            declare
               Next : constant Word_Type := Fetch_Word (Encloser, 0);
            begin
               if Next = 0 then
                  --  OK, no type descriptor
                  return null;
               elsif Next = Type_Indicator then
                  --  We have a type
                  exit;
               else
                  --  Keep looking
                  Encloser := Word_To_Word_Ptr (Next);
               end if;
            end;
         end loop;

         --  Encloser offset now identifies the type.
         if Encloser = null then
            --  No type identified
            return null;
         else
            Result := To_Type_Desc_Or_Op_Map (Encloser);
         end if;

         if Type_Base in
           Enclosing_Type_Areas'First + 1 .. Enclosing_Type_Areas'Last
         then
            --  Find relevant enclosing type area
            for I in 1 .. Type_Base rem Max_Up_Levels loop
               Result := Result.Enclosing_Type;
               pragma Assert (Result /= null);
            end loop;
         end if;

         return Result;
      end Get_Enclosing_Type_Or_Op_Map;

      -------------------------------
      -- Get_Formal_Type_Or_Op_Map --
      -------------------------------

      function Get_Formal_Type_Or_Op_Map
        (Type_Desc : Non_Op_Map_Type_Ptr;
         Index     : Offset_Within_Area) return Type_Descriptor_Ptr is
         --  Return Nth formal type within Type parameter array
      begin
         return Type_Desc.Parameters (Natural (Index)).Data.Type_Desc;
      end Get_Formal_Type_Or_Op_Map;

      -------------------------------
      -- Get_Nested_Type_Or_Op_Map --
      -------------------------------

      function Get_Nested_Type_Or_Op_Map
        (Type_Desc : Non_Op_Map_Type_Ptr;
         Index     : Offset_Within_Area) return Type_Descriptor_Ptr is
         --  Return Nth nested type within nested type array
      begin
         return Type_Desc.Nested_Types (Natural (Index));
      end Get_Nested_Type_Or_Op_Map;

      -------------------
      -- Get_Type_Desc --
      -------------------

      function Get_Type_Desc
        (Context   : Exec_Context;
         Type_Info : Object_Locator) return Non_Op_Map_Type_Ptr is
      begin
         --  Just pass the buck to the more general version, and then
         --  skip over the op-map, if any.
         return Skip_Over_Op_Map
           (Get_Type_Desc_Or_Op_Map (Context, Type_Info));
      end Get_Type_Desc;

      ----------------------------
      -- Get_Type_Desc_By_Name  --
      ----------------------------

      function Get_Type_Desc_By_Name
        (Name : Strings.U_String) return Type_Descriptor_Ptr is
      --  Lookup to see if type-descriptor with name given by Name
      --  already exists.  If so return it.  If not return null.
         Existing_Type_Ref : constant Type_Name_Maps.Element_Ref :=
            Type_Name_Maps.Find_Element
              (Table => Type_Name_Map,
               Key   => Name);
         use type Type_Name_Maps.Element_Ref;
      begin
         if Existing_Type_Ref = null then
            --  Not in table yet
            return null;
         else
            --  Return existing element
            return Existing_Type_Ref.all;
         end if;
      end Get_Type_Desc_By_Name;

      -----------------------------
      -- Get_Type_Desc_Or_Op_Map --
      -----------------------------

      function Get_Type_Desc_Or_Op_Map
        (Context   : Exec_Context;
         Type_Info : Object_Locator)
         return Type_Descriptor_Ptr is
      --  Return type descriptor/op-map given Object_Locator for type area.
      --  This does not skip over an op-map.
         Result : Type_Descriptor_Ptr := null;
      begin
         case Type_Info.Base is
            when Zero_Base =>
               --  Absolute identification of type
               if Type_Info.Offset = 0
                 or else
                  Type_Info = Unknown_Func_Type_Obj_Locator
               then
                  Result := null;
               else
                  Result :=
                    Nth_Element
                      (Type_Table, Type_Elem_Index (Type_Info.Offset));
               end if;

            when Local_Area =>
               --  Relative to current local area
               --  No local types
               raise Program_Error;

            when Const_Area =>
               --  No types in global constant table
               raise Program_Error;

            when Param_Area | Enclosing_Param_Areas =>
               --  Relative to some param area;
               --  Type passed as parameter to enclosing operation
               --  (TBD: Will this ever happen?)
               Result :=
                  To_Type_Desc (Type_Index (Fetch_Word (Context, Type_Info)));

            when Type_Area =>
               --  Relative to enclosing type for current operation;
               --  equivalent to relative to "outermost" enclosing local area.
               Result :=
                  Nth_Type_Area_Type
                    (Context.Enclosing_Type, Type_Info.Offset);

            when Enclosing_Type_Areas =>
               --  Relative to type for some enclosing module.
               Result :=
                  Nth_Type_Area_Type
                    (Get_Enclosing_Type
                       (Context.Enclosing_Type, Type_Info.Base),
                     Type_Info.Offset);

            when Any_Base_Register =>
               --  Relative to some temp base register kept in the
               --  current Local_Area
               --  NOTE: Should never happen
               raise Program_Error;

            when Enclosing_Local_Areas =>
               --  Relative to a local area of an enclosing block or operation
               --  The first word of a local area points at its enclosing
               --  block, operation, or type area.
               --  Enclosing_Local_Areas'First is equivalent to Local_Area.
               --  NOTE: Should never happen.
               raise Program_Error;

            when others =>
               Messages.Put_RT_Error
                 ("Internal: " &
                  "Get_Type_Desc_Or_Op_Map: Ill-formed Object_Locator",
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
         end case;

         return Result;
      end Get_Type_Desc_Or_Op_Map;

      function Get_Type_Desc_Or_Op_Map_Exported
        (Static_Link      : Word_Ptr;
         Type_Info_Base   : Area_Base_Indicator;
         Type_Info_Offset : Offset_Within_Area)
         return Type_Descriptor_Ptr is
      --  Return type descriptor/op-map given Object_Locator for type area.
      --  This does not skip over an op-map.
      --  This version takes static link and base/offset as separate params.
      begin
         case Type_Info_Base is
            when Type_Area | Enclosing_Type_Areas =>
               --  Relative to enclosing type for current operation;
               --  equivalent to relative to "outermost" enclosing local area.
               return
                  Nth_Type_Area_Type
                    (Get_Enclosing_Type (Static_Link, Type_Info_Base),
                     Type_Info_Offset);
            when others =>
               --  Only supports type-relative locators
               raise Program_Error;
         end case;
      end Get_Type_Desc_Or_Op_Map_Exported;

      --------------------------------
      -- Install_Type_Desc_By_Name  --
      --------------------------------

      function Install_Type_Desc_By_Name
        (New_Type_Desc : Type_Descriptor;
         Is_New : access Boolean) return Type_Descriptor_Ptr is
      --  Lookup to see if type-descriptor with name New_Type_Desc.Name
      --  already exists.  If not, copy New_Type_Desc into heap and install it.
      --  In any case return pointer to installed type descriptor.
      --  Type_Info.Index and Type_Info.Location will be filled in.
      --  Is_New indicates whether the type descriptor already existed.
         Result : Type_Descriptor_Ptr :=
           Get_Type_Desc_By_Name (New_Type_Desc.Name);
      begin
         if Result = null then
            --  Need to create and install a new type descriptor
            Is_New.all := True;
            Result := new Type_Descriptor'(New_Type_Desc);
            Result.Index := 0;
            Result.Location := Null_Object_Locator;
            Install_Type_Info (Result);
            if Debug_Type_Descs then
               Put_Line ("Install_Type_Desc_By_Name: " &
                 Strings.To_String (New_Type_Desc.Name) & ", index old,new ="
                 & Type_Index'Image (New_Type_Desc.Index) & "," &
                 Type_Index'Image (Result.Index));
            end if;
         else
            Is_New.all := False;
         end if;
         return Result;
      end Install_Type_Desc_By_Name;

      -----------------------------------
      -- Is_Univ_String_Or_Enumeration --
      -----------------------------------

      function Is_Univ_String_Or_Enumeration (Type_Desc : Type_Descriptor_Ptr)
        return Boolean is
      --  Return True if, after unwrapping and skipping op-maps, we end up
      --  at a type descriptor for Univ_String or Univ_Enumeration.
      --  These values need to be handled specially when streaming values,
      --  since their value is actually an index into a global table.
         Unwrapped : constant Type_Descriptor_Ptr :=
           Unwrapped_Type_Desc (Type_Desc);
      begin
         return Unwrapped /= null and then
           (Unwrapped.Type_Kind = Univ_String_Kind
            or else Unwrapped.Type_Kind = Univ_Enum_Kind);
      end Is_Univ_String_Or_Enumeration;

      ---------------------------
      -- Nth_Operation_Of_Type --
      ---------------------------

      function Nth_Operation_Of_Type
        (Callee_Type_Area_Or_Map : Type_Descriptor_Ptr;
         Index                   : Operation_Index;
         Ignore_Abstract_Op      : Boolean := False) return Routine_Info is
      begin
         if Index = Optional_Operation_Index then
            --  calling a missing optional operation is equivalent to calling
            --  the "null" operation
            return Null_Routine_Info;
         elsif Callee_Type_Area_Or_Map.Has_Op_Map then
            --  We have an op map; use mapping and recurse.
            if Index not in Callee_Type_Area_Or_Map.Op_Map'Range then
               --  TBF: Give some kind of useful error
               Messages.Put_Warning ("Out of range Op_Map index =" &
                 Operation_Index'Image (Index),
                 Source_Positions.Null_Source_Position);
               return Null_Routine_Info;
            end if;

            return Nth_Operation_Of_Type
                     (Callee_Type_Area_Or_Map.Actual_Type,
                      Callee_Type_Area_Or_Map.Op_Map (Index),
                      Ignore_Abstract_Op => Ignore_Abstract_Op);

         elsif Callee_Type_Area_Or_Map.Is_Polymorphic then
            --  No operations here; get them from the underlying type.
            declare
               Result_Info : Routine_Info := Nth_Operation_Of_Type
                     (Callee_Type_Area_Or_Map.Components (1).Type_Desc,
                      Index,
                      Ignore_Abstract_Op => Ignore_Abstract_Op);
            begin
               --  Set the action field appropriately (presume op-index set)
               Result_Info.Action := Polymorphic_Type_Action;
               pragma Assert (Result_Info.Op_Index /= 0);
               return Result_Info;
            end;

         else
            --  "Normal" call
            if Index not in Callee_Type_Area_Or_Map.Operations'Range then
               --  TBF: Given a more useful error
               Messages.Put_Warning ("Out of range operation index =" &
                 Operation_Index'Image (Index),
                 Source_Positions.Null_Source_Position);
               return Null_Routine_Info;
            end if;

            declare
               Result_Routine : Routine_Info renames
                 Callee_Type_Area_Or_Map.Operations (Index);
            begin
               if Result_Routine.Index = 0 then
                  --  Abstract operation
                  --  TBD: As of 8/22/2012, abstract ops have a non-zero index,
                  --       but Routine record has a null Code_Ptr.
                  if Ignore_Abstract_Op then
                     --  Handle abstract op by returning a null
                     return Null_Routine_Info;
                  else
                     --  This is an attempt to call an abstract operation
                     Messages.Put_RT_Error
                       ("Attempt to call an abstract operation",
                        Src_Pos => Execution_Source_Pos);
                     raise Program_Error;
                  end if;
               else
                  --  We have a normal type desc
                  return Result_Routine;
               end if;
            end;
         end if;
      end Nth_Operation_Of_Type;

      --------------------------
      --  Get_Enclosing_Type  --
      --------------------------

      function Get_Enclosing_Type (Type_Desc : Non_Op_Map_Type_Ptr;
         Type_Base : Area_Base_Indicator) return Non_Op_Map_Type_Ptr is
      --  If Type_Base is Enclosing Type_Areas,
      --    follow the enclosing type the specified number of times
      --  else do nothing and return Type_Desc
      begin
         if Type_Base not in Enclosing_Type_Areas then
            return Type_Desc;
         else
            --  Reference to an enclosing type, follow Enclosing_Type chain
            declare
               Type_Desc_To_Use : Non_Op_Map_Type_Ptr := Type_Desc;
            begin
               for I in 1 .. Type_Base rem Max_Up_Levels loop
                  Type_Desc_To_Use := Type_Desc_To_Use.Enclosing_Type;
               end loop;
               return Type_Desc_To_Use;
            end;
         end if;
      end Get_Enclosing_Type;

      ---------------------------
      -- Nth_Type_Area_Element --
      ---------------------------

      function Nth_Type_Area_Element
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area) return Element_Info
      is
         Type_Desc_To_Use : Non_Op_Map_Type_Ptr :=
            Get_Enclosing_Type (Type_Desc, Type_Base);
         Item_Index       : Natural := 0;
      begin

         case Item_Offset is
            when Type_Self_Reference_Offset =>
               --  self reference
               return
                 (Type_Desc => Type_Desc_To_Use,
                  Addr => To_Virtual_Address
                    ((null, Offset_Within_Chunk (Type_Desc_To_Use.Index))),
                  Value =>
                    -(Word_Type (Type_Desc_To_Use.Index) * Chunk_Divisor) - 1);

            when Type_Formal_Parameter_Offsets'First + 1 ..
                 Type_Formal_Parameter_Offsets'Last =>
               --  Module parameter
               Item_Index :=
                 Natural (Item_Offset - Type_Formal_Parameter_Offsets'First);
               if not Type_Desc_To_Use.Is_Finished
                 and then Type_Desc_To_Use.Parameters (Item_Index).Data.Addr =
                          Null_Virtual_Address
               then
                  --  Make sure type descriptor is fully initialized
                  Finish_Type_Descriptor (Type_Desc_To_Use);
               end if;
               return Type_Desc_To_Use.Parameters (Item_Index).Data;

            when Type_Actual_Of_Formal_Offsets'Range =>
               --  Actual of formal
               Item_Index :=
                 Natural (Item_Offset - Type_Actual_Of_Formal_Offsets'First);
               return Type_Desc_To_Use.Actuals_Of_Formals (Item_Index).Data;

            when Type_Nested_Type_Offsets'Range =>
               --  Nested type
               declare
                  Nested_Type : constant Type_Descriptor_Ptr :=
                    Type_Desc_To_Use.Nested_Types
                      (Natural (Item_Offset - Type_Nested_Type_Offsets'First));
               begin
                  return
                    (Type_Desc => Nested_Type,
                     Addr => To_Virtual_Address
                       ((null, Offset_Within_Chunk (Nested_Type.Index))),
                     Value =>
                       -(Word_Type (Nested_Type.Index) * Chunk_Divisor) - 1);
               end;

            when Type_Nested_Obj_Offsets'Range =>
               --  Nested object
               Item_Index :=
                 Natural (Item_Offset - Type_Nested_Obj_Offsets'First);
               if not Type_Desc_To_Use.Is_Finished
                 and then Type_Desc_To_Use.Nested_Objs (Item_Index).Data.Addr
                             = Null_Virtual_Address
               then
                  --  Make sure type descriptor is fully initialized
                  Finish_Type_Descriptor (Type_Desc_To_Use);
               end if;

               return Type_Desc_To_Use.Nested_Objs (Item_Index).Data;

            when Corresponding_Polymorphic_Type_Offsets =>
               --  Polymorphic type
               declare
                  Poly_Type : constant Type_Descriptor_Ptr :=
                    Nth_Type_Area_Type
                      (Type_Desc_To_Use,
                       Item_Offset =>
                         Item_Offset -
                           Corresponding_Polymorphic_Type_Offsets'First)
                       .Corresponding_Polymorphic_Type_Desc;
               begin
                  return
                    (Type_Desc => Poly_Type,
                     Addr => To_Virtual_Address
                       ((null, Offset_Within_Chunk (Poly_Type.Index))),
                     Value =>
                       -(Word_Type (Poly_Type.Index) * Chunk_Divisor) - 1);
               end;

            when others =>
               Messages.Put_RT_Error
                 ("Internal: Bad offset for type area " &
                  Offset_Within_Area'Image (Item_Offset),
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
         end case;
      end Nth_Type_Area_Element;

      ------------------------
      -- Nth_Type_Area_Type --
      ------------------------

      function Nth_Type_Area_Type
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area)
         return Type_Descriptor_Ptr
      is
         Item_Index       : Natural := 0;
      begin

         case Item_Offset is
            when Type_Self_Reference_Offset =>
               --  self reference
               return Type_Desc;

            when Type_Formal_Parameter_Offsets'First + 1 ..
                 Type_Formal_Parameter_Offsets'Last =>
               --  Module parameter
               Item_Index :=
                 Natural (Item_Offset - Type_Formal_Parameter_Offsets'First);

               return Type_Desc.Parameters (Item_Index).Data.Type_Desc;

            when Type_Actual_Of_Formal_Offsets'Range =>
               --  Actual of formal
               Item_Index :=
                 Natural (Item_Offset - Type_Actual_Of_Formal_Offsets'First);

               return
                 Type_Desc.Actuals_Of_Formals
                   (Item_Index).Data.Type_Desc;

            when Type_Nested_Type_Offsets'Range =>
               --  Nested type
               Item_Index :=
                 Natural (Item_Offset - Type_Nested_Type_Offsets'First);

               return Type_Desc.Nested_Types (Item_Index);

            when Corresponding_Polymorphic_Type_Offsets =>
               --  Polymorphic type
               return Nth_Type_Area_Type (Type_Desc,
                 Item_Offset => Item_Offset -
                   Corresponding_Polymorphic_Type_Offsets'First)
                 .Corresponding_Polymorphic_Type_Desc;

            when others =>
               Messages.Put_RT_Error
                 ("Internal: Bad offset for type in type area " &
                  Offset_Within_Area'Image (Item_Offset),
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
         end case;
      end Nth_Type_Area_Type;

      ------------------------
      -- Nth_Type_Area_Word --
      ------------------------

      function Nth_Type_Area_Word
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area) return Word_Type
      is
         pragma Assert (Item_Offset /= 0);
         Type_Desc_To_Use : Non_Op_Map_Type_Ptr :=
            Get_Enclosing_Type (Type_Desc, Type_Base);
         Item_Index       : Natural := 0;
      begin

         if not Type_Desc_To_Use.Is_Finished then
            --  Make sure type descriptor is fully initialized
            --  but return immediately if in the middle of finishing it
            --  since we might simply be referring to a prior constant.
            Finish_Type_Descriptor (Type_Desc_To_Use,
              Return_On_Recursion => True);
         end if;

         case Item_Offset is
            when Type_Self_Reference_Offset =>
               --  Self reference, refer to type itself
               return
                 -(Word_Type (Type_Desc_To_Use.Index) * Chunk_Divisor) - 1;

            when Type_Formal_Parameter_Offsets'First + 1 ..
                 Type_Formal_Parameter_Offsets'Last =>
               --  Module parameter
               Item_Index :=
                 Natural (Item_Offset - Type_Formal_Parameter_Offsets'First);
               return Type_Desc_To_Use.Parameters (Item_Index).Data.Value;

            when Type_Actual_Of_Formal_Offsets'Range =>
               --  Actual of formal
               Item_Index :=
                 Natural (Item_Offset - Type_Actual_Of_Formal_Offsets'First);
               return Type_Desc_To_Use.Actuals_Of_Formals
                        (Item_Index).Data.Value;

            when Type_Nested_Obj_Offsets'Range =>
               --  Nested constant or global var
               Item_Index :=
                 Natural (Item_Offset - Type_Nested_Obj_Offsets'First);
               --  Always re-fetch from address in case it is a var.
               --  TBD: We could set addr to point at Value component
               --       if Value were declared "aliased" and Virt_Is_Phys
               return Fetch_Word
                 (Type_Desc_To_Use.Nested_Objs (Item_Index).Data.Addr);

            when others =>
               Messages.Put_RT_Error
                 ("Internal: Bad offset for object in type area " &
                  Offset_Within_Area'Image (Item_Offset),
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
         end case;
      end Nth_Type_Area_Word;

      --------------------------------
      -- Nth_Type_Area_Item_Address --
      --------------------------------

      function Nth_Type_Area_Item_Address
        (Type_Desc   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
        return Object_Virtual_Address
      is
         Type_Desc_To_Use : Non_Op_Map_Type_Ptr :=
            Get_Enclosing_Type (Type_Desc, Type_Base);
         Item_Index       : Natural := 0;
      begin

         case Item_Offset is
            when Type_Self_Reference_Offset =>
               --  self reference
               return To_Virtual_Address
                 ((null, Offset_Within_Chunk (Type_Desc_To_Use.Index)));

            when Type_Formal_Parameter_Offsets'First + 1 ..
                 Type_Formal_Parameter_Offsets'Last =>
               --  Module parameter
               Item_Index :=
                 Natural (Item_Offset - Type_Formal_Parameter_Offsets'First);

               if not Type_Desc_To_Use.Is_Finished
                 and then Type_Desc_To_Use.Parameters (Item_Index).Data.Addr =
                          Null_Virtual_Address
               then
                  --  Make sure type descriptor is fully initialized
                  Finish_Type_Descriptor (Type_Desc_To_Use);
               end if;

               return Type_Desc_To_Use.Parameters (Item_Index).Data.Addr;

            when Type_Actual_Of_Formal_Offsets'Range =>
               --  Actual of formal
               Item_Index :=
                 Natural (Item_Offset - Type_Actual_Of_Formal_Offsets'First);
               return Type_Desc_To_Use.Actuals_Of_Formals
                        (Item_Index).Data.Addr;

            when Type_Nested_Type_Offsets'Range =>
               --  Nested type
               declare
                  Nested_Type : constant Type_Descriptor_Ptr :=
                    Type_Desc_To_Use.Nested_Types
                      (Natural (Item_Offset - Type_Nested_Type_Offsets'First));
               begin
                  return To_Virtual_Address
                    ((null, Offset_Within_Chunk (Nested_Type.Index)));
               end;

            when Type_Nested_Obj_Offsets'Range =>
               --  Nested object
               Item_Index :=
                 Natural (Item_Offset - Type_Nested_Obj_Offsets'First);
               if not Type_Desc_To_Use.Is_Finished
                 and then Type_Desc_To_Use.Nested_Objs (Item_Index).Data.Addr
                             = Null_Virtual_Address
               then
                  --  Make sure type descriptor is fully initialized
                  Finish_Type_Descriptor (Type_Desc_To_Use);
               end if;

               return Type_Desc_To_Use.Nested_Objs (Item_Index).Data.Addr;

            when Corresponding_Polymorphic_Type_Offsets =>
               --  Polymorphic type
               declare
                  Poly_Type : constant Type_Descriptor_Ptr :=
                    Nth_Type_Area_Type (Type_Desc_To_Use,
                      Item_Offset => Item_Offset -
                        Corresponding_Polymorphic_Type_Offsets'First)
                      .Corresponding_Polymorphic_Type_Desc;
               begin
                  return To_Virtual_Address
                    ((null, Offset_Within_Chunk (Poly_Type.Index)));
               end;

            when others =>
               Messages.Put_RT_Error
                 ("Internal: Bad offset for item in type area " &
                  Offset_Within_Area'Image (Item_Offset),
                  Src_Pos => Execution_Source_Pos);
               raise Program_Error;
         end case;
      end Nth_Type_Area_Item_Address;

      function Nth_Type_Area_Item_Physical_Address
        (Type_Info   : Non_Op_Map_Type_Ptr;
         Item_Offset : Offset_Within_Area;
         Type_Base   : Area_Base_Indicator := Type_Area)
         return Word_Ptr is
         Type_Desc : constant Non_Op_Map_Type_Ptr :=
            Skip_Over_Op_Map (Get_Enclosing_Type (Type_Info, Type_Base));
      begin
         return Virtual_To_Physical_Address
            (Nth_Type_Area_Item_Address (Type_Desc, Item_Offset));
      end Nth_Type_Area_Item_Physical_Address;

      -------------------
      -- Null_For_Type --
      -------------------

      function Null_For_Type
        (Type_Desc : Type_Descriptor_Ptr) return Word_Type
      is
         pragma Assert (Type_Desc.Is_Small);
      begin
         return Type_Desc.Null_Value;
      end Null_For_Type;

      ----------------------------------
      -- Reconstruct_Type_Descriptors --
      ----------------------------------

      procedure Reconstruct_Type_Descriptors
        (Num_Entries : Stream_Local_Count;
         Stream_Rep_Table : Stream_Rep_Table_Type;
         String_Table     : String_Table_Ptr;
         Type_Desc_Table  : out Type_Desc_Table_Type) is

         use String_Streams;
         Recon_Map : aliased Reconstructed_Type_Desc_Map.Hash_Table;
         Type_Desc_Is_Incomplete : array (1 .. Num_Entries) of aliased Boolean
           := (others => False);
         use Per_File_Strings;

         function Op_Map_Or_Type (Type_Desc : Type_Descriptor)
           return String;
         --  Return "op-map " or "type "

         function Op_Map_Or_Type (Type_Desc : Type_Descriptor)
           return String is
         --  Return "op-map " or "type "
         begin
            if Type_Desc.Has_Op_Map then
               return "op-map ";
            else
               return "type ";
            end if;
         end Op_Map_Or_Type;

      begin  --  Reconstruct_Type_Descriptors
         if Debug_Type_Descs then
            Ada.Text_IO.Put_Line ("Reconstructing" &
              Stream_Local_Count'Image (Num_Entries) & " types, pass 1");
         end if;
         --  Do this in three passes.
         --  In the first pass, create type descriptors.
         for I in 1 .. Num_Entries loop
            declare
               use Reconstructed_Type_Desc_Map;
               Local_Stream : aliased Buffered_Reader_With_Strings
                                        (Stream_Rep_Table (I), String_Table);
               Recon_Type_Desc : Type_Descriptor renames
                 Type_Descriptor'Input (Local_Stream'Access);
               Orig_Index   : constant Type_Index := Recon_Type_Desc.Index;
               Existing_TD  : Element_Ref;
            begin
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 1 done on " &
                    Op_Map_Or_Type (Recon_Type_Desc) &
                    Strings.To_String (Recon_Type_Desc.Name));
               end if;
               Type_Desc_Table (I) := Install_Type_Desc_By_Name
                 (Recon_Type_Desc, Type_Desc_Is_Incomplete (I)'Access);

               --  Add this type desc to the mapping indexed by its orig index
               Enter_Element
                 (Recon_Map, Key => Orig_Index, Elem => Type_Desc_Table (I),
                  Existing_Elem => Existing_TD);

               if Existing_TD /= null and then Debug_Type_Descs then
                  Put_Line (" Reconstruct_Type_Descriptors: " &
                    Op_Map_Or_Type (Existing_TD.all.all) &
                    Strings.To_String (Existing_TD.all.Name) &
                    " has same index" &
                    Type_Index'Image (Orig_Index) & " as " &
                    Op_Map_Or_Type (Recon_Type_Desc) &
                    Strings.To_String (Recon_Type_Desc.Name) &
                    " in local slot" & Stream_Local_Count'Image (I));
               end if;
            end;
         end loop;

         if Debug_Type_Descs then
            Ada.Text_IO.Put_Line ("Reconstructing" &
              Stream_Local_Count'Image (Num_Entries) & " types, pass 2");
         end if;

         --  In the second pass, just overwrite the entries that
         --  were newly created in the first pass, but don't try
         --  to read any PSVM objects.
         for I in 1 .. Num_Entries loop
            if Type_Desc_Is_Incomplete (I) then
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 2 starting on " &
                    Strings.To_String (Type_Desc_Table (I).Name));
               end if;
               declare
                  Local_Stream : aliased Buffered_Desc_Reader
                    (Stream_Rep_Table (I),
                     Type_Map => Recon_Map'Access,
                     Strings  => String_Table,
                     Pass     => 2);
                  Saved_TD     : constant Type_Descriptor :=
                                            Type_Desc_Table (I).all;
               begin
                  --  Overwrite entry
                  Type_Desc_Table (I).all :=
                    Type_Descriptor'Input (Local_Stream'Access);

                  --  Fix some fields up
                  Type_Desc_Table (I).Index    := Saved_TD.Index;
                  Type_Desc_Table (I).Location := Saved_TD.Location;
                  Type_Desc_Is_Incomplete (I) := Local_Stream.Is_Incomplete;
               end;
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 2 done on " &
                    Op_Map_Or_Type (Type_Desc_Table (I).all) &
                    Strings.To_String (Type_Desc_Table (I).Name));
               end if;
            else
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 2 skipped on " &
                    Op_Map_Or_Type (Type_Desc_Table (I).all) &
                    Strings.To_String (Type_Desc_Table (I).Name));
               end if;
            end if;
         end loop;

         if Debug_Type_Descs then
            Ada.Text_IO.Put_Line ("Reconstructing" &
              Stream_Local_Count'Image (Num_Entries) & " types, pass 3");
         end if;

         --  In the third pass, overwrite the entries again
         --  but this time read the PSVM objects that are inside
         --  the arrays of Params, Actuals_Of_Formals, and Nested_Objs
         for I in 1 .. Num_Entries loop
            if Type_Desc_Is_Incomplete (I) then
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 3 starting on " &
                    Strings.To_String (Type_Desc_Table (I).Name));
               end if;
               declare
                  Local_Stream : aliased Buffered_Desc_Reader
                    (Stream_Rep_Table (I),
                     Type_Map => Recon_Map'Access,
                     Strings  => String_Table,
                     Pass     => 3);
                  Saved_TD     : constant Type_Descriptor :=
                                            Type_Desc_Table (I).all;
               begin
                  --  Should no longer be marked as incomplete
                  pragma Assert (not Local_Stream.Is_Incomplete);

                  --  Overwrite entry
                  Type_Desc_Table (I).all :=
                    Type_Descriptor'Input (Local_Stream'Access);

                  --  Fix some fields up
                  Type_Desc_Table (I).Index    := Saved_TD.Index;
                  Type_Desc_Table (I).Location := Saved_TD.Location;
               end;
               if Debug_Type_Descs then
                  Ada.Text_IO.Put_Line ("Pass 3 done on " &
                    Op_Map_Or_Type (Type_Desc_Table (I).all) &
                    Strings.To_String (Type_Desc_Table (I).Name));
               end if;
            else
               null;  --  Debugging is too verbose here.
               --  if Debug_Type_Descs then
               --     Ada.Text_IO.Put_Line ("Pass 3 skipped on " &
               --       Strings.To_String (Type_Desc_Table (I).Name));
               --  end if;
            end if;
         end loop;

         --  Reclaim storage of recon map.
         Reconstructed_Type_Desc_Map.Reclaim (Recon_Map);
      end Reconstruct_Type_Descriptors;

      ------------------------------
      -- Root_Of_Polymorphic_Type --
      ------------------------------

      function Root_Of_Polymorphic_Type
        (Poly_Type : Non_Op_Map_Type_Ptr) return Type_Descriptor_Ptr
      is
         pragma Assert (Poly_Type.Is_Polymorphic);
      begin
         return Poly_Type.Root_Type_Desc;
      end Root_Of_Polymorphic_Type;

      ----------------------
      -- Skip_Over_Op_Map --
      ----------------------

      function Skip_Over_Op_Map
        (Type_Desc_Or_Op_Map : Type_Descriptor_Ptr) return Non_Op_Map_Type_Ptr
      is
      begin
         if Type_Desc_Or_Op_Map /= null
           and then Type_Desc_Or_Op_Map.Has_Op_Map
         then
            return Type_Desc_Or_Op_Map.Actual_Type;
         else
            return Type_Desc_Or_Op_Map;
         end if;
      end Skip_Over_Op_Map;

      ------------------
      -- To_Type_Desc --
      ------------------

      function To_Type_Desc (Type_Id : Type_Index) return Non_Op_Map_Type_Ptr
      is
      begin
         return Skip_Over_Op_Map (To_Type_Desc_Or_Op_Map (Type_Id));
      end To_Type_Desc;

      function To_Type_Desc (Addr : Word_Ptr) return Non_Op_Map_Type_Ptr is
      --  Convert physical address of type descriptor to the corresponding type
      --  descriptor; skip over any op-map.
      begin
         return Skip_Over_Op_Map (To_Type_Desc_Or_Op_Map (Addr));
      end To_Type_Desc;

      ----------------------------
      -- To_Type_Desc_Or_Op_Map --
      ----------------------------

      function To_Type_Desc_Or_Op_Map
        (Type_Id : Type_Index) return Type_Descriptor_Ptr is
      begin
         if Type_Id = 0
           or else
            Type_Elem_Index (Type_Id) > Num_Elements (Type_Table)
         then
            --  Null or invalid type descriptor index.
            return null;
         else
            return Nth_Element (Type_Table, Type_Elem_Index (Type_Id));
         end if;
      end To_Type_Desc_Or_Op_Map;

      ----------------------------
      -- Type_Desc_Name_And_Num --
      ----------------------------

      function Type_Desc_Name_And_Num
         (Type_Desc      : Type_Descriptor_Ptr;
          Use_Short_Form : Boolean := False) return String is
      begin
         if Type_Desc = null then
            return "null type";
         elsif Type_Desc.Has_Op_Map or else Type_Desc.Is_Polymorphic then
            return Type_Sem_Image (Type_Desc.Type_Sem, Use_Short_Form) &
              " #" & Type_Index'Image (Type_Desc.Index);
         else
            return Type_Sem_Image (Type_Desc.Type_Sem, Use_Short_Form);
         end if;
      end Type_Desc_Name_And_Num;

      -------------------------
      -- Unwrapped_Type_Desc --
      -------------------------

      function Unwrapped_Type_Desc (Type_Desc : Type_Descriptor_Ptr)
        return Non_Op_Map_Type_Ptr is
      --  Return type descriptor stripped of Op_Maps and Wrappers
         Result : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Desc);
      begin
         if Result = null then
            return Result;
         else
            --  Skip over wrappers and op-maps
            while Result.Is_Wrapper loop
               Result := Skip_Over_Op_Map (Result.Components (1).Type_Desc);
            end loop;
            return Result;
         end if;
      end Unwrapped_Type_Desc;

   end Type_Descriptor_Ops;

   ------------------------
   -- Type_Desc_Ptr_Read --
   ------------------------

   procedure Type_Desc_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Type_Descriptor_Ptr) is
   --  Read the type-desc index and has_op_map flag and return
   --  a pointer to a type desc that might not be fully filled in yet.
      Index : Type_Index;
      Has_Op_Map : Boolean;
      use String_Streams;
   begin
      Type_Index'Read (Stream, Index);
      Boolean'Read (Stream, Has_Op_Map);

      --  Return null by default;
      Item := null;

      if Index /= 0 and then Stream.all in Buffered_Desc_Reader then
         --  We have a mapping from old index to new type-desc
         declare
            use Reconstructed_Type_Desc_Map;
            Recon_Map : Hash_Table
              renames Buffered_Desc_Reader (Stream.all).Type_Map.all;
            Recon_Elem : constant Element_Ref := Find_Element
                                                   (Recon_Map, Index);
         begin
            if Recon_Elem /= null then
               --  We have a new type desc for this old index
               Item := Recon_Elem.all;
            else
               Put_Line ("Type_Desc_Ptr_Read Index =" &
                 Type_Index'Image (Index) & " (Op_Map=" &
                 Boolean'Image (Has_Op_Map)(1) & ") not in map");
            end if;
         end;
      end if;

   end Type_Desc_Ptr_Read;

   -------------------------
   -- Type_Desc_Ptr_Write --
   -------------------------

   procedure Type_Desc_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Type_Descriptor_Ptr) is
   --  Write out the type-desc index and a has_op_map flag
   --  rather than the type desc ptr itself
   begin
      if Item = null then
         Type_Index'Write (Stream, 0);
         Boolean'Write (Stream, False);
      else
         Type_Index'Write (Stream, Item.Index);
         Boolean'Write (Stream, Item.Has_Op_Map);
      end if;
   end Type_Desc_Ptr_Write;

   --------------------------------------
   -- Type_Desc info array Write/Reads --
   --------------------------------------

   procedure Obj_Element_Info_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Data : Element_Info) is
   --  Write out the Element_Info for an object
   --  We need Per-File string table to write these out
      use Per_File_Strings;
      pragma Assert (Stream.all in Buffered_Stream_With_PFS'Class);

      Local_Stream : aliased Buffered_Stream_With_PFS
        (Buffered_Stream_With_PFS'Class (Stream.all).PFS);
   begin
      --  First write out the type.
      Type_Descriptor_Ptr'Write (Stream, Data.Type_Desc);
      --  Write out the length and then the stream
      --  representation of the value.

      --  Compute the length of stream
      PSVM_Object_Write
        (Local_Stream'Access, Data.Type_Desc, Data.Value);

      --  Write out the length
      Ada.Streams.Stream_Element_Count'Write (Stream,
        Stream_Length (Local_Stream));

      --  Now write out the data for "real"
      PSVM_Object_Write
        (Stream, Data.Type_Desc, Data.Value);

      --  Reclaim storage of Local_Stream
      Reset_Stream (Local_Stream);
   end Obj_Element_Info_Write;

   procedure Obj_Element_Info_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Data : out Element_Info;
      Obj_Info_Incomplete : in out Boolean) is
   --  Read in the Element_Info for an object
   --  Set Obj_Info_Incomplete to True if only filled in Type_Desc;
   --  otherwise leave it as it came in.
      use String_Streams;
   begin
      --  Read in the type descriptor for the object
      Type_Descriptor_Ptr'Read (Stream, Data.Type_Desc);

      --  Read in the length and then the value (if any)
      declare
         use Ada.Streams;
         Len : constant Stream_Element_Count :=
           Stream_Element_Count'Input (Stream);
      begin
         if Stream.all in Buffered_Desc_Reader
           and then
            Buffered_Desc_Reader (Stream.all).Pass >= 3
         then
            --  Read in info and preserve in global stg region
            --  NOTE: We don't do this until the third pass
            --        because we need the type descriptors to
            --        have their component field filled out.

            --  Make sure Global_Data_Stg_Rgn exists
            Initialize_Global_Data_Stg_Rgn;

            --  Read in the value
            PSVM_Object_Read
              (Stream, Data.Type_Desc, Data.Value,
               Stg_Rgn => Global_Data_Stg_Rgn);

            --  Store it in global memory
            --  so we can provide an address.
            Data.Addr :=
              Allocate_From_Stg_Rgn (Global_Data_Stg_Rgn, 1,
                Server_Index => Main_Thread_Server_Index);

            Store_Word (Data.Addr, Data.Value);
         else
            --  Just skip over the stream rep of the value
            declare
               Buff : Stream_Element_Array (1 .. Len);
               Last : Stream_Element_Offset := 0;
            begin
               Ada.Streams.Read (Stream.all, Buff, Last);
               pragma Assert (Last = Len);

               --  Remember that we are incomplete and need
               --  a third pass.
               Obj_Info_Incomplete := True;
               if Stream.all in Buffered_Desc_Reader then
                  Buffered_Desc_Reader (Stream.all).Is_Incomplete := True;
               end if;
               Data.Value := 0;
               Data.Addr  := 0;
            end;
         end if;
      end;
   end Obj_Element_Info_Read;

   procedure Param_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Parameter_Info_Array_Ptr) is
   --  Write out the contents of the param info array.
   --  Requires: (Stream : in out Buffered_Stream_With_PFS'Class;
   begin
      if Item = null then
         Natural'Write (Stream, 0);
      else
         Natural'Write (Stream, Item'Length);
         for I in Item'Range loop
            declare
               Param : Parameter_Info renames Item (I);
               use Per_File_Strings;
            begin
               --  Always write out the kind first
               Parameter_Kind_Enum'Write (Stream, Param.Kind);

               case Param.Kind is
                  when Formal_Type =>
                     --  Nothing special to do -- just write out element info
                     Element_Info'Write (Stream, Param.Data);
                  when Formal_Object =>
                     --  Write out so can be read back in in a different
                     --  interpreter context.
                     Obj_Element_Info_Write
                       (Stream, Param.Data);
                  when Formal_Operation =>
                     Messages.Put_RT_Error
                       ("NYI: Formal Operations of Modules",
                        Src_Pos => Source_Positions.Null_Source_Position);
               end case;
            end;
         end loop;
      end if;
   end Param_Array_Ptr_Write;

   procedure Param_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Parameter_Info_Array_Ptr) is
   --  Read in the contents of the param info array
      Len : constant Natural := Natural'Input (Stream);
      Info_Arr : Parameter_Info_Array (1 .. Len);
      Obj_Info_Incomplete : Boolean := False;
         --  Set to True if object found but not able to read in yet
   begin
      for I in 1 .. Len loop
         declare
            Param : Parameter_Info renames Info_Arr (I);
         begin
            --  Always read in the kind first
            Parameter_Kind_Enum'Read (Stream, Param.Kind);

            case Param.Kind is
               when Formal_Type =>
                  --  Nothing special to do
                  Element_Info'Read (Stream, Param.Data);
               when Formal_Object =>
                  --  Read in the object and reconstruct info
                  Obj_Element_Info_Read (Stream, Param.Data,
                    Obj_Info_Incomplete);
               when Formal_Operation =>
                  Messages.Put_RT_Error ("NYI: Formal Operations of Modules",
                    Src_Pos => Source_Positions.Null_Source_Position);
            end case;
         end;
      end loop;
      if Stream.all in Buffered_Desc_Reader
        and then not Obj_Info_Incomplete
      then
         --  We have a map, these are worth saving
         --  NOTE: We need to do the 'Reads anyway so everything
         --        matches up correctly.
         Item := new Parameter_Info_Array'(Info_Arr);
      else
         Item := null;
      end if;
   end Param_Array_Ptr_Read;

   procedure Comp_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Component_Info_Array_Ptr) is
   --  Write out the contents of the component info array.
   begin
      if Item = null then
         Natural'Write (Stream, 0);
      else
         Natural'Write (Stream, Item'Length);
         for I in Item'Range loop
            Component_Info'Write (Stream, Item (I));
         end loop;
      end if;
   end Comp_Array_Ptr_Write;

   procedure Comp_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Component_Info_Array_Ptr) is
   --  Read in the contents of the component info array
      Len : constant Natural := Natural'Input (Stream);
      Info_Arr : Component_Info_Array (1 .. Len);
   begin
      for I in Info_Arr'Range loop
         Component_Info'Read (Stream, Info_Arr (I));
      end loop;
      if Stream.all in Buffered_Desc_Reader then
         --  We have a map, these are worth saving
         Item := new Component_Info_Array'(Info_Arr);
      else
         Item := null;
      end if;
   end Comp_Array_Ptr_Read;

   procedure Desc_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Type_Desc_Array_Ptr) is
   --  Write out the contents of the type_desc array.
   begin
      if Item = null then
         Natural'Write (Stream, 0);
      else
         Natural'Write (Stream, Item'Length);
         for I in Item'Range loop
            Type_Descriptor_Ptr'Write (Stream, Item (I));
         end loop;
      end if;
   end Desc_Array_Ptr_Write;

   procedure Desc_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Type_Desc_Array_Ptr) is
   --  Read in the contents of the component info array
      Len : constant Natural := Natural'Input (Stream);
      Info_Arr : Type_Desc_Array (1 .. Len);
   begin
      for I in Info_Arr'Range loop
         Type_Descriptor_Ptr'Read (Stream, Info_Arr (I));
      end loop;
      if Stream.all in Buffered_Desc_Reader then
         --  We have a map, these are worth saving
         Item := new Type_Desc_Array'(Info_Arr);
      else
         Item := null;
      end if;
   end Desc_Array_Ptr_Read;

   procedure Const_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Const_Info_Array_Ptr) is
   --  Write out the contents of the constant info array.
   begin
      if Item = null then
         Natural'Write (Stream, 0);
      else
         Natural'Write (Stream, Item'Length);
         for I in Item'Range loop
            --  Write the name and then the object type/value
            Strings.U_String'Write (Stream, Item (I).Name);
            Obj_Element_Info_Write (Stream, Item (I).Data);
         end loop;
      end if;
   end Const_Array_Ptr_Write;

   procedure Const_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Const_Info_Array_Ptr) is
   --  Read in the contents of the constant info array
      Len : constant Natural := Natural'Input (Stream);
      Info_Arr : Const_Info_Array (1 .. Len);
      Obj_Info_Incomplete : Boolean := False;
   begin
      for I in Info_Arr'Range loop
         --  Read in name and type/value; generate addr.
         Strings.U_String'Read (Stream, Info_Arr (I).Name);
         Obj_Element_Info_Read (Stream, Info_Arr (I).Data,
           Obj_Info_Incomplete);
      end loop;

      if Stream.all in Buffered_Desc_Reader
        and then not Obj_Info_Incomplete
      then
         --  We have a map, these are worth saving
         Item := new Const_Info_Array'(Info_Arr);
      else
         --  Not worth saving
         Item := null;
      end if;
   end Const_Array_Ptr_Read;

   procedure Routine_Array_Ptr_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : Routine_Info_Array_Ptr) is
   --  Write out the contents of the routine info array.
   begin
      if Item = null then
         Natural'Write (Stream, 0);
      else
         Operation_Index'Write (Stream, Item'Length);
         for I in Item'Range loop
            Routine_Info'Write (Stream, Item (I));
         end loop;
      end if;
   end Routine_Array_Ptr_Write;

   procedure Routine_Array_Ptr_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Routine_Info_Array_Ptr) is
   --  Read in the contents of the routine info array
      Len : constant Operation_Index := Operation_Index'Input (Stream);
      Info_Arr : Routine_Info_Array (1 .. Len);
   begin
      for I in Info_Arr'Range loop
         Routine_Info'Read (Stream, Info_Arr (I));
      end loop;
      if Stream.all in Buffered_Desc_Reader then
         --  We have a map, these are worth saving
         Item := new Routine_Info_Array'(Info_Arr);
      else
         Item := null;
      end if;
   end Routine_Array_Ptr_Read;

   -----------
   -- Debug --
   -----------

   package body Debug is

      ---------------------
      -- Area_Base_Image --
      ---------------------

      function Area_Base_Image (Area : Area_Base_Indicator) return String is
      begin
         case Area is
            when Zero_Base =>
               return "Zero_Base";

            when Local_Area =>
               return "Local_Area";

            when Param_Area =>
               return "Param_Area";

            when Type_Area =>
               return "Type_Area";

            when Const_Area =>
               return "Const_Area";

            when Enclosing_Type_Areas =>
               return "Enclosing_Type_Areas'First +" &
                 Area_Base_Indicator'Image (Area - Enclosing_Type_Areas'First);

            when Base_Registers =>
               return "Base_Registers'First +" &
                 Area_Base_Indicator'Image (Area - Base_Registers'First);

            when Phys_Base_Registers =>
               return "Phys_Base_Registers'First +" &
                 Area_Base_Indicator'Image (Area - Phys_Base_Registers'First);

            when Enclosing_Param_Areas =>
               return "Enclosing_Param_Areas'First +" &
                 Area_Base_Indicator'Image
                   (Area - Enclosing_Param_Areas'First);

            when Enclosing_Local_Areas =>
               return "Enclosing_Local_Areas'First +" &
                 Area_Base_Indicator'Image
                   (Area - Enclosing_Local_Areas'First);

            when others =>
               return Area_Base_Indicator'Image (Area);
         end case;
      end Area_Base_Image;

      ----------------
      -- Chunk_Addr --
      ----------------

      function Chunk_Addr (Chunk : Stg_Rgn_Chunk_Ptr) return Word_Ptr is
      begin
         return To_Word_Ptr (Chunk.Data'Address);
      end Chunk_Addr;

      ----------------------
      -- Code_Block_Image --
      ----------------------

      function Code_Block_Image
        (Code_Block : Code_Block_Descriptor) return String is
      begin
         return
           "(PCO =>" &
           Code_Offset'Image (Code_Block.Pc_Offset) &
           ", UQ => " &
           Boolean'Image (Code_Block.Uses_Queuing) (1) &
           ", USR => " &
           Boolean'Image (Code_Block.Uses_Stg_Rgn) (1) &
           ", LAL =>" &
           Offset_Within_Area'Image (Code_Block.Local_Area_Length) &
           ", SOCL =>" &
           Offset_Within_Area'Image (Code_Block.Start_Callee_Locals) &
           ", NL =>" &
           Code_Nesting_Level'Image (Code_Block.Nesting_Level) &
           ")";
      end Code_Block_Image;

      ---------------------
      -- Direction_Image --
      ---------------------

      function Direction_Image (Dir : Direction) return String is
      begin
         case Dir is
            when Unordered_Dir =>
               return "unordered";
            when Forward_Dir =>
               return "forward";
            when Reverse_Dir =>
               return "reverse";
            when Concurrent_Dir =>
               return "concurrent";
         end case;
      end Direction_Image;

      ----------------
      -- Dump_Chunk --
      ----------------

      procedure Dump_Chunk (Chunk : Stg_Rgn_Chunk_Ptr) is

         function Addr_Image (Addr : System.Address) return String;
         --  Return image of address in "C" 0x... format

         ----------------
         -- Addr_Image --
         ----------------

         function Addr_Image (Addr : System.Address) return String is
            Hex_Im : String renames Hex_Image
                (Addr_To_Word (System.Storage_Elements.To_Integer (Addr)), 0);
         begin
            return "0x" & Hex_Im (Hex_Im'First + 3 .. Hex_Im'Last - 1);
         end Addr_Image;

      begin
         Put_Line ("  Chunk at " & Addr_Image (Chunk.all'Address));
         Put_Line ("    Starting_Virt_Addr = " &
           Hex_Image (Chunk.Starting_Virtual_Address));
         Put_Line ("    Starting_Phys_Addr = " &
           Addr_Image (Chunk.Data'Address));
      end Dump_Chunk;

      ---------------
      -- Dump_Code --
      ---------------
      procedure Dump_Code
        (Code : Code_Ptr) is
      --  Display instructions of Code_Type for debugging purposes.
      begin
         if Code = null then
            Put_Line (" Code = null");
         else
            --  Not an abstract routine "stub"
            Put_Line (" Uses_Stg_Rgn = " &
              Boolean'Image (Code.Uses_Stg_Rgn));
            Put_Line
              (" Code_Length =" &
               Code_Length_Type'Image (Code.Code_Length));
            for I in 1 .. Code.Code_Length loop
               Debug.Dump_One_Instruction (Code.Instrs (I));
               if Code.Instrs (I).Op = Skip_Op
                 and then Code.Instrs (I).Skip_Count = 0
                 and then I + 10 < Code.Code_Length
                 and then Code.Instrs (I + 1).Op = Skip_Op
                 and then Code.Instrs (I + 1).Skip_Count = 0
               then
                  Put_Line (" ...");
                  exit;
               end if;
            end loop;
         end if;
      end Dump_Code;

      -------------
      -- Dump_Dq --
      -------------

      procedure Dump_Dq is
      begin
         Delay_Queue.Dump_Delay_Queue;
      end Dump_Dq;

      ----------------
      -- Dump_Locks --
      ----------------

      procedure Dump_Locks is
      begin
         Thread_Manager.Dump_Locks;
      end Dump_Locks;

      ----------------------
      -- Dump_Obj_Locator --
      ----------------------

      procedure Dump_Obj_Locator (Locator : Object_Locator) is
      begin
         Put_Line (Obj_Locator_Image (Locator));
      end Dump_Obj_Locator;

      --------------------------
      -- Dump_Obj_With_Indent --
      --------------------------

      procedure Dump_Obj_With_Indent
        (Value     : Word_Type;
         Type_Desc : Type_Descriptor_Ptr := null;
         Indent    : Natural := 0)
      is
         Type_Desc_To_Use : Non_Op_Map_Type_Ptr :=
                              Skip_Over_Op_Map (Type_Desc);
         Indent_Str : constant String (1 .. Indent) := (others => ' ');
         Presuming_Is_Small : Boolean := False;
      begin
         if Type_Desc_To_Use /= null and then Type_Desc_To_Use.Is_Wrapper then
            if Type_Desc_To_Use.Components = null then
               --  Oops, type-desc components info not filled in yet
               Type_Desc_To_Use := null;
            else
               --  Is a wrapper, recurse with only component
               Dump_Obj_With_Indent
                 (Value,
                  Type_Desc_To_Use.Components (1).Type_Desc,
                  Indent);
               return;
            end if;
         end if;

         if Type_Desc_To_Use = null then
            --  See whether might be a large object
            if Is_Special_Large_Value (Value)
              and then Large_Obj_Stg_Rgn_Index (Value) in
                 1 .. Num_Stg_Rgns
            then
               --  Presume is large null
               null;

            elsif Virt_Is_Phys then
               --  Virtual is physical address, not as easy a problem
               if Value mod Word_SU_Size /= 0
                 or else
                  (Value not in Lowest_Virt_Addr .. Highest_Virt_Addr
                     and then
                   Value not in Lowest_Stack_Addr .. Highest_Stack_Addr)
               then
                  --  Presume is small
                  Presuming_Is_Small := True;
               end if;

            elsif Value / Chunk_Divisor not in
                 1 .. Word_Type (Stg_Rgn_Manager.Num_Stg_Rgn_Chunks)
              or else Value mod Chunk_Divisor not in
                 0 .. Word_Type (Min_Chunk_Size - 1)
              or else not Large_Obj_Type_Info_Is_In_Range (Value)
              or else Large_Obj_Size (Value) not in
                 Large_Obj_Header_Size .. 100_000
              or else Large_Obj_Stg_Rgn_Index (Value) not in
                 1 .. Num_Stg_Rgns
            then
               Presuming_Is_Small := True;
            end if;

            if Presuming_Is_Small then
               Put_Line
                 (Indent_Str & Hex_Image (Value) & " : presuming is small");
               return;
            end if;

            --  Presume is a large obj
            Put_Line
              (Indent_Str & Hex_Image (Value) & " : presuming is large");
            Type_Desc_To_Use := Large_Obj_Type_Desc (Value);

         elsif Is_Small (Type_Desc_To_Use) then
            if Is_Null_Value (Value, Type_Desc_To_Use) then
               Put_Line (Indent_Str & "null");
            else
               Put_Line (Indent_Str & Hex_Image (Value));
               --  TBD: display as appropriate type
            end if;

            return;

         elsif not Is_Special_Large_Value (Value)
           and then not Virt_Is_Phys
           and then Value / Chunk_Divisor not in
                       1 .. Word_Type (Stg_Rgn_Manager.Num_Stg_Rgn_Chunks)
         then
            Put_Line
              (Indent_Str & Hex_Image (Value) &
               " : Not a good large obj addr");

            return;
         end if;

         if Value = Null_Value then
            Put_Line
              (Indent_Str &
               "null (** small null but type is large**)");
         elsif Is_Large_Null (Value) then
            Put_Line
              (Indent_Str &
               "null (large -- region" &
               Stg_Rgn_Index'Image (Large_Obj_Stg_Rgn_Index (Value)) &
               ")");
         elsif Is_Special_Large_Value (Value) then
            Put_Line
              (Indent_Str &
               "special value " &
               Hex_Image (Word_Type (To_Unsigned_Word (Value) / Chunk_Divisor))
               & " (large -- region" &
               Stg_Rgn_Index'Image (Large_Obj_Stg_Rgn_Index (Value)) &
               ")");
            Put_Line
              (Indent_Str & " string value = """ &
               Univ_Strings.To_String (Univ_Strings.From_Word_Type (Value)) &
               '"');
         else
            --  Dump each component
            declare
               Lock_Obj : constant Lock_Obj_Index :=
                            Large_Obj_Lock_Obj (Value);
            begin
               --  Get "actual" type
               Type_Desc_To_Use := Large_Obj_Type_Desc (Value);

               Put_Line
                 (Indent_Str &
                  "Large obj at " &
                  Hex_Image (Value) &
                  ", size = " &
                  Hex_Image (Word_Type (Large_Obj_Size (Value))));
               Put_Line
                 (Indent_Str &
                  " region =" &
                  Stg_Rgn_Index'Image (Large_Obj_Stg_Rgn_Index (Value)));
               if Type_Desc_To_Use /= null then
                  Put_Line
                    (Indent_Str &
                     " type = " &
                     Strings.To_String (Type_Desc_To_Use.Name));
               else
                  Put_Line
                    (Indent_Str &
                     " type = null??");
               end if;

               if Lock_Obj /= 0 then
                  Put_Line
                    (Indent_Str & " lock =" & Lock_Obj_Index'Image (Lock_Obj));
               end if;

               if Type_Desc_To_Use = null then
                  null;
               elsif Type_Desc_To_Use.Type_Kind = Basic_Array_Kind then
                  --  dump "Basic_Array" components
                  declare
                     Comp_Type : constant Type_Descriptor_Ptr :=
                       Basic_Array_Comp_Type (Type_Desc_To_Use);
                     Len : constant Word_Type :=
                       Content_Of_Virtual_Address
                          (Value + Large_Obj_Header_Size);
                  begin
                     for I in 1 .. Offset_Within_Area (Len) loop
                        declare
                           Offset : constant Offset_Within_Area :=
                             Large_Obj_Header_Size + I;
                           Comp_Value : constant Word_Type :=
                             Content_Of_Virtual_Address (Value + Offset);
                        begin
                           --  Dump each component
                           Dump_Obj_With_Indent
                             (Comp_Value,
                              Comp_Type,
                              Indent + 2);
                        end;
                     end loop;
                  end;
               else
                  --  Not a "Basic_Array" so each component of a different type
                  for I in 1 .. Type_Desc_To_Use.Num_Components loop
                     declare
                        Comp_Type : constant Non_Op_Map_Type_Ptr :=
                          Skip_Over_Op_Map
                             (Type_Desc_To_Use.Components (I).Type_Desc);
                        Comp_Value : constant Word_Type :=
                          Content_Of_Virtual_Address
                             (Value +
                              Large_Obj_Header_Size +
                              Offset_Within_Area (I - 1));

                        use Trees;

                        Comp_Decl : constant Optional_Tree :=
                          Type_Desc_To_Use.Components (I).Decl;
                        Comp_Name_Image : constant String :=
                          (if Not_Null (Comp_Decl)
                             and then
                              Tree_Ptr_Of (Comp_Decl).all in
                                Obj_Decl.Tree'Class
                           then
                              Subtree_Image
                                (Obj_Decl.Tree'Class
                                   (Tree_Ptr_Of (Comp_Decl).all).Name) & " => "
                           else
                              "");
                     begin
                        if Type_Desc_To_Use.Components (I).Is_By_Ref then
                           Put_Line
                             (Indent_Str & "  " &
                              Comp_Name_Image &
                              "Ref: " & Hex_Image (Comp_Value));
                        else
                           if Comp_Name_Image'Length > 0 then
                              Put (Indent_Str & ' ' & Comp_Name_Image);
                           end if;
                           Dump_Obj_With_Indent
                             (Comp_Value,
                              Comp_Type,
                              Indent + 2);
                        end if;
                     end;
                  end loop;
               end if;  --  Whether is Basic_Array
            end;
         end if;  --  Whether is non-null
      end Dump_Obj_With_Indent;

      --------------------------
      -- Dump_One_Instruction --
      --------------------------

      procedure Dump_One_Instruction (Instr : Instruction;
                                      Use_Message_Format : Boolean := False) is
         --  Dump one instruction, either in an instruction format,
         --  or in the "message" format that is understood by "vim -q" and
         --  other similar tools.
         use Ada.Text_IO;
         use type Strings.U_String_Index;

         Indent     : constant String := " ";   --  NOTE: This used to be a
                                                --        parameter
         Op         : constant Opcode_Enum := Instr.Op;
         Line_Image : constant String :=
                        Source_Positions.Line_Number'Image
                          (Instr.Source_Pos.Line);
         Col_Image  : constant String :=
                        Source_Positions.Column_Number'Image
                          (Instr.Source_Pos.Col);
         Line_Col_Image : constant String :=
           Line_Image (Line_Image'First + 1 .. Line_Image'Last) & ':' &
           Col_Image (Col_Image'First + 1 .. Col_Image'Last);

         Spaces     : constant String (1 .. 7) := "       ";

         function Call_Target_Image
           (Call_Target : Object_Locator;
            Static_Link : Object_Locator;
            Target_Index : Routine_Index := 0;
            Prefix : String := "") return String;
         --  Return printable name for Call_Target,
         --  and Static_Link, if known, prepended by Prefix.
         --  Use provided Target_Index as a fall back.
         --  Return "" if no printable name available.

         procedure Display_Call_Target
           (Call_Target : Object_Locator;
            Static_Link : Object_Locator;
            Target_Index : Routine_Index := 0);
         --  Display " = <name of call target>" if known

         procedure Display_Type_Name (Static_Link : Object_Locator);
         --  Display " = type-name" if determinable from Static_Link/Type info

         function Ext_Obj_Locator_Image
           (Locator : Object_Locator;
            Enclosing_Type : Object_Locator := Null_Object_Locator)
            return String;
         --  Return obj-locator image, optionally followed by " = <name>".
         --  NOTE: Currently only works for Const_Area

         function Line_Num return String;

         function Printable_Char_Image (Char : Word_Type) return String;

         function Printable_String_Image (Str : String) return String;
         --  Expand control characters, etc., so string is printable.
         --  Surround in "" as well.

         procedure Put_Using_Message_Format;
         --  Display the instruction using the "message" format
         --  that is understandable by "vim -q" etc.

         function Type_Name_Image (Static_Link : Object_Locator;
                                   Prefix : String := "") return String;
         --  Return type name associated with given static link, if known,
         --  prepended with specified prefix.
         --  Return "" if type name not known.

         -----------------------
         -- Call_Target_Image --
         -----------------------

         function Call_Target_Image
           (Call_Target : Object_Locator;
            Static_Link : Object_Locator;
            Target_Index : Routine_Index := 0;
            Prefix : String := "") return String
         --  Return printable name for Call_Target,
         --  and Static_Link, if known, prepended by Prefix.
         --  Use provided Target_Index as a fall back.
         --  Return "" if no printable name available.
         is
            Index : Routine_Index := 0;
         begin
            if Call_Target.Base = Zero_Base then
               --  "Absolute" address of code
               Index := Routine_Index (Call_Target.Offset);

            elsif Call_Target.Base = Type_Area
              and then Call_Target.Offset in Type_Operation_Offsets
              and then Static_Link.Base = Zero_Base
              and then Static_Link.Offset /= 0
            then
               --  Type-relative operation
               declare
                  Call_Type_Desc : constant Type_Descriptor_Ptr :=
                                     Known_Type_Desc (Static_Link);

                  Info : constant Routine_Info :=
                           Nth_Operation_Of_Type
                              (Call_Type_Desc,
                               Operation_Index (Call_Target.Offset -
                                                Type_Operation_Offsets'First),
                               Ignore_Abstract_Op => True);
               begin
                  --  Get routine-index for nth operation of callee type.
                  Index := Info.Index;
               end;
            else
               Index := Target_Index;
            end if;

            if Routine_Elem_Index (Index) in
                 1 .. Num_Elements (Routine_Table)
            then
               declare
                  Target_Routine : constant Routine_Ptr :=
                    Nth_Routine (Index);
               begin
                  if Target_Routine /= null then
                     if not Target_Routine.Is_PSVM_Routine
                       and then Target_Routine.Built_In_Desig =
                                   Identity_Builtin_Str
                     then
                        --  Don't display routine name, as that could
                        --  be misleading, since we share all routines
                        --  that map to the identity.
                        return Prefix & Identity_Desig;
                     else
                        return Prefix &
                          Strings.To_String (Target_Routine.Name);
                     end if;
                  end if;
               end;
            end if;
            --  No printable name available
            return "";
         end Call_Target_Image;

         -------------------------
         -- Display_Call_Target --
         -------------------------

         procedure Display_Call_Target
           (Call_Target : Object_Locator;
            Static_Link : Object_Locator;
            Target_Index : Routine_Index := 0)
         is
         begin
            --  Just pass the buck the the Call_Target_Image function.
            Put (Call_Target_Image (Call_Target, Static_Link, Target_Index,
                 Prefix => " = "));
         end Display_Call_Target;

         -----------------------
         -- Display_Type_Name --
         -----------------------

         procedure Display_Type_Name (Static_Link : Object_Locator) is
         begin
            Put (Type_Name_Image (Static_Link, Prefix => " = "));
         end Display_Type_Name;

         ---------------------------
         -- Ext_Obj_Locator_Image --
         ---------------------------

         function Ext_Obj_Locator_Image
           (Locator        : Object_Locator;
            Enclosing_Type : Object_Locator := Null_Object_Locator)
            return String
         is
         begin
            if Name_For_Object_Locator_Ptr /= null then
               declare
                  Name : String renames
                    Name_For_Object_Locator_Ptr.all
                     (Locator, Enclosing_Type);
               begin
                  if Name /= "" then
                        --  OK, we have a name, so add it.
                     return Obj_Locator_Image (Locator) & " = " & Name;
                  end if;
               end;
            end if;

            --  Fall back to simple obj-locator image
            return Obj_Locator_Image (Locator);
         end Ext_Obj_Locator_Image;

         --------------
         -- Line_Num --
         --------------

         function Line_Num return String is
            use Source_Positions;
         begin
            if Instr.Source_Pos /= Null_Source_Position then
               if Line_Col_Image'Length < 7 then
                  return Spaces (1 .. (7 - Line_Col_Image'Length)) &
                         Line_Col_Image;
               else
                  return Line_Col_Image;
               end if;
            else
               return Spaces;
            end if;
         end Line_Num;

         --------------------------
         -- Printable_Char_Image --
         --------------------------

         function Printable_Char_Image (Char : Word_Type) return String is
         begin
            case Char is
               when Character'Pos (' ') .. 16#7E# =>
                  case Char is
                     when Character'Pos (''') =>
                        return "'\''";
                     when Character'Pos ('\') =>
                        return "'\\'";
                     when others =>
                        return "'" & Character'Val (Char) & "'";
                  end case;

               when Character'Pos (ASCII.LF) =>
                  return "'\n'";

               when Character'Pos (ASCII.CR) =>
                  return "'\r'";

               when Character'Pos (ASCII.NUL) =>
                  return "'\0'";

               when Character'Pos (ASCII.HT) =>
                  return "'\t'";

               when Character'Pos (ASCII.FF) =>
                  return "'\f'";

               when others =>
                  declare
                     Img : String renames Hex_Image
                                            (Char,
                                             Underscores_Every => 2);
                  begin
                     --  Return extended image format
                     return "'\#" & Img (Img'First + 2 .. Img'Last) & "#'";
                  end;
            end case;
         end Printable_Char_Image;

         ----------------------------
         -- Printable_String_Image --
         ----------------------------

         function Printable_String_Image (Str : String) return String is
            Printable_String : String (1 .. Str'Last * 10 + 2);
            Index            : Positive := 1;
         begin
            Printable_String (Index) := '"';
            Index := Index + 1;

            for I in Str'Range loop
               if Str (I) = '"' then
                  --  Need to quote double-quote
                  Printable_String (Index) := '\';
                  Printable_String (Index + 1) := '"';
                  Index := Index + 2;

               elsif Str (I) = ''' then
                  --  Do not need to quote single-quote
                  Printable_String (Index) := ''';
                  Index := Index + 1;

               else
                  --  Expand character
                  declare
                     --  TBD: Handle UTF_8 properly
                     Printable_Char : String renames Printable_Char_Image
                        (Character'Pos (Str (I)));
                  begin
                     --  Copy printable image of character into
                     --  Printable_String

                     for J in Printable_Char'First + 1 ..
                       Printable_Char'Last - 1
                     loop
                        Printable_String (Index) := Printable_Char (J);
                        Index := Index + 1;
                     end loop;
                  end;
               end if;
            end loop;

            --  Terminate the string literal
            Printable_String (Index) := '"';

            --  Return the printable string
            return Printable_String (1 .. Index);
         end Printable_String_Image;

         ---------------------
         -- Type_Name_Image --
         ---------------------

         function Type_Name_Image (Static_Link : Object_Locator;
                                   Prefix : String := "") return String is
         --  Return type name associated with given static link, if known,
         --  prepended with specified prefix.
         --  Return "" if type name not known.
         begin
            if Static_Link.Base = Zero_Base
              and then Static_Link.Offset /= 0
              and then Static_Link /= Unknown_Func_Type_Obj_Locator
            then
               --  Type is known
               return Prefix &
                 Strings.To_String (Known_Type_Desc (Static_Link).Name);
            else
               return "";
            end if;
         end Type_Name_Image;

         ------------------------------
         -- Put_Using_Message_Format --
         ------------------------------

         procedure Put_Using_Message_Format is
         --  Display the instruction using the "message" format
         --  that is understandable by "vim -q" etc.
         begin
            case Op is
               when Call_Op =>
                  declare
                     Msg : constant String :=
                       "within call" & Call_Target_Image
                          (Instr.Call_Target, Instr.Static_Link,
                           Instr.Target_Index,
                           Prefix => " of ") &
                       Type_Name_Image (Instr.Static_Link, ", type => ");
                  begin
                     Messages.Put_RT_Error (Msg,
                        Src_Pos => Instr.Source_Pos,
                        Message_Kind => "Info");
                  end;
               when Indirect_Call_Op =>
                  Messages.Put_RT_Error
                    ("within indirect call" &
                     Type_Name_Image (Instr.Static_Link, ", type => "),
                     Src_Pos => Instr.Source_Pos,
                     Message_Kind => "Info");
               when Call_Nested_Block_Op =>
                  Messages.Put_RT_Error
                    ("within call on nested block" &
                     Type_Name_Image (Instr.Static_Link, ", type => "),
                     Src_Pos => Instr.Source_Pos,
                     Message_Kind => "Info");
               when Check_Nested_Block_Op =>
                  Messages.Put_RT_Error
                    (Strings.To_String
                       (Strings.To_U_String (Instr.Assertion_Str)) &
                     Type_Name_Image (Instr.Static_Link, ", type => "),
                     Src_Pos => Instr.Source_Pos,
                     Message_Kind => "Error: Assertion failed");
               when Wait_For_Parallel_Op =>
                  Messages.Put_RT_Error
                    ("waiting for subthread to complete",
                     Src_Pos => Instr.Source_Pos,
                     Message_Kind => "Info");
               when others =>
                  Messages.Put_RT_Error ("executing in interpreter",
                    Src_Pos => Instr.Source_Pos,
                    Message_Kind => "Info");
            end case;
         end Put_Using_Message_Format;

      begin  --  Dump_One_Instruction

         if Use_Message_Format then
            Put_Using_Message_Format;
            return;  --  All done  --
         end if;

         Put (Indent & Line_Num & " (" & Opcode_Enum'Image (Instr.Op));
         case Op is
            when Skip_Op | Exit_Op =>
               Put (", Skip_Count =>" & Code_Offset'Image (Instr.Skip_Count));

               case Op is
                  when Exit_Op =>
                     Put (", Level_Diff =>" &
                          Natural'Image (Instr.Level_Diff));
                  --  This indicates how many nested block levels
                  --  the exit statement is exiting from.
                  when others =>
                     null;
               end case;

            when Call_Op | Indirect_Call_Op |
                 Call_Nested_Block_Op | Check_Nested_Block_Op =>
               Put (", Params => " & Obj_Locator_Image (Instr.Params));
               Put (", Static_Link => " &
                    Obj_Locator_Image (Instr.Static_Link));

               --  Points to enclosing local area or type area
               Display_Type_Name (Instr.Static_Link);

               case Op is
                  when Call_Op | Indirect_Call_Op =>
                     Put
                       (", Call_Target => " &
                        Obj_Locator_Image (Instr.Call_Target));

                     if Op = Call_Op then
                        --  Try to display name of target routine
                        Display_Call_Target
                          (Instr.Call_Target, Instr.Static_Link,
                           Instr.Target_Index);
                           --  Index of called routine, which might be abstract
                     end if;

                     if Instr.Locked_Param_Info.Param_Index > 0 then
                     --  If greater than zero, indicates which parameter
                     --  is locked during the execution of the call.
                     --  If zero, then there is no lock acquired, though it
                     --  might be a lock-free operation on a concurrent object.
                        Put
                          (", Locked_Param_Info => (" &
                           Natural'Image
                             (Instr.Locked_Param_Info.Param_Index));
                        if Instr.Locked_Param_Info.Is_Var then
                           Put (", Is_Var");
                        end if;
                        if Instr.Locked_Param_Info.Is_By_Ref then
                           Put (", Is_By_Ref");
                        end if;
                        --  Only parallel calls can be queued
                        pragma Assert
                          (not Instr.Locked_Param_Info.Is_Queued_Call);
                        Put (")");
                     end if;
                     if Instr.Precond_Proved then
                        Put (", Precond_Proved");
                     end if;
                     if Instr.Output_Inited_Null then
                        Put (", Output_Inited_Null");
                     end if;
                     if Op = Indirect_Call_Op then
                        Put
                          (", Indirect_Num_In_Params  =>" &
                           Natural'Image (Instr.Indirect_Num_In_Params));
                           --  Number of input parameters in the indir call

                        Put
                          (", Indirect_Num_Out_Params =>" &
                           Natural'Image (Instr.Indirect_Num_Out_Params));
                           --  Number of output parameters in the indir call
                     end if;
                  when Call_Nested_Block_Op | Check_Nested_Block_Op =>
                     Put
                       (", Code_Block =>" &
                        Code_Block_Image (Instr.Code_Block));
                     --  This describes the nested block of code
                     case Op is
                        when Check_Nested_Block_Op =>
                           Put
                             (", Assertion_Str => " &
                              Strings.To_String
                                 (Strings.To_U_String (Instr.Assertion_Str)));
                        --  A string representation of assertion being checked
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;

            when Return_Op =>
               null;

            when Store_Int_Lit_Op                    |
                 Store_Str_Lit_Op                    |
                 Store_Real_Lit_Op                   |
                 Store_Enum_Lit_Op                   |
                 Store_Char_Lit_Op                   |
                 Store_Operation_Desc_Op             |
                 Copy_Word_Op                        |
                 Copy_Address_Op                     |
                 Store_Address_Op                    |
                 Assign_Word_Op                      |
                 Swap_Obj_Op                         |
                 Move_Obj_Op                         |
                 Store_Local_Null_Op                 |
                 Store_Large_Local_Null_Op           |
                 Store_Null_Of_Same_Stg_Rgn_Op       |
                 Create_Obj_Op                       |
                 Create_Lock_For_Obj_Op              |
                 Create_Polymorphic_Obj_Op           |
                 Unwrap_Polymorphic_Obj_Op           |
                 Store_Type_Related_Obj_Op           |
                 Store_Type_Related_Addr_Op          |
                 Select_Polymorphic_Ancestor_Part_Op |
                 Select_Ancestor_Part_Op             |
                 Make_Copy_In_Stg_Rgn_Op             |
                 Is_Null_Op                          |
                 Not_Null_Op                         |
                 Declare_Obj_Op                      |
                 Check_Not_Null_Op                   =>

               Put (", Destination => " &
                    Obj_Locator_Image (Instr.Destination));

               --  For Create_Polymorphic_Obj_Op, identifies object to be
               --  replaced with it wrapped as a polymorphic object.
               if Instr.Dest_Name /= Strings.Null_U_String_Index then
                  --  If non-null, identifies object being initialized
                  Put (", Dest_Name => " &
                    Printable_String_Image (Strings.To_String
                      (Strings.To_U_String (Instr.Dest_Name))));
               end if;

               case Op is
                  when Store_Local_Null_Op | Check_Not_Null_Op =>
                     --  Type of object determines whether or not object is
                     --  small. Stg_Rgn is the current local region.
                     Put
                       (", Null_Type_Info => " &
                        Obj_Locator_Image (Instr.Null_Type_Info));
                     Display_Type_Name (Instr.Null_Type_Info);

                  when Store_Large_Local_Null_Op =>
                     --  Store a large null of the local region
                     --  Use region associated with given local area
                     Put
                       (", Local_Addr => " &
                        Obj_Locator_Image (Instr.Local_Addr));

                  when Store_Int_Lit_Op =>
                     Put (", Int_Value =>" &
                          Word_Type'Image (Instr.Int_Value));

                  when Store_Char_Lit_Op =>
                     Put
                       (", Char_Value => " &
                        Printable_Char_Image (Instr.Char_Value));

                  when Store_Real_Lit_Op =>
                     Put (", Real_Value => " & Real_Image (Instr.Real_Value));

                  when Store_Str_Lit_Op =>
                     Put
                       (", Str_Value => " &
                        Printable_String_Image (Strings.To_String
                           (Strings.To_U_String (Instr.Str_Value))));

                     if not Is_Null_Obj_Locator
                                (Instr.Existing_Str_In_Stg_Rgn)
                     then
                        --  Determines storage region for literal
                        Put
                          (", Existing_Str_In_Stg_Rgn => " &
                           Obj_Locator_Image
                              (Instr.Existing_Str_In_Stg_Rgn));
                     end if;

                  when Store_Enum_Lit_Op =>
                     Put
                       (", Enum_Value => " &
                        Strings.To_String
                           (Strings.To_U_String (Instr.Enum_Value)));

                  when Declare_Obj_Op =>
                        Put
                          (", Is_By_Ref => " &
                           Boolean'Image (Instr.Is_By_Ref));
                        Put
                          (", Is_Var => " &
                           Boolean'Image (Instr.Is_Var));
                        Put
                          (", Declare_Type_Info => " &
                           Obj_Locator_Image (Instr.Declare_Type_Info));
                        Display_Type_Name (Instr.Declare_Type_Info);

                  when Store_Operation_Desc_Op             |
                       Copy_Word_Op                        |
                       Copy_Address_Op                     |
                       Store_Address_Op                    |
                       Assign_Word_Op                      |
                       Swap_Obj_Op                         |
                       Move_Obj_Op                         |
                       Store_Null_Of_Same_Stg_Rgn_Op       |
                       Create_Obj_Op                       |
                       Create_Polymorphic_Obj_Op           |
                       Unwrap_Polymorphic_Obj_Op           |
                       Store_Type_Related_Obj_Op           |
                       Store_Type_Related_Addr_Op          |
                       Select_Polymorphic_Ancestor_Part_Op |
                       Select_Ancestor_Part_Op             |
                       Make_Copy_In_Stg_Rgn_Op             |
                       Is_Null_Op                          |
                       Not_Null_Op                         =>
                     Put (", Source => " &
                          Ext_Obj_Locator_Image (Instr.Source));
                     --  For Select_*Ancestor_Part_Op, is object
                     --  whose ancestor part is to be extracted.
                     if not Instr.Might_Be_Null then
                        Put (" [non-null]");
                     end if;

                     case Op is
                        when Store_Operation_Desc_Op =>
                           --  Operation_Desc includes both static link and
                           --  routine locator.
                           --  A "null" operation is represented by a null
                           --  static link and a null locator.
                           Put
                             (", Operation_Static_Link => " &
                              Obj_Locator_Image (Instr.Operation_Static_Link));
                           Display_Type_Name (Instr.Operation_Static_Link);
                           Put
                             (", Operation_Locator => " &
                              Obj_Locator_Image (Instr.Operation_Locator));
                           Display_Call_Target (Instr.Operation_Locator,
                                                Static_Link =>
                                                  Instr.Operation_Static_Link);

                        when Assign_Word_Op                      |
                             Swap_Obj_Op                         |
                             Move_Obj_Op                         |
                             Store_Null_Of_Same_Stg_Rgn_Op       |
                             Create_Obj_Op                       |
                             Create_Polymorphic_Obj_Op           |
                             Unwrap_Polymorphic_Obj_Op           |
                             Store_Type_Related_Obj_Op           |
                             Store_Type_Related_Addr_Op          |
                             Select_Polymorphic_Ancestor_Part_Op |
                             Select_Ancestor_Part_Op             |
                             Make_Copy_In_Stg_Rgn_Op             |
                             Is_Null_Op                          |
                             Not_Null_Op                         =>
                           Put
                             (", Type_Info => " &
                              Obj_Locator_Image (Instr.Type_Info));
                           --  Determines whether small or large.
                           --  For Store_Null, determines kind of null if
                           --   small.
                           --  For Create_Obj_Op determines type of obj to
                           --   create if large, and kind of null if small.
                           --  For Select_*Ancestor_Part_Op, determines type of
                           --  ancestor part to be extracted.  This becomes a
                           --  simple
                           --  Copy_Word_Op if the component-extension levels
                           --  are the same between the source object's type-id
                           --  and the ancestor type.
                           --  For Unwrap_Polymorphic_Obj_Op, this is the
                           --  non-polymorphic type for which we are testing.
                           Display_Type_Name (Instr.Type_Info);
                           case Op is
                              when Make_Copy_In_Stg_Rgn_Op =>
                                 Put
                                   (", Existing_Obj_In_Stg_Rgn => " &
                                    Obj_Locator_Image
                                       (Instr.Existing_Obj_In_Stg_Rgn));

                              --  Determines region for Make_Copy
                              when Select_Ancestor_Part_Op |
                                Unwrap_Polymorphic_Obj_Op  |
                                Store_Type_Related_Obj_Op  |
                                Store_Type_Related_Addr_Op =>
                                 Put
                                   (", Source_Type_Info => " &
                                    Obj_Locator_Image
                                      (Instr.Source_Type_Info));
                                 Display_Type_Name (Instr.Source_Type_Info);
                                 case Op is
                                    when Select_Ancestor_Part_Op =>
                                       Put
                                         (", Ancestor_Lvalue => " &
                                          Boolean'Image
                                            (Instr.Ancestor_Lvalue));
                                    when others =>
                                       null;
                                 end case;

                              when Select_Polymorphic_Ancestor_Part_Op =>
                                 Put
                                   (", Polymorphic_Ancestor_Lvalue => " &
                                    Boolean'Image
                                      (Instr.Polymorphic_Ancestor_Lvalue));

                              when others =>
                                 null;
                           end case;
                        when others =>
                           null;
                     end case;
                  when others =>
                     null;
               end case;

            when If_Op =>
               Put (", If_Source => " &
                    Ext_Obj_Locator_Image (Instr.If_Source));
               Put
                 (", If_Condition =>" &
                  Condition_Bit_Mask'Image (Instr.If_Condition));
               Put
                 (", Skip_If_False =>" &
                  Code_Offset'Image (Instr.Skip_If_False));

            when Start_Parallel_Op      |
                 Start_Handled_Op       |
                 Add_Parallel_Op        |
                 Wait_For_Parallel_Op   |
                 Start_Parallel_Call_Op |
                 Add_Parallel_Call_Op   |
                 Create_Tcb_Op          |
                 Prepare_To_Exit_Parallel_Op =>

               Put
                 (", Parallel_Master => " &
                  Obj_Locator_Image (Instr.Parallel_Master));

               case Op is
                  when Start_Parallel_Op      |
                       Start_Handled_Op       |
                       Add_Parallel_Op        |
                       Start_Parallel_Call_Op |
                       Add_Parallel_Call_Op   |
                       Create_Tcb_Op          =>

                     Put
                       (", Parallel_Control => " &
                        Obj_Locator_Image (Instr.Parallel_Control));

                     Put
                       (", Parallel_Static_Link => " &
                        Obj_Locator_Image (Instr.Parallel_Static_Link));
                        --  Points to enclosing local area or type area
                     Display_Type_Name (Instr.Parallel_Static_Link);

                     Put
                       (", Num_In_Params  =>" &
                        Natural'Image (Instr.Num_In_Params));
                        --  Number of input parameters in the parallel call

                     Put
                       (", Num_Out_Params =>" &
                        Natural'Image (Instr.Num_Out_Params));
                        --  Number of output parameters in the parallel call

                     case Op is
                        when Start_Parallel_Op |
                             Start_Handled_Op  |
                             Add_Parallel_Op   =>

                           Put
                             (", Parallel_Code_Block =>" &
                              Code_Block_Image (Instr.Parallel_Code_Block));

                        when Start_Parallel_Call_Op | Add_Parallel_Call_Op =>

                           Put
                             (", Parallel_Call_Target => " &
                              Obj_Locator_Image (Instr.Parallel_Call_Target));
                           --  This identifies the operation to be called
                           Display_Call_Target
                             (Instr.Parallel_Call_Target,
                              Instr.Parallel_Static_Link,
                              Instr.Parallel_Target_Index);
                           --  Index of called routine, which might be abstract
                           Put (", Parallel_Target_Index =>" &
                                Routine_Index'Image
                                  (Instr.Parallel_Target_Index));

                           if Instr.Parallel_Locked_Param_Info.Param_Index
                             > 0
                           then
                              --  Indicates which param, if any, is locked
                              --  during the execution of the call.
                              Put
                                (", Parallel_Locked_Param_Info => (" &
                                 Natural'Image
                                   (Instr.Parallel_Locked_Param_Info.
                                      Param_Index));
                              if Instr.Parallel_Locked_Param_Info.Is_Var then
                                 Put (", Is_Var");
                              end if;
                              if Instr.Parallel_Locked_Param_Info.
                                Is_By_Ref
                              then
                                 Put (", Is_By_Ref");
                              end if;
                              if Instr.Parallel_Locked_Param_Info.
                                Is_Queued_Call
                              then
                                 --  Indicates this is a queued call.
                                 Put (", Is_Queued_Call");
                              end if;
                              Put (")");
                           end if;
                           if Instr.Parallel_Precond_Proved then
                              Put (", Precond_Proved");
                           end if;
                           if Instr.Parallel_Output_Inited_Null then
                              Put (", Output_Inited_Null");
                           end if;
                        when Create_Tcb_Op =>
                           null;

                        when others =>
                           null;
                     end case;
                  when Wait_For_Parallel_Op =>
                     if Instr.Skip_Counts /= null then
                        --  Array of skip counts which can occur on exit
                        Put
                          (", Skip_Counts => (");
                        for I in Instr.Skip_Counts'Range loop
                           if I > Instr.Skip_Counts'First then
                              Put (",");
                           end if;
                           Put (Code_Offset'Image (Instr.Skip_Counts (I)));
                        end loop;
                        Put (")");
                     end if;

                  when others =>
                     null;
               end case;

            when Case_Op =>
               Put
                 (", Case_Selector => " &
                  Ext_Obj_Locator_Image (Instr.Case_Selector));
               Put (", Case_First =>" &
                    Non_Null_Value'Image (Instr.Case_First));
               --  Skips number of instructions determined by
               --    Case_Selector - Case_First
               --      if Case_Selector in Case_First .. Case_Last.
               Put (", Case_Last =>" & Non_Null_Value'Image (Instr.Case_Last));
               Put
                 (", Case_Default_Skip =>" &
                  Code_Offset'Image (Instr.Case_Default_Skip));
               --  Number of instructions to skip
               --  if Case_Selector not in Case_First .. Case_Last.
               --  Negative for loop back.

            when Begin_Nested_Block_Op =>
               Put
                 (", Nested_Code_Block =>" &
                  Code_Block_Image (Instr.Nested_Code_Block));
               --  Description of nested block.
               --  Pc_Offset always identifies self.

               Put
                 (", Nested_Block_Region =>" &
                  Block_Region_Index'Image (Instr.Nested_Block_Region));
               --  Region index of associated region.
               --  Convert to Symbols.Region_Index to identify Region

            when Loop_Op =>
               null;
         end case;

         Put_Line (")");
      end Dump_One_Instruction;

      ----------------
      -- Dump_Param --
      ----------------

      procedure Dump_Param
        (Param          : Routine_Param_Info;
         Use_Short_Form : Boolean := False)
      is
         pragma Assert (not Param.Compiled);
         Param_Name : String renames Strings.To_String (Param.Name);
         Type_Name  : String renames Strings.To_String (Param.Type_Name);
      begin
         if Param.Is_Declared_Ref then
            if not Param.Is_Passed_By_Ref then
               Put ("by-copy ");
            end if;
            Put ("ref ");
         elsif Param.Is_Passed_By_Ref then
            Put ("by-ref ");
         end if;

         if Param.Is_Var and then not Param.Is_Operation_Output then
            Put ("var ");
         end if;

         if Param_Name /= Strings.Simple_Name (Type_Name) then
            Put (Param_Name & " : ");
         end if;

         if Param.Is_Optional then
            Put ("optional ");
         end if;

         Put (Type_Name);

         if not Use_Short_Form then
            Put
              (ASCII.HT &
               "// " &
               Trees.Subtree_Image (Param.Decl, Use_Short_Form => True));

            if Param.Is_Of_Current_Inst_Type then
               Put (" *cur-inst*");
            end if;

            New_Line;
         else
            if Param.Is_Of_Current_Inst_Type then
               --  Just indicate cur-inst with an "*"
               Put ("*");
            end if;
         end if;
      end Dump_Param;

      ----------------------
      -- Dump_Param_Decls --
      ----------------------

      procedure Dump_Param_Decls (Code : Routine_Ptr) is
         Num_Inputs  : Natural := 0;
         Num_Outputs : Natural := 0;
      begin
         if Code.Parameters = null then
            Put ("(?no param info?)");
            return;
         end if;

         --  First the operation inputs
         Put ("(");
         for I in Code.Parameters'Range loop
            declare
               Param : Routine_Param_Info renames Code.Parameters (I);
            begin
               if Param.Is_Operation_Output then
                  Num_Outputs := Num_Outputs + 1;
               else
                  Num_Inputs := Num_Inputs + 1;
                  if Param.Compiled then
                     Put ("??");
                  else
                     Dump_Param (Param, Use_Short_Form => True);
                  end if;
                  if I < Code.Parameters'Last then
                     Put ("; ");
                  end if;
               end if;
            end;
         end loop;
         Put (")");

         if Num_Outputs > 0 then
            --  Now the operation outputs
            Put (" -> ");
            if Num_Outputs > 1 then
               Put ("(");
            end if;
            for I in Code.Parameters'Range loop
               declare
                  Param : Routine_Param_Info renames Code.Parameters (I);
               begin
                  exit when not Param.Is_Operation_Output;
                  if Param.Compiled then
                     Put ("??");
                  else
                     Dump_Param (Param, Use_Short_Form => True);
                  end if;
                  if I < Num_Outputs then
                     Put ("; ");
                  end if;
               end;
            end loop;
            if Num_Outputs > 1 then
               Put (")");
            end if;
         end if;
         New_Line;
      end Dump_Param_Decls;

      -----------------------
      -- Dump_Param_Values --
      -----------------------

      procedure Dump_Param_Values
        (Code        : Routine_Ptr;
         Context     : in out Exec_Context;
         On_Entering : Boolean)
      is
         Num_Inputs  : Natural := 0;
         Num_Outputs : Natural := 0;
      begin
         if Context.Enclosing_Type /= null then
            Put_Line
              (" Enclosing_Type is " &
               Strings.To_String (Context.Enclosing_Type.Name));
         end if;

         if Code.Parameters = null then
            Put (" [no param info]");
            Put (" Param 0 = ");
            Dump_Obj
              (Content_Of_Physical_Address
                  (Locator_To_Physical_Address
                     (Context, (Param_Area, 0, No_VM_Obj_Id))));
            return;
         end if;

         --  First the operation inputs
         Put_Line ("Inputs:");
         for I in Code.Parameters'Range loop
            declare
               Param : Routine_Param_Info renames Code.Parameters (I);
               use type Trees.Optional_Tree;
            begin
               if Param.Is_Operation_Output then
                  Num_Outputs := Num_Outputs + 1;

               elsif not Param.Compiled
                 and then Param.Decl /= Trees.Null_Optional_Tree
               then
                  --  This is an input
                  --  (Param.Decl can be null if some of the inputs
                  --  were actually Implicit_Module parameters).
                  declare
                     Param_Value : Word_Type :=
                       Content_Of_Physical_Address
                          (Locator_To_Physical_Address
                              (Context,
                               (Param_Area, Offset_Within_Area (I - 1),
                                No_VM_Obj_Id)));
                  begin
                     Num_Inputs := Num_Inputs + 1;
                     if Param.Is_Passed_By_Ref then
                        Param_Value :=
                          Content_Of_Physical_Address
                            (Word_To_Word_Ptr (Param_Value));
                     end if;

                     Dump_Param (Param);
                     Dump_Obj_With_Type
                       (Param_Value,
                        Type_Desc => Get_Type_Desc (Context, Param.Type_Info));
                  end;
               end if;
            end;
         end loop;

         if Num_Outputs > 0 and then not On_Entering then
            --  Now the operation outputs
            Put_Line ("Outputs:");
            for I in Code.Parameters'Range loop
               declare
                  Param : Routine_Param_Info renames Code.Parameters (I);
               begin
                  exit when not Param.Is_Operation_Output;

                  declare
                     Param_Value : Word_Type :=
                       Content_Of_Physical_Address
                          (Locator_To_Physical_Address
                              (Context,
                               (Param_Area, Offset_Within_Area (I - 1),
                                No_VM_Obj_Id)));
                  begin
                     if Param.Is_Passed_By_Ref then
                        Param_Value :=
                          Content_Of_Physical_Address
                            (Word_To_Word_Ptr (Param_Value));
                     end if;

                     if Param.Compiled then
                        Put ("Output" & Integer'Image (I));
                        Dump_Obj (Param_Value);
                     else
                        Dump_Param (Param);
                        Dump_Obj_With_Type
                          (Param_Value,
                           Type_Desc =>
                             Get_Type_Desc (Context, Param.Type_Info));
                     end if;
                  end;
               end;
            end loop;
         end if;

         New_Line;
      end Dump_Param_Values;

      ------------------
      -- Dump_Routine --
      ------------------

      procedure Dump_Routine (Code : Routine_Ptr) is
         use Ada.Text_IO;
         use type Strings.U_String;
      begin
         if Code.Name /= Strings.Null_U_String then
            if Code.Full_Module_Name /= Strings.Null_U_String then
               Put (Strings.To_String (Code.Full_Module_Name) & "::");
            end if;
            Put (Strings.To_String (Code.Name) & ": ");
         end if;

         Put_Line ("Routine #" & Routine_Index'Image (Code.Index));

         if Code.Parameters /= null then
            --  Display info on parameters
            Dump_Param_Decls (Code);
         end if;

         for Bc in Code.Boundary_Conditions'Range loop
            if Code.Boundary_Conditions (Bc).Pc_Offset > 0 then
               Put_Line
                 (" " &
                  Boundary_Condition_Enum'Image (Bc) &
                  " at" &
                  Code_Offset'Image
                     (Code.Boundary_Conditions (Bc).Pc_Offset +
                      Code_Index'First));
            end if;
         end loop;

         Put_Line (" Uses_Queuing = " & Boolean'Image (Code.Uses_Queuing));

         if not Code.Is_PSVM_Routine then
            Put_Line (" Routine is imported, desig = " &
              Strings.To_String (Code.Built_In_Desig));
         else
            Put_Line
              (" Local_Area_Length =" &
               Offset_Within_Area'Image (Code.Local_Area_Length));
            Put_Line
              (" Start_Callee_Locals =" &
               Offset_Within_Area'Image (Code.Start_Callee_Locals));
            if Code.Code /= null then
               --  Not an abstract routine "stub"
               Put_Line (" Uses_Stg_Rgn = " &
                 Boolean'Image (Code.Code.Uses_Stg_Rgn));
               Put_Line
                 (" Code_Length =" &
                  Code_Length_Type'Image (Code.Code.Code_Length));
               for I in 1 .. Code.Code.Code_Length loop
                  Debug.Dump_One_Instruction (Code.Code.Instrs (I));
               end loop;
            end if;
         end if;
      end Dump_Routine;

      -----------------------
      -- Dump_Routine_Info --
      -----------------------

      procedure Dump_Routine_Info
        (Info              : Routine_Info;
         Current_Op_Index  : Operation_Index := 0;
         Current_Type_Desc : Type_Descriptor_Ptr := null)
      is
      begin
         if Info.Index
           in 1 .. Routine_Index (Num_Elements (Routine_Table))
         then
            declare
               Code : constant Routine_Ptr := Nth_Routine (Info.Index);
            begin
               if Code /= null then
                  Put ("  " & Strings.To_String (Code.Name) & ":");
               else
                  Put (" ");
               end if;
            end;
         else
            Put (" ");
         end if;

         Put (" Routine #" & Routine_Index'Image (Info.Index));

         if Info.Action /= No_Action then
            Put (" (Action: " & Wrapper_Action_Enum'Image (Info.Action) & ")");
         end if;

         if Info.Op_Index /= Current_Op_Index then
            Put (" (Op_Index:" & Operation_Index'Image (Info.Op_Index) & ")");
         end if;

         if Info.Use_Static_Link_For_Type then
            Put (" (is generic op)");
         end if;

         if Info.Type_Desc /= null
           and then Info.Type_Desc /= Current_Type_Desc
         then
            Put_Line
              (" [of " &
               Type_Sem_Image
                  (Info.Type_Desc.Type_Sem,
                   Use_Short_Form => True) &
               "]");
         else
            New_Line;
         end if;
      end Dump_Routine_Info;

      ----------------
      -- Dump_State --
      ----------------

      procedure Dump_State is
      begin
         Thread_Manager.Dump_Thread_State;
      end Dump_State;

      ----------------------------
      -- Dump_Type_Element_Data --
      ----------------------------

      procedure Dump_Type_Element_Data
        (Data   : Element_Info;
         Indent : String := "") is
      begin
         Put_Line
           (Indent & "Type: " & Type_Desc_Name_And_Num (Data.Type_Desc));
         Put_Line
           (Indent & "Addr: " & Hex_Image (Data.Addr));

         if Data.Value = Null_Virtual_Address
           or else
            Data.Value = Null_Value
         then
            Put_Line (Indent & "Value: " & Hex_Image (Data.Value));
         else
            Put_Line (Indent & "Value:");
            Dump_Obj_With_Indent
              (Data.Value,
               Data.Type_Desc,
               Indent => Indent'Length + 1);
         end if;
      end Dump_Type_Element_Data;

      --------------------
      -- Dump_Type_Desc --
      --------------------

      procedure Dump_Type_Desc (Type_Desc : Type_Descriptor_Ptr) is
      begin
         Put_Line ("---- Type Descriptor ----");
         Put_Line
           (Strings.To_String (Type_Desc.Name) &
            ": Type_Desc #" &
            Type_Index'Image (Type_Desc.Index));
         Put_Line
           (" Location: " &
            Obj_Locator_Image (Type_Desc.Location) &
            ", Is_Finished = " &
            Boolean'Image (Type_Desc.Is_Finished));
         Put_Line (" Type: " & Type_Sem_Image (Type_Desc.Type_Sem));

         if Type_Desc.Corresponding_Polymorphic_Type_Desc /= null then
            Put_Line (" Polymorphic type: " &
              Type_Desc_Name_And_Num
                (Type_Desc.Corresponding_Polymorphic_Type_Desc));
         end if;

         Put (" Kind: " & Type_Kind_Enum'Image (Type_Desc.Type_Kind));

         if Type_Desc.Is_Small then
            Put (", Is_Small");
         end if;

         if Type_Desc.Is_Wrapper then
            Put (", Is_Wrapper");
         end if;

         if Type_Desc.Is_Polymorphic then
            Put (", Is_Polymorphic");
         end if;

         if not Type_Desc.Has_Op_Map then

            if Type_Desc.Is_Abstract then
               Put (", Is_Abstract");
            end if;

            if Type_Desc.Is_Partially_Abstract then
               Put (", Is_Partially_Abstract");
            end if;

            if Type_Desc.Is_Concurrent then
               Put (", Is_Concurrent");
            end if;

         end if;

         New_Line;

         if Type_Desc.Is_Small then
            Put_Line (" Null_Value = " & Hex_Image (Type_Desc.Null_Value));
         end if;

         if Type_Desc.Has_Op_Map then
            Put_Line
              (" Has_Op_Map for actual type #" &
               Type_Index'Image (Type_Desc.Actual_Type.Index) &
               " " & Type_Sem_Image (Type_Desc.Actual_Type.Type_Sem) &
               " implementing formal type " &
               Type_Sem_Image (Type_Desc.Formal_Type_Sem));

            if Type_Desc.Op_Map /= null then
               for I in Type_Desc.Op_Map'Range loop
                  Put_Line
                    (" " &
                     Operation_Index'Image (I) &
                     " =>" &
                     Operation_Index'Image (Type_Desc.Op_Map (I)));
               end loop;
            end if;
         else
            if Type_Desc.Root_Type_Desc /= null then
               Put_Line
                 (" Root_Of_Polymorphic: " &
                  Type_Sem_Image (Type_Desc.Root_Type_Desc.Type_Sem));
            end if;

            if Type_Desc.Enclosing_Type /= null then
               Put_Line
                 (" Encloser: #" & Type_Index'Image
                    (Type_Desc.Enclosing_Type.Index) & " " &
                  Type_Sem_Image (Type_Desc.Enclosing_Type.Type_Sem));
            end if;

            if Type_Desc.Parent_Type /= null then
               Put_Line
                 (" Parent: #" & Type_Index'Image
                    (Type_Desc.Parent_Type.Index) & " " &
                  Type_Sem_Image (Type_Desc.Parent_Type.Type_Sem));
            end if;

            if Type_Desc.Component_Extension_Level > 0 then
               Put_Line
                 (" Component_Extension_Level:" &
                  Natural'Image (Type_Desc.Component_Extension_Level));
            end if;

            if Type_Desc.Parameters /= null then
               for I in Type_Desc.Parameters'Range loop
                  Put_Line
                    ("  Param" &
                     Natural'Image (I) &
                     " of kind " &
                     Parameter_Kind_Enum'Image
                       (Type_Desc.Parameters (I).Kind) &
                     ":");
                  Dump_Type_Element_Data
                    (Type_Desc.Parameters (I).Data,
                     Indent => "   ");
               end loop;
            end if;

            if Type_Desc.Actuals_Of_Formals /= null then
               for I in Type_Desc.Actuals_Of_Formals'Range loop
                  Put_Line
                    ("  Actual of Formal" &
                     Natural'Image (I) &
                     " of kind " &
                     Parameter_Kind_Enum'Image
                        (Type_Desc.Actuals_Of_Formals (I).Kind) &
                     ":");
                  Dump_Type_Element_Data
                    (Type_Desc.Actuals_Of_Formals (I).Data,
                     Indent => "   ");
               end loop;
            end if;

            if Type_Desc.Components /= null then
               for I in Type_Desc.Components'Range loop
                  declare
                     Comp : Component_Info renames Type_Desc.Components (I);
                     use Trees;
                  begin
                     Put ("  Component" & Natural'Image (I) & ": ");
                     if Not_Null (Comp.Decl)
                       and then
                        Tree_Ptr_Of (Comp.Decl).all in Obj_Decl.Tree'Class
                     then
                        Put (Subtree_Image
                         (Obj_Decl.Tree'Class
                           (Tree_Ptr_Of (Comp.Decl).all).Name) & " : ");
                     end if;

                     if Comp.Is_By_Ref then
                        Put ("ref ");
                     end if;

                     Put_Line
                       (Type_Desc_Name_And_Num
                           (Comp.Type_Desc));
                  end;
               end loop;
            end if;

            if Type_Desc.Nested_Types /= null then
               for I in Type_Desc.Nested_Types'Range loop
                  Put_Line
                    ("  Nested type" &
                     Natural'Image (I) &
                     ": " &
                     Type_Desc_Name_And_Num (Type_Desc.Nested_Types (I)));
               end loop;
            end if;

            if Type_Desc.Nested_Objs /= null then
               Put_Line (" Nested objects:");

               for I in Type_Desc.Nested_Objs'Range loop
                  Put_Line
                    ("  const " &
                     Strings.To_String (Type_Desc.Nested_Objs (I).Name) &
                     ":");
                  Dump_Type_Element_Data
                    (Type_Desc.Nested_Objs (I).Data,
                     Indent => "   ");
               end loop;
            end if;

            if Type_Desc.Operations /= null then
               Put_Line (" Operations:");

               for I in Type_Desc.Operations'Range loop
                  Dump_Routine_Info (Type_Desc.Operations (I),
                    Current_Op_Index  => I,
                    Current_Type_Desc => Type_Desc);
               end loop;
            end if;
         end if;

         Put_Line ("---- end type descriptor ----");
      end Dump_Type_Desc;

      ---------------
      -- Hex_Image --
      ---------------

      function Hex_Image
        (Addr              : Object_Virtual_Address;
         Underscores_Every : Natural := 4) return String
      is
         package Word_IO is new Ada.Text_IO.Integer_IO (Word_Type);

         Result         : String (1 .. 22);
                              --  22 >= 16 digits + 4 for radix + 1 space
         Result_With_Underscores : String (1 .. 44);
                              --  2 * 22 to make static analyzer happy
         J              : Natural;
         Between_Sharps : Boolean := False;
         Modulus        : Natural := Underscores_Every;
      begin
         if Modulus = 0 then
            --  No underscores.
            Modulus := 100;
         end if;

         Word_IO.Put (Result, Addr, Base => 16);

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

      function Hex_Image
        (Addr              : Word_Ptr;
         Underscores_Every : Natural := 4) return String
      is
      begin
         return Hex_Image (Word_Ptr_To_Word (Addr), Underscores_Every);
      end Hex_Image;

      function Hex_Image
        (Addr              : Routine_Code_Address;
         Underscores_Every : Natural := 4) return String
      is
      begin
         return Hex_Image
           (Routine_Code_Address_To_Word (Addr), Underscores_Every);
      end Hex_Image;

      -------------------
      -- Integer_Value --
      -------------------

      function Integer_Value (Img : String) return Word_Type is
      begin
         if Img = "null" then
            return Null_Value;
         elsif Img'Length > 2 and then Img (Img'First) = '0' then
            case Img (Img'First + 1) is
               when 'x' | 'X' =>
                  --  Base 16
                  return Word_Type'Value
                           ("16#" & Img (Img'First + 2 .. Img'Last) & '#');
               when 'b' | 'B' =>
                  --  Base 2
                  return Word_Type'Value
                           ("2#" & Img (Img'First + 2 .. Img'Last) & '#');
               when others =>
                  null;
            end case;
         end if;

         return Word_Type'Value (Img);
      exception
         when others =>
            return Null_Value;
      end Integer_Value;

      -----------------------
      -- Obj_Address_Image --
      -----------------------

      function Obj_Address_Image (Addr : Object_Address) return String is
      begin
         if Addr.Enclosing_Chunk /= null then
            return "(Chunk => " &
                   Chunk_Index'Image
                      (Addr.Enclosing_Chunk.Index) &
                   ", Offset =>" &
                   Offset_Within_Chunk'Image (Addr.Offset) &
                   ')';
         else
            return "(Chunk => null, Offset =>" &
                   Offset_Within_Chunk'Image (Addr.Offset) &
                   ')';
         end if;
      end Obj_Address_Image;

      -----------------------
      -- Obj_Locator_Image --
      -----------------------

      function Obj_Locator_Image (Locator : Object_Locator) return String is

         function VM_Info_Image (VM_Obj_Id : VM_Obj_Id_Type) return String;
            --  Return VM_Obj_Id info

         function VM_Info_Image (VM_Obj_Id : VM_Obj_Id_Type) return String is
            --  Return VM_Obj_Id info
            Num_Image : constant String :=
              VM_Obj_Unique_Num'Image (VM_Obj_Id.Num);
            subtype NB is Positive range Num_Image'First + 1 .. Num_Image'Last;
               --  Non-blank part of 'Image

            function Augmented_Num_Image return String;
            --  Add in the appropriate number of "*" in front

            function Augmented_Num_Image return String is
            begin
               case VM_Obj_Id.Indir is
                  when -1 => return " &" & Num_Image (NB);
                  when  0 => return Num_Image;
                  when +1 => return " *" & Num_Image (NB);
                  when +2 => return " **" & Num_Image (NB);
                  when +3 => return " ***" & Num_Image (NB);
               end case;
            end Augmented_Num_Image;

         begin  --  VM_Info_Image

            if VM_Obj_Id.Kind = Component_Kind then
               --  Probably fetching output parameter following a TCB
               --  Return num[offset]
               return Augmented_Num_Image & '[' &
                 Offset_Within_Area'Image (VM_Obj_Id.Offset) & ']';

            elsif VM_Obj_Id.Kind = Local_Kind
              and then VM_Obj_Id.First_Call_Param_Num > 0
            then
               --  Include first-call-param-num
               return Augmented_Num_Image &
                 VM_Obj_Unique_Num'Image (VM_Obj_Id.First_Call_Param_Num) &
                 "..";  --  Include First_Call_Param_Num at end
            else
               return Augmented_Num_Image;
            end if;
         end VM_Info_Image;

      begin  --  Obj_Locator_Image

         case Locator.Base is
         when Local_Area | Enclosing_Local_Areas |
              Base_Registers | Phys_Base_Registers =>
            return '(' &
                   Area_Base_Image (Locator.Base) &
                   "," &
                   Offset_Within_Area'Image (Locator.Offset) &
                   "," &
                   VM_Info_Image (Locator.VM_Obj_Id) &
                   ')';
         when others =>
            return '(' &
                   Area_Base_Image (Locator.Base) &
                   "," &
                   Offset_Within_Area'Image (Locator.Offset) &
                   ')';
         end case;
      end Obj_Locator_Image;

      ----------------
      -- Real_Image --
      ----------------

      function Real_Image (Val : Univ_Real) return String is

         package Univ_Real_IO is new Ada.Text_IO.Float_IO (Univ_Real);

      begin
         if From_Univ_Real (Val) = Interpreter.Null_Float_Value then
            return "null";
         else
            declare
               Image           : String (1 .. 40);
               Abs_Val         : constant Univ_Real := abs Val;
               First_Non_Blank : Natural := Image'First;
               Last_Non_Zero   : Natural := Image'Last;
               Aft             : Natural := Univ_Real'Digits - 1;
            begin
               if Abs_Val = 0.0
                 or else Abs_Val in 10.0 ** (-3) .. 10.0 ** 10
               then
                  --  Avoid scientific notation if possible
                  if Abs_Val > 10.0 ** 5 then
                     Aft := Aft - 5;
                     if Abs_Val > 10.0 ** 8 then
                        Aft := Aft - 3;
                     end if;
                  elsif Abs_Val > 10.0 ** 2 then
                     Aft := Aft - 2;
                  elsif Abs_Val > 10.0 then
                     Aft := Aft - 1;
                  end if;

                  Univ_Real_IO.Put (Image, Val, Aft => Aft, Exp => 0);

                  --  Find last non-zero
                  for I in reverse Image'Range loop
                     if Image (I) /= '0' then
                        if Image (I) = '.' then
                           Last_Non_Zero := I + 1;
                        else
                           Last_Non_Zero := I;
                        end if;
                        exit;
                     end if;
                  end loop;
               else
                  --  Use scientific notation
                  Univ_Real_IO.Put (Image, Val, Aft => Aft, Exp => 4);

                  --  Compress out unneeded zeroes
                  for I in reverse Image'Range loop
                     if Image (I) = 'E' then
                        --  Found the exponent
                        --  Now suppress zeroes in both directions
                        declare
                           Copy_Digits : Boolean := False;
                        begin
                           Last_Non_Zero := I - 1;
                           for J in reverse Image'First .. I - 1 loop
                              if Image (J) /= '0' then
                                 if Image (J) = '.' then
                                    Last_Non_Zero := J + 1;
                                 else
                                    Last_Non_Zero := J;
                                 end if;
                                 exit;
                              end if;
                           end loop;

                           for J in I .. Image'Last loop
                              if Image (J) not in '0' .. '9' then
                                 --  Copy non digits
                                 Last_Non_Zero := Last_Non_Zero + 1;
                                 Image (Last_Non_Zero) := Image (J);
                              else
                                 if Image (J) /= '0' then
                                    Copy_Digits := True;
                                 end if;

                                 if Copy_Digits then
                                    --  Copy significant digits
                                    Last_Non_Zero := Last_Non_Zero + 1;
                                    Image (Last_Non_Zero) := Image (J);
                                 end if;
                              end if;
                           end loop;

                           exit;
                        end;
                     end if;
                  end loop;
               end if;

               --  Find first non blank
               for I in Image'Range loop
                  if Image (I) /= ' ' then
                     First_Non_Blank := I;
                     exit;
                  end if;
               end loop;

               --  Skip leading blanks and trailing zeroes
               return Image (First_Non_Blank .. Last_Non_Zero);
            end;
         end if;
      end Real_Image;

      ----------------
      -- Real_Value --
      ----------------

      function Real_Value (Img : String) return Univ_Real is
      begin
         if Img = "null" then
            return Null_Real_Value;
         else
            --  TBD: Handle bases > 16
            return Univ_Real'Value (Img);
         end if;
      exception
         when others =>
            return Null_Real_Value;
      end Real_Value;

      --------------------
      -- Type_Sem_Image --
      --------------------

      function Type_Sem_Image
        (Type_Sem       : Trees.Root_Sem_Ptr;
         Use_Short_Form : Boolean := False)
         return String is
      --  Return an image of a type, given its type semantic info
         use type Trees.Root_Sem_Ptr;
      begin
         if Type_Sem = null then
            return "[null type]";
         else
            --  Make a dispatching call to reach Sem_Image for Type_Sem's
            return Trees.Sem_Image (Type_Sem, Use_Short_Form);
         end if;
      end Type_Sem_Image;

      ---------------------------
      -- Virt_To_Large_Obj_Ptr --
      ---------------------------

      function Virt_To_Large_Obj_Ptr
        (Large_Obj_Virt_Addr : Object_Virtual_Address)
         return Large_Obj_Header_Ptr
      is
      begin
         return
           Addr_To_Large_Obj_Ptr
             (Virtual_To_Physical_Address (Large_Obj_Virt_Addr).all'Address);
      end Virt_To_Large_Obj_Ptr;

   end Debug;

   --------------- Inlined Visible Subprograms ------------------

   function "+"
     (Base   : Object_Address;
      Offset : Offset_Within_Area) return Object_Address is
   begin
      return (Base.Enclosing_Chunk, Base.Offset + Offset);
   end "+";

   function "+"
     (Base   : Object_Virtual_Address;
      Offset : Offset_Within_Area) return Object_Virtual_Address is
   begin
      if Virt_Is_Phys then
         return Base + Word_Type (Offset) * Word_SU_Size;
      else
         return Base + Word_Type (Offset);
      end if;
   end "+";

   ---------------- Visible Subprograms -----------------

   ---------
   -- Add --
   ---------

   function Add (Base : Word_Ptr; Offset : Offset_Within_Area)
     return Word_Ptr is
   --  Do a word-pointer add of base + offset
      use System.Storage_Elements;
      SUs_Per_Word : constant := Word_Type'Size / System.Storage_Unit;
      function To_Addr is
        new Ada.Unchecked_Conversion (Word_Ptr, System.Address);
   begin
      return To_Word_Ptr
        (To_Addr (Base) + Storage_Offset (Offset) * SUs_Per_Word);
   end Add;

   ---------------------------
   -- Allocate_From_Stg_Rgn --
   ---------------------------

   function Allocate_From_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Server_Index  : Thread_Server_Index) return Word_Type
   --  This attempts to reuse storage freed by Deallocate_From_Stg_Rgn.
   --  Initialize allocated space with region/size/null-type.
   is
      Result : Word_Type;
   begin
      if Server_Index = Stg_Rgn.Owning_Server then
         --  Owner is doing the allocation
         --  Use unshared part of storage region -- no locking needed
         Allocate_From_Unshared_Stg_Rgn
           (Stg_Rgn, Size_In_Words, Result, Server_Index);
      else
         --  Use shared part of storage region and get a lock
         Stg_Rgn.Shared_Part.Manager.Allocate_From_Stg_Rgn
           (Stg_Rgn.Shared_Part, Size_In_Words, Result, Server_Index);
      end if;
      return Result;
   end Allocate_From_Stg_Rgn;

   ------------------------------------
   -- Allocate_From_Unshared_Stg_Rgn --
   ------------------------------------

   procedure Allocate_From_Unshared_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Obj_Addr      : out Word_Type;
      Server_Index  : Thread_Server_Index)
   is
      --  This attempts to reuse storage freed by
      --  Deallocate_From_Unshared_Stg_Rgn.
      --  Initialize allocated space with region/size/null-type.
      --  Caller gets a lock if needed.
      use type Hash_Value;

      --  True if allocating from region owned by server
      By_Owner : constant Boolean := Server_Index = Stg_Rgn.Owning_Server;
      --  NOTE: If not By_Owner, caller should have gotten the lock

      Reusing : constant Boolean := True;

   begin  --  Allocate_From_Unshared_Stg_Rgn

      if Stg_Rgn.Reclamation /= null
        and then Size_In_Words >= Min_Reclaimable_Block_Size
      then
         --  See whether there are any reclaimable blocks
         declare
            Index : constant Hash_Value :=
              Hash_Value (Size_In_Words) rem  --  Size > 0 so rem==mod
              (Stg_Rgn.Reclamation.Modulus_Minus_One + 1);
            Ptr : Reclaimable_Block_Ptr :=
              Stg_Rgn.Reclamation.Reclamation_Table (Index);
         begin
            if Ptr /= Null_Virtual_Address then
               --  Scan for a block of matching size
               declare
                  Prev : Reclaimable_Block_Ptr := Null_Virtual_Address;
               begin
                  loop
                     if Large_Obj_Size (Ptr) = Size_In_Words then
                        --  Found a block of the right size
                        --  Unlink it
                        if Debug_Stg_Rgns then
                           Put_Line
                             (" Able to reuse space for obj of size" &
                              Offset_Within_Area'Image (Size_In_Words) &
                              " in region #" &
                              Stg_Rgn_Index'Image (Stg_Rgn.Index) &
                              " at " &
                              Hex_Image (Ptr));
                        end if;

                        if Prev /= Null_Virtual_Address then
                           Set_Large_Obj_Next_Block
                             (Prev,
                              Large_Obj_Next_Block (Ptr));
                        else
                           Stg_Rgn.Reclamation.Reclamation_Table (Index) :=
                              Large_Obj_Next_Block (Ptr);
                        end if;

                        --  Initialize object header
                        Set_Large_Obj_Header
                          (Ptr,
                           Size => Size_In_Words,
                           Stg_Rgn_Id => Stg_Rgn.Index,
                           Type_Id => 0,
                           Lock_Obj => 0);

                        --  Return its address
                        Obj_Addr := Ptr;

                        if Debug_Statistics then
                           --  Statistics; Bump counter for reusing block
                           Stg_Rgn_Stats (By_Owner, Reusing) :=
                             Stg_Rgn_Stats (By_Owner, Reusing) + 1;
                        end if;

                        return;
                     end if;
                     --  TBD: Keep statistics on # of mismatches in hash
                     --  table
                     Prev := Ptr;
                     Ptr := Large_Obj_Next_Block (Ptr);
                     exit when Ptr = Null_Virtual_Address;
                  end loop;
               end;
            end if;
         end;
      end if;

      --  No reclaimable block of appropriate size
      declare
         Result : constant Word_Type := Basic_Allocate_From_Stg_Rgn
                             (Stg_Rgn, Size_In_Words, Server_Index);
      begin

         --  Initialize new object with region and size and null type
         --  (this makes it look like a large "null" object),
         --  presuming it is large enough.

         if Size_In_Words >= Large_Obj_Header_Size then
            Set_Large_Obj_Header
              (Result,
               Size => Size_In_Words,
               Stg_Rgn_Id => Stg_Rgn.Index,
               Type_Id => 0,
               Lock_Obj => 0);
         end if;

         if Debug_Stg_Rgns then
            Put_Line
              (" Allocated a new object of size" &
               Offset_Within_Area'Image (Size_In_Words) &
               " in region #" &
               Stg_Rgn_Index'Image (Stg_Rgn.Index) &
               " at " &
               Hex_Image (Result));

            --  Initialize rest of object to obviously bad-news values
            for I in Large_Obj_Header_Size .. Size_In_Words - 1 loop
               Store_Word (Result + I, 16#1badbeefdeadbeef#);
            end loop;
         end if;
         Obj_Addr := Result;

         if Debug_Statistics then
            --  Statistics; Bump counter for not reusing block
            Stg_Rgn_Stats (By_Owner, not Reusing) :=
              Stg_Rgn_Stats (By_Owner, not Reusing) + 1;
         end if;
      end;
   end Allocate_From_Unshared_Stg_Rgn;

   ----------------------------
   -- Allocate_Stg_Rgn_Chunk --
   ----------------------------

   function Allocate_Stg_Rgn_Chunk
     (Min_Size          : Offset_Within_Area;
      Server_Index      : Thread_Server_Index;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr := null)
      return Stg_Rgn_Chunk_Ptr
   is
      Result   : Stg_Rgn_Chunk_Ptr;
      Encloser : Stg_Rgn_Ptr := Enclosing_Stg_Rgn;
      Depth    : Natural := 0;
   begin
      if Encloser = null then
         --  Immediately go to server's list of chunks
         null;
      elsif Encloser.Manager = null then
         --  Owner is doing the allocation, use unshared region-part chain.
         while Encloser /= null loop
            --  Call operation to borrow chunk from
            --  given region.
            Borrow_Unshared_Stg_Rgn_Chunk
              (Encloser, Min_Size, Result);

            --  Exit loop if found a suitable chunk.
            exit when Result /= null;

            --  Keep looking up the chain
            Encloser := Encloser.Enclosing_Stg_Rgn;

            --  Keep track of extra depth
            Depth    := Depth + 1;
         end loop;
      else
         --  We are on the shared/"protected" storage region chain.
         while Encloser /= null loop
            --  Call protected operation to borrow chunk from
            --  given region.
            Encloser.Manager.Borrow_Stg_Rgn_Chunk
              (Encloser, Min_Size, Result);

            --  Exit loop if found a suitable chunk.
            exit when Result /= null;

            --  Keep looking up the chain
            Encloser := Encloser.Enclosing_Stg_Rgn;

            --  Keep track of extra depth
            Depth    := Depth + 1;
         end loop;
      end if;

      if Encloser = null then
         --  We hit the top.
         --  Check the server's free-chunk list
         declare
            Prev : Stg_Rgn_Chunk_Ptr := null;
            Info : Server_Info renames Server_Info_Array (Server_Index);
         begin
            if Debug_Stg_Rgns then
               Put_Line
                 (" Server" & Thread_Server_Index'Image (Server_Index) &
                  " reached null enclosing region, looking for free chunk");
            end if;

            --  See if there is a big enough chunk on the free list
            Result := Info.Free_Rgn_Chunks;
            while Result /= null loop
               if Result.Chunk_Length >= Min_Size + 2 then
                  --  OK, we have one
                  if Prev /= null then
                     --  Need to carve it out from the middle
                     Prev.Next_Chunk := Result.Next_Chunk;
                  else
                     --  It was the first one
                     Info.Free_Rgn_Chunks := Result.Next_Chunk;
                  end if;
                  if Debug_Stg_Rgns then
                     Put_Line
                       (" Server" & Thread_Server_Index'Image (Server_Index) &
                        " found chunk on free list = " &
                        Chunk_Index'Image (Result.Index));
                  end if;
                  exit;
               end if;
               Prev := Result;
               Result := Result.Next_Chunk;
            end loop;

            if Result = null then
               --  Need to allocate a new chunk
               declare
                  Index_Of_New_Chunk : Chunk_Index;
               begin
                  Result :=
                    new Stg_Rgn_Chunk
                    (Chunk_Length => Offset_Within_Chunk'Max
                                       (Min_Chunk_Size,
                                        Min_Size + 2));

                  Stg_Rgn_Manager.Get_Stg_Rgn_Chunk_Index
                    (Index_Of_New_Chunk);

                  --  Install chunk in table
                  Install_Chunk (Result, Index_Of_New_Chunk);

                  if Debug_Stg_Rgns then
                     Put_Line
                       (" Server" & Thread_Server_Index'Image (Server_Index) &
                        " allocating a new chunk = " &
                        Chunk_Index'Image (Result.Index));
                  end if;
               end;
            end if;

            --  Initialize the unused chunk
            Result.Last_In_Use := Result.Data'First - 1;
            Result.Mark := Result.Last_In_Use;
            Result.Depth_Of_Mark := 0;

            --  Compute Space_Left in chunk
            Result.Space_Left := Result.Chunk_Length - Result.Last_In_Use;
         end;
      end if;

      if Result.Last_In_Use = Result.Mark then
         --  Result chunk hadn't been used, just bump up the mark depth
         Result.Depth_Of_Mark := Result.Depth_Of_Mark + 1;
      else
         --  Save old Mark/Depth-of-mark and set them for new region
         Result.Last_In_Use := Result.Last_In_Use + 1;
         Store_Word
           ((Result, Result.Last_In_Use),
            Word_Type (Result.Depth_Of_Mark));
         Result.Last_In_Use := Result.Last_In_Use + 1;
         Store_Word
           ((Result, Result.Last_In_Use),
            Word_Type (Result.Mark));
         Result.Mark := Result.Last_In_Use;
         Result.Depth_Of_Mark := 0;

         --  Compute Space_Left in chunk
         Result.Space_Left := Result.Chunk_Length - Result.Last_In_Use;
      end if;

      --  Adjust the depth, adding one for each enclosing region we skipped.
      --  NOTE: This is necessary because we later release the chunk
      --        to the immediately enclosing region, even if we borrowed
      --        it from further up on the region chain, and as it gets
      --        released progressively up the chain, we decrement Depth_Of_Mark
      --        at each level.
      Result.Depth_Of_Mark := Result.Depth_Of_Mark + Depth;

      pragma Assert (Result.Last_In_Use > 0 or else Result.Depth_Of_Mark > 0);

      return Result;
   end Allocate_Stg_Rgn_Chunk;

   ---------------------
   -- Area_Base_Image --
   ---------------------

   function Area_Base_Image (Area : Area_Base_Indicator) return String is
   begin
      return Debug.Area_Base_Image (Area);
   end Area_Base_Image;

   ---------------
   -- Arg_Array --
   ---------------

   function Arg_Array (Context : Exec_Context_RW_Ptr; Argc : Integer;
     Argv : Argv_Array_Type) return Word_Type is
   --  Create a Basic_Array<Univ_String> object from the arguments
   --  passed to the main routine of a compiled ParaSail program.
   begin
      --  TBD: We need a type desc for Basic_Array<Univ_String>.
      --       For now we just return a large null.
      return Null_For_Stg_Rgn (Context.Local_Stg_Rgn);
   end Arg_Array;

   -------------------
   -- Base_Register --
   -------------------

   function Base_Register
     (Local_Area_Offset : Offset_Within_Area) return Area_Base_Indicator is
      pragma Assert (Local_Area_Offset <= Max_Offset_For_Base_Register);
   begin
      return Base_Registers'First + Area_Base_Indicator (Local_Area_Offset);
   end Base_Register;

   ---------------------------------
   -- Basic_Allocate_From_Stg_Rgn --
   ---------------------------------

   function Basic_Allocate_From_Stg_Rgn
     (Stg_Rgn       : Stg_Rgn_Ptr;
      Size_In_Words : Offset_Within_Area;
      Server_Index  : Thread_Server_Index) return Word_Type is
   --  Allocate object of given size within given region.
   --  Space is uninitialized.
   --  Caller gets a lock if needed.
      Old_First : constant Stg_Rgn_Chunk_Ptr := Stg_Rgn.First_Chunk;
      Chunk     : Stg_Rgn_Chunk_Ptr := Old_First;
   begin
      if Old_First /= null then
         loop
            if Chunk.Space_Left >= Size_In_Words then
               --  Found a chunk with space
               --  Rotate list so this is now the first chunk
               Stg_Rgn.First_Chunk := Chunk;
               if Debug_Stg_Rgns then
                  Put_Line
                    (" Found chunk with space for " &
                     Offset_Within_Area'Image (Size_In_Words) &
                     " in region #" &
                     Stg_Rgn_Index'Image (Stg_Rgn.Index) &
                     " at " &
                     Chunk_Index'Image (Chunk.Index));
               end if;

               exit;   ---------- found chunk with space -----
            end if;

            Chunk := Chunk.Next_Chunk;
            if Chunk = Old_First then
               --  Circled back to front of list
               Chunk := null;
               exit;
            end if;
         end loop;
      end if;

      if Chunk = null then
         --  All chunks are full.
         --  Borrow a chunk from enclosing region.
         --  NOTE: we might get yet another lock to borrow
         --  from the enclosing region.
         Chunk := Allocate_Stg_Rgn_Chunk
           (Size_In_Words,
            Server_Index,
            Stg_Rgn.Enclosing_Stg_Rgn);

         --  Hook chunk into circular list of chunks for this region
         if Old_First = null then
            --  This is the first and only chunk
            Chunk.Prev_Chunk := Chunk;
            Chunk.Next_Chunk := Chunk;
         else
            --  Hook into circularly-linked list
            Chunk.Prev_Chunk := Old_First.Prev_Chunk;
            Chunk.Next_Chunk := Old_First;
            Old_First.Prev_Chunk.Next_Chunk := Chunk;
            Old_First.Prev_Chunk := Chunk;
         end if;
         --  Borrowed or allocated chunk becomes first chunk
         Stg_Rgn.First_Chunk := Chunk;

         Chunk.Associated_Stg_Rgn := Stg_Rgn;
      end if;

      --  Now allocate space for object out of chunk
      declare
         Result : constant Object_Virtual_Address :=
           To_Virtual_Address ((Chunk, Chunk.Last_In_Use + 1));
      begin
         Chunk.Last_In_Use := Chunk.Last_In_Use + Size_In_Words;
         pragma Assert (Chunk.Last_In_Use > 0
           or else Chunk.Depth_Of_Mark > 0);

         --  Compute Space_Left in chunk
         Chunk.Space_Left := Chunk.Chunk_Length - Chunk.Last_In_Use;

         return Result;
      end;

   end Basic_Allocate_From_Stg_Rgn;

   -----------------------------------
   -- Borrow_Unshared_Stg_Rgn_Chunk --
   -----------------------------------

   procedure Borrow_Unshared_Stg_Rgn_Chunk
     (Enclosing_Stg_Rgn : Stg_Rgn_Ptr;
      Min_Size : Offset_Within_Area;
      Borrowed_Chunk : out Stg_Rgn_Chunk_Ptr) is
      --  Borrow a chunk from enclosing region that has at least
      --  the given amount of space.
      --  Borrowed_Chunk is null if region has no appropriate chunk.
      --  Caller must get a lock, if needed.
      pragma Assert (Enclosing_Stg_Rgn /= null);
      Space_Needed : constant Offset_Within_Chunk := Min_Size + 2;
      --  Leave room for a mark in chunk
      Old_First : constant Stg_Rgn_Chunk_Ptr := Enclosing_Stg_Rgn.First_Chunk;
      Result : Stg_Rgn_Chunk_Ptr := Old_First;
   begin
      --  Try to find a chunk in enclosing region
      if Old_First /= null then
         loop
            exit when Space_Needed <= Result.Space_Left;
            --  Chunk has room for this object and room to mark the
            --  current amount in use.

            Result := Result.Next_Chunk;
            if Result = Old_First then
               Result := null;
               exit;
            end if;
         end loop;
      end if;

      if Result /= null then
         --  Found a suitable chunk; need to carve chunk out of list
         if Result.Next_Chunk = Result then
            --  Result is the one and only chunk
            Enclosing_Stg_Rgn.First_Chunk := null;
         else
            --  There is at least one chunk left, carve Result out of the list.
            Result.Prev_Chunk.Next_Chunk := Result.Next_Chunk;
            Result.Next_Chunk.Prev_Chunk := Result.Prev_Chunk;
            if Result = Old_First then
               --  Result was the first chunk; set the new first chunk
               Enclosing_Stg_Rgn.First_Chunk := Result.Next_Chunk;
            end if;
         end if;

         if Debug_Stg_Rgns then
            Put_Line
              (" Found chunk with enough room in region #" &
               Stg_Rgn_Index'Image (Enclosing_Stg_Rgn.Index) &
               " = " &
               Chunk_Index'Image (Result.Index));
         end if;

      end if;

      Borrowed_Chunk := Result;
   end Borrow_Unshared_Stg_Rgn_Chunk;

   -----------------------
   -- Bump_Server_Count --
   -----------------------

   procedure Bump_Server_Count (Amount : Integer) is
   --  Bump the maximum number of servers to be used by the interpreter by
   --  the given Amount.
      New_Max : Natural := Integer'Max (0,
        Max_Dynamically_Allocated_Thread_Servers + Amount);
   begin
      Thread_Manager.Set_Max_Dynamically_Allocated_Servers (New_Max);
   end Bump_Server_Count;

   -------------------------
   -- Call_Target_Routine --
   -------------------------

   procedure Call_Target_Routine
     (Cur_Context    : in out Exec_Context;
      New_Local_Area : Word_Ptr;
      Target_Routine : Routine_Ptr;
      Params         : Word_Ptr;
      Static_Link    : Word_Ptr);
      --  Call routine given its Routine_Ptr, Params, and Static link

   procedure Call_Target_Routine
     (Cur_Context    : in out Exec_Context;
      New_Local_Area : Word_Ptr;
      Target_Routine : Routine_Ptr;
      Params         : Word_Ptr;
      Static_Link    : Word_Ptr) is
      --  Call routine given its Routine_Ptr, Params, and Static link

      New_Context : Exec_Context :=
        (Local_Null          => Null_Virtual_Address,  -- init'ed below
         Enclosing_Type      => Get_Enclosing_Type (Static_Link),
         Local_Stg_Rgn       => null,  --  Initialized below
         Control_Area        => Cur_Context.Control_Area,
         Open_Master         => null,
         Server_Index        => Cur_Context.Server_Index,
         Params              => Params,
         Local_Area          => New_Local_Area,
         Local_Area_Length   => Target_Routine.Local_Area_Length,
         Start_Callee_Locals => Target_Routine.Start_Callee_Locals);

      Thread_Was_Queued : Boolean := False;

   begin  --  Call_Target_Routine

      if not Target_Routine.Is_PSVM_Routine then
         --  Reuse prior stg rgn
         New_Context.Local_Stg_Rgn := Cur_Context.Local_Stg_Rgn;
      else
         --  Get a new local region
         New_Context.Local_Stg_Rgn :=
            Get_New_Local_Stg_Rgn
              (New_Local_Area => New_Local_Area,
               Server_Index => New_Context.Server_Index);

         --  Initialize static link
         Init_Static_Link
           (New_Local_Area, Static_Link);
      end if;

      --  Initialize local null based on local stg rgn
      New_Context.Local_Null := New_Context.Local_Stg_Rgn.Null_Value;

      Execute
        (Instructions => Target_Routine,
         Start_Pc => Code_Index'First,
         Context => New_Context,
         Thread_Was_Queued => Thread_Was_Queued,
         Server_Index => New_Context.Server_Index);

      pragma Assert (not Thread_Was_Queued);
   end Call_Target_Routine;

   ---------------------------------
   -- Call_Through_Operation_Desc --
   ---------------------------------

   procedure Call_Through_Operation_Desc
     (Operation_Desc : Word_Type;
      Inputs         : Word_Array;
      Outputs        : in out Word_Array)
   is
      Op_Desc : constant Object_Virtual_Address :=
         Object_Virtual_Address (Operation_Desc);
      Op_Desc_Phys : constant Word_Ptr :=
         Virtual_To_Physical_Address (Op_Desc);

      --  Fetch contents of Operation_Descriptor Object
      Routine_Addr : constant Word_Ptr := Word_To_Word_Ptr
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size));

      Index : constant Routine_Index := Routine_Index
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 1));

      Static_Link : constant Word_Ptr := Word_To_Word_Ptr
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 2));

      Conv_Desc : constant Convention_Descriptor := Convention_Descriptor
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 3));

      Target_Routine : Routine_Ptr;
      Code : aliased Routine (Is_PSVM_Routine => False);

   begin  --  Call_Through_Operation_Desc

      if Routine_Addr = null then
         Target_Routine := Nth_Routine (Index);

         if Debug_Calls then
            Put_Line (" Calling through op desc with Routine = " &
              Strings.To_String (Target_Routine.Name) & " (#" &
              Routine_Index'Image (Index) &
              "), static_link = " &
              Hex_Image (Static_Link));
         end if;

      else
         Code.Routine_Addr := Word_Ptr_To_Routine_Code_Address
            (Routine_Addr);
         Code.Is_Compiled_Routine := True;
         Code.Conv_Desc := Conv_Desc;
         Code.Internal_Precond_Addr := null;
         --  Create a Routine on the fly with this address
         --  instead of using the index
         Target_Routine := Code'Unchecked_Access;

         if Debug_Calls then
            Put_Line (" Calling through op desc with Routine at addr " &
              Hex_Image (Routine_Addr) & ", static_link = " &
              Hex_Image (Static_Link));
         end if;

      end if;

      declare

         Server_Index : constant Thread_Server_Index := Current_Server_Index;

         Cur_State_Context : constant Exec_Context_RW_Ptr :=
           Current_Server_Context (Server_Index);

         Enclosing_Local_Area : constant Word_Ptr :=
                                  Cur_State_Context.Local_Area;
         Params_Address       : constant Word_Ptr :=
                                  Add (Enclosing_Local_Area,
                                       Cur_State_Context.Start_Callee_Locals +
                                       Local_Area_Local_Data_Offset);
         --  TBD: This presumes that caller local area
         --       includes room for longest callee's local area.

         New_Local_Area : constant Word_Ptr :=
            Add (Params_Address, Outputs'Length + Inputs'Length);

      begin

         --  Copy inputs and outputs into param area
         for I in Outputs'Range loop
            Store_Word (Params_Address, I - Outputs'First, Outputs (I));
         end loop;
         for I in Inputs'Range loop
            Store_Word (Params_Address,
              I - Inputs'First + Outputs'Length, Inputs (I));
         end loop;
         --  TBD: Share more code with Execute_Call

         Call_Target_Routine (Cur_State_Context.all, New_Local_Area,
           Target_Routine, Params_Address, Static_Link);

         --  Copy outputs back
         for I in Outputs'Range loop
            Outputs (I) := Fetch_Word (Params_Address, I - Outputs'First);
         end loop;
      end;
   end Call_Through_Operation_Desc;

   ------------------------------------------
   -- Call_Through_Operation_Desc_Exported --
   ------------------------------------------

   procedure Call_Through_Operation_Desc_Exported
     (Context        : in out Exec_Context;
      Operation_Desc : Word_Type;
      Params         : Word_Ptr) is
   --  Call a ParaSail routine given the execution context,
   --  an operation descriptor for the routine,
   --  and a pointer to the list of parameters.
      Op_Desc : constant Object_Virtual_Address :=
         Object_Virtual_Address (Operation_Desc);
      Op_Desc_Phys : constant Word_Ptr :=
         Virtual_To_Physical_Address (Op_Desc);

      --  Fetch contents of Operation_Descriptor Object
      Routine_Addr : constant Routine_Code_Address :=
        Word_Ptr_To_Routine_Code_Address (Word_To_Word_Ptr
          (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size)));

      Static_Link : constant Word_Ptr := Word_To_Word_Ptr
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 2));

      Conv_Desc : constant Convention_Descriptor := Convention_Descriptor
         (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 3));
   begin
      if Routine_Addr /= null then
         --  Make the indirect call to a compiled routine
         --  TBD: We should disallow passing a locking operation
         Call_Compiled_Routine
           (Context, Params, To_Non_Op_Map_Type_Desc (Static_Link),
            Routine_Addr, Conv_Desc => Conv_Desc);
      else
         --  Make the indirect call to a PSVM routine
         declare
            Index : constant Routine_Index := Routine_Index
               (Fetch_Word (Op_Desc_Phys, Large_Obj_Header_Size + 1));

            Target_Routine : constant Routine_Ptr := Nth_Routine (Index);
            New_Local_Area : constant Word_Ptr :=
                               Add (Context.Local_Area,
                                    Context.Start_Callee_Locals +
                                    Local_Area_Local_Data_Offset +
                                    Target_Routine.Parameters'Length);
         begin
            Call_Target_Routine (Context, New_Local_Area,
              Target_Routine, Params, Static_Link);
         end;
      end if;
   end Call_Through_Operation_Desc_Exported;

   ----------------
   -- Code_Block --
   ----------------

   function Code_Block
     (Code  : Code_Ptr;
      Index : Code_Index) return Code_Block_RW_Ptr
   is
      function To_Code_Block_RW_Ptr is
         new Ada.Unchecked_Conversion (System.Address, Code_Block_RW_Ptr);
      --  NOTE: We can't just use 'Access because
      --       we run afoul of 3.10.2(26) disallowing 'Access
      --       on a discriminant-dependent component.
   begin
      case Code.Instrs (Index).Op is
         when Call_Nested_Block_Op | Check_Nested_Block_Op =>
            return To_Code_Block_RW_Ptr
                     (Code.Instrs (Index).Code_Block'Address);

         when Start_Parallel_Op |
              Start_Handled_Op  |
              Add_Parallel_Op   =>
            return To_Code_Block_RW_Ptr
                     (Code.Instrs (Index).Parallel_Code_Block'Address);

         when Begin_Nested_Block_Op =>
            return To_Code_Block_RW_Ptr
                     (Code.Instrs (Index).Nested_Code_Block'Address);

         when others =>
            raise Program_Error;
      end case;
   end Code_Block;

   -------------------------------------------------
   -- Compute_Routine_Name_With_Overloading_Index --
   -------------------------------------------------

   procedure Compute_Routine_Name_With_Overloading_Index
     (Routine : Routine_RW_Ptr) is
   --  Computes Routine.Name_With_Overloading_Index
   --  by calling Routine_Name_With_Overloading_Index if not already filled in.
      use type Strings.U_String;
   begin
      if Routine.Name_With_Overloading_Index = Strings.Null_U_String then
         --  Hasn't been computed before, so compute it now.
         Routine.Name_With_Overloading_Index :=
           Routine_Name_With_Overloading_Index (Routine_Ptr (Routine));
      end if;
   end Compute_Routine_Name_With_Overloading_Index;

   --------------------------------
   -- Content_Of_Virtual_Address --
   --------------------------------

   function Content_Of_Virtual_Address
     (Virtual : Object_Virtual_Address) return Word_Type is
   begin
      if Virtual >= 0 or else Virtual mod 2 = 0 then
         return Content_Of_Physical_Address
                  (Virtual_To_Physical_Address (Virtual));
      else
         --  Type relative info
         return Nth_Type_Area_Word
                  (Nth_Element
                      (Type_Table,
                       Type_Elem_Index ((-Virtual) / Chunk_Divisor)),
                   Offset_Within_Area ((-Virtual) rem Chunk_Divisor / 2));
      end if;
   end Content_Of_Virtual_Address;

   --------------------
   -- Copy_Large_Obj --
   --------------------

   function Copy_Large_Obj
     (Type_Desc     : Non_Op_Map_Type_Ptr;
      Value_To_Copy : Word_Type;
      Stg_Rgn       : Stg_Rgn_Ptr;
      Server_Index  : Thread_Server_Index;
      Old_Stg_Rgn   : Stg_Rgn_Ptr := null) return Word_Type
   is
      pragma Assert (not Is_Small (Type_Desc));
   begin
      Check_Is_Large (Value_To_Copy);

      if Debug_Stg_Rgns then
         if Old_Stg_Rgn /= null then
            --  Check that region is as expected
            pragma Assert (Old_Stg_Rgn = Stg_Rgn_Of_Large_Obj (Value_To_Copy));
            null;
         end if;
      end if;

      if Is_Special_Large_Value (Value_To_Copy) then
         return Replace_Special_Value_Stg_Rgn (Value_To_Copy, Stg_Rgn);
      else
         --  Copy large object (use info from object being copied)

         declare
            Old_Obj_Size : constant Offset_Within_Area :=
              Large_Obj_Size (Value_To_Copy);
            pragma Assert
              (Large_Obj_Lock_Obj (Value_To_Copy) in 0 .. Lock_Obj_Limit);
            New_Obj : constant Word_Type :=
              Allocate_From_Stg_Rgn (Stg_Rgn, Old_Obj_Size, Server_Index);
            Old_Type_Info : constant Type_Index :=
              Large_Obj_Type_Info (Value_To_Copy);
            Old_Type_Desc : constant Non_Op_Map_Type_Ptr :=
              To_Type_Desc (Old_Type_Info);
            Old_Stg_Rgn_To_Pass : Stg_Rgn_Ptr := Old_Stg_Rgn;
         --  For Debugging
         begin
            pragma Assert
              (Type_Desc.Is_Wrapper
              or else Old_Type_Desc.Num_Components =
                      Type_Desc.Num_Components);

            --  Unless Type_Desc is for a wrapper type,
            --  Old_Type_Desc and Type_Desc should have
            --  same number of components

            Check_Is_Large (New_Obj);

            if Debug_Stg_Rgns then
               Put_Line
                 (" Copying large obj " &
                  Hex_Image (Value_To_Copy) &
                  " of size" &
                  Offset_Within_Area'Image (Old_Obj_Size) &
                  ", type-index" &
                  Type_Index'Image (Old_Type_Info) &
                  ", and region #" &
                  Stg_Rgn_Index'Image
                    (Large_Obj_Stg_Rgn_Index (Value_To_Copy)) &
                  " into region #" &
                  Stg_Rgn_Index'Image (Stg_Rgn.Index) &
                  " at " &
                  Hex_Image (New_Obj));

               if Old_Stg_Rgn_To_Pass = null
                 and then not Old_Type_Desc.Is_Polymorphic
               then
                  --  Set Old_Stg_Rgn_To_Pass if not yet set.
                  --  However, if this is a "top-level" polymorphic
                  --  object, don't require the regions to match.
                  Old_Stg_Rgn_To_Pass := Stg_Rgn_Of_Large_Obj (Value_To_Copy);
               end if;
            end if;

            --  Copy the type info
            Set_Large_Obj_Type_Info (New_Obj, Old_Type_Info);

            --  Initialize to be unlocked
            Set_Large_Obj_Lock_Obj (New_Obj, 0);

            --  Now copy the components

            if Old_Type_Desc.Type_Kind = Basic_Array_Kind then

               --  Copy array components

               declare
                  Arr_Comp_Type : constant Type_Descriptor_Ptr :=
                    Basic_Array_Comp_Type (Old_Type_Desc);

                  Len : constant Word_Type :=
                          Content_Of_Virtual_Address
                             (Value_To_Copy + Large_Obj_Header_Size);
                  --  TBD: This "len" is a bit of a waste as it can
                  --       be computed from the size by subtracting header
                  --       size.
               begin
                  Store_Word (New_Obj + Large_Obj_Header_Size, Len);

                  for I in 1 .. Offset_Within_Area (Len) loop
                     declare
                        Offset : constant Offset_Within_Area :=
                          Large_Obj_Header_Size + I;
                        Old_Comp : Word_Type :=
                          Content_Of_Virtual_Address (Value_To_Copy + Offset);
                     begin
                        --  Copy each component
                        if not Is_Small (Arr_Comp_Type) then
                           --  Recurse to copy large component
                           Old_Comp :=
                              Copy_Large_Obj
                                (Arr_Comp_Type,
                                 Old_Comp,
                                 Stg_Rgn,
                                 Server_Index,
                                 Old_Stg_Rgn => Old_Stg_Rgn_To_Pass);
                        end if;

                        Store_Word (New_Obj + Offset, Old_Comp);
                     end;
                  end loop;
               end;

            else
               --  Copy normal components

               for I in 1 .. Old_Type_Desc.Num_Components loop
                  declare
                     Comp_Type : constant Non_Op_Map_Type_Ptr :=
                       Skip_Over_Op_Map
                          (Old_Type_Desc.Components (I).Type_Desc);
                     Offset : constant Offset_Within_Area :=
                       Large_Obj_Header_Size + Offset_Within_Area (I - 1);
                     Old_Comp : Word_Type :=
                       Content_Of_Virtual_Address (Value_To_Copy + Offset);
                  begin
                     --  Copy each component
                     if not Old_Type_Desc.Components (I).Is_By_Ref
                       and then not Is_Small (Comp_Type)
                     then
                        --  Recurse to copy large component
                        Old_Comp :=
                           Copy_Large_Obj
                             (Comp_Type,
                              Old_Comp,
                              Stg_Rgn,
                              Server_Index,
                              Old_Stg_Rgn => Old_Stg_Rgn_To_Pass);
                     end if;

                     Store_Word (New_Obj + Offset, Old_Comp);
                  end;
               end loop;

               if Old_Type_Desc.Type_Kind = Aliased_Object_Kind then
                  --  Null out self-access value when copied
                  Store_Word (New_Obj + Aliased_Obj_Self_Offset, 0);
               end if;
            end if;

            pragma Assert (Large_Obj_Lock_Obj (New_Obj) = 0);
            return New_Obj;
         end;
      end if;
   end Copy_Large_Obj;

   -----------------
   -- Copy_Object --
   -----------------

   function Copy_Object
     (Context    : in out Exec_Context;
      Type_Desc  : Type_Descriptor_Ptr;
      Object     : Word_Type;
      Stg_Rgn_Of : Word_Type) return Word_Type is
   --  Copy object, given type descriptor and target object.
   --  Return result of copying.
   begin
      if Is_Small (Type_Desc) then
         --  Copying is trivial
         return Object;
      else
         --  Pass the buck to the large copy routine
         return Copy_Large_Obj
           (Type_Desc     => Skip_Over_Op_Map (Type_Desc),
            Value_To_Copy => Object,
            Stg_Rgn       => Stg_Rgn_Of_Large_Obj (Stg_Rgn_Of),
            Server_Index  => Context.Server_Index);
      end if;
   end Copy_Object;

   ---------------------------
   -- Basic_Array_Comp_Type --
   ---------------------------

   function Basic_Array_Comp_Type (Basic_Array_Type : Non_Op_Map_Type_Ptr)
     return Non_Op_Map_Type_Ptr is
   --  Given a type identified as a Basic_Array kind,
   --  retrieve the component type.
   begin
      if Basic_Array_Type.Num_Parameters >= 1 then
         --  ParaSail-style Basic_Array
         return Basic_Array_Type.Parameters (1).Data.Type_Desc;
      elsif Basic_Array_Type.Enclosing_Type /= null
        and then Basic_Array_Type.Enclosing_Type.Num_Parameters >= 1
      then
         --  Ada/SPARK-style Basic_Array
         return
           Basic_Array_Type.Enclosing_Type.Parameters (1).Data.Type_Desc;
      else
         --  Unrecognized style.
         raise Malformed_Basic_Array_Error;
      end if;
   end Basic_Array_Comp_Type;

   ----------------------------
   -- Create_Basic_Array_Obj --
   ----------------------------

   function Create_Basic_Array_Obj
     (Array_Type_Desc : Non_Op_Map_Type_Ptr;
      Array_Len       : Natural;
      Stg_Rgn         : Stg_Rgn_Ptr;
      Server_Index    : Thread_Server_Index;
      Init_Elements   : Boolean := False;
      Element_Value   : Word_Type := Null_Value) return Word_Type
   --  Create a "Basic_Array" object, given type descriptor, length,
   --  server, and, optionally, initial value for elements.
   is
      pragma Assert (Array_Type_Desc.Type_Kind = Basic_Array_Kind);
      pragma Assert (Array_Type_Desc.Num_Components = 0);
      pragma Assert (not Array_Type_Desc.Is_Wrapper);
      New_Obj_Size : constant Offset_Within_Area := Large_Obj_Header_Size +
                       Offset_Within_Area (Array_Len) + 1;
      New_Obj      : constant Word_Type :=
                       Allocate_From_Stg_Rgn
                         (Stg_Rgn, New_Obj_Size, Server_Index);
      New_Obj_Ptr  : constant Word_Ptr :=
                       Virtual_To_Physical_Address (New_Obj);
      --  NOTE: Calling the 2-parameter Store_Word produced bad code
      --        when optimizing, resulting in negative values for the
      --        offset when it exceeded 32767.

      Element_Type : constant Type_Descriptor_Ptr :=
        Basic_Array_Comp_Type (Array_Type_Desc);
   begin
      --  Fill in header
      Set_Large_Obj_Header
        (New_Obj,
         New_Obj_Size,
         Stg_Rgn.Index,
         Array_Type_Desc.Index);

      --  Store length
      Store_Word (New_Obj_Ptr, Large_Obj_Header_Size, Word_Type (Array_Len));

      if Init_Elements then
         if Is_Small (Element_Type) then
            --  Don't need to copy Element_Value
            for I in 1 .. Offset_Within_Area (Array_Len) loop
               Store_Word
                 (New_Obj_Ptr, Large_Obj_Header_Size + I, Element_Value);
            end loop;
         else
            --  Copy Element_Value repeatedly into target region
            --  NOTE: We use Copy_Large_Obj even if Element_Value is null
            --       to be sure it ends up with a null for correct region.
            for I in 1 .. Offset_Within_Area (Array_Len) loop
               declare
                  Copy_Of_Val : constant Object_Virtual_Address :=
                    Copy_Large_Obj (Element_Type, Element_Value, Stg_Rgn,
                      Server_Index);
               begin
                  Store_Word
                    (New_Obj_Ptr, Large_Obj_Header_Size + I, Copy_Of_Val);
                  if Element_Type.Is_Concurrent then
                     Create_Lock_For_Obj (Copy_Of_Val, Server_Index);
                  end if;
               end;
            end loop;
         end if;
      else
         --  Initialize components to null
         declare
            Null_Comp_Val : constant Word_Type :=
               Null_For_Type_Or_Stg_Rgn
                 (Element_Type, Stg_Rgn, Is_By_Ref => False);
         begin
            for I in 1 .. Offset_Within_Area (Array_Len) loop
               --  Assign an appropriate null into component
               Store_Word
                 (New_Obj_Ptr, Large_Obj_Header_Size + I, Null_Comp_Val);
            end loop;
         end;
      end if;
      Check_Is_Large (New_Obj);
      pragma Assert (Large_Obj_Lock_Obj (New_Obj) = 0);

      return New_Obj;
   end Create_Basic_Array_Obj;

   ----------------------
   -- Create_Large_Obj --
   ----------------------

   function Create_Large_Obj
     (Type_Desc    : Non_Op_Map_Type_Ptr;
      Stg_Rgn      : Stg_Rgn_Ptr;
      Server_Index : Thread_Server_Index) return Word_Type
   --  Create large (non-array) object in given region.
   --  Initialize all subobjects to appropriate kind of null
   is
      pragma Assert (not Is_Small (Type_Desc));
      pragma Assert (Type_Desc.Type_Kind /= Basic_Array_Kind);

      New_Obj_Size : constant Offset_Within_Area :=
                       Offset_Within_Area (Type_Desc.Num_Components) +
                       Large_Obj_Header_Size;
      New_Obj      : constant Word_Type :=
                       Allocate_From_Stg_Rgn
                         (Stg_Rgn, New_Obj_Size, Server_Index);
   begin
      --  Wrapper types should never show up on an actual object
      pragma Assert (not Type_Desc.Is_Wrapper);

      --  Fill in header
      Set_Large_Obj_Header
        (New_Obj,
         New_Obj_Size,
         Stg_Rgn.Index,
         Type_Desc.Index);

      --  Initialize components to null
      for I in 1 .. Type_Desc.Num_Components loop
         declare
            Comp_Type : constant Type_Descriptor_Ptr :=
              Type_Desc.Components (I).Type_Desc;
         begin
            --  Assign an appropriate null into component
            Store_Word
              (New_Obj + Large_Obj_Header_Size + Offset_Within_Area (I - 1),
               Null_For_Type_Or_Stg_Rgn
                 (Comp_Type,
                  Stg_Rgn,
                  Is_By_Ref => Type_Desc.Components (I).Is_By_Ref));
         end;
      end loop;

      Check_Is_Large (New_Obj);
      pragma Assert (Large_Obj_Lock_Obj (New_Obj) = 0);

      return New_Obj;
   end Create_Large_Obj;

   -------------------------
   -- Create_Lock_For_Obj --
   -------------------------

   procedure Create_Lock_For_Obj (Dest_Obj : Object_Virtual_Address;
     Server_Index : Thread_Server_Index) is
      Cur_Lock : Lock_Obj_Index := Large_Obj_Lock_Obj (Dest_Obj);
      pragma Assert (Cur_Lock in 0 .. Lock_Obj_Limit);
   begin
      if Debug_Threading then
         Put_Line (" Create_Lock_For_Obj Obj = " & Hex_Image (Dest_Obj));
         Put_Line
           ("   current Lock_Obj_Index =" & Lock_Obj_Index'Image (Cur_Lock));
      end if;

      if Cur_Lock /= 0 then
         --  Already has a lock
         return;
      end if;

      Allocate_Lock_Obj (Cur_Lock, Server_Index);
      --  Allocate a new lock obj

      Set_Large_Obj_Lock_Obj (Dest_Obj, Cur_Lock);
   end Create_Lock_For_Obj;

   ----------------------------------
   -- Create_Lock_For_Obj_Exported --
   ----------------------------------

   procedure Create_Lock_For_Obj_Exported (Context : in out Exec_Context;
      Dest_Obj : Object_Virtual_Address) is
   begin
      Create_Lock_For_Obj (Dest_Obj, Context.Server_Index);
   end Create_Lock_For_Obj_Exported;

   --------------------------
   -- Current_Server_Index --
   --------------------------

   function Current_Server_Index return Thread_Server_Index is
   begin
      --  Just pass the buck to the task attribute package
      return Server_Index_Attribute.Value;
   end Current_Server_Index;

   ----------------------------
   -- Current_Server_Context --
   ----------------------------

   function Current_Server_Context
     (Server_Index : Thread_Server_Index := Current_Server_Index)
     return Exec_Context_RW_Ptr is
   begin
      return Server_Info_Array (Server_Index).Current_State.Context;
   end Current_Server_Context;

   -----------------------------
   -- Deallocate_From_Stg_Rgn --
   -----------------------------

   procedure Deallocate_From_Stg_Rgn
     (Stg_Rgn         : Stg_Rgn_Ptr;
      Storage_Address : Object_Virtual_Address;
      Server_Index    : Thread_Server_Index) is
   begin
      if Server_Index = Stg_Rgn.Owning_Server then
         --  Owner is doing the deallocation.
         --  Use the unshared part of the storage region -- no locking needed
         Deallocate_From_Unshared_Stg_Rgn
           (Stg_Rgn, Storage_Address, Server_Index);
      else
         --  Use shared part of the storage region
         Stg_Rgn.Shared_Part.Manager.Deallocate_From_Stg_Rgn
           (Stg_Rgn.Shared_Part, Storage_Address, Server_Index);
      end if;
   end Deallocate_From_Stg_Rgn;

   --------------------------------------
   -- Deallocate_From_Unshared_Stg_Rgn --
   --------------------------------------

   procedure Deallocate_From_Unshared_Stg_Rgn
     (Stg_Rgn         : Stg_Rgn_Ptr;
      Storage_Address : Object_Virtual_Address;
      Server_Index    : Thread_Server_Index)
   is
      --  This adds the given storage to a list indexed by the size.
      --  Requires: Object has size embedded in its header.
      use type Hash_Value;
      Size_In_Words : constant Offset_Within_Area :=
                        Large_Obj_Size (Storage_Address);

      procedure Bump_Proxy_Gen_Num (Acc_Val_Addr : Word_Ptr);
      --  Bump the proxy generation number in the access value at given addr.

      procedure Bump_Proxy_Gen_Num (Acc_Val_Addr : Word_Ptr) is
         Self_Ptr : constant Proxy_Self_Ptr :=
           To_Proxy_Self_Ptr (Acc_Val_Addr);
      begin
         Self_Ptr.Proxy_Gen_Num := Self_Ptr.Proxy_Gen_Num + 1;
      end Bump_Proxy_Gen_Num;

   begin  --  Deallocate_From_Unshared_Stg_Rgn

      if Debug_Stg_Rgns then
         if Never_Deallocate then
            Put (" NOT");
         end if;
         Put_Line
           (" Deallocating an object of size" &
            Offset_Within_Area'Image (Size_In_Words) &
            " at " &
            Hex_Image (Storage_Address));
      end if;

      if Never_Deallocate then
         --  Don't try to reclaim storage
         return;
      end if;

      --  TBD: Check whether obj borders space at end of chunk
      --      and if so just merge with that free space.

      if Size_In_Words = Aliased_Obj_Size_In_Words
        and then Large_Obj_Type_Desc (Storage_Address).Type_Kind =
          Aliased_Object_Kind
      then
         declare
            Self_Acc_Val : constant Word_Type :=
              Fetch_Word (Storage_Address + Aliased_Obj_Self_Offset);
         begin
            if Self_Acc_Val /= 0 and then Self_Acc_Val /= Null_Value then
               --  We have a special free list for aliased objects
               --  that have an assigned access value.
               --  First bump the generation number.
               Bump_Proxy_Gen_Num (Word_To_Word_Ptr
                 (Storage_Address + Aliased_Obj_Self_Offset));

               --  Now add to the free list headed by Free_Proxies.
               Store_Word (Storage_Address + Aliased_Obj_Payload_Offset,
                 Stg_Rgn.Free_Proxies);
               Stg_Rgn.Free_Proxies := Storage_Address;

               return;  --  Don't reclaim this aliased object.
            end if;
         end;
      end if;

      if Size_In_Words >= Min_Reclaimable_Block_Size then
         --  Add to reclaimable table
         if Stg_Rgn.Reclamation = null then
            --  Allocate a reclamation info record
            declare
               subtype Constrained_Reclamation_Info is
                 Reclamation_Info_Record
                   (Modulus_Minus_One => Initial_Modulus - 1);

               Reclam_Size_In_Words : constant Offset_Within_Area :=
                 To_Size_In_Words (Constrained_Reclamation_Info'Size);

               Reclam_Addr : constant Object_Virtual_Address :=
                               Basic_Allocate_From_Stg_Rgn
                                 (Stg_Rgn, Reclam_Size_In_Words, Server_Index);

               --  Default-initialize the allocated space
               Const_Addr : constant Word_Ptr :=
                              Virtual_To_Physical_Address (Reclam_Addr);

               pragma Warnings (Off); --  default init wanted
               Allocated_Info : aliased Constrained_Reclamation_Info;
               for Allocated_Info'Address use Const_Addr.all'Address;
               pragma Warnings (On);

            begin
               Stg_Rgn.Reclamation := Allocated_Info'Unchecked_Access;

               if Debug_Stg_Rgns then
                  Put_Line
                    (" Allocated a reclamation region of size" &
                     Offset_Within_Area'Image (Reclam_Size_In_Words) &
                     " in region #" &
                     Stg_Rgn_Index'Image (Stg_Rgn.Index) &
                     " at " &
                     Hex_Image (Reclam_Addr));
               end if;
            end;
         end if;

         --  Add deallocated area to appropriate reclaimable block list
         declare
            Index : constant Hash_Value :=
                      Hash_Value (Size_In_Words) rem  --  Size > 0 so rem==mod
                        (Stg_Rgn.Reclamation.Modulus_Minus_One + 1);
         begin
            --  Link into reclamation table
            Set_Large_Obj_Size (Storage_Address, Size_In_Words);
            Set_Large_Obj_Next_Block
              (Storage_Address,
               Stg_Rgn.Reclamation.Reclamation_Table (Index));
            Stg_Rgn.Reclamation.Reclamation_Table (Index) :=
              Storage_Address;
         end;

      else
         --  Too small to worry about
         null;
      end if;
   end Deallocate_From_Unshared_Stg_Rgn;

   ---------------------
   -- Delay_Tcb_Until --
   ---------------------

   procedure Delay_Tcb_Until
     (Tcb_Addr    : Word_Ptr;
      Delay_Until : Ada.Calendar.Time)
     renames Delay_Queue_Handling.Delay_Tcb_Until;
   --  Add TCB to the delay queue, to wake up when the wall clock reaches
   --  Delay_Until.

   ---------------------
   -- Direction_Image --
   ---------------------

   function Direction_Image (Dir : Direction) return String is
   begin
      return Debug.Direction_Image (Dir);
   end Direction_Image;

   ---------------
   -- Dump_Code --
   ---------------

   procedure Dump_Code
     (Code : Code_Ptr) is
   --  Display instructions of Code_Type for debugging purposes.
   begin
      Debug.Dump_Code (Code);
   end Dump_Code;

   --------------
   -- Dump_Obj --
   --------------

   procedure Dump_Obj (Value : Word_Type) is
   begin
      Dump_Obj_With_Type (Value, Type_Desc => null);
   end Dump_Obj;

   ------------------------
   -- Dump_Obj_With_Type --
   ------------------------

   procedure Dump_Obj_With_Type
     (Value     : Word_Type;
      Type_Desc : Type_Descriptor_Ptr) is
      use Debug;
   begin
      Dump_Obj_With_Indent (Value, Type_Desc, Indent => 0);
   exception
      when Storage_Error =>
         --  Not much to do here
         raise;

      when E : others =>
         Put_Line (" dump_obj raised " & Ada.Exceptions.Exception_Name (E));
   end Dump_Obj_With_Type;

   --------------------------
   -- Dump_One_Instruction --
   --------------------------

   procedure Dump_One_Instruction (Instr : Instruction) is
   begin
      Debug.Dump_One_Instruction (Instr);
   end Dump_One_Instruction;

   ------------------
   -- Dump_Routine --
   ------------------

   procedure Dump_Routine (Code : Routine_Ptr) is
   begin
      Debug.Dump_Routine (Code);
   end Dump_Routine;

   --------------------
   -- Dump_Type_Desc --
   --------------------

   procedure Dump_Type_Desc (Type_Desc : Type_Descriptor_Ptr) is
   begin
      Debug.Dump_Type_Desc (Type_Desc);
   end Dump_Type_Desc;

   ----------------
   -- Create_Tcb --
   ----------------

   procedure Create_Tcb (
      Context : in out Exec_Context;
      Parallel_Master : Word_Ptr;
      Parallel_Control : Word_Ptr;
      Num_Params : Integer) is
      --  Execute the Create_TCB instruction
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Num_Params : Natural;
      --   NOTE: Parallel_Master is a pseudo-large-object at the given address
      --         Parallel_Control is the address of a word which will be
      --         set to point at the space allocated for the Tcb.
      Tcb_Addr : constant Word_Ptr :=
        New_Tcb (Context, Parallel_Master, Num_Params);
   begin
      --  Store phys addr into TCB pointer identified by Parallel_Control.
      Store_Word_Ptr (Parallel_Control, 0, Tcb_Addr);
   end Create_Tcb;

   -----------------------------------------
   -- Execute_Prepare_To_Exit_Parallel_Op --
   -----------------------------------------

   function Execute_Prepare_To_Exit_Parallel_Op
      (Context        : in out Exec_Context;
       Master_Address : Word_Ptr) return Boolean
   is
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;

      Holding_Lock_Obj : Lock_Obj_Index := 0;
   begin  --  Execute_Prepare_To_Exit_Parallel_Op

      if Context.Control_Area /= null then
         --  Lock object of TCB is the innermost lock held
         Holding_Lock_Obj := Large_Obj_Lock_Obj (Context.Control_Area);
      end if;

      return Prepare_To_Exit
        (Master_Address,
         Context.Server_Index,
         Holding_Lock_Obj,
         Exiting_Tcb => Context.Control_Area);
   end Execute_Prepare_To_Exit_Parallel_Op;

   -------------------------------
   -- Unwrapped_Polymorphic_Obj --
   -------------------------------

   function Unwrapped_Polymorphic_Obj
     (Source_Type_Info : Type_Descriptor_Ptr;
      Dest_Type_Info   : Type_Descriptor_Ptr;
      Poly_Obj         : Word_Type)
     return Word_Type is
      --  Execute the Unwrap_Polymorphic_Obj instruction
      --  by checking if underlying type matches Type_Info,
      --  and return a ref to underlying obj if so, or null ref otherwise.

      Source_Type_Desc : constant Non_Op_Map_Type_Ptr :=
         Skip_Over_Op_Map (Source_Type_Info);
      Dest_Type_Desc : constant Non_Op_Map_Type_Ptr :=
         Skip_Over_Op_Map (Dest_Type_Info);

      Poly_Type_Desc : constant Type_Descriptor_Ptr :=
        Large_Obj_Type_Desc (Poly_Obj);
      pragma Assert (Poly_Type_Desc.Is_Polymorphic);
      pragma Assert (Source_Type_Desc.Is_Polymorphic);
      --  Poly now OK: pragma Assert (not Dest_Type_Desc.Is_Polymorphic);
      --  TBD: Poly_Type ought to match Source_Type in some way

      Underlying_Type : constant Non_Op_Map_Type_Ptr :=
        Skip_Over_Op_Map (Poly_Type_Desc.Components (1).Type_Desc);

      Result : Word_Type := Null_Virtual_Address;

      use Debug;
   begin
      if Debug_Calls then
         Put_Line (" Unwrap polymorphic obj:");
         Dump_Obj_With_Indent (Poly_Obj, Poly_Type_Desc, Indent => 2);
         Put_Line (" To produce:");
      end if;

      if Dest_Type_Desc.Is_Polymorphic then
         --  Check to see if destination is for an ancestor of Underlying_Type
         --  If so, we can use polymorphic object as is.
         --  Otherwise we will create a null ref.
         --  TBD: This doesn't really handle the case of the object
         --       "implementing" the destination type.
         declare
            Dest_Level : constant Natural := Dest_Type_Desc.Num_Nested_Types;
            --  TBD: Currently using Nested_Types for ancestor table
            Obj_Level : constant Natural := Poly_Type_Desc.Num_Nested_Types;
         begin
            if Dest_Level > 0 and then Obj_Level >= Dest_Level
              and then
                Poly_Type_Desc.Nested_Types (Dest_Level) =
                  Dest_Type_Desc.Nested_Types (Dest_Level)
            then
               --  We have a match
               --  Create a (shallow) copy of poly obj (this is *not* a ref)
               Result := Poly_Obj;
               if Debug_Calls then
                  Put_Line ("   the same poly object");
               end if;
            else
               --  Create a "null" obj
               Result := Null_Virtual_Address;
               if Debug_Calls then
                  Put_Line ("   a null (poly) reference.");
               end if;
            end if;
         end;

      elsif Underlying_Type /= Dest_Type_Desc then
         --  We do not have a match for specified type
         --  Create a "null" ref
         Result := Null_Virtual_Address;
         if Debug_Calls then
            Put_Line ("   a null reference.");
         end if;

      else
         --  We have a match for specified type.
         --  Create a "ref" to underlying obj after sharing lock if any

         if not Is_Small (Underlying_Type) then
            Share_Lock (Poly_Obj);
         end if;

         Result := Word_Ptr_To_Word
           (Virtual_To_Physical_Address (Poly_Obj + Large_Obj_Header_Size));


         if Debug_Calls then
            Dump_Obj_With_Indent
              (Fetch_Word (Poly_Obj + Large_Obj_Header_Size),
               Underlying_Type,
               Indent => 2);
         end if;
      end if;

      return Result;
   end Unwrapped_Polymorphic_Obj;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Instructions      : Routine_Ptr;
      Start_Pc          : Code_Index;
      Context           : in out Exec_Context;
      Thread_Was_Queued : out Boolean;
      Debugger_Console  : Boolean := False;
      Server_Index      : Thread_Server_Index := Main_Thread_Server_Index)
   is
      --  This executes the instructions
      --  starting at Start_PC in the given routine,
      --  with the given Exec_Context.
      --  Note that the caller must pre-allocate a Local_Area of size
      --  indicated in the header of the Routine.  Forcing the caller
      --  to pre-allocate this area can potentially minimize
      --  the number of allocations/deallocations required.
      --  Results are returned in the Params area, or in
      --  objects pointed-to from the Params area.
      --  Up-level references to an enclosing block, operation, or type area
      --  are done through static link stored in the pre-allocated local area.
      --  If Thread_Was_Queued is True upon return, then thread did *not*
      --  complete, but instead was queued waiting for a dequeue
      --  condition to be true.
      --  If Debugger_Console is true, then we immediately invoke the
      --  debugger console rather than executing instructions.
      --  Context.Server_Index is set to zero if user requests shutdown.

      Pc : Code_Index := Start_Pc;
      Info : Server_Info renames Server_Info_Array (Server_Index);
      Cur_State : Server_State renames Info.Current_State;
      Last_Src_Pos : Source_Positions.Source_Position
        renames Info.Last_Src_Pos;
      Static_Link : constant Word_Ptr :=
        Fetch_Word_Ptr (Context.Local_Area, Local_Area_Static_Link_Offset);
         --  TBD: Static_Link should be passed as a separate parameter

      use Debug;

      procedure Execute_Call_Nested_Block_Op (Instr : Instruction);
      --  Execute a Call_ or Check_Nested_Block_Op instruction,
      --   both of which represent calls on a nested block.
      --   These are used for non-concurrent loop bodies,
      --   and for assertions.
      --  Fields of Instruction are:
      --   Params : Object_Locator;
      --   Static_Link : Object_Locator;
      --   Code_Block : Code_Block_Descriptor;

      procedure Execute_Call_Op (Instr : Instruction);
      --  Execute the Call_Op instruction
      --  Fields of Instruction are:
      --   Params : Object_Locator;
      --   Static_Link : Object_Locator;
      --   Call_Target : Object_Locator;
      --   Locked_Param_Info : Locked_Param_Info_Type;

      procedure Execute_Check_Nested_Block_Op (Instr : Instruction);
      --  Execute the Check_Nested_Block_Op instruction
      --  Fields of Instruction are:
      --   Params : Object_Locator;
      --   Static_Link : Object_Locator;
      --   Code_Block : aliased Code_Block_Descriptor;
      --   Assertion_Str : Strings.U_String_Index :=

      procedure Execute_Parallel_Call_Op (Instr : Instruction);
      --  Execute the Start/Add_Parallel_Call_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Parallel_Call_Target : Object_Locator;
      --   Parallel_Locked_Param_Info : Locked_Param_Info_Type;
      --   Parallel_Is_Queued_Call : Boolean := False;

      procedure Execute_Parallel_Op (Instr : Instruction);
      --  Execute the Start/Add_Parallel_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Parallel_Code_Block : Code_Block_Descriptor;

      procedure Execute_Handled_Op (Instr : Instruction);
      --  Execute the Start_Handled_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Parallel_Code_Block : Code_Block_Descriptor;

      procedure Execute_Wait_For_Parallel_Op (Instr : Instruction);
      --  Execute the Wait_For_Parallel_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;

      ----------------------------------
      -- Execute_Call_Nested_Block_Op --
      ----------------------------------

      procedure Execute_Call_Nested_Block_Op (Instr : Instruction) is
      begin
         Execute_Nested_Block
           (Context,
            Instructions => Instructions,
            Params_Address =>
              Locator_To_Physical_Address (Context, Instr.Params),
            Static_Link => Get_Static_Link (Context, Instr.Static_Link),
            Code_Block => Instr.Code_Block,
            Server_Index => Server_Index,
            Base_For_Pc_Offset => Pc);
      end Execute_Call_Nested_Block_Op;

      ---------------------
      -- Execute_Call_Op --
      ---------------------

      procedure Execute_Call_Op (Instr : Instruction) is
         Params_Address : constant Word_Ptr :=
           Locator_To_Physical_Address (Context, Instr.Params);
         Null_Routine_Cpy : aliased Routine := Null_Routine.all;
         --  This Unchecked_Access is OK because it's only passed to
         --  Find_Routine_Context which doesn't store it.
         Target_Routine_Ctx : Routine_Context :=
            (Null_Routine_Cpy'Unchecked_Access, null, null);
      begin
         Find_Routine_Context
           (Context,
            Instr.Call_Target,
            Static_Link => Instr.Static_Link,
            Params => Params_Address,
            Result => Target_Routine_Ctx);

         --  Pass the buck to shared routine
         Execute_Call_Op (Context,
            Params => Params_Address,
            Static_Link => Target_Routine_Ctx.Static_Link,
            Target_Routine => Routine_Ptr (Target_Routine_Ctx.Code),
            Locked_Param_Info => Instr.Locked_Param_Info,
            Polymorphic_Output_Type =>
              Target_Routine_Ctx.Polymorphic_Output_Type);
      end Execute_Call_Op;

      -----------------------------------
      -- Execute_Check_Nested_Block_Op --
      -----------------------------------

      procedure Execute_Check_Nested_Block_Op (Instr : Instruction) is
      --  Execute the Check_Nested_Block_Op instruction
      --  Fields of Instruction are:
      --   Params : Object_Locator;
      --   Static_Link : Object_Locator;
      --   Code_Block : aliased Code_Block_Descriptor;
      --   Assertion_Str : Strings.U_String_Index :=
      begin
         if Doing_Run_Time_Checks then
            --  Execute the check
            Execute_Call_Nested_Block_Op (Instr);

            --  Should be #true
            if Content_Of_Physical_Address (Locator_To_Physical_Address
                                      (Context,
                                       Instr.Params)) = 0
            then
               --  Failed an assertion
               Messages.Put_RT_Error
                 (Strings.To_String
                     (Strings.To_U_String (Instr.Assertion_Str)),
                  Src_Pos => Last_Src_Pos,
                  Message_Kind => "Error: Assertion failed");
                  --  NOTE: We use Last_Src_Pos here because
                  --        it provides a more precise
                  --        source-position of the part of
                  --        the assertion expression that failed.

               --  TBD: Should we continue or raise an exception?
               --      For now we will dump the stack and keep going
               Dump_Stack
                 (Cur_State, Skip_First => True, Use_Cur_Err => True);

               --  Invoke the debugging console if available.
               Invoke_Debug_Console (Context, Reason => Assertion_Failure);

            end if;
         end if;
      end Execute_Check_Nested_Block_Op;

      ------------------------------
      -- Execute_Parallel_Call_Op --
      ------------------------------

      procedure Execute_Parallel_Call_Op (Instr : Instruction) is
         Master_Address : constant Word_Ptr :=
           Locator_To_Physical_Address
              (Context, Instr.Parallel_Master);
         New_Tcb : constant Word_Ptr :=
           Locator_To_Physical_Address (Context, Instr.Parallel_Control);
         Null_Routine_Cpy : aliased Routine := Null_Routine.all;
         --  This Unchecked_Access is OK because it's only passed to
         --  Find_Routine_Context which doesn't store it.
         Target_Routine_Ctx : Routine_Context :=
            (Null_Routine_Cpy'Unchecked_Access, null, null);
      begin  --  Execute_Parallel_Call_Op

         Find_Routine_Context
           (Context,
            Instr.Parallel_Call_Target,
            Static_Link => Instr.Parallel_Static_Link,
            Params => Add (New_Tcb, Thread_Control_Block_Size),
            Result => Target_Routine_Ctx);
         declare
            Target_Routine : constant Routine_Ptr := Routine_Ptr
               (Target_Routine_Ctx.Code);
            Static_Link : constant Word_Ptr :=
              Target_Routine_Ctx.Static_Link;
            Spawning_Tcb : constant Word_Ptr :=
              Context.Control_Area;
            Current_Lock_Index : Lock_Obj_Index := 0;
         begin
            if Debug_Calls then
               Put_Line
                 (" parallel call of " &
                  Strings.To_String (Target_Routine.Name) &
                  ", routine #" &
                  Routine_Index'Image (Target_Routine.Index));
            end if;

            if Target_Routine_Ctx.Polymorphic_Output_Type /= null then
               Messages.Put_RT_Error
                 ("NYI: polymorphic call that needs output wrapped as " &
                    Type_Desc_Name_And_Num
                      (Target_Routine_Ctx.Polymorphic_Output_Type,
                       Use_Short_Form => True),
                  Src_Pos => Execution_Source_Pos (Server_Index));
               raise Program_Error;
            end if;

            pragma Assert
              (Instr.Parallel_Control.Base /= Local_Area
               or else
                 Instr.Parallel_Control.Offset + Thread_Control_Block_Size <=
                   Context.Start_Callee_Locals);

            --  Pass the buck to the general routine to spawn a picothread
            Spawn_Parallel_Thread (Context,
              Master_Address,
              New_Tcb      => New_Tcb,
              Static_Link  => Static_Link,
              Tcb_Is_Local =>
                Instr.Parallel_Control.Base not in Any_Base_Register,
              Is_Start_Op  => Instr.Op = Start_Parallel_Call_Op,
              Routine      => Target_Routine,
              Locked_Param_Info => Instr.Parallel_Locked_Param_Info);
         end;
      end Execute_Parallel_Call_Op;

      -------------------------
      -- Execute_Parallel_Op --
      -------------------------

      procedure Execute_Parallel_Op (Instr : Instruction) is
      --  Execute the Start/Add_Parallel_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Parallel_Code_Block : Code_Block_Descriptor;
         Master_Address : constant Word_Ptr :=
           Locator_To_Physical_Address
              (Context, Instr.Parallel_Master);
         New_Tcb : constant Word_Ptr :=
           Locator_To_Physical_Address
              (Context, Instr.Parallel_Control);
         Static_Link : constant Word_Ptr :=
           Get_Static_Link (Context, Instr.Parallel_Static_Link);

      begin  --  Execute_Parallel_Op

         if Debug_Calls then
            declare
               Param_0_Addr : constant Word_Ptr :=
                 Add (New_Tcb, Tcb_Param_List_Offset);
               Param_0 : constant Word_Type :=
                 Content_Of_Physical_Address (Param_0_Addr);
               use Debug;
            begin
               Put_Line
                 (" invoking parallel block at PC =" &
                  Code_Offset'Image
                     (Pc + Instr.Parallel_Code_Block.Pc_Offset) &
                  ", TCB at " &
                  Hex_Image (New_Tcb) &
                  " Code_Block " &
                  Code_Block_Image (Instr.Parallel_Code_Block));
               Put_Line
                 (" (Param_Area, 0) i.e. " &
                  Hex_Image (Param_0_Addr) &
                  " = " &
                  Hex_Image (Param_0));
               Put (" dump_obj () = ");
               Dump_Obj (Param_0);
            end;
         end if;

         Spawn_Parallel_Thread (Context,
           Master_Address,
           New_Tcb      => New_Tcb,
           Static_Link  => Static_Link,
           Tcb_Is_Local =>
             Instr.Parallel_Control.Base not in Any_Base_Register,
           Is_Start_Op  => Instr.Op = Start_Parallel_Op,
           Routine      => Instructions,
           Nested_Block => Instr.Parallel_Code_Block'Unchecked_Access,
           Start_Pc     => Pc + Instr.Parallel_Code_Block.Pc_Offset);

      end Execute_Parallel_Op;

      ------------------------
      -- Execute_Handled_Op --
      ------------------------

      procedure Execute_Handled_Op (Instr : Instruction) is
      --  Execute the Start_Handled_Op instructions
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Parallel_Code_Block : Code_Block_Descriptor;
         Master_Address : constant Word_Ptr :=
           Locator_To_Physical_Address
              (Context, Instr.Parallel_Master);
         Static_Link : constant Word_Ptr :=
           Get_Static_Link (Context, Instr.Parallel_Static_Link);

      begin  --  Execute_Handled_Op

         if Debug_Calls then
            Put_Line
              (" invoking handled block at PC =" &
               Code_Offset'Image
                  (Pc + Instr.Parallel_Code_Block.Pc_Offset) &
               ", Code_Block " &
               Code_Block_Image (Instr.Parallel_Code_Block));
         end if;

         Spawn_Parallel_Thread (Context,
           Master_Address,
           New_Tcb      => null,  --  Indicates is a handler
           Static_Link  => Static_Link,
           Tcb_Is_Local => True,
           Is_Start_Op  => True,
           Routine      => Instructions,
           Nested_Block => Instr.Parallel_Code_Block'Unchecked_Access,
           Start_Pc     => Pc + Instr.Parallel_Code_Block.Pc_Offset);

      end Execute_Handled_Op;

      ----------------------------------
      -- Execute_Wait_For_Parallel_Op --
      ----------------------------------

      procedure Execute_Wait_For_Parallel_Op (Instr : Instruction) is
         Master_Address : constant Word_Ptr :=
           Locator_To_Physical_Address
              (Context, Instr.Parallel_Master);

         --  Master should be the current "open" master.
         pragma Assert (Master_Address = Context.Open_Master);
      begin  --  Execute_Wait_For_Parallel_Op

         --  Pass the buck to shared routine
         Wait_For_Open_Master (Context);
      end Execute_Wait_For_Parallel_Op;

      function Src_Pos_Of_Exc_Info (Exc_Info : String)
        return Source_Positions.Source_Position;
      --  Return source-position part of exception info

      function Src_Pos_Of_Exc_Info (Exc_Info : String)
        return Source_Positions.Source_Position is
         Result : Source_Positions.Source_Position;
      begin
         --  Look for " : " and treat next contiguous string of non-blanks
         --  as a file name and line.
         Outer_Loop :
         for I in Exc_Info'First .. Exc_Info'Last - 2 loop
            if Exc_Info (I .. I + 2) = " : " then
               --  Look for next white space
               for J in I + 3 .. Exc_Info'Last loop
                  if Exc_Info (J) = ' ' then
                     Result.File := Strings.Index
                       (Strings.String_Lookup (Exc_Info (I + 3 .. J - 1)));
                     exit Outer_Loop;
                  end if;
               end loop;
               Result.File := Strings.Index
                 (Strings.String_Lookup (Exc_Info (I + 3 .. Exc_Info'Last)));
               exit Outer_Loop;
            end if;
         end loop Outer_Loop;
         return Result;
      end Src_Pos_Of_Exc_Info;

      function Rest_Of_Exc_Info (Exc_Info : String) return String;
      --  Return error indicator and traceback part of exception info

      function Rest_Of_Exc_Info (Exc_Info : String) return String is
      begin
         --  Look for " : " and return everything after
         --  next contiguous string of non-blanks
         for I in Exc_Info'First .. Exc_Info'Last - 2 loop
            if Exc_Info (I .. I + 2) = " : " then
               --  Look for next white space
               for J in I + 3 .. Exc_Info'Last loop
                  if Exc_Info (J) = ' ' then
                     return Exc_Info (J + 1 .. Exc_Info'Last);
                  end if;
               end loop;
               return "";
            end if;
         end loop;
         return Exc_Info;
      end Rest_Of_Exc_Info;

      Prev_State : aliased Server_State := Cur_State;

      procedure Set_Cur_State;
      pragma Inline (Set_Cur_State);
         --  Set up Cur_State and link to Prev_State.

      procedure Set_Cur_State is
      begin
         --  Remember state (for debugging)
         Cur_State.Prev_State := Prev_State'Unchecked_Access;
         Cur_State.Code := Instructions;
         Cur_State.Context := Context'Unchecked_Access;
         Cur_State.Pc := Start_Pc;
         Cur_State.Start_Pc := Start_Pc;

         --  Reset the debugger flags, as appropriate
         Cur_State.Stopped_At_Line := 0;
         if Cur_State.Step_Indicator = Single_Step_Over then
            --  We want to step over this call, so clear the flag.
            Cur_State.Step_Indicator := Continue_Execution;
         end if;

         if Context.Control_Area /= null then
            Cur_State.Locked_Param_Index :=
               Tcb_Locked_Param_Index (Context.Control_Area);
         else
            Cur_State.Locked_Param_Index := 0;
         end if;
      end Set_Cur_State;

      use type Strings.U_String_Index;

   begin --  Execute

--       if Context.Params.Enclosing_Chunk =
--          Context.Local_Area.Enclosing_Chunk
--       then
--          --  Params should be earlier in chunk, if both in same chunk.
--          --  NOTE: Can be the same if there are no parameters (e.g.
--          --       a parameterless call, or an operand of "||").
--          pragma Assert (Context.Params.Offset <= Context.Local_Area.Offset);
--          null;
--       end if;

      --  Local area length should always be more than start callee locals
      --  (except for an imported or null routine).
      pragma Assert
        (not Instructions.Is_PSVM_Routine
        or else Context.Start_Callee_Locals < Context.Local_Area_Length
        or else Instructions.Code = null);

      Check_Static_Chain (Context.Local_Area);

      Thread_Was_Queued := False;  --  Presume not queued by default.

      --  Remember state (for debugging)
      Set_Cur_State;

      --  TBD: Check for locked-param, and call Locked_Call instead.

      if Debug_Calls then
         declare
            Param_Addr : constant Word_Ptr :=
              Locator_To_Physical_Address (Context, (Param_Area, 0,
                                                     No_VM_Obj_Id));
            use Debug;
         begin
            Put ("--> " & Strings.To_String (Instructions.Name));
            if Start_Pc > Code_Index'First then
               Put (" +" & Code_Index'Image (Start_Pc));
            elsif not Instructions.Is_PSVM_Routine then
               Put ("[imported]");
            end if;
            Dump_Param_Decls (Instructions);
            Put_Line (" Param_Area at " & Hex_Image (Param_Addr));
            if Start_Pc = Code_Index'First then
               Dump_Param_Values (Instructions, Context, On_Entering => True);
            else
               Put ("Param 0 = ");
               Dump_Obj (Content_Of_Physical_Address (Param_Addr));
            end if;
         end;
      end if;

      if not Instructions.Is_PSVM_Routine then
         --  This is a built-in or compiled routine
         --  Call it and return.
         Cur_State.Src_Pos := Source_Positions.Null_Source_Position;

         Call_Compiled_Routine
           (Context, Context.Params,
            To_Non_Op_Map_Type_Desc (Static_Link),
            Instructions.Routine_Addr, Instructions.Conv_Desc);

         if Debug_Calls then
            Put
              ("<-- " &
               Strings.To_String (Instructions.Name) &
               "[imported]");
            Dump_Param_Decls (Instructions);
            Dump_Param_Values (Instructions, Context, On_Entering => False);
         end if;

         --  Restore state
         Cur_State := Prev_State;
         return;   --- All done ---
      end if;

      if Instructions.Code = null then
         --  Presume this is an "optional" routine.
         --  TBD: Should check this somehow!
         if Debug_Calls then
            Messages.Put_Message ("Null routine found for call on " &
                Strings.To_String (Instructions.Name),
              Src_Pos => Prev_State.Src_Pos,
              Message_Kind => "Info");
         end if;

         --  Restore state
         Cur_State := Prev_State;
         return;
      end if;

      Cur_State.Src_Pos := Instructions.Code.Instrs (Start_Pc).Source_Pos;

      --  Initialize link to param area
      --  TBD: This might more optimally be done in start parallel op
      --      or other op that creates a nested local area.
      Store_Word_Ptr (Context.Local_Area, Local_Area_Param_Ptr_Offset,
         Context.Params);

      if Debugger_Console then
         --  User has requested we enter the debug console
         --  before doing anything else.
         Invoke_Debug_Console (Context,
           Reason => No_Reason);
      end if;

      loop
         --  Execute the instruction at given PC
         declare
            --  Not allowed to just "fall off" the end
            pragma Assert (Pc <= Instructions.Code.Instrs'Last);

            Instr : Instruction renames Instructions.Code.Instrs (Pc);
            use type Source_Positions.Line_Number;
         begin
            Cur_State.Pc := Pc;
            if Instr.Source_Pos.Line /= 0 then
               Cur_State.Src_Pos := Instr.Source_Pos;
               if Cur_State.Step_Indicator /= Continue_Execution then
                  --  See if we should stop now
                  case Cur_State.Step_Indicator is
                     when Single_Step_Into =>
                        if Cur_State.Src_Pos.Line /=
                          Cur_State.Stopped_At_Line
                        then
                           --  Stop as soon as the line does not match
                           if Pc = 1
                             and then
                              Prev_State.Step_Indicator = Single_Step_Into
                           then
                              --  But first clear the indicator in the
                              --  enclosing frame
                              Prev_State.Step_Indicator := Continue_Execution;
                              Prev_State.Stopped_At_Line := 0;
                           end if;

                           Invoke_Debug_Console (Context,
                             Reason => Step_Into_Finished);
                        end if;

                     when Single_Step_Over =>
                        if Cur_State.Src_Pos.Line /=
                          Cur_State.Stopped_At_Line
                        then
                           --  Stop as soon as the line does not match
                           Invoke_Debug_Console (Context,
                             Reason => Step_Over_Finished);
                        end if;

                     when Step_Out =>
                        --  Reached the frame where we should stop
                        Invoke_Debug_Console
                          (Context, Reason => Step_Out_Finished);

                     when Stop_Execution
                        | Continue_Execution =>
                        --  These shouldn't occur
                        pragma Assert (False);
                        null;
                  end case;
               end if;
            elsif Instr.Source_Pos.End_Line /= 0 then
               --  A zero Line with a non-zero End_Line is a special
               --  combination meaning "enter the debugger"
               --  (i.e. is a breakpoint).
               --  End_Col contains the breakpoint index.
               Cur_State.Src_Pos := Instr.Source_Pos;
               --  Copy the line form the end-line
               Cur_State.Src_Pos.Line := Instr.Source_Pos.End_Line;

               --  Check to see if we have already stopped at current line
               if Cur_State.Stopped_At_Line /= Cur_State.Src_Pos.Line then
                  --  We were not just stopped at this line, so enter the
                  --  debugger console (presuming it is loaded).
                  --  End_Col contains the breakpoint index
                  Invoke_Debug_Console (Context,
                    Reason => Debugger_Reason (Cur_State.Src_Pos.End_Col));
               end if;
            end if;

            Pc := Pc + 1;

            case Instr.Op is
            --  Do the op-code specific actions
            when Skip_Op =>
               Pc := Pc + Instr.Skip_Count;

            when Call_Op | Indirect_Call_Op =>
               Execute_Call_Op (Instr);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Call_Nested_Block_Op =>
               Execute_Call_Nested_Block_Op (Instr);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Check_Nested_Block_Op =>
               --  TBD: Only do this if checks are on, and/or this one is
               --       not proved.
               Execute_Check_Nested_Block_Op (Instr);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Check_Not_Null_Op =>
               --  We want to be sure the value of destination is not null.
               Check_Not_Null
                 (Context,
                  Instr.Destination,
                  Strings.To_U_String (Instr.Dest_Name),
                  Instr.Null_Type_Info,
                  Src_Pos => Cur_State.Src_Pos);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Exit_Op =>
               --   Set Master_Outcome to Exit_Op if required
               if Instr.Level_Diff > 1
                 or else Instr.Skip_Count > 0
               then
                  Set_Enclosing_Master_Outcome (Context,
                    Outcome         => Exit_Outcome,
                    Exit_Level_Diff => Instr.Level_Diff,
                    Exit_Skip_Count => Instr.Skip_Count);
               end if;

               exit;  ----- exit the loop

            when Return_Op =>
               if Start_Pc > Code_Index'First then
                  --  Set Master_Outcome to Return_From_Op
                  Set_Enclosing_Master_Outcome
                    (Context, Return_From_Operation_Outcome);
               end if;
               exit;  ----- exit the loop

            when Is_Null_Op | Not_Null_Op =>
               --  Compute is_null/not_null
               declare
                  Obj_Value : constant Word_Type :=
                    Fetch_Word (Context, Instr.Source);
                  Obj_Is_Null : constant Boolean :=
                    Is_Null_Value
                       (Obj_Value,
                        Get_Type_Desc (Context, Instr.Type_Info));
               begin
                  --  Convert bool answer into a word and store in destination
                  Store_Word
                    (Context,
                     Instr.Destination,
                     Boolean'Pos (Obj_Is_Null = (Instr.Op = Is_Null_Op)));
               end;

            when Store_Local_Null_Op =>
               --  We want to store a "large" null if the
               --  run-time type info implies that is necessary
               --  We want to store the right "kind" of null if small.
               Store_Local_Null
                 (Context,
                  Instr.Destination,
                  Instr.Null_Type_Info);

            when Store_Large_Local_Null_Op =>
               --  Store a null for a large local object
               Store_Word
                 (Context,
                  Instr.Destination,
                  Null_For_Local_Stg_Rgn (Locator_To_Physical_Address
                    (Context, Instr.Local_Addr)));

            when Store_Null_Of_Same_Stg_Rgn_Op =>
               --  We want to store a "large" null if the
               --  run-time type info implies that is necessary
               --  Stg_Rgn comes from source if large.
               --  We want to store the right "kind" of null if small.
               Store_Null_Of_Same_Stg_Rgn
                 (Context,
                  Destination => Instr.Destination,
                  Existing_Obj_In_Stg_Rgn => Instr.Source,
                  Type_Info => Instr.Type_Info);

            when Store_Int_Lit_Op =>
               Store_Word (Context, Instr.Destination, Instr.Int_Value);

            when Store_Char_Lit_Op =>
               Store_Word (Context, Instr.Destination, Instr.Char_Value);

            when Store_Real_Lit_Op =>
               Store_Real (Context, Instr.Destination, Instr.Real_Value);

            when Store_Str_Lit_Op =>
               --  Create Univ_String object in appropriate storage region
               declare
                  Null_In_Rgn : Word_Type := Null_Value;
               begin
                  if not Is_Null_Obj_Locator
                             (Instr.Existing_Str_In_Stg_Rgn)
                  then
                     --  Target determines region
                     Null_In_Rgn :=
                       Univ_Strings.Null_Of_Same_Rgn
                         (Univ_Strings.From_Word_Type (Fetch_Word
                           (Context, Instr.Existing_Str_In_Stg_Rgn)));
                  else
                     --  Use local region
                     Null_In_Rgn := Null_For_Stg_Rgn (Local_Stg_Rgn (Context));
                  end if;

                  Store_Word
                    (Context,
                     Instr.Destination,
                     Univ_Strings.To_Word_Type (Univ_Strings.From_U_String
                       (Strings.To_U_String (Instr.Str_Value), Null_In_Rgn)));
               end;
            when Store_Enum_Lit_Op =>
               Store_Word
                 (Context,
                  Instr.Destination,
                  Word_Type (Instr.Enum_Value));

            when Store_Type_Related_Obj_Op =>
               --  Store the value of a nested obj or some other
               --  type-related constant.
               Store_Word
                 (Context,
                  Instr.Destination,
                  Nth_Type_Area_Word
                    (Get_Type_Desc (Context, Instr.Source_Type_Info),
                     Item_Offset => Instr.Source.Offset,
                     Type_Base   => Instr.Source.Base));

            when Store_Type_Related_Addr_Op =>
               --  Store the address of a nested obj or some other
               --  type-related constant.
               Store_Word
                 (Context,
                  Instr.Destination,
                  Word_Ptr_To_Word
                    (Nth_Type_Area_Item_Physical_Address
                       (Get_Type_Desc (Context, Instr.Source_Type_Info),
                        Item_Offset => Instr.Source.Offset,
                        Type_Base   => Instr.Source.Base)));

            when Store_Operation_Desc_Op =>
               Store_Word
                 (Context,
                  Instr.Destination,
                  Create_Operation_Desc (Context,
                                         Instr.Operation_Locator,
                                         Operation_Static_Link =>
                                           Instr.Operation_Static_Link,
                                         Existing_Obj_In_Stg_Rgn =>
                                           Instr.Source));

            when Declare_Obj_Op =>
               --  This is a no-op at the moment in the interpreter
               if Debug_Stack and then Solo_Server /= 0
                 and then Instr.Dest_Name /= Strings.Null_U_String_Index
               then
                  Put_Line ("Declare_Obj_Op " &
                    Strings.To_String
                      (Strings.To_U_String (Instr.Dest_Name)) & " at " &
                    Obj_Locator_Image (Instr.Destination) & " = " &
                    Hex_Image
                      (Locator_To_Physical_Address
                        (Context, Instr.Destination)));
               end if;

            when Copy_Word_Op | Copy_Address_Op =>
               Store_Word
                 (Context,
                  Instr.Destination,
                  Fetch_Word (Context, Instr.Source));

            when Store_Address_Op =>
               --  Turn obj "locator" into a physical address.
               --  Used for "ref" parameters.
               declare
                  Addr : constant Word_Ptr :=
                    Locator_To_Physical_Address (Context, Instr.Source);
               begin
                  if Addr = null then
                     Messages.Put_RT_Error
                       ("Passing a null address for a ref",
                        Src_Pos => Execution_Source_Pos (Server_Index));
                  end if;

                  Store_Word
                    (Context,
                     Instr.Destination,
                     Word_Ptr_To_Word (Addr));
               end;

            when Assign_Word_Op =>
               --  We want to release the old value of
               --  the destination before overwriting it with source.
               Assign_Word
                 (Context,
                  Destination => Instr.Destination,
                  Source => Instr.Source,
                  Type_Info => Instr.Type_Info);

            when Swap_Obj_Op =>
               --  We want to release the old values of
               --  the destination and source before overwriting them
               --  if necessary.
               Swap_Obj
                 (Context,
                  LHS => Instr.Destination,
                  RHS => Instr.Source,
                  Type_Info => Instr.Type_Info);

            when Move_Obj_Op =>
               --  We want to release old value of destination
               --  before overwriting it, and set source to null
               --  without releasing old value.
               Move_Obj
                 (Context,
                  LHS => Instr.Destination,
                  RHS => Instr.Source,
                  Type_Info => Instr.Type_Info);

            when Create_Obj_Op =>
               --  If object is large then create object of appropriate
               --  size in region determined by existing object,
               --  and initialize its subcomponents to nulls of appropriate
               --  kinds and regions.
               --  If object is small then create null of appropriate kind.
               --  If object is a wrapper, then recurse on component type.
               Create_Obj
                 (Context,
                  Destination => Instr.Destination,
                  Existing_Obj_In_Stg_Rgn => Instr.Source,
                  Type_Info => Instr.Type_Info);

            when Make_Copy_In_Stg_Rgn_Op =>
               --  We want to make copy of source if large
               --  in region determined by Existing_Obj_In_Stg_Rgn
               Make_Copy_In_Stg_Rgn
                 (Context,
                  Destination => Instr.Destination,
                  Source => Instr.Source,
                  Existing_Obj_In_Stg_Rgn => Instr.Existing_Obj_In_Stg_Rgn,
                  Type_Info => Instr.Type_Info);

            when Select_Ancestor_Part_Op =>
               --  Select desired ancestor part
               if Instr.Ancestor_Lvalue then
                  --  We want an Lvalue
                  Store_Word (Context, Instr.Destination,
                     Select_Ancestor_Part
                       (Context,
                        Source_Obj => Word_Ptr_To_Word
                                        (Locator_To_Physical_Address
                                          (Context, Instr.Source)),
                        Ancestor_Type_Desc =>
                           Get_Type_Desc (Context, Instr.Type_Info),
                        Source_Type_Desc =>
                           Get_Type_Desc (Context, Instr.Source_Type_Info),
                        Is_Passed_By_Ref => True));
               else
                  --  We want an Rvalue
                  Store_Word (Context, Instr.Destination,
                     Select_Ancestor_Part
                       (Context,
                        Source_Obj => Fetch_Word (Context, Instr.Source),
                        Ancestor_Type_Desc =>
                           Get_Type_Desc (Context, Instr.Type_Info),
                        Source_Type_Desc =>
                           Get_Type_Desc (Context, Instr.Source_Type_Info),
                        Is_Passed_By_Ref => False));
               end if;

            when Select_Polymorphic_Ancestor_Part_Op =>
               --  Select desired ancestor part of polymorphic obj
               Select_Polymorphic_Ancestor_Part
                 (Context,
                  Destination => Instr.Destination,
                  Source => Instr.Source,
                  Type_Info => Instr.Type_Info,
                  Polymorphic_Ancestor_Lvalue =>
                    Instr.Polymorphic_Ancestor_Lvalue);

            when Start_Handled_Op =>
               Execute_Handled_Op (Instr);
               --  tbd:? exit when Tcb_Exit_Requested (Context.Control_Area);

            when Start_Parallel_Op | Add_Parallel_Op =>
               Execute_Parallel_Op (Instr);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Start_Parallel_Call_Op | Add_Parallel_Call_Op =>
               Execute_Parallel_Call_Op (Instr);
               exit when Tcb_Exit_Requested (Context.Control_Area);

            when Prepare_To_Exit_Parallel_Op =>
               if not Execute_Prepare_To_Exit_Parallel_Op
                  (Context, Locator_To_Physical_Address
                       (Context, Instr.Parallel_Master))
               then
                  --  Prepare-to-exit failed, so we should just quit now.
                  --  Outcome will have already been set in master.
                  exit;
               end if;

            when Wait_For_Parallel_Op =>
               Execute_Wait_For_Parallel_Op (Instr);
               --  Now check the outcome
               declare
                  Master_Address : constant Word_Ptr :=
                    Locator_To_Physical_Address
                       (Context, Instr.Parallel_Master);
                  Outcome : constant Master_Outcome_Enum :=
                    Master_Outcome (Master_Address);
               begin
                  --  deal with outcome of the "wait"
                  case Outcome is
                     when Normal_Outcome =>
                        --  Check for exit-requested flag (TBD: Is this right?)
                        exit when Tcb_Exit_Requested (Context.Control_Area);
                     when Return_From_Operation_Outcome =>
                        --  Keep returning until we return from operation.
                        if Start_Pc > Code_Index'First then
                           --  Need to keep returning
                           --  Update outer master to indicate Return outcome
                           Set_Enclosing_Master_Outcome
                             (Context, Return_From_Operation_Outcome);
                        end if;
                        exit;  --  Keep returning
                     when Exit_Outcome =>
                        --  Keep exiting if necessary
                        declare
                           Skip_Count : constant Code_Offset :=
                             Exit_Skip_Count (Master_Address);
                           Level_Diff : constant Natural :=
                             Exit_Level_Diff (Master_Address);
                        begin
                           if Level_Diff > 1 then
                              if Level_Diff > 2 or else Skip_Count > 0 then
                                 --  Update outer master with one
                                 --  lower Level_Diff
                                 --  NOTE: We don't bother if this is "normal"
                                 --        single-level skip-free exit.
                                 Set_Enclosing_Master_Outcome
                                   (Context,
                                    Outcome         => Exit_Outcome,
                                    Exit_Level_Diff => Level_Diff - 1,
                                    Exit_Skip_Count => Skip_Count);
                              end if;
                              exit;  --- Keep exiting
                           end if;
                           if Skip_Count /= 0 then
                              --  Skip the specified amount
                              Pc := Pc + Skip_Count;
                           end if;
                        end;
                  end case;
               end;

            when Create_Lock_For_Obj_Op =>
               --  Execute the Create_Lock_For_Obj_Op instruction
               --  which creates a lock if the object doesn't already
               --  have one.
               Create_Lock_For_Obj (Fetch_Word (Context, Instr.Destination),
                 Server_Index);

            when Create_Tcb_Op =>
               Create_Tcb (
                  Context,
                  Locator_To_Physical_Address (Context, Instr.Parallel_Master),
                  Locator_To_Physical_Address
                     (Context, Instr.Parallel_Control),
                  Instr.Num_In_Params + Instr.Num_Out_Params);

            when Create_Polymorphic_Obj_Op =>
               --  Execute the Create_Polymorphic_Obj_Op instruction
               --  which wraps the Destination in a 2-word polymorphic obj.
               declare
                  Existing_Obj : Word_Type := 0;
               begin
                  if not Is_Null_Obj_Locator (Instr.Source) then
                     --  Fetch existing obj which determines region to use
                     Existing_Obj := Fetch_Word (Context, Instr.Source);
                  end if;

                  Create_Poly_Obj_Exported (Context,
                     Existing_Obj,
                     Locator_To_Physical_Address (Context, Instr.Destination),
                     Get_Type_Desc (Context, Instr.Type_Info));
               end;

            when Unwrap_Polymorphic_Obj_Op =>
               --  Execute the Unwrap_Polymorphic_Obj_Op instruction
               --  which unwraps the source to create a "ref," if its
               --  type matches Instr.Type_Info.  Otherwise it creates
               --  a "null" ref.
               --  Fields of Instruction are:
               --  Destination : Object_Locator;
               --  Source : Object_Locator;
               --  Type_Info : Object_Locator;
               --  Source_Type_Info : Object_Locator;
               Store_Word
                 (Locator_To_Physical_Address
                    (Context, Instr.Destination),
                  0,
                  Unwrapped_Polymorphic_Obj
                    (Get_Type_Desc (Context, Instr.Source_Type_Info),
                     Get_Type_Desc (Context, Instr.Type_Info),
                     Fetch_Word (Context, Instr.Source)));

            when If_Op =>
               declare
                  Source_Value : constant Ordering :=
                    Ordering'Val (Fetch_Word (Context, Instr.If_Source));
               begin
                  if (Ordering_Mask (Source_Value)
                    and Instr.If_Condition) = 0
                  then
                     --  Go to "else" part of "if"
                     Pc := Pc + Instr.Skip_If_False;
                  end if;
               end;

            when Case_Op =>
               --  Skip appropriate amount based on Case_Selector
               declare
                  Selector_Value : constant Word_Type :=
                    Fetch_Word (Context, Instr.Case_Selector);
               begin
                  if Selector_Value in
                       Instr.Case_First .. Instr.Case_Last
                  then
                     --  Skip appropriate amount
                     Pc := Pc +
                           Code_Offset (Selector_Value - Instr.Case_First);
                  else
                     --  Go to default
                     Pc := Pc + Instr.Case_Default_Skip;
                  end if;
               end;

            when Begin_Nested_Block_Op =>
               --  This is a no-op in the interpreter
               --  but it must only be reached when it is the first
               --  instruction in a call on Execute.
               pragma Assert (Pc = Start_Pc + 1);
               null;

            when Loop_Op =>
               null;  --  TBD
            end case;
         end;
      end loop;

      --  Exit or return statement causes the loop to be exited.

      if Context.Open_Master /= null then
         --  Should not exit with an "open" master
         if not Tcb_Exit_Requested (Context.Control_Area) then
            --  We are not "abruptly" exiting a thread, so this
            --  looks like a bug.
            Messages.Put_RT_Error
              ("Internal: Missing a ""Wait"" for master" &
                Master_Index'Image (Index_Of_Master (Context.Open_Master)) &
                " at " & Hex_Image (Context.Open_Master),
               Src_Pos => Execution_Source_Pos (Server_Index));
         else
            if Debug_Threading then
               Put_Line (" About to wait for open master" &
                 Master_Index'Image (Index_Of_Master (Context.Open_Master)));
            end if;
         end if;

         --  Wait for master now, and reset Open_Master to null.
         Wait_For_Open_Master (Context);
         --  TBD: Should we be checking the master "outcome" here?
      end if;

      if Debug_Calls then
         declare
            Param_Addr : constant Word_Ptr :=
              Locator_To_Physical_Address (Context, (Param_Area, 0,
                                                     No_VM_Obj_Id));
         begin
            Put ("<-- " & Strings.To_String (Instructions.Name));
            if Start_Pc > Code_Index'First then
               Put (" +" & Code_Index'Image (Start_Pc));
            end if;
            Dump_Param_Decls (Instructions);
            Put_Line (" Param_Area at " & Hex_Image (Param_Addr));
            if Start_Pc = Code_Index'First then
               Dump_Param_Values
                 (Instructions,
                  Context,
                  On_Entering => False);
            else
               Put ("Param 0 = ");
               Dump_Obj (Content_Of_Physical_Address (Param_Addr));
            end if;
         end;
      end if;

      --  Save source position just prior to return.
      --  NOTE: This should preserve a more precise source position in case
      --        some part of an assertion expression and-then chain fails.
      Last_Src_Pos := Cur_State.Src_Pos;

      if Cur_State.Step_Indicator /= Continue_Execution then
         --  We are in the middle of a single step.
         --  Stop immediately after restoring the state.
         declare
            Prior_Step_Indicator : constant Single_Step_Indicator :=
              Cur_State.Step_Indicator;
         begin
            --  Restore the state now
            Cur_State := Prev_State;

            case Prior_Step_Indicator is
               when Single_Step_Into =>
                  --  Stop upon exiting a frame
                  Invoke_Debug_Console (Context,
                    Reason => Step_Into_Exited_Frame);

               when Single_Step_Over =>
                  --  Stop upon exiting a frame
                  Invoke_Debug_Console (Context,
                    Reason => Step_Over_Exited_Frame);

               when Step_Out
                  | Stop_Execution
                  | Continue_Execution =>
                  --  These shouldn't occur
                  pragma Assert (False);
                  null;
            end case;
         end;
      else
         --  Restore state
         Cur_State := Prev_State;
      end if;

      if Is_Shut_Down
        and then Prev_State.Context = null
        and then Server_Index = Main_Thread_Server_Index
      then
         --  Indicate that the user terminated the interpreter.
         --  TBD: Find a better way to indicate this.
         Context.Server_Index := Thread_Server_Index'Last;
      end if;

   exception
      when Storage_Error =>
         --  Not much to do here
         raise;

      when Propagating_Exception =>
         Messages.Put_RT_Error
           ("Propagating exception",
            Src_Pos => Execution_Source_Pos (Server_Index),
            Message_Kind => "Info");

         --  Reset the Cur_State in case it comes from lower on the stack.
         Set_Cur_State;

         Dump_Stack (Cur_State, Num_Stack_Frames => 1, Use_Cur_Err => True);

         --  Invoke the debugging console if available.
         Invoke_Debug_Console (Context, Reason => Internal_Failure);

         --  Restore state
         Cur_State := Prev_State;
         raise;
      when E : others =>
         Messages.Put_RT_Error
           (Rest_Of_Exc_Info (Ada.Exceptions.Exception_Information (E)),
            Src_Pos =>
              Src_Pos_Of_Exc_Info (Ada.Exceptions.Exception_Information (E)),
            Message_Kind => "Error");
         Messages.Put_RT_Error
           ("Exception " & Ada.Exceptions.Exception_Name (E) & " propagated.",
            Src_Pos => Execution_Source_Pos (Server_Index));

         --  Reset the Cur_State in case it comes from lower on the stack.
         Set_Cur_State;

         Dump_Stack (Cur_State, Num_Stack_Frames => 1, Use_Cur_Err => True);

         --  Invoke the debugging console if available.
         Invoke_Debug_Console (Context, Reason => Internal_Failure);

         --  Restore state
         Cur_State := Prev_State;
         raise Propagating_Exception;
   end Execute;

   ---------------------
   -- Execute_Call_Op --
   ---------------------

   procedure Execute_Call_Op
     (Context                 : in out Exec_Context;
      Params                  : Word_Ptr;
      Static_Link             : Word_Ptr;
      Target_Routine          : Routine_Ptr;
      Locked_Param_Info       : Locked_Param_Info_Type;
      Polymorphic_Output_Type : Type_Descriptor_Ptr := null)
   is
      New_Local_Area : constant Word_Ptr :=
        Add (Context.Local_Area, Context.Start_Callee_Locals);
      --  TBD: This presumes that caller local area
      --      includes room for longest callee's local area.
      --      Clearly doesn't work for recursion!
      --      Once we start allocating dynamically we need to
      --      worry about reaching the end of the chunk,
      --      and more generally whether space after end
      --      of current local area is in use for other
      --      threads.

      New_Context : aliased Exec_Context :=
        (Local_Null => Null_Virtual_Address, -- init'ed below
         Enclosing_Type => Get_Enclosing_Type (Static_Link),
         Local_Stg_Rgn => null,  --  Initialized below
         Control_Area => Context.Control_Area,
         Open_Master => null,
         Server_Index => Context.Server_Index,
         Params => Params,
         Local_Area => New_Local_Area,
         Local_Area_Length => Target_Routine.Local_Area_Length,
         Start_Callee_Locals => Target_Routine.Start_Callee_Locals);

      Thread_Was_Queued : Boolean;

      --  Used for Polymorphic_Output_Type
      Original_Output : Word_Type := Null_Virtual_Address;

      Info : Server_Info renames Server_Info_Array (Context.Server_Index);

      Cur_State : Server_State renames Info.Current_State;

      Stack_Check_Obj : aliased Word_Type := 0;

      use Debug;

      use System.Storage_Elements;
   begin --  Execute_Call_Op

      if Debug_Stack then
         if To_Integer (Stack_Check_Obj'Address) < Info.Lowest_Stack then
            Info.Lowest_Stack := To_Integer (Stack_Check_Obj'Address);
            Put_Line ("--> Server" &
              Thread_Server_Index'Image (Context.Server_Index) &
              " stack now " &
              Hex_Image (Word_Type (Info.Lowest_Stack)) &
              " calling " & Strings.To_String (Target_Routine.Name));
         end if;
      end if;

      if Debug_Calls then
         Put_Line
           (" calling " &
            Strings.To_String (Target_Routine.Name) &
            ", routine #" &
            Routine_Index'Image (Target_Routine.Index));
      end if;

      if Polymorphic_Output_Type /= null then
         --  Save the output(s) that need to be wrapped,
         --  in case they will be small when unwrapped.
         --  TBD: Handle multiple outputs
         Original_Output := Content_Of_Physical_Address (Params);
         if Debug_Calls then
            Put_Line
              ("  polymorphic call that needs output wrapped as " &
                 Type_Desc_Name_And_Num
                   (Polymorphic_Output_Type,
                    Use_Short_Form => True));
         end if;
         Check_Is_Large (Original_Output);
      end if;

      if not Target_Routine.Is_PSVM_Routine then
         --  Just pass through the current local region if imported
         New_Context.Local_Stg_Rgn := Context.Local_Stg_Rgn;
         New_Context.Local_Null := Context.Local_Null;
      else
         --  Get a new local region
         New_Context.Local_Stg_Rgn :=
            Get_New_Local_Stg_Rgn
              (New_Local_Area => New_Local_Area,
               Server_Index => Context.Server_Index);

         New_Context.Local_Null := New_Context.Local_Stg_Rgn.Null_Value;

         --  Initialize static link
         Init_Static_Link
           (New_Local_Area, Static_Link);
      end if;

      --  TBD: Do the external precondition if not doing static checking.

      if Locked_Param_Info.Param_Index > 0 then
         if Debug_Calls then
            Put_Line
              (" with lock on param #" &
               Natural'Image (Locked_Param_Info.Param_Index));
         end if;
         --  NOTE: The Locked_Param_Index is only set if the caller-
         --       does *not* already have a lock.  If the caller
         --       has a lock, then there is no new lock required,
         --       and a dequeue condition becomes a precondition.
         --       Interestingly, the dequeue condition is not generally
         --       visible externally, but presumably if the caller
         --       already has the lock, then they can "see" the
         --       dequeue condition, and so can the compiler.
         --  NOTE2: There is no equivalent of a "requeue" in ParaSail.
         --       Hopefully this won't be needed, since the dequeue
         --       condition can depend on parameters.  Also, if
         --       an operation involves multiple steps, then it
         --       is possible to export a routine that does not
         --       get a lock, and calls two queued operations in
         --       sequence.

         --  Execute the call under a lock on the given parameter.
         --  Use the internal precondition as a dequeue condition.
         --  TBD: A queued call should *never* go through this code
         --      since it should always be executed as a "parallel_call"
         --      and hence involve a call on Execute_For_Thread.
         Execute_Locked_Call_Op
           (Target_Routine,
            New_Context,
            Locked_Param_Info => Locked_Param_Info,
            Server_Index => Context.Server_Index,
            Thread_Was_Queued => Thread_Was_Queued);

      elsif not Target_Routine.Is_PSVM_Routine then

         --  This is a built-in routine.
         --  Call it.
         --  NOTE: We are assuming the built-in operation checks the
         --       internal precondition if not doing static checking.

         if Solo_Server = 0 then
            --  Update Cur_State.Context temporarily to point to New_Context
            --  so callbacks will have something to start from.
            --  NOTE: We suppress this while in the interactive debugger
            --        since it fouls up Nth_Stack_Frame.
            Cur_State.Context := New_Context'Unchecked_Access;
         end if;

         if Debug_Calls then
            Dump_Param_Values
              (Target_Routine, New_Context, On_Entering => True);
         end if;

         if Target_Routine.Routine_Addr /= null then
            Call_Compiled_Routine
              (New_Context, New_Context.Params,
               To_Non_Op_Map_Type_Desc (Static_Link),
               Target_Routine.Routine_Addr, Target_Routine.Conv_Desc);
               --  NOTE: To_Non_Op_Map_Type_Desc is an unchecked conversion
               --        and we are relying on no discriminant check being
               --        performed on the result of the conversion.
         else
            --  Presume routine is optional.
            if Debug_Calls then
               Put_Line (" Missing compiled code for routine " &
                 Strings.To_String (Target_Routine.Name));
               Put_Line (" Presuming is optional.");
            end if;
         end if;

         --  Restore Cur_State context pointer
         Cur_State.Context := Context'Unchecked_Access;

         if Debug_Calls then
            Put_Line
              (" returning from " &
               Strings.To_String (Target_Routine.Name) &
               ", imported routine #" &
               Routine_Index'Image (Target_Routine.Index));
            Put_Line
              (" (param area, 0) =" &
               Hex_Image (Fetch_Word (New_Context, (Param_Area, 0,
                                                    No_VM_Obj_Id))));
         end if;

         Thread_Was_Queued := False;
      else
         --  TBD: Check the internal precondition
         --      if not doing static checking.

         --  Recurse to execute the operation
         Execute
           (Instructions => Target_Routine,
            Start_Pc => Code_Index'First,
            Context => New_Context,
            Thread_Was_Queued => Thread_Was_Queued,
            Server_Index => Context.Server_Index);

      end if;

      pragma Assert (not Thread_Was_Queued);

      if Target_Routine.Is_PSVM_Routine then
         --  Release local region allocated for call
         Release_Stg_Rgn (New_Context.Local_Stg_Rgn);
      end if;

      --  TBD: If we allocated a new chunk for callee's locals,
      --      we need to reclaim it or at least not lose track of it.
      if Polymorphic_Output_Type /= null then
         --  Wrap the output(s).
         --  TBD: Handle multiple outputs
         declare
            Output_Addr : constant Word_Ptr :=
              Params;  --  TBD: Handle multiple outputs
            Stg_Rgn_For_Creation : constant Stg_Rgn_Ptr :=
              Stg_Rgn_Of_Large_Obj (Original_Output);
         begin
            Create_Polymorphic_Obj (Context,
              Stg_Rgn_For_Creation, Output_Addr, Polymorphic_Output_Type);

            if Debug_Calls then
               Put_Line
                 ("  now adding polymorphic wrapper to output of type " &
                    Type_Desc_Name_And_Num
                      (Polymorphic_Output_Type,
                       Use_Short_Form => True));
               Put_Line ("  wrapped output:");
               Dump_Obj_With_Type
                 (Content_Of_Physical_Address (Output_Addr),
                  Polymorphic_Output_Type);
            end if;
         end;
      end if;
   end Execute_Call_Op;

   ---------------------------------------
   ---------------------------------------
   -- Execute_Compiled_Indirect_Call_Op --
   ---------------------------------------

   procedure Execute_Compiled_Indirect_Call_Op
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Op_Desc_Virt_Addr  : Object_Virtual_Address;
      Code_Address       : Routine_Code_Address;
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type) is
   --  Execute an indirect call on an operation in compiled code.
   --  Static link must refer to a type descriptor, not a local area.
   begin
      --  Should not be used if Code_Address not null, unless
      --  we have an operation descriptor.
      pragma Assert (Code_Address = null
        or else Op_Desc_Virt_Addr /= Null_Virtual_Address);

      Execute_Compiled_Indirect_Call_Op_With_Conv
        (Context, Params, Static_Link, Static_Link_Base, Static_Link_Offset,
         Target_Base, Target_Offset, Op_Desc_Virt_Addr, Code_Address,
         Info_As_Byte, Conv_Desc => Null_Conv_Desc);

   end Execute_Compiled_Indirect_Call_Op;

   -------------------------------------------------
   -- Execute_Compiled_Indirect_Call_Op_With_Conv --
   -------------------------------------------------

   procedure Execute_Compiled_Indirect_Call_Op_With_Conv
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Op_Desc_Virt_Addr  : Object_Virtual_Address;
      Code_Address       : Routine_Code_Address;
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type;
      Conv_Desc          : Convention_Descriptor) is
   --  Execute an indirect call on an operation in compiled code.
   --  Static link must refer to a type descriptor, not a local area.
   --  Includes convention descriptor to determine calling convention.

      Locked_Param_Info  : constant Locked_Param_Info_Type :=
        Locked_Param_Info_From_Byte (Info_As_Byte);
      Null_Routine_Cpy : aliased Routine := Null_Routine.all;
      --  This Unchecked_Access is OK because it's only passed to
      --  Find_Routine_Context which doesn't store it.
      Target_Routine_Ctx : Routine_Context :=
         (Null_Routine_Cpy'Unchecked_Access, null, null);
      Compiled_Routine : Routine_Ptr;
      Pass_Static_Link : Word_Ptr;
   begin
      if Code_Address = null
        or else Op_Desc_Virt_Addr /= Null_Virtual_Address
      then
         Find_Routine_Context
           (Context,
            Routine_Locator => (Target_Base, Target_Offset, No_VM_Obj_Id),
            Static_Link =>
              (Static_Link_Base, Static_Link_Offset, No_VM_Obj_Id),
            Params => Params,
            SL_Addr => To_Word_Ptr (Static_Link),
            Op_Desc_Virt_Addr => Op_Desc_Virt_Addr,
            Result => Target_Routine_Ctx);
         Compiled_Routine := Routine_Ptr (Target_Routine_Ctx.Code);
         Pass_Static_Link := Target_Routine_Ctx.Static_Link;
      else
         Compiled_Routine := new Routine'(Is_PSVM_Routine => False,
            --  Set Routine_Addr
            Routine_Addr        => Code_Address,
            Is_Compiled_Routine => True,
            Is_Nested_Block     => False,
            Internal_Precond_Addr => null,
               --  Queued calls are always parallel calls
            --  Set all others to null or null-like values
            Index                 => 0,
            Name                  => Null_Str,
            Num_Prior_Homonyms    => 0,
            Name_With_Overloading_Index => Null_Str,
            Full_Module_Name      => Strings.Null_U_String,
            Uses_Queuing          => False,
            Local_Area_Length     => 0,
            Start_Callee_Locals   => 0,
            Boundary_Conditions   => (others => Null_Code_Block_Descriptor),
            Parameters            => null,
            Nesting_Level         => 0,
            Convention            => Convention (Conv_Desc),
            Conv_Desc             => Conv_Desc,
            Built_In_Desig        => Strings.Null_U_String);
         Pass_Static_Link := To_Word_Ptr (Static_Link);
      end if;

      --  Pass the buck to shared routine
      Execute_Call_Op (Context,
         Params => Params,
         Static_Link => Pass_Static_Link,
         Target_Routine => Compiled_Routine,
         Locked_Param_Info => Locked_Param_Info,
         Polymorphic_Output_Type =>
           Target_Routine_Ctx.Polymorphic_Output_Type);
   end Execute_Compiled_Indirect_Call_Op_With_Conv;

   -------------------------------------
   -- Execute_Compiled_Nth_Op_Of_Type --
   -------------------------------------

   procedure Execute_Compiled_Nth_Op_Of_Type
     (Context            : in out Exec_Context;
      Params             : Word_Ptr;
      Static_Link        : Type_Descriptor_Ptr;
      Target_Base        : Area_Base_Indicator;
      Op_Index           : Operation_Index) is
   --  Execute the nth operation of the type identified by the Static link.
   --  Static link must refer to a type descriptor (or op map).

      pragma Assert (Target_Base = Type_Area);  --  TBD: Handle enclosing_type
      pragma Assert (Op_Index /= Optional_Operation_Index);

      Callee_Type_Area_Or_Map : constant Type_Descriptor_Ptr := Static_Link;
         --  TBD: Handle case of Enclosing_Type

   begin
      if not Callee_Type_Area_Or_Map.Has_Op_Map
        and then not Callee_Type_Area_Or_Map.Is_Polymorphic
      then
         --  See if can use fast path
         declare
            Info : Routine_Info renames
              Callee_Type_Area_Or_Map.Operations (Op_Index);
            Code : constant Routine_Ptr := Nth_Routine (Info.Index);
         begin
            if not Code.Is_PSVM_Routine
              and then Info.Action = No_Action
            then
               --  Fast path
               if Info.Use_Static_Link_For_Type then
                  Call_Compiled_Routine (Context, Params, Static_Link,
                                         Code.Routine_Addr, Code.Conv_Desc);
               else
                  Call_Compiled_Routine
                    (Context, Params, Info.Type_Desc,
                     Code.Routine_Addr, Code.Conv_Desc);
               end if;
               return;
            end if;
         end;
      end if;

      --  Fall back to slow path
      declare
         Null_Routine_Cpy : aliased Routine := Null_Routine.all;
         --  This Unchecked_Access is OK because it's only passed to
         --  Find_Routine_Context which doesn't store it.
         Target_Routine_Ctx : Routine_Context :=
            (Null_Routine_Cpy'Unchecked_Access, null, null);
         Compiled_Routine : Routine_Ptr;
         Pass_Static_Link : Word_Ptr;
      begin
         Find_Routine_Context
           (Context,
            Routine_Locator =>
              (Target_Base, Type_Operation_Offsets'First +
                               Offset_Within_Area (Op_Index), No_VM_Obj_Id),
            Static_Link => Null_Object_Locator,
            Params => Params,
            SL_Addr => To_Word_Ptr (Static_Link),
            Op_Desc_Virt_Addr => 0,
            Result => Target_Routine_Ctx);

         Compiled_Routine := Routine_Ptr (Target_Routine_Ctx.Code);
         Pass_Static_Link := Target_Routine_Ctx.Static_Link;

         --  Pass the buck to shared routine
         Execute_Call_Op (Context,
            Params => Params,
            Static_Link => Pass_Static_Link,
            Target_Routine => Compiled_Routine,
            Locked_Param_Info => Null_Locked_Param_Info,
            Polymorphic_Output_Type =>
              Target_Routine_Ctx.Polymorphic_Output_Type);
      end;
   end Execute_Compiled_Nth_Op_Of_Type;

   ---------------------------------------
   -- Execute_Compiled_Parallel_Call_Op --
   ---------------------------------------

   procedure Execute_Compiled_Parallel_Call_Op (Context : in out Exec_Context;
      Master_Address           : Word_Ptr;
      New_Tcb                  : Word_Ptr;       -- Parallel_Control
      Code_Address             : Routine_Code_Address;  -- Parallel_Call_Target
      Static_Link              : Word_Ptr;  -- Parallel_Static_Link
      Internal_Precond_Address : Nested_Blk_Address;
      Tcb_Is_Local             : Boolean;   -- Whether tcb addr in Local_Area
      Is_Start_Op              : Boolean;   -- Whether is Start_Par vs. Add_Par
      Info_As_Byte             : Locked_Param_Info_As_Byte_Type) is
   --  Execute a parallel call on an operation in compiled code.
   --  TBD: This doesn't properly set the static link for inherited routines
   --       or when calling through an interface where an "op-map" would be
   --       needed.
      --  NOTE: This is for backward-compatibility only, when we didn't
      --        support multiple calling conventions.
      --        This should not be called from new code.
   begin
      Execute_Compiled_Parallel_Call_Op_With_Conv
        (Context, Master_Address, New_Tcb, Code_Address, Static_Link,
         Internal_Precond_Address, Tcb_Is_Local, Is_Start_Op, Info_As_Byte,
         Conv_Desc => Nested_Block_Conv_Desc);
   end Execute_Compiled_Parallel_Call_Op;

   -------------------------------------------------
   -- Execute_Compiled_Parallel_Call_Op_With_Conv --
   -------------------------------------------------

   procedure Execute_Compiled_Parallel_Call_Op_With_Conv
     (Context                  : in out Exec_Context;
      Master_Address           : Word_Ptr;
      New_Tcb                  : Word_Ptr;       -- Parallel_Control
      Code_Address             : Routine_Code_Address; -- Parallel_Call_Target
      Static_Link              : Word_Ptr;  -- Parallel_Static_Link
      Internal_Precond_Address : Nested_Blk_Address;
      Tcb_Is_Local             : Boolean;   -- Whether tcb addr in Local_Area
      Is_Start_Op              : Boolean;   -- Whether is Start_Par vs. Add_Par
      Info_As_Byte             : Locked_Param_Info_As_Byte_Type;
      Conv_Desc                : Convention_Descriptor) is
   --  Execute a parallel call on an operation in compiled code
   --  with convention descriptor.
   --  TBD: This doesn't properly set the static link for inherited routines
   --       or when calling through an interface where an "op-map" would be
   --       needed.
      Locked_Param_Info  : constant Locked_Param_Info_Type :=
        Locked_Param_Info_From_Byte (Info_As_Byte);
      Compiled_Routine : aliased Routine (Is_PSVM_Routine => False);
         --  Create a temporary compiled routine
   begin
      Compiled_Routine.Conv_Desc := Conv_Desc;
      Compiled_Routine.Convention := Convention (Conv_Desc);
      Compiled_Routine.Routine_Addr := Code_Address;
      Compiled_Routine.Internal_Precond_Addr := Internal_Precond_Address;
      Compiled_Routine.Is_Compiled_Routine := True;
      Compiled_Routine.Is_Nested_Block := False;

      Spawn_Parallel_Thread (Context,
        Master_Addr  => Master_Address,
        New_Tcb      => New_Tcb,
        Static_Link  => Static_Link,
        Tcb_Is_Local => Tcb_Is_Local,
        Is_Start_Op  => Is_Start_Op,
        Routine      => Compiled_Routine'Unchecked_Access,
        Locked_Param_Info => Locked_Param_Info);

   end Execute_Compiled_Parallel_Call_Op_With_Conv;

   ------------------------------------------------
   -- Execute_Compiled_Indirect_Parallel_Call_Op --
   ------------------------------------------------

   procedure Execute_Compiled_Indirect_Parallel_Call_Op (
      Context            : in out Exec_Context;
      Master_Address     : Word_Ptr;
      New_Tcb            : Word_Ptr;       -- Parallel_Control
      Target_Base        : Area_Base_Indicator;
      Target_Offset      : Offset_Within_Area;
      Static_Link        : Type_Descriptor_Ptr;  -- Parallel_Static_Link
      Static_Link_Base   : Area_Base_Indicator;
      Static_Link_Offset : Offset_Within_Area;
      Tcb_Is_Local       : Boolean;  -- Whether tcb address is in Local_Area
      Is_Start_Op        : Boolean;  -- Whether is Start_Par vs. Add_Par
      Info_As_Byte       : Locked_Param_Info_As_Byte_Type) is
   --  Execute an indirect parallel call on an operation in compiled code.
   --  Static link must refer to a type descriptor, not a local area.
   --  TBD: This doesn't properly set the static link for inherited routines
   --       or when calling through an interface where an "op-map" would be
   --       needed.
      Locked_Param_Info  : constant Locked_Param_Info_Type :=
        Locked_Param_Info_From_Byte (Info_As_Byte);
      Null_Routine_Cpy : aliased Routine := Null_Routine.all;
      --  This Unchecked_Access is OK because it's only passed to
      --  Find_Routine_Context which doesn't store it.
      Target_Routine_Ctx : Routine_Context :=
         (Null_Routine_Cpy'Unchecked_Access, null, null);
   begin
      Find_Routine_Context
        (Context,
         Routine_Locator => (Target_Base, Target_Offset, No_VM_Obj_Id),
         Static_Link => (Static_Link_Base, Static_Link_Offset, No_VM_Obj_Id),
         Params => Add (New_Tcb, Tcb_Param_List_Offset),
         SL_Addr => To_Word_Ptr (Static_Link),
         Result => Target_Routine_Ctx);

      Spawn_Parallel_Thread (Context,
        Master_Addr  => Master_Address,
        New_Tcb      => New_Tcb,
        Static_Link  => Target_Routine_Ctx.Static_Link,
        Tcb_Is_Local => Tcb_Is_Local,
        Is_Start_Op  => Is_Start_Op,
        Routine      => Routine_Ptr (Target_Routine_Ctx.Code),
        Locked_Param_Info => Locked_Param_Info);

   end Execute_Compiled_Indirect_Parallel_Call_Op;

   ----------------------------------
   -- Execute_Compiled_Parallel_Op --
   ----------------------------------

   procedure Execute_Compiled_Parallel_Op (Context : in out Exec_Context;
      Master_Address : Word_Ptr;
      New_Tcb        : Word_Ptr;       -- Parallel_Control
      Code_Address   : Nested_Blk_Address;  -- Parallel_Code_Block
      Static_Link    : Word_Ptr;  -- Parallel_Static_Link
      Uses_Queuing   : Boolean;   -- Whether nested blk uses queuing
      Tcb_Is_Local   : Boolean;   -- Whether tcb address is in Local_Area
      Is_Start_Op    : Boolean)   -- Whether is Start_Par vs. Add_Par
   is
   --  Execute a call on a parallel nested block in compiled code.
      Compiled_Routine : aliased Routine (Is_PSVM_Routine => False);
         --  Create a temporary compiled routine
   begin
      Compiled_Routine.Routine_Addr  := To_Routine_Code_Address (Code_Address);
      Compiled_Routine.Is_Compiled_Routine := True;
      Compiled_Routine.Is_Nested_Block := True;
      Compiled_Routine.Uses_Queuing := Uses_Queuing;

      Compiled_Routine.Conv_Desc := Nested_Block_Conv_Desc;

      Spawn_Parallel_Thread (Context,
        Master_Addr  => Master_Address,
        New_Tcb      => New_Tcb,
        Static_Link  => Static_Link,
        Tcb_Is_Local => Tcb_Is_Local,
        Is_Start_Op  => Is_Start_Op,
        Routine      => Compiled_Routine'Unchecked_Access);

   end Execute_Compiled_Parallel_Op;

   ----------------------------------
   -- Execute_Wait_For_Parallel_Op --
   ----------------------------------

   function Execute_Wait_For_Parallel_Op
     (Context      : Exec_Context_RW_Ptr;
      Master_Addr  : Word_Ptr) return Nested_Block_Outcome_As_Int is
   --  Execute the Wait_For_Parallel_Op instructions
   --  This version returns the Master_Outcome, encoded in a Word
   --  < 0 means return from enclosing non-nested-block routine;
   --  >= 0 means the level diff (minus 1), where
   --      = 0 means it was a normal exit;
   --      > 0 means that further exits are necessary.
      --  Master should be the current "open" master.
      pragma Assert (Master_Addr = Context.Open_Master);
      function Export_NBO is new Ada.Unchecked_Conversion
            (Nested_Block_Outcome, Nested_Block_Outcome_As_Int);
   begin
      --  Pass the buck to common routine
      Wait_For_Open_Master (Context.all);
      declare
         Outcome : constant Master_Outcome_Enum :=
           Master_Outcome (Master_Addr);
      begin
         --  deal with outcome of the "wait"
         case Outcome is
            when Normal_Outcome =>
               --  Check for exit-requested flag (TBD: Is this right?)
               if Tcb_Exit_Requested (Context.Control_Area) then
                  return Export_NBO ((Level => 1, Skip => 0));
                  --  TBD: Should keep returning until reach tcb
                  --       where this flag is off.
               end if;
               return Export_NBO ((Level => 0, Skip => 0));
            when Return_From_Operation_Outcome =>
               --  Keep returning until we return from operation.
               return Nested_Block_Return_Outcome;
            when Exit_Outcome =>
               --  Keep exiting if necessary
               declare
                  Skip_Count : constant Code_Offset :=
                    Exit_Skip_Count (Master_Addr);
                  Level_Diff : constant Natural :=
                    Exit_Level_Diff (Master_Addr);
               begin
                  if Level_Diff > 2 or else Skip_Count > 0 then
                     --  Update outer master with one lower Level_Diff
                     Set_Enclosing_Master_Outcome
                       (Context.all,
                        Outcome         => Exit_Outcome,
                        Exit_Level_Diff => Level_Diff - 1,
                        Exit_Skip_Count => Skip_Count);
                     return Export_NBO ((Level_Diff - 1, Skip_Count));
                  end if;

                  return Export_NBO ((Level => 0, Skip => Skip_Count));
               end;
         end case;
      end;
   end Execute_Wait_For_Parallel_Op;

   ----------------------------
   -- Create_Polymorphic_Obj --
   ----------------------------

   procedure Create_Polymorphic_Obj (Context : in out Exec_Context;
      Stg_Rgn_For_Creation : Stg_Rgn_Ptr;
      Dest_Addr : Word_Ptr;
      Poly_Type_Desc : Non_Op_Map_Type_Ptr) is

      --  Execute the Create_Polymorphic_Obj instruction
      --  Fields of Instruction are:
      --  Destination : Object_Locator;
      --  Source : Object_Locator;
      --  Type_Info : Object_Locator;

      Underlying_Value : constant Word_Type :=
        Content_Of_Physical_Address (Dest_Addr);
      Underlying_Type : constant Type_Descriptor_Ptr :=
        Poly_Type_Desc.Components (1).Type_Desc;

      Poly_Obj_Size : constant Offset_Within_Area :=
        Large_Obj_Header_Size + 1;
      Poly_Obj : Word_Type;

      use Debug;
   begin
      if Is_Null_Value (Underlying_Value, Underlying_Type) then
         --  NOTE: We need for a null underlying value to appear as a
         --        null polymorphic object, because we rely on this
         --        in a polymorphic case statement, where null coming
         --        back from Unwrap_Polymorphic_Obj implies
         --        a type mismatch.
         --        We make the object as a whole null to simplify the
         --        Is_Null_Value test, which otherwise, would have to
         --        recurse on polymorphic objects.
         --  Create large null value for correct region
         Poly_Obj := Null_For_Stg_Rgn (Stg_Rgn_For_Creation);
      else
         --  Create non-null polymorphic value
         Poly_Obj :=
           Allocate_From_Stg_Rgn (Stg_Rgn_For_Creation, Poly_Obj_Size,
             Context.Server_Index);

         --  Fill in header
         Set_Large_Obj_Header
           (Poly_Obj,
            Poly_Obj_Size,
            Stg_Rgn_For_Creation.Index,
            Poly_Type_Desc.Index);

         --  Initialize only "component" to underlying value
         Store_Word (Poly_Obj + Large_Obj_Header_Size, Underlying_Value);
      end if;

      --  Replace original value with Poly_Obj
      Store_Word (Dest_Addr, 0, Poly_Obj);

      if Debug_Calls then
         Put_Line (" Created new polymorphic obj:");
         Dump_Obj_With_Indent (Poly_Obj, Poly_Type_Desc, Indent => 2);
      end if;
   end Create_Polymorphic_Obj;

   ------------------------------
   -- Create_Poly_Obj_Exported --
   ------------------------------

   procedure Create_Poly_Obj_Exported (Context : in out Exec_Context;
      Existing_Obj : Word_Type;
      Dest_Addr : Word_Ptr;
      Poly_Type_Desc : Non_Op_Map_Type_Ptr) is

      --  Execute the Create_Polymorphic_Obj instruction
      --  Fields of Instruction are:
      --  Destination : Object_Locator;
      --  Source : Object_Locator;
      --  Type_Info : Object_Locator;

      Stg_Rgn_For_Creation : Stg_Rgn_Ptr;
   begin
      --  Get storage region determined by existing obj, if any
      if Existing_Obj = 0 then
         Stg_Rgn_For_Creation := Local_Stg_Rgn (Context);
      else
         Stg_Rgn_For_Creation := Stg_Rgn_Of_Large_Obj (Existing_Obj);
      end if;
      --  Pass the buck to routine that takes storage region
      Create_Polymorphic_Obj
        (Context, Stg_Rgn_For_Creation, Dest_Addr, Poly_Type_Desc);
   end Create_Poly_Obj_Exported;

   -------------------------
   -- Enum_Word_To_String --
   -------------------------

   function Enum_Word_To_String (Word : Word_Type) return String is
   --  Convert ParaSail Univ_Enumeration to Ada String.
   begin
      if Word = Null_Value then
         return "null";
      else
         return Strings.To_String
           (Strings.To_U_String (Strings.U_String_Index (Word)));
      end if;
   end Enum_Word_To_String;

   --------------------------
   -- Execution_Source_Pos --
   --------------------------

   function Execution_Source_Pos
     (Server_Index : Thread_Server_Index := Current_Server_Index)
      return Source_Positions.Source_Position
   is
      Cur_State : Server_State renames
        Server_Info_Array (Server_Index).Current_State;
      use type Source_Positions.Line_Number;
   begin
      if Cur_State.Pc /= 0
        and then Cur_State.Code /= null
        and then Cur_State.Code.Is_PSVM_Routine = True
        and then Cur_State.Code.Code /= null
        and then Cur_State.Code.Code.Instrs
                    (Cur_State.Pc).Source_Pos.Line /= 0
      then
         --  Return source pos of current instruction
         return Cur_State.Code.Code.Instrs (Cur_State.Pc).Source_Pos;
      else
         --  Use last saved source position
         return Cur_State.Src_Pos;
      end if;
   end Execution_Source_Pos;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  Exit the ParaSail program
   begin
      Shut_Down_Thread_Servers (Total_Errors => 0);
      --  TBD: Abort the environment task
   end Exit_Program;

   --  Operations for extracting/inserting Object_Locator from/into ParaSail
   --  Representation in ParaSail is two integers, one is Base/Offset
   --  and the other is the "extra" LLVM information such as VM_Name,
   --  VM_Param_Name, and VM_Is_Indir.

   -----------------------
   -- Extract_VM_Obj_Id --
   -----------------------

   function Extract_VM_Obj_Id
     (Base : Area_Base_Indicator;
      Offset : Offset_Within_Area;
      VM_Info : Word_Type) return VM_Obj_Id_Type is
      --  Extract VM_Obj_Id from ParaSail representation for second word
      --  of ParaSail two-slot representation.
   begin
      case Base is
         when Zero_Base | Param_Area | Type_Area | Const_Area |
           Enclosing_Param_Areas | Enclosing_Type_Areas =>
            --  Should not be an extra VM info for these areas
            pragma Assert (VM_Info = 0);
            return No_VM_Obj_Id;

         when Local_Area | Enclosing_Local_Areas |
              Base_Registers | Phys_Base_Registers =>

            if VM_Info >= 0 then
               return VM_Obj_Id_Type'(Kind => Local_Kind,
                 Is_Var => (VM_Info mod 2**32 >= 2**31),
                 Level => 0,  --  TBD
                 Indir => Boolean'Pos (VM_Info mod 2**32 >= 2**31),
                 Num => VM_Obj_Unique_Num (VM_Info mod 2**31),
                 First_Call_Param_Num => VM_Obj_Unique_Num (VM_Info / 2**32));
            else
               --  Sign bit on indicates Component_Kind
               return VM_Obj_Id_Type'(Kind => Component_Kind,
                 Is_Var => (VM_Info mod 2**32 >= 2**31),
                 Level => 0,  --  TBD
                 Indir => Boolean'Pos (VM_Info mod 2**32 >= 2**31),
                 Num => VM_Obj_Unique_Num (VM_Info mod 2**31),
                 Offset =>
                   Offset_Within_Area
                     ((VM_Info + Word_Type'(2**62) + Word_Type'(2**62)) /
                        2**32));
            end if;

         when others =>
            pragma Assert (False);  --  Should not occur
            return No_VM_Obj_Id;

      end case;
   end Extract_VM_Obj_Id;

   ----------------------------
   -- Extract_Object_Locator --
   ----------------------------

   function Extract_Object_Locator (Locator_PS : Word_Type)
     return Object_Locator is
      --  Extract an Object_Locator from the ParaSail 2-slot representation.
      Locator_PS_Ptr : constant Word_Ptr :=
        Virtual_To_Physical_Address (Locator_PS);
      Locator_Base_Offset : constant Word_Type :=
        Fetch_Word (Locator_PS_Ptr, Large_Obj_Header_Size);
      Locator_Offset : constant Offset_Within_Area :=
         Offset_Within_Area (Locator_Base_Offset mod 2**16);
      Locator_Base : constant Area_Base_Indicator :=
        Area_Base_Indicator (Locator_Base_Offset / 2**32);
      Locator_VM_Info : constant Word_Type :=
        Fetch_Word (Locator_PS_Ptr, Large_Obj_Header_Size +
                                      Offset_Within_Area'(1));
   begin
      return Object_Locator'
        (Base => Locator_Base,
         Offset => Locator_Offset,
         VM_Obj_Id => Extract_VM_Obj_Id
                        (Locator_Base, Locator_Offset, Locator_VM_Info));
   end Extract_Object_Locator;

   --------------------
   -- Init_Large_Obj --
   --------------------

   procedure Init_Large_Obj
     (Type_Desc    : Non_Op_Map_Type_Ptr;
      Stg_Rgn      : Stg_Rgn_Ptr;
      New_Obj      : Word_Type)
   --  Initialize large (non-array) object in given region.
   --  Initialize all subobjects to appropriate kind of null
   is
      pragma Assert (not Is_Small (Type_Desc));
      pragma Assert (Type_Desc.Type_Kind /= Basic_Array_Kind);

      New_Obj_Size : constant Offset_Within_Area :=
                       Offset_Within_Area (Type_Desc.Num_Components) +
                       Large_Obj_Header_Size;
   begin
      --  Wrapper types should never show up on an actual object
      pragma Assert (not Type_Desc.Is_Wrapper);

      --  Fill in header
      Set_Large_Obj_Header
        (New_Obj,
         New_Obj_Size,
         Stg_Rgn.Index,
         Type_Desc.Index,
         On_Stack => True);

      --  Initialize components to null
      for I in 1 .. Type_Desc.Num_Components loop
         declare
            Comp_Type : constant Type_Descriptor_Ptr :=
              Type_Desc.Components (I).Type_Desc;
         begin
            --  Assign an appropriate null into component
            Store_Word
              (New_Obj + Large_Obj_Header_Size + Offset_Within_Area (I - 1),
               Null_For_Type_Or_Stg_Rgn
                 (Comp_Type,
                  Stg_Rgn,
                  Is_By_Ref => Type_Desc.Components (I).Is_By_Ref));
         end;
      end loop;

      if New_Obj < Lowest_Stack_Addr then
         --  NOTE: Race condition
         Lowest_Stack_Addr := New_Obj;
      end if;

      if New_Obj > Highest_Stack_Addr then
         --  NOTE: Race condition
         Highest_Stack_Addr := New_Obj;
      end if;

      Check_Is_Large (New_Obj);
      pragma Assert (Large_Obj_Lock_Obj (New_Obj) = 0);
   end Init_Large_Obj;

   -----------------------------
   -- Init_Large_Obj_Exported --
   -----------------------------

   procedure Init_Large_Obj_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      New_Obj      : Word_Type) is
   --  Init new large object in current region.
      Type_Desc  : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
      pragma Assert (not Is_Small (Type_Desc));
   begin
      Init_Large_Obj (Type_Desc, Local_Stg_Rgn (Context), New_Obj);
   end Init_Large_Obj_Exported;

   ----------------------
   -- Insert_VM_Obj_Id --
   ----------------------

   function Insert_VM_Obj_Id (Locator : Object_Locator) return Word_Type is
      --  Create ParaSail representation for Locator.VM_Obj_Id
   begin
      if Locator.VM_Obj_Id.Kind = No_VM_Obj_Kind then
         return 0;
      end if;

      case Locator.Base is
         when Zero_Base | Param_Area | Type_Area | Const_Area |
           Enclosing_Param_Areas | Enclosing_Type_Areas =>
            --  Do not need any extra VM info
            return 0;

         when Local_Area | Enclosing_Local_Areas |
              Base_Registers | Phys_Base_Registers =>
            if Locator.VM_Obj_Id.Kind = Local_Kind then
               return Boolean'Pos (Locator.VM_Obj_Id.Is_Var) * 2**31 +
                      Word_Type (Locator.VM_Obj_Id.Num) +
                      Word_Type (Locator.VM_Obj_Id.First_Call_Param_Num) *
                        2**32;
            else
               pragma Assert (Locator.VM_Obj_Id.Kind = Component_Kind);
               pragma Assert (Locator.VM_Obj_Id.Offset > 0);

               --  Is_Var indicates whether an extra level of indirection
               --  is required *after* applying the offset to the base reg.
               --  TBD: Not sure that ever happens!
               --  NOTE: For a Component_Kind associated with a Local_Area,
               --        the offset is (probably) the offset into a TCB
               --        that is living on the stack.
               --  We indicate this is a Component_Kind node by setting
               --  the high bit, which we do by subtracting 2**63.
               return Boolean'Pos (Locator.VM_Obj_Id.Is_Var) * 2**31 +
                      Word_Type (Locator.VM_Obj_Id.Num) +
                      Word_Type (Locator.VM_Obj_Id.Offset) *
                        2**32 -
                      2**62 - 2**62;
            end if;

         when others =>
            pragma Assert (False);  --  Should not occur
            return 0;

      end case;
   end Insert_VM_Obj_Id;

   ---------------------------
   -- Insert_Object_Locator --
   ---------------------------

   function Insert_Object_Locator
     (Locator : Object_Locator;
      Obj_Locator_Type : Non_Op_Map_Type_Ptr;
      Target : Word_Type;
      Server_Index : Thread_Server_Index)
     return Word_Type is
      --  Convert object_locator to two-slot ParaSail representation
      --  given obj-locator type, target object, and server index.
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);
      Result : constant Word_Type :=
                Create_Large_Obj   --  Create the Object_Locator object
                  (Type_Desc    => Obj_Locator_Type,
                   Stg_Rgn      => Target_Stg_Rgn,
                   Server_Index => Server_Index);
   begin
      --  Set the Base/Offset word
      Store_Word
        (Result + Large_Obj_Header_Size,
         Word_Type (Locator.Base) * 2**32 + Word_Type (Locator.Offset));

      --  Set the VM_Obj_Id word
      Store_Word (Result +
        Large_Obj_Header_Size + Offset_Within_Area'(1),
        Insert_VM_Obj_Id (Locator));

      --  Return the result
      return Result;
   end Insert_Object_Locator;

   ----------------
   -- Fetch_Real --
   ----------------

   function Fetch_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator) return Univ_Real is
   begin
      return To_Univ_Real (Fetch_Word (Context, Real_Locator));
   end Fetch_Real;

   function Fetch_Real (Virtual : Object_Virtual_Address) return Univ_Real is
   begin
      return To_Univ_Real (Content_Of_Virtual_Address (Virtual));
   end Fetch_Real;

   function Fetch_Real
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Univ_Real is
   --  Fetch real at Base + Offset
   begin
      return To_Univ_Real (Fetch_Word (Base, Offset));
   end Fetch_Real;

   ------------------------
   -- Fetch_Nonnull_Real --
   ------------------------

   function Fetch_Nonnull_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator) return Univ_Real
   is
      Result : constant Word_Type := Fetch_Word (Context, Real_Locator);
   begin
      if Result = Null_Float_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return To_Univ_Real (Result);
   end Fetch_Nonnull_Real;

   function Fetch_Nonnull_Real
     (Virtual : Object_Virtual_Address) return Univ_Real
   is
      Result : constant Word_Type := Content_Of_Virtual_Address (Virtual);
   begin
      if Result = Null_Float_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return To_Univ_Real (Result);
   end Fetch_Nonnull_Real;

   function Fetch_Nonnull_Real
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Univ_Real is
   --  Fetch real at Base + Offset; complain if null.
      Result : constant Word_Type := Fetch_Word (Base, Offset);
   begin
      if Result = Null_Float_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return To_Univ_Real (Result);
   end Fetch_Nonnull_Real;

   ----------------
   -- Fetch_Word --
   ----------------

   function Fetch_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Type is
   begin
      case Word_Locator.Base is
         when Type_Area | Enclosing_Type_Areas =>
            --  Handle type areas specially
            return Nth_Type_Area_Word
                     (Context.Enclosing_Type,
                      Word_Locator.Offset,
                      Type_Base => Word_Locator.Base);

         when Phys_Base_Registers =>
            --  These require special handling as they contain a physical
            --  address rather than a virtual address.
            return Content_Of_Physical_Address (Fetch_Word_Ptr (Context,
                    (Local_Area, Offset_Within_Area
                      (Word_Locator.Base - Phys_Base_Registers'First),
                     No_VM_Obj_Id)));

         when others =>
            return Content_Of_Physical_Address
                     (Locator_To_Physical_Address (Context, Word_Locator));
      end case;
   end Fetch_Word;

   function Fetch_Word
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Type is
   --  Fetch word at Base + Offset

      subtype Word_Array_Max is Word_Array (Offset_Within_Area);
      type Word_Array_Max_Ptr is access all Word_Array_Max;
      pragma No_Strict_Aliasing (Word_Array_Max_Ptr);

      function To_Word_Array is
        new Ada.Unchecked_Conversion (Word_Ptr, Word_Array_Max_Ptr);
   begin
      return To_Word_Array (Base)(Offset);
   end Fetch_Word;

   function Fetch_Word
     (Base : Object_Virtual_Address; Offset : Offset_Within_Area)
     return Word_Type is
   --  Fetch word at Base + Offset
   begin
      return Fetch_Word (Virtual_To_Physical_Address (Base), Offset);
   end Fetch_Word;

   ------------------------
   -- Fetch_Nonnull_Word --
   ------------------------

   function Fetch_Nonnull_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Type
   is
      Val : constant Word_Type := Fetch_Word (Context, Word_Locator);
   begin
      if Val = Null_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return Val;
   end Fetch_Nonnull_Word;

   function Fetch_Nonnull_Word
     (Virtual : Object_Virtual_Address) return Word_Type
   is
      Val : constant Word_Type := Fetch_Word (Virtual);
   begin
      if Val = Null_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return Val;
   end Fetch_Nonnull_Word;

   function Fetch_Nonnull_Word
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Type is
   --  Fetch word at Base + Offset; complain if null.
      Val : constant Word_Type := Fetch_Word (Base, Offset);
   begin
      if Val = Null_Value then
         Messages.Put_RT_Error
           ("Null value not allowed here",
            Src_Pos => Execution_Source_Pos);
         raise Program_Error;
      end if;

      return Val;
   end Fetch_Nonnull_Word;

   --------------------
   -- Fetch_Word_Ptr --
   --------------------

   function Fetch_Word_Ptr
     (Context      : Exec_Context;
      Word_Locator : Object_Locator) return Word_Ptr is
   begin
      return Word_To_Word_Ptr (Fetch_Word (Context, Word_Locator));
   end Fetch_Word_Ptr;

   function Fetch_Word_Ptr
     (Base : Word_Ptr; Offset : Offset_Within_Area) return Word_Ptr is
   --  Fetch word pointer at Base + Offset
   begin
      return Word_To_Word_Ptr (Add (Base, Offset).all);
   end Fetch_Word_Ptr;

   ----------------------
   -- Free_Initial_Tcb --
   ----------------------

   procedure Free_Initial_Tcb
     (Initial_Tcb  : Word_Ptr;
      Server_Index : Thread_Server_Index) is
   begin
      --  All we need to do is release the master index and "extra" info
      Release_Master
        (Index_Of_Master (Tcb_Master_Ptr (Initial_Tcb)), Server_Index);
   end Free_Initial_Tcb;

   ----------------------------
   -- Get_New_Local_Stg_Rgn --
   ----------------------------

   function Get_New_Local_Stg_Rgn
     (New_Local_Area     : Word_Ptr;
      Server_Index       : Thread_Server_Index) return Stg_Rgn_Ptr
   is
      Info        : Server_Info renames Server_Info_Array (Server_Index);
      New_Stg_Rgn : Stg_Rgn_Ptr := Info.Free_Stg_Rgns;
   begin
      if New_Stg_Rgn = null then
         --  Need to allocate a new region
         Stg_Rgn_Manager.Create_Stg_Rgn (New_Stg_Rgn);

         if Debug_Stg_Rgns then
            Put_Line
              (" Server" & Thread_Server_Index'Image (Server_Index) &
               " is allocating new region #" &
               Stg_Rgn_Index'Image (New_Stg_Rgn.Index));
         end if;

         New_Stg_Rgn.Owning_Server := Server_Index;
         New_Stg_Rgn.Shared_Part.Owning_Server := Server_Index;
         New_Stg_Rgn.Shared_Part.Manager := new Stg_Rgn_Manager_Type;

      else
         --  There was a region on the free list;
         --  update the list to skip over this one.
         Info.Free_Stg_Rgns := New_Stg_Rgn.Enclosing_Stg_Rgn;

         if Debug_Stg_Rgns then
            Put_Line
              (" Server" & Thread_Server_Index'Image (Server_Index) &
               " is reusing region #" &
               Stg_Rgn_Index'Image (New_Stg_Rgn.Index));
         end if;

      end if;

      --  Initialize enclosing rgn pointer from innermost stg rgn for server
      --  and set this new region as the new innermost region.
      --  NOTE: We don't use the passed-in Enclosing_Stg_Rgn because
      --        that might be from some other server and we want to
      --        minimize borrowing across servers.
      if Info.Innermost_Stg_Rgn = null then
         --  This is the outermost storage region
         New_Stg_Rgn.Enclosing_Stg_Rgn := null;
         New_Stg_Rgn.Shared_Part.Enclosing_Stg_Rgn := null;
      else
         --  This is not the outermost storage region;
         --  link both the shared and unshared parts to corresponding
         --  enclosing regions.
         New_Stg_Rgn.Enclosing_Stg_Rgn := Info.Innermost_Stg_Rgn;
         New_Stg_Rgn.Shared_Part.Enclosing_Stg_Rgn :=
           Info.Innermost_Stg_Rgn.Shared_Part;
      end if;
      Info.Innermost_Stg_Rgn        := New_Stg_Rgn;

      if Debug_Stg_Rgns then
         if New_Stg_Rgn.Enclosing_Stg_Rgn /= null then
            Put_Line
              (" New region enclosed by region #" &
               Stg_Rgn_Index'Image (New_Stg_Rgn.Enclosing_Stg_Rgn.Index));
         else
            Put_Line
              (" New region has no encloser");
         end if;
      end if;

      if New_Local_Area /= null then
         --  Initialize pointer in new local area to region
         Store_Word (New_Local_Area, Local_Area_Stg_Rgn_Ptr_Offset,
           Word_Type (New_Stg_Rgn.Index));
      end if;

      --  Initialize back pointer to local area
      New_Stg_Rgn.Associated_Local_Area := New_Local_Area;

      return New_Stg_Rgn;
   end Get_New_Local_Stg_Rgn;

   function Get_New_Local_Stg_Rgn
     (New_Local_Area    : Object_Address;
      Server_Index      : Thread_Server_Index) return Stg_Rgn_Ptr
   is
   begin
      --  Just pass the buck to the version that takes a local_area pointer.
      return Get_New_Local_Stg_Rgn
        (Object_To_Physical_Address (New_Local_Area),
         Server_Index);
   end Get_New_Local_Stg_Rgn;

   ------------------------------
   -- Get_Routine_By_Global_Id --
   ------------------------------

   function Get_Routine_By_Global_Id (Global_Id : Global_Operation_Id)
     return Routine_RW_Ptr is
   --  Look up routine in Id_To_Routine_Map.
   --  If not there, create it as a compiled routine, but with a
   --  null Routine_Addr pointer.
      use Id_To_Routine_Maps;
      Index_Ref : constant Element_Ref :=
        Find_Element (Id_To_Routine_Map, Global_Id);
   begin
      if Index_Ref /= null then
         --  Retrieve routine from Routine table
         return Nth_Element (Routine_Table,
                             Routine_Elem_Index (Index_Ref.all));
      else
         --  Need to create a routine
         declare
            New_Routine : Routine_RW_Ptr :=
              new Routine (Is_PSVM_Routine => False);
            Op_U_Str    : constant Strings.U_String :=
              Strings.To_U_String (Global_Id.Operation_Name);
            Op_Str      : constant String := Strings.To_String (Op_U_Str);
         begin
            --  Fill in name/full_module_name and mark as compiled
            New_Routine.Name := Op_U_Str;
               --  NOTE: Num_Prior_Homonyms built into this name already

            New_Routine.Full_Module_Name := Strings.To_U_String
              (Global_Id.Module_Name);

            if Global_Id.Is_Builtin then
               --  This routine is a built-in, so fill in Built_In_Desig too.
               New_Routine.Built_In_Desig := Op_U_Str;

               --  Find built-in in registration table
               New_Routine.Routine_Addr :=
                 Find_Builtin (New_Routine.Built_In_Desig);

               if New_Routine.Routine_Addr = null then
                  Messages.Put_Warning ("Built-in operation " &
                      Strings.To_String (New_Routine.Built_In_Desig) &
                      " not found",
                    Src_Pos => Source_Positions.Null_Source_Position);
               end if;

               New_Routine.Convention := Languages.Convention_External_Default;
               New_Routine.Conv_Desc := External_Default_Conv_Desc;
            else
               --  This is not a built-in, so must be compiled.
               New_Routine.Is_Compiled_Routine := True;
            end if;

            --  Install in routine table
            Install_Code (New_Routine);
            return New_Routine;
         end;
      end if;
   end Get_Routine_By_Global_Id;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link
     (Context   : in out Exec_Context;
      Static_Link_Locator : Object_Locator) return Word_Ptr is
   --  Get pointer to enclosing local area or enclosing type
   begin
      case Static_Link_Locator.Base is
         when Zero_Base
            | Param_Area
            | Enclosing_Param_Areas
            | Type_Area
            | Enclosing_Type_Areas =>
            --  Pass the buck to Get_Type_Desc_Or_Op_Map
            return To_Word_Ptr (Get_Type_Desc_Or_Op_Map
                                  (Context, Static_Link_Locator));

         when Local_Area =>
            --  Relative to current local area
            pragma Assert (Static_Link_Locator.Offset = 0);

            return Context.Local_Area;

         when Enclosing_Local_Areas =>
            --  Relative to a local area of an enclosing block or operation
            --  The first word of a local area points at its enclosing
            --  block, operation, or type area.
            --  Enclosing_Local_Areas'First is equivalent to Local_Area.

            pragma Assert (Static_Link_Locator.Offset = 0);

            --  Pass the buck to Locator_To_Physical_Address
            return Locator_To_Physical_Address
                     (Context, Static_Link_Locator);

         when Const_Area =>
            --  No types or local areas in global constant table
            Messages.Put_RT_Error
              ("Internal: " &
               "Get_Static_Link: Passed a const-area locator",
               Src_Pos => Execution_Source_Pos);
            raise Program_Error;

         when Any_Base_Register =>
            --  Relative to some temp base register kept in the
            --  current Local_Area
            --  NOTE: Should never happen
            Messages.Put_RT_Error
              ("Internal: " &
               "Get_Static_Link: Passed a base-register locator",
               Src_Pos => Execution_Source_Pos);
            raise Program_Error;

         when others =>
            Messages.Put_RT_Error
              ("Internal: " &
               "Get_Static_Link: Ill-formed Object_Locator",
               Src_Pos => Execution_Source_Pos);
            raise Program_Error;
      end case;
   end Get_Static_Link;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Addr              : Object_Virtual_Address;
      Underscores_Every : Natural := 4)
      return String is
   begin
      return Debug.Hex_Image (Addr, Underscores_Every);
   end Hex_Image;

   function Hex_Image
     (Addr              : Word_Ptr;
      Underscores_Every : Natural := 4)
      return String is
   begin
      return Debug.Hex_Image (Addr, Underscores_Every);
   end Hex_Image;

   ------------------------
   -- Initialize_Stg_Rgn --
   ------------------------

   procedure Initialize_Stg_Rgn
     (Context : in out Exec_Context;
      Local_Area : Word_Ptr) is
   --  Allocate a new storage region associated with given Local_Area
   --  Resulting stg_rgn pointer is stored in the Context.
   --  Save the current value of Open_Master and set it to null.
   --  NOTE: This should be called at the beginning of a compiled
   --        ParaSail routine or nested block if Code.Uses_Stg_Rgn is true.
      New_Stg_Rgn : constant Stg_Rgn_Ptr :=
        Get_New_Local_Stg_Rgn
          (New_Local_Area => Local_Area,
           Server_Index => Context.Server_Index);
   begin
      Context.Local_Stg_Rgn := New_Stg_Rgn;
      --  Copy large null value for local region into context for quick access
      Context.Local_Null    := New_Stg_Rgn.Null_Value;

      --  Save value of Open_Master, and set to null
      New_Stg_Rgn.Saved_Open_Master := Context.Open_Master;
      Context.Open_Master           := null;
   end Initialize_Stg_Rgn;

   ---------------------
   -- Initial_Context --
   ---------------------

   function Initial_Context return Exec_Context_RW_Ptr is
   --  Return a pointer to an Exec_Context which can be passed to the
   --  main routine of a compiled program.

      function Get_Global_Chunk return Stg_Rgn_Chunk_Ptr;
         --  Initialize Global_Stack_Chunk if null, and in any case
         --  return its value.

      function Get_Global_Chunk return Stg_Rgn_Chunk_Ptr is
      begin
         if Interpreter.Global_Stack_Chunk = null then
            --  Not yet initialized
            Stg_Rgn_Manager.Initialize_Global_Stack_Chunk;
         end if;
         return Interpreter.Global_Stack_Chunk;
      end Get_Global_Chunk;

      Initial_Offset : constant Offset_Within_Area := 30;

      Initial_Routine : constant Routine_Ptr := Routine_Ptr (Null_Routine);
         --  TBD: Perhaps should be a "real" routine.

      New_Local_Area : constant Word_Ptr :=
        Object_To_Physical_Address ((Get_Global_Chunk,
         Offset => Initial_Offset));

      New_Tcb : constant Word_Ptr :=
        Initial_Tcb
          (Target_Routine => Initial_Routine,
           New_Tcb => Object_To_Physical_Address
             ((Interpreter.Global_Stack_Chunk,
               Offset =>
                 Initial_Offset -
                   (Interpreter.Thread_Control_Block_Size +
                    Interpreter.Thread_Master_Size))),
           Server_Index => Main_Thread_Server_Index);

      New_Local_Rgn : constant Stg_Rgn_Ptr :=
         Get_New_Local_Stg_Rgn
           (New_Local_Area => New_Local_Area,
            Server_Index => Main_Thread_Server_Index);

      New_Context : constant Exec_Context_RW_Ptr := new Exec_Context'
        (Local_Null => New_Local_Rgn.Null_Value,
         Enclosing_Type => null,
         Local_Stg_Rgn => New_Local_Rgn,
         Control_Area => New_Tcb,
         Server_Index =>
           Interpreter.Main_Thread_Server_Index,
         Open_Master => null,
         Params => Object_To_Physical_Address
           ((Interpreter.Global_Stack_Chunk,
            Offset =>
              Interpreter.Global_Stack_Chunk.Data'First)),
         Local_Area => New_Local_Area,
         Local_Area_Length =>
           Interpreter.Global_Stack_Chunk.Chunk_Length -
             Initial_Offset,
         Start_Callee_Locals =>
           Initial_Routine.Start_Callee_Locals);  --  TBD: might need to be > 0
   begin
      --  Initialize static link in new local area to null.
      Init_Static_Link (New_Local_Area);

      return New_Context;
   end Initial_Context;

   -----------------
   -- Initial_Tcb --
   -----------------

   function Initial_Tcb
     (Target_Routine   : Routine_Ptr;
      New_Tcb          : Word_Ptr;
      Server_Index     : Thread_Server_Index) return Word_Ptr
   is
      New_Master : constant Word_Ptr :=
                     Add (New_Tcb, Thread_Control_Block_Size);
   begin
      --  Provide a suitable default initialization
      Initialize_Tcb
        (New_Tcb,
         Tcb_Is_Local => True,
         Routine => Target_Routine,
         Static_Link => null);

      Initialize_Master (New_Master,
        Local_Stg_Rgn_Index     => 0,
        Server_Index            => Server_Index,
        Initial_Subthread_Count => 1);

      --  Link the tcb and master together
      Set_Tcb_Master_Ptr (New_Tcb, New_Master);
      Set_First_Subthread (New_Master, New_Tcb);

      return New_Tcb;
   end Initial_Tcb;

   ----------------------
   -- Init_Static_Link --
   ----------------------

   procedure Init_Static_Link
     (New_Local_Area : Word_Ptr;
      Static_Link    : Word_Ptr := null) is
   begin
      Store_Word_Ptr (New_Local_Area, Local_Area_Static_Link_Offset,
        Static_Link);
   end Init_Static_Link;

   -------------------
   -- Install_Chunk --
   -------------------

   procedure Install_Chunk
     (Chunk : Stg_Rgn_Chunk_Ptr;
      Index : Chunk_Index) is
      Slot_In_Table : Stg_Rgn_Chunk_Ptr renames
        Chunk_Group_Table (Chunk_Group_Index (Index / Chunk_Group_Size))
          (Index_In_Chunk_Group (Index rem Chunk_Group_Size));

      pragma Assert (Slot_In_Table = null);  --  Not already in table
   begin
      --  Compute starting address of chunk.
      Chunk.Index := Index;

      if Virt_Is_Phys then
         Chunk.Starting_Virtual_Address :=
           Word_Ptr_To_Word (Chunk.Data (Chunk.Data'First)'Access) -
             Object_Virtual_Address (Chunk.Data'First) * Word_SU_Size;
      else
         Chunk.Starting_Virtual_Address :=
           Object_Virtual_Address (Index) * Chunk_Divisor;
      end if;

      --  Remember lowest and highest virtual addresses
      if Chunk.Starting_Virtual_Address < Lowest_Virt_Addr then
         Lowest_Virt_Addr := Chunk.Starting_Virtual_Address;
      end if;
      if Chunk.Starting_Virtual_Address >= Highest_Virt_Addr then
         Highest_Virt_Addr := Chunk.Starting_Virtual_Address +
                                Chunk.Data'Last;
      end if;

      --  Store chunk descriptor in chunk table.
      Slot_In_Table := Chunk;

      if Debug_Stg_Rgns then
         Put_Line
           (" Installing a chunk = " & Chunk_Index'Image (Index));
      end if;
   end Install_Chunk;

   ------------------
   -- Install_Code --
   ------------------

   procedure Install_Code (Code : in out Routine_RW_Ptr) is
   --  Install some code in Routine table.
   --  Requires: Code.Index is 0 or Routine with Code.Index
   --           doesn't already exist in table, or it already
   --           exists with the same name, presumably due to having
   --           been compiled and loaded in before the front end started.
   --  Effects: If Code.Index is 0, will be set to unique Routine_Index.
   --           If a routine with the same name already appears in the table,
   --           merge the information and update Code to point to it if
   --           Code is marked as imported.
      use type Routine_Elem_Index, Strings.U_String;
      use Id_To_Routine_Maps;

      Is_Builtin : constant Boolean := not Code.Is_PSVM_Routine
                                         and then Code.Built_In_Desig /=
                                           Strings.Null_U_String;

      function Op_Name return Strings.U_String;
      --  Operation name to use based on whether is builtin

      function Op_Name return Strings.U_String is
      begin
         if Is_Builtin then
            --  Use designator as key if a built-in
            return Code.Built_In_Desig;
         else
            --  Use routine name with overloading index as key
            return Routine_Name_With_Overloading_Index (Routine_Ptr (Code));
         end if;
      end Op_Name;

      Code_Global_Id : constant Global_Operation_Id :=
      --  Global_Operation_Id for routine identified by "Code"
                   (Module_Name => Strings.Index (Code.Full_Module_Name),
                     --  NOTE: Will be 0 if Full_Module_Name is Null_U_String
                    Operation_Name => Strings.Index (Op_Name),
                    Is_Builtin => Is_Builtin);

      Existing_Routine_Ref : Element_Ref :=
         Find_Element (Id_To_Routine_Map, Key => Code_Global_Id);

      procedure Put_Full_Name;
      --  Put full name of routine identified by "Code"

      procedure Put_Full_Name is
         Name_With_Oload : Strings.U_String :=
           Routine_Name_With_Overloading_Index (Routine_Ptr (Code));
      begin
         if Code.Full_Module_Name /= Strings.Null_U_String then
            Put (Strings.To_String (Code.Full_Module_Name) &
              Languages.Module_Name_Separator);
         end if;
         Put (Strings.To_String (Name_With_Oload));
         if not Code.Is_PSVM_Routine
           and then Code.Built_In_Desig /= Strings.Null_U_String
         then
            if Code.Built_In_Desig /= Name_With_Oload then
               --  Designator does not match name
               Put_Line
                 ("(builtin:" &
                  Strings.To_String (Code.Built_In_Desig) & ')');
            else
               --  Designator matches name
               Put_Line ("(builtin)");
            end if;
         else
            New_Line;
         end if;
      end Put_Full_Name;

   begin  --  Install_Code

      if Code.Num_Prior_Homonyms > 0 then
         --  Precompute name with overloading index
         Compute_Routine_Name_With_Overloading_Index (Code);
      end if;

      Existing_Routine_Ref :=
         Find_Element (Id_To_Routine_Map, Key => Code_Global_Id);

      if Existing_Routine_Ref /= null then
         --  Already in table, reuse the index
         Code.Index := Existing_Routine_Ref.all;
         declare
            Existing_Routine : constant Routine_RW_Ptr :=
              Nth_Element (Routine_Table, Routine_Elem_Index (Code.Index));
         begin
            if Code.Is_PSVM_Routine
              or else (not Existing_Routine.Is_PSVM_Routine
                and then Existing_Routine.Is_Compiled_Routine
                and then Existing_Routine.Routine_Addr = null)
            then
               --  Created earlier but no compiled code linked-in;
               --  Overwrite with this definition.
               Set_Nth_Element (Routine_Table,
                 Routine_Elem_Index (Code.Index), Code);

               if Debug_Type_Descs then
                  Put (" Overwriting pre-declared routine for ");
               end if;
            else
               --  Merge information from two imported routines.
               Existing_Routine.Parameters := Code.Parameters;
               Existing_Routine.Uses_Queuing := Code.Uses_Queuing;
               Code := Existing_Routine;
            end if;

            if Debug_Type_Descs then
               Put_Full_Name;
            end if;

            return;  --  All done
         end;

      elsif Code.Index = 0 then
         --  Assign next sequential routine index
         Add_Element
           (Routine_Table,
            Code,
            Routine_Elem_Index (Code.Index));
      else
         --  User picked one, make sure it isn't used yet
         pragma Assert
           (Routine_Elem_Index (Code.Index) > Num_Elements (Routine_Table)
           or else Nth_Element
                      (Routine_Table,
                       Routine_Elem_Index (Code.Index)) =
                   null);

         --  Add enough elements to include pre-chosen index
         while Routine_Elem_Index (Code.Index) >
               Num_Elements (Routine_Table)
         loop
            declare
               Ignore : Routine_Elem_Index;
            begin
               Add_Element (Routine_Table, null, Ignore);
            end;
         end loop;

         --  Now fill in the element
         Set_Nth_Element
           (Routine_Table,
            Routine_Elem_Index (Code.Index),
            Code);
      end if;

      if Existing_Routine_Ref = null then
         --  Enter into Id_To_Routine_Map
         Enter_Element (Id_To_Routine_Map,
           Key => Code_Global_Id,
           Elem => Code.Index,
           Existing_Elem => Existing_Routine_Ref);

         --  Already checked that it isn't in the table yet.
         pragma Assert (Existing_Routine_Ref = null);

         if Debug_Type_Descs then
            Put ("New routine: ");
            Put_Full_Name;
         end if;

         --  See if we are installing code for the debugging console.
         if Debug_Console_Routine = null
           and then Code.Full_Module_Name /= Strings.Null_U_String
           and then Languages.Debug_Console_Full_Name =
             Strings.To_String (Code.Full_Module_Name) &
             Languages.Module_Name_Separator &
             Strings.To_String (Code.Name)
         then
            Put_Line ("Installing Debugging Console!");
            Debug_Console_Routine := Routine_Ptr (Code);
         end if;
      end if;
   end Install_Code;

   -----------------------
   -- Install_Type_Info --
   -----------------------

   procedure Install_Type_Info (Type_Desc : Type_Descriptor_Ptr) is
      Prior_Type_Ref : Type_Name_Maps.Element_Ref := null;
      use type Type_Name_Maps.Element_Ref;
      Src_Pos : Source_Positions.Source_Position :=
        Source_Positions.Null_Source_Position;
      use type Source_Positions.Source_Position, Trees.Root_Sem_Ptr;
   begin
      Add_Element (Type_Table, Type_Desc, Type_Elem_Index (Type_Desc.Index));
      Type_Desc.Location := (Zero_Base, Offset_Within_Area (Type_Desc.Index),
                             No_VM_Obj_Id);
      --  TBD: We are using same format of "addresses" for type info
      --      as we use for routines.  Hopefully there will never
      --      be any ambiguity.  If such ambiguity develops, we could
      --      invent another "Area_Base_Indicator" to use for types
      --      distinct from Zero_Base.

      if Debug_Type_Descs then
         Put_Line (" Type_Desc #" & Type_Index'Image (Type_Desc.Index) & ": " &
           Strings.To_String (Type_Desc.Name));
      end if;

      --  Make sure Type_Desc.Name is unique
      Type_Name_Maps.Enter_Element
        (Table => Type_Name_Map,
         Key   => Type_Desc.Name,
         Elem  => Type_Desc,
         Existing_Elem => Prior_Type_Ref);

      if Prior_Type_Ref /= null then
         --  We have a duplication.
         --  Get a source position for the warning if possible.
         if Type_Desc.Type_Sem /= null then
            Src_Pos := Trees.Find_Source_Pos (Type_Desc.Type_Sem.Definition);
         end if;

         if Src_Pos = Source_Positions.Null_Source_Position
           and then Prior_Type_Ref.all.Type_Sem /= null
         then
            Src_Pos := Trees.Find_Source_Pos
                         (Prior_Type_Ref.all.Type_Sem.Definition);
         end if;

         if Debug_Type_Descs then
            Messages.Put_Warning (Strings.To_String (Type_Desc.Name) &
                ": appears multiple times in type table, indices" &
                Type_Index'Image (Prior_Type_Ref.all.Index) & " &" &
                Type_Index'Image (Type_Desc.Index),
              Src_Pos => Src_Pos);
         end if;
      end if;
   end Install_Type_Info;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Img : String) return Word_Type is
   begin
      return Debug.Integer_Value (Img);
   end Integer_Value;

   --------------------------
   -- Invoke_Debug_Console --
   --------------------------

   procedure Invoke_Debug_Console (Context : in out Exec_Context;
                                   Reason : Debugger_Reason) is
   --  Invoke the debugging console.
   --  Pause other servers while the console is executing.
      Params : Word_Array (1 .. 2) := (0, Word_Type (Reason));
      Result : Single_Step_Indicator;

      Info : Server_Info renames Server_Info_Array (Context.Server_Index);
      Cur_State : Server_State renames Info.Current_State;
      use Source_Positions;
   begin
      Cur_State.Step_Indicator := Continue_Execution;
      Cur_State.Stopped_At_Line := 0;

      if Debug_Console_Routine /= null then
         --  Pause the other servers
         Thread_Manager.Pause_Other_Servers (Context.Server_Index);
         if Is_Shut_Down then
            --  Try to be sure we don't get "stuck"
            Pause_Nesting := 0;
            Solo_Server := 0;
            return;
         end if;

         if Reason in Breakpoint_Encountered then
            --  Double check that the breakpoint is still set
            if Cur_State.Code.Code.Instrs (Cur_State.Pc).Source_Pos.Line /= 0
            then
               --  Breakpoint has been cleared while we were waiting
               --  for the other servers to pause.
               --  Resume the other servers and return immediately.
               Thread_Manager.Resume_Other_Servers (Context.Server_Index);
               return;
            end if;
         end if;

         --  Invoke the debugging console
         Execute_Call_Op (Context,
            Params => Params (Params'First)'Unchecked_Access,
            Static_Link => null,
            Target_Routine => Debug_Console_Routine,
            Locked_Param_Info => Null_Locked_Param_Info,
            Polymorphic_Output_Type => null);

         Result := Single_Step_Indicator (Params (Params'First));

         --  Don't reenter the debugger until the line changes
         Cur_State.Stopped_At_Line := Cur_State.Src_Pos.Line;

         case Result is
            when Stop_Execution =>
               --  Exit the program
               Shut_Down_Thread_Servers (Total_Errors => 0);

            when Single_Step_Into =>
               --  Return to the debugger as soon as leaving the current line
               --  including by calling another routine or returning.
               Cur_State.Step_Indicator := Result;

            when Single_Step_Over =>
               --  Return to the debugger as soon as leaving the current line,
               --  but "step over" any calls.
               Cur_State.Step_Indicator := Result;

            when Continue_Execution =>
               --  Continue until hitting a breakpoint or an assertion failure.
               --  We need to suppress the current breakpoint, if any,
               --  until we get past it.
               null;

            when Step_Out =>
               --  Continue until exiting the given number of stack frames.
               declare
                  Where_To_Stop : Server_State_Ptr := Cur_State.Prev_State;
               begin
                  --  Find where to set the Step_Indicator.
                  for I in 2 .. Result loop
                     if Where_To_Stop /= null then
                        Where_To_Stop := Where_To_Stop.Prev_State;
                     end if;
                  end loop;
                  if Where_To_Stop /= null then
                     Where_To_Stop.Step_Indicator := Result;
                  end if;
               end;
         end case;

         --  Resume the other servers
         Thread_Manager.Resume_Other_Servers (Context.Server_Index);
      end if;
   end Invoke_Debug_Console;

   --------------------------------------
   -- Invoke_Parameterless_Computation --
   --------------------------------------

   function Invoke_Parameterless_Computation
     (Computation    : Routine_Ptr;
      Result_Type    : Type_Descriptor_Ptr;
      Enclosing_Type : Non_Op_Map_Type_Ptr := null;
      Server_Index   : Thread_Server_Index := Main_Thread_Server_Index)
     return Word_Type is
      --  Invoke a parameterless routine to compute the value of a
      --  compile-time-known constant.

      New_Stack_Chunk : Stg_Rgn_Chunk_Ptr :=
        Allocate_Stg_Rgn_Chunk (Min_Size => 5_000,
          Server_Index => Server_Index);
      --  Get a big enough primary stack

      Initial_Offset : constant Offset_Within_Area := 30;
      New_Local_Area_Obj_Addr : constant Object_Address :=
        (New_Stack_Chunk,
         Offset => New_Stack_Chunk.Last_In_Use + Initial_Offset);
      New_Local_Area : constant Word_Ptr := Object_To_Physical_Address
        (New_Local_Area_Obj_Addr);

      Initial_Tcb_Addr : constant Word_Ptr :=
        Object_To_Physical_Address
          ((New_Stack_Chunk,
            Offset => New_Local_Area_Obj_Addr.Offset -
              (Thread_Control_Block_Size + Thread_Master_Size)));
      Initial_Tcb : constant Word_Ptr :=
        Interpreter.Initial_Tcb
          (Target_Routine => Routine_Ptr (Computation),
           New_Tcb        => Initial_Tcb_Addr,
           Server_Index   => Server_Index);
      Initial_Rgn : constant Stg_Rgn_Ptr :=
           Get_New_Local_Stg_Rgn
             (New_Local_Area => New_Local_Area,
              Server_Index => Server_Index);
      Context : Exec_Context :=
        (Local_Null => Initial_Rgn.Null_Value,
         Enclosing_Type => Enclosing_Type,
         Local_Stg_Rgn => Initial_Rgn,
         Control_Area => Initial_Tcb,
         Server_Index => Server_Index,
         Open_Master => null,
         Params => Object_To_Physical_Address ((New_Stack_Chunk,
                    Offset => New_Stack_Chunk.Last_In_Use + 1)),
         Local_Area => New_Local_Area,
         Local_Area_Length => New_Stack_Chunk.Chunk_Length -
                              New_Local_Area_Obj_Addr.Offset,
         Start_Callee_Locals => Computation.Start_Callee_Locals);

      Thread_Was_Queued : Boolean;

      Result : Word_Type := Null_Value;
   begin

      if Debug_Calls then
         Put_Line (" Invoke_Parameterless_Computation " &
           Strings.To_String (Computation.Name) &
           ", initial_tcb at " & Hex_Image (Initial_Tcb));
      end if;

      --  Make sure Global_Data_Stg_Rgn exists
      Initialize_Global_Data_Stg_Rgn;

      --  Initialize static link in new local area to null.
      Init_Static_Link (New_Local_Area);

      --  Initialize Output, indicating it should be allocated
      --  in Global_Data_Stg_Rgn
      Store_Word
        (Context,
         (Param_Area, 0, No_VM_Obj_Id),
         Null_For_Type_Or_Stg_Rgn
            (Type_Desc => Result_Type,
             Stg_Rgn => Global_Data_Stg_Rgn));

      --  Execute compiled code
      Execute
        (Routine_Ptr (Computation),
         Start_Pc => 1,
         Context => Context,
         Thread_Was_Queued => Thread_Was_Queued);

      pragma Assert (not Thread_Was_Queued);

      --  Free up the initial tcb/master
      Free_Initial_Tcb (Initial_Tcb,
        Server_Index => Server_Index);

      --  Return the output,
      --  and also store it in global memory
      --  so we can return an address.
      Result := Fetch_Word (Context, (Param_Area, 0, No_VM_Obj_Id));

      --  Release region allocated for this evaluation
      Release_Stg_Rgn (Context.Local_Stg_Rgn);

      --  Release stack chunk
      Release_Stg_Rgn_Chunk (New_Stack_Chunk,
        Server_Index => Server_Index);

      return Result;
   end Invoke_Parameterless_Computation;

   -------------------
   -- Is_Null_Value --
   -------------------

   function Is_Null_Value
     (Word      : Word_Type;
      Type_Desc : Type_Descriptor_Ptr) return Boolean is
   begin
      if Type_Desc = null then
         --  Check for a null "ref"
         return Word_To_Word_Ptr (Word) = null;
      elsif Type_Desc.Is_Small then
         return Word = Type_Desc.Null_Value;
      elsif Is_Large_Null (Word) then
         return True;
      elsif Type_Desc.Is_Polymorphic then
         declare
            --  Skip over op map, if any
            Real_Type_Desc : constant Non_Op_Map_Type_Ptr :=
              Skip_Over_Op_Map (Type_Desc);
         begin
            --  Check for polymorphic null
            if Is_Null_Value  --  Recurse with underlying value/type
                (Fetch_Word (Word + Large_Obj_Header_Size),
                 Type_Desc => Real_Type_Desc.Components (1).Type_Desc)
            then
               --  NOTE: This shouldn't happen any more because
               --        when we create a polymorphic object
               --        we create an overall null if the underlying
               --        value is null.
               Put_Line ("*** Found wrapped polymorphic null ***");
               return True;
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Is_Null_Value;

   -------------------
   -- Is_Large_Null --
   -------------------

   function Is_Large_Null (Value : Word_Type) return Boolean is
   --  NOTE: We reserve a special chunk number for all large nulls.
   --        We also require it to be odd.
   begin
      return Value = Null_Value  --  NOTE: Also check for Integer Null_Value
        or else (To_Unsigned_Word (Value) and (Chunk_Mask + 1)) =
                 Large_Null_Base_Value;
   end Is_Large_Null;

   --------------
   -- Is_Small --
   --------------

   function Is_Small (Type_Desc : Type_Descriptor_Ptr) return Boolean is
   begin
      return Type_Desc.Is_Small;
   end Is_Small;

   ----------------------------
   -- Is_Special_Large_Value --
   ----------------------------

   function Is_Special_Large_Value (Value : Word_Type) return Boolean is
   --  NOTE: We reserve special (negative) chunk #s for special large values.
   --        We also require special large values to be odd.
      subtype Special_Chunk_Value_Range is Unsigned_Word_Type
        range (Large_Null_Base_Value - 1) * 5 .. Large_Null_Base_Value - 1;
   begin
      return Value < 0 and then Value mod 2 = 1
        and then (To_Unsigned_Word (Value) and Chunk_Mask) in
          Special_Chunk_Value_Range;
   end Is_Special_Large_Value;

   ---------------------
   -- Known_Type_Desc --
   ---------------------

   function Known_Type_Desc
     (Location     : Object_Locator;
      Now_Building : Type_Descriptor_Ptr := null;
      Src_Pos      : Source_Positions.Source_Position :=
                       Source_Positions.Null_Source_Position)
     return Type_Descriptor_Ptr
   is
      use type Trees.Root_Sem_Ptr;
   begin
      if Now_Building /= null
        and then Location = (Type_Area, 0, No_VM_Obj_Id)
      then
         --  Self reference
         return Now_Building;
      end if;

      if Is_Null_Obj_Locator (Location)
        or else Location.Base /= Zero_Base
      then
         if Now_Building /= null and then Now_Building.Type_Sem /= null then
            Messages.Put_RT_Error
              ("Internal: Nested type not compile-time known " &
               "while building type descriptor",
               Src_Pos);
         else
            Messages.Put_RT_Error
              ("Internal: Type descriptor not compile-time known",
               Src_Pos);
         end if;

         --  Return an empty type descriptor (with Index = 0)
         return new Type_Descriptor (Has_Op_Map => False);
      end if;

      pragma Assert (not Is_Null_Obj_Locator (Location));
      pragma Assert (Location.Base = Zero_Base);
      declare
         Result : constant Type_Descriptor_Ptr :=
           Nth_Element (Type_Table, Type_Elem_Index (Location.Offset));
      begin
         --  NOTE: Might have an abstract type here if needed for
         --       polymorphic type.
         return Result;
      end;
   end Known_Type_Desc;

   ------------------------
   -- Large_Obj_On_Stack --
   ------------------------

   function Large_Obj_On_Stack
     (Addr : Object_Virtual_Address) return Boolean
   --  Return True if given non-null large obj is residing on stack
     renames Large_Obj_Header_Ops.Large_Obj_On_Stack;

   --------------------
   -- Large_Obj_Size --
   --------------------

   function Large_Obj_Size
     (Addr : Object_Virtual_Address) return Offset_Within_Chunk
   --  Return size in words given a non-null large object
     renames Large_Obj_Header_Ops.Large_Obj_Size;

   -------------------------
   -- Large_Obj_Type_Desc --
   -------------------------

   function Large_Obj_Type_Desc
     (Addr : Object_Virtual_Address) return Non_Op_Map_Type_Ptr is
   begin
      return To_Type_Desc (Large_Obj_Type_Info (Addr));
   end Large_Obj_Type_Desc;

   -----------------------------
   -- Large_Obj_Stg_Rgn_Index --
   -----------------------------

   function Large_Obj_Stg_Rgn_Index
     (Large_Obj_Addr : Object_Virtual_Address)
      return Stg_Rgn_Index
      renames Large_Obj_Header_Ops.Large_Obj_Stg_Rgn_Index;
   --  Return region index associated with (possibly null) large object

   -------------------------
   -- New_Object_Exported --
   -------------------------

   function New_Object_Exported
     (Context      : in out Exec_Context;
      Type_Info    : Type_Descriptor_Ptr;
      Existing_Obj : Word_Type) return Word_Type is
   --  Create new object in region based on Existing_Obj, unless is 0.
      Stg_Region : Stg_Rgn_Ptr := null;
      Type_Desc  : Non_Op_Map_Type_Ptr := Skip_Over_Op_Map (Type_Info);
   begin
      if Is_Small (Type_Desc) then
         --  Return appropriate kind of "null"
         return Null_For_Type (Type_Desc);
      else
         if Existing_Obj = 0 then
            Stg_Region := Local_Stg_Rgn (Context);
         else
            Stg_Region := Stg_Rgn_Of_Large_Obj (Existing_Obj);
         end if;
         --  Return large object appropriately initialized.
         return Create_Large_Obj
                 (Type_Desc,
                  Stg_Region,
                  Context.Server_Index);
      end if;
   end New_Object_Exported;

   -------------
   -- New_Tcb --
   -------------

   function New_Tcb (
      Context : in out Exec_Context;
      Parallel_Master : Word_Ptr;
      Num_Params : Integer) return Word_Ptr is
      --  Execute the main part of the Create_Tcb instruction.
      --  Return the value to be stored in the Parallel_Control slot.
      --  Fields of Instruction are:
      --   Parallel_Master : Object_Locator;
      --   Parallel_Control : Object_Locator;
      --   Parallel_Static_Link : Object_Locator;
      --   Num_Params : Natural;
      --   NOTE: Parallel_Master is a pseudo-large-object at the given address
      --         Parallel_Control is the address of a word which will be
      --         set to point at the space allocated for the Tcb.
      Tcb_Virt_Addr : constant Object_Virtual_Address :=
         Allocate_From_Stg_Rgn
           (Stg_Rgn_Of_Large_Obj (Parallel_Master),
            Thread_Control_Block_Size +
            Offset_Within_Area (Num_Params),
            Context.Server_Index);
            --  Allocate space for a TCB object from the
            --  region associated with the master.
      Tcb_Addr : constant Word_Ptr :=
        Virtual_To_Physical_Address (Tcb_Virt_Addr);
   begin
      if Debug_Threading then
         Put_Line (" Create_TCB addr =" & Hex_Image (Tcb_Addr));
      end if;
      --  Initialize self (virtual) address to enable reclaiming the TCB
      Word_To_Tcb_Ptr (Tcb_Addr).Self_Address := Tcb_Virt_Addr;

      --  Return phys addr to be stored into Parallel_Control.
      return Tcb_Addr;
   end New_Tcb;

   ----------------------------
   -- Null_For_Local_Stg_Rgn --
   ----------------------------

   function Null_For_Local_Stg_Rgn (Local_Area : Word_Ptr) return Word_Type
   --  Return a (large) null for the region associated with the local area
   --  starting at the given addr
   is
   begin
      --  Fetch region from offset in local area, and then
      --  pass the buck to the version that takes an explicit region
      return Null_For_Stg_Rgn (Stg_Rgn_Table (Stg_Rgn_Index
        (Fetch_Word (Local_Area, Local_Area_Stg_Rgn_Ptr_Offset))));
   end Null_For_Local_Stg_Rgn;

   ----------------------
   -- Null_For_Stg_Rgn --
   ----------------------

   function Null_For_Stg_Rgn (Stg_Rgn : Stg_Rgn_Ptr) return Word_Type is
      pragma Assert (Stg_Rgn.Null_Value /= Null_Virtual_Address);
   begin
      return Stg_Rgn.Null_Value;
   end Null_For_Stg_Rgn;

   --------------------------
   -- Null_Of_Same_Stg_Rgn --
   --------------------------

   function Null_Of_Same_Stg_Rgn
     (Type_Info               : Type_Descriptor_Ptr;
      Existing_Obj_In_Stg_Rgn : Word_Type) return Word_Type is
   --  Return a null for the given type, and if large, for the region
   --  associated with the existing object.
   begin
      if Type_Info.Is_Small then
         --  Return appropriate kind of "null"
         return Type_Info.Null_Value;
      else
         --  Return large null for region determined by Existing obj
         return Null_For_Stg_Rgn
               (Stg_Rgn_Of_Large_Obj (Existing_Obj_In_Stg_Rgn));
      end if;
   end Null_Of_Same_Stg_Rgn;

   ------------------------------
   -- Null_For_Type_Or_Stg_Rgn --
   ------------------------------

   function Null_For_Type_Or_Stg_Rgn
     (Type_Desc : Type_Descriptor_Ptr;
      Stg_Rgn   : Stg_Rgn_Ptr;
      Is_By_Ref : Boolean := False) return Word_Type is
   begin
      if Is_By_Ref then
         return Word_Ptr_To_Word (null);

      elsif Is_Small (Type_Desc) then
         return Type_Desc.Null_Value;

      else
         return Null_For_Stg_Rgn (Stg_Rgn);
      end if;
   end Null_For_Type_Or_Stg_Rgn;

   ------------------------------------
   -- Null_For_Type_Or_Local_Stg_Rgn --
   ------------------------------------

   function Null_For_Type_Or_Local_Stg_Rgn
     (Context : Exec_Context;
      Type_Desc : Type_Descriptor_Ptr) return Word_Type is
   --  Return a (large or small) null for the local region associated with the
   --  current context, using the Type_Desc to decide whether is large.
   begin
      if Is_Small (Type_Desc) then
         return Type_Desc.Null_Value;
      else
         pragma Assert (Context.Local_Null /= Null_Virtual_Address);
         return Context.Local_Null;
      end if;
   end Null_For_Type_Or_Local_Stg_Rgn;

   -----------------------
   -- Null_Routine_Body --
   -----------------------

   procedure Null_Routine_Body
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      null;
   end Null_Routine_Body;

   ----------------
   -- Num_Inputs --
   ----------------

   function Num_Inputs (Conv_Desc : Convention_Descriptor)
     return Natural is
      --  Extract input parameter count from Convention_Descriptor
   begin
      return Natural (Conv_Desc / 2 ** Conv_Width mod 2 ** Num_Inp_Width);
   end Num_Inputs;

   -----------------
   -- Num_Outputs --
   -----------------

   function Num_Outputs (Conv_Desc : Convention_Descriptor)
     return Natural is
      --  Extract output parameter count from Convention_Descriptor
   begin
      return Natural (Conv_Desc / 2 ** (Conv_Width + Num_Inp_Width)
        mod 2 ** Num_Out_Width);
   end Num_Outputs;

   ----------------------
   -- Num_Stack_Frames --
   ----------------------

   procedure Num_Stack_Frames
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Return count of number of stack frames for current thread.
      Info : Server_Info renames Server_Info_Array (Context.Server_Index);
      Cur_State : Server_State renames Info.Current_State;
      Result : Word_Type := 0;
      Caller_State : Server_State_Ptr := Caller_Of (Cur_State);
   begin
      while Caller_State /= null loop
         Result := Result + Word_Type'(1);
         Caller_State := Caller_Of (Caller_State.all);
      end loop;
      Store_Word (Params, 0, Result);
   end Num_Stack_Frames;

   ---------------------
   -- Nth_Stack_Frame --
   ---------------------

   procedure Nth_Stack_Frame
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Return nth stack frame for current thread.
      Frame_Num : Natural := Natural (Fetch_Word (Params, 1));

      Info : Server_Info renames Server_Info_Array (Context.Server_Index);
      This_State : Server_State_Ptr := Info.Current_State'Access;
      Caller_State : Server_State_Ptr := Caller_Of (Info.Current_State);

      Target : constant Word_Type := Fetch_Word (Params, 0);
      Target_Stg_Rgn : constant Stg_Rgn_Ptr := Stg_Rgn_Of_Large_Obj (Target);

      Thread_Id : Word_Type := 0;  --  Identifies current thread
      Master_Id : Word_Type := 0;  --  Identifies master of current thread
   begin
      for I in 2 .. Frame_Num loop
         exit when Caller_State = null;
         This_State := Caller_State;
         Caller_State := Caller_Of (Caller_State.all);
      end loop;

      declare
         --  Create the Stack_Frame_Info object
         Result : constant Word_Type :=
                   Create_Large_Obj
                     (Type_Desc    => Static_Link,
                      Stg_Rgn      => Target_Stg_Rgn,
                      Server_Index => Context.Server_Index);
      begin
         --  Fill in the "Code : optional Routine"
         if This_State.Code = null then
            Store_Word (Result + Large_Obj_Header_Size, Null_Value);
         else
            Store_Word_Ptr
              (Word_To_Word_Ptr (Result),
               Large_Obj_Header_Size,
               To_Word_Ptr (This_State.Code.all'Address));
         end if;

         --  Fill in the "Enclosing_Type : optional Type_Descriptor"
         if This_State.Context.Enclosing_Type = null then
            Store_Word
             (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(1),
              Null_Value);
         else
            Store_Word_Ptr
              (Word_To_Word_Ptr (Result),
               Large_Obj_Header_Size + Offset_Within_Chunk'(1),
               To_Word_Ptr
                 (This_State.Context.Enclosing_Type.all'Address));
         end if;

         --  Fill in the "Params : optional Univ_Integer"
         Store_Word_Ptr
           (Word_To_Word_Ptr (Result),
            Large_Obj_Header_Size + Offset_Within_Chunk'(2),
            This_State.Context.Params);

         --  Fill in the "Local_Area : optional Univ_Integer"
         Store_Word_Ptr
           (Word_To_Word_Ptr (Result),
            Large_Obj_Header_Size + Offset_Within_Chunk'(3),
            This_State.Context.Local_Area);

         --  Fill in the "Pc : optional Instruction::Code_Offset"
         Store_Word (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(4),
                     Word_Type (This_State.Pc));

         --  Fill in the "Start_Pc : optional Instruction::Code_Offset"
         Store_Word (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(5),
                     Word_Type (This_State.Start_Pc));

         --  Fill in the "Src_Pos : optional Source_Position"
         declare
            use Source_Positions;
            Line : Line_Number := This_State.Src_Pos.Line;
         begin
            if Line = 0 and then This_State.Src_Pos.End_Line > 0 then
               --  Fix up the line number since we seem to have a breakpoint
               Line := This_State.Src_Pos.End_Line;
            end if;

            Store_Word
              (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(6),
               Word_Type (This_State.Src_Pos.File) * 2**32 +
               Word_Type (Line) * 2**10 +
               Word_Type (This_State.Src_Pos.Col));
         end;

         --  Fill in Master_Id and Thread_Id
         Thread_Id := Word_Ptr_To_Word (This_State.Context.Control_Area);
         if Thread_Id /= 0 then
            declare
               Master : constant Word_Ptr :=
                 Tcb_Master_Ptr (This_State.Context.Control_Area);
            begin
               if Master /= null then
                  declare
                     Index : constant Master_Index := Index_Of_Master (Master);
                     Master_Extra : Master_Extra_Rec renames
                       Master_Extras (Index);
                  begin
                     Master_Id := Word_Type (Index) * 2**16 +
                       Word_Type (Master_Extra.Reuse_Count);
                  end;
               end if;
            end;
         end if;

         --  Fill in the "Master_Id : Univ_Integer"
         Store_Word
           (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(7),
            Master_Id);

         --  Fill in the "Thread_Id : Univ_Integer"
         Store_Word
           (Result + Large_Obj_Header_Size + Offset_Within_Chunk'(8),
            Thread_Id);

         --  Return the result
         Store_Word (Params, 0, Result);
      end;
   end Nth_Stack_Frame;

   -------------------------------
   -- Nth_Frame_Type_At_Locator --
   -------------------------------

   procedure Nth_Frame_Type_At_Locator
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Return type relative to nth stack frame for current thread.
   --  func Nth_Frame_Type_At_Locator (Frame_Num, Type_Locator)
   --    -> Type_Descriptor
      Frame_Num : Natural := Natural (Fetch_Word (Params, 1));
      Type_Locator : constant Object_Locator :=
         Extract_Object_Locator (Fetch_Word (Params, 2));

      Info : Server_Info renames Server_Info_Array (Context.Server_Index);
      This_State : Server_State_Ptr := Info.Current_State'Access;
      Caller_State : Server_State_Ptr := Caller_Of (Info.Current_State);

      Result : Non_Op_Map_Type_Ptr;
   begin
      if Type_Locator.Base = Zero_Base then
         --  Don't need the "true" context to decode this type locator
         if Type_Locator.Offset = 0 then
            --  null type
            Store_Word (Params, 0, Null_Value);
         else
            Result := Get_Type_Desc (Context, Type_Locator);
            Store_Word_Ptr (Params, 0, To_Word_Ptr (Result.all'Address));
         end if;
         return;  --  all done  --
      end if;

      --  Find the Nth frame, to get the right context
      for I in 2 .. Frame_Num loop
         exit when Caller_State = null;
         This_State := Caller_State;
         Caller_State := Caller_Of (Caller_State.all);
      end loop;

      --  Get the "real" type descriptor using the context
      Result := Get_Type_Desc (This_State.Context.all, Type_Locator);
      Store_Word_Ptr (Params, 0, To_Word_Ptr (Result.all'Address));
   end Nth_Frame_Type_At_Locator;

   ---------------------
   -- Nth_Instruction --
   ---------------------

   function Nth_Instruction
     (Index : Routine_Index;
      N     : Code_Index) return Instruction is
   begin
      return Nth_Element
        (Routine_Table, Routine_Elem_Index (Index)).Code.Instrs (N);
   end Nth_Instruction;

   -----------------
   -- Nth_Routine --
   -----------------

   function Nth_Routine
     (Index : Routine_Index) return Routine_Ptr is
   begin
      return Routine_Ptr (Nth_Element
        (Routine_Table, Routine_Elem_Index (Index)));
   end Nth_Routine;

   ---------------------------
   -- Nth_Type_Area_Element --
   ---------------------------

   function Nth_Type_Area_Element
     (Type_Desc   : Non_Op_Map_Type_Ptr;
      Item_Offset : Offset_Within_Area;
      Type_Base   : Area_Base_Indicator := Type_Area) return Element_Info is
   begin
      return Type_Descriptor_Ops.Nth_Type_Area_Element
               (Type_Desc, Item_Offset, Type_Base);
   end Nth_Type_Area_Element;

   --------------------------------
   -- Object_To_Physical_Address --
   --------------------------------

   function Object_To_Physical_Address
     (Addr : Object_Address) return Word_Ptr is
   begin
      if Addr.Enclosing_Chunk = null then
         return null;
      else
         return To_Word_Ptr (Addr.Enclosing_Chunk.Data (Addr.Offset)'Address);
      end if;
   end Object_To_Physical_Address;

   -----------------------
   -- Obj_Address_Image --
   -----------------------

   function Obj_Address_Image (Addr : Object_Address) return String is
   begin
      return Debug.Obj_Address_Image (Addr);
   end Obj_Address_Image;

   -----------------------
   -- Obj_Locator_Image --
   -----------------------

   function Obj_Locator_Image (Locator : Object_Locator) return String is
   begin
      return Debug.Obj_Locator_Image (Locator);
   end Obj_Locator_Image;

   -----------------------
   -- Output_Needs_Init --
   -----------------------

   function Output_Needs_Init (Conv_Desc : Convention_Descriptor)
     return Boolean is
      --  Whether output(s) need to be initialized
   begin
      return Conv_Desc >=
        2 ** (Conv_Width + Num_Inp_Width + Num_Out_Width + 1);
   end Output_Needs_Init;

   ---------------------
   -- Peek_At_Address --
   ---------------------

   procedure Peek_At_Address
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
      --  Return value at given address + offset
      Base_Word : constant Word_Type := Fetch_Word (Params, 1);
      Offset : constant Offset_Within_Area :=
        Offset_Within_Area (Fetch_Word (Params, 2));
   begin
      if Base_Word mod (Word_Type'Size / System.Storage_Unit) = 0
        and then Base_Word > 0
      then
         begin
            --  Address might be OK
            Store_Word
              (Params, 0, Fetch_Word (Word_To_Word_Ptr (Base_Word), Offset));
            return;
         exception
            when others =>
               --  Apparently not a good address!
               null;
         end;
      end if;

      --  Not a good address
      Put_Line (Standard_Error, "Peek at bad address: " &
        Hex_Image (Base_Word) &
        "[" & Offset_Within_Area'Image (Offset) & "]");

      Store_Word (Params, 0, 0);
   end Peek_At_Address;

   -----------------------
   -- PSVM_Object_Input --
   -----------------------

   procedure PSVM_Object_Input
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : out Type_Descriptor_Ptr;
      Object    : out Word_Type;
      Stg_Rgn   : Stg_Rgn_Ptr) is
   --  Read name of Type and stream representation of Object from stream;
   --  return both type descriptor and value of object.
   --  Stg_Rgn must be a storage region in which to create a large object.
      Type_Name : constant Strings.U_String :=
        Strings.U_String'Input (Stream);
      Len : Offset_Within_Area := 0;
   begin
      Type_Desc := Get_Type_Desc_By_Name (Type_Name);
      if Type_Desc = null then
         --  Oops, type-descriptor unknown
         Object := Null_Value;
         return;
      end if;

      --  Now pass the buck to the "Read" routine where Type is an *input*
      PSVM_Object_Read (Stream, Type_Desc, Object, Stg_Rgn);
   end PSVM_Object_Input;

   ------------------------
   -- PSVM_Object_Output --
   ------------------------

   procedure PSVM_Object_Output
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : Word_Type) is
   --  Write name of Type and stream representation of Object into stream
   begin
      --  Write name of type and then pass the buck to the "Write" routine
      Strings.U_String'Output (Stream, Type_Desc.Name);

      PSVM_Object_Write (Stream, Type_Desc, Object);
   end PSVM_Object_Output;

   ----------------------
   -- PSVM_Object_Read --
   ----------------------

   procedure PSVM_Object_Read
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : out Word_Type;
      Stg_Rgn   : Stg_Rgn_Ptr) is
   --  Read stream representation of Object from stream given type;
   --  return value of object.
   --  Stg_Rgn must be a storage region in which to create a large object.
      Len : Offset_Within_Area := 0;
      U_Type_Desc : constant Type_Descriptor_Ptr :=
        Unwrapped_Type_Desc (Type_Desc);  --  Unwrap the type desc
   begin
      if U_Type_Desc.Is_Small then
         case U_Type_Desc.Type_Kind is
            when Univ_String_Kind | Univ_Enum_Kind =>
               --  Need to read in a string and look it up
               Object := To_Word_Type (Strings.U_String'Input (Stream));
            when others =>
               --  The easy case
               Word_Type'Read (Stream, Object);
         end case;
         return;
      end if;

      if U_Type_Desc.Type_Kind = Univ_String_Kind then
         --  Large Univ_String representation.
         --  Use U_String to read it in and then convert to Univ_String.
         Object := Univ_Strings.To_Word_Type (Univ_Strings.From_U_String
                     (Strings.U_String'Input (Stream),
                      Null_For_Stg_Rgn (Stg_Rgn)));
         return;
      end if;

      --  Read the length; 0 means null value
      Offset_Within_Area'Read (Stream, Len);

      if Len = 0 then
         --  Construct a large null
         Object := Null_For_Stg_Rgn (Stg_Rgn);
         return;
      end if;

      --  Create a large object and read in each piece
      Object :=
        Allocate_From_Stg_Rgn (Stg_Rgn, Len, Stg_Rgn.Owning_Server);
      --  Set the type desc
      Set_Large_Obj_Type_Info (Object, U_Type_Desc.Index);

      --  Initialize to be unlocked
      Set_Large_Obj_Lock_Obj (Object, 0);

      --  Now reconstruct the components

      if U_Type_Desc.Type_Kind = Basic_Array_Kind then

         --  Reconstruct the array components

         declare
            Arr_Comp_Type : constant Type_Descriptor_Ptr :=
              Basic_Array_Comp_Type (U_Type_Desc);

            --  Compute the array length from the obj length
            Arr_Len : constant Offset_Within_Area :=
              Len - Large_Obj_Header_Size - 1;
         begin
            Store_Word (Object + Large_Obj_Header_Size,
              Word_Type (Arr_Len));

            for I in 1 .. Arr_Len loop
               declare
                  Offset : constant Offset_Within_Area :=
                    Large_Obj_Header_Size + I;
                  Component : Word_Type;
               begin
                  --  Reconstruct the component
                  PSVM_Object_Read (Stream, Arr_Comp_Type, Component, Stg_Rgn);

                  --  Store it into the array
                  Store_Word (Object + Offset, Component);
               end;
            end loop;
         end;

      else
         --  Reconstruct normal components

         for I in 1 .. U_Type_Desc.Num_Components loop
            declare
               Comp_Type : Non_Op_Map_Type_Ptr :=
                 Skip_Over_Op_Map
                    (U_Type_Desc.Components (I).Type_Desc);
               Offset : constant Offset_Within_Area :=
                 Large_Obj_Header_Size + Offset_Within_Area (I - 1);
               Component : Word_Type;
            begin
               --  Reconstruct the component
               if U_Type_Desc.Is_Polymorphic then
                  --  Read in actual component type first if enclosing
                  --  type is polymorphic.
                  PSVM_Object_Input (Stream, Comp_Type, Component, Stg_Rgn);

                  --  And overwrite the enclosing object's type to match
                  if Comp_Type.Corresponding_Polymorphic_Type_Desc /= null then
                     Set_Large_Obj_Type_Info (Object,
                       Comp_Type.Corresponding_Polymorphic_Type_Desc.Index);
                  else
                     if True or else Debug_Type_Descs then
                        Ada.Text_IO.Put_Line
                          ("  Type " & Strings.To_String (Comp_Type.Name) &
                           " has no corresponding polymorphic type");
                     end if;
                  end if;
               else
                  PSVM_Object_Read (Stream, Comp_Type, Component, Stg_Rgn);
               end if;

               --  Store it into the allocated space
               Store_Word (Object + Offset, Component);
            end;
         end loop;
      end if;
   end PSVM_Object_Read;

   -----------------------
   -- PSVM_Object_Write --
   -----------------------

   procedure PSVM_Object_Write
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Type_Desc : Type_Descriptor_Ptr;
      Object    : Word_Type) is
   --  Write stream representation of Object of given Type_Desc into stream
      Len : Offset_Within_Area := 0;
      U_Type_Desc : Type_Descriptor_Ptr :=
        Unwrapped_Type_Desc (Type_Desc);  --  Unwrap the type desc
   begin
      --  Should never get here with an abstract type.
      pragma Assert (not U_Type_Desc.Is_Abstract);

      if U_Type_Desc.Is_Small then
         case U_Type_Desc.Type_Kind is
            when Univ_String_Kind | Univ_Enum_Kind =>
               --  Need to write out a string
               Strings.U_String'Write
                 (Stream, To_U_String (Object));
            when others =>
               --  The easy case
               Word_Type'Write (Stream, Object);
         end case;
         return;
      end if;

      if Is_Large_Null (Object) then
         --  Length of zero means a null object
         Offset_Within_Area'Write (Stream, 0);
         return;
      end if;

      if U_Type_Desc.Type_Kind = Univ_String_Kind then
         --  Univ_String; use a representation that can be read
         --  by U_String'Read.
         declare
            use Univ_Strings;
            Univ_Str : constant Univ_String := From_Word_Type (Object);
            Str_Len : constant Natural := Length (Univ_Str);
         begin
            if not Is_Special_Large_Value (Object)
              or else Str_Len <= Max_Short_String
            then
               --  Just write out short strings and large-rep strings
               Integer'Write (Stream, Str_Len);
               String'Write (Stream, To_String (Univ_Str));
            else
               --  Extract U_String and write its index
               Strings.U_String'Write
                 (Stream, To_U_String (Univ_Str));
            end if;
            return;
         end;
      end if;

      --  Shouldn't be any "special" large values left.
      pragma Assert (not Is_Special_Large_Value (Object));

      --  A non-null large object
      --  Start with the length
      Len := Large_Obj_Size (Object);
      Offset_Within_Area'Write (Stream, Len);

      if U_Type_Desc.Type_Kind = Basic_Array_Kind then

         --  Stream out the array components

         declare
            Arr_Comp_Type : constant Type_Descriptor_Ptr :=
              Basic_Array_Comp_Type (U_Type_Desc);

            --  Compute the array length from the obj length
            Arr_Len : constant Offset_Within_Area :=
              Len - Large_Obj_Header_Size - 1;

            --  Computed length should equal stored length
            pragma Assert (Fetch_Word (Object + Large_Obj_Header_Size) =
              Word_Type (Arr_Len));

         begin

            --  Stream out the components
            for I in 1 .. Arr_Len loop
               declare
                  Offset : constant Offset_Within_Area :=
                    Large_Obj_Header_Size + I;
                  Component : constant Word_Type :=
                    Fetch_Word (Object + Offset);
               begin
                  --  Stream out the component
                  PSVM_Object_Write (Stream, Arr_Comp_Type, Component);
               end;
            end loop;
         end;
      else
         --  Stream out the normal components
         if U_Type_Desc.Is_Polymorphic then
            --  Get the "true" (polymorphic) type of the object
            U_Type_Desc := Large_Obj_Type_Desc (Object);
            pragma Assert (U_Type_Desc.Is_Polymorphic);
            pragma Assert (not U_Type_Desc.Root_Type_Desc.Is_Abstract);
         end if;

         for I in 1 .. U_Type_Desc.Num_Components loop
            declare
               Comp_Type : constant Non_Op_Map_Type_Ptr :=
                 Skip_Over_Op_Map
                    (U_Type_Desc.Components (I).Type_Desc);
               Offset : constant Offset_Within_Area :=
                 Large_Obj_Header_Size + Offset_Within_Area (I - 1);
               Component : constant Word_Type :=
                 Fetch_Word (Object + Offset);
            begin
               --  Stream out the component
               if U_Type_Desc.Is_Polymorphic then
                  --  Precede value by its type if enclosing object
                  --  is polymorphic.
                  PSVM_Object_Output (Stream, Comp_Type, Component);
               else
                  PSVM_Object_Write (Stream, Comp_Type, Component);
               end if;
            end;
         end loop;
      end if;
   end PSVM_Object_Write;

   --------------------------------
   -- Raise_Exception_Occurrence --
   --------------------------------

   procedure Raise_Exception_Occurrence
     (Context : in out Exec_Context;
      Excep_Obj : in out Word_Type) is
      --  Raise exception given the exception object
      --  The exception object is moved to the handler's stg-rgn.

      Excep_Type_Desc : constant Type_Descriptor_Ptr :=
        Large_Obj_Type_Desc (Excep_Obj);
      Cur_Tcb      : constant Word_Ptr := Context.Control_Area;
      Excep_Master_Index : Master_Index :=
        Index_Of_Master (Tcb_Master_Ptr (Cur_Tcb));
      Handler_Kind : Handler_Kind_Enum := Not_A_Handler;
   begin
      --  Find innermost enclosing master that has a handler.
      Messages.Put_Warning
        ("raising exception ",
         Src_Pos => Execution_Source_Pos);

      Dump_Obj (Excep_Obj);

      while Excep_Master_Index /= 0 loop
         declare
            Excep_Master_Extra : Master_Extra_Rec
              renames Master_Extras (Excep_Master_Index);
            Excep_Master_Addr  : constant Word_Ptr :=
              Excep_Master_Extra.Master_Address;
         begin
            Handler_Kind := Excep_Master_Extra.Handler_Info.Kind;
            exit when Handler_Kind /= Not_A_Handler;
               --  Found a handler

            Excep_Master_Index := Excep_Master_Extra.Enclosing_Master;
         end;
      end loop;

      if Excep_Master_Index = 0 then
         Messages.Put_Warning
           ("no handler found!",
            Src_Pos => Execution_Source_Pos);
      elsif Prepare_To_Exit
                 (Master_Extras (Excep_Master_Index).Master_Address,
                  Context.Server_Index,
                  Large_Obj_Lock_Obj (Cur_Tcb),
                  Exiting_Tcb => Cur_Tcb,
                  Raising_Excep => True)
      then
         --  OK, we have the master and we successfully won the "race"
         --  to raise the exception (not much of a race, actually).
         Messages.Put_Warning
           ("handler found!",
            Src_Pos => Execution_Source_Pos);

         --  Move the exception occurrence to the region of the master.
         declare
            Excep_Master_Extra : Master_Extra_Rec
              renames Master_Extras (Excep_Master_Index);
            Excep_Master_Addr  : constant Word_Ptr :=
              Excep_Master_Extra.Master_Address;
            Excep_Obj_Copy : aliased Word_Type := Excep_Obj;
         begin
            Excep_Master_Extra.Excep_Occurrence := Null_For_Stg_Rgn
               (Stg_Rgn_Of_Large_Obj (Excep_Master_Addr));
            Move_Object (Context, Excep_Type_Desc,
                         LHS_Ptr => Excep_Master_Extra.Excep_Occurrence'Access,
                         RHS_Ptr => Excep_Obj_Copy'Unchecked_Access);

            --  Set original exception object to null, since it was moved.
            Excep_Obj := Excep_Obj_Copy;
         end;
         --  On return, we will exit the calling function
         --  and ultimately terminate the thread.  When the
         --  master is awaited, it will notice the presence
         --  of a non-null Excep_Occurrence, and pass it to
         --  the handler.
         if Handler_Kind = Compiled_Handler then
            --  If this is compiled code, we cannot rely on a check
            --  for Tcb_Exit_Requested, so we raise this exception which
            --  is caught in the code that invoked the code covered
            --  by the compiled handler.
            Messages.Put_Warning
              ("About to raise Propagate_To_Compiled_Handler in " &
               Hex_Image (Cur_Tcb),
               Src_Pos => Execution_Source_Pos (Context.Server_Index));
            raise Propagate_To_Compiled_Handler;
         end if;
      else
         Messages.Put_Warning
           ("exception not raised!",
            Src_Pos => Execution_Source_Pos);
      end if;

   end Raise_Exception_Occurrence;

   ----------------
   -- Real_Image --
   ----------------

   function Real_Image (Val : Univ_Real) return String is
   begin
      return Debug.Real_Image (Val);
   end Real_Image;

   ----------------
   -- Real_Value --
   ----------------

   function Real_Value (Img : String) return Univ_Real is
   begin
      return Debug.Real_Value (Img);
   end Real_Value;

   -------------------------
   -- Reconstruct_Strings --
   -------------------------

   procedure Reconstruct_Strings
     (Num_Entries : Stream_Local_Count;
      Stream_Rep_Table : Stream_Rep_Table_Type;
      String_Table  : out String_Table_Type) is
   --  Reconstruct Univ_String values from their stream representations

      use String_Streams;
   begin
      if Debug_Type_Descs then
         Ada.Text_IO.Put_Line ("Reconstructing" & Stream_Local_Count'Image
           (Num_Entries) & " string entries");
      end if;
      for I in 1 .. Num_Entries loop
         declare
            Local_Stream : aliased Buffered_Reader (Stream_Rep_Table (I));
         begin
            String_Table (Per_File_Strings.Local_String_Index (I)) :=
              Per_File_Strings.String_As_Word_Type
                (Strings.U_String_Index'Input (Local_Stream'Access));
         end;
      end loop;
   end Reconstruct_Strings;

   -----------------------
   -- Reconstruct_Value --
   -----------------------

   function Reconstruct_Value (Stream_Rep : Stream_Rep_Ptr;
     String_Table : String_Table_Ptr) return Word_Type is
   --  Reconstruct a ParaSail value given a stream representation
   --  and a string table.
      use String_Streams;
      Local_Stream : aliased Per_File_Strings.Buffered_Reader_With_Strings
                               (Stream_Rep, String_Table);
      Result : Word_Type := Null_Value;
      Type_Desc : Type_Descriptor_Ptr := null;
   begin
      Initialize_Global_Data_Stg_Rgn;

      PSVM_Object_Input (Local_Stream'Access, Type_Desc, Result,
        Stg_Rgn => Global_Data_Stg_Rgn);

      return Result;
   end Reconstruct_Value;

   ----------------------------------
   -- Register_Compiled_Operations --
   ----------------------------------

   procedure Register_Compiled_Operations
     (Num_Operations  : Local_Operation_Count;
      Operation_Ids   : Operation_Id_Table;
      Operation_Addrs : Operation_Address_Table;
      Operation_Conv_Descs : Operation_Conv_Desc_Table;
      String_Tab      : String_Table_Ptr;
      Num_Internal_Preconds  : Natural;
      Internal_Precond_Ops   : Local_Op_Index_List;
      Internal_Precond_Addrs : Internal_Precond_Addr_List) is
   --  Register a set of compiled operations
      Internal_Precond_Index : Natural := 1;
   begin
      for I in 1 .. Num_Operations loop
         declare
            use Id_To_Routine_Maps;
            use type Per_File_Strings.Local_String_Index_Base;
            Local_Op_Id  : Operation_Id renames Operation_Ids (I);
            Global_Op_Id : constant Global_Operation_Id :=
             (Module_Name =>
                Strings.U_String_Index (String_Tab (Local_Op_Id.Module_Name)),
              Operation_Name =>
                Strings.U_String_Index
                  (String_Tab (abs Local_Op_Id.Operation_Name)),
              Is_Builtin => Local_Op_Id.Operation_Name < 0);
            Compiled_Routine : constant Routine_RW_Ptr :=
              Get_Routine_By_Global_Id (Global_Op_Id);
         begin
            --  Make sure it was marked "compiled" and not PSVM
            pragma Assert (not Compiled_Routine.Is_PSVM_Routine);
            pragma Assert (Compiled_Routine.Is_Compiled_Routine);

            if Compiled_Routine.Routine_Addr /= null
              and then Compiled_Routine.Routine_Addr /=
                         Operation_Addrs (I)
            then
               --  Mismatched definitions of operation
               Messages.Put_Warning ("Non-matching duplicate definitions of "
                 & Strings.To_String (Strings.To_U_String
                   (Global_Op_Id.Module_Name)) & "::" &
                     Strings.To_String (Strings.To_U_String
                       (Global_Op_Id.Operation_Name)),
                 Src_Pos => Source_Positions.Null_Source_Position);
            end if;

            --  Fill in Address of operation, convention,
            --  convention descriptor, and Uses_Queuing flag
            Compiled_Routine.Routine_Addr := Operation_Addrs (I);
            Compiled_Routine.Conv_Desc := Operation_Conv_Descs (I);
            Compiled_Routine.Convention :=
              Convention (Compiled_Routine.Conv_Desc);
            Compiled_Routine.Uses_Queuing :=
              Uses_Queuing (Compiled_Routine.Conv_Desc);

            --  Fill in Internal_Precond_Addr, if any
            if Internal_Precond_Index <= Num_Internal_Preconds
              and then I = Internal_Precond_Ops (Internal_Precond_Index)
            then
               Compiled_Routine.Internal_Precond_Addr :=
                 Internal_Precond_Addrs (Internal_Precond_Index);
               Internal_Precond_Index := Internal_Precond_Index + 1;
            end if;
         end;
      end loop;

      --  Should have used up all of the internal preconditions.
      pragma Assert (Internal_Precond_Index = Num_Internal_Preconds + 1);
   end Register_Compiled_Operations;

   ---------------------
   -- Release_Stg_Rgn --
   ---------------------

   procedure Release_Stg_Rgn (Stg_Rgn : in out Stg_Rgn_Ptr) is
   --  Release all chunks of region back to enclosing region,
   --  or back to master list of free chunks.
   --  Reset Last_In_Use, Mark, and Depth_Of_Mark as appropriate.
   --  Put region on free list, and null out Stg_Rgn parameter.

      Old_First    : constant Stg_Rgn_Chunk_Ptr := Stg_Rgn.First_Chunk;
      Chunk_Ptr    : Stg_Rgn_Chunk_Ptr := Old_First;

      Server_Index : constant Thread_Server_Index := Stg_Rgn.Owning_Server;
      Info         : Server_Info renames Server_Info_Array (Server_Index);

      pragma Assert (Stg_Rgn /= Info.Free_Stg_Rgns);
   begin
      if Debug_Stg_Rgns then
         Put_Line
           (" Server" & Thread_Server_Index'Image (Server_Index) &
            " is releasing region #" & Stg_Rgn_Index'Image (Stg_Rgn.Index));
      end if;

      --  Release all chunks of region back to enclosing region,
      --  or back to master list of free chunks.
      --  Reset Last_In_Use, Mark, and Depth_Of_Mark as appropriate.

      if Old_First /= null then
         loop
            declare
               Next : constant Stg_Rgn_Chunk_Ptr := Chunk_Ptr.Next_Chunk;
            begin
               --  Release to enclosing region, or server's free list
               Release_Stg_Rgn_Chunk
                 (Chunk_Ptr, Server_Index, Stg_Rgn.Enclosing_Stg_Rgn);

               exit when Next = Old_First;
               Chunk_Ptr := Next;
            end;
         end loop;
      end if;

      --  Null out region
      Stg_Rgn.First_Chunk := null;
      Stg_Rgn.Reclamation := null;
      Stg_Rgn.Associated_Local_Area := null;

      if Stg_Rgn.Shared_Part /= null then
         --  Free associated shared storage region
         Release_Stg_Rgn (Stg_Rgn.Shared_Part);

         --  Restore innermost-stg-rgn pointer.
         Info.Innermost_Stg_Rgn := Stg_Rgn.Enclosing_Stg_Rgn;

         --  Bump generation number
         Stg_Rgn.Gen_Num := Stg_Rgn.Gen_Num + 1;
         Stg_Rgn.Shared_Part.Gen_Num := Stg_Rgn.Gen_Num;

         --  Add region to free list
         Stg_Rgn.Enclosing_Stg_Rgn := Info.Free_Stg_Rgns;
         Info.Free_Stg_Rgns := Stg_Rgn;

         --  Null out caller's pointer
         Stg_Rgn := null;
      end if;
   end Release_Stg_Rgn;

   ---------------------------
   -- Release_Stg_Rgn_Chunk --
   ---------------------------

   procedure Release_Stg_Rgn_Chunk
     (Chunk             : in out Stg_Rgn_Chunk_Ptr;
      Server_Index      : Thread_Server_Index;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr := null) is
   begin
      if Enclosing_Stg_Rgn /= null then
         --  Restore last in use from mark
         Chunk.Last_In_Use := Chunk.Mark;
         if Chunk.Depth_Of_Mark > 0 then
            --  Just decrement depth counter
            Chunk.Depth_Of_Mark := Chunk.Depth_Of_Mark - 1;
         else
            --  Pop old mark/depth-of-mark off of chunk.
            Chunk.Mark :=
              Offset_Within_Chunk (Content_Of_Address
                                      ((Chunk, Chunk.Last_In_Use)));
            Chunk.Last_In_Use := Chunk.Last_In_Use - 1;
            Chunk.Depth_Of_Mark :=
              Natural (Content_Of_Address ((Chunk, Chunk.Last_In_Use)));
            Chunk.Last_In_Use := Chunk.Last_In_Use - 1;
         end if;

         --  Compute Space_Left in chunk
         Chunk.Space_Left := Chunk.Chunk_Length - Chunk.Last_In_Use;

         pragma Assert (Chunk.Last_In_Use > 0 or else Chunk.Depth_Of_Mark > 0);

         if Debug_Stg_Rgns then
            Put_Line
              (" Cut back chunk " &
               Chunk_Index'Image (Chunk.Index) &
               " to" &
               Offset_Within_Chunk'Image (Chunk.Last_In_Use) &
               " and release to enclosing region");
         end if;

         --  Add to chunk list associated with Enclosing_Stg_Rgn
         if Enclosing_Stg_Rgn.Manager /= null then
            --  Shared region chunk
            Enclosing_Stg_Rgn.Manager.Return_Stg_Rgn_Chunk
              (Chunk, Enclosing_Stg_Rgn);
         else
            --  Unshared region chunk
            Return_Unshared_Stg_Rgn_Chunk (Chunk, Enclosing_Stg_Rgn);
         end if;
      else
         --  Add to front of server's (unshared) free-chunk list.
         declare
            Info : Server_Info renames Server_Info_Array (Server_Index);
         begin
            if Debug_Stg_Rgns then
               Put_Line
                 (" Server" & Thread_Server_Index'Image (Server_Index) &
                  " is freeing chunk " &
                  Chunk_Index'Image (Chunk.Index));
            end if;
            Chunk.Next_Chunk := Info.Free_Rgn_Chunks;
            Info.Free_Rgn_Chunks := Chunk;
         end;
      end if;

      --  Null out caller's pointer
      Chunk := null;
   end Release_Stg_Rgn_Chunk;

   -----------------------------------
   -- Replace_Special_Value_Stg_Rgn --
   -----------------------------------

   function Replace_Special_Value_Stg_Rgn
     (Val : Word_Type; New_Rgn : Stg_Rgn_Ptr)
     return Word_Type is
      --  Return special value, with region index replaced with that of
      --  newly specified region.
      Old_Rgn_Index : constant Stg_Rgn_Index := Large_Obj_Stg_Rgn_Index (Val);
   begin
      return Val + Word_Type (New_Rgn.Index - Old_Rgn_Index) * 2;
   end Replace_Special_Value_Stg_Rgn;

   -----------------------------------
   -- Return_Unshared_Stg_Rgn_Chunk --
   -----------------------------------

   procedure Return_Unshared_Stg_Rgn_Chunk
     (Chunk             : Stg_Rgn_Chunk_Ptr;
      Enclosing_Stg_Rgn : Stg_Rgn_Ptr) is
   --  Return chunk back to enclosing region
   --  Caller must get a lock, if needed.
      pragma Assert (Enclosing_Stg_Rgn /= null);
      Old_First : constant Stg_Rgn_Chunk_Ptr := Enclosing_Stg_Rgn.First_Chunk;
   begin
      --  Now associated with enclosing region
      Chunk.Associated_Stg_Rgn := Enclosing_Stg_Rgn;

      --  Hook into front of list of enclosing region
      if Old_First = null then
         --  This is the only chunk in this region
         Chunk.Next_Chunk := Chunk;
         Chunk.Prev_Chunk := Chunk;
      else
         --  Link into circular list in front of Old_First.
         Chunk.Next_Chunk := Old_First;
         Chunk.Prev_Chunk := Old_First.Prev_Chunk;
         Old_First.Prev_Chunk.Next_Chunk := Chunk;
         Old_First.Prev_Chunk := Chunk;
      end if;
      --  Returned chunk becomes first chunk of enclosing region
      Enclosing_Stg_Rgn.First_Chunk := Chunk;
   end Return_Unshared_Stg_Rgn_Chunk;

   ---------------------
   -- Runtime_Message --
   ---------------------

   procedure Runtime_Message
     (Context : in out Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   --  Print a message as part of some kind of runtime failure or warning.
   --  Print value of Param 0 as a string and (someday) give a traceback.
      Msg : constant Word_Type := Fetch_Word (Params, 0);
   begin
      if Msg = Null_Value then
         Ada.Text_IO.Put (Ada.Text_IO.Current_Error, "[null runtime message]");
      else
         --  Convert U_String_Index to String
         Ada.Text_IO.Put
           (Ada.Text_IO.Current_Error, Enum_Word_To_String (Msg));
      end if;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, " Continuing.");
   end Runtime_Message;

   -----------------------------
   -- Set_Large_Obj_Type_Info --
   -----------------------------

   procedure Set_Large_Obj_Type_Info
     (Large_Obj_Addr : Object_Virtual_Address;
      Type_Id        : Type_Index)
     renames Large_Obj_Header_Ops.Set_Large_Obj_Type_Info;

   ----------------------------
   -- Set_Large_Obj_Lock_Obj --
   ----------------------------

   procedure Set_Large_Obj_Lock_Obj
     (Large_Obj_Addr : Object_Virtual_Address;
      Lock_Obj       : Lock_Obj_Index)
     renames Large_Obj_Header_Ops.Set_Large_Obj_Lock_Obj;

   -------------------------
   -- Set_Run_Time_Checks --
   -------------------------

   procedure Set_Run_Time_Checks (On : Boolean) is
   begin
      Doing_Run_Time_Checks := On;
   end Set_Run_Time_Checks;

   ----------------------
   -- Set_Server_Count --
   ----------------------

   procedure Set_Server_Count (Max_Servers : Natural) is
   --  Set maximum number of servers to be used by the interpreter.
      New_Max : Natural := Integer'Max (0,
        Max_Servers - Num_Initial_Thread_Servers - 1);
   begin
      Thread_Manager.Set_Max_Dynamically_Allocated_Servers (New_Max);
   end Set_Server_Count;

   ------------------------------
   -- Set_Stg_Rgn_Of_Large_Obj --
   ------------------------------

   procedure Set_Stg_Rgn_Of_Large_Obj
     (New_Obj : Word_Type;
      Stg_Rgn : Stg_Rgn_Ptr) is
   begin
      Set_Large_Obj_Stg_Rgn_Index (New_Obj, Stg_Rgn.Index);
   end Set_Stg_Rgn_Of_Large_Obj;

   ------------------------
   -- Set_Tcb_Was_Queued --
   ------------------------

   procedure Set_Tcb_Was_Queued
     (Tcb_Addr   : Word_Ptr;
      Was_Queued : Boolean) is
   begin
      Thread_Scheduling.Set_Tcb_Was_Queued (Tcb_Addr, Was_Queued);
   end Set_Tcb_Was_Queued;

   ------------------
   -- Show_Servers --
   ------------------

   procedure Show_Servers is
   --  Show number of servers and other information about servers.
   begin
      Put_Line
        (" Num_Initial_Thread_Servers :" &
         Natural'Image (Num_Initial_Thread_Servers) & " + 1");
      Put_Line
        (" Num_Dynamically_Allocated_Thread_Servers :" &
         Natural'Image (Num_Dynamically_Allocated_Thread_Servers));
      Put_Line
        (" Max_Dynamically_Allocated_Thread_Servers :" &
         Natural'Image (Max_Dynamically_Allocated_Thread_Servers));
   end Show_Servers;

   ----------------
   -- Show_Stats --
   ----------------

   procedure Show_Stats (Clear : Boolean := False) is
      Total_Allocs_By_Owner    : constant Longest_Natural :=
                                   Longest_Natural (Stg_Rgn_Stats (True, False)
                                     + Stg_Rgn_Stats (True, True));
      Total_Allocs_By_Nonowner : constant Longest_Natural :=
                                   Longest_Natural
                                     (Stg_Rgn_Stats (False, False)
                                     + Stg_Rgn_Stats (False, True));

      --  Avoid divide by zero
      Total_Allocs : constant Longest_Natural :=
                       Longest_Natural'Max
                         (1, Total_Allocs_By_Owner + Total_Allocs_By_Nonowner);

      Total_Unshared_Thread_Initiations    : Longest_Natural := 0;
      Total_Bypassed_Thread_Initiations    : Longest_Natural := 0;
      Overall_Max_Waiting_Unshared_Threads : Thread_Count_Base := 0;
      Overall_Summed_Unshared_Waiting      : Longest_Natural := 0;
   begin
      --  Aggregate the statistics over the servers
      for I in 1 .. Last_Server_Index loop
         declare
            Info : Server_Info renames Server_Info_Array (I);
         begin
            Total_Unshared_Thread_Initiations :=
              Total_Unshared_Thread_Initiations +
                Longest_Natural (Info.Num_Unshared_Thread_Initiations);

            Total_Bypassed_Thread_Initiations :=
              Total_Bypassed_Thread_Initiations +
                Longest_Natural (Info.Num_Bypassed_Thread_Initiations);

            if Info.Max_Waiting_Unshared_Threads >
              Overall_Max_Waiting_Unshared_Threads
            then
               Overall_Max_Waiting_Unshared_Threads :=
                 Info.Max_Waiting_Unshared_Threads;
            end if;

            Overall_Summed_Unshared_Waiting :=
              Overall_Summed_Unshared_Waiting +
                Info.Num_Waiting_Unshared_Summed;
         end;
      end loop;

      New_Line;
      Put_Line ("Univ_Integer Statistics:");
      Univ_Integers.Dump_Stats;

      New_Line;
      Put_Line ("Stg_Rgn Statistics:");
      Put_Line (" New allocations by owner:      " &
        Natural'Image (Stg_Rgn_Stats (True, False)) &
        "  =" & Longest_Natural'Image
          (100 * Longest_Natural (Stg_Rgn_Stats (True, False)) / Total_Allocs)
        & "%");
      Put_Line (" Re-allocations by owner:       " &
        Natural'Image (Stg_Rgn_Stats (True, True)) &
        "  =" & Longest_Natural'Image
           (100 * Longest_Natural (Stg_Rgn_Stats (True, True)) / Total_Allocs)
        & "%");
      Put_Line (" Total allocations by owner:    " &
        Longest_Natural'Image (Total_Allocs_By_Owner) &
        "  =" & Longest_Natural'Image
           (100 * Total_Allocs_By_Owner / Total_Allocs)
        & "%");
      New_Line;
      Put_Line (" New allocations by non-owner:  " &
        Natural'Image (Stg_Rgn_Stats (False, False)) & "  =" &
        Longest_Natural'Image
          (100 * Longest_Natural (Stg_Rgn_Stats (False, False)) / Total_Allocs)
        & "%");
      Put_Line (" Re-allocations by non-owner:   " &
        Natural'Image (Stg_Rgn_Stats (False, True)) &
        "  =" & Longest_Natural'Image
          (100 * Longest_Natural (Stg_Rgn_Stats (False, True)) / Total_Allocs)
        & "%");
      Put_Line (" Total allocations by non-owner:" &
        Longest_Natural'Image (Total_Allocs_By_Nonowner) &
        "  =" & Longest_Natural'Image
           (100 * Total_Allocs_By_Nonowner / Total_Allocs)
        & "%");
      New_Line;
      Put_Line (" Total allocations:             " &
        Longest_Natural'Image (Total_Allocs));

      New_Line;
      Put_Line ("Threading Statistics:");
      Put_Line
        (" Num_Initial_Thread_Servers :" &
         Natural'Image (Num_Initial_Thread_Servers) & " + 1");
      Put_Line
        (" Num_Dynamically_Allocated_Thread_Servers :" &
         Natural'Image (Num_Dynamically_Allocated_Thread_Servers));

      if Num_Shared_Thread_Initiations = 0 then
         --  Prevent a divide-by-zero error if statistics reset
         Num_Shared_Thread_Initiations := 1;
      end if;

      if Total_Unshared_Thread_Initiations = 0 then
         --  Prevent a divide-by-zero error if statistics reset
         Total_Unshared_Thread_Initiations := 1;
      end if;

      declare
         Average_Shared_Waiting : constant Long_Float :=
           Long_Float (Num_Waiting_Shared_Summed) /
             Long_Float (Num_Shared_Thread_Initiations);
         Average_Unshared_Waiting : constant Long_Float :=
           Long_Float (Overall_Summed_Unshared_Waiting) /
             Long_Float (Total_Unshared_Thread_Initiations);
         Average_Active : constant Long_Float :=
           Long_Float (Num_Active_Summed_Over_Initiations) /
             Long_Float (Num_Shared_Thread_Initiations);
      begin
         Put_Line
           (" Max_Waiting_Shared_Threads (on all servers' queues):" &
            Thread_Count'Image (Max_Waiting_Shared_Threads));
         Put (" Average waiting shared threads: ");
         Ada.Long_Float_Text_IO.Put (Average_Shared_Waiting,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put_Line
           (" Max_Waiting_Unshared_Threads (on any one server's queue):" &
            Thread_Count'Image (Overall_Max_Waiting_Unshared_Threads));
         Put (" Average waiting unshared threads: ");
         Ada.Long_Float_Text_IO.Put (Average_Unshared_Waiting,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
         Put_Line (" Max_Active (threads):" & Natural'Image (Max_Active));
         Put (" Average active threads: ");
         Ada.Long_Float_Text_IO.Put (Average_Active,
           Fore => 1, Aft => 2, Exp => 0);
         New_Line;
      end;

      Put_Line
        (" Max_Active_Masters :" & Natural'Image (Max_Active_Masters));
      Put_Line
        (" Max_Subthreads_Per_Master :" &
         Thread_Count'Image (Max_Subthreads_Per_Master));
      Put_Line
        (" Max_Waiting_For_Subthreads :" &
         Natural'Image (Max_Waiting_For_Subthreads));
      Put_Line (" Max_Servers_Waiting_For_Masters:" &
        Natural'Image (Max_Servers_Waiting_For_Masters));
      Put_Line
        (" Num_Masters :" &
         Natural'Image (Num_Masters) & " (Shared :" &
         Natural'Image (Num_Shared_Masters) & ")");
      Put_Line
        (" Num_Thread_Steals :" &
         Natural'Image (Num_Thread_Steals) & " out of" &
         Longest_Natural'Image (Total_Unshared_Thread_Initiations) &
         " +" & Natural'Image (Num_Shared_Thread_Initiations) &
         " (U+S) thread initiations =" &
         Longest_Natural'Image (Longest_Natural (Num_Thread_Steals) * 100 /
           (Total_Unshared_Thread_Initiations +
             Longest_Natural (Num_Shared_Thread_Initiations)))
         & "%");
      Put_Line
        (" Num_Bypassed_Thread_Initiations :" &
           Longest_Natural'Image (Total_Bypassed_Thread_Initiations) &
           " (" &
           Longest_Natural'Image (Total_Bypassed_Thread_Initiations * 100 /
             (Total_Bypassed_Thread_Initiations +
              Total_Unshared_Thread_Initiations +
              Longest_Natural (Num_Shared_Thread_Initiations))) & "%)");
      New_Line;

      if Clear then
         --  Reset statistics
         Stg_Rgn_Stats (True, True) := 0;
         Stg_Rgn_Stats (True, False) := 0;
         Stg_Rgn_Stats (False, True) := 0;
         Stg_Rgn_Stats (False, False) := 0;

         Max_Waiting_Shared_Threads := 0;
         if Num_Active > 0 then
            Max_Active := Num_Active;
         else
            Max_Active := 0;
         end if;
         Num_Waiting_Shared_Summed := 0;
         Num_Shared_Thread_Initiations := 0;
         Num_Active_Summed_Over_Initiations := 0;
         Max_Waiting_For_Subthreads := Num_Waiting_For_Subthreads;
         if Num_Active_Masters > 0 then
            Max_Active_Masters := Num_Active_Masters;
         else
            Max_Active_Masters := 0;
         end if;
         Num_Masters := 0;
         Num_Shared_Masters := 0;
         Num_Thread_Steals := 0;
         Max_Subthreads_Per_Master := 0;

         --  Reset the statistics for each server
         for I in 1 .. Last_Server_Index loop
            declare
               Info : Server_Info renames Server_Info_Array (I);
            begin
               Info.Num_Unshared_Thread_Initiations := 0;
               Info.Num_Bypassed_Thread_Initiations := 0;
               Info.Max_Waiting_Unshared_Threads    := 0;
               Info.Num_Waiting_Unshared_Summed     := 0;
            end;
         end loop;

      end if;
   end Show_Stats;

   ------------------------------
   -- Shut_Down_Thread_Servers --
   ------------------------------

   procedure Shut_Down_Thread_Servers (Total_Errors : Natural) is
      use Ada.Text_IO;
   begin
      --  Just pass the buck to the thread manager
      Thread_Manager.Shut_Down;

      --  Now shut down the delay server
      Delay_Queue.Shut_Down;

      if Debug_Statistics and then Total_Errors = 0 then
         --  Display some statistics
         Show_Stats (Clear => True);
      end if;

      if Total_Errors > 0 then
         --  Set exit status
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Shut_Down_Thread_Servers;

   -------------------------
   -- Spawn_Parallel_Thread --
   -------------------------

   procedure Spawn_Parallel_Thread
     (Context      : in out Exec_Context;
      Master_Addr  : Word_Ptr;
      New_Tcb      : Word_Ptr;
      Static_Link  : Word_Ptr;
      Routine      : Routine_Ptr;
      Tcb_Is_Local : Boolean;   -- Whether tcb address is in Local_Area
      Is_Start_Op  : Boolean;   -- Whether is Start_Par vs. Add_Par
      Nested_Block : Code_Block_Ptr := null;
      Start_Pc     : Code_Index := Code_Index'First;
      Locked_Param_Info   : Locked_Param_Info_Type :=
                            Null_Locked_Param_Info;
      Master_Never_Shared : Boolean := False)
      --  Execute the Start/Add_Parallel_[Call_]Op instructions
      --  Nested_Block is null if this is invoked for compiled code
      --  or is a Start/Add_Parallel_Call_Op.
      --  Master_Never_Shared is True if this master should be executed
      --  all on the same server.
   is
      Spawning_Tcb : constant Word_Ptr :=
        Context.Control_Area;
      Current_Lock_Index : Lock_Obj_Index := 0;
   begin  --  Spawn_Parallel_Thread

      if Is_Start_Op then
         --  Initialize Master if Start_Parallel_Op
         Initialize_Master (Master_Addr, Context.Local_Stg_Rgn.Index,
           Context.Server_Index, Spawning_Tcb => Spawning_Tcb,
           Never_Shared => Master_Never_Shared);

         --  There should not already be an open master
         pragma Assert (Context.Open_Master = null);
         Context.Open_Master := Master_Addr;
      end if;

      if Spawning_Tcb /= null then
         Current_Lock_Index := Large_Obj_Lock_Obj (Spawning_Tcb);
      end if;

      if New_Tcb = null then
         --  Null New_TCB means this is an exception handler
         Debug_Threading := True;   --  stt:tbd
         Initialize_Handler (Master_Addr, Routine, Nested_Block, Start_Pc);
         return;
      end if;

      --  Initialize control area
      Initialize_Tcb
        (New_Tcb,
         Tcb_Is_Local => Tcb_Is_Local,
         Routine => Routine,
         Static_Link => Static_Link,
         Locked_Param_Info => Locked_Param_Info,
         Current_Lock_Index => Current_Lock_Index,
         Nested_Block => Nested_Block,
         Start_Pc => Start_Pc);

      --  Now schedule the thread for execution
      Spawn_Thread
        (Context,
         Master_Addr,
         New_Tcb,
         Spawning_Tcb => Spawning_Tcb);
   exception
      when E : others =>
         Messages.Put_Warning
           ("Spawn_Parallel_Thread: " &
            Hex_Image (New_Tcb) &
            Ada.Exceptions.Exception_Name (E) &
            " raised.",
            Src_Pos => Execution_Source_Pos (Context.Server_Index));
         Put_Line (Ada.Exceptions.Exception_Information (E));
         raise;
   end Spawn_Parallel_Thread;

   -----------------------------
   -- Special_Value_Indicator --
   -----------------------------

   function Special_Value_Indicator (Value : Word_Type)
     return Indicator_Type is
   --  Return Indicator "buried" in special value of Univ_String, Univ_Integer

      function W2I is
        new Ada.Unchecked_Conversion (Word_Type, Indicator_Type);

   begin

      return 2**8 - (W2I (Value) / Indicator_Multiplier);
   end Special_Value_Indicator;

   -----------------------------
   -- Start_Up_Thread_Servers --
   -----------------------------

   type Thread_Server_Ptr is access Thread_Server;

   Thread_Servers : array (1 ..
                           Num_Initial_Thread_Servers) of Thread_Server_Ptr;
   --  These are created in Start_Up_Thread_Servers.

   Thread_Servers_Started : Boolean := False;

   procedure Start_Up_Thread_Servers is
   --  Start up the default number of thread servers
      Server_Creator_Instance : Server_Creator_Ptr;
      Delay_Server_Instance : Delay_Server_Ptr;
   begin
      if Thread_Servers_Started then
         --  Already did this
         return;
      end if;

      --  Prevent multiple executions
      Thread_Servers_Started := True;

      --  Start up delay server
      Delay_Server_Instance := new Delay_Server;

      --  Start up initial thread servers
      for I in 1 .. Num_Initial_Thread_Servers loop
         Thread_Servers (I) := new Thread_Server;
      end loop;

      --  Now allow the creation of "dynamically allocated" thread servers
      Server_Creator_Instance := new Server_Creator;
      Server_Creator_Instance.Start;
   end Start_Up_Thread_Servers;

   --------------------------
   -- Stg_Rgn_Of_Large_Obj --
   --------------------------

   function Stg_Rgn_Of_Large_Obj_Exported
     (Existing_Obj : Word_Type) return Stg_Rgn_Ptr is
   begin
      return Stg_Rgn_Table (Large_Obj_Stg_Rgn_Index (Existing_Obj));
   end Stg_Rgn_Of_Large_Obj_Exported;

   function Stg_Rgn_Of_Large_Obj
     (Existing_Obj : Word_Ptr) return Stg_Rgn_Ptr is
   begin
      return Stg_Rgn_Table (Large_Obj_Stg_Rgn_Index (Existing_Obj));
   end Stg_Rgn_Of_Large_Obj;

   ----------------
   -- Store_Real --
   ----------------

   procedure Store_Real
     (Context      : Exec_Context;
      Real_Locator : Object_Locator;
      Value        : Univ_Real) is
   begin
      Store_Word (Context, Real_Locator, From_Univ_Real (Value));
   end Store_Real;

   procedure Store_Real (Address : Object_Address; Value : Univ_Real) is
   begin
      Store_Word (Address, From_Univ_Real (Value));
   end Store_Real;

   procedure Store_Real
     (Address : Object_Virtual_Address;
      Value   : Univ_Real) is
   begin
      Store_Word (Address, From_Univ_Real (Value));
   end Store_Real;

   procedure Store_Real
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Univ_Real) is
   --  Store value into Base[Offset]
   begin
      Store_Word (Base, Offset, From_Univ_Real (Value));
   end Store_Real;

   ----------------
   -- Store_Word --
   ----------------

   procedure Store_Word
     (Context      : Exec_Context;
      Word_Locator : Object_Locator;
      Value        : Word_Type)
   is
      Word_Address : constant Word_Ptr :=
                       Locator_To_Physical_Address (Context, Word_Locator);
   begin
      Store_Word (Word_Address, 0, Value);
   end Store_Word;

   procedure Store_Word
     (Address : Object_Address;
      Value   : Word_Type) is
   begin
      Address.Enclosing_Chunk.Data (Address.Offset) := Value;
   end Store_Word;

   procedure Store_Word
     (Address : Object_Virtual_Address;
      Value   : Word_Type)
   is
   begin
      Virtual_To_Physical_Address (Address).all := Value;
   end Store_Word;

   procedure Store_Word
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Type)
   is
   begin
      Add (Base, Offset).all := Value;
   end Store_Word;

   --------------------
   -- Store_Word_Ptr --
   --------------------

   procedure Store_Word_Ptr
     (Base    : Word_Ptr;
      Offset  : Offset_Within_Area;
      Value   : Word_Ptr)
   is
   begin
      Add (Base, Offset).all := Word_Ptr_To_Word (Value);
   end Store_Word_Ptr;

   ---------------------------
   -- Stream_To_Basic_Array --
   ---------------------------

   procedure Stream_To_Basic_Array
     (Stream : in out String_Streams.Buffered_Stream'Class;
      Univ_Int_Array_Type : Non_Op_Map_Type_Ptr;
      Target : in out Word_Type;
      Server_Index : Thread_Server_Index) is
   --  Convert buffered stream into a basic array, updating Target
   --  to refer to basic array.  Basic array is allocated in region
   --  of Target.
      use String_Streams;
      use Ada.Streams;
      Elems_Per_Word : constant Stream_Element_Count :=
        Word_Type'Size / Stream_Element'Size;
      Len : constant Stream_Element_Count := Stream_Length (Stream);
      Result : constant Object_Virtual_Address := Create_Basic_Array_Obj
        (Array_Type_Desc => Univ_Int_Array_Type,
         Array_Len       => 1 + Natural
                              ((Len + Elems_Per_Word - 1) / Elems_Per_Word),
         Stg_Rgn         => Stg_Rgn_Of_Large_Obj (Target),
         Server_Index    => Server_Index);
      Stream_Start : constant Word_Ptr :=
        Virtual_To_Physical_Address
          (Result + Large_Obj_Header_Size +
            Offset_Within_Area'(2));
      Stream_Arr : aliased Stream_Element_Array (1 .. Len);
      pragma Import (Ada, Stream_Arr);
      for Stream_Arr'Address use Stream_Start.all'Address;

      Stream_Last : Stream_Element_Count;
   begin
      --  Store length in first word of basic array
      Store_Word (Result + Large_Obj_Header_Size +
          Offset_Within_Area'(1), Word_Type (Len));

      --  Read contents of stream into basic array
      Read (Stream, Stream_Arr, Stream_Last);

      --  Last should match reported length
      pragma Assert (Stream_Last = Len);

      --  Release contents of stream
      Reset_Stream (Stream);

      --  Store back newly created basic array
      Target := Result;
   end Stream_To_Basic_Array;

   --------------------
   -- String_To_Word --
   --------------------

   function String_To_Word (Str : String;
                            Null_For_Rgn : Word_Type;
                            Server_Index : Thread_Server_Index)
     return Word_Type is
   --  Convert String to a Univ_String
      use Univ_Strings;
   begin
      return To_Word_Type (From_String (Str, Null_For_Rgn, Server_Index));
   end String_To_Word;

   ----------------------------
   -- Tcb_Call_Can_Be_Queued --
   ----------------------------

   function Tcb_Call_Can_Be_Queued
     (Tcb_Addr : Word_Ptr) return Boolean is
   begin
      return Thread_Scheduling.Tcb_Call_Can_Be_Queued (Tcb_Addr);
   end Tcb_Call_Can_Be_Queued;

   --------------------
   -- Tcb_Was_Queued --
   --------------------

   function Tcb_Was_Queued
     (Tcb_Addr : Word_Ptr) return Boolean is
   begin
      return Thread_Scheduling.Tcb_Was_Queued (Tcb_Addr);
   end Tcb_Was_Queued;

   -------------------------
   -- To_Univ_String_Null --
   -------------------------

   function To_Univ_String_Null (Obj_In_Rgn : Word_Type) return Word_Type is
   --  Return "null" for a Univ_String, given an object in the right region.
   begin
      return Univ_Strings.Null_Of_Same_Rgn
        (Univ_Strings.From_Word_Type (Obj_In_Rgn));
   end To_Univ_String_Null;

   -------------------------
   -- To_Univ_String_Word --
   -------------------------

   function To_Univ_String_Word
     (U_Str        : Strings.U_String;
      Null_For_Rgn : Word_Type) return Word_Type is
      --  Return a word representing a Univ_String, given a U_String
   begin
      return Univ_Strings.To_Word_Type (Univ_Strings.From_U_String
           (U_Str, Null_For_Rgn));
   end To_Univ_String_Word;

   function To_Univ_String_Word
     (Index        : Strings.U_String_Index;
      Null_For_Rgn : Word_Type) return Word_Type is
      --  Return a word representing a Univ_String, given a U_String_Index
   begin
      return Univ_Strings.To_Word_Type (Univ_Strings.From_U_String
           (Strings.To_U_String (Index), Null_For_Rgn));
   end To_Univ_String_Word;

   --------------------
   -- Unsigned_Image --
   --------------------

   function Unsigned_Image (Val : Unsigned_Word_Type) return String is
   --  Return Val as an image, removing leading blank from Ada 'image.
      Img : String renames Unsigned_Word_Type'Image (Val);
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Unsigned_Image;

   --------------------
   -- Unsigned_Value --
   --------------------

   function Unsigned_Value (Img : String) return Unsigned_Word_Type is
   --  Convert image to an unsigned value.
   --  Allow 0xFF, 0b01, 16#ff#, etc.
   begin
      if Img'Length > 2 and then Img (Img'First) = '0' then
         case Img (Img'First + 1) is
            when 'x' | 'X' =>
               --  Base 16
               return Unsigned_Word_Type'Value
                        ("16#" & Img (Img'First + 2 .. Img'Last) & '#');
            when 'b' | 'B' =>
               --  Base 2
               return Unsigned_Word_Type'Value
                        ("2#" & Img (Img'First + 2 .. Img'Last) & '#');
            when others =>
               null;
         end case;
      end if;

      return Unsigned_Word_Type'Value (Img);
   exception
      when others =>
         --  There is no "null" for unsigned so return something silly
         return 16#0BAD_FEED_DEAD_BEEF#;
   end Unsigned_Value;

   ------------------
   -- Uses_Queuing --
   ------------------

   function Uses_Queuing (Conv_Desc : Convention_Descriptor)
     return Boolean is
      --  Whether routine uses queuing internally
   begin
      return (Conv_Desc and
        2 ** (Conv_Width + Num_Inp_Width + Num_Out_Width)) /= 0;
   end Uses_Queuing;

   -------------------------------
   -- Virtual_To_Object_Address --
   -------------------------------

   function Virtual_To_Object_Address
     (Virtual : Object_Virtual_Address) return Object_Address is
   begin
      if Virt_Is_Phys then
         pragma Assert (False);
      end if;
      if Virtual < Chunk_Divisor then
         if Virtual <= 0 then
            --  Null address
            return Null_Object_Address;
         else
            --  "Absolute" address, only usable for Routine indices
            return
              (Enclosing_Chunk => null,
               Offset => Offset_Within_Chunk (Virtual));
         end if;

      else
         --  elsif Virtual <
         --    (Object_Virtual_Address (Chunk_Index'Last + 1) * Chunk_Divisor)
         --  then
         declare
            Index : constant Chunk_Index :=
              Chunk_Index (Virtual / Chunk_Divisor);
            Enclosing_Chunk : constant Stg_Rgn_Chunk_Ptr :=
              Chunk_Group_Table (Chunk_Group_Index
               (Index / Chunk_Group_Size))
                 (Index_In_Chunk_Group (Index rem Chunk_Group_Size));
                  --  Get chunk based on high bits of virtual address
         begin
            --  if Enclosing_Chunk = null then
            --     Messages.Put_RT_Error
            --       ("Internal: To_Object_Address: " &
            --        "Chunk #" & Chunk_Index'Image (Index) &
            --        " not initialized in Chunk_Table",
            --        Src_Pos => Execution_Source_Pos);
            --     raise Program_Error;
            --  end if;

            return
              (Enclosing_Chunk,
               Offset => Offset_Within_Chunk
                 (Virtual - Enclosing_Chunk.Starting_Virtual_Address));
            --  Subtract out the starting address to produce
            --  the offset within chunk.
         end;
      end if;

      --  else
      --     Messages.Put_RT_Error
      --       ("Internal: To_Object_Address: " &
      --        "Virtual Address " & Hex_Image (Virtual) &
      --        " out of range",
      --        Src_Pos => Execution_Source_Pos);
      --     raise Program_Error;
      --  end if;
   end Virtual_To_Object_Address;

   ---------------------------------
   -- Locator_To_Physical_Address --
   ---------------------------------

   function Locator_To_Physical_Address
     (Context : Exec_Context;
      Locator : Object_Locator) return Word_Ptr is
   begin
      case Locator.Base is
         when Local_Area =>
            --  Relative to current local area
            pragma Assert (Locator.Offset < Context.Start_Callee_Locals);
            return Add (Context.Local_Area, Locator.Offset);

         when Param_Area | Enclosing_Param_Areas'First =>
            --  Relative to param area of current operation
            return Add (Context.Params, Locator.Offset);

         when Phys_Base_Registers =>
            --  These require special handling as they
            --  contain physical addresses
            return Add (Fetch_Word_Ptr (Context, (Local_Area,
                          Offset_Within_Area
                            (Locator.Base - Phys_Base_Registers'First),
                          No_VM_Obj_Id)),
                        Locator.Offset);

         when Enclosing_Local_Areas                                         |
            --  Relative to a local area of an enclosing block or operation
            --  The first word of a local area points at its enclosing
            --  block, operation, or type area.
            --  Enclosing_Local_Areas'First is equivalent to Local_Area.
            --  Larger values correspond to dereferences through static links.
              Enclosing_Param_Areas'First + 1 .. Enclosing_Param_Areas'Last =>
            --  Relative to a param area of an enclosing operation
            --  The second word of a local area points at its param area
            --  if it has nested operations.  This walks up through the
            --  static links in the local areas, and then when it reaches
            --  the right local area, it gets the param area from there.
            --  Enclosing_Param_Areas'First is equivalent to Param_Area
            --  Larger values correspond to dereferences through static links.
            declare
               Up_Level_Count : Natural :=
                 Natural (Locator.Base rem Max_Up_Levels);
               Up_Level_Base : Word_Ptr := Context.Local_Area;

               pragma Assert (Local_Area_Static_Link_Offset = 0);
                  --  We are assuming static link is first word of local area
            begin
               while Up_Level_Count > 0 loop
                  Up_Level_Base := Fetch_Word_Ptr (Up_Level_Base, 0);
                  --  Static link is always first element of local area
                  Up_Level_Count := Up_Level_Count - 1;
               end loop;
               if Locator.Base in Enclosing_Param_Areas then
                  --  Get Param area (pointed to by second element of local
                  --  area)
                  Up_Level_Base :=
                     Fetch_Word_Ptr
                       (Up_Level_Base, Local_Area_Param_Ptr_Offset);
               end if;
               --  Now create the desired physical address
               return Add (Up_Level_Base, Locator.Offset);
            end;

         when Zero_Base =>
            --  No objects use "Zero_Base"
            Messages.Put_RT_Error
              ("Internal: " &
               "Locator_To_Physical_Address: Passed a Zero_Base Locator",
               Src_Pos => Execution_Source_Pos);
            raise Program_Error;

         when Type_Area | Enclosing_Type_Areas =>
            --  Relative to enclosing type for current operation;
            --  equivalent to relative to "outermost" enclosing local area.
            --  Return an address with a null chunk and the type index
            --  as the offset.
            return Nth_Type_Area_Item_Physical_Address
                     (Context.Enclosing_Type,
                      Locator.Offset,
                      Type_Base => Locator.Base);

         when Const_Area =>
            --  Relative to table of compile-time-known constants
            declare
               CTK_Info : constant Computable_Const_Info_Ptr :=
                 Nth_Element (Compile_Time_Known_Consts,
                   CTK_Info_Index (Locator.Offset));
            begin
               --  Check whether already computed.
               --  If not, compute it now.
               if CTK_Info.Info.Data.Addr = Null_Virtual_Address
                 and then CTK_Info.Computation /= 0
               then
                  --  Not yet computed, so compute value now
                  CTK_Info.Info.Data.Value :=
                    Invoke_Parameterless_Computation
                      (Nth_Routine (CTK_Info.Computation),
                       Result_Type  => CTK_Info.Info.Data.Type_Desc,
                       Server_Index => Context.Server_Index);

                  --  Allocate address in global data region
                  CTK_Info.Info.Data.Addr :=
                    Allocate_From_Stg_Rgn
                      (Global_Data_Stg_Rgn, 1,
                       Server_Index => Main_Thread_Server_Index);

                  --  Store the computed value in the address
                  Store_Word (CTK_Info.Info.Data.Addr,
                              Value => CTK_Info.Info.Data.Value);
               end if;
               --  Return the address
               return Virtual_To_Physical_Address (CTK_Info.Info.Data.Addr);
            end;

         when Base_Registers =>
            --  Relative to some temp base register kept in the
            --  current Local_Area
            declare
               Base_Register_Value : constant Object_Virtual_Address :=
               --  Fetch base register value from local area
                    Fetch_Word
                        (Context.Local_Area,
                         Offset_Within_Chunk (Locator.Base -
                                              Base_Registers'First));
            begin
               --  Make sure we aren't dereferencing null
               if Base_Register_Value = Null_Virtual_Address
                 or else (Locator.Offset > 0
                          and then
                            Large_Obj_Type_Info (Base_Register_Value) = 0
                          and then
                            Large_Obj_Size (Base_Register_Value) <=
                              Locator.Offset)
               then
                  Messages.Put_RT_Error
                    ("Attempt to select component of null object",
                     Src_Pos => Execution_Source_Pos);
                  raise Program_Error;
               end if;
               return Add (Virtual_To_Physical_Address (Base_Register_Value),
                           Locator.Offset);
            end;

         when others =>
            Messages.Put_RT_Error
              ("Internal: " &
               "Locator_To_Physical_Address: Ill-formed Object_Locator",
               Src_Pos => Execution_Source_Pos);
            raise Program_Error;
      end case;
   end Locator_To_Physical_Address;

   function Locator_To_Physical_Address_Exported
     (Context : Exec_Context;
      Base : Area_Base_Indicator;
      Offset : Offset_Within_Area) return Word_Ptr is
      Locator : constant Object_Locator := (Base, Offset, No_VM_Obj_Id);
   begin
      --  Pass the buck to version that takes a locator
      return Locator_To_Physical_Address (Context, Locator);
   end Locator_To_Physical_Address_Exported;

   -----------------
   -- To_U_String --
   -----------------

   function To_U_String (Val : Word_Type) return Strings.U_String is
   begin
      if Val = Null_Value then
         return Strings.Null_U_String;
      else
         return Strings.To_U_String (Strings.U_String_Index (Val));
      end if;
   end To_U_String;

   ------------------
   -- To_Word_Type --
   ------------------

   function To_Word_Type (Str : Strings.U_String) return Word_Type is
      use type Strings.U_String;
   begin
      if Str = Strings.Null_U_String then
         return Null_Value;
      else
         return Word_Type (Strings.Index (Str));
      end if;
   end To_Word_Type;

   ---------------------
   -- Type_Index_Hash --
   ---------------------

   function Type_Index_Hash (Index : Type_Index) return Strings.Hash_Type is
   --  Hash for Type_Index type
   begin
      return Strings.Hash_Type (Index);
   end Type_Index_Hash;

   ---------------------------------
   -- Virtual_To_Physical_Address --
   ---------------------------------

   function Virtual_To_Physical_Address
     (Addr : Object_Virtual_Address) return Word_Ptr is
   begin
      if Virt_Is_Phys then
         return Word_To_Word_Ptr (Addr);
      else
         return Object_To_Physical_Address (Virtual_To_Object_Address (Addr));
      end if;
   end Virtual_To_Physical_Address;

   --------------------------
   -- Wait_For_Open_Master --
   --------------------------

   procedure Wait_For_Open_Master (Context : in out Exec_Context) is
      --  Wait for the Open_Master, then set Open_Master to null.

      Master_Address : constant Word_Ptr :=
        Context.Open_Master;

      New_Local_Area : constant Word_Ptr :=
        Add (Context.Local_Area, Context.Start_Callee_Locals);
      --  TBD: This presumes that caller local area
      --      includes room for longest callee's local area.
      --      Clearly doesn't work for recursion!
      --      Once we start allocating dynamically we need to
      --      worry about reaching the end of the chunk,
      --      and more generally whether space after end
      --      of current local area is in use for other
      --      threads.
      Holding_Lock_Obj : Lock_Obj_Index := 0;

   begin  --  Wait_For_Open_Master

      if Context.Control_Area /= null then
         --  Lock object of TCB is the innermost lock held
         Holding_Lock_Obj := Large_Obj_Lock_Obj (Context.Control_Area);
      end if;

      Wait_For_Threads
        (Context,
         Master_Address,
         Holding_Lock_Obj,
         New_Local_Area);

      --  Indicate that there is no longer an "open" master
      Context.Open_Master := null;
   end Wait_For_Open_Master;

   --------------------
   -- Word_To_String --
   --------------------

   function Word_To_String (Word : Word_Type) return String is
   --  Convert ParaSail Univ_String to Ada String.
   begin
      if Word = Null_Value then
         return "null";
      else
         return Univ_Strings.To_String (Univ_Strings.From_Word_Type (Word));
      end if;
   end Word_To_String;

   ------------------------------
   -- Word_To_Wide_Wide_String --
   ------------------------------

   function Word_To_Wide_Wide_String (Word : Word_Type)
     return Wide_Wide_String is
   --  Convert ParaSail Univ_String to Ada Wide_Wide_String.
   begin
      if Word = Null_Value then
         return "null";
      else
         return Univ_Strings.To_Wide_Wide_String
                  (Univ_Strings.From_Word_Type (Word));
      end if;
   end Word_To_Wide_Wide_String;

   ----------------------
   -- Word_Ptr_To_Word --
   ----------------------

   function Word_Ptr_To_Word (Addr : Word_Ptr) return Word_Type is
      --  Convert a Word_Ptr to a Word, for a "by-ref" parameter.
   begin
      if Addr = null then
         return 0;
      else
         return Addr_To_Word (System.Storage_Elements.To_Integer
                  (Addr.all'Address));
      end if;
   end Word_Ptr_To_Word;

   ----------------------
   -- Word_To_Word_Ptr --
   ----------------------

   function Word_To_Word_Ptr (Word : Word_Type) return Word_Ptr is
      --  Convert a Word to a Word_Ptr, for a "by-ref" parameter.
   begin
      return Addr_To_Word_Ptr (System.Storage_Elements.To_Address
               (Word_To_Addr (Word)));
   end Word_To_Word_Ptr;

   ------------------------
   -- Thread_Server_Info --
   ------------------------

   package body Thread_Server_Info is

      -------------------
      -- Thread_Server --
      -------------------

      task body Thread_Server is
         --  One of these is created for each server process, each of
         --  which serves a queue of pico-threads.
         Stack_Chunk_Length : constant := 10_000;  --  10K 64-bit words
         Chunk              : constant Stg_Rgn_Chunk_Ptr :=
                                new Stg_Rgn_Chunk (Stack_Chunk_Length);
         My_Index           : Thread_Server_Index := Thread_Server_Index'First;

         Local_Area_Start   : constant Offset_Within_Chunk :=
                                Chunk.Data'First + 2;
         --  Place where local area in a new chunk should start.
         --  We leave some room for using start of chunk for something
         --  special.

         Server_Local_Area  : constant Word_Ptr :=
                                Object_To_Physical_Address
                                  ((Enclosing_Chunk => Chunk,
                                    Offset => Local_Area_Start));

         Exception_Count : Natural := 0;
         Exception_Limit : constant := 3;  --  Don't allow too many exceptions
      begin --  Thread_Server

         --  Get a unique index for the server
         Thread_Manager.Get_Server_Index (My_Index);

         --  Remember it as a task attribute
         Server_Index_Attribute.Set_Value (My_Index);

         --  Initialize beginning of chunk to ease debugging
         Chunk.Data (1 .. 40) := (others => 16#1BAD_BEEF_DEAD_BEEF#);

         --  Install chunk in table
         Install_Chunk (Chunk, New_Stg_Rgn_Chunk_Index);

         --  Now loop getting threads and servicing them
         loop
            declare
               Tcb_To_Run : Word_Ptr := null;
               Thread_Was_Queued : Boolean := False;
            begin
               --  Get a thread to serve
               Get_Thread (My_Index, Tcb_To_Run);

               exit when Tcb_To_Run = null;
               --  Null indicates Shut_Down

               if not Tcb_Exit_Requested (Tcb_To_Run) then
                  --  Perform processing for thread
                  Execute_For_Thread
                    (Tcb_To_Run,
                     My_Index,
                     Server_Local_Area,
                     Thread_Was_Queued);
               end if;

               if not Thread_Was_Queued then
                  --  Indicate we are done with thread.
                  Finish_Thread (My_Index, Tcb_To_Run);
               end if;

               --  We completed some processing exception free, so reset
               --  counter.
               Exception_Count := 0;

            exception
               when E : others =>
                  Messages.Put_RT_Error
                    ("Internal: " &
                     "Thread_Server" &
                     Thread_Server_Index'Image (My_Index) &
                     ": " &
                     Ada.Exceptions.Exception_Name (E) &
                     " raised.",
                     Src_Pos => Execution_Source_Pos (My_Index));
                  Put_Line (Ada.Exceptions.Exception_Information (E));

                  if Tcb_To_Run /= null
                    and then not Thread_Was_Queued
                  then
                     --  Finish off thread that raised an exception
                     Finish_Thread (My_Index, Tcb_To_Run);
                  end if;

                  Exception_Count := Exception_Count + 1;
                  if Exception_Count > Exception_Limit then
                     Messages.Put_RT_Error
                       ("Internal: " &
                        "Thread_Server" &
                        Thread_Server_Index'Image (My_Index) &
                        ": Shutting down because of" &
                        Natural'Image (Exception_Count) &
                        " consecutive exceptions.",
                        Src_Pos => Source_Positions.Null_Source_Position);
                     exit;
                  end if;
            end;
         end loop;
      end Thread_Server;

      task body Server_Creator is
         --  Task which creates thread servers dynamically
         Shut_Down_Now : Boolean := False;
      begin
         accept Start;
         loop
            declare
               Num_Live : Natural := 0;
               Server : Thread_Server_Ptr;
            begin

               Thread_Manager.Spawn_New_Server (Num_Live, Shut_Down_Now);

               exit when Shut_Down_Now;

               if Debug_Threading then
                  Ada.Text_IO.Put_Line
                    ("Server_Creator -- num live" &
                     Natural'Image (Num_Live) &
                     " < Minimum_Live (" &
                     Natural'Image (Minimum_Live) &
                     "); " &
                     "starting new server");
               end if;

               Num_Dynamically_Allocated_Thread_Servers :=
                 Num_Dynamically_Allocated_Thread_Servers + 1;

               --  Create a new thread server
               Server := new Thread_Server;

            end;
         end loop;

      end Server_Creator;

      ---------------
      -- Caller_Of --
      ---------------

      function Caller_Of (State : Server_State) return Server_State_Ptr is
      --  Return pointer to server state for caller of given stack frame.
      --  Normally this is just State.Prev_State, but if we are crossing
      --  pico-thread boundaries, we need to retrieve the state from
      --  the enclosing master.
         Prev_State : constant Server_State_Ptr := State.Prev_State;
      begin
         if State.Context /= null
           and then State.Context.Control_Area /= null
           and then
             (Prev_State = null
              or else Prev_State.Context = null
              or else State.Context.Control_Area /=
                        Prev_State.Context.Control_Area)
         then
            --  TCB changed; need to find master and get
            --  spawner's state from there.
            declare
               This_Tcb : constant Word_Ptr := State.Context.Control_Area;
               Master_Ptr : Word_Ptr := Tcb_Master_Ptr (This_Tcb);
               Index : Master_Index;
            begin
               if Master_Ptr = null then
                  --  Reached top of program, presumably
                  return null;
               else
                  Index := Index_Of_Master (Master_Ptr);
                  if Tcb_State (This_Tcb) = Waiting_For_Master then
                     --  While waiting for a master, the master of the Tcb
                     --  actually points to the master being awaited,
                     --  so we get the master enclosing that.
                     Index := Master_Extras (Index).Enclosing_Master;
                  end if;
                  return Master_Extras (Index).State_Of_Master'Access;
               end if;
            end;
         else
            --  Same TCB
            return Prev_State;
         end if;
      end Caller_Of;

      ---------------------
      -- Dump_One_Thread --
      ---------------------

      procedure Dump_One_Thread
        (Tcb    : Word_Ptr;
         Indent : Natural := 0)
      is
         Routine_Index : constant Routine_Elem_Index :=
                           Routine_Elem_Index (Tcb_Routine_Index (Tcb));
         Start_Pc      : constant Code_Offset := Tcb_Start_Pc (Tcb);
         Params        : constant Word_Ptr :=
                           Add (Tcb, Thread_Control_Block_Size);
         Static_Link   : constant Word_Ptr :=
                           Tcb_Static_Link (Tcb);
         Thread_Master : constant Word_Ptr :=
                           Tcb_Master_Ptr (Tcb);
         Thread_Master_Index : constant Master_Index :=
                                 Index_Of_Master (Thread_Master);
         Lock_Held : constant Lock_Obj_Index := Large_Obj_Lock_Obj (Tcb);
         Ind_Str : constant String (1 .. Indent) := (others => ' ');
      begin
         Put_Line (Ind_Str & " TCB = " & Hex_Image (Tcb));
         Put (Ind_Str &
           "  State = " & Thread_State_Enum'Image (Tcb_State (Tcb)));
         if Tcb_Exit_Requested (Tcb) then
            Put (" [exit requested]");
         end if;
         if Tcb_Uses_Queuing (Tcb) then
            Put (" [uses queuing]");
         end if;
         New_Line;

         Put_Line (Ind_Str &
           "  Master =" & Master_Index'Image (Thread_Master_Index));
         if Lock_Held > 0 then
            Put_Line (Ind_Str &
              "  Lock_Held =" & Lock_Obj_Index'Image (Lock_Held));
         end if;
         if Routine_Index > 0 then
            Put_Line
              (Ind_Str & "  Routine #" &
               Routine_Elem_Index'Image (Routine_Index) & ": " &
               Strings.To_String
                 (Nth_Element (Routine_Table, Routine_Index).Name));
            Put_Line
              (Ind_Str & "  Start_PC = " & Code_Offset'Image (Start_Pc));
            if Start_Pc /= Code_Index'First then
               --  If not starting at first instr, then we need to
               --  info on local area/callee-local start.
               Put_Line
                 (Ind_Str & "  Local_Area_Length = " &
                  Offset_Within_Area'Image (Tcb_Local_Area_Length (Tcb)));
               Put_Line
                 (Ind_Str & "  Start_Callee_Locals = " &
                  Offset_Within_Area'Image (Tcb_Start_Callee_Locals (Tcb)));
            end if;
            Put_Line (Ind_Str & "  Params at " & Hex_Image (Params));
            Put_Line (Ind_Str & "  Static_Link = " & Hex_Image (Static_Link));
         end if;
         Flush;
      end Dump_One_Thread;

      ----------------
      -- Dump_Stack --
      ----------------

      procedure Dump_Stack
        (State            : Server_State;
         Num_Stack_Frames : Positive := Positive'Last;
         Depth            : Natural := 0;
         Skip_First       : Boolean := False;
         Use_Cur_Err      : Boolean := False) is
      begin
         if Use_Cur_Err then
            Set_Output (Current_Error);
         end if;

         if Depth > 0 and then
           (State.Code /= null or else State.Context /= null)
         then
            Put_Line ("Frame #" & Natural'Image (Depth));
         end if;
         if State.Code /= null then
            Put_Line
              ("Executing " &
               Strings.To_String (State.Code.Name) &
               " at PC" &
               Code_Offset'Image (State.Pc));
            if not State.Code.Is_PSVM_Routine then
               if State.Code.Routine_Addr /= null then
                  Put_Line (" Routine address = " &
                   Hex_Image (To_Word_Ptr
                     (State.Code.Routine_Addr.all'Address)));
               end if;
            elsif State.Code.Code = null then
               Put_Line (" Current routine is abstract");
            elsif State.Pc in State.Code.Code.Instrs'Range then
               if not Skip_First then
                  if Use_Cur_Err then
                     --  Don't leave output set to Current_Error
                     --  when calling Dump_One_Instruction
                     --  with Use_Message_Format => True
                     --  because Put_Message already does the "right"
                     --  thing as far as putting output to both.
                     Set_Output (Standard_Output);
                  end if;
                  Debug.Dump_One_Instruction
                    (State.Code.Code.Instrs (State.Pc),
                     Use_Message_Format => True);
                  if Use_Cur_Err then
                     Set_Output (Current_Error);
                  end if;
               end if;
            else
               Put_Line
                 (" Trouble, PC not in Instrs'Range =" &
                  Code_Offset'Image (State.Code.Code.Instrs'First) &
                  " .." &
                  Code_Offset'Image (State.Code.Code.Instrs'Last));
            end if;
         elsif State.Prev_State /= null then
            --  Only warn about a null code pointer if not on bottom of stack
            Put_Line ("Code pointer is null");
         end if;

         if State.Context /= null then
            Put_Line
              ("  Locals at " & Hex_Image (State.Context.Local_Area));
         end if;

         if Use_Cur_Err then
            Set_Output (Standard_Output);
         end if;

         if Num_Stack_Frames > 1 and then State.Prev_State /= null then
            --  Dump deeper stack frames
            Dump_Stack (State.Prev_State.all, Num_Stack_Frames - 1,
              Depth => Depth + 1, Use_Cur_Err => Use_Cur_Err);
         end if;
      exception
         when others =>
            if Use_Cur_Err then
               --  Reset on way out
               Set_Output (Standard_Output);
            end if;
            raise;
      end Dump_Stack;

      ------------------
      -- Dump_Masters --
      ------------------

      procedure Dump_Masters is
      begin
         Put_Line (" Masters " &
                   "(enclosing/server/shared/threads/subs/lock/addr):");
         for I in 0 .. Last_Master loop
            declare
               Master_Extra : Master_Extra_Rec renames Master_Extras (I);
               U_Or_S : constant array (Boolean) of Character := ('U', 'S');
            begin
               if I = 0
                 or else Master_Extra.Master_Address /= null
               then
                  Put_Line ("  " & Master_Index'Image (I) & ":" &
                    Master_Index'Image (Master_Extra.Enclosing_Master) & "/" &
                    Thread_Server_Index'Image (Master_Extra.Owned_By_Server)
                    & "/" &
                    U_Or_S (Master_Extra.Master_Is_Shared) & "/" &
                    Thread_Count'Image (Master_Extra.Subthread_Count) & "/" &
                    Thread_Count'Image (Master_Extra.Num_Subordinates) & "/" &
                    Lock_Obj_Index'Image (Master_Extra.Lock_Held) & "/" &
                    Hex_Image (Master_Extra.Master_Address));
               end if;
            end;
         end loop;
         Put_Line ("  Num_Waiting =" &
           Thread_Count'Image (Num_Waiting_Shared_Threads));
         Put_Line ("  Num_Nonqueuing =" &
           Thread_Count'Image (Num_Nonqueuing_Threads));
         Put_Line (" Dump_Masters finished.");
         Flush;
      end Dump_Masters;

   end Thread_Server_Info;

begin
   --  Install the null routine
   declare
      Null_Routine_Copy : Routine_RW_Ptr := Null_Routine;
      --  Make a copy so can be passed to in-out parameter.
   begin
      Install_Code (Null_Routine_Copy);

      --  Should not be changed.
      pragma Assert (Null_Routine = Null_Routine_Copy);
   end;

   --  Register the builtin routines defined directly in the Interpreter
   Register_Builtin
     (Strings.String_Lookup ("#nth_stack_frame"), Nth_Stack_Frame'Access);

   Register_Builtin
     (Strings.String_Lookup ("#nth_frame_type_at_locator"),
        Nth_Frame_Type_At_Locator'Access);

   Register_Builtin
     (Strings.String_Lookup ("#num_stack_frames"), Num_Stack_Frames'Access);

   Register_Builtin
     (Strings.String_Lookup ("#peek_at_address"), Peek_At_Address'Access);

   Register_Builtin
     (Strings.String_Lookup ("#runtime_message"), Runtime_Message'Access);

   Register_Builtin
     (Strings.String_Lookup ("#exit_program"), Exit_Program'Access);

end PSC.Interpreter;
