------------------------------------------------------------------------------
--                            Ada 2022 Parallelism                          --
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

--  Prototype implementation of LWT.Parallelism package for Ada 2022

with Ada.Iterator_Interfaces;
with LWT.Aspects;
with System;

private with LWT.Scheduler;

package LWT.Parallelism is

   --  This provides "higher level" interfaces that are implemented
   --  on top of System.LWT, which is the connector to the underlying
   --  LWT scheduler plug-in.  The compiler is expected to generate
   --  calls on this package.

   type Longest_Integer is range System.Min_Int .. System.Max_Int;
      --  Not worrying about unsigned ranges with
      --  upper bound > System.Max_Int for now.
      --  Could be handled by having a version of Par_Range_Loop
      --  that operates on unsigned integers.

   procedure Par_Range_Loop
     (Low, High : Longest_Integer;
      Num_Chunks : Natural := 0;    --  0 means no chunk specification present
      Aspects : access LWT.Aspects.Root_Aspect'Class := null;
                                    --  null means no aspects specified
      Loop_Body : access procedure
        (Low, High : Longest_Integer; Chunk_Index : Positive));
   --  with Parallel_Iterator;  --  it can call Loop_Body in parallel

   --  Parallel "for" loop over discrete range

   type Par_Loop_Id is private;
   --  Identifies the thread group associated with a parallel loop

   function Early_Exit (PID : Par_Loop_Id) return Boolean;
   --  Call this function to attempt an early exit.
   --  If it returns True, the calling thread "won" the race to
   --  perform an early exit.
   --  If it returns False, the calling thread should quit, as some
   --  other thread has canceled the thread group.

   procedure Par_Range_Loop_With_Early_Exit
     (Low, High : Longest_Integer;
      Num_Chunks : Natural := 0;    --  0 means no chunk specification present
      Aspects : access LWT.Aspects.Root_Aspect'Class := null;
                                    --  null means no aspects specified
      Loop_Body : access procedure
        (Low, High : Longest_Integer; Chunk_Index : Positive;
         PID : Par_Loop_Id));
   --  with Parallel_Iterator;  --  it can call Loop_Body in parallel

   --  Parallel "for" loop over discrete range, where loop might contain
   --  an "early exit" construct that leaves the loop before it completes,
   --  such as a goto to a label outside the loop, a return from an enclosing
   --  function or procedure, or an exit from the loop or an enclosing loop.
   --  Call on Early_Exit (PID) is used to attempt an early exit.

   function Chunk_Index return Positive
      with Convention => Intrinsic;
   --  This returns the current chunk index, when using pragmas rather
   --  than syntax for the parallel loop.

   --   The following generic is part of Ada.Iterator_Interfaces in Ada 2022
   --   We put it here merely for convenience of prototyping.
   generic
      with package Iter is new Ada.Iterator_Interfaces (<>);
   package Parallel_Iterator_Interfaces is

      type Parallel_Iterator is limited interface and Iter.Forward_Iterator;

      subtype Chunk_Index is Positive;

      function Is_Split (Object : Parallel_Iterator)
        return Boolean is abstract;

      procedure Split_Into_Chunks (Object     : in out Parallel_Iterator;
                                   Max_Chunks : Chunk_Index) is abstract
         with Pre'Class   => not Object.Is_Split or else raise Program_Error,
              Post'Class  => Object.Is_Split and then
                             Object.Chunk_Count <= Max_Chunks;

      function Chunk_Count (Object : Parallel_Iterator)
         return Chunk_Index is abstract
         with Pre'Class   => Object.Is_Split or else raise Program_Error;

      function First (Object : Parallel_Iterator;
                      Chunk  : Chunk_Index) return Iter.Cursor is abstract
         with Pre'Class   => (Object.Is_Split and then
                                 Chunk <= Object.Chunk_Count)
                              or else raise Program_Error;

      function Next (Object   : Parallel_Iterator;
                     Position : Iter.Cursor;
                     Chunk    : Chunk_Index) return Iter.Cursor is abstract
         with Pre'Class   => (Object.Is_Split and then
                                 Chunk <= Object.Chunk_Count)
                              or else raise Program_Error;

      type Parallel_Reversible_Iterator is limited interface
         and Parallel_Iterator and Iter.Reversible_Iterator;
   end Parallel_Iterator_Interfaces;

   generic
      with package Inst is
        new Parallel_Iterator_Interfaces (<>);
         --  In Ada 2022, will be "new Ada.Iterator_Interfaces (<>);"
   procedure Generic_Par_Iterator_Loop
     (Iterator : in out Inst.Parallel_Iterator'Class;
      Num_Chunks : Natural := 0;    --  0 means no chunk specification present
      Aspects : access LWT.Aspects.Root_Aspect'Class := null;
                                    --  null means no aspects specified
      Loop_Body : access procedure
        (Iterator : Inst.Parallel_Iterator'Class;
         Chunk_Index : Positive;
         PID : Par_Loop_Id));
   --  with Parallel_Iterator;  --  it can call Loop_Body in parallel

   --  Parallel "for loop" over a container or generalized iterator
   --  TBD: No need to pass in Num_Chunks or use "in out" mode
   --       if we call Split_Into_Chunks *before* calling this routine
   --       (i.e. Iterator.Is_Split would be true before the call).

private

   type Par_Loop_Id is access constant LWT.Scheduler.Root_Data'Class;

end LWT.Parallelism;
