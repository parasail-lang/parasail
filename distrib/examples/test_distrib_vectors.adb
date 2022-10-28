------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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

--  pragma Ada_2020;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Parallelism;
with System_Distrib; use System_Distrib;
with Distributed_Vectors;
with Ada.Exceptions;
with Generic_Histograms;
with Distributed_Reduction;

procedure Test_Distrib_Vectors is

   Num_Args : constant Natural := Argument_Count;

   Len : constant Natural :=
     (if Num_Args > 0 then Integer'Value (Argument (1)) else 100);

   Num_Threads_To_Use : constant Natural :=
     (if Num_Args > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

   Default_Num_Nodes : constant := 4;

   Min_Num_Nodes : constant Natural :=
     (if Num_Args < 3 then Default_Num_Nodes
      else Natural'Value (Argument (3)));

   --  Join a group; find out local portion of problem space determined by
   --  effective thread count.
   GC : aliased Group_Context := Join_Group
     ("com.adacore.taft.Distrib_Vectors" & Min_Num_Nodes'Image,
      Num_Threads_To_Use,
      Min_Num_Nodes => Min_Num_Nodes);

   --  For Random
   Gen : Generator;

   --  Time_Start : array (1 .. 4) of Ada.Calendar.Time;
   --  Time_End : array (1 .. 4) of Ada.Calendar.Time;

   --  use Ada.Calendar;

   package Dis_Vec is new Distributed_Vectors
     (Index_Type => Positive,
      Element_Type => Float,
      Distrib_Type_Id => "Dis_Vec_Float");

   subtype Vec_Type is Dis_Vec.Vector;

   Vec : Vec_Type (GC'Unchecked_Access, 1);

   Vec2 : Vec_Type (GC'Unchecked_Access, 2);

   package Flt_Histograms is new Generic_Histograms
     (Value_Type => Float);
      --  Flt_Histograms.Histogram has one discriminant, Size.
      --  The low and high bounds, and the width of each bucket will
      --  automatically shift to accommodate values as they are added.
      --  The bounds will be a multiple of the bucket width.
      --  The widths of the buckets will double as needed to fit all
      --  of the values, so one outlier could cause the buckets to
      --  become very wide.  One could imagine eventually recognizing
      --  outliers somehow, and e.g., limiting bounds to 2 or 3 std. devs.
      --  from the mean.

   package Dis_Flt_Histo is new Distributed_Reduction
     (Accum_Type => Flt_Histograms.Histogram,  --  Indefinite type
      Combiner => Flt_Histograms.Combine,
      Index_Type => Positive,
      Distrib_Type_Id => "Dis_Histo_Float");
   --  This provides a "Collector" type which is a distributed object
   --  spread over the nodes.  Ultimately, the "Distrib_Reduce"
   --  procedure takes an "Add_One_Value" procedure which adds a value
   --  to an accumulator that was initialized by copying from the initial
   --  state of the "Result" parameter.  The Add_One_Value procedure only
   --  gets two parameters, namely an accumulator where a value should be
   --  added, and an index which presumably identifies where to get the
   --  value.
begin
   Reset (Gen, Initiator => Integer (GC.Node_Index) * Integer (Len));

   Put_Line ("Seed =" &
     Integer'Image (Integer (GC.Node_Index) * Integer (Len)));

   for I in 1 .. Num_Threads_To_Use * 10 loop
      Vec.Append (Random (Gen) * 100.0 * Float (Num_Threads_To_Use));
   end loop;

   declare
      I : Natural := 0;
      procedure Loop_Body (Element : Float) is
      begin
         if I = 0 then
            Put_Line ("Display of" & Vec.Length'Image &
              " elements of vector shard on node" &
              GC.Node_Index'Image);
         end if;
         I := I + 1;
         Put_Line (I'Image & ":" & Element'Image);
      end Loop_Body;
   begin
      Vec.Iterate (Process => Loop_Body'Access);
   end;

   declare
      Histo_1 : Dis_Flt_Histo.Collector
                  (GC'Unchecked_Access, Distrib_Obj_Id => 1);
      Result : Flt_Histograms.Histogram
        (Size => Natural'Max (10, 2 * Positive (GC.Num_Nodes)));
         --  This object is used to initialize all of the singletons
         --  We set a size of twice the number of nodes to get adequate
         --  granularity.

      procedure Add_One_Value (Accum : in out Flt_Histograms.Histogram;
                               Index : Positive) is
      --  Add one value to the Histogram
      begin
         Accum.Add_Value (Vec.Element (Index));
      end Add_One_Value;

   begin

      --  Iterate in parallel over local indices 1 to Vec.Length, in chunks.
      --  Call Add_One_Value to add a single value to the accumulator.
      --  Will call Combiner to combine accumulators built up
      --  in parallel (some day doing a tree-wise combination in case
      --  the number of threads on a given node is very large).
      --  The node/shard-level result is shared with other nodes/shards
      --  and then combined into a single global accumulator, namely Result.
      Histo_1.Distrib_Reduce
        (Result, First => 1, Last => Vec.Length,
         Num_Chunks => Num_Threads_To_Use * 2,
         Add_One_Value => Add_One_Value'Access);

      --  Display histogram
      Result.Dump_Histo;
   end;

   Put_Line (GC.Node_Index'Image &
     ": About to append some elements onto other shards of Vec2");

   for I in 1 .. Positive (GC.Num_Nodes) loop
      if I /= Positive (GC.Node_Index) then
         Vec2.Append_Slice (From => Vec,
           First => (I - 1) * Natural (Vec.Length) /
             Positive (GC.Num_Nodes) + 1,
           Last => I * Natural (Vec.Length) / Positive (GC.Num_Nodes),
           Onto_Shard => Shard_Index_Type (I),
           Time_Stamp => 3);
      end if;
   end loop;

   --  Wait for other nodes to append on our shard
   Vec2.Await_Incoming_Updates (Time_Stamp => 3);

   declare
      I : Natural := 0;
      procedure Loop_Body (Element : Float) is
      begin
         if I = 0 then
            Put_Line
              ("Node" & GC.Node_Index'Image &
                ": Display of" & Vec2.Length'Image &
                " elements of vector2 shard");
         end if;
         I := I + 1;
         Put_Line (I'Image & ":" & Element'Image);
      end Loop_Body;
   begin
      Vec2.Iterate (Process => Loop_Body'Access);
   end;

   Finish (GC);
exception
   when E : others =>
      Put_Line ("Exception " & Ada.Exceptions.Exception_Name (E) &
        "raised: " & Ada.Exceptions.Exception_Information (E));
end Test_Distrib_Vectors;
