------------------------------------------------------------------------------
--                              Distributed Reduction                       --
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
------------------------------------------------------------------------------

--  pragma Ada_2020;

with System_Distrib;
generic
   type Accum_Type (<>) is private;
   with procedure Combiner (Left, Right : in out Accum_Type);
   type Index_Type is range <>;
   Distrib_Type_Id : String;
package Distributed_Reduction is
   --  This provides a "Collector" type which is a distributed object
   --  spread over the nodes.  Ultimately, the "Distrib_Reduce"
   --  procedure takes an "Add_One_Value" procedure which adds a value
   --  to an accumulator that was initialized by copying from the initial
   --  state of the "Result" parameter.  The Add_One_Value procedure only
   --  gets two parameters, namely an accumulator where a value should be
   --  added, and an index which presumably identifies where to get the
   --  value.
   type Collector
     (GC : not null System_Distrib.Group_Context_Ptr;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type) is
      tagged limited private;

   procedure Distrib_Reduce
     (Dis_Obj : in out Collector;
      Result : in out Accum_Type;
      First : Index_Type;
      Last : Index_Type'Base;
      Num_Chunks : Natural := 0;
      Add_One_Value : access procedure
        (Accum : in out Accum_Type; Index : Index_Type));
      --  Iterate in parallel over local indices 1 to Vec.Length, in chunks.
      --  Call Add_One_Value to add a single value to the accumulator.
      --  Will call Combiner to combine accumulators built up
      --  in parallel (some day doing a tree-wise combination in case
      --  the number of threads on a given node is very large).
      --  The node/shard-level result is shared with other nodes/shards
      --  and then combined into a single global accumulator, namely Result.

   function Next_Obj_Id return System_Distrib.Distrib_Object_Id_Type;
      --  This can be used to assign the next sequential object id
      --  presuming the Distrib_Type_Id is unique to each instance
      --  of this package.

private

   type Reduction_Info;
   type Reduction_Info_Ptr is access all Reduction_Info;

   function Init_Info (Self : not null access Collector)
     return not null Reduction_Info_Ptr;

   type Collector
     (GC : not null System_Distrib.Group_Context_Ptr;
      Distrib_Obj_Id : System_Distrib.Distrib_Object_Id_Type) is
     tagged limited record
      Info : not null Reduction_Info_Ptr :=
        Init_Info (Collector'Unchecked_Access);
   end record;

end Distributed_Reduction;
