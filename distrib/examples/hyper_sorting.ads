---------------------------------------------------
--  Example Distributed program -- Hyper_QuickSort

--  Copyright (C) 2011-2020, AdaCore
--  This program is provided "as is" with no warranty.
--  Report errors at:
--    http://groups.google.com/group/parasail-programming-language

with Distributed_Vectors;
generic
   type Elem_Type is digits <>;
   type Index_Type is range <>;
   type Array_Type is array (Index_Type range <>) of Elem_Type;
   with package Dis_Vec is new
     Distributed_Vectors
      (Index_Type => Index_Type,
       Element_Type => Elem_Type,
       Distrib_Type_Id => <>);
package Hyper_Sorting is

   procedure Hyper_Qsort (A : in out Array_Type; Num_Chunks : Natural);
      --  Partition array into chunks; sort each chunk
      --  Come up with multiple pivots, and partition into a new
      --  set of chunks using these pivots
      --  Shuffle data into new partitioning (probably using a temp array).
      --  Sort each new chunk.

   procedure Distrib_Qsort (V : in out Dis_Vec.Vector);
      --  Do a distributed hyper sort, presuming the vector
      --  has already been divided into shards, and is to be
      --  sorted in a distributed fashion, with parallel sort(s)
      --  being performed on each node.

end Hyper_Sorting;
