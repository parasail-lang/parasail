---------------------------------------------------
--  Generic Hyper Sorting routine for a distributed program

--  Copyright (C) 2011-2021, AdaCore
--  This program is provided "as is" with no warranty.
--  Report errors at:
--    http://groups.google.com/group/parasail-programming-language

with Distributed_Vectors;

generic
   type Elem_Type is digits <>;
   with package Dis_Vec is new Distributed_Vectors
                                  (Element_Type => Elem_Type, others => <>);
   --  TBD: Could pass in a "Before" or "<" operator
   --       to determine direction of sorting, though
   --       that would add complexity to the use of the histogram, etc.
package Distrib_Hyper_Sorting is

   procedure Hyper_Qsort
     (Vec : in out Dis_Vec.Vector);
      --  Compute histogram over vector, with sufficient resolution
      --  so it is possible to know how to break the values into
      --  approximately equal-size groups by value ranges.
      --  Come up with multiple pivots, and partition into a new
      --  set of shards using these pivots
      --  Shuffle data into new partitioning (probably using a temp vector).
      --  Then use the histogram data again to come up with local pivots
      --  for splitting the shard into chunks.
      --  Shuffle the values into these chunks, and sort each new shard.

      --  To do a shuffle, it makes sense to produce an NxM set of streams
      --  to be sent to other nodes/shards.

end Distrib_Hyper_Sorting;
