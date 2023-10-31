------------------------------------------------------------------------------
--                     Generic_Parallel_Work_Lists                          --
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
generic
   type Work_Item is private;
package LWT.Generic_Parallel_Work_Lists
--  TBD: with Pure
is
   --  This generic package supports a parallel procedural iterator
   --  that calls a loop body multiple times, potentially in parallel
   --  (limited to Num_Chunks logical threads running in parallel at a time)
   --  providing a work item which the loop body can process and possibly
   --  generate new work items, which can be added onto the work list.
   --  The Max_Chunks parameter to the Loop_Body indicates
   --  the maximum number of threads that the Loop_Body should itself
   --  spawn, to avoid creating too many logical threads.

   procedure Par_Iterate_Work_List
     (Initial_Item : Work_Item; Num_Chunks : Positive;
      Loop_Body : not null access procedure
        (Item : Work_Item;          --  Item to be processed.
         Max_Chunks : Positive;     --  Max new threads to spawn.
         Add_Work_Item : not null access procedure (New_Item : Work_Item)));
                                    --  Routine to call to add new work item;
                                    --  may be called from any thread.
      --  with Parallel_Iterator;   --  Might call Loop_Body in parallel.

end LWT.Generic_Parallel_Work_Lists;
