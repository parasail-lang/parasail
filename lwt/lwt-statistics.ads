------------------------------------------------------------------------------
--                              L W T Statistics                            --
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
--                                                                          --
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

package LWT.Statistics is

   Recursion_Level : Natural := 0
     with Thread_Local_Storage;

   Recursion_Max : constant := 50;
      --  Limit recursion when executing LWT_Body immediately

   Num_Bypassed_LWT_Initiations : Natural := 0
     with Thread_Local_Storage;
      --  Number of times we skip initiating a thread and just execute
      --  the LWT_Body directly.
      --  This is collected even if we are not otherwise collecting
      --  statistics, as we treat it as an indicator of having
      --  because when it is too low we might want to reduce our
      --  need for more LWTs on the deques, so we can bypass more
      --  initiations.

   Max_Default_Servers : constant := 20;
      --  Maximum number of server threads that will be used by default
      --  for the scheduling of light-weight threads.
      --  An explicit number of server threads larger than this may be
      --  specified in the LWT Control object.

   Num_LWTs_Needed_Per_Server : Positive := 10;
      --  Number of LWTs needed per active server to allow
      --  work stealing to work efficiently.

   Max_Steal_Iterations : Positive := Max_Default_Servers;
      --  Number of times we try to steal a LWT

   Min_LWTs_Before_Stealing : Positive := 1;
      --  A server should have at least this many LWTs before
      --  we steal from it

   Enough_LWTs_Before_Stealing : Positive := Num_LWTs_Needed_Per_Server - 1;
      --  If a server has this many LWTs we will steal from it,
      --  without looking for a server that has more on its queue.

   Steal_Failure_Delay : constant := 0.001;
      --  Amount to delay before trying to steal again

   Debug_Statistics : constant Boolean := True;
      --  Whether to collect threading statistics

   Debug_Storage_Stats : constant Boolean := True;
      --  Whether to collect storage statistics

end LWT.Statistics;
