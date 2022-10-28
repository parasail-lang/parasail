------------------------------------------------------------------------------
--                    S Y S T E M . L W T . O P E N M P                     --
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
------------------------------------------------------------------------------

package System.LWT.OpenMP is
   --  This package implements a plug-in LWT scheduler for OpenMP.

   --  These two routines keep track of the number of active OpenMP
   --  parallel regions.  While there is at least one active, we establish
   --  OpenMP as the LWT scheduler.  These are called as a side-effect
   --  of initializing/finalizing an Interfaces.OpenMP.OMP_Parallel object.
   --  It is implementation-defined whether there is one LWT scheduler
   --  plug-in per Ada task, or one per program partition.
   procedure Incr_OpenMP_Parallel_Regions;
   procedure Decr_OpenMP_Parallel_Regions;

   procedure Set_Ada_Task_Identity
     (Identity : Ada.Task_Identification.Task_Id);
   --  Set the Ada task identity to be associated with the current LWT server

end System.LWT.OpenMP;
