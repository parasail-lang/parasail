------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

--  Package defining locks used by the Runtime. This version relies on the
--  OpenMP library implementaton for GCC (GOMP).

with Ada.Text_IO; use Ada.Text_IO;
package body PSC.Interpreter.Locks is

   pragma Linker_Options ("-lgomp");

   procedure Initialize (Lock : in out Simple_Lock) is
   begin
      omp_init_lock (Lock.Lock'Access);
   end Initialize;

   procedure Acquire (Lock : in out Simple_Lock) is
   begin
      omp_set_lock (Lock.Lock'Access);
   end Acquire;

   procedure Release (Lock : in out Simple_Lock) is
   begin
      omp_unset_lock (Lock.Lock'Access);
   end Release;

   procedure Destroy (Lock : in out Simple_Lock) is
   begin
      omp_destroy_lock (Lock.Lock'Access);
   end Destroy;

end PSC.Interpreter.Locks;
