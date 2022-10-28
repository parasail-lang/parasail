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

with Ada.Text_IO; use Ada.Text_IO;
package body PSC.Interpreter.Locks is

   protected body Simple_Lock is

      entry Acquire when not Is_Locked is
      begin
         Is_Locked := True;
      end Acquire;

      procedure Release is
      begin
         Is_Locked := False;
      end Release;
   end Simple_Lock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Lock : in out Simple_Lock) is
   begin
      null;
   end Initialize;

   -------------
   -- Acquire --
   -------------

   procedure Acquire (Lock : in out Simple_Lock) is
   begin
      Lock.Acquire;
   end Acquire;

   -------------
   -- Release --
   -------------

   procedure Release (Lock : in out Simple_Lock) is
   begin
      Lock.Release;
   end Release;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Lock : in out Simple_Lock) is
   begin
      null;
   end Destroy;

end PSC.Interpreter.Locks;
