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

package PSC.Command_Input is
   --  Input from the command line
   --  These will use the GNU "readline" package if available

   Max_Line : constant := 1000;  --  Max length for one line

   function End_Of_File (Prompt : String := "") return Boolean;
      --  Return True if at end-of-file.
      --  If unknown, call Get_Line and save result to determine if at EOF.

   function Get_Line (Prompt : String := "") return String;
      --  Return next line from standard input.
      --  Remember non-blank lines in history, if available.
      --  Raise Ada.IO_Exceptions.End_Of_File if reach end-of-file

   type End_Of_File_Func is access function (Prompt : String) return Boolean;
   type Readline_Func is access function (Prompt : String) return String;
   type Add_History_Proc is access procedure (Line : String);

   procedure Install_Readline_Support
     (EOF : End_Of_File_Func; RL : Readline_Func; AH : Add_History_Proc);
      --  Install routines that support readline/history mechanism.

end PSC.Command_Input;
