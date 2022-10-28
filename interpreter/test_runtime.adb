------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

--  Test routine to see if we can pull in the interpreter and its builtins
--  without pulling in the entire compiler.

with PSC.Interpreter.Builtins;
with PSC.Interpreter.IO;
with PSC.Messages;
with PSC.Source_Positions;
use PSC;
procedure Test_Runtime is
begin
   Interpreter.Start_Up_Thread_Servers;

   Messages.Put_Message
     ("Hi there", Src_Pos => Source_Positions.Null_Source_Position,
      Message_Kind => "Just-for-fun");

   Interpreter.Shut_Down_Thread_Servers (Total_Errors => 0);
exception
   when others =>
      Interpreter.Shut_Down_Thread_Servers (Total_Errors => 1);
end Test_Runtime;
