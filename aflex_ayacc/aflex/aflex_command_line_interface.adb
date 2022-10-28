pragma Style_Checks(Off);
-- Copyright (c) 1990 Regents of the University of California.
-- All rights reserved.
--
-- This software was developed by John Self of the Arcadia project
-- at the University of California, Irvine.
--
-- Redistribution and use in source and binary forms are permitted
-- provided that the above copyright notice and this paragraph are
-- duplicated in all such forms and that any documentation,
-- advertising materials, and other materials related to such
-- distribution and use acknowledge that the software was developed
-- by the University of California, Irvine.  The name of the
-- University may not be used to endorse or promote products derived
-- from this software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
-- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

-- TITLE command line interface
-- AUTHOR: John Self (UCI)
-- DESCRIPTION command line interface body for use with the VERDIX VADS system.
-- NOTES this file is system dependent
-- $Header: /Users/stt/_parasail/_aflex_ayacc/_adamagic/aflex/RCS/command_line_interface.adb,v 1.1 2011/03/02 22:14:39 stt Exp stt $ 

with TSTRING; use TSTRING; 
with Ada.Command_Line; use Ada.Command_Line;

package body Aflex_COMMAND_LINE_INTERFACE is 
  procedure INITIALIZE_COMMAND_LINE is 
  begin
      for i in 1 .. Ada.Command_Line.Argument_Count
      loop
        ARGV(i):= vstr(Argument(i));
      end loop;
      ARGC := Ada.Command_Line.Argument_Count + 1;
  end INITIALIZE_COMMAND_LINE; 

end Aflex_COMMAND_LINE_INTERFACE; 
