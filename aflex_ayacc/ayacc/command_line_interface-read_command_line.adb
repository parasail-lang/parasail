pragma Style_Checks(Off);
-- Copyright (c) 1990 Regents of the University of California.
-- All rights reserved.
--
--    The primary authors of ayacc were David Taback and Deepak Tolani.
--    Enhancements were made by Ronald J. Schmalz.
--
--    Send requests for ayacc information to ayacc-info@ics.uci.edu
--    Send bug reports for ayacc to ayacc-bugs@ics.uci.edu
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

--|
--| Notes: This routine contains the machine specific details of how
--|        Ayacc obtains the command line arguments from the host Operating
--|        System.  This version assumes GNAT.
--|
--|        The only requirement on this subunit is that it place the string
--|        of characters typed by the user on the command line into the
--|        parameter "Command_Args".
--|

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
separate (Command_Line_Interface)
procedure Read_Command_Line (Command_Args : out Command_Line_Type) is
  package cli_strings is new Generic_Bounded_Length(Maximum_Command_Length);
  use cli_strings;

  tmp_command_args : Bounded_String;
begin
  tmp_command_args := To_Bounded_String("");
  for i in 1 .. Ada.Command_Line.Argument_Count
  loop
    tmp_command_args := append(tmp_command_args, Argument(i));
    if ( i < Ada.Command_Line.Argument_Count) then
      tmp_command_args := append(tmp_command_args, " ");
    end if;
  end loop;
  Command_Args(1..length(tmp_command_args)) := To_String(tmp_command_args);
  command_args(length(tmp_command_args) + 1 .. Maximum_Command_Length) :=
      (others => ' ');
end Read_Command_Line;
