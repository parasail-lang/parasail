pragma Style_Checks(Off);
-- $Header: /Users/stt/_parasail/_aflex_ayacc/_adamagic/ayacc/RCS/ayacc-print_statistics.adb,v 1.1 2011/03/02 22:15:02 stt Exp stt $ 
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

-- Module       : ayacc_separates.ada
-- Component of : ayacc
-- Version      : 1.2
-- Date         : 11/21/86  12:28:51
-- SCCS File    : disk21~/rschm/hasee/sccs/ayacc/sccs/sxayacc_separates.ada

-- $Header: /Users/stt/_parasail/_aflex_ayacc/_adamagic/ayacc/RCS/ayacc-print_statistics.adb,v 1.1 2011/03/02 22:15:02 stt Exp stt $ 
-- $Log: ayacc-print_statistics.adb,v $
-- Revision 1.1  2011/03/02 22:15:02  stt
-- Initial revision
--
--Revision 1.1  88/08/08  12:07:39  arcadia
--Initial revision
--
-- Revision 0.0  86/02/19  18:36:14  ada
-- 
-- These files comprise the initial version of Ayacc
-- designed and implemented by David Taback and Deepak Tolani.
-- Ayacc has been compiled and tested under the Verdix Ada compiler
-- version 4.06 on a vax 11/750 running Unix 4.2BSD.
--  

separate (Ayacc)
procedure Print_Statistics is
    use Text_IO, Parse_Table, Rule_Table, Symbol_Table;
begin

    if Options.Summary then

	Put_Line(Rule'Image(Last_Rule - First_Rule + 1) & " Productions");

	Put_Line(Grammar_Symbol'Image
	  (Last_Symbol(Nonterminal) - First_Symbol(Nonterminal) + 1) &
	   " Nonterminals");

	Put_Line(Grammar_Symbol'Image
	  (Last_Symbol(Terminal) - First_Symbol(Terminal) + 1) &
	   " Terminals");

	Put_Line(Integer'Image(Number_of_States) & " States");

	Put_Line (Integer'Image(Shift_Reduce_Conflicts) &
		  " Shift/Reduce conflicts");

	Put_Line (Integer'Image(Reduce_Reduce_Conflicts) &
		  " Reduce/Reduce conflicts");

    else

	if Shift_Reduce_Conflicts /= 0 then
	    Put_Line (Integer'Image(Shift_Reduce_Conflicts) &
		      " Shift/Reduce Conflicts");
	end if;
	if Reduce_Reduce_Conflicts /= 0 then
	    Put_Line (Integer'Image(Reduce_Reduce_Conflicts) &
		      " Reduce/Reduce Conflicts");
	end if;

    end if;

end Print_Statistics;
