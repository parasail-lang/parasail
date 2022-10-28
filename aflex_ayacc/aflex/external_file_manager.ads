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

-- TITLE external_file_manager
-- AUTHOR: John Self (UCI)
-- DESCRIPTION opens external files for other functions
-- NOTES This package opens external files, and thus may be system dependent
--       because of limitations on file names.
--       This version is for the VADS 5.5 Ada development system.
-- $Header: /Users/stt/_parasail/_aflex_ayacc/_adamagic/aflex/RCS/file_manager.ads,v 1.1 2011/03/02 22:14:39 stt Exp stt $ 

with TEXT_IO; use TEXT_IO; 
package EXTERNAL_FILE_MANAGER is 
  procedure GET_IO_FILE(F : in out FILE_TYPE); 
  procedure GET_DFA_FILE(F : in out FILE_TYPE); 
  procedure GET_SCANNER_FILE(F : in out FILE_TYPE); 
  procedure GET_BACKTRACK_FILE(F : in out FILE_TYPE); 
  procedure INITIALIZE_FILES; 
end EXTERNAL_FILE_MANAGER; 
