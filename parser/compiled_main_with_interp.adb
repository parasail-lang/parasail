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

pragma Style_Checks (Off);

with PSC.Call_Main_Builtin;  --  Pull in #call_compiled_main
with PSC.Interpreter;
with ParaSail_Main;
procedure Compiled_Main_With_Interp is
--  Version of ParaSail which includes "#call_compiled_main" builtin
--  as well as full ParaSail interpreter.
begin
   --  Turn on/off debugging before reconstructing type descriptors
   PSC.Interpreter.Debug_Type_Descs := False;

   --  Call the per-file global constructors
   PSC.Interpreter.Invoke_Per_File_Initializers;

   --  Now invoke the parser/interpreter.
   ParaSail_Main;
end Compiled_Main_With_Interp;
