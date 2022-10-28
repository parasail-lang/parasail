------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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

--  Package providing support for calling compiled main routine

with PSC.Interpreter; use PSC.Interpreter;
with PSC.Strings;
pragma Elaborate (PSC.Interpreter);
package body PSC.Call_Main_Builtin is

   procedure parasail_main_routine (Context : Exec_Context;
     Params : Word_Ptr; Static_Link : Type_Descriptor_Ptr);
   pragma Import (Ada, parasail_main_routine, "_parasail_main_routine");
   --  llvm generator uses this link-name for suitable main ParaSail routine

   procedure Call_Compiled_Main
     (Context : Exec_Context;
      Params : Word_Ptr;
      Static_Link : Non_Op_Map_Type_Ptr) is
   begin
      --  Then call the main routine
      parasail_main_routine (Context, Params, Static_Link);
   end Call_Compiled_Main;

begin
   Register_Builtin
     (Strings.String_Lookup ("#call_compiled_main"),
      Call_Compiled_Main'Access);

end PSC.Call_Main_Builtin;
