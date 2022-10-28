------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

--  Package providing support for readline and add_history

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.IO_Exceptions;

with PSC.Command_Input;
pragma Elaborate (PSC.Command_Input);

package body RL_C_Interface is
   --  Interface to readline(prompt) and add_history(line) routines.
   --  These declarations are based on <readline/{readline,history}.h>

   Peek : chars_ptr := Null_Ptr;
   Peek_Is_Valid : Boolean := False;

   function C_Readline (prompt : chars_ptr) return chars_ptr;
   pragma Import (C, C_Readline, "readline");

   procedure C_Add_History (line : chars_ptr);
   pragma Import (C, C_Add_History, "add_history");

   --  pragma Linker_Options ("-lreadline");

   function End_Of_File (Prompt : String := "") return Boolean is
      --  Return True if at end-of-file.  If unknown,
      --  call Readline (Prompt) to determine; save result for
      --  subsequent call on Readline.
   begin
      if not Peek_Is_Valid then
         declare
            C_Prompt : chars_ptr := New_String (Prompt);
         begin
            Peek := C_Readline (C_Prompt);
            Free (C_Prompt);
            Peek_Is_Valid := True;
         end;
      end if;
      return Peek = Null_Ptr;
   end End_Of_File;

   function Readline (Prompt : String := "") return String is
      --  Call C "readline"
      --  Raise Ada.IO_Exceptions.End_Of_File if reach end-of-file
   begin
      --  Call End_Of_File to initialize "Peek"
      if End_Of_File (Prompt) then
         --  end-of-file reached
         Peek_Is_Valid := False;  --  Force re-check on next Readline
         raise Ada.IO_Exceptions.End_Error;
      end if;

      --  Return Peek as an Ada String
      declare
         Result : constant String := Value (Peek);
      begin
         Peek_Is_Valid := False;
         Free (Peek);
         return Result;
      end;
   end Readline;

   procedure Add_History (Line : String) is
      --  Add Line to history used by readline
      C_Line : chars_ptr := New_String (Line);
   begin
      C_Add_History (C_Line);
      Free (C_Line);
   end Add_History;

begin
   --  Install operations in Readline_Support package
   PSC.Command_Input.Install_Readline_Support
     (End_Of_File'Access, Readline'Access, Add_History'Access);
end RL_C_Interface;
