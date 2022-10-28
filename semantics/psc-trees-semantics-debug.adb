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

package body PSC.Trees.Semantics.Debug is

   procedure Turn_On_Debugging (Which : Debug_Index := 0) is
   --  Turn on some or all debugging
   begin
      case Which is
         when 0 =>
            --  All
            Debug_First_Pass := True;
            Debug_Second_Pass := True;
            Debug_Pre_Cg := True;
            Debug_Code_Gen := True;
            Interpreter.Debug_Type_Descs := True;
            Interpreter.Debug_Threading := True;
            Interpreter.Debug_Stg_Rgns := True;
            Interpreter.Debug_Calls := True;
            Interpreter.Debug_Statistics := True;
         when 1 =>
            Debug_First_Pass := True;
         when 2 =>
            Debug_Second_Pass := True;
         when 3 =>
            Debug_Pre_Cg := True;
         when 4 =>
            Debug_Code_Gen := True;
            Interpreter.Debug_Type_Descs := True;
         when 5 =>
            Interpreter.Debug_Threading := True;
            Interpreter.Debug_Statistics := True;
         when 6 =>
            Interpreter.Debug_Stg_Rgns := True;
         when 7 =>
            Interpreter.Debug_Calls := True;
         when 8 =>
            Interpreter.Debug_Statistics := True;
         when 9 =>
            --  Turn it *all* on
            Turn_On_Debugging (0);
            Debug_Matching := True;
            Debug_Substitution := True;
      end case;
   end Turn_On_Debugging;

   procedure Turn_Off_Debugging (Which : Debug_Index := 0) is
   --  Turn off some or all debugging
   begin
      case Which is
         when 0 =>
            --  All
            Debug_First_Pass := False;
            Debug_Second_Pass := False;
            Debug_Pre_Cg := False;
            Debug_Code_Gen := False;
            Interpreter.Debug_Type_Descs := False;
            Interpreter.Debug_Threading := False;
            Interpreter.Debug_Stg_Rgns := False;
            Interpreter.Debug_Calls := False;
            Interpreter.Debug_Statistics := False;
         when 1 =>
            Debug_First_Pass := False;
         when 2 =>
            Debug_Second_Pass := False;
         when 3 =>
            Debug_Pre_Cg := False;
         when 4 =>
            Debug_Code_Gen := False;
            Interpreter.Debug_Type_Descs := False;
         when 5 =>
            Interpreter.Debug_Threading := False;
         when 6 =>
            Interpreter.Debug_Stg_Rgns := False;
         when 7 =>
            Interpreter.Debug_Calls := False;
         when 8 =>
            Interpreter.Debug_Statistics := False;
         when 9 =>
            --  Turn it *all* off
            Turn_Off_Debugging (0);
            Debug_Matching := False;
            Debug_Substitution := False;
      end case;
   end Turn_Off_Debugging;

end PSC.Trees.Semantics.Debug;
