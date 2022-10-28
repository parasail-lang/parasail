------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

private package PSC.Trees.Semantics.Debug is

   Debug_First_Pass : Boolean := False;
   Debug_Second_Pass : Boolean := False;
   Debug_Pre_Cg : Boolean := False;
   Debug_Code_Gen : Boolean := False;

   Debug_Matching : Boolean := False;
   Debug_Substitution : Boolean := False;

   procedure Turn_On_Debugging (Which : Debug_Index := 0);
   --  Turn on some or all debugging

   procedure Turn_Off_Debugging (Which : Debug_Index := 0);
   --  Turn off some or all debugging

end PSC.Trees.Semantics.Debug;
