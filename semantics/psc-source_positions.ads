------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation. See           --
-- documentation/COPYING3 and documentation/GCC_RUNTIME3_1 for details.     --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

with PSC.Strings;
package PSC.Source_Positions is

   --  A simple representation for a location within a source file.

   Max_Line_Number : constant := 2 ** 22 - 1;
   Max_Column_Number : constant := 2 ** 10 - 1;

   type Line_Number is range 0 .. Max_Line_Number;
   type Column_Number is range 0 .. Max_Column_Number;
   subtype File_Name_Index is Strings.U_String_Index;

   type Source_Position is record
      File : File_Name_Index := Strings.Null_U_String_Index;
      Line : Line_Number := 0;
      Col  : Column_Number := 0;
      End_Line : Line_Number := 0;
      End_Col  : Column_Number := 0;
   end record;
   pragma Pack (Source_Position);

   Null_Source_Position : constant Source_Position :=
     (File => Strings.Null_U_String_Index,
      Line => 0,
      Col => 0,
      End_Line => 0,
      End_Col => 0);

   function Image (Src_Pos : Source_Position;
                   Sep : String := ":") return String;
      --  Create a readable representation of the source position
      --  in the form "<file>:<line>:<col>:".
      --  If Sep is specified, it takes the place of ":".

   function Hash (Src_Pos : Source_Position) return Strings.Hash_Type;
      --  Return a hash of Src_Pos, ignoring End_Line and End_Col.

end PSC.Source_Positions;
