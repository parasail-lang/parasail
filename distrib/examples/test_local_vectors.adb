------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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

with Local_Vectors;
with Ada.Text_IO;
procedure Test_Local_Vectors is
   package Int_Vecs is new Local_Vectors (Integer);
   use Int_Vecs;
begin
   for K in 1 .. 20 loop
      declare
         Max : constant Elem_Index := Elem_Index (((K - 1) * 50) ** 3 + 100);
         V : Vector;
         J : Elem_Index;
      begin
         for I in 1 .. Max loop
            Add_Element (V, Integer (2 * I), J);
            pragma Assert (J = I);
         end loop;

         for I in 1 .. Max loop
            Set_Nth_Element (V, I, Nth_Element (V, I) - 1);
            pragma Assert (Nth_Element (V, I) = Integer (2 * I - 1));
         end loop;

         Ada.Text_IO.Put_Line ("Test succeeded with Max =" &
           Elem_Index'Image (Max));
      end;
   end loop;
end Test_Local_Vectors;
