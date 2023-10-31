------------------------------------------------------------------------------
--                              L W T Scheduler                             --
--                                                                          --
--                     Copyright (C) 2012-2023, AdaCore                     --
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
------------------------------------------------------------------------------

with LWT.Generic_Synchronized_Deques;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Deque is
   package Deques is new LWT.Generic_Synchronized_Deques (Integer, Empty => 0);
   type Deque is new Deques.Deque;

   DQ : Deque;

   X : Integer;
   Steal_Failed : Boolean;
begin
   Put_Line ("Test_Deque: Initial length = " &
     Length (DQ)'Image);

   Push (DQ, 1);
   Push (DQ, 2);
   Push (DQ, 3);

   Put_Line (" 3 Pushes, length = " &
     Length (DQ)'Image);

   Pop (DQ, X);

   Put_Line (" 1 Pop of " & X'Image & ", length = " &
     Length (DQ)'Image);

   Steal (DQ, X, Steal_Failed);

   Put_Line (" 1 Steal of " & X'Image & ", length = " &
     Length (DQ)'Image & ", Steal_Failed = " & Steal_Failed'Image);

   Push (DQ, 4);

   Put_Line (" Push a 4, all elements:");
   declare
      Elems : constant Deques.Element_Vector := All_Elements (DQ);
   begin
      for E of Elems loop
         Put (E'Image);
      end loop;
      New_Line;
   end;

   Pop (DQ, X);
   pragma Assert (X /= 0);

   Pop (DQ, X);
   pragma Assert (X /= 0);

   Pop (DQ, X);
   pragma Assert (X = 0);

   Steal (DQ, X, Steal_Failed);
   pragma Assert (X = 0);
   pragma Assert (Steal_Failed = False);
   Put_Line (" Third pop was empty, as was a steal");
end Test_Deque;
