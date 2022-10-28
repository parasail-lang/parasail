------------------------------------------------------------------------------
--                     A T O M I C  O P E R A T I O N S                     --
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
with Ada95_Atomic_Operations.Arithmetic;
with Ada95_Atomic_Operations.Test_And_Set;
with Ada95_Atomic_Operations.Exchange;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Ada95_Atomic is

   type Atomic_Long is new Long_Integer;
   pragma Atomic (Atomic_Long);

   package Exchange is new Ada95_Atomic_Operations.Exchange (Atomic_Long);
   package Arith is new Ada95_Atomic_Operations.Arithmetic (Atomic_Long);
   package Test_And_Set renames Ada95_Atomic_Operations.Test_And_Set;

   X, Y : aliased Atomic_Long;
   T : aliased Test_And_Set.Test_And_Set_Flag;

   Prior_Zero : aliased Atomic_Long := 0;
begin
   Put_Line ("Testing Test_And_Set");
   Put_Line ("Clear");
   Test_And_Set.Atomic_Clear (T'Access);
   Put_Line ("Test-and-set #1 returns " &
     Boolean'Image (Test_And_Set.Atomic_Test_And_Set (T'Access)));
   Put_Line ("Test-and-set #2 returns " &
     Boolean'Image (Test_And_Set.Atomic_Test_And_Set (T'Access)));

   Put_Line ("Testing fetch-and-add");
   X := 0;
   Put_Line ("X = " & Atomic_Long'Image (X));
   Put_Line ("Fetch-and-add (X'Access, 2) = " &
     Atomic_Long'Image (Arith.Atomic_Fetch_And_Add (X'Access, 2)));
   Put_Line ("X = " & Atomic_Long'Image (X));
   Put_Line ("Fetch-and-add (X'Access, -3) = " &
     Atomic_Long'Image (Arith.Atomic_Fetch_And_Add (X'Access, -3)));
   Put_Line ("X = " & Atomic_Long'Image (X));

   Put_Line ("Testing compare-and-exchange");
   Y := 0;
   Put_Line ("Y = " & Y'Img);
   Put_Line ("Compare-and-Swap (Y'Access, Prior => 0, Desired => -3) = " &
     Exchange.Atomic_Compare_And_Exchange
       (Y'Access, Prior_Zero'Access, Desired => -3)'Img);
   Put_Line ("Y = " & Y'Img);
   Put_Line ("Prior_Zero = " & Prior_Zero'Img);
   Put_Line ("Compare-and-Swap (Y'Access, Prior => 0, Desired => 5) = " &
     Exchange.Atomic_Compare_And_Exchange
       (Y'Access, Prior_Zero'Access, Desired => 5)'Img);
   Put_Line ("Y = " & Y'Img);
   Put_Line ("Prior_Zero = " & Prior_Zero'Img);

end Test_Ada95_Atomic;
