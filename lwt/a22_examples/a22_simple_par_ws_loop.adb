------------------------------------------------------------------------------
--                            Ada 202X Parallelism                          --
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

--  Example using Ada 2022
pragma Ada_2022;
--  Testing simple loop

with LWT.Work_Stealing; use LWT.Work_Stealing;
with Ada.Text_IO; use Ada.Text_IO;
procedure A22_Simple_Par_WS_Loop is
   Num_Chunks : constant := 10;

   Control : WS_Parallel (Num_Servers => 6, Options => null);
                              --  use 6 server threads

   type Int_Arr is array (Positive range <>) of Integer;

   pragma Warnings (Off);
   Arr : access Int_Arr := new Int_Arr (1 .. 100_000_000);
   pragma Warnings (On);

   Partial_Sums : array (1 .. Num_Chunks) of Long_Integer := (others => 0);

   Total : Long_Integer := 0;

begin

   --  Initialize array to simple sequence.
   --  NOTE: We use a separate task to use a different processor's cache.
   declare
      task Init;
      task body Init is
      begin
         for I in Arr'Range loop
            Arr (I) := I;
         end loop;
      end Init;
   begin
      null;
   end;

   begin
      --  Now sum the array in parallel

      parallel (Chunk in Partial_Sums'Range)
      for I in Arr'Range loop

         if Partial_Sums (Chunk) = 0 then
            Put_Line (" Loop_Body doing chunk" & Chunk'Image);
         end if;

         Partial_Sums (Chunk) := Partial_Sums (Chunk) + Integer'Pos (Arr (I));
      end loop;
   end;

   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Put_Line ("  Partial_Sums (" & I'Image & " ) =" &
        Partial_Sums (I)'Image);

      Total := Total + Partial_Sums (I);
   end loop;

   Put_Line ("Final total = " & Total'Image);

end A22_Simple_Par_WS_Loop;
