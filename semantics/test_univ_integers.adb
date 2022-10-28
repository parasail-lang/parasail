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

with PSC.String_Streams; use PSC.String_Streams;
with PSC.Univ_Integers; use PSC.Univ_Integers;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with PSC.Interpreter; use PSC.Interpreter;
with Ada.Exceptions;
procedure Test_Univ_Integers is
   X : constant Univ_Integer := From_Word (42);
   Y : constant Univ_Integer := X ** 10;
   Z : constant Univ_Integer := From_Word (2) ** 100;
   A : constant Univ_Integer := From_Word (2) ** 40;
   B : constant Univ_Integer := From_Word (2) ** 30 + From_Word (3);
   M1 : constant Univ_Integer := From_Word (-1);
   M1_P2 : constant Univ_Integer := From_Word (-1) + From_Word (2);
   Buf : aliased Buffered_Stream;
   Out1, Out2, In1, In2 : Univ_Integer;
begin
   PSC.Interpreter.Start_Up_Thread_Servers;

   Put_Line ("42 = " & Image (X));
   Put_Line ("42 ** 10 = " & Image (Y));
   Put_Line ("-1 = " & Image (M1));
   Put_Line ("-1 + 2 = " & Image (M1_P2));
   Put_Line ("2 ** 100 = " & Hex_Image (Z));

   Put_Line ("2**40 = " & Hex_Image (A));
   Put_Line ("2**30+3 = " & Hex_Image (B));
   Put_Line ("(2**40) * (2**30+3) = " &
     Hex_Image (A * B));
   Put_Line ("(2**40) / (2**30+3) = " &
     Hex_Image (A / B));

   Put_Line ("-(2**40) / (2**30+3) = " &
     Hex_Image ((-A) / B));

   Put_Line ("-(2**40) rem (2**30+3) = " &
     Hex_Image ((-A) rem B));

   Put_Line ("-(2**40) mod (2**30+3) = " &
     Hex_Image ((-A) mod B));

   Put_Line ("(2**100) / (2**30+3) = " &
     Hex_Image (Z / B));

   Put_Line ("(2**100) rem (2**30+3) = " &
     Hex_Image (Z rem B));

   Put_Line ("-(2**100) mod (2**30+3) = " &
     Hex_Image ((-Z) mod B));

   Put_Line ("2**120 / (2**100 + 3) = " &
     Hex_Image ((From_Word (2)**120) / (Z + From_Word (3))));

   Put_Line ("2**120 rem (2**100 + 3) = " &
     Hex_Image ((From_Word (2)**120) rem (Z + From_Word (3))));

   Put_Line ("-2**120 mod (2**100 + 3) = " &
     Hex_Image ((-From_Word (2)**120) mod (Z + From_Word (3))));

   Put_Line ("(5 * 2**64 - 1) / (4 * 2**64 + 1) = " &
     Hex_Image (((From_Word (5) * From_Word (2)**64 - One) /
                (From_Word (4) * From_Word (2)**64 + One))));

   Put_Line ("(2**94 - 1) / (2**64 + 1) = " &
     Hex_Image (((From_Word (2)**94 - One) /
                (From_Word (2)**64 + One))));

   Put_Line ("2**127 - 1 = " &
     Hex_Image (From_Word (2)**127 - One));

   Put_Line ("2**133 / 2**33 = " &
     Hex_Image (From_Word (2)**133 / From_Word (2)**33));

   Put_Line ("2**127 - 1 = 2**127? " &
     Boolean'Image ((From_Word (2)**127 - One) = From_Word (2)**127));

   Put_Line ("2**126 + 3 * 2**125 = " &
     Hex_Image (From_Word (2)**126) & " + " &
       Hex_Image (From_Word (3) * From_Word (2)**125) & " = " &
     Hex_Image (From_Word (2)**126 + From_Word (3) * From_Word (2)**125));

   Put_Line ("Value(""16#ABCD_0000_EE33_0000_1234#"") = " &
     Hex_Image (Value ("16#ABCD_0000_EE33_0000_1234#")));

   Out1 := Value ("16#ABCD_0000_EE33_0000_1234#");
   Out2 := One - From_Word (2) ** 127;

   Put_Line ("About to 'Write " & Hex_Image (Out1) &
     " and " & Hex_Image (Out2));

   Univ_Integer'Write (Buf'Access, Out1);
   Univ_Integer'Write (Buf'Access, Out2);

   Put (" Stream contents:");
   for I in 1 .. Stream_Length (Buf) loop
      declare
         use Ada.Streams;
         Elem : Stream_Element;

         function Hex_Img (Elem : Stream_Element) return String is
            Img : String renames Hex_Image (From_Word (Word_Type (Elem)));
         begin
            return Img (Img'First + 3 .. Img'Last - 1);
               --  strip off leading 16# and trailing #
         end Hex_Img;

      begin
         Stream_Element'Read (Buf'Access, Elem);
         Put (" " & Hex_Img (Elem));
      end;
   end loop;
   Put_Line ("");

   Put_Line ("About to again 'Write " & Hex_Image (Out1) &
     " and " & Hex_Image (Out2));

   Reset_Stream (Buf);

   Univ_Integer'Write (Buf'Access, Out1);
   Univ_Integer'Write (Buf'Access, Out2);

   Put_Line ("About to 'Read back in two univ-ints");

   Univ_Integer'Read (Buf'Access, In1);
   Univ_Integer'Read (Buf'Access, In2);

   Put_Line ("Result of 'Read is " & Hex_Image (In1) &
     " and " & Hex_Image (In2));

   pragma Assert (Out1 = In1);
   pragma Assert (Out2 = In2);

   PSC.Univ_Integers.Dump_Stats;

   PSC.Interpreter.Shut_Down_Thread_Servers (Total_Errors => 0);
exception
   when E : others =>
      Put_Line ("Exception raised: " &
        Ada.Exceptions.Exception_Information (E));
      PSC.Interpreter.Shut_Down_Thread_Servers (Total_Errors => 1);
end Test_Univ_Integers;
