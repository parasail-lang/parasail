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
--  Testing exceptions and Task_Id

with LWT.Work_Stealing; use LWT.Work_Stealing;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
procedure A22_Excep_WS_Loop is
   pragma Warnings (Off);
   Num_Chunks : constant := 10;

   Control : WS_Parallel (Num_Servers => 6, Options => null);
                              --  use 6 server threads

   type Int_Arr is array (Positive range <>) of Integer;

   Arr : access Int_Arr := new Int_Arr (1 .. 100_000_000);

   Partial_Sums : array (1 .. Num_Chunks) of Long_Integer := (others => 0);

   Task_Identities : array (1 .. Num_Chunks) of Task_Id;

   Early_Death : exception;

   Total : Long_Integer := 0;

   Task_Id_Mismatch : Boolean := False;

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
         if Task_Identities (Chunk) = Null_Task_Id then
            Put_Line (" Loop_Body doing chunk" & Chunk'Image);
         end if;

         --  Record the task identity
         Task_Identities (Chunk) := Current_Task;

         if Chunk = 2 or else Chunk = Num_Chunks - 1 then
            --  Test raising an exception
            Put_Line (" About to raise Early_Death exception.");
            raise Early_Death with "Exception during parallel iteration";
            --  Put_Line (" [not really!]");
         end if;

         Partial_Sums (Chunk) := Partial_Sums (Chunk) + Integer'Pos (Arr (I));
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception Information: " &
           Ada.Exceptions.Exception_Information (E));
   end;

   --  Now do the final summation
   for I in Partial_Sums'Range loop
      Put_Line ("  Partial_Sums (" & I'Image & " ) =" &
        Partial_Sums (I)'Image);
      if Task_Identities (I) = Null_Task_Id then
         Put_Line (" Task_Identities (" & I'Image & " ) is Null_Task_Id");
      else
         Put_Line (" Task_Identities (" & I'Image & " ) is " &
           Image (Task_Identities (I)));
         for J in 1 .. I - 1 loop
            if Task_Identities (J) /= Null_Task_Id and then
               Task_Identities (I) /= Task_Identities (J)
            then
               Put_Line (" Task_Identities (" & I'Image &
                " ) /= Task_Identities (" & J'Image & " )");
               Task_Id_Mismatch := True;
            end if;
         end loop;
      end if;

      Total := Total + Partial_Sums (I);
   end loop;

   if not Task_Id_Mismatch then
      Put_Line (" Non-null Task-Ids all match");
   end if;

   Put_Line ("Final total = " & Total'Image);

   pragma Warnings (On);
end A22_Excep_WS_Loop;
