------------------------------------------------------------------------------
--                              Test Distrib Sort                           --
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
------------------------------------------------------------------------------

--  pragma Ada_2020;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Distrib_Hyper_Sorting;
with Gen_Qsort;
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Parallelism;
with System_Distrib; use System_Distrib;
with Distributed_Vectors;

procedure Test_Distrib_Sort is

   Debug : constant Boolean := True;

   Num_Args : constant Natural := Argument_Count;

   Len : constant Natural :=
     (if Num_Args > 0 then Integer'Value (Argument (1)) else 100);

   Num_Threads_To_Use : constant Natural :=
     (if Num_Args > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

   Default_Num_Nodes : constant := 4;

   Min_Num_Nodes : constant Natural :=
     (if Num_Args < 3 then Default_Num_Nodes
      else Natural'Value (Argument (3)));

   --  Join a group; find out local portion of problem space determined by
   --  effective thread count.
   GC : aliased Group_Context := Join_Group
     ("com.adacore.taft.hyper_sort" & Min_Num_Nodes'Image, Num_Threads_To_Use,
      Min_Num_Nodes => Min_Num_Nodes);

   --  For Random
   Gen : Generator;

   Time_Start : array (1 .. 2) of Ada.Calendar.Time;
   Time_End : array (1 .. 2) of Ada.Calendar.Time;

   use Ada.Calendar;

   package Distrib_Decls is
      package Dis_Vec is new Distributed_Vectors
        (Index_Type => Positive,
         Element_Type => Float,
         Distrib_Type_Id => "Dis_Vec_Float");

      subtype Vec_Type is Dis_Vec.Vector;

      package Dis_Sorter is new
         Distrib_Hyper_Sorting
           (Elem_Type => Float, Dis_Vec => Dis_Vec);

      procedure Print_Vec (Vec : Vec_Type);

   end Distrib_Decls;

   package body Distrib_Decls is
      procedure Print_Vec (Vec : Vec_Type) is
         use Dis_Vec;
         Len : constant Natural := Natural (Vec.Length);
      begin
         for I in 1 .. Len loop
            if I not in 51 .. Len - 50 and then I mod 10 = 0 then
               Put (" " & Vec (I)'Image);
               if I < Len then
                  Put (",");
                  if I mod 50 = 0 then
                     New_Line;
                  end if;
                  if I = 50 and then Len > 100 then
                     Put_Line (" ...");
                  end if;
               end if;
            end if;
         end loop;

         New_Line;
      end Print_Vec;

   end Distrib_Decls;

   procedure Distrib_Sort is
      use Distrib_Decls;

      Lvec : Vec_Type (GC'Unchecked_Access, 1);

      Max_Plus_One : constant Natural := Len * 2;  --  Maximum value in array

   begin  --  Distrib_Sort

      Reset (Gen, Initiator => Len + Integer (GC.Node_Index));
      Put_Line ("Seed = " & Integer'Image (Len + Integer (GC.Node_Index)));

      for I in 1 .. Len loop
         Lvec.Append (Random (Gen) * Float (Max_Plus_One));
      end loop;

      declare
         I : Natural := 0;
         procedure Loop_Body (Element : Float) is
         begin
            if I = 0 then
               Put_Line ("Display of" & Lvec.Length'Image &
                 " elements of vector shard on node" &
                 GC.Node_Index'Image);
            end if;
            I := I + 1;
            if I mod 5 = 0 then
               Put_Line (I'Image & ":" & Element'Image);
            end if;
         end Loop_Body;
      begin
         if False then
            Lvec.Iterate (Process => Loop_Body'Access);
         end if;
      end;

      Put_Line (GC.Node_Index'Image & ": About to copy vector");

      declare
         Lvec2 : Vec_Type := Lvec.Copy_Vector;
      begin
         declare
            I : Natural := 0;
            procedure Loop_Body (Element : Float) is
            begin
               if I = 0 then
                  Put_Line ("Display of" & Lvec2.Length'Image &
                    " elements of vector 2 shard on node" &
                    GC.Node_Index'Image);
               end if;
               I := I + 1;
               if I mod 5 = 0 then
                  Put_Line (I'Image & ":" & Element'Image);
               end if;
            end Loop_Body;
         begin
            if False then
               Lvec2.Iterate (Process => Loop_Body'Access);
            end if;
         end;

         New_Line;
         if Debug then
            Put_Line ("Before sort, Lvec = ");
            Print_Vec (Lvec);
         end if;

         Time_Start (1) := Ada.Calendar.Clock;
         Dis_Sorter.Hyper_Qsort (Lvec);
         Time_End (1) := Ada.Calendar.Clock;

         New_Line;
         if Debug then
            Put_Line ("After HQ sort, Lvec = ");
            Print_Vec (Lvec);
         end if;

         Time_Start (2) := Ada.Calendar.Clock;
         Dis_Sorter.Hyper_Qsort (Lvec2);
         Time_End (2) := Ada.Calendar.Clock;

         New_Line;
         if Debug then
            Put_Line ("After 2nd HQ sort, Lvec2 = ");
            Print_Vec (Lvec2);
         end if;

         New_Line;
         for I in 1 .. 2 loop
            Put_Line ("Sort" & I'Image & " Time:" &
              Duration'Image (Time_End (I) - Time_Start (I)));
         end loop;

         New_Line;
         Put_Line ("Checking if sorts worked:");

         Put_Line ("Comparing Lvec and Lvec2 => " &
           Boolean'Image (for all I in 1 .. Lvec.Length =>
                             Lvec (I) = Lvec2 (I)));

         New_Line;
         Put_Line ("Lvec is sorted?");
         for I in 1 .. Lvec.Length - 1 loop
            declare
               Left : constant Float := Lvec (I);
               Right : constant Float := Lvec (I + 1);
            begin
               if Left > Right then
                  Put_Line ("Lvec (" & I'Image & " ) > Lvec (" &
                    Integer'Image (I + 1) & " ):" &
                    Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
               end if;
            end;
         end loop;

         New_Line;
         Put_Line ("Lvec2 is sorted?");
         for I in 1 .. Lvec2.Length - 1 loop
            declare
               Left : constant Float := Lvec2 (I);
               Right : constant Float := Lvec2 (I + 1);
            begin
               if Left > Right then
                  Put_Line ("Lvec2 (" & I'Image & " ) > Lvec2 (" &
                    Integer'Image (I + 1) & " ):" &
                    Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
               end if;
            end;
         end loop;
      end;

   end Distrib_Sort;

begin

   Distrib_Sort;

   Finish (GC);
exception
   when E : others =>
      Put_Line ("Exception " & Ada.Exceptions.Exception_Name (E) &
        "raised: " & Ada.Exceptions.Exception_Information (E));
end Test_Distrib_Sort;
