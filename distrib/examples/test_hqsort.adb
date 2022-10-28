--  pragma Ada_2020;

with Ada.Containers;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Distributed_Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Hyper_Sorting;
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with Ada.Calendar;
with Ada.Unchecked_Conversion;

procedure Test_HQSort is

   package Dis_Vec is new Distributed_Vectors
     (Index_Type => Positive,
      Element_Type => Float,
      Distrib_Type_Id => "Dis_Vec_Float");

   type Array_Type is array (Positive range <>) of Float;

   package My_Sorter is new
      Hyper_Sorting (Float, Positive, Array_Type, Dis_Vec);

   Len : constant Natural :=
     (if Argument_Count > 0 then Integer'Value (Argument (1)) else 100);

   Max_Plus_One : constant Natural := Len * 2;

   Num_Threads_To_Use : constant Natural :=
     (if Argument_Count > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);

   --  For Random
   Gen : Generator;

   type Float_Int is mod 2 ** Float'Size;
   function To_Int is new Ada.Unchecked_Conversion (Float, Float_Int);

   procedure Print_Arr (Arr : Array_Type) is
   begin
      for I in Arr'Range loop
         if I not in 51 .. Arr'Length - 50 then
            Put (" " & Arr (I)'Image);
            if I < Arr'Length then
               Put (",");
               if I mod 5 = 0 then
                  New_Line;
               end if;
               if I = 50 and then Arr'Length > 100 then
                  Put_Line (" ...");
               end if;
            end if;
         end if;
      end loop;

      New_Line;
   end Print_Arr;

   procedure Print_Vec (Vec : Dis_Vec.Vector) is
      use Dis_Vec;
      use type Ada.Containers.Count_Type;
      Len : constant Natural := Natural (Length (Vec));
   begin
      for I in 1 .. Len loop
         if I not in 51 .. Len - 50 then
            Put (" " & Vec (I)'Image);
            if I < Len then
               Put (",");
               if I mod 5 = 0 then
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

   --  Vec : Dis_Vec.Vector := [];
   type Array_Type_Ptr is access Array_Type;

   Arr_Ptr : constant Array_Type_Ptr := new Array_Type (1 .. Len);
   Arr2_Ptr : constant Array_Type_Ptr := new Array_Type (1 .. Len);
   Arr : Array_Type renames Arr_Ptr.all;
   Arr2 : Array_Type renames Arr2_Ptr.all;

   Time_Start : array (1 .. 2) of Ada.Calendar.Time;
   Time_End : array (1 .. 2) of Ada.Calendar.Time;

   use Ada.Calendar;

begin

   Reset (Gen, Initiator => Len);
   Put_Line ("Seed = " & Len'Image);

   for I in 1 .. Len loop
      Arr (I) := Random (Gen) * Float (Max_Plus_One);
   end loop;

   Arr2 := Arr;

   New_Line;
   Put_Line ("Before sort, Arr = ");
   Print_Arr (Arr);

   Time_Start (1) := Ada.Calendar.Clock;
   My_Sorter.Hyper_Qsort (Arr, Num_Chunks => Num_Threads_To_Use);
   Time_End (1) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After HQ sort, Arr = ");
   Print_Arr (Arr);

   Time_Start (2) := Ada.Calendar.Clock;
   My_Sorter.Hyper_Qsort (Arr2, Num_Chunks => Num_Threads_To_Use * 2);
   Time_End (2) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After 2nd HQ sort, Arr2 = ");
   Print_Arr (Arr2);

   New_Line;
   for I in 1 .. 2 loop
      Put_Line ("Sort" & I'Image & " Time:" &
        Duration'Image (Time_End (I) - Time_Start (I)));
   end loop;

   New_Line;
   Put_Line ("Checking if sorts worked:");

   Put_Line ("Comparing Arr and Arr2 => " &
     Boolean'Image (for all I in Arr'Range => Arr (I) = Arr2 (I)));

   New_Line;
   Put_Line ("Arr is sorted?");
   for I in 1 .. Arr'Length - 1 loop
      declare
         Left : constant Float := Arr (I);
         Right : constant Float := Arr (I + 1);
      begin
         if Left > Right then
            Put_Line ("Arr (" & I'Image & " ) > Arr (" &
              Integer'Image (I + 1) & " ):" &
              Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
            Put_Line (" aka:" & To_Int (Left)'Image & " >" &
              To_Int (Right)'Image);
         end if;
      end;
   end loop;

   New_Line;
   Put_Line ("Arr2 is sorted?");
   for I in 1 .. Arr2'Length - 1 loop
      declare
         Left : constant Float := Arr2 (I);
         Right : constant Float := Arr2 (I + 1);
      begin
         if Left > Right then
            Put_Line ("Arr2 (" & I'Image & " ) > Arr2 (" &
              Integer'Image (I + 1) & " ):" &
              Long_Float (Left)'Image & " >" & Long_Float (Right)'Image);
            Put_Line (" aka:" & To_Int (Left)'Image & " >" &
              To_Int (Right)'Image);
         end if;
      end;
   end loop;

end Test_HQSort;
