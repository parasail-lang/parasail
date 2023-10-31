pragma Ada_2022;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;
with LWT.Work_Stealing; use LWT.Work_Stealing;
with Ada.Calendar;
with LWT.Par_Constrained_Sort;
procedure Test_Par_QSort is

   type Array_Type is array (Positive range <>) of Float;

   Len : constant Natural :=
     (if Argument_Count > 0 then Integer'Value (Argument (1)) else 100);

   subtype Con_Index_Type is Positive range 1 .. Len;

   subtype Con_Array_Type is Array_Type (Con_Index_Type);

   procedure Par_Qsort is new LWT.Par_Constrained_Sort
     (Element_Type => Float,
      Index_Type => Con_Index_Type,
      Array_Type => Con_Array_Type);

   Max_Plus_One : constant Natural := Len * 2;

   Num_Threads_To_Use : constant Natural :=
     (if Argument_Count > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

   --  For Random
   Gen : Generator;

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
   Par_Qsort (Arr);
   Time_End (1) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After Par Qsort, Arr = ");
   Print_Arr (Arr);

   Time_Start (2) := Ada.Calendar.Clock;
   Par_Qsort (Arr2);
   Time_End (2) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After 2nd Par Qsort, Arr2 = ");
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
      if Arr (I) > Arr (I + 1) then
         Put_Line ("Arr (" & I'Image & " ) > Arr (" &
           Integer'Image (I + 1) & " ):" &
           Arr (I)'Image & " >" & Arr (I + 1)'Image);
      end if;
   end loop;

   New_Line;
   Put_Line ("Arr2 is sorted?");
   for I in 1 .. Arr2'Length - 1 loop
      if Arr2 (I) > Arr2 (I + 1) then
         Put_Line ("Arr2 (" & I'Image & " ) > Arr2 (" &
           Integer'Image (I + 1) & " ):" &
           Arr2 (I)'Image & " >" & Arr2 (I + 1)'Image);
      end if;
   end loop;

end Test_Par_QSort;
