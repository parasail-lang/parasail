--  pragma Ada_2020;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Generic_Hyper_Sorting;
with Seq_Qsort; pragma Elaborate (Seq_Qsort);
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with Ada.Calendar;
with Ada.Unchecked_Conversion;
with System.Parallelism;

procedure Test_Gen_HSort is

   type Array_Type is array (Positive range <>) of Float;

   function Length (A : Array_Type) return Natural is (A'Length);

   function First (A : Array_Type) return Positive is (A'First);

   function Image (Val : Float) return String is (Val'Image);

   function "/" (Left : Float; Right : Integer) return Float is
     (Left / Float (Right));

   function Element
     (A : Array_Type; Index : Positive;
      Part : Natural := 0)
     return Float is
      pragma Unreferenced (Part);
   begin
      return A (Index);
   end Element;

   function Copy (A : in out Array_Type) return Array_Type is (A);

   procedure Assign_Slice
     (From : Array_Type;
      From_Part : Positive;
      From_First : Positive;
      To : in out Array_Type;
      To_Part : Positive;
      To_First : Positive;
      Count : Natural) is
      pragma Unreferenced (From_Part, To_Part);
   begin
      To (To_First .. To_First + Count - 1) :=
        From (From_First .. From_First + Count - 1);
   end Assign_Slice;

   procedure Quicksort is new Seq_Qsort (Positive, Float, Array_Type);

   procedure Sort_One_Part
     (A : in out Array_Type;
      First : Positive; Last : Natural;
      Part : Natural := 0) is
      pragma Unreferenced (Part);
   begin
      Quicksort (A (First .. Last));
   end Sort_One_Part;

   procedure For_Each_Part
     (Num_Parts : Positive;
      Loop_Body : access procedure (Part_Index : Positive)) is

      use System.Parallelism;

      procedure Iter_Loop_Body
        (Low, High : Longest_Integer; Local_Part_Index : Positive) is
         --  NOTE: When doing things in parallel,
         --        Low = High = Local_Part_Index.
         --        When doing things sequentially,
         --        Low = Local_Part_Index = 1, High = Num_Parts.
         pragma Assert (Positive (Low) = Local_Part_Index);
      begin
         for Part_Index in Integer (Low) .. Integer (High) loop
            Loop_Body (Part_Index);
         end loop;
      end Iter_Loop_Body;
   begin
      Par_Range_Loop (1, Longest_Integer (Num_Parts), Num_Parts,
        Loop_Body => Iter_Loop_Body'Access);
   end For_Each_Part;

   package My_Sorter is new
      Generic_Hyper_Sorting
        (Elem_Type => Float,
         Image => Image,
         Elem_First => Float'First,
         Elem_Last => Float'Last,
         Elem_Zero => 0.0,
         Index_Type => Positive,
         Part_Index_Type => Positive,
         Indexable_Type => Array_Type,
         Length => Length,
         First => First,
         Element => Element,
         Move => Copy,
         Copy_Slice => Assign_Slice,
         Sort_One_Part => Sort_One_Part,
         For_Each_Part => For_Each_Part);

   Len : constant Natural :=
     (if Argument_Count > 0 then Integer'Value (Argument (1)) else 100);

   Max_Plus_One : constant Natural := Len * 2;

   Num_Threads_To_Use : constant Natural :=
     (if Argument_Count > 1 then Integer'Value (Argument (2)) else 10);

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

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
   My_Sorter.Hyper_Qsort (Arr, Num_Parts => Num_Threads_To_Use);
   Time_End (1) := Ada.Calendar.Clock;

   New_Line;
   Put_Line ("After HQ sort, Arr = ");
   Print_Arr (Arr);

   Time_Start (2) := Ada.Calendar.Clock;
   My_Sorter.Hyper_Qsort (Arr2, Num_Parts => Num_Threads_To_Use * 2);
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

end Test_Gen_HSort;
