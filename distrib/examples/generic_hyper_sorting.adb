--  pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Hyper_Sorting is

   subtype Nat_Index_Type is Index_Type'Base range 0 .. Index_Type'Last;

   type Nat_Array is array (Part_Index_Type range <>) of Nat_Index_Type;

   Debug : constant Boolean := True;

   type Per_Part is record
      Min : Elem_Type := Elem_Last;
      Max : Elem_Type := Elem_First;
      Sum : Elem_Type := Elem_Zero;
      Sum_Sq : Elem_Type := Elem_Zero;

      Old_Part_First : Index_Type := Index_Type'First;
      Old_Part_Last : Index_Type'Base := Index_Type'Base'First;

      New_Part_Start_Value : Elem_Type := Elem_First;
      New_Part_End_Value : Elem_Type := Elem_First;

      New_Part_First_Index : Index_Type := Index_Type'First;
      New_Part_Size : Nat_Index_Type := 0;
   end record;

   type Transfer_Vector (Num_Parts : Part_Index_Type) is record
      Amount_To_Send : Nat_Array (1 .. Num_Parts) := (others => 0);
      Starting_Index : Nat_Array (1 .. Num_Parts) := (others => 0);
   end record;

   function Part_Sizes (Overall_Len : Nat_Index_Type;
     Num_Parts : Part_Index_Type)
     return Nat_Array
     with Post => Part_Sizes'Result'Length = Num_Parts;
   --  Divide the overall length into a vector of Part sizes which together
   --  add up to Overall_Len, while keeping the differences in Part size
   --  no greater than one.

   function Part_Sizes (Overall_Len : Nat_Index_Type;
     Num_Parts : Part_Index_Type)
     return Nat_Array is
      Part_Size : constant Nat_Index_Type := Overall_Len /
                                                Nat_Index_Type (Num_Parts);
      Extra : constant Nat_Index_Type :=
        Overall_Len mod Nat_Index_Type (Num_Parts);
      Sum : Nat_Index_Type := 0;  --  Could be a Ghost variable
   begin
      return Result : Nat_Array (1 .. Num_Parts) do
         for I in 1 .. Num_Parts loop
            if Nat_Index_Type (I) <= Extra then
               --  This Part is slightly bigger
               Result (I) := Part_Size + 1;
            else
               Result (I) := Part_Size;
            end if;
            Sum := Sum + Result (I);
         end loop;
         pragma Assert (Sum = Overall_Len);
      end return;
   end Part_Sizes;

   procedure Hyper_Qsort
     (A : in out Indexable_Type; Num_Parts : Part_Index_Type) is

      Length_Vec : constant Nat_Array (1 .. Num_Parts) :=
        Part_Sizes (Length (A), Num_Parts);

      Per_Part_Info : array (1 .. Num_Parts) of Per_Part;

      Transfer_Matrix : array (1 .. Num_Parts)
        of Transfer_Vector (Num_Parts);

      Start_Index : Index_Type := 1;

   begin  --  Hyper_Qsort

      --  Initialize start/end index for each Part.
      for I in 1 .. Num_Parts loop
         Per_Part_Info (I).Old_Part_First := Start_Index;
         Start_Index := Start_Index + Length_Vec (I);
         Per_Part_Info (I).Old_Part_Last := Start_Index - 1;
      end loop;

      if Debug then
         Put_Line ("Orig Parts:");
         for PC of Per_Part_Info loop
            Put ("[" & PC.Old_Part_First'Image & " .." &
               PC.Old_Part_Last'Image & " ]");
         end loop;
         New_line;
      end if;

      --  Sort each Part and gather statistics, in parallel
      declare
         procedure Sort_One_Part (Part_Index : Part_Index_Type) is
            PC : Per_Part renames Per_Part_Info (Part_Index);
            First : constant Index_Type := PC.Old_Part_First;
            Last : constant Nat_Index_Type := PC.Old_Part_Last;
         begin

            Sort_One_Part (A, First, Last, Part => Part_Index);

            --  Gather per-Part statistics
            PC.Min := Element (A, First);
            PC.Max := Element (A, Last);

            PC.Sum := Elem_Zero;
            PC.Sum_Sq := Elem_Zero;
            for I in First .. Last loop
               PC.Sum := PC.Sum + Element (A, I);
               PC.Sum_Sq := PC.Sum_Sq + Element (A, I) ** 2;
            end loop;
         end Sort_One_Part;

      begin

         --  Do the parallel loop
         For_Each_Part (Num_Parts, Loop_Body => Sort_One_Part'Access);
      end;

      --  Gather overall statistics
      declare
         Total_Sum : Elem_Type := Elem_Zero;
         Total_SSq : Elem_Type := Elem_Zero;
         Overall_Min : Elem_Type := Elem_Last;
         Overall_Max : Elem_Type := Elem_First;
         Total_Num : constant Nat_Index_Type := Length (A);
         Avg : Elem_Type;
         Variance : Elem_Type;
         Next_Pivot : Elem_Type;
         Increment : Elem_Type;

      begin

         for PC of Per_Part_Info loop
            Total_Sum := Total_Sum + PC.Sum;
            Total_SSq := Total_SSq + PC.Sum_Sq;
            if PC.Min < Overall_Min then
               Overall_Min := PC.Min;
            end if;
            if Overall_Max < PC.Max then
               Overall_Max := PC.Max;
            end if;
         end loop;

         if Debug then
            Put_Line ("Sum =" & Image (Total_Sum) &
              ", Sum_Sq =" & Image (Total_SSq));
         end if;

         Avg := Total_Sum / Integer (Total_Num);
         Variance := Total_SSq / Integer (Total_Num) - (Avg) ** 2;

         if Debug then
            Put_Line ("Avg =" & Image (Avg) &
              ", Variance =" & Image (Variance) &
              ", Min =" & Image (Overall_Min) &
              ", Max =" & Image (Overall_Max));
         end if;

         --  Choose pivots so that new Parts are of nearly equal size
         --  TBD: Current algorithm is the simplest possible; presumes
         --       flat distribution.
         Next_Pivot := Overall_Min;
         Increment := (Overall_Max - Overall_Min) / Integer (Num_Parts);
         for PC of Per_Part_Info loop
            PC.New_Part_Start_Value := Next_Pivot;
            Next_Pivot := Next_Pivot + Increment;
            PC.New_Part_End_Value := Next_Pivot;
         end loop;

         Per_Part_Info (Num_Parts).New_Part_End_Value := Elem_Last;
            --  Make sure we put the highest values into the last Part.
      end;

      --  Compute amount going to new Part from each old Part
      declare

         procedure Compute_Transfer_Matrix
           (Source_Part_Index : Part_Index_Type) is
            Source_PC : Per_Part renames
              Per_Part_Info (Source_Part_Index);
            Source_TM : Transfer_Vector renames
              Transfer_Matrix (Source_Part_Index);

            --  Look at each target Part
            Source_Index : Index_Type := Source_PC.Old_Part_First;
            Source_Last : constant Nat_Index_Type :=
              Source_PC.Old_Part_Last;

         begin

            for Target_Index in 1 .. Num_Parts loop
               declare
                  Target_PC : Per_Part renames
                    Per_Part_Info (Target_Index);
                  New_Part_End_Value : constant Elem_Type :=
                    Target_PC.New_Part_End_Value;
                  Starting_Index : constant Index_Type := Source_Index;
               begin
                  --  Figure out which range of values to
                  --  send to target Part
                  Source_TM.Starting_Index (Target_Index) :=
                    Source_Index;
                  while Source_Index <= Source_Last
                    and then Element (A, Source_Index) < New_Part_End_Value
                  loop
                     Source_Index := Source_Index + 1;
                  end loop;
                  Source_TM.Amount_To_Send (Target_Index) :=
                    Source_Index - Starting_Index;
               end;
            end loop;
         end Compute_Transfer_Matrix;

      begin
         --  Do the parallel loop
         For_Each_Part (Num_Parts,
           Loop_Body => Compute_Transfer_Matrix'Access);
      end;

      --  Compute sizes of new Parts
      declare
         procedure Compute_New_Parts
           (Target_Index : Part_Index_Type) is
            Target_PC : Per_Part renames Per_Part_Info (Target_Index);
         begin
            Target_PC.New_Part_Size := 0;
            for Source_TM of Transfer_Matrix loop
               Target_PC.New_Part_Size := Target_Pc.New_Part_Size +
                 Source_TM.Amount_To_Send (Target_Index);
            end loop;
         end Compute_New_Parts;
      begin
         For_Each_Part (Num_Parts,
           Loop_Body => Compute_New_Parts'Access);
      end;

      if Debug then
         declare
            Min_Part_Size : Nat_Index_Type := Nat_Index_Type'Last;
            Max_Part_Size : Nat_Index_Type := Nat_Index_Type'First;
         begin
            for PC of Per_Part_Info loop
               Min_Part_Size :=
                 Nat_Index_Type'Min (Min_Part_Size, PC.New_Part_Size);
               Max_Part_Size :=
                 Nat_Index_Type'Max (Max_Part_Size, PC.New_Part_Size);
            end loop;

            New_Line;
            Put_Line
              ("New Part sizes:" &
                Min_Part_Size'Image & " .." &
                Max_Part_Size'Image);
         end;
      end if;

      --  Compute offsets of new Parts (sequentially)
      declare
         First_Index : Index_Type := First (A);
      begin
         for Target_PC of Per_Part_Info loop
            Target_PC.New_Part_First_Index := First_Index;
            First_Index := First_Index + Target_PC.New_Part_Size;
         end loop;
      end;

      if Debug then
         Put_Line ("New Parts:");
         for PC of Per_Part_Info loop
            Put_Line ("[" & PC.New_Part_First_Index'Image & " .." &
              Index_Type'Image (PC.New_Part_First_Index +
                PC.New_Part_Size - 1) & ", values:" &
                Image (PC.New_Part_Start_Value) & " ..<" &
                Image (PC.New_Part_End_Value));
         end loop;
         New_line;
      end if;

      --  Copy data to final resting place and sort
      declare
         type Indexable_Type_CPtr is access constant Indexable_Type;

         Temp_Ptr : constant Indexable_Type_CPtr :=
           new Indexable_Type'(Move (A));  --  First move/copy whole array
         Temp : Indexable_Type renames Temp_Ptr.all;

         procedure Init_And_Sort_New_Part
           (Target_Part_Index : Part_Index_Type) is
            Target_PC : Per_Part renames
              Per_Part_Info (Target_Part_Index);
            Target_Index : Index_Type := Target_PC.New_Part_First_Index;
         begin
            --  Now copy data from source Parts into target Part
            --  Copy from source (now in Temp) into target (back into A)
            for Source_Part_Index in 1 .. Num_Parts loop
               declare
                  Source_TM : Transfer_Vector renames
                    Transfer_Matrix (Source_Part_Index);
                  Amount_To_Send : constant Nat_Index_Type :=
                    Source_TM.Amount_To_Send (Target_Part_Index);
                  Starting_Index : constant Index_Type :=
                    Source_TM.Starting_Index (Target_Part_Index);
               begin

                  if Debug and then Amount_To_Send > 0 then
                     Put_Line ("Copying from" & Starting_Index'Image &
                       " .." &
                       Index_Type'Image
                         (Starting_Index + Amount_To_Send - 1) & " to" &
                       Target_Index'Image & " .." &
                       Index_Type'Image
                         (Target_Index + Amount_To_Send - 1) &
                       ", values:" & Image (Element (Temp, Starting_Index)) &
                       " .." &
                       Image (Element
                         (Temp, Starting_Index + Amount_To_Send - 1)));
                  end if;

                  Copy_Slice
                   (From => Temp, From_Part => Source_Part_Index,
                    From_Index => Starting_Index,
                    To => A, To_Part => Target_Part_Index,
                    To_Index => Target_Index,
                    Count => Amount_To_Send);

                  Target_Index := Target_Index + Amount_To_Send;
               end;
            end loop;

            --  Now sort the "new" Part
            Sort_One_Part
              (A, Target_PC.New_Part_First_Index,
                  Target_PC.New_Part_First_Index +
                      Target_PC.New_Part_Size - 1,
               Part => Target_Part_Index);

         end Init_And_Sort_New_Part;
      begin
         For_Each_Part (Num_Parts,
           Loop_Body => Init_And_Sort_New_Part'Access);
      end;
   end Hyper_Qsort;

end Generic_Hyper_Sorting;
