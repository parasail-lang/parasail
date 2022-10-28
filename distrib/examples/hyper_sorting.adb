--  pragma Ada_2020;

with Seq_Qsort; pragma Elaborate (Seq_Qsort);
with System.Parallelism;
with System_Distrib;
with Ada.Text_IO; use Ada.Text_IO;
package body Hyper_Sorting is

   subtype Nat_Index_Type is Index_Type'Base range 0 .. Index_Type'Last;

   Debug : constant Boolean := False;

   type Nat_Array is array (Positive range <>) of Nat_Index_Type;

   procedure Quicksort is new Seq_Qsort (Index_Type, Elem_Type, Array_Type);

   type Per_Chunk is record
      Min : Elem_Type := 0.0;
      Max : Elem_Type := 0.0;
      Sum : Elem_Type := 0.0;
      Sum_Sq : Elem_Type := 0.0;

      Old_Chunk_First : Index_Type := Index_Type'First;
      Old_Chunk_Last : Index_Type'Base := Index_Type'Base'First;

      New_Chunk_Start_Value : Elem_Type := 0.0;
      New_Chunk_End_Value : Elem_Type := 0.0;

      New_Chunk_First_Index : Index_Type := Index_Type'First;
      New_Chunk_Size : Nat_Index_Type := 0;
   end record;

   type Transfer_Vector (Num_Chunks : Positive) is record
      Amount_To_Send : Nat_Array (1 .. Num_Chunks) := (others => 0);
      Starting_Index : Nat_Array (1 .. Num_Chunks) := (others => 0);
   end record;

   function Chunk_Sizes (Overall_Len : Nat_Index_Type; Num_Chunks : Natural)
     return Nat_Array
     with Post => Chunk_Sizes'Result'Length = Num_Chunks;
   --  Divide the overall length into a vector of chunk sizes which together
   --  add up to Overall_Len, while keeping the differences in chunk size
   --  no greater than one.

   function Chunk_Sizes (Overall_Len : Nat_Index_Type; Num_Chunks : Natural)
     return Nat_Array is
      Chunk_Size : constant Nat_Index_Type := Overall_Len /
                                                Nat_Index_Type (Num_Chunks);
      Extra : constant Natural := Natural (Overall_Len) mod Num_Chunks;
      Sum : Nat_Index_Type := 0;  --  Could be a Ghost variable
   begin
      return Result : Nat_Array (1 .. Num_Chunks) do
         for I in 1 .. Num_Chunks loop
            if I <= Extra then
               --  This chunk is slightly bigger
               Result (I) := Chunk_Size + 1;
            else
               Result (I) := Chunk_Size;
            end if;
            Sum := Sum + Result (I);
         end loop;
         pragma Assert (Sum = Overall_Len);
      end return;
   end Chunk_Sizes;

   procedure Hyper_Qsort (A : in out Array_Type; Num_Chunks : Natural) is

      Length_Vec : constant Nat_Array (1 .. Num_Chunks) :=
        Chunk_Sizes (A'Length, Num_Chunks);

      Per_Chunk_Info : array (1 .. Num_Chunks) of Per_Chunk;

      Transfer_Matrix : array (1 .. Num_Chunks)
        of Transfer_Vector (Num_Chunks);

      Start_Index : Index_Type := 1;

   begin  --  Hyper_Qsort

      --  Initialize start/end index for each chunk.
      for I in 1 .. Num_Chunks loop
         Per_Chunk_Info (I).Old_Chunk_First := Start_Index;
         Start_Index := Start_Index + Length_Vec (I);
         Per_Chunk_Info (I).Old_Chunk_Last := Start_Index - 1;
      end loop;

      if Debug then
         Put_Line ("Orig chunks:");
         for PC of Per_Chunk_Info loop
            Put ("[" & PC.Old_Chunk_First'Image & " .." &
               PC.Old_Chunk_Last'Image & " ]");
         end loop;
         New_line;
      end if;

      --  Sort each chunk and gather statistics, in parallel
      declare
         use System.Parallelism;

         procedure Sort_One_Chunk
           (Low, High : Longest_Integer; Local_Chunk_Index : Positive) is
            --  NOTE: When doing things in parallel,
            --        Low = High = Local_Chunk_Index.
            --        When doing things sequentially,
            --        Low = Local_Chunk_Index = 1, High = Num_Chunks.
         begin
            for Chunk_Index in Integer (Low) .. Integer (High) loop
               declare
                  PC : Per_Chunk renames Per_Chunk_Info (Chunk_Index);
                  First : constant Index_Type := PC.Old_Chunk_First;
                  Last : constant Nat_Index_Type := PC.Old_Chunk_Last;
                  Num : constant Nat_Index_Type := Last - First + 1;
               begin

                  Quicksort (A (First .. Last));

                  --  Gather per-chunk statistics
                  PC.Min := A (First);
                  PC.Max := A (Last);

                  PC.Sum := 0.0;
                  PC.Sum_Sq := 0.0;
                  for I in First .. Last loop
                     PC.Sum := PC.Sum + A (I);
                     PC.Sum_Sq := PC.Sum_Sq + A (I) ** 2;
                  end loop;
               end;
            end loop;
         end Sort_One_Chunk;

      begin

         --  Do the parallel loop
         Par_Range_Loop (1, Longest_Integer (Num_Chunks), Num_Chunks,
           Loop_Body => Sort_One_Chunk'Access);
      end;

      --  Gather overall statistics
      declare
         Total_Sum : Elem_Type := 0.0;
         Total_SSq : Elem_Type := 0.0;
         Overall_Min : Elem_Type := Elem_Type'Last;
         Overall_Max : Elem_Type := Elem_Type'First;
         Total_Num : constant Nat_Index_Type := A'Length;
         Avg : Elem_Type;
         Variance : Elem_Type;
         Next_Pivot : Elem_Type;
         Increment : Elem_Type;

      begin

         for PC of Per_Chunk_Info loop
            Total_Sum := Total_Sum + PC.Sum;
            Total_SSq := Total_SSq + PC.Sum_Sq;
            Overall_Min := Elem_Type'Min (Overall_Min, PC.Min);
            Overall_Max := Elem_Type'Max (Overall_Max, PC.Max);
         end loop;

         if Debug then
            Put_Line ("Sum =" & Total_Sum'Image &
              ", Sum_Sq =" & Total_SSq'Image);
         end if;

         Avg := Total_Sum / Elem_Type (Total_Num);
         Variance := Total_SSq / Elem_Type (Total_Num) - (Avg) ** 2;

         if Debug then
            Put_Line ("Avg =" & Avg'Image &
              ", Variance =" & Variance'Image &
              ", Min =" & Overall_Min'Image &
              ", Max =" & Overall_Max'Image);
         end if;

         --  Choose pivots so that new chunks are of nearly equal size
         --  TBD: Current algorithm is the simplest possible; presumes
         --       flat distribution.
         Next_Pivot := Overall_Min;
         Increment := (Overall_Max - Overall_Min) / Elem_Type (Num_Chunks);
         for PC of Per_Chunk_Info loop
            PC.New_Chunk_Start_Value := Next_Pivot;
            Next_Pivot := Next_Pivot + Increment;
            PC.New_Chunk_End_Value := Next_Pivot;
         end loop;

         Per_Chunk_Info (Num_Chunks).New_Chunk_End_Value := Elem_Type'Last;
            --  Make sure we put the highest values into the last chunk.
      end;

      --  Compute amount going to new chunk from each old chunk
      declare
         use System.Parallelism;

         procedure Compute_Transfer_Matrix
           (Low, High : Longest_Integer; Local_Chunk_Index : Positive) is
            --  NOTE: When doing things in parallel,
            --        Low = High = Local_Chunk_Index.
            --        When doing things sequentially,
            --        Low = Local_Chunk_Index = 1, High = Num_Chunks.
         begin
            for Source_Chunk_Index in Integer (Low) .. Integer (High) loop
               declare
                  Source_PC : Per_Chunk renames
                    Per_Chunk_Info (Source_Chunk_Index);
                  Source_TM : Transfer_Vector renames
                    Transfer_Matrix (Source_Chunk_Index);

                  --  Look at each target chunk
                  Source_Index : Index_Type := Source_PC.Old_Chunk_First;
                  Source_Last : constant Nat_Index_Type :=
                    Source_PC.Old_Chunk_Last;

               begin

                  for Target_Index in 1 .. Num_Chunks loop
                     declare
                        Target_PC : Per_Chunk renames
                          Per_Chunk_Info (Target_Index);
                        New_Chunk_End_Value : constant Elem_Type :=
                          Target_PC.New_Chunk_End_Value;
                        Starting_Index : constant Index_Type := Source_Index;
                     begin
                        --  Figure out which range of values to
                        --  send to target chunk
                        Source_TM.Starting_Index (Target_Index) :=
                          Source_Index;
                        while Source_Index <= Source_Last
                          and then A (Source_Index) < New_Chunk_End_Value
                        loop
                           Source_Index := Source_Index + 1;
                        end loop;
                        Source_TM.Amount_To_Send (Target_Index) :=
                          Source_Index - Starting_Index;
                     end;
                  end loop;
               end;
            end loop;

         end Compute_Transfer_Matrix;

      begin
         --  Do the parallel loop
         Par_Range_Loop (1, Longest_Integer (Num_Chunks), Num_Chunks,
           Loop_Body => Compute_Transfer_Matrix'Access);
      end;

      --  Compute sizes of new chunks
      declare
         use System.Parallelism;

         procedure Compute_New_Chunks
           (Low, High : Longest_Integer; Local_Chunk_Index : Positive) is
            --  NOTE: When doing things in parallel,
            --        Low = High = Local_Chunk_Index.
            --        When doing things sequentially,
            --        Low = Local_Chunk_Index = 1, High = Num_Chunks.
         begin
            for Target_Index in Integer (Low) .. Integer (High) loop
               declare
                  Target_PC : Per_Chunk renames Per_Chunk_Info (Target_Index);
               begin
                  Target_PC.New_Chunk_Size := 0;
                  for Source_TM of Transfer_Matrix loop
                     Target_PC.New_Chunk_Size := Target_Pc.New_Chunk_Size +
                       Source_TM.Amount_To_Send (Target_Index);
                  end loop;
               end;
            end loop;
         end Compute_New_Chunks;
      begin
         Par_Range_Loop (1, Longest_Integer (Num_Chunks), Num_Chunks,
           Loop_Body => Compute_New_Chunks'Access);
      end;

      if Debug then
         declare
            Min_Chunk_Size : Nat_Index_Type := Nat_Index_Type'Last;
            Max_Chunk_Size : Nat_Index_Type := Nat_Index_Type'First;
         begin
            for PC of Per_Chunk_Info loop
               Min_Chunk_Size :=
                 Nat_Index_Type'Min (Min_Chunk_Size, PC.New_Chunk_Size);
               Max_Chunk_Size :=
                 Nat_Index_Type'Max (Max_Chunk_Size, PC.New_Chunk_Size);
            end loop;

            New_Line;
            Put_Line
              ("New chunk sizes:" &
                Min_Chunk_Size'Image & " .." &
                Max_Chunk_Size'Image);
         end;
      end if;

      --  Compute offsets of new chunks (sequentially)
      declare
         First_Index : Index_Type := A'First;
      begin
         for Target_PC of Per_Chunk_Info loop
            Target_PC.New_Chunk_First_Index := First_Index;
            First_Index := First_Index + Target_PC.New_Chunk_Size;
         end loop;
      end;

      if Debug then
         Put_Line ("New chunks:");
         for PC of Per_Chunk_Info loop
            Put_Line ("[" & PC.New_Chunk_First_Index'Image & " .." &
              Index_Type'Image (PC.New_Chunk_First_Index +
                PC.New_Chunk_Size - 1) & ", values:" &
                PC.New_Chunk_Start_Value'Image & " ..<" &
                PC.New_Chunk_End_Value'Image);
         end loop;
         New_line;
      end if;

      --  Copy data to final resting place and sort
      declare
         type Array_Type_CPtr is access constant Array_Type;

         Temp : constant Array_Type_CPtr :=
           new Array_Type'(A);  --  First copy whole array

         use System.Parallelism;

         procedure Init_And_Sort_New_Chunk
           (Low, High : Longest_Integer; Local_Chunk_Index : Positive) is
            --  NOTE: When doing things in parallel,
            --        Low = High = Local_Chunk_Index.
            --        When doing things sequentially,
            --        Low = Local_Chunk_Index = 1, High = Num_Chunks.
         begin
            for Target_Chunk_Index in Integer (Low) .. Integer (High) loop
               declare
                  Target_PC : Per_Chunk renames
                    Per_Chunk_Info (Target_Chunk_Index);
                  Target_Index : Index_Type := Target_PC.New_Chunk_First_Index;
               begin
                  --  Now copy data from source chunks into target chunk
                  --  Copy from source (now in Temp) into target (back into A)
                  for Source_Chunk_Index in 1 .. Num_Chunks loop
                     declare
                        Source_PC : Per_Chunk
                          renames Per_Chunk_Info (Source_Chunk_Index);
                        Source_TM : Transfer_Vector renames
                          Transfer_Matrix (Source_Chunk_Index);
                        Amount_To_Send : constant Nat_Index_Type :=
                          Source_TM.Amount_To_Send (Target_Chunk_Index);
                        Starting_Index : constant Index_Type :=
                          Source_TM.Starting_Index (Target_Chunk_Index);
                     begin

                        if Debug and then Amount_To_Send > 0 then
                           Put_Line ("Copying from" & Starting_Index'Image &
                             " .." &
                             Index_Type'Image
                               (Starting_Index + Amount_To_Send - 1) & " to" &
                             Target_Index'Image & " .." &
                             Index_Type'Image
                               (Target_Index + Amount_To_Send - 1) &
                             ", values:" & Temp (Starting_Index)'Image &
                             " .." &
                             Temp (Starting_Index + Amount_To_Send - 1)'Image);
                        end if;

                        A (Target_Index ..
                           Target_index + Amount_To_Send - 1) :=
                          Temp
                            (Starting_Index ..
                             Starting_Index + Amount_To_Send - 1);

                        Target_Index := Target_Index + Amount_To_Send;
                     end;
                  end loop;

                  --  Now sort the "new" chunk
                  Quicksort
                    (A (Target_PC.New_Chunk_First_Index ..
                          Target_PC.New_Chunk_First_Index +
                            Target_PC.New_Chunk_Size - 1));

               end;
            end loop;
         end Init_And_Sort_New_Chunk;
      begin
         Par_Range_Loop (1, Longest_Integer (Num_Chunks), Num_Chunks,
           Loop_Body => Init_And_Sort_New_Chunk'Access);
      end;
   end Hyper_Qsort;

   procedure Distrib_Qsort (V : in out Dis_Vec.Vector)
      --  Do a distributed hyper sort, presuming the vector
      --  has already been divided into shards, and is to be
      --  sorted in a distributed fashion, with parallel sort(s)
      --  being performed on each node.
   is
   begin
      null;  -- TBD
   end Distrib_Qsort;

end Hyper_Sorting;
