------------------------------------------------------------------------------
--                              N Queens using ZeroMQ                       --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Generic_Parallel_Work_Lists;
with Interfaces_Work_Stealing; use Interfaces_Work_Stealing;
with System_Distrib; use System_Distrib;
with System.Atomic_Operations.Integer_Arithmetic;
with Distributed_Counts;
with Distributed_Vectors;
procedure N_Queens_Distrib_WS is
   --  Usage: n_queens_distrib [num-queens [num-threads [num-nodes]]]
   --  NOTE: This is based on n_queens_omp.adb
   --        Link with: gcc -lzyre -lczmq -lzmq

   Num_Args : constant Natural := Argument_Count;

   Default_Num_Threads : constant := 6;

   Num_Threads_To_Use : constant Natural :=
     (if Num_Args < 2 then Default_Num_Threads
      else Natural'Value (Argument (2)));

   Control : WS_Parallel (Num_Servers => Num_Threads_To_Use, Options => null);
   pragma Unreferenced (Control);

   Debug : constant Boolean := False;

   Max_Rows : constant := 200;

   type Chess_Unit is range 1 - Max_Rows .. Max_Rows * 2;
   --  Chess_Unit corresponds to number of rows or columns on a chess board,
   --  as well as sums and differences of row# and col#.

   Default_N : constant := 8;

   --  Get count of queens from command line
   N : constant Chess_Unit range 1 .. Max_Rows :=
     (if Num_Args = 0 then Default_N else Chess_Unit'Value (Argument (1)));

   --  This row range will be changed once we find out how many
   --  nodes are in the group.
   My_First : Chess_Unit range 1 .. N := 1;
   My_Last : Chess_Unit'Base range 0 .. N := N;

   subtype Row_Col_Count is Chess_Unit range 0 .. N;
   No_Queen : constant Row_Col_Count := 0;

   subtype Row_Col_Num is Row_Col_Count range 1 .. N;

   subtype Row_Col_Sum is Chess_Unit range 2 .. N * 2;
   --  Used to see whether "row + col" diagonals already occupied

   subtype Row_Col_Diff is Chess_Unit range 1 - N .. N - 1;
   --  Used to see whether "row - col" diagonals already occupied

   type Queen_Config is array (Row_Col_Num) of Row_Col_Count;

   type Sum_Config is array (Row_Col_Sum) of Boolean;
   type Diff_Config is array (Row_Col_Diff) of Boolean;

   type Board_Config is record
      Num_Cols_Filled : Row_Col_Count := 0;
      Rows_In_Use : Queen_Config := (others => No_Queen);
      Cols_In_Use : Queen_Config := (others => No_Queen);
      Sum_Diags_Used : Sum_Config := (others => False);
      Diff_Diags_Used : Diff_Config := (others => False);
   end record;

   package Inst is new Generic_Parallel_Work_Lists (Board_Config);

   function Can_Add (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Boolean;
   function Can_Add (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Boolean is
      --  Return True if can add a queen at given Row, and column
      --  Partial_Solution.Num_Cols_Filled + 1, without any
      --  conflicts with existing queens.
      Col : constant Row_Col_Num := Partial_Solution.Num_Cols_Filled + 1;
   begin
      return Partial_Solution.Rows_In_Use (Row) = No_Queen
        and then not Partial_Solution.Sum_Diags_Used (Row + Col)
        and then not Partial_Solution.Diff_Diags_Used (Row - Col);
   end Can_Add;

   function Add_Queen (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Board_Config;
   function Add_Queen (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Board_Config is
      --  Return a board with Num_Cols_Filled incremented by one,
      --  and a queen placed on given Row and column the new Num_Cols_Filled.
      Col : constant Row_Col_Num := Partial_Solution.Num_Cols_Filled + 1;
   begin
      return Result : Board_Config := Partial_Solution do
         Result.Num_Cols_Filled := Col;
         Result.Rows_In_Use (Row) := Col;
         Result.Cols_In_Use (Col) := Row;
         Result.Sum_Diags_Used (Row + Col) := True;
         Result.Diff_Diags_Used (Row - Col) := True;
      end return;
   end Add_Queen;

   Max_Solutions : constant := 100;  --  only remember first 100
   Local_Max_Solutions : Natural := Max_Solutions;  --  Will be updated

   Default_Num_Nodes : constant := 4;

   Min_Num_Nodes : constant Natural :=
     (if Num_Args < 3 then Default_Num_Nodes
      else Natural'Value (Argument (3)));

   --  Join a group; find out local portion of problem space determined by
   --  effective thread count.
   Group : aliased Group_Context := Join_Group
     ("com.adacore.taft.n_queens" & N'Image, Num_Threads_To_Use,
      Min_Num_Nodes => Min_Num_Nodes);

   package Accum is
      --  package to accumulate solutions
      procedure Remember_Solution (Solution : Queen_Config);
      function Total_Num_Solutions return Natural;
      function Total_Num_Remembered return Natural;
      function Overall_Nth_Solution (Index : Positive) return Queen_Config;

      package Queen_Config_Vecs is
        new Distributed_Vectors (Positive, Queen_Config,
                                 Distrib_Type_Id => "Queen_Config_Vecs");

      Solutions : Queen_Config_Vecs.Vector
                    (Group'Unchecked_Access, Distrib_Obj_Id => 1);
   end Accum;

   package body Accum is

      type Atomic_Integer is new Integer with Atomic;
      subtype Atomic_Natural is Atomic_Integer range 0 .. Atomic_Integer'Last;

      package Atomic_Arith is
        new System.Atomic_Operations.Integer_Arithmetic (Atomic_Natural);

      package Queen_Config_Counts is
        new Distributed_Counts (Integer, "Queen_Config_Counts");

      Num_Sols : Queen_Config_Counts.Count
                   (Group'Unchecked_Access, Distrib_Obj_Id => 1);
      Num_Saved : aliased Atomic_Natural := 0;

      procedure Remember_Solution (Solution : Queen_Config) is
      begin
         --  Compute a total number of solutions.
         Num_Sols.Add (1);

         if Natural (Atomic_Arith.Atomic_Fetch_And_Add (Num_Saved, 1)) >=
              Local_Max_Solutions
         then
            --  Already saved enough locally; don't save another one
            Atomic_Arith.Atomic_Subtract (Num_Saved, 1);
         else
            --  Save this one.
            Solutions.Append (Solution);
         end if;
      end Remember_Solution;

      function Total_Num_Remembered return Natural is
      begin
         return Natural (Solutions.Length);
      end Total_Num_Remembered;

      function Total_Num_Solutions return Natural is
      begin
         return Num_Sols.Value;
      end Total_Num_Solutions;

      function Overall_Nth_Solution (Index : Positive) return Queen_Config is
      begin
         return Solutions (Index);
      end Overall_Nth_Solution;
   end Accum;

   procedure Outer_Loop_Body
    (Partial_Solution : Board_Config;
     Max_Chunks : Positive;
     Add_Work_Item : not null access procedure (New_Config : Board_Config));

   procedure Outer_Loop_Body
    (Partial_Solution : Board_Config;
     Max_Chunks : Positive;
     Add_Work_Item : not null access procedure (New_Config : Board_Config))
   --  with Parallel_Calls  --  indicate we expect parallel calls
   is
      --  Loop body to try all possible extensions
      --  of given Partial_Solution, and add any solutions found
      --  to the global "Accum" list of solutions.
      pragma Unreferenced (Max_Chunks);

      First : constant Chess_Unit :=
        (if Partial_Solution.Num_Cols_Filled = 0 then My_First else 1);
      Last : constant Chess_Unit :=
        (if Partial_Solution.Num_Cols_Filled = 0 then My_Last else N);

   begin  --  Outer_Loop_Body

      if Debug then
         Put_Line (" Have filled " & Partial_Solution.Num_Cols_Filled'Image &
           " columns.");
      end if;

      for I in First .. Last loop
         if Can_Add (Partial_Solution, I) then
            declare
               Next_Config : constant Board_Config :=
                 Add_Queen (Partial_Solution, I);
            begin
               if Next_Config.Num_Cols_Filled = N then
                  --  We are all done
                  Accum.Remember_Solution (Next_Config.Cols_In_Use);
               else
                  --  Add work item to try to fill further columns
                  Add_Work_Item (Next_Config);
               end if;
            end;
         end if;
      end loop;

   end Outer_Loop_Body;

begin  --  N_Queens_Distrib

   Put_Line ("Portion" & Group.Node_Index'Image &
     ": offset" & Group.Node_Offset'Image & ", size" &
     Group.Node_Size'Image & ", out of" & Group.Total_Size'Image);

   --  Compute first/last using portion [offset,size] as a proportion of total
   My_First := Chess_Unit
     (Group.Node_Offset * Natural (N) / Group.Total_Size + 1);
   My_Last := Chess_Unit
     ((Group.Node_Offset + Group.Node_Size) * Natural (N) /
         Group.Total_Size);

   Put_Line (Group.Node_Index'Image & ":  Queens, N =" & N'Image &
     ", first row =" & My_First'Image &
     ", last row =" & My_Last'Image);

   --  Verify we have something to do
   if My_Last < My_First then
      Put_Line ("*** Nothing to do ***");
   end if;

   --  Compute what proportion of solutions we should save.
   Local_Max_Solutions :=
     (Group.Node_Offset + Group.Node_Size) * Max_Solutions /
        Group.Total_Size -
          Group.Node_Offset * Max_Solutions / Group.Total_Size;

   -------------------------------------------
   --  Now actually compute some solutions  --
   -------------------------------------------

   Inst.Par_Iterate_Work_List
     (Initial_Item => (others => <>),
      Num_Chunks => Positive'Last,  --  No limit on parallelism
      Loop_Body => Outer_Loop_Body'Access);

   ---------------------------------------------
   --  Now report on (some of) the solutions  --
   ---------------------------------------------

   if False and then Group.Node_Offset /= 0 then
      --  TBD: Give a partial report
      declare
         Total_Solutions : constant Natural := Accum.Total_Num_Solutions;
      begin
         Put_Line (Group.Node_Index'Image & ": remembered" &
           Accum.Total_Num_Remembered'Image & " out of" &
           Total_Solutions'Image & " total solutions");
      end;
   else
      --  Give a full report
      declare
         Total_Solutions : constant Natural := Accum.Total_Num_Solutions;

         procedure Process (Sol : Queen_Config) is
         begin
            Put (Group.Node_Index'Image & ":[");
            for J in 1 .. N loop
               Put (Row_Col_Num'Image (Sol (J)) &
                     (if J < N then "," else ""));
            end loop;
            Put_Line (" ]");
         end Process;

      begin
         if Group.Node_Index = 1 then
            Put_Line (Group.Node_Index'Image & ":" &
               " First" & Natural'Image (Accum.Total_Num_Remembered) &
               " out of" &
               Natural'Image (Total_Solutions) & " solutions:");
         end if;

         Accum.Solutions.Iterate (Process => Process'Access);

         if Group.Node_Index = Group.Num_Nodes then
            Put_Line (Group.Node_Index'Image & ": Done printing" &
              " first" & Natural'Image (Accum.Total_Num_Remembered) &
              " out of" &
              Natural'Image (Total_Solutions) & " solutions.");
         end if;
      end;
   end if;

   Finish (Group);

end N_Queens_Distrib_WS;
