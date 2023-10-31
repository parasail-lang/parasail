------------------------------------------------------------------------------
--                              N Queens using OpenMP                       --
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

pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with LWT.Generic_Parallel_Work_Lists;
with LWT.OpenMP; use LWT.OpenMP;
procedure N_Queens_OMP is
   --  Usage: n_queens_omp [num-queens [num-threads]]

   Num_Args : constant Natural := Argument_Count;

   Default_Num_Threads : constant := 6;

   Num_Threads_To_Use : constant Natural :=
     (if Num_Args < 2 then Default_Num_Threads
      else Natural'Value (Argument (2)));

   Control : OMP_Parallel (Num_Threads => Num_Threads_To_Use);
   pragma Unreferenced (Control);

   Debug : constant Boolean := False;

   Max_Rows : constant := 20;

   type Chess_Unit is range 1 - Max_Rows .. Max_Rows * 2;
   --  Chess_Unit corresponds to number of rows or columns on a chess board,
   --  as well as sums and differences of row# and col#.

   Default_N : constant := 8;

   --  Get count of queens from command line
   N : constant Chess_Unit range 1 .. Max_Rows :=
     (if Num_Args = 0 then Default_N else Chess_Unit'Value (Argument (1)));

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
      Placed_Queens : Queen_Config := (others => No_Queen);
      Sum_Diags_Used : Sum_Config := (others => False);
      Diff_Diags_Used : Diff_Config := (others => False);
   end record;

   package Inst is new LWT.Generic_Parallel_Work_Lists (Board_Config);

   function Can_Add (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Boolean;
   function Can_Add (Partial_Solution : Board_Config; Row : Row_Col_Num)
     return Boolean is
      --  Return True if can add a queen at given Row, and column
      --  Partial_Solution.Num_Cols_Filled + 1, without any
      --  conflicts with existing queens.
      Col : constant Row_Col_Num := Partial_Solution.Num_Cols_Filled + 1;
   begin
      return Partial_Solution.Placed_Queens (Row) = No_Queen
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
         Result.Placed_Queens (Row) := Col;
         Result.Sum_Diags_Used (Row + Col) := True;
         Result.Diff_Diags_Used (Row - Col) := True;
      end return;
   end Add_Queen;

   type Sol_Array is array (Positive range <>) of Queen_Config;
   Max_Solutions : constant := 100;  --  only remember first 100

   protected Prot is
      --  Protected object to accumulate solutions
      procedure Remember_Solution (Solution : Queen_Config);
      function Num_Solutions return Natural;
      function Num_Remembered return Natural;
      function Nth_Solution (Index : Positive) return Queen_Config;
   private
      Num_Sols : Natural := 0;
      Num_Saved : Natural := 0;
      Solutions : Sol_Array (1 .. Max_Solutions);
   end Prot;

   protected body Prot is
      procedure Remember_Solution (Solution : Queen_Config) is
      begin
         Num_Sols := Num_Sols + 1;
         if Num_Sols <= Solutions'Last then
            Num_Saved := Num_Saved + 1;
            Solutions (Num_Saved) := Solution;
         end if;
      end Remember_Solution;

      function Num_Remembered return Natural is
      begin
         return Num_Saved;
      end Num_Remembered;

      function Num_Solutions return Natural is
      begin
         return Num_Sols;
      end Num_Solutions;

      function Nth_Solution (Index : Positive) return Queen_Config is
      begin
         return Solutions (Index);
      end Nth_Solution;
   end Prot;

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
      --  to the global "Prot" list of solutions.
      pragma Unreferenced (Max_Chunks);

   begin  --  Outer_Loop_Body

      if Debug then
         Put_Line (" Have filled " & Partial_Solution.Num_Cols_Filled'Image &
           " columns.");
      end if;

      for I in 1 .. N loop
         if Can_Add (Partial_Solution, I) then
            declare
               Next_Config : constant Board_Config :=
                 Add_Queen (Partial_Solution, I);
            begin
               if Next_Config.Num_Cols_Filled = N then
                  --  We are all done
                  Prot.Remember_Solution (Next_Config.Placed_Queens);
               else
                  --  Add work item to try to fill further columns
                  Add_Work_Item (Next_Config);
               end if;
            end;
         end if;
      end loop;

   end Outer_Loop_Body;

begin  --  N_Queens_OMP

   Inst.Par_Iterate_Work_List
     (Initial_Item => (others => <>),
      Num_Chunks => Positive'Last,  --  No limit on parallelism
      Loop_Body => Outer_Loop_Body'Access);

   if Prot.Num_Solutions > Prot.Num_Remembered then
      Put (" First" & Natural'Image (Prot.Num_Remembered) & " out of");
   end if;
   Put_Line (Natural'Image (Prot.Num_Solutions) & " solutions:");

   for I in 1 .. Prot.Num_Remembered loop
      declare
         Sol : constant Queen_Config := Prot.Nth_Solution (I);
      begin
         Put ("[");
         for I in 1 .. N loop
            Put (Row_Col_Num'Image (Sol (I)));
            if I < N then
               Put (",");
            end if;
         end loop;
         Put_Line (" ]");
      end;
   end loop;
   Put ("Done printing");
   if Prot.Num_Solutions > Prot.Num_Remembered then
      Put (" first" & Natural'Image (Prot.Num_Remembered) & " out of");
   end if;
   Put_Line (Natural'Image (Prot.Num_Solutions) & " solutions.");

end N_Queens_OMP;
