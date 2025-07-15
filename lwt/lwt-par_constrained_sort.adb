------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                           LWT.PAR_CONSTRAINED_SORT                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- The sequential version of this unit was developed by Matthew J Heaney.   --
-- This parallel version was developed by Tucker Taft                       --
------------------------------------------------------------------------------

--  This algorithm is a parallel variant of quicksort

with LWT.Generic_Parallel_Work_Lists;

with Ada.Text_IO; use Ada.Text_IO;

procedure LWT.Par_Constrained_Sort
  (Container : in out Array_Type)
is
   --  Instantiate parallel "work list" generic
   --  and use it for parallelizing the quicksort algorithm.

   pragma Warnings (Off);

   Debug : constant Boolean := False;
   Less_Debug : constant Boolean := Debug or else False;
   Checks : constant Boolean := Debug or else False;

   Arr : Array_Type renames Container;

   type Work_Item is record
      First : Index_Type;
      Last : Index_Type'Base;
   end record;

   package Inst is new LWT.Generic_Parallel_Work_Lists (Work_Item);
      --  Provides parallel iterator with Add_Work_Item operation

   procedure Swap (X, Y : in out Element_Type) with Inline is
      --  Swap X and Y
      Tmp : constant Element_Type := X;
   begin
      X := Y;
      Y := Tmp;
   end Swap;

   procedure Sort_Slice
     (Bounds : Work_Item; Max_Chunks : Positive;
      Add_Work_Item : not null access procedure (New_Item : Work_Item))
   --  with Parallel_Calls  --  Indicate might be called in parallel
   is
      --  Sort given slice of Arr and spawn sorts of bigger half
      First : Index_Type := Bounds.First;
      Last : Index_Type := Bounds.Last;
   begin
      if Debug then
         Put_Line ("Sort_Slice (" & First'Image & "," &
           Last'Image & ")");
      end if;

      while Last > First loop
         --  Loop while having a non-trivial partition to sort
         if Last = Index_Type'Succ (First) then
            --  Two-component array
            if Arr (Last) < Arr (First) then
               --  Need to swap elements
               Swap (Arr (First), Arr (Last));
            end if;
            exit;  --  All done
         else
            --  Three or more components; partition the array
            --  TBD: Should handle short arrays specially.
            if Less_Debug and then not Debug then
               Put_Line ("Sort_Slice (" & First'Image & "," &
                 Last'Image & ")");
            end if;

            declare
               Mid_Index : constant Index_Type :=
                 Index_Type'Val ((Index_Type'Pos (First) +
                                  Index_Type'Pos (Last)) / 2);
               Mid_Val : constant Element_Type := Arr (Mid_Index);
               Right_First : Index_Type := First;
               Left_Last : Index_Type := Last;
            begin
               if Debug then
                  Put_Line (" Chose pivot at index " & Mid_Index'Image);
               end if;
               while Right_First <= Left_Last loop
                  declare
                     Swap_Left : Index_Type :=
                       Index_Type'Succ
                         (Index_Type'Min
                            (Left_Last, Index_Type'Pred (Last)));
                     Swap_Right : Index_Type :=
                       Index_Type'Pred
                         (Index_Type'Max
                            (Right_First, Index_Type'Succ (First)));
                  begin
                     --  parallel do
                     begin
                        --  Find item in left half to swap
                        for I in Right_First .. Left_Last loop
                           if not (Arr (I) < Mid_Val) then
                              --  Found an item OK for right partitition
                              Swap_Left := I;
                              exit when Mid_Val < Arr (I);
                                 --  Found an item ONLY for right ptn
                           end if;
                        end loop;
                     end;
                     --  and
                     begin
                        --  Find item in right half to swap
                        for J in reverse Right_First .. Left_Last loop
                           if not (Mid_Val < Arr (J)) then
                              --  Found an item OK for left partitition
                              Swap_Right := J;
                              exit when Arr (J) < Mid_Val;
                                 --  Found an item that *must* go into left ptn
                           end if;
                        end loop;
                     end;
                     --  end do;

                     if Swap_Left > Swap_Right then
                        --  Nothing more to swap
                        --  Exit loop and start sorts on two partitions
                        Right_First := Swap_Left;
                        Left_Last := Swap_Right;
                        exit;
                     end if;

                     if Debug then
                        Put_Line (" Found items to swap at " &
                          Swap_Left'Image & " and " & Swap_Right'Image);
                     end if;

                     --  Swap items
                     Swap (Arr (Swap_Left), Arr (Swap_Right));

                     --  continue looking for items to swap
                     Right_First := Index_Type'Succ (Swap_Left);
                     Left_Last := Index_Type'Pred (Swap_Right);
                  end;
               end loop;

               --  At this point, "Left_Last" is high bound of left partition
               --  and "Right_First" is low bound of right partition
               --  and the partitions don't overlap
               --  and neither is the whole array
               --  and everything in the left partition can precede Mid_Val
               --  and everything in the right partition can follow Mid_Val
               --  and everything between the partitions is equal to Mid_Val.

               if Debug then
                  Put_Line (" Left partition at " &
                    First'Image & " .. " & Left_Last'Image);
                  Put_Line (" Right partition at " &
                    Right_First'Image & " .. " & Last'Image);
               end if;
               --  TBD: pragma Assert
               if Checks and then not
                 (Right_First > Left_Last
                  and then Left_Last < Last
                  and then Right_First > First)
               then
                  Put_Line ("** assertion failed for " &
                    Right_First'Image & " > " & Left_Last'Image);
               end if;

               --  Left partition element all less than Mid_Val
               --  TBD: pragma Assert
               if Checks and then not
                 ((for all I in First .. Left_Last =>
                      not (Mid_Val < Arr (I))))
               then
                  Put_Line ("** assertion failed for Left ptn");
               end if;

               --  Right partition elements all greater than Mid_Val
               --  TBD: pragma Assert
               if Checks and then not
                 ((for all J in Right_First .. Last =>
                      not (Arr (J) < Mid_Val)))
               then
                  Put_Line ("** assertion failed for Right ptn");
               end if;

               --  Elements between partitions all equal to Mid_Val
               --  TBD: pragma Assert
               if Debug then
                  for K in
                     Index_Type'Succ (Left_Last) ..
                       Index_Type'Pred (Right_First)
                  loop
                     if (Mid_Val < Arr (K)) or else (Arr (K) < Mid_Val) then
                        Put_Line ("*** Arr (" & K'Image & ") /= Mid_Val");
                        exit;
                     end if;
                  end loop;
               end if;
               if Checks and then not
                 ((for all K in
                     Index_Type'Succ (Left_Last) ..
                       Index_Type'Pred (Right_First) =>
                         not (Mid_Val < Arr (K))
                           and then
                         not (Arr (K) < Mid_Val)))
               then
                  Put_Line ("** assertion failed for middle values");
               end if;

               --  Now sort the two halves in parallel
               if First < Left_Last then
                  --  Left ptn is non-trivial
                  if Right_First >= Last then
                     --  Right partition is trivial, loop around with
                     --  left ptn.
                     Last := Left_Last;

                  --  Both partitions are non-trivial, spawn a thread
                  --  for the bigger one.
                  elsif Index_Type'Pos (Left_Last) - Index_Type'Pos (First) >
                    Index_Type'Pos (Last) - Index_Type'Pos (Right_First)
                  then
                     --  Left ptn is bigger, spawn thread to sort it
                     if Debug then
                        Put_Line (" Spawning thread to sort slice at " &
                          First'Image & " .. " & Left_Last'Image);
                     end if;
                     Add_Work_Item ((First, Left_Last));

                     --  Loop around to process right partition
                     First := Right_First;
                  else
                     --  Right ptn is bigger, spawn thread to sort it
                     if Debug then
                        Put_Line (" Spawning thread to sort slice at " &
                          Right_First'Image & " .. " & Last'Image);
                     end if;
                     Add_Work_Item ((Right_First, Last));

                     --  Loop around to process left partition
                     Last := Left_Last;
                  end if;
               else
                  --  Left partition is trivial, loop around with right ptn
                  First := Right_First;
               end if;
            end;
         end if;
      end loop;
   end Sort_Slice;

begin  --  LWT.Par_Constrained_Sort

   --  Start the sort, giving the full bounds of the array and
   --  no limit on parallelism.
   Inst.Par_Iterate_Work_List
     (Initial_Item => (Arr'First, Arr'Last),
      Num_Chunks => Positive'Last,
      Loop_Body => Sort_Slice'Access);

   pragma Warnings (On);
end LWT.Par_Constrained_Sort;
