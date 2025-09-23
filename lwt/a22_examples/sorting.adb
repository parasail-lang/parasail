-- Example Ada 2022 program -- QuickSort

with Ada.Text_IO; use Ada.Text_IO;
package body Sorting is
   procedure Quicksort(Arr : in out Array_Type; Debug : Boolean := False) is
      -- Handle short slices directly.  Partition longer slices.
      procedure Sort_Slice (Slc_First, Slc_Last : Index_Type) is
         Slc_Length : constant Index_Type := Slc_Last - Slc_First + 1;
      begin

         case Slc_Length is
         when 0 .. 1 =>
            null;

         when 2 =>
            if Arr(Slc_Last) < Arr(Slc_First) then
               -- Swap elements
               declare
                  Tmp : constant Elem_Type := Arr(Slc_First);
               begin
                  Arr(Slc_First) := Arr(Slc_Last);
                  Arr(Slc_Last) := Tmp;
               end;
            end if;

         when others =>
            -- Partition array
            declare
               Mid : constant Elem_Type := Arr(Slc_First + Slc_Length/2);
               Left : Index_Type := Slc_First;
               Right : Index_Type := Slc_Last;
            begin
               while Left <= Right loop
                  declare
                     New_Left : Index_Type'Base := Right+1;
                     New_Right : Index_Type'Base := Left-1;
                  begin
                     parallel do
                        -- Find item in left half to swap
                        for I in Left .. Right loop
                           if not (Arr(I) < Mid) then
                              -- Found an item that can go
                              -- into right partitition
                              New_Left := I;
                              exit when Mid < Arr(I);
                                 -- Found an item that *must* go
                                 -- into right part
                           end if;
                        end loop;
                     and
                        -- Find item in right half to swap
                        for J in reverse Left .. Right loop
                           if not (Mid < Arr(J)) then
                              -- Found an item that can go into
                              -- left partitition
                              New_Right := J;
                              exit when Arr(J) < Mid;
                                 -- Found an item that *must* go
                                 -- into left part
                           end if;
                        end loop;

                     end do;

                     if New_Left > New_Right then
                        -- Nothing more to swap
                        -- Exit loop and recurse on two partitions
                        Left := New_Left;
                        Right := New_Right;
                        exit;
                     end if;
                    
                     -- Swap items
                     declare
                        Tmp : constant Elem_Type := Arr(New_Left);
                     begin
                        Arr(New_Left) := Arr(New_Right);
                        Arr(New_Right) := Tmp;
                     end;
           
                     -- continue looking for items to swap
                     Left := New_Left + 1;
                     Right := New_Right - 1;
                  end;
               end loop;
               
               -- At this point, "Right" is right end of left partition
               -- and "Left" is left end of right partition
               -- and the partitions don't overlap
               -- and neither is the whole array
               -- and everything in the left partition can precede Mid
               -- and everything in the right partition can follow Mid
               -- and everything between the partitions is equal to Mid.
               pragma Assert
                 (Left > Right and then
                  Right < Slc_Last and then
                  Left > Slc_First);

               pragma Assert
                 ((for all I in Slc_First .. Right => not (Mid < Arr(I)))
                    and then
                  (for all J in Left .. Slc_Last => not (Arr(J) < Mid))
                    and then
                  (for all K in Right+1 .. Left-1 => 
                     not (Mid < Arr(K)) and not (Arr(K) < Mid)));
               
               if Debug then
                  Put_Line ("Divide and Conquer: Sort Arr(" &
                    Image(Left) & " .. " & Image(Slc_Last) & ") & Arr(" &
                    Image(Slc_First) & " .. " & Image(Right) & ")");
               end if;

               -- continue with two halves in parallel
               parallel do
                  Sort_Slice (Slc_First, Right);
               and
                  Sort_Slice (Left, Slc_Last);
               end do;
             end;
          end case;
       end Sort_Slice;
    begin

       Sort_Slice (Arr'First, Arr'Last);
    end Quicksort;
end Sorting;


