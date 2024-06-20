------------------------------------------------------------------------------
--            G e n e r i c _ S y n c h r o n i z e d _ D e q u e s         --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
--                                                                          --
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System;
with System.Atomic_Operations.Exchange;  --  TBD: System.Atomic_Operations...
package body Generic_Synchronized_Deques is
   --  Provide a double-ended queue that is
   --  synchronized to allow use in a work-stealing
   --  scheduler.

   --  This implementation is based on the paper by David Chase and Yassi Lev:

   --  David Chase and Yossi Lev. 2005.
   --  Dynamic circular work-stealing deque.
   --  In Proceedings of the seventeenth annual ACM symposium on
   --  Parallelism in algorithms and architectures (SPAA '05).
   --  ACM, New York, NY, USA, 21-28.
   --  DOI=http://dx.doi.org/10.1145/1073970.1073974

   --  PDF:
   --   https://www.dre.vanderbilt.edu/~schmidt/PDF/work-stealing-dequeue.pdf

   --  Deque data structure:

   --  type Element_Data (Capacity_Minus_One : Actual_Index_Type)
   --   is record
   --     --  TBD: Recent_Top : Index_Type := 0;  --  no need to be atomic
   --     Elements : Element_Array (0 .. Capacity_Minus_One);
   --  end record;

   --  type Deque is limited record
   --     Top    : aliased Atomic_Index_Type := 0;  --  next to steal
   --     Bottom : Atomic_Index_Type := 0;          --  next to fill
   --        --  TBD: Move into Data to minimize cache conflicts?
   --     Data   : not null Element_Data_Ptr with atomic :=
   --       new Element_Data (Capacity_Minus_One => Initial_Capacity - 1);
   --  end record;

   --  Example deque of Capacity 8 (cap-minus-one = 7), current Length 4:
   --  Top (modulo Capacity) points at slot from which to steal.
   --  Bottom (modulo Capacity) points at slot to push into, and one
   --  below slot to pop from.
   --
   --  0
   --  1
   --  2   <--  Top (mod Capacity)     --  stealing from here
   --  3
   --  4
   --  5
   --  6   <--  Bottom (mod Capacity)  --  pushing here
   --  7

   Debug : constant Boolean := False;

   procedure Free is new Ada.Unchecked_Deallocation
                               (Element_Data, Element_Data_Ptr);

   package Index_Exchange is
     new System.Atomic_Operations.Exchange (Atomic_Index_Type);

   procedure Push (Q : in out Deque; Element : Element_Type) is
   --  Add the Element to the "LIFO" end of the Deque.
      Prior_Bot : constant Index_Type := Q.Bottom;  --  TBD: or Q.Data.Bottom?
      Bot : Index_Type := Prior_Bot;
      Prior_Top : aliased Atomic_Index_Type := Q.Top;
                                                --  TBD: or Q.Data.Recent_Top?
      --  TBD: Could copy Q.Top into Q.Data.Recent_Top whenever
      --       Bot is a multiple of, say, 8.
      Length : constant Index_Type := Prior_Bot - Index_Type (Prior_Top);
      Data   : not null Element_Data_Ptr := Q.Data;
      Capacity : Actual_Index_Type := Data.Capacity_Minus_One + 1;
   begin
      if Actual_Index_Type (Length) + 1 >= Capacity then
         --  We need to expand
         declare
            Old_Cap  : constant Actual_Index_Type := Capacity;
            Old_Data : constant not null Element_Data_Ptr := Data;

            New_Cap  : constant Actual_Index_Type := Old_Cap * 2;
            New_Data : constant not null Element_Data_Ptr :=
              new Element_Data (New_Cap - 1);
         begin
            if Debug then
               Put_Line ("Expanding from capacity of" & Capacity'Image &
                 " to" & New_Cap'Image);
            end if;

            --  Copy relevant data, re-mod'ing the indices
            for I in Index_Type (Prior_Top) .. Prior_Bot - 1 loop
               New_Data.Elements (To_Actual_Index (I, Modulo => New_Cap)) :=
                 Old_Data.Elements (To_Actual_Index (I, Modulo => Old_Cap));
            end loop;
            --  TBD: New_Data.Recent_Top := Q.Top;
            Q.Data := New_Data;
            Capacity := New_Cap;

            --  Before we deallocate old Data array, we need to cause any
            --  ongoing "steal"s to fail.  We do this by temporarily
            --  "steal"ing all of the elements, and then pushing them
            --  back onto the queue in a different place (but with
            --  the same "actual" index into the (doubled) array so
            --  we don't actually have to move them again).
            --  This is done by adding (new) Capacity to Top, which steals
            --  "Capacity" items, and then adding (new) Capacity to Bottom,
            --  which ends up pushing them all back on the queue.

            --  Keep trying to bump Top by the new Capacity
            loop
               exit when Index_Exchange.Atomic_Compare_And_Exchange
                           (Item => Q.Top,
                            Prior => Prior_Top,
                            Desired => Atomic_Index_Type
                             (Index_Type (Prior_Top) + Index_Type (Capacity)));
            end loop;

            if Debug then
               Put_Line ("Have added" & Capacity'Image & " to Top of" &
                 Prior_Top'Image & " but not bumped Bot yet");
            end if;

            --  Now bump Bottom by the same amount
            Bot := Prior_Bot + Index_Type (Capacity);
            Q.Bottom := Bot;

            --  Now we can safely deallocate the old array
            --  because any ongoing "steal" would have failed
            --  due to our (big) change of Top.
            declare
               Temp : Element_Data_Ptr := Data;
            begin
               --  NOTE: Cannot do a Free directly on Data because
               --        it is "not null" and Free would set it to null.
               Data := New_Data;
               Free (Temp);
            end;
            if Debug then
               Put_Line ("Finished expanding from capacity of" &
                 Old_Cap'Image & " to" & New_Cap'Image);
            end if;
         end;
      end if;

      --  Store the new element at FIFO end of (potentially expanded) deque
      Data.Elements (To_Actual_Index (Bot, Modulo => Capacity)) :=
        Element;
      --  Bump the FIFO index
      Q.Bottom := Bot + 1;
   end Push;

   procedure Pop (Q : in out Deque; Element : out Element_Type) is
   --  Remove an Element from the "LIFO" end of the Deque.
   --  Element will come back as Empty if the Deque is empty.
      Data : constant Element_Data_Ptr := Q.Data;
      Capacity : constant Actual_Index_Type := Data.Capacity_Minus_One + 1;
      Prior_Bot : constant Index_Type := Q.Bottom;     --  TBD: or Data.Bottom?
   begin
      --  Decrement Q.Bottom now to prevent two "Steal"s causing trouble.
      --  NOTE: Temporarily it is possible that Q.Bottom = Top - 1.
      Q.Bottom := Prior_Bot - 1;

      declare
         --  Now get Top, having decremented Bottom.
         Prior_Top : aliased Atomic_Index_Type := Q.Top;
         Length : constant Index_Type := Prior_Bot - Index_Type (Prior_Top);
      begin
         if Length = 0 then
            --  Queue was already empty
            Element := Empty;
            --  Restore Bottom
            Q.Bottom := Prior_Bot;
         else
            --  Queue not empty when we started, so fetch what was
            --  the bottom, but check whether it has been stolen
            --  in the mean time.
            Element :=
              Data.Elements
                (To_Actual_Index (Prior_Bot - 1, Modulo => Capacity));
            if Length = 1 then
               --  We are competing with stealing,
               --  so we try to steal from ourself and then restore Bottom.
               if not Index_Exchange.Atomic_Compare_And_Exchange
                 (Item => Q.Top,
                  Prior => Prior_Top,
                  Desired => Prior_Top + 1)
               then
                  --  If swap fails, then we just treat it as empty
                  Element := Empty;
               end if;
               --  Restore Bottom in any case, since we bumped Top
               --  if the Swap succeeded.
               Q.Bottom := Prior_Bot;
            --  else Length > 1, Element is correct, and Bottom already decr'd
            end if;
         end if;
      end;
   end Pop;

   procedure Steal
     (Q : in out Deque;
      Element : out Element_Type;
      Steal_Failed : out Boolean) is
   --  Remove an Element from the "FIFO" end of the Deque.
   --  Element will come back as Empty if the Deque is empty.
   --  Steal_Failed will come back as True if the attempt to steal fails
   --  due to a concurrent Steal or Pop or doubling in size.

   --  Read "Top" first and then the Compare-And-Exchange will fail
   --  if any change in Top occurs since this read.
      Prior_Top : aliased Atomic_Index_Type := Q.Top;

      Bot    : constant Index_Type := Q.Bottom;    --  TBD: or Data.Bottom?
      Length : constant Index_Type := Bot - Index_Type (Prior_Top);
      Data   : constant Element_Data_Ptr := Q.Data;
      Capacity : constant Actual_Index_Type := Data.Capacity_Minus_One + 1;
      Cap_Times_Two : constant Index_Type := Index_Type (Capacity * 2);
   begin
      if Length + Cap_Times_Two <= Cap_Times_Two then
         --  We either have an empty queue, or we are in the
         --  middle of an expansion.
         Element := Empty;

         --  If empty, Length will be 0 or -1 (modulo Index_Type'Modulus).
         --  If in the middle of an expansion, Length will be
         --  in the range -Capacity*2 .. -Capacity (mod Index_Type'Modulus)
         --  and we want to handle it like a failed steal.
         Steal_Failed := (Length + 1) not in 0 .. 1;
      else
         --  There is at least one element in the deque.
         --  Try to steal it.
         Element :=
           Data.Elements
             (To_Actual_Index (Index_Type (Prior_Top), Modulo => Capacity));

         --  Add one to Q.Top, if Q.Top still equals Prior_Top
         Steal_Failed := not Index_Exchange.Atomic_Compare_And_Exchange
           (Item => Q.Top, Prior => Prior_Top, Desired => Prior_Top + 1);
         if Steal_Failed then
            --  Return Empty if Steal_Failed
            Element := Empty;
         end if;
      end if;
   end Steal;

   function Is_Empty (Q : Deque) return Boolean is
   --  Returns true if the Deque is empty at the moment of the call of the
   --  function.  This can become False at any time if there is a thread still
   --  executing that might Push on the deque.
   begin
      return Q.Bottom - Index_Type (Q.Top) + 1 <= 1;
      --  NOTE: It is possible for Bottom = Top-1 during a Pop
   end Is_Empty;

   function Length (Q : Deque) return Natural is
   --  Returns count of number of items in Deque, at time of the call.
   --  Can change immediately after the call.
   begin
      return Natural'Max (0, Integer (Q.Bottom - Index_Type (Q.Top) + 1) - 1);
      --  Deal with case when Bottom = Top - 1, which can happen during a Pop.
      --  Need to do initial computation using unsigned, then can convert
      --  to signed and do final decrement.
   end Length;

   procedure Reset (Q : in out Deque) is
   --  Reset Deque to minimal capacity, reclaiming any additional storage
   --  devoted to deque from growth due to past calls on Push.
   --  This should not be called while there are still threads running
   --  that might do a Push.
   begin
      if Q.Data.Capacity_Minus_One > Initial_Capacity - 1 then
         --  Data has been expanded, so restore it to its Initial_Capacity
         --  and free old Data.
         declare
            Old_Data : Element_Data_Ptr := Q.Data;
         begin
            Q.Data := new Element_Data (Initial_Capacity - 1);
            Free (Old_Data);
            --  NOTE: Cannot do a Free directly on Q.Data because Data
            --        is a "not null" field and Free would set it to null.
         end;
      end if;
   end Reset;

   function All_Elements (Q : Deque) return Element_Vector is
   --  Return a vector of all of the elements, in order from
   --  oldest to newest.
   --  This should not be called while items are being added or removed!
      Elements : constant Element_Array := Q.Data.Elements;
      Bot : constant Index_Type := Q.Bottom;
      Top : constant Index_Type := Index_Type (Q.Top);
   begin
      if Bot - Top + 1 <= 1 then
         --  Empty
         return Element_Vector'(1 .. 0 => <>);
      else
         --  Copy the elements into a Result vector
         declare
            Result : Element_Vector (1 .. Natural (Bot - Top));
            Index : Index_Type := Top;
         begin
            for E of Result loop
               E := Elements
                      (To_Actual_Index (Index, Modulo => Elements'Length));
               Index := Index + 1;
            end loop;
            return Result;
         end;
      end if;
   end All_Elements;

end Generic_Synchronized_Deques;
