------------------------------------------------------------------------------
--            G e n e r i c _ S y n c h r o n i z e d _ D e q u e s         --
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
--                                                                          --
-- This is derived from the light-weight scheduler in the ParaSail language --
-- which was originally developed by S. Tucker Taft.                        --
------------------------------------------------------------------------------

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with System;
with System.Atomic_Operations.Exchange;
package body LWT.Generic_Synchronized_Deques is
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

   --  type Element_Data (Modulo_Minus_One : Actual_Index_Type)
   --   is record
   --     --  TBD: Recent_Top : Index_Type := 0;  --  no need to be atomic
   --     Elements : Element_Array (0 .. Modulo_Minus_One);
   --  end record;

   --  type Deque is limited record
   --     Top    : aliased Atomic_Index_Type := 0;  --  next to steal
   --     Bottom : Atomic_Index_Type := 0;          --  next to fill
   --        --  TBD: Move into Data to minimize cache conflicts?
   --     Data   : not null Element_Data_Ptr with atomic :=
   --       new Element_Data (Modulo_Minus_One => Initial_Size - 1);
   --  end record;

   procedure Free is new Ada.Unchecked_Deallocation
                               (Element_Data, Element_Data_Ptr);

   package Index_Exchange is
     new System.Atomic_Operations.Exchange (Atomic_Index_Type);

   procedure Push (Q : in out Deque; Element : Element_Type) is
   --  Add the Element to the "LIFO" end of the Deque.
      Bot : constant Index_Type := Q.Bottom;    --  TBD: or Q.Data.Bottom?
      Top : constant Index_Type := Index_Type (Q.Top);
                                                --  TBD: or Q.Data.Recent_Top?
      --  TBD: Could copy Q.Top into Q.Data.Recent_Top whenever
      --       Bot is a multiple of, say, 8.
      Length : constant Index_Type := Bot - Top;
      Data   : not null Element_Data_Ptr := Q.Data;
      Mod_Minus_One : Actual_Index_Type := Data.Modulo_Minus_One;
   begin
      if Actual_Index_Type (Length) = Mod_Minus_One then
         --  We need to expand
         declare
            Old_Mod  : constant Actual_Index_Type := Mod_Minus_One + 1;
            Old_Data : constant not null Element_Data_Ptr := Data;

            New_Mod  : constant Actual_Index_Type := Old_Mod * 2;
            New_Data : constant not null Element_Data_Ptr :=
              new Element_Data (New_Mod - 1);
         begin
            --  Copy relevant data, re-mod'ing the indices
            for I in Top .. Bot - 1 loop
               New_Data.Elements (To_Actual_Index (I, New_Mod)) :=
                 Old_Data.Elements (To_Actual_Index (I, Old_Mod));
            end loop;
            --  TBD: New_Data.Recent_Top := Q.Top;
            Q.Data := New_Data;
            Mod_Minus_One := New_Mod - 1;

            --  Deallocate old data, and re-assign
            declare
               Temp : Element_Data_Ptr := Data;
            begin
               --  NOTE: Cannot do a Free directly on Data because
               --        it is "not null" and Free would set it to null.
               Data := New_Data;
               Free (Temp);
            end;
         end;
      end if;

      --  Store the new element at FIFO end
      Data.Elements (To_Actual_Index (Bot, Mod_Minus_One + 1)) :=
        Element;
      --  Bump the FIFO index
      Q.Bottom := Bot + 1;
   end Push;

   procedure Pop (Q : in out Deque; Element : out Element_Type) is
   --  Remove an Element from the "LIFO" end of the Deque.
   --  Element will come back as Empty if the Deque is empty.
      Data : constant Element_Data_Ptr := Q.Data;
      Mod_Minus_One : constant Actual_Index_Type := Data.Modulo_Minus_One;
      Bot : constant Index_Type := Q.Bottom;     --  TBD: or Data.Bottom?
   begin
      --  Decrement Bottom now to prevent two "Steal"s causing trouble.
      --  NOTE: Temporarily it is possible that Bottom = Top - 1.
      Q.Bottom := Bot - 1;

      declare
         --  Now get Top, having decremented Bottom.
         Top    : aliased Atomic_Index_Type := Q.Top;
         Length : constant Index_Type := Bot - Index_Type (Top);
      begin
         if Length = 0 then
            --  Queue was already empty
            Element := Empty;
            --  Restore Bottom
            Q.Bottom := Bot;
         else
            --  Queue not empty when we started, so fetch what was
            --  the bottom, but check whether it has been stolen
            --  in the mean time.
            Element :=
              Data.Elements (To_Actual_Index (Bot - 1, Mod_Minus_One + 1));
            if Length = 1 then
               --  We are competing with stealing,
               --  so we try to steal from ourself and then restore Bottom.
               if not Index_Exchange.Atomic_Compare_And_Exchange
                 (Item => Q.Top,
                  Prior => Top,
                  Desired => Top + 1)
               then
                  --  If swap fails, then we just treat it as empty
                  Element := Empty;
               end if;
               --  Restore Bottom in any case, since we bumped Top
               --  if the Swap succeeded.
               Q.Bottom := Bot;
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
   --  due to a concurrent Steal or Pop.
      Data   : constant Element_Data_Ptr := Q.Data;
      Bot    : constant Index_Type := Q.Bottom;    --  TBD: or Data.Bottom?
      Top    : aliased Atomic_Index_Type := Q.Top;
      Length : constant Index_Type := Bot - Index_Type (Top);
      Mod_Minus_One : constant Actual_Index_Type := Data.Modulo_Minus_One;
   begin
      if Length + 1 <= 1 then
         --  We have an empty deque.
         --  NOTE: Temporarily Length can be -1 (modulo Index_Type'Modulus)
         --        if we are in the middle of a concurrent "Pop".
         Element := Empty;
         Steal_Failed := False;
      else
         --  There is at least one element in the deque.
         --  Try to steal it.
         Element := Data.Elements
                      (To_Actual_Index (Index_Type (Top), Mod_Minus_One + 1));

         --  Add one, if Q.Top still equals Top
         if not Index_Exchange.Atomic_Compare_And_Exchange
           (Item => Q.Top,
            Prior => Top,
            Desired => Top + 1)
         then
            --  Return Empty if Steal_Failed
            Steal_Failed := True;
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
      if Q.Data.Modulo_Minus_One > Initial_Size - 1 then
         --  Data has been expanded, so restore it to its Initial_Size
         --  and free old Data.
         declare
            Old_Data : Element_Data_Ptr := Q.Data;
         begin
            Q.Data := new Element_Data (Initial_Size - 1);
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
               E := Elements (To_Actual_Index (Index, Elements'Length));
               Index := Index + 1;
            end loop;
            return Result;
         end;
      end if;
   end All_Elements;

end LWT.Generic_Synchronized_Deques;
