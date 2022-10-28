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

generic
   type Element_Type is private;
   Empty : Element_Type;
package Generic_Synchronized_Deques is
   --  Provide a double-ended queue that is
   --  synchronized to allow use in a work-stealing
   --  scheduler.

   type Deque is limited private;
   --  A double-ended queue with a LIFO end supporting Push/Pop
   --  and a FIFO end supporting only Steal.

   procedure Push (Q : in out Deque; Element : Element_Type)
     with Pre => Element /= Empty;
   --  Add the Element to the "LIFO" end of the Deque.

   procedure Pop (Q : in out Deque; Element : out Element_Type);
   --  Remove an Element from the "LIFO" end of the Deque.
   --  Element will come back as Empty if the Deque is empty.

   procedure Steal
     (Q : in out Deque;
      Element : out Element_Type;
      Steal_Failed : out Boolean);
   --  Remove an Element from the "FIFO" end of the Deque.
   --  Element will come back as Empty if the Deque is empty.
   --  Steal_Failed will come back as True if the attempt to steal fails
   --  due to a concurrent Steal or Pop.

   function Is_Empty (Q : Deque) return Boolean;
   --  Returns true if the Deque is empty at the moment of the call of the
   --  function.  This can become False at any time if there is a thread still
   --  executing that might Push on the deque.

   function Length (Q : Deque) return Natural;
   --  Returns count of number of items in Deque, at time of the call.
   --  Can change immediately after the call.

   procedure Reset (Q : in out Deque)
     with Pre => Is_Empty (Q), Post => Is_Empty (Q);
   --  Reset Deque to minimal capacity, reclaiming any additional storage
   --  devoted to deque from growth due to past calls on Push.
   --  This should not be called while there are still threads running
   --  that might do a Push.

   type Element_Vector is array (Positive range <>) of Element_Type;
   function All_Elements (Q : Deque) return Element_Vector;
   --  Return a vector of all of the elements, in order from
   --  oldest to newest.
   --  This should not be called while items are being added or removed!

private

   Initial_Size : constant := 2**8;
   --  Initial size of double-ended queue for a work-stealing server.

   type Index_Type is mod 2**32;
   --  This index type is allowed to wrap around
   --  We get a Actual_Index_Type value by taking the Index_Type
   --  modulo the size of the array.

   type Atomic_Index_Type is new Index_Type with Atomic;
   --  Atomic index type used for compare-and-exchange.

   type Actual_Index_Type is mod 2**32;
   --  This is the type that actually indexes into the Element_Array

   function To_Actual_Index
     (Index : Index_Type; Modulo : Actual_Index_Type)
     return Actual_Index_Type is (Actual_Index_Type (Index) mod Modulo);
   --  This function translates the "abstract" index into an "actual" index.

   type Element_Array is array (Actual_Index_Type range <>) of Element_Type;

   type Element_Data (Modulo_Minus_One : Actual_Index_Type) is record
      --  TBD: Recent_Top : Index_Type := 0;  --  no need to be atomic
      Elements : Element_Array (0 .. Modulo_Minus_One);
   end record;

   type Element_Data_Ptr is access Element_Data;

   --  Get an object that can be updated using compare-and-swap.

   type Deque is limited record
      Top    : aliased Atomic_Index_Type := 0;  --  next to steal
      Bottom : Index_Type := 0 with Atomic;     --  next to fill
         --  TBD: Move into Data to minimize cache conflicts?
      Data   : not null Element_Data_Ptr :=
        new Element_Data (Modulo_Minus_One => Initial_Size - 1)
        with Atomic;
      --  Normally Top is less than or equal to Bottom (yes, not intuitive!),
      --  with Bottom - Top being the number of items in the deque.
      --  Pushing means +1 to Bottom, Popping means -1 to Bottom.
      --  Stealing means +1 to Top.
      --  During a Pop, Bottom can temporarily be Top - 1, so need to
      --  be aware of that situation in Steal.
   end record;

end Generic_Synchronized_Deques;
