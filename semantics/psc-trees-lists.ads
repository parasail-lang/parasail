------------------------------------------------------------------------------
--                              P A R A S A I L                             --
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
-- In particular,  you can freely  distribute your programs  built with     --
-- the ParaSail, Sparkel, Javallel, or Parython compiler, including any     --
-- required library run-time units written in Ada or in any of the above    --
-- languages, using any licensing terms  of your choosing.                  --
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

package PSC.Trees.Lists is

   --  Lists of tree nodes, useful for statement sequences, etc.

   type List is private;
   --  A possibly empty list

   function Length (L : List) return Natural;
   --  Length of list

   function Nth_Element (L : List; N : Positive) return Optional_Tree;
   --  Return Nth element of list.
   --  Requires N <= Length(L)

   function Tail (L : List) return List;
   --  Return everything but the first element.
   --  Requires Length (L) >= 1

   function Replace_Nth_Element
     (L : List;
      N : Positive;
      New_Operand : Optional_Tree)
      return List;
   --  Return a new list after replacing the Nth element of list.
   --  Requires N <= Length(L)

   procedure Append (L : in out List; Element : Optional_Tree);
   --  Add element to end of list

   procedure Append (L : in out List; Tail : List);
   --  Add list to end of list

   function Prepend
     (Element : Optional_Tree;
      Tail : List)
      return List;
   --  Put Element at front of list

   function Make (Elements : Tree_Array) return List;
   --  Create list out of element array

   function Empty_List return List;
   --  Create empty list

   function Is_Empty (L : List) return Boolean;
   --  Return True if list is empty.

   generic
      with procedure Action (Elem : Tree'Class);
   procedure Apply_To_Nth_Element (L : List; N : Positive);
   --  Apply Action to Nth element of list

   generic
      with procedure Action (Elem : in out Tree'Class);
   procedure Apply_To_Nth_Element_RW (L : in out List; N : Positive);
   --  Apply RW Action to Nth element of list

   generic
      with procedure Action (Elem : Tree'Class);
   procedure Apply_To_List (L : List);
   --  Apply Action to all elements of list

   generic
      with procedure Action (Elem : in out Tree'Class);
   procedure Apply_To_List_RW (L : in out List);
   --  Apply RW Action to all elements of list

   procedure Display_List
     (L : List;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Separator : String := "";
      Terminator : String := "";
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False);
   --  Display list on stream with given separator
   --  between elements.
   --  If Indent > 0, then each list element starts on its
   --  own line.
   --  If Indent > 0 and Terminator is not empty, then
   --  last list element is followed by a newline.
   --  Use_Short_Form is passed through to calls on Display_Subtree.

   function List_Image
     (L : List;
      Separator : String := "";
      Terminator : String := "";
      Max_Chars : Positive := 2000)
      return String;
   --  Return a text image of the list (up to Max_Chars in length)

private

   type List_Element;
   type List_Element_Ptr is access List_Element;

   type List_Element is record
   --  An element of a list
      Item : Optional_Tree;
      Next : List_Element_Ptr;
   end record;

   type List is record
   --  The header of a list
      First : List_Element_Ptr := null;
      Last : List_Element_Ptr := null;
      Length : Natural := 0;
   end record;

end PSC.Trees.Lists;
