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

with PSC.Stream_Output;
with PSC.String_Streams;
package body PSC.Trees.Lists is

   --  Lists of tree nodes, useful for statement sequences, etc.

   function Length (L : List) return Natural is
      --  Length of list
   begin
      return L.Length;
   end Length;

   function Nth_Element (L : List; N : Positive) return Optional_Tree is
      --  Return Nth element of list.
      --  Requires N <= Length(L)
   begin
      if N = L.Length then
         --  Fast path to the last element
         return L.Last.Item;
      else
         --  Skip over the preceding elements
         declare
            Elem : List_Element_Ptr := L.First;
            pragma Assert (Elem /= null);
         begin
            for I in 2 .. N loop
               Elem := Elem.Next;
               pragma Assert (Elem /= null);
            end loop;

            return Elem.Item;
         end;
      end if;
   end Nth_Element;

   function Tail (L : List) return List is
   --  Return everything but the first element.
   --  Requires Length (L) >= 1
      Len : constant Natural := Length (L);
      pragma Assert (Len >= 1);
   begin
      if Len = 1 then
         return Empty_List;
      else
         --  NOTE: We do not copy the list -- we just point to tail.
         return List'(First => L.First.Next, Last => L.Last,
            Length => Len - 1);
      end if;
   end Tail;

   function Replace_Nth_Element
     (L : List;
      N : Positive;
      New_Operand : Optional_Tree)
      return List
   is
      --  Return a new list after replacing the Nth element of list.
      --  Requires N <= Length(L)
      --  TBD: This is painful if we end up replacing multiple elements in
      --  list.
      --      Better would be to build up an array of new values and
      --      create a new tree node from that array of values.
      Old_Elem : List_Element_Ptr := L.First;
      Result : List := L;
      Last_Elem : List_Element_Ptr := null;
   begin
      --  Copy the first N elements of list
      for I in 1 .. N loop
         declare
            pragma Assert (Old_Elem /= null);
            --  Make a copy of Old_Elem.
            New_Elem : constant List_Element_Ptr :=
              new List_Element'(Old_Elem.all);
         begin
            --  Hook New_Elem into new list
            if Last_Elem = null then
               Result.First := New_Elem;
            else
               Last_Elem.Next := New_Elem;
            end if;
            Old_Elem := Old_Elem.Next;
            Last_Elem := New_Elem;
         end;
      end loop;

      --  Set the (new) Nth element value.
      Last_Elem.Item := New_Operand;

      if Last_Elem.Next = null then
         --  We replaced the last element of the list
         Result.Last := Last_Elem;
      end if;

      --  Return the new list
      return Result;
   end Replace_Nth_Element;

   procedure Append (L : in out List; Element : Optional_Tree) is
      --  Add element to end of list
      New_Element : constant List_Element_Ptr :=
        new List_Element'(Item => Element, Next => null);
   begin
      if L.Last = null then
         --  This is the first element
         L.First := New_Element;
      else
         --  Append to end
         L.Last.Next := New_Element;
      end if;
      --  New last element
      L.Last := New_Element;
      L.Length := L.Length + 1;
   end Append;

   procedure Append (L : in out List; Tail : List) is
   --  Add list to end of list
   begin
      --  Just do it one at a time...
      for I in 1 .. Length (Tail) loop
         Append (L, Nth_Element (Tail, I));
      end loop;
   end Append;

   function Prepend
     (Element : Optional_Tree;
      Tail : List)
      return List
   is
      --  Put Element at front of list
      New_Element : constant List_Element_Ptr :=
        new List_Element'(Item => Element, Next => Tail.First);
   begin
      if Tail.Last = null then
         --  Tail is empty, only element is new one
         return (First => New_Element, Last => New_Element, Length => 1);
      else
         --  Tail non-empty, new list has same last element
         return (First => New_Element, Last => Tail.Last,
                 Length => Tail.Length + 1);
      end if;
   end Prepend;

   function Make (Elements : Tree_Array) return List is
      --  Create list out of element array
      Result : List;
   begin
      for I in Elements'Range loop
         Append (Result, Elements (I));
      end loop;
      return Result;
   end Make;

   function Empty_List return List is
   --  Create empty list
   begin
      return List'(First => null, Last => null, Length => 0);
   end Empty_List;

   function Is_Empty (L : List) return Boolean is
   --  Return True if list is empty.
   begin
      return L.First = null;
   end Is_Empty;

   procedure Apply_To_Nth_Element (L : List; N : Positive) is
      --  Apply Action to Nth element of list
      procedure Apply_Action is new Trees.Apply (Action);
   begin
      Apply_Action (Nth_Element (L, N));
   end Apply_To_Nth_Element;

   procedure Apply_To_Nth_Element_RW (L : in out List; N : Positive) is
      --  Apply RW Action to Nth element of list
      procedure Apply_Action is new Trees.Apply_RW (Action);
      Var_Ot : Optional_Tree := Nth_Element (L, N);
   --  Make a variable so as to apply an RW action
   begin
      Apply_Action (Var_Ot);
   end Apply_To_Nth_Element_RW;

   procedure Apply_To_List (L : List) is
      --  Apply Action to all elements of list
      procedure Apply_Action is new Trees.Apply (Action);

      Elem : List_Element_Ptr := L.First;
   begin
      while Elem /= null loop
         Apply_Action (Elem.Item);
         Elem := Elem.Next;
      end loop;
   end Apply_To_List;

   procedure Apply_To_List_RW (L : in out List) is
      --  Apply RW Action to all elements of list
      procedure Apply_Action is new Trees.Apply_RW (Action);

      Elem : List_Element_Ptr := L.First;
   begin
      while Elem /= null loop
         declare
            Var_Ot : Optional_Tree := Elem.Item;
         --  Make a variable so as to apply an RW action
         begin
            Apply_Action (Var_Ot);
            Elem := Elem.Next;
         end;
      end loop;
   end Apply_To_List_RW;

   procedure Display_List
     (L : List;
      On : access Ada.Streams.Root_Stream_Type'Class;
      Separator : String := "";
      Terminator : String := "";
      Indent : Natural := 0;
      Use_Short_Form : Boolean := False) is
      --  Display list on stream with given separator
      --  between elements.
      --  If Indent > 0, then each list element starts on its
      --  own line.
      --  If Indent > 0 and Terminator is not empty, then
      --  last list element is followed by a newline.
      --  Use_Short_Form is passed through to calls on Display_Subtree.
      Elem : List_Element_Ptr := L.First;
      Indent_To_Pass : Natural := Indent;
      Indent_To_Do : Natural := 0;
   begin
      if Terminator = "" then
         --  If no terminator, then do indent ourselves
         --  after each newline.
         Indent_To_Do := Indent;
         Indent_To_Pass := 0;
      end if;

      while Elem /= null loop
         if Is_Null (Elem.Item) then
            --  Skip over null items
            Elem := Elem.Next;
         else
            Display_Subtree (Elem.Item, On, Indent_To_Pass,
              Use_Short_Form => Use_Short_Form);
            Elem := Elem.Next;
            while Elem /= null and then Is_Null (Elem.Item) loop
               --  Skip over null items
               Elem := Elem.Next;
            end loop;
            if Elem /= null then
               PSC.Stream_Output.Put (On, Separator);
               if Indent > 0 then
                  --  If Indent > 0 then each item goes on its own line
                  PSC.Stream_Output.New_Line (On, Indent => Indent_To_Do);
               end if;
            elsif Terminator /= "" then
               PSC.Stream_Output.Put (On, Terminator);
               if Indent > 0 then
                  --  If Indent > 0 and Terminator not empty
                  --  then end with a newline.
                  PSC.Stream_Output.New_Line (On);
               end if;
            end if;
         end if;
      end loop;
   end Display_List;

   function List_Image
     (L : List;
      Separator : String := "";
      Terminator : String := "";
      Max_Chars : Positive := 2000)
      return String
   is
      --  Return a text image of the list (up to Max_Chars in length)
      use PSC.String_Streams;
      Str : aliased String_Stream
        (Ada.Streams.Stream_Element_Offset (Max_Chars * Character'Size /
                                            Ada.Streams.Stream_Element'Size));
   begin
      --  Put image into the stream
      Display_List
        (L,
         On => Str'Access,
         Separator => Separator,
         Terminator => Terminator,
         Indent => 0);
      --  Get stream as a string
      return String_Of (Str'Access);
   end List_Image;

end PSC.Trees.Lists;
