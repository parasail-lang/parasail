------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2021, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
package body PSC.Hash_Tables is

   ------- Hash_Table implementation --------

   Initial_Modulus : constant := 61;  --  A medium-sized prime number

   type Backbone_Index is new Positive;

   type Hash_Table_Entry_Ptr is access Hash_Table_Entry;

   type Hash_Table_Entry is record
      Key : aliased Key_Type;
      Elem : aliased Element_Type;
      Next_Same_Hash : Hash_Table_Entry_Ptr;
   end record;

   type Table_Entry_Array is
     array (Backbone_Index range <>) of Hash_Table_Entry_Ptr;

   type Hash_Table_Rec (Modulus : Backbone_Index) is record
      Backbone : Table_Entry_Array (1 .. Modulus);
      Num_Entries : Natural := 0;
   end record;

   Stats_Num_Enters : Natural := 0;
   Stats_Num_Enters_With_Existing_Elem : Natural := 0;
   Stats_Num_Finds : Natural := 0;
   Stats_Num_Finds_With_Existing_Elem : Natural := 0;
   Stats_Num_Entries_Skipped : Natural := 0;
   Stats_Num_Expands : Natural := 0;

   procedure Do_Nothing (Key : Key_Type; Elem : Element_Type) is
   begin
      null;
   end Do_Nothing;

   procedure Free is new Ada.Unchecked_Deallocation (
      Hash_Table_Entry,
      Hash_Table_Entry_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation (
      Hash_Table_Rec,
      Hash_Table);

   procedure Expand_Table
     (Table : in out Hash_Table; New_Modulus : Backbone_Index);
   --  Expand the table to a length given by New_Modulus.

   procedure Expand_Table
     (Table : in out Hash_Table; New_Modulus : Backbone_Index) is
      New_Table : constant Hash_Table :=
        new Hash_Table_Rec (New_Modulus);
   begin
      if Collect_Stats then
         Stats_Num_Expands := Stats_Num_Expands + 1;
      end if;
      for I in Table.Backbone'Range loop
         declare
            Ptr : Hash_Table_Entry_Ptr := Table.Backbone (I);
         begin
            while Ptr /= null loop
               --  Visit each element of old table
               --  Add element to new table
               declare
                  Next : constant Hash_Table_Entry_Ptr := Ptr.Next_Same_Hash;
                  Tab_Index : constant Backbone_Index := Backbone_Index
                    (Hash (Ptr.Key) mod Hash_Type (New_Modulus) + 1);
                  First_In_Bucket : Hash_Table_Entry_Ptr
                    renames New_Table.Backbone (Tab_Index);
               begin
                  Ptr.Next_Same_Hash := First_In_Bucket;
                  First_In_Bucket := Ptr;

                  New_Table.Num_Entries := New_Table.Num_Entries + 1;
                  Ptr := Next;
               end;
            end loop;
         end;
      end loop;
      --  Should end up with same number of entries
      pragma Assert (New_Table.Num_Entries = Table.Num_Entries);

      --  Dispose of old table
      Free (Table);

      --  Point to new table
      Table := New_Table;
   end Expand_Table;

   ------- Visible Subprograms ----------

   function Empty_Hash_Table return Hash_Table is
   --  An empty hash table
   begin
      return null;
   end Empty_Hash_Table;

   function Num_Entries (Table : Hash_Table) return Natural is
   --  Number of entries in hash table.
   begin
      if Table = null then
         return 0;
      else
         return Table.Num_Entries;
      end if;
   end Num_Entries;

   procedure Enter_Element
     (Table : in out Hash_Table;
      Key : Key_Type;
      Elem : Element_Type;
      Existing_Elem : out Element_Ref) is
      --  Add element to hash table, unless element with same key
      --  already there, in which case Existing_Elem will be set non-null.
      Existing_Pair : Pair_Ref;
   begin
      --  Pass the buck to "pair" routine
      Enter_Element_Pair (Table, Key, Elem, Existing_Pair);

      if Existing_Pair /= null then
         Existing_Elem := Existing_Pair.Elem'Access;
      else
         Existing_Elem := null;
      end if;
   end Enter_Element;

   function Find_Element
     (Table : Hash_Table;
      Key : Key_Type)
      return Element_Ref
   is
      --  Find element in table with given Key.  Return null if not found

      Element_Pair : constant Pair_Ref := Find_Element_Pair (Table, Key);
   --  Pass the buck to "pair" routine
   begin
      if Element_Pair /= null then
         --  Found
         return Element_Pair.Elem'Access;
      else
         --  Key not in table
         return null;
      end if;
   end Find_Element;

   ------- Versions returning key/element pairs -------

   procedure Enter_Element_Pair
     (Table : in out Hash_Table;
      Key : Key_Type;
      Elem : Element_Type;
      Existing_Pair : out Pair_Ref) is
   --  Add element to hash table, unless element with same key
   --  already there, in which case Existing_Pair will be set non-null.
   begin
      if Collect_Stats then
         Stats_Num_Enters := Stats_Num_Enters + 1;
      end if;

      if Table = null then
         --  First element in table
         Table := new Hash_Table_Rec (Modulus => Initial_Modulus);
      end if;

      declare
         Tab_Index : constant Backbone_Index :=
           Backbone_Index (Hash (Key) mod Hash_Type (Table.Modulus) + 1);
         First_In_Bucket : Hash_Table_Entry_Ptr renames Table.Backbone (
           Tab_Index);
         Ptr : Hash_Table_Entry_Ptr := First_In_Bucket;
      begin
         while Ptr /= null loop
            if Equiv (Key, Ptr.Key) then
               --  Already in table
               Existing_Pair := Pair_Ref (Ptr);

               if Collect_Stats then
                  Stats_Num_Enters_With_Existing_Elem :=
                    Stats_Num_Enters_With_Existing_Elem + 1;
               end if;

               return;
            end if;
            --  Go to next entry in chunk
            Ptr := Ptr.Next_Same_Hash;
            if Collect_Stats then
               Stats_Num_Entries_Skipped :=
                 Stats_Num_Entries_Skipped + 1;
            end if;
         end loop;

         --  Element with given key not in table yet,
         --  add as first entry in bucket
         Existing_Pair := null;
         First_In_Bucket :=
           new Hash_Table_Entry'
           (Key => Key,
            Elem => Elem,
            Next_Same_Hash => First_In_Bucket);

         Table.Num_Entries := Table.Num_Entries + 1;

         if Table.Num_Entries > Natural (Table.Modulus) then
            --  Double the size of the table to keep small buckets
            Expand_Table (Table, Table.Modulus * 2 + 1);
         end if;
      end;
   end Enter_Element_Pair;

   function Find_Element_Pair
     (Table : Hash_Table;
      Key : Key_Type)
      return Pair_Ref
   is
   --  Find key/element pair in table with given Key.
   --  Return null if not found
   begin
      if Collect_Stats then
         Stats_Num_Finds := Stats_Num_Finds + 1;
      end if;

      if Table /= null then
         declare
            Tab_Index : constant Backbone_Index :=
              Backbone_Index (Hash (Key) mod Hash_Type (Table.Modulus) + 1);
            Ptr : Hash_Table_Entry_Ptr := Table.Backbone (Tab_Index);
         begin
            while Ptr /= null loop
               if Equiv (Ptr.Key, Key) then
                  --  Found
                  if Collect_Stats then
                     Stats_Num_Finds_With_Existing_Elem :=
                       Stats_Num_Finds_With_Existing_Elem + 1;
                  end if;

                  return Pair_Ref (Ptr);
               end if;
               --  Go to next entry in chunk
               Ptr := Ptr.Next_Same_Hash;
               if Collect_Stats then
                  Stats_Num_Entries_Skipped :=
                    Stats_Num_Entries_Skipped + 1;
               end if;
            end loop;
         end;
      end if;

      --  Key not in table
      return null;
   end Find_Element_Pair;

   function Key (Pair : Pair_Ref) return Key_Ref is
   --  Return R/O reference to key of pair
   begin
      return Pair.Key'Access;
   end Key;

   function Element (Pair : Pair_Ref) return Element_Ref is
   --  Return R/W reference to element of pair
   begin
      return Pair.Elem'Access;
   end Element;

   procedure Iterate (Table : Hash_Table) is
   --  Iterate over each entry in the table.
   begin
      if Table /= null then
         for I in Table.Backbone'Range loop
            declare
               Ptr : Hash_Table_Entry_Ptr := Table.Backbone (I);
            begin
               while Ptr /= null loop
                  Action (Pair_Ref (Ptr));
                  Ptr := Ptr.Next_Same_Hash;
               end loop;
            end;
         end loop;
      end if;
   end Iterate;

   procedure Iterate_And_Remove (Table : in out Hash_Table) is
   --  Iterate over the table and remove each entry.
   --  Reclaim the storage occupied by the table.
   begin
      if Table /= null then
         for I in Table.Backbone'Range loop
            declare
               Ptr : Hash_Table_Entry_Ptr renames Table.Backbone (I);
            --  Renaming allow us to remove elements as they are processed
            begin
               while Ptr /= null loop
                  declare
                     Next : Hash_Table_Entry_Ptr := Ptr.Next_Same_Hash;
                  begin
                     --  Call Action with Key/Element pair and then
                     --  Reclaim the storage.
                     Action (Ptr.Key, Ptr.Elem);
                     Free (Ptr);
                     Ptr := Next;  --  Carve element out of bucket.
                  end;
               end loop;
            end;
         end loop;
         --  Release the backbone
         Free (Table);
      end if;
   end Iterate_And_Remove;

   procedure Reclaim_Inst is new Iterate_And_Remove (Do_Nothing);
   --  Create instance that reclaims the elements but doesn't otherwise
   --  do anything.

   procedure Reclaim (Table : in out Hash_Table) renames Reclaim_Inst;
   --  Reclaim the storage occupied by the table.
   --  Leave the table empty.

   procedure Dump_Stats is
   --  Dump statistics for this instantiation of Hash_Tables
      use Ada.Text_IO;
   begin
      if Collect_Stats then
         Put_Line (" Num_Enters: " & Natural'Image (Stats_Num_Enters));
         Put_Line (" Num_Enters_With_Existing_Elem : " &
           Natural'Image (Stats_Num_Enters_With_Existing_Elem));
         Put_Line (" Num_Finds: " &
           Natural'Image (Stats_Num_Finds));
         Put_Line (" Num_Finds_With_Existing_Elem: " &
           Natural'Image (Stats_Num_Finds_With_Existing_Elem));
         Put_Line (" Num_Entries_Skipped: " &
           Natural'Image (Stats_Num_Entries_Skipped));
         Put_Line (" Num_Expands: " &
           Natural'Image (Stats_Num_Expands));
      end if;
   end Dump_Stats;
   --  This is a no-op if Collect_Stats is False.

end PSC.Hash_Tables;
