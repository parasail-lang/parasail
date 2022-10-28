------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2022, AdaCore                     --
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

with Ada.Characters.Handling;
with PSC.Per_File_Strings;
with Ada.Wide_Wide_Text_IO;
with Ada.Unchecked_Deallocation;
package body PSC.Strings is

   procedure Free is new Ada.Unchecked_Deallocation (U_String_Rec, U_String);
   procedure Free is new Ada.Unchecked_Deallocation
     (Wide_Wide_String, Wide_Wide_Str_Ptr);

   String_Table : array (U_String_Index range 1 .. U_String_Index'Last) of
     U_String;
   pragma Atomic_Components (String_Table);

   Hash_Modulus : constant Hash_Type := 9901;  --  A prime near 10,000

   Hash_Table : array (0 .. Hash_Modulus - 1) of U_String;
   pragma Atomic_Components (Hash_Table);

   Null_String_Stream_Rep : constant Integer := Integer'First;
      --  This value is used for the length to indicate a null string
      --  in the stream representation of a U_String.

   Last_Char_Pos : constant := Character'Pos (Character'Last);
      --  Largest character code that can be stored in a (narrow) String.

   protected Prot_Hash_Table is
      --  Protected object for adding elements to string table atomically

      procedure Add_Element (New_Elem : U_String; Str : out U_String);
      --  Add element, unless already present.
      --  Return Str which is same as New_Elem if new, or is pre-existing
      --  U_String if already in table.

   private

      Num_Strings : U_String_Index := Empty_U_String_Index;

   end Prot_Hash_Table;

   protected body Prot_Hash_Table is

      procedure Add_Element (New_Elem : U_String; Str : out U_String) is
         Hash_Bucket : U_String
           renames Hash_Table (New_Elem.Hash mod Hash_Modulus);
         Ptr : U_String := Hash_Bucket;
      begin
         while Ptr /= null loop
            if New_Elem.Hash = Ptr.Hash and then New_Elem.Str = Ptr.Str
              and then
                (if New_Elem.Wide_Wide_Str = null
                 then Ptr.Wide_Wide_Str = null
                 else Ptr.Wide_Wide_Str /= null
                   and then
                      New_Elem.Wide_Wide_Str.all = Ptr.Wide_Wide_Str.all)
            then
               --  Found it
               Str := Ptr;

               return;

            end if;
            Ptr := Ptr.Next;
         end loop;

         --  Not in table ==> Add it

         --  But first, add string to end of string vector and
         --  thereby assign it a unique index.
         Num_Strings := Num_Strings + 1;
         String_Table (Num_Strings) := New_Elem;
         New_Elem.Index := Num_Strings;

         --  Now insert as new first element in hash bucket.
         New_Elem.Next := Hash_Bucket;

         Hash_Bucket := New_Elem;  -- Atomic update

         Str := New_Elem;  -- New element is the unique U_String.
      end Add_Element;

   end Prot_Hash_Table;

   function Hash_String (S : String; Prefix : Hash_Type := 0)
     return Hash_Type is
   --  Get a hash for the string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   --  Prefix is hash value of chars preceding S, if we are computing
   --  the "hash" for a concatenation.
      Result : Hash_Type := Prefix;
      use Ada.Characters.Handling;
   begin
      for I in S'Range loop
         Result := Result * 127 + Character'Pos (To_Lower (S (I)));
      end loop;
      return Result;
   end Hash_String;

   function Hash_Wide_Wide_String
      (S : Wide_Wide_String; Prefix : Hash_Type := 0) return Hash_Type is
   --  Get a hash for the wide-wide-string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   --  Prefix is hash value of chars preceding S, if we are computing
   --  the "hash" for a concatenation.
      Narrow_Str : String (S'Range);
      Ignore_Wide : Boolean := False;
   begin
      --  We hash the "narrowing" of the string.
      Narrow_String (S, Narrow_Str, Ignore_Wide);
      return Hash_String (Narrow_Str);
   end Hash_Wide_Wide_String;

   procedure Dump_U_String (U : U_String);
   --  Dump U_String on the standard output
   pragma Export (Ada, Dump_U_String, "dump_ustring");

   procedure Dump_U_String (U : U_String) is
   --  Dump U_String on the standard output
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (To_Wide_Wide_String (U));
   end Dump_U_String;

   procedure Dump_U_String_Index (I : U_String_Index);
   --  Dump U_String_Index on the standard output
   pragma Export (Ada, Dump_U_String_Index, "dump_uindex");

   procedure Dump_U_String_Index (I : U_String_Index) is
   --  Dump U_String_Index on the standard output
   begin
      Dump_U_String (To_U_String (I));
   end Dump_U_String_Index;

   procedure Narrow_String
     (S : Wide_Wide_String; Narrow : out String; Uses_Wide_Char : out Boolean)
   is
   begin
      Uses_Wide_Char := False;
      for I in 1 .. S'Length loop
         declare
            Pos : constant Long_Integer :=
              Wide_Wide_Character'Pos (S (I + S'First - 1));
         begin
            if Pos > Last_Char_Pos then
               Uses_Wide_Char := True;
            end if;
            Narrow (I + Narrow'First - 1) :=
              Character'Val (Pos rem (Last_Char_Pos + 1));
         end;
      end loop;
   end Narrow_String;

   function Any_String_Lookup
     (S : String; Must_Be_New : Boolean := False;
      WW_Str : Wide_Wide_String := "")
     return U_String;
   --  WW_Str is the wide-wide version of S before it was narrowed,
   --  or is "" if the original string was already narrow.

   function Any_String_Lookup
     (S : String; Must_Be_New : Boolean := False;
      WW_Str : Wide_Wide_String := "")
     return U_String is
      --  Get the U_String for the given string
      --  If Must_Be_New is True, then return the Null_U_String
      --  if the given string is *already* in the hash table.
      Hash_Val : constant Hash_Type := Hash_String (S);
      Ptr : U_String := Hash_Table (Hash_Val mod Hash_Modulus); -- Atomic read
      Result : U_String;
   begin
      if S = "" then
         --  Special case for empty string
         return Empty_U_String;
      end if;

      --  First look it up
      while Ptr /= null loop
         if Hash_Val = Ptr.Hash and then S = Ptr.Str
            and then
              (if WW_Str = "" then Ptr.Wide_Wide_Str = null
               else Ptr.Wide_Wide_Str /= null
                      and then WW_Str = Ptr.Wide_Wide_Str.all)
         then
            --  Found it
            if Must_Be_New then
               --  Indicate string is not unique
               return Null_U_String;
            else
               return Ptr;
            end if;
         end if;
         Ptr := Ptr.Next;
      end loop;

      --  Not in table ==> Add it
      Ptr :=
        new U_String_Rec'
        (Len => S'Length,
         Next => null,
         Hash => Hash_Val,
         Index => 0,
         Str => S,
         Wide_Wide_Str => (if WW_Str = "" then null
                           else new Wide_Wide_String'(WW_Str)));

      --  Atomically add element to table
      Prot_Hash_Table.Add_Element (New_Elem => Ptr, Str => Result);

      if Ptr /= Result then
         --  Free the newly allocated element, as a matching string was
         --  already in the table.
         Free (Ptr.Wide_Wide_Str);
         Free (Ptr);
      end if;

      return Result;
   end Any_String_Lookup;

   ---------- externally visible subprograms --------

   function String_Lookup
     (S : String; Must_Be_New : Boolean := False) return U_String is
      --  Get the U_String for the given string
      --  If Must_Be_New is True, then return the Null_U_String
      --  if the given string is *already* in the hash table.
   begin
      if S = "" then
         --  Special case for empty string
         return Empty_U_String;
      else
         --  Pass the buck to the shared routine
         return Any_String_Lookup (S, Must_Be_New, WW_Str => "");
      end if;
   end String_Lookup;

   function To_String (U : U_String) return String is
   begin
      if U = Null_U_String then
         return "[null]";
      else
         return U.Str;
      end if;
   end To_String;
   --  Get back the original string

   function Find_Max_Wide_Wide_Char (S : Wide_Wide_String)
     return Wide_Wide_Character is
   --  Scan wide-wide string S and return character with max character code.
      Result : Wide_Wide_Character := Wide_Wide_Character'First;
   begin
      for C of S loop
         if C > Result then
            Result := C;
         end if;
      end loop;
      return Result;
   end Find_Max_Wide_Wide_Char;

   function Wide_Wide_String_Lookup
     (S : Wide_Wide_String; Must_Be_New : Boolean := False) return U_String is
   --  Get the U_String for the given wide-wide-string
   --  If Must_Be_New is True, then return the Null_U_String
   --  if the given string is *already* in the hash table.
      Narrow_Str : String (S'Range);
      Uses_Wide_Char : Boolean := False;
   begin
      if S = "" then
         --  Special case for empty string
         return Empty_U_String;
      end if;

      --  Create a narrow string
      Narrow_String
        (S, Narrow => Narrow_Str, Uses_Wide_Char => Uses_Wide_Char);

      if not Uses_Wide_Char then
         --  No wide characters, so treat it as a narrow string
         return Any_String_Lookup (Narrow_Str, Must_Be_New, WW_Str => "");
      else
         --  Use shared routine to do the lookup
         return Any_String_Lookup (Narrow_Str, Must_Be_New, WW_Str => S);
      end if;
   end Wide_Wide_String_Lookup;

   function To_Wide_Wide_String (U : U_String) return Wide_Wide_String is
   --  Get back the original wide-wide-string
   begin
      if U = Null_U_String then
         return "[null]";
      elsif U.Wide_Wide_Str /= null then
         return U.Wide_Wide_Str.all;
      else
         --  Was a narrow string, so need to widen it.
         declare
            Result : Wide_Wide_String (U.Str'Range);
         begin
            for I in Result'Range loop
               Result (I) := Wide_Wide_Character'Val
                               (Character'Pos (U.Str (I)));
            end loop;
            return Result;
         end;
      end if;
   end To_Wide_Wide_String;

   function Length (U : U_String) return Natural is
   --  Return length of U_String.  Length(Null_U_String) = 0.
   begin
      if U = Null_U_String then
         return 0;
      else
         return U.Len;
      end if;
   end Length;

   function Hash (U : U_String) return Hash_Type is
   --  Get a hash for the unique string.  Hash is
   --  case insensitive, so may be used for case-sensitive
   --  and case-insensitive data structures.
   begin
      if U = Null_U_String then
         return 0;
      else
         return U.Hash;
      end if;
   end Hash;

   function Index (U : U_String) return U_String_Index is
   --  Return unique index associated with string
   begin
      if U = Null_U_String then
         return 0;
      else
         return U.Index;
      end if;
   end Index;

   function To_U_String (Index : U_String_Index) return U_String is
   --  Return U_String given its unique index
   begin
      if Index = 0 then
         return Null_U_String;
      else
         return String_Table (Index);
      end if;
   end To_U_String;

   function Case_Insensitive_Equal (Left, Right : U_String) return Boolean is
      --  Return True if Left and Right are equal ignoring case.
      use Ada.Characters.Handling;
   begin
      return Left = Right
            or else (Left /= null
                    and then Right /= null
                    and then Left.Hash = Right.Hash
                    and then To_Lower (Left.Str) = To_Lower (Right.Str));
   end Case_Insensitive_Equal;

   function Escaped_Char (Char : Character) return Character is
   --  Return meaning of Char when preceded by a back slash.
   begin
      case Char is
         when '0' =>
            return ASCII.NUL;
         when 'n' =>
            return ASCII.LF;
         when 'r' =>
            return ASCII.CR;
         when 'f' =>
            return ASCII.FF;
         when 't' =>
            return ASCII.HT;
         when others =>
            --  Character is just itself
            return Char;
      end case;
   end Escaped_Char;

   function To_UTF_8 (S : String) return String is
      --  Convert any escaped characters and produce a UTF-8 string.
      Result : String (1 .. S'Length);
      I : Positive := S'First;
      J : Positive := Result'First;
   begin
      while I <= S'Last loop
         declare
            Chr : Character := S (I);
         begin
            if I = S'Last then
               --  Can't be an escape character
               null;
            elsif Chr = '\' then
               --  We have an escaped character
               I := I + 1;
               Chr := Escaped_Char (S (I));  --  TBD: Not really UTF-8
            elsif Chr = '"' and then S (I + 1) = '"' then
               --  Doubled "" => '"'
               I := I + 1;
            end if;
            Result (J) := Chr;
            I := I + 1;
            J := J + 1;
         end;
      end loop;

      return Result (1 .. J - 1);
   end To_UTF_8;

   function To_UTF_16 (S : String) return Wide_String is
      --  Convert any escaped characters and produce a UTF-16 string
      Conv : constant String := To_UTF_8 (S);
      Result : Wide_String (Conv'Range);
   begin
      for I in Conv'Range loop
         Result (I) := Wide_Character'Val (Character'Pos (Conv (I))); --  TBD
      end loop;
      return Result;
   end To_UTF_16;

   function Tokenize (S : String; Seps : String := ' ' & ASCII.HT)
     return U_String_Array is
      --  Break up S into tokens and return as array of U_Strings.
      --  Seps specifies characters that act as separators.
      Max_Tokens : constant := 100;
      Result : U_String_Array (1 .. Max_Tokens);
      Num_Tokens : Natural := 0;
      Idx : Positive := S'First;
      Start_Idx : Positive;
      Starter : Character;
   begin
      --  Break into tokens
      loop
         --  Skip over separators
         while Idx <= S'Last
           and then Strings.Contains (Seps, S (Idx))
         loop
            Idx := Idx + 1;
         end loop;

         if Idx > S'Last then
            --  All done
            return Result (1 .. Num_Tokens);
         end if;

         --  Collect another token
         Num_Tokens := Num_Tokens + 1;
         Start_Idx := Idx;
         Starter := S (Start_Idx);
         Idx := Idx + 1;

         Scan_Token : while Idx <= S'Last loop
            case S (Idx) is
               when '\' =>
                  if Idx < S'Last then
                     --  Don't look at next character
                     Idx := Idx + 1;
                  end if;
               when '"' =>
                  if Starter = '"' then
                     Idx := Idx + 1;  -- Consume this double-quote
                     if Idx >= S'Last or else S (Idx) /= '"' then
                        --  Not two double-quotes in a row; exit now
                        exit Scan_Token;
                     end if;
                  end if;
               when ''' =>
                  if Starter = ''' and then Idx > Start_Idx + 1 then
                     --  Apostrophe not the first character, so quit now
                     Idx := Idx + 1;
                     exit Scan_Token;
                  end if;
               when others =>
                  if Starter /= '"' and then Starter /= ''' then
                     --  Look for a separator when not inside a string/char lit
                     for I in Seps'Range loop
                        exit Scan_Token when Seps (I) = S (Idx);
                     end loop;
                  end if;
            end case;
            Idx := Idx + 1;
         end loop Scan_Token;

         Result (Num_Tokens) :=
            Strings.String_Lookup (S (Start_Idx .. Idx - 1));
      end loop;
   end Tokenize;

   function Skip_Leading_Spaces (S : String) return String is
   --  Return S stripped of any leading spaces
   begin
      for I in S'Range loop
         if S (I) /= ' ' then
            if I = S'First then
               return S;
            else
               return S (I .. S'Last);
            end if;
         end if;
      end loop;
      return "";
   end Skip_Leading_Spaces;

   function Contains (S : String; Char : Character) return Boolean is
   --  Return True if S contains Char.
   begin
      for I in S'Range loop
         if Char = S (I) then
            --  Found it
            return True;
         end if;
      end loop;

      --  Char not in S
      return False;
   end Contains;

   function Simple_Name (Full_Name : String) return String is
      --  Return Full_Name stripped of module parameters, if any
      --  and prefix.
      Start : Natural := Full_Name'First;
      Opt_Str : constant String := "optional";
      Opt_Len : constant Natural := Opt_Str'Length;
   begin
      for I in Full_Name'Range loop
         case Full_Name (I) is
            when '<' | '+' =>
               return Full_Name (Start .. I - 1);
            when ' ' =>
               if I = Start then
                  --  This is a leading blank
                  Start := I + 1;
               else
                  --  This is a trailing blank
                  return Full_Name (Start .. I - 1);
               end if;
            when ':' | '.' =>
               --  Start after ':' or '.'
               Start := I + 1;
            when '"' =>
               --  Return string name unmodified
               return Full_Name (I .. Full_Name'Last);
            when ''' | '#' =>
               --  This is a property; drop the property if is Class
               --  or Base; otherwise drop the prefix and add '@' prefix.
               declare
                  Prop_Str : String renames
                    Full_Name (I + 1 .. Full_Name'Last);
               begin
                  if Prop_Str = "Class" or else Prop_Str = "Base" then
                     --  Drop the property part
                     return Full_Name (Start .. I - 1);
                  else
                     --  Drop the prefix and add a '@'
                     return '@' & Full_Name (I + 1 .. Full_Name'Last);
                  end if;
               end;
            when 'o' =>
               if I = Start
                 and then Full_Name'Last >= Start + Opt_Len
                 and then Full_Name (Start .. Start + Opt_Len) =
                   Opt_Str & ' '
               then
                  --  Leading "optional "; recurse without it.
                  return Simple_Name (Full_Name
                    (Start + Opt_Len + 1 .. Full_Name'Last));
               end if;
            when others =>
               null;
         end case;
      end loop;
      --  Must go up to end of Full_Name
      return Full_Name (Start .. Full_Name'Last);
   end Simple_Name;

   procedure U_String_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_String) is
   --  Read in the contents of the string an uniquify it
      Len : constant Integer := Integer'Input (Stream);
   begin
      if Len < 0 then
         --  Most Negative means null;
         --  Other negative values means use string table.
         if Len = Null_String_Stream_Rep then
            --  Most negative length means is null
            Item := null;
         else
            --  Other negative values are indices into the String_Tab
            --  that is associated with the stream.
            declare
               use Per_File_Strings;
               String_Tab : Local_String_Map_Type renames
                 Buffered_Reader_With_Strings'Class (Stream.all).Strings.all;
            begin
               --  Fetch item from string table, select low 32 bits,
               --  and treat it as a U_String_Index.
               Item := To_U_String (U_String_Index
                 (String_Tab (Local_String_Index (-Len)) mod 2**32));
            end;
         end if;
      else
         --  Create correct-sized string and read into it
         --  and then uniquify.
         declare
            Str : String (1 .. Len);
         begin
            String'Read (Stream, Str);
            Item := String_Lookup (Str);
         end;
      end if;
   end U_String_Read;

   procedure U_String_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : U_String) is
   --  Write out the contents of the string
   begin
      if Item = null then
         --  Write out a most-negative length to indicate is null
         Integer'Write (Stream, Null_String_Stream_Rep);
      elsif Stream.all in Per_File_Strings.Buffered_Stream_With_PFS'Class then
         --  Write out a negative value indicating the index into
         --  the per-file string table.
         declare
            use Per_File_Strings;
            PFST : constant Per_File_String_Table_Ptr :=
              Buffered_Stream_With_PFS'Class
                (Stream.all).PFS.all'Unchecked_Access;
            Local_Index : constant Local_String_Index :=
              Get_Local_Index (PFST, Index (Item));
               --  Get local index assigned to string
         begin
            --  Write out negative of local index
            Integer'Write (Stream, -Integer (Local_Index));
         end;
      else
         declare
            Str : String renames To_String (Item);
         begin
            Integer'Write (Stream, Str'Length);
            String'Write (Stream, Str);
         end;
      end if;
   end U_String_Write;

   procedure U_String_Index_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : out U_String_Index'Base) is
   --  Read in the contents of the string and get its index
   begin
      Item := Index (U_String'Input (Stream));
   end U_String_Index_Read;

   procedure U_String_Index_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item : U_String_Index'Base) is
   --  Write out the contents of the string
   begin
      --  Just piggy-back on U_String'Write
      U_String'Write (Stream, To_U_String (Item));
   end U_String_Index_Write;

begin
   --  Put the empty string into the string-vector table
   String_Table (Empty_U_String_Index) := Empty_U_String;
end PSC.Strings;
