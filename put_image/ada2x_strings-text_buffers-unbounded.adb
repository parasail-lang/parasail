pragma Ada_2020;

with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Conversions;
with Ada.Unchecked_Deallocation;
package body Ada2X_Strings.Text_Buffers.Unbounded is

   function To_Wide_Wide (C : Character) return Wide_Wide_Character is
     (Wide_Wide_Character'Val (Character'Pos (C)));

   function To_Wide_Wide (C : Wide_Character) return Wide_Wide_Character is
     (Wide_Wide_Character'Val (Wide_Character'Pos (C)));
   --  Conversions [Wide_]Character --> Wide_Wide_Character.
   --  These cannot fail.

   NL : String renames New_Line_String;

   procedure Wide_Get (
      Buffer     : in out Buffer_Type;
      Item       :    out Wide_String;
      Last       :    out Natural;
      Substitute :        Wide_Character := ' ') is
      Wide_Wide_Item : Wide_Wide_String (Item'Range);
   begin
      Wide_Wide_Get (Buffer, Wide_Wide_Item, Last);
      Item (Item'First .. Last) :=
        Ada.Characters.Conversions.To_Wide_String
          (Wide_Wide_Item (Wide_Wide_Item'First .. Last), Substitute);
   end Wide_Get;

   procedure Wide_Put_UTF_16 (
      Buffer : in out Buffer_Type;
      Item   :        UTF_Encoding.UTF_16_Wide_String) is
   begin
      Wide_Wide_Put
        (Buffer, UTF_Encoding.Wide_Wide_Strings.Decode (Item));
   end Wide_Put_UTF_16;

   procedure Increase_Indent
     (Buffer : in out Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) is
   begin
      Buffer.Current_Indent := Buffer.Current_Indent + Amount;
   end Increase_Indent;

   procedure Decrease_Indent
     (Buffer : in out Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) is
   begin
      Buffer.Current_Indent := Buffer.Current_Indent - Amount;
   end Decrease_Indent;

   function From_Wide (C : Wide_Character) return Character is
     (Character'Val (Wide_Character'Pos (C)));
   function From_Wide_Wide (C : Wide_Wide_Character) return Character is
     (Character'Val (Wide_Wide_Character'Pos (C)));
   function From_Wide_Wide (C : Wide_Wide_Character) return Wide_Character is
     (Wide_Character'Val (Wide_Wide_Character'Pos (C)));
   --  Conversions [Wide_]Wide_Character --> [Wide_]Character.
   --  These fail if the character is out of range.

   subtype Character_7 is Character range
     Character'Val (0) .. Character'Val (2**7 - 1);

   procedure Put_Octet (S : in out Buffer_Type; Item : Character) with Inline;
   --  Send a single octet to the current Chunk

   procedure Tab_To_Column
     (S : in out Buffer_Type; Column : Text_Buffer_Count);
   --  Emit enough spaces to reach given Column.

   procedure Adjust_Column (S : in out Buffer_Type) with Inline;
   --  Adjust the column for a non-NL character.
   --  Initialize Cur_Chunk if null.

   function Chars_In_UTF_8_String (Item : UTF_8_Lines) return Natural;
   --  Return count of characters in UTF_8_String by counting number
   --  of octets that are < 16#80# or >= 16#C0#

   procedure Alloc_Chunk (S : in out Buffer_Type);
   --  Chunk is full, so allocate a new chunk.

   procedure Put_Character (S : in out Buffer_Type; Item : Character);
   --  Put one Character into buffer, and check for NL, etc.

   procedure Put_Wide_Character
     (S : in out Buffer_Type; Item : Wide_Character);
   --  Put one Wide_Character into buffer, and check for NL, etc.

   procedure Put_Wide_Wide_Character
     (S : in out Buffer_Type; Item : Wide_Wide_Character);
   --  Put one Wide_Wide_Character into buffer, and check for NL, etc.

   procedure Put_UTF_8_No_NL (S : in out Buffer_Type; Item : UTF_8);
   --  Put out a UTF_8_String that has no embedded newlines.

   procedure Put_UTF_8_Outline (S : in out Buffer_Type; Item : UTF_8);
   --  Out-of-line portion of Put_UTF_8_No_NL.
   --  This exists solely to make Put_UTF_8_No_NL
   --  small enough to reasonably inline it.

   function UTF_8_Length (S : Buffer_Type) return Natural;
   --  Total number of UTF_8 octets remaining to be read.

   function Count_UTF_8_Octets (S : Buffer_Type; Num_Chars : Natural)
     return Natural
      with Pre => Num_Chars < Integer (S.Character_Count);
   --  Count how many UTF_8 octets need to be read to
   --  produce Num_Chars (wide) characters.

   procedure Read_UTF_8
     (S : in out Buffer_Type; Result : out UTF_8_Lines);
   --  Read UTF_8 characters out of buffer.
   --  Update S.Num_UTF_8_Read.
   --  Do *not* update S.Character_Count

   procedure Free_Chunks (S : in out Buffer_Type);
   --  Reclaim space in Buffer.
   --  Reset Cur_Chunk, Num_Extra_Chunks,
   --  Last, Num_UTF_8_Read, Character_Count.

   procedure Free_Chunks (S : in out Buffer_Type) is
      procedure Free is new Ada.Unchecked_Deallocation (Chunk, Chunk_Access);
      Cur : Chunk_Access := S.Initial_Chunk.Next;
   begin
      while Cur /= null loop
         declare
            Temp : constant Chunk_Access := Cur.Next;
         begin
            Free (Cur);
            Cur := Temp;
         end;
      end loop;
      S.Initial_Chunk.Next := null;

      --  Reset Cur_Chunk, Last, Num_UTF_8_Read, Character_Count, etc.
      S.Cur_Chunk := S.Initial_Chunk'Unchecked_Access;
      S.Num_Extra_Chunks := 0;
      S.Last := 0;
      S.Num_UTF_8_Read := 0;
      S.Character_Count := 0;
   end Free_Chunks;

   procedure Clear (Buffer : in out Buffer_Type) is
   begin
      Free_Chunks (Buffer);
   end Clear;

   procedure Alloc_Chunk (S : in out Buffer_Type) is
   begin
      pragma Assert (S.Last = S.Chunk_Length);
      pragma Assert (S.Cur_Chunk.Next = null);
      S.Cur_Chunk.Next := new Chunk (S.Chunk_Length);
      S.Cur_Chunk := S.Cur_Chunk.Next;
      S.Num_Extra_Chunks := @ + 1;
      S.Last := 0;
   end Alloc_Chunk;

   procedure Put_Octet (S : in out Buffer_Type; Item : Character) is
   begin
      S.Last := @ + 1;
      S.Cur_Chunk.Chars (S.Last) := Item;
      pragma Assert (S.Chunk_Length = S.Cur_Chunk.Chars'Length);
      if S.Last = S.Chunk_Length then
         Alloc_Chunk (S);
      end if;
   end Put_Octet;

   procedure New_Line (Buffer : in out Buffer_Type) is
   begin
      if Buffer.Cur_Chunk = null then
         --  Check for uninitialized Cur_Chunk.
         Buffer.Cur_Chunk := Buffer.Initial_Chunk'Unchecked_Access;
      end if;
      Buffer.Pos_In_Line := 1;
      for I in NL'Range loop
         Put_Octet (Buffer, NL (I));
         Buffer.Character_Count := @ + 1;
      end loop;
   end New_Line;

   procedure Tab_To_Column
     (S : in out Buffer_Type; Column : Text_Buffer_Count) is
   begin
      if S.Pos_In_Line < Column then
         for X in 1 .. Column - S.Pos_In_Line loop
            Put_Octet (S, ' ');
            S.Character_Count := @ + 1;
         end loop;
         S.Pos_In_Line := Column;
      end if;
   end Tab_To_Column;

   procedure Adjust_Column (S : in out Buffer_Type) is
   begin
      --  If we're in the first column, indent. This is handled here, rather
      --  than when we see NL, because we don't want spaces in a blank line.
      --  The character we're about to put is not NL; NL is handled in
      --  New_Line. So after indenting, we simply increment the Column.

      if S.Pos_In_Line = 1 then
         if S.Cur_Chunk = null then
            --  Check for uninitialized Cur_Chunk
            S.Cur_Chunk := S.Initial_Chunk'Unchecked_Access;
         end if;
         Tab_To_Column (S, S.Current_Indent + 1);
      end if;
      S.Pos_In_Line := @ + 1;
   end Adjust_Column;

   function Chars_In_UTF_8_String (Item : UTF_8_Lines) return Natural is
      Result : Natural := 0;
   begin
      for Octet of Item loop
         if Character'Pos (Octet) not in 16#80# .. 16#BF# then
            --  Count octets that are < 16#80# or >= 16#C0#
            --  i.e. that start a UTF-8 multi-byte code.
            Result := @ + 1;
         end if;
      end loop;
      return Result;
   end Chars_In_UTF_8_String;

   procedure Put_7bit (S : in out Buffer_Type; Item : Character_7) is
   begin
      Adjust_Column (S);
      Put_Octet (S, Item);
      S.Character_Count := @ + 1;
   end Put_7bit;

   procedure Put_7bit_NL (S : in out Buffer_Type; Item : Character_7) is
   begin
      if Item = NL (NL'Last) then
         New_Line (S);
      elsif NL'Last > NL'First then
         for I in NL'First .. NL'Last - 1 loop
            if Item = NL (I) then
               --  Ignore other control characters appearing in NL.
               return;
            end if;
         end loop;
         Put_7bit (S, Item);
      else
         Put_7bit (S, Item);
      end if;
   end Put_7bit_NL;

   procedure Put_Character (S : in out Buffer_Type; Item : Character) is
   begin
      if Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, Item);
      else
         Put_Wide_Wide_Character (S, To_Wide_Wide (Item));
      end if;
   end Put_Character;

   procedure Put_Wide_Character
     (S : in out Buffer_Type; Item : Wide_Character) is
   begin
      if Wide_Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, From_Wide (Item));
      else
         Put_Wide_Wide_Character (S, To_Wide_Wide (Item));
      end if;
   end Put_Wide_Character;

   procedure Put_Wide_Wide_Character
     (S : in out Buffer_Type; Item : Wide_Wide_Character) is
   begin
      if Wide_Wide_Character'Pos (Item) < 2**7 then
         Put_7bit_NL (S, From_Wide_Wide (Item));
      else
         S.All_7_Bits := False;
         if Wide_Wide_Character'Pos (Item) >= 2**8 then
            S.All_8_Bits := False;
         end if;
         declare
            Temp : UTF_8_Lines renames
              UTF_Encoding.Wide_Wide_Strings.Encode ((1 => Item));
         begin
            Adjust_Column (S);
            for X of Temp loop
               Put_Octet (S, X);
            end loop;
            S.Character_Count := @ + 1;
         end;
      end if;
   end Put_Wide_Wide_Character;

   procedure Put_UTF_8_Outline (S : in out Buffer_Type; Item : UTF_8) is
   begin
      if S.Last + Item'Length = S.Chunk_Length then
         --  Item fits exactly in current chunk

         S.Cur_Chunk.Chars (S.Last + 1 .. S.Last + Item'Length) := Item;
         S.Last := S.Last + Item'Length;
         S.Character_Count := @ +
           Text_Buffer_Count (Chars_In_UTF_8_String (Item));
         Alloc_Chunk (S);
         --  ???Seems like maybe we shouldn't call Alloc_Chunk until we
         --  have MORE characters. But then we can't pass Chunk_Length => 1 to
         --  Create_File to get unbuffered output.
      else
         --  We get here only if Item doesn't fit in the current chunk, which
         --  should be fairly rare. We split Item into Left and Right, where
         --  Left exactly fills the current chunk, and recurse on Left and
         --  Right. Right will fit into the next chunk unless it's very long,
         --  so another level of recursion will be extremely rare.

         declare
            Left_Length : constant Natural := S.Chunk_Length - S.Last;
            Right_First : constant Natural := Item'First + Left_Length;
            Left : UTF_8 renames Item (Item'First .. Right_First - 1);
            Right : UTF_8 renames Item (Right_First .. Item'Last);
            pragma Assert (Left & Right = Item);
         begin
            Put_UTF_8_No_NL (S, Left);    -- This will call Alloc_Chunk.
            Put_UTF_8_No_NL (S, Right);   -- This might call Alloc_Chunk,
                                    --  but probably not.
         end;
      end if;
   end Put_UTF_8_Outline;

   procedure Put_UTF_8_No_NL (S : in out Buffer_Type; Item : UTF_8) is
   begin
      if Item'Length = 0 then
         return;  --  Ignore empty strings
      end if;

      Adjust_Column (S);

      if S.Last + Item'Length < S.Chunk_Length then
         --  Item fits in current chunk

         S.Cur_Chunk.Chars (S.Last + 1 .. S.Last + Item'Length) := Item;
         S.Last := S.Last + Item'Length;
         S.Character_Count := @ +
           Text_Buffer_Count (Chars_In_UTF_8_String (Item));
      else
         Put_UTF_8_Outline (S, Item);
      end if;
   end Put_UTF_8_No_NL;

   procedure Put_UTF_8_Lines (S : in out Buffer_Type; Item : UTF_8_Lines) is
      Line_Start, Index : Integer := Item'First;
      --  Needs to be Integer, because Item'First might be negative for empty
      --  Items.
   begin
      while Index <= Item'Last loop
         if Item (Index) = NL (NL'First)
           and then
             (NL'Last = NL'First
              or else
                (Index + (NL'Last - NL'First) <= Item'Last
                   and then
                 Item (Index + 1 .. Index + (NL'Last - NL'First)) =
                   NL (NL'First + 1 .. NL'Last)))
         then
            --  We match the NL string
            if Index > Line_Start then
               Put_UTF_8_No_NL (S, Item (Line_Start .. Index - 1));
            end if;
            New_Line (S);
            S.Pos_In_Line := 1;
            Index := @ + NL'Length;
            Line_Start := Index;
         else
            --  Not an NL; presume is a normal character.
            Index := @ + 1;
         end if;
      end loop;

      if Index > Line_Start then
         Put_UTF_8_No_NL (S, Item (Line_Start .. Index - 1));
      end if;
   end Put_UTF_8_Lines;

   procedure Put_UTF_8 (
      Buffer : in out Buffer_Type;
      Item   :        UTF_Encoding.UTF_8_String) is
   begin
      Put_UTF_8_Lines (Buffer, Item);
   end Put_UTF_8;

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   :        String) is
   begin
      for X of Item loop
         Put_Character (Buffer, X);
      end loop;
   end Put;

   procedure Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_String) is
   begin
      for X of Item loop
         Put_Wide_Character (Buffer, X);
      end loop;
   end Wide_Put;

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_Wide_String) is
   begin
      for X of Item loop
         Put_Wide_Wide_Character (Buffer, X);
      end loop;
   end Wide_Wide_Put;

   function UTF_8_Length (S : Buffer_Type) return Natural is
   begin
      return S.Num_Extra_Chunks * S.Chunk_Length + S.Last - S.Num_UTF_8_Read;
   end UTF_8_Length;

   function Count_UTF_8_Octets (S : Buffer_Type; Num_Chars : Natural)
     return Natural is

      Num_Octets : Natural := 0;
      Chars_Left : Natural := Num_Chars;

      procedure Count (Str : UTF_Encoding.UTF_8_String);
      --  Bump Num_Octets by number of octets in Str it
      --  will take to produce "Chars_Left" characters,
      --  or by Str'Length if it will take all of them.
      --  Decrement Chars_Left by number of fully characters accounted for.

      procedure Count (Str : UTF_Encoding.UTF_8_String) is
         I : Positive := Str'First;
         subtype Char_Pos is Natural range 0 .. 16#FF#;
      begin
         while Chars_Left > 0 loop
            case Char_Pos'(Character'Pos (Str (I))) is
               when 16#00# .. 16#7F# =>
                  --  An ASCII character
                  I := @ + 1;

               when 16#80# .. 16#BF# =>
                  --  We are in the middle of a multi-byte sequence
                  --  Should only happen on first character of string
                  --  which should be at the beginning of a chunk.
                  pragma Assert (I = Str'First);
                  --  Skip over the rest of the multi-byte sequence
                  while I <= Str'Last
                    and then Character'Pos (Str (I)) in 16#80# .. 16#BF#
                  loop
                     I := @ + 1;
                  end loop;

               when 16#C0# .. 16#DF# =>
                  I := @ + 2;

               when 16#E0# .. 16#EF# =>
                  I := @ + 3;

               when 16#F0# .. 16#F7# =>
                  I := @ + 4;

               when 16#F8# .. 16#FF# =>
                  --  NOTE: Not officially part of UTF-8
                  I := @ + 5;
            end case;

            --  Don't count partial multi-byte sequence as a character
            exit when I > Str'Last + 1;

            Chars_Left := @ - 1;
         end loop;

         --  Count number of octets
         if I > Str'Last then
            --  We used them all
            Num_Octets := @ + Str'Length;
         else
            Num_Octets := @ + I - Str'First + 1;
         end if;
      end Count;

      Cur : access constant Chunk := S.Initial_Chunk'Access;
      Num_Chunks_To_Skip : constant Natural :=
        S.Num_UTF_8_Read / S.Chunk_Length;
      First_Unread : Positive :=   --  Where to start reading in chunk
        S.Num_UTF_8_Read mod S.Chunk_Length + 1;

   begin  --  Count_UTF_8_Octets

      if S.All_7_Bits then
         --  Easy case
         return Num_Chars;
      end if;

      --  Skip already-read chunks
      for I in 1 .. Num_Chunks_To_Skip loop
         Cur := Cur.Next;
      end loop;

      while Chars_Left > 0 loop
         if Cur.Next = null then
            --  Last chunk
            Count (Cur.Chars (First_Unread .. S.Last));

            --  Should have accounted for all characters
            pragma Assert (Chars_Left = 0);

            exit;
         end if;

         Count (Cur.Chars (First_Unread .. S.Chunk_Length));

         --  Will start at beginning of next chunk
         First_Unread := 1;

         Cur := Cur.Next;
      end loop;

      return Num_Octets;
   end Count_UTF_8_Octets;

   procedure Read_UTF_8
     (S : in out Buffer_Type; Result : out UTF_8_Lines)
   is
      Cur : access constant Chunk := S.Initial_Chunk'Access;
      First : Positive := Result'First;
      Num_Chunks_To_Skip : constant Natural :=
        S.Num_UTF_8_Read / S.Chunk_Length;
      First_Unread : Positive :=   --  Where to start reading in chunk
        S.Num_UTF_8_Read mod S.Chunk_Length + 1;
   begin
      --  Skip already-read chunks
      for I in 1 .. Num_Chunks_To_Skip loop
         pragma Assert (S.Chunk_Length = Cur.Chars'Last);
         Cur := Cur.Next;
      end loop;

      while First <= Result'Last loop
         pragma Assert (S.Chunk_Length = Cur.Chars'Last);

         if Cur.Next = null then
            --  Last chunk
            Result (First .. Result'Last) :=
              Cur.Chars (First_Unread ..
                         First_Unread + (Result'Last - First));
            exit;
         end if;

         if First_Unread > 1 then
            --  Starting in middle of a chunk
            declare
               Last_Minus_First : constant Natural :=
                 Integer'Min
                   (S.Chunk_Length - First_Unread, Result'Last - First);
            begin
               Result (First .. First + Last_Minus_First) :=
                 Cur.Chars (First_Unread .. First_Unread + Last_Minus_First);

               First := First + Last_Minus_First + 1;

               --  Will start at beginning of next chunk
               First_Unread := 1;
            end;

         elsif Result'Last < First + S.Chunk_Length - 1 then
            --  Reading less than a full chunk
            Result (First .. Result'Last) :=
              Cur.Chars (First_Unread .. First_Unread + (Result'Last - First));

            exit;  --  All done now

         else
            --  Reading a whole chunk
            Result (First .. First + S.Chunk_Length - 1) := Cur.Chars;
            First := First + S.Chunk_Length;
         end if;
         Cur := Cur.Next;
      end loop;

      --  Update the number of UTF_8 octets that have been read
      S.Num_UTF_8_Read := @ + Result'Length;
   end Read_UTF_8;

   function Get_UTF_8 (
      Buffer : in out Buffer_Type)
      return          UTF_Encoding.UTF_8_String is
   begin
      return Result : String (1 .. UTF_8_Length (Buffer)) do
         Read_UTF_8 (Buffer, Result);
         --  We have read the entire buffer; reset it
         Clear (Buffer);
      end return;
   end Get_UTF_8;

   function Wide_Get_UTF_16 (
      Buffer : in out Buffer_Type)
      return          UTF_Encoding.UTF_16_Wide_String is
   begin
      return UTF_Encoding.Conversions.Convert (Get_UTF_8 (Buffer));
   end Wide_Get_UTF_16;

   procedure Get (
      Buffer     : in out Buffer_Type;
      Item       :    out String;
      Last       :    out Natural;
      Substitute :        Character := ' ') is
   begin
      if Buffer.All_7_Bits then
         --  Recognize special case where no conversion needed
         Last := Integer'Min
           (Item'Last, Item'First + Integer (Buffer.Character_Count) - 1);

         Read_UTF_8 (Buffer, Item (Item'First .. Last));
         Buffer.Character_Count :=
           @ - Text_Buffer_Count (Last - Item'First + 1);

      else
         --  Fall back on going through Wide_Character
         declare
            Wide_Wide_Item : Wide_Wide_String (Item'Range);
         begin
            Wide_Wide_Get (Buffer, Wide_Wide_Item, Last);
            Item (Item'First .. Last) :=
              Ada.Characters.Conversions.To_String
                (Wide_Wide_Item (Wide_Wide_Item'First .. Last), Substitute);
         end;
      end if;
   end Get;

   procedure Wide_Wide_Get (
      Buffer : in out Buffer_Type;
      Item   :    out Wide_Wide_String;
      Last   :    out Natural) is

      Item_Len : constant Natural := Item'Length;
      Chars_In_Buf : constant Natural := Natural (Buffer.Character_Count);
   begin
      if Item_Len = 0 or else Chars_In_Buf = 0 then
         --  Nothing to do except set Last
         Last := Item'First - 1;

      elsif Item_Len >= Chars_In_Buf then
         --  Reading everything
         Last := Item'First + Chars_In_Buf - 1;
         Item (Item'First .. Last) :=
           UTF_Encoding.Wide_Wide_Strings.Decode (Get_UTF_8 (Buffer));

      else
         --  Not reading everything; will be completely filling Item
         Last := Item'Last;

         declare
            --  Count how many UTF_8 octets to read
            Num_UTF_8_To_Read : constant Natural :=
              Count_UTF_8_Octets (Buffer, Item_Len);
            --  Create sufficiently large temp
            UTF_8_Chars : UTF_8_Lines (1 .. Num_UTF_8_To_Read);
         begin
            --  Get UTF_8 octets
            Read_UTF_8 (Buffer, UTF_8_Chars);

            --  Decode the UTF_8 string
            Item := UTF_Encoding.Wide_Wide_Strings.Decode (UTF_8_Chars);
         end;
      end if;
   end Wide_Wide_Get;

   function Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Substitution_Function :=
                            Image_Substitution'Access)
      return String is
   begin
      if Buffer.All_7_Bits then
         --  Special case of no substitutions or conversions needed
         return Get_UTF_8 (Buffer);
      else
         --  Get the Wide_Wide_String and walk through it
         declare
            Contents : Wide_Wide_String (1 ..
                                         Natural (Buffer.Character_Count));
            Last : Natural;
            Extra : Natural := 0;
            Substitutions : Boolean := False;
         begin
            --  Get the characters
            Wide_Wide_Get (Buffer, Contents, Last);
            pragma Assert (Last = Contents'Last);

            if not Buffer.All_8_Bits then
               --  Count how many extra characters are needed
               for C of Contents loop
                  if Wide_Wide_Character'Pos (C) >
                    Character'Pos (Character'Last)
                  then
                     Extra := Extra + Substitute (C)'Length - 1;
                     Substitutions := True;
                  end if;
               end loop;
            end if;

            if not Substitutions then
               --  No substitutions to be made
               return Ada.Characters.Conversions.To_String (Contents);
            else
               declare
                  Result : String (1 .. Contents'Last + Extra);
                  I : Positive := Result'First;
               begin
                  for C of Contents loop
                     if Wide_Wide_Character'Pos (C) >
                       Character'Pos (Character'Last)
                     then
                        --  Get the substitution string
                        declare
                           Sub : String renames Substitute (C);
                        begin
                           Result (I .. I + Sub'Length - 1) := Sub;
                           I := I + Sub'Length;
                        end;
                     else
                        Result (I) :=
                          Character'Val (Wide_Wide_Character'Pos (C));
                        I := I + 1;
                     end if;
                  end loop;
                  return Result;
               end;
            end if;
         end;
      end if;
   end Get;

   function Wide_Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Wide_Substitution_Function :=
                            Wide_Image_Substitution'Access)
      return Wide_String is
      Contents : Wide_Wide_String (1 .. Natural (Buffer.Character_Count));
      Last : Natural;
      Extra : Natural := 0;
      Substitutions : Boolean := False;
   begin
      --  Get the characters
      Wide_Wide_Get (Buffer, Contents, Last);
      pragma Assert (Last = Contents'Last);

      --  Count how many extra characters are needed
      for C of Contents loop
         if Wide_Wide_Character'Pos (C) >
           Wide_Character'Pos (Wide_Character'Last)
         then
            Extra := Extra + Substitute (C)'Length - 1;
            Substitutions := True;
         end if;
      end loop;

      if not Substitutions then
         --  No substitutions to be made
         return Ada.Characters.Conversions.To_Wide_String (Contents);
      else
         --  Build up result with substitutions
         declare
            Result : Wide_String (1 .. Contents'Last + Extra);
            I : Positive := Result'First;
         begin
            for C of Contents loop
               if Wide_Wide_Character'Pos (C) >
                 Wide_Character'Pos (Wide_Character'Last)
               then
                  --  Get the substitution string
                  declare
                     Sub : Wide_String renames Substitute (C);
                  begin
                     Result (I .. I + Sub'Length - 1) := Sub;
                     I := I + Sub'Length;
                  end;
               else
                  Result (I) :=
                    Wide_Character'Val (Wide_Wide_Character'Pos (C));
                  I := I + 1;
               end if;
            end loop;
            return Result;
         end;
      end if;
   end Wide_Get;

end Ada2X_Strings.Text_Buffers.Unbounded;
