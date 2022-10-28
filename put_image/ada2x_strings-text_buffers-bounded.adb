
with Ada.Characters.Conversions;
package body Ada2X_Strings.Text_Buffers.Bounded is

   Wide_Wide_NL : constant Wide_Wide_String :=
     (1 => Wide_Wide_Character'Val (Character'Pos (ASCII.LF)));

   procedure Clear (Buffer : in out Buffer_Type) is
   begin
      Buffer.Character_Count := 0;
   end Clear;

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   :        String) is
   begin
      Wide_Wide_Put
        (Buffer, Ada.Characters.Conversions.To_Wide_Wide_String (Item));
   end Put;

   procedure Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_String) is
   begin
      Wide_Wide_Put
        (Buffer, Ada.Characters.Conversions.To_Wide_Wide_String (Item));
   end Wide_Put;

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_Wide_String) is
      Item_Len : constant Text_Buffer_Count := Item'Length;
      Cur_Char_Count : constant Text_Buffer_Count := Buffer.Character_Count;
      New_Char_Count : constant Text_Buffer_Count :=
        Cur_Char_Count + Item_Len;
      Num_Read : constant Text_Buffer_Count := Buffer.Num_Read;
      Cur_Indent : constant Text_Buffer_Count := Buffer.Current_Indent;
   begin
      if Num_Read > 0 then
         --  We have some characters in the buffer that have been read
         --  Move the remaining unread characters to the head of the buffer.
         Buffer.Data (1 .. Cur_Char_Count) :=
           Buffer.Data (Num_Read + 1 .. Num_Read + Cur_Char_Count);

         --  Reset Num_Read to zero to indicate none of the characters
         --  in the buffer have been read.
         Buffer.Num_Read := 0;
      end if;

      if Cur_Indent > 0
        and then Buffer.Pos_In_Line = 1
        and then Item_Len > 0
        and then Item /= Wide_Wide_NL
      then
         --  We need to insert the indentation
         --  NOTE: Might raise Constraint_Error
         --        if Current_Indent puts us over the top
         for I in 1 .. Cur_Indent loop
            Buffer.Data (Cur_Char_Count + I) := ' ';
         end loop;
         Buffer.Data (Cur_Char_Count + Cur_Indent + 1 ..
           New_Char_Count + Cur_Indent) := Wide_Wide_Vec (Item);
         Buffer.Character_Count := New_Char_Count + Cur_Indent;
      else
         --  Add to end of buffer, and set new count
         Buffer.Data (Cur_Char_Count + 1 .. New_Char_Count) :=
           Wide_Wide_Vec (Item);
         Buffer.Character_Count := New_Char_Count;
      end if;
      Buffer.Pos_In_Line := Buffer.Pos_In_Line +
        (Buffer.Character_Count - Cur_Char_Count);
   end Wide_Wide_Put;

   procedure New_Line (Buffer : in out Buffer_Type) is
   begin
      Wide_Wide_Put (Buffer, Wide_Wide_NL);
      --  Remember most recent new-line
      Buffer.Pos_In_Line := 1;
   end New_Line;

   procedure Get (
      Buffer     : in out Buffer_Type;
      Item       :    out String;
      Last       :    out Natural;
      Substitute :        Character := ' ') is
      Wide_Wide_Item : Wide_Wide_String (Item'Range);
   begin
      Wide_Wide_Get (Buffer, Wide_Wide_Item, Last);
      Item (Item'First .. Last) :=
        Ada.Characters.Conversions.To_String
          (Wide_Wide_Item (Wide_Wide_Item'First .. Last), Substitute);
   end Get;

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

   function Get_UTF_8 (
      Buffer : in out Buffer_Type)
      return          UTF_Encoding.UTF_8_String
   is
      Result : Wide_Wide_String (1 .. Natural (Buffer.Character_Count));
      Last : Natural;
   begin
      Wide_Wide_Get (Buffer, Result, Last);
      pragma Assert (Last = Result'Last);
      return UTF_Encoding.Wide_Wide_Strings.Encode (Result);
   end Get_UTF_8;

   function Wide_Get_UTF_16 (
      Buffer : in out Buffer_Type)
      return          UTF_Encoding.UTF_16_Wide_String
   is
      Result : Wide_Wide_String (1 .. Natural (Buffer.Character_Count));
      Last : Natural;
   begin
      Wide_Wide_Get (Buffer, Result, Last);
      pragma Assert (Last = Result'Last);
      return UTF_Encoding.Wide_Wide_Strings.Encode (Result);
   end Wide_Get_UTF_16;

   procedure Put_UTF_8 (
      Buffer : in out Buffer_Type;
      Item   :        UTF_Encoding.UTF_8_String) is
   begin
      Wide_Wide_Put
        (Buffer,
         UTF_Encoding.Wide_Wide_Strings.Decode (Item));
   end Put_UTF_8;

   procedure Wide_Put_UTF_16 (
      Buffer : in out Buffer_Type;
      Item   :        UTF_Encoding.UTF_16_Wide_String) is
   begin
      Wide_Wide_Put
        (Buffer,
         UTF_Encoding.Wide_Wide_Strings.Decode (Item));
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

   procedure Wide_Wide_Get (
      Buffer : in out Buffer_Type;
      Item   :    out Wide_Wide_String;
      Last   :    out Natural) is
      Num_Read : constant Text_Buffer_Count := Buffer.Num_Read;
      Cur_Char_Count : constant Text_Buffer_Count := Buffer.Character_Count;
      Amount_To_Read : constant Text_Buffer_Count :=
        Text_Buffer_Count'Min (Cur_Char_Count, Item'Length);
      New_Num_Read : constant Text_Buffer_Count :=
        Num_Read + Amount_To_Read;
      New_Char_Count : constant Text_Buffer_Count :=
        Cur_Char_Count - Amount_To_Read;
   begin
      Item := Wide_Wide_String (Buffer.Data (Num_Read + 1 .. New_Num_Read));
      Last := Item'First + Integer (Amount_To_Read - 1);
      Buffer.Character_Count := New_Char_Count;

      if New_Char_Count = 0 then
         --  Buffer is empty, reset Num_Read.
         Buffer.Num_Read := 0;
      else
         --  Buffer still has some unread characters in it
         Buffer.Num_Read := New_Num_Read;
      end if;
   end Wide_Wide_Get;

   function Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Substitution_Function :=
                            Image_Substitution'Access)
      return String is
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
         if Wide_Wide_Character'Pos (C) > Character'Pos (Character'Last) then
            Extra := Extra + Substitute (C)'Length - 1;
            Substitutions := True;
         end if;
      end loop;

      if not Substitutions then
         --  No substitutions to be made
         return Ada.Characters.Conversions.To_String (Contents);
      else
         declare
            Result : String (1 .. Contents'Last + Extra);
            I : Positive := Result'First;
         begin
            for C of Contents loop
               if Wide_Wide_Character'Pos (C) > Character'Pos (Character'Last)
               then
                  --  Get the substitution string
                  declare
                     Sub : String renames Substitute (C);
                  begin
                     Result (I .. I + Sub'Length - 1) := Sub;
                     I := I + Sub'Length;
                  end;
               else
                  Result (I) := Character'Val (Wide_Wide_Character'Pos (C));
                  I := I + 1;
               end if;
            end loop;
            return Result;
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

end Ada2X_Strings.Text_Buffers.Bounded;
