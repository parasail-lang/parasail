
package Ada2X_Strings.Text_Buffers.Unbounded
   with Preelaborate is  --  , Nonblocking, Global => null is

   type Buffer_Type is new Root_Buffer_Type with private
      with Default_Initial_Condition =>
         Character_Count (Buffer_Type) = 0
         and then Current_Indent (Buffer_Type) = 0
         and then Position_In_Line (Buffer_Type) = 1;

   function Character_Count (Buffer : Buffer_Type)
     return Text_Buffer_Count;

   function Current_Indent (Buffer : Buffer_Type)
     return Text_Buffer_Count;

   function Position_In_Line (Buffer : Buffer_Type)
     return Positive_Text_Buffer_Count;

private

   --  The buffer is implemented as a linked list of chunks.
   --  When the current chunk is full, we allocate a new one.

   --  We have two subtypes of String that are encoded in UTF-8. UTF_8 cannot
   --  contain newline characters; UTF_8_Lines can. Sending UTF_8 data to a
   --  Buffer is more efficient, because end-of-line processing is not needed.
   --  Both of these are more efficient than [[Wide_]Wide_]String, because no
   --  encoding is needed.
   subtype UTF_8_Lines is UTF_Encoding.UTF_8_String with
     Predicate =>
       UTF_Encoding.Wide_Wide_Strings.Encode
         (UTF_Encoding.Wide_Wide_Strings.Decode (UTF_8_Lines)) = UTF_8_Lines;

   subtype UTF_8 is UTF_8_Lines with
     Predicate =>
       (for all I in UTF_8'First .. UTF_8'Last - New_Line_String'Length + 1 =>
        UTF_8 (I .. I + New_Line_String'Length - 1) /= New_Line_String);

   Default_Chunk_Length : constant := 500;

   type Chunk;
   type Chunk_Access is access all Chunk;
   type Chunk (Length : Positive) is limited record
      Next : Chunk_Access := null;
      Chars : UTF_8_Lines (1 .. Length);
   end record;

   type Buffer_Type is
     new Root_Buffer_Type with record
      Character_Count : Text_Buffer_Count := 0;
      Current_Indent  : Text_Buffer_Count := 0;
      Pos_In_Line     : Positive_Text_Buffer_Count := 1;

      Chunk_Length : Positive := Default_Chunk_Length;
      --  This could be a discriminant, but for now we make it fixed

      All_7_Bits : Boolean := True;
      --  For optimization of Text_Output.Buffers.Get (cf).
      --  True if all characters seen so far fit in 7 bits.
      --  7-bit characters are represented the same in Character
      --  and in UTF-8, so they don't need translation.

      All_8_Bits : Boolean := True;
      --  True if all characters seen so far fit in 8 bits.
      --  This is needed in Text_Output.Buffers.Get to distinguish
      --  the case where all characters are Latin-1 (so it should
      --  decode) from the case where some characters are bigger than
      --  8 bits (so the result is implementation defined).

      Cur_Chunk : Chunk_Access;
      --  Points to the chunk we are currently sending characters to.
      --  We want to say:
      --     Cur_Chunk : Chunk_Access := Initial_Chunk'Access;
      --  but that's illegal, so we have some horsing around to do.

      Last : Natural := 0;
      --  Last-used character in Cur_Chunk.all.

      Num_UTF_8_Read : Natural := 0;
      --  Number of UTF_8 octets that have been "consumed" by *Get* routines.

      Initial_Chunk : aliased Chunk (Length => Default_Chunk_Length);
      --  For Buffer, this is the first chunk. Subsequent chunks are allocated
      --  on the heap.

      Num_Extra_Chunks : Natural := 0;
      --  Number of chunks in the list headed by Initial_Chunk.Next.
   end record;

   procedure New_Line (Buffer : in out Buffer_Type);

   procedure Clear (Buffer : in out Buffer_Type);

   procedure Get (
      Buffer     : in out Buffer_Type;
      Item       :    out String;
      Last       :    out Natural;
      Substitute :        Character := ' ');

   procedure Wide_Get (
      Buffer     : in out Buffer_Type;
      Item       :    out Wide_String;
      Last       :    out Natural;
      Substitute : in     Wide_Character := ' ');

   procedure Wide_Wide_Get (
      Buffer : in out Buffer_Type;
      Item   :    out Wide_Wide_String;
      Last   :    out Natural);

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   :        String);

   procedure Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_String);

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_Wide_String);

   procedure Put_UTF_8 (
      Buffer : in out Buffer_Type;
      Item   : in     UTF_Encoding.UTF_8_String);

   procedure Wide_Put_UTF_16 (
      Buffer : in out Buffer_Type;
      Item   : in     UTF_Encoding.UTF_16_Wide_String);

   function Get_UTF_8 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_8_String;

   function Wide_Get_UTF_16 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_16_Wide_String;

   function Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Substitution_Function := Image_Substitution'Access)
      return String;

   function Wide_Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Wide_Substitution_Function :=
                            Wide_Image_Substitution'Access)
      return Wide_String;

   procedure Increase_Indent
     (Buffer : in out Buffer_Type;
      Amount : in     Text_Buffer_Count := Standard_Indent);

   procedure Decrease_Indent
     (Buffer : in out Buffer_Type;
      Amount : in     Text_Buffer_Count := Standard_Indent);

   function Character_Count (Buffer : Buffer_Type)
     return Text_Buffer_Count is (Buffer.Character_Count);

   function Current_Indent (Buffer : Buffer_Type)
     return Text_Buffer_Count is (Buffer.Current_Indent);

   function Position_In_Line (Buffer : Buffer_Type)
     return Positive_Text_Buffer_Count is (Buffer.Pos_In_Line);

end Ada2X_Strings.Text_Buffers.Unbounded;
