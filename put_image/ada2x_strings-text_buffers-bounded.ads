
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
use Ada.Strings;
package Ada2X_Strings.Text_Buffers.Bounded
   with Pure is  --  , Nonblocking, Global => null is

   type Buffer_Type
     (Max_Characters : Text_Buffer_Count)
      is new Root_Buffer_Type with private
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

   procedure New_Line (Buffer : in out Buffer_Type)
      with Pre =>
         Character_Count (Buffer) + New_Line_Count <= Buffer.Max_Characters
         or else raise Constraint_Error;

   procedure Put (
      Buffer : in out Buffer_Type;
      Item   :        String)
      with Pre =>
         Character_Count (Buffer) + Item'Length <= Buffer.Max_Characters
         or else raise Constraint_Error;

   procedure Put_UTF_8 (
      Buffer : in out Buffer_Type;
      Item   :        String)
      with Pre =>
         Character_Count (Buffer) +
           UTF_Encoding.Wide_Wide_Strings.Decode (Item)'Length <=
             Buffer.Max_Characters
         or else raise Constraint_Error;

   procedure Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_String)
      with Pre =>
         Character_Count (Buffer) + Item'Length <= Buffer.Max_Characters
         or else raise Constraint_Error;

   procedure Wide_Put_UTF_16 (
      Buffer : in out Buffer_Type;
      Item   :        UTF_Encoding.UTF_16_Wide_String)
      with Pre =>
         Character_Count (Buffer) +
           UTF_Encoding.Wide_Wide_Strings.Decode (Item)'Length <=
             Buffer.Max_Characters
         or else raise Constraint_Error;

   procedure Wide_Wide_Put (
      Buffer : in out Buffer_Type;
      Item   :        Wide_Wide_String)
      with Pre =>
         Character_Count (Buffer) + Item'Length <= Buffer.Max_Characters
         or else raise Constraint_Error;

private

   type Wide_Wide_Vec is array (Positive_Text_Buffer_Count range <>)
     of Wide_Wide_Character;

   type Buffer_Type
     (Max_Characters : Text_Buffer_Count)
      is new Root_Buffer_Type with record
         Character_Count : Text_Buffer_Count := 0;
         Current_Indent  : Text_Buffer_Count := 0;
         Pos_In_Line     : Positive_Text_Buffer_Count := 1;
         Num_Read        : Text_Buffer_Count := 0;
         Data            : Wide_Wide_Vec (1 .. Max_Characters);
      end record;

   procedure Clear (Buffer : in out Buffer_Type);

   procedure Get (
      Buffer     : in out Buffer_Type;
      Item       :    out String;
      Last       :    out Natural;
      Substitute : in     Character := ' ');

   procedure Wide_Get (
      Buffer     : in out Buffer_Type;
      Item       :    out Wide_String;
      Last       :    out Natural;
      Substitute : in     Wide_Character := ' ');

   procedure Wide_Wide_Get (
      Buffer : in out Buffer_Type;
      Item   :    out Wide_Wide_String;
      Last   :    out Natural);

   function Get_UTF_8 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_8_String;

   function Wide_Get_UTF_16 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_16_Wide_String;

   function Get (
      Buffer     : in out Buffer_Type;
      Substitute : in     Substitution_Function :=
                            Image_Substitution'Access)
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

end Ada2X_Strings.Text_Buffers.Bounded;
