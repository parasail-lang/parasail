
pragma Ada_2020;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package Ada2X_Strings.Text_Buffers
   with Pure is  --  , Nonblocking, Global => null is

   type Text_Buffer_Count is range 0 .. Integer'Last;  --  impl-def

   subtype Positive_Text_Buffer_Count is
     Text_Buffer_Count range 1 .. Text_Buffer_Count'Last;

   New_Line_String : constant String := "" & ASCII.LF;  -- impl-def

   New_Line_Count : constant Text_Buffer_Count := New_Line_String'Length;

   Standard_Indent : constant Text_Buffer_Count := 3;
                                    --  Indent used in the Ada standard

   type Root_Buffer_Type is abstract tagged limited private;

   function Character_Count (Buffer : Root_Buffer_Type)
      return Text_Buffer_Count is abstract;

   procedure Clear (Buffer : in out Root_Buffer_Type) is abstract
      with Post'Class => Character_Count (Buffer) = 0
                            and then
                         Current_Indent (Buffer) = 0
                            and then
                         Position_In_Line (Buffer) = 1;

   procedure Get (
      Buffer     : in out Root_Buffer_Type;
      Item       :    out String;
      Last       :    out Natural;
      Substitute :        Character := ' ') is abstract;
--       with Post'Class =>
--          (declare
--             Num_Read : constant Text_Buffer_Count :=
--                Text_Buffer_Count'Min
--                   (Character_Count(Buffer)'Old, Item'Length);
--           begin
--              Last = Num_Read + Item'First - 1 and then
--              Character_Count (Buffer) =
--                Character_Count (Buffer)'Old - Num_Read);

   procedure Wide_Get (
      Buffer     : in out Root_Buffer_Type;
      Item       :    out Wide_String;
      Last       :    out Natural;
      Substitute : in     Wide_Character := ' ') is abstract;
--       with Post'Class =>
--          (declare
--             Num_Read : constant Text_Buffer_Count :=
--                Text_Buffer_Count'Min
--                   (Character_Count(Buffer)'Old, Item'Length);
--           begin
--              Last = Num_Read + Item'First - 1 and then
--              Character_Count (Buffer) =
--                Character_Count (Buffer)'Old - Num_Read);

   procedure Wide_Wide_Get (
      Buffer : in out Root_Buffer_Type;
      Item   :    out Wide_Wide_String;
      Last   :    out Natural) is abstract;
--       with Post'Class =>
--          (declare
--             Num_Read : constant Text_Buffer_Count :=
--                Text_Buffer_Count'Min
--                   (Character_Count(Buffer)'Old, Item'Length);
--           begin
--              Last = Num_Read + Item'First - 1 and then
--              Character_Count (Buffer) =
--                Character_Count (Buffer)'Old - Num_Read);

   procedure Put (
      Buffer : in out Root_Buffer_Type;
      Item   :        String) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
           Character_Count (Buffer)'Old + Item'Length +
             (if Position_In_Line (Buffer)'Old = 1
              then Current_Indent (Buffer) else 0);

   procedure Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   :        Wide_String) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
           Character_Count (Buffer)'Old + Item'Length +
             (if Position_In_Line (Buffer)'Old = 1
              then Current_Indent (Buffer) else 0);

   procedure Wide_Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   :        Wide_Wide_String) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
           Character_Count (Buffer)'Old + Item'Length +
             (if Position_In_Line (Buffer)'Old = 1
              then Current_Indent (Buffer) else 0);

   procedure Put_UTF_8 (
      Buffer : in out Root_Buffer_Type;
      Item   : in     UTF_Encoding.UTF_8_String) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
         Character_Count (Buffer)'Old +
           UTF_Encoding.Wide_Wide_Strings.Decode (Item)'Length +
             (if Position_In_Line (Buffer)'Old = 1
              then Current_Indent (Buffer) else 0);

   procedure Wide_Put_UTF_16 (
      Buffer : in out Root_Buffer_Type;
      Item   : in     UTF_Encoding.UTF_16_Wide_String) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
         Character_Count (Buffer)'Old +
           UTF_Encoding.Wide_Wide_Strings.Decode (Item)'Length +
             (if Position_In_Line (Buffer)'Old = 1
              then Current_Indent (Buffer) else 0);

   function Get_UTF_8 (
      Buffer : in out Root_Buffer_Type)
      return UTF_Encoding.UTF_8_String is abstract
      with Post'Class => Character_Count (Buffer) = 0;

   function Wide_Get_UTF_16 (
      Buffer : in out Root_Buffer_Type)
      return UTF_Encoding.UTF_16_Wide_String is abstract
      with Post'Class => Character_Count (Buffer) = 0;

   type Substitution_Function is
     access function (Item : Wide_Wide_Character) return String;

   type Wide_Substitution_Function is
     access function (Item : Wide_Wide_Character) return Wide_String;

   function Image_Substitution   --  impl-def
     (Item : Wide_Wide_Character) return String;

   function Wide_Image_Substitution  --  impl-def
     (Item : Wide_Wide_Character) return Wide_String;

   function Get (
      Buffer     : in out Root_Buffer_Type;
      Substitute : in     Substitution_Function :=
                            Image_Substitution'Access)
      return String is abstract
      with Post'Class =>
        Get'Result'First = 1 and then Character_Count (Buffer) = 0;

   function Wide_Get (
      Buffer     : in out Root_Buffer_Type;
      Substitute : in     Wide_Substitution_Function :=
                            Wide_Image_Substitution'Access)
      return Wide_String is abstract
      with Post'Class =>
        Wide_Get'Result'First = 1 and then Character_Count (Buffer) = 0;

   function Position_In_Line
     (Buffer : Root_Buffer_Type) return Positive_Text_Buffer_Count is abstract;

   procedure New_Line (Buffer : in out Root_Buffer_Type) is abstract
      with Post'Class =>
         Character_Count (Buffer) =
           Character_Count (Buffer)'Old + New_Line_Count;

   function Current_Indent (Buffer : Root_Buffer_Type)
     return Text_Buffer_Count is abstract;

   procedure Increase_Indent
     (Buffer : in out Root_Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) is abstract
     with Post'Class => Current_Indent (Buffer) =
        Current_Indent (Buffer)'Old + Amount;

   procedure Decrease_Indent
     (Buffer : in out Root_Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) is abstract
     with Pre'Class =>
             Current_Indent (Buffer) >= Amount,
          Post'Class => Current_Indent (Buffer) =
             Current_Indent (Buffer)'Old - Amount;
private

   --  Not defined by the language

   type Root_Buffer_Type is
      abstract tagged limited null record;

end Ada2X_Strings.Text_Buffers;
