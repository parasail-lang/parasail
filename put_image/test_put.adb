with Ada.Text_IO;
with Ada2X_Strings.Text_Buffers.Bounded;
with Ada2X_Strings.Text_Buffers.Unbounded;
use Ada2X_Strings.Text_Buffers;
with System_Put_Images; use System_Put_Images;
procedure Test_Put is

   procedure Test_One_Buf (My_Buf : in out Root_Buffer_Type'Class) is
      X : constant Thin_Pointer := new Byte'('X');
      Y : constant Fat_Pointer := new Byte_String'("Fat Object");
   begin
      
      Put (My_Buf, "Signed Integers:");
      New_Line (My_Buf);
      Put_Image_Integer (My_Buf, -42);
      Put (My_Buf, ", ");
      Put_Image_Long_Long_Integer (My_Buf, -4422E15);
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "Unsigned Integers:");
      New_Line (My_Buf);
      Put_Image_Unsigned (My_Buf, 16#DEAF#);
      Put (My_Buf, ", ");
      Put_Image_Long_Long_Unsigned
        (My_Buf, 16#DEAD_BEEF_DAD4_FADE#);
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "Thin and Fat Pointers:");
      New_Line (My_Buf);
      Put_Image_Thin_Pointer (My_Buf, X);
      Put (My_Buf, ", ");
      Put_Image_Fat_Pointer (My_Buf, Y);
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "Acc-to-Subp, unprot and prot:");
      New_Line (My_Buf);
      Put_Image_Access_Subp (My_Buf, X);
      --  For access-to-subprogram types
      Put (My_Buf, ", ");
      Put_Image_Access_Prot_Subp (My_Buf, X);
      --  For access-to-protected-subprogram types
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "String Images:");
      New_Line (My_Buf);
      Put_Image_String (My_Buf, "Normal String");
      Put (My_Buf, ", ");
      Put_Image_Wide_String (My_Buf, "Wide_String");
      Put (My_Buf, ", ");
      Put_Image_Wide_Wide_String (My_Buf, "Wide_Wide_String");
      New_Line (My_Buf);
      
      New_Line (My_Buf);
      Put (My_Buf, "Array stuff:");
      New_Line (My_Buf);
      Put (My_Buf, "Before: '");
      Array_Before (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);
      Put (My_Buf, "Between: '");
      Array_Between (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);
      Put (My_Buf, "After: '");
      Array_After (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);

      Put (My_Buf, "Simple_Between: '");
      Simple_Array_Between (My_Buf);
      Put (My_Buf, "'");
      --  For "simple" arrays, where we don't want a newline between every
      --  component.
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "Record stuff: ");
      New_Line (My_Buf);
      Put (My_Buf, "Before: '");
      Record_Before (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);
      Put (My_Buf, "Between: '");
      Record_Between (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);
      Put (My_Buf, ", After: '");
      Record_After (My_Buf);
      Put (My_Buf, "'");
      New_Line (My_Buf);

      New_Line (My_Buf);
      Put (My_Buf, "Unknown_Image: ");
      Put_Image_Unknown (My_Buf, "Unknown Image");
      --  For Put_Image of types that don't have the attribute, such as type
      --  Sink.
      New_Line (My_Buf);

      Ada.Text_IO.Put_Line ("Content: " &
        Get_UTF_8 (My_Buf));
   end Test_One_Buf;

   Bounded_Buf : Bounded.Buffer_Type
     (Max_Characters => 2000);
   Unbounded_Buf : Unbounded.Buffer_Type;

begin

   Ada.Text_IO.Put_Line ("Bounded buffer test:");
   Test_One_Buf (Bounded_Buf);

   Ada.Text_IO.Put_Line ("=================================");
   Ada.Text_IO.Put_Line ("Unbounded buffer test:");
   Test_One_Buf (Unbounded_Buf);
end Test_Put;
