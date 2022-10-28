with Ada.Characters.Conversions;
package body Ada2X_Strings.Text_Buffers is
   --  Standard substitutions for extended characters

   function Hex (Item : Wide_Wide_Character; Size : Positive) return String is
      Result : String (1 .. Size) := (others => '0');
      type U32 is mod 2**32;
      Pos : U32 := Wide_Wide_Character'Pos (Item);
      Hex_Digit : constant array (U32 range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7',
         '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
   begin
      for I in reverse 1 .. Size loop
         exit when Pos = 0;
         Result (I) := Hex_Digit (Pos mod 16);
         Pos := Pos / 16;
      end loop;
      return Result;
   end Hex;

   function Image_Substitution
     (Item : Wide_Wide_Character) return String is
   begin
      return "[""" & Hex (Item, 4) & """]";
   end Image_Substitution;

   function Wide_Image_Substitution
     (Item : Wide_Wide_Character) return Wide_String is
   begin
      return "[""" &
                Ada.Characters.Conversions.To_Wide_String (Hex (Item, 8)) &
             """]";
   end Wide_Image_Substitution;

end Ada2X_Strings.Text_Buffers;
