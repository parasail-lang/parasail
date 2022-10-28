package body Simple_Text_IO is
   -- A subset of Text_IO which only provides output to Standard_Output
   -- and to strings.

   package ASCII is
      NUL : constant Character := ' ' - 32;
      LF : constant Character :=  ' ' - 20;
   end ASCII;

   function Trim (S : Univ_String; Width : Natural := 0) return Univ_String is
      Trimmed : Univ_String;

      if S[S'First] = ' ' then
         Trimmed := S[S'First+1 .. S'Last];
      else
         Trimmed := S;
      end if;
      if Trimmed'Length >= Width then
         return Trimmed;
      else
         return ' ' * ([[Width]] - Trimmed'Length) & Trimmed;
      end if;
   end Trim;

   procedure New_Line is
   -- begin
      Univ_String.Put_Line("");
   end New_Line;

   procedure Put(Item : in  Character) is
   -- begin
      Univ_Character.Put([[Item]]);
   end Put;

   procedure Put(Item : in  String) is
   -- begin
      Univ_String.Put([[Item]]);
   end Put;

   procedure Put_Line(Item : in  String) is
   -- begin
      Univ_String.Put_Line([[Item]]);
   end Put_Line;

-- Generic packages for Input-Output of Integer Types

   Base_Not_Supported : exception;
   
   procedure Put_Long_Integer(Item  : in Long_Integer;
		Width : in Field;
		Base  : in Number_Base) is
   -- begin
      if Base /= 10 then
         Univ_String.Put (Trim (Base'Image) & '#');
         --  ** TBD **
         Univ_String.Put (Trim (Item'Image) & '#');
      else
         Univ_String.Put (Trim (Item'Image));
      end if;
   end Put_Long_Integer;

   procedure Put_Long_Integer(To   : out String;
                 Item : in Long_Integer;
                 Base : in Number_Base) is
      Len : constant Field := To'Length;
      Result : Univ_String;
   -- begin
      if Base /= 10 then
         --  ** TBD **
         Result :=
           Trim (Base'Image) & '#' & Trim (Item'Image) & '#';
      else
         Result := Trim (Item'Image);
      end if;
      if Result'Length > Len then
         To := Len * '*';
      else
         To := Trim (Result, Len);
      end if;
   end Put_Long_Integer;

   package body Integer_IO is
      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Integer(Long_Integer'([[Item]]), Width, Base);
      end Put;
	    
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Integer(To, Long_Integer'([[Item]]), Base);
      end Put;

   end Integer_IO;

   subtype Long_Modular is Unsigned_64;

   procedure Put_Long_Modular(Item  : in Long_Modular;
		 Width : in Field;
		 Base  : in Number_Base) is
   -- begin
      if Base /= 10 then
         Univ_String.Put (Trim (Base'Image) & '#');
         --  ** TBD **
         Univ_String.Put (Trim (Item'Image) & '#');
      else
         Univ_String.Put (Trim (Item'Image));
      end if;
   end Put_Long_Modular;

   procedure Put_Long_Modular(To   : out String;
		 Item : in Long_Modular;
		 Base : in Number_Base) is
      Len : constant Field := To'Length;
      Result : Univ_String;
   -- begin
      if Base /= 10 then
         --  ** TBD **
         Result :=
           Trim (Base'Image) & '#' & Trim (Item'Image) & '#';
      else
         Result := Trim (Item'Image);
      end if;
      if Result'Length > Len then
         To := '*' * Len;
      else
         To := Trim (Result, Len);
      end if;
   end Put_Long_Modular;

   package body Modular_IO is

      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Modular(Long_Modular'([[Item]]), Width, Base);
      end Put;

      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Modular(To, Long_Modular'([[Item]]), Base);
      end Put;

   end Modular_IO;

   -- Generic packages for Input-Output of Real Types

   procedure Put_Long_Float(Item : in Long_Float;
		 Fore : in Field;
		 Aft  : in Field;
		 Exp  : in Field) is
   -- begin
      --  ** TBD **
      Univ_String.Put (Trim (Item'Image));
   end Put_Long_Float;
 
   procedure Put_Long_Float(To   : out String;
		 Item : in Long_Float;
		 Aft  : in Field;
		 Exp  : in Field) is
      Len : constant Field := To'Length;
      Result : Univ_String;
   -- begin
      --  ** TBD **
      Result := Trim (Item'Image);
      if Result'Length > Len then
         To := '*' * Len;
      else
         To := Trim (Result, Len);
      end if;
   end Put_Long_Float;

   package body Float_IO is
      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      -- begin
	 Put_Long_Float(Long_Float'([[Item]]), Fore, Aft, Exp);
      end Put;

      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      -- begin
	 Put_Long_Float(To, Long_Float'([[Item]]), Aft, Exp);
      end Put;
   end Float_IO;

end Simple_Text_IO;
