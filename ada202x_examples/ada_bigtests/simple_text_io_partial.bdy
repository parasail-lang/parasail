package body Simple_Text_IO is
   -- A subset of Text_IO which only provides output to Standard_Output
   -- and to strings.

   package ASCII is
      NUL : constant Character := ' ' - 32;
      LF : constant Character :=  ' ' - 20;
   end ASCII;

   subtype C_String is String;
   pragma Convention(C, C_String);
     -- Convention C ensures bounds are not passed.
   
   subtype Long_Modular is Unsigned_64; -- Long_Integer'Size;
   --  pragma Import(C, Long_Modular, "#include <stdio.h>", "unsigned long");

   procedure printf(Format : C_String)
     with Import, Convention => C, External_Name => "printf";
   procedure printf(Format : C_String; Item : in Character)
     with Import, Convention => C, External_Name => "printf";
   procedure printf(Format : C_String; Len : Integer; Item : in C_String)
     with Import, Convention => C, External_Name => "printf";
   procedure printf(Format : C_String; Width : Field;
     Value : Long_Integer)
     with Import, Convention => C, External_Name => "printf";
   procedure printf(Format : C_String; Width : Field; Value : Long_Modular)
     with Import, Convention => C, External_Name => "printf";
   procedure printf(Format : C_String; Width : Field;
     Aft : Field; Value : Long_Float)
     with Import, Convention => C, External_Name => "printf";

   procedure sprintf(Output : out C_String; Format : C_String; 
     Width : Field; Max_Width : Field; Value : Long_Integer)
     with Import, Convention => C, External_Name => "sprintf";
   procedure sprintf(Output : out C_String; Format : C_String; 
     Width : Field; Max_Width : Field; Value : Long_Modular)
     with Import, Convention => C, External_Name => "sprintf";
   procedure sprintf(Output : out C_String; Format : C_String; Width : Field;
     Aft : Field; Value : Long_Float)
     with Import, Convention => C, External_Name => "sprintf";
 
   New_Line_Format : constant C_String := "" & ASCII.LF & ASCII.NUL;

   procedure New_Line is
   -- begin
      printf(New_Line_Format);
   end New_Line;

   Put_Char_Format : constant C_String := "%c" & ASCII.NUL;

   procedure Put(Item : in  Character) is
   -- begin
      printf(Put_Char_Format, Item);
   end Put;

   Put_Str_Format : constant C_String := "%.*s" & ASCII.NUL;

   procedure Put(Item : in  String) is
   -- begin
      printf(Put_Str_Format, |Item|, C_String'(Item));
   end Put;

   Put_Line_Format : constant C_String := "%.*s" & ASCII.LF & ASCII.NUL;

   procedure Put_Line(Item : in  String) is
   -- begin
      Println
        ("About to call printf with format of " & [[Put_Line_Format]]);
      printf(Put_Line_Format, |Item|, C_String'(Item));
      Println ("Returned from printf");
   end Put_Line;

-- Generic packages for Input-Output of Integer Types

   Base_Not_Supported : exception;
   
   procedure Put_Long_Integer(Item  : in Long_Integer;
		Width : in Field;
		Base  : in Number_Base) is
   -- begin
      case Base is
        when 8 =>
	  printf("%*lo" & ASCII.NUL, Width, Item);
        when 10 =>
	  printf("%*ld" & ASCII.NUL, Width, Item);
        when 16 =>
	  printf("%*lx" & ASCII.NUL, Width, Item);
        when others =>
	  raise Base_Not_Supported;
      end case;
   end Put_Long_Integer;

   procedure Put_Long_Integer(To   : out String;
                 Item : in Long_Integer;
                 Base : in Number_Base) is
      Len : constant Field := |To|; -- 'Length;
   -- begin
      case Base is
        when 8 =>
	  sprintf(C_String'(To), "%*.*lo" & ASCII.NUL, Len, Len, Item);
        when 10 =>
	  sprintf(C_String'(To), "%*.*ld" & ASCII.NUL, Len, Len, Item);
        when 16 =>
	  sprintf(C_String'(To), "%*.*lx" & ASCII.NUL, Len, Len, Item);
        when others =>
	  raise Base_Not_Supported;
      end case;
   end Put_Long_Integer;

   package body Integer_IO is
      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Integer([[Item]], Width, Base);
      end Put;
	    
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Integer(To, [[Item]], Base);
      end Put;

   end Integer_IO;

   procedure Put_Long_Modular(Item  : in Long_Modular;
		 Width : in Field;
		 Base  : in Number_Base) is
   -- begin
      case Base is
        when 8 =>
	  printf("%*lo" & ASCII.NUL, Width, Item);
        when 10 =>
	  printf("%*lu" & ASCII.NUL, Width, Item);
        when 16 =>
	  printf("%*lx" & ASCII.NUL, Width, Item);
        when others =>
	  raise Base_Not_Supported;
      end case;
   end Put_Long_Modular;

   procedure Put_Long_Modular(To   : out String;
		 Item : in Long_Modular;
		 Base : in Number_Base) is
      Len : constant Field := |To|; -- 'Length;
   -- begin
      case Base is
        when 8 =>
	  sprintf(To, "%*.*lo" & ASCII.NUL, Len, Len, Item);
        when 10 =>
	  sprintf(To, "%*.*lu" & ASCII.NUL, Len, Len, Item);
        when 16 =>
	  sprintf(To, "%*.*lx" & ASCII.NUL, Len, Len, Item);
        when others =>
	  raise Base_Not_Supported;
      end case;
   end Put_Long_Modular;

   package body Modular_IO is

      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Modular([[Item]], Width, Base);
      end Put;

      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base) is
      -- begin
	 Put_Long_Modular(To, [[Item]], Base);
      end Put;

   end Modular_IO;

   -- Generic packages for Input-Output of Real Types

   procedure Put_Long_Float(Item : in Long_Float;
		 Fore : in Field;
		 Aft  : in Field;
		 Exp  : in Field) is
   -- begin
      if Exp = 0 then
	 printf("%*.*f" & ASCII.NUL, Fore + Aft + 1, Aft, Item);
      else
	 printf("%*.*e" & ASCII.NUL, Fore + Aft + Exp + 2, Aft, Item);
      end if;
   end Put_Long_Float;
 
   procedure Put_Long_Float(To   : out String;
		 Item : in Long_Float;
		 Aft  : in Field;
		 Exp  : in Field) is
      Len : constant Field := |To|; -- 'Length;
   -- begin
      if Exp = 0 then
	 sprintf(To, "%*.*f" & ASCII.NUL, Len, Aft, Item);
      else
	 sprintf(To, "%*.*e" & ASCII.NUL, Len, Aft, Item);
      end if;
   end Put_Long_Float;

   package body Float_IO is
      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      -- begin
	 Put_Long_Float([[Item]], Fore, Aft, Exp);
      end Put;

      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp) is
      -- begin
	 Put_Long_Float(To, [[Item]], Aft, Exp);
      end Put;
   end Float_IO;

end Simple_Text_IO;
