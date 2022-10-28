-- $Revision: $ $Date: $

--  pragma Suppress(Elaboration_Check);

package Simple_Text_IO is
   -- A subset of Text_IO which only provides output to Standard_Output
   -- and to strings.

   subtype Field       is Integer range 0..Integer'Last;
   subtype Number_Base is Integer range 2 .. 16;

   procedure New_Line;

   procedure Put(Item : in  Character);

   procedure Put(Item : in  String);

   procedure Put_Line(Item : in  String);

   pragma Inline(New_Line, Put, Put_Line);

   -- Generic packages for Input-Output of Integer Types

   generic
      type Num is range <>;
      Default_Width : Field := 10; -- Num'Width;
      Default_Base  : Number_Base := 10;
   package Integer_IO is

      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base);

   end Integer_IO;

   generic
      type Num is mod <>;
   package Modular_IO is

      Default_Width : Field := 10; -- Num'Width;
      Default_Base  : Number_Base := 10;

      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base);

   end Modular_IO;

   -- Generic packages for Input-Output of Real Types

   generic
      type Num is digits <>;
      Default_Fore : Field := 2;
      Default_Aft  : Field := 6; -- Num'Digits-1;
      Default_Exp  : Field := 3;
   package Float_IO is

      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);

      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
   end Float_IO;

end Simple_Text_IO;
