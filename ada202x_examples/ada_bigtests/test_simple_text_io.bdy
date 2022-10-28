with Simple_Text_IO;
procedure Test_Simple_Text_IO is
-- begin
   Simple_Text_IO.Put_Line("This is a test");
   type My_Int is range -30 .. +30;
   package Int_IO is new Simple_Text_IO.Integer_IO (My_Int);
   Int_IO.Put (17, Base => 16);
   Simple_Text_IO.New_Line;
end Test_Simple_Text_IO;
