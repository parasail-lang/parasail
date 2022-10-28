
with Word_Count;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Word_Count is
   Input : constant String := Ada.Command_Line.Argument (1);
   Debug : constant Boolean := Input /= "" and then Input(Input'Last) = '!';

   Str : constant String :=
     (if Input /= "" and then Input /= "!" then Input
      else "This is a test of the amazing word counter");
begin

   Put_Line ("The string """ & Str & """ has " &
     Word_Count (Str, Separators => " ", Debug => Debug)'Image & " words.");
end Test_Word_Count;
