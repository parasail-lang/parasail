with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with LWT.Work_Stealing; use LWT.Work_Stealing;
with Sorting;
procedure Test_Sorting is
    Control : WS_Parallel (Num_Servers => 6, Options => null);
                              --  use 6 server threads
    pragma Unreferenced (Control);

    Input_Len : constant Integer :=
      (if Argument_Count > 0 then Integer'Value (Argument(1)) else 10);
    Len : constant Natural := abs Input_Len;
    Debug : constant Boolean := Input_Len < 0;

    -- For Random
    Mult : constant := 7**5;
    Modulus : constant := 2**31 - 1;
    Ran_Seed : Long_Integer := Long_Integer (Input_Len);

    function Next_Ran return Long_Integer is
    begin
       Ran_Seed := Ran_Seed * Mult mod Modulus;
       return Ran_Seed;
    end Next_Ran;

    type Arr_Of_Int is array(Positive range <>) of Integer;

    package My_Sorter is
      new Sorting(Integer, Positive, Arr_Of_Int, Integer'Image);
    Vec : Arr_Of_Int (1 .. Len) := (others => 0);
    Vec2 : Arr_Of_Int (Vec'Range);
begin

    Put_Line("Seed = " & Len'Image &
      ", Mult = " & Mult'Image & ", Mod = " & Modulus'Image);

    for I in 1..Len loop
       Vec(I) := Integer (Next_Ran mod 100);
    end loop;

    Vec2 := Vec;

    Put_Line("Before sort, Vec = ");
    for I in Vec'Range loop
        Put(" " & Vec(I)'Image);
        if I < Vec'Length then
            Put(",");
            if I mod 10 = 0 then
                Put_Line("");
            end if;
        end if;
    end loop;
    Put_Line("");

    My_Sorter.Quicksort(Vec, Debug);
    pragma Assert((for all I in 1 .. Vec'Length - 1 => Vec(I) <= Vec(I+1)));

    Put_Line("After sort, Vec = ");
    for I in Vec'Range loop
        Put(" " & Vec(I)'Image);
        if I < Vec'Length then
            Put(",");
            if I mod 10 = 0 then
                Put_Line("");
            end if;
        end if;
    end loop;
    Put_Line("");

    My_Sorter.Quicksort(Vec2, Debug => False);

    Put_Line("After 2nd sort, Vec2 = ");
    for I in Vec2'Range loop
        Put(" " & Vec2(I)'Image);
        if I < Vec2'Length then
            Put(",");
            if I mod 10 = 0 then
                Put_Line("");
            end if;
        end if;
    end loop;
    Put_Line("");

end Test_Sorting;

