pragma Style_Checks(Off);
with Java_Parser, Java_Lexer_io, Java_Lexer, Ada.Text_IO;
use  Java_Parser, Ada.Text_IO;
with Utils.Spellings;
 use Utils.Spellings;
with Utils.Strings;
 use Utils.Strings;
with Utils.Output.Streams.Files;
with ST.Lister_Utils;
with DB.Source_Files;
 use DB.Source_Files;
with Misc;

with Java_Lexer_dfa;

with Ada.Command_Line; use Ada.Command_Line;

procedure Java_Parser.Test is
  in_file_name: string(1..80);
  last        : natural;
  Total_Errors : natural := 0;
  Source_Contents_Ptr : Access_Constant_String;
begin

    for I in 1..Integer'Max(Argument_Count, 1) loop

        if Argument_Count = 0 then
            Ada.Text_IO.put("Enter input file: ");
            Ada.Text_IO.get_line(in_file_name, last);
--            Java_Lexer_io.open_input(in_file_name(1..last));

            -- Remember current file name
            Java_Lexer.Cur_File :=
              Utils.Spellings.Intern(in_file_name(1..Last));
        else
            Ada.Text_IO.Put_Line("Parsing " & Argument(I));
--            Java_Lexer_io.open_input(Argument(I));

            -- Remember current file name
            Java_Lexer.Cur_File :=
              Utils.Spellings.Intern(Argument(I));
        end if;

        Source_Contents_Ptr := ST.Lister_Utils.Read_Source_File(
          FN => To_String
          (DB.Source_Files.File_On_Disk(Java_Lexer.Cur_File)));
        Misc.set_input_buffer(Source_Contents_Ptr);
        Java_Lexer_IO.YY_INPUT := Misc.YY_INPUT'Access;

        put_line("---- Starting parse ----");

        Java_Lexer.Start_New_File;

        -- Java_Lexer_dfa.aflex_debug := True;

        Java_Lexer.lines := 1;

        Java_Lexer.linenum;
        Java_Parser.Num_Annonymous_Classes := 0;
        Utils.Output.Streams.Files.Create(Java_Parser.Listing, "list.txt");
        yyparse;
        Utils.Output.Streams.Files.Close(Java_Parser.Listing);

--        Java_Lexer_io.close_input;

        put_line("---- Finished parse ----");
        new_line;
        put(integer'image(number_of_errors));
        put_line(" errors found");
        Total_Errors := Total_Errors + number_of_errors;
        number_of_errors := 0;
    end loop;

end Java_Parser.Test;
