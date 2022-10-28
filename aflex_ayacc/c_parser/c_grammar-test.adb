pragma Warnings(Off); pragma Style_Checks(Off);
pragma Style_Checks(Off);
with C_Grammar, C_Lexer_io, C_Lexer, Ada.Text_IO;
use  C_Grammar, Ada.Text_IO;
with Utils.Spellings;
 use Utils.Spellings;
with Utils.Strings;
 use Utils.Strings;
with Utils.Output.Streams.Files;
with ST.Lister_Utils;
with DB.Source_Files;
 use DB.Source_Files;
with C_Misc;

with C_Lexer_dfa;

with Ada.Command_Line; use Ada.Command_Line;

procedure C_Grammar.Test is
  in_file_name: string(1..80);
  last        : natural;
  Total_Errors : natural := 0;
  Source_Contents_Ptr : Access_Constant_String;
begin

    for I in 1..Integer'Max(Argument_Count, 1) loop

        if Argument_Count = 0 then
            Ada.Text_IO.put("Enter input file: ");
            Ada.Text_IO.get_line(in_file_name, last);
--            C_Lexer_io.open_input(in_file_name(1..last));

            -- Remember current file name
            C_Lexer.Cur_File :=
              Utils.Spellings.Intern(in_file_name(1..Last));
        else
            Ada.Text_IO.Put_Line("Parsing " & Argument(I));
--            C_Lexer_io.open_input(Argument(I));

            -- Remember current file name
            C_Lexer.Cur_File :=
              Utils.Spellings.Intern(Argument(I));
        end if;

        Source_Contents_Ptr := ST.Lister_Utils.Read_Source_File(
          FN => To_String
          (DB.Source_Files.File_On_Disk(C_Lexer.Cur_File)));
        C_Misc.set_input_buffer(Source_Contents_Ptr);
        C_Lexer_IO.YY_INPUT := C_Misc.Ptr;

        put_line("---- Starting parse ----");

        C_Lexer.Start_New_File;

        -- C_Lexer_dfa.aflex_debug := True;

        C_Lexer.lines := 1;

        C_Lexer.linenum;
        C_Grammar.Num_Annonymous_Classes := 0;
        Utils.Output.Streams.Files.Create(C_Grammar.Listing, "list.txt");
        yyparse;
        Utils.Output.Streams.Files.Close(C_Grammar.Listing);

--        C_Lexer_io.close_input;

        put_line("---- Finished parse ----");
        new_line;
        put(integer'image(number_of_errors));
        put_line(" errors found");
        Total_Errors := Total_Errors + number_of_errors;
        number_of_errors := 0;
    end loop;

end C_Grammar.Test;
