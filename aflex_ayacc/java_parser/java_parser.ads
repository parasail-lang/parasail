pragma Style_Checks(Off);
with Utils.Output.Streams.Files;

package Java_Parser is

    procedure yyparse;

    Listing  : Utils.Output.Streams.Files.Unopened_Stream;
    Num_Annonymous_Classes : Integer := 0;
    -- Nesting level for  annonymous inner classes
    Nested_Level  : Integer := 0;
    echo : boolean := false;
    number_of_errors : natural := 0;

    procedure yyerror(s: in string := "syntax error");

end Java_Parser;

