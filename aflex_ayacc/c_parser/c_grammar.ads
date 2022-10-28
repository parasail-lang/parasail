pragma Warnings(Off); pragma Style_Checks(Off);
pragma Style_Checks(Off);
with Utils.Output.Streams.Files;
with Utils.Spellings;
with Lists;

package C_Grammar is

    procedure yyparse;
    package Typename_Lists is new Lists(ItemType => Utils.Spellings.Spelling, Equal => Utils.Spellings."=");

    Listing  : Utils.Output.Streams.Files.Unopened_Stream;
    Num_Annonymous_Classes : Integer := 0;
    -- Nesting level for  annonymous inner classes
    Nested_Level  : Integer := 0;
    echo : boolean := false;
    number_of_errors : natural := 0;
    Typename_List : Typename_Lists.List;

    procedure yyerror(s: in string := "syntax error");

end C_Grammar;

