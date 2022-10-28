
%START NORMAL
%x COMMENT

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
%%
"auto"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(AUTO_tk); }
"break"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(BREAK_tk); }
"case"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CASE_tk); }
"char"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CHAR_tk); }
"const"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONST_tk); }
"continue"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONTINUE_tk); }
"default"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(DEFAULT_tk); }
"do"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(DO_tk); }
"double"		{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(DOUBLE_tk); }
"else"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(ELSE_tk); }
"enum"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(ENUM_tk); }
"extern"		{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(EXTERN_tk); }
"float"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(FLOAT_tk); }
"for"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(FOR_tk); }
"goto"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(GOTO_tk); }
"if"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(IF_tk); }
"int"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(INT_tk); }
"long"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(LONG_tk); }
"register"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(REGISTER_tk); }
"return"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(RETURN_tk); }
"short"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(SHORT_tk); }
"signed"		{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(SIGNED_tk); }
"sizeof"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(SIZEOF_tk); }
"static"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(STATIC_tk); }
"struct"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(STRUCT_tk); }
"switch"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(SWITCH_tk); }
"typedef"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(TYPEDEF_tk); }
"union"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(UNION_tk); }
"unsigned"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(UNSIGNED_tk); }
"void"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(VOID_tk); }
"volatile"		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(VOLATILE_tk); }
"while"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(WHILE_tk); }
{L}({L}|{D})*		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          if Typename_Lists.IsInList(Typename_List, Intern(yytext)) then
                            return (TYPE_NAME_tk);
                          end if;
                          return(IDENTIFIER_tk); }
0[xX]{H}+{IS}?		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
0{D}+{IS}?		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
{D}+{IS}?		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
L?'(\\.|[^\\'])+'	{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
{D}+{E}{FS}?		{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
{D}*"."{D}+({E})?{FS}?	{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
{D}+"."{D}*({E})?{FS}?	{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(CONSTANT_tk); }
L?\"(\\.|[^\\"])*\"	{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(STRING_LITERAL); }
"..."			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(ELLIPSIS_tk); }
">>="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(RIGHT_ASSIGN_tk); }
"<<="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(LEFT_ASSIGN_tk); }
"+="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(ADD_ASSIGN_tk); }
"-="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(SUB_ASSIGN_tk); }
"*="			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(MUL_ASSIGN_tk); }
"/="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(DIV_ASSIGN_tk); }
"%="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(MOD_ASSIGN_tk); }
"&="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(AND_ASSIGN_tk); }
"^="			{ ECHO_L;
                          return(XOR_ASSIGN_tk); }
"|="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(OR_ASSIGN_tk); }
">>"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(RIGHT_OP_tk); }
"<<"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(LEFT_OP_tk); }
"++"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(INC_OP_tk); }
"--"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(DEC_OP_tk); }
"->"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(PTR_OP_tk); }
"&&"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(AND_OP_tk); }
"||"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(OR_OP_tk); }
"<="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(LE_OP_tk); }
">="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(GE_OP_tk); }
"=="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(EQ_OP_tk); }
"!="			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(NE_OP_tk); }
";"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(';'); }
("{"|"<%")		{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('{'); }
("}"|"%>")		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('}'); }
","			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(','); }
":"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(':'); }
"="			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('='); }
"("			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('('); }
")"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(')'); }
("["|"<:")		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('['); }
("]"|":>")		{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return(']'); }
"."			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('.'); }
"&"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('&'); }
"!"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('!'); }
"~"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('~'); }
"-"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('-'); }
"+"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('+'); }
"*"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('*'); }
"/"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('/'); }
"%"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('%'); }
"<"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('<'); }
">"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('>'); }
"^"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('^'); }
"|"			{ ECHO_L;
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('|'); }
"?"			{ ECHO_L; 
                          yylval := (Simple_Token, Cur_Source_Pos,Intern(yytext));
                          return('?'); }
"//".*    {ECHO_L;}           -- ignore comments to end-of-line

"/*"	{ECHO_L; ENTER(COMMENT);}  -- ignore start of comment

<COMMENT>"*/" {ECHO_L; ENTER(NORMAL);}  -- ignore end of comment

<COMMENT>[^*]*   {ECHO_L;}  -- ignore internal part of comment
<COMMENT>\*\**/[^/]  {ECHO_L;}  -- ignore internal part of comment
<COMMENT>[\r]?[\n]       {
	ECHO_NL;
}  -- ignore NL in comment

  -- The following matches all whitespace.  Except for vertical tabs.  AFLEX,
  -- ALEX and LEX does not support vertical tabs.  This causes us to fail
  -- when parsing some of the ACVC Btests.

[ \r\t\f]+ {ECHO_L;}        -- ignore spaces,Carriage returns,tabs,form feeds

  -- The following matches all new lines.

[\n]       {
	ECHO_NL;
}

[ \t\v\n\f]		{ ECHO_L; }

  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.

.          {ECHO_L; 
	    yyerror("lexical error: '" &
	      C_Lexer_dfa.yytext & "'");
}


%%
with C_Grammar_Tokens; 
use  C_Grammar_Tokens;
with C_Grammar;
use C_Grammar;
with Utils.Messages;
use  Utils.Messages;
with Utils.Spellings;
use  Utils.Spellings;
with Ada.Text_IO;
use Ada.Text_IO;
with Utils.Output.Streams;
with Lists;

package C_Lexer is
  
  lines          : positive := 1;
  Cur_File       : Spelling := No_Spelling;
  Trace          : Boolean := True;

  procedure ECHO_L;  -- Local version of the "ECHO" macro
  procedure linenum; 

  function yylex return token;

  function Cur_Source_Pos return Lex_Source_Position;

  procedure Flush_Lex_Buffer(
    Listing : in out Utils.Output.Streams.Stream'Class;
    Up_To : Lex_Buffer_Index);
    -- Flush out text to listing file upto given position

  procedure Flush_All_Lex_Buffer (
    Listing : in out Utils.Output.Streams.Stream'Class);
    -- Flush out whole buffer to listing file

  procedure Start_New_File;
    -- Called when new text file is opened for lexing

  procedure Comment;
   -- Called when you start a C comment
end C_Lexer;

package body C_Lexer is

  Char_Count : Natural := 0;

  Lex_Buffer : String(1..Max_Buffer_Index);  -- copy of file characters

  Cur_Lex_Index : Lex_Buffer_Index := 0;  -- start of current token
  Last_Lex_Index : Lex_Buffer_Index := 0;  -- end of current token

  Prev_Lex_Index : Lex_Buffer_Index := 1;  -- we start from index 1
      -- How much of file has been copied into listing stream


  procedure Comment is
    c,c1 : Character;
    Line_Break : Boolean;
  begin
   <<label1>>
     Ada.Text_IO.Get(c);
     while  (c /= '*' and c /= ASCII.NUL) loop
       Ada.Text_IO.Put(c);
       Ada.Text_IO.Get(c);
     end loop;

      Ada.Text_IO.Look_Ahead(c1,Line_Break);
      if ( c1 /= '/' and c1 /= ASCII.NUL) then
        goto label1; 
      end if;

    if (c /= ASCII.NUL) then
      Ada.Text_IO.Put(c); 
    end if;

  end Comment;

  procedure ECHO_L is
  --
  -- Local version of the ECHO macro
  -- 
  begin
	-- Add to Lex_Buffer
	Cur_Lex_Index := Last_Lex_Index + 1;
	Last_Lex_Index := Last_Lex_Index + YYText'Length;
        Lex_Buffer(Cur_Lex_Index .. Last_Lex_Index) := YYText;

        if not Trace then
          return;
        end if;

	Ada.Text_IO.Put("'");
	for I in YYText'Range loop
	  declare
	    Chr : constant Character := YYText(I);
	  begin
	    case Chr is
	      when ASCII.HT =>
		-- Expand tab
	      declare
		Next_Mult_Of_Eight : constant Natural := Char_Count + 8 -
		  Char_Count mod 8;
	      begin
		for J in Char_Count..Next_Mult_Of_Eight loop
		    Ada.Text_IO.Put(' ');
		end loop;
		Char_Count := Next_Mult_Of_Eight;
	      end;
	      when ASCII.LF =>
		-- This must have been a multi-line comment
		Ada.Text_IO.New_Line; 
		linenum;
	      when others =>
		Ada.Text_IO.Put(yytext(I));
		Char_Count := Char_Count + 1;
	    end case;
	  end;
	end loop;
  end ECHO_L;

  procedure ECHO_NL is
    -- We just processed a new-line token
  begin
	-- Add newline to Lex_Buffer
	Cur_Lex_Index := Last_Lex_Index + 1;
	Last_Lex_Index := Cur_Lex_Index;
        Lex_Buffer(Cur_Lex_Index) := ASCII.LF;

	if Trace then
	    Ada.Text_IO.New_Line; 
	end if;

	linenum;
  end ECHO_NL;

  procedure linenum is
    line_number_string : constant String :=
          Integer'image ( lines );
  begin
    lines := lines + 1;
    if Trace then
	Put(line_number_string);
	for i in 1 .. 5 - integer ( line_number_string'length ) loop
	  Ada.Text_IO.Put(" ");
	end loop;
    end if;
    Char_Count := 0;

  end linenum;

  function Cur_Source_Pos return Lex_Source_Position is
     Result : constant Lex_Source_Position :=
	(MSP => (Line => Line_Number(lines),  
	  Column => Unknown_Column_Number), -- Column is TBD
         File_Name => Cur_File,    -- record the File_Name
	 Lex_Index => Cur_Lex_Index ,
         Lex_End_Index => Last_Lex_Index);
  begin
    return Result;
  end Cur_Source_Pos;

  procedure Flush_Lex_Buffer(
    Listing : in out Utils.Output.Streams.Stream'Class;
    Up_To : Lex_Buffer_Index) is
    -- Flush out text to listing file
  begin
    -- Put out next chunk of source to Listing stream
    Utils.Output.Streams.Put(Listing, Lex_Buffer(Prev_Lex_Index .. Up_To-1));

    -- Remember how much we put out
    Prev_Lex_Index := Up_To;
  end Flush_Lex_Buffer;

  procedure Flush_All_Lex_Buffer( 
    Listing : in out Utils.Output.Streams.Stream'Class ) is
    -- Flush out whole buffer to listing file
  begin
    -- Flush out the whole buffer to Listing stream
    Utils.Output.Streams.Put(Listing, Lex_Buffer(Prev_Lex_Index .. Last_Lex_Index));

    -- Remember how much we put out
    Prev_Lex_Index := Last_Lex_Index + 1;
  end Flush_All_Lex_Buffer;

  procedure Start_New_File is
    -- Called when new text file is opened for lexing
  begin
    -- Reinitialize all of the global indices into Lex_Buffer
    Cur_Lex_Index := 0;
    Last_Lex_Index := 0;
    Prev_Lex_Index := 1;    -- we are writing from index 1
  end Start_New_File;
    
##

end C_Lexer;


