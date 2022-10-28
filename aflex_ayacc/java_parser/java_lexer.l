
%START NORMAL
%x COMMENT


GRAPHIC_CHAR  [ !"#$%&'()*+,-./0-9:;<=>?@A-Z\[\\\]^_`a-z{|}~]

STRING_LITERAL  (\"([^\\\"]|\\.)*\")

CHAR_LITERAL    (\'([^\\\']|\\.)\')

IDENTIFIER        [_a-zA-Z][_a-zA-Z0-9]*

  -- The following are used to match all numeric literals.
  -- Note that double underscores are rejected.
DIGIT_SEQUENCE    [0-9][0-9]*
HEX_SEQUENCE      [0-9a-fA-F]([_]?[0-9a-fA-F])*
EXPONENT          [Ee][-+]?{DIGIT_SEQUENCE}

%%

  -- Java reserved words
"abstract"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (ABSTRACT_tk);
}
"assert"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (ASSERT_tk);
}
"boolean"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (Boolean_tk);
}
"break"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (BREAK_tk);
}
"byte"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (BYTE_tk);
}
"case"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (CASE_tk);
}
"catch"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (CATCH_tk);
}
"class"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (CLASS_tk);
}
"continue"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (CONTINUE_tk);
}
"default"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (DEFAULT_tk);
}
"do"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (DO_tk);
}
"double"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (DOUBLE_tk);
}
"else"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (ELSE_tk);
}
"enum"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (ENUM_tk);
}
"extends"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (EXTENDS_tk);
}
"false"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (BOOLLIT_tk);
}
"final"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (FINAL_tk);
}
"finally"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (FINALLY_tk);
}
"float"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (FLOAT_tk);
}
"for"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (FOR_tk);
}
"future"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (FUTURE_tk);
}
"goto"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (GOTO_tk);
}
"if"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (IF_tk);
}
"implements"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (IMPLEMENTS_tk);
}
"import"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (IMPORT_tk);
}
"instanceof"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (INSTANCEOF_tk);
}
"int"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (INT_tk);
}
"interface"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (INTERFACE_tk);
}
"long"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (LONG_tk);
}
"native"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (NATIVE_tk);
}
"new"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (NEW_tk);
}
"null"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (JNULL_tk);
}
"operator"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (OPERATOR_tk);
}
"package"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (PACKAGE_tk);
}
"private"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (PRIVATE_tk);
}
"protected"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (PROTECTED_tk);
}
"public"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (PUBLIC_tk);
}
"return"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (RETURN_tk);
}
"short"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (SHORT_tk);
}
"static"	{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (STATIC_tk);
}
"super"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (SUPER_tk);
}
"switch"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (SWITCH_tk);
}
"synchronized"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (SYNCHRONIZED_tk);
}
"this"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (THIS_tk);
}
"throw"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (THROW_tk);
}
"throws"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (THROWS_tk);
}
"transient"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (TRANSIENT_tk);
}
"true"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (BOOLLIT_tk);
}
"try"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (TRY_tk);
}
"void"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (VOID_tk);
}
"volatile"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (VOLATILE_tk);
}
"while"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return (WHILE_tk);
}

  -- Match all the compound Java delimiters. 
"=="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_EQ_tk);
}
"!="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_NE_tk);
}
">="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_GE_tk);
}
"<="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_LE_tk);
}
-- NOTE: We now assemble the shift ops in the parser to avoid ambiguities
-- "<<"		{
-- 	ECHO_L;  
-- 	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
-- 	return(OP_SHL_tk);
-- }
-- ">>"		{
-- 	ECHO_L;  
-- 	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
-- 	return(OP_SHR_tk);
-- }
-- ">>>"		{
-- 	ECHO_L;  
-- 	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
-- 	return(OP_SHRR_tk);
-- }
"&&"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_LAND_tk);
}
"||"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_LOR_tk);
}
"--"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_DEC_tk);
}
"++"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_INC_tk);
}
"[]"		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(OP_DIM_tk);
}
"+="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_ADD_tk);
}
"-="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_SUB_tk);
}
"*="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_MUL_tk);
}
"/="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_DIV_tk);
}
"%="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_MOD_tk);
}
"<<="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_SHL_tk);
}
">>="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_SHR_tk);
}
">>>="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_SHRR_tk);
}
"|="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_OR_tk);
}
"&="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_AND_tk);
}
"^="		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(ASGN_XOR_tk);
}

  -- Match all the Java single-character delimiters.
"("        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('(');
}
")"        {
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(')');
}
"["        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('[');
}
"]"        {
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(']');
}
"<"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('<');
}
">"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('>');
}
"{"	   {
	ECHO_L; 	  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('{');
}
"}"	   {
	ECHO_L; 	  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('}');
}
"*"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('*');
}
"+"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('+');
}
","        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(',');
}
"-"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('-');
}
"."        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('.');
}
"/"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('/');
}
"%"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('%');
}
":"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(':');
}
";"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(';');
}
"&"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('&');
}
"|"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('|');
}
"^"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('^');
}
"?"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('?');
}
"="        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('=');
}
"!"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('!');
}
"~"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('~');
}
"@"        {
	ECHO_L;      
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return('@');
}

  -- The following is used to match all valid Java identifiers
  -- except reserved words. 

{IDENTIFIER}       {
	ECHO_L; 
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(IDENTIFIER_tk);
}

  -- Decimal numeric literals
{DIGIT_SEQUENCE}{EXPONENT}?[lLfF]?  {
	ECHO_L;  
	yylval := (Simple_Token, 
	  Cur_Source_Pos, Intern(yytext));
	return(LITERAL_tk);
}

{DIGIT_SEQUENCE}[.]{DIGIT_SEQUENCE}?{EXPONENT}?[fF]?  {
      ECHO_L; 
      yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
      return(LITERAL_tk);
}

[.]{DIGIT_SEQUENCE}{EXPONENT}?[fF]?  {
      ECHO_L; 
      yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
      return(LITERAL_tk);
}

"0"[xX]{HEX_SEQUENCE}[lL]?		{
      ECHO_L;  
      yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
      return(LITERAL_tk);
}

"0"[bB]{DIGIT_SEQUENCE}[lL]?		{
      ECHO_L;  
      yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
      return(LITERAL_tk);
}

  -- Match all valid character literals.

{CHAR_LITERAL}    		{
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(LITERAL_tk);
}

  -- Match all valid string literals.

\"([^\"\\]|\\.)*\" {
	ECHO_L;  
	yylval := (Simple_Token, Cur_Source_Pos, Intern(yytext));
	return(LITERAL_tk);
}

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

  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.

.          {ECHO_L; 
	    yyerror("lexical error: '" &
	      Java_Lexer_dfa.yytext & "'");
}

%%

with Java_Parser_Tokens; 
use  Java_Parser_Tokens;
with Java_Parser;
use Java_Parser;
with Utils.Messages;
use  Utils.Messages;
with Utils.Spellings;
use  Utils.Spellings;
with Ada.Text_IO;
use Ada.Text_IO;
with Utils.Output.Streams;

package Java_Lexer is
  
  lines          : positive := 1;
  Cur_File       : Spelling := No_Spelling;
  Trace          : Boolean := False;

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

end Java_Lexer;

package body Java_Lexer is

  Char_Count : Natural := 0;

  Lex_Buffer : String(1..Max_Buffer_Index);  -- copy of file characters

  Cur_Lex_Index : Lex_Buffer_Index := 0;  -- start of current token
  Last_Lex_Index : Lex_Buffer_Index := 0;  -- end of current token

  Prev_Lex_Index : Lex_Buffer_Index := 1;  -- we start from index 1
      -- How much of file has been copied into listing stream

  procedure ECHO_L is
  --
  -- Local version of the ECHO macro
  -- 
  begin
	-- Add to Lex_Buffer
	Cur_Lex_Index := Last_Lex_Index + 1;
	Last_Lex_Index := Last_Lex_Index + yytext'Length;
        Lex_Buffer(Cur_Lex_Index .. Last_Lex_Index) := yytext;

        if not Trace then
          return;
        end if;

	Ada.Text_IO.put("'");
	for I in yytext'Range loop
	  declare
	    Chr : constant Character := yytext(I);
	  begin
	    case Chr is
	      when ASCII.HT =>
		-- Expand tab
	      declare
		Next_Mult_Of_Eight : constant Natural := Char_Count + 8 -
		  Char_Count mod 8;
	      begin
		for J in Char_Count..Next_Mult_Of_Eight loop
		    Ada.Text_IO.put(' ');
		end loop;
		Char_Count := Next_Mult_Of_Eight;
	      end;
	      when ASCII.LF =>
		-- This must have been a multi-line comment
		Ada.Text_IO.new_line; 
		linenum;
	      when others =>
		Ada.Text_IO.put(yytext(I));
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
	    Ada.Text_IO.new_line; 
	end if;

	linenum;
  end ECHO_NL;

  procedure linenum is
    line_number_string : constant string :=
          integer'image ( lines );
  begin
    lines := lines + 1;
    if Trace then
	put(line_number_string);
	for i in 1 .. 5 - integer ( line_number_string'length ) loop
	  Ada.Text_IO.put(" ");
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

end Java_Lexer;

