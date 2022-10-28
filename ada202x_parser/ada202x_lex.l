------------------------------------------------------------------------------
--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------


--  These start states determine whether an apostrophe is interpreted
--  as a "tick" (aka "prime") or as a character literal.
%START TICK CHARLIT

--  These start states are associated with indentation.
--  RESCANNING state is used to avoid endlessly re-recognizing the pattern
--  designed to ignore "begin" or "exception" that occurs at the very
--  beginning of a line and erroneously returning NEW_LINE repeatedly.
--  Instead, when in RESCANNING state, we immediately return the next token
--  whatever it is.
--  OUTDENTING state is used when we see that the indentation level has
--  shrunk, and we need to emit one or more OUTDENT tokens followed by
--  a NEW_LINE token.
%x OUTDENTING RESCANNING


GRAPHIC_CHAR  [ !"#$%&'()*+,-./0-9:;<=>?@A-Z\[\\\]^_`a-z{|}~]

STRING_LITERAL  (\"([^\"\n]|\"\")*[\"\n])

CHAR_LITERAL    (\'.\'|\'\\.\')
--  Catch attempts to use a backslash inside a character
--  Will report the problem 

IDENTIFIER        [a-zA-Z]([_a-zA-Z0-9])*

  -- The following are used to match all numeric literals.
  -- Note that double underscores are rejected.
DIGIT_SEQUENCE    [0-9]([_]?[0-9])*
HEX_SEQUENCE      [0-9a-fA-F]([_]?[0-9a-fA-F])*
EXPONENT          [Ee][-+]?{DIGIT_SEQUENCE}

%%

^[ \r\t\f]+/"is"[ \r\t\f\n] {
        -- ignore "is" at start of line as far as indenting
        ECHO_L; 
}

^[ \r\t\f]*("begin"|"exception")/[ \r\t\f\n] {
        -- ignore "begin", "exception", etc. at start of line as far as
        -- indenting but return a NEWLINE if not expecting an INDENT
        -- otherwise return an INDENT.
    declare
        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        for I in reverse YYT'Range loop
           if YYT(I) <= ' ' then
              --  Echo the spaces at the front of the line
              ECHO_L(YYT(YYT'First .. I));
              exit;
           end if;
           --  Unput the non-space characters
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           --  Next time, return the token
           ENTER(RESCANNING);
        end if;

        --  NOTE: We do *not* set yylval here;
        --        because if we do, we get syntax errors (not sure why!).
        if Expecting_Indent then
           return INDENT;
        else
           return NEWLINE;
        end if;
    end;
}

^([ \r\t\f]+|"abstract"|"end"|"function"|"generic"|"with"|"use"|"package"|"protected"|"procedure"|"task")   {
    declare
        -- ignore spaces,Carriage returns,tabs,form feeds
        -- 
        -- determine indent (expanding tabs appropriately)
        -- compare to top of indent "stack"
        -- if less, emit OUTDENTs while popping from stack until find match
        --   (except ignore such a line if it starts with 
        --     "exception" or "begin")
        --   * complain if new level does not match one already on stack
        -- if same, emit NEWLINE; leave stack as is.
        -- if indent is more than top of stack:
        --   * if prior line ends with "is", "then", "else", etc.
        --        (i.e. Expecting_Indent is True) then
        --           push indent on stack and emit INDENT
        --   * if prior line does *not* end with "is", "then", etc.
        --        (i.e. Expecting_Indent is False) then
        --           do not push indent on stack

        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        if YYT(YYT'First) in 'a'..'z' then
           --  Reserved word at start of line.
           --  Indent is zero; put back characters
           for I in reverse YYT'Range loop
              unput(YYT(I));
           end loop;
           --  Next time, return the token
           ENTER(RESCANNING);
        else
           --  Start of line, determine indent
           ECHO_L; 
        end if;

        --  At this point, Col_Count is indent level

        if Top = 0 then
           --  No indents on stack yet
           Push_Indent (Col_Count, Error);
        elsif Col_Count > Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           if Expecting_Indent then
              --  Indenting is only significant if
              --  preceded by is/then/else/loop
              Push_Indent (Col_Count, Bracketing_Token);
              yylval := Create_Token(Text => "");
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning INDENT] "); Text_IO.Flush;
              end if;
              return INDENT;
           end if;
        elsif Col_Count = Indent_Stack(Top) then
           --  Same indent, this is a "significant" newline
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will pop indent-stack until we get a match
           pragma Assert (Col_Count < Indent_Stack (Top));

           --  tbd: convert last OUTDENT into a NEWLINE: Top := Top - 1;
           if Debug_Indent then
              Text_IO.Put_Line(" [entering OUTDENTING; returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           ENTER(OUTDENTING);
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
        if Debug_Indent and then Expecting_Indent then
           Text_IO.Put(" [NL: indent off] ");
        end if;
        Expecting_Indent := False;
    end;
}

<RESCANNING>^"abstract"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);
}

<RESCANNING>^"generic"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GENERIC_kw);
}

<RESCANNING>^"protected"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PROTECTED_kw);
}

<RESCANNING>^"end"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);
}

<RESCANNING>^"function"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNCTION_kw);
}

<RESCANNING>^"procedure"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PROCEDURE_kw);
}

<RESCANNING>^"with"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);
}

<RESCANNING>^"use"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (USE_kw);
}

<RESCANNING>^"interface"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);
}

<RESCANNING>^"package"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PACKAGE_kw);
}

  -- Ada202x reserved words
"abort"    	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABORT_kw);
}
"abs"    	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABS_kw);
}
"abstract"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ABSTRACT_kw);
}
"accept"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ACCEPT_kw);
}
"access"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ACCESS_kw);
}
"aliased"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALIASED_kw);
}
"all"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);
}
"and"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := DO_kw;
        Expecting_Indent := True;
	return (AND_kw);
}
"array"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ARRAY_kw);
}
"at"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AT_kw);
}
"begin"		{
        declare
           --  Remember state of "Expecting_Indent" before "begin"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;  -- Restore Expecting_Indent

           if not Expecting_Indent
             and then (Bracketing_Token_Stack (Top) = DECLARE_kw
                or else Bracketing_Token_Stack (Top) = IS_kw)
           then
              --  This is a "begin" matching a "declare" or an "is"
              --  with some declarative part (i.e. normal case).
              --  Update the stacked bracketing token, and preserve
              --  expecting-indent as False.
              Bracketing_Token_Stack (Top) := BEGIN_kw;
           else
              --  This is a "naked" begin, or a "declare" or "is"
              --  immediately followed by a "begin"
              --  (with no intervening newline).
              --  We are still expecting indent.
              --  Update the Bracketing_Token to be "BEGIN_kw"
              --  and make sure the Expecting_Indent is on.
              Expecting_Indent := True;
              Bracketing_Token := BEGIN_kw;
           end if;

           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (BEGIN_kw);
        end;
}
<RESCANNING>^"begin"		{
        declare
           --  Preserve "Expecting_Indent" across "begin".
           --  Note that this cannot be a "naked" begin, since that
           --  isn't allowed at the top level.
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (BEGIN_kw);
        end;
}
"body"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BODY_kw);
}
"case"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);
}
"constant"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONSTANT_kw);
}
"declare"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := DECLARE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (DECLARE_kw);
}
"delay"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DELAY_kw);
}
"delta"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DELTA_kw);
}
"digits"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DIGITS_kw);
}
"do"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := DO_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (DO_kw);
}
"else"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := ELSE_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (ELSE_kw);
}
"elseif"		{
	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""elseif""");
	return (ELSIF_kw);
}
"elif"		{
	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
        --  Allow "elif" as an alias for "elsif" for Python-like syntax
	return (ELSIF_kw);
}
"else if"		{
	ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token("elsif");
	yyerror("Use ""elsif"" rather than ""else if""");
	return (ELSIF_kw);
}
"elsif"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ELSIF_kw);
}
"endif"		{
        unput('f'); unput('i');
	ECHO_L("end"); ENTER(CHARLIT); 
        yylval := Create_Token("end");
	yyerror("Use ""end if"" rather than ""endif""");
	return (END_kw);
}
"end"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);
}
"entry"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ENTRY_kw);
}
<RESCANNING>^"exception"  {
        --  No need to preserve Expecting_Indent across exception
        --  since "exception" should never immediately follow a token
        --  that introduces a new level of indentation
        --  (unlike "begin")
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXCEPTION_kw);
}
"exception"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXCEPTION_kw);
}
"exit"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);
}
"for"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);
}
"function"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNCTION_kw);
}
"generic"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GENERIC_kw);
}
"goto"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GOTO_kw);
}
"if"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);
}
"in"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IN_kw);
}
"interface"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);
}
"is"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := IS_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (IS_kw);
}
"limited"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LIMITED_kw);
}
"loop"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := LOOP_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (LOOP_kw);
}
"mod"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (MOD_kw);
}
"new"		{
        ECHO_L; ENTER(CHARLIT); 
        yylval := Create_Token;
        return (NEW_kw);
}
"not"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NOT_kw);
}
"null"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (NULL_kw);
}
"of"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OF_kw);
}
"or"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);
}
"others"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OTHERS_kw);
}
"out"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OUT_kw);
}
"overriding"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OVERRIDING_kw);
}
"package"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PACKAGE_kw);
}
"parallel"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PARALLEL_kw);
}
"pragma"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRAGMA_kw);
}
"private"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);
}
"procedure"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PROCEDURE_kw);
}
"protected"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PROTECTED_kw);
}
"raise"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RAISE_kw);
}
"range"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RANGE_kw);
}
"record"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := RECORD_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (RECORD_kw);
}
"rem"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);
}
"renames"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RENAMES_kw);
}
"requeue"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REQUEUE_kw);
}
"return"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (RETURN_kw);
}
"reverse"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REVERSE_kw);
}
"select"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SELECT_kw);
}
"separate"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SEPARATE_kw);
}
"some"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);
}
"subtype"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SUBTYPE_kw);
}
"synchronized"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SYNCHRONIZED_kw);
}
"tagged"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TAGGED_kw);
}
"task"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TASK_kw);
}
"terminate"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TERMINATE_kw);
}
"then"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);
}
"type"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (TYPE_kw);
}
"until"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (UNTIL_kw);
}
"use"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (USE_kw);
}
"when"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHEN_kw);
}
"while"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WHILE_kw);
}
"with"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (WITH_kw);
}
"xor"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (XOR_kw);
}

  -- Match all the compound Ada202x delimiters. 
"=="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	yyerror("Use ""="" rather than ""==""");
	return('=');
}
"/="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(NEQ);
}
"!="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	yyerror("Use ""/="" rather than ""!=""");
	return(NEQ);
}
">="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GEQ);
}
"<="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LEQ);
}
"=>"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);
}
"<<"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LLABEL);
}
"**"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);
}
":="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(ASSIGN);
}
"<>"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(BOX);
}
".."		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);
}

  -- Match all the Ada202x single-character delimiters.
<TICK>\'  {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(PRIME);
}
"("        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('(');
}
")"        {
	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(')');
}
"["        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('[');
}
"]"        {
	ECHO_L; ENTER(TICK); 
	yylval := Create_Token;
	return(']');
}
"<"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('<');
}
">"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('>');
}
"*"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('*');
}
"+"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('+');
}
","        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(',');
}
"-"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('-');
}
"."        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('.');
}
"/"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('/');
}
"@"        {
	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return('@');
}
":" {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(':');
}
";"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return(';');
}
"|"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('|');
}
"&"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('&');
}
"="        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');
}

  -- The following is used to match all valid Ada202x identifiers
  -- except reserved words. Note that leading digits and underscores
  -- are not allowed and that double underscores are not allowed.

{IDENTIFIER}       {
	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);
}

  -- Enumeration literals
  --  #<id> is not legal Ada syntax, but might still come in handy
  --  when defining some low-level stuff, and shouldn't conflict
  --  with existing uses of '#'
[#]{IDENTIFIER}    {
       ECHO_L; ENTER(TICK);
       yylval := Create_Token;
       return(Enum_Literal);
}

  -- Decimal numeric literals
{DIGIT_SEQUENCE}{EXPONENT}?  {
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);
}

{DIGIT_SEQUENCE}[.]{DIGIT_SEQUENCE}{EXPONENT}?  {
      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);
}

  -- Based numeric literals.

{DIGIT_SEQUENCE}[#]{HEX_SEQUENCE}[#]{EXPONENT}? {
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Integer_Literal);
}

{DIGIT_SEQUENCE}[#]{HEX_SEQUENCE}[.]{HEX_SEQUENCE}[#]{EXPONENT}? {
      ECHO_L; ENTER(CHARLIT);
      yylval := Create_Token;
      return(Real_Literal);
}

  -- Match all valid character literals.  See Ada LRM 2.6.

<CHARLIT>{CHAR_LITERAL}    		{
	ECHO_L; ENTER(CHARLIT); 
        declare
           Tok : constant String := yytext;
        begin
           yylval := Create_Token (Tok);
           if Tok'Length > 3 then
              yyerror
                ("backslash cannot be used as an escape character in Ada",
                 At_Token => yylval);
           end if;
           return(Char_Literal);
        end;
}

  -- Match all valid string literals.  See Ada LRM 2.6.

{STRING_LITERAL}                {
	ECHO_L; ENTER(TICK); 
        declare
           Tok : constant String := yytext;
           Quote : constant String := """";
        begin
           yylval := Create_Token (Tok);
           if Tok (Tok'Last) /= Quote(1) then
              yyerror ("unterminated string", At_Token => yylval);
           end if;
           return(String_Literal);
        end;
}

  -- Handle white space --

[ \r\t\f]*"--".*    {ECHO_L;} -- ignore white space and comments to end-of-line

[ \r\t\f]+ {
        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;
}


<OUTDENTING>"b"/"egin"[ \r\t\f\n]        {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>"e"/"xception"[ \r\t\f\n]        {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>.  {
        --  Return another OUTDENT
        declare
           Do_Match_Check : Boolean := True;
        begin
           unput(yytext(1));
           if Top > 1 and then Col_Count < Indent_Stack(Top)
             and then (Col_Match_Expected
               or else Col_Count < Indent_Stack(Top-1))
           then
              --  Not yet reached a matching indent;
              --  Pop the stack
              if Debug_Indent then
                 Text_IO.Put_Line(" [popping" &
                   Integer'Image(Indent_Stack(Top)) &
                   " from Indent_Stack] ");
                 Text_IO.Flush;
              end if;
              Last_Pop := Indent_Stack(Top);  --  Save last pop
              Top := Top - 1;
              if Col_Count >= Indent_Stack(Top)
                or else not Col_Match_Expected
                or else Bracketing_Token_Stack(Top + 1) = REFERS_TO
              then
                 --  Don't check for indent match this time;
                 --  wait til next time since only one outdent occurring,
                 --  or "=>" used at end of line (which allows multiple
                 --  outdents at a time).
                 Do_Match_Check := False;
              end if;
           end if;

           if Do_Match_Check then
              --  Found (nearly) matching indent
              if Debug_Indent then
                 Text_IO.Put_Line(" [exiting OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              if Col_Count = 0 then
                 --  Return the token at the beginning of the line
                 ENTER(RESCANNING);
              else
                 ENTER(CHARLIT);
              end if;
              if Col_Count /= Indent_Stack(Top)
                and then Col_Match_Expected
              then
                 --  Not quite matching
                 if Indent_Stack(Top) = 0 and then Last_Pop > 0 then
                    --  Use Last_Pop instead
                    Parser_Warning("indent mismatch; expected indent of" &
                      Integer'Image(Last_Pop) & ", found" &
                      Integer'Image(Col_Count));
                 else
                    Parser_Warning("indent mismatch; expected indent of" &
                      Integer'Image(Indent_Stack(Top)) & ", found" &
                      Integer'Image(Col_Count));
                end if;

                 --  Force a match
                 --  TBD: Not sure this is the right choice
                 if Top = 1 then
                    Top := Top + 1;
                    if Debug_Indent then
                       Text_IO.Put_Line(" [pushing" &
                         Integer'Image(Col_Count) &
                         " on Indent_Stack to force a match] ");
                       Text_IO.Flush;
                    end if;
                 else
                    if Debug_Indent then
                       Text_IO.Put_Line(" [setting top of Indent_Stack to" &
                         Integer'Image(Col_Count) & "] "); Text_IO.Flush;
                    end if;
                 end if;
                 Indent_Stack(Top) := Col_Count;
              end if;
           end if;
           yylval := Create_Token(Text => "");
           if Do_Match_Check then
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE after OUTDENTs] ");
                 Text_IO.Flush;
              end if;
              return NEWLINE;
           else
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning OUTDENT] ");
                 Text_IO.Flush;
              end if;
              return OUTDENT;
           end if;
        end;
}

<CHARLIT,TICK,INITIAL><<EOF>>         {
        if Top = 0 then
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [EOF: indent off] "); Text_IO.Flush;
           end if;
           Expecting_Indent := False;
           if Debug_Indent then
              Text_IO.Put(" [returning EOF] "); Text_IO.Flush;
           end if;
           return End_Of_Input;
        else
           unput(YY_END_OF_BUFFER_CHAR);
           Col_Count := Indent_Stack(1);
           if Debug_Indent then
              Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
                " from Indent_Stack] ");
              Text_IO.Flush;
           end if;
           Top := Top - 1;
           if Top > 0 then
              if Debug_Indent then
                 Text_IO.Put_Line(" [entering EOF OUTDENTING] ");
                 Text_IO.Flush;
              end if;
              ENTER(OUTDENTING);
           else
              --  This is a significant NEWLINE
              if Debug_Indent then
                 Text_IO.Put_Line(" [returning NEWLINE] ");
                 Text_IO.Flush;
              end if;
              yylval := Create_Token(Text => "");
              return NEWLINE;
           end if;
        end if;
}

<OUTDENTING><<EOF>>     {
        --  Return another OUTDENT
        unput(YY_END_OF_BUFFER_CHAR);
        if Debug_Indent then
           Text_IO.Put_Line(" [popping" & Integer'Image(Indent_Stack(Top)) &
             " from Indent_Stack] ");
           Text_IO.Flush;
        end if;
        Top := Top - 1;
        if Top = 0 then
           --  All done
           if Debug_Indent then
              Text_IO.Put_Line(" [exiting EOF OUTDENTING] ");
              Text_IO.Flush;
           end if;
           ENTER(CHARLIT);
        end if;

        yylval := Create_Token(Text => "");
        if Debug_Indent then
           Text_IO.Put_Line(" [returning EOF OUTDENT] ");
           Text_IO.Flush;
        end if;
        return OUTDENT;
}


  -- The following matches all new lines (and the preceding white space).

[ \r\t\f]*[\n] {
        --  TBD: Should we return NEWLINE if this line is entirely blank?
        if Echo_Input then
           text_io.new_line;
        end if;
        Display_Linenum;
}

  -- The following matches everything else and prints an error message
  -- indicating that something unexpected was found.

.          {ECHO_L; 
	    yyerror("lexical error: '" &
	      Ada202x_lex_dfa.yytext & "'");
}

%%

with Ada202x_tokens; 
use  Ada202x_tokens;
with Ada202x_parser;
use Ada202x_parser;
with PSC.Source_Positions;
use  PSC.Source_Positions;
with PSC.Strings;
use  PSC.Strings;
with PSC.Syntax;
use  PSC.Syntax;
use text_io;

package Ada202x_lex is
  
  Debug_Indent   : Boolean := False;
  Bracketing_Token : Token := Error;

  Expecting_Indent : Boolean := False;  
  --  Whether expecting indent on next line
  --  If this is True, then if the next line starts more indented,
  --  we produce an INDENT; if not more indented we do not produce an INDENT,
  --  which will probably produce a parse error, unless it is a word
  --  like "begin" which is not indented, even if we are "expecting"
  --  an indent.
  --  If this is False, then if the next line starts more indented,
  --  we generate neither a NEW_LINE nor an INDENT, and if the next line
  --  is at the same level of indentation, we generate a NEW_LINE.

  Top : Natural := 0;
  --  Top of indent stack

  procedure Push_Indent (Count : Natural; Bracket : Token);
  --  Push Count and Bracket on indent/bracketing-token stack

  procedure ECHO_L(YYT : String := yytext);  --  Instead of "ECHO" builtin
  --  Bump Char_Count and Col_Count based on characters of string.
  --  Also echoes to standard output if Echo_Input is True.
  --  Finally unconditionally sets Col_Match_Expected to True.

  procedure Init;

  function yylex return token;

  function Create_Token(Text : String := yytext) return YYSType;
  --  Unconditionally turns off "Expecting_Indent"

end Ada202x_lex;

package body Ada202x_lex is

  --  Indicates whether outdenting should expect a perfect col-count match
  Col_Match_Expected : Boolean := True;

  Max_Depth : constant := 100;
  Indent_Stack : array(Positive range 1 .. Max_Depth) of Natural :=
    (others => 0);
     --  This indicates the 
  Bracketing_Token_Stack : array(Positive range 1 .. Max_Depth) of Token :=
    (others => Error);

  Last_Pop : Natural := 0;  --  Keeps track of most recent pop from indent stk

  procedure ECHO_L(YYT : String := yytext) is
  --
  -- Local version of the  define string.
  -- 
  begin
     Echo_Token (YYT);
     --  We normally expect an exact match
     Col_Match_Expected := True;
  end ECHO_L;

  procedure Init is
  begin
     --  Reset indent level stack.
     Top := 0;
  end Init;

  function Create_Token(Text : String := yytext) return YYSType is
     Src_Pos : Source_Position := Cur_Source_Pos;
  begin
     --  Point to beginning of token
     Src_Pos.Col := Column_Number'Max (1, Src_Pos.Col - Text'Length + 1);
     --  TBD: "gcc" prefers column count rather than character count

     if Debug_Indent and then Expecting_Indent then
        Text_IO.Put (" [CT: indent off] ");
     end if;
     Expecting_Indent := False;

     --  Create a token for the parser.
     return (One_Token, Src_Pos, String_Lookup(Text));
  end Create_Token;
     
  procedure Push_Indent (Count : Natural; Bracket : Token) is
  begin
     Top := Top + 1;
     Indent_Stack(Top) := Count;
     Bracketing_Token_Stack(Top) := Bracket;
     if Debug_Indent then
        Text_IO.Put_Line(" [pushing" & Integer'Image(Count) &
          " on Indent_Stack at" & Integer'Image (Top) & "] ");
        Text_IO.Flush;
     end if;
  end Push_Indent;
##

end Ada202x_lex;

