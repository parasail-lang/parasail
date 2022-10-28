------------------------------------------------------------------------------
--                              P A R Y T H O N                             --
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
%START TICK CHARLIT AFTER_IMPLEMENTS
%x OUTDENTING RESCANNING


GRAPHIC_CHAR  [ !"#$%&'()*+,-./0-9:;<=>?@A-Z\[\\\]^_`a-z{|}~]

STRING_LITERAL  (\"([^\\\"\n]|\\.)*[\"\n])

CHAR_LITERAL    (\'([^\\\']|\\.)\')

IDENTIFIER        [a-zA-Z]([_a-zA-Z0-9])*

  -- The following are used to match all numeric literals.
  -- Note that double underscores are rejected.
DIGIT_SEQUENCE    [0-9]([_]?[0-9])*
HEX_SEQUENCE      [0-9a-fA-F]([_]?[0-9a-fA-F])*
EXPONENT          [Ee][-+]?{DIGIT_SEQUENCE}

%%

-- ^[ \r\t\f]+/"queued"[ \r\t\f\n] {
--         -- ignore "queued" at start of line as far as indenting
--         ECHO_L; 
--         if Debug_Indent and then Expecting_Indent then
--            Text_IO.Put(" [queued: indent off] ");
--         end if;
--         Expecting_Indent := False;  -- "then" will turn it back on
-- }

^[ \r\t\f]+/"is"[ \r\t\f\n] {
        -- ignore "is" at start of line as far as indenting
        ECHO_L; 
}

^[ \r\t\f]*"exports"([ \t]*":")?/[ \r\t\f\n] {
        -- this keyword when used at the beginning of a line
        -- is expected to be "outdented" relative to the preceding and
        -- following construct.  We want to treat it as though it
        -- is the same indent as the prior construct that is just slightly
        -- more indented than this keyword.
        -- However, if we are expecting an *indent* then we don't look
        -- at the stack, and just return an INDENT, while preserving
        -- the "expecting indent" flag.
        -- If not expecting an indent, then we generate zero or more
        -- OUTDENTs and one NEWLINE if it is outdented relative to
        -- the top of the indent stack.  If not outdented relative to
        -- the top of the indent stack, we don't produce anything extra.

    declare
        YYT : constant String := yytext;
        Export_Start : Positive := YYT'First;
    begin
        pragma Assert (Col_Count = 0);

        --  Start of line, determine indent
        while Export_Start < YYT'Last and then YYT(Export_Start) /= 'e' loop
           Export_Start := Export_Start + 1;
        end loop;

        ECHO_L(YYT(YYT'First .. Export_Start - 1));

        --  Put "exports..." back
        for I in reverse Export_Start .. YYT'Last loop
           unput(YYT(I));
        end loop;

        if Col_Count = 0 then
           ENTER(RESCANNING);
        end if;

        --  At this point, Col_Count is indent level

        if Expecting_Indent then
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning INDENT] ");
              Text_IO.Flush;
           end if;
           --  Preserve indent expectation
           Expecting_Indent := True;
           return INDENT;
        elsif Top = 0 then
           --  No indents on stack yet
           --  This shouldn't happen.
           yyerror("Source file cannot start with this token");
        elsif Col_Count >= Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           --  Do nothing
           null;
        elsif Top = 1 then
           --  This is unexpected; should be at least two
           --  Do nothing
           null;
        elsif Col_Count >= Indent_Stack(Top-1) then
           --  See where we stand relative to the next indent level.
           --  We are not more outdented than next level, so return NEWLINE
           yylval := Create_Token(Text => "");
           if Debug_Indent then
              Text_IO.Put_Line(" [returning NEWLINE] ");
              Text_IO.Flush;
           end if;
           return NEWLINE;
        else
           --  Outdenting relative to remembered indent;
           --  we will set Col_Match_Expected to False so we
           --  won't complain about a col-count mismatch.
           pragma Assert (Col_Count < Indent_Stack (Top-1));

           Col_Match_Expected := False;

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

^[ \r\t\f]*("new"|"then"|"||")/[ \r\t\f\n] {
        -- ignore "new", "then", "||" at start of line as far as indenting
        -- but return a NEWLINE if not expecting an INDENT
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

        if not Expecting_Indent then
           yylval := Create_Token(Text => "");
           return NEWLINE;
        end if;
    end;
}

^[ \r\t\f]+/"*"[^*=] {
        -- Remember Expecting_Indent state, and return INDENT
        -- if expecting indent, but don't push new indent level on stack;
        -- return NEWLINE if not expecting an indent.
        Expecting_Indent_At_Label := Expecting_Indent;
        ECHO_L; 
        if Expecting_Indent then
            yylval := Create_Token(Text => "");
            if Debug_Indent then
               Text_IO.Put_Line(" [label: returning INDENT] "); Text_IO.Flush;
            end if;
            Expecting_Indent := True;
            return INDENT;
        else
            yylval := Create_Token(Text => "");
            return NEWLINE;
        end if;
}

<AFTER_IMPLEMENTS>^[ \r\t\f]+/"for"[ \r\t\f\n] {
        -- ignore "for" after "implements" at start of line as far as indenting
        ECHO_L; 
        ENTER(CHARLIT);
}

^("abstract"|"class"|"concurrent"|"end"|"def"|"import"|"interface"|"defop"|[ \r\t\f]+)   {
    declare
        -- ignore spaces,Carriage returns,tabs,form feeds
        -- 
        -- determine indent (expanding tabs appropriately)
        -- compare to top of indent "stack"
        -- if less, emit OUTDENTs while popping from stack until find match
        --   (except ignore such a line if it starts with 
        --     "then"/"new"/"implements"/"exports"/"||"/"*XXX*")
        --   * complain if new level does not match one already on stack
        -- if same, emit NEWLINE; leave stack as is.
        -- if indent is more than top of stack:
        --   * if prior line ends with ":", push indent on stack and emit INDENT
        --   * if prior line does *not* end with ":" do not push indent on stack

        YYT : constant String := yytext;
    begin

        pragma Assert (Col_Count = 0);

        if YYT(YYT'First) in 'a'..'z' then
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
           Top := Top + 1;
           Indent_Stack(Top) := Col_Count;
           Bracketing_Token_Stack(Top) := Error;
           if Debug_Indent then
              Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                " on Indent_Stack] ");
              Text_IO.Flush;
           end if;
        elsif Col_Count > Indent_Stack(Top) then
           --  Indenting relative to remembered stack
           if Expecting_Indent then
              --  Indenting is only significant if
              --  preceded by ':'/is/of/then/else/loop
              Top := Top + 1;
              Indent_Stack(Top) := Col_Count;
              Bracketing_Token_Stack(Top) := Bracketing_Token;
              if Debug_Indent then
                 Text_IO.Put_Line(" [pushing" & Integer'Image(Col_Count) &
                   " on Indent_Stack] "); Text_IO.Flush;
              end if;
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

<RESCANNING>^"class"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);
}

<RESCANNING>^"concurrent"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);
}

<RESCANNING>^"end"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (END_kw);
}

<RESCANNING>^"func"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);
}

<RESCANNING>^"def"	{  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEF_kw);
}

<RESCANNING>^"import"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);
}

<RESCANNING>^"interface"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (INTERFACE_kw);
}

<RESCANNING>^"defop"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEFOP_kw);
}

  -- Parython reserved words
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
"all"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (ALL_kw);
}
"and"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AND_kw);
}
"as"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (AS_kw);
}
"begin"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BEGIN_kw);
}
"block"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := BLOCK_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (BLOCK_kw);
}
"break"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (BREAK_kw);
}
"case"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CASE_kw);
}
"class"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CLASS_kw);
}
"concurrent"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONCURRENT_kw);
}
"const"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONST_kw);
}
"continue"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (CONTINUE_kw);
}
"each"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EACH_kw);
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
"exit"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXIT_kw);
}
<RESCANNING>^"exports"([ \t]*":")?	{
        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;
}
"exports"([ \t]*":")?	{
        declare
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (EXPORTS_kw);
        end;
}
"extends"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (EXTENDS_kw);
}
"for"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FOR_kw);
}
"forward"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FORWARD_kw);
}
"func"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (FUNC_kw);
}
"def"	{  --  "def" is used in Python, equiv to "func"
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEF_kw);
}
"global"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (GLOBAL_kw);
}
"if"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IF_kw);
}
"implements"	{
	ECHO_L; ENTER(AFTER_IMPLEMENTS); 
        --  TBD: We might want to return a NEWLINE first.
	yylval := Create_Token;
	return (IMPLEMENTS_kw);
}
"import"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (IMPORT_kw);
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
"lambda"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LAMBDA_kw);
}
"locked"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (LOCKED_kw);
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
<RESCANNING>^"new"		{
        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;
}
"new"		{
        declare
           --  Preserve "Expecting_Indent" across "new"
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           Expecting_Indent := Old_Indent;
           if Debug_Indent and then Expecting_Indent then
              Text_IO.Put(" [indent on] ");
           end if;
	   return (NEW_kw);
        end;
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
        Bracketing_Token := OF_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (OF_kw);
}
"defop"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (DEFOP_kw);
}
"opt"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPT_kw);
}
"optional"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OPTIONAL_kw);
}
"or"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (OR_kw);
}
"private"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (PRIVATE_kw);
}
"queued"	{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (QUEUED_kw);
}
"ref"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REF_kw);
}
"rem"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (REM_kw);
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
"some"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SOME_kw);
}
"switch"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (SWITCH_kw);
}
<RESCANNING>^"then"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
        Bracketing_Token := THEN_kw;
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return (THEN_kw);
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
"var"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return (VAR_kw);
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

  -- Match all the compound Parython delimiters. 
"=?"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMPARE);
}
"=="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(EQ);
}
"!="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
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
"<<"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT);
}
"**"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER);
}
":="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COLON_EQUAL);
}
"<=="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MOVE);
}
"<=>"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(SWAP);
}
".."		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOT_DOT);
}
"<..<"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_INTERVAL);
}
"<.."		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OPEN_CLOSED_INTERVAL);
}
"..<"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(CLOSED_OPEN_INTERVAL);
}
"::"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DOUBLE_COLON);
}
"=>"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(REFERS_TO);
}
"->"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(GIVES);
}
"==>"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(IMPLIES);
}
<RESCANNING>^"||"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);
}
"||"		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PARALLEL);
}
"+="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_ASSIGN);
}
"-="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(MINUS_ASSIGN);
}
"*="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(TIMES_ASSIGN);
}
"/="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(DIVIDE_ASSIGN);
}
"**="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(POWER_ASSIGN);
}
"|="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_ASSIGN);
}
"<|="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(COMBINE_MOVE);
}
"and="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(AND_ASSIGN);
}
"or="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(OR_ASSIGN);
}
"xor="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(XOR_ASSIGN);
}
"<<="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(LSHIFT_ASSIGN);
}
">>="		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(RSHIFT_ASSIGN);
}
".+"|"+."|".+."|":+"|"+:"|":+:" {
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(PLUS_BASED_OP);
}

  -- Match all the Parython single-character delimiters.
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
"{"	   {
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_SET);  --  or L_SET in PARython mode
}
"{>"	   {  --  Allow {> ... <} so PARython can use { ... } for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(L_ASSERT);
}
"}"	   {
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_SET);  --  or R_SET in PARython mode
}
"<}"	   {  --  Allow {> ... <} so PARython can use { ... } for sets/maps
	ECHO_L; ENTER(CHARLIT);	  
	yylval := Create_Token;
	return(R_ASSERT);
}
"*"        {
        declare
           --  Remember Expecting_Indent
           Old_Indent : constant Boolean := Expecting_Indent;
        begin
	   ECHO_L; ENTER(CHARLIT); 
	   yylval := Create_Token;
           if Expecting_Indent_At_Label then
              if not Old_Indent then
                 --  Seen '*' twice; set Expecting_Indent back on
                 Expecting_Indent_At_Label := False;
                 Expecting_Indent := True;
                 if Debug_Indent then
                    Text_IO.Put(" [indent on] ");
                 end if;
              end if;
           end if;
	   return ('*');
        end;
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
":"/[ \r\t\f]*(("//"|"#"[# \r\t\f]).*)?[\n]        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
        Bracketing_Token := ':';
        Expecting_Indent := True;
        if Debug_Indent then
           Text_IO.Put(" [indent on] ");
        end if;
	return(EOL_COLON);
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
"?"        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('?');
}
"="        {
	ECHO_L; ENTER(CHARLIT);     
	yylval := Create_Token;
	return('=');
}

  -- The following is used to match all valid Parython identifiers
  -- except reserved words. Note that leading digits and underscores
  -- are not allowed and that double underscores are not allowed.

{IDENTIFIER}       {
	ECHO_L; ENTER(TICK);
	yylval := Create_Token;
	return(Identifier);
}

  -- Enumeration literals
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

"0"[xX]{HEX_SEQUENCE}		{
      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);
}

"0"[bB]{DIGIT_SEQUENCE}		{
      ECHO_L; ENTER(CHARLIT); 
      yylval := Create_Token;
      return(Integer_Literal);
}

  -- Match all valid character literals.  See Ada LRM 2.6.

<CHARLIT,AFTER_IMPLEMENTS>{CHAR_LITERAL}    		{
	ECHO_L; ENTER(CHARLIT); 
	yylval := Create_Token;
	return(Char_Literal);
}

  -- Match all valid string literals.  See Ada LRM 2.6.

{STRING_LITERAL}                {
	ECHO_L; ENTER(CHARLIT); 
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

[ \r\t\f]*("//"|"#"[# \r\t\f]).*    {ECHO_L;} -- ignore white space and comments to end-of-line

[ \r\t\f]*"--".*    {ECHO_L;} -- ignore white space and comments to end-of-line

[ \r\t\f]+ {
        --  Not start of line (or whole line); no need to keep track
        pragma Assert (Col_Count > 0);
        ECHO_L;
}


<OUTDENTING>"t"/"hen"[ \r\t\f\n]        {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>"i"/"mplements"[ \r\t\f\n]  {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>"n"/"ew"[ \r\t\f\n]         {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>"|"/"|"     {
        --  Ignore outdent for these "divider" constructs
        unput(yytext(1));
        if Debug_Indent then
           Text_IO.Put_Line(" [exiting OUTDENTING] ");
           Text_IO.Flush;
        end if;
        ENTER(CHARLIT);
}

<OUTDENTING>"*"/[^*=] {
        --  Ignore outdent for label
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
              Top := Top - 1;
              if Col_Count >= Indent_Stack(Top)
                or else not Col_Match_Expected
                or else Bracketing_Token_Stack(Top + 1) = ':'
                or else Bracketing_Token_Stack(Top + 1) = REFERS_TO
              then
                 --  Don't check for indent match this time;
                 --  wait til next time since only one outdent occurring,
                 --  or ":" or "=>" used at end of line (which allows multiple
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
                 Parser_Warning("indent mismatch; expected indent of" &
                   Integer'Image(Indent_Stack(Top)) & ", found" &
                   Integer'Image(Col_Count));

                 --  Force a match
                 --  TBD: Not sure this is the right choice
                 if Top = 1 then
                    Top := Top + 1;
                    if Debug_Indent then
                       Text_IO.Put_Line(" [pushing" &
                         Integer'Image(Col_Count) &
                         " on Indent_Stack] ");
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

<CHARLIT,TICK,AFTER_IMPLEMENTS,INITIAL><<EOF>>         {
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
	      Parython_Lex_DFA.yytext & "'");
}

%%
pragma Style_Checks (Off);
with Parython_Tokens; 
use  Parython_Tokens;
with Parython_Parser;
use Parython_Parser;
with PSC.Source_Positions;
use  PSC.Source_Positions;
with PSC.Strings;
use  PSC.Strings;
with PSC.Syntax;
use  PSC.Syntax;
use text_io;

package Parython_Lex is
  
  Debug_Indent   : Boolean := False;
  Bracketing_Token : Token := Error;

  Expecting_Indent : Boolean := False;  
  --  Whether expecting indent on next line

  Expecting_Indent_At_Label   : Boolean := False;  
  --  Whether expecting indent when encountering a label

  Top : Natural := 0;
  --  Top of indent stack

  procedure ECHO_L(YYT : String := yytext);  --  Instead of "ECHO" builtin

  procedure Init;

  function yylex return token;

  function Create_Token(Text : String := yytext) return YYSType;

end Parython_Lex;

pragma Style_Checks (Off);
package body Parython_Lex is

  --  Indicates whether outdenting should expect a perfect col-count match
  Col_Match_Expected : Boolean := True;

  Max_Depth : constant := 100;
  Indent_Stack : array(Positive range 1 .. Max_Depth) of Natural :=
    (others => 0);
  Bracketing_Token_Stack : array(Positive range 1 .. Max_Depth) of Token :=
    (others => Error);

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
     
##

end Parython_Lex;

