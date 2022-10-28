
pragma Style_Checks (Off);
with Parython_tokens, Parython_lex_io, Parython_goto, Parython_shift_reduce;
with Parython_lex, text_io;

use  Parython_tokens, Parython_lex_io, Parython_goto, Parython_shift_reduce;
use  Parython_lex, text_io;

with PSC.Trees; use PSC.Trees;

with PSC.Messages;
with PSC.Symbols;
with PSC.Syntax;
with PSC.Strings;
with PSC.Source_Positions;
with PSC.Trees.Module;
with PSC.Trees.Implements_Element;
with PSC.Trees.Binary;
with PSC.Trees.Unary;
with PSC.Trees.Identifier;
with PSC.Trees.Qualified_Name;
with PSC.Trees.Lists;
with PSC.Trees.Obj_Decl;
with PSC.Trees.Param_Decl;
with PSC.Trees.Operation;
with PSC.Trees.Reference;
with PSC.Trees.Assign_Stmt;
with PSC.Trees.Conditional;
with PSC.Trees.Invocation;
with PSC.Trees.Control_Stmt;
with PSC.Trees.Type_Decl;
with PSC.Trees.Selection;
with PSC.Trees.Case_Construct;
with PSC.Trees.Iterator;
with PSC.Trees.While_Stmt;
with PSC.Trees.Block_Stmt;
with PSC.Trees.For_Loop_Construct;
with PSC.Trees.Qualifier;
with PSC.Trees.Annotation;
with PSC.Trees.Property;
with PSC.Trees.Compound_Stmt;
with PSC.Trees.Semantics;

package body Parython_Parser is

    use type Param_Decl.Param_Kind;
    use type PSC.Strings.U_String;

    Last_Import_File : PSC.Strings.U_String := PSC.Strings.Null_U_String;
    Last_Import_List : PSC.Trees.Lists.List;

    function List_Or_Empty(S : YYSType) return Lists.List is
    --  Return S.List if S.Kind = One_List, else return Empty_List
    begin
        if S.Kind = One_List then
            return S.List;
        else
            return Lists.Empty_List;
        end if;
    end List_Or_Empty;

    function Token_Src_Pos (Token : YYSType)
      return PSC.Source_Positions.Source_Position is
    --  Try to extract source-pos from Token, else
    --  return PSC.Syntax.Cur_Source_Pos.
        use PSC.Source_Positions;
        Result : Source_Position := Null_Source_Position;
    begin
        case Token.Kind is
           when One_Token =>
              Result := Token.Source_Pos;
           when One_Tree =>
              Result := PSC.Trees.Find_Source_Pos (Token.Tree);
           when others =>
              null;
        end case;
        if Result /= Null_Source_Position then
           return Result;
        else
           return PSC.Syntax.Cur_Source_Pos;
        end if;
    end Token_Src_Pos;

   procedure Yyerror (S : String := "syntax error";
     At_Token : Parython_Tokens.YYSType := (Parython_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Error(S, Src_Pos => Token_Src_Pos (At_Token));
    end yyerror;

   procedure Parser_Warning (S : String;
     At_Token : Parython_Tokens.YYSType := (Parython_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Warning(S, Src_Pos => Token_Src_Pos (At_Token));
    end Parser_Warning;

    function Name_For_Module(Defining_Name : Optional_Tree) 
      return Optional_Tree is
	-- Return Optional_Name for module, extracting it
	-- from the "name[#label]" construct if necessary.
	Def_Name : Tree'Class renames Tree_Ptr_Of(Defining_Name).all;
    begin
	if Def_Name not in Invocation.Tree'Class then
	    -- No Add-on label
	    return Defining_Name;
	else
	    -- Has an add-on label
	    return Invocation.Tree(Def_Name).Prefix;
	end if;
    end Name_For_Module;

    procedure Check_Id_Match(Starting_Id : Optional_Tree;
      Ending_Id : Optional_Tree) is
      -- Check that starting and ending id's match 
        Start_Strs : constant PSC.Symbols.Str_Array :=
          PSC.Symbols.Name_Strings (Starting_Id);
        End_Strs : constant PSC.Symbols.Str_Array :=
          PSC.Symbols.Name_Strings (Ending_Id);
	use type PSC.Strings.U_String;
	use type PSC.Symbols.Str_Array;
    begin
	if Start_Strs /= End_Strs then
            --  Mismatch between start and end designators
            declare
                End_Token : constant YYSType(One_Tree) :=
                  (One_Tree, Ending_Id);
            begin
		yyerror("Start and end designators must match",
                  At_Token => End_Token);
            end;
	end if;
    end Check_Id_Match;

    function Add_On_For_Module(Defining_Name : Optional_Tree)
      return Lists.List is
	-- Return add-on label(s), if any, as a list
	Def_Name : Tree'Class renames Tree_Ptr_Of(Defining_Name).all;
    begin
	if Def_Name not in Invocation.Tree'Class then
	    -- No Add-on label
	    return Lists.Empty_List;
	else
	    -- Has an add-on label
	    return Invocation.Tree(Def_Name).Operands;
	end if;
    end Add_On_For_Module;

    function Conditionally_Complement(Cond : Optional_Tree;
      Complement : Boolean) return Optional_Tree is
	-- Return Tree, optionally surrounded by a "not" operator
    begin
	if Complement then
	    return Unary.Make(
	      Operator => Unary.Not_Op,
	      Operand => Cond);
	else
	    return Cond;
	end if;
    end Conditionally_Complement;

    procedure Check_Stmt_Label(Compound_Stmt : YYSType;
      Start_Label_Token : YYSType) is
	-- Make sure that starting label matches
	-- ending label.
        pragma Assert (Compound_Stmt.Kind = One_Tree);
        Compound_Stmt_OT : constant Optional_Tree := Compound_Stmt.Tree;
    begin
	if Is_Null(Compound_Stmt_OT) then
	    -- Must have already been some error in the compound statement
	    return;
	else
	  declare
	    pragma Assert(Tree_Ptr_Of(Compound_Stmt_OT).all in 
	      PSC.Trees.Compound_Stmt.Tree'Class);
            Compound_Tree : PSC.Trees.Compound_Stmt.Tree'Class renames
	      PSC.Trees.Compound_Stmt.Tree'Class
                (Tree_Ptr_Of(Compound_Stmt_OT).all);
	    End_Label : constant Optional_Tree := Compound_Tree.Label;
            Start_Label : Optional_Tree := Null_Optional_Tree;
	  begin
            if Start_Label_Token.Kind = One_Tree then
                Start_Label := Start_Label_Token.Tree;
            end if;
            if not Compound_Tree.Check_Label then
                --  No end label; copy from start label
                pragma Assert (Is_Null (End_Label));
                Compound_Tree.Label := Start_Label;
	    elsif Is_Null(Start_Label) then
		if Is_Null(End_Label) then
		    -- Both null, that's fine
		    return;
		else
		    yyerror("Missing start label",
                      At_Token => Compound_Stmt);
		end if;
	    elsif Is_Null(End_Label) then
		yyerror("Missing end label",
                  At_Token => Compound_Stmt);
	    else
	      declare
		pragma Assert(
		  Tree_Ptr_Of(Start_Label).all in PSC.Trees.Identifier.Tree);
		pragma Assert(
		  Tree_Ptr_Of(End_Label).all in PSC.Trees.Identifier.Tree);
		use PSC.Strings;
		Start_Label_Id : constant U_String :=
		  PSC.Trees.Identifier.Tree(Tree_Ptr_Of(Start_Label).all).Str;
		End_Label_Id : constant U_String :=
		  PSC.Trees.Identifier.Tree(Tree_Ptr_Of(End_Label).all).Str;
	      begin
		if Start_Label_Id /= End_Label_Id then
		    yyerror("Start label " & 
		      To_String(Start_Label_Id) & 
		      " does not match end label " & 
		      To_String(End_Label_Id),
                      At_Token => Start_Label_Token);
		end if;
	      end;
	    end if; -- whether start and end label not null
	  end;
	end if;  -- whether Compound stmt not null
    end Check_Stmt_Label;
	
    function Copy_If_Not_First (OT : Optional_Tree; Index : Positive)
      return Optional_Tree is
    --  Return OT if Index = 1, or Trees.Copy_Tree (OT) otherwise.
    --  This is used when we have a list of identifiers, and we want
    --  to copy the type and the default expressions for the 2nd, 3rd, ...
    --  identifiers in the list.
    begin
       if Index = 1 or else Is_Null (OT) then
          return OT;
       else
          return PSC.Trees.Copy_Tree (OT);
       end if;
    end Copy_If_Not_First;

    use Qualifier; -- For Qualifier_Enum literals

pragma Style_Checks (Off);
procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Parython_Goto;
    package yy_shift_reduce_tables renames
      Parython_Shift_Reduce;
    package yy_tokens              renames
      Parython_Tokens;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       stack_size : constant Natural := 3000;

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        yyerror("Syntax Error");
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line(" Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when  3 =>
--#line  255

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  4 =>
--#line  262

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  5 =>
--#line  269

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  6 =>
--#line  276

	null;
    

when  10 =>
--#line  283

        if not Lists.Is_Empty (
yy.value_stack(yy.tos).List)
          or else PSC.Syntax.Cur_File /= Last_Import_File then
            --  We have a new non-empty list of imports, or a new file;
            --  this list overrides any earlier list
            Last_Import_List := 
yy.value_stack(yy.tos).List;
            Last_Import_File := PSC.Syntax.Cur_File;
        end if;
        
yyval := (One_List, Last_Import_List);
    

when  11 =>
--#line  295

	
yyval := (One_List, Lists.Empty_List);
    

when  12 =>
--#line  298

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);  
	  -- TBD: Do we care how these were grouped?
    

when  13 =>
--#line  306

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  14 =>
--#line  309

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  15 =>
--#line  316

        
yyval := (One_Tree, PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  16 =>
--#line  319

        
yyval := 
yy.value_stack(yy.tos);
    

when  17 =>
--#line  322

	
yyval := (One_Tree, Qualified_Name.Make (
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  18 =>
--#line  337

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-5).List;
      begin
	if 
yy.value_stack(yy.tos-10).Is_Private and then 
yy.value_stack(yy.tos-7).Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-4).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos-4).List));
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-8).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-8).Tree),
	  Is_Interface => True,
	  Is_Abstract => True,
	  Is_Private => 
yy.value_stack(yy.tos-10).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-10).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-7).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-7).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-7).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-7).Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => 
yy.value_stack(yy.tos-3).List,
	  Module_Implements => 
yy.value_stack(yy.tos-2).List));

        if 
yy.value_stack(yy.tos).Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-8).Tree),
              Ending_Id => 
yy.value_stack(yy.tos).Label);
        end if;

      end;
   

when  19 =>
--#line  379

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-5).List;
      begin
	if 
yy.value_stack(yy.tos-11).Is_Private and then 
yy.value_stack(yy.tos-7).Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-4).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos-4).List));
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-8).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-8).Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => 
yy.value_stack(yy.tos-11).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-11).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-7).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-7).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-7).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-7).Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => 
yy.value_stack(yy.tos-3).List,
	  Module_Implements => 
yy.value_stack(yy.tos-2).List));

        if 
yy.value_stack(yy.tos).Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-8).Tree),
              Ending_Id => 
yy.value_stack(yy.tos).Label);
        end if;

      end;
   

when  20 =>
--#line  415

        --  This is an interface without any declarations

	if 
yy.value_stack(yy.tos-4).Is_Private and then 
yy.value_stack(yy.tos-1).Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-2).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-2).Tree),
	  Is_Interface => True,
	  Is_Abstract => True,
	  Is_Private => 
yy.value_stack(yy.tos-4).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-4).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-1).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-1).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-1).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-1).Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    

when  22 =>
--#line  441

        yyerror ("Syntax error before ""is""", At_Token => 
yy.value_stack(yy.tos-1));
    

when  25 =>
--#line  449

        yyerror ("Syntax error at end-of-line", At_Token => 
yy.value_stack(yy.tos));
    

when  26 =>
--#line  452

        yyerror ("Syntax error before ':'", At_Token => 
yy.value_stack(yy.tos-1));
    

when  30 =>
--#line  465

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  31 =>
--#line  469

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  34 =>
--#line  478

	yyerror("Should be ""end interface <id>"" rather than ""end <id>""");
    

when  35 =>
--#line  484
 
yyval := 
yy.value_stack(yy.tos); 

when  36 =>
--#line  485

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  37 =>
--#line  493
 
yyval := 
yy.value_stack(yy.tos); 

when  38 =>
--#line  494

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  39 =>
--#line  504
 
yyval := 
yy.value_stack(yy.tos); 

when  40 =>
--#line  505
 
	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  41 =>
--#line  512

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	       Is_Concurrent => True, others => False);
    

when  42 =>
--#line  519

        --  NOTE: We don't allow stand-alone operator definitions
        
yyval := 
yy.value_stack(yy.tos);
  

when  43 =>
--#line  523
 
yyval := 
yy.value_stack(yy.tos-1); 

when  44 =>
--#line  524
 
yyval := 
yy.value_stack(yy.tos-1); 

when  45 =>
--#line  528
 
yyval := 
yy.value_stack(yy.tos-1); 

when  46 =>
--#line  529
 
yyval := (One_List, Lists.Empty_List); 

when  47 =>
--#line  533

	
yyval := (Formals_And_Interfaces,
	  Has_Module_Formals => (
yy.value_stack(yy.tos-1).Kind = One_List),
	  Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-1)),
	  Extends => Null_Optional_Tree,
	  Implements => 
yy.value_stack(yy.tos).List);
    

when  48 =>
--#line  540

        
yyval := (Formals_And_Interfaces,
          Has_Module_Formals => (
yy.value_stack(yy.tos-3).Kind = One_List),
          Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-3)),
          Extends => Param_Decl.Make(
            Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => 
yy.value_stack(yy.tos-1).Tree,
            Param_Default => Null_Optional_Tree),
          Implements => 
yy.value_stack(yy.tos).List);
    

when  49 =>
--#line  553

      declare
	Extends_Decl : constant Optional_Tree := Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,  -- TBD
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => Null_Optional_Tree);
      begin
        
yyval := (Formals_And_Interfaces,
          Has_Module_Formals => (
yy.value_stack(yy.tos-5).Kind = One_List),
          Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-5)),
          Extends => Extends_Decl,
          Implements => 
yy.value_stack(yy.tos).List);
      end;
    

when  51 =>
--#line  572

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  52 =>
--#line  582
 
yyval := 
yy.value_stack(yy.tos); 

when  53 =>
--#line  583
 
	
yyval := (Optional, Is_Present => False);
    

when  54 =>
--#line  589
 
yyval := 
yy.value_stack(yy.tos); 

when  55 =>
--#line  590

	
yyval := (One_List, Lists.Empty_List);
    

when  56 =>
--#line  595
 
yyval := 
yy.value_stack(yy.tos); 

when  57 =>
--#line  598
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  58 =>
--#line  601

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  59 =>
--#line  608
 
yyval := 
yy.value_stack(yy.tos); 

when  60 =>
--#line  609
 
yyval := 
yy.value_stack(yy.tos); 

when  61 =>
--#line  612
 
yyval := 
yy.value_stack(yy.tos); 

when  62 =>
--#line  615
 
yyval := 
yy.value_stack(yy.tos); 

when  63 =>
--#line  616
 
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  64 =>
--#line  625

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  65 =>
--#line  630
 
yyval := 
yy.value_stack(yy.tos); 

when  66 =>
--#line  631

	
yyval := (One_List, Lists.Empty_List);
    

when  67 =>
--#line  636
 
yyval := 
yy.value_stack(yy.tos); 

when  68 =>
--#line  637

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  69 =>
--#line  644

	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos).List);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  70 =>
--#line  653

	
yyval := 
yy.value_stack(yy.tos-1);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    -- Add annotation to first element
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, 1), 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;

	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Add annotation to last element
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	end if;
    

when  71 =>
--#line  670
 
yyval := 
yy.value_stack(yy.tos); 

when  72 =>
--#line  671

	
yyval := (One_List, Lists.Empty_List);
    

when  73 =>
--#line  677
 
yyval := 
yy.value_stack(yy.tos); 

when  74 =>
--#line  678

	
yyval := (One_List, Lists.Empty_List);
    

when  75 =>
--#line  684

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  76 =>
--#line  690
 
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  77 =>
--#line  698

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  78 =>
--#line  739
 
yyval := 
yy.value_stack(yy.tos); 

when  79 =>
--#line  740
 
yyval := (One_Tree, Null_Optional_Tree); 

when  80 =>
--#line  746

	-- "simple_expression" to avoid use of '>'
	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-4).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-4).List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I)),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  81 =>
--#line  764

	-- "simple_expression" to avoid use of '>'
	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-4).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-4).List, I),
	      Kind => 
yy.value_stack(yy.tos-5).Param_Kind,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I)),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  82 =>
--#line  781
 
	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  83 =>
--#line  792
 
yyval := 
yy.value_stack(yy.tos); 

when  84 =>
--#line  795
 
yyval := 
yy.value_stack(yy.tos); 

when  85 =>
--#line  796
 
yyval := (One_Tree, Null_Optional_Tree); 

when  86 =>
--#line  800

	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos));
    

when  87 =>
--#line  809

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  88 =>
--#line  812

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  89 =>
--#line  819
 
yyval := 
yy.value_stack(yy.tos); 

when  90 =>
--#line  820
 
yyval := 
yy.value_stack(yy.tos); 

when  91 =>
--#line  823

	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  92 =>
--#line  830
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  93 =>
--#line  833

	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  94 =>
--#line  840
 
yyval := 
yy.value_stack(yy.tos); 

when  95 =>
--#line  841

        -- String_Literal can be used as a "name" when it is an operator
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  96 =>
--#line  848

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  97 =>
--#line  854

	-- Include extension label in module name
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
	    Invocation.Make(
	      Kind => Invocation.Container_Indexing,
	      Prefix => 
yy.value_stack(yy.tos-6).Tree,
	      Operands => 
yy.value_stack(yy.tos-4).List),
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  98 =>
--#line  868
 
yyval := 
yy.value_stack(yy.tos); 

when  99 =>
--#line  869
 
	
yyval := (One_List, Lists.Empty_List);
    

when  100 =>
--#line  875
 
yyval := 
yy.value_stack(yy.tos); 

when  101 =>
--#line  876

	
yyval := (One_List, Lists.Empty_List);
    

when  102 =>
--#line  882

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  103 =>
--#line  885

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  104 =>
--#line  892
 
yyval := 
yy.value_stack(yy.tos); 

when  105 =>
--#line  893

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  106 =>
--#line  902
 
	-- polymorphic type name not allowed here
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  107 =>
--#line  907
 
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  108 =>
--#line  911

	-- This is a polymorphic type name, presumably.
	-- We use adding_expression instead of qualified_name
	-- to avoid reduce/reduce conflicts in the grammar.
	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  109 =>
--#line  919
 
yyval := 
yy.value_stack(yy.tos); 

when  110 =>
--#line  921
 
yyval := 
yy.value_stack(yy.tos); 

when  111 =>
--#line  922
 
yyval := 
yy.value_stack(yy.tos); 

when  112 =>
--#line  926
 
yyval := 
yy.value_stack(yy.tos); 

when  113 =>
--#line  927

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  114 =>
--#line  931

        
yyval := 
yy.value_stack(yy.tos);
    

when  115 =>
--#line  937
 
yyval := 
yy.value_stack(yy.tos); 

when  116 =>
--#line  938
 
yyval := 
yy.value_stack(yy.tos); 

when  117 =>
--#line  942
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  118 =>
--#line  945
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  119 =>
--#line  948
 
	
yyval := (One_Tree, Invocation.Add_Extends(
	  Instantiation => 
yy.value_stack(yy.tos-2).Tree, 
	  Extends => 
yy.value_stack(yy.tos).Tree));
    

when  120 =>
--#line  956
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  121 =>
--#line  963
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  122 =>
--#line  971
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-3).Is_Concurrent,
	    others => False), 
	  Operand => Invocation.Add_Extends(
	    Instantiation => 
yy.value_stack(yy.tos-2).Tree, 
	    Extends => 
yy.value_stack(yy.tos).Tree)));
    

when  123 =>
--#line  983
 
	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Present,
	  others => False);
    

when  124 =>
--#line  990

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Concurrent => True, others => False);
    

when  125 =>
--#line  998

	
yyval := (Optional, True);
    

when  126 =>
--#line  1001

	
yyval := (Optional, False);
    

when  127 =>
--#line  1007

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => 
yy.value_stack(yy.tos).List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  128 =>
--#line  1021

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).List,
	  Operation_Outputs => 
yy.value_stack(yy.tos).List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  129 =>
--#line  1037
 
yyval := (One_List, Lists.Empty_List); 

when  130 =>
--#line  1038

	if Lists.Is_Empty(
yy.value_stack(yy.tos-1).List) then
	    -- We want to make sure that we return a non-empty list
	    
yyval := (One_List, 
	      Lists.Make((1 => Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List))));
	else
	    
yyval := 
yy.value_stack(yy.tos-1);
	    if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
		-- Add annotation to item at end of list
		Annotation.Add_Annotation(
		  Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
		  
yy.value_stack(yy.tos).List);
	    end if;
	end if;
    

when  133 =>
--#line  1057

	
yyval := (One_List, Lists.Empty_List);
    

when  134 =>
--#line  1060

	
yyval := 
yy.value_stack(yy.tos-2);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-1).List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos).Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  135 =>
--#line  1069

	
yyval := 
yy.value_stack(yy.tos-3);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  136 =>
--#line  1078

	
yyval := 
yy.value_stack(yy.tos-3);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  137 =>
--#line  1091

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  138 =>
--#line  1097
 
yyval := 
yy.value_stack(yy.tos-1); 

when  139 =>
--#line  1098
 
yyval := 
yy.value_stack(yy.tos); 

when  140 =>
--#line  1102
 
yyval := 
yy.value_stack(yy.tos); 

when  141 =>
--#line  1103
 
yyval := 
yy.value_stack(yy.tos); 

when  142 =>
--#line  1104
 
yyval := 
yy.value_stack(yy.tos); 

when  143 =>
--#line  1108
 
yyval := 
yy.value_stack(yy.tos); 

when  144 =>
--#line  1109

	
yyval := (One_List, Lists.Empty_List);
    

when  145 =>
--#line  1115
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-1).List;
      begin
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
	end if;
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    

when  146 =>
--#line  1129

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-1).List;
      begin
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
	end if;
	
yyval := (One_List, Lists.Make((1 =>
	  Implements_Element.Make(
	    For_Interfaces => 
yy.value_stack(yy.tos-2).List, Elements => Elem_List))));
      end;
    

when  147 =>
--#line  1143

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-1).List;
      begin
	
yyval := 
yy.value_stack(yy.tos-5);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
	end if;
	Lists.Append(
yyval.List,
	  Implements_Element.Make(
	    For_Interfaces => 
yy.value_stack(yy.tos-2).List, Elements => Elem_List));
      end;
    

when  150 =>
--#line  1161
 
yyval := 
yy.value_stack(yy.tos); 

when  151 =>
--#line  1162
 
yyval := 
yy.value_stack(yy.tos-1); 

when  152 =>
--#line  1166
 
yyval := 
yy.value_stack(yy.tos); 

when  153 =>
--#line  1167

	
yyval := (One_List, Lists.Empty_List);
    

when  154 =>
--#line  1173
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    

when  155 =>
--#line  1183
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => 
yy.value_stack(yy.tos-1).List,
	  Elements => Elem_List))));
      end;
    

when  156 =>
--#line  1193

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := 
yy.value_stack(yy.tos-4);
	Lists.Append(
yyval.List,
	  Implements_Element.Make(
	    For_Interfaces => 
yy.value_stack(yy.tos-1).List, Elements => Elem_List));
      end;
    

when  157 =>
--#line  1206

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  158 =>
--#line  1210

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  160 =>
--#line  1218

        --  Pop the indent stack
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;
    

when  161 =>
--#line  1227

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  162 =>
--#line  1231

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  163 =>
--#line  1235

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  164 =>
--#line  1240

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  165 =>
--#line  1245

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  166 =>
--#line  1251

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  167 =>
--#line  1257

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => 
yy.value_stack(yy.tos-1).List,
	       Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos);
	    
yyval := (One_Tree, Optional(Op_Decl));
	end;
    

when  168 =>
--#line  1272

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => 
yy.value_stack(yy.tos-1).List,
	       Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos);
	    
yyval := (One_Tree, Optional(Op_Decl));
	end;
    

when  169 =>
--#line  1287

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
    

when  170 =>
--#line  1295

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
    

when  171 =>
--#line  1303

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
    

when  172 =>
--#line  1313

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
    

when  173 =>
--#line  1323

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => 
yy.value_stack(yy.tos-1).List,
	       Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos);
	    
yyval := (One_Tree, Optional(Op_Decl));
            if Parython_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  174 =>
--#line  1342

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => 
yy.value_stack(yy.tos-1).List,
	       Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos);
	    
yyval := (One_Tree, Optional(Op_Decl));
            if Parython_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Parython_Lex.Top := Parython_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  175 =>
--#line  1370

        -- TBD: allow an annotation after class_element_list
	
yyval := (One_Tree, PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-7).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-7).Tree),
	  Is_Interface => False,
	  Is_Abstract => 
yy.value_stack(yy.tos-9).Is_Abstract,
	  Is_Private => 
yy.value_stack(yy.tos-9).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-9).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-6).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-6).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-6).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-6).Implements,
	  Class_Locals => 
yy.value_stack(yy.tos-4).First_List,
	  Module_Exports => 
yy.value_stack(yy.tos-4).Second_List,
	  Module_New_Exports => 
yy.value_stack(yy.tos-3).List,
	  Module_Implements => 
yy.value_stack(yy.tos-2).List));
	    -- NOTE: Module_Implements is where bodies would go
	    --       if there is some ambiguity between operations that
	    --       are in the "normal" interface part vs. in the
	    --       "implements" part of the interface.

        if 
yy.value_stack(yy.tos).Check_Label then
	    Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-7).Tree),
	      Ending_Id => 
yy.value_stack(yy.tos).Label);
        end if;

   

when  176 =>
--#line  1402

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  177 =>
--#line  1406

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  179 =>
--#line  1413

	yyerror("Should be ""end class <id>"" rather than ""end <id>""");
    

when  180 =>
--#line  1418
 
yyval := (One_List, Lists.Empty_List); 

when  181 =>
--#line  1419

	if Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- We want to make sure that we return a non-empty list
	    
yyval := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    
yyval := 
yy.value_stack(yy.tos);
	end if;
    

when  182 =>
--#line  1431

	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  183 =>
--#line  1442

	-- Include annotation at end of locals
      declare
	Locals : Lists.List := 
yy.value_stack(yy.tos-3).List;
      begin
        if not Lists.Is_Empty (
yy.value_stack(yy.tos-2).List) then
            Lists.Append(Locals, Annotation.Make(Annotations => 
yy.value_stack(yy.tos-2).List));
        end if;

	
yyval := (Two_Lists, Locals, 
yy.value_stack(yy.tos).List);
      end;
    

when  184 =>
--#line  1454

	yyerror("Missing ""exports"" keyword");
	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  188 =>
--#line  1462

	
yyval := (One_List, Lists.Empty_List);
    

when  189 =>
--#line  1465

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  190 =>
--#line  1472
 
yyval := 
yy.value_stack(yy.tos); 

when  191 =>
--#line  1473
 
yyval := 
yy.value_stack(yy.tos-1); 

when  192 =>
--#line  1474
 
yyval := 
yy.value_stack(yy.tos-1); 

when  193 =>
--#line  1475
 
yyval := 
yy.value_stack(yy.tos); 

when  194 =>
--#line  1478

	
yyval := (One_List, Lists.Empty_List);
    

when  195 =>
--#line  1481

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  196 =>
--#line  1485

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  197 =>
--#line  1489

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  198 =>
--#line  1493

	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  199 =>
--#line  1499

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  200 =>
--#line  1505
 
yyval := 
yy.value_stack(yy.tos); 

when  201 =>
--#line  1506
 
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  202 =>
--#line  1509

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  203 =>
--#line  1516
 
yyval := 
yy.value_stack(yy.tos); 

when  204 =>
--#line  1517
 
yyval := 
yy.value_stack(yy.tos); 

when  205 =>
--#line  1518
 
yyval := 
yy.value_stack(yy.tos); 

when  206 =>
--#line  1522
 
yyval := 
yy.value_stack(yy.tos); 

when  207 =>
--#line  1523
 
yyval := 
yy.value_stack(yy.tos); 

when  208 =>
--#line  1524

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  209 =>
--#line  1531
 
yyval := 
yy.value_stack(yy.tos-1); 

when  210 =>
--#line  1532

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  211 =>
--#line  1538
 
yyval := 
yy.value_stack(yy.tos-1); 

when  212 =>
--#line  1539

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  213 =>
--#line  1546

        
yyval := 
yy.value_stack(yy.tos);
    

when  214 =>
--#line  1549

        --  A labeled annotation list becomes a separate nested annotation
        
yyval := (One_List, Lists.Make
                 ((1 => Annotation.Make
                   (Annotations => 
yy.value_stack(yy.tos).List, Label => 
yy.value_stack(yy.tos-1).Tree))));
    

when  215 =>
--#line  1559

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  216 =>
--#line  1562

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  217 =>
--#line  1566

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  218 =>
--#line  1572
 
yyval := 
yy.value_stack(yy.tos); 

when  219 =>
--#line  1573
 
yyval := 
yy.value_stack(yy.tos); 

when  220 =>
--#line  1574
 
yyval := 
yy.value_stack(yy.tos); 

when  221 =>
--#line  1575
 
yyval := 
yy.value_stack(yy.tos); 

when  222 =>
--#line  1576
 
yyval := 
yy.value_stack(yy.tos); 

when  223 =>
--#line  1577

	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  224 =>
--#line  1587
 
yyval := 
yy.value_stack(yy.tos); 

when  225 =>
--#line  1591
 
yyval := 
yy.value_stack(yy.tos); 

when  226 =>
--#line  1592
 
yyval := 
yy.value_stack(yy.tos); 

when  227 =>
--#line  1597

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-1).List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => 
yy.value_stack(yy.tos-4).Is_Abstract,
	  Is_Optional => 
yy.value_stack(yy.tos-4).Is_Optional,
	  Is_Queued => 
yy.value_stack(yy.tos-4).Is_Queued,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  228 =>
--#line  1613

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-4).List,
	  Operation_Outputs => 
yy.value_stack(yy.tos-1).List,
	  Preconditions => Annotation.Make (
yy.value_stack(yy.tos-3).List),
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => 
yy.value_stack(yy.tos-7).Is_Abstract,
	  Is_Optional => 
yy.value_stack(yy.tos-7).Is_Optional,
	  Is_Queued => 
yy.value_stack(yy.tos-7).Is_Queued,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  229 =>
--#line  1630
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Abstract => True, others => False); 
    

when  230 =>
--#line  1635
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Optional => True, others => False); 
    

when  231 =>
--#line  1640
 
yyval := (Construct_Qualifier,
             Source_Pos => PSC.Source_Positions.Null_Source_Position,
             others => False); 

when  232 =>
--#line  1646
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  233 =>
--#line  1651
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Optional => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  234 =>
--#line  1656
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  235 =>
--#line  1664

	
yyval := (Optional, Is_Present => True);
    

when  236 =>
--#line  1667

	
yyval := (Optional, Is_Present => False);
    

when  237 =>
--#line  1673
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  238 =>
--#line  1676

	yyerror("Operator designator must be in quotes");
	
yyval := 
yy.value_stack(yy.tos);
    

when  240 =>
--#line  1683

	yyerror("Use ""->"" in Parython rather than ""return""");
    

when  241 =>
--#line  1689

	
yyval := 
yy.value_stack(yy.tos);
	declare
	    Func_Tree : Operation.Tree renames 
	      Operation.Tree(Tree_Ptr_Of(
yyval.Tree).all);
	begin
	    Func_Tree.Is_Abstract := 
yy.value_stack(yy.tos-1).Is_Abstract;
	    Func_Tree.Is_Optional := 
yy.value_stack(yy.tos-1).Is_Optional;
	    Func_Tree.Is_Queued := 
yy.value_stack(yy.tos-1).Is_Queued;
	end;
    

when  242 =>
--#line  1703

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-1).List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  243 =>
--#line  1718

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-4).List,
	  Operation_Outputs => 
yy.value_stack(yy.tos-1).List,
	  Preconditions => Annotation.Make (
yy.value_stack(yy.tos-3).List),
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  244 =>
--#line  1735

        if Qualified_Name.Contains_String (
yy.value_stack(yy.tos).Tree) then
            yyerror("Should use ""op"" rather than ""func"" for an operator");
        end if;
	
yyval := 
yy.value_stack(yy.tos);
    

when  245 =>
--#line  1744
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  246 =>
--#line  1747

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  247 =>
--#line  1750

      declare
	Id_List : Lists.List := 
yy.value_stack(yy.tos-3).List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (One_List, Lists.Empty_List);
	Lists.Append(Id_List, 
yy.value_stack(yy.tos-1).Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    

when  248 =>
--#line  1769

	yyerror("Parython requires at least ""()"" in operation definition");
	
yyval := (One_List, Lists.Empty_List);
    

when  249 =>
--#line  1776
 
yyval := 
yy.value_stack(yy.tos); 

when  250 =>
--#line  1777

        yyerror ("Expecting one ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := (One_Token,
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.String_Lookup(")")); 
    

when  251 =>
--#line  1787

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-4).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  252 =>
--#line  1799

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  253 =>
--#line  1811
 
	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  254 =>
--#line  1823
 
yyval := 
yy.value_stack(yy.tos); 

when  255 =>
--#line  1824

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  256 =>
--#line  1832
 
yyval := 
yy.value_stack(yy.tos); 

when  257 =>
--#line  1833
 
yyval := 
yy.value_stack(yy.tos); 

when  258 =>
--#line  1834

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  259 =>
--#line  1839

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  260 =>
--#line  1844

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  261 =>
--#line  1849

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  262 =>
--#line  1854

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  263 =>
--#line  1859

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  264 =>
--#line  1864

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  265 =>
--#line  1872
 
yyval := 
yy.value_stack(yy.tos); 

when  266 =>
--#line  1873

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  267 =>
--#line  1881

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  268 =>
--#line  1886

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  269 =>
--#line  1891

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  270 =>
--#line  1899

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  271 =>
--#line  1904

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  272 =>
--#line  1912
 
yyval := 
yy.value_stack(yy.tos); 

when  273 =>
--#line  1913

	
yyval := (One_List, Lists.Empty_List);
    

when  274 =>
--#line  1919
 
yyval := 
yy.value_stack(yy.tos); 

when  275 =>
--#line  1920

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  276 =>
--#line  1927

	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Add annotations to last element of list
	    
yyval := 
yy.value_stack(yy.tos-1);
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	else
	    
yyval := 
yy.value_stack(yy.tos-1);
	end if;
    

when  277 =>
--#line  1937

	-- Add annotations to first/last element of list
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
	  Lists.Nth_Element(
yyval.List, 1), 
yy.value_stack(yy.tos-2).List, Precedes => True);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	end if;
    

when  278 =>
--#line  1959

	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-5).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-5).List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional,
              In_Region => 
yy.value_stack(yy.tos-1).Tree,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos-2).Tree, I)),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  279 =>
--#line  1977

	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-5).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-5).List, I),
	      Kind => 
yy.value_stack(yy.tos-6).Param_Kind,
	      Locking => 
yy.value_stack(yy.tos-6).Param_Locking,
	      Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional,
              In_Region => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I),
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos-2).Tree, I)),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  280 =>
--#line  1993

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  281 =>
--#line  2003

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-3).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-3).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos-1).Tree),
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  282 =>
--#line  2015

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  283 =>
--#line  2025

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos-1).Tree),
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  284 =>
--#line  2037

	
yyval := 
yy.value_stack(yy.tos-1);
	-- Set Is_Implicit_Module_Param on each parameter
	for I in 1..Lists.Length(
yyval.List) loop
	  declare
	    Param_Decl_Tree : Param_Decl.Tree renames
	      Param_Decl.Tree(Tree_Ptr_Of(Lists.Nth_Element(
yyval.List, I)).all);
	  begin
	    Param_Decl_Tree.Is_Implicit_Module_Param := True;
	  end;
	end loop;
    

when  285 =>
--#line  2052
 
yyval := 
yy.value_stack(yy.tos); 

when  286 =>
--#line  2053

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  287 =>
--#line  2061
 
yyval := 
yy.value_stack(yy.tos); 

when  288 =>
--#line  2062
 
yyval := 
yy.value_stack(yy.tos); 

when  289 =>
--#line  2066
 
yyval := 
yy.value_stack(yy.tos); 

when  290 =>
--#line  2067

         -- NOTE: Operation can have "type" parameters 
         -- such as "Left_Type is Integer<>"
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  291 =>
--#line  2075

        
yyval := 
yy.value_stack(yy.tos);
    

when  292 =>
--#line  2081

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  293 =>
--#line  2090
 
yyval := 
yy.value_stack(yy.tos); 

when  294 =>
--#line  2091

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  295 =>
--#line  2099

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  296 =>
--#line  2109
 
yyval := (Optional, Is_Present => True); 

when  297 =>
--#line  2110
 
yyval := (Optional, Is_Present => False); 

when  298 =>
--#line  2114

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  299 =>
--#line  2117

	Annotation.Add_Annotation(
yy.value_stack(yy.tos).Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  300 =>
--#line  2121

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  301 =>
--#line  2124

      declare
	Id_List : Lists.List := 
yy.value_stack(yy.tos-3).List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => 
yy.value_stack(yy.tos-3));
	
yyval := (One_List, Lists.Empty_List);
	Lists.Append(Id_List, 
yy.value_stack(yy.tos-1).Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    

when  302 =>
--#line  2143

      declare
	Id_List : Lists.List := 
yy.value_stack(yy.tos-3).List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (One_List, Lists.Empty_List);
	Lists.Append(Id_List, 
yy.value_stack(yy.tos-1).Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    

when  303 =>
--#line  2166

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-4).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  304 =>
--#line  2178

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  305 =>
--#line  2190

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-1).Param_Locking,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  306 =>
--#line  2200

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  307 =>
--#line  2212

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree));
    

when  308 =>
--#line  2224

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  309 =>
--#line  2236

	
yyval := 
yy.value_stack(yy.tos);
    

when  310 =>
--#line  2239

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  311 =>
--#line  2246

	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Add annotations to last element of list
	    
yyval := 
yy.value_stack(yy.tos-1);
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	else
	    
yyval := 
yy.value_stack(yy.tos-1);
	end if;
    

when  312 =>
--#line  2256

	-- Add annotations to first/last element of list
	
yyval := 
yy.value_stack(yy.tos-1);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, 1), 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	end if;
    

when  313 =>
--#line  2272

	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-3).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-3).List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)),
	      Param_Default => Null_Optional_Tree));
	end loop;
    

when  314 =>
--#line  2289

	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-3).List) loop
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-3).List, I),
	      Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	      Locking => 
yy.value_stack(yy.tos-4).Param_Locking,
	      Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
                 Operand => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)),
	      Param_Default => Null_Optional_Tree));
	end loop;
    

when  315 =>
--#line  2304

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  316 =>
--#line  2313

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree))));
    

when  317 =>
--#line  2325

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-1).Param_Locking,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  318 =>
--#line  2334

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional, others => False),
             Operand => 
yy.value_stack(yy.tos).Tree),
	  Param_Default => Null_Optional_Tree))));
    

when  319 =>
--#line  2350

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  320 =>
--#line  2362

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  321 =>
--#line  2373

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  322 =>
--#line  2386

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  323 =>
--#line  2398

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  324 =>
--#line  2410

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  325 =>
--#line  2422

	yyerror("Must specify ""var,"" ""const,"" or ""ref""",
          At_Token => 
yy.value_stack(yy.tos-3));
	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Obj_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  326 =>
--#line  2437
 
yyval := 
yy.value_stack(yy.tos); 

when  327 =>
--#line  2438

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  328 =>
--#line  2444
 
yyval := 
yy.value_stack(yy.tos); 

when  329 =>
--#line  2445

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  330 =>
--#line  2451
 
yyval := 
yy.value_stack(yy.tos); 

when  331 =>
--#line  2452

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  332 =>
--#line  2457
 
yyval := 
yy.value_stack(yy.tos-1); 

when  333 =>
--#line  2460

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  334 =>
--#line  2471

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  335 =>
--#line  2481

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  336 =>
--#line  2491

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  337 =>
--#line  2501

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  338 =>
--#line  2512

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  339 =>
--#line  2525
 
yyval := 
yy.value_stack(yy.tos); 

when  340 =>
--#line  2526
 
yyval := 
yy.value_stack(yy.tos); 

when  341 =>
--#line  2529
 
yyval := 
yy.value_stack(yy.tos); 

when  342 =>
--#line  2532

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_New_Type => 
yy.value_stack(yy.tos-1).Is_Present,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  343 =>
--#line  2540

	
yyval := (Optional, True);
    

when  344 =>
--#line  2543

	
yyval := (Optional, False);
    

when  345 =>
--#line  2549
 
yyval := 
yy.value_stack(yy.tos); 

when  346 =>
--#line  2550
 
yyval := 
yy.value_stack(yy.tos); 

when  347 =>
--#line  2556

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));

            if 
yy.value_stack(yy.tos).Check_Label then
	        Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  348 =>
--#line  2576

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  349 =>
--#line  2593

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-5).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := 
yy.value_stack(yy.tos-3).Tree;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  351 =>
--#line  2612

	yyerror("Should be 'end defop ""id""' not simply 'end ""id""'");
    

when  353 =>
--#line  2618

	yyerror("Should be ""end def <id>"" not simply ""end <id>""");
    

when  354 =>
--#line  2624
 
yyval := 
yy.value_stack(yy.tos); 

when  355 =>
--#line  2625
 
yyval := (One_Tree, Null_Optional_Tree); 

when  356 =>
--#line  2629
 
yyval := 
yy.value_stack(yy.tos); 

when  357 =>
--#line  2630
 
yyval := 
yy.value_stack(yy.tos); 

when  358 =>
--#line  2633
 
yyval := 
yy.value_stack(yy.tos-1); 

when  359 =>
--#line  2635
 
yyval := 
yy.value_stack(yy.tos-1); 

when  360 =>
--#line  2639

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree,
	  Complement => 
yy.value_stack(yy.tos-2).Is_While));  
	    -- Complement cond if "while" present
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  361 =>
--#line  2647
 
        --  Pop the indent stack
        if Parython_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        Parython_Lex.Top := Parython_Lex.Top - 1;
    

when  363 =>
--#line  2657

        yyerror ("Syntax error before ':'", At_Token => 
yy.value_stack(yy.tos));
    

when  367 =>
--#line  2667
 
yyval := 
yy.value_stack(yy.tos); 

when  368 =>
--#line  2668
 
yyval := 
yy.value_stack(yy.tos); 

when  369 =>
--#line  2669

        yyerror("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  370 =>
--#line  2673

        yyerror("Syntax error in condition", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  371 =>
--#line  2680

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  372 =>
--#line  2684

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  373 =>
--#line  2691

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  374 =>
--#line  2695

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  375 =>
--#line  2702

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  376 =>
--#line  2707
 
yyval := 
yy.value_stack(yy.tos); 

when  377 =>
--#line  2708
 
yyval := 
yy.value_stack(yy.tos); 

when  378 =>
--#line  2712
 
yyval := 
yy.value_stack(yy.tos); 

when  379 =>
--#line  2713

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  380 =>
--#line  2722
 
yyval := 
yy.value_stack(yy.tos); 

when  381 =>
--#line  2723

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  382 =>
--#line  2732
 
yyval := 
yy.value_stack(yy.tos); 

when  383 =>
--#line  2733
 
yyval := 
yy.value_stack(yy.tos); 

when  384 =>
--#line  2737
 
yyval := 
yy.value_stack(yy.tos); 

when  385 =>
--#line  2739

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  386 =>
--#line  2749

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  387 =>
--#line  2759

	-- "begin" is not used in Parython; treat like "then" for now
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  388 =>
--#line  2766

	-- "begin" is not used in Parython
	
yyval := 
yy.value_stack(yy.tos);
    

when  389 =>
--#line  2772

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  396 =>
--#line  2786

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  397 =>
--#line  2795

	yyerror("No need for ""begin"" in Parython operation definition");
    

when  398 =>
--#line  2801
 
yyval := 
yy.value_stack(yy.tos); 

when  399 =>
--#line  2803

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  400 =>
--#line  2813

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  401 =>
--#line  2825
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  402 =>
--#line  2828

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  403 =>
--#line  2835

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  404 =>
--#line  2844

        
yyval := 
yy.value_stack(yy.tos);
    

when  405 =>
--#line  2847

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  406 =>
--#line  2852
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  407 =>
--#line  2855

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  408 =>
--#line  2862

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  409 =>
--#line  2872
 
yyval := 
yy.value_stack(yy.tos); 

when  410 =>
--#line  2873

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  411 =>
--#line  2882
 
yyval := 
yy.value_stack(yy.tos); 

when  412 =>
--#line  2883

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  413 =>
--#line  2892

        
yyval := 
yy.value_stack(yy.tos);
    

when  414 =>
--#line  2895

        
yyval := 
yy.value_stack(yy.tos);
    

when  415 =>
--#line  2901

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  416 =>
--#line  2907

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  417 =>
--#line  2912

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  418 =>
--#line  2916

	-- An annotation can appear by itself
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  419 =>
--#line  2923

            -- NOTE: these already allow trailing annotations
	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  420 =>
--#line  2928

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  421 =>
--#line  2932
 
yyval := 
yy.value_stack(yy.tos); 

when  422 =>
--#line  2934

	
yyval := 
yy.value_stack(yy.tos);
    

when  423 =>
--#line  2940
 
yyval := 
yy.value_stack(yy.tos-1); 

when  424 =>
--#line  2943
 
yyval := 
yy.value_stack(yy.tos); 

when  425 =>
--#line  2944
 
	if Not_Null(
yy.value_stack(yy.tos).Tree) then
	    -- Stmt might be null if there was an error.
	    Check_Stmt_Label(
	      Compound_Stmt => 
yy.value_stack(yy.tos), Start_Label_Token => 
yy.value_stack(yy.tos-1));
	end if;

	
yyval := 
yy.value_stack(yy.tos);
    

when  426 =>
--#line  2953
 
	if Not_Null(
yy.value_stack(yy.tos).Tree) then
	    -- Stmt might be null if there was an error.
	    Check_Stmt_Label(
	      Compound_Stmt => 
yy.value_stack(yy.tos),
                Start_Label_Token => (Optional, Is_Present => False));
	end if;

	
yyval := 
yy.value_stack(yy.tos); 
    

when  427 =>
--#line  2966

	
yyval := 
yy.value_stack(yy.tos);
  

when  428 =>
--#line  2969

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  429 =>
--#line  2975

	-- multiple assignment 
	-- NOTE: Using "opt_operation_actual_list" rather 
	--       than "operation_actual_list" to avoid ambiguity
	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => Invocation.Make(
	    Kind => Invocation.Class_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => 
yy.value_stack(yy.tos-3).List,
	    Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  430 =>
--#line  2988
 
	-- A "null" statement (i.e. a no-op)
	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Null_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => 
yy.value_stack(yy.tos).Source_Pos));
    

when  431 =>
--#line  2997

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  432 =>
--#line  3003
 
yyval := 
yy.value_stack(yy.tos); 

when  433 =>
--#line  3004

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Continue_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => 
yy.value_stack(yy.tos-1).Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos));
    

when  434 =>
--#line  3012

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Exit_Stmt,
	  Applies_To => 
yy.value_stack(yy.tos-2).Exitable_Construct,
	  Id => 
yy.value_stack(yy.tos-1).Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos));
    

when  435 =>
--#line  3020

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos-1);
    

when  436 =>
--#line  3027

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  437 =>
--#line  3037

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  438 =>
--#line  3045

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  439 =>
--#line  3056
 
yyval := 
yy.value_stack(yy.tos); 

when  440 =>
--#line  3057

	yyerror("""loop"" required after ""continue""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  441 =>
--#line  3066

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => 
yy.value_stack(yy.tos-1).Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  442 =>
--#line  3072

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Divide_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  443 =>
--#line  3078

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Combine_Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  444 =>
--#line  3084

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  445 =>
--#line  3090

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Swap_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  446 =>
--#line  3096

	-- multiple assignment 
	-- NOTE: Using "opt_operation_actual_list" rather 
	--       than "operation_actual_list" to avoid ambiguity
	yyerror("Use ""="" rather than "":="" in Parython");
	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => Invocation.Make(
	    Kind => Invocation.Class_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => 
yy.value_stack(yy.tos-3).List,
	    Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  447 =>
--#line  3113
 
yyval := 
yy.value_stack(yy.tos); 

when  448 =>
--#line  3114

	
yyval := (One_List, Lists.Empty_List);
    

when  449 =>
--#line  3120
 
yyval := 
yy.value_stack(yy.tos); 

when  450 =>
--#line  3121

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  451 =>
--#line  3126

	-- NOTE: This used to be '(' operation_actual_list ')'
	--       but that prevented continuing with a single expression.
	
yyval := 
yy.value_stack(yy.tos);
    

when  452 =>
--#line  3133
 
yyval := 
yy.value_stack(yy.tos); 

when  453 =>
--#line  3134

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  454 =>
--#line  3140
 
yyval := 
yy.value_stack(yy.tos); 

when  455 =>
--#line  3141
 
	yyerror(
	  """loop,"" ""if,"" ""case,"" or ""block"" must follow ""exit""");
	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  456 =>
--#line  3149

	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  457 =>
--#line  3152

	
yyval := (Construct_Kind, Control_Stmt.If_Stmt);
    

when  458 =>
--#line  3155

	
yyval := (Construct_Kind, Control_Stmt.Case_Stmt);
    

when  459 =>
--#line  3158

	
yyval := (Construct_Kind, Control_Stmt.Block_Stmt);
    

when  460 =>
--#line  3163

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  461 =>
--#line  3173
 
yyval := 
yy.value_stack(yy.tos); 

when  462 =>
--#line  3174
 
yyval := 
yy.value_stack(yy.tos); 

when  463 =>
--#line  3175
 
yyval := 
yy.value_stack(yy.tos); 

when  464 =>
--#line  3179
 
yyval := 
yy.value_stack(yy.tos); 

when  465 =>
--#line  3180
 
yyval := 
yy.value_stack(yy.tos); 

when  466 =>
--#line  3181
 
yyval := 
yy.value_stack(yy.tos-1); 

when  467 =>
--#line  3184
 
yyval := 
yy.value_stack(yy.tos); 

when  468 =>
--#line  3185
 
yyval := 
yy.value_stack(yy.tos-1); 

when  469 =>
--#line  3186
 
yyval := 
yy.value_stack(yy.tos-1); 

when  470 =>
--#line  3189
 
yyval := 
yy.value_stack(yy.tos-1); 

when  471 =>
--#line  3192
 
yyval := 
yy.value_stack(yy.tos); 

when  472 =>
--#line  3193
 
yyval := 
yy.value_stack(yy.tos); 

when  473 =>
--#line  3194
 
yyval := 
yy.value_stack(yy.tos); 

when  474 =>
--#line  3195
 
yyval := 
yy.value_stack(yy.tos); 

when  475 =>
--#line  3196
 
yyval := 
yy.value_stack(yy.tos); 

when  476 =>
--#line  3197
 
yyval := 
yy.value_stack(yy.tos); 

when  477 =>
--#line  3198
 
yyval := (One_Tree, Null_Optional_Tree); 

when  478 =>
--#line  3204

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-4).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-2).Tree,
	  Else_Part => 
yy.value_stack(yy.tos-1).Tree,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
	  Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  479 =>
--#line  3219

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  480 =>
--#line  3227

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  481 =>
--#line  3230

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  482 =>
--#line  3235
 
yyval := 
yy.value_stack(yy.tos); 

when  487 =>
--#line  3246

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  488 =>
--#line  3250

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  489 =>
--#line  3260

	declare
	    Case_Alt_List : Lists.List := 
yy.value_stack(yy.tos-3).List;
	begin
	    if Not_Null(
yy.value_stack(yy.tos-2).Tree) then
		Lists.Append(Case_Alt_List, 
yy.value_stack(yy.tos-2).Tree);
	    end if;
	    
yyval := (One_Tree, Case_Construct.Make(
              Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos,
	      Case_Selector => 
yy.value_stack(yy.tos-5).Tree,
	      Case_Alt_List => Case_Alt_List,
	      End_With_Values => Null_Optional_Tree,
	      Check_Label => False,
              Label => Null_Optional_Tree));
	end;
    

when  490 =>
--#line  3279

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  491 =>
--#line  3283

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  492 =>
--#line  3290

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  493 =>
--#line  3293

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  494 =>
--#line  3301

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  495 =>
--#line  3310
 
yyval := 
yy.value_stack(yy.tos); 

when  496 =>
--#line  3311
 
yyval := 
yy.value_stack(yy.tos); 

when  497 =>
--#line  3315
 
yyval := 
yy.value_stack(yy.tos); 

when  498 =>
--#line  3316

	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-2).Tree,
          Param_Default => Null_Optional_Tree));
     

when  499 =>
--#line  3329

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 =>
              Binary.Make(Binary.Closed_Interval_Op,
                Left_Operand => Null_Optional_Tree,
                Right_Operand => Null_Optional_Tree)))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  500 =>
--#line  3340

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make (Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 =>
              Param_Decl.Make(
                Name => 
yy.value_stack(yy.tos-2).Tree,
                Kind => Param_Decl.Default_Param,
                Locking => Param_Decl.Not_Locked,
                Is_Optional => False,
                Param_Type => Binary.Make(Binary.Closed_Interval_Op,
                   Left_Operand => Null_Optional_Tree,
                   Right_Operand => Null_Optional_Tree),
                Param_Default => Null_Optional_Tree)))),
	  Referent => 
yy.value_stack(yy.tos-1).Tree));
    

when  501 =>
--#line  3356

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  502 =>
--#line  3362
 
yyval := 
yy.value_stack(yy.tos-1); 

when  503 =>
--#line  3363
 
yyval := 
yy.value_stack(yy.tos-2); 

when  504 =>
--#line  3367

	
yyval := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree));
     

when  505 =>
--#line  3377

	
yyval := (One_Tree, While_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  While_Cond => Null_Optional_Tree,
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
          Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  506 =>
--#line  3391

	
yyval := (One_Tree, While_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	  While_Cond => Conditionally_Complement(
yy.value_stack(yy.tos-4).Tree,
	    Complement => 
yy.value_stack(yy.tos-5).Is_Until),
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
          Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  507 =>
--#line  3404
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_While => True, others => False); 

when  508 =>
--#line  3407
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_Until => True, others => False); 

when  511 =>
--#line  3415
 
yyval := 
yy.value_stack(yy.tos); 

when  512 =>
--#line  3416
 
yyval := 
yy.value_stack(yy.tos); 

when  513 =>
--#line  3417

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  514 =>
--#line  3421

        yyerror ("Syntax error in loop header", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  515 =>
--#line  3428

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  516 =>
--#line  3432

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  517 =>
--#line  3443

	
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-7).Source_Pos,
	  Kind => For_Loop_Construct.For_Loop_Statement,
	  Iterators => 
yy.value_stack(yy.tos-6).List,
	  Filter => 
yy.value_stack(yy.tos-5).List,
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  Direction => 
yy.value_stack(yy.tos-4).Str,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
          Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  518 =>
--#line  3458

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  519 =>
--#line  3461
 
yyval := 
yy.value_stack(yy.tos-1); 

when  520 =>
--#line  3465

	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := 
yy.value_stack(yy.tos-1).Tree;
	begin
	    if 
yy.value_stack(yy.tos).Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, 
yy.value_stack(yy.tos).Str);
	    end if;
	    
yyval := (One_List, Lists.Make((1 => Iterator_Tree)));
	end;
    

when  521 =>
--#line  3476

	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : Optional_Tree := 
yy.value_stack(yy.tos-1).Tree;
	begin
	    if 
yy.value_stack(yy.tos).Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, 
yy.value_stack(yy.tos).Str);
	    end if;
	    
yyval := 
yy.value_stack(yy.tos-3);
	    Lists.Append(
yyval.List, Iterator_Tree);
	end;
    

when  522 =>
--#line  3488

	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := 
yy.value_stack(yy.tos-1).Tree;
	begin
	    yyerror("Iterators must be separated by "";""",
              At_Token => 
yy.value_stack(yy.tos-2));
	    if 
yy.value_stack(yy.tos).Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, 
yy.value_stack(yy.tos).Str);
	    end if;
	    
yyval := 
yy.value_stack(yy.tos-3);
	    Lists.Append(
yyval.List, Iterator_Tree);
	end;
    

when  523 =>
--#line  3505
 
yyval := 
yy.value_stack(yy.tos); 

when  524 =>
--#line  3506

	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  525 =>
--#line  3514
 
yyval := 
yy.value_stack(yy.tos); 

when  526 =>
--#line  3515
 
	yyerror("Use ""for ..."" or ""for each ..."" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  527 =>
--#line  3521
 
yyval := 
yy.value_stack(yy.tos); 

when  528 =>
--#line  3522
 
	yyerror("""for-each"" iterator uses ""of"" rather than ""in""",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  529 =>
--#line  3527
 
	yyerror("Use ""for each ..."" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  530 =>
--#line  3533
 
	yyerror("Missing ""each"" in container element ""for-each"" iterator",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  531 =>
--#line  3538
 
yyval := 
yy.value_stack(yy.tos); 

when  532 =>
--#line  3539
 
yyval := 
yy.value_stack(yy.tos); 

when  533 =>
--#line  3543

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  535 =>
--#line  3553

	yyerror("The ""reverse"" keyword goes immediately before ""loop""");
    

when  536 =>
--#line  3558

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  537 =>
--#line  3566

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Key_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree,
	  Key_Name => 
yy.value_stack(yy.tos-5).Tree));
    

when  538 =>
--#line  3577

        if Parython_Lex.Debug_Indent
          and then Parython_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        Parython_Lex.Expecting_Indent := False;
    

when  539 =>
--#line  3588

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-6).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-5).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  540 =>
--#line  3599

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-6).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-5).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  541 =>
--#line  3612

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  542 =>
--#line  3621

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  543 =>
--#line  3633

	
yyval := 
yy.value_stack(yy.tos);
    

when  544 =>
--#line  3636

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  545 =>
--#line  3642
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  546 =>
--#line  3645

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  547 =>
--#line  3652
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  548 =>
--#line  3655

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  549 =>
--#line  3662

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree, Complement => 
yy.value_stack(yy.tos-1).Is_Until));
	    -- Complement condition if used "until"
    

when  550 =>
--#line  3667

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  551 =>
--#line  3672
 
yyval := 
yy.value_stack(yy.tos); 

when  552 =>
--#line  3673
 
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  553 =>
--#line  3681

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("concurrent"));
    

when  554 =>
--#line  3685
 
yyval := 
yy.value_stack(yy.tos); 

when  555 =>
--#line  3689

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("forward"));
    

when  556 =>
--#line  3693

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("reverse"));
    

when  557 =>
--#line  3702

	
yyval := (One_Tree, Block_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Block_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
          Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  559 =>
--#line  3713

	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
    

when  560 =>
--#line  3719

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  561 =>
--#line  3723

        
yyval := (Optional_End_Token, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  562 =>
--#line  3730

	
yyval := 
yy.value_stack(yy.tos);
    

when  563 =>
--#line  3733
 
	-- Error recovery
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  564 =>
--#line  3743
 
	yyerror("Use ""!="" rather than ""/="" in Parython");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op);
    

when  565 =>
--#line  3750
 
yyval := 
yy.value_stack(yy.tos); 

when  566 =>
--#line  3753

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Quest_Colon,
          Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-4).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-2).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  567 =>
--#line  3761
 
yyval := 
yy.value_stack(yy.tos); 

when  568 =>
--#line  3766

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Lambda_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Def => True,
	  Statements => 
yy.value_stack(yy.tos).Tree)); 
    

when  569 =>
--#line  3781

	
yyval := (One_List, Lists.Empty_List);
    

when  570 =>
--#line  3784

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  571 =>
--#line  3787

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  572 =>
--#line  3792

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  573 =>
--#line  3795

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  574 =>
--#line  3801

        
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => Null_Optional_Tree,
          Param_Default => Null_Optional_Tree));
    

when  575 =>
--#line  3813
 
yyval := 
yy.value_stack(yy.tos); 

when  576 =>
--#line  3814

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  577 =>
--#line  3822

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  578 =>
--#line  3828

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
     

when  579 =>
--#line  3837
 
yyval := 
yy.value_stack(yy.tos); 

when  580 =>
--#line  3838
 
yyval := 
yy.value_stack(yy.tos); 

when  581 =>
--#line  3839
 
yyval := 
yy.value_stack(yy.tos); 

when  582 =>
--#line  3843
 
yyval := 
yy.value_stack(yy.tos); 

when  583 =>
--#line  3844

      declare
	Left_Tree : PSC.Trees.Tree'Class renames Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all;
	use type Binary.Binary_Operator_Enum;
      begin
	if Left_Tree in Binary.Tree'Class and then
	  Binary.Tree(Left_Tree).Operator in Binary.Logical_Ops then
	    if Binary.Tree(Left_Tree).Operator /= 
yy.value_stack(yy.tos-1).Binary_Op then
		-- logical operators are associative only with same op
		yyerror(
		  "must use parentheses in sequence of " &
		    "distinct logical operators",
                  At_Token => 
yy.value_stack(yy.tos-1));
	    elsif 
yy.value_stack(yy.tos-1).Binary_Op = Binary.Implies_Op then
		-- Implication should associate right-to-left but that
		-- is too confusing.
		yyerror(
		  "must use parentheses in sequence of implication operators",
                  At_Token => 
yy.value_stack(yy.tos-1));
	    end if;
	end if;

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
      end;
    

when  584 =>
--#line  3875
 
yyval := 
yy.value_stack(yy.tos); 

when  585 =>
--#line  3876

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  586 =>
--#line  3882

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  587 =>
--#line  3888

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  588 =>
--#line  3894

	
yyval := (One_Tree, Unary.Make(
	  Operator => Unary.Is_Null_Op,
	  Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  589 =>
--#line  3899

	-- We use adding_expression before "NOT" instead of simple_expression
	-- to avoid ambiguity associated with polymorphic type names
        -- (which are included in simple_expression but not adding_expression):
	--    Integer+ not null 
	-- could be interpreted as:
	--    Integer + not(null)
	
yyval := (One_Tree, Unary.Make(
	  Operator => Unary.Not_Null_Op,
	  Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  590 =>
--#line  3910

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Is_Function_Of,
	  Prefix => 
yy.value_stack(yy.tos-5).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  591 =>
--#line  3919
 
yyval := 
yy.value_stack(yy.tos); 

when  592 =>
--#line  3920

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  593 =>
--#line  3929
 
yyval := 
yy.value_stack(yy.tos); 

when  594 =>
--#line  3930
 
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  595 =>
--#line  3953
 
yyval := 
yy.value_stack(yy.tos); 

when  596 =>
--#line  3954

        --  NOTE: We treat '+' here separately to avoid
        --        reduce/reduce conflicts
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Plus_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  597 =>
--#line  3962

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  598 =>
--#line  3971
 
yyval := 
yy.value_stack(yy.tos); 

when  599 =>
--#line  3972

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  600 =>
--#line  3981
 
yyval := 
yy.value_stack(yy.tos); 

when  601 =>
--#line  3982

	 -- right associative
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  602 =>
--#line  3989

	-- unary ops have higher precedence 
	-- than every operator except the power_operator.
	
yyval := (One_Tree, Unary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Unary_Op,
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  603 =>
--#line  3999
 
yyval := 
yy.value_stack(yy.tos); 

when  604 =>
--#line  4000
 
yyval := 
yy.value_stack(yy.tos); 

when  605 =>
--#line  4001
 
yyval := 
yy.value_stack(yy.tos-1); 

when  606 =>
--#line  4002
 
yyval := 
yy.value_stack(yy.tos-1); 

when  607 =>
--#line  4003
 
yyval := 
yy.value_stack(yy.tos-1); 

when  608 =>
--#line  4004

        
yyval := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  609 =>
--#line  4008
 
yyval := 
yy.value_stack(yy.tos); 

when  610 =>
--#line  4009

        --  This is used in a map_reduce expression to specify the initial val
        
yyval := (One_Tree, Unary.Make(Unary.Initial_Value_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  611 =>
--#line  4017
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  612 =>
--#line  4020
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  613 =>
--#line  4023
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  614 =>
--#line  4026
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  615 =>
--#line  4029
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make("null", 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  616 =>
--#line  4035
 
	if 
yy.value_stack(yy.tos).Is_Present then
	    
yyval := (One_Tree, Unary.Make(Unary.Updated_Value_Op,
	      Operand => 
yy.value_stack(yy.tos-1).Tree));
	else
	    
yyval := 
yy.value_stack(yy.tos-1); 
	end if;
    

when  617 =>
--#line  4043

	-- Use "::" to specify type of literal and
	-- to disambiguate operator specified as a string.
	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  618 =>
--#line  4050

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  619 =>
--#line  4056

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  620 =>
--#line  4062

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  621 =>
--#line  4068

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  622 =>
--#line  4076
 
yyval := 
yy.value_stack(yy.tos); 

when  623 =>
--#line  4077

	
yyval := (One_Tree, Property.Make(Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Property_Id => PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)));
    

when  624 =>
--#line  4084
 
yyval := (Optional, True); 

when  625 =>
--#line  4085

	yyerror("Use ""#"" instead of ""'"" to query property in Parython",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := (Optional, True);
    

when  626 =>
--#line  4090
 
yyval := (Optional, False); 

when  627 =>
--#line  4094

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  628 =>
--#line  4097

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  629 =>
--#line  4104
 
yyval := 
yy.value_stack(yy.tos); 

when  630 =>
--#line  4105

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  631 =>
--#line  4112
 
yyval := 
yy.value_stack(yy.tos); 

when  632 =>
--#line  4115
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  633 =>
--#line  4116
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  634 =>
--#line  4117
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Abs_Op); 

when  635 =>
--#line  4118
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Not_Op); 

when  636 =>
--#line  4119
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  637 =>
--#line  4120
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  638 =>
--#line  4124
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  639 =>
--#line  4125
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Plus_Op); 

when  640 =>
--#line  4126
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  641 =>
--#line  4130
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Times_Op); 

when  642 =>
--#line  4131
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Divide_Op); 

when  643 =>
--#line  4132
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Mod_Op); 

when  644 =>
--#line  4133
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Rem_Op); 

when  645 =>
--#line  4136
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Power_Op); 
    

when  646 =>
--#line  4140
 
yyval := 
yy.value_stack(yy.tos); 

when  647 =>
--#line  4141
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Divide_Assign_Op); 
    

when  648 =>
--#line  4147

	yyerror("Use ""="" rather than "":="" in Parython");
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
     

when  649 =>
--#line  4151
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Plus_Assign_Op); 
    

when  650 =>
--#line  4154
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Minus_Assign_Op); 
    

when  651 =>
--#line  4157
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Times_Assign_Op); 
    

when  652 =>
--#line  4160
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Power_Assign_Op); 
    

when  653 =>
--#line  4163
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Combine_Assign_Op); 
    

when  654 =>
--#line  4166
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.And_Assign_Op); 
    

when  655 =>
--#line  4169
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Or_Assign_Op); 
    

when  656 =>
--#line  4172
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Xor_Assign_Op); 
    

when  657 =>
--#line  4175

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Left_Shift_Assign_Op);
    

when  658 =>
--#line  4178

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Right_Shift_Assign_Op);
    

when  659 =>
--#line  4183
 
	yyerror("Use ""="" rather than "":="" in Parython");
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  660 =>
--#line  4187
 
yyval := 
yy.value_stack(yy.tos); 

when  661 =>
--#line  4190

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  662 =>
--#line  4196
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Compare_Op); 

when  663 =>
--#line  4197
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op); 

when  664 =>
--#line  4198
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op); 

when  665 =>
--#line  4199
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Less_Op); 

when  666 =>
--#line  4200
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.LEQ_Op); 

when  667 =>
--#line  4201
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Greater_Op); 

when  668 =>
--#line  4202
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.GEQ_Op); 

when  669 =>
--#line  4203
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Left_Shift_Op ); 

when  670 =>
--#line  4204
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Right_Shift_Op); 

when  671 =>
--#line  4205
 
	yyerror("Use ""=="" rather than ""="" in Parython");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op);
    

when  672 =>
--#line  4212
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.And_Op); 

when  673 =>
--#line  4213
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Or_Op); 

when  674 =>
--#line  4214
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Xor_Op); 

when  675 =>
--#line  4216
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.And_Then_Op); 

when  676 =>
--#line  4218
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Or_Else_Op); 

when  677 =>
--#line  4219
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Implies_Op); 

when  678 =>
--#line  4223
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Interval_Op); 
    

when  679 =>
--#line  4226
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Interval_Op); 
    

when  680 =>
--#line  4229
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Open_Interval_Op); 
    

when  681 =>
--#line  4232
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Closed_Interval_Op); 
    

when  682 =>
--#line  4238
 
yyval := 
yy.value_stack(yy.tos); 

when  683 =>
--#line  4239
 
yyval := 
yy.value_stack(yy.tos); 

when  684 =>
--#line  4240
 
yyval := 
yy.value_stack(yy.tos); 

when  685 =>
--#line  4244

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  686 =>
--#line  4251

	-- Error recovery
	yyerror("Use ""!="" rather than ""/="" in Parython",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => Binary.Make(
	    Operator => Binary.NEQ_Op,
	    Left_Operand => 
yy.value_stack(yy.tos-4).Tree,
	    Right_Operand => 
yy.value_stack(yy.tos-2).Tree)))));
     

when  687 =>
--#line  4263

	-- Type of aggregate specified
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => 
yy.value_stack(yy.tos-4).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  688 =>
--#line  4274

	
yyval := 
yy.value_stack(yy.tos);
    

when  689 =>
--#line  4277

	
yyval := (One_List, Lists.Empty_List);
    

when  690 =>
--#line  4283

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  691 =>
--#line  4286

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  692 =>
--#line  4293
 
yyval := 
yy.value_stack(yy.tos); 

when  693 =>
--#line  4294

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  694 =>
--#line  4299

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  695 =>
--#line  4309

      declare
	use type Invocation.Invocation_Kind_Enum;
	Is_Double_Bracket_Special_Case : Boolean := False;
      begin
-- TBD: The following fails at execution time due to a master
--      being init'ed in a short-circuit, and then finalized
--      outside the short-circuit:
-- 	if Lists.Length($2.List) = 1 and then
-- 	  Tree_Of(Lists.Nth_Element($2.List, 1)) in Invocation.Tree and then
-- 	  Invocation.Tree(Tree_Of(Lists.Nth_Element($2.List, 1))).Kind = 
-- 	    Invocation.Container_Aggregate and then
-- 	  Is_Null(Invocation.Tree(
-- 	    Tree_Of(Lists.Nth_Element($2.List, 1))).Prefix) then

	if Lists.Length(
yy.value_stack(yy.tos-1).List) = 1 then
	  declare
	    Solo_Operand : PSC.Trees.Tree'Class renames 
	      Tree_Ptr_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-1).List, 1)).all;
	  begin
	    if Solo_Operand in Invocation.Tree
              and then Invocation.Tree (Solo_Operand).Kind = 
		Invocation.Container_Aggregate
              and then
                Lists.Length (Invocation.Tree (Solo_Operand).Operands) = 1
              and then Is_Null(Invocation.Tree (Solo_Operand).Prefix)
            then
		-- We have the special case of "[[...]]"
		Is_Double_Bracket_Special_Case := True;
	    end if;
	  end;
	end if;

	if Is_Double_Bracket_Special_Case then

	    -- [[...]] is a special case, and invokes the "meaning" op.
	    -- TBD: Should we recognize this later as part of name resolution?

	    
yyval := (One_Tree, Unary.Make(Unary.Meaning_Op,
	      Operand => 
		Lists.Nth_Element(Invocation.Tree(
		  Tree_Ptr_Of
                    (Lists.Nth_Element(
yy.value_stack(yy.tos-1).List, 1)).all).Operands, 1)));
	else
	    -- Normal case of [...], create an invocation node.
	    
yyval := (One_Tree, Invocation.Make(
	      Kind => Invocation.Container_Aggregate,
	      Prefix => Null_Optional_Tree,
	      Operands => 
yy.value_stack(yy.tos-1).List,
	      Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
	end if;
      end;
    

when  696 =>
--#line  4362

	-- Type of result specified
      declare
	use type Invocation.Invocation_Kind_Enum;
	Is_Double_Bracket_Special_Case : Boolean := False;
      begin
	if Lists.Length(
yy.value_stack(yy.tos-1).List) = 1 then
	  declare
	    Solo_Operand : PSC.Trees.Tree'Class renames 
	      Tree_Ptr_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-1).List, 1)).all;
	  begin
	    if Solo_Operand in Invocation.Tree
              and then
	        Invocation.Tree(Solo_Operand).Kind = 
		  Invocation.Container_Aggregate
              and then
                Lists.Length (Invocation.Tree (Solo_Operand).Operands) = 1
              and then
	        Is_Null(Invocation.Tree(Solo_Operand).Prefix)
            then
		-- We have the special case of "[[...]]"
		Is_Double_Bracket_Special_Case := True;
	    end if;
	  end;
	end if;

	if Is_Double_Bracket_Special_Case then

	    -- Type::[[...]] invokes the (binary) "meaning" op.

	    
yyval := (One_Tree, Binary.Make(Binary.Meaning_Op,
	      Left_Operand => 
yy.value_stack(yy.tos-4).Tree,
	      Right_Operand => 
		Lists.Nth_Element(Invocation.Tree(
		  Tree_Ptr_Of
                    (Lists.Nth_Element(
yy.value_stack(yy.tos-1).List, 1)).all).Operands, 1)));
	else
	    -- Normal case of Type::[...], create an invocation node.
	    
yyval := (One_Tree, Invocation.Make(
	      Kind => Invocation.Container_Aggregate,
	      Prefix => 
yy.value_stack(yy.tos-4).Tree,
	      Operands => 
yy.value_stack(yy.tos-1).List,
              Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
	end if;
      end;
    

when  697 =>
--#line  4411
 
yyval := 
yy.value_stack(yy.tos); 

when  698 =>
--#line  4412

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  699 =>
--#line  4415

	
yyval := (One_List, Lists.Empty_List);
    

when  700 =>
--#line  4421

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  701 =>
--#line  4424

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  702 =>
--#line  4431
 
yyval := 
yy.value_stack(yy.tos); 

when  703 =>
--#line  4432

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  704 =>
--#line  4437

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  705 =>
--#line  4443

	-- This gives an ID to an expression which can be used
	-- to parameterize the initial value expression for each element.
      declare
        Value : constant Optional_Tree := 
yy.value_stack(yy.tos-4).Tree;
        use type PSC.Strings.U_String;
      begin
        if 
yy.value_stack(yy.tos).Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("forward" is the default)
            Iterator.Add_Direction(
yy.value_stack(yy.tos-2).Tree, 
yy.value_stack(yy.tos).Str);
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
          Kind => For_Loop_Construct.Container_Comprehension,
          Iterators => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
          Filter => 
yy.value_stack(yy.tos-1).List,
          Loop_Body => Value));
      end;
    

when  706 =>
--#line  4465

	-- This gives an ID to an expression which can be used
	-- to parameterize the initial value expression for each element.
      declare
        Value : constant Optional_Tree := Reference.Make(
          Key => 
yy.value_stack(yy.tos-6).Tree, Referent => 
yy.value_stack(yy.tos-4).Tree);
        use type PSC.Strings.U_String;
      begin

        if 
yy.value_stack(yy.tos).Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("forward" is the default)
            Iterator.Add_Direction(
yy.value_stack(yy.tos-2).Tree, 
yy.value_stack(yy.tos).Str);
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
          Kind => For_Loop_Construct.Container_Comprehension,
          Iterators => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
          Filter => 
yy.value_stack(yy.tos-1).List,
          Loop_Body => Value));
      end;
    

when  707 =>
--#line  4490

        
yyval := (One_Tree, Null_Optional_Tree);
    

when  708 =>
--#line  4494

        
yyval := 
yy.value_stack(yy.tos);
    

when  709 =>
--#line  4500

      declare
	use type Invocation.Invocation_Kind_Enum;
      begin

        -- {...}, create an invocation node.
        
yyval := (One_Tree, Invocation.Make(
          Kind => Invocation.Map_Set_Aggregate,
          Prefix => Null_Optional_Tree,
          Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
      end;
    

when  710 =>
--#line  4513

	-- Type of result specified
      declare
	use type Invocation.Invocation_Kind_Enum;
      begin
	-- Type::{...}, create an invocation node.
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Map_Set_Aggregate,
	  Prefix => 
yy.value_stack(yy.tos-4).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
      end;
    

when  711 =>
--#line  4529
 
yyval := 
yy.value_stack(yy.tos); 

when  712 =>
--#line  4530
 
yyval := 
yy.value_stack(yy.tos); 

when  713 =>
--#line  4536

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.If_Expr,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos);
    

when  714 =>
--#line  4548

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Expr,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos);
    

when  715 =>
--#line  4556

	
yyval := 
yy.value_stack(yy.tos);
    

when  716 =>
--#line  4559

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  717 =>
--#line  4566
 
yyval := 
yy.value_stack(yy.tos); 

when  718 =>
--#line  4567
 
yyval := 
yy.value_stack(yy.tos); 

when  719 =>
--#line  4568

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  720 =>
--#line  4572

        yyerror ("Syntax error in condition", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  721 =>
--#line  4580

	
yyval := (One_Tree, Case_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
	  Case_Selector => 
yy.value_stack(yy.tos-2).Tree,
	  Case_Alt_List => 
yy.value_stack(yy.tos).List,
          Is_Case_Expr => True));
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  722 =>
--#line  4591

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  723 =>
--#line  4594

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  724 =>
--#line  4601

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  725 =>
--#line  4608

	-- NOTE: default alternative must come last
	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
              Binary.Make(Binary.Closed_Interval_Op,
                Left_Operand => Null_Optional_Tree,
                Right_Operand => Null_Optional_Tree)))),
          Referent => 
yy.value_stack(yy.tos).Tree));
    

when  726 =>
--#line  4623

	declare
	    Kind_Of_For_Loop: constant array(Boolean) of 
	      For_Loop_Construct.For_Loop_Kind_Enum := (
		False => For_Loop_Construct.Existential_Quantified_Expr,
		True => For_Loop_Construct.Univ_Quantified_Expr);
	begin
	    
yyval := (One_Tree, For_Loop_Construct.Make(
              Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	      Kind => Kind_Of_For_Loop(
yy.value_stack(yy.tos-4).Is_Present),
	      Iterators => Lists.Make((1 => 
yy.value_stack(yy.tos-3).Tree)),
	      Filter => 
yy.value_stack(yy.tos-2).List,
	      Loop_Body => 
yy.value_stack(yy.tos).Tree));
            Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos);
	end;
    

when  727 =>
--#line  4640

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-2).Tree;
     begin
        if Is_Null (Obj_Type) then
           --  Presume id is the type name when not both specified.
           Obj_Type := 
yy.value_stack(yy.tos-3).Tree;
        end if;

        if not 
yy.value_stack(yy.tos-4).Is_Present then
           yyerror ("Must specify ""for all [E : ] T"" or " &
             """for all/some E in/of Container"" in quantified expression",
             At_Token => 
yy.value_stack(yy.tos-3));
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
          Kind => For_Loop_Construct.Univ_Quantified_Expr,
          Iterators => Lists.Make((1 =>
            Iterator.Make(
              Kind => Iterator.Set_Iterator,
              Name => 
yy.value_stack(yy.tos-3).Tree,
              Is_Ref => False,
              Obj_Type => Obj_Type,
              Obj_Value => Null_Optional_Tree))),
          Filter => Lists.Empty_List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos);
     end;
    

when  728 =>
--#line  4674

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-3).Tree;
     begin
        if Is_Null (Obj_Type) then
           --  Presume id is the type name when not both specified.
           Obj_Type := 
yy.value_stack(yy.tos-4).Tree;
        end if;

        if not 
yy.value_stack(yy.tos-5).Is_Present then
           yyerror ("Must specify ""for all [E :] T"" or " &
             """for all/some E in/of Container"" in quantified expression",
             At_Token => 
yy.value_stack(yy.tos-4));
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos,
          Kind => For_Loop_Construct.Univ_Quantified_Expr,
          Iterators => Lists.Make((1 =>
            Iterator.Make(
              Kind => Iterator.Set_Iterator,
              Name => 
yy.value_stack(yy.tos-4).Tree,
              Is_Ref => False,
              Obj_Type => Obj_Type,
              Obj_Value => Null_Optional_Tree))),
          Filter => 
yy.value_stack(yy.tos-2).List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos);
     end;
    

when  729 =>
--#line  4710
 
yyval := 
yy.value_stack(yy.tos); 

when  730 =>
--#line  4711
 
yyval := 
yy.value_stack(yy.tos); 

when  731 =>
--#line  4712
 
yyval := 
yy.value_stack(yy.tos); 

when  732 =>
--#line  4716
 
yyval := (Optional, True); 

when  733 =>
--#line  4717
 
yyval := (Optional, False); 

when  734 =>
--#line  4721
 
yyval := 
yy.value_stack(yy.tos); 

when  735 =>
--#line  4722
 
yyval := 
yy.value_stack(yy.tos); 

when  736 =>
--#line  4723
 
yyval := 
yy.value_stack(yy.tos); 

when  737 =>
--#line  4728

	-- This does a map/reduce operation where the initial/next result
        -- is given in <...> and the overall expression represents the
        -- reduction to be performed on each element.
      declare
        use type PSC.Strings.U_String;
      begin
        if 
yy.value_stack(yy.tos-2).Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("unordered" is the default)
            Iterator.Add_Direction(
yy.value_stack(yy.tos-4).Tree, 
yy.value_stack(yy.tos-2).Str);
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
          Kind => For_Loop_Construct.Map_Reduce_Expr,
          Iterators => Lists.Make((1 => 
yy.value_stack(yy.tos-4).Tree)),
          Filter => 
yy.value_stack(yy.tos-3).List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos);
      end;
    

when  738 =>
--#line  4752
 
yyval := 
yy.value_stack(yy.tos); 

when  739 =>
--#line  4753
 
yyval := 
yy.value_stack(yy.tos); 

when  740 =>
--#line  4754
 
yyval := 
yy.value_stack(yy.tos); 

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;

end Parython_Parser;
