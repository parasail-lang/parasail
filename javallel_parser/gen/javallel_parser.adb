

with Javallel_Tokens, Javallel_Lex_IO, Javallel_Goto, Javallel_Shift_Reduce;
with Javallel_Lex, Text_IO;

use  Javallel_Tokens, Javallel_Lex_IO, Javallel_Goto, Javallel_Shift_Reduce;
use  Javallel_Lex, Text_IO;

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

package body Javallel_Parser is

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
     At_Token : Javallel_Tokens.YYSType := (Javallel_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Error(S, Src_Pos => Token_Src_Pos (At_Token));
    end yyerror;

   procedure Parser_Warning (S : String;
     At_Token : Javallel_Tokens.YYSType := (Javallel_Tokens.Optional,
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
	
    use Qualifier; -- For Qualifier_Enum literals

pragma Style_Checks (Off);
procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Javallel_Goto;
    package yy_shift_reduce_tables renames
      Javallel_Shift_Reduce;
    package yy_tokens              renames
      Javallel_Tokens;

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
--#line  265

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  4 =>
--#line  272

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  5 =>
--#line  279

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  6 =>
--#line  286

	null;
    

when  10 =>
--#line  293

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
--#line  305

	
yyval := (One_List, Lists.Empty_List);
    

when  12 =>
--#line  308

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);  
	  -- TBD: Do we care how these were grouped?
    

when  13 =>
--#line  316

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  14 =>
--#line  319

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  15 =>
--#line  326

        
yyval := (One_Tree, PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  16 =>
--#line  329

        
yyval := 
yy.value_stack(yy.tos);
    

when  17 =>
--#line  332

	
yyval := (One_Tree, Qualified_Name.Make (
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  18 =>
--#line  347

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
      end;
   

when  19 =>
--#line  383

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
      end;
   

when  20 =>
--#line  414

	if 
yy.value_stack(yy.tos-5).Is_Private and then 
yy.value_stack(yy.tos-2).Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-3).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-3).Tree),
	  Is_Interface => True,
	  Is_Abstract => True,
	  Is_Private => 
yy.value_stack(yy.tos-5).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-5).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-2).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-2).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-2).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-2).Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    

when  22 =>
--#line  438

        yyerror ("Syntax error before ""is""", At_Token => 
yy.value_stack(yy.tos-1));
    

when  28 =>
--#line  453

	yyerror("Should be ""end interface <id>"" rather than ""end <id>""");
    

when  29 =>
--#line  459
 
yyval := 
yy.value_stack(yy.tos); 

when  30 =>
--#line  460

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  31 =>
--#line  468
 
yyval := 
yy.value_stack(yy.tos); 

when  32 =>
--#line  469

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  33 =>
--#line  479
 
yyval := 
yy.value_stack(yy.tos); 

when  34 =>
--#line  480
 
	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  35 =>
--#line  487

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	       Is_Concurrent => True, others => False);
    

when  36 =>
--#line  494

        --  NOTE: We don't allow stand-alone operator definitions
        
yyval := 
yy.value_stack(yy.tos);
  

when  37 =>
--#line  498
 
yyval := 
yy.value_stack(yy.tos-1); 

when  38 =>
--#line  499
 
yyval := 
yy.value_stack(yy.tos-1); 

when  39 =>
--#line  503
 
yyval := 
yy.value_stack(yy.tos-1); 

when  40 =>
--#line  504
 
yyval := (One_List, Lists.Empty_List); 

when  41 =>
--#line  508

	
yyval := (Formals_And_Interfaces,
	  Has_Module_Formals => (
yy.value_stack(yy.tos-1).Kind = One_List),
	  Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-1)),
	  Extends => Null_Optional_Tree,
	  Implements => 
yy.value_stack(yy.tos).List);
    

when  42 =>
--#line  515

        
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
    

when  43 =>
--#line  528

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
    

when  45 =>
--#line  547

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  46 =>
--#line  557
 
yyval := 
yy.value_stack(yy.tos); 

when  47 =>
--#line  558
 
	
yyval := (Optional, Is_Present => False);
    

when  48 =>
--#line  564
 
yyval := 
yy.value_stack(yy.tos); 

when  49 =>
--#line  565

	
yyval := (One_List, Lists.Empty_List);
    

when  50 =>
--#line  570
 
yyval := 
yy.value_stack(yy.tos); 

when  51 =>
--#line  573
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  52 =>
--#line  576

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  53 =>
--#line  583
 
yyval := 
yy.value_stack(yy.tos); 

when  54 =>
--#line  584
 
yyval := 
yy.value_stack(yy.tos); 

when  55 =>
--#line  587
 
yyval := 
yy.value_stack(yy.tos); 

when  56 =>
--#line  590
 
yyval := 
yy.value_stack(yy.tos); 

when  57 =>
--#line  591
 
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  58 =>
--#line  600

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  59 =>
--#line  605
 
yyval := 
yy.value_stack(yy.tos); 

when  60 =>
--#line  606

	
yyval := (One_List, Lists.Empty_List);
    

when  61 =>
--#line  611
 
yyval := 
yy.value_stack(yy.tos); 

when  62 =>
--#line  612

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  63 =>
--#line  616

	yyerror("Parameters are separated by ',' in Javallel",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  64 =>
--#line  625

	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos).List);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  65 =>
--#line  630

	
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
    

when  66 =>
--#line  647
 
yyval := 
yy.value_stack(yy.tos); 

when  67 =>
--#line  648

	
yyval := (One_List, Lists.Empty_List);
    

when  68 =>
--#line  654
 
yyval := 
yy.value_stack(yy.tos); 

when  69 =>
--#line  655

	
yyval := (One_List, Lists.Empty_List);
    

when  70 =>
--#line  661

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos-2).Tree));
    

when  71 =>
--#line  667
 
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  72 =>
--#line  675

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  73 =>
--#line  684
 
yyval := 
yy.value_stack(yy.tos); 

when  74 =>
--#line  685

        
yyval := 
yy.value_stack(yy.tos);
        declare
            First_Item : Param_Decl.Tree renames Param_Decl.Tree
              (Tree_Ptr_Of (Lists.Nth_Element(
yy.value_stack(yy.tos).List, 1)).all);
        begin
            First_Item.Kind := 
yy.value_stack(yy.tos-1).Param_Kind;
        end;
    

when  75 =>
--#line  698

	
yyval := (One_List, Lists.Make ((1 => Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-1).Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  76 =>
--#line  709
 
yyval := 
yy.value_stack(yy.tos); 

when  77 =>
--#line  712
 
yyval := 
yy.value_stack(yy.tos); 

when  78 =>
--#line  713
 
yyval := (One_Tree, Null_Optional_Tree); 

when  79 =>
--#line  717

	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos));
    

when  80 =>
--#line  726

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  81 =>
--#line  729

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  82 =>
--#line  736
 
yyval := 
yy.value_stack(yy.tos); 

when  83 =>
--#line  737
 
yyval := 
yy.value_stack(yy.tos); 

when  84 =>
--#line  741
 
yyval := 
yy.value_stack(yy.tos); 

when  85 =>
--#line  744

	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  86 =>
--#line  751
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  87 =>
--#line  754

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  88 =>
--#line  762
 
yyval := 
yy.value_stack(yy.tos); 

when  89 =>
--#line  763
 
yyval := 
yy.value_stack(yy.tos); 

when  90 =>
--#line  766

        -- String_Literal can be used as a "name" when it is an operator
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  91 =>
--#line  773

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  92 =>
--#line  790
 
yyval := 
yy.value_stack(yy.tos); 

when  93 =>
--#line  791
 
	
yyval := (One_List, Lists.Empty_List);
    

when  94 =>
--#line  797
 
yyval := 
yy.value_stack(yy.tos); 

when  95 =>
--#line  798

	
yyval := (One_List, Lists.Empty_List);
    

when  96 =>
--#line  804

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  97 =>
--#line  807

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  98 =>
--#line  814
 
yyval := 
yy.value_stack(yy.tos); 

when  99 =>
--#line  815

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  100 =>
--#line  824
 
	-- polymorphic type name not allowed here
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  101 =>
--#line  829
 
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  102 =>
--#line  833

	-- This is a polymorphic type name, presumably.
	-- We use adding_expression instead of qualified_name
	-- to avoid reduce/reduce conflicts in the grammar.
	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  103 =>
--#line  841
 
yyval := 
yy.value_stack(yy.tos); 

when  104 =>
--#line  843
 
yyval := 
yy.value_stack(yy.tos); 

when  105 =>
--#line  844
 
yyval := 
yy.value_stack(yy.tos); 

when  106 =>
--#line  848
 
yyval := 
yy.value_stack(yy.tos); 

when  107 =>
--#line  849

        
yyval := 
yy.value_stack(yy.tos);
    

when  108 =>
--#line  855
 
yyval := 
yy.value_stack(yy.tos); 

when  109 =>
--#line  856

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  110 =>
--#line  863
 
yyval := 
yy.value_stack(yy.tos); 

when  111 =>
--#line  864
 
yyval := 
yy.value_stack(yy.tos); 

when  112 =>
--#line  868
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  113 =>
--#line  871
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  114 =>
--#line  882
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  115 =>
--#line  889
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  116 =>
--#line  909
 
	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Present,
	  others => False);
    

when  117 =>
--#line  916

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Concurrent => True, others => False);
    

when  118 =>
--#line  924

	
yyval := (Optional, True);
    

when  119 =>
--#line  927

	
yyval := (Optional, False);
    

when  120 =>
--#line  933

	
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
    

when  121 =>
--#line  947

	
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
    

when  122 =>
--#line  963
 
yyval := (One_List, Lists.Empty_List); 

when  123 =>
--#line  964

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
    

when  126 =>
--#line  983

	
yyval := (One_List, Lists.Empty_List);
    

when  127 =>
--#line  986

	
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
    

when  128 =>
--#line  995

	
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
    

when  129 =>
--#line  1004

	
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
    

when  130 =>
--#line  1017

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  131 =>
--#line  1023
 
yyval := 
yy.value_stack(yy.tos-1); 

when  132 =>
--#line  1024
 
yyval := 
yy.value_stack(yy.tos); 

when  133 =>
--#line  1028
 
yyval := 
yy.value_stack(yy.tos); 

when  134 =>
--#line  1029
 
yyval := 
yy.value_stack(yy.tos); 

when  135 =>
--#line  1033
 
yyval := 
yy.value_stack(yy.tos); 

when  136 =>
--#line  1034
 
yyval := 
yy.value_stack(yy.tos); 

when  137 =>
--#line  1035
 
yyval := 
yy.value_stack(yy.tos); 

when  138 =>
--#line  1039
 
yyval := 
yy.value_stack(yy.tos); 

when  139 =>
--#line  1040

	
yyval := (One_List, Lists.Empty_List);
    

when  140 =>
--#line  1046
 
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
    

when  141 =>
--#line  1060

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
    

when  142 =>
--#line  1074

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
    

when  145 =>
--#line  1092
 
yyval := 
yy.value_stack(yy.tos); 

when  146 =>
--#line  1093
 
yyval := 
yy.value_stack(yy.tos-1); 

when  147 =>
--#line  1097
 
yyval := 
yy.value_stack(yy.tos); 

when  148 =>
--#line  1098

	
yyval := (One_List, Lists.Empty_List);
    

when  149 =>
--#line  1104
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    

when  150 =>
--#line  1114
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => 
yy.value_stack(yy.tos-1).List,
	  Elements => Elem_List))));
      end;
    

when  151 =>
--#line  1124

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
    

when  152 =>
--#line  1137

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  154 =>
--#line  1145

        --  Pop the indent stack
        if Javallel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Top := Javallel_Lex.Top - 1;
    

when  155 =>
--#line  1155

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  156 =>
--#line  1159

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  157 =>
--#line  1164

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  158 =>
--#line  1170

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
    

when  159 =>
--#line  1185

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if Javallel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Top := Javallel_Lex.Top - 1;  --  Pop the indent stack
    

when  160 =>
--#line  1193

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if Javallel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Top := Javallel_Lex.Top - 1;  --  Pop the indent stack
    

when  161 =>
--#line  1203

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
            if Javallel_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Javallel_Lex.Top := Javallel_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  162 =>
--#line  1231

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
   

when  164 =>
--#line  1257

	yyerror("Should be ""end class <id>"" rather than ""end <id>""");
    

when  165 =>
--#line  1262
 
yyval := (One_List, Lists.Empty_List); 

when  166 =>
--#line  1263

	if Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- We want to make sure that we return a non-empty list
	    
yyval := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    
yyval := 
yy.value_stack(yy.tos);
	end if;
    

when  167 =>
--#line  1275

	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  168 =>
--#line  1286

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
    

when  169 =>
--#line  1298

	yyerror("Missing ""exports"" keyword");
	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  173 =>
--#line  1306

	
yyval := (One_List, Lists.Empty_List);
    

when  174 =>
--#line  1309

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  175 =>
--#line  1316
 
yyval := 
yy.value_stack(yy.tos); 

when  176 =>
--#line  1317
 
yyval := 
yy.value_stack(yy.tos-1); 

when  177 =>
--#line  1318
 
yyval := 
yy.value_stack(yy.tos-1); 

when  178 =>
--#line  1319
 
yyval := 
yy.value_stack(yy.tos); 

when  179 =>
--#line  1322

	
yyval := (One_List, Lists.Empty_List);
    

when  180 =>
--#line  1325

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  181 =>
--#line  1329

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  182 =>
--#line  1333

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  183 =>
--#line  1337

	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  184 =>
--#line  1343

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  185 =>
--#line  1349
 
yyval := 
yy.value_stack(yy.tos); 

when  186 =>
--#line  1350
 
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  187 =>
--#line  1353

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  188 =>
--#line  1360
 
yyval := 
yy.value_stack(yy.tos); 

when  189 =>
--#line  1361
 
yyval := 
yy.value_stack(yy.tos); 

when  190 =>
--#line  1362
 
yyval := 
yy.value_stack(yy.tos); 

when  191 =>
--#line  1366
 
yyval := 
yy.value_stack(yy.tos); 

when  192 =>
--#line  1367
 
yyval := 
yy.value_stack(yy.tos); 

when  193 =>
--#line  1368

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  194 =>
--#line  1375
 
yyval := 
yy.value_stack(yy.tos-1); 

when  195 =>
--#line  1376

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  196 =>
--#line  1382
 
yyval := 
yy.value_stack(yy.tos-1); 

when  197 =>
--#line  1383

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  198 =>
--#line  1390

        
yyval := 
yy.value_stack(yy.tos);
    

when  199 =>
--#line  1393

        --  A labeled annotation list becomes a separate nested annotation
        
yyval := (One_List, Lists.Make
                 ((1 => Annotation.Make
                   (Annotations => 
yy.value_stack(yy.tos).List, Label => 
yy.value_stack(yy.tos-1).Tree))));
    

when  200 =>
--#line  1403

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  201 =>
--#line  1406

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  202 =>
--#line  1410

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  203 =>
--#line  1416
 
yyval := 
yy.value_stack(yy.tos); 

when  204 =>
--#line  1419
 
yyval := 
yy.value_stack(yy.tos); 

when  205 =>
--#line  1420
 
yyval := 
yy.value_stack(yy.tos); 

when  206 =>
--#line  1421

	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  207 =>
--#line  1431
 
yyval := 
yy.value_stack(yy.tos); 

when  208 =>
--#line  1435
 
yyval := 
yy.value_stack(yy.tos); 

when  209 =>
--#line  1439
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  210 =>
--#line  1444
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => True, others => False); 
    

when  211 =>
--#line  1452
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  212 =>
--#line  1457
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  213 =>
--#line  1465

	
yyval := (Optional, Is_Present => True);
    

when  214 =>
--#line  1468

	
yyval := (Optional, Is_Present => False);
    

when  215 =>
--#line  1474
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  216 =>
--#line  1477

	yyerror("Operator designator must be in quotes");
	
yyval := 
yy.value_stack(yy.tos);
    

when  218 =>
--#line  1484

	yyerror("Use ""->"" in Javallel rather than ""return""");
    

when  219 =>
--#line  1490

	
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
    

when  220 =>
--#line  1501

	
yyval := 
yy.value_stack(yy.tos);
    

when  221 =>
--#line  1507

	
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
    

when  222 =>
--#line  1522

        if Not_Null (
yy.value_stack(yy.tos).Tree) then
            --  Fill in name/type of output parameter
            declare
                Output_Param : Param_Decl.Tree renames Param_Decl.Tree
                  (Tree_Ptr_Of (
yy.value_stack(yy.tos-4).Tree).all);
                Output_Name : PSC.Trees.Tree'Class renames
                  Tree_Ptr_Of (
yy.value_stack(yy.tos).Tree).all;
            begin
                if Output_Name in Param_Decl.Tree then
                   --  Providing type and name
                   declare
                      Param_Info : Param_Decl.Tree renames Param_Decl.Tree
                        (Output_Name);
                      Param_Type : Type_Decl.Tree renames Type_Decl.Tree
                        (Tree_Ptr_Of (Param_Info.Param_Type).all);
                   begin
                      Output_Param.Name := Param_Info.Name;

                      --  Move type down into Type_Definition of Type_Decl
                      Param_Type.Type_Definition := Output_Param.Param_Type;
                      Output_Param.Param_Type := Param_Info.Param_Type;
                   end;
                else
                   --  Just providing a new name for output
                   Output_Param.Name := 
yy.value_stack(yy.tos).Tree;
                end if;
            end;
        end if;

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).List,
	  Operation_Outputs => Lists.Make ((1 => 
yy.value_stack(yy.tos-4).Tree)),
	  Preconditions => Annotation.Make (
yy.value_stack(yy.tos-1).List),
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  223 =>
--#line  1566

        if Not_Null (
yy.value_stack(yy.tos).Tree) then
            --  Fill in name/type of output parameter
            declare
                Output_Param : Param_Decl.Tree renames Param_Decl.Tree
                  (Tree_Ptr_Of (
yy.value_stack(yy.tos-4).Tree).all);
                Output_Name : PSC.Trees.Tree'Class renames
                  Tree_Ptr_Of (
yy.value_stack(yy.tos).Tree).all;
            begin
                if Output_Name in Param_Decl.Tree then
                   --  Providing type and name
                   declare
                      Param_Info : Param_Decl.Tree renames Param_Decl.Tree
                        (Output_Name);
                      Param_Type : Type_Decl.Tree renames Type_Decl.Tree
                        (Tree_Ptr_Of (Param_Info.Param_Type).all);
                   begin
                      Output_Param.Name := Param_Info.Name;

                      --  Move type down into Type_Definition of Type_Decl
                      Param_Type.Type_Definition := Output_Param.Param_Type;
                      Output_Param.Param_Type := Param_Info.Param_Type;
                   end;
                else
                   --  Just providing a new name for output
                   Output_Param.Name := 
yy.value_stack(yy.tos).Tree;
                end if;
            end;
        end if;

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).List,
	  Operation_Outputs => Lists.Make ((1 => 
yy.value_stack(yy.tos-4).Tree)),
	  Preconditions => Annotation.Make (
yy.value_stack(yy.tos-1).List),
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    

when  224 =>
--#line  1612

	
yyval := 
yy.value_stack(yy.tos);
    

when  225 =>
--#line  1618
 
yyval := 
yy.value_stack(yy.tos); 

when  226 =>
--#line  1619

        --  Giving name to type as well as result
	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos).Tree,
	  Kind => Param_Decl.Default_Param,  --  Will be filled in
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
          Param_Type => Type_Decl.Make(
             Name => 
yy.value_stack(yy.tos-1).Tree,
             Is_New_Type => False,
             Type_Definition => Null_Optional_Tree),  --  Will be filled in
	  Param_Default => Null_Optional_Tree));
    

when  227 =>
--#line  1632
 
yyval := (One_Tree, Null_Optional_Tree); 

when  228 =>
--#line  1636

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  229 =>
--#line  1642
 
yyval := 
yy.value_stack(yy.tos); 

when  230 =>
--#line  1643

        yyerror ("Expecting one ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := (One_Token,
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.String_Lookup(")")); 
    

when  231 =>
--#line  1652
 
yyval := 
yy.value_stack(yy.tos); 

when  232 =>
--#line  1653

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  233 =>
--#line  1661
 
yyval := 
yy.value_stack(yy.tos); 

when  234 =>
--#line  1662
 
yyval := 
yy.value_stack(yy.tos); 

when  235 =>
--#line  1663

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  236 =>
--#line  1668

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  237 =>
--#line  1673

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  238 =>
--#line  1678

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  239 =>
--#line  1683

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  240 =>
--#line  1688

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  241 =>
--#line  1693

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  242 =>
--#line  1701
 
yyval := 
yy.value_stack(yy.tos); 

when  243 =>
--#line  1702

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  244 =>
--#line  1710

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  245 =>
--#line  1715

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  246 =>
--#line  1720

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  247 =>
--#line  1728

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  248 =>
--#line  1733

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  249 =>
--#line  1741
 
yyval := 
yy.value_stack(yy.tos); 

when  250 =>
--#line  1742

	
yyval := (One_List, Lists.Empty_List);
    

when  251 =>
--#line  1748
 
yyval := 
yy.value_stack(yy.tos); 

when  252 =>
--#line  1749

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  253 =>
--#line  1753

	yyerror ("Parameters are separated by ',' in Javallel",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  254 =>
--#line  1762

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
    

when  255 =>
--#line  1772

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
    

when  256 =>
--#line  1785
 
yyval := 
yy.value_stack(yy.tos); 

when  257 =>
--#line  1787

	
yyval := (One_List, Lists.Make ((1 =>
	    Param_Decl.Make(
	      Name => 
yy.value_stack(yy.tos-2).Tree,
	      Kind => 
yy.value_stack(yy.tos-5).Param_Kind,
	      Locking => 
yy.value_stack(yy.tos-5).Param_Locking,
	      Is_Optional => 
yy.value_stack(yy.tos-4).Is_Optional,
              In_Region => 
yy.value_stack(yy.tos-1).Tree,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-4).Is_Optional, others => False),
                 Operand => 
yy.value_stack(yy.tos-3).Tree),
	      Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  258 =>
--#line  1802

	
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
    

when  259 =>
--#line  1814

	
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
    

when  260 =>
--#line  1830

	
yyval := (One_List, Lists.Make ((1 =>
	    Param_Decl.Make(
	      Name => 
yy.value_stack(yy.tos-2).Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-4).Is_Optional,
              In_Region => 
yy.value_stack(yy.tos-1).Tree,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => 
yy.value_stack(yy.tos-4).Is_Optional, others => False),
                 Operand => 
yy.value_stack(yy.tos-3).Tree),
	      Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  261 =>
--#line  1845

	
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
    

when  262 =>
--#line  1858

	
yyval := (One_List, Lists.Make ((1 =>
	    Param_Decl.Make(
	      Name => 
yy.value_stack(yy.tos-2).Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
              In_Region => 
yy.value_stack(yy.tos-1).Tree,
	      Param_Type => 
yy.value_stack(yy.tos-3).Tree,
	      Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  263 =>
--#line  1869

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  264 =>
--#line  1881
 
yyval := 
yy.value_stack(yy.tos); 

when  265 =>
--#line  1882

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  266 =>
--#line  1890
 
yyval := 
yy.value_stack(yy.tos); 

when  267 =>
--#line  1891
 
yyval := 
yy.value_stack(yy.tos); 

when  268 =>
--#line  1895
 
yyval := 
yy.value_stack(yy.tos); 

when  269 =>
--#line  1896

         -- NOTE: Operation can have "type" parameters 
         -- such as "Integer<> as LeftType Left"
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos-2).Tree));
    

when  270 =>
--#line  1904

        
yyval := 
yy.value_stack(yy.tos);
    

when  271 =>
--#line  1910

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  272 =>
--#line  1919
 
yyval := 
yy.value_stack(yy.tos); 

when  273 =>
--#line  1920

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  274 =>
--#line  1928

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  275 =>
--#line  1938
 
yyval := (Optional, Is_Present => True); 

when  276 =>
--#line  1939
 
yyval := (Optional, Is_Present => False); 

when  277 =>
--#line  1950
 
yyval := 
yy.value_stack(yy.tos); 

when  278 =>
--#line  1954

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  279 =>
--#line  1967

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos).Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  280 =>
--#line  1976

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos).Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  281 =>
--#line  1988
 
yyval := 
yy.value_stack(yy.tos); 

when  282 =>
--#line  1989
 
yyval := 
yy.value_stack(yy.tos); 

when  283 =>
--#line  1993

	
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
    

when  284 =>
--#line  2005

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  285 =>
--#line  2017

	
yyval := 
yy.value_stack(yy.tos);
    

when  286 =>
--#line  2020

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  287 =>
--#line  2027

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
    

when  288 =>
--#line  2037

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
    

when  289 =>
--#line  2052

	
yyval := (One_List, Lists.Make((1 =>
	    Param_Decl.Make(
	      Name => 
yy.value_stack(yy.tos).Tree,
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
	      Param_Default => Null_Optional_Tree))));
    

when  290 =>
--#line  2067

	
yyval := (One_List, Lists.Make((1 =>
	    Param_Decl.Make(
	      Name => 
yy.value_stack(yy.tos).Tree,
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
	      Param_Default => Null_Optional_Tree))));
    

when  291 =>
--#line  2080

	
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
    

when  292 =>
--#line  2092

	
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
    

when  293 =>
--#line  2107
 
yyval := 
yy.value_stack(yy.tos); 

when  294 =>
--#line  2108
 
yyval := 
yy.value_stack(yy.tos); 

when  295 =>
--#line  2113

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  296 =>
--#line  2125

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  297 =>
--#line  2136

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-3).Tree).all),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  298 =>
--#line  2148

        if Qualified_Name.Contains_String (
yy.value_stack(yy.tos-1).Tree) then
            yyerror("Variable names may not use string-literal syntax",
              At_Token => 
yy.value_stack(yy.tos-1));
        end if;

        declare
            --  Retrieve information from created param-decl node
            Output : Param_Decl.Tree renames Param_Decl.Tree
              (Tree_Ptr_Of (
yy.value_stack(yy.tos-2).Tree).all);
        begin
           
yyval := (One_Tree, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-1).Tree).all),
             Is_Var => Output.Kind = Param_Decl.Ref_Var_Param,
             Is_Const => Output.Kind = Param_Decl.Ref_Const_Param,
             Is_Ref => True,
             Is_Optional => False, -- TBD
             Obj_Type => Output.Param_Type,
             Obj_Value => 
yy.value_stack(yy.tos).Tree));
        end;
    

when  299 =>
--#line  2173

	yyerror("Must specify ""var,"" ""final,"" or ""ref""",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (One_Tree, Obj_Decl.Make(
          Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-1).Tree).all),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Obj_Type => Lists.Nth_Element (
yy.value_stack(yy.tos-2).List, 1),
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  300 =>
--#line  2188
 
yyval := 
yy.value_stack(yy.tos); 

when  301 =>
--#line  2189

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  302 =>
--#line  2195
 
yyval := 
yy.value_stack(yy.tos); 

when  303 =>
--#line  2196

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  304 =>
--#line  2202
 
yyval := 
yy.value_stack(yy.tos); 

when  305 =>
--#line  2203

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  306 =>
--#line  2208
 
yyval := 
yy.value_stack(yy.tos-1); 

when  307 =>
--#line  2211

	
yyval := (One_Tree, Obj_Decl.Make(
          Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-3).Tree).all),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  308 =>
--#line  2222

	
yyval := (One_Tree, Obj_Decl.Make(
          Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all),
	  Is_Var => 
yy.value_stack(yy.tos-3).Param_Kind = Param_Decl.Ref_Var_Param,
	  Is_Const => 
yy.value_stack(yy.tos-3).Param_Kind = Param_Decl.Ref_Const_Param,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  309 =>
--#line  2232

	
yyval := (One_Tree, Obj_Decl.Make(
          Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  310 =>
--#line  2243

	
yyval := (One_Tree, Obj_Decl.Make(
          Name => PSC.Trees.Identifier.Tree(Tree_Ptr_Of(
yy.value_stack(yy.tos-2).Tree).all),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  311 =>
--#line  2256
 
yyval := 
yy.value_stack(yy.tos); 

when  312 =>
--#line  2257
 
yyval := 
yy.value_stack(yy.tos); 

when  313 =>
--#line  2260
 
yyval := 
yy.value_stack(yy.tos); 

when  314 =>
--#line  2263

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_New_Type => 
yy.value_stack(yy.tos-1).Is_Present,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  315 =>
--#line  2271

	
yyval := (Optional, True);
    

when  316 =>
--#line  2274

	
yyval := (Optional, False);
    

when  317 =>
--#line  2280
 
yyval := 
yy.value_stack(yy.tos); 

when  318 =>
--#line  2286

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
	end;
    

when  319 =>
--#line  2298

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
	end;
    

when  321 =>
--#line  2312

	yyerror("Should be 'end op ""id""' not simply 'end ""id""'");
    

when  323 =>
--#line  2318

	yyerror("Should be ""end func <id>"" not simply ""end <id>""");
    

when  324 =>
--#line  2324
 
yyval := 
yy.value_stack(yy.tos); 

when  325 =>
--#line  2325
 
yyval := (One_Tree, Null_Optional_Tree); 

when  326 =>
--#line  2329
 
yyval := 
yy.value_stack(yy.tos); 

when  327 =>
--#line  2332
 
yyval := 
yy.value_stack(yy.tos-1); 

when  328 =>
--#line  2336

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree,
	  Complement => 
yy.value_stack(yy.tos-2).Is_While));  
	    -- Complement cond if "while" present
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  329 =>
--#line  2344
 
        --  Pop the indent stack
        if Javallel_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Top := Javallel_Lex.Top - 1;
    

when  333 =>
--#line  2359

        
yyval := (Optional_End_Token, Check_Label => False,
                others => Null_Optional_Tree);
    

when  334 =>
--#line  2366

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  335 =>
--#line  2371
 
yyval := 
yy.value_stack(yy.tos); 

when  336 =>
--#line  2372
 
yyval := 
yy.value_stack(yy.tos); 

when  337 =>
--#line  2376
 
yyval := 
yy.value_stack(yy.tos); 

when  338 =>
--#line  2377

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  339 =>
--#line  2386

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  340 =>
--#line  2396

	-- "begin" is not used in Javallel; treat like "then" for now
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  341 =>
--#line  2403

	-- "begin" is not used in Javallel
	
yyval := 
yy.value_stack(yy.tos);
    

when  342 =>
--#line  2409

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  347 =>
--#line  2422

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  348 =>
--#line  2431

	yyerror("No need for ""begin"" in Javallel operation definition",
          At_Token => 
yy.value_stack(yy.tos));
    

when  349 =>
--#line  2438
 
yyval := 
yy.value_stack(yy.tos); 

when  350 =>
--#line  2439

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  351 =>
--#line  2448

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  352 =>
--#line  2460
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  353 =>
--#line  2463

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  354 =>
--#line  2470

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  355 =>
--#line  2479

        
yyval := 
yy.value_stack(yy.tos);
    

when  356 =>
--#line  2482

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  357 =>
--#line  2488
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  358 =>
--#line  2491

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  359 =>
--#line  2498

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  360 =>
--#line  2508
 
yyval := 
yy.value_stack(yy.tos); 

when  361 =>
--#line  2509

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  362 =>
--#line  2518
 
yyval := 
yy.value_stack(yy.tos); 

when  363 =>
--#line  2519

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  364 =>
--#line  2528

        
yyval := 
yy.value_stack(yy.tos);
    

when  365 =>
--#line  2531

        
yyval := 
yy.value_stack(yy.tos);
    

when  366 =>
--#line  2537

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  367 =>
--#line  2543

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  368 =>
--#line  2548

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  369 =>
--#line  2552

	-- An annotation can appear by itself
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  370 =>
--#line  2559

            -- NOTE: these already allow trailing annotations
	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  371 =>
--#line  2564

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  372 =>
--#line  2568
 
yyval := 
yy.value_stack(yy.tos); 

when  373 =>
--#line  2570

	
yyval := 
yy.value_stack(yy.tos);
    

when  374 =>
--#line  2576
 
yyval := 
yy.value_stack(yy.tos-1); 

when  375 =>
--#line  2579
 
yyval := 
yy.value_stack(yy.tos); 

when  376 =>
--#line  2580
 
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
    

when  377 =>
--#line  2589
 
yyval := 
yy.value_stack(yy.tos); 

when  378 =>
--#line  2590
 
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
    

when  379 =>
--#line  2603

	
yyval := 
yy.value_stack(yy.tos);
  

when  380 =>
--#line  2606

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  381 =>
--#line  2612
 
	-- A "null" statement (i.e. a no-op)
	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Null_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => 
yy.value_stack(yy.tos).Source_Pos));
    

when  382 =>
--#line  2621

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  383 =>
--#line  2627
 
yyval := 
yy.value_stack(yy.tos); 

when  384 =>
--#line  2628

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Continue_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => 
yy.value_stack(yy.tos-1).Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  385 =>
--#line  2636

        declare
           Applies_To : Control_Stmt.Exitable_Construct_Enum :=
             
yy.value_stack(yy.tos-2).Exitable_Construct;
           use type Control_Stmt.Exitable_Construct_Enum;
        begin
           if Applies_To = Control_Stmt.Any_Labeled_Stmt
             and then Is_Null (
yy.value_stack(yy.tos-1).Tree)
           then
              --  "break" without a label means break from switch or loop
              Applies_To := Control_Stmt.Case_Or_Loop_Stmt;
           end if;
           
yyval := (One_Tree, Control_Stmt.Make(
             Kind => Control_Stmt.Exit_Stmt,
             Applies_To => Applies_To,
             Id => 
yy.value_stack(yy.tos-1).Tree,
             Values => 
yy.value_stack(yy.tos).Tree,
             Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos));
        end;
    

when  386 =>
--#line  2656

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos-1);
    

when  387 =>
--#line  2664

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  388 =>
--#line  2672

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  389 =>
--#line  2683

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => 
yy.value_stack(yy.tos-1).Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  390 =>
--#line  2689

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Divide_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  391 =>
--#line  2695

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Combine_Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  392 =>
--#line  2701

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  393 =>
--#line  2707

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Swap_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  394 =>
--#line  2713

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Plus_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-1).Tree,
	  RHS => PSC.Trees.Identifier.Make
            (PSC.Strings.String_Lookup("1"), 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  395 =>
--#line  2720

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Minus_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-1).Tree,
	  RHS => PSC.Trees.Identifier.Make
            (PSC.Strings.String_Lookup("1"), 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  396 =>
--#line  2740

	
yyval := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))));
    

when  397 =>
--#line  2744

	
yyval := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
                  Label => 
yy.value_stack(yy.tos).Tree));
    

when  398 =>
--#line  2752
 
yyval := 
yy.value_stack(yy.tos); 

when  399 =>
--#line  2753

	
yyval := (One_List, Lists.Empty_List);
    

when  400 =>
--#line  2759
 
yyval := 
yy.value_stack(yy.tos); 

when  401 =>
--#line  2760

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  402 =>
--#line  2765

	-- NOTE: This used to be '(' operation_actual_list ')'
	--       but that prevented continuing with a single expression.
	
yyval := 
yy.value_stack(yy.tos);
    

when  403 =>
--#line  2772
 
yyval := 
yy.value_stack(yy.tos); 

when  404 =>
--#line  2773

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  405 =>
--#line  2779
 
yyval := 
yy.value_stack(yy.tos); 

when  406 =>
--#line  2780
 
	
yyval := (Construct_Kind, Control_Stmt.Any_Labeled_Stmt);
    

when  407 =>
--#line  2786

	
yyval := (Construct_Kind, Control_Stmt.If_Stmt);
    

when  408 =>
--#line  2789

	
yyval := (Construct_Kind, Control_Stmt.Case_Stmt);
    

when  409 =>
--#line  2792

	
yyval := (Construct_Kind, Control_Stmt.Block_Stmt);
    

when  410 =>
--#line  2797

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  411 =>
--#line  2807
 
yyval := 
yy.value_stack(yy.tos); 

when  412 =>
--#line  2808
 
yyval := 
yy.value_stack(yy.tos); 

when  413 =>
--#line  2809
 
yyval := 
yy.value_stack(yy.tos); 

when  414 =>
--#line  2810
 
yyval := 
yy.value_stack(yy.tos); 

when  415 =>
--#line  2814
 
yyval := 
yy.value_stack(yy.tos); 

when  416 =>
--#line  2815
 
yyval := 
yy.value_stack(yy.tos); 

when  417 =>
--#line  2816
 
yyval := 
yy.value_stack(yy.tos-1); 

when  418 =>
--#line  2819
 
yyval := 
yy.value_stack(yy.tos); 

when  419 =>
--#line  2820
 
yyval := 
yy.value_stack(yy.tos-1); 

when  420 =>
--#line  2821
 
yyval := 
yy.value_stack(yy.tos-1); 

when  421 =>
--#line  2824
 
yyval := 
yy.value_stack(yy.tos-1); 

when  422 =>
--#line  2827
 
yyval := 
yy.value_stack(yy.tos); 

when  423 =>
--#line  2828
 
yyval := 
yy.value_stack(yy.tos); 

when  424 =>
--#line  2829
 
yyval := 
yy.value_stack(yy.tos); 

when  425 =>
--#line  2830
 
yyval := 
yy.value_stack(yy.tos); 

when  426 =>
--#line  2831
 
yyval := 
yy.value_stack(yy.tos); 

when  427 =>
--#line  2832
 
yyval := (One_Tree, Null_Optional_Tree); 

when  428 =>
--#line  2838

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree,
	  End_With_Values => Null_Optional_Tree,
	  Check_Label => False,
          Label => Null_Optional_Tree));
    

when  429 =>
--#line  2853

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  430 =>
--#line  2861

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  431 =>
--#line  2864

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  432 =>
--#line  2870
 
yyval := 
yy.value_stack(yy.tos); 

when  433 =>
--#line  2871
 
yyval := 
yy.value_stack(yy.tos-1); 

when  434 =>
--#line  2877

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  435 =>
--#line  2885

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  436 =>
--#line  2888

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  439 =>
--#line  2901

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
    

when  442 =>
--#line  2922

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  443 =>
--#line  2925

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  444 =>
--#line  2933

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  445 =>
--#line  2942
 
yyval := 
yy.value_stack(yy.tos); 

when  446 =>
--#line  2943
 
yyval := 
yy.value_stack(yy.tos); 

when  447 =>
--#line  2947
 
yyval := 
yy.value_stack(yy.tos); 

when  448 =>
--#line  2948
  --  Java order, type comes before ID
	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos-1).Tree,
          Param_Default => Null_Optional_Tree));
     

when  449 =>
--#line  2961

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 =>
              Binary.Make(Binary.Closed_Interval_Op,
                Left_Operand => Null_Optional_Tree,
                Right_Operand => Null_Optional_Tree)))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  450 =>
--#line  2972

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
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
    

when  451 =>
--#line  2988

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  452 =>
--#line  2994
 
yyval := 
yy.value_stack(yy.tos-1); 

when  453 =>
--#line  2995
 
yyval := 
yy.value_stack(yy.tos-2); 

when  454 =>
--#line  2996
 
yyval := 
yy.value_stack(yy.tos); 

when  455 =>
--#line  3000
 
yyval := 
yy.value_stack(yy.tos); 

when  456 =>
--#line  3001

	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
          Param_Default => Null_Optional_Tree));
     

when  457 =>
--#line  3012

	
yyval := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree));
     

when  458 =>
--#line  3022

	
yyval := (One_Tree, While_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  While_Cond => Null_Optional_Tree,
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => Null_Optional_Tree,
          Check_Label => False,
          Label => Null_Optional_Tree));
    

when  459 =>
--#line  3036

	
yyval := (One_Tree, While_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	  While_Cond => Conditionally_Complement(
yy.value_stack(yy.tos-4).Tree,
	    Complement => 
yy.value_stack(yy.tos-5).Is_Until),
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => Null_Optional_Tree,
          Check_Label => False,
          Label => Null_Optional_Tree));
    

when  460 =>
--#line  3049
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_While => True, others => False); 

when  461 =>
--#line  3052
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_Until => True, others => False); 

when  462 =>
--#line  3062

	
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
	  End_With_Values => Null_Optional_Tree,
          Check_Label => False,
          Label => Null_Optional_Tree));
    

when  463 =>
--#line  3077

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  464 =>
--#line  3080
 
yyval := 
yy.value_stack(yy.tos-1); 

when  465 =>
--#line  3084

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
    

when  466 =>
--#line  3095

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
    

when  467 =>
--#line  3107

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
    

when  468 =>
--#line  3124
 
yyval := 
yy.value_stack(yy.tos); 

when  469 =>
--#line  3125

	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  470 =>
--#line  3133
 
yyval := 
yy.value_stack(yy.tos); 

when  471 =>
--#line  3134
 
	yyerror("Use ""for ..."" or ""for each ..."" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  472 =>
--#line  3140
 
yyval := 
yy.value_stack(yy.tos); 

when  473 =>
--#line  3141
 
	yyerror("""for-each"" iterator uses ""of"" rather than ""in""",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  474 =>
--#line  3146
 
	yyerror("Use ""for each ..."" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  475 =>
--#line  3152
 
	yyerror("Missing ""each"" in container element ""for-each"" iterator",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  476 =>
--#line  3157
 
yyval := 
yy.value_stack(yy.tos); 

when  477 =>
--#line  3158
 
yyval := 
yy.value_stack(yy.tos); 

when  478 =>
--#line  3162

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-4).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  479 =>
--#line  3170

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => False,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  481 =>
--#line  3180

	yyerror("The ""reverse"" keyword goes immediately before ""loop""");
    

when  482 =>
--#line  3185

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  483 =>
--#line  3193

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  484 =>
--#line  3201

	
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
    

when  485 =>
--#line  3212

        if Javallel_Lex.Debug_Indent
          and then Javallel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        Javallel_Lex.Expecting_Indent := False;
    

when  486 =>
--#line  3223

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-6).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  487 =>
--#line  3234

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-6).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  488 =>
--#line  3245

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Is_Ref => False,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  489 =>
--#line  3256

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-3).Tree,
	  Next_Values => 
yy.value_stack(yy.tos-1).List,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  490 =>
--#line  3269

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-4).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  491 =>
--#line  3278

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-4).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  492 =>
--#line  3287

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => False,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  493 =>
--#line  3296

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos-1).Tree,
	  While_Cond => 
yy.value_stack(yy.tos).Tree));
    

when  494 =>
--#line  3308

	
yyval := 
yy.value_stack(yy.tos);
    

when  495 =>
--#line  3311

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  496 =>
--#line  3317
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  497 =>
--#line  3320

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  498 =>
--#line  3327
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  499 =>
--#line  3330

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  500 =>
--#line  3337

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree, Complement => 
yy.value_stack(yy.tos-1).Is_Until));
	    -- Complement condition if used "until"
    

when  501 =>
--#line  3342

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  502 =>
--#line  3347
 
yyval := 
yy.value_stack(yy.tos); 

when  503 =>
--#line  3348
 
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  504 =>
--#line  3356

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("concurrent"));
    

when  505 =>
--#line  3360
 
yyval := 
yy.value_stack(yy.tos); 

when  506 =>
--#line  3364

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("forward"));
    

when  507 =>
--#line  3368

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("reverse"));
    

when  508 =>
--#line  3377

	
yyval := (One_Tree, Block_Stmt.Make(
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Block_Body => 
yy.value_stack(yy.tos-2).Tree,
	  End_With_Values => Null_Optional_Tree,
          Check_Label => False,
          Label => 
yy.value_stack(yy.tos-4).Tree));
    

when  510 =>
--#line  3388

	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
    

when  511 =>
--#line  3394

	
yyval := 
yy.value_stack(yy.tos);
    

when  512 =>
--#line  3400
 
yyval := 
yy.value_stack(yy.tos); 

when  513 =>
--#line  3403

	
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
    

when  514 =>
--#line  3411
 
yyval := 
yy.value_stack(yy.tos); 

when  515 =>
--#line  3416

	
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
    

when  516 =>
--#line  3431

	
yyval := (One_List, Lists.Empty_List);
    

when  517 =>
--#line  3434

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  518 =>
--#line  3437

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  519 =>
--#line  3442

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  520 =>
--#line  3445

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  521 =>
--#line  3451

        
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => Null_Optional_Tree,
          Param_Default => Null_Optional_Tree));
    

when  522 =>
--#line  3463
 
yyval := 
yy.value_stack(yy.tos); 

when  523 =>
--#line  3464

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  524 =>
--#line  3472

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  525 =>
--#line  3478

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
     

when  526 =>
--#line  3487
 
yyval := 
yy.value_stack(yy.tos); 

when  527 =>
--#line  3488
 
yyval := 
yy.value_stack(yy.tos); 

when  528 =>
--#line  3489
 
yyval := 
yy.value_stack(yy.tos); 

when  529 =>
--#line  3493
 
yyval := 
yy.value_stack(yy.tos); 

when  530 =>
--#line  3494

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
    

when  531 =>
--#line  3525
 
yyval := 
yy.value_stack(yy.tos); 

when  532 =>
--#line  3526

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  533 =>
--#line  3532

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  534 =>
--#line  3538

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  535 =>
--#line  3544

	
yyval := (One_Tree, Unary.Make(
	  Operator => Unary.Is_Null_Op,
	  Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  536 =>
--#line  3549

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
    

when  537 =>
--#line  3560

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Is_Function_Of,
	  Prefix => 
yy.value_stack(yy.tos-5).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  538 =>
--#line  3569
 
yyval := 
yy.value_stack(yy.tos); 

when  539 =>
--#line  3570

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  540 =>
--#line  3579
 
yyval := 
yy.value_stack(yy.tos); 

when  541 =>
--#line  3580
 
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  542 =>
--#line  3603
 
yyval := 
yy.value_stack(yy.tos); 

when  543 =>
--#line  3604

        --  NOTE: We treat '+' here separately to avoid
        --        reduce/reduce conflicts
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Plus_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  544 =>
--#line  3612

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  545 =>
--#line  3621
 
yyval := 
yy.value_stack(yy.tos); 

when  546 =>
--#line  3622

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  547 =>
--#line  3631
 
yyval := 
yy.value_stack(yy.tos); 

when  548 =>
--#line  3632

	 -- right associative
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  549 =>
--#line  3639

	-- unary ops have higher precedence 
	-- than every operator except the power_operator.
	
yyval := (One_Tree, Unary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Unary_Op,
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  550 =>
--#line  3649
 
yyval := 
yy.value_stack(yy.tos); 

when  551 =>
--#line  3650
 
yyval := 
yy.value_stack(yy.tos); 

when  552 =>
--#line  3651
 
yyval := 
yy.value_stack(yy.tos-1); 

when  553 =>
--#line  3652
 
yyval := 
yy.value_stack(yy.tos-1); 

when  554 =>
--#line  3653
 
yyval := 
yy.value_stack(yy.tos-1); 

when  555 =>
--#line  3654

        
yyval := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  556 =>
--#line  3658

	-- Use "(type)" to specify type of literal and
	-- to disambiguate operator specified as a string.
	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  557 =>
--#line  3665

	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  558 =>
--#line  3670
 
yyval := 
yy.value_stack(yy.tos); 

when  559 =>
--#line  3671

        --  This is used in a map_reduce expression to specify the initial val
        
yyval := (One_Tree, Unary.Make(Unary.Initial_Value_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  560 =>
--#line  3679
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  561 =>
--#line  3682
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  562 =>
--#line  3685
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  563 =>
--#line  3688
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  564 =>
--#line  3691
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make("null", 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  565 =>
--#line  3697
 
yyval := 
yy.value_stack(yy.tos); 

when  566 =>
--#line  3698

        
yyval := 
yy.value_stack(yy.tos);
    

when  567 =>
--#line  3704
 
        
yyval := (One_Tree, Unary.Make(Unary.Updated_Value_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  568 =>
--#line  3708
 
yyval := 
yy.value_stack(yy.tos); 

when  569 =>
--#line  3712

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  570 =>
--#line  3718
 
yyval := 
yy.value_stack(yy.tos); 

when  571 =>
--#line  3719

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  572 =>
--#line  3727

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  573 =>
--#line  3733

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  574 =>
--#line  3742
 
yyval := 
yy.value_stack(yy.tos); 

when  575 =>
--#line  3743

	
yyval := (One_Tree, Property.Make(Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Property_Id => PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)));
    

when  576 =>
--#line  3750
 
yyval := (Optional, True); 

when  577 =>
--#line  3751
 
yyval := (Optional, False); 

when  578 =>
--#line  3755

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  579 =>
--#line  3758

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  580 =>
--#line  3765
 
yyval := 
yy.value_stack(yy.tos); 

when  581 =>
--#line  3766

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  582 =>
--#line  3773
 
yyval := 
yy.value_stack(yy.tos); 

when  583 =>
--#line  3776
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  584 =>
--#line  3777
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  585 =>
--#line  3778
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Abs_Op); 

when  586 =>
--#line  3779
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Not_Op); 

when  587 =>
--#line  3780
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  588 =>
--#line  3781
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  589 =>
--#line  3785
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  590 =>
--#line  3786
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Plus_Op); 

when  591 =>
--#line  3787
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  592 =>
--#line  3791
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Times_Op); 

when  593 =>
--#line  3792
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Divide_Op); 

when  594 =>
--#line  3793
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Mod_Op); 

when  595 =>
--#line  3794
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Rem_Op); 

when  596 =>
--#line  3797
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Power_Op); 
    

when  597 =>
--#line  3801
 
yyval := 
yy.value_stack(yy.tos); 

when  598 =>
--#line  3802
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Divide_Assign_Op); 
    

when  599 =>
--#line  3808

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
     

when  600 =>
--#line  3811
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Plus_Assign_Op); 
    

when  601 =>
--#line  3814
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Minus_Assign_Op); 
    

when  602 =>
--#line  3817
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Times_Assign_Op); 
    

when  603 =>
--#line  3820
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Power_Assign_Op); 
    

when  604 =>
--#line  3823
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Combine_Assign_Op); 
    

when  605 =>
--#line  3826
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.And_Assign_Op); 
    

when  606 =>
--#line  3829
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Or_Assign_Op); 
    

when  607 =>
--#line  3832
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Xor_Assign_Op); 
    

when  608 =>
--#line  3835

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Left_Shift_Assign_Op);
    

when  609 =>
--#line  3838

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Right_Shift_Assign_Op);
    

when  610 =>
--#line  3843
 
	yyerror("Use ""="" rather than "":="" in Javallel");
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  611 =>
--#line  3847
 
yyval := 
yy.value_stack(yy.tos); 

when  612 =>
--#line  3850

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  613 =>
--#line  3856
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Compare_Op); 

when  614 =>
--#line  3857
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op); 

when  615 =>
--#line  3858
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op); 

when  616 =>
--#line  3859
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Less_Op); 

when  617 =>
--#line  3860
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.LEQ_Op); 

when  618 =>
--#line  3861
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Greater_Op); 

when  619 =>
--#line  3862
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.GEQ_Op); 

when  620 =>
--#line  3863
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Left_Shift_Op ); 

when  621 =>
--#line  3864
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Right_Shift_Op); 

when  622 =>
--#line  3865
 
	yyerror("Use ""=="" rather than ""="" in Javallel");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op);
    

when  623 =>
--#line  3872
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.And_Op); 

when  624 =>
--#line  3873
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Or_Op); 

when  625 =>
--#line  3874
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Xor_Op); 

when  626 =>
--#line  3876
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.And_Then_Op); 

when  627 =>
--#line  3878
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Or_Else_Op); 

when  628 =>
--#line  3879
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Implies_Op); 

when  629 =>
--#line  3883
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Interval_Op); 
    

when  630 =>
--#line  3886
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Interval_Op); 
    

when  631 =>
--#line  3889
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Open_Interval_Op); 
    

when  632 =>
--#line  3892
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Closed_Interval_Op); 
    

when  633 =>
--#line  3898
 
yyval := 
yy.value_stack(yy.tos); 

when  634 =>
--#line  3899
 
yyval := 
yy.value_stack(yy.tos); 

when  636 =>
--#line  3904

	-- Type of aggregate specified
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  637 =>
--#line  3914
 
yyval := 
yy.value_stack(yy.tos); 

when  638 =>
--#line  3917

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  639 =>
--#line  3927

	
yyval := 
yy.value_stack(yy.tos);
    

when  640 =>
--#line  3930

	
yyval := (One_List, Lists.Empty_List);
    

when  641 =>
--#line  3936

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  642 =>
--#line  3939

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  643 =>
--#line  3946
 
yyval := 
yy.value_stack(yy.tos); 

when  644 =>
--#line  3947

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  645 =>
--#line  3952

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  646 =>
--#line  3962

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
    

when  647 =>
--#line  4015

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
yy.value_stack(yy.tos-3).Tree,
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
yy.value_stack(yy.tos-3).Tree,
	      Operands => 
yy.value_stack(yy.tos-1).List,
              Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
	end if;
      end;
    

when  648 =>
--#line  4064
 
yyval := 
yy.value_stack(yy.tos); 

when  649 =>
--#line  4065

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  650 =>
--#line  4068

	
yyval := (One_List, Lists.Empty_List);
    

when  651 =>
--#line  4074

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  652 =>
--#line  4077

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  653 =>
--#line  4084
 
yyval := 
yy.value_stack(yy.tos); 

when  654 =>
--#line  4085

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  655 =>
--#line  4090

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  656 =>
--#line  4096

	-- This gives an ID to an expression which can be used
	-- to parameterize the initial value expression for each element.
        -- TBD: Allow nested iterators, e.g.:
        --        [for I in 1..10 => for J in 1..10 => I + J]
        --      In such nested iterators, unless there is a "<|=" operator,
        --      number of keys must match number of "index" parameters in
        --      "var_indexing" operator.  If only the last iterator
        --      has an explicit key specified, then that is treated as
        --      the single index into "var_indexing."
      declare
        Value : Optional_Tree := 
yy.value_stack(yy.tos).Tree;
        use type PSC.Strings.U_String;
      begin
        if Not_Null(
yy.value_stack(yy.tos-2).Tree) then
           -- User has specified a key for the element
           Value := Reference.Make(
             Key => 
yy.value_stack(yy.tos-2).Tree,
             Referent => Value);
        end if;

        if 
yy.value_stack(yy.tos-3).Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("forward" is the default)
            Iterator.Add_Direction(
yy.value_stack(yy.tos-5).Tree, 
yy.value_stack(yy.tos-3).Str);
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos,
          Kind => For_Loop_Construct.Container_Comprehension,
          Iterators => Lists.Make((1 => 
yy.value_stack(yy.tos-5).Tree)),
          Filter => 
yy.value_stack(yy.tos-4).List,
          Loop_Body => Value));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos);
      end;
    

when  657 =>
--#line  4134

        
yyval := (One_Tree, Null_Optional_Tree);
    

when  658 =>
--#line  4138

        
yyval := 
yy.value_stack(yy.tos);
    

when  659 =>
--#line  4144
 
yyval := 
yy.value_stack(yy.tos); 

when  660 =>
--#line  4145
 
yyval := 
yy.value_stack(yy.tos); 

when  661 =>
--#line  4151

	
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
    

when  662 =>
--#line  4163

	
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
    

when  663 =>
--#line  4171

	
yyval := 
yy.value_stack(yy.tos);
    

when  664 =>
--#line  4174

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  665 =>
--#line  4182

	
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
    

when  666 =>
--#line  4193

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  667 =>
--#line  4196

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  668 =>
--#line  4203

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  669 =>
--#line  4210

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
    

when  670 =>
--#line  4225

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
    

when  671 =>
--#line  4242

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-3).Tree;
     begin
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
yy.value_stack(yy.tos-2).Tree,
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
    

when  672 =>
--#line  4271

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-4).Tree;
     begin
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
yy.value_stack(yy.tos-3).Tree,
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
    

when  673 =>
--#line  4300

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-2).Tree;
     begin
        if not 
yy.value_stack(yy.tos-3).Is_Present then
           yyerror ("Must specify ""for all [E : ] T"" or " &
             """for all/some E in/of Container"" in quantified expression",
             At_Token => 
yy.value_stack(yy.tos-2));
        end if;

        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
          Kind => For_Loop_Construct.Univ_Quantified_Expr,
          Iterators => Lists.Make((1 =>
            Iterator.Make(
              Kind => Iterator.Set_Iterator,
              Name => 
yy.value_stack(yy.tos-2).Tree,
              Is_Ref => False,
              Obj_Type => Obj_Type,
              Obj_Value => Null_Optional_Tree))),
          Filter => Lists.Empty_List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos);
     end;
    

when  674 =>
--#line  4329

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-3).Tree;
     begin
        if not 
yy.value_stack(yy.tos-4).Is_Present then
           yyerror ("Must specify ""for all [E :] T"" or " &
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
          Filter => 
yy.value_stack(yy.tos-2).List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree));
        Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos);
     end;
    

when  675 =>
--#line  4360
 
yyval := 
yy.value_stack(yy.tos); 

when  676 =>
--#line  4361
 
yyval := 
yy.value_stack(yy.tos); 

when  677 =>
--#line  4362
 
yyval := 
yy.value_stack(yy.tos); 

when  678 =>
--#line  4366
 
yyval := (Optional, True); 

when  679 =>
--#line  4367
 
yyval := (Optional, False); 

when  680 =>
--#line  4371
 
yyval := 
yy.value_stack(yy.tos); 

when  681 =>
--#line  4372
 
yyval := 
yy.value_stack(yy.tos); 

when  682 =>
--#line  4373
 
yyval := 
yy.value_stack(yy.tos); 

when  683 =>
--#line  4378

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
    

when  684 =>
--#line  4402
 
yyval := 
yy.value_stack(yy.tos); 

when  685 =>
--#line  4403
 
yyval := 
yy.value_stack(yy.tos); 

when  686 =>
--#line  4404
 
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

end javallel_parser;
