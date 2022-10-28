
pragma Style_Checks (Off);
with parasail_tokens, ParaSail_Lex, parasail_goto, parasail_shift_reduce;
with text_io;

use  parasail_tokens, ParaSail_Lex, parasail_goto, parasail_shift_reduce;
use  text_io;

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

package body ParaSail_Parser is

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
           when One_Token | Construct_Qualifier |
             One_Unary_Op | One_Binary_Op | One_Assign_Op |
             Optional_End_Token =>
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
     At_Token : ParaSail_Tokens.YYSType := (ParaSail_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Error(S, Src_Pos => Token_Src_Pos (At_Token));
    end yyerror;

   procedure Parser_Warning (S : String;
     At_Token : ParaSail_Tokens.YYSType := (ParaSail_Tokens.Optional,
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
      Parasail_Goto;
    package yy_shift_reduce_tables renames
      Parasail_Shift_Reduce;
    package yy_tokens              renames
      Parasail_Tokens;

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
--#line  260

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  4 =>
--#line  267

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  5 =>
--#line  274

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  6 =>
--#line  281

	null;
    

when  10 =>
--#line  288

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
--#line  300

	
yyval := (One_List, Lists.Empty_List);
    

when  12 =>
--#line  303

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);  
	  -- TBD: Do we care how these were grouped?
    

when  13 =>
--#line  311

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  14 =>
--#line  314

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  15 =>
--#line  321

        
yyval := (One_Tree, PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  16 =>
--#line  324

        
yyval := 
yy.value_stack(yy.tos);
    

when  17 =>
--#line  327

	
yyval := (One_Tree, Qualified_Name.Make (
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  18 =>
--#line  342

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
	  Is_Abstract => 
yy.value_stack(yy.tos-10).Is_Abstract,
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
--#line  380

	if 
yy.value_stack(yy.tos-8).Is_Private and then 
yy.value_stack(yy.tos-5).Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-6).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-6).Tree),
	  Is_Interface => True,
	  Is_Abstract => 
yy.value_stack(yy.tos-8).Is_Abstract,
	  Is_Private => 
yy.value_stack(yy.tos-8).Is_Private,
	  Is_Concurrent => 
yy.value_stack(yy.tos-8).Is_Concurrent,
	  Is_Limited => False,
	  Has_Formals => 
yy.value_stack(yy.tos-5).Has_Module_Formals,
	  Module_Formals => 
yy.value_stack(yy.tos-5).Module_Formals,
	  Extends_Interface => 
yy.value_stack(yy.tos-5).Extends,
	  Implements_Interfaces => 
yy.value_stack(yy.tos-5).Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));

	Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-6).Tree),
	  Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
    

when  20 =>
--#line  405

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
	  Is_Abstract => 
yy.value_stack(yy.tos-4).Is_Abstract,
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
    

when  21 =>
--#line  430
 
yyval := 
yy.value_stack(yy.tos-1); 

when  22 =>
--#line  431

        yyerror ("Syntax error before ""is""", At_Token => 
yy.value_stack(yy.tos-1));
    

when  23 =>
--#line  437
 
yyval := 
yy.value_stack(yy.tos-1); 

when  24 =>
--#line  438
 
yyval := 
yy.value_stack(yy.tos); 

when  25 =>
--#line  439

        yyerror ("Syntax error before ""is""", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  28 =>
--#line  448

        yyerror ("Syntax error at end-of-line", At_Token => 
yy.value_stack(yy.tos));
    

when  29 =>
--#line  451

        yyerror ("Syntax error before ':'", At_Token => 
yy.value_stack(yy.tos-1));
    

when  30 =>
--#line  457
 
yyval := 
yy.value_stack(yy.tos); 

when  31 =>
--#line  458
 
yyval := 
yy.value_stack(yy.tos); 

when  32 =>
--#line  461
 
yyval := 
yy.value_stack(yy.tos); 

when  33 =>
--#line  464

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  34 =>
--#line  469

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-3)),
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  37 =>
--#line  479

	yyerror("Should be ""end interface <id>"" rather than ""end <id>""");
    

when  38 =>
--#line  485
 
yyval := 
yy.value_stack(yy.tos); 

when  39 =>
--#line  486

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  40 =>
--#line  494
 
yyval := 
yy.value_stack(yy.tos); 

when  41 =>
--#line  495

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  42 =>
--#line  502

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  43 =>
--#line  512
 
yyval := 
yy.value_stack(yy.tos); 

when  44 =>
--#line  513
 
	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  45 =>
--#line  520

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	       Is_Concurrent => True, others => False);
    

when  46 =>
--#line  527

        --  NOTE: We don't allow stand-alone operator definitions
        
yyval := 
yy.value_stack(yy.tos);
  

when  47 =>
--#line  531
 
yyval := 
yy.value_stack(yy.tos-1); 

when  48 =>
--#line  532
 
yyval := 
yy.value_stack(yy.tos-1); 

when  49 =>
--#line  536
 
yyval := 
yy.value_stack(yy.tos-1); 

when  50 =>
--#line  537

        --  TBD: Do something with global_access_list
        
yyval := 
yy.value_stack(yy.tos-3);
    

when  51 =>
--#line  541
 
yyval := (One_List, Lists.Empty_List); 

when  52 =>
--#line  542

        --  TBD: Do something with global_access_list
        
yyval := (One_List, Lists.Empty_List);
    

when  53 =>
--#line  549

	
yyval := (Formals_And_Interfaces,
	  Has_Module_Formals => (
yy.value_stack(yy.tos-1).Kind = One_List),
	  Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-1)),
	  Extends => Null_Optional_Tree,
	  Implements => 
yy.value_stack(yy.tos).List);
    

when  54 =>
--#line  556

        
yyval := (Formals_And_Interfaces,
          Has_Module_Formals => (
yy.value_stack(yy.tos-3).Kind = One_List),
          Module_Formals => List_Or_Empty (
yy.value_stack(yy.tos-3)),
          Extends => Param_Decl.Make(
            Name => Null_Optional_Tree,
            Kind => Param_Decl.Var_Param,  --  So is writable
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => 
yy.value_stack(yy.tos-1).Tree,
            Param_Default => Null_Optional_Tree),
          Implements => 
yy.value_stack(yy.tos).List);
    

when  55 =>
--#line  569

      declare
	Extends_Decl : constant Optional_Tree := Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => Param_Decl.Var_Param,  --  So is writable
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
    

when  57 =>
--#line  588

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    

when  58 =>
--#line  598
 
yyval := 
yy.value_stack(yy.tos); 

when  59 =>
--#line  599
 
	
yyval := (Optional, Is_Present => False);
    

when  60 =>
--#line  605
 
yyval := 
yy.value_stack(yy.tos); 

when  61 =>
--#line  606

	
yyval := (One_List, Lists.Empty_List);
    

when  62 =>
--#line  611
 
yyval := 
yy.value_stack(yy.tos); 

when  63 =>
--#line  614
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  64 =>
--#line  617

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  65 =>
--#line  624
 
yyval := 
yy.value_stack(yy.tos); 

when  66 =>
--#line  625
 
yyval := 
yy.value_stack(yy.tos); 

when  67 =>
--#line  628
 
yyval := 
yy.value_stack(yy.tos); 

when  68 =>
--#line  631
 
yyval := 
yy.value_stack(yy.tos); 

when  69 =>
--#line  632
 
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  70 =>
--#line  641

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  71 =>
--#line  646
 
yyval := 
yy.value_stack(yy.tos); 

when  72 =>
--#line  647

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  73 =>
--#line  654

	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos).List);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  74 =>
--#line  659

	
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
    

when  75 =>
--#line  676
 
yyval := 
yy.value_stack(yy.tos); 

when  76 =>
--#line  677

	
yyval := (One_List, Lists.Empty_List);
    

when  77 =>
--#line  683
 
yyval := 
yy.value_stack(yy.tos); 

when  78 =>
--#line  684

	
yyval := (One_List, Lists.Empty_List);
    

when  79 =>
--#line  690

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  80 =>
--#line  696
 
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  81 =>
--#line  704

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  82 =>
--#line  715
 
yyval := 
yy.value_stack(yy.tos); 

when  83 =>
--#line  716
 
yyval := (One_Tree, Null_Optional_Tree); 

when  84 =>
--#line  722

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
    

when  85 =>
--#line  741
 
yyval := 
yy.value_stack(yy.tos); 

when  86 =>
--#line  742
 
yyval := (One_Tree, Null_Optional_Tree); 

when  87 =>
--#line  746

	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos));
    

when  88 =>
--#line  755

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  89 =>
--#line  758

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  90 =>
--#line  765
 
yyval := 
yy.value_stack(yy.tos); 

when  91 =>
--#line  766
 
yyval := 
yy.value_stack(yy.tos); 

when  92 =>
--#line  769

	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  93 =>
--#line  776
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  94 =>
--#line  779

	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  95 =>
--#line  784

        --  Using Rust's "TurboFish" notation to provide module params
        
yyval := (One_Tree, Invocation.Make(
          Kind => Invocation.Module_Instantiation,
          Prefix => 
yy.value_stack(yy.tos-4).Tree,
          Operands => 
yy.value_stack(yy.tos-1).List));
    

when  96 =>
--#line  794
 
yyval := 
yy.value_stack(yy.tos); 

when  97 =>
--#line  795

        -- String_Literal can be used as a "name" when it is an operator
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  98 =>
--#line  802

        
yyval := (Input_And_Global_Lists, Global_Read_List => 
yy.value_stack(yy.tos).List,
               others => Lists.Empty_List);
    

when  99 =>
--#line  806

        
yyval := (Input_And_Global_Lists,
               Global_Read_List => 
yy.value_stack(yy.tos-2).List,
               Global_Update_List => 
yy.value_stack(yy.tos).List,
               others => Lists.Empty_List);
    

when  100 =>
--#line  812

        
yyval := (Input_And_Global_Lists, Global_Update_List => 
yy.value_stack(yy.tos).List,
               others => Lists.Empty_List);
    

when  101 =>
--#line  819

	
yyval := 
yy.value_stack(yy.tos);
    

when  102 =>
--#line  825

	
yyval := 
yy.value_stack(yy.tos);
    

when  103 =>
--#line  831

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  104 =>
--#line  837

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
    

when  105 =>
--#line  851
 
yyval := 
yy.value_stack(yy.tos); 

when  106 =>
--#line  852
 
	
yyval := (One_List, Lists.Empty_List);
    

when  107 =>
--#line  858
 
yyval := 
yy.value_stack(yy.tos); 

when  108 =>
--#line  859

	
yyval := (One_List, Lists.Empty_List);
    

when  109 =>
--#line  865

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  110 =>
--#line  868

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  111 =>
--#line  875
 
yyval := 
yy.value_stack(yy.tos); 

when  112 =>
--#line  876

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  113 =>
--#line  885
 
	-- polymorphic type name not allowed here
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  114 =>
--#line  890
 
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  115 =>
--#line  894

	-- This is a polymorphic type name, presumably.
	-- We use adding_expression instead of qualified_name
	-- to avoid reduce/reduce conflicts in the grammar.
	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  116 =>
--#line  902
 
yyval := 
yy.value_stack(yy.tos); 

when  117 =>
--#line  904
 
yyval := 
yy.value_stack(yy.tos); 

when  118 =>
--#line  905
 
yyval := 
yy.value_stack(yy.tos); 

when  119 =>
--#line  906
 
yyval := 
yy.value_stack(yy.tos); 

when  120 =>
--#line  910
 
yyval := 
yy.value_stack(yy.tos); 

when  121 =>
--#line  911

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  122 =>
--#line  915

        
yyval := 
yy.value_stack(yy.tos);
    

when  123 =>
--#line  918

        
yyval := 
yy.value_stack(yy.tos);
    

when  124 =>
--#line  924

        
yyval := (One_Tree, Invocation.Make
	        (Kind => Invocation.Tuple_Type_Definition,
	         Prefix => Null_Optional_Tree,
	         Operands => 
yy.value_stack(yy.tos-1).List,
	         Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  125 =>
--#line  934

        
yyval := 
yy.value_stack(yy.tos);
    

when  126 =>
--#line  937

        
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  127 =>
--#line  941

	yyerror("Tuple types must be separated by "";""",
          At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  128 =>
--#line  950

      declare
	Id_List : Lists.List := 
yy.value_stack(yy.tos-2).List;
      begin
	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(Id_List) loop
            declare
                Id : constant Optional_Tree :=
                  Lists.Nth_Element(Id_List, I);
            begin
                if Tree_Ptr_Of (Id).all not in
                  PSC.Trees.Identifier.Tree then
                    PSC.Messages.Parser_Error
                      ("Tuple component name must be a simple identifier",
                       Src_Pos => Find_Source_Pos (Id));
                else
                    Lists.Append(
yyval.List, Obj_Decl.Make(
                      Name => PSC.Trees.Identifier.Tree (Tree_Ptr_Of (Id).all),
                      Is_Var => False,
                      Is_Const => True,
                      Is_Ref => False,
                      Is_Optional => False,
                      In_Region => Null_Optional_Tree,
                      Obj_Type => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I),
                      Obj_Value => Null_Optional_Tree));
                end if;
            end;
	end loop;
      end;
    

when  129 =>
--#line  983
 
yyval := 
yy.value_stack(yy.tos); 

when  130 =>
--#line  984
 
yyval := 
yy.value_stack(yy.tos); 

when  131 =>
--#line  988
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  132 =>
--#line  991
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  133 =>
--#line  994
 
	
yyval := (One_Tree, Invocation.Add_Extends(
	  Instantiation => 
yy.value_stack(yy.tos-2).Tree, 
	  Extends => 
yy.value_stack(yy.tos).Tree));
    

when  134 =>
--#line  1002
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  135 =>
--#line  1009
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  136 =>
--#line  1017
 
	
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
    

when  137 =>
--#line  1029
 
	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Present,
	  others => False);
    

when  138 =>
--#line  1036

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Concurrent => True, others => False);
    

when  139 =>
--#line  1044

	
yyval := (Optional, True);
    

when  140 =>
--#line  1047

	
yyval := (Optional, False);
    

when  141 =>
--#line  1053

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => 
yy.value_stack(yy.tos).Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => 
yy.value_stack(yy.tos).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos).Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-1)));
    

when  142 =>
--#line  1070

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).Inputs_List,
	  Operation_Outputs => 
yy.value_stack(yy.tos).List,
          Global_Read_List => 
yy.value_stack(yy.tos-2).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos-2).Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-3)));
    

when  143 =>
--#line  1089
 
yyval := (One_List, Lists.Empty_List); 

when  144 =>
--#line  1090

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
    

when  147 =>
--#line  1109

	
yyval := (One_List, Lists.Empty_List);
    

when  148 =>
--#line  1112

	
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
    

when  149 =>
--#line  1121

	
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
    

when  150 =>
--#line  1130

	
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
    

when  151 =>
--#line  1143

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  152 =>
--#line  1149
 
yyval := 
yy.value_stack(yy.tos-1); 

when  153 =>
--#line  1150
 
yyval := 
yy.value_stack(yy.tos); 

when  154 =>
--#line  1154
 
yyval := 
yy.value_stack(yy.tos); 

when  155 =>
--#line  1155
 
yyval := 
yy.value_stack(yy.tos); 

when  156 =>
--#line  1156
 
yyval := 
yy.value_stack(yy.tos); 

when  157 =>
--#line  1160
 
yyval := 
yy.value_stack(yy.tos); 

when  158 =>
--#line  1161

	
yyval := (One_List, Lists.Empty_List);
    

when  159 =>
--#line  1167
 
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
    

when  160 =>
--#line  1181

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
    

when  161 =>
--#line  1195

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
    

when  164 =>
--#line  1213
 
yyval := 
yy.value_stack(yy.tos); 

when  165 =>
--#line  1214
 
yyval := 
yy.value_stack(yy.tos-1); 

when  166 =>
--#line  1218
 
yyval := 
yy.value_stack(yy.tos); 

when  167 =>
--#line  1219

	
yyval := (One_List, Lists.Empty_List);
    

when  168 =>
--#line  1225
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    

when  169 =>
--#line  1235
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => 
yy.value_stack(yy.tos-1).List,
	  Elements => Elem_List))));
      end;
    

when  170 =>
--#line  1245

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
    

when  171 =>
--#line  1258

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  172 =>
--#line  1262

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  174 =>
--#line  1270

        --  Pop the indent stack
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;
    

when  175 =>
--#line  1279

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  176 =>
--#line  1283

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  177 =>
--#line  1287

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  178 =>
--#line  1292

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  179 =>
--#line  1297

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  180 =>
--#line  1303

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  181 =>
--#line  1309

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);
	end;
    

when  182 =>
--#line  1325

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);
	end;
    

when  183 =>
--#line  1341

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    

when  184 =>
--#line  1349

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    

when  185 =>
--#line  1357

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    

when  186 =>
--#line  1367

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    

when  187 =>
--#line  1377

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);
            if ParaSail_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  188 =>
--#line  1397

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);
            if ParaSail_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  189 =>
--#line  1426

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
   

when  190 =>
--#line  1460

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

   

when  191 =>
--#line  1492

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  192 =>
--#line  1497

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-3)),
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  194 =>
--#line  1505

	yyerror("Should be ""end class <id>"" rather than ""end <id>""");
    

when  195 =>
--#line  1510
 
yyval := (One_List, Lists.Empty_List); 

when  196 =>
--#line  1511

	if Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- We want to make sure that we return a non-empty list
	    
yyval := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    
yyval := 
yy.value_stack(yy.tos);
	end if;
    

when  197 =>
--#line  1523

	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  198 =>
--#line  1534

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
    

when  199 =>
--#line  1546

	yyerror("Missing ""exports"" keyword");
	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  203 =>
--#line  1554

	
yyval := (One_List, Lists.Empty_List);
    

when  204 =>
--#line  1557

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  205 =>
--#line  1564
 
yyval := 
yy.value_stack(yy.tos); 

when  206 =>
--#line  1565
 
yyval := 
yy.value_stack(yy.tos-1); 

when  207 =>
--#line  1566
 
yyval := 
yy.value_stack(yy.tos-1); 

when  208 =>
--#line  1567
 
yyval := 
yy.value_stack(yy.tos); 

when  209 =>
--#line  1570

	
yyval := (One_List, Lists.Empty_List);
    

when  210 =>
--#line  1573

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  211 =>
--#line  1577

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  212 =>
--#line  1581

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  213 =>
--#line  1585

	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  214 =>
--#line  1591

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  215 =>
--#line  1597
 
yyval := 
yy.value_stack(yy.tos); 

when  216 =>
--#line  1598
 
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  217 =>
--#line  1601

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  218 =>
--#line  1608
 
yyval := 
yy.value_stack(yy.tos); 

when  219 =>
--#line  1609
 
yyval := 
yy.value_stack(yy.tos); 

when  220 =>
--#line  1610
 
yyval := 
yy.value_stack(yy.tos); 

when  221 =>
--#line  1614
 
yyval := 
yy.value_stack(yy.tos); 

when  222 =>
--#line  1615
 
yyval := 
yy.value_stack(yy.tos); 

when  223 =>
--#line  1616

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  224 =>
--#line  1623
 
yyval := 
yy.value_stack(yy.tos-1); 

when  225 =>
--#line  1624

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  226 =>
--#line  1630
 
yyval := 
yy.value_stack(yy.tos-1); 

when  227 =>
--#line  1631

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  228 =>
--#line  1638

        
yyval := 
yy.value_stack(yy.tos);
    

when  229 =>
--#line  1641

        --  A labeled annotation list becomes a separate nested annotation
        
yyval := (One_List, Lists.Make
                 ((1 => Annotation.Make
                   (Annotations => 
yy.value_stack(yy.tos).List, Label => 
yy.value_stack(yy.tos-1).Tree))));
    

when  230 =>
--#line  1651

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  231 =>
--#line  1654

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  232 =>
--#line  1658

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  233 =>
--#line  1664
 
yyval := 
yy.value_stack(yy.tos); 

when  234 =>
--#line  1665
 
yyval := 
yy.value_stack(yy.tos); 

when  235 =>
--#line  1666
 
yyval := 
yy.value_stack(yy.tos); 

when  236 =>
--#line  1667
 
yyval := 
yy.value_stack(yy.tos); 

when  237 =>
--#line  1668
 
yyval := 
yy.value_stack(yy.tos); 

when  238 =>
--#line  1669

        --  An annotation of the form <property_id> => <expression>
        --  is used to associate a property with a declaration,
        --  such as "convention => #c," similar to an Ada "aspect
        --  specification."
	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  239 =>
--#line  1678

	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  240 =>
--#line  1688
 
yyval := 
yy.value_stack(yy.tos); 

when  241 =>
--#line  1691
 
yyval := 
yy.value_stack(yy.tos); 

when  242 =>
--#line  1692
 
yyval := 
yy.value_stack(yy.tos); 

when  243 =>
--#line  1697

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-1).Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => 
yy.value_stack(yy.tos-1).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos-1).Global_Update_List,
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
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-3)));
    

when  244 =>
--#line  1716

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-4).Inputs_List,
	  Operation_Outputs => 
yy.value_stack(yy.tos-1).List,
          Global_Read_List => 
yy.value_stack(yy.tos-4).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos-4).Global_Update_List,
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
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-6)));
    

when  245 =>
--#line  1736
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Abstract => True, others => False); 
    

when  246 =>
--#line  1741
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Optional => True, others => False); 
    

when  247 =>
--#line  1746
 
yyval := (Construct_Qualifier,
             Source_Pos => PSC.Source_Positions.Null_Source_Position,
             others => False); 

when  248 =>
--#line  1752
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  249 =>
--#line  1757
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Optional => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  250 =>
--#line  1762
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  251 =>
--#line  1770

	
yyval := (Optional, Is_Present => True);
    

when  252 =>
--#line  1773

	
yyval := (Optional, Is_Present => False);
    

when  253 =>
--#line  1779
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  254 =>
--#line  1782

	yyerror("Operator designator must be in quotes");
	
yyval := 
yy.value_stack(yy.tos);
    

when  256 =>
--#line  1789

	yyerror("Use ""->"" in ParaSail rather than ""return""");
    

when  257 =>
--#line  1795

	
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
    

when  258 =>
--#line  1809

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-1).Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => 
yy.value_stack(yy.tos-1).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos-1).Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-3)));
    

when  259 =>
--#line  1827

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-5).Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-4).Inputs_List,
	  Operation_Outputs => 
yy.value_stack(yy.tos-1).List,
          Global_Read_List => 
yy.value_stack(yy.tos-4).Global_Read_List,
          Global_Update_List => 
yy.value_stack(yy.tos-4).Global_Update_List,
	  Preconditions => Annotation.Make (
yy.value_stack(yy.tos-3).List),
	  Postconditions => Annotation.Make (
yy.value_stack(yy.tos).List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-6)));
    

when  260 =>
--#line  1847

        if Qualified_Name.Contains_String (
yy.value_stack(yy.tos).Tree) then
            yyerror("Should use ""op"" rather than ""func"" for an operator");
        end if;
	
yyval := 
yy.value_stack(yy.tos);
    

when  261 =>
--#line  1856
 
	
yyval := (Input_And_Global_Lists,
          Inputs_List => Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)),
          others => Lists.Empty_List);
    

when  262 =>
--#line  1861

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  263 =>
--#line  1864

      declare
	Id_List : Lists.List := 
yy.value_stack(yy.tos-3).List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (Input_And_Global_Lists, others => Lists.Empty_List);
	Lists.Append(Id_List, 
yy.value_stack(yy.tos-1).Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append(
yyval.Inputs_List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    

when  264 =>
--#line  1883

	yyerror("ParaSail requires at least ""()"" in operation definition");
	
yyval := (Input_And_Global_Lists, others => Lists.Empty_List);
    

when  265 =>
--#line  1890
 
yyval := 
yy.value_stack(yy.tos); 

when  266 =>
--#line  1891
 
yyval := 
yy.value_stack(yy.tos); 

when  267 =>
--#line  1892

        yyerror ("Expecting one ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := (One_Token,
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.String_Lookup(")")); 
    

when  268 =>
--#line  1902

	
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
    

when  269 =>
--#line  1914

	
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
    

when  270 =>
--#line  1926
 
	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  271 =>
--#line  1938
 
yyval := 
yy.value_stack(yy.tos); 

when  272 =>
--#line  1939

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  273 =>
--#line  1947
 
yyval := 
yy.value_stack(yy.tos); 

when  274 =>
--#line  1948

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  275 =>
--#line  1953

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  276 =>
--#line  1958

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  277 =>
--#line  1963

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  278 =>
--#line  1968

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  279 =>
--#line  1973

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  280 =>
--#line  1978

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  281 =>
--#line  1986
 
yyval := 
yy.value_stack(yy.tos); 

when  282 =>
--#line  1987

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  283 =>
--#line  1995

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  284 =>
--#line  2000

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  285 =>
--#line  2005

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  286 =>
--#line  2013

      
yyval := (Input_And_Global_Lists,
             Inputs_List => 
yy.value_stack(yy.tos).List, others => Lists.Empty_List);
    

when  287 =>
--#line  2017

        
yyval := 
yy.value_stack(yy.tos);
        
yyval.Inputs_List := 
yy.value_stack(yy.tos-2).List;
    

when  288 =>
--#line  2021

        
yyval := 
yy.value_stack(yy.tos);
    

when  289 =>
--#line  2024

	
yyval := (Input_And_Global_Lists, others => Lists.Empty_List);
    

when  290 =>
--#line  2030
 
yyval := 
yy.value_stack(yy.tos); 

when  291 =>
--#line  2031

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  292 =>
--#line  2038

        
yyval := 
yy.value_stack(yy.tos-1);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- Add annotations to last element of list
	    Annotation.Add_Annotation(
	      Lists.Nth_Element(
yyval.List, Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	end if;
    

when  293 =>
--#line  2046

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
yyval.List,
                Lists.Length(
yyval.List)), 
yy.value_stack(yy.tos).List);
	end if;
    

when  294 =>
--#line  2062

	
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
    

when  295 =>
--#line  2080

	
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
    

when  296 =>
--#line  2096

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  297 =>
--#line  2106

	
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
    

when  298 =>
--#line  2118

	
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
    

when  299 =>
--#line  2128

	
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
    

when  300 =>
--#line  2140

	
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
    

when  301 =>
--#line  2155
 
yyval := 
yy.value_stack(yy.tos); 

when  302 =>
--#line  2156

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  303 =>
--#line  2164
 
yyval := 
yy.value_stack(yy.tos); 

when  304 =>
--#line  2165
 
yyval := 
yy.value_stack(yy.tos); 

when  305 =>
--#line  2169
 
yyval := 
yy.value_stack(yy.tos); 

when  306 =>
--#line  2170

         -- NOTE: Operation can have "type" parameters 
         -- such as "Left_Type is Integer<>"
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  307 =>
--#line  2178

        
yyval := 
yy.value_stack(yy.tos);
    

when  308 =>
--#line  2184

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  309 =>
--#line  2193
 
yyval := 
yy.value_stack(yy.tos); 

when  310 =>
--#line  2194

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  311 =>
--#line  2202

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  312 =>
--#line  2211

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  313 =>
--#line  2214

	Annotation.Add_Annotation(
yy.value_stack(yy.tos).Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  314 =>
--#line  2218

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  315 =>
--#line  2221

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
    

when  316 =>
--#line  2240

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
    

when  317 =>
--#line  2263

	
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
    

when  318 =>
--#line  2275

	
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
    

when  319 =>
--#line  2287

	
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
    

when  320 =>
--#line  2297

	
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
    

when  321 =>
--#line  2309

	
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
    

when  322 =>
--#line  2321

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  323 =>
--#line  2333

	
yyval := 
yy.value_stack(yy.tos);
    

when  324 =>
--#line  2336

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  325 =>
--#line  2343

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
    

when  326 =>
--#line  2353

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
    

when  327 =>
--#line  2369

	
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
    

when  328 =>
--#line  2386

	
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
    

when  329 =>
--#line  2401

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  330 =>
--#line  2410

	
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
    

when  331 =>
--#line  2422

	
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
    

when  332 =>
--#line  2431

	
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
    

when  333 =>
--#line  2447

	
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
    

when  334 =>
--#line  2459

	
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
    

when  335 =>
--#line  2470

	
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
    

when  336 =>
--#line  2483

	
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
    

when  337 =>
--#line  2495

	
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
    

when  338 =>
--#line  2507

	
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
    

when  339 =>
--#line  2518

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => Null_Optional_Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree,
          Is_Global => True));
    

when  340 =>
--#line  2531

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-3).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => Null_Optional_Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree,
          Is_Global => True));
    

when  341 =>
--#line  2545

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
    

when  342 =>
--#line  2560
 
yyval := 
yy.value_stack(yy.tos); 

when  343 =>
--#line  2561

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  344 =>
--#line  2567
 
yyval := 
yy.value_stack(yy.tos); 

when  345 =>
--#line  2568

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  346 =>
--#line  2574
 
yyval := 
yy.value_stack(yy.tos); 

when  347 =>
--#line  2575

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  348 =>
--#line  2580
 
yyval := 
yy.value_stack(yy.tos-1); 

when  349 =>
--#line  2583

	
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
    

when  350 =>
--#line  2594

	
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
    

when  351 =>
--#line  2604

	
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
    

when  352 =>
--#line  2614

	
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
    

when  353 =>
--#line  2624

	
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
    

when  354 =>
--#line  2635

	
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
    

when  355 =>
--#line  2648
 
yyval := 
yy.value_stack(yy.tos); 

when  356 =>
--#line  2649
 
yyval := 
yy.value_stack(yy.tos); 

when  357 =>
--#line  2652
 
yyval := 
yy.value_stack(yy.tos); 

when  358 =>
--#line  2655

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_New_Type => 
yy.value_stack(yy.tos-1).Is_Present,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  359 =>
--#line  2663

	
yyval := (Optional, True);
    

when  360 =>
--#line  2666

	
yyval := (Optional, False);
    

when  361 =>
--#line  2672
 
yyval := 
yy.value_stack(yy.tos); 

when  362 =>
--#line  2673
 
yyval := 
yy.value_stack(yy.tos); 

when  363 =>
--#line  2679

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
	        Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  364 =>
--#line  2697

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-6).Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => 
yy.value_stack(yy.tos-4));
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-4).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos-3).Source_Pos);
	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
	end;
    

when  365 =>
--#line  2714

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
	        Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  366 =>
--#line  2735

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  367 =>
--#line  2753

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  368 =>
--#line  2772

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-6).Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => 
yy.value_stack(yy.tos-4));
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-4).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos-3).Source_Pos);

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
	end;
    

when  369 =>
--#line  2790

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-7).Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => 
yy.value_stack(yy.tos-4));
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := 
yy.value_stack(yy.tos-5).Tree;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-4).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos-3).Source_Pos);

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
	end;
    

when  370 =>
--#line  2809

        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := 
yy.value_stack(yy.tos-2).Tree;
	    
yyval := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  371 =>
--#line  2827

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
            Set_End_Source_Pos (
yyval.Tree, 
yy.value_stack(yy.tos).Source_Pos);

            if 
yy.value_stack(yy.tos).Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => 
yy.value_stack(yy.tos).Label);
            end if;
	end;
    

when  373 =>
--#line  2847

	yyerror("Should be 'end op ""id""' not simply 'end ""id""'");
    

when  375 =>
--#line  2853

	yyerror("Should be ""end func <id>"" not simply ""end <id>""");
    

when  376 =>
--#line  2859
 
yyval := 
yy.value_stack(yy.tos); 

when  377 =>
--#line  2860
 
yyval := (One_Tree, Null_Optional_Tree); 

when  378 =>
--#line  2864
 
yyval := 
yy.value_stack(yy.tos); 

when  379 =>
--#line  2865
 
yyval := 
yy.value_stack(yy.tos); 

when  380 =>
--#line  2868
 
yyval := 
yy.value_stack(yy.tos-1); 

when  381 =>
--#line  2870
 
yyval := 
yy.value_stack(yy.tos-1); 

when  382 =>
--#line  2874

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree,
	  Complement => 
yy.value_stack(yy.tos-2).Is_While));  
	    -- Complement cond if "while" present
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  383 =>
--#line  2882
 
        --  Pop the indent stack
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;
    

when  385 =>
--#line  2892

        yyerror ("Syntax error before ':'", At_Token => 
yy.value_stack(yy.tos));
    

when  389 =>
--#line  2902
 
yyval := 
yy.value_stack(yy.tos); 

when  390 =>
--#line  2903
 
yyval := 
yy.value_stack(yy.tos); 

when  391 =>
--#line  2904

        yyerror("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  392 =>
--#line  2908

        yyerror("Syntax error in condition", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  393 =>
--#line  2915

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  394 =>
--#line  2920

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-3)),
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  395 =>
--#line  2928

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  396 =>
--#line  2933

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-3)),
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  397 =>
--#line  2941

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  398 =>
--#line  2946
 
yyval := 
yy.value_stack(yy.tos); 

when  399 =>
--#line  2947
 
yyval := 
yy.value_stack(yy.tos); 

when  400 =>
--#line  2951
 
yyval := 
yy.value_stack(yy.tos); 

when  401 =>
--#line  2952

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  402 =>
--#line  2961
 
yyval := 
yy.value_stack(yy.tos); 

when  403 =>
--#line  2962

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  404 =>
--#line  2971
 
yyval := 
yy.value_stack(yy.tos); 

when  405 =>
--#line  2972
 
yyval := 
yy.value_stack(yy.tos); 

when  406 =>
--#line  2976
 
yyval := 
yy.value_stack(yy.tos); 

when  407 =>
--#line  2977

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  408 =>
--#line  2987

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  409 =>
--#line  2998

	-- "begin" is not used in ParaSail; treat like "then" for now
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  410 =>
--#line  3005

	-- "begin" is not used in ParaSail
	
yyval := 
yy.value_stack(yy.tos);
    

when  411 =>
--#line  3011

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  412 =>
--#line  3021

        
yyval := 
yy.value_stack(yy.tos);
    

when  413 =>
--#line  3024

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  416 =>
--#line  3030

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    

when  417 =>
--#line  3039

	yyerror("No need for ""begin"" in ParaSail operation definition");
    

when  418 =>
--#line  3045
 
yyval := 
yy.value_stack(yy.tos); 

when  419 =>
--#line  3046

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  420 =>
--#line  3055

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  421 =>
--#line  3067
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  422 =>
--#line  3070

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  423 =>
--#line  3077

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  424 =>
--#line  3086

        
yyval := 
yy.value_stack(yy.tos);
    

when  425 =>
--#line  3089

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  426 =>
--#line  3094
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  427 =>
--#line  3097

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  428 =>
--#line  3104

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  429 =>
--#line  3114
 
yyval := 
yy.value_stack(yy.tos); 

when  430 =>
--#line  3115

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  431 =>
--#line  3124
 
yyval := 
yy.value_stack(yy.tos); 

when  432 =>
--#line  3125

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  433 =>
--#line  3134

        
yyval := 
yy.value_stack(yy.tos);
    

when  434 =>
--#line  3137

        
yyval := 
yy.value_stack(yy.tos);
    

when  435 =>
--#line  3143

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  436 =>
--#line  3149

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  437 =>
--#line  3154

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  438 =>
--#line  3158

	-- An annotation can appear by itself
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  439 =>
--#line  3165

            -- NOTE: these already allow trailing annotations
	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  440 =>
--#line  3170

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  441 =>
--#line  3174
 
yyval := 
yy.value_stack(yy.tos); 

when  442 =>
--#line  3176

	
yyval := 
yy.value_stack(yy.tos);
    

when  443 =>
--#line  3182
 
yyval := 
yy.value_stack(yy.tos-1); 

when  444 =>
--#line  3185
 
yyval := 
yy.value_stack(yy.tos); 

when  445 =>
--#line  3186
 
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
    

when  446 =>
--#line  3195
 
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
    

when  447 =>
--#line  3208

	
yyval := 
yy.value_stack(yy.tos);
  

when  448 =>
--#line  3211

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  449 =>
--#line  3217
 
	-- A "null" statement (i.e. a no-op)
	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Null_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => 
yy.value_stack(yy.tos).Source_Pos));
    

when  450 =>
--#line  3226

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  451 =>
--#line  3232
 
yyval := 
yy.value_stack(yy.tos); 

when  452 =>
--#line  3233

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Continue_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => 
yy.value_stack(yy.tos-1).Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos));
    

when  453 =>
--#line  3241

	
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
    

when  454 =>
--#line  3249

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos-1);
    

when  455 =>
--#line  3256

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    

when  456 =>
--#line  3266

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  457 =>
--#line  3274

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  458 =>
--#line  3285
 
yyval := 
yy.value_stack(yy.tos); 

when  459 =>
--#line  3286

	yyerror("""loop"" required after ""continue""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  460 =>
--#line  3295

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => 
yy.value_stack(yy.tos-1).Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  461 =>
--#line  3301

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Divide_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  462 =>
--#line  3307

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Combine_Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  463 =>
--#line  3313

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  464 =>
--#line  3319

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Swap_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  465 =>
--#line  3325

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
    

when  466 =>
--#line  3341
 
yyval := 
yy.value_stack(yy.tos); 

when  467 =>
--#line  3342

	
yyval := (One_List, Lists.Empty_List);
    

when  468 =>
--#line  3348
 
yyval := 
yy.value_stack(yy.tos); 

when  469 =>
--#line  3349

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  470 =>
--#line  3354

	-- NOTE: This used to be '(' operation_actual_list ')'
	--       but that prevented continuing with a single expression.
	
yyval := 
yy.value_stack(yy.tos);
    

when  471 =>
--#line  3361
 
yyval := 
yy.value_stack(yy.tos); 

when  472 =>
--#line  3362

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  473 =>
--#line  3368
 
yyval := 
yy.value_stack(yy.tos); 

when  474 =>
--#line  3369
 
	yyerror(
	  """loop,"" ""if,"" ""case,"" or ""block"" must follow ""exit""");
	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  475 =>
--#line  3377

	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  476 =>
--#line  3380

	
yyval := (Construct_Kind, Control_Stmt.If_Stmt);
    

when  477 =>
--#line  3383

	
yyval := (Construct_Kind, Control_Stmt.Case_Stmt);
    

when  478 =>
--#line  3386

	
yyval := (Construct_Kind, Control_Stmt.Block_Stmt);
    

when  479 =>
--#line  3391

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    

when  480 =>
--#line  3401
 
yyval := 
yy.value_stack(yy.tos); 

when  481 =>
--#line  3402
 
yyval := 
yy.value_stack(yy.tos); 

when  482 =>
--#line  3403
 
yyval := 
yy.value_stack(yy.tos); 

when  483 =>
--#line  3407
 
yyval := 
yy.value_stack(yy.tos); 

when  484 =>
--#line  3408
 
yyval := 
yy.value_stack(yy.tos); 

when  485 =>
--#line  3409
 
yyval := 
yy.value_stack(yy.tos-1); 

when  486 =>
--#line  3412
 
yyval := 
yy.value_stack(yy.tos); 

when  487 =>
--#line  3413
 
yyval := 
yy.value_stack(yy.tos-1); 

when  488 =>
--#line  3414
 
yyval := 
yy.value_stack(yy.tos-1); 

when  489 =>
--#line  3417
 
yyval := 
yy.value_stack(yy.tos-1); 

when  490 =>
--#line  3420
 
yyval := 
yy.value_stack(yy.tos); 

when  491 =>
--#line  3421
 
yyval := 
yy.value_stack(yy.tos); 

when  492 =>
--#line  3422
 
yyval := 
yy.value_stack(yy.tos); 

when  493 =>
--#line  3423
 
yyval := 
yy.value_stack(yy.tos); 

when  494 =>
--#line  3424
 
yyval := 
yy.value_stack(yy.tos); 

when  495 =>
--#line  3425
 
yyval := 
yy.value_stack(yy.tos); 

when  496 =>
--#line  3426
 
yyval := (One_Tree, Null_Optional_Tree); 

when  497 =>
--#line  3432

	
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
    

when  498 =>
--#line  3447

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  499 =>
--#line  3454

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  500 =>
--#line  3457

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  501 =>
--#line  3465

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  502 =>
--#line  3473

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  503 =>
--#line  3476

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  504 =>
--#line  3481
 
yyval := 
yy.value_stack(yy.tos); 

when  509 =>
--#line  3492

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  510 =>
--#line  3497

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-4)),
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  511 =>
--#line  3508

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
	      End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
	      Check_Label => 
yy.value_stack(yy.tos).Check_Label,
              Label => 
yy.value_stack(yy.tos).Label));
	end;
    

when  514 =>
--#line  3529
 
yyval := 
yy.value_stack(yy.tos); 

when  515 =>
--#line  3530
 
yyval := 
yy.value_stack(yy.tos); 

when  516 =>
--#line  3531

        yyerror
          ("Use ""of"" rather than ""is"" for a case statement",
           At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  517 =>
--#line  3537

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  518 =>
--#line  3548

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  519 =>
--#line  3553

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-4)),
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  520 =>
--#line  3561

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  521 =>
--#line  3564

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  522 =>
--#line  3572

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-3).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  523 =>
--#line  3581

        yyerror("Missing ']'", At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  524 =>
--#line  3592

        if ParaSail_Lex.Debug_Indent then
           Text_IO.Put(" [indent on] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Bracketing_Token := REFERS_TO;
        ParaSail_Lex.Expecting_Indent := True;
        
yyval := 
yy.value_stack(yy.tos);
    

when  525 =>
--#line  3602
 
yyval := 
yy.value_stack(yy.tos); 

when  526 =>
--#line  3604
 
	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
          Param_Default => Null_Optional_Tree));
     

when  527 =>
--#line  3617

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-3).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  528 =>
--#line  3625

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  529 =>
--#line  3631
 
yyval := 
yy.value_stack(yy.tos-1); 

when  530 =>
--#line  3632
 
yyval := 
yy.value_stack(yy.tos-2); 

when  531 =>
--#line  3633
 
yyval := 
yy.value_stack(yy.tos); 

when  532 =>
--#line  3637
 
yyval := 
yy.value_stack(yy.tos); 

when  533 =>
--#line  3638

	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
          Param_Default => Null_Optional_Tree));
     

when  534 =>
--#line  3649

	
yyval := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos));
     

when  535 =>
--#line  3660

	
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
    

when  536 =>
--#line  3674

	
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
    

when  537 =>
--#line  3687
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_While => True, others => False); 

when  538 =>
--#line  3690
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_Until => True, others => False); 

when  541 =>
--#line  3698
 
yyval := 
yy.value_stack(yy.tos); ParaSail_Lex.Inside_For_Header := False; 

when  542 =>
--#line  3699
 
yyval := 
yy.value_stack(yy.tos); ParaSail_Lex.Inside_For_Header := False; 

when  543 =>
--#line  3700

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
        ParaSail_Lex.Inside_For_Header := False;
    

when  544 =>
--#line  3705

        yyerror ("Syntax error in loop header", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
        ParaSail_Lex.Inside_For_Header := False;
    

when  545 =>
--#line  3713

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  546 =>
--#line  3718

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-4)),
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  547 =>
--#line  3730

	
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
    

when  548 =>
--#line  3744

        ParaSail_Lex.Inside_For_Header := True;  
yyval := 
yy.value_stack(yy.tos);
    

when  549 =>
--#line  3750

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  550 =>
--#line  3753
 
yyval := 
yy.value_stack(yy.tos-1); 

when  551 =>
--#line  3757

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
    

when  552 =>
--#line  3768

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
    

when  553 =>
--#line  3780

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
    

when  554 =>
--#line  3797
 
yyval := 
yy.value_stack(yy.tos); 

when  555 =>
--#line  3798

	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  556 =>
--#line  3806
 
yyval := 
yy.value_stack(yy.tos); 

when  557 =>
--#line  3807
 
	yyerror("Use ""for ..."" or ""for each ..."" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  558 =>
--#line  3813
 
yyval := 
yy.value_stack(yy.tos); 

when  559 =>
--#line  3814
 
	yyerror("""for-each"" iterator uses ""of"" rather than ""in""",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  560 =>
--#line  3819
 
	yyerror("Use ""for each ..."" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  561 =>
--#line  3825
 
	yyerror("Missing ""each"" in container element ""for-each"" iterator",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  562 =>
--#line  3830
 
yyval := 
yy.value_stack(yy.tos); 

when  563 =>
--#line  3831
 
yyval := 
yy.value_stack(yy.tos); 

when  564 =>
--#line  3835

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  566 =>
--#line  3845

	yyerror("The ""reverse"" keyword goes immediately before ""loop""");
    

when  567 =>
--#line  3850

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  568 =>
--#line  3858

	
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
    

when  569 =>
--#line  3869

        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    

when  570 =>
--#line  3880

	
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
    

when  571 =>
--#line  3891

	
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
    

when  572 =>
--#line  3904

	
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
    

when  573 =>
--#line  3913

	
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
    

when  574 =>
--#line  3925

	
yyval := 
yy.value_stack(yy.tos);
    

when  575 =>
--#line  3928

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  576 =>
--#line  3934
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  577 =>
--#line  3937

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  578 =>
--#line  3944
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  579 =>
--#line  3947

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  580 =>
--#line  3954

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree, Complement => 
yy.value_stack(yy.tos-1).Is_Until));
	    -- Complement condition if used "until"
    

when  581 =>
--#line  3959

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  582 =>
--#line  3964
 
yyval := 
yy.value_stack(yy.tos); 

when  583 =>
--#line  3965
 
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  584 =>
--#line  3973

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("concurrent"));
    

when  585 =>
--#line  3977
 
yyval := 
yy.value_stack(yy.tos); 

when  586 =>
--#line  3981

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("forward"));
    

when  587 =>
--#line  3985

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("reverse"));
    

when  588 =>
--#line  3994

	
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
    

when  590 =>
--#line  4005

	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
    

when  591 =>
--#line  4011

        
yyval := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    

when  592 =>
--#line  4016

        
yyval := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos (
yy.value_stack(yy.tos-4)),
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  593 =>
--#line  4024

	
yyval := 
yy.value_stack(yy.tos);
    

when  594 =>
--#line  4027
 
	-- Error recovery
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  595 =>
--#line  4038
 
	yyerror("Use ""!="" rather than ""/="" in ParaSail");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op);
    

when  596 =>
--#line  4045
 
yyval := 
yy.value_stack(yy.tos); 

when  597 =>
--#line  4048

	
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
    

when  598 =>
--#line  4056
 
yyval := 
yy.value_stack(yy.tos); 

when  599 =>
--#line  4061

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Lambda_Operation,
	  Operation_Inputs => 
yy.value_stack(yy.tos-2).List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => Lists.Empty_List,
          Global_Update_List => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Def => True,
	  Statements => 
yy.value_stack(yy.tos).Tree)); 
        Set_Source_Pos (
yyval.Tree, Token_Src_Pos (
yy.value_stack(yy.tos-3)));
    

when  600 =>
--#line  4079

	
yyval := (One_List, Lists.Empty_List);
    

when  601 =>
--#line  4082

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  602 =>
--#line  4085

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  603 =>
--#line  4090

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  604 =>
--#line  4093

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  605 =>
--#line  4099

        
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => Null_Optional_Tree,
          Param_Default => Null_Optional_Tree));
    

when  606 =>
--#line  4111
 
yyval := 
yy.value_stack(yy.tos); 

when  607 =>
--#line  4112

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)),
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  608 =>
--#line  4121

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  609 =>
--#line  4128

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
     

when  610 =>
--#line  4138
 
yyval := 
yy.value_stack(yy.tos); 

when  611 =>
--#line  4139
 
yyval := 
yy.value_stack(yy.tos); 

when  612 =>
--#line  4140
 
yyval := 
yy.value_stack(yy.tos); 

when  613 =>
--#line  4144
 
yyval := 
yy.value_stack(yy.tos); 

when  614 =>
--#line  4145

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
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
      end;
    

when  615 =>
--#line  4177
 
yyval := 
yy.value_stack(yy.tos); 

when  616 =>
--#line  4178

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  617 =>
--#line  4185

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  618 =>
--#line  4192

	
yyval := (One_Tree, Unary.Make(
	  Operator => Unary.Is_Null_Op,
	  Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  619 =>
--#line  4197

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
    

when  620 =>
--#line  4208

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Is_Function_Of,
	  Prefix => 
yy.value_stack(yy.tos-5).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  621 =>
--#line  4217
 
yyval := 
yy.value_stack(yy.tos); 

when  622 =>
--#line  4218

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  623 =>
--#line  4228
 
yyval := 
yy.value_stack(yy.tos); 

when  624 =>
--#line  4229

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  625 =>
--#line  4239
 
yyval := 
yy.value_stack(yy.tos); 

when  626 =>
--#line  4240
 
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  627 =>
--#line  4264
 
yyval := 
yy.value_stack(yy.tos); 

when  628 =>
--#line  4265

        --  NOTE: We treat '+' here separately to avoid
        --        reduce/reduce conflicts
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Plus_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  629 =>
--#line  4274

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  630 =>
--#line  4284
 
yyval := 
yy.value_stack(yy.tos); 

when  631 =>
--#line  4285

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  632 =>
--#line  4295
 
yyval := 
yy.value_stack(yy.tos); 

when  633 =>
--#line  4296

	 -- right associative
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  634 =>
--#line  4304

	-- unary ops have higher precedence 
	-- than every operator except the power_operator.
	
yyval := (One_Tree, Unary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Unary_Op,
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  635 =>
--#line  4314
 
yyval := 
yy.value_stack(yy.tos); 

when  636 =>
--#line  4315
 
yyval := 
yy.value_stack(yy.tos); 

when  637 =>
--#line  4316
 
yyval := 
yy.value_stack(yy.tos-1); 

when  638 =>
--#line  4317
 
yyval := 
yy.value_stack(yy.tos-1); 

when  639 =>
--#line  4318
 
yyval := 
yy.value_stack(yy.tos-1); 

when  640 =>
--#line  4319

        
yyval := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  641 =>
--#line  4323
 
yyval := 
yy.value_stack(yy.tos); 

when  642 =>
--#line  4324

        --  This is used in a map_reduce expression to specify the initial val
        
yyval := (One_Tree, Unary.Make(Unary.Initial_Value_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  643 =>
--#line  4332
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  644 =>
--#line  4335
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  645 =>
--#line  4338
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  646 =>
--#line  4341
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  647 =>
--#line  4344
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make("null", 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  648 =>
--#line  4350
 
	if 
yy.value_stack(yy.tos).Is_Present then
	    
yyval := (One_Tree, Unary.Make(Unary.Updated_Value_Op,
	      Operand => 
yy.value_stack(yy.tos-1).Tree));
	else
	    
yyval := 
yy.value_stack(yy.tos-1); 
	end if;
    

when  649 =>
--#line  4358

	-- Use "::" to specify type of literal and
	-- to disambiguate operator specified as a string.
	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  650 =>
--#line  4365

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  651 =>
--#line  4371

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  652 =>
--#line  4377

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  653 =>
--#line  4383

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  654 =>
--#line  4391
 
yyval := 
yy.value_stack(yy.tos); 

when  655 =>
--#line  4392

        declare
           --  Substitute '@' for '#'
           Enum_Str : constant String := PSC.Strings.To_String (
yy.value_stack(yy.tos).Str);
           Prop_Str : constant String := '@' &
             Enum_Str (Enum_Str'First + 1 .. Enum_Str'Last);
        begin
           
yyval := (One_Tree, Property.Make(Operand => 
yy.value_stack(yy.tos-1).Tree,
             Property_Id =>
               PSC.Trees.Identifier.Make (Prop_Str, 
yy.value_stack(yy.tos).Source_Pos)));
        end;
    

when  656 =>
--#line  4407
 
yyval := (Optional, True); 

when  657 =>
--#line  4408

	yyerror("Use ""#"" instead of ""'"" to query property in ParaSail",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := (Optional, True);
    

when  658 =>
--#line  4413
 
yyval := (Optional, False); 

when  659 =>
--#line  4417

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  660 =>
--#line  4420

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  661 =>
--#line  4427
 
yyval := 
yy.value_stack(yy.tos); 

when  662 =>
--#line  4428

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  663 =>
--#line  4435
 
yyval := 
yy.value_stack(yy.tos); 

when  664 =>
--#line  4438
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  665 =>
--#line  4439
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  666 =>
--#line  4440
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Abs_Op); 

when  667 =>
--#line  4441
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Not_Op); 

when  668 =>
--#line  4442
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  669 =>
--#line  4443
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  670 =>
--#line  4447
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  671 =>
--#line  4448
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Plus_Op); 

when  672 =>
--#line  4449
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  673 =>
--#line  4453
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Times_Op); 

when  674 =>
--#line  4454
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Divide_Op); 

when  675 =>
--#line  4455
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Mod_Op); 

when  676 =>
--#line  4456
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Rem_Op); 

when  677 =>
--#line  4459
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Power_Op); 
    

when  678 =>
--#line  4463
 
yyval := 
yy.value_stack(yy.tos); 

when  679 =>
--#line  4464
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Divide_Assign_Op); 
    

when  680 =>
--#line  4470

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
     

when  681 =>
--#line  4473
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Plus_Assign_Op); 
    

when  682 =>
--#line  4476
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Minus_Assign_Op); 
    

when  683 =>
--#line  4479
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Times_Assign_Op); 
    

when  684 =>
--#line  4482
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Power_Assign_Op); 
    

when  685 =>
--#line  4485
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Combine_Assign_Op); 
    

when  686 =>
--#line  4488
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.And_Assign_Op); 
    

when  687 =>
--#line  4491
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Or_Assign_Op); 
    

when  688 =>
--#line  4494
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Xor_Assign_Op); 
    

when  689 =>
--#line  4497

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Left_Shift_Assign_Op);
    

when  690 =>
--#line  4500

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Right_Shift_Assign_Op);
    

when  691 =>
--#line  4505
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  692 =>
--#line  4508
 
yyval := 
yy.value_stack(yy.tos); 

when  693 =>
--#line  4511

	yyerror("Use "":="" rather than ""="" in ParaSail");
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  694 =>
--#line  4518
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Compare_Op); 

when  695 =>
--#line  4519
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op); 

when  696 =>
--#line  4520
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op); 

when  697 =>
--#line  4521
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Less_Op); 

when  698 =>
--#line  4522
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.LEQ_Op); 

when  699 =>
--#line  4523
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Greater_Op); 

when  700 =>
--#line  4524
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.GEQ_Op); 

when  701 =>
--#line  4525
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Left_Shift_Op ); 

when  702 =>
--#line  4526
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Right_Shift_Op); 

when  703 =>
--#line  4527
 
	yyerror("Use ""=="" rather than ""="" in ParaSail");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op);
    

when  704 =>
--#line  4534
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.And_Op); 

when  705 =>
--#line  4535
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Or_Op); 

when  706 =>
--#line  4536
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Xor_Op); 

when  707 =>
--#line  4538
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.And_Then_Op); 

when  708 =>
--#line  4540
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Or_Else_Op); 

when  709 =>
--#line  4541
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Implies_Op); 

when  710 =>
--#line  4545
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Interval_Op); 
    

when  711 =>
--#line  4548
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Interval_Op); 
    

when  712 =>
--#line  4551
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Open_Interval_Op); 
    

when  713 =>
--#line  4554
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Closed_Interval_Op); 
    

when  714 =>
--#line  4560
 
yyval := 
yy.value_stack(yy.tos); 

when  715 =>
--#line  4561
 
yyval := 
yy.value_stack(yy.tos); 

when  716 =>
--#line  4565

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  717 =>
--#line  4572

	-- Error recovery
	yyerror("Use ""!="" rather than ""/="" in ParaSail",
          At_Token => 
yy.value_stack(yy.tos-2));
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => Binary.Make(
	    Operator => Binary.NEQ_Op,
	    Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	    Right_Operand => 
yy.value_stack(yy.tos-1).Tree,
            Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos))),
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos));
     

when  718 =>
--#line  4586

	-- Type of aggregate specified
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => 
yy.value_stack(yy.tos-4).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  719 =>
--#line  4597

	
yyval := 
yy.value_stack(yy.tos);
    

when  720 =>
--#line  4600

	
yyval := (One_List, Lists.Empty_List);
    

when  721 =>
--#line  4606

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  722 =>
--#line  4609

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  723 =>
--#line  4616
 
yyval := 
yy.value_stack(yy.tos); 

when  724 =>
--#line  4617

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  725 =>
--#line  4622

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  726 =>
--#line  4632

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
    

when  727 =>
--#line  4685

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
    

when  728 =>
--#line  4734
 
yyval := 
yy.value_stack(yy.tos); 

when  729 =>
--#line  4735

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  730 =>
--#line  4738

	
yyval := (One_List, Lists.Empty_List);
    

when  731 =>
--#line  4744

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  732 =>
--#line  4747

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  733 =>
--#line  4754
 
yyval := 
yy.value_stack(yy.tos); 

when  734 =>
--#line  4755

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  735 =>
--#line  4760

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  736 =>
--#line  4766

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
        ParaSail_Lex.Inside_For_Header := False;
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
    

when  737 =>
--#line  4805

        
yyval := (One_Tree, Null_Optional_Tree);
    

when  738 =>
--#line  4809

        
yyval := 
yy.value_stack(yy.tos);
    

when  739 =>
--#line  4815
 
yyval := 
yy.value_stack(yy.tos); 

when  740 =>
--#line  4816
 
yyval := 
yy.value_stack(yy.tos); 

when  741 =>
--#line  4822

	
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
    

when  742 =>
--#line  4834

	
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
    

when  743 =>
--#line  4842

	
yyval := 
yy.value_stack(yy.tos);
    

when  744 =>
--#line  4845

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  745 =>
--#line  4852
 
yyval := 
yy.value_stack(yy.tos); 

when  746 =>
--#line  4853
 
yyval := 
yy.value_stack(yy.tos); 

when  747 =>
--#line  4854

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  748 =>
--#line  4858

        yyerror ("Syntax error in condition", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  749 =>
--#line  4866

	
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
    

when  750 =>
--#line  4877
 
yyval := 
yy.value_stack(yy.tos); 

when  751 =>
--#line  4878
 
yyval := 
yy.value_stack(yy.tos); 

when  752 =>
--#line  4879

        yyerror
          ("Use ""of"" rather than ""is"" for a case statement",
           At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  753 =>
--#line  4885

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos-1));
        
yyval := 
yy.value_stack(yy.tos);
    

when  754 =>
--#line  4889

        yyerror ("Syntax error in case selector", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos);
    

when  755 =>
--#line  4896

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  756 =>
--#line  4899

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  757 =>
--#line  4906

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-3).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  758 =>
--#line  4914

	-- NOTE: ".." alternative must come last
	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-3).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  759 =>
--#line  4923

        yyerror("Missing ']'", At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  760 =>
--#line  4936

	declare
	    Kind_Of_For_Loop: constant array(Boolean) of 
	      For_Loop_Construct.For_Loop_Kind_Enum := (
		False => For_Loop_Construct.Existential_Quantified_Expr,
		True => For_Loop_Construct.Univ_Quantified_Expr);
	begin
            ParaSail_Lex.Inside_For_Header := False;
	    
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
    

when  761 =>
--#line  4954

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-2).Tree;
     begin
        ParaSail_Lex.Inside_For_Header := False;
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
    

when  762 =>
--#line  4989

        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := 
yy.value_stack(yy.tos-3).Tree;
     begin
        ParaSail_Lex.Inside_For_Header := False;
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
    

when  763 =>
--#line  5026
 
yyval := 
yy.value_stack(yy.tos); 

when  764 =>
--#line  5027
 
yyval := 
yy.value_stack(yy.tos); 

when  765 =>
--#line  5028
 
yyval := 
yy.value_stack(yy.tos); 

when  766 =>
--#line  5032
 
yyval := (Optional, True); 

when  767 =>
--#line  5033
 
yyval := (Optional, False); 

when  768 =>
--#line  5037
 
yyval := 
yy.value_stack(yy.tos); 

when  769 =>
--#line  5038
 
yyval := 
yy.value_stack(yy.tos); 

when  770 =>
--#line  5039
 
yyval := 
yy.value_stack(yy.tos); 

when  771 =>
--#line  5044

	-- This does a map/reduce operation where the initial/next result
        -- is given in <...> and the overall expression represents the
        -- reduction to be performed on each element.

        ParaSail_Lex.Inside_For_Header := False;
        
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
          Kind => For_Loop_Construct.Map_Reduce_Expr,
          Iterators => 
yy.value_stack(yy.tos-4).List,
          Filter => 
yy.value_stack(yy.tos-3).List,
          Loop_Body => 
yy.value_stack(yy.tos).Tree,
          Direction => 
yy.value_stack(yy.tos-2).Str));
    

when  772 =>
--#line  5061

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  773 =>
--#line  5064

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  774 =>
--#line  5070
 
yyval := 
yy.value_stack(yy.tos); 

when  775 =>
--#line  5071
 
yyval := 
yy.value_stack(yy.tos); 

when  776 =>
--#line  5072
 
yyval := 
yy.value_stack(yy.tos); 

when  777 =>
--#line  5076

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
    

when  778 =>
--#line  5087

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
    

when  779 =>
--#line  5099

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

end ParaSail_Parser;
