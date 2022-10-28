

with Sparkel_tokens, Sparkel_lex_io, Sparkel_goto, Sparkel_shift_reduce;
with Sparkel_lex, text_io;

use  Sparkel_tokens, Sparkel_lex_io, Sparkel_goto, Sparkel_shift_reduce;
use  Sparkel_lex, text_io;

with PSC.Trees; use PSC.Trees;

with PSC.Messages;
with PSC.Symbols;
with PSC.Syntax;
with PSC.Strings;
pragma Elaborate (PSC.Strings);
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

package body Sparkel_Parser is

    use type Param_Decl.Param_Kind;
    use type PSC.Strings.U_String;

    Concurrent_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("concurrent");

    Forward_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("forward");

    Reverse_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("reverse");

    Func_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("func");

    Function_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("function");

    Proc_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("proc");

    Procedure_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("procedure");

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
     At_Token : Sparkel_Tokens.YYSType := (Sparkel_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Error(S, Src_Pos => Token_Src_Pos (At_Token));
    end yyerror;

   procedure Parser_Warning (S : String;
     At_Token : Sparkel_Tokens.YYSType := (Sparkel_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Warning(S, Src_Pos => Token_Src_Pos (At_Token));
    end Parser_Warning;

    function Name_For_Module(Defining_Name : Optional_Tree) 
      return Optional_Tree is
	-- Return Optional_Name for package, extracting it
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

    procedure Check_Func_Proc_Match(Op_Decl : Operation.Tree;
      End_Construct_Token : YYSType) is
    --  Check that "proc" ends with "end proc" and "func" ends with "end func"
       use Operation;
    begin
       if End_Construct_Token.Kind = Optional_End_Token
         and then End_Construct_Token.End_Construct_Str /=
           PSC.Strings.Null_U_String
       then
          case Operation.Func_Proc_Specifier'(Op_Decl.Operation_Kind) is
             when Func_Operation =>
                if End_Construct_Token.End_Construct_Str /= Func_Str then
                   yyerror("Should be ""end func""",
                     At_Token => End_Construct_Token);
                end if;
             when Function_Operation =>
                if End_Construct_Token.End_Construct_Str /= Function_Str then
                   yyerror("Should be ""end function""",
                     At_Token => End_Construct_Token);
                end if;
             when Proc_Operation =>
                if End_Construct_Token.End_Construct_Str /= Proc_Str then
                   yyerror("Should be ""end proc""",
                     At_Token => End_Construct_Token);
                end if;
             when Procedure_Operation =>
                if End_Construct_Token.End_Construct_Str /= Procedure_Str then
                   yyerror("Should be ""end procedure""",
                     At_Token => End_Construct_Token);
                end if;
          end case;
       end if;
    end Check_Func_Proc_Match;

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

    function Make_Statement_Sequence (L : Lists.List) return Optional_Tree is
    --  Build up a statement sequence by inserting the "Next_Stmt" operator
    --  between each element of the list.
        Result : Optional_Tree := Lists.Nth_Element (L, 1);
    begin
        for I in 2 .. Lists.Length (L) loop
            Result := Binary.Make(
             Operator => Binary.Next_Stmt_Op,
             Left_Operand => Result,
             Right_Operand => Lists.Nth_Element (L, I));

        end loop;
        return Result;       
    end Make_Statement_Sequence;

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

    function Make_Enum_Literals (Enum_Names : Lists.List) return Lists.List is
    --  Given a list of identifiers, construct a list of enum-literals
    --  by prepending "#" on to each.
       Result : Lists.List;
    begin
       for I in 1 .. Lists.Length (Enum_Names) loop
          declare
             Nth_Enum : constant PSC.Trees.Identifier.Tree :=
               PSC.Trees.Identifier.Tree (PSC.Trees.Tree_Ptr_Of
                 (Lists.Nth_Element (Enum_Names, I)).all);
          begin
             --  Add a '#' on front of the enum-lit name.
             Lists.Append (Result, 
	       PSC.Trees.Identifier.Make
                 ('#' & PSC.Strings.To_String (Nth_Enum.Str),
                  Nth_Enum.Source_Pos)); 
          end;
       end loop;        
       return Result;
    end Make_Enum_Literals;

    function Make_Array_Indexer (Index_Subtypes : Lists.List)
      return Optional_Tree is
    --  If there is more than one index subtype in the list, then
    --  create a record type out of the types.
    --  Otherwise just return the index subtype as is.
    begin
       if Lists.Length (Index_Subtypes) = 1 then
          return Lists.Nth_Element (Index_Subtypes, 1);
       else
          --  TBF: Construct a new record (tuple) type
          return Lists.Nth_Element (Index_Subtypes, 1);
       end if;
    end Make_Array_Indexer;

    use Qualifier; -- For Qualifier_Enum literals

pragma Style_Checks (Off);
procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Sparkel_Goto;
    package yy_shift_reduce_tables renames
      Sparkel_Shift_Reduce;
    package yy_tokens              renames
      Sparkel_Tokens;

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

when  1 =>
--#line  278

        null;  --  TBD: do something with pragmas
    

when  2 =>
--#line  281

        null;  --  TBD: do something with pragmas
    

when  3 =>
--#line  287

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  4 =>
--#line  294

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  5 =>
--#line  301

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  6 =>
--#line  308

	Semantics.Add_Top_Level_Tree(
yy.value_stack(yy.tos).Tree, Imports => 
yy.value_stack(yy.tos-1).List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree(
yy.value_stack(yy.tos).Tree);
        end if;
    

when  7 =>
--#line  315

	null;
    

when  11 =>
--#line  322

        
yyval := (One_List, Lists.Empty_List);
    

when  12 =>
--#line  325

        
yyval := 
yy.value_stack(yy.tos-1);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  13 =>
--#line  332
 
yyval := 
yy.value_stack(yy.tos-1); 

when  14 =>
--#line  336

        
yyval := (One_Tree, Annotation.Make
                 (Annotations =>
                    Lists.Make ((1 => Reference.Make
                      (Key => 
yy.value_stack(yy.tos-3).Tree,
                       Referent =>
                         Invocation.Make(
                           Kind => Invocation.Class_Aggregate,
                           Prefix => Null_Optional_Tree,
                           Operands => 
yy.value_stack(yy.tos-1).List,
                           Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos)))),
	          Label =>
                    PSC.Trees.Identifier.Make("pragma", 
yy.value_stack(yy.tos-4).Source_Pos))); 
    

when  15 =>
--#line  350

        
yyval := (One_Tree, Annotation.Make
                 (Annotations =>
                    Lists.Make ((1 => Reference.Make
                      (Key => 
yy.value_stack(yy.tos).Tree,
                       Referent => Null_Optional_Tree))),
--                          Invocation.Make(
--                            Kind => Invocation.Class_Aggregate,
--                            Prefix => Null_Optional_Tree,
--                            Operands => Lists.Empty_List,
--                            Source_Pos => $2.Source_Pos)))),
	          Label =>
                    PSC.Trees.Identifier.Make("pragma", 
yy.value_stack(yy.tos-1).Source_Pos))); 
    

when  16 =>
--#line  366
 
yyval := 
yy.value_stack(yy.tos); 

when  17 =>
--#line  367
 
yyval := 
yy.value_stack(yy.tos); 

when  18 =>
--#line  371

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  19 =>
--#line  374

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  20 =>
--#line  381

        
yyval := (One_Tree, PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  21 =>
--#line  384

        
yyval := 
yy.value_stack(yy.tos);
    

when  22 =>
--#line  387

	
yyval := (One_Tree, Qualified_Name.Make (  -- TBD: Selection?
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => PSC.Trees.Identifier.Make (
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos))); 
    

when  23 =>
--#line  395

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
    

when  24 =>
--#line  408

        
yyval := (One_List, Lists.Empty_List);  --  TBD: use pragmas ($1.List)
    

when  25 =>
--#line  411
 
        
yyval := 
yy.value_stack(yy.tos-2);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos-1).List);
        --  TBD: use pragmas ($3.List)
    

when  26 =>
--#line  416

        
yyval := 
yy.value_stack(yy.tos-2);
        --  TBD: Lists.Append ($$.List, $2.List);  --  need to distinguish
        --  TBD: use pragmas ($3.List)
    

when  27 =>
--#line  423

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  28 =>
--#line  428

       
yyval := 
yy.value_stack(yy.tos-1);  --  TBD: Need to distinguish from "with" clause
    

when  29 =>
--#line  433

       
yyval := 
yy.value_stack(yy.tos);
    

when  30 =>
--#line  439

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  31 =>
--#line  442

        
yyval := 
yy.value_stack(yy.tos-2);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  32 =>
--#line  451

	
yyval := 
yy.value_stack(yy.tos);
	declare
	    Mod_Tree : Module.Tree renames 
	      Module.Tree(Tree_Ptr_Of(
yyval.Tree).all);
	begin
	    Mod_Tree.Has_Formals := True;
	    Mod_Tree.Module_Formals := 
yy.value_stack(yy.tos-1).List;
	end;
    

when  33 =>
--#line  463

        
yyval := 
yy.value_stack(yy.tos-1); --  TBD
    

when  34 =>
--#line  469

        
yyval := (One_List, Lists.Empty_List);
    

when  35 =>
--#line  472

        
yyval := 
yy.value_stack(yy.tos);
    

when  36 =>
--#line  477

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  37 =>
--#line  480

        
yyval := 
yy.value_stack(yy.tos-2);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  38 =>
--#line  487

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  39 =>
--#line  490

        
yyval := 
yy.value_stack(yy.tos);
    

when  40 =>
--#line  496

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  41 =>
--#line  502

        yyerror ("""new"" required", At_Token => 
yy.value_stack(yy.tos));
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  42 =>
--#line  512

        
yyval := 
yy.value_stack(yy.tos);
    

when  43 =>
--#line  518
 
yyval := 
yy.value_stack(yy.tos); 

when  44 =>
--#line  525

      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos-5).List;
      begin
	--  if $1.Is_Private and then $4.Has_Module_Formals then
	--     yyerror("Private pkg_spec may not add package parameters");
	--  end if;
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-4).List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => 
yy.value_stack(yy.tos-4).List));
	end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-7).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-7).Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => 
yy.value_stack(yy.tos-9).Is_Present,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => 
yy.value_stack(yy.tos-3).List,
	  Module_Implements => 
yy.value_stack(yy.tos-2).List));

        if 
yy.value_stack(yy.tos).Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-7).Tree),
              Ending_Id => 
yy.value_stack(yy.tos).Label);
        end if;

      end;
    

when  45 =>
--#line  562

	--  if $1.Is_Private and then $4.Has_Module_Formals then
	--     yyerror("Private pkg_spec may not add package parameters");
	--  end if;
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-5).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-5).Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => 
yy.value_stack(yy.tos-7).Is_Present,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));

	Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-5).Tree),
	  Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
    

when  46 =>
--#line  590
 
yyval := 
yy.value_stack(yy.tos-1); 

when  47 =>
--#line  594

        --  Instantiate a package, not intended to be used as a type.
	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Add_On_Label => Lists.Empty_List,
          Is_Interface => True,
          Is_Abstract => False,
          Is_Private => 
yy.value_stack(yy.tos-4).Is_Present,
          Is_Concurrent => False,
          Is_Limited => True,
          Has_Formals => True,
          Treat_As_Type => False,
          Module_Formals => Lists.Empty_List,
          Extends_Interface => 
yy.value_stack(yy.tos).Tree,
          Implements_Interfaces => Lists.Empty_List,
          Class_Locals => Lists.Empty_List,
          Module_Exports => Lists.Empty_List,
          Module_New_Exports => Lists.Empty_List,
          Module_Implements => Lists.Empty_List));
    

when  49 =>
--#line  617

        yyerror ("Syntax error before ""is""", At_Token => 
yy.value_stack(yy.tos-1));
    

when  53 =>
--#line  630

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-2).Str, Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  55 =>
--#line  640
 
yyval := 
yy.value_stack(yy.tos); 

when  56 =>
--#line  641
 
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  57 =>
--#line  649
 
yyval := 
yy.value_stack(yy.tos); 

when  58 =>
--#line  650

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  59 =>
--#line  658
 
yyval := 
yy.value_stack(yy.tos); 

when  60 =>
--#line  659

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  61 =>
--#line  666

	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Concurrent,
	  others => False);
    

when  62 =>
--#line  676
 
yyval := 
yy.value_stack(yy.tos); 

when  63 =>
--#line  677
 
	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  64 =>
--#line  684

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	       Is_Concurrent => True, others => False);
    

when  65 =>
--#line  691

        
yyval := 
yy.value_stack(yy.tos);
    

when  66 =>
--#line  694
 
yyval := 
yy.value_stack(yy.tos-1); 

when  67 =>
--#line  695
 
yyval := 
yy.value_stack(yy.tos-1); 

when  68 =>
--#line  699
 
yyval := 
yy.value_stack(yy.tos-1); 

when  69 =>
--#line  700
 
yyval := (One_List, Lists.Empty_List); 

when  71 =>
--#line  703

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  72 =>
--#line  714
 
yyval := 
yy.value_stack(yy.tos); 

when  73 =>
--#line  715
 
	
yyval := (Optional, Is_Present => False);
    

when  74 =>
--#line  720
 
yyval := 
yy.value_stack(yy.tos); 

when  75 =>
--#line  723
 
yyval := 
yy.value_stack(yy.tos); 

when  76 =>
--#line  724
 
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  77 =>
--#line  733

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  78 =>
--#line  738
 
yyval := 
yy.value_stack(yy.tos); 

when  79 =>
--#line  739

	
yyval := (One_List, Lists.Empty_List);
    

when  80 =>
--#line  744
 
yyval := 
yy.value_stack(yy.tos); 

when  81 =>
--#line  745

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  82 =>
--#line  752

	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos).List);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  83 =>
--#line  761

	
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
    

when  84 =>
--#line  778
 
yyval := 
yy.value_stack(yy.tos); 

when  85 =>
--#line  779

	
yyval := (One_List, Lists.Empty_List);
    

when  86 =>
--#line  785
 
yyval := 
yy.value_stack(yy.tos); 

when  87 =>
--#line  786

	
yyval := (One_List, Lists.Empty_List);
    

when  88 =>
--#line  792

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  89 =>
--#line  798
 
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  90 =>
--#line  806

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  91 =>
--#line  848
 
yyval := 
yy.value_stack(yy.tos); 

when  92 =>
--#line  849
 
yyval := (One_Tree, Null_Optional_Tree); 

when  93 =>
--#line  855

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
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  94 =>
--#line  870

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
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  95 =>
--#line  884
 
	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-1).Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  96 =>
--#line  895
 
yyval := 
yy.value_stack(yy.tos); 

when  97 =>
--#line  898
 
yyval := 
yy.value_stack(yy.tos); 

when  98 =>
--#line  899
 
yyval := (One_Tree, Null_Optional_Tree); 

when  99 =>
--#line  903

	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos));
    

when  100 =>
--#line  909

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  101 =>
--#line  912

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  102 =>
--#line  919
 
yyval := 
yy.value_stack(yy.tos); 

when  103 =>
--#line  920
 
yyval := 
yy.value_stack(yy.tos); 

when  104 =>
--#line  923

	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  105 =>
--#line  930
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  106 =>
--#line  933

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  107 =>
--#line  938

	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  108 =>
--#line  946
 
yyval := 
yy.value_stack(yy.tos); 

when  109 =>
--#line  947
 
yyval := 
yy.value_stack(yy.tos); 

when  110 =>
--#line  950

        -- String_Literal can be used as a "name" when it is an operator
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  111 =>
--#line  957

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
yy.value_stack(yy.tos-1).Tree,
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  112 =>
--#line  963

	-- Include extension label in package name
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
	    Invocation.Make(
	      Kind => Invocation.Container_Indexing,
	      Prefix => 
yy.value_stack(yy.tos-4).Tree,
	      Operands => 
yy.value_stack(yy.tos-2).List),
	  Operands => 
yy.value_stack(yy.tos).List));
    

when  113 =>
--#line  977
 
yyval := 
yy.value_stack(yy.tos); 

when  114 =>
--#line  978
 
	
yyval := (One_List, Lists.Empty_List);
    

when  115 =>
--#line  984
 
yyval := 
yy.value_stack(yy.tos-1); 

when  116 =>
--#line  985

	
yyval := (One_List, Lists.Empty_List);
    

when  117 =>
--#line  991
 
yyval := 
yy.value_stack(yy.tos); 

when  118 =>
--#line  992

	
yyval := (One_List, Lists.Empty_List);
    

when  119 =>
--#line  998

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  120 =>
--#line  1001

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  121 =>
--#line  1008
 
yyval := 
yy.value_stack(yy.tos); 

when  122 =>
--#line  1009

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  123 =>
--#line  1018
 
	-- polymorphic type name not allowed here
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  124 =>
--#line  1023
 
	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  125 =>
--#line  1027

	-- This is a polymorphic type name, presumably.
	-- We use adding_expression instead of expanded_name
	-- to avoid reduce/reduce conflicts in the grammar.
	
yyval := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  126 =>
--#line  1035
 
yyval := 
yy.value_stack(yy.tos); 

when  127 =>
--#line  1037
 
yyval := 
yy.value_stack(yy.tos); 

when  128 =>
--#line  1038
 
yyval := 
yy.value_stack(yy.tos); 

when  129 =>
--#line  1042
 
yyval := 
yy.value_stack(yy.tos); 

when  130 =>
--#line  1043

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  131 =>
--#line  1050
 
yyval := 
yy.value_stack(yy.tos); 

when  132 =>
--#line  1051

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  133 =>
--#line  1055

        
yyval := 
yy.value_stack(yy.tos);
    

when  134 =>
--#line  1061
 
yyval := 
yy.value_stack(yy.tos); 

when  135 =>
--#line  1062
 
yyval := 
yy.value_stack(yy.tos); 

when  136 =>
--#line  1066
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  137 =>
--#line  1069
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  138 =>
--#line  1072
 
	
yyval := (One_Tree, Invocation.Add_Extends(
	  Instantiation => 
yy.value_stack(yy.tos-2).Tree, 
	  Extends => 
yy.value_stack(yy.tos).Tree));
    

when  139 =>
--#line  1080
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Qualifier.Is_Ref => True, others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  140 =>
--#line  1085

	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  141 =>
--#line  1093
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  142 =>
--#line  1100
 
	
yyval := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	    Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
	    others => False), 
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  143 =>
--#line  1108
 
	
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
    

when  144 =>
--#line  1120
 
	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => 
yy.value_stack(yy.tos).Is_Present,
	  others => False);
    

when  145 =>
--#line  1127

	
yyval := (Construct_Qualifier, 
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Concurrent => True, others => False);
    

when  146 =>
--#line  1135

	
yyval := (Optional, True);
    

when  147 =>
--#line  1138

	
yyval := (Optional, False);
    

when  148 =>
--#line  1144

	
yyval := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Proc_Type_Specifier,
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
    

when  149 =>
--#line  1158

	
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
    

when  150 =>
--#line  1174
 
yyval := (One_List, Lists.Empty_List); 

when  151 =>
--#line  1175

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
    

when  154 =>
--#line  1194

	
yyval := (One_List, Lists.Empty_List);
    

when  155 =>
--#line  1197

	
yyval := 
yy.value_stack(yy.tos-2);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-1).List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos).Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  156 =>
--#line  1207

	
yyval := 
yy.value_stack(yy.tos-3);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  157 =>
--#line  1217

	
yyval := 
yy.value_stack(yy.tos-3);
	if not Lists.Is_Empty(
yy.value_stack(yy.tos-2).List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      
yy.value_stack(yy.tos-1).Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	end if;
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  158 =>
--#line  1230

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  159 =>
--#line  1237
 
yyval := 
yy.value_stack(yy.tos-1); 

when  160 =>
--#line  1238

        
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  161 =>
--#line  1245
 
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 

when  162 =>
--#line  1246
 
yyval := 
yy.value_stack(yy.tos); 

when  163 =>
--#line  1247
 
yyval := 
yy.value_stack(yy.tos); 

when  164 =>
--#line  1248
 
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 

when  165 =>
--#line  1249

        --  TBF: We shouldn't just ignore use clauses!
        
yyval := (One_List, Lists.Empty_List);
    

when  166 =>
--#line  1253
 
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 

when  167 =>
--#line  1265
 
yyval := 
yy.value_stack(yy.tos); 

when  168 =>
--#line  1266

	
yyval := (One_List, Lists.Empty_List);
    

when  169 =>
--#line  1272
 
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
    

when  172 =>
--#line  1290
 
yyval := 
yy.value_stack(yy.tos); 

when  173 =>
--#line  1291

	
yyval := (One_List, Lists.Empty_List);
    

when  174 =>
--#line  1297
 
      declare
	Elem_List : Lists.List := 
yy.value_stack(yy.tos).List;
      begin
	
yyval := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    

when  175 =>
--#line  1309

      
yyval := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Import_Info => 
yy.value_stack(yy.tos).List));
    

when  177 =>
--#line  1317

        --  Pop the indent stack
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;
    

when  178 =>
--#line  1326

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  179 =>
--#line  1330

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
    

when  180 =>
--#line  1334

	-- Indicate that operation should be found in given type
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-3).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  181 =>
--#line  1339

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
    

when  182 =>
--#line  1345

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
    

when  183 =>
--#line  1360

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-2).Tree, Op_Equiv => 
yy.value_stack(yy.tos).Tree));
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
    

when  184 =>
--#line  1368

	
yyval := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Equiv => 
yy.value_stack(yy.tos-2).Tree));
	
yyval := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => 
yy.value_stack(yy.tos-4).Tree, Op_Location => 
yy.value_stack(yy.tos).Tree));
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
    

when  185 =>
--#line  1378

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
            if Sparkel_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
	end;
    

when  186 =>
--#line  1399
 
yyval := (One_List, Lists.Empty_List); 

when  187 =>
--#line  1400

	if Lists.Is_Empty(
yy.value_stack(yy.tos).List) then
	    -- We want to make sure that we return a non-empty list
	    
yyval := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    
yyval := 
yy.value_stack(yy.tos);
	end if;
    

when  188 =>
--#line  1412

	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  189 =>
--#line  1423

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
    

when  190 =>
--#line  1435

	--  yyerror("Missing ""exports"" keyword");
	
yyval := (Two_Lists, Lists.Empty_List, 
yy.value_stack(yy.tos).List);
    

when  194 =>
--#line  1443

	
yyval := (One_List, Lists.Empty_List);
    

when  195 =>
--#line  1446

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  196 =>
--#line  1454
 
yyval := 
yy.value_stack(yy.tos); 

when  197 =>
--#line  1455

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  198 =>
--#line  1458

        
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  199 =>
--#line  1461
 
yyval := 
yy.value_stack(yy.tos); 

when  200 =>
--#line  1464

	
yyval := (One_List, Lists.Empty_List);
    

when  201 =>
--#line  1468

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  202 =>
--#line  1472

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  203 =>
--#line  1476

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  204 =>
--#line  1480

	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  205 =>
--#line  1486

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  206 =>
--#line  1492
 
yyval := 
yy.value_stack(yy.tos); 

when  207 =>
--#line  1493
 
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  208 =>
--#line  1496

	
yyval := 
yy.value_stack(yy.tos);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
    

when  209 =>
--#line  1504

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  210 =>
--#line  1507
 
yyval := 
yy.value_stack(yy.tos); 

when  211 =>
--#line  1508

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  212 =>
--#line  1519

        -- TBD: allow an annotation after pkg_body_element_list
	
yyval := (One_Tree, PSC.Trees.Module.Make(
	  Name => Name_For_Module(
yy.value_stack(yy.tos-6).Tree),
	  Add_On_Label => Add_On_For_Module(
yy.value_stack(yy.tos-6).Tree),
	  Is_Interface => False,
	  Is_Abstract => False,
	  Is_Private => 
yy.value_stack(yy.tos-9).Is_Present,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
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
	    --       are in the "normal" pkg_spec part vs. in the
	    --       "implements" part of the pkg_spec.

        if 
yy.value_stack(yy.tos).Check_Label then
	    Check_Id_Match(Starting_Id => Name_For_Module(
yy.value_stack(yy.tos-6).Tree),
	      Ending_Id => 
yy.value_stack(yy.tos).Label);
        end if;
    

when  213 =>
--#line  1549

        
yyval := (Optional, Is_Present => False); 
    

when  214 =>
--#line  1552

        
yyval := (Optional, Is_Present => True);
    

when  215 =>
--#line  1557
 
yyval := 
yy.value_stack(yy.tos); 

when  216 =>
--#line  1558
 
yyval := 
yy.value_stack(yy.tos); 

when  217 =>
--#line  1559

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  218 =>
--#line  1566
 
yyval := 
yy.value_stack(yy.tos-1); 

when  219 =>
--#line  1567

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  220 =>
--#line  1573
 
yyval := 
yy.value_stack(yy.tos-1); 

when  221 =>
--#line  1574

	
yyval := 
yy.value_stack(yy.tos-3);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos-1).List);
    

when  222 =>
--#line  1581

        
yyval := 
yy.value_stack(yy.tos);
    

when  223 =>
--#line  1584

        --  A labeled annotation list becomes a separate nested annotation
        
yyval := (One_List, Lists.Make
                 ((1 => Annotation.Make
                   (Annotations => 
yy.value_stack(yy.tos).List, Label => 
yy.value_stack(yy.tos-1).Tree))));
    

when  224 =>
--#line  1594

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  225 =>
--#line  1597

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  226 =>
--#line  1601

	
yyval := 
yy.value_stack(yy.tos-2);
    

when  227 =>
--#line  1607
 
yyval := 
yy.value_stack(yy.tos); 

when  228 =>
--#line  1608
 
yyval := 
yy.value_stack(yy.tos); 

when  229 =>
--#line  1609

	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  230 =>
--#line  1623
 
yyval := 
yy.value_stack(yy.tos); 

when  231 =>
--#line  1626
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Abstract => True, others => False); 
    

when  232 =>
--#line  1631
 
	
yyval := (Construct_Qualifier,
               Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
               Is_Optional => True, others => False); 
    

when  233 =>
--#line  1636
 
yyval := (Construct_Qualifier,
             Source_Pos => PSC.Source_Positions.Null_Source_Position,
             others => False); 

when  234 =>
--#line  1642
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Abstract => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  235 =>
--#line  1647
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Optional => True, Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  236 =>
--#line  1652
 
	
yyval := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => 
yy.value_stack(yy.tos).Is_Present, others => False); 
    

when  237 =>
--#line  1660

	
yyval := (Optional, Is_Present => True);
    

when  238 =>
--#line  1663

	
yyval := (Optional, Is_Present => False);
    

when  239 =>
--#line  1669
 
	
yyval := 
yy.value_stack(yy.tos);
    

when  240 =>
--#line  1672

	yyerror("Operator designator must be in quotes");
	
yyval := 
yy.value_stack(yy.tos);
    

when  241 =>
--#line  1679

	
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
--#line  1693

	
yyval := (One_Tree, Operation.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Operation_Kind => Operation.Proc_Operation,
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
--#line  1708

	
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
--#line  1725

	
yyval := 
yy.value_stack(yy.tos);
    

when  245 =>
--#line  1731
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  246 =>
--#line  1734

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  247 =>
--#line  1737

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
--#line  1756

	yyerror("Sparkel requires at least ""()"" in operation definition");
	
yyval := (One_List, Lists.Empty_List);
    

when  249 =>
--#line  1763
 
yyval := 
yy.value_stack(yy.tos); 

when  250 =>
--#line  1764

        yyerror ("Expecting one ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := (One_Token,
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.String_Lookup(")")); 
    

when  251 =>
--#line  1774

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-4).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  252 =>
--#line  1783

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  253 =>
--#line  1792
 
	
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
--#line  1804
 
yyval := 
yy.value_stack(yy.tos); 

when  255 =>
--#line  1805

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  256 =>
--#line  1813
 
yyval := 
yy.value_stack(yy.tos); 

when  257 =>
--#line  1814
 
yyval := 
yy.value_stack(yy.tos); 

when  258 =>
--#line  1815

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  259 =>
--#line  1820

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  260 =>
--#line  1825

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Queued_Param);
    

when  261 =>
--#line  1830

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  262 =>
--#line  1835

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  263 =>
--#line  1840

	
yyval := (Param_Mode, 
	  Param_Kind => 
yy.value_stack(yy.tos).Param_Kind,
	  Param_Locking => Param_Decl.Locked_Param);
    

when  264 =>
--#line  1845

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  265 =>
--#line  1853
 
yyval := 
yy.value_stack(yy.tos); 

when  266 =>
--#line  1854

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  267 =>
--#line  1862

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  268 =>
--#line  1867

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  269 =>
--#line  1872

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  270 =>
--#line  1880

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  271 =>
--#line  1885

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  272 =>
--#line  1893
 
yyval := 
yy.value_stack(yy.tos); 

when  273 =>
--#line  1894

	
yyval := (One_List, Lists.Empty_List);
    

when  274 =>
--#line  1900
 
yyval := 
yy.value_stack(yy.tos); 

when  275 =>
--#line  1901

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  276 =>
--#line  1908

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
--#line  1918

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
--#line  1940

	
yyval := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length(
yy.value_stack(yy.tos-6).List) loop
            --  TBF: aliased, not null needs to be passed along
	    Lists.Append(
yyval.List, Param_Decl.Make(
	      Name => Lists.Nth_Element(
yy.value_stack(yy.tos-6).List, I),
	      Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => 
yy.value_stack(yy.tos-3).Is_Optional,
              In_Region => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I),
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-2).Tree, I),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  279 =>
--#line  1956

	
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
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-2).Tree, I),
	      Param_Default => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
	end loop;
    

when  280 =>
--#line  1969

	
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
--#line  1979

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-3).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-3).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  282 =>
--#line  1988

	
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
--#line  1998

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-2).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos-1).Tree,
	  Param_Default => 
yy.value_stack(yy.tos).Tree))));
    

when  284 =>
--#line  2007

	
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
--#line  2022

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  286 =>
--#line  2028

	
yyval := (Construct_Qualifier,
                Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
                Is_Ref => True,
                others => False);
    

when  287 =>
--#line  2037

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  288 =>
--#line  2043

	
yyval := (Construct_Qualifier,
                Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                Is_Ref => True,
                others => False);
    

when  289 =>
--#line  2052

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  290 =>
--#line  2058

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  291 =>
--#line  2064

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Out_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  292 =>
--#line  2070

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  293 =>
--#line  2075

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  294 =>
--#line  2081

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  295 =>
--#line  2087

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Out_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  296 =>
--#line  2093

	
yyval := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    

when  297 =>
--#line  2101
 
yyval := 
yy.value_stack(yy.tos); 

when  298 =>
--#line  2102

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  299 =>
--#line  2110
 
yyval := 
yy.value_stack(yy.tos); 

when  300 =>
--#line  2111
 
yyval := 
yy.value_stack(yy.tos); 

when  301 =>
--#line  2115
 
yyval := 
yy.value_stack(yy.tos); 

when  302 =>
--#line  2116

         -- NOTE: Operation can have "type" parameters 
         -- such as "Left_Type is Integer<>"
	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  303 =>
--#line  2124

        
yyval := 
yy.value_stack(yy.tos);
    

when  304 =>
--#line  2130

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  305 =>
--#line  2136

        
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Not_Null => True,
	  others => False);
    

when  306 =>
--#line  2145
 
yyval := 
yy.value_stack(yy.tos); 

when  307 =>
--#line  2146

	
yyval := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    

when  308 =>
--#line  2154

	
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
	  Is_Optional => True,
	  others => False);
    

when  309 =>
--#line  2160

        
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
	  Is_Not_Null => True,
	  others => False);
    

when  310 =>
--#line  2170
 
yyval := (Optional, Is_Present => True); 

when  311 =>
--#line  2171
 
yyval := (Optional, Is_Present => False); 

when  312 =>
--#line  2175

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  313 =>
--#line  2178

	Annotation.Add_Annotation(
yy.value_stack(yy.tos).Tree, 
yy.value_stack(yy.tos-1).List, Precedes => True);
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  314 =>
--#line  2182

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  315 =>
--#line  2185

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
--#line  2204

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
--#line  2227

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => 
yy.value_stack(yy.tos-4).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-4).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  318 =>
--#line  2236

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  319 =>
--#line  2245

	
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
--#line  2255

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  321 =>
--#line  2264

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  322 =>
--#line  2273

	
yyval := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree));
    

when  323 =>
--#line  2285

	
yyval := 
yy.value_stack(yy.tos);
    

when  324 =>
--#line  2288

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  325 =>
--#line  2295

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
--#line  2305

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
--#line  2321

	
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
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
    

when  328 =>
--#line  2335

	
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
	      Param_Type => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
    

when  329 =>
--#line  2347

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  330 =>
--#line  2356

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => 
yy.value_stack(yy.tos-2).Param_Kind,
	  Locking => 
yy.value_stack(yy.tos-2).Param_Locking,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  331 =>
--#line  2365

	
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
--#line  2374

	
yyval := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => 
yy.value_stack(yy.tos-1).Is_Optional,
	  Param_Type => 
yy.value_stack(yy.tos).Tree,
	  Param_Default => Null_Optional_Tree))));
    

when  333 =>
--#line  2387

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  334 =>
--#line  2390
 
yyval := 
yy.value_stack(yy.tos); 

when  335 =>
--#line  2395

	
yyval := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-4).Tree)),
	  Is_Var => True,
          Is_Global => True,  --  Only relevant if package element
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => 
yy.value_stack(yy.tos-1).Tree,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  336 =>
--#line  2407

        
yyval := 
yy.value_stack(yy.tos);
    

when  337 =>
--#line  2416

        
yyval := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length (
yy.value_stack(yy.tos-2).List) loop
           Lists.Append (
yyval.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-2).List, I))),
             Is_Var => True,
             Is_Global => True,  --  Only relevant if at package level
             Is_Const => False,
             Is_Ref => PSC.Trees.Qualifier.Qualifiers (
yy.value_stack(yy.tos-1).Tree)(Is_Ref),
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First
               (PSC.Trees.Qualifier.Unqualified_Tree (
yy.value_stack(yy.tos-1).Tree), I),
             Obj_Value => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
        end loop;
    

when  338 =>
--#line  2434

        
yyval := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length (
yy.value_stack(yy.tos-3).List) loop
           Lists.Append (
yyval.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-3).List, I))),
             Is_Var => False,
             Is_Const => False,
             Is_Ref => True,
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-2).Tree, I),
             Obj_Value => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
        end loop;
    

when  339 =>
--#line  2450

        
yyval := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length (
yy.value_stack(yy.tos-3).List) loop
           Lists.Append (
yyval.List,  Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-3).List, I))),
             Is_Var => False,
             Is_Const => True,
             Is_Ref => 
yy.value_stack(yy.tos-2).Is_Ref,
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First (
yy.value_stack(yy.tos-1).Tree, I),
             Obj_Value => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
        end loop;
    

when  340 =>
--#line  2465

        
yyval := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length (
yy.value_stack(yy.tos-3).List) loop
           Lists.Append (
yyval.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element(
yy.value_stack(yy.tos-3).List, I))),
             Is_Var => False,
             Is_Const => True,
             Is_Ref => 
yy.value_stack(yy.tos-2).Is_Ref,
             Is_Optional => False, -- TBD
             Obj_Type => Null_Optional_Tree,
             Obj_Value => Copy_If_Not_First (
yy.value_stack(yy.tos).Tree, I)));
        end loop;
    

when  341 =>
--#line  2480

        --  An exception turns into "type Id is new Exception_Type<>;"
        --  and when an exception is raised, we create an object of the
        --  type.  Exception_Type+ represents an Exception_Occurrence.
        --  The full name of the exception type as a Univ_Enumeral
        --  is the Exception_Id (what about "."s? -- Perhaps #"A.B.C")

        
yyval := (One_List, Lists.Empty_List);

        for I in 1 .. Lists.Length (
yy.value_stack(yy.tos-1).List) loop
           declare
              Nth_Name : constant Optional_Tree :=
                                    Lists.Nth_Element(
yy.value_stack(yy.tos-1).List, I);
           begin
              Lists.Append (
yyval.List, Type_Decl.Make(
                Name => Nth_Name,
                Is_New_Type => True,
                Type_Definition => Invocation.Make(
                  Kind => Invocation.Module_Instantiation,
                  Prefix => PSC.Trees.Identifier.Make
                    ("Exception_Type", Find_Source_Pos (Nth_Name)),
                  Operands => Lists.Empty_List)));
           end;
        end loop;
    

when  342 =>
--#line  2509

	
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
    

when  343 =>
--#line  2520

	
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
    

when  344 =>
--#line  2533

	
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
    

when  345 =>
--#line  2545

	
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
    

when  346 =>
--#line  2557

	
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
    

when  347 =>
--#line  2570
 
yyval := 
yy.value_stack(yy.tos); 

when  348 =>
--#line  2571

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  349 =>
--#line  2577
 
yyval := 
yy.value_stack(yy.tos); 

when  350 =>
--#line  2578

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  351 =>
--#line  2584
 
yyval := 
yy.value_stack(yy.tos); 

when  352 =>
--#line  2585

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  353 =>
--#line  2591

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  354 =>
--#line  2594

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos-2).Tree)));
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  355 =>
--#line  2601
 
yyval := 
yy.value_stack(yy.tos-1); 

when  356 =>
--#line  2605

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
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
yy.value_stack(yy.tos).Tree))));
    

when  357 =>
--#line  2616

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree))));
    

when  358 =>
--#line  2626

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree))));
    

when  359 =>
--#line  2636

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree))));
    

when  360 =>
--#line  2646

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree))));
    

when  361 =>
--#line  2657

	
yyval := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of(
yy.value_stack(yy.tos-2).Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree))));
    

when  362 =>
--#line  2670
 
yyval := 
yy.value_stack(yy.tos); 

when  363 =>
--#line  2674

        
yyval := 
yy.value_stack(yy.tos);
    

when  364 =>
--#line  2677

        declare
           Enum_Names : constant Lists.List := 
yy.value_stack(yy.tos-1).List;
           Enum_Lits : constant Lists.List :=
                           Make_Enum_Literals (Enum_Names);

           --  Enum-function result type
	   Enum_Func_Result : constant Optional_Tree :=
             Param_Decl.Make(
                Name => Null_Optional_Tree,
                Kind => Param_Decl.Default_Param,
                Locking => Param_Decl.Not_Locked,
                Is_Optional => False,
                Param_Type => PSC.Trees.Copy_Tree (
yy.value_stack(yy.tos-4).Tree),
                Param_Default => Null_Optional_Tree);
        begin
           --  Make the type decl
           
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
             Name => 
yy.value_stack(yy.tos-4).Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               Prefix => PSC.Trees.Identifier.Make ("Enum", 
yy.value_stack(yy.tos-3).Source_Pos),
               Operands => Lists.Make
                 ((1 => Invocation.Make(Invocation.Container_Aggregate,
                           Prefix => Null_Optional_Tree,
                           Operands => Enum_Lits,
                           Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos))))))));

           --  Now append declarations for enum-lit parameterless functions.
           for I in 1 .. Lists.Length (Enum_Lits) loop
              declare
                 Nth_Enum : constant Optional_Tree :=
                   Lists.Nth_Element (Enum_Names, I);
              begin
                 Lists.Append (
yyval.List, Operation.Make(
                   Name => Nth_Enum,
                   Operation_Kind => Operation.Function_Operation,
                   Operation_Inputs => Lists.Empty_List,
                   Operation_Outputs => Lists.Make
                     ((1 => Copy_If_Not_First (Enum_Func_Result, I))),
                   Preconditions => Null_Optional_Tree,
                   Postconditions => Null_Optional_Tree,
                   Is_Abstract => False,
                   Is_Optional => False,
                   Is_Queued => False,
                   Is_Def => True,
                   Is_Expression_Function => True,
                   Statements => Invocation.Make
                    (Kind => Invocation.Class_Aggregate,
                     Prefix => Null_Optional_Tree,
                     Operands => Lists.Make ((1 => PSC.Trees.Copy_Tree
                                    (Lists.Nth_Element (Enum_Lits, I)))),
                     Source_Pos => Find_Source_Pos (Nth_Enum))));
              end;
           end loop;
        end;
    

when  365 =>
--#line  2736
  -- TBD: discrims, abstract
        declare
           Rec_Mod : Module.Tree
             renames Module.Tree (Tree_Ptr_Of (
yy.value_stack(yy.tos).Tree).all);
        begin
           --  Fill in name and is-abstract
           Rec_Mod.Name := 
yy.value_stack(yy.tos-3).Tree;
           Rec_Mod.Is_Abstract := 
yy.value_stack(yy.tos-1).Is_Abstract;

           --  Now build a type-decl
           
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
             Name => 
yy.value_stack(yy.tos-3).Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               --  NOTE: Private/Record type decl has module as Prefix of inst
               Prefix => 
yy.value_stack(yy.tos).Tree,
               Operands => Lists.Empty_List)))));
        end;
    

when  366 =>
--#line  2756
  -- TBD: discrims
        
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => True,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree))));
    

when  367 =>
--#line  2763
  -- TBD: discrims
        
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            --  NOTE: Private/Record type decl has module as Prefix of inst
            Prefix => Module.Make(
              Name => 
yy.value_stack(yy.tos-4).Tree,
              Add_On_Label => Lists.Empty_List,
              Is_Interface => True,
              Is_Abstract => 
yy.value_stack(yy.tos-2).Is_Abstract,
              Is_Private => True,
              Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
              Is_Limited => 
yy.value_stack(yy.tos-1).Is_Limited,
              Has_Formals => False,
              Treat_As_Type => True,
              Module_Formals => Lists.Empty_List,
              Extends_Interface => Null_Optional_Tree,
              Implements_Interfaces => Lists.Empty_List,
              Class_Locals => Lists.Empty_List,
              Module_Exports => Lists.Empty_List,
              Module_New_Exports => Lists.Empty_List,
              Module_Implements => Lists.Empty_List),
            Operands => Lists.Empty_List)))));
    

when  368 =>
--#line  2790

        
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-7).Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Array_Type", Token_Src_Pos (
yy.value_stack(yy.tos-5))),
            Operands => Lists.Make
                          ((
yy.value_stack(yy.tos).Tree, Make_Array_Indexer (
yy.value_stack(yy.tos-3).List))))))));
    

when  369 =>
--#line  2802

        
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => 
yy.value_stack(yy.tos-1).Tree,
            Operands => Lists.Make
                          ((1 => 
yy.value_stack(yy.tos).Tree)),
            Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos)))));
    

when  370 =>
--#line  2816

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  371 =>
--#line  2819

        
yyval := 
yy.value_stack(yy.tos-2);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos).List);
    

when  372 =>
--#line  2826

        
yyval := (One_Tree, Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Integer", Find_Source_Pos (
yy.value_stack(yy.tos).Tree)),
            Operands => Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree))));
    

when  373 =>
--#line  2833

        
yyval := 
yy.value_stack(yy.tos-2);
	Annotation.Add_Annotation (
yyval.Tree,
          Lists.Make
             ((1 => Binary.Make(Binary.Closed_Interval_Op,
                      Left_Operand => Null_Optional_Tree,
                      Right_Operand => Null_Optional_Tree))));
    

when  374 =>
--#line  2841

        
yyval := 
yy.value_stack(yy.tos-2);
	Annotation.Add_Annotation(
yyval.Tree, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  375 =>
--#line  2847

        
yyval := (Construct_Qualifier,
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
          others => False);
    

when  376 =>
--#line  2852

        
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
          Is_Abstract => True,
          others => False);
    

when  377 =>
--#line  2861

        
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
          Is_Limited => True,
          others => False);
    

when  378 =>
--#line  2867

        
yyval := (Construct_Qualifier,
          Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
          Is_Concurrent => True,
          others => False);
    

when  379 =>
--#line  2873

        
yyval := (Construct_Qualifier,
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
          others => False);
    

when  380 =>
--#line  2881

        
yyval := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_All_Type", 
yy.value_stack(yy.tos).Source_Pos));
    

when  381 =>
--#line  2885

        
yyval := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_Const_Type", 
yy.value_stack(yy.tos).Source_Pos));
    

when  382 =>
--#line  2889

        
yyval := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_Type", PSC.Syntax.Cur_Source_Pos));
    

when  383 =>
--#line  2899

        -- TBD: discrims, abstract
	
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make
	  (Name => 
yy.value_stack(yy.tos-4).Tree,
	   Is_New_Type => True,
           Type_Definition => Invocation.Make
            (Kind => Invocation.Module_Instantiation,
             --  NOTE: type decl has module as Prefix of inst
             Prefix => PSC.Trees.Module.Make(
               Name => 
yy.value_stack(yy.tos-4).Tree,
               Add_On_Label => Lists.Empty_List,
               Is_Interface => True,
               Is_Abstract => 
yy.value_stack(yy.tos-2).Is_Abstract,
               Is_Private => False,
               Is_Concurrent => 
yy.value_stack(yy.tos-1).Is_Concurrent,
               Is_Limited => 
yy.value_stack(yy.tos-1).Is_Limited,
               Has_Formals => False,
               Treat_As_Type => True,
               Module_Formals => Lists.Empty_List,
               Extends_Interface => 
yy.value_stack(yy.tos).Tree,
               Implements_Interfaces => Lists.Empty_List,
               Class_Locals => Lists.Empty_List,
               Module_Exports => Lists.Empty_List,
               Module_New_Exports => Lists.Empty_List,
               Module_Implements => Lists.Empty_List),
             Operands => Lists.Empty_List)))));
    

when  384 =>
--#line  2927

        -- TBD: discrims, abstract
	
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make
	  (Name => 
yy.value_stack(yy.tos-6).Tree,
	   Is_New_Type => True,
           Type_Definition => Invocation.Make
            (Kind => Invocation.Module_Instantiation,
             --  NOTE: type decl has module as Prefix of inst
             Prefix => PSC.Trees.Module.Make(
               Name => 
yy.value_stack(yy.tos-6).Tree,
               Add_On_Label => Lists.Empty_List,
               Is_Interface => True,
               Is_Abstract => 
yy.value_stack(yy.tos-4).Is_Abstract,
               Is_Private => True,
               Is_Concurrent => 
yy.value_stack(yy.tos-3).Is_Concurrent,
               Is_Limited => 
yy.value_stack(yy.tos-3).Is_Limited,
               Has_Formals => False,
               Treat_As_Type => True,
               Module_Formals => Lists.Empty_List,
               Extends_Interface => 
yy.value_stack(yy.tos-2).Tree,
               Implements_Interfaces => Lists.Empty_List,
               Class_Locals => Lists.Empty_List,
               Module_Exports => Lists.Empty_List,
               Module_New_Exports => Lists.Empty_List,
               Module_Implements => Lists.Empty_List),
             Operands => Lists.Empty_List)))));
    

when  385 =>
--#line  2956
  -- TBD: discrims
        declare
           Rec_Mod : Module.Tree
             renames Module.Tree (Tree_Ptr_Of (
yy.value_stack(yy.tos).Tree).all);
        begin
           --  Fill in name and other info
           Rec_Mod.Name := 
yy.value_stack(yy.tos-6).Tree;
           Rec_Mod.Is_Abstract := 
yy.value_stack(yy.tos-4).Is_Abstract;
           Rec_Mod.Is_Limited := 
yy.value_stack(yy.tos-3).is_Limited;
           Rec_Mod.Is_Concurrent := 
yy.value_stack(yy.tos-3).is_Concurrent;
           Rec_Mod.Extends_Interface := 
yy.value_stack(yy.tos-2).Tree;

           --  Now build a type-decl
           
yyval := (One_List, Lists.Make ((1 => Type_Decl.Make (
             Name => 
yy.value_stack(yy.tos-6).Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               --  NOTE: type decl has module as Prefix of inst
               Prefix => 
yy.value_stack(yy.tos).Tree,
               Operands => Lists.Empty_List)))));
        end;
    

when  386 =>
--#line  2982

        --  We use a param-decl for the parent type for uniformity
        
yyval := (One_Tree, Tree => Param_Decl.Make
           (Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => 
yy.value_stack(yy.tos).Tree,
            Param_Default => Null_Optional_Tree));
    

when  387 =>
--#line  2992

        --  We use a param-decl for the parent type for uniformity
        yyerror("""new"" required unless defining a subtype",
           At_Token => 
yy.value_stack(yy.tos));
        
yyval := (One_Tree, Tree => Param_Decl.Make
           (Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => 
yy.value_stack(yy.tos).Tree,
            Param_Default => Null_Optional_Tree));

    

when  388 =>
--#line  3008

	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Null_Optional_Tree,
	  Add_On_Label => Lists.Empty_List,
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
	  Is_Concurrent => 
yy.value_stack(yy.tos-2).Is_Concurrent,
          Is_Limited => 
yy.value_stack(yy.tos-2).Is_Limited,
	  Has_Formals => False,
          Treat_As_Type => True,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    

when  389 =>
--#line  3030

	
yyval := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Null_Optional_Tree,
	  Add_On_Label => Lists.Empty_List,
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
	  Is_Concurrent => 
yy.value_stack(yy.tos-5).Is_Concurrent,
          Is_Limited => 
yy.value_stack(yy.tos-5).Is_Limited,
	  Has_Formals => False,
          Treat_As_Type => True,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => 
yy.value_stack(yy.tos-2).List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    

when  390 =>
--#line  3052

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  391 =>
--#line  3055

        
yyval := 
yy.value_stack(yy.tos-2);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos-1).Tree);
    

when  392 =>
--#line  3062

        
yyval := 
yy.value_stack(yy.tos);
    

when  393 =>
--#line  3066

	
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
    

when  394 =>
--#line  3081

        
yyval := (Optional_End_Token, 
                Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-1).Str,
                Check_Label => Not_Null (
yy.value_stack(yy.tos).Tree),
                Label => 
yy.value_stack(yy.tos).Tree, others => Null_Optional_Tree);
    

when  395 =>
--#line  3090
 
yyval := 
yy.value_stack(yy.tos); 

when  396 =>
--#line  3091

	yyerror("Should be ""end record [id]"" rather than ""end [id]""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  397 =>
--#line  3100

	
yyval := (One_Tree, Type_Decl.Make(
	  Name => 
yy.value_stack(yy.tos-2).Tree,
	  Is_New_Type => False,
	  Type_Definition => 
yy.value_stack(yy.tos).Tree));
    

when  398 =>
--#line  3111

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
                Check_Func_Proc_Match(Op_Decl, 
yy.value_stack(yy.tos));
            end if;
	end;
    

when  399 =>
--#line  3129

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
                Check_Func_Proc_Match(Op_Decl, 
yy.value_stack(yy.tos));
            end if;
	end;
    

when  400 =>
--#line  3148

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

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
	end;
    

when  401 =>
--#line  3165

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

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => 
yy.value_stack(yy.tos-1).Tree);
	end;
    

when  402 =>
--#line  3184

        
yyval := 
yy.value_stack(yy.tos);
    

when  403 =>
--#line  3187

        
yyval := 
yy.value_stack(yy.tos);
    

when  404 =>
--#line  3190

	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  405 =>
--#line  3198
 
yyval := 
yy.value_stack(yy.tos); 

when  406 =>
--#line  3199
 
yyval := (One_Tree, Null_Optional_Tree); 

when  407 =>
--#line  3203
 
yyval := 
yy.value_stack(yy.tos); 

when  408 =>
--#line  3206
 
yyval := 
yy.value_stack(yy.tos-1); 

when  409 =>
--#line  3210

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree,
	  Complement => 
yy.value_stack(yy.tos-2).Is_While));  
	    -- Complement cond if "while" present
	Set_Source_Pos(
yyval.Tree, Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
    

when  410 =>
--#line  3218
 
        --  Pop the indent stack
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;
    

when  414 =>
--#line  3231

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-2).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-1).Tree, others => Null_Optional_Tree);
    

when  415 =>
--#line  3241

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  416 =>
--#line  3246
 
yyval := 
yy.value_stack(yy.tos); 

when  417 =>
--#line  3247
 
yyval := 
yy.value_stack(yy.tos); 

when  418 =>
--#line  3251
 
yyval := 
yy.value_stack(yy.tos); 

when  419 =>
--#line  3252

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  420 =>
--#line  3261
 
yyval := 
yy.value_stack(yy.tos); 

when  421 =>
--#line  3262

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  422 =>
--#line  3271
 
yyval := 
yy.value_stack(yy.tos); 

when  423 =>
--#line  3274

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  424 =>
--#line  3282

	
yyval := 
yy.value_stack(yy.tos);
    

when  425 =>
--#line  3288
 
yyval := 
yy.value_stack(yy.tos); 

when  426 =>
--#line  3289
 
yyval := 
yy.value_stack(yy.tos); 

when  427 =>
--#line  3293
 
yyval := 
yy.value_stack(yy.tos); 

when  428 =>
--#line  3294
 
yyval := 
yy.value_stack(yy.tos); 

when  429 =>
--#line  3297

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => Binary.Make(
            Operator => Binary.Next_Stmt_Op,
            Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
            Right_Operand => 
yy.value_stack(yy.tos-1).Tree),
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  430 =>
--#line  3307

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  431 =>
--#line  3316

        --  Use the Handled_Stmt_Op for exception handlers
        --  TBF: Should "hoist" out the "then" statement from LHS
        --       or restructure the grammar a bit.
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Handled_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => Find_Source_Pos (
yy.value_stack(yy.tos).Tree)));
    

when  432 =>
--#line  3329

        
yyval := (One_Tree, Case_Construct.Make(
           Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos,
           Case_Selector => Null_Optional_Tree,
           Case_Alt_List => 
yy.value_stack(yy.tos).List));
    

when  433 =>
--#line  3335

        
yyval := (One_Tree, Case_Construct.Make(
           Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos,
           Case_Selector => Null_Optional_Tree,
           Case_Alt_List => 
yy.value_stack(yy.tos).List));
    

when  434 =>
--#line  3349

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  435 =>
--#line  3352

        
yyval := 
yy.value_stack(yy.tos-1);
        Lists.Append (
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  436 =>
--#line  3360

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  437 =>
--#line  3368

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  438 =>
--#line  3378
 
yyval := 
yy.value_stack(yy.tos); 

when  439 =>
--#line  3379

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
    

when  440 =>
--#line  3389

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
    

when  441 =>
--#line  3401

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  442 =>
--#line  3411

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [record with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  443 =>
--#line  3422

        
yyval := 
yy.value_stack(yy.tos);
    

when  444 =>
--#line  3425

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  447 =>
--#line  3431

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  448 =>
--#line  3441

	
yyval := 
yy.value_stack(yy.tos);  --  Ignored for now
    

when  449 =>
--#line  3447
 
yyval := 
yy.value_stack(yy.tos); 

when  450 =>
--#line  3448

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
    

when  451 =>
--#line  3458

	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  452 =>
--#line  3470
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  453 =>
--#line  3473

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  454 =>
--#line  3480

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  455 =>
--#line  3489

        
yyval := 
yy.value_stack(yy.tos);
    

when  456 =>
--#line  3492

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  457 =>
--#line  3497
 
yyval := 
yy.value_stack(yy.tos); 

when  458 =>
--#line  3498
 
yyval := 
yy.value_stack(yy.tos); 

when  459 =>
--#line  3502
 
	
yyval := 
yy.value_stack(yy.tos); 
    

when  460 =>
--#line  3505

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  461 =>
--#line  3512

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree,
          Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  462 =>
--#line  3522
 
yyval := 
yy.value_stack(yy.tos); 

when  463 =>
--#line  3523

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  464 =>
--#line  3532
 
yyval := 
yy.value_stack(yy.tos); 

when  465 =>
--#line  3533

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  466 =>
--#line  3542

        
yyval := 
yy.value_stack(yy.tos);
    

when  467 =>
--#line  3545

        
yyval := 
yy.value_stack(yy.tos);
    

when  468 =>
--#line  3551

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  469 =>
--#line  3557

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos-2).List, Precedes => True);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  470 =>
--#line  3562

	
yyval := 
yy.value_stack(yy.tos-1);
	Annotation.Add_Annotation(
yyval.Tree, 
yy.value_stack(yy.tos).List);
    

when  471 =>
--#line  3566

	-- An annotation can appear by itself
	
yyval := (One_Tree, Annotation.Make(Annotations => 
yy.value_stack(yy.tos).List));
    

when  472 =>
--#line  3573

        -- NOTE: these already allow trailing annotations
	Annotation.Add_Annotation(Lists.Nth_Element(
yy.value_stack(yy.tos).List, 1),
          
yy.value_stack(yy.tos-1).List, Precedes => True);
        --  Turn list into a statement sequence, separated by ";"s
        
yyval := (One_Tree, Make_Statement_Sequence (
yy.value_stack(yy.tos).List));
    

when  473 =>
--#line  3580

	Annotation.Add_Annotation(Lists.Nth_Element(
yy.value_stack(yy.tos).List, 1),
          
yy.value_stack(yy.tos-1).List, Precedes => True);
        
yyval := 
yy.value_stack(yy.tos);
    

when  474 =>
--#line  3585

        -- NOTE: these already allow trailing annotations
        --  Turn list into a statement sequence, separated by ";"s
        
yyval := (One_Tree, Make_Statement_Sequence (
yy.value_stack(yy.tos).List));
    

when  475 =>
--#line  3590

	
yyval := 
yy.value_stack(yy.tos);
    

when  476 =>
--#line  3597
 
yyval := 
yy.value_stack(yy.tos-1); 

when  477 =>
--#line  3600

        --  Turn list into a statement sequence, separated by ";"s
        
yyval := (One_Tree, Make_Statement_Sequence (
yy.value_stack(yy.tos).List));
    

when  478 =>
--#line  3604
 
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
    

when  479 =>
--#line  3613
 
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
    

when  480 =>
--#line  3626

	
yyval := 
yy.value_stack(yy.tos);
    

when  481 =>
--#line  3629

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  482 =>
--#line  3635
 
	-- A "null" statement (i.e. a no-op)
	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Null_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => 
yy.value_stack(yy.tos).Source_Pos));
    

when  483 =>
--#line  3644

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  484 =>
--#line  3650
 
yyval := 
yy.value_stack(yy.tos); 

when  485 =>
--#line  3651

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Continue_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => 
yy.value_stack(yy.tos-1).Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos));
    

when  486 =>
--#line  3659

	
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
    

when  487 =>
--#line  3668

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos).Tree,
	  Then_Part => Control_Stmt.Make(
             Kind => Control_Stmt.Exit_Stmt,
             Applies_To => 
yy.value_stack(yy.tos-4).Exitable_Construct,
             Id => 
yy.value_stack(yy.tos-3).Tree,
             Values => 
yy.value_stack(yy.tos-2).Tree,
             Source_Pos => 
yy.value_stack(yy.tos-5).Source_Pos),
	  Else_Part => Null_Optional_Tree,
	  End_With_Values => Null_Optional_Tree,
	  Check_Label => False,
          Label => Null_Optional_Tree));
    

when  488 =>
--#line  3683

        declare
           Operands : Lists.List;
        begin
           if Not_Null (
yy.value_stack(yy.tos).Tree) then
              Lists.Append (Operands, 
yy.value_stack(yy.tos).Tree);
           end if;

           --  Exception_Type'Raise_Occurrence (Excep'Create([message]))
           
yyval := (One_Tree, Invocation.Make(
             Kind => Invocation.Operation_Call,
             Prefix => Qualified_Name.Make(
               Prefix => PSC.Trees.Identifier.Make
                 ("Exception_Type", 
yy.value_stack(yy.tos-2).Source_Pos),
               Id => PSC.Trees.Identifier.Make
                 ("Raise_Occurrence", 
yy.value_stack(yy.tos-2).Source_Pos)),
             Operands => Lists.Make ((1 =>
                Invocation.Make(
                   Kind => Invocation.Operation_Call,
                   Prefix => Qualified_Name.Make(
                     Prefix => 
yy.value_stack(yy.tos-1).Tree,
                      Id => PSC.Trees.Identifier.Make
                        ("Create", 
yy.value_stack(yy.tos-2).Source_Pos)),
                   Operands => Operands)))));
        end;
    

when  489 =>
--#line  3709

        yyerror ("Extra ')'", At_Token => 
yy.value_stack(yy.tos));
        
yyval := 
yy.value_stack(yy.tos-1);
    

when  490 =>
--#line  3716

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  491 =>
--#line  3727

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  492 =>
--#line  3735

	
yyval := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => 
yy.value_stack(yy.tos).Tree,
	  Source_Pos => 
yy.value_stack(yy.tos-1).Source_Pos));
    

when  493 =>
--#line  3746
 
yyval := 
yy.value_stack(yy.tos); 

when  494 =>
--#line  3747

	yyerror("""loop"" required after ""continue""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  495 =>
--#line  3756

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => 
yy.value_stack(yy.tos-1).Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  496 =>
--#line  3762

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Divide_Assign_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  497 =>
--#line  3768

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Combine_Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  498 =>
--#line  3774

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  499 =>
--#line  3780

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Swap_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  500 =>
--#line  3786

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
    

when  501 =>
--#line  3799

	
yyval := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))));
    

when  502 =>
--#line  3803

	
yyval := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree)),
                  Label => 
yy.value_stack(yy.tos).Tree));
    

when  503 =>
--#line  3808
 
yyval := 
yy.value_stack(yy.tos); 

when  504 =>
--#line  3812
 
yyval := 
yy.value_stack(yy.tos); 

when  505 =>
--#line  3813

	
yyval := (One_List, Lists.Empty_List);
    

when  506 =>
--#line  3819
 
yyval := 
yy.value_stack(yy.tos); 

when  507 =>
--#line  3820

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  508 =>
--#line  3825

	-- NOTE: This used to be '(' operation_actual_list ')'
	--       but that prevented continuing with a single expression.
	
yyval := 
yy.value_stack(yy.tos);
    

when  509 =>
--#line  3832
 
yyval := 
yy.value_stack(yy.tos); 

when  510 =>
--#line  3833

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  511 =>
--#line  3839
 
yyval := 
yy.value_stack(yy.tos); 

when  512 =>
--#line  3840
 
	yyerror(
	  """loop,"" ""if,"" ""case,"" or ""block"" must follow ""exit""");
	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  513 =>
--#line  3848

	
yyval := (Construct_Kind, Control_Stmt.Loop_Stmt);
    

when  514 =>
--#line  3851

	
yyval := (Construct_Kind, Control_Stmt.If_Stmt);
    

when  515 =>
--#line  3854

	
yyval := (Construct_Kind, Control_Stmt.Case_Stmt);
    

when  516 =>
--#line  3857

	
yyval := (Construct_Kind, Control_Stmt.Block_Stmt);
    

when  517 =>
--#line  3860

	
yyval := (Construct_Kind, Control_Stmt.Block_Stmt); --  TBD: Parallel_Stmt
    

when  518 =>
--#line  3865

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  519 =>
--#line  3875

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [parallel with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  520 =>
--#line  3887
 
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree))); 

when  521 =>
--#line  3888
 
yyval := 
yy.value_stack(yy.tos); 

when  522 =>
--#line  3889
 
yyval := 
yy.value_stack(yy.tos); 

when  523 =>
--#line  3890
 
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree))); 

when  524 =>
--#line  3895
 
yyval := 
yy.value_stack(yy.tos); 

when  525 =>
--#line  3896

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  526 =>
--#line  3899

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos-1).Tree)));
    

when  527 =>
--#line  3902

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  528 =>
--#line  3905

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  529 =>
--#line  3910
 
yyval := 
yy.value_stack(yy.tos); 

when  530 =>
--#line  3911
 
yyval := 
yy.value_stack(yy.tos-1); 

when  531 =>
--#line  3912
 
yyval := 
yy.value_stack(yy.tos-1); 

when  532 =>
--#line  3915
 
yyval := 
yy.value_stack(yy.tos-1); 

when  533 =>
--#line  3918
 
yyval := 
yy.value_stack(yy.tos); 

when  534 =>
--#line  3919
 
yyval := 
yy.value_stack(yy.tos); 

when  535 =>
--#line  3920
 
yyval := 
yy.value_stack(yy.tos); 

when  536 =>
--#line  3921
 
yyval := 
yy.value_stack(yy.tos); 

when  537 =>
--#line  3922
 
yyval := 
yy.value_stack(yy.tos); 

when  538 =>
--#line  3923
 
yyval := 
yy.value_stack(yy.tos); 

when  539 =>
--#line  3924
 
yyval := 
yy.value_stack(yy.tos); 

when  540 =>
--#line  3925
 
yyval := 
yy.value_stack(yy.tos); 

when  541 =>
--#line  3926
 
yyval := (One_Tree, Null_Optional_Tree); 

when  542 =>
--#line  3932

	
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
    

when  543 =>
--#line  3947

	
yyval := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
	  Cond => 
yy.value_stack(yy.tos-3).Tree,
	  Then_Part => 
yy.value_stack(yy.tos-1).Tree,
	  Else_Part => 
yy.value_stack(yy.tos).Tree));
    

when  544 =>
--#line  3955

	
yyval := 
yy.value_stack(yy.tos-1);
    

when  545 =>
--#line  3958

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  546 =>
--#line  3963
 
yyval := 
yy.value_stack(yy.tos); 

when  548 =>
--#line  3969

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-3).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  549 =>
--#line  3982

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
    

when  550 =>
--#line  4001

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-3).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  551 =>
--#line  4011

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  552 =>
--#line  4014

	
yyval := 
yy.value_stack(yy.tos-1);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  553 =>
--#line  4022

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  554 =>
--#line  4031

        if Sparkel_Lex.Debug_Indent then
           Text_IO.Put(" [indent on] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Bracketing_Token := REFERS_TO;
        Sparkel_Lex.Expecting_Indent := True;
        
yyval := 
yy.value_stack(yy.tos);
    

when  555 =>
--#line  4041
 
yyval := 
yy.value_stack(yy.tos); 

when  556 =>
--#line  4043
 
	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
          Param_Default => Null_Optional_Tree));
     

when  557 =>
--#line  4056

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  558 =>
--#line  4063

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  559 =>
--#line  4069
 
	
yyval := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree));
    

when  560 =>
--#line  4077
 
yyval := 
yy.value_stack(yy.tos-1); 

when  561 =>
--#line  4078
 
yyval := 
yy.value_stack(yy.tos-2); 

when  562 =>
--#line  4079
 
yyval := 
yy.value_stack(yy.tos); 

when  563 =>
--#line  4083
 
yyval := 
yy.value_stack(yy.tos); 

when  564 =>
--#line  4084

	
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos-2).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => 
yy.value_stack(yy.tos).Tree,
          Param_Default => Null_Optional_Tree));
     

when  565 =>
--#line  4095

	
yyval := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree));
     

when  566 =>
--#line  4105

	
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
    

when  567 =>
--#line  4119

	
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
    

when  568 =>
--#line  4132
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_While => True, others => False); 

when  569 =>
--#line  4135
 
yyval := (Construct_Qualifier,
                      Source_Pos => 
yy.value_stack(yy.tos).Source_Pos,
                      Is_Until => True, others => False); 

when  572 =>
--#line  4143

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-3).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  573 =>
--#line  4157

	
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
    

when  574 =>
--#line  4172

	
yyval := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => 
yy.value_stack(yy.tos-6).Source_Pos,
	  Kind => For_Loop_Construct.For_Loop_Statement,
	  Iterators => 
yy.value_stack(yy.tos-5).List,
	  Filter => 
yy.value_stack(yy.tos-4).List,
	  Loop_Body => 
yy.value_stack(yy.tos-2).Tree,
	  Direction => Concurrent_Str,
          Chunk_Spec => 
yy.value_stack(yy.tos-7).Tree,
	  End_With_Values => 
yy.value_stack(yy.tos).End_With_Values,
          Check_Label => 
yy.value_stack(yy.tos).Check_Label,
          Label => 
yy.value_stack(yy.tos).Label));
    

when  575 =>
--#line  4188

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  576 =>
--#line  4191
 
yyval := 
yy.value_stack(yy.tos-1); 

when  577 =>
--#line  4195

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
    

when  578 =>
--#line  4206

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
    

when  579 =>
--#line  4218

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
    

when  580 =>
--#line  4235
 
yyval := 
yy.value_stack(yy.tos); 

when  581 =>
--#line  4236

	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  582 =>
--#line  4244
 
yyval := 
yy.value_stack(yy.tos); 

when  583 =>
--#line  4245
 
	yyerror("Use ""for ..."" or ""for each ..."" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  584 =>
--#line  4251
 
yyval := 
yy.value_stack(yy.tos); 

when  585 =>
--#line  4252
 
	yyerror("""for-each"" iterator uses ""of"" rather than ""in""",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  586 =>
--#line  4257
 
	yyerror("Use ""for each ..."" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => 
yy.value_stack(yy.tos-1));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  587 =>
--#line  4263
 
	yyerror("Missing ""each"" in container element ""for-each"" iterator",
          At_Token => 
yy.value_stack(yy.tos));
	
yyval := 
yy.value_stack(yy.tos); 
    

when  588 =>
--#line  4268
 
yyval := 
yy.value_stack(yy.tos); 

when  589 =>
--#line  4269
 
yyval := 
yy.value_stack(yy.tos); 

when  590 =>
--#line  4273

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => 
yy.value_stack(yy.tos-4).Tree,
	  Is_Ref => False,
	  Obj_Type => 
yy.value_stack(yy.tos-3).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  592 =>
--#line  4283

	yyerror("The ""reverse"" keyword goes immediately before ""loop""");
        --  TBD: But not in Ada 202X!
    

when  593 =>
--#line  4289

	
yyval := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => 
yy.value_stack(yy.tos-3).Tree,
	  Is_Ref => True,
	  Obj_Type => 
yy.value_stack(yy.tos-2).Tree,
	  Obj_Value => 
yy.value_stack(yy.tos).Tree));
    

when  594 =>
--#line  4297

	
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
    

when  595 =>
--#line  4308

        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        
yyval := 
yy.value_stack(yy.tos);
    

when  596 =>
--#line  4320

	
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
    

when  597 =>
--#line  4331

	
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
    

when  598 =>
--#line  4344

	
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
    

when  599 =>
--#line  4353

	
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
    

when  600 =>
--#line  4365

	
yyval := 
yy.value_stack(yy.tos);
    

when  601 =>
--#line  4368

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  602 =>
--#line  4374
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  603 =>
--#line  4377

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  604 =>
--#line  4384
 
	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree))); 
    

when  605 =>
--#line  4387

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  606 =>
--#line  4394

	
yyval := (One_Tree, Conditionally_Complement(
	  
yy.value_stack(yy.tos).Tree, Complement => 
yy.value_stack(yy.tos-1).Is_Until));
	    -- Complement condition if used "until"
    

when  607 =>
--#line  4399

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  608 =>
--#line  4404
 
yyval := 
yy.value_stack(yy.tos); 

when  609 =>
--#line  4405
 
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  610 =>
--#line  4413
 
yyval := 
yy.value_stack(yy.tos); 

when  611 =>
--#line  4414
 
yyval := 
yy.value_stack(yy.tos); 

when  612 =>
--#line  4418

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos, Concurrent_Str);
    

when  613 =>
--#line  4424

	
yyval := (One_Tree, Null_Optional_Tree); 
    

when  614 =>
--#line  4427

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  615 =>
--#line  4430

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  616 =>
--#line  4436

        
yyval := 
yy.value_stack(yy.tos);
    

when  617 =>
--#line  4440

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  618 =>
--#line  4446

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos, Forward_Str);
    

when  619 =>
--#line  4449

	
yyval := (One_Token, PSC.Syntax.Cur_Source_Pos, Reverse_Str);
    

when  620 =>
--#line  4457

	
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
    

when  621 =>
--#line  4467
 
yyval := 
yy.value_stack(yy.tos); 

when  622 =>
--#line  4468

	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  623 =>
--#line  4477

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-3).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  624 =>
--#line  4487
 
yyval := 
yy.value_stack(yy.tos); 

when  625 =>
--#line  4488
 
yyval := 
yy.value_stack(yy.tos); 

when  626 =>
--#line  4494

        --  Replace block body with a "Then" statement
        --  where LHS is the list of declarations, and RHS
        --  is the handled-seq-of-statements.
        declare
           Begin_Stmt : Block_Stmt.Tree renames
             Block_Stmt.Tree (Tree_Ptr_Of (
yy.value_stack(yy.tos).Tree).all);
        begin
           
yyval := 
yy.value_stack(yy.tos);
           Begin_Stmt.Block_Body := Binary.Make(
             Operator => Binary.Then_Stmt_Op,
             Left_Operand => 
yy.value_stack(yy.tos-1).Tree,
             Right_Operand => Begin_Stmt.Block_Body,
             Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos);
        end;
    

when  627 =>
--#line  4515

	
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
    

when  628 =>
--#line  4526

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-3).Source_Pos,
                End_Construct_Str => PSC.Strings.Null_U_String,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  629 =>
--#line  4536

        --  Convert list to a sequence of Next_Stmt_Op(A, B)
        if Lists.Length (
yy.value_stack(yy.tos).List) = 0 then
           
yyval := (One_Tree, Null_Optional_Tree);
        else
           
yyval := (One_Tree, Lists.Nth_Element (
yy.value_stack(yy.tos).List, 1));
           for I in 2 .. Lists.Length (
yy.value_stack(yy.tos).List) loop
              
yyval := (One_Tree, Binary.Make(
                Operator => Binary.Next_Stmt_Op,
                Left_Operand => 
yyval.Tree,
                Right_Operand => Lists.Nth_Element (
yy.value_stack(yy.tos).List, I)));
           end loop;
        end if;
    

when  630 =>
--#line  4555

	
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
    

when  631 =>
--#line  4565
 
yyval := 
yy.value_stack(yy.tos); 

when  632 =>
--#line  4566

	yyerror("Should be ""end parallel <id>"" rather than ""end <id>""");
	
yyval := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    

when  633 =>
--#line  4575

        
yyval := (Optional_End_Token,
                Source_Pos => 
yy.value_stack(yy.tos-4).Source_Pos,
                End_Construct_Str => 
yy.value_stack(yy.tos-3).Str,
                Check_Label => True,
                Label => 
yy.value_stack(yy.tos-2).Tree, End_With_Values => 
yy.value_stack(yy.tos-1).Tree);
    

when  634 =>
--#line  4585

	
yyval := 
yy.value_stack(yy.tos);
    

when  635 =>
--#line  4598
 
	-- Treat "/=" equiv to "!=" in an expression
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op);
    

when  636 =>
--#line  4605
 
yyval := 
yy.value_stack(yy.tos); 

when  637 =>
--#line  4608

	
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
    

when  638 =>
--#line  4616
 
yyval := 
yy.value_stack(yy.tos); 

when  639 =>
--#line  4621

	
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
    

when  640 =>
--#line  4636

	
yyval := (One_List, Lists.Empty_List);
    

when  641 =>
--#line  4639

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  642 =>
--#line  4642

        
yyval := 
yy.value_stack(yy.tos-1);
    

when  643 =>
--#line  4647

        
yyval := (One_List, Lists.Make ((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  644 =>
--#line  4650

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  645 =>
--#line  4656

        
yyval := (One_Tree, Param_Decl.Make(
          Name => 
yy.value_stack(yy.tos).Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => Null_Optional_Tree,
          Param_Default => Null_Optional_Tree));
    

when  646 =>
--#line  4668
 
yyval := 
yy.value_stack(yy.tos); 

when  647 =>
--#line  4669

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  648 =>
--#line  4677

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  649 =>
--#line  4683

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
     

when  650 =>
--#line  4692
 
yyval := 
yy.value_stack(yy.tos); 

when  651 =>
--#line  4693
 
yyval := 
yy.value_stack(yy.tos); 

when  652 =>
--#line  4697
 
yyval := 
yy.value_stack(yy.tos); 

when  653 =>
--#line  4698

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
    

when  654 =>
--#line  4729
 
yyval := 
yy.value_stack(yy.tos); 

when  655 =>
--#line  4730

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  656 =>
--#line  4739
 
yyval := 
yy.value_stack(yy.tos); 

when  657 =>
--#line  4740

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  658 =>
--#line  4746

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-3).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  659 =>
--#line  4752

	
yyval := (One_Tree, Unary.Make(
	  Operator => Unary.Is_Null_Op,
	  Operand => 
yy.value_stack(yy.tos-2).Tree));
    

when  660 =>
--#line  4757

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
    

when  661 =>
--#line  4768

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Is_Function_Of,
	  Prefix => 
yy.value_stack(yy.tos-5).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  662 =>
--#line  4777
 
yyval := 
yy.value_stack(yy.tos); 

when  663 =>
--#line  4778

	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  664 =>
--#line  4787
 
yyval := 
yy.value_stack(yy.tos); 

when  665 =>
--#line  4788
 
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  666 =>
--#line  4811
 
yyval := 
yy.value_stack(yy.tos); 

when  667 =>
--#line  4812

        --  NOTE: We treat '+' here separately to avoid
        --        reduce/reduce conflicts
	
yyval := (One_Tree, Binary.Make(
	  Operator => Binary.Plus_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  668 =>
--#line  4820

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  669 =>
--#line  4829
 
yyval := 
yy.value_stack(yy.tos); 

when  670 =>
--#line  4830

	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  671 =>
--#line  4839
 
yyval := 
yy.value_stack(yy.tos); 

when  672 =>
--#line  4840

	 -- right associative
	
yyval := (One_Tree, Binary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Binary_Op,
	  Left_Operand => 
yy.value_stack(yy.tos-2).Tree,
	  Right_Operand => 
yy.value_stack(yy.tos).Tree));
    

when  673 =>
--#line  4847

	-- unary ops have higher precedence 
	-- than every operator except the power_operator.
	
yyval := (One_Tree, Unary.Make(
	  Operator => 
yy.value_stack(yy.tos-1).Unary_Op,
	  Operand => 
yy.value_stack(yy.tos).Tree));
    

when  674 =>
--#line  4857
 
yyval := 
yy.value_stack(yy.tos); 

when  675 =>
--#line  4858
 
yyval := 
yy.value_stack(yy.tos); 

when  676 =>
--#line  4859
 
yyval := 
yy.value_stack(yy.tos-1); 

when  677 =>
--#line  4860
 
yyval := 
yy.value_stack(yy.tos-1); 

when  678 =>
--#line  4861
 
yyval := 
yy.value_stack(yy.tos-1); 

when  679 =>
--#line  4862

        
yyval := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  680 =>
--#line  4866
 
yyval := 
yy.value_stack(yy.tos); 

when  681 =>
--#line  4867

        --  An Sparkel Reduce expression
	
yyval := (One_Tree, Qualified_Name.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Id => 
yy.value_stack(yy.tos).Tree));
    

when  682 =>
--#line  4873

        --  An Sparkel Reduce expression
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => Qualified_Name.Make(
             Prefix => 
yy.value_stack(yy.tos-5).Tree,
             Id => 
yy.value_stack(yy.tos-3).Tree),
	  Operands => 
yy.value_stack(yy.tos-3).List));
    

when  683 =>
--#line  4882

        --  This is used in a map_reduce expression to specify the initial val
        
yyval := (One_Tree, Unary.Make(Unary.Initial_Value_Op,
          Operand => 
yy.value_stack(yy.tos-1).Tree));
    

when  684 =>
--#line  4890
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  685 =>
--#line  4893
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  686 =>
--#line  4896
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  687 =>
--#line  4899
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make(
yy.value_stack(yy.tos).Str, 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  688 =>
--#line  4902
 
	
yyval := (One_Tree, PSC.Trees.Identifier.Make("null", 
yy.value_stack(yy.tos).Source_Pos)); 
    

when  689 =>
--#line  4908

        
yyval := 
yy.value_stack(yy.tos);
    

when  690 =>
--#line  4911

        
yyval := 
yy.value_stack(yy.tos);
    

when  691 =>
--#line  4917

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  692 =>
--#line  4923

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List));
    

when  693 =>
--#line  4929

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => 
yy.value_stack(yy.tos-3).Tree,
	  Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-1).Tree))));
    

when  694 =>
--#line  4935

	
yyval := (One_Tree, Selection.Make(
	  Prefix => 
yy.value_stack(yy.tos-2).Tree,
	  Selector => 
yy.value_stack(yy.tos).Tree));
    

when  695 =>
--#line  4943

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  696 =>
--#line  4946

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  697 =>
--#line  4953
 
yyval := 
yy.value_stack(yy.tos); 

when  698 =>
--#line  4954

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  699 =>
--#line  4961
 
yyval := 
yy.value_stack(yy.tos); 

when  700 =>
--#line  4964
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  701 =>
--#line  4965
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  702 =>
--#line  4966
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Abs_Op); 

when  703 =>
--#line  4967
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Not_Op); 

when  704 =>
--#line  4968
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Plus_Op); 

when  705 =>
--#line  4969
 
yyval := (One_Unary_Op, 
yy.value_stack(yy.tos).Source_Pos, Unary.Minus_Op); 

when  706 =>
--#line  4973
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  707 =>
--#line  4974
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Plus_Op); 

when  708 =>
--#line  4975
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Minus_Op); 

when  709 =>
--#line  4976
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Ampersand_Op); 

when  710 =>
--#line  4980
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Times_Op); 

when  711 =>
--#line  4981
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Divide_Op); 

when  712 =>
--#line  4982
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Mod_Op); 

when  713 =>
--#line  4983
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Rem_Op); 

when  714 =>
--#line  4986
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Power_Op); 
    

when  715 =>
--#line  4990
 
yyval := 
yy.value_stack(yy.tos); 

when  716 =>
--#line  4991
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Divide_Assign_Op); 
    

when  717 =>
--#line  4997

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
     

when  718 =>
--#line  5000
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Plus_Assign_Op); 
    

when  719 =>
--#line  5003
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Minus_Assign_Op); 
    

when  720 =>
--#line  5006
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Times_Assign_Op); 
    

when  721 =>
--#line  5009
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Power_Assign_Op); 
    

when  722 =>
--#line  5012
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Combine_Assign_Op); 
    

when  723 =>
--#line  5015
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Ampersand_Assign_Op); 
    

when  724 =>
--#line  5018
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.And_Assign_Op); 
    

when  725 =>
--#line  5021
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Or_Assign_Op); 
    

when  726 =>
--#line  5024
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Xor_Assign_Op); 
    

when  727 =>
--#line  5027

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Left_Shift_Assign_Op);
    

when  728 =>
--#line  5030

	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Right_Shift_Assign_Op);
    

when  729 =>
--#line  5035
 
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  730 =>
--#line  5038
 
yyval := 
yy.value_stack(yy.tos); 

when  731 =>
--#line  5041

	yyerror("Use "":="" rather than ""="" in Sparkel");
	
yyval := (One_Assign_Op, 
yy.value_stack(yy.tos).Source_Pos, Assign_Stmt.Assign_Op); 
    

when  732 =>
--#line  5048
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Compare_Op); 

when  733 =>
--#line  5049
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op); 

when  734 =>
--#line  5050
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op); 

when  735 =>
--#line  5051
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.NEQ_Op); 

when  736 =>
--#line  5052
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Less_Op); 

when  737 =>
--#line  5053
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.LEQ_Op); 

when  738 =>
--#line  5054
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Greater_Op); 

when  739 =>
--#line  5055
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.GEQ_Op); 

when  740 =>
--#line  5056
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Left_Shift_Op ); 

when  741 =>
--#line  5057
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Right_Shift_Op); 

when  742 =>
--#line  5058
 
	yyerror("Use ""="" rather than ""=="" in Sparkel");
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Equal_Op);
    

when  743 =>
--#line  5065
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.And_Op); 

when  744 =>
--#line  5066
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Or_Op); 

when  745 =>
--#line  5067
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Xor_Op); 

when  746 =>
--#line  5069
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.And_Then_Op); 

when  747 =>
--#line  5071
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos-1).Source_Pos, Binary.Or_Else_Op); 

when  748 =>
--#line  5072
 
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Implies_Op); 

when  749 =>
--#line  5076
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Interval_Op); 
    

when  750 =>
--#line  5079
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Interval_Op); 
    

when  751 =>
--#line  5082
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Closed_Open_Interval_Op); 
    

when  752 =>
--#line  5085
 
	
yyval := (One_Binary_Op, 
yy.value_stack(yy.tos).Source_Pos, Binary.Open_Closed_Interval_Op); 
    

when  753 =>
--#line  5091
 
yyval := 
yy.value_stack(yy.tos); 

when  754 =>
--#line  5092
 
yyval := 
yy.value_stack(yy.tos); 

when  755 =>
--#line  5096

	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  756 =>
--#line  5113

	-- Type of aggregate specified
	
yyval := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => 
yy.value_stack(yy.tos-4).Tree,
	  Operands => 
yy.value_stack(yy.tos-1).List,
          Source_Pos => 
yy.value_stack(yy.tos-2).Source_Pos));
    

when  757 =>
--#line  5124

	
yyval := 
yy.value_stack(yy.tos);
    

when  758 =>
--#line  5127

	
yyval := (One_List, Lists.Empty_List);
    

when  759 =>
--#line  5133

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  760 =>
--#line  5136

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  761 =>
--#line  5143
 
yyval := 
yy.value_stack(yy.tos); 

when  762 =>
--#line  5144

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  763 =>
--#line  5149

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  764 =>
--#line  5154

	
yyval := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => 
yy.value_stack(yy.tos-2).Tree,
	  RHS => 
yy.value_stack(yy.tos).Tree));
    

when  765 =>
--#line  5164

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
    

when  766 =>
--#line  5217

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
    

when  767 =>
--#line  5266
 
yyval := 
yy.value_stack(yy.tos); 

when  768 =>
--#line  5267

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  769 =>
--#line  5270

	
yyval := (One_List, Lists.Empty_List);
    

when  770 =>
--#line  5276

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  771 =>
--#line  5279

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  772 =>
--#line  5286
 
yyval := 
yy.value_stack(yy.tos); 

when  773 =>
--#line  5287

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  774 =>
--#line  5292

	
yyval := (One_Tree, Reference.Make(
	  Key => 
yy.value_stack(yy.tos-2).Tree,
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  775 =>
--#line  5298

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
    

when  776 =>
--#line  5336

        
yyval := (One_Tree, Null_Optional_Tree);
    

when  777 =>
--#line  5340

        
yyval := 
yy.value_stack(yy.tos);
    

when  778 =>
--#line  5346
 
yyval := 
yy.value_stack(yy.tos); 

when  779 =>
--#line  5347
 
yyval := 
yy.value_stack(yy.tos); 

when  780 =>
--#line  5353

	
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
    

when  781 =>
--#line  5365

	
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
    

when  782 =>
--#line  5373

	
yyval := 
yy.value_stack(yy.tos);
    

when  783 =>
--#line  5376

	
yyval := (One_Tree, Null_Optional_Tree);
    

when  784 =>
--#line  5384

	
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
    

when  785 =>
--#line  5395

	
yyval := (One_List, Lists.Make((1 => 
yy.value_stack(yy.tos).Tree)));
    

when  786 =>
--#line  5398

	
yyval := 
yy.value_stack(yy.tos-2);
	Lists.Append(
yyval.List, 
yy.value_stack(yy.tos).Tree);
    

when  787 =>
--#line  5405

	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  788 =>
--#line  5412

	-- NOTE: "others" alternative must come last
	
yyval := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => 
yy.value_stack(yy.tos-2).Tree))),
	  Referent => 
yy.value_stack(yy.tos).Tree));
    

when  789 =>
--#line  5424

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
    

when  790 =>
--#line  5441

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
    

when  791 =>
--#line  5475

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
    

when  792 =>
--#line  5511
 
yyval := 
yy.value_stack(yy.tos); 

when  793 =>
--#line  5512
 
yyval := 
yy.value_stack(yy.tos); 

when  794 =>
--#line  5513
 
yyval := 
yy.value_stack(yy.tos); 

when  795 =>
--#line  5517
 
yyval := (Optional, True); 

when  796 =>
--#line  5518
 
yyval := (Optional, False); 

when  797 =>
--#line  5522
 
yyval := 
yy.value_stack(yy.tos); 

when  798 =>
--#line  5523
 
yyval := 
yy.value_stack(yy.tos); 

when  799 =>
--#line  5524
 
yyval := 
yy.value_stack(yy.tos); 

when  800 =>
--#line  5529

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
    

when  801 =>
--#line  5553
 
yyval := 
yy.value_stack(yy.tos); 

when  802 =>
--#line  5554
 
yyval := 
yy.value_stack(yy.tos); 

when  803 =>
--#line  5555
 
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

end Sparkel_Parser;
