--                              P A R A S A I L                             --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
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
--                                                                          --
-- The ParaSail language and implementation were originally developed by    --
-- S. Tucker Taft.                                                          --
------------------------------------------------------------------------------

--------------------------------------
-- YACC Grammar for ParaSail
--------------------------------------

-- Single-character delimiters --
%token ',' ';' ':' '.'
%token '+' '-' '*' '/' 
%token '?'
%token '(' ')' '[' ']' '<' '>'
%token '|' 
%token '='    -- for error recovery only
%token PRIME -- '''

-- Compound delimiters --
%token L_ASSERT -- "{>"  -- so PARython can use { ... } for sets/maps
%token R_ASSERT -- "<}"  -- so PARython can use { ... } for sets/maps
%token L_SET    -- "{"   -- only used in PARython mode
%token R_SET    -- "}"   -- only used in PARython mode
%token COMPARE -- "=?"
%token EQ   -- "=="
%token NEQ  -- "!="
%token GEQ  -- ">="
%token LEQ  -- "<="
%token LSHIFT -- "<<"
%token POWER  -- "**"
%token ASSIGN -- ":="
%token SWAP   -- "<=>"
%token MOVE   -- "<=="
%token COMBINE_MOVE -- "<|="
%token DOT_DOT -- ".."
%token OPEN_CLOSED_INTERVAL -- "<.."
%token OPEN_INTERVAL -- "<..<"
%token CLOSED_OPEN_INTERVAL -- "..<"
%token DOUBLE_COLON  -- "::"
%token REFERS_TO  -- "=>"
%token GIVES    -- "->"
%token IMPLIES    -- "==>"
%token PARALLEL   -- "||"
%token PLUS_ASSIGN -- "+="
%token MINUS_ASSIGN -- "-="
%token TIMES_ASSIGN -- "*="
%token DIVIDE_ASSIGN -- "/="
%token POWER_ASSIGN -- "**="
%token COMBINE_ASSIGN -- "|="
%token AND_ASSIGN -- "and="
%token OR_ASSIGN -- "or="
%token XOR_ASSIGN -- "xor="
%token LSHIFT_ASSIGN -- "<<="
%token RSHIFT_ASSIGN -- ">>="

--  Significant indenting and newlines.
--  General idea: ':' takes place of "then", "is", etc.
--  Newline is significant when *not* following ':' or ';'
--  and next line is *not* indented.
--  A blank line ensures that next line is *not* considered a continuation line
--  Indent is significant after ':'
%token NEWLINE
%token OUTDENT  -- used for "significant" indenting
%token INDENT   -- used for "significant" indenting
%token EOL_COLON -- ':' at the end of a line

-- The "normal" operators with "." or ":" on either or both sides
-- (useful for user-defined operators -- same precedence as base op).
%token PLUS_BASED_OP
%token MINUS_BASED_OP
%token MUL_BASED_OP
%token DIV_BASED_OP
%token EXP_BASED_OP
%token OR_BASED_OP
%token AND_BASED_OP
%token XOR_BASED_OP
%token NOT_BASED_OP
%token LSHIFT_BASED_OP
%token RSHIFT_BASED_OP
%token COMBINE_BASED_OP

-- Literals --
%token Char_Literal
%token Enum_Literal
%token Integer_Literal 
%token Real_Literal
%token String_Literal

-- Identifier --
%token Identifier 

-- Reserved words --
%token ABS_kw
%token ABSTRACT_kw
%token ALL_kw
%token AND_kw
%token BEGIN_kw   -- used for error recovery only
%token BLOCK_kw
%token CASE_kw
%token CLASS_kw
%token CONCURRENT_kw
%token CONST_kw
%token CONTINUE_kw
%token EACH_kw
%token ELSE_kw
%token ELSIF_kw
%token END_kw
%token EXIT_kw
%token EXPORTS_kw
%token EXTENDS_kw
%token FOR_kw
%token FORWARD_kw
%token FUNC_kw
%token GLOBAL_kw
%token IF_kw
%token IMPLEMENTS_kw
%token IMPORT_kw
%token IN_kw
%token INTERFACE_kw
%token IS_kw
%token LAMBDA_kw
%token LIMITED_kw
%token LOCKED_kw
%token LOOP_kw
%token MOD_kw
%token NEW_kw
%token NOT_kw
%token NULL_kw
%token OF_kw
%token OP_kw
%token OPT_kw
%token OPTIONAL_kw
%token OR_kw
%token PRIVATE_kw
%token QUEUED_kw
%token REF_kw
%token REM_kw
%token RETURN_kw
%token REVERSE_kw
%token SOME_kw
%token THEN_kw
%token TYPE_kw
%token UNTIL_kw
%token VAR_kw
%token WHILE_kw
%token WITH_kw
%token XOR_kw

%start module_list

%with PSC.Messages;
%with PSC.Trees;
%with PSC.Strings;
%with PSC.Source_Positions;
%with PSC.Trees.Lists;
%with PSC.Trees.Param_Decl;
%with PSC.Trees.Unary;
%with PSC.Trees.Binary;
%with PSC.Trees.Assign_Stmt;
%with PSC.Trees.Control_Stmt;
%use PSC;
%use PSC.Trees;

{ 
    type yystype_enum is (
      One_Token, One_Tree, One_List, Optional, 
      Construct_Qualifier,
      Formals_And_Interfaces,
      Two_Lists,
      Input_And_Global_Lists,
      Param_Mode,
      One_Unary_Op,
      One_Binary_Op,
      One_Assign_Op,
      Construct_Kind,
      Optional_End_Token);

    type yystype(Kind : yystype_enum := One_Tree) is record
      case Kind is
	when One_Tree =>
	  Tree : Optional_Tree;
	when One_List =>
	  List : Lists.List;
	when Optional =>
	  Is_Present : Boolean := False;
	when Formals_And_Interfaces =>
	  Has_Module_Formals : Boolean := False;
	  Module_Formals : Lists.List;
	  Extends : Optional_Tree;
	  Implements : Lists.List;
        when Two_Lists =>
	  First_List : Lists.List;
	  Second_List : Lists.List;
        when Input_And_Global_Lists =>
	  Inputs_List : Lists.List;
	  Global_Read_List : Lists.List;
	  Global_Update_List : Lists.List;
	when Param_Mode =>
	  Param_Kind : Param_Decl.Param_Kind;
	  Param_Locking : Param_Decl.Param_Locking;
	when One_Token | Construct_Qualifier |
           One_Unary_Op | One_Binary_Op | One_Assign_Op |
           Optional_End_Token =>
	  Source_Pos : Source_Positions.Source_Position;
	  case Kind is
	    when One_Token =>
	      Str : PSC.Strings.U_String;
            when Construct_Qualifier =>
              Is_Abstract : Boolean := False;
              Is_Private : Boolean := False;
              Is_Concurrent : Boolean := False;
              Is_Limited : Boolean := False;
              Is_Optional : Boolean := False;
              Is_While : Boolean := False;
              Is_Until : Boolean := False;
              Is_Ref : Boolean := False;
              Is_Global : Boolean := False;
              Is_Queued : Boolean := False;
	    when One_Unary_Op =>
	      Unary_Op : Unary.Unary_Operator_Enum;
	    when One_Binary_Op =>
	      Binary_Op : Binary.Binary_Operator_Enum;
	    when One_Assign_Op =>
	      Assign_Op : Assign_Stmt.Assign_Operator_Enum;
            when Optional_End_Token =>
              Label : Optional_Tree := Null_Optional_Tree;
              End_With_Values : Optional_Tree := Null_Optional_Tree;
              Check_Label : Boolean := False;
	    when others => null;
	  end case;
	when Construct_Kind =>
	  Exitable_Construct : Control_Stmt.Exitable_Construct_Enum;
      end case;
    end record;

    use type Param_Decl.Param_Kind;
}


%%

module_list : 
    module
  | module_list module
  ;

module : 
    import_clauses interface_declaration_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | import_clauses class_definition_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | import_clauses standalone_operation_definition_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | import_clauses error SEMI_or_NEWLINE{
	null;
    }
  ;

SEMI_or_NEWLINE : NEWLINE | ';' | ';' NEWLINE ;

import_clauses : import_clause_list {
        if not Lists.Is_Empty ($1.List)
          or else PSC.Syntax.Cur_File /= Last_Import_File then
            --  We have a new non-empty list of imports, or a new file;
            --  this list overrides any earlier list
            Last_Import_List := $1.List;
            Last_Import_File := PSC.Syntax.Cur_File;
        end if;
        $$ := (One_List, Last_Import_List);
    }
  ;

import_clause_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | import_clause_list IMPORT_kw imported_module_name_list SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $3.List);  
	  -- TBD: Do we care how these were grouped?
    }
  ;

imported_module_name_list : 
    imported_module_name {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | imported_module_name_list ',' imported_module_name {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

imported_module_name :
    '*' {
        $$ := (One_Tree, PSC.Trees.Identifier.Make ($1.Str, $1.Source_Pos)); 
    }
  | qualified_name {
        $$ := $1;
    }
  | qualified_name DOUBLE_COLON '*' {
	$$ := (One_Tree, Qualified_Name.Make (
	  Prefix => $1.Tree,
	  Id => PSC.Trees.Identifier.Make ($3.Str, $3.Source_Pos))); 
    }
  ;

interface_declaration_with_term : 
   opt_interface_qualifier INTERFACE_kw module_defining_name 
     formals_and_implemented_interfaces
     COLON_OR_IS_kw_INDENT
      interface_element_list
      opt_annotation_opt_term
      opt_new_interface_element_list
      opt_restricted_element_list
   OUTDENT_opt_NEWLINE opt_END_INTERFACE {
      declare
	Elem_List : Lists.List := $6.List;
      begin
	if $1.Is_Private and then $4.Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	if not Lists.Is_Empty($7.List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => $7.List));
	end if;
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => True,
	  Is_Abstract => $1.Is_Abstract,
	  Is_Private => $1.Is_Private,
	  Is_Concurrent => $1.Is_Concurrent,
	  Is_Limited => $1.Is_Limited,
	  Has_Formals => $4.Has_Module_Formals,
	  Module_Formals => $4.Module_Formals,
	  Extends_Interface => $4.Extends,
	  Implements_Interfaces => $4.Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => $8.List,
	  Module_Implements => $9.List));

        if $11.Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
              Ending_Id => $11.Label);
        end if;

      end;
   }
 | opt_interface_qualifier INTERFACE_kw module_defining_name 
     formals_and_implemented_interfaces
     IS_kw_NEWLINE
   END_kw opt_INTERFACE_kw module_defining_name SEMI_or_NEWLINE {
	if $1.Is_Private and then $4.Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => True,
	  Is_Abstract => $1.Is_Abstract,
	  Is_Private => $1.Is_Private,
	  Is_Concurrent => $1.Is_Concurrent,
	  Is_Limited => $1.Is_Limited,
	  Has_Formals => $4.Has_Module_Formals,
	  Module_Formals => $4.Module_Formals,
	  Extends_Interface => $4.Extends,
	  Implements_Interfaces => $4.Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));

	Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
	  Ending_Id => $8.Tree);
    }
 | opt_interface_qualifier INTERFACE_kw module_defining_name 
     formals_and_implemented_interfaces opt_EOL_COLON_NEWLINE {
        --  This is an interface without any declarations

	if $1.Is_Private and then $4.Has_Module_Formals then
	    yyerror("Private interface may not add module parameters");
	end if;
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => True,
	  Is_Abstract => $1.Is_Abstract,
	  Is_Private => $1.Is_Private,
	  Is_Concurrent => $1.Is_Concurrent,
	  Is_Limited => $1.Is_Limited,
	  Has_Formals => $4.Has_Module_Formals,
	  Module_Formals => $4.Module_Formals,
	  Extends_Interface => $4.Extends,
	  Implements_Interfaces => $4.Implements,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    }
  ;

IS_kw_INDENT : IS_kw INDENT { $$ := $1; }
  | error IS_kw INDENT {
        yyerror ("Syntax error before ""is""", At_Token => $2);
    }
  ;

COLON_or_IS_kw_INDENT : 
    IS_kw INDENT  { $$ := $1; }
  | COLON_INDENT  { $$ := $1; }
  | error COLON_or_IS_kw_INDENT {
        yyerror ("Syntax error before ""is""", At_Token => $2);
        $$ := $2;
    }
  ;

opt_EOL_COLON_NEWLINE :
    NEWLINE
  | EOL_COLON NEWLINE
  | error NEWLINE {
        yyerror ("Syntax error at end-of-line", At_Token => $2);
    }
  | error EOL_COLON NEWLINE {
        yyerror ("Syntax error before ':'", At_Token => $2);
    }
  ;

OUTDENT_opt_NEWLINE :
    OUTDENT { $$ := $1; }
  | OUTDENT NEWLINE  { $$ := $2; }
  ;

OUTDENT_END_kw : OUTDENT_opt_NEWLINE END_kw { $$ := $2; };

opt_END_INTERFACE : 
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw opt_INTERFACE_kw module_defining_name SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;

IS_kw_NEWLINE : IS_kw NEWLINE ;

opt_INTERFACE_kw : INTERFACE_kw
  | {
	yyerror("Should be ""end interface <id>"" rather than ""end <id>""");
    }
  ;
   
opt_interface_qualifier : 
    interface_qualifier { $$ := $1; }
  | {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;

interface_qualifier : 
    class_qualifier { $$ := $1; }
  | ABSTRACT_kw opt_class_qualifier {
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Abstract => True, 
	  Is_Concurrent => $2.Is_Concurrent,
          Is_Limited => $2.Is_Limited,
	  others => False);
    }
  | PRIVATE_kw opt_class_qualifier {
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => $2.Is_Concurrent,
          Is_Limited => $2.Is_Limited,
	  others => False);
    }
  ;

opt_class_qualifier : 
    class_qualifier { $$ := $1; }
  | { 
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;

class_qualifier :
      CONCURRENT_kw {
	$$ := (Construct_Qualifier, 
               Source_Pos => $1.Source_Pos,
	       Is_Concurrent => True, others => False);
      }
    | LIMITED_kw {
	$$ := (Construct_Qualifier, 
               Source_Pos => $1.Source_Pos,
	       Is_Limited => True, others => False);
      }
    ;

standalone_operation_definition_with_term : 
    func_definition_with_term {
        --  NOTE: We don't allow stand-alone operator definitions
        $$ := $1;
  }
  | operation_import SEMI_or_NEWLINE { $$ := $1; }
  | operation_equiv SEMI_or_NEWLINE { $$ := $1; }
  ;

formals : 
    '<' module_formal_list '>' { $$ := $2; }
  | '<' module_formal_list ';' global_access_list '>' {
        --  TBD: Do something with global_access_list
        $$ := $2;
    }
  | '<' '>' { $$ := (One_List, Lists.Empty_List); }
  | '<' global_access_list '>' {
        --  TBD: Do something with global_access_list
        $$ := (One_List, Lists.Empty_List);
    }
  ;

formals_and_implemented_interfaces :
    opt_formals opt_implements_list {
	$$ := (Formals_And_Interfaces,
	  Has_Module_Formals => ($1.Kind = One_List),
	  Module_Formals => List_Or_Empty ($1),
	  Extends => Null_Optional_Tree,
	  Implements => $2.List);
    }
  | opt_formals EXTENDS_kw interface_name opt_implements_list {
        $$ := (Formals_And_Interfaces,
          Has_Module_Formals => ($1.Kind = One_List),
          Module_Formals => List_Or_Empty ($1),
          Extends => Param_Decl.Make(
            Name => Null_Optional_Tree,
            Kind => Param_Decl.Var_Param,  --  So is writable
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => $3.Tree,
            Param_Default => Null_Optional_Tree),
          Implements => $4.List);
    }
  | opt_formals EXTENDS_kw id ':' interface_name opt_implements_list {
      declare
	Extends_Decl : constant Optional_Tree := Param_Decl.Make(
	  Name => $3.Tree,
	  Kind => Param_Decl.Var_Param,  --  So is writable
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,  -- TBD
	  Param_Type => $5.Tree,
	  Param_Default => Null_Optional_Tree);
      begin
        $$ := (Formals_And_Interfaces,
          Has_Module_Formals => ($1.Kind = One_List),
          Module_Formals => List_Or_Empty ($1),
          Extends => Extends_Decl,
          Implements => $6.List);
      end;
    }
  ; 

COLON_no_indent : ':' | EOL_COLON {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    };

opt_formals : 
    formals { $$ := $1; } 
  | { 
	$$ := (Optional, Is_Present => False);
    }
  ;

opt_implements_list : 
    implements_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

implements_list : IMPLEMENTS_kw interface_name_list { $$ := $2; } ;

interface_name_list :
    interface_name { 
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | interface_name_list ',' interface_name {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

interface_name : 
    module_name { $$ := $1; }
  | module_instantiation { $$ := $1; }
  ;
   
module_name : qualified_name { $$ := $1; };

module_defining_name : 
    qualified_name { $$ := $1; }
  | qualified_name add_on_label { 
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => $1.Tree,
	  Operands => $2.List));
    }
  ;

add_on_label : 
    '[' operation_actual_list ']' {
	$$ := $2;
    };

module_formal_list : 
    annotated_module_formal { $$ := $1; }
  | module_formal_list ';' annotated_module_formal {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

annotated_module_formal : 
    opt_annotation type_formal opt_annotation {
	Annotation.Add_Annotation($2.Tree, $1.List, Precedes => True);
	Annotation.Add_Annotation($2.Tree, $3.List);
	$$ := (One_List, Lists.Make((1 => $2.Tree)));
    }
  | opt_annotation value_formal opt_annotation {
	$$ := $2;
	if not Lists.Is_Empty($1.List) then
	    -- Add annotation to first element
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, 1), $1.List, Precedes => True);
	end if;

	if not Lists.Is_Empty($3.List) then
	    -- Add annotation to last element
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $3.List);
	end if;
    }
  ;

opt_annotation : 
    annotation { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

opt_annotation_opt_term : 
    annotation_opt_term { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

type_formal : 
    id IS_no_indent module_instantiation {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $1.Tree,
	  Is_New_Type => False,
	  Type_Definition => $3.Tree));
    }
  | module_instantiation { 
	$$ := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => $1.Tree));
    }
  ;

IS_no_indent : IS_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
        $$ := $1;
    };

opt_operation_default : 
    IS_no_indent simple_expression { $$ := $2; }
  | { $$ := (One_Tree, Null_Optional_Tree); }
  ;

value_formal : 
    id_list COLON_no_indent
      opt_output_type_qualifier operand_type_specifier 
      opt_ASSIGN_or_equal_simple_expression  {
	-- "simple_expression" to avoid use of '>'
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => $3.Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => $3.Is_Optional, others => False),
                 Operand => Copy_If_Not_First ($4.Tree, I)),
	      Param_Default => Copy_If_Not_First ($5.Tree, I)));
	end loop;
    }
  ;

opt_ASSIGN_or_equal_simple_expression :
    ASSIGN_or_equal simple_expression { $$ := $2; }
  | { $$ := (One_Tree, Null_Optional_Tree); }
  ;
	

id : Identifier {
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos));
    }
--    | BEGIN_kw {   -- TBD: Should be allowed as identifier too eventually
-- 	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos));
--     }
   ;

id_list : 
    id {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | id_list ',' id {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

type_name : 
    qualified_name { $$ := $1; }
  | polymorphic_type_name { $$ := $1; }
  ;

polymorphic_type_name : qualified_name '+' {
	$$ := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => $1.Tree));
    };

qualified_name : 
    id_or_string_literal { 
	$$ := $1;
    }
  | qualified_name DOUBLE_COLON id_or_string_literal {
	$$ := (One_Tree, Qualified_Name.Make(
	  Prefix => $1.Tree,
	  Id => $3.Tree));
    }
  | qualified_name DOUBLE_COLON '<' opt_module_actual_list '>' {
        --  Using Rust's "TurboFish" notation to provide module params
        $$ := (One_Tree, Invocation.Make(
          Kind => Invocation.Module_Instantiation,
          Prefix => $1.Tree,
          Operands => $4.List));
    }
  ;

id_or_string_literal :
    id { $$ := $1; }
  | String_Literal {
        -- String_Literal can be used as a "name" when it is an operator
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  ;
  
global_access_list :
    global_read_only_list {
        $$ := (Input_And_Global_Lists, Global_Read_List => $1.List,
               others => Lists.Empty_List);
    }
  | global_read_only_list ';' global_var_list {
        $$ := (Input_And_Global_Lists,
               Global_Read_List => $1.List,
               Global_Update_List => $3.List,
               others => Lists.Empty_List);
    }
  | global_var_list {
        $$ := (Input_And_Global_Lists, Global_Update_List => $1.List,
               others => Lists.Empty_List);
    }
  ;

global_read_only_list :
    GLOBAL_kw imported_module_name_list {
	$$ := $2;
    }
  ;

global_var_list :
    GLOBAL_kw VAR_kw imported_module_name_list {
	$$ := $3;
    }
  ;

module_instantiation : 
    module_name '<' opt_module_actual_list '>' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => $1.Tree,
	  Operands => $3.List));
    }
  | name '[' opt_operation_actual_list ']' '<' opt_module_actual_list '>' {
	-- Include extension label in module name
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
	    Invocation.Make(
	      Kind => Invocation.Container_Indexing,
	      Prefix => $1.Tree,
	      Operands => $3.List),
	  Operands => $6.List));
    }
  ;

opt_add_on_label :
    add_on_label { $$ := $1; }
  | { 
	$$ := (One_List, Lists.Empty_List);
    }
  ;

opt_module_actual_list : 
    module_actual_list { $$ := $1; }
    | {
	$$ := (One_List, Lists.Empty_List);
    }
    ;

module_actual_list :
    module_actual {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | module_actual_list ',' module_actual {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

module_actual : 
    simple_type_specifier_or_expression { $$ := $1; }
  | id REFERS_TO simple_type_specifier_or_expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  ;

-- simple_expression subsumes simple type_name in this rule
simple_type_specifier_or_expression : 
    qualified_name annotation { 
	-- polymorphic type name not allowed here
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  | qualified_type_specifier opt_annotation { 
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  | adding_expression '+' {
	-- This is a polymorphic type name, presumably.
	-- We use adding_expression instead of qualified_name
	-- to avoid reduce/reduce conflicts in the grammar.
	$$ := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => $1.Tree));
    }
  | simple_expression { $$ := $1; }             
	-- simple_expr to avoid problems with '>'
  | lambda_expression { $$ := $1; }
  | module_instantiation { $$ := $1; }
  | tuple_type_specifier { $$ := $1; }
  ;
  
annotated_type_specifier : 
    type_specifier { $$ := $1; }
  | type_specifier annotation {
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  | func_type_specifier {
        $$ := $1;
    }
  | tuple_type_specifier {
        $$ := $1;
    }
  ;

tuple_type_specifier :
    '(' tuple_element_list ')' {
        $$ := (One_Tree, Invocation.Make
	        (Kind => Invocation.Tuple_Type_Definition,
	         Prefix => Null_Optional_Tree,
	         Operands => $2.List,
	         Source_Pos => $1.Source_Pos));
    }
  ;

tuple_element_list :
    tuple_element {
        $$ := $1;
    }
  | tuple_element_list ';' tuple_element {
        $$ := $1;
	Lists.Append($$.List, $3.List);
    }
  | tuple_element_list ',' tuple_element {
	yyerror("Tuple types must be separated by "";""",
          At_Token => $2);
        $$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

tuple_element :
    class_component_list ':' type_specifier {
      declare
	Id_List : Lists.List := $1.List;
      begin
	$$ := (One_List, Lists.Empty_List);
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
                    Lists.Append($$.List, Obj_Decl.Make(
                      Name => PSC.Trees.Identifier.Tree (Tree_Ptr_Of (Id).all),
                      Is_Var => False,
                      Is_Const => True,
                      Is_Ref => False,
                      Is_Optional => False,
                      In_Region => Null_Optional_Tree,
                      Obj_Type => Copy_If_Not_First ($3.Tree, I),
                      Obj_Value => Null_Optional_Tree));
                end if;
            end;
	end loop;
      end;
    }
  ;

type_specifier :
    basic_type_specifier { $$ := $1; }
  | qualified_type_specifier { $$ := $1; }
  ;

basic_type_specifier : 
    type_name { 
	$$ := $1;
    }
  | module_instantiation { 
	$$ := $1;
    }
  | module_instantiation EXTENDS_kw type_specifier { 
	$$ := (One_Tree, Invocation.Add_Extends(
	  Instantiation => $1.Tree, 
	  Extends => $3.Tree));
    }
  ;

qualified_type_specifier : 
    type_qualifier type_name { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => $1.Is_Optional,
	    Is_Concurrent => $1.Is_Concurrent,
	    others => False), 
	  Operand => $2.Tree));
    }
  | type_qualifier module_instantiation { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => $1.Is_Optional,
	    Is_Concurrent => $1.Is_Concurrent,
	    others => False), 
	  Operand => $2.Tree));
    }
  | type_qualifier module_instantiation 
      EXTENDS_kw type_specifier { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => $1.Is_Optional,
	    Is_Concurrent => $1.Is_Concurrent,
	    others => False), 
	  Operand => Invocation.Add_Extends(
	    Instantiation => $2.Tree, 
	    Extends => $4.Tree)));
    }
  ;

type_qualifier :
    OPT_or_OPTIONAL_kw opt_CONCURRENT_kw { 
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => $2.Is_Present,
	  others => False);
    }
  | CONCURRENT_kw {
	$$ := (Construct_Qualifier, 
               Source_Pos => $1.Source_Pos,
               Is_Concurrent => True, others => False);
    }
  ;

opt_CONCURRENT_kw : 
    CONCURRENT_kw {
	$$ := (Optional, True);
    }
  | {
	$$ := (Optional, False);
    }
  ;

func_type_specifier :  -- TBD: "optional" and "queued" not handled yet
    FUNC_kw operation_inputs {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => $2.Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => $2.Global_Read_List,
          Global_Update_List => $2.Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($1));
    }
  | FUNC_kw operation_inputs GIVES_or_RETURN_kw operation_outputs {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => $2.Inputs_List,
	  Operation_Outputs => $4.List,
          Global_Read_List => $2.Global_Read_List,
          Global_Update_List => $2.Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($1));
    }
  ;

opt_new_interface_element_list : { $$ := (One_List, Lists.Empty_List); }
  | NEW_kw_opt_NL interface_element_list opt_annotation {
	if Lists.Is_Empty($2.List) then
	    -- We want to make sure that we return a non-empty list
	    $$ := (One_List, 
	      Lists.Make((1 => Annotation.Make(Annotations => $3.List))));
	else
	    $$ := $2;
	    if not Lists.Is_Empty($3.List) then
		-- Add annotation to item at end of list
		Annotation.Add_Annotation(
		  Lists.Nth_Element($$.List, Lists.Length($$.List)), 
		  $3.List);
	    end if;
	end if;
    }
  ;

NEW_kw_opt_NL : NEW_kw | NEW_kw NEWLINE ;

interface_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | interface_element_list opt_annotation_opt_term interface_element_with_term {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.Tree);
    }
  | interface_element_list opt_annotation_opt_term operation_import SEMI_or_NEWLINE {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.Tree);
    }
  | interface_element_list opt_annotation_opt_term operation_equiv SEMI_or_NEWLINE {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to interface_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.Tree);
    }
--   | interface_element_list annotation_with_term {
-- 	$$ := $1;
-- 	Lists.Append($$.List, Annotation.Make(Annotations => $2.List));
--     }
  | interface_element_list error SEMI_or_NEWLINE {
	$$ := $1;
    }
  ;

interface_element_with_term : 
    simple_interface_element SEMI_or_NEWLINE { $$ := $1; }
  | interface_declaration_with_term { $$ := $1; }
  ;

simple_interface_element :
    operation_declaration { $$ := $1; }
  | object_declaration { $$ := $1; }
  | type_declaration { $$ := $1; }
  ;

opt_restricted_element_list :
    restricted_element_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

restricted_element_list :
    IMPLEMENTS_kw_opt_NL interface_element_list opt_annotation_opt_term { 
      declare
	Elem_List : Lists.List := $2.List;
      begin
	if not Lists.Is_Empty($3.List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => $3.List));
	end if;
	$$ := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    }
  | IMPLEMENTS_kw_opt_NL FOR_kw interface_name_list_opt_NL
      interface_element_list opt_annotation_opt_term {
      declare
	Elem_List : Lists.List := $4.List;
      begin
	if not Lists.Is_Empty($5.List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => $5.List));
	end if;
	$$ := (One_List, Lists.Make((1 =>
	  Implements_Element.Make(
	    For_Interfaces => $3.List, Elements => Elem_List))));
      end;
    }
  | restricted_element_list IMPLEMENTS_kw_opt_NL FOR_kw interface_name_list_opt_NL 
      interface_element_list opt_annotation_opt_term {
      declare
	Elem_List : Lists.List := $5.List;
      begin
	$$ := $1;
	if not Lists.Is_Empty($6.List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => $6.List));
	end if;
	Lists.Append($$.List,
	  Implements_Element.Make(
	    For_Interfaces => $4.List, Elements => Elem_List));
      end;
    }
  ;

IMPLEMENTS_kw_opt_NL : IMPLEMENTS_kw | IMPLEMENTS_kw NEWLINE ;

interface_name_list_opt_NL : interface_name_list { $$ := $1; }
  | interface_name_list NEWLINE { $$ := $1; }
  ;

opt_restricted_class_element_list :
    restricted_class_element_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

restricted_class_element_list :
    IMPLEMENTS_kw exported_class_element_list { 
      declare
	Elem_List : Lists.List := $2.List;
      begin
	$$ := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    }
  | IMPLEMENTS_kw_opt_NL FOR_kw interface_name_list_opt_NL 
     exported_class_element_list { 
      declare
	Elem_List : Lists.List := $4.List;
      begin
	$$ := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => $3.List,
	  Elements => Elem_List))));
      end;
    }
  | restricted_class_element_list IMPLEMENTS_kw_opt_NL FOR_kw interface_name_list_opt_NL 
      exported_class_element_list {
      declare
	Elem_List : Lists.List := $5.List;
      begin
	$$ := $1;
	Lists.Append($$.List,
	  Implements_Element.Make(
	    For_Interfaces => $4.List, Elements => Elem_List));
      end;
    }
  ;

operation_import :
    op_declaration IS_kw_ignore_INDENT import_operation {
      $$ := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => $1.Tree, Import_Info => $3.List));
    }
  | func_declaration IS_kw_ignore_INDENT import_operation {
      $$ := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => $1.Tree, Import_Info => $3.List));
    }
  ;

IS_kw_ignore_INDENT :
    IS_kw
  | IS_kw INDENT {
        --  Pop the indent stack
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;
    }
  ;
operation_equiv :
    op_declaration IS_kw qualified_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
    }
  | func_declaration IS_kw qualified_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
    }
  | op_declaration IS_kw IN_kw basic_type_specifier {
	-- Indicate that operation should be found in given type
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $4.Tree));
    }
  | func_declaration IS_kw IN_kw basic_type_specifier {
	-- Indicate that operation should be found in given type
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $4.Tree));
    }
  | op_declaration IS_kw qualified_name IN_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
    }
  | func_declaration IS_kw qualified_name IN_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
    }
  | op_declaration IS_kw '(' opt_class_component_list right_paren {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => $4.List,
	       Source_Pos => $3.Source_Pos);
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);
	end;
    }
  | func_declaration IS_kw '(' opt_class_component_list right_paren {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => $4.List,
	       Source_Pos => $3.Source_Pos);
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);
	end;
    }
  | op_declaration IS_kw_INDENT qualified_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    }
  | func_declaration IS_kw_INDENT qualified_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    }
  | op_declaration IS_kw_INDENT qualified_name IN_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    }
  | func_declaration IS_kw_INDENT qualified_name IN_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
    }
  | op_declaration IS_kw_INDENT '(' opt_class_component_list right_paren {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => $4.List,
	       Source_Pos => $3.Source_Pos);
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);
            if ParaSail_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
	end;
    }
  | func_declaration IS_kw_INDENT '(' opt_class_component_list right_paren {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Is_Expression_Function := True;
	    Op_Decl.Statements := Invocation.Make
	      (Kind => Invocation.Class_Aggregate,
	       Prefix => Null_Optional_Tree,
	       Operands => $4.List,
	       Source_Pos => $3.Source_Pos);
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);
            if ParaSail_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            ParaSail_Lex.Top := ParaSail_Lex.Top - 1;  --  Pop the indent stack
	end;
    }
  ;

class_definition_with_term :
   opt_class_qualifier CLASS_kw module_defining_name 
      formals_and_implemented_interfaces
   IS_kw
      INDENT_class_element_list
      opt_new_class_element_list
      opt_restricted_class_element_list
   OUTDENT_opt_NEWLINE opt_END_CLASS {
        -- TBD: allow an annotation after class_element_list
	$$ := (One_Tree, PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => False,
	  Is_Abstract => $1.Is_Abstract,
	  Is_Private => $1.Is_Private,
	  Is_Concurrent => $1.Is_Concurrent,
	  Is_Limited => $1.Is_Limited,
	  Has_Formals => $4.Has_Module_Formals,
	  Module_Formals => $4.Module_Formals,
	  Extends_Interface => $4.Extends,
	  Implements_Interfaces => $4.Implements,
	  Class_Locals => $6.First_List,
	  Module_Exports => $6.Second_List,
	  Module_New_Exports => $7.List,
	  Module_Implements => $8.List));
	    -- NOTE: Module_Implements is where bodies would go
	    --       if there is some ambiguity between operations that
	    --       are in the "normal" interface part vs. in the
	    --       "implements" part of the interface.

        if $10.Check_Label then
	    Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
	      Ending_Id => $10.Label);
        end if;
   } 
 | opt_class_qualifier CLASS_kw module_defining_name 
      formals_and_implemented_interfaces
   EOL_COLON
      INDENT_class_element_list
      opt_new_class_element_list
      opt_restricted_class_element_list
   OUTDENT_opt_NEWLINE opt_END_CLASS {
        -- TBD: allow an annotation after class_element_list
	$$ := (One_Tree, PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => False,
	  Is_Abstract => $1.Is_Abstract,
	  Is_Private => $1.Is_Private,
	  Is_Concurrent => $1.Is_Concurrent,
	  Is_Limited => $1.Is_Limited,
	  Has_Formals => $4.Has_Module_Formals,
	  Module_Formals => $4.Module_Formals,
	  Extends_Interface => $4.Extends,
	  Implements_Interfaces => $4.Implements,
	  Class_Locals => $6.First_List,
	  Module_Exports => $6.Second_List,
	  Module_New_Exports => $7.List,
	  Module_Implements => $8.List));
	    -- NOTE: Module_Implements is where bodies would go
	    --       if there is some ambiguity between operations that
	    --       are in the "normal" interface part vs. in the
	    --       "implements" part of the interface.

        if $10.Check_Label then
	    Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
	      Ending_Id => $10.Label);
        end if;

   }
  ; 

opt_END_CLASS : 
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw opt_CLASS_kw module_defining_name SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;
   
opt_CLASS_kw : CLASS_kw
  | {
	yyerror("Should be ""end class <id>"" rather than ""end <id>""");
    }
  ;

opt_new_class_element_list : { $$ := (One_List, Lists.Empty_List); }
  | NEW_kw_opt_NL exported_class_element_list {
	if Lists.Is_Empty($2.List) then
	    -- We want to make sure that we return a non-empty list
	    $$ := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    $$ := $2;
	end if;
    }
  ;

INDENT_class_element_list : 
  EXPORTS_kw INDENT 
    exported_class_element_list {
	$$ := (Two_Lists, Lists.Empty_List, $3.List);
    }
--   | INDENT local_class_element_list
--   EXPORTS_kw_NEWLINE
--     exported_class_element_list {
-- 	$$ := (Two_Lists, $2.List, $4.List);
--     }
  | INDENT local_class_element_list
    opt_annotation
  EXPORTS_kw_NEWLINE
    exported_class_element_list {
	-- Include annotation at end of locals
      declare
	Locals : Lists.List := $2.List;
      begin
        if not Lists.Is_Empty ($3.List) then
            Lists.Append(Locals, Annotation.Make(Annotations => $3.List));
        end if;

	$$ := (Two_Lists, Locals, $5.List);
      end;
    }
  | INDENT local_class_element_list {
	yyerror("Missing ""exports"" keyword");
	$$ := (Two_Lists, Lists.Empty_List, $2.List);
    }
  ;

EXPORTS_kw_NEWLINE : EXPORTS_kw | EXPORTS_kw NEWLINE | EXPORTS_kw INDENT ;

local_class_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | local_class_element_list local_class_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  ;

local_class_element_with_term : 
    interface_element_with_term { $$ := $1; } 
  | operation_import SEMI_or_NEWLINE { $$ := $1; }
  | operation_equiv SEMI_or_NEWLINE { $$ := $1; }
  | annotated_exported_class_element_with_term { $$ := $1; }
  ;
  
exported_class_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | exported_class_element_list annotated_exported_class_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_class_element_list operation_import SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_class_element_list operation_equiv SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_class_element_list interface_element_with_term {
	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => $2);
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_class_element_list error SEMI_or_NEWLINE {
	$$ := $1;
    }
  ;

annotated_exported_class_element_with_term : 
    exported_class_element_with_term { $$ := $1; }
  | annotation_with_term { 
	$$ := (One_Tree, Annotation.Make(Annotations => $1.List));
    }
  | annotation exported_class_element_with_term {
	$$ := $2;
	Annotation.Add_Annotation($$.Tree, $1.List, Precedes => True);
    }
  ;

exported_class_element_with_term : 
    operation_definition_with_term  { $$ := $1; }
  | object_definition_with_term  { $$ := $1; }
  | class_definition_with_term  { $$ := $1; }
  ;
  
   
annotation_opt_term : annotation { $$ := $1; }
  | annotation_with_term { $$ := $1; }
  | annotation_with_term annotation {
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  ;

annotation_with_term :
    annotation NEWLINE { $$ := $1; }
  | annotation_with_term annotation NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  ;

annotation : L_ASSERT annotation_element_list R_ASSERT { $$ := $2; }
  | annotation L_ASSERT annotation_element_list R_ASSERT {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

annotation_element_list :
    unlabeled_annotation_element_list {
        $$ := $1;
    }
  | label unlabeled_annotation_element_list {
        --  A labeled annotation list becomes a separate nested annotation
        $$ := (One_List, Lists.Make
                 ((1 => Annotation.Make
                   (Annotations => $2.List, Label => $1.Tree))));
    }
  ;


unlabeled_annotation_element_list : 
    annotation_element {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | unlabeled_annotation_element_list ';' annotation_element {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  | unlabeled_annotation_element_list ';' error {
	$$ := $1;
    }
  ;

annotation_element : 
    simple_interface_element { $$ := $1; }
  | operation_import { $$ := $1; }
  | operation_equiv { $$ := $1; }
  | condition { $$ := $1; }
  | quantified_expression { $$ := $1; }
  | id REFERS_TO expression {
        --  An annotation of the form <property_id> => <expression>
        --  is used to associate a property with a declaration,
        --  such as "convention => #c," similar to an Ada "aspect
        --  specification."
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | annotation {
	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	$$ := (One_Tree, Annotation.Make(Annotations => $1.List));
    }
  ;

condition : expression { $$ := $1; } ;

operation_declaration : 
    op_declaration { $$ := $1; }
  | func_declaration { $$ := $1; }
  ;

op_declaration :
    opt_ABSTRACT_or_OPTIONAL_or_QUEUED_kw OP_kw operator_designator 
      operation_inputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $3.Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => $4.Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => $4.Global_Read_List,
          Global_Update_List => $4.Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make ($5.List),
	  Is_Abstract => $1.Is_Abstract,
	  Is_Optional => $1.Is_Optional,
	  Is_Queued => $1.Is_Queued,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($2));
    }
  | opt_ABSTRACT_or_OPTIONAL_or_QUEUED_kw OP_kw operator_designator 
      operation_inputs opt_annotation
      GIVES_or_RETURN_kw operation_outputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $3.Tree,
	  Operation_Kind => Operation.Op_Operation,
	  Operation_Inputs => $4.Inputs_List,
	  Operation_Outputs => $7.List,
          Global_Read_List => $4.Global_Read_List,
          Global_Update_List => $4.Global_Update_List,
	  Preconditions => Annotation.Make ($5.List),
	  Postconditions => Annotation.Make ($8.List),
	  Is_Abstract => $1.Is_Abstract,
	  Is_Optional => $1.Is_Optional,
	  Is_Queued => $1.Is_Queued,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($2));
    }
  ;

opt_ABSTRACT_or_OPT_or_OPTIONAL_kw :
    ABSTRACT_kw { 
	$$ := (Construct_Qualifier,
               Source_Pos => $1.Source_Pos,
               Is_Abstract => True, others => False); 
    }
  | OPT_or_OPTIONAL_kw { 
	$$ := (Construct_Qualifier,
               Source_Pos => $1.Source_Pos,
               Is_Optional => True, others => False); 
    }
  | { $$ := (Construct_Qualifier,
             Source_Pos => PSC.Source_Positions.Null_Source_Position,
             others => False); }
  ;

opt_ABSTRACT_or_OPTIONAL_or_QUEUED_kw :
    ABSTRACT_kw opt_QUEUED_kw { 
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Abstract => True, Is_Queued => $2.Is_Present, others => False); 
    }
  | OPT_or_OPTIONAL_kw opt_QUEUED_kw { 
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Optional => True, Is_Queued => $2.Is_Present, others => False); 
    }
  | opt_QUEUED_kw { 
	$$ := (Construct_Qualifier, 
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
	  Is_Queued => $1.Is_Present, others => False); 
    }
  ;

opt_QUEUED_kw :
    QUEUED_kw {
	$$ := (Optional, Is_Present => True);
    }
  | {
	$$ := (Optional, Is_Present => False);
    }
  ;

operator_designator : 
    String_Literal { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | id {
	yyerror("Operator designator must be in quotes");
	$$ := $1;
    }
  ;
  
GIVES_or_RETURN_kw : GIVES
  | RETURN_kw {
	yyerror("Use ""->"" in ParaSail rather than ""return""");
    }
  ;

func_declaration :
    opt_ABSTRACT_or_OPTIONAL_or_QUEUED_kw basic_func_declaration {
	$$ := $2;
	declare
	    Func_Tree : Operation.Tree renames 
	      Operation.Tree(Tree_Ptr_Of($$.Tree).all);
	begin
	    Func_Tree.Is_Abstract := $1.Is_Abstract;
	    Func_Tree.Is_Optional := $1.Is_Optional;
	    Func_Tree.Is_Queued := $1.Is_Queued;
	end;
    }
  ;

basic_func_declaration :
    FUNC_kw func_designator operation_inputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $2.Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => $3.Inputs_List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => $3.Global_Read_List,
          Global_Update_List => $3.Global_Update_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make ($4.List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($1));
    }
  | FUNC_kw func_designator operation_inputs 
      opt_annotation GIVES_or_RETURN_kw operation_outputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $2.Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => $3.Inputs_List,
	  Operation_Outputs => $6.List,
          Global_Read_List => $3.Global_Read_List,
          Global_Update_List => $3.Global_Update_List,
	  Preconditions => Annotation.Make ($4.List),
	  Postconditions => Annotation.Make ($7.List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($1));
    }
  ;

func_designator : 
    qualified_name {
        if Qualified_Name.Contains_String ($1.Tree) then
            yyerror("Should use ""op"" rather than ""func"" for an operator");
        end if;
	$$ := $1;
    }
  ;
  
operation_inputs :
    simple_operation_input { 
	$$ := (Input_And_Global_Lists,
          Inputs_List => Lists.Make((1 => $1.Tree)),
          others => Lists.Empty_List);
    }
  | '(' opt_annotated_operation_input_list right_paren {
	$$ := $2;
    }
  | '(' id_list ',' id right_paren {
      declare
	Id_List : Lists.List := $2.List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => $3);
	$$ := (Input_And_Global_Lists, others => Lists.Empty_List);
	Lists.Append(Id_List, $4.Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append($$.Inputs_List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    }
  | {
	yyerror("ParaSail requires at least ""()"" in operation definition");
	$$ := (Input_And_Global_Lists, others => Lists.Empty_List);
    }
  ;

right_paren :
    ')' { $$ := $1; }
  | NEWLINE ')' { $$ := $2; }
  | error {
        yyerror ("Expecting one ')'", At_Token => $1);
        $$ := (One_Token,
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.String_Lookup(")")); 
    }
  ;

simple_operation_input :   -- avoids trailing use of "IS"
    opt_input_mode id ':'
      opt_input_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => $2.Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $4.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $4.Is_Optional, others => False),
             Operand => $5.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | opt_input_mode input_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $2.Is_Optional, others => False),
             Operand => $3.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | opt_input_mode simple_operand_type_specifier { 
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => $2.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  ;

opt_input_mode : 
    input_mode { $$ := $1; }
  | {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  ;

input_mode :
    output_mode { $$ := $1; }
  | QUEUED_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    }
  | QUEUED_kw VAR_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Queued_Param);
    }
  | QUEUED_kw output_mode {
	$$ := (Param_Mode, 
	  Param_Kind => $2.Param_Kind,
	  Param_Locking => Param_Decl.Queued_Param);
    }
  | LOCKED_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    }
  | LOCKED_kw VAR_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Locked_Param);
    }
  | LOCKED_kw output_mode {
	$$ := (Param_Mode, 
	  Param_Kind => $2.Param_Kind,
	  Param_Locking => Param_Decl.Locked_Param);
    }
  | VAR_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  ;

opt_output_mode :
    output_mode { $$ := $1; }
  | {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  ;

output_mode :
    REF_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | REF_kw CONST_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | REF_kw VAR_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  ;

opt_annotated_operation_input_list : 
    annotated_operation_input_list {
      $$ := (Input_And_Global_Lists,
             Inputs_List => $1.List, others => Lists.Empty_List);
    }
  | annotated_operation_input_list ';' global_access_list {
        $$ := $3;
        $$.Inputs_List := $1.List;
    }
  | global_access_list {
        $$ := $1;
    }
  | {
	$$ := (Input_And_Global_Lists, others => Lists.Empty_List);
    }
  ;

annotated_operation_input_list : 
    annotated_operation_input { $$ := $1; }
  | annotated_operation_input_list ';' annotated_operation_input {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

annotated_operation_input : 
    operation_input opt_annotation {
        $$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotations to last element of list
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $2.List);
	end if;
    }
  | annotation operation_input opt_annotation {
	-- Add annotations to first/last element of list
	$$ := $2;
	Annotation.Add_Annotation(
	  Lists.Nth_Element($$.List, 1), $1.List, Precedes => True);
	if not Lists.Is_Empty($3.List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List,
                Lists.Length($$.List)), $3.List);
	end if;
    }
  ;

operation_input : 
    id_list COLON_no_indent
      opt_input_type_qualifier operand_type_specifier
      opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => $3.Is_Optional,
              In_Region => Copy_If_Not_First ($5.Tree, I),
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => $3.Is_Optional, others => False),
                 Operand => Copy_If_Not_First ($4.Tree, I)),
	      Param_Default => Copy_If_Not_First ($6.Tree, I)));
	end loop;
    }
  | input_mode id_list COLON_no_indent
      opt_input_type_qualifier operand_type_specifier
      opt_region_specifier opt_ASSIGN_expression{
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($2.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($2.List, I),
	      Kind => $1.Param_Kind,
	      Locking => $1.Param_Locking,
	      Is_Optional => $4.Is_Optional,
              In_Region => Copy_If_Not_First ($6.Tree, I),
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => $4.Is_Optional, others => False),
                 Operand => Copy_If_Not_First ($5.Tree, I)),
	      Param_Default => Copy_If_Not_First ($7.Tree, I)));
	end loop;
    }
  | operand_type_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => $1.Tree,
	  Param_Default => $2.Tree))));
    }
  | input_mode input_type_qualifier
      operand_type_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $2.Is_Optional, others => False),
             Operand => $3.Tree),
	  Param_Default => $4.Tree))));
    }
  | input_mode operand_type_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => False,
	  Param_Type => $2.Tree,
	  Param_Default => $3.Tree))));
    }
  | input_type_qualifier
      operand_type_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => $1.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $1.Is_Optional, others => False),
             Operand => $2.Tree),
	  Param_Default => $3.Tree))));
    }
  | '<' opt_input_mode value_formal '>' {
	$$ := $3;
	-- Set Is_Implicit_Module_Param and input-mode info on each parameter
	for I in 1..Lists.Length($$.List) loop
	  declare
	    Param_Decl_Tree : Param_Decl.Tree renames
	      Param_Decl.Tree(Tree_Ptr_Of(Lists.Nth_Element($$.List, I)).all);
	  begin
	    Param_Decl_Tree.Is_Implicit_Module_Param := True;
	    Param_Decl_Tree.Kind := $2.Param_Kind;
	    Param_Decl_Tree.Locking := $2.Param_Locking;
	  end;
	end loop;
    }
  ;

opt_input_type_qualifier : 
    input_type_qualifier { $$ := $1; }
  | {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;
  
simple_operand_type_specifier :
    type_name { $$ := $1; }
  | module_instantiation { $$ := $1; }
  ;

operand_type_specifier : 
    simple_operand_type_specifier { $$ := $1; }
  | id IS_no_indent module_instantiation {
         -- NOTE: Operation can have "type" parameters 
         -- such as "Left_Type is Integer<>"
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $1.Tree,
	  Is_New_Type => False,
	  Type_Definition => $3.Tree));
    }
  | func_type_specifier {
        $$ := $1;
    }
  ;

input_type_qualifier : 
    OPT_or_OPTIONAL_kw {
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Optional => True,
	  others => False);
    }
  ;

opt_output_type_qualifier :
    output_type_qualifier { $$ := $1; }
  | {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;

output_type_qualifier :
    OPT_or_OPTIONAL_kw {
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Optional => True,
	  others => False);
    }
  ;

operation_outputs : 
    simple_operation_output {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | annotation simple_operation_output {
	Annotation.Add_Annotation($2.Tree, $1.List, Precedes => True);
	$$ := (One_List, Lists.Make((1 => $2.Tree)));
    }
  | '(' annotated_operation_output_list right_paren {
	$$ := $2;
    }
  | '(' output_mode id_list ',' id right_paren {
      declare
	Id_List : Lists.List := $3.List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => $3);
	$$ := (One_List, Lists.Empty_List);
	Lists.Append(Id_List, $5.Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    }
  | '(' id_list ',' id right_paren {
      declare
	Id_List : Lists.List := $2.List;
      begin
	yyerror("Parameter types must be separated by "";""",
          At_Token => $3);
	$$ := (One_List, Lists.Empty_List);
	Lists.Append(Id_List, $4.Tree);
	for I in 1..Lists.Length(Id_List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Null_Optional_Tree,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Lists.Nth_Element(Id_List, I),
	      Param_Default => Null_Optional_Tree));
	end loop;
      end;
    }
  ;

simple_operation_output :   -- avoids trailing use of "IS"
    output_mode id ':' opt_output_type_qualifier 
      simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => $2.Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $4.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $4.Is_Optional, others => False),
             Operand => $5.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | output_mode output_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $2.Is_Optional, others => False),
             Operand => $3.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | output_mode simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => False,
	  Param_Type => $2.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  | id ':'
      opt_output_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => $1.Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => $3.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $3.Is_Optional, others => False),
             Operand => $4.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | output_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => $1.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $1.Is_Optional, others => False),
             Operand => $2.Tree),
	  Param_Default => Null_Optional_Tree));
    }
  | simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => $1.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  ;

annotated_operation_output_list :
    annotated_operation_output {
	$$ := $1;
    }
  | annotated_operation_output_list ';' annotated_operation_output {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

annotated_operation_output : 
    operation_output opt_annotation {
	if not Lists.Is_Empty($2.List) then
	    -- Add annotations to last element of list
	    $$ := $1;
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $2.List);
	else
	    $$ := $1;
	end if;
    }
  | annotation operation_output opt_annotation  {
	-- Add annotations to first/last element of list
	$$ := $2;
	if not Lists.Is_Empty($1.List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, 1), $1.List, Precedes => True);
	end if;
	if not Lists.Is_Empty($3.List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $3.List);
	end if;
    }
  ;

operation_output : 
    id_list COLON_no_indent
      opt_output_type_qualifier operand_type_specifier {
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => $3.Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => $3.Is_Optional, others => False),
                 Operand => Copy_If_Not_First ($4.Tree, I)),
	      Param_Default => Null_Optional_Tree));
	end loop;
    }
  | output_mode id_list COLON_no_indent
      opt_output_type_qualifier 
      operand_type_specifier {
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($2.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($2.List, I),
	      Kind => $1.Param_Kind,
	      Locking => $1.Param_Locking,
	      Is_Optional => $4.Is_Optional,
	      Param_Type => Qualifier.Qualify
                (Qualifiers =>
                  (Is_Optional => $4.Is_Optional, others => False),
                 Operand => Copy_If_Not_First ($5.Tree, I)),
	      Param_Default => Null_Optional_Tree));
	end loop;
    }
  | operand_type_specifier {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => $1.Tree,
	  Param_Default => Null_Optional_Tree))));
    }
  | output_mode output_type_qualifier operand_type_specifier {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $2.Is_Optional, others => False),
             Operand => $3.Tree),
	  Param_Default => Null_Optional_Tree))));
    }
  | output_mode operand_type_specifier {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => False,
	  Param_Type => $2.Tree,
	  Param_Default => Null_Optional_Tree))));
    }
  | output_type_qualifier operand_type_specifier {
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => $1.Is_Optional,
          Param_Type => Qualifier.Qualify
            (Qualifiers =>
              (Is_Optional => $1.Is_Optional, others => False),
             Operand => $2.Tree),
	  Param_Default => Null_Optional_Tree))));
    }
  ;

object_declaration : 
    VAR_kw id COLON_no_indent annotated_type_specifier 
      opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => $5.Tree,
	  Obj_Type => $4.Tree,
	  Obj_Value => $6.Tree));
    }
  | CONST_kw id COLON_no_indent annotated_type_specifier 
      opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => $5.Tree,
	  Obj_Type => $4.Tree,
	  Obj_Value => $6.Tree));
    }
  | CONST_kw id opt_region_specifier ASSIGN_or_equal expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
          In_Region => $3.Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree));
    }
  | REF_kw VAR_kw id COLON_no_indent
      annotated_type_specifier 
      opt_REFERS_TO_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => $5.Tree,
	  Obj_Value => $6.Tree));
    }
  | REF_kw CONST_kw id COLON_no_indent
      annotated_type_specifier 
      opt_REFERS_TO_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => $5.Tree,
	  Obj_Value => $6.Tree));
    }
  | REF_kw id COLON_no_indent
      annotated_type_specifier 
      opt_REFERS_TO_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => $4.Tree,
	  Obj_Value => $5.Tree));
    }
  | GLOBAL_kw VAR_kw id COLON_no_indent annotated_type_specifier 
      ASSIGN_or_equal expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => Null_Optional_Tree,
	  Obj_Type => $5.Tree,
	  Obj_Value => $7.Tree,
          Is_Global => True));
    }
  | GLOBAL_kw VAR_kw id COLON_no_indent 
      ASSIGN_or_equal expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => Null_Optional_Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $6.Tree,
          Is_Global => True));
    }
  | id COLON_no_indent
      annotated_type_specifier 
      opt_ASSIGN_expression {
	yyerror("Must specify ""var,"" ""const,"" or ""ref""",
          At_Token => $1);
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($1.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Obj_Type => $3.Tree,
	  Obj_Value => $4.Tree));
    }
  ;

opt_region_specifier :
    FOR_kw name { $$ := $2; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;
	
opt_ASSIGN_expression : 
    ASSIGN_or_equal expression { $$ := $2; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;
   
opt_REFERS_TO_expression :
    REFERS_TO expression { $$ := $2; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

object_definition_with_term : object_definition SEMI_or_NEWLINE { $$ := $1; };

object_definition :
    VAR_kw id opt_region_specifier ASSIGN_or_equal expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => $3.Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree));
    }
  | REF_kw CONST_kw id REFERS_TO name {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree));
    }
  | REF_kw VAR_kw id REFERS_TO name {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree));
    }
  | REF_kw id REFERS_TO name {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree));
    }
  | CONST_kw id MOVE movable_object {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree));
    }
  | VAR_kw id MOVE movable_object {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree));
    }
  ;

OPT_or_OPTIONAL_kw : OPT_kw { $$ := $1; }
  | OPTIONAL_kw { $$ := $1; }
  ;

movable_object : expression { $$ := $1; };

type_declaration : 
    TYPE_kw id IS_no_indent opt_NEW_kw annotated_type_specifier {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => $4.Is_Present,
	  Type_Definition => $5.Tree));
    };

opt_NEW_kw : 
    NEW_kw {
	$$ := (Optional, True);
    }
  | {
	$$ := (Optional, False);
    }
  ;

operation_definition_with_term : 
    op_definition_with_term { $$ := $1; }
  | func_definition_with_term { $$ := $1; }
  ;

op_definition_with_term : 
  op_declaration IS_kw_INDENT
     statement_list_opt_term 
  OUTDENT_opt_NEWLINE opt_END_OP {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);

            if $5.Check_Label then
	        Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $5.Label);
            end if;
	end;
    }
  | op_declaration IS_kw
     unindented_statement_list_opt_term 
  END_kw opt_OP_kw operator_designator SEMI_or_NEWLINE {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => $3);
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $4.Source_Pos);
	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => $6.Tree);
	end;
    }
 | op_declaration opterr_COLON_INDENT
     statement_list_opt_term 
  OUTDENT_opt_NEWLINE opt_END_OP {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);

            if $5.Check_Label then
	        Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $5.Label);
            end if;
	end;
    }
  ;

func_definition_with_term : 
   func_declaration IS_kw_INDENT
     statement_list_opt_term 
   OUTDENT_opt_NEWLINE opt_END_FUNC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);

            if $5.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $5.Label);
            end if;
	end;
    }
  | func_declaration IS_kw_INDENT queued_clause_THEN
     statement_list_opt_term 
    OUTDENT_opt_NEWLINE opt_END_FUNC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := $3.Tree;
	    Op_Decl.Statements := $4.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $6.Source_Pos);

            if $6.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $6.Label);
            end if;
	end;
    }
 | func_declaration IS_kw
     unindented_statement_list_opt_term 
   END_kw opt_FUNC_kw func_designator SEMI_or_NEWLINE {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => $3);
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $4.Source_Pos);

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => $6.Tree);
	end;
    }
  | func_declaration IS_kw queued_clause_THEN
     unindented_statement_list_opt_term 
    OUTDENT_END_kw opt_FUNC_kw func_designator SEMI_or_NEWLINE {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => $4);
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := $3.Tree;
	    Op_Decl.Statements := $4.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => $7.Tree);
	end;
    }
 | func_declaration opterr_COLON_INDENT
     statement_list_opt_term 
   OUTDENT_opt_NEWLINE opt_END_FUNC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $5.Source_Pos);

            if $5.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $5.Label);
            end if;
	end;
    }
  | func_declaration opterr_COLON_INDENT queued_clause_COLON
     statement_list_opt_term 
    OUTDENT_opt_NEWLINE opt_END_FUNC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := $3.Tree;
	    Op_Decl.Statements := $4.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));
            Set_End_Source_Pos ($$.Tree, $6.Source_Pos);

            if $6.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $6.Label);
            end if;
	end;
    }
  ;

opt_OP_kw : OP_kw
  | {
	yyerror("Should be 'end op ""id""' not simply 'end ""id""'");
    }
  ;

opt_FUNC_kw : FUNC_kw
  | {
	yyerror("Should be ""end func <id>"" not simply ""end <id>""");
    }
  ;

opt_queued_clause : 
    queued_clause { $$ := $1; }
  | { $$ := (One_Tree, Null_Optional_Tree); }
  ;

queued_clause : 
    queued_clause_THEN { $$ := $1; }
  | queued_clause_COLON { $$ := $1; }
  ;

queued_clause_THEN : queued_clause_condition THEN_kw_INDENT { $$ := $1; };

queued_clause_COLON : queued_clause_condition opterr_COLON_INDENT { $$ := $1; };

queued_clause_condition :
    QUEUED_kw WHILE_or_UNTIL_kw pop_indent_stack 
      condition {
	$$ := (One_Tree, Conditionally_Complement(
	  $4.Tree,
	  Complement => $2.Is_While));  
	    -- Complement cond if "while" present
	Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
    };

pop_indent_stack : { 
        --  Pop the indent stack
        if ParaSail_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Top := ParaSail_Lex.Top - 1;
    };

opterr_COLON_INDENT :
    COLON_INDENT
  | error COLON_INDENT {
        yyerror ("Syntax error before ':'", At_Token => $2);
    }
  ;

COLON_INDENT : EOL_COLON INDENT ;

THEN_kw_INDENT : THEN_kw INDENT | NEWLINE THEN_kw INDENT ;

COLON_or_THEN_kw_INDENT :
    COLON_INDENT   { $$ := $1; }
  | THEN_kw_INDENT { $$ := $1; }
  | ')' COLON_or_THEN_kw_INDENT {
        yyerror("Extra ')'", At_Token => $1);
        $$ := $2;
    }
  | error COLON_or_THEN_kw_INDENT {
        yyerror("Syntax error in condition", At_Token => $2);
        $$ := $2;
    }
  ;

opt_END_FUNC :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw opt_FUNC_kw func_designator SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;

opt_END_OP :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw opt_OP_kw operator_designator SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;

import_operation :
    IMPORT_kw '(' opt_operation_actual_list ')' {
	$$ := $3;
    };

unindented_statement_list_opt_term :  --  only allows simple statements
    unindented_statement_list { $$ := $1; }
  | unindented_statement_list_with_term { $$ := $1; }
  ;

unindented_statement_list :
    annotated_simple_statement { $$ := $1; }
  | unindented_statement_list_with_term annotated_simple_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

unindented_statement_list_with_term :
    annotated_simple_statement_with_term { $$ := $1; }
  | unindented_statement_list_with_term annotated_simple_statement_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

statement_list_opt_term : 
    statement_list_with_term { $$ := $1; }
  | statement_list_no_term { $$ := $1; }
  ;

statement_list_with_term :
    parallel_sequence_with_term { $$ := $1; }
  | statement_list_no_term THEN_no_indent_NL parallel_sequence_with_term {
	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | statement_list_with_term THEN_no_indent_NL parallel_sequence_with_term {
	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | statement_list_with_term use_BEGIN_kw 
    parallel_sequence_with_term {
	-- "begin" is not used in ParaSail; treat like "then" for now
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  | use_BEGIN_kw parallel_sequence_with_term {
	-- "begin" is not used in ParaSail
	$$ := $2;
    }
  ;

THEN_no_indent : THEN_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
        $$ := $1;
    };

THEN_no_indent_NL : THEN_no_indent {
        $$ := $1;
    }
  | THEN_no_indent NEWLINE {
        $$ := $1;
    };

ELSE_opt_COLON_no_indent : ELSE_no_INDENT | ELSE_kw COLON_no_INDENT ;

ELSE_no_indent : ELSE_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    };

use_BEGIN_kw : BEGIN_kw {
	yyerror("No need for ""begin"" in ParaSail operation definition");
    };


statement_list_no_term : 
    parallel_sequence_no_term { $$ := $1; }
  | statement_list_no_term THEN_no_indent_NL parallel_sequence_no_term {
	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  | statement_list_with_term THEN_no_indent_NL parallel_sequence_no_term {
	-- "then" forces sequential processing; it has lower precedence
	-- than "||" so declarations preceding "then" are visible to both
	-- sides of the "||".
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  ;

parallel_sequence_with_term : 
    statement_sequence_with_term { 
	$$ := $1; 
    }
  | parallel_sequence_no_term PARALLEL_opt_NL statement_sequence_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | parallel_sequence_with_term PARALLEL_opt_NL statement_sequence_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

PARALLEL_opt_NL : PARALLEL {
        $$ := $1;
    }
  | PARALLEL NEWLINE {
        $$ := $1;
    };

parallel_sequence_no_term :
    statement_sequence { 
	$$ := $1; 
    }
  | parallel_sequence_no_term PARALLEL_opt_NL statement_sequence {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | parallel_sequence_with_term PARALLEL_opt_NL statement_sequence {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Parallel_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

statement_sequence_with_term :
    annotated_statement_with_term { $$ := $1; }
  | statement_sequence_with_term annotated_statement_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

statement_sequence :
    annotated_simple_statement { $$ := $1; }
  | statement_sequence_with_term annotated_simple_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;
  
annotated_statement_with_term : 
    annotated_simple_statement_with_term {
        $$ := $1;
    }
  | annotated_compound_statement_with_term {
        $$ := $1;
    }
  ;

annotated_simple_statement_with_term : 
    annotated_simple_statement SEMI_or_NEWLINE {
        $$ := $1;
    }
  ;

annotated_simple_statement : 
    annotation simple_statement opt_annotation {
	$$ := $2;
	Annotation.Add_Annotation($$.Tree, $1.List, Precedes => True);
	Annotation.Add_Annotation($$.Tree, $3.List);
    }
  | simple_statement opt_annotation {
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  | annotation {
	-- An annotation can appear by itself
	$$ := (One_Tree, Annotation.Make(Annotations => $1.List));
    }
  ;

annotated_compound_statement_with_term : 
    annotation local_declaration_with_term {
            -- NOTE: these already allow trailing annotations
	$$ := $2;
	Annotation.Add_Annotation($$.Tree, $1.List, Precedes => True);
    }
  | annotation opt_label_compound_statement_with_term {
	$$ := $2;
	Annotation.Add_Annotation($$.Tree, $1.List, Precedes => True);
    }
  | local_declaration_with_term { $$ := $1; }
            -- NOTE: these already allow trailing annotations
  | opt_label_compound_statement_with_term {
	$$ := $1;
    }
  ;

local_declaration_with_term :
    local_declaration SEMI_or_NEWLINE { $$ := $1; };

opt_label_compound_statement_with_term : 
    local_definition_with_term { $$ := $1; }
  | label_opt_NL compound_statement_with_term { 
	if Not_Null($2.Tree) then
	    -- Stmt might be null if there was an error.
	    Check_Stmt_Label(
	      Compound_Stmt => $2, Start_Label_Token => $1);
	end if;

	$$ := $2;
    }
  | compound_statement_with_term { 
	if Not_Null($1.Tree) then
	    -- Stmt might be null if there was an error.
	    Check_Stmt_Label(
	      Compound_Stmt => $1,
                Start_Label_Token => (Optional, Is_Present => False));
	end if;

	$$ := $1; 
    }
  ;

simple_statement :
    primitive_statement {
	$$ := $1;
  }
  | name equal_as_assign expression {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | NULL_kw { 
	-- A "null" statement (i.e. a no-op)
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Null_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | name '(' opt_operation_actual_list ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => $1.Tree,
	  Operands => $3.List));
    }
  | return_stmt { $$ := $1; }
  | CONTINUE_kw opt_LOOP_no_indent opt_id opt_WITH_values  {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Continue_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => $3.Tree,
	  Values => $4.Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | EXIT_kw opt_compound_statement_kind opt_id opt_WITH_values {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Exit_Stmt,
	  Applies_To => $2.Exitable_Construct,
	  Id => $3.Tree,
	  Values => $4.Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | simple_statement ')' {
        yyerror ("Extra ')'", At_Token => $2);
        $$ := $1;
    }
    
  ;

LOOP_no_indent : LOOP_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    };

return_stmt :
     RETURN_kw expression {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => $2.Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | RETURN_kw opt_WITH_values  {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => $2.Tree,
	  Source_Pos => $1.Source_Pos));
    }
  ;

opt_LOOP_no_indent : 
    LOOP_no_indent { $$ := $1; }
  | {
	yyerror("""loop"" required after ""continue""");
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

primitive_statement :
    name assign_operator_not_divide expression {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => $2.Assign_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | name DIVIDE_ASSIGN expression {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Divide_Assign_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | name COMBINE_MOVE movable_object {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Combine_Move_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | name MOVE movable_object {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | name SWAP name {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Swap_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  | '(' opt_class_component_list ')' ASSIGN expression {
	-- multiple assignment 
	-- NOTE: Using "opt_operation_actual_list" rather 
	--       than "operation_actual_list" to avoid ambiguity
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Assign_Op,
	  LHS => Invocation.Make(
	    Kind => Invocation.Class_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => $2.List,
	    Source_Pos => $1.Source_Pos),
	  RHS => $5.Tree));
    }
  ;

opt_operation_actual_list : 
    operation_actual_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

opt_WITH_values : 
    WITH_values { $$ := $1; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

WITH_values : WITH_kw operation_actual {
	-- NOTE: This used to be '(' operation_actual_list ')'
	--       but that prevented continuing with a single expression.
	$$ := $2;
    };

opt_id : 
    id { $$ := $1; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

opt_compound_statement_kind :
    compound_statement_kind { $$ := $1; }
  | { 
	yyerror(
	  """loop,"" ""if,"" ""case,"" or ""block"" must follow ""exit""");
	$$ := (Construct_Kind, Control_Stmt.Loop_Stmt);
    }
  ;
	
compound_statement_kind : 
    LOOP_no_indent {
	$$ := (Construct_Kind, Control_Stmt.Loop_Stmt);
    }
  | IF_kw {
	$$ := (Construct_Kind, Control_Stmt.If_Stmt);
    }
  | CASE_kw {
	$$ := (Construct_Kind, Control_Stmt.Case_Stmt);
    }
  | BLOCK_no_indent {
	$$ := (Construct_Kind, Control_Stmt.Block_Stmt);
    }
  ;

BLOCK_no_indent : BLOCK_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    };

local_declaration : 
    operation_declaration { $$ := $1; }
  | type_declaration { $$ := $1; }
  | object_declaration{ $$ := $1; }
  ;

local_definition_with_term :
    object_definition_with_term { $$ := $1; }
  | operation_definition_with_term { $$ := $1; }
  | operation_equiv SEMI_or_NEWLINE { $$ := $1; }
  ;

label_opt_NL : label { $$ := $1; }
  | label NEWLINE { $$ := $1; }
  | label INDENT { $$ := $1; }
  ;

label : '*' id_or_string_literal '*' { $$ := $2; };

compound_statement_with_term :
    if_statement { $$ := $1; }
  | case_statement { $$ := $1; }
  | indefinite_loop_statement { $$ := $1; }
  | while_until_loop_statement { $$ := $1; }
  | for_loop_statement { $$ := $1; }
  | block_statement  { $$ := $1; }
  | error SEMI_or_NEWLINE { $$ := (One_Tree, Null_Optional_Tree); }
  ;

if_statement : 
    IF_kw condition COLON_or_THEN_kw_INDENT
     statement_list_opt_term
  OUTDENT_opt_else_part opt_END_IF {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree,
	  End_With_Values => $6.End_With_Values,
	  Check_Label => $6.Check_Label,
          Label => $6.Label));
    }
  ;

OUTDENT_opt_else_part_END : 
    OUTDENT_ELSIF_kw condition THEN_kw_INDENT
      statement_list_opt_term 
    OUTDENT_opt_else_part_END {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
    }
  | OUTDENT_ELSE_kw_INDENT statement_list_opt_term OUTDENT_END_kw {
	$$ := $2;
    }
  | OUTDENT_END_kw {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

OUTDENT_opt_else_part : 
    OUTDENT_ELSIF_kw condition COLON_or_THEN_kw_INDENT
      statement_list_opt_term 
    OUTDENT_opt_else_part {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
    }
  | OUTDENT_ELSE_kw_opt_COLON_INDENT statement_list_opt_term
      OUTDENT_opt_NEWLINE {
	$$ := $2;
    }
  | OUTDENT_opt_NEWLINE {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

OUTDENT_ELSIF_kw : OUTDENT NEWLINE ELSIF_kw { $$ := $3; };

OUTDENT_ELSE_kw_INDENT :
    OUTDENT NEWLINE ELSE_kw INDENT ;

OUTDENT_ELSE_kw_opt_COLON_INDENT :
    OUTDENT NEWLINE ELSE_kw opt_COLON_INDENT ;

opt_COLON_INDENT : COLON_INDENT | INDENT ;

opt_END_IF :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw IF_kw opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

case_statement : 
    CASE_kw comparison_expression COLON_or_OF_kw_INDENT
      case_alt_list
      opt_default_alt
    OUTDENT_opt_NEWLINE opt_END_CASE {
	declare
	    Case_Alt_List : Lists.List := $4.List;
	begin
	    if Not_Null($5.Tree) then
		Lists.Append(Case_Alt_List, $5.Tree);
	    end if;
	    $$ := (One_Tree, Case_Construct.Make(
              Source_Pos => $1.Source_Pos,
	      Case_Selector => $2.Tree,
	      Case_Alt_List => Case_Alt_List,
	      End_With_Values => $7.End_With_Values,
	      Check_Label => $7.Check_Label,
              Label => $7.Label));
	end;
    }
  ;

OF_kw_INDENT : OF_kw INDENT | NEWLINE OF_kw INDENT ;

COLON_or_OF_kw_INDENT :
    COLON_INDENT { $$ := $1; }
  | OF_kw_INDENT { $$ := $1; }
  | IS_kw_INDENT {
        yyerror
          ("Use ""of"" rather than ""is"" for a case statement",
           At_Token => $1);
        $$ := $1;
    }
  | ')' COLON_or_OF_kw_INDENT {
        yyerror ("Extra ')'", At_Token => $1);
        $$ := $2;
    }
--   | error COLON_or_OF_kw_INDENT {
--         yyerror ("Syntax error in case selector", At_Token => $2);
--         $$ := $2;
--     }
  ;

opt_END_CASE :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw CASE_kw opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

case_alt_list : 
    case_alt {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | case_alt_list case_alt {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  ;

case_alt :
    '[' simple_expression_opt_named ']' REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $5.Tree));
    }
  | '[' simple_expression_opt_named REFERS_TO_with_indent
      indented_statement_list_with_term {
        yyerror("Missing ']'", At_Token => $3);
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $4.Tree));
    }
  ;

REFERS_TO_with_indent : REFERS_TO {
        if ParaSail_Lex.Debug_Indent then
           Text_IO.Put(" [indent on] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Bracketing_Token := REFERS_TO;
        ParaSail_Lex.Expecting_Indent := True;
        $$ := $1;
    };

simple_expression_opt_named :
    simple_type_specifier_or_expression { $$ := $1; }
  | id COLON_no_indent
      simple_type_specifier_or_expression { 
	$$ := (One_Tree, Param_Decl.Make(
          Name => $1.Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => $3.Tree,
          Param_Default => Null_Optional_Tree));
     }
  ;
  
opt_default_alt : 
    '[' dot_dot_opt_named ']' REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $5.Tree));
    }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

indented_statement_list_with_term :
    INDENT statement_list_with_term OUTDENT { $$ := $2; }
  | INDENT statement_list_with_term OUTDENT NEWLINE { $$ := $2; }
  | statement_list_with_term { $$ := $1; }
  ;

dot_dot_opt_named :
    dot_dot_as_interval { $$ := $1; }
  | id COLON_no_indent dot_dot_as_interval {
	$$ := (One_Tree, Param_Decl.Make(
          Name => $1.Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => $3.Tree,
          Param_Default => Null_Optional_Tree));
     }
  ;
	
dot_dot_as_interval : DOT_DOT {
	$$ := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree,
          Source_Pos => $1.Source_Pos));
     };


indefinite_loop_statement :
    LOOP_kw opt_COLON_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE opt_END_LOOP {
	$$ := (One_Tree, While_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  While_Cond => Null_Optional_Tree,
	  Loop_Body => $3.Tree,
	  End_With_Values => $5.End_With_Values,
          Check_Label => $5.Check_Label,
          Label => $5.Label));
    }
  ;

while_until_loop_statement :
    WHILE_or_UNTIL_kw condition COLON_or_LOOP_kw_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE opt_END_LOOP {
	$$ := (One_Tree, While_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  While_Cond => Conditionally_Complement($2.Tree,
	    Complement => $1.Is_Until),
	  Loop_Body => $4.Tree,
	  End_With_Values => $6.End_With_Values,
          Check_Label => $6.Check_Label,
          Label => $6.Label));
    }
  ;

WHILE_or_UNTIL_kw :
    WHILE_kw { $$ := (Construct_Qualifier,
                      Source_Pos => $1.Source_Pos,
                      Is_While => True, others => False); }
  | UNTIL_kw { $$ := (Construct_Qualifier,
                      Source_Pos => $1.Source_Pos,
                      Is_Until => True, others => False); }
  ;

LOOP_kw_INDENT : LOOP_kw INDENT | NEWLINE LOOP_kw INDENT ;

COLON_or_LOOP_kw_INDENT :
    COLON_INDENT   { $$ := $1; ParaSail_Lex.Inside_For_Header := False; }
  | LOOP_kw_INDENT { $$ := $1; ParaSail_Lex.Inside_For_Header := False; }
  | ')' COLON_or_LOOP_kw_INDENT {
        yyerror ("Extra ')'", At_Token => $1);
        $$ := $2;
        ParaSail_Lex.Inside_For_Header := False;
    }
  | error COLON_or_LOOP_kw_INDENT {
        yyerror ("Syntax error in loop header", At_Token => $2);
        $$ := $2;
        ParaSail_Lex.Inside_For_Header := False;
    }
  ;

opt_END_LOOP :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw LOOP_no_indent opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;


for_loop_statement :
    FOR_kw_set_flag iterator_spec opt_annotation opt_direction
        COLON_or_LOOP_kw_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE opt_END_LOOP {
	$$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
	  Kind => For_Loop_Construct.For_Loop_Statement,
	  Iterators => $2.List,
	  Filter => $3.List,
	  Loop_Body => $6.Tree,
	  Direction => $4.Str,
	  End_With_Values => $8.End_With_Values,
          Check_Label => $8.Check_Label,
          Label => $8.Label));
    }
  ;

FOR_kw_set_flag : FOR_kw {
        ParaSail_Lex.Inside_For_Header := True;  $$ := $1;
    }
    ;

iterator_spec : 
    iterator {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | '(' iterator_list ')' { $$ := $2; }
  ;

iterator_list :
    iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := $1.Tree;
	begin
	    if $2.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $2.Str);
	    end if;
	    $$ := (One_List, Lists.Make((1 => Iterator_Tree)));
	end;
    }
  | iterator_list ';' iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : Optional_Tree := $3.Tree;
	begin
	    if $4.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $4.Str);
	    end if;
	    $$ := $1;
	    Lists.Append($$.List, Iterator_Tree);
	end;
    }
  | iterator_list ',' iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := $3.Tree;
	begin
	    yyerror("Iterators must be separated by "";""",
              At_Token => $2);
	    if $4.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $4.Str);
	    end if;
	    $$ := $1;
	    Lists.Append($$.List, Iterator_Tree);
	end;
    }
  ;

opt_forward_or_reverse :
    forward_or_reverse { $$ := $1; }
  | {
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

iterator :
    index_set_iterator { $$ := $1; }
  | ALL_kw index_set_iterator { 
	yyerror("Use ""for ..."" or ""for each ..."" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => $1);
	$$ := $2; 
    }
  | EACH_kw element_iterator { $$ := $2; }
  | EACH_kw index_set_iterator { 
	yyerror("""for-each"" iterator uses ""of"" rather than ""in""",
          At_Token => $1);
	$$ := $2; 
    }
  | ALL_kw element_iterator { 
	yyerror("Use ""for each ..."" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => $1);
	$$ := $2; 
    }
  | element_iterator { 
	yyerror("Missing ""each"" in container element ""for-each"" iterator",
          At_Token => $1);
	$$ := $1; 
    }
  | initial_next_value_iterator { $$ := $1; }
  | initial_value_iterator { $$ := $1; }
  ;

index_set_iterator :
    id opt_COLON_type_specifier IN_kw opt_REVERSE_kw expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => $1.Tree,
	  Is_Ref => False,
	  Obj_Type => $2.Tree,
	  Obj_Value => $5.Tree));
    };

opt_REVERSE_kw : 
  | REVERSE_kw {
	yyerror("The ""reverse"" keyword goes immediately before ""loop""");
    };

element_iterator :
    id opt_COLON_type_specifier OF_no_indent expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => $1.Tree,
	  Is_Ref => True,
	  Obj_Type => $2.Tree,
	  Obj_Value => $4.Tree));
    }
  | '[' id REFERS_TO id ']' OF_no_indent expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Key_Value,
	  Name => $4.Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $7.Tree,
	  Key_Name => $2.Tree));
    }
  ;

OF_no_indent : OF_kw {
        if ParaSail_Lex.Debug_Indent
          and then ParaSail_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        ParaSail_Lex.Expecting_Indent := False;
    };

initial_next_value_iterator :
    id opt_COLON_type_specifier ASSIGN_or_equal expression 
      THEN_no_indent next_value_list opt_while_condition {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => $1.Tree,
	  Is_Ref => False,
	  Obj_Type => $2.Tree,
	  Obj_Value => $4.Tree,
	  Next_Values => $6.List,
	  While_Cond => $7.Tree));
    }
  | id opt_COLON_type_specifier REFERS_TO name 
      THEN_no_indent next_name_list opt_while_condition {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Next_Value,
	  Name => $1.Tree,
	  Is_Ref => True,
	  Obj_Type => $2.Tree,
	  Obj_Value => $4.Tree,
	  Next_Values => $6.List,
	  While_Cond => $7.Tree));
    }
  ;

initial_value_iterator :
    id opt_COLON_type_specifier ASSIGN_or_equal expression opt_while_condition {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => $1.Tree,
	  Is_Ref => False,
	  Obj_Type => $2.Tree,
	  Obj_Value => $4.Tree,
	  While_Cond => $5.Tree));
    }
  | id opt_COLON_type_specifier REFERS_TO name opt_while_condition {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Initial_Value,
	  Name => $1.Tree,
	  Is_Ref => True,
	  Obj_Type => $2.Tree,
	  Obj_Value => $4.Tree,
	  While_Cond => $5.Tree));
    }
  ;

opt_COLON_type_specifier : 
    COLON_no_indent type_specifier {
	$$ := $2;
    }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

next_value_list : 
    expression { 
	$$ := (One_List, Lists.Make((1 => $1.Tree))); 
    } 
  | next_value_list PARALLEL expression {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

next_name_list : 
    name  { 
	$$ := (One_List, Lists.Make((1 => $1.Tree))); 
    }
  | next_name_list PARALLEL name {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

opt_while_condition :
    WHILE_or_UNTIL_kw condition {
	$$ := (One_Tree, Conditionally_Complement(
	  $2.Tree, Complement => $1.Is_Until));
	    -- Complement condition if used "until"
    }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;
    
opt_direction : direction { $$ := $1; } 
  | { 
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

direction : 
    CONCURRENT_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("concurrent"));
    }
  | forward_or_reverse { $$ := $1; }
  ;

forward_or_reverse : 
    FORWARD_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("forward"));
    }
  | REVERSE_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos,
	  PSC.Strings.String_Lookup("reverse"));
    }
  ;

block_statement :
    BLOCK_kw opt_COLON_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE opt_END_BLOCK {
	$$ := (One_Tree, Block_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  Block_Body => $3.Tree,
	  End_With_Values => $5.End_With_Values,
          Check_Label => $5.Check_Label,
          Label => $5.Label));
    }
  ;

opt_BLOCK_no_indent : BLOCK_no_indent
  | {
	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
    }
  ;
   
opt_END_BLOCK :
    {
        $$ := (Optional_End_Token, Check_Label => False,
               Source_Pos => PSC.Syntax.Cur_Source_Pos,
                others => Null_Optional_Tree);
    }
  | END_kw opt_BLOCK_no_indent opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token, Check_Label => True,
               Source_Pos => Token_Src_Pos ($1),
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

expression :
    expression_no_err {
	$$ := $1;
    }
  | expression_no_err divide_assign_as_not_equal expression_no_err { 
	-- Error recovery
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

divide_assign_as_not_equal :
    DIVIDE_ASSIGN { 
	yyerror("Use ""!="" rather than ""/="" in ParaSail");
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.NEQ_Op);
    }
  ;

expression_no_err :
    logical_expression { $$ := $1; }
  | logical_expression '?' 
      expression COLON_no_indent
      expression_no_err {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Quest_Colon,
          Source_Pos => $2.Source_Pos,
	  Cond => $1.Tree,
	  Then_Part => $3.Tree,
	  Else_Part => $5.Tree));
	Set_Source_Pos($$.Tree, Source_Pos => $2.Source_Pos);
    }
  | lambda_expression { $$ := $1; }
  ;

lambda_expression :
    LAMBDA_kw lambda_opt_id_list GIVES_or_RETURN_kw
       simple_expression_or_expr_stmt_seq {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Lambda_Operation,
	  Operation_Inputs => $2.List,
	  Operation_Outputs => Lists.Empty_List,
          Global_Read_List => Lists.Empty_List,
          Global_Update_List => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Def => True,
	  Statements => $4.Tree)); 
        Set_Source_Pos ($$.Tree, Token_Src_Pos ($1));
    }
  ;

lambda_opt_id_list : '(' right_paren {
	$$ := (One_List, Lists.Empty_List);
    }
  | id_as_param {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | '(' id_as_param_list right_paren {
        $$ := $2;
    }
  ; 

id_as_param_list : id_as_param {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | id_as_param_list ',' id_as_param {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

id_as_param : id {
        $$ := (One_Tree, Param_Decl.Make(
          Name => $1.Tree,
          Kind => Param_Decl.Default_Param,
          Locking => Param_Decl.Not_Locked,
          Is_Optional => False,
          Param_Type => Null_Optional_Tree,
          Param_Default => Null_Optional_Tree));
    }
  ;

simple_expression_or_expr_stmt_seq :
    simple_expression { $$ := $1; }
  | '(' expr_statement_seq ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => $2.Tree)),
          Source_Pos => $1.Source_Pos));
    }
  ;

expr_statement_seq : expr_statement ';' expr_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | expr_statement_seq ';' expr_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
     }
  ;

expr_statement : 
    primitive_statement { $$ := $1; }
  | return_stmt { $$ := $1; }
  | expression_no_err { $$ := $1; }
  ;

logical_expression :  
    comparison_or_test_expression { $$ := $1; }
  | logical_expression logical_operator comparison_or_test_expression {
      declare
	Left_Tree : PSC.Trees.Tree'Class renames Tree_Ptr_Of($1.Tree).all;
	use type Binary.Binary_Operator_Enum;
      begin
	if Left_Tree in Binary.Tree'Class and then
	  Binary.Tree(Left_Tree).Operator in Binary.Logical_Ops then
	    if Binary.Tree(Left_Tree).Operator /= $2.Binary_Op then
		-- logical operators are associative only with same op
		yyerror(
		  "must use parentheses in sequence of " &
		    "distinct logical operators",
                  At_Token => $2);
	    elsif $2.Binary_Op = Binary.Implies_Op then
		-- Implication should associate right-to-left but that
		-- is too confusing.
		yyerror(
		  "must use parentheses in sequence of implication operators",
                  At_Token => $2);
	    end if;
	end if;

	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
      end;
    }
  ;

comparison_or_test_expression :
    comparison_expression { $$ := $1; }
  | adding_expression IN_kw simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | adding_expression NOT_kw IN_kw simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $4.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | adding_expression IS_no_indent NULL_kw {
	$$ := (One_Tree, Unary.Make(
	  Operator => Unary.Is_Null_Op,
	  Operand => $1.Tree));
    }
  | adding_expression NOT_kw NULL_kw {
	-- We use adding_expression before "NOT" instead of simple_expression
	-- to avoid ambiguity associated with polymorphic type names
        -- (which are included in simple_expression but not adding_expression):
	--    Integer+ not null 
	-- could be interpreted as:
	--    Integer + not(null)
	$$ := (One_Tree, Unary.Make(
	  Operator => Unary.Not_Null_Op,
	  Operand => $1.Tree));
    }
  | adding_expression IS_no_indent FUNC_kw '(' opt_operation_actual_list ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Is_Function_Of,
	  Prefix => $1.Tree,
	  Operands => $5.List));
    }
  ;

comparison_expression : -- comparisons are non associative
    simple_expression { $$ := $1; }
  | simple_expression comparison_operator simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

simple_expression : -- used to avoid use of '>' in module instantiation
    simple_expression_component { $$ := $1; }
  | simple_expression '|' simple_expression_component {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

simple_expression_component :
    adding_expression { $$ := $1; }
  | adding_expression interval_operator adding_expression { 
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
--  NOTE: As of 11/14/2012 We are removing these "very open" intervals
--        as they create ambiguity with the map-reduce initial-val <...>
--   | adding_expression interval_operator { 
-- 	$$ := (One_Tree, Binary.Make(
-- 	  Operator => $2.Binary_Op,
-- 	  Left_Operand => $1.Tree,
-- 	  Right_Operand => Null_Optional_Tree));
--     }
--   | interval_operator adding_expression { 
-- 	$$ := (One_Tree, Binary.Make(
-- 	  Operator => $1.Binary_Op,
-- 	  Left_Operand => Null_Optional_Tree,
-- 	  Right_Operand => $2.Tree));
--     }
  ;

adding_expression :
    term { $$ := $1; }
  | adding_expression '+' term {
        --  NOTE: We treat '+' here separately to avoid
        --        reduce/reduce conflicts
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Plus_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | adding_expression adding_operator term {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

term : 
    factor { $$ := $1; }
  | term multiplying_operator factor {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

factor : 
    primary { $$ := $1; }
  | primary power_operator factor {
	 -- right associative
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | unary_operator factor  {
	-- unary ops have higher precedence 
	-- than every operator except the power_operator.
	$$ := (One_Tree, Unary.Make(
	  Operator => $1.Unary_Op,
	  Operand => $2.Tree));
    }
  ;

primary :
    name { $$ := $1; }
  | literal { $$ := $1; }
  | '(' conditional_expression ')' { $$ := $2; }
  | '(' quantified_expression ')' { $$ := $2; }
  | '(' map_reduce_expression ')' { $$ := $2; }
  | '|' simple_expression_component '|' {
        $$ := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => $2.Tree));
    }
  | aggregate { $$ := $1; }
  | '<' simple_expression '>' {
        --  This is used in a map_reduce expression to specify the initial val
        $$ := (One_Tree, Unary.Make(Unary.Initial_Value_Op,
          Operand => $2.Tree));
    }
  ;
  
literal:  -- NOTE: See "name" for String_Literal
    Integer_Literal { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | Real_Literal { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | Char_Literal { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | Enum_Literal { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | NULL_kw { 
	$$ := (One_Tree, PSC.Trees.Identifier.Make("null", $1.Source_Pos)); 
    }
  ;

name :
    qualified_name_and_property opt_PRIME { 
	if $2.Is_Present then
	    $$ := (One_Tree, Unary.Make(Unary.Updated_Value_Op,
	      Operand => $1.Tree));
	else
	    $$ := $1; 
	end if;
    }
  | qualified_name DOUBLE_COLON literal {
	-- Use "::" to specify type of literal and
	-- to disambiguate operator specified as a string.
	$$ := (One_Tree, Qualified_Name.Make(
	  Prefix => $1.Tree,
	  Id => $3.Tree));
    }
  | name '(' opt_operation_actual_list ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => $1.Tree,
	  Operands => $3.List));
    }
  | name '[' opt_operation_actual_list ']' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => $1.Tree,
	  Operands => $3.List));
    }
  | name '[' dot_dot_as_interval ']' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Container_Indexing,
	  Prefix => $1.Tree,
	  Operands => Lists.Make((1 => $3.Tree))));
    }
  | name '.' selector {
	$$ := (One_Tree, Selection.Make(
	  Prefix => $1.Tree,
	  Selector => $3.Tree));
    }
  ;

qualified_name_and_property :
    qualified_name { $$ := $1; }
  | qualified_name_and_property Enum_Literal {
        declare
           --  Substitute '@' for '#'
           Enum_Str : constant String := PSC.Strings.To_String ($2.Str);
           Prop_Str : constant String := '@' &
             Enum_Str (Enum_Str'First + 1 .. Enum_Str'Last);
        begin
           $$ := (One_Tree, Property.Make(Operand => $1.Tree,
             Property_Id =>
               PSC.Trees.Identifier.Make (Prop_Str, $2.Source_Pos)));
        end;
    }
  ;

opt_PRIME : 
    PRIME { $$ := (Optional, True); }
  | PRIME Identifier {
	yyerror("Use ""#"" instead of ""'"" to query property in ParaSail",
          At_Token => $1);
	$$ := (Optional, True);
    }
  | { $$ := (Optional, False); }
  ;

operation_actual_list : 
    operation_actual {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | operation_actual_list ',' operation_actual {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

operation_actual : 
    expression_no_err { $$ := $1; }
  | id REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  ;

selector : id { $$ := $1; };

unary_operator : 
    '+' { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Plus_Op); }
  | '-' { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Minus_Op); }
  | ABS_kw { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Abs_Op); }
  | NOT_kw { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Not_Op); }
  | PLUS_BASED_OP { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Plus_Op); }
  | MINUS_BASED_OP { $$ := (One_Unary_Op, $1.Source_Pos, Unary.Minus_Op); }
  ;

adding_operator : 
    '-' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Minus_Op); }
  | PLUS_BASED_OP { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Plus_Op); }
  | MINUS_BASED_OP { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Minus_Op); }
  ;

multiplying_operator : 
    '*' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Times_Op); }
  | '/' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Divide_Op); }
  | MOD_kw { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Mod_Op); }
  | REM_kw { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Rem_Op); }
  ;

power_operator : POWER { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Power_Op); 
    };

assign_operator : assign_operator_not_divide { $$ := $1; }
  | DIVIDE_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Divide_Assign_Op); 
    }
  ;

assign_operator_not_divide :
    ASSIGN {
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
     }
  | PLUS_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Plus_Assign_Op); 
    }
  | MINUS_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Minus_Assign_Op); 
    }
  | TIMES_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Times_Assign_Op); 
    }
  | POWER_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Power_Assign_Op); 
    }
  | COMBINE_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Combine_Assign_Op); 
    }
  | AND_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.And_Assign_Op); 
    }
  | OR_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Or_Assign_Op); 
    }
  | XOR_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Xor_Assign_Op); 
    }
  | LSHIFT_ASSIGN {
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Left_Shift_Assign_Op);
    }
  | RSHIFT_ASSIGN {
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Right_Shift_Assign_Op);
    }
  ;

ASSIGN_or_equal : ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
    }
  | equal_as_assign { $$ := $1; }
  ;

equal_as_assign : '=' {
	yyerror("Use "":="" rather than ""="" in ParaSail");
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
    }
  ;

comparison_operator : 
    COMPARE { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Compare_Op); }
  | EQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Equal_Op); }
  | NEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.NEQ_Op); }
  | '<' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Less_Op); }
  | LEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.LEQ_Op); }
  | '>' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Greater_Op); }
  | GEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.GEQ_Op); }
  | LSHIFT { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Left_Shift_Op ); }
  | '>' '>' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Right_Shift_Op); }
  | '=' { 
	yyerror("Use ""=="" rather than ""="" in ParaSail");
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Equal_Op);
    }
  ;

logical_operator :
    AND_kw { $$ := (One_Binary_Op, $1.Source_Pos, Binary.And_Op); }
  | OR_kw { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Or_Op); }
  | XOR_kw  { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Xor_Op); }
  | AND_kw THEN_no_indent
      { $$ := (One_Binary_Op, $1.Source_Pos, Binary.And_Then_Op); }
  | OR_kw ELSE_no_indent
      { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Or_Else_Op); }
  | IMPLIES { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Implies_Op); }
  ;

interval_operator :
    DOT_DOT { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Closed_Interval_Op); 
    }
  | OPEN_INTERVAL { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Open_Interval_Op); 
    }
  | CLOSED_OPEN_INTERVAL { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Closed_Open_Interval_Op); 
    }
  | OPEN_CLOSED_INTERVAL { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Open_Closed_Interval_Op); 
    }
  ;

aggregate : 
    class_aggregate { $$ := $1; } 
  | container_aggregate { $$ := $1; } 
  ;

class_aggregate : 
    '(' opt_class_component_list ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => $2.List,
          Source_Pos => $1.Source_Pos));
    }
  | '(' name DIVIDE_ASSIGN expression ')' {
	-- Error recovery
	yyerror("Use ""!="" rather than ""/="" in ParaSail",
          At_Token => $3);
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => Lists.Make((1 => Binary.Make(
	    Operator => Binary.NEQ_Op,
	    Left_Operand => $2.Tree,
	    Right_Operand => $4.Tree,
            Source_Pos => $3.Source_Pos))),
          Source_Pos => $1.Source_Pos));
     }
  | qualified_name DOUBLE_COLON '(' opt_class_component_list ')' {
	-- Type of aggregate specified
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => $1.Tree,
	  Operands => $4.List,
          Source_Pos => $3.Source_Pos));
    }
  ;

opt_class_component_list : 
    class_component_list {
	$$ := $1;
    }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

class_component_list : 
    class_component {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | class_component_list ',' class_component {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

class_component : 
    expression_no_err { $$ := $1; }
  | id REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | name MOVE movable_object {
	$$ := (One_Tree, Assign_Stmt.Make(
	  Assign_Operator => Assign_Stmt.Move_Op,
	  LHS => $1.Tree,
	  RHS => $3.Tree));
    }
  ;


container_aggregate : 
    '[' opt_container_element_list ']' {
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

	if Lists.Length($2.List) = 1 then
	  declare
	    Solo_Operand : PSC.Trees.Tree'Class renames 
	      Tree_Ptr_Of(Lists.Nth_Element($2.List, 1)).all;
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

	    $$ := (One_Tree, Unary.Make(Unary.Meaning_Op,
	      Operand => 
		Lists.Nth_Element(Invocation.Tree(
		  Tree_Ptr_Of
                    (Lists.Nth_Element($2.List, 1)).all).Operands, 1)));
	else
	    -- Normal case of [...], create an invocation node.
	    $$ := (One_Tree, Invocation.Make(
	      Kind => Invocation.Container_Aggregate,
	      Prefix => Null_Optional_Tree,
	      Operands => $2.List,
	      Source_Pos => $1.Source_Pos));
	end if;
      end;
    }
  | qualified_name DOUBLE_COLON '[' opt_container_element_list ']' {
	-- Type of result specified
      declare
	use type Invocation.Invocation_Kind_Enum;
	Is_Double_Bracket_Special_Case : Boolean := False;
      begin
	if Lists.Length($4.List) = 1 then
	  declare
	    Solo_Operand : PSC.Trees.Tree'Class renames 
	      Tree_Ptr_Of(Lists.Nth_Element($4.List, 1)).all;
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

	    $$ := (One_Tree, Binary.Make(Binary.Meaning_Op,
	      Left_Operand => $1.Tree,
	      Right_Operand => 
		Lists.Nth_Element(Invocation.Tree(
		  Tree_Ptr_Of
                    (Lists.Nth_Element($4.List, 1)).all).Operands, 1)));
	else
	    -- Normal case of Type::[...], create an invocation node.
	    $$ := (One_Tree, Invocation.Make(
	      Kind => Invocation.Container_Aggregate,
	      Prefix => $1.Tree,
	      Operands => $4.List,
              Source_Pos => $3.Source_Pos));
	end if;
      end;
    }
  ;
  
opt_container_element_list : 
    container_element_list { $$ := $1; }
  | dot_dot_as_interval {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

container_element_list : 
    container_element {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | container_element_list ',' container_element {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

container_element : 
    expression { $$ := $1; }
  | simple_expression REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | dot_dot_as_interval REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | FOR_kw_set_flag iterator opt_annotation opt_forward_or_reverse
      opt_comma_id REFERS_TO expression {
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
        Value : Optional_Tree := $7.Tree;
        use type PSC.Strings.U_String;
      begin
        ParaSail_Lex.Inside_For_Header := False;
        if Not_Null($5.Tree) then
           -- User has specified a key for the element
           Value := Reference.Make(
             Key => $5.Tree,
             Referent => Value);
        end if;

        if $4.Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("forward" is the default)
            Iterator.Add_Direction($2.Tree, $4.Str);
        end if;

        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
          Kind => For_Loop_Construct.Container_Comprehension,
          Iterators => Lists.Make((1 => $2.Tree)),
          Filter => $3.List,
          Loop_Body => Value));
        Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
      end;
    }
  ;

opt_comma_id :
    {
        $$ := (One_Tree, Null_Optional_Tree);
    }
  | ',' expression
    {
        $$ := $2;
    }
  ;

conditional_expression :
    if_expression { $$ := $1; }
  | case_expression { $$ := $1; }
  ;

if_expression : 
  IF_kw condition COLON_or_THEN_no_indent 
     expression
  opt_else_expr {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.If_Expr,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
	Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
    };

opt_else_expr : 
    ELSIF_kw condition COLON_or_THEN_no_indent 
      expression 
    opt_else_expr {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Expr,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
	Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
    }
  | ELSE_opt_COLON_no_indent expression {
	$$ := $2;
    }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;


COLON_or_THEN_no_indent :
    COLON_no_indent { $$ := $1; }
  | THEN_no_indent  { $$ := $1; }
  | ')' COLON_or_THEN_no_indent {
        yyerror ("Extra ')'", At_Token => $1);
        $$ := $2;
    }
  | error COLON_or_THEN_no_indent {
        yyerror ("Syntax error in condition", At_Token => $2);
        $$ := $2;
    }
  ;

case_expression : 
    CASE_kw comparison_expression COLON_or_OF_no_indent
      case_expr_alt_list {
	$$ := (One_Tree, Case_Construct.Make(
          Source_Pos => $1.Source_Pos,
	  Case_Selector => $2.Tree,
	  Case_Alt_List => $4.List,
          Is_Case_Expr => True));
	Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
    }
  ;

COLON_or_OF_no_indent :
    COLON_no_indent  { $$ := $1; }
  | OF_no_indent     { $$ := $1; }
  | IS_no_indent {
        yyerror
          ("Use ""of"" rather than ""is"" for a case statement",
           At_Token => $1);
        $$ := $1;
    }
  | ')' COLON_or_OF_no_indent {
        yyerror ("Extra ')'", At_Token => $1);
        $$ := $2;
    }
  | error COLON_or_OF_no_indent {
        yyerror ("Syntax error in case selector", At_Token => $2);
        $$ := $2;
    }
  ;

case_expr_alt_list : 
    case_expr_alt {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | case_expr_alt_list ';' case_expr_alt {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

case_expr_alt : 
    '[' simple_expression_opt_named ']' REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $5.Tree));
    }
  | '[' dot_dot_opt_named ']' REFERS_TO expression {
	-- NOTE: ".." alternative must come last
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $5.Tree));
    }
  | '[' simple_expression_opt_named REFERS_TO expression {
        yyerror("Missing ']'", At_Token => $3);
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree)),
            Source_Pos => $1.Source_Pos),
	  Referent => $4.Tree));
    }
  ;

quantified_expression :
    FOR_kw_set_flag ALL_or_SOME_kw quantified_iterator opt_annotation REFERS_TO
      condition_or_quantified_expression {
	declare
	    Kind_Of_For_Loop: constant array(Boolean) of 
	      For_Loop_Construct.For_Loop_Kind_Enum := (
		False => For_Loop_Construct.Existential_Quantified_Expr,
		True => For_Loop_Construct.Univ_Quantified_Expr);
	begin
            ParaSail_Lex.Inside_For_Header := False;
	    $$ := (One_Tree, For_Loop_Construct.Make(
              Source_Pos => $1.Source_Pos,
	      Kind => Kind_Of_For_Loop($2.Is_Present),
	      Iterators => Lists.Make((1 => $3.Tree)),
	      Filter => $4.List,
	      Loop_Body => $6.Tree));
            Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
	end;
    }
  | FOR_kw_set_flag ALL_or_SOME_kw id opt_COLON_type_specifier REFERS_TO 
      condition_or_quantified_expression {
        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := $4.Tree;
     begin
        ParaSail_Lex.Inside_For_Header := False;
        if Is_Null (Obj_Type) then
           --  Presume id is the type name when not both specified.
           Obj_Type := $3.Tree;
        end if;

        if not $2.Is_Present then
           yyerror ("Must specify ""for all [E : ] T"" or " &
             """for all/some E in/of Container"" in quantified expression",
             At_Token => $3);
        end if;

        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
          Kind => For_Loop_Construct.Univ_Quantified_Expr,
          Iterators => Lists.Make((1 =>
            Iterator.Make(
              Kind => Iterator.Set_Iterator,
              Name => $3.Tree,
              Is_Ref => False,
              Obj_Type => Obj_Type,
              Obj_Value => Null_Optional_Tree))),
          Filter => Lists.Empty_List,
          Loop_Body => $6.Tree));
        Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
     end;
    }
  | FOR_kw_set_flag ALL_or_SOME_kw id opt_COLON_type_specifier annotation
      REFERS_TO condition_or_quantified_expression {
        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := $4.Tree;
     begin
        ParaSail_Lex.Inside_For_Header := False;
        if Is_Null (Obj_Type) then
           --  Presume id is the type name when not both specified.
           Obj_Type := $3.Tree;
        end if;

        if not $2.Is_Present then
           yyerror ("Must specify ""for all [E :] T"" or " &
             """for all/some E in/of Container"" in quantified expression",
             At_Token => $3);
        end if;

        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
          Kind => For_Loop_Construct.Univ_Quantified_Expr,
          Iterators => Lists.Make((1 =>
            Iterator.Make(
              Kind => Iterator.Set_Iterator,
              Name => $3.Tree,
              Is_Ref => False,
              Obj_Type => Obj_Type,
              Obj_Value => Null_Optional_Tree))),
          Filter => $5.List,
          Loop_Body => $7.Tree));
        Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
     end;
    }
  ;

condition_or_quantified_expression :
    condition { $$ := $1; }
  | if_expression { $$ := $1; }
  | quantified_expression { $$ := $1; }
  ;

ALL_or_SOME_kw : 
    ALL_kw { $$ := (Optional, True); }
  | SOME_kw { $$ := (Optional, False); }
  ;

quantified_iterator :
    index_set_iterator { $$ := $1; }
  | element_iterator { $$ := $1; }
  | initial_next_value_iterator { $$ := $1; }
  ;

map_reduce_expression :
    FOR_kw_set_flag map_reduce_iterator_spec opt_annotation
      opt_forward_or_reverse REFERS_TO expression {
	-- This does a map/reduce operation where the initial/next result
        -- is given in <...> and the overall expression represents the
        -- reduction to be performed on each element.

        ParaSail_Lex.Inside_For_Header := False;
        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
          Kind => For_Loop_Construct.Map_Reduce_Expr,
          Iterators => $2.List,
          Filter => $3.List,
          Loop_Body => $6.Tree,
          Direction => $4.Str));
    }
  ;

map_reduce_iterator_spec :
    map_reduce_iterator {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | '(' map_reduce_iterator_list ')' {
        $$ := $2;
    }
  ;

map_reduce_iterator :
    index_set_iterator { $$ := $1; }
  | EACH_kw element_iterator { $$ := $2; }
  | initial_next_value_iterator { $$ := $1; }
  ;

map_reduce_iterator_list :
    map_reduce_iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := $1.Tree;
	begin
	    if $2.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $2.Str);
	    end if;
	    $$ := (One_List, Lists.Make((1 => Iterator_Tree)));
	end;
    }
  | map_reduce_iterator_list ';' map_reduce_iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : Optional_Tree := $3.Tree;
	begin
	    if $4.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $4.Str);
	    end if;
	    $$ := $1;
	    Lists.Append($$.List, Iterator_Tree);
	end;
    }
  | map_reduce_iterator_list ',' map_reduce_iterator opt_forward_or_reverse {
	declare
	    use type PSC.Strings.U_String;
	    Iterator_Tree : constant Optional_Tree := $3.Tree;
	begin
	    yyerror("Iterators must be separated by "";""",
              At_Token => $2);
	    if $4.Str /= PSC.Strings.Null_U_String then
		Iterator.Add_Direction(Iterator_Tree, $4.Str);
	    end if;
	    $$ := $1;
	    Lists.Append($$.List, Iterator_Tree);
	end;
    }
  ;

%%

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

##%procedure_parse

end ParaSail_Parser;

