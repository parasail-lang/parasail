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
------------------------------------------------------------------------------

--------------------------------------
-- YACC Grammar for Sparkel
--------------------------------------

-- Single-character delimiters --
%token ',' ';' ':' '.'
%token '+' '-' '*' '/' 
%token '?'
%token '(' ')' '[' ']' '<' '>'
%token '|' '&'
%token '='
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
%token BOX    -- "<>"
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
%token AMPERSAND_ASSIGN -- "&="
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
%token OUTDENT  -- used for "significant" out-denting
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
%token ACCESS_kw
%token ALIASED_kw
%token ALL_kw
%token AND_kw
%token ARRAY_kw
%token ASSERT_kw
%token BEGIN_kw
%token BLOCK_kw
%token BODY_kw
%token CASE_kw
%token CLASS_kw
%token CONCURRENT_kw
%token CONST_kw
%token CONSTANT_kw
%token CONTINUE_kw
%token DECLARE_kw
%token DELTA_kw
%token DIGITS_kw
%token EACH_kw
%token ELSE_kw
%token ELSIF_kw
%token END_kw
%token EXCEPTION_kw
%token EXIT_kw
%token EXPORTS_kw
%token EXTENDS_kw
%token FOR_kw
%token FORWARD_kw
%token FUNC_kw
%token GENERIC_kw
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
%token OPTIONAL_kw
%token OR_kw
%token OTHERS_kw
%token OUT_kw
%token PACKAGE_kw
%token PARALLEL_kw
%token PRAGMA_kw
%token PRIVATE_kw
%token PROC_kw
%token PROTECTED_kw
%token QUEUED_kw
%token RAISE_kw
%token RANGE_kw
%token RECORD_kw
%token REF_kw
%token REM_kw
%token RENAMES_kw
%token RETURN_kw
%token REVERSE_kw
%token SOME_kw
%token SUBTYPE_kw
%token TAGGED_kw
%token THEN_kw
%token TYPE_kw
%token UNTIL_kw
%token USE_kw
%token VAR_kw
%token WHEN_kw
%token WHILE_kw
%token WITH_kw
%token XOR_kw

%start compilation

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
              Is_Not_Null : Boolean := False;
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
              End_Construct_Str : PSC.Strings.U_String;
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

compilation : 
    pragma_list {
        null;  --  TBD: do something with pragmas
    }
  | compilation compilation_unit pragma_list {
        null;  --  TBD: do something with pragmas
    }
  ;

compilation_unit : 
    context_clause package_declaration_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | context_clause package_body_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | context_clause standalone_operation_definition_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | context_clause generic_declaration_with_term {
	Semantics.Add_Top_Level_Tree($2.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($2.Tree);
        end if;
    }
  | context_clause error SEMI_or_NEWLINE{
	null;
    }
  ;

SEMI_or_NEWLINE : NEWLINE | ';' | ';' NEWLINE ;

pragma_list : {
        $$ := (One_List, Lists.Empty_List);
    }
  | pragma_list pragma_with_term {
        $$ := $1;
        Lists.Append ($$.List, $2.Tree);
    }
  ;

pragma_with_term :
    pragma SEMI_or_NEWLINE { $$ := $1; }
  ;

pragma :
    PRAGMA_kw id_or_ASSERT_kw '(' record_component_list ')' {
        $$ := (One_Tree, Annotation.Make
                 (Annotations =>
                    Lists.Make ((1 => Reference.Make
                      (Key => $2.Tree,
                       Referent =>
                         Invocation.Make(
                           Kind => Invocation.Class_Aggregate,
                           Prefix => Null_Optional_Tree,
                           Operands => $4.List,
                           Source_Pos => $3.Source_Pos)))),
	          Label =>
                    PSC.Trees.Identifier.Make("pragma", $1.Source_Pos))); 
    }
  | PRAGMA_kw id_or_ASSERT_kw {
        $$ := (One_Tree, Annotation.Make
                 (Annotations =>
                    Lists.Make ((1 => Reference.Make
                      (Key => $2.Tree,
                       Referent => Null_Optional_Tree))),
--                          Invocation.Make(
--                            Kind => Invocation.Class_Aggregate,
--                            Prefix => Null_Optional_Tree,
--                            Operands => Lists.Empty_List,
--                            Source_Pos => $2.Source_Pos)))),
	          Label =>
                    PSC.Trees.Identifier.Make("pragma", $1.Source_Pos))); 
    }
  ;

id_or_ASSERT_kw : id { $$ := $1; }
  | ASSERT_kw { $$ := $1; }     
  ;

imported_libunit_name_list : 
    imported_libunit_name {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | imported_libunit_name_list ',' imported_libunit_name {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

imported_libunit_name :
    '*' {
        $$ := (One_Tree, PSC.Trees.Identifier.Make ($1.Str, $1.Source_Pos)); 
    }
  | expanded_name {
        $$ := $1;
    }
  | expanded_name '.' '*' {
	$$ := (One_Tree, Qualified_Name.Make (  -- TBD: Selection?
	  Prefix => $1.Tree,
	  Id => PSC.Trees.Identifier.Make ($3.Str, $3.Source_Pos))); 
    }

  ;

context_clause : context_clause_list {
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

context_clause_list :
    {
        $$ := (One_List, Lists.Empty_List);  --  TBD: use pragmas ($1.List)
    }
  | context_clause_list with_clause pragma_list { 
        $$ := $1;
        Lists.Append ($$.List, $2.List);
        --  TBD: use pragmas ($3.List)
    }
  | context_clause_list context_use_clause pragma_list {
        $$ := $1;
        --  TBD: Lists.Append ($$.List, $2.List);  --  need to distinguish
        --  TBD: use pragmas ($3.List)
    }
  ;

with_clause : WITH_kw imported_libunit_name_list SEMI_or_NEWLINE {
        $$ := $2;
    }
  ;
  
context_use_clause : USE_kw imported_libunit_name_list SEMI_or_NEWLINE {
       $$ := $2;  --  TBD: Need to distinguish from "with" clause
    }
  ;

use_clause : USE_kw expanded_name_list {
       $$ := $2;
    }
  ;

expanded_name_list :
    expanded_name {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | expanded_name_list ',' expanded_name {
        $$ := $1;
        Lists.Append ($$.List, $3.Tree);
    }
  ;

generic_declaration_with_term :
    GENERIC_kw
      opt_formal_generic_param_decl_list
    package_declaration_with_term {
	$$ := $3;
	declare
	    Mod_Tree : Module.Tree renames 
	      Module.Tree(Tree_Ptr_Of($$.Tree).all);
	begin
	    Mod_Tree.Has_Formals := True;
	    Mod_Tree.Module_Formals := $2.List;
	end;
    }
  | GENERIC_kw
      opt_formal_generic_param_decl_list
    basic_operation_declaration SEMI_or_NEWLINE {
        $$ := $3; --  TBD
    }
  ;

opt_formal_generic_param_decl_list :
    SEMI_or_NEWLINE {
        $$ := (One_List, Lists.Empty_List);
    }
  | formal_generic_param_decl_list {
        $$ := $1;
    }
  ;

formal_generic_param_decl_list : formal_generic_param_decl SEMI_or_NEWLINE {
        $$ := $1;
    }
  | formal_generic_param_decl_list formal_generic_param_decl SEMI_or_NEWLINE {
        $$ := $1;
        Lists.Append ($$.List, $2.List);
    }
  ;

formal_generic_param_decl :
    formal_type_declaration {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | formal_object_declaration {
        $$ := $1;
    }
  ;

formal_type_declaration :
    TYPE_kw id IS_no_indent NEW_kw annotated_type_specifier {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => $5.Tree));
    }
  | TYPE_kw id IS_no_indent annotated_type_specifier {
        yyerror ("""new"" required", At_Token => $4);
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $1.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => $4.Tree));
    }
  ;

formal_object_declaration :
    value_formal {
        $$ := $1;
    }
  ;

package_declaration_with_term : 
   package_instantiation_with_term { $$ := $1; }
 | opt_PRIVATE_kw PACKAGE_kw package_defining_name 
     IS_kw_INDENT
      pkg_spec_element_list
      opt_annotation_opt_term
      opt_new_pkg_spec_element_list
      opt_restricted_element_list
   OUTDENT_opt_NEWLINE END_PACKAGE {
      declare
	Elem_List : Lists.List := $5.List;
      begin
	--  if $1.Is_Private and then $4.Has_Module_Formals then
	--     yyerror("Private pkg_spec may not add package parameters");
	--  end if;
	if not Lists.Is_Empty($6.List) then
	    -- Include the opt_annotation
	    Lists.Append(Elem_List, Annotation.Make(Annotations => $6.List));
	end if;
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => $1.Is_Present,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => $7.List,
	  Module_Implements => $8.List));

        if $10.Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
              Ending_Id => $10.Label);
        end if;

      end;
    }
 | opt_PRIVATE_kw PACKAGE_kw package_defining_name 
     IS_kw_NEWLINE
   END_kw opt_PACKAGE_kw package_defining_name SEMI_or_NEWLINE {
	--  if $1.Is_Private and then $4.Has_Module_Formals then
	--     yyerror("Private pkg_spec may not add package parameters");
	--  end if;
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($3.Tree),
	  Add_On_Label => Add_On_For_Module($3.Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => $1.Is_Present,
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

	Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
	  Ending_Id => $7.Tree);
    }
  ;


package_instantiation_with_term :
   package_instantiation SEMI_or_NEWLINE { $$ := $1; };

package_instantiation :
   opt_PRIVATE_kw PACKAGE_kw package_defining_name 
     IS_no_indent parent_type_specifier {
        --  Instantiate a package, not intended to be used as a type.
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
          Name => $3.Tree,
          Add_On_Label => Lists.Empty_List,
          Is_Interface => True,
          Is_Abstract => False,
          Is_Private => $1.Is_Present,
          Is_Concurrent => False,
          Is_Limited => True,
          Has_Formals => True,
          Treat_As_Type => False,
          Module_Formals => Lists.Empty_List,
          Extends_Interface => $5.Tree,
          Implements_Interfaces => Lists.Empty_List,
          Class_Locals => Lists.Empty_List,
          Module_Exports => Lists.Empty_List,
          Module_New_Exports => Lists.Empty_List,
          Module_Implements => Lists.Empty_List));
    }
  ;

IS_kw_INDENT : IS_kw INDENT
  | error IS_kw INDENT {
        yyerror ("Syntax error before ""is""", At_Token => $2);
    }
  ;

OUTDENT_opt_NEWLINE :
    OUTDENT
  | OUTDENT NEWLINE
  ;

OUTDENT_END_kw : OUTDENT_opt_NEWLINE END_kw ;

END_PACKAGE : 
    END_kw opt_PACKAGE_kw package_defining_name SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str, Check_Label => True,
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;

IS_kw_NEWLINE : IS_kw NEWLINE ;

opt_PACKAGE_kw : PACKAGE_kw { $$ := $1; }
  | { 
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;
   
opt_pkg_spec_qualifier : 
    pkg_spec_qualifier { $$ := $1; }
  | {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;

pkg_spec_qualifier : 
    pkg_body_qualifier { $$ := $1; }
  | ABSTRACT_kw opt_pkg_body_qualifier {
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Abstract => True, 
	  Is_Concurrent => $2.Is_Concurrent,
	  others => False);
    }
  | PRIVATE_kw opt_pkg_body_qualifier {
	$$ := (Construct_Qualifier, 
          Source_Pos => $1.Source_Pos,
	  Is_Private => True, 
	  Is_Concurrent => $2.Is_Concurrent,
	  others => False);
    }
  ;

opt_pkg_body_qualifier : 
    pkg_body_qualifier { $$ := $1; }
  | { 
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  ;

pkg_body_qualifier : PROTECTED_kw {
	$$ := (Construct_Qualifier, 
               Source_Pos => $1.Source_Pos,
	       Is_Concurrent => True, others => False);
    };

standalone_operation_definition_with_term : 
    operation_definition_with_term {
        $$ := $1;
    }
  | operation_import SEMI_or_NEWLINE { $$ := $1; }
  | operation_equiv SEMI_or_NEWLINE { $$ := $1; }
  ;

formals : 
    '<' package_formal_list '>' { $$ := $2; }
  | BOX { $$ := (One_List, Lists.Empty_List); }
  ;

COLON_no_indent : ':' | EOL_COLON {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [colon with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

opt_formals : 
    formals { $$ := $1; } 
  | { 
	$$ := (Optional, Is_Present => False);
    }
  ;

package_name : expanded_name { $$ := $1; };

package_defining_name : 
    expanded_name { $$ := $1; }
  | expanded_name add_on_label { 
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

opt_package_formal_list : 
    package_formal_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    };

package_formal_list : 
    annotated_package_formal { $$ := $1; }
  | package_formal_list ';' annotated_package_formal {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

annotated_package_formal : 
    opt_annotation type_formal opt_annotation {
	Annotation.Add_Annotation($2.Tree, $1.List, Precedes => True);
	Annotation.Add_Annotation($2.Tree, $3.List);
	$$ := (One_List, Lists.Make((1 => $2.Tree)));
    }
--   | opt_annotation operation_formal {
-- 	Annotation.Add_Annotation($2.Tree, $1.List, Precedes => True);
-- 	$$ := (One_List, Lists.Make((1 => $2.Tree)));
--     }
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
    id IS_no_indent type_instantiation {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $1.Tree,
	  Is_New_Type => False,
	  Type_Definition => $3.Tree));
    }
  | type_instantiation { 
	$$ := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => $1.Tree));
    }
  ;

IS_no_indent : IS_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

-- operation_formal : 
--     basic_operation_declaration opt_operation_default { 
-- 	if Not_Null($2.Tree) then
-- 	    $$ := (One_Tree, Operation.Add_Op_Default(
-- 	      Op_Decl => $1.Tree, Op_Default => $2.Tree));
-- 	else
-- 	    $$ := $1; 
-- 	end if;
--     } 
--   | OPTIONAL_kw basic_operation_declaration opt_operation_default { 
-- 	if Not_Null($3.Tree) then
-- 	    $$ := (One_Tree, Operation.Add_Op_Default(
-- 	      Op_Decl => $2.Tree, Op_Default => $3.Tree));
-- 	else
-- 	    $$ := $2; 
-- 	end if;
-- 
-- 	Operation.Tree(Tree_Ptr_Of($$.Tree).all).Is_Optional := True;
--     } 
--   | QUEUED_kw basic_operation_declaration opt_operation_default { 
-- 	if Not_Null($3.Tree) then
-- 	    $$ := (One_Tree, Operation.Add_Op_Default(
-- 	      Op_Decl => $2.Tree, Op_Default => $3.Tree));
-- 	else
-- 	    $$ := $2; 
-- 	end if;
-- 
-- 	Operation.Tree(Tree_Ptr_Of($$.Tree).all).Is_Queued := True;
--     } 
--   ;

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
	      Param_Type => Copy_If_Not_First ($4.Tree, I),
	      Param_Default => Copy_If_Not_First ($5.Tree, I)));
	end loop;
    }
  | value_formal_mode id_list COLON_no_indent
      opt_output_type_qualifier operand_type_specifier 
      opt_ASSIGN_or_equal_simple_expression  {
	-- "simple_expression" to avoid use of '>'
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($2.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($2.List, I),
	      Kind => $1.Param_Kind,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => $4.Is_Optional,
	      Param_Type => Copy_If_Not_First ($5.Tree, I),
	      Param_Default => Copy_If_Not_First ($6.Tree, I)));
	end loop;
    }
  | value_formal_mode operand_type_specifier 
    { 
	$$ := (One_List, Lists.Make((1 => Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => False,
	  Param_Type => $2.Tree,
	  Param_Default => Null_Optional_Tree))));
    }
  ;

value_formal_mode : global_mode { $$ := $1; };

opt_ASSIGN_or_equal_simple_expression :
    ASSIGN_or_equal simple_expression { $$ := $2; }
  | { $$ := (One_Tree, Null_Optional_Tree); }
  ;
	

id : Identifier {
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos));
    }
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
    expanded_name { $$ := $1; }
  | polymorphic_type_name { $$ := $1; }
  ;

polymorphic_type_name : expanded_name PRIME CLASS_kw {
	$$ := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => $1.Tree));
    };

expanded_name : 
    id_or_string_literal { 
	$$ := $1;
    }
  | expanded_name '.' id_or_string_literal {
	$$ := (One_Tree, Selection.Make(
	  Prefix => $1.Tree,
	  Selector => $3.Tree));
    }
  | expanded_name PRIME id_or_string_literal {
	$$ := (One_Tree, Qualified_Name.Make(
	  Prefix => $1.Tree,
	  Id => $3.Tree));
    }
  ;

id_or_string_literal :
    id { $$ := $1; }
  | string_lit { $$ := $1; }
  ;

string_lit : String_Literal {
        -- String_Literal can be used as a "name" when it is an operator
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  ;
  
type_instantiation : 
    package_name boxed_package_actual_list {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => $1.Tree,
	  Operands => $2.List));
    }
  | name '[' opt_operation_actual_list ']' boxed_package_actual_list {
	-- Include extension label in package name
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => 
	    Invocation.Make(
	      Kind => Invocation.Container_Indexing,
	      Prefix => $1.Tree,
	      Operands => $3.List),
	  Operands => $5.List));
    }
  ;

opt_add_on_label :
    add_on_label { $$ := $1; }
  | { 
	$$ := (One_List, Lists.Empty_List);
    }
  ;

boxed_package_actual_list : 
    '<' package_actual_list '>' { $$ := $2; }
    | BOX {
	$$ := (One_List, Lists.Empty_List);
    }
    ;

opt_package_actual_list : 
    package_actual_list { $$ := $1; }
    | {
	$$ := (One_List, Lists.Empty_List);
    }
    ;

package_actual_list :
    package_actual {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | package_actual_list ',' package_actual {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

package_actual : 
    simple_type_specifier_or_expression { $$ := $1; }
  | id REFERS_TO simple_type_specifier_or_expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  ;

-- simple_expression subsumes simple type_name in this rule
simple_type_specifier_or_expression : 
    expanded_name annotation { 
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
	-- We use adding_expression instead of expanded_name
	-- to avoid reduce/reduce conflicts in the grammar.
	$$ := (One_Tree, Qualifier.Qualify(
	    Qualifiers => (Qualifier.Is_Polymorphic => True, others => False),
	    Operand => $1.Tree));
    }
  | simple_expression { $$ := $1; }             
	-- simple_expr to avoid problems with '>'
  | lambda_expression { $$ := $1; }
  | type_instantiation { $$ := $1; }
  ;
  
annotated_basic_type_specifier : 
    basic_type_specifier { $$ := $1; }
  | basic_type_specifier annotation {
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  ;

annotated_type_specifier : 
    type_specifier { $$ := $1; }
  | type_specifier annotation {
	$$ := $1;
	Annotation.Add_Annotation($$.Tree, $2.List);
    }
  | operation_type_specifier {
        $$ := $1;
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
  | type_instantiation { 
	$$ := $1;
    }
  | type_instantiation EXTENDS_kw type_specifier { 
	$$ := (One_Tree, Invocation.Add_Extends(
	  Instantiation => $1.Tree, 
	  Extends => $3.Tree));
    }
  ;

opt_aliased_basic_type_specifier : 
    ALIASED_kw type_name { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Qualifier.Is_Ref => True, others => False), 
	  Operand => $2.Tree));
    }
  | basic_type_specifier {
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (others => False), 
	  Operand => $1.Tree));
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
  | type_qualifier type_instantiation { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Is_Optional => $1.Is_Optional,
	    Is_Concurrent => $1.Is_Concurrent,
	    others => False), 
	  Operand => $2.Tree));
    }
  | type_qualifier type_instantiation 
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
    OPTIONAL_kw opt_PROTECTED_kw { 
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Optional => True, 
	  Is_Concurrent => $2.Is_Present,
	  others => False);
    }
  | PROTECTED_kw {
	$$ := (Construct_Qualifier, 
               Source_Pos => $1.Source_Pos,
               Is_Concurrent => True, others => False);
    }
  ;

opt_PROTECTED_kw : 
    PROTECTED_kw {
	$$ := (Optional, True);
    }
  | {
	$$ := (Optional, False);
    }
  ;

operation_type_specifier :  -- TBD: "optional" and "queued" not handled yet
    PROC_kw operation_inputs {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Proc_Type_Specifier,
	  Operation_Inputs => $2.List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    }
  | FUNC_kw operation_inputs RETURN_kw operation_outputs {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Func_Type_Specifier,
	  Operation_Inputs => $2.List,
	  Operation_Outputs => $4.List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    }
  ;

opt_new_pkg_spec_element_list : { $$ := (One_List, Lists.Empty_List); }
  | NEW_kw_opt_NL pkg_spec_element_list opt_annotation {
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

pkg_spec_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | pkg_spec_element_list opt_annotation_opt_term pkg_spec_element_with_term {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.List);
    }
  | pkg_spec_element_list
     opt_annotation_opt_term operation_import SEMI_or_NEWLINE {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.Tree);
    }
  | pkg_spec_element_list
      opt_annotation_opt_term operation_equiv SEMI_or_NEWLINE {
	$$ := $1;
	if not Lists.Is_Empty($2.List) then
	    -- Add annotation to pkg_spec_element
	    Annotation.Add_Annotation(
	      $3.Tree, $2.List, Precedes => True);
	end if;
	Lists.Append($$.List, $3.Tree);
    }
--   | pkg_spec_element_list annotation_with_term {
-- 	$$ := $1;
-- 	Lists.Append($$.List, Annotation.Make(Annotations => $2.List));
--     }
  | pkg_spec_element_list error SEMI_or_NEWLINE {
	$$ := $1;
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
pkg_spec_element_with_term : 
    simple_pkg_spec_element SEMI_or_NEWLINE { $$ := $1; }
  | package_declaration_with_term {
        $$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
simple_pkg_spec_element :
    operation_declaration { $$ := (One_List, Lists.Make((1 => $1.Tree))); }
  | object_declaration { $$ := $1; }
  | type_declaration { $$ := $1; }
  | subtype_declaration { $$ := (One_List, Lists.Make((1 => $1.Tree))); }
  | use_clause {
        --  TBF: We shouldn't just ignore use clauses!
        $$ := (One_List, Lists.Empty_List);
    }
  | pragma { $$ := (One_List, Lists.Make((1 => $1.Tree))); }
  ;

--  NOTE: These were originally allowed within an annotation_element
--  good_simple_pkg_spec_element :
--     operation_declaration { $$ := $1; }
--   | var_or_const_object_declaration { $$ := $1; }
--   | type_declaration { $$ := $1; }
--   | subtype_declaration { $$ := $1; }
--   ;

opt_restricted_element_list :
    restricted_element_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

restricted_element_list :
    IMPLEMENTS_kw_opt_NL pkg_spec_element_list opt_annotation_opt_term { 
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
  ;

IMPLEMENTS_kw_opt_NL : IMPLEMENTS_kw | IMPLEMENTS_kw NEWLINE ;

opt_restricted_pkg_body_element_list :
    restricted_pkg_body_element_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

restricted_pkg_body_element_list :
    IMPLEMENTS_kw exported_pkg_body_element_list { 
      declare
	Elem_List : Lists.List := $2.List;
      begin
	$$ := (One_List, Lists.Make((1 => Implements_Element.Make(
	  For_Interfaces => Lists.Empty_List, 
	  Elements => Elem_List))));
      end;
    }
  ;

operation_import :
    operation_declaration IS_kw_ignore_INDENT import_operation {
      $$ := (One_Tree, Operation.Add_Import_Info(
	Op_Decl => $1.Tree, Import_Info => $3.List));
    }
  ;

IS_kw_ignore_INDENT :
    IS_kw
  | IS_kw INDENT {
        --  Pop the indent stack
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;
    }
  ;
operation_equiv :
    operation_declaration IS_kw expanded_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
    }
  | operation_declaration RENAMES_kw expanded_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
    }
  | operation_declaration IS_kw OF_kw basic_type_specifier {
	-- Indicate that operation should be found in given type
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $4.Tree));
    }
  | operation_declaration IS_kw expanded_name OF_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
    }
  | operation_declaration IS_kw '(' opt_record_component_list ')' {
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
	end;
    }
  | operation_declaration IS_kw_INDENT expanded_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
    }
  | operation_declaration IS_kw_INDENT expanded_name OF_kw basic_type_specifier {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
	$$ := (One_Tree, Operation.Add_Op_Location(
	  Op_Decl => $1.Tree, Op_Location => $5.Tree));
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
    }
  | operation_declaration IS_kw_INDENT '(' opt_record_component_list ')' {
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
            if Sparkel_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Sparkel_Lex.Top := Sparkel_Lex.Top - 1;  --  Pop the indent stack
	end;
    }
  ;

opt_new_pkg_body_element_list : { $$ := (One_List, Lists.Empty_List); }
  | NEW_kw_opt_NL exported_pkg_body_element_list {
	if Lists.Is_Empty($2.List) then
	    -- We want to make sure that we return a non-empty list
	    $$ := (One_List, Lists.Make((1 => Null_Optional_Tree)));
	else
	    $$ := $2;
	end if;
    }
  ;

INDENT_pkg_body_element_list : 
  EXPORTS_kw INDENT 
    exported_pkg_body_element_list {
	$$ := (Two_Lists, Lists.Empty_List, $3.List);
    }
--   | INDENT local_pkg_body_element_list
--   EXPORTS_kw_NEWLINE
--     exported_pkg_body_element_list {
-- 	$$ := (Two_Lists, $2.List, $4.List);
--     }
  | INDENT local_pkg_body_element_list
    opt_annotation
  EXPORTS_kw_NEWLINE
    exported_pkg_body_element_list {
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
  | INDENT local_pkg_body_element_list {
	--  yyerror("Missing ""exports"" keyword");
	$$ := (Two_Lists, Lists.Empty_List, $2.List);
    }
  ;

EXPORTS_kw_NEWLINE : EXPORTS_kw | EXPORTS_kw NEWLINE | EXPORTS_kw INDENT ;

local_pkg_body_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | local_pkg_body_element_list local_pkg_body_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  ;

--  NOTE: The following returns a List rather than a Tree
local_pkg_body_element_with_term : 
    pkg_spec_element_with_term { $$ := $1; } 
  | operation_import SEMI_or_NEWLINE {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | operation_equiv SEMI_or_NEWLINE {
        $$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | annotated_exported_pkg_body_element_with_term { $$ := $1; }
  ;
  
exported_pkg_body_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | exported_pkg_body_element_list
      annotated_exported_pkg_body_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  | exported_pkg_body_element_list operation_import SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_pkg_body_element_list operation_equiv SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
    }
  | exported_pkg_body_element_list pkg_spec_element_with_term {
	yyerror("This kind of declaration not permitted after ""exports""",
          At_Token => $2);
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  | exported_pkg_body_element_list error SEMI_or_NEWLINE {
	$$ := $1;
    }
  ;

annotated_exported_pkg_body_element_with_term : 
    exported_pkg_body_element_with_term { $$ := $1; }
  | annotation_with_term { 
	$$ := (One_Tree, Annotation.Make(Annotations => $1.List));
    }
  | annotation exported_pkg_body_element_with_term {
	$$ := $2;
	Annotation.Add_Annotation($$.Tree, $1.List, Precedes => True);
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
exported_pkg_body_element_with_term : 
    operation_definition_with_term {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | object_definition_with_term  { $$ := $1; }
  | package_body_with_term  {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  ;
  
package_body_with_term :
   opt_PRIVATE_kw PACKAGE_kw BODY_kw package_defining_name 
   IS_kw
      INDENT_pkg_body_element_list
      opt_new_pkg_body_element_list
      opt_restricted_pkg_body_element_list
   OUTDENT_opt_NEWLINE END_PACKAGE {
        -- TBD: allow an annotation after pkg_body_element_list
	$$ := (One_Tree, PSC.Trees.Module.Make(
	  Name => Name_For_Module($4.Tree),
	  Add_On_Label => Add_On_For_Module($4.Tree),
	  Is_Interface => False,
	  Is_Abstract => False,
	  Is_Private => $1.Is_Present,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => $6.First_List,
	  Module_Exports => $6.Second_List,
	  Module_New_Exports => $7.List,
	  Module_Implements => $8.List));
	    -- NOTE: Module_Implements is where bodies would go
	    --       if there is some ambiguity between operations that
	    --       are in the "normal" pkg_spec part vs. in the
	    --       "implements" part of the pkg_spec.

        if $10.Check_Label then
	    Check_Id_Match(Starting_Id => Name_For_Module($4.Tree),
	      Ending_Id => $10.Label);
        end if;
    } 
  ; 

opt_PRIVATE_kw : {
        $$ := (Optional, Is_Present => False); 
    }
  | PRIVATE_kw {
        $$ := (Optional, Is_Present => True);
    }
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
    condition { $$ := $1; }
  | quantified_expression { $$ := $1; }
  | annotation {
	-- Nested annotations are intended to represent
	-- "correctness" rather than "safety" concerns,
	-- and as such are *not* required to be provable 
	-- at compile-time, though a warning is expected,
	-- and a debugger breakpoint if running in debug mode.
	$$ := (One_Tree, Annotation.Make(Annotations => $1.List));
    }
  ;
--  NOTE: These were originally included in annotation_element above
--     good_simple_pkg_spec_element { $$ := $1; }
--   | operation_import { $$ := $1; }
--   | operation_equiv { $$ := $1; }

condition : expression { $$ := $1; } ;

opt_ABSTRACT_or_OPTIONAL_kw :
    ABSTRACT_kw { 
	$$ := (Construct_Qualifier,
               Source_Pos => $1.Source_Pos,
               Is_Abstract => True, others => False); 
    }
  | OPTIONAL_kw { 
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
  | OPTIONAL_kw opt_QUEUED_kw { 
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
    string_lit { 
	$$ := $1;
    }
  | id {
	yyerror("Operator designator must be in quotes");
	$$ := $1;
    }
  ;
  
operation_declaration :
    opt_ABSTRACT_or_OPTIONAL_or_QUEUED_kw basic_operation_declaration {
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

basic_operation_declaration :
    PROC_kw operation_designator operation_inputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $2.Tree,
	  Operation_Kind => Operation.Proc_Operation,
	  Operation_Inputs => $3.List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Annotation.Make ($4.List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    }
  | FUNC_kw operation_designator operation_inputs 
      opt_annotation RETURN_kw operation_outputs opt_annotation {
	$$ := (One_Tree, Operation.Make(
	  Name => $2.Tree,
	  Operation_Kind => Operation.Func_Operation,
	  Operation_Inputs => $3.List,
	  Operation_Outputs => $6.List,
	  Preconditions => Annotation.Make ($4.List),
	  Postconditions => Annotation.Make ($7.List),
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    }
  ;

operation_designator : 
    expanded_name {
	$$ := $1;
    }
  ;
  
operation_inputs :
    simple_operation_input { 
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
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
  | {
	yyerror("Sparkel requires at least ""()"" in operation definition");
	$$ := (One_List, Lists.Empty_List);
    }
  ;

right_paren :
    ')' { $$ := $1; }
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
	  Param_Type => $5.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  | opt_input_mode input_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
	  Param_Type => $3.Tree,
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
  | global_mode { $$ := $1; }
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

global_mode :
    GLOBAL_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | GLOBAL_kw VAR_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Global_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  ;

opt_annotated_operation_input_list : 
    annotated_operation_input_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
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
	if not Lists.Is_Empty($2.List) then
	    -- Add annotations to last element of list
	    $$ := $1;
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $2.List);
	else
	    $$ := $1;
	end if;
    }
  | annotation operation_input opt_annotation {
	-- Add annotations to first/last element of list
	$$ := $2;
	Annotation.Add_Annotation(
	  Lists.Nth_Element($$.List, 1), $1.List, Precedes => True);
	if not Lists.Is_Empty($3.List) then
	    Annotation.Add_Annotation(
	      Lists.Nth_Element($$.List, Lists.Length($$.List)), $3.List);
	end if;
    }
--   | operation_formal { 
-- 	$$ := (One_List, Lists.Make((1 => $1.Tree)));
--     }
--   | annotation operation_formal {
-- 	Annotation.Add_Annotation($2.Tree, $1.List, Precedes => True);
-- 	$$ := (One_List, Lists.Make((1 => $2.Tree)));
--     }
  ;

operation_input : 
    id_list COLON_no_indent
      opt_in_out_mode opt_input_type_qualifier
      operand_type_specifier opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
            --  TBF: aliased, not null needs to be passed along
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => $3.Param_Kind,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => $4.Is_Optional,
              In_Region => Copy_If_Not_First ($6.Tree, I),
	      Param_Type => Copy_If_Not_First ($5.Tree, I),
	      Param_Default => Copy_If_Not_First ($7.Tree, I)));
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
	      Param_Type => Copy_If_Not_First ($5.Tree, I),
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
	  Param_Type => $3.Tree,
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
	  Param_Type => $2.Tree,
	  Param_Default => $3.Tree))));
    }
  | '<' value_formal '>' {
	$$ := $2;
	-- Set Is_Implicit_Module_Param on each parameter
	for I in 1..Lists.Length($$.List) loop
	  declare
	    Param_Decl_Tree : Param_Decl.Tree renames
	      Param_Decl.Tree(Tree_Ptr_Of(Lists.Nth_Element($$.List, I)).all);
	  begin
	    Param_Decl_Tree.Is_Implicit_Module_Param := True;
	  end;
	end loop;
    }
  ;

opt_aliased_CONSTANT_kw :
    CONSTANT_kw {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  | ALIASED_kw CONSTANT_kw
    {
	$$ := (Construct_Qualifier,
                Source_Pos => $1.Source_Pos,
                Is_Ref => True,
                others => False);
    }
  ;

opt_aliased :
    {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               others => False);
    }
  | ALIASED_kw
    {
	$$ := (Construct_Qualifier,
                Source_Pos => $1.Source_Pos,
                Is_Ref => True,
                others => False);
    }
  ;

opt_in_out_mode :
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | IN_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Default_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | OUT_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Out_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | IN_kw OUT_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | ALIASED_kw {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | ALIASED_kw IN_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Const_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | ALIASED_kw OUT_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Out_Param,
	  Param_Locking => Param_Decl.Not_Locked);
    }
  | ALIASED_kw IN_kw OUT_kw
    {
	$$ := (Param_Mode, 
	  Param_Kind => Param_Decl.Ref_Var_Param,
	  Param_Locking => Param_Decl.Not_Locked);
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
  | type_instantiation { $$ := $1; }
  ;

operand_type_specifier : 
    simple_operand_type_specifier { $$ := $1; }
  | id IS_no_indent type_instantiation {
         -- NOTE: Operation can have "type" parameters 
         -- such as "Left_Type is Integer<>"
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $1.Tree,
	  Is_New_Type => False,
	  Type_Definition => $3.Tree));
    }
  | operation_type_specifier {
        $$ := $1;
    }
  ;

input_type_qualifier : 
    OPTIONAL_kw {
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Optional => True,
	  others => False);
    }
  | NOT_kw NULL_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Not_Null => True,
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
    OPTIONAL_kw {
	$$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Optional => True,
	  others => False);
    }
  | NOT_kw NULL_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Not_Null => True,
	  others => False);
    }
  ;


opt_VAR_kw : 
    VAR_kw { $$ := (Optional, Is_Present => True); }
  | { $$ := (Optional, Is_Present => False); }
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
	  Param_Type => $5.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  | output_mode output_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => $1.Param_Kind,
	  Locking => $1.Param_Locking,
	  Is_Optional => $2.Is_Optional,
	  Param_Type => $3.Tree,
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
	  Param_Type => $4.Tree,
	  Param_Default => Null_Optional_Tree));
    }
  | output_type_qualifier simple_operand_type_specifier {
	$$ := (One_Tree, Param_Decl.Make(
	  Name => Null_Optional_Tree,
	  Kind => Param_Decl.Default_Param,
	  Locking => Param_Decl.Not_Locked,
	  Is_Optional => $1.Is_Optional,
	  Param_Type => $2.Tree,
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
	      Param_Type => Copy_If_Not_First ($4.Tree, I),
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
	      Param_Type => Copy_If_Not_First ($5.Tree, I),
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
	  Param_Type => $3.Tree,
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
	  Param_Type => $2.Tree,
	  Param_Default => Null_Optional_Tree))));
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
object_declaration :
    var_or_const_object_declaration {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | ada_style_object_declaration { $$ := $1; }
  ;

var_or_const_object_declaration : 
    VAR_kw id COLON_no_indent annotated_type_specifier 
      opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
          Is_Global => True,  --  Only relevant if package element
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => $5.Tree,
	  Obj_Type => $4.Tree,
	  Obj_Value => $6.Tree));
    }
  | non_var_object_declaration {
        $$ := $1;
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
ada_style_object_declaration :
    id_list_with_colon
      opt_aliased_basic_type_specifier 
      opt_ASSIGN_expression {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           Lists.Append ($$.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element($1.List, I))),
             Is_Var => True,
             Is_Global => True,  --  Only relevant if at package level
             Is_Const => False,
             Is_Ref => PSC.Trees.Qualifier.Qualifiers ($2.Tree)(Is_Ref),
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First
               (PSC.Trees.Qualifier.Unqualified_Tree ($2.Tree), I),
             Obj_Value => Copy_If_Not_First ($3.Tree, I)));
        end loop;
    }
  | id_list_with_colon
      basic_type_specifier 
      RENAMES_kw expanded_name {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           Lists.Append ($$.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element($1.List, I))),
             Is_Var => False,
             Is_Const => False,
             Is_Ref => True,
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First ($2.Tree, I),
             Obj_Value => Copy_If_Not_First ($4.Tree, I)));
        end loop;
    }
  | id_list_with_colon opt_aliased_CONSTANT_kw
      basic_type_specifier 
      opt_ASSIGN_expression {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           Lists.Append ($$.List,  Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element($1.List, I))),
             Is_Var => False,
             Is_Const => True,
             Is_Ref => $2.Is_Ref,
             Is_Optional => False, -- TBD
             Obj_Type => Copy_If_Not_First ($3.Tree, I),
             Obj_Value => Copy_If_Not_First ($4.Tree, I)));
        end loop;
    }
  | id_list_with_colon opt_aliased_CONSTANT_kw
      ASSIGN_or_equal expression {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           Lists.Append ($$.List, Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree
                       (Tree_Of(Lists.Nth_Element($1.List, I))),
             Is_Var => False,
             Is_Const => True,
             Is_Ref => $2.Is_Ref,
             Is_Optional => False, -- TBD
             Obj_Type => Null_Optional_Tree,
             Obj_Value => Copy_If_Not_First ($4.Tree, I)));
        end loop;
    }
  | id_list_with_colon
      EXCEPTION_kw {
        --  An exception turns into "type Id is new Exception_Type<>;"
        --  and when an exception is raised, we create an object of the
        --  type.  Exception_Type+ represents an Exception_Occurrence.
        --  The full name of the exception type as a Univ_Enumeral
        --  is the Exception_Id (what about "."s? -- Perhaps #"A.B.C")

        $$ := (One_List, Lists.Empty_List);

        for I in 1 .. Lists.Length ($1.List) loop
           declare
              Nth_Name : constant Optional_Tree :=
                                    Lists.Nth_Element($1.List, I);
           begin
              Lists.Append ($$.List, Type_Decl.Make(
                Name => Nth_Name,
                Is_New_Type => True,
                Type_Definition => Invocation.Make(
                  Kind => Invocation.Module_Instantiation,
                  Prefix => PSC.Trees.Identifier.Make
                    ("Exception_Type", Find_Source_Pos (Nth_Name)),
                  Operands => Lists.Empty_List)));
           end;
        end loop;
    }
  ;

non_var_object_declaration :
    CONST_kw id COLON_no_indent annotated_type_specifier 
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
      opt_REFERS_TO_name {
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
      opt_REFERS_TO_name {
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
      opt_REFERS_TO_name {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => $4.Tree,
	  Obj_Value => $5.Tree));
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
   
opt_REFERS_TO_name :
    REFERS_TO name { $$ := $2; }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

id_list_with_colon :
    label_opt_NL {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | id ',' id_list_with_colon {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
        Lists.Append ($$.List, $3.List);
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
object_definition_with_term : object_definition SEMI_or_NEWLINE { $$ := $1; };

--  NOTE: The following returns a List rather than a Tree:
object_definition :
    VAR_kw id opt_region_specifier ASSIGN_or_equal expression {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => $3.Tree,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree))));
    }
  | REF_kw CONST_kw id REFERS_TO name {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree))));
    }
  | REF_kw VAR_kw id REFERS_TO name {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($3.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $5.Tree))));
    }
  | REF_kw id REFERS_TO name {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => False,
	  Is_Ref => True,
	  Is_Optional => False, -- TBD
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree))));
    }
  | CONST_kw id MOVE movable_object {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => False,
	  Is_Const => True,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree))));
    }
  | VAR_kw id MOVE movable_object {
	$$ := (One_List, Lists.Make ((1 => Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  Is_Move => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $4.Tree))));
    }
  ;

movable_object : expression { $$ := $1; };

--  NOTE: The following returns a List rather than a Tree:
type_declaration : 
    type_derivation {
        $$ := $1;
    }
  | TYPE_kw id IS_no_indent '(' opt_package_actual_list ')' {
        declare
           Enum_Names : constant Lists.List := $5.List;
           Enum_Lits : constant Lists.List :=
                           Make_Enum_Literals (Enum_Names);

           --  Enum-function result type
	   Enum_Func_Result : constant Optional_Tree :=
             Param_Decl.Make(
                Name => Null_Optional_Tree,
                Kind => Param_Decl.Default_Param,
                Locking => Param_Decl.Not_Locked,
                Is_Optional => False,
                Param_Type => PSC.Trees.Copy_Tree ($2.Tree),
                Param_Default => Null_Optional_Tree);
        begin
           --  Make the type decl
           $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
             Name => $2.Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               Prefix => PSC.Trees.Identifier.Make ("Enum", $3.Source_Pos),
               Operands => Lists.Make
                 ((1 => Invocation.Make(Invocation.Container_Aggregate,
                           Prefix => Null_Optional_Tree,
                           Operands => Enum_Lits,
                           Source_Pos => $4.Source_Pos))))))));

           --  Now append declarations for enum-lit parameterless functions.
           for I in 1 .. Lists.Length (Enum_Lits) loop
              declare
                 Nth_Enum : constant Optional_Tree :=
                   Lists.Nth_Element (Enum_Names, I);
              begin
                 Lists.Append ($$.List, Operation.Make(
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
    }
  | TYPE_kw id IS_no_indent
      opt_ABSTRACT_kw record_definition {  -- TBD: discrims, abstract
        declare
           Rec_Mod : Module.Tree
             renames Module.Tree (Tree_Ptr_Of ($5.Tree).all);
        begin
           --  Fill in name and is-abstract
           Rec_Mod.Name := $2.Tree;
           Rec_Mod.Is_Abstract := $4.Is_Abstract;

           --  Now build a type-decl
           $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
             Name => $2.Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               --  NOTE: Private/Record type decl has module as Prefix of inst
               Prefix => $5.Tree,
               Operands => Lists.Empty_List)))));
        end;
    }
  | TYPE_kw id IS_no_indent operation_type_specifier {  -- TBD: discrims
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => $4.Tree))));
    }
  | TYPE_kw id IS_no_indent
      opt_ABSTRACT_kw opt_LIMITED_or_PROTECTED_kw PRIVATE_kw {  -- TBD: discrims
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            --  NOTE: Private/Record type decl has module as Prefix of inst
            Prefix => Module.Make(
              Name => $2.Tree,
              Add_On_Label => Lists.Empty_List,
              Is_Interface => True,
              Is_Abstract => $4.Is_Abstract,
              Is_Private => True,
              Is_Concurrent => $5.Is_Concurrent,
              Is_Limited => $5.Is_Limited,
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
    }
  | TYPE_kw id IS_no_indent
      ARRAY_kw '(' index_subtype_list ')' OF_kw basic_type_specifier {
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Array_Type", Token_Src_Pos ($4)),
            Operands => Lists.Make
                          (($9.Tree, Make_Array_Indexer ($6.List))))))));
    }
  | TYPE_kw id IS_no_indent ACCESS_kw opt_ALL_or_CONSTANT_kw
      annotated_type_specifier {
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => $5.Tree,
            Operands => Lists.Make
                          ((1 => $6.Tree)),
            Source_Pos => $4.Source_Pos)))));
    }
  ;

index_subtype_list :
    index_subtype {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | index_subtype_list ',' index_subtype {
        $$ := $1;
        Lists.Append ($$.List, $3.List);
    }
  ;

index_subtype :
    simple_expression_component {
        $$ := (One_Tree, Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Integer", Find_Source_Pos ($1.Tree)),
            Operands => Lists.Make ((1 => $1.Tree))));
    }
  | type_specifier RANGE_kw BOX {
        $$ := $1;
	Annotation.Add_Annotation ($$.Tree,
          Lists.Make
             ((1 => Binary.Make(Binary.Closed_Interval_Op,
                      Left_Operand => Null_Optional_Tree,
                      Right_Operand => Null_Optional_Tree))));
    }
  | type_specifier RANGE_kw simple_expression_component {
        $$ := $1;
	Annotation.Add_Annotation($$.Tree, Lists.Make ((1 => $3.Tree)));
    }
  ;

opt_ABSTRACT_kw : {
        $$ := (Construct_Qualifier,
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
          others => False);
    }
  | ABSTRACT_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Abstract => True,
          others => False);
    }
  ;

opt_LIMITED_or_PROTECTED_kw : 
    LIMITED_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Limited => True,
          others => False);
    }
  | PROTECTED_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Concurrent => True,
          others => False);
    }
  | {
        $$ := (Construct_Qualifier,
          Source_Pos => PSC.Source_Positions.Null_Source_Position,
          others => False);
    }
  ;

opt_ALL_or_CONSTANT_kw :
    ALL_kw {
        $$ := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_All_Type", $1.Source_Pos));
    }
  | CONSTANT_kw {
        $$ := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_Const_Type", $1.Source_Pos));
    }
  | {
        $$ := (One_Tree, PSC.Trees.Identifier.Make
                ("Access_Type", PSC.Syntax.Cur_Source_Pos));
    }
  ;


--  NOTE: The following returns a List rather than a Tree:
type_derivation :
    TYPE_kw id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_PROTECTED_kw
      parent_type_specifier {
        -- TBD: discrims, abstract
	$$ := (One_List, Lists.Make ((1 => Type_Decl.Make
	  (Name => $2.Tree,
	   Is_New_Type => True,
           Type_Definition => Invocation.Make
            (Kind => Invocation.Module_Instantiation,
             --  NOTE: type decl has module as Prefix of inst
             Prefix => PSC.Trees.Module.Make(
               Name => $2.Tree,
               Add_On_Label => Lists.Empty_List,
               Is_Interface => True,
               Is_Abstract => $4.Is_Abstract,
               Is_Private => False,
               Is_Concurrent => $5.Is_Concurrent,
               Is_Limited => $5.Is_Limited,
               Has_Formals => False,
               Treat_As_Type => True,
               Module_Formals => Lists.Empty_List,
               Extends_Interface => $6.Tree,
               Implements_Interfaces => Lists.Empty_List,
               Class_Locals => Lists.Empty_List,
               Module_Exports => Lists.Empty_List,
               Module_New_Exports => Lists.Empty_List,
               Module_Implements => Lists.Empty_List),
             Operands => Lists.Empty_List)))));
    }
  | TYPE_kw id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_PROTECTED_kw
      parent_type_specifier WITH_kw PRIVATE_kw {
        -- TBD: discrims, abstract
	$$ := (One_List, Lists.Make ((1 => Type_Decl.Make
	  (Name => $2.Tree,
	   Is_New_Type => True,
           Type_Definition => Invocation.Make
            (Kind => Invocation.Module_Instantiation,
             --  NOTE: type decl has module as Prefix of inst
             Prefix => PSC.Trees.Module.Make(
               Name => $2.Tree,
               Add_On_Label => Lists.Empty_List,
               Is_Interface => True,
               Is_Abstract => $4.Is_Abstract,
               Is_Private => True,
               Is_Concurrent => $5.Is_Concurrent,
               Is_Limited => $5.Is_Limited,
               Has_Formals => False,
               Treat_As_Type => True,
               Module_Formals => Lists.Empty_List,
               Extends_Interface => $6.Tree,
               Implements_Interfaces => Lists.Empty_List,
               Class_Locals => Lists.Empty_List,
               Module_Exports => Lists.Empty_List,
               Module_New_Exports => Lists.Empty_List,
               Module_Implements => Lists.Empty_List),
             Operands => Lists.Empty_List)))));
    }
  | TYPE_kw id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_PROTECTED_kw
      parent_type_specifier
      WITH_kw record_definition {  -- TBD: discrims
        declare
           Rec_Mod : Module.Tree
             renames Module.Tree (Tree_Ptr_Of ($8.Tree).all);
        begin
           --  Fill in name and other info
           Rec_Mod.Name := $2.Tree;
           Rec_Mod.Is_Abstract := $4.Is_Abstract;
           Rec_Mod.Is_Limited := $5.is_Limited;
           Rec_Mod.Is_Concurrent := $5.is_Concurrent;
           Rec_Mod.Extends_Interface := $6.Tree;

           --  Now build a type-decl
           $$ := (One_List, Lists.Make ((1 => Type_Decl.Make (
             Name => $2.Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               --  NOTE: type decl has module as Prefix of inst
               Prefix => $8.Tree,
               Operands => Lists.Empty_List)))));
        end;
    }
  ;

parent_type_specifier :
    NEW_kw annotated_basic_type_specifier {
        --  We use a param-decl for the parent type for uniformity
        $$ := (One_Tree, Tree => Param_Decl.Make
           (Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => $2.Tree,
            Param_Default => Null_Optional_Tree));
    }
  | annotated_basic_type_specifier {
        --  We use a param-decl for the parent type for uniformity
        yyerror("""new"" required unless defining a subtype",
           At_Token => $1);
        $$ := (One_Tree, Tree => Param_Decl.Make
           (Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => $1.Tree,
            Param_Default => Null_Optional_Tree));

    }
  ;

record_definition : 
    opt_LIMITED_or_PROTECTED_kw NULL_kw RECORD_no_indent {
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Null_Optional_Tree,
	  Add_On_Label => Lists.Empty_List,
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
	  Is_Concurrent => $1.Is_Concurrent,
          Is_Limited => $1.Is_Limited,
	  Has_Formals => False,
          Treat_As_Type => True,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Lists.Empty_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    }
  | opt_LIMITED_or_PROTECTED_kw
      RECORD_kw INDENT
        record_component_decl_list
      OUTDENT_opt_NEWLINE END_RECORD {
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Null_Optional_Tree,
	  Add_On_Label => Lists.Empty_List,
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
	  Is_Concurrent => $1.Is_Concurrent,
          Is_Limited => $1.Is_Limited,
	  Has_Formals => False,
          Treat_As_Type => True,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => $4.List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => Lists.Empty_List));
    }
  ;

record_component_decl_list :
    record_component_decl SEMI_or_NEWLINE {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | record_component_decl_list record_component_decl SEMI_or_NEWLINE {
        $$ := $1;
        Lists.Append ($$.List, $2.Tree);
    }
  ;

record_component_decl :
    non_var_object_declaration {
        $$ := $1;
    }
  | id COLON_no_indent annotated_type_specifier 
      opt_region_specifier opt_ASSIGN_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($1.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False, -- TBD
	  In_Region => $4.Tree,
	  Obj_Type => $3.Tree,
	  Obj_Value => $5.Tree));
    }
  ;


END_RECORD :
    END_kw opt_RECORD_kw opt_id {
        $$ := (Optional_End_Token, 
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => Not_Null ($3.Tree),
                Label => $3.Tree, others => Null_Optional_Tree);
    }
  ;

opt_RECORD_kw : RECORD_kw { $$ := $1; }
  | {
	yyerror("Should be ""end record [id]"" rather than ""end [id]""");
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

subtype_declaration :
    SUBTYPE_kw id IS_kw annotated_type_specifier {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,
	  Type_Definition => $4.Tree));
    }
  ;

operation_definition_with_term : 
   operation_declaration IS_kw_INDENT
     statement_list_opt_eh
   OUTDENT_opt_NEWLINE END_FUNC_or_PROC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));

            if $5.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $5.Label);
                Check_Func_Proc_Match(Op_Decl, $5);
            end if;
	end;
    }
  | operation_declaration IS_kw_INDENT queued_clause_THEN
      statement_list_opt_eh
    OUTDENT_opt_NEWLINE END_FUNC_or_PROC {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
	    Op_Decl.Is_Def := True;
	    Op_Decl.Dequeue_Condition := $3.Tree;
	    Op_Decl.Statements := $4.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));

            if $6.Check_Label then
                Check_Id_Match(Starting_Id => Op_Decl.Name,
	          Ending_Id => $6.Label);
                Check_Func_Proc_Match(Op_Decl, $6);
            end if;
	end;
    }
 | operation_declaration IS_kw
     unindented_statement_list_opt_term 
   END_kw opt_FUNC_or_PROC_kw operation_designator SEMI_or_NEWLINE {
        declare
	    Op_Decl : Operation.Tree := 
	      Operation.Tree(Tree_Of($1.Tree));
	begin
            Parser_Warning ("Statements should be indented",
              At_Token => $3);
	    Op_Decl.Is_Def := True;
	    Op_Decl.Statements := $3.Tree;
	    $$ := (One_Tree, Optional(Op_Decl));

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => $6.Tree);
	end;
    }
  | operation_declaration IS_kw queued_clause_THEN
     unindented_statement_list_opt_term 
    OUTDENT_END_kw opt_FUNC_or_PROC_kw operation_designator SEMI_or_NEWLINE {
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

	    Check_Id_Match(Starting_Id => Op_Decl.Name,
	      Ending_Id => $7.Tree);
	end;
    }
  ;

opt_FUNC_or_PROC_kw :
    FUNC_kw {
        $$ := $1;
    }
  | PROC_kw {
        $$ := $1;
    }
  | {
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

opt_queued_clause : 
    queued_clause { $$ := $1; }
  | { $$ := (One_Tree, Null_Optional_Tree); }
  ;

queued_clause : 
    queued_clause_THEN { $$ := $1; }
  ;

queued_clause_THEN : queued_clause_condition THEN_kw_INDENT { $$ := $1; };

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
        if Sparkel_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Top := Sparkel_Lex.Top - 1;
    };

COLON_INDENT : EOL_COLON INDENT ;

THEN_kw_INDENT : THEN_kw INDENT | NEWLINE THEN_kw INDENT ;

END_FUNC_or_PROC :
    END_kw opt_FUNC_or_PROC_kw operation_designator SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
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

statement_list_opt_eh :
    statement_list_opt_term { $$ := $1; }
  | statement_list_opt_term
    use_BEGIN_kw NEWLINE
      parallel_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $4.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | use_BEGIN_kw NEWLINE
      parallel_sequence_opt_term_opt_eh {
	$$ := $3;
    }
  ;

statement_list_opt_term : 
    statement_list_with_term { $$ := $1; }
  | statement_list_no_term { $$ := $1; }
  ;

parallel_sequence_opt_term_opt_eh :
    parallel_sequence_opt_term { $$ := $1; }
  | parallel_sequence_with_eh { $$ := $1; }
  | parallel_sequence_with_term
    begin_statement
    parallel_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => Binary.Make(
            Operator => Binary.Next_Stmt_Op,
            Left_Operand => $1.Tree,
            Right_Operand => $2.Tree),
	  Right_Operand => $3.Tree));
    }
  | begin_statement
    parallel_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

parallel_sequence_with_eh :
    parallel_sequence_opt_term exception_handler {
        --  Use the Handled_Stmt_Op for exception handlers
        --  TBF: Should "hoist" out the "then" statement from LHS
        --       or restructure the grammar a bit.
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Handled_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree,
          Source_Pos => Find_Source_Pos ($2.Tree)));
    }
  ;

exception_handler :
    EXCEPTION_kw NEWLINE handler_list {
        $$ := (One_Tree, Case_Construct.Make(
           Source_Pos => $1.Source_Pos,
           Case_Selector => Null_Optional_Tree,
           Case_Alt_List => $3.List));
    }
  | EXCEPTION_kw handler_list {
        $$ := (One_Tree, Case_Construct.Make(
           Source_Pos => $1.Source_Pos,
           Case_Selector => Null_Optional_Tree,
           Case_Alt_List => $2.List));
    }
  ;

-- OUTDENT_EXCEPTION_kw : OUTDENT_opt_NEWLINE EXCEPTION_kw {
--         $$ := $2;
--     }
--   ;

handler_list :
    handler {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | handler_list handler  {
        $$ := $1;
        Lists.Append ($$.List, $2.Tree);
    }
  ;

handler :
    WHEN_kw simple_expression_opt_named REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  | WHEN_kw OTHERS_opt_named REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
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
  ;

THEN_no_indent : THEN_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

RECORD_no_indent : RECORD_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [record with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    }
  ;

THEN_no_indent_NL : THEN_no_indent {
        $$ := $1;
    }
  | THEN_no_indent NEWLINE {
        $$ := $1;
    };

ELSE_opt_COLON_no_indent : ELSE_no_INDENT | ELSE_kw COLON_no_INDENT ;

ELSE_no_indent : ELSE_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

use_BEGIN_kw : BEGIN_kw {
	$$ := $1;  --  Ignored for now
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
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
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

parallel_sequence_opt_term :
    parallel_sequence_with_term { $$ := $1; }
  | parallel_sequence_no_term { $$ := $1; }
  ;

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
	Annotation.Add_Annotation(Lists.Nth_Element($2.List, 1),
          $1.List, Precedes => True);
        --  Turn list into a statement sequence, separated by ";"s
        $$ := (One_Tree, Make_Statement_Sequence ($2.List));
    }
  | annotation opt_label_compound_statement_with_term {
	Annotation.Add_Annotation(Lists.Nth_Element($2.List, 1),
          $1.List, Precedes => True);
        $$ := $2;
    }
  | local_declaration_with_term {
        -- NOTE: these already allow trailing annotations
        --  Turn list into a statement sequence, separated by ";"s
        $$ := (One_Tree, Make_Statement_Sequence ($1.List));
    }
  | opt_label_compound_statement_with_term {
	$$ := $1;
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
local_declaration_with_term :
    local_declaration SEMI_or_NEWLINE { $$ := $1; };

opt_label_compound_statement_with_term : 
    local_definition_with_term {
        --  Turn list into a statement sequence, separated by ";"s
        $$ := (One_Tree, Make_Statement_Sequence ($1.List));
    }
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
  | EXIT_kw opt_compound_statement_kind opt_id opt_WITH_values
      WHEN_kw condition {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $6.Tree,
	  Then_Part => Control_Stmt.Make(
             Kind => Control_Stmt.Exit_Stmt,
             Applies_To => $2.Exitable_Construct,
             Id => $3.Tree,
             Values => $4.Tree,
             Source_Pos => $1.Source_Pos),
	  Else_Part => Null_Optional_Tree,
	  End_With_Values => Null_Optional_Tree,
	  Check_Label => False,
          Label => Null_Optional_Tree));
    }
  | RAISE_kw simple_expression opt_WITH_values {
        declare
           Operands : Lists.List;
        begin
           if Not_Null ($3.Tree) then
              Lists.Append (Operands, $3.Tree);
           end if;

           --  Exception_Type'Raise_Occurrence (Excep'Create([message]))
           $$ := (One_Tree, Invocation.Make(
             Kind => Invocation.Operation_Call,
             Prefix => Qualified_Name.Make(
               Prefix => PSC.Trees.Identifier.Make
                 ("Exception_Type", $1.Source_Pos),
               Id => PSC.Trees.Identifier.Make
                 ("Raise_Occurrence", $1.Source_Pos)),
             Operands => Lists.Make ((1 =>
                Invocation.Make(
                   Kind => Invocation.Operation_Call,
                   Prefix => Qualified_Name.Make(
                     Prefix => $2.Tree,
                      Id => PSC.Trees.Identifier.Make
                        ("Create", $1.Source_Pos)),
                   Operands => Operands)))));
        end;
    }   
  | simple_statement ')' {
        yyerror ("Extra ')'", At_Token => $2);
        $$ := $1;
    }
    
  ;

LOOP_no_indent : LOOP_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
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
  | '(' opt_record_component_list ')' ASSIGN expression {
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
  | ASSERT_kw expression {
	$$ := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => $2.Tree))));
    }
  | ASSERT_kw expression ',' string_lit {
	$$ := (One_Tree, Annotation.Make
                 (Annotations => Lists.Make((1 => $2.Tree)),
                  Label => $4.Tree));
    }
  | pragma { $$ := $1; }
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
  | PARALLEL_no_indent {
	$$ := (Construct_Kind, Control_Stmt.Block_Stmt); --  TBD: Parallel_Stmt
    }
  ;

BLOCK_no_indent : BLOCK_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [block with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

PARALLEL_no_indent : PARALLEL_kw {
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [parallel with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
    };

--  NOTE: The following returns a List rather than a Tree:
local_declaration : 
    operation_declaration { $$ := (One_List, Lists.Make ((1 => $1.Tree))); }
  | type_declaration { $$ := $1; }
  | object_declaration { $$ := $1; }
  | subtype_declaration { $$ := (One_List, Lists.Make ((1 => $1.Tree))); }
  ;

--  NOTE: The following returns a List rather than a Tree:
local_definition_with_term :
    object_definition_with_term { $$ := $1; }
  | operation_definition_with_term {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | operation_equiv SEMI_or_NEWLINE {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | package_declaration_with_term {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | package_body_with_term {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  ;

label_opt_NL : label { $$ := $1; }
  | label NEWLINE { $$ := $1; }
  | label INDENT { $$ := $1; }
  ;

label : id COLON_no_indent { $$ := $1; };

compound_statement_with_term :
    if_statement { $$ := $1; }
  | case_statement { $$ := $1; }
  | indefinite_loop_statement { $$ := $1; }
  | while_until_loop_statement { $$ := $1; }
  | for_loop_statement { $$ := $1; }
  | block_statement  { $$ := $1; }
  | declare_statement  { $$ := $1; }
  | parallel_statement  { $$ := $1; }
  | error SEMI_or_NEWLINE { $$ := (One_Tree, Null_Optional_Tree); }
  ;

if_statement : 
    IF_kw condition THEN_kw_INDENT
     statement_list_opt_term
  OUTDENT_opt_else_part END_IF {
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

OUTDENT_opt_else_part : 
    OUTDENT_ELSIF_kw condition THEN_kw_INDENT
      statement_list_opt_term 
    OUTDENT_opt_else_part {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
    }
  | OUTDENT_ELSE_kw_INDENT statement_list_opt_term
      OUTDENT_opt_NEWLINE {
	$$ := $2;
    }
  | OUTDENT_opt_NEWLINE {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

OUTDENT_ELSIF_kw : OUTDENT_opt_NEWLINE ELSIF_kw { $$ := $2; };

OUTDENT_ELSE_kw_INDENT :
    OUTDENT_opt_NEWLINE ELSE_kw INDENT ;

END_IF :
    END_kw IF_kw opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

case_statement : 
    CASE_kw simple_comparison_expression IS_kw_INDENT
      case_alt_list
      opt_default_alt
    OUTDENT_opt_NEWLINE END_CASE {
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

END_CASE :
    END_kw CASE_kw opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
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
    WHEN_kw simple_expression_opt_named REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  ;

REFERS_TO_with_indent : REFERS_TO {
        if Sparkel_Lex.Debug_Indent then
           Text_IO.Put(" [indent on] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Bracketing_Token := REFERS_TO;
        Sparkel_Lex.Expecting_Indent := True;
        $$ := $1;
    };

simple_expression_opt_named :
    simple_expression { $$ := $1; }
  | id COLON_no_indent
      simple_expression { 
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
    WHEN_kw OTHERS_kw_as_interval REFERS_TO_with_indent
      indented_statement_list_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  | {
	$$ := (One_Tree, Null_Optional_Tree);
    }
  ;

OTHERS_kw_as_interval :
    OTHERS_kw { 
	$$ := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => Null_Optional_Tree));
    }
  ;

indented_statement_list_with_term :
    INDENT statement_list_opt_term OUTDENT { $$ := $2; }
  | INDENT statement_list_opt_term OUTDENT NEWLINE { $$ := $2; }
  | statement_list_with_term { $$ := $1; }
  ;

OTHERS_opt_named :
    OTHERS_kw_as_interval { $$ := $1; }
  | id COLON_no_indent OTHERS_kw_as_interval {
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
	  Right_Operand => Null_Optional_Tree));
     };


indefinite_loop_statement :
    LOOP_kw INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {
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
    WHILE_or_UNTIL_kw condition LOOP_kw_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {
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

END_LOOP :
    END_kw LOOP_no_indent opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;


for_loop_statement :
    FOR_kw iterator_spec opt_annotation opt_direction
        LOOP_kw_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {
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
  | par_direction_and_chunk_opt_NL FOR_kw iterator_spec opt_annotation 
        LOOP_kw_INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {
	$$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $2.Source_Pos,
	  Kind => For_Loop_Construct.For_Loop_Statement,
	  Iterators => $3.List,
	  Filter => $4.List,
	  Loop_Body => $6.Tree,
	  Direction => Concurrent_Str,
          Chunk_Spec => $1.Tree,
	  End_With_Values => $8.End_With_Values,
          Check_Label => $8.Check_Label,
          Label => $8.Label));
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
        --  TBD: But not in Ada 202X!
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
        if Sparkel_Lex.Debug_Indent
          and then Sparkel_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        Sparkel_Lex.Expecting_Indent := False;
        $$ := $1;
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
    par_direction { $$ := $1; }
  | forward_or_reverse { $$ := $1; }
  ;

par_direction :
    PARALLEL_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos, Concurrent_Str);
    }
    ;

par_direction_and_chunk :
    par_direction {
	$$ := (One_Tree, Null_Optional_Tree); 
    }
  | par_direction '(' adding_expression ')' {
        $$ := $3;
    }
  | par_direction '(' index_set_iterator ')' {
        $$ := $3;
    }
  ;

par_direction_and_chunk_opt_NL :
    par_direction_and_chunk {
        $$ := $1;
    }
  |
    par_direction_and_chunk NEWLINE {
        $$ := $1;
    }
  ;

forward_or_reverse : 
    FORWARD_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos, Forward_Str);
    }
  | REVERSE_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos, Reverse_Str);
    }
  ;

block_statement :
    BLOCK_kw INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_BLOCK {
	$$ := (One_Tree, Block_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  Block_Body => $3.Tree,
	  End_With_Values => $5.End_With_Values,
          Check_Label => $5.Check_Label,
          Label => $5.Label));
    }
  ;

opt_BLOCK_no_indent : BLOCK_no_indent { $$ := $1; }
  | {
	yyerror("Should be ""end block <id>"" rather than ""end <id>""");
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;
   
END_BLOCK :
    END_kw opt_BLOCK_no_indent opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

declare_or_begin_statement :
    declare_statement { $$ := $1; }
  | begin_statement { $$ := $1; }
  ;

declare_statement :
    DECLARE_kw INDENT
      decl_list_with_term
    begin_statement {
        --  Replace block body with a "Then" statement
        --  where LHS is the list of declarations, and RHS
        --  is the handled-seq-of-statements.
        declare
           Begin_Stmt : Block_Stmt.Tree renames
             Block_Stmt.Tree (Tree_Ptr_Of ($4.Tree).all);
        begin
           $$ := $4;
           Begin_Stmt.Block_Body := Binary.Make(
             Operator => Binary.Then_Stmt_Op,
             Left_Operand => $3.Tree,
             Right_Operand => Begin_Stmt.Block_Body,
             Source_Pos => $1.Source_Pos);
        end;
    }
  ;

begin_statement :
    use_BEGIN_kw NEWLINE
      parallel_sequence_opt_term_opt_eh
    OUTDENT_opt_NEWLINE END_DECLARE {
	$$ := (One_Tree, Block_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  Block_Body => $3.Tree,
	  End_With_Values => $5.End_With_Values,
          Check_Label => $5.Check_Label,
          Label => $5.Label));
    }
  ;

END_DECLARE :
    END_kw opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => PSC.Strings.Null_U_String,
                Check_Label => True,
                Label => $2.Tree, End_With_Values => $3.Tree);
    }
  ;

decl_list_with_term :
    local_pkg_body_element_list {
        --  Convert list to a sequence of Next_Stmt_Op(A, B)
        if Lists.Length ($1.List) = 0 then
           $$ := (One_Tree, Null_Optional_Tree);
        else
           $$ := (One_Tree, Lists.Nth_Element ($1.List, 1));
           for I in 2 .. Lists.Length ($1.List) loop
              $$ := (One_Tree, Binary.Make(
                Operator => Binary.Next_Stmt_Op,
                Left_Operand => $$.Tree,
                Right_Operand => Lists.Nth_Element ($1.List, I)));
           end loop;
        end if;
    }
  ;

parallel_statement :
    PARALLEL_kw INDENT
      statement_list_opt_term
    OUTDENT_opt_NEWLINE END_PARALLEL {
	$$ := (One_Tree, Block_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  Block_Body => $3.Tree,
	  End_With_Values => $5.End_With_Values,
          Check_Label => $5.Check_Label,
          Label => $5.Label));
    }
  ;

opt_PARALLEL_no_indent : PARALLEL_no_indent { $$ := $1; }
  | {
	yyerror("Should be ""end parallel <id>"" rather than ""end <id>""");
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;
   
END_PARALLEL :
    END_kw opt_PARALLEL_no_indent opt_id opt_WITH_values SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree, End_With_Values => $4.Tree);
    }
  ;

expression :
    expression_no_err {
	$$ := $1;
    }
--   | expression_no_err divide_assign_as_not_equal expression_no_err { 
-- 	-- Treat "/=" equiv to "!=" in an expression
-- 	$$ := (One_Tree, Binary.Make(
-- 	  Operator => $2.Binary_Op,
-- 	  Left_Operand => $1.Tree,
-- 	  Right_Operand => $3.Tree));
--     }
  ;

divide_assign_as_not_equal :
    DIVIDE_ASSIGN { 
	-- Treat "/=" equiv to "!=" in an expression
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
    LAMBDA_kw lambda_opt_id_list IS_kw
       simple_expression_or_expr_stmt_seq {
	$$ := (One_Tree, Operation.Make(
	  Name => Null_Optional_Tree,
	  Operation_Kind => Operation.Lambda_Operation,
	  Operation_Inputs => $2.List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Def => True,
	  Statements => $4.Tree)); 
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
	  Operands => Lists.Make((1 => $2.Tree))));
    }
  ;

expr_statement_seq : expr_statement ';' expr_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  | expr_statement_seq ';' expr_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
     }
  ;

expr_statement : 
    return_stmt { $$ := $1; }
  | expression_no_err { $$ := $1; }
  ;

logical_expression :  
    comparison_expression { $$ := $1; }
  | logical_expression logical_operator comparison_expression {
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
	  Right_Operand => $3.Tree));
      end;
    }
  ;

simple_comparison_expression :  -- comparisons are non associative
    simple_expression { $$ := $1; }
  | simple_expression comparison_operator simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  ;

comparison_expression :
    simple_comparison_expression { $$ := $1; }
  | adding_expression IN_kw simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  | adding_expression NOT_kw IN_kw simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Not_In_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $4.Tree));
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

simple_expression : -- used to avoid use of '>' in package instantiation
    simple_expression_component { $$ := $1; }
  | simple_expression '|' simple_expression_component {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Combine_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  ;

simple_expression_component :
    adding_expression { $$ := $1; }
  | adding_expression interval_operator adding_expression { 
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
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
	  Right_Operand => $3.Tree));
    }
  | adding_expression adding_operator term {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  ;

term : 
    factor { $$ := $1; }
  | term multiplying_operator factor {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
    }
  ;

factor : 
    primary { $$ := $1; }
  | primary power_operator factor {
	 -- right associative
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree));
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
  | aggregate PRIME id_or_string_literal {
        --  An Sparkel Reduce expression
	$$ := (One_Tree, Qualified_Name.Make(
	  Prefix => $1.Tree,
	  Id => $3.Tree));
    }
  | aggregate PRIME id_or_string_literal '(' opt_operation_actual_list ')' {
        --  An Sparkel Reduce expression
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => Qualified_Name.Make(
             Prefix => $1.Tree,
             Id => $3.Tree),
	  Operands => $3.List));
    }
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
    expanded_name {
        $$ := $1;
    }
  | complex_name {
        $$ := $1;
    }
  ;

complex_name :
    name '(' opt_operation_actual_list ')' {
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
  | complex_name '.' selector {
	$$ := (One_Tree, Selection.Make(
	  Prefix => $1.Tree,
	  Selector => $3.Tree));
    }
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
  | '&' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Ampersand_Op); }
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
  | AMPERSAND_ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Ampersand_Assign_Op); 
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
	yyerror("Use "":="" rather than ""="" in Sparkel");
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
    }
  ;

comparison_operator : 
    COMPARE { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Compare_Op); }
  | '=' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Equal_Op); }
  | NEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.NEQ_Op); }
  | DIVIDE_ASSIGN { $$ := (One_Binary_Op, $1.Source_Pos, Binary.NEQ_Op); }
  | '<' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Less_Op); }
  | LEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.LEQ_Op); }
  | '>' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Greater_Op); }
  | GEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.GEQ_Op); }
  | LSHIFT { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Left_Shift_Op ); }
  | '>' '>' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Right_Shift_Op); }
  | EQ { 
	yyerror("Use ""="" rather than ""=="" in Sparkel");
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
    record_aggregate { $$ := $1; } 
  | container_aggregate { $$ := $1; } 
  ;

record_aggregate : 
    '(' opt_record_component_list ')' {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => Null_Optional_Tree,
	  Operands => $2.List,
          Source_Pos => $1.Source_Pos));
    }
--   | '(' name DIVIDE_ASSIGN expression ')' {
-- 	-- Treat "/=" equiv to "!=" in an expression
-- 	$$ := (One_Tree, Invocation.Make(
-- 	  Kind => Invocation.Class_Aggregate,
-- 	  Prefix => Null_Optional_Tree,
-- 	  Operands => Lists.Make((1 => Binary.Make(
-- 	    Operator => Binary.NEQ_Op,
-- 	    Left_Operand => $2.Tree,
-- 	    Right_Operand => $4.Tree)))));
--      }
  | expanded_name PRIME '(' opt_record_component_list ')' {
	-- Type of aggregate specified
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Class_Aggregate,
	  Prefix => $1.Tree,
	  Operands => $4.List,
          Source_Pos => $3.Source_Pos));
    }
  ;

opt_record_component_list : 
    record_component_list {
	$$ := $1;
    }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

record_component_list : 
    record_component {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | record_component_list ',' record_component {
	$$ := $1;
	Lists.Append($$.List, $3.Tree);
    }
  ;

record_component : 
    expression_no_err { $$ := $1; }
  | id REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | OTHERS_kw_as_interval REFERS_TO expression {
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
  | expanded_name PRIME '[' opt_container_element_list ']' {
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
  | FOR_kw iterator opt_annotation opt_forward_or_reverse
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
  IF_kw condition THEN_no_indent 
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
    ELSIF_kw condition THEN_no_indent 
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


case_expression : 
    CASE_kw simple_comparison_expression IS_no_indent
      case_expr_alt_list {
	$$ := (One_Tree, Case_Construct.Make(
          Source_Pos => $1.Source_Pos,
	  Case_Selector => $2.Tree,
	  Case_Alt_List => $4.List,
          Is_Case_Expr => True));
	Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
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
    WHEN_kw simple_expression_opt_named REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  | WHEN_kw OTHERS_opt_named REFERS_TO expression {
	-- NOTE: "others" alternative must come last
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  ;

quantified_expression :
    FOR_kw ALL_or_SOME_kw quantified_iterator opt_annotation REFERS_TO
      condition_or_quantified_expression {
	declare
	    Kind_Of_For_Loop: constant array(Boolean) of 
	      For_Loop_Construct.For_Loop_Kind_Enum := (
		False => For_Loop_Construct.Existential_Quantified_Expr,
		True => For_Loop_Construct.Univ_Quantified_Expr);
	begin
	    $$ := (One_Tree, For_Loop_Construct.Make(
              Source_Pos => $1.Source_Pos,
	      Kind => Kind_Of_For_Loop($2.Is_Present),
	      Iterators => Lists.Make((1 => $3.Tree)),
	      Filter => $4.List,
	      Loop_Body => $6.Tree));
            Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
	end;
    }
  | FOR_kw ALL_or_SOME_kw id opt_COLON_type_specifier REFERS_TO 
      condition_or_quantified_expression {
        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := $4.Tree;
     begin
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
  | FOR_kw ALL_or_SOME_kw id opt_COLON_type_specifier annotation REFERS_TO 
      condition_or_quantified_expression {
        -- This is a set iterator without the set, meaning it applies
        -- to all values of the given type, even if the type lacks
        -- a "universal" set.
     declare
        Obj_Type : Optional_Tree := $4.Tree;
     begin
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
    FOR_kw map_reduce_iterator opt_annotation opt_forward_or_reverse
      REFERS_TO expression {
	-- This does a map/reduce operation where the initial/next result
        -- is given in <...> and the overall expression represents the
        -- reduction to be performed on each element.
      declare
        use type PSC.Strings.U_String;
      begin
        if $4.Str /= PSC.Strings.Null_U_String then
            --  Record "forward" or "reverse" ("unordered" is the default)
            Iterator.Add_Direction($2.Tree, $4.Str);
        end if;

        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
          Kind => For_Loop_Construct.Map_Reduce_Expr,
          Iterators => Lists.Make((1 => $2.Tree)),
          Filter => $3.List,
          Loop_Body => $6.Tree));
        Set_Source_Pos($$.Tree, Source_Pos => $1.Source_Pos);
      end;
    }
  ;

map_reduce_iterator :
    index_set_iterator { $$ := $1; }
  | EACH_kw element_iterator { $$ := $2; }
  | initial_next_value_iterator { $$ := $1; }
  ;

%%


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

##%procedure_parse

end Sparkel_Parser;

