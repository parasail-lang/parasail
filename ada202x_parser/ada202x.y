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
-- YACC Grammar for Ada202x
--------------------------------------

-- Single-character delimiters --
%token ',' ';' ':' '.'
%token '+' '-' '*' '/' 
%token '(' ')' '[' ']' '<' '>'
%token '|' '&'
%token '='
%token '@'
%token PRIME -- '''

-- Compound delimiters --
%token EQ   -- "="
%token NEQ  -- "/="
%token GEQ  -- ">="
%token LEQ  -- "<="
%token LLABEL -- "<<"
%token RLABEL -- ">>"
%token POWER  -- "**"
%token ASSIGN -- ":="
%token BOX    -- "<>"
%token DOT_DOT -- ".."
%token REFERS_TO  -- "=>"

--  Significant indenting and newlines.
--  Newline is significant when *not* following then/else/is/loop
--  and next line is *not* indented (it can substitute for a ';').
--  A blank line ensures that next line is *not* considered a continuation line
--  Indent is significant after then, else, is, ...
%token NEWLINE
%token OUTDENT  -- used for "significant" out-denting
%token INDENT   -- used for "significant" indenting

-- Literals --
%token Char_Literal
%token Enum_Literal
%token Integer_Literal 
%token Real_Literal
%token String_Literal

-- Identifier --
%token Identifier 

-- Reserved words --
%token ABORT_kw
%token ABS_kw
%token ABSTRACT_kw
%token ACCEPT_kw
%token ACCESS_kw
%token ALIASED_kw
%token ALL_kw
%token AND_kw
%token ARRAY_kw
%token AT_kw
%token BEGIN_kw
%token BODY_kw
%token CASE_kw
%token CONSTANT_kw
%token DECLARE_kw
%token DELAY_kw
%token DELTA_kw
%token DIGITS_kw
%token DO_kw
%token ELSE_kw
%token ELSIF_kw
%token END_kw
%token ENTRY_kw
%token EXCEPTION_kw
%token EXIT_kw
%token FOR_kw
%token FUNCTION_kw
%token GENERIC_kw
%token GOTO_kw
%token IF_kw
%token IN_kw
%token INTERFACE_kw
%token IS_kw
%token LIMITED_kw
%token LOOP_kw
%token MOD_kw
%token NEW_kw
%token NOT_kw
%token NULL_kw
%token OF_kw
%token OR_kw
%token OTHERS_kw
%token OUT_kw
%token OVERRIDING_kw
%token PACKAGE_kw
%token PARALLEL_kw
%token PRAGMA_kw
%token PRIVATE_kw
%token PROCEDURE_kw
%token PROTECTED_kw
%token RAISE_kw
%token RANGE_kw
%token RECORD_kw
%token REM_kw
%token RENAMES_kw
%token REQUEUE_kw
%token RETURN_kw
%token REVERSE_kw
%token SELECT_kw
%token SEPARATE_kw
%token SOME_kw
%token SUBTYPE_kw
%token SYNCHRONIZED_kw
%token TAGGED_kw
%token TASK_kw
%token TERMINATE_kw
%token THEN_kw
%token TYPE_kw
%token UNTIL_kw
%token USE_kw
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
              Is_Const : Boolean := False;
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
    context_clause opt_PRIVATE_kw package_declaration_with_term {
        --  TBD: incorporate "Is_Private" flag
	Semantics.Add_Top_Level_Tree($3.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($3.Tree);
        end if;
    }
  | context_clause opt_PRIVATE_kw package_body_with_term {
        if $2.Is_Present then
           Parser_Warning
             ("""private"" goes only on the spec of a private child unit",
              At_Token => $2);
        end if;
	Semantics.Add_Top_Level_Tree($3.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($3.Tree);
        end if;
    }
  | context_clause opt_PRIVATE_kw standalone_operation_definition_with_term {
        --  TBD: incorporate "Is_Private" flag
	Semantics.Add_Top_Level_Tree($3.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($3.Tree);
        end if;
    }
  | context_clause opt_PRIVATE_kw generic_declaration_with_term {
        --  TBD: incorporate "Is_Private" flag
	Semantics.Add_Top_Level_Tree($3.Tree, Imports => $1.List);
        if PSC.Syntax.Echo_Input then
           New_Line;
           Dump_Subtree($3.Tree);
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
    PRAGMA_kw id '(' record_component_list ')' {
        declare
           Id : constant Optional_Tree := $2.Tree;
           Id_Str : constant String :=
             PSC.Strings.To_String
               (PSC.Trees.Identifier.Tree (Tree_Ptr_Of (Id).all).Str);
        begin
           if Id_Str = "Assert"
             and then Lists.Length ($4.List) in 1 .. 2
           then
              --  Special case of pragma Assert (condition[, message).
              --  Turn it into {*message* condition}
              declare
                 Label : Optional_Tree;
              begin
                 if Lists.Length ($4.List) > 1 then
                    --  Use message as the label
                    Label := PSC.Trees.Identifier.Make
                      (Subtree_Image (Lists.Nth_Element ($4.List, 2)),
                       Source_Pos =>
                         Find_Source_Pos (Lists.Nth_Element ($4.List, 2)));
                 else
                    --  Use "Assert" as the label
                    Label := Id;
                 end if;
                 $$ := (One_Tree, Annotation.Make
                         (Annotations => Lists.Make
                            ((1 => Lists.Nth_Element ($4.List, 1))),
                          Label => Label));
              end;
           else
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
           end if;
        end;
    }
  | PRAGMA_kw id {
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
        Lists.Append ($$.List, $2.List);
        --  TBD: use pragmas ($3.List)
    }
  ;

with_clause : WITH_kw imported_libunit_name_list SEMI_or_NEWLINE {
        $$ := $2;
    }
  ;
  
context_use_clause : USE_kw use_expanded_name_list SEMI_or_NEWLINE {
       $$ := $2;
    }
  ;

use_clause : USE_kw use_expanded_name_list {
       $$ := $2;
    }
  ;

use_expanded_name_list :
    expanded_name {
        --  Put a "*" on end of each "with"ed package.
        $$ := (One_List, Lists.Make ((1 =>
                Qualified_Name.Make (  -- TBD: Selection?
                  Prefix => $1.Tree,
                  Id => PSC.Trees.Identifier.Make
                          ("*", Find_Source_Pos ($1.Tree))))));
    }
  | use_expanded_name_list ',' expanded_name {
        --  Put a "*" on end of each "with"ed package.
        $$ := $1;
        Lists.Append ($$.List,
	  Qualified_Name.Make (  -- TBD: Selection?
	    Prefix => $3.Tree,
	    Id => PSC.Trees.Identifier.Make ("*", Find_Source_Pos ($3.Tree))));
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
  | formal_subprogram_declaration {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  ;

formal_type_declaration :
    TYPE_kw id IS_no_indent NEW_kw type_specifier {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => $5.Tree));
    }
  | TYPE_kw id IS_no_indent
      opt_LIMITED_or_SYNCHRONIZED_kw PRIVATE_kw {
        -- TBD: discrims
        if $4.Is_Limited or else $4.Is_Concurrent then
           --  Not necessarily Assignable
           $$ := (One_Tree, Type_Decl.Make(
             Name => $2.Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               Prefix => PSC.Trees.Identifier.Make
                 ("Any", Token_Src_Pos ($4)),
               Operands => Lists.Empty_List,
               Source_Pos => Token_Src_Pos ($4))));
        else
           --  Must be Assignable
           $$ := (One_Tree, Type_Decl.Make(
             Name => $2.Tree,
             Is_New_Type => True,
             Type_Definition => Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               Prefix => PSC.Trees.Identifier.Make
                 ("Assignable", Token_Src_Pos ($4)),
               Operands => Lists.Empty_List,
               Source_Pos => Token_Src_Pos ($4))));
        end if;
    }
  | TYPE_kw id IS_no_indent opt_not_null_qualifier access_type_def {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => Qualifier.Qualify
            (Qualifiers => (Is_Not_Null => $4.Is_Not_Null, others => False),
             Operand => $5.Tree)));
    }
  | TYPE_kw id IS_no_indent
      ARRAY_kw '(' index_subtype_list ')' OF_kw type_or_access_type_specifier {
        $$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Array_Type", Token_Src_Pos ($4)),
            Operands => Lists.Make
                          (($9.Tree, Make_Array_Indexer ($6.List))))));
    }
  | TYPE_kw id IS_no_indent RANGE_kw BOX {
        $$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Integer", Token_Src_Pos ($4)),
            Operands => Lists.Empty_List,
            Source_Pos => Token_Src_Pos ($4))));
    }
  | TYPE_kw id IS_no_indent MOD_kw BOX {
        $$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Modular", Token_Src_Pos ($4)),
            Operands => Lists.Empty_List,
            Source_Pos => Token_Src_Pos ($4))));
    }
  | TYPE_kw id IS_no_indent DIGITS_kw BOX {
        $$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,  --  Note, not really a "new" type
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Float", Token_Src_Pos ($4)),
            Operands => Lists.Empty_List,
            Source_Pos => Token_Src_Pos ($4))));
    }
  ;

formal_object_declaration :
    value_formal {
        $$ := $1;
    }
  ;

formal_subprogram_declaration :
    WITH_kw basic_operation_declaration {
        declare
           use type Operation.Operation_Kind_Enum;

           Subp_Tree : Operation.Tree renames Operation.Tree
             (Tree_Ptr_Of ($2.Tree).all);
           Subp_Kind_Array : constant array (Boolean)
             of Operation.Operation_Kind_Enum :=
             (False => Operation.Func_Type_Specifier,
              True => Operation.Proc_Type_Specifier);
           Is_Proc : constant Boolean :=
             Subp_Tree.Operation_Kind = Operation.Procedure_Operation;
        begin
           $$ := (One_Tree, Param_Decl.Make(
	      Name => Subp_Tree.Name,
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => $2.Tree,
	      Param_Default => Null_Optional_Tree));  --  TBD

           --  Turn Operation tree into a type specifier
           Subp_Tree.Operation_Kind := Subp_Kind_Array (Is_Proc);
           Subp_Tree.Name := Null_Optional_Tree;
        end;
    }
  ;

package_declaration_with_term : 
   package_instantiation_with_term { $$ := $1; }
 | PACKAGE_kw package_defining_name 
     IS_kw_INDENT
      pkg_spec_element_list
      opt_private_pkg_spec_element_list
   OUTDENT_opt_NEWLINE END_PACKAGE {
      declare
	Elem_List : Lists.List := $4.List;
      begin
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($2.Tree),
	  Add_On_Label => Add_On_For_Module($2.Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
	  Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
	  Has_Formals => False,
	  Module_Formals => Lists.Empty_List,
	  Extends_Interface => Null_Optional_Tree,
	  Implements_Interfaces => Lists.Empty_List,
	  Class_Locals => Lists.Empty_List,
	  Module_Exports => Elem_List,
	  Module_New_Exports => Lists.Empty_List,
	  Module_Implements => $5.List));
           --  NOTE: Module_Implements is used for private part of spec

        if $7.Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module($2.Tree),
              Ending_Id => $7.Label);
        end if;

      end;
    }
 | PACKAGE_kw package_defining_name 
     IS_kw_NEWLINE
   END_kw opt_PACKAGE_kw package_defining_name SEMI_or_NEWLINE {
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
	  Name => Name_For_Module($2.Tree),
	  Add_On_Label => Add_On_For_Module($2.Tree),
	  Is_Interface => True,
	  Is_Abstract => False,
	  Is_Private => False,
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

	Check_Id_Match(Starting_Id => Name_For_Module($2.Tree),
	  Ending_Id => $6.Tree);
    }
  ;


package_instantiation_with_term :
   package_instantiation SEMI_or_NEWLINE { $$ := $1; };

package_instantiation :
   PACKAGE_kw package_defining_name 
     IS_kw_ignore_INDENT
        NEW_kw expanded_name parenthesized_package_actual_list {
        --  Instantiate a package, not intended to be used as a type.
	$$ := (One_Tree, Tree => PSC.Trees.Module.Make(
          Name => $2.Tree,
          Add_On_Label => Lists.Empty_List,
          Is_Interface => True,
          Is_Abstract => False,
          Is_Private => False,
          Is_Concurrent => False,
          Is_Limited => True,
          Has_Formals => True,
          Treat_As_Type => False,
          Module_Formals => Lists.Empty_List,
          Extends_Interface =>
           --  We use a param-decl for the parent type for uniformity
            Param_Decl.Make
              (Name => Null_Optional_Tree,
               Kind => Param_Decl.Default_Param,
               Locking => Param_Decl.Not_Locked,
               Is_Optional => False,
               Param_Type =>
                  Invocation.Make(
                    Kind => Invocation.Module_Instantiation,
                    Prefix => $5.Tree,
                    Operands => $6.List),
               Param_Default => Null_Optional_Tree),
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
  | OUTDENT error {
        yyerror ("Indentation incorrect", At_Token => $1);
    }
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

standalone_operation_definition_with_term : 
    operation_definition_with_term {
        $$ := $1;
    }
  | operation_equiv SEMI_or_NEWLINE { $$ := $1; }
  ;

package_name : expanded_name { $$ := $1; };

package_defining_name : 
    expanded_name { $$ := $1; }
  ;

opt_package_formal_list : 
    package_formal_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    };

package_formal_list : 
    package_formal { $$ := $1; }
  | package_formal_list ';' package_formal {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

package_formal : 
    type_formal {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | value_formal {
	$$ := $1;
    }
  ;

type_formal : 
    type_instantiation { 
	$$ := (One_Tree, Type_Decl.Make(
	  Name => Null_Optional_Tree,
	  Is_New_Type => False,
	  Type_Definition => $1.Tree));
    }
  ;

IS_no_indent : IS_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [is with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
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
    id_list ':'
      operand_type_specifier 
      opt_ASSIGN_or_equal_simple_expression  {
	-- "simple_expression" to avoid use of '>'
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => Param_Decl.Default_Param,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
	      Param_Type => Copy_If_Not_First ($3.Tree, I),
	      Param_Default => Copy_If_Not_First ($4.Tree, I)));
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
  ;

attribute_id : Identifier {
        --  Add an '@' on the front of the attribute name.
	$$ := (One_Tree, PSC.Trees.Identifier.Make
                 ('@' & PSC.Strings.To_String ($1.Str),
                  $1.Source_Pos));
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
    attributed_expanded_name {
        if Tree_Of ($1.Tree) in Property.Tree then
           declare
              Prop_Tree : constant Property.Tree :=
                Property.Tree (Tree_Of ($1.Tree));
           begin
              if Tree_Of (Prop_Tree.Property_Id) in
                   PSC.Trees.Identifier.Tree then
                 declare
                    Id_Str : constant String :=
                      PSC.Strings.To_String
                        (PSC.Trees.Identifier.Tree
                          (Tree_Of (Prop_Tree.Property_Id)).Str);
                 begin
                    if Id_Str = "@Class" then
                       $$ := (One_Tree, Qualifier.Qualify(
                           Qualifiers =>
                             (Qualifier.Is_Polymorphic => True,
                              others => False),
                           Operand => Prop_Tree.Operand));
                    elsif Id_Str = "@Base" then
                       --  TBD: create unconstrained subtype
                       $$ := (One_Tree, Prop_Tree.Operand);
                    elsif Id_Str = "@Properties" then
                       --  Just return type name followed by "@Properties"
                       --  TBD: Handle case where prefix is an expanded name
                       declare
                          Prefix_Str : constant String :=
                            PSC.Strings.To_String
                              (PSC.Trees.Identifier.Tree
                                (Tree_Of (Prop_Tree.Operand)).Str);
                       begin
                          $$ := (One_Tree, PSC.Trees.Identifier.Make
                                   (Prefix_Str & "@Properties",
                                    Find_Source_Pos ($1.Tree))); 
                       end;
                    else
                       yyerror
                         ("Only T'Class, T'Base, and T'Properties " &
                          "are permitted as subtype marks",
                          At_Token => $1);
                       $$ := $1;
                    end if;
                 end;
              end if;
           end;
        else
           $$ := $1;
        end if;
    }
  ;

expanded_name : 
    id_or_string_literal { 
	$$ := $1;
    }
  | '@' {
	$$ := (One_Tree, PSC.Trees.Identifier.Make($1.Str, $1.Source_Pos)); 
    }
  | expanded_name '.' id_or_string_literal {
	$$ := (One_Tree, Selection.Make(
	  Prefix => $1.Tree,
	  Selector => $3.Tree));
    }
  ;

attributed_expanded_name : 
    expanded_name {
	$$ := $1;
    }
  | attributed_expanded_name PRIME attribute_id {
	$$ := (One_Tree, Property.Make(
	  Operand => $1.Tree,
	  Property_Id => $3.Tree,
          Source_Pos => $2.Source_Pos));
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
    expanded_name boxed_package_actual_list {
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Module_Instantiation,
	  Prefix => $1.Tree,
	  Operands => $2.List));
    }
  ;

boxed_package_actual_list : 
    '<' package_actual_list '>' { $$ := $2; }
    | BOX {
	$$ := (One_List, Lists.Empty_List);
    }
    ;

parenthesized_package_actual_list : 
    '(' package_actual_list ')' { $$ := $2; }
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
    qualified_type_specifier { 
	$$ := $1;
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
  | type_instantiation { $$ := $1; }
  ;
  
type_or_access_type_specifier : 
    type_specifier { $$ := $1; }
  | opt_not_null_qualifier access_type_def {
        if $1.Is_Not_Null then
           $$ := (One_Tree, Qualifier.Qualify
                     (Qualifiers =>
                        (Is_Not_Null => $1.Is_Not_Null, others => False),
                      Operand => $2.Tree));
        else
           $$ := $2;
        end if;
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
  ;

opt_aliased_type_or_access_type_specifier : 
    ALIASED_kw type_or_access_type_specifier { 
	$$ := (One_Tree, Qualifier.Qualify(
	  Qualifiers => (Qualifier.Is_Ref => True, others => False), 
	  Operand => $2.Tree));
    }
  | type_or_access_type_specifier {
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
  ;

type_qualifier :
    SYNCHRONIZED_kw {
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

operation_type_specifier :
    ACCESS_kw opt_PROTECTED_kw PROCEDURE_kw operation_inputs {
        declare
           Kind_Array : constant array (Boolean)
             of Operation.Operation_Kind_Enum :=
             (False => Operation.Proc_Type_Specifier,
              True => Operation.Protected_Proc_Type);
        begin
           $$ := (One_Tree, Operation.Make(
             Name => Null_Optional_Tree,
             Operation_Kind => Kind_Array ($2.Is_Present),
             Operation_Inputs => $4.List,
             Operation_Outputs => Lists.Empty_List,
             Preconditions => Null_Optional_Tree,
             Postconditions => Null_Optional_Tree,
             Is_Abstract => False,
             Is_Optional => False,
             Is_Queued => False,
             Is_Def => False,
             Statements => Null_Optional_Tree)); 
        end;
    }
  | ACCESS_kw opt_PROTECTED_kw FUNCTION_kw operation_inputs
      RETURN_kw opt_aliased_or_aliased_constant operand_type_specifier {
        declare
           Func_Kind_Array : constant array (Boolean)
             of Operation.Operation_Kind_Enum :=
             (False => Operation.Func_Type_Specifier,
              True => Operation.Protected_Func_Type);
           Param_Kind_Array : constant array (Boolean, Boolean)
             --  Indexed by (Is_Ref, Is_Const)
             of Param_Decl.Param_Kind :=
               (False => (False => Param_Decl.Default_Param,
                          True => Param_Decl.Default_Param), -- not legal
                True =>  (False => Param_Decl.Ref_Var_Param,
                          True => Param_Decl.Ref_Const_Param));
	   Func_Result : constant Optional_Tree :=
             Param_Decl.Make(
                Name => Null_Optional_Tree,
                Kind => Param_Kind_Array ($6.Is_Ref, $6.Is_Const),
                Locking => Param_Decl.Not_Locked,
                Is_Optional => False,
                Param_Type => PSC.Trees.Copy_Tree ($7.Tree),
                Param_Default => Null_Optional_Tree);
        begin
           $$ := (One_Tree, Operation.Make(
             Name => Null_Optional_Tree,
             Operation_Kind => Func_Kind_Array ($2.Is_Present),
             Operation_Inputs => $4.List,
             Operation_Outputs => Lists.Make((1 => Func_Result)),
             Preconditions => Null_Optional_Tree,
             Postconditions => Null_Optional_Tree,
             Is_Abstract => False,
             Is_Optional => False,
             Is_Queued => False,
             Is_Def => False,
             Statements => Null_Optional_Tree)); 
        end;
    }
  ;

opt_private_pkg_spec_element_list : { $$ := (One_List, Lists.Empty_List); }
  | PRIVATE_kw_opt_NL pkg_spec_element_list {
        $$ := $2;
    }
  ;

PRIVATE_kw_opt_NL : PRIVATE_kw | PRIVATE_kw NEWLINE ;

pkg_spec_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | pkg_spec_element_list pkg_spec_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.List);
    }
  | pkg_spec_element_list
      operation_equiv SEMI_or_NEWLINE {
	$$ := $1;
	Lists.Append($$.List, $2.Tree);
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
  | generic_declaration_with_term {
        $$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
simple_pkg_spec_element :
    operation_declaration { $$ := (One_List, Lists.Make((1 => $1.Tree))); }
  | object_declaration {
        $$ := $1;
        --  Set "global" if is a "var"
        declare
           Decls : constant Lists.List := $$.List;
        begin
           for I in 1 .. Lists.Length (Decls) loop
              declare
                 Decl : constant Optional_Tree :=
                   Lists.Nth_Element (Decls, I);
              begin
                 --  Decl might be from an exception declaration,
                 --  in which case it is *not* an Obj_Decl.

                 if Tree_Ptr_Of (Decl).all in Obj_Decl.Tree then
                    declare
                       Obj : Obj_Decl.Tree renames
                         Obj_Decl.Tree (Tree_Ptr_Of (Decl).all);
                    begin
                       if Obj.Is_Var then
                          --  A package-level variable is considered a "global"
                          --  NOTE: This isn't quite right if package itself
                          --        is not a library-level package.
                          --        In this context, "global" really means
                          --        it exists without having to create an
                          --        instance of the enclosing module.
                          Obj.Is_Global := True;
                       end if;
                    end;
                 end if;
              end;
           end loop;
        end;
  }
  | type_declaration {
        $$ := $1;
    }
  | subtype_declaration {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
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

IS_kw_ignore_INDENT :
    IS_no_indent {
        $$ := $1;
    }
  | IS_kw_INDENT {
        --  The INDENT was returned before we had a chance
        --  to suppress it.  Pop the indent stack.
        if Ada202x_Lex.Debug_Indent then
            Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Top := Ada202x_Lex.Top - 1;
        $$ := $1;
    }
  ;

operation_equiv :
    basic_operation_declaration RENAMES_kw attributed_expanded_name {
	$$ := (One_Tree, Operation.Add_Op_Equiv(
	  Op_Decl => $1.Tree, Op_Equiv => $3.Tree));
    }
  | basic_operation_declaration IS_kw '(' opt_record_component_list ')' {
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
  | basic_operation_declaration IS_kw_INDENT '(' opt_record_component_list ')' {
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
            if Ada202x_Lex.Debug_Indent then
                Text_IO.Put(" [IS: popping top indent] "); Text_IO.Flush;
            end if;
            Ada202x_Lex.Top := Ada202x_Lex.Top - 1;  --  Pop the indent stack
	end;
    }
  ;

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
  | operation_equiv SEMI_or_NEWLINE {
        $$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | exported_pkg_body_element_with_term { $$ := $1; }
  ;
  
exported_pkg_body_element_list : {
	$$ := (One_List, Lists.Empty_List);
    }
  | exported_pkg_body_element_list
      exported_pkg_body_element_with_term {
	$$ := $1;
	Lists.Append($$.List, $2.List);
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

--  NOTE: The following returns a List rather than a Tree:
exported_pkg_body_element_with_term : 
    operation_definition_with_term {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  | package_body_with_term  {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  ;
  
package_body_with_term :
   PACKAGE_kw BODY_kw package_defining_name 
   IS_kw
      INDENT local_pkg_body_element_list
   opt_begin_part
   OUTDENT_opt_NEWLINE END_PACKAGE {
        $$ := (One_Tree, PSC.Trees.Module.Make(
          Name => Name_For_Module($3.Tree),
          Add_On_Label => Add_On_For_Module($3.Tree),
          Is_Interface => False,
          Is_Abstract => False,
          Is_Private => False,
          Is_Concurrent => False,
          Is_Limited => True,  --  Packages are never assignable
          Has_Formals => False,
          Module_Formals => Lists.Empty_List,
          Extends_Interface => Null_Optional_Tree,
          Implements_Interfaces => Lists.Empty_List,
          Class_Locals => $6.List, --  Ada Doesn't distinguish locals/exports
          Module_Exports => Lists.Empty_List,
          Module_New_Exports => Lists.Empty_List,
          Module_Implements => $7.List));
            -- NOTE: Module_Implements is used for the begin part of a body

        if $9.Check_Label then
            Check_Id_Match(Starting_Id => Name_For_Module($3.Tree),
              Ending_Id => $9.Label);
        end if;
    } 
  ; 

--  NOTE: This returns a list; empty if no begin part
opt_begin_part : {
      $$ := (One_List, Lists.Empty_List);
    }
  | BEGIN_kw INDENT
      statement_sequence_opt_term_opt_eh {
	$$ := (One_List, Lists.Make ((1 => $3.Tree)));
    }
  ;

opt_PRIVATE_kw : {
        $$ := (Optional, Is_Present => False); 
    }
  | PRIVATE_kw {
        $$ := (Optional, Is_Present => True);
    }
  ;
  
condition : expression { $$ := $1; } ;

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
    basic_operation_declaration opt_aspect_spec {
	$$ := $1;
	Annotation.Add_Annotation ($$.Tree, $2.List);
    }
  | ABSTRACT_kw basic_operation_declaration opt_aspect_spec {
	$$ := $2;
	Annotation.Add_Annotation ($$.Tree, $3.List);
	declare
	    Func_Tree : Operation.Tree renames 
	      Operation.Tree(Tree_Ptr_Of($$.Tree).all);
	begin
	    Func_Tree.Is_Abstract := True;
	end;
    }
  ;

opt_aspect_spec :
    aspect_spec {
        $$ := $1;
    }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

aspect_spec :
    WITH_kw aspect_def {
        $$ := (One_List, Lists.Make((1 => $2.Tree)));
    }
  | aspect_spec ',' aspect_def {
        $$ := $1;
        Lists.Append ($$.List, $3.Tree);
    }
  ;

aspect_def :
    id REFERS_TO expression {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => $3.Tree));
    }
  | id {
	$$ := (One_Tree, Reference.Make(
	  Key => $1.Tree,
	  Referent => PSC.Trees.Identifier.Make ("#True")));
    }
  ;

basic_operation_declaration :
    PROCEDURE_kw operation_designator operation_inputs {
	$$ := (One_Tree, Operation.Make(
	  Name => $2.Tree,
	  Operation_Kind => Operation.Procedure_Operation,
	  Operation_Inputs => $3.List,
	  Operation_Outputs => Lists.Empty_List,
	  Preconditions => Null_Optional_Tree,
	  Postconditions => Null_Optional_Tree,
	  Is_Abstract => False,
	  Is_Optional => False,
	  Is_Queued => False,
	  Is_Def => False,
	  Statements => Null_Optional_Tree)); 
    }
  | FUNCTION_kw operation_designator operation_inputs 
      RETURN_kw opt_aliased_or_aliased_constant operand_type_specifier {
        declare
           Param_Kind_Array : constant array (Boolean, Boolean)
             --  Indexed by (Is_Ref, Is_Const)
             of Param_Decl.Param_Kind :=
               (False => (False => Param_Decl.Default_Param,
                          True => Param_Decl.Default_Param), -- not legal
                True =>  (False => Param_Decl.Ref_Var_Param,
                          True => Param_Decl.Ref_Const_Param));
	   Func_Result : constant Optional_Tree :=
             Param_Decl.Make(
                Name => Null_Optional_Tree,
                Kind => Param_Kind_Array ($5.Is_Ref, $5.Is_Const),
                Locking => Param_Decl.Not_Locked,
                Is_Optional => False,
                Param_Type => PSC.Trees.Copy_Tree ($6.Tree),
                Param_Default => Null_Optional_Tree);
        begin
           $$ := (One_Tree, Operation.Make(
             Name => $2.Tree,
             Operation_Kind => Operation.Function_Operation,
             Operation_Inputs => $3.List,
             Operation_Outputs => Lists.Make((1 => Func_Result)),
             Preconditions => Null_Optional_Tree,
             Postconditions => Null_Optional_Tree,
             Is_Abstract => False,
             Is_Optional => False,
             Is_Queued => False,
             Is_Def => False,
             Statements => Null_Optional_Tree)); 
        end;
    }
  ;

operation_designator : 
    attributed_expanded_name {
	$$ := $1;
    }
  ;
  
operation_inputs :
    '(' opt_operation_input_list right_paren {
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

opt_operation_input_list : 
    operation_input_list { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

operation_input_list : 
    operation_input { $$ := $1; }
  | operation_input_list ';' operation_input {
	$$ := $1;
	Lists.Append($$.List, $3.List);
    }
  ;

operation_input : 
    id_list ':'
      opt_in_out_mode 
      operand_type_specifier opt_ASSIGN_expression {
	$$ := (One_List, Lists.Empty_List);
	for I in 1..Lists.Length($1.List) loop
            --  TBF: aliased, not null needs to be passed along
	    Lists.Append($$.List, Param_Decl.Make(
	      Name => Lists.Nth_Element($1.List, I),
	      Kind => $3.Param_Kind,
	      Locking => Param_Decl.Not_Locked,
	      Is_Optional => False,
              In_Region => Null_Optional_Tree,
	      Param_Type => Copy_If_Not_First ($4.Tree, I),
	      Param_Default => Copy_If_Not_First ($5.Tree, I)));
	end loop;
    }
  ;

opt_aliased_or_aliased_constant :
    opt_aliased { $$ := $1; }
  | ALIASED_kw CONSTANT_kw
    {
	$$ := (Construct_Qualifier,
                Source_Pos => $1.Source_Pos,
                Is_Const => True,
                Is_Ref => True,
                others => False);
    }
  ;

opt_aliased_opt_constant :
    opt_aliased { $$ := $1; }
  | opt_aliased_CONSTANT_kw { $$ := $1; }
  ;

opt_aliased_CONSTANT_kw :
    CONSTANT_kw {
	$$ := (Construct_Qualifier,
               Source_Pos => PSC.Source_Positions.Null_Source_Position,
               Is_Const => True,
               others => False);
    }
  | ALIASED_kw CONSTANT_kw
    {
	$$ := (Construct_Qualifier,
                Source_Pos => $1.Source_Pos,
                Is_Const => True,
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

opt_not_null_qualifier : 
    not_null_qualifier { $$ := $1; }
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
  | access_type_def {
        $$ := $1;
    }
  ;

not_null_qualifier : 
    NOT_kw NULL_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
	  Is_Not_Null => True,
	  others => False);
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
object_declaration :
    id_list_with_colon
      opt_aliased_type_or_access_type_specifier 
      opt_ASSIGN_expression opt_aspect_spec {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           declare
              Decl : constant Optional_Tree := Obj_Decl.Make(
                Name => PSC.Trees.Identifier.Tree
                          (Tree_Of(Lists.Nth_Element($1.List, I))),
                Is_Var => True,
                Is_Global => False,  --  Will be set True if at package level
                Is_Const => False,
                Is_Ref => PSC.Trees.Qualifier.Qualifiers ($2.Tree)(Is_Ref),
                Is_Optional => False, -- TBD
                Obj_Type => Copy_If_Not_First
                  (PSC.Trees.Qualifier.Unqualified_Tree ($2.Tree), I),
                Obj_Value => Copy_If_Not_First ($3.Tree, I));
           begin
              Annotation.Add_Annotation (Decl, $4.List);
                 --  TBD: This aspect-spec is being shared!

              Lists.Append ($$.List,  Decl);
           end;
        end loop;
    }
  | id_list_with_colon
      type_or_access_type_specifier 
      RENAMES_kw name {
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
  | id RENAMES_kw name {
        $$ := (One_List, Lists.Make
          ((1 => Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree (Tree_Of($1.Tree)),
             Is_Var => False,
             Is_Const => False,
             Is_Ref => True,
             Is_Optional => False, -- TBD
             Obj_Type => Null_Optional_Tree,
             Obj_Value => $3.Tree))));
    }
  | id_list_with_colon opt_aliased_CONSTANT_kw
      type_or_access_type_specifier 
      opt_ASSIGN_expression opt_aspect_spec {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           declare
              Decl : constant Optional_Tree := Obj_Decl.Make(
                Name => PSC.Trees.Identifier.Tree
                          (Tree_Of(Lists.Nth_Element($1.List, I))),
                Is_Var => False,
                Is_Const => True,
                Is_Ref => $2.Is_Ref,
                Is_Optional => False, -- TBD
                Obj_Type => Copy_If_Not_First ($3.Tree, I),
                Obj_Value => Copy_If_Not_First ($4.Tree, I));
           begin
              Annotation.Add_Annotation (Decl, $5.List);
                 --  TBD: This aspect-spec is being shared!

              Lists.Append ($$.List,  Decl);
           end;
        end loop;
    }
  | id_list_with_colon opt_aliased_CONSTANT_kw
      ASSIGN_or_equal expression opt_aspect_spec {
        $$ := (One_List, Lists.Empty_List);
        for I in 1 .. Lists.Length ($1.List) loop
           declare
              Decl : constant Optional_Tree := Obj_Decl.Make(
                Name => PSC.Trees.Identifier.Tree
                          (Tree_Of(Lists.Nth_Element($1.List, I))),
                Is_Var => False,
                Is_Const => True,
                Is_Ref => $2.Is_Ref,
                Is_Optional => False, -- TBD
                Obj_Type => Null_Optional_Tree,
                Obj_Value => Copy_If_Not_First ($4.Tree, I));
           begin
              Annotation.Add_Annotation (Decl, $5.List);
                 --  TBD: This aspect-spec is being shared!

              Lists.Append ($$.List,  Decl);
           end;
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
type_declaration : 
    type_derivation {
        $$ := $1;
    }
  | non_derived_type_declaration opt_aspect_spec {
        $$ := $1;
	Annotation.Add_Annotation (Lists.Nth_Element($$.List, 1), $2.List);
    }
  ;

non_derived_type_declaration :
    TYPE_kw type_id IS_no_indent '(' opt_package_actual_list ')' {
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
  | TYPE_kw type_id IS_no_indent
      opt_ABSTRACT_kw record_definition {  -- TBD: discrims
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
  | TYPE_kw type_id IS_no_indent
      opt_ABSTRACT_kw opt_LIMITED_or_SYNCHRONIZED_kw PRIVATE_kw {
        -- TBD: discrims
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
  | TYPE_kw type_id IS_no_indent
      ARRAY_kw '(' index_subtype_list ')' OF_kw type_or_access_type_specifier {
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
  | TYPE_kw type_id IS_no_indent opt_not_null_qualifier access_type_def {
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Qualifier.Qualify
            (Qualifiers => (Is_Not_Null => $4.Is_Not_Null, others => False),
             Operand => $5.Tree)))));
    }
  | TYPE_kw type_id IS_no_indent
      RANGE_kw simple_expression_component {
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Integer", Token_Src_Pos ($4)),
            Operands => Lists.Make ((1 => $5.Tree)),
            Source_Pos => $4.Source_Pos)))));
    }
  | TYPE_kw type_id IS_no_indent
      MOD_kw expression {
        $$ := (One_List, Lists.Make ((1 => Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => True,
	  Type_Definition => Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => PSC.Trees.Identifier.Make
              ("Modular", Token_Src_Pos ($4)),
            Operands => Lists.Make ((1 => $5.Tree)),
            Source_Pos => $4.Source_Pos)))));
    }
  ;

type_id :
    id {
        $$ := $1;
    }
  | id PRIME attribute_id {
        --  Used for "T'Properties" only for now
        declare
           Id_Str : constant String := PSC.Strings.To_String
             (PSC.Trees.Identifier.Tree (Tree_Ptr_Of ($1.Tree).all).Str);
           Attr_Str : constant String := PSC.Strings.To_String
             (PSC.Trees.Identifier.Tree (Tree_Ptr_Of ($3.Tree).all).Str);
        begin
           if Attr_Str /= "@Properties" then
              yyerror ("The only type that may be declared using" &
                " a name of the form T'<attribute> is T'Properties",
                At_Token => $3);
           end if;
           --  Just concatenate the name for now
           --  TBD: Leave it as a Property node later so we can easily
           --       check that the prefix identifies the current instance
           --       of the enclosing module (though I suppose we might
           --       use it for nested types other than the "primary" nested
           --       type).
           $$ := (One_Tree, PSC.Trees.Identifier.Make
             (Id_Str & Attr_Str, $2.Source_Pos));
        end;
    }
  ;

access_type_def :
    ACCESS_kw opt_ALL_or_CONSTANT_kw type_specifier {
        $$ := (One_Tree,
	  Invocation.Make(
            Kind => Invocation.Module_Instantiation,
            Prefix => $2.Tree,
            Operands => Lists.Make
                          ((1 => $3.Tree)),
            Source_Pos => $1.Source_Pos));
    }
  | operation_type_specifier { $$ := $1; }
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
        if Tree_Ptr_Of ($1.Tree).all in Binary.Tree then
           --  Presume we have a range
           $$ := (One_Tree, Invocation.Make(
               Kind => Invocation.Module_Instantiation,
               Prefix => PSC.Trees.Identifier.Make
                 ("Integer", Find_Source_Pos ($1.Tree)),
               Operands => Lists.Make ((1 => $1.Tree))));
        else
           --  Presume we have a simple type name
           $$ := $1;
        end if;
    }
  | type_specifier RANGE_kw BOX {
        $$ := $1;
	Annotation.Add_Annotation ($$.Tree,
          Lists.Make
             ((1 => Binary.Make(Binary.Closed_Interval_Op,
                      Left_Operand => Null_Optional_Tree,
                      Right_Operand => Null_Optional_Tree,
                      Source_Pos => $3.Source_Pos))));
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

opt_LIMITED_or_SYNCHRONIZED_kw : 
    LIMITED_kw {
        $$ := (Construct_Qualifier,
          Source_Pos => $1.Source_Pos,
          Is_Limited => True,
          others => False);
    }
  | SYNCHRONIZED_kw {
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
    TYPE_kw type_id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_SYNCHRONIZED_kw
      parent_type_specifier opt_aspect_spec {
        -- TBD: discrims
        if $4.Is_Abstract then
           yyerror ("Only tagged types can be declared abstract",
                    At_Token => $4);
        end if;
	$$ := (One_List, Lists.Make ((1 => Type_Decl.Make
	  (Name => $2.Tree,
	   Is_New_Type => True,
           Type_Definition =>
             Param_Decl.Tree (Tree_Ptr_Of ($6.Tree).all).Param_Type))));
    }
  | TYPE_kw type_id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_SYNCHRONIZED_kw
      parent_type_specifier WITH_kw PRIVATE_kw opt_aspect_spec {
        -- TBD: discrims
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
  | TYPE_kw type_id IS_no_indent opt_ABSTRACT_kw opt_LIMITED_or_SYNCHRONIZED_kw
      parent_type_specifier
      WITH_kw record_definition opt_aspect_spec {  -- TBD: discrims
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
    NEW_kw basic_type_specifier {
        --  We use a param-decl for the parent type for uniformity
        $$ := (One_Tree, Tree => Param_Decl.Make
           (Name => Null_Optional_Tree,
            Kind => Param_Decl.Default_Param,
            Locking => Param_Decl.Not_Locked,
            Is_Optional => False,
            Param_Type => $2.Tree,
            Param_Default => Null_Optional_Tree));
    }
  | basic_type_specifier {
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
    opt_LIMITED_or_SYNCHRONIZED_kw NULL_kw RECORD_no_indent {
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
  | opt_LIMITED_or_SYNCHRONIZED_kw
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
    id ':' type_or_access_type_specifier 
      opt_ASSIGN_expression {
	$$ := (One_Tree, Obj_Decl.Make(
	  Name => PSC.Trees.Identifier.Tree(Tree_Of($1.Tree)),
	  Is_Var => True,
	  Is_Const => False,
	  Is_Ref => False,
	  Is_Optional => False,
	  In_Region => Null_Optional_Tree,
	  Obj_Type => $3.Tree,
	  Obj_Value => $4.Tree));
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
    SUBTYPE_kw id IS_kw subtype_indication opt_aspect_spec {
	$$ := (One_Tree, Type_Decl.Make(
	  Name => $2.Tree,
	  Is_New_Type => False,
	  Type_Definition => $4.Tree));
	Annotation.Add_Annotation ($$.Tree, $5.List);
    }
  ;

subtype_indication :
    type_specifier {
        $$ := $1;
    }
  | type_specifier RANGE_kw simple_expression_component {
        $$ := $1;
	Annotation.Add_Annotation($$.Tree, Lists.Make ((1 => $3.Tree)));
    }
  ;

operation_definition_with_term : 
   basic_operation_declaration IS_kw_INDENT
     statement_sequence_opt_eh
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
 | basic_operation_declaration IS_kw
     unindented_statement_sequence_opt_term 
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
  ;

opt_FUNC_or_PROC_kw :
    FUNCTION_kw {
        $$ := $1;
    }
  | PROCEDURE_kw {
        $$ := $1;
    }
  | {
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;

pop_indent_stack : { 
        --  Pop the indent stack
        if Ada202x_Lex.Debug_Indent then
            Text_IO.Put(" [QUEUED: popping top indent] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Top := Ada202x_Lex.Top - 1;
    };

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

unindented_statement_sequence_opt_term :  --  only allows simple statements
    unindented_statement_sequence { $$ := $1; }
  | unindented_statement_sequence_with_term { $$ := $1; }
  ;

unindented_statement_sequence :
    simple_statement { $$ := $1; }
  | unindented_statement_sequence_with_term simple_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

unindented_statement_sequence_with_term :
    simple_statement_with_term { $$ := $1; }
  | unindented_statement_sequence_with_term simple_statement_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

statement_sequence_opt_eh :
    statement_sequence_opt_term { $$ := $1; }
  | statement_sequence_opt_term
    BEGIN_kw OUTDENT_absorbing_NEWLINE
      statement_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $4.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | BEGIN_kw INDENT
      statement_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Then_Stmt_Op,
           --  Use a "then" operator to indicate the presence
           --  of a "begin", to improve error messages when in
           --  "strict" mode.
	  Left_Operand => Null_Optional_Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $1.Source_Pos));
    }
  ;

OUTDENT_absorbing_NEWLINE :
    NEWLINE { $$ := $1; }
  | OUTDENT_absorbing_NEWLINE OUTDENT NEWLINE {
        $$ := $1;
        Parser_Warning ("Wrong indentation", At_Token => $2);
     }
  ;

statement_sequence_opt_term_opt_eh :
    statement_sequence_opt_term { $$ := $1; }
  | statement_sequence_with_eh { $$ := $1; }
  | statement_sequence_with_term
    begin_statement
    statement_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => Binary.Make(
            Operator => Binary.Next_Stmt_Op,
            Left_Operand => $1.Tree,
            Right_Operand => $2.Tree),
	  Right_Operand => $3.Tree));
    }
  | begin_statement
    statement_sequence_opt_term_opt_eh {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

statement_sequence_with_eh :
    statement_sequence_opt_term exception_handler {
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
      opt_indented_statement_sequence_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  | WHEN_kw OTHERS_opt_named REFERS_TO_with_indent
      opt_indented_statement_sequence_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  ;

THEN_no_indent : THEN_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [then with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    };

RECORD_no_indent : RECORD_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [record with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    }
  ;

THEN_no_indent_NL : THEN_no_indent {
        $$ := $1;
    }
  | THEN_no_indent NEWLINE {
        $$ := $1;
    };

ELSE_no_indent : ELSE_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [else with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    };

statement_sequence_opt_term :
    statement_sequence_with_term { $$ := $1; }
  | statement_sequence { $$ := $1; }
  ;

statement_sequence_with_term :
    statement_with_term { $$ := $1; }
  | statement_sequence_with_term statement_with_term {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;

statement_sequence :
    simple_statement { $$ := $1; }
  | statement_sequence_with_term simple_statement {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.Next_Stmt_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $2.Tree));
    }
  ;
  
statement_with_term : 
    simple_statement_with_term {
        $$ := $1;
    }
  | opt_label_compound_statement_with_term {
        $$ := $1;
    }
  ;

simple_statement_with_term : 
    simple_statement SEMI_or_NEWLINE {
        $$ := $1;
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
local_declaration_with_term :
    local_declaration SEMI_or_NEWLINE {
        $$ := $1;
    }
  ;

opt_label_compound_statement_with_term : 
    local_declaration_with_term {
        --  Turn list into a statement sequence, separated by ";"s
        $$ := (One_Tree, Make_Statement_Sequence ($1.List));
    }
  | local_definition_with_term {
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
  | name {
        declare
           Name_Tree : PSC.Trees.Tree'Class
             renames Tree_Ptr_Of ($1.Tree).all;
        begin
           if Name_Tree in Invocation.Tree then
              --  Already an invocation
              $$ := $1;
           else
              --  Turn into a parameterless invocation
              $$ := (One_Tree, Invocation.Make(
                Kind => Invocation.Operation_Call,
                Prefix => $1.Tree,
                Operands => Lists.Empty_List));
           end if;
        end;
    }
  | return_stmt { $$ := $1; }
  | EXIT_kw opt_id {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Exit_Stmt,
	  Applies_To => Control_Stmt.Loop_Stmt,
	  Id => $2.Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | EXIT_kw opt_id WHEN_kw condition {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.If_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $4.Tree,
	  Then_Part => Control_Stmt.Make(
             Kind => Control_Stmt.Exit_Stmt,
             Applies_To => Control_Stmt.Loop_Stmt,
             Id => $2.Tree,
             Values => Null_Optional_Tree,
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
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [loop with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
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
  | RETURN_kw {
	$$ := (One_Tree, Control_Stmt.Make(
	  Kind => Control_Stmt.Return_Stmt,
	  Applies_To => Control_Stmt.Operation_Body,
	  Id => Null_Optional_Tree,
	  Values => Null_Optional_Tree,
	  Source_Pos => $1.Source_Pos));
    }
  | RETURN_kw id ':' opt_aliased_opt_constant
      type_or_access_type_specifier 
      opt_ASSIGN_expression DO_kw INDENT
        statement_sequence_opt_term_opt_eh
    OUTDENT_opt_NEWLINE END_kw RETURN_kw {
        --  Set the "Values" field of return statement
        --  to be a "Then" statement where LHS is the declaration
        --  of the return object, and RHS
        --  is the handled-seq-of-statements.
        declare
           Return_Obj : constant Optional_Tree := Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
             Is_Var => not $4.Is_Const,
             Is_Const => $4.Is_Const,
             Is_Ref => $4.Is_Ref,
             Is_Optional => False,
             In_Region => Null_Optional_Tree,
             Obj_Type => $5.Tree,
             Obj_Value => $6.Tree);
        begin
           $$ := (One_Tree, Control_Stmt.Make(
             Kind => Control_Stmt.Return_Stmt,
             Applies_To => Control_Stmt.Operation_Body,
             Id => Null_Optional_Tree,
             Values => Binary.Make(
                Operator => Binary.Then_Stmt_Op,
                Left_Operand => Return_Obj,
                Right_Operand => $10.Tree,
                Source_Pos => $1.Source_Pos),
             Source_Pos => $1.Source_Pos));
        end;
    }
  | RETURN_kw id ':' opt_aliased_opt_constant
      type_or_access_type_specifier 
      opt_ASSIGN_expression {
        --  Set the "Values" field of return statement
        --  to be a "Then" statement where LHS is the declaration
        --  of the return object, and RHS is a null tree.
        declare
           Return_Obj : constant Optional_Tree := Obj_Decl.Make(
             Name => PSC.Trees.Identifier.Tree(Tree_Of($2.Tree)),
             Is_Var => not $4.Is_Const,
             Is_Const => $4.Is_Const,
             Is_Ref => $4.Is_Ref,
             Is_Optional => False,
             In_Region => Null_Optional_Tree,
             Obj_Type => $5.Tree,
             Obj_Value => $6.Tree);
        begin
           $$ := (One_Tree, Control_Stmt.Make(
             Kind => Control_Stmt.Return_Stmt,
             Applies_To => Control_Stmt.Operation_Body,
             Id => Null_Optional_Tree,
             Values => Binary.Make(
                Operator => Binary.Then_Stmt_Op,
                Left_Operand => Return_Obj,
                Right_Operand => Null_Optional_Tree,
                Source_Pos => $1.Source_Pos),
             Source_Pos => $1.Source_Pos));
        end;
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
  | PARALLEL_kw {
	$$ := (Construct_Kind, Control_Stmt.Block_Stmt); --  TBD: Parallel_Stmt
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
local_declaration : 
    operation_declaration { $$ := (One_List, Lists.Make ((1 => $1.Tree))); }
--   | type_declaration aspect_spec {
--         $$ := $1;
-- 	Annotation.Add_Annotation (Lists.Nth_Element($$.List, 1), $2.List);
--     }
  | type_declaration {
        $$ := $1;
    }
  | object_declaration { $$ := $1; }
  | subtype_declaration {
        $$ := (One_List, Lists.Make ((1 => $1.Tree)));
    }
  ;

--  NOTE: The following returns a List rather than a Tree:
local_definition_with_term :
    operation_definition_with_term {
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

label : id ':' { $$ := $1; };

compound_statement_with_term :
    if_statement { $$ := $1; }
  | case_statement { $$ := $1; }
  | indefinite_loop_statement { $$ := $1; }
  | while_until_loop_statement { $$ := $1; }
  | for_loop_statement { $$ := $1; }
  | declare_statement  { $$ := $1; }
  | parallel_statement  { $$ := $1; }
  | error SEMI_or_NEWLINE { $$ := (One_Tree, Null_Optional_Tree); }
  ;

if_statement : 
    IF_kw condition THEN_kw_INDENT
     statement_sequence_opt_term
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
      statement_sequence_opt_term 
    OUTDENT_opt_else_part {
	$$ := (One_Tree, Conditional.Make(Kind => Conditional.Elsif_Stmt,
          Source_Pos => $1.Source_Pos,
	  Cond => $2.Tree,
	  Then_Part => $4.Tree,
	  Else_Part => $5.Tree));
    }
  | OUTDENT_ELSE_kw_INDENT statement_sequence_opt_term
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
    END_kw IF_kw SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => Null_Optional_Tree,
                End_With_Values => Null_Optional_Tree);
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
    END_kw CASE_kw SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => Null_Optional_Tree,
                End_With_Values => Null_Optional_Tree);
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
      opt_indented_statement_sequence_with_term {
	$$ := (One_Tree, Reference.Make(
	  Key => Invocation.Make(Invocation.Container_Aggregate,
	    Prefix => Null_Optional_Tree,
	    Operands => Lists.Make((1 => $2.Tree))),
	  Referent => $4.Tree));
    }
  ;

REFERS_TO_with_indent : REFERS_TO {
        if Ada202x_Lex.Debug_Indent then
           Text_IO.Put(" [indent on] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Bracketing_Token := REFERS_TO;
        Ada202x_Lex.Expecting_Indent := True;
        $$ := $1;
    };

simple_expression_opt_named :
    simple_expression { $$ := $1; }
  | id ':'
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
      opt_indented_statement_sequence_with_term {
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
	  Right_Operand => Null_Optional_Tree,
          Source_Pos => $1.Source_Pos));
    }
  ;

opt_indented_statement_sequence_with_term :
    indented_statement_sequence_with_term { $$ := $1; }
  | statement_sequence_with_term { $$ := $1; }
  ;

indented_statement_sequence_with_term :
    INDENT statement_sequence_opt_term OUTDENT { $$ := $2; }
  | INDENT statement_sequence_opt_term OUTDENT NEWLINE { $$ := $2; }
  | INDENT statement_sequence_opt_term OUTDENT error {
        yyerror ("Indentation incorrect", At_Token => $3);
        $$ := $2;
    }
  ;

OTHERS_opt_named :
    OTHERS_kw_as_interval { $$ := $1; }
  | id ':' OTHERS_kw_as_interval {
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
    LOOP_kw INDENT
      statement_sequence_opt_term
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
      statement_sequence_opt_term
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
    END_kw LOOP_no_indent opt_id SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree, End_With_Values => Null_Optional_Tree);
    }
  ;


for_loop_statement :
    FOR_kw iterator_spec opt_filter
        LOOP_kw_INDENT
      statement_sequence_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {
	$$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $1.Source_Pos,
	  Kind => For_Loop_Construct.For_Loop_Statement,
	  Iterators => $2.List,
	  Filter => $3.List,
	  Loop_Body => $5.Tree,
	  Direction => PSC.Strings.Null_U_String,
	  End_With_Values => $7.End_With_Values,
          Check_Label => $7.Check_Label,
          Label => $7.Label));
    }
  | par_direction_and_chunk_opt_NL FOR_kw iterator_spec opt_filter 
        LOOP_kw_INDENT
      statement_sequence_opt_term
    OUTDENT_opt_NEWLINE END_LOOP {

        --  Create the loop;
        --  it will be turned into one loop nested in another if chunked.
        $$ := (One_Tree, For_Loop_Construct.Make(
          Source_Pos => $2.Source_Pos,
          Kind => For_Loop_Construct.For_Loop_Statement,
          Iterators => $3.List,
          Filter => $4.List,
          Loop_Body => $6.Tree,
          Direction => For_Loop_Construct.Concurrent_Str,
          Chunk_Spec => $1.Tree,
          End_With_Values => $8.End_With_Values,
          Check_Label => $8.Check_Label,
          Label => $8.Label));

    }
  ;

filter :
    WHEN_kw expression {
	$$ := (One_List, Lists.Make ((1 => $2.Tree)));
    }
  ;

opt_filter :
    filter { $$ := $1; }
  | {
	$$ := (One_List, Lists.Empty_List);
    }
  ;

iterator_spec : 
    iterator {
	$$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | '(' iterator_list ')' { $$ := $2; }
  ;

iterator_list :
    iterator {
        $$ := (One_List, Lists.Make((1 => $1.Tree)));
    }
  | iterator_list ';' iterator {
        $$ := $1;
        Lists.Append($$.List, $3.Tree);
    }
  | iterator_list ',' iterator {
        yyerror("Iterators must be separated by "";""",
          At_Token => $2);
        $$ := $1;
        Lists.Append($$.List, $3.Tree);
    }
  ;

iterator :
    index_set_iterator { $$ := $1; }
  | ALL_kw index_set_iterator { 
	yyerror("Use ""for ... in"" rather " &
          "than ""for all ..."" in iterator of for-loop",
          At_Token => $1);
	$$ := $2; 
    }
  | ALL_kw element_iterator { 
	yyerror("Use ""for ... of"" rather than ""for all ..."" in " &
          "container element iterator",
          At_Token => $1);
	$$ := $2; 
    }
  | element_iterator { 
	$$ := $1; 
    }
  ;

index_set_iterator :
    id opt_COLON_type_specifier IN_kw opt_REVERSE_kw expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name => $1.Tree,
	  Is_Ref => False,
	  Obj_Type => $2.Tree,
	  Obj_Value => $5.Tree));

        if $4.Str /= PSC.Strings.Null_U_String then
            --  Record "reverse" ("forward" is the default)
            Iterator.Add_Direction($$.Tree, $4.Str);
        end if;

    };

opt_REVERSE_kw : {
        --  In Ada, "forward" is the default
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
          For_Loop_Construct.Forward_Str);
    }
  | REVERSE_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos, For_Loop_Construct.Reverse_Str);
    };

element_iterator :
    id opt_COLON_type_specifier OF_no_indent opt_REVERSE_kw expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Value,
	  Name => $1.Tree,
	  Is_Ref => True,
	  Obj_Type => $2.Tree,
	  Obj_Value => $5.Tree));

        if $4.Str /= PSC.Strings.Null_U_String then
            --  Record "reverse" ("forward" is the default)
            Iterator.Add_Direction($$.Tree, $4.Str);
        end if;
    }
  | '[' id REFERS_TO id ']' OF_no_indent opt_REVERSE_kw expression {
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Each_Key_Value,
	  Name => $4.Tree,
	  Is_Ref => True,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value => $8.Tree,
	  Key_Name => $2.Tree));

        if $7.Str /= PSC.Strings.Null_U_String then
            --  Record "reverse" ("forward" is the default)
            Iterator.Add_Direction($$.Tree, $7.Str);
        end if;
    }
  ;

OF_no_indent : OF_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [of with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    };

opt_COLON_type_specifier : 
    ':' type_specifier {
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
  ;

next_name_list : 
    name  { 
	$$ := (One_List, Lists.Make((1 => $1.Tree))); 
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
    
par_direction :
    PARALLEL_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos,
               For_Loop_Construct.Concurrent_Str);
    }
    ;

par_direction_and_chunk :
    par_direction opt_aspect_spec {
        if Lists.Is_Empty ($2.List) then
           $$ := (One_Tree, Null_Optional_Tree); 
        else
           $$ := (One_Tree, Annotation.Make
                    (Annotations => $2.List,
                     Label =>
                       PSC.Trees.Identifier.Make
                         ("parallel_aspect", $1.Source_Pos)));
        end if;
    }
  | par_direction '(' adding_expression ')' opt_aspect_spec {
        --  Create an implicit "I in 1 .. N"
	$$ := (One_Tree, Iterator.Make(
	  Kind => Iterator.Set_Iterator,
	  Name =>
            PSC.Trees.Identifier.Make
              (PSC.Symbols.Generate_Unique_Label
                 (Find_Source_Pos ($3.Tree), Preceding => "chunk_"),
               Find_Source_Pos ($3.Tree)),
	  Is_Ref => False,
	  Obj_Type => Null_Optional_Tree,
	  Obj_Value =>
            Binary.Make
              (Operator => Binary.Closed_Interval_Op,
               Left_Operand => PSC.Trees.Identifier.Make
                 (Literal_One_Str, Find_Source_Pos ($3.Tree)),
               Right_Operand => $3.Tree,
               Source_Pos => Find_Source_Pos ($3.Tree))));
	Annotation.Add_Annotation ($$.Tree, $5.List);
    }
  | par_direction '(' index_set_iterator ')' opt_aspect_spec {
        $$ := $3;
	Annotation.Add_Annotation ($$.Tree, $5.List);
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

reverse : 
    REVERSE_kw {
	$$ := (One_Token, PSC.Syntax.Cur_Source_Pos, For_Loop_Construct.Reverse_Str);
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
    BEGIN_kw NEWLINE
      statement_sequence_opt_term_opt_eh
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
    END_kw opt_id SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => PSC.Strings.Null_U_String,
                Check_Label => True,
                Label => $2.Tree, End_With_Values => Null_Optional_Tree);
    }
  ;

decl_list_with_term :
    local_pkg_body_element_list {
        --  Convert list to a sequence of Next_Stmt_Op(A, B)
        $$ := (One_Tree, Make_Statement_Sequence ($1.List));
    }
  ;

parallel_statement :
    PARALLEL_kw DO_kw
      parallel_alt_list
    END_DO {
	$$ := (One_Tree, Block_Stmt.Make(
          Source_Pos => $1.Source_Pos,
	  Block_Body => $3.Tree,
	  End_With_Values => $4.End_With_Values,
          Check_Label => $4.Check_Label,
          Label => $4.Label));
    }
  ;

parallel_alt_list : 
    parallel_alt {
	$$ := $1;
    }
  |    parallel_alt_list
    AND_kw
       parallel_alt {
	$$ := (One_Tree, Binary.Make (Binary.Parallel_Stmt_Op,
                  Left_Operand => $1.Tree,
                  Right_Operand => $3.Tree,
                  Source_Pos => Token_Src_Pos ($2)));
    }
  ;

parallel_alt :
    indented_statement_sequence_with_term {
	$$ := $1;
    }
  ;

opt_DO_no_indent : DO_no_indent { $$ := $1; }
  | {
	yyerror("Should be ""end do <id>"" rather than ""end <id>""");
	$$ := (One_Token, 
	  PSC.Source_Positions.Null_Source_Position, 
	  PSC.Strings.Null_U_String); 
    }
  ;
   
DO_no_indent : DO_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [do with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    };

END_DO :
    END_kw opt_DO_no_indent opt_id SEMI_or_NEWLINE {
        $$ := (Optional_End_Token,
                Source_Pos => $1.Source_Pos,
                End_Construct_Str => $2.Str,
                Check_Label => True,
                Label => $3.Tree,
                End_With_Values => Null_Optional_Tree);
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

expression_no_err :
    logical_expression { $$ := $1; }
  | BOX {
        $$ := (One_Tree, Binary.Make(Binary.Closed_Interval_Op,
                   Left_Operand => Null_Optional_Tree,
                   Right_Operand => Null_Optional_Tree,
                   Source_Pos => $1.Source_Pos));
    }
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
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
      end;
    }
  ;

simple_comparison_expression :  -- comparisons are non associative
    simple_expression { $$ := $1; }
  | simple_expression comparison_operator simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => $2.Binary_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  ;

comparison_expression :
    simple_comparison_expression { $$ := $1; }
  | adding_expression IN_kw simple_expression {
	$$ := (One_Tree, Binary.Make(
	  Operator => Binary.In_Op,
	  Left_Operand => $1.Tree,
	  Right_Operand => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | adding_expression NOT_kw IN_kw simple_expression {
	$$ := (One_Tree, Unary.Make(
                 Operator => Unary.Not_Op,
                 Operand => Binary.Make(
                   Operator => Binary.In_Op,
                   Left_Operand => $1.Tree,
                   Right_Operand => $4.Tree,
                   Source_Pos => $3.Source_Pos)));
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
  | adding_expression IS_no_indent FUNCTION_kw
      '(' opt_operation_actual_list ')' {
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
  | '|' simple_expression_component '|' {
        $$ := (One_Tree, Unary.Make(Unary.Magnitude_Op,
          Operand => $2.Tree));
    }
  | aggregate { $$ := $1; }
  | container_aggregate PRIME attribute_id {
        --  An Ada202x Reduce expression
	$$ := (One_Tree, Property.Make(
	  Operand => $1.Tree,
	  Property_Id => $3.Tree,
          Source_Pos => $2.Source_Pos));
    }
  | container_aggregate PRIME attribute_id '(' opt_operation_actual_list ')' {
        --  An Ada202x Reduce expression
	$$ := (One_Tree, Invocation.Make(
	  Kind => Invocation.Operation_Call,
	  Prefix => Property.Make(
             Operand => $1.Tree,
             Property_Id => $3.Tree,
             Source_Pos => $2.Source_Pos),
	  Operands => $5.List));
    }
  | '<' id '>' { 
        $$ := (One_Tree,
                 Unary.Make(Unary.Initial_Value_Op, Operand => $2.Tree));
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
    attributed_expanded_name {
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
  | complex_name PRIME attribute_id {
	$$ := (One_Tree, Property.Make(
	  Operand => $1.Tree,
	  Property_Id => $3.Tree,
          Source_Pos => $2.Source_Pos));
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
  ;

adding_operator : 
    '-' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Minus_Op); }
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
  ;

assign_operator_not_divide :
    ASSIGN {
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
     }
  ;

ASSIGN_or_equal : ASSIGN { 
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
    }
  | equal_as_assign { $$ := $1; }
  ;

equal_as_assign : '=' {
	yyerror("Use "":="" rather than ""="" in Ada202x");
	$$ := (One_Assign_Op, $1.Source_Pos, Assign_Stmt.Assign_Op); 
    }
  ;

comparison_operator : 
    '=' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Equal_Op); }
  | NEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.NEQ_Op); }
  | '<' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Less_Op); }
  | LEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.LEQ_Op); }
  | '>' { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Greater_Op); }
  | GEQ { $$ := (One_Binary_Op, $1.Source_Pos, Binary.GEQ_Op); }
  ;

logical_operator :
    AND_no_indent { $$ := (One_Binary_Op, $1.Source_Pos, Binary.And_Op); }
  | OR_kw { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Or_Op); }
  | XOR_kw  { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Xor_Op); }
  | AND_no_indent THEN_no_indent
      { $$ := (One_Binary_Op, $1.Source_Pos, Binary.And_Then_Op); }
  | OR_kw ELSE_no_indent
      { $$ := (One_Binary_Op, $1.Source_Pos, Binary.Or_Else_Op); }
  ;

AND_no_indent : AND_kw {
        if Ada202x_Lex.Debug_Indent
          and then Ada202x_Lex.Expecting_Indent
        then
            Text_IO.Put(" [""and"" with indent off] "); Text_IO.Flush;
        end if;
        Ada202x_Lex.Expecting_Indent := False;
        $$ := $1;
    };

interval_operator :
    DOT_DOT { 
	$$ := (One_Binary_Op, $1.Source_Pos, Binary.Closed_Interval_Op); 
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
  | attributed_expanded_name PRIME '(' opt_record_component_list ')' {
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
  | attributed_expanded_name PRIME '[' opt_container_element_list ']' {
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
  | FOR_kw iterator opt_filter
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
        Value : Optional_Tree := $6.Tree;
        use type PSC.Strings.U_String;
      begin
        if Not_Null($4.Tree) then
           -- User has specified a key for the element
           Value := Reference.Make(
             Key => $4.Tree,
             Referent => Value);
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
  | ELSE_no_indent expression {
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
    FOR_kw ALL_or_SOME_kw quantified_iterator opt_filter REFERS_TO
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
  | FOR_kw ALL_or_SOME_kw id opt_COLON_type_specifier filter REFERS_TO 
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
  ;

%%


with Ada202x_tokens, Ada202x_lex_io, Ada202x_goto, Ada202x_shift_reduce;
with Ada202x_lex, text_io;

use  Ada202x_tokens, Ada202x_lex_io, Ada202x_goto, Ada202x_shift_reduce;
use  Ada202x_lex, text_io;

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

package body Ada202x_Parser is

    use type Param_Decl.Param_Kind;
    use type PSC.Strings.U_String;

    Func_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("func");

    Function_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("function");

    Proc_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("proc");

    Procedure_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup("procedure");

    Literal_One_Str : constant PSC.Strings.U_String :=
      PSC.Strings.String_Lookup ("1");

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
     At_Token : Ada202x_Tokens.YYSType := (Ada202x_Tokens.Optional,
       Is_Present => False)) is
    begin
	PSC.Messages.Parser_Error(S, Src_Pos => Token_Src_Pos (At_Token));
    end yyerror;

   procedure Parser_Warning (S : String;
     At_Token : Ada202x_Tokens.YYSType := (Ada202x_Tokens.Optional,
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
    --  Check that "procedure" ends with "end [procedure]" and
    --  "function" ends with "end [function]"
       use Operation;
    begin
       if End_Construct_Token.Kind = Optional_End_Token
         and then End_Construct_Token.End_Construct_Str /=
           PSC.Strings.Null_U_String
       then
          case Op_Decl.Operation_Kind is
             when Function_Operation =>
                if End_Construct_Token.End_Construct_Str /= Function_Str then
                   yyerror("Should be ""end function""",
                     At_Token => End_Construct_Token);
                end if;
             when Procedure_Operation =>
                if End_Construct_Token.End_Construct_Str /= Procedure_Str then
                   yyerror("Should be ""end procedure""",
                     At_Token => End_Construct_Token);
                end if;
             when others =>
                null;
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
        Result : Optional_Tree := Null_Optional_Tree;
    begin
        if not Lists.Is_Empty (L) then
           Result := Lists.Nth_Element (L, 1);
           for I in 2 .. Lists.Length (L) loop
               Result := Binary.Make(
                Operator => Binary.Next_Stmt_Op,
                Left_Operand => Result,
                Right_Operand => Lists.Nth_Element (L, I));

           end loop;
        end if;
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
             --  Add a '#' on the front of the enum-lit name.
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

end Ada202x_Parser;

