%token IDENTIFIER_tk 
%token CONSTANT_tk STRING_LITERAL SIZEOF_tk
%token PTR_OP_tk INC_OP_tk DEC_OP_tk LEFT_OP_tk RIGHT_OP_tk LE_OP_tk GE_OP_tk EQ_OP_tk NE_OP_tk
%token AND_OP_tk OR_OP_tk MUL_ASSIGN_tk DIV_ASSIGN_tk MOD_ASSIGN_tk ADD_ASSIGN_tk
%token SUB_ASSIGN_tk LEFT_ASSIGN_tk RIGHT_ASSIGN_tk AND_ASSIGN_tk
%token XOR_ASSIGN_tk OR_ASSIGN_tk TYPE_NAME_tk

%token TYPEDEF_tk EXTERN_tk STATIC_tk AUTO_tk REGISTER_tk
%token CHAR_tk SHORT_tk INT_tk LONG_tk SIGNED_tk UNSIGNED_tk FLOAT_tk DOUBLE_tk CONST_tk VOLATILE_tk VOID_tk
%token STRUCT_tk UNION_tk ENUM_tk ELLIPSIS_tk

%token CASE_tk DEFAULT_tk IF_tk ELSE_tk SWITCH_tk WHILE_tk DO_tk FOR_tk GOTO_tk CONTINUE_tk BREAK_tk RETURN_tk

%start translation_unit

%with Std_Output;
%use Std_Output;
%with Utils.Messages;
%with Utils.Spellings;
%use Utils;
%use Utils.Spellings;
%with Utils.Sequences;
%with Utils.Fast_Sets;
%with C_Misc; 
%use C_Misc;
%with Lists;
%with Utils.Output.Streams;
%use Utils.Output.Streams;
%with Utils.Output.Streams.Files;
%use Utils.Output.Streams.Files;
%with Utils.Strings;
%use Utils.Strings ;

{ 

    Max_Buffer_Index : constant := 200_000;
    subtype Lex_Buffer_Index is Integer range 0..Max_Buffer_Index;
  
    type Lex_Source_Position is record
        MSP : Utils.Messages.Source_Position;
        File_Name : Utils.Spellings.Spelling;
        Lex_Index : Lex_Buffer_Index;  -- where in the buffer the token starts
        Lex_End_Index : Lex_Buffer_Index;  -- where in the buffer the token ends
    end record;

    Lex_Null_Source_Position : constant Lex_Source_Position :=
      (MSP => Utils.Messages.Null_Source_Position,
        File_Name =>  No_Spelling, 
        Lex_Index => 0, Lex_End_Index => 0);

    type yystype_enum is (
      Simple_Token,
      Decl_Qualifier,
      Param_Qualifier,
      Param_List_Qualifier,
      Type_Specifier_Qualifier,
      Type_Arg_Qualifier,
      Type_Arg_List_Qualifier,  
      Mod_List_Qualifier,
      Method_Decl_Qualifier,
      Method_Decln_Qualifier,
      Constr_Decl_Qualifier,
      Dim_Qualifier,
      Storage_Qualifier,
      Type_Qualifier,
      Decln_Spec_Qualifier,
      Init_Decl_List_Qualifier,
      Decln_List_Qualifier,
      Direct_Decl_Qualifier
     );

     type arg_enum is (
       Regular_Arg,
       Extends_Arg,
       Super_Arg,
       Wildcard_Arg);
       
    type yystype(Kind : yystype_enum := Simple_Token);

    type yystype_ptr is access yystype;
    subtype TypeArg_Ptr is yystype_ptr(Type_Arg_Qualifier);

    subtype Stack_Int is Integer range 1..20;

    subtype Decl_Ptr is yystype_ptr(Decl_Qualifier);
    package Init_Decl_Lists is new Lists(ItemType => Decl_Ptr, Equal => "=");

    package Arg_Lists is new Lists(ItemType => TypeArg_Ptr, Equal => "=");
    
    package Param_Lists is new Lists(ItemType => C_Misc.Param_Ptr, Equal => "=");
    package Spelling_Lists is new Lists(ItemType => Spellings.Spelling, Equal => "=");
    package Classname_Stacks is new Sequences
      (Name => "Classname_Stack",
      Element => Spellings.Spelling,
      Seq_Index => Stack_Int);
--      "=" => Spellings.Spellings_Equal);
    package Methodname_Stacks is new Sequences
      (Name =>  "Methodname_Stack",
      Element => Spellings.Spelling,
      Seq_Index => Stack_Int);

    package Interface_Sets is new Fast_Sets 
      ( Name => "Interface_Set",
        Key_Type => Spellings.Spelling,
        Hash => Spellings.Hash_Spelling,
        Null_Key => Spellings.No_Spelling,
        Deleted_Key => Spellings.No_Spelling);

    subtype Decl_Spec_Ptr is yystype_ptr(Decln_Spec_Qualifier);
    package Decl_Spec_Lists is new Lists(ItemType => Decl_Spec_Ptr, Equal => "=");

    type yystype(Kind : yystype_enum := Simple_Token) is record
      Position : Lex_Source_Position;
      case Kind is
	when Simple_Token =>
          Str : Spellings.Spelling;
        when Decl_Qualifier =>
          DeclStr : Spellings.Spelling;
          Has_Dimensions : Boolean := False;
          Is_Pointer  :  Boolean := False;
        when Direct_Decl_Qualifier =>
          DirectStr : Spellings.Spelling;
          Has_Array : Boolean := False;
          Has_Pointer  :  Boolean := False;
        when Param_Qualifier =>
          ParamInfo : C_Misc.Param_Ptr;
        when Param_List_Qualifier =>
          ParamList : Param_Lists.List;
        when Type_Specifier_Qualifier =>
          TypeSpec_Info : C_Misc.TypeSpec_Ptr;
          ArgsList : Arg_Lists.List;
	when Type_Arg_Qualifier =>
          ArgKind : arg_enum := Regular_Arg;
          ArgInfo : yystype_ptr(Type_Specifier_Qualifier);	    
        when Type_Arg_List_Qualifier =>
          TypeArgsList : Arg_Lists.List;
        when Mod_List_Qualifier =>
          ModList : Spelling_Lists.List;
        when Method_Decl_Qualifier =>
          MethodName : Spellings.Spelling;
          MethodParams : Param_Lists.List;
          Method_Dimensions : Boolean;
          Has_Params     : Boolean;
        when Method_Decln_Qualifier =>
          MethodMods : Spelling_Lists.List;
          ReturnType : yystype_ptr(Type_Specifier_Qualifier);
          MethodDeclr : yystype_ptr(Method_Decl_Qualifier);
        when Constr_Decl_Qualifier =>
          ConstrName : Spellings.Spelling;
          ConstrParams : Param_Lists.List;
        when Dim_Qualifier =>
          DimInfo : Natural;
        when  Storage_Qualifier =>
          Is_Auto  : Boolean := False;
          Is_Static : Boolean  := False;
          Is_Extern : Boolean := False;
          Is_Register : Boolean := False;
          Is_Typedef  : Boolean := False;
        when Type_Qualifier =>
          Is_Const : Boolean := False;
          Is_Volatile : Boolean := False;
        when Decln_Spec_Qualifier =>
          StorageInfo : yystype_ptr(Storage_Qualifier);
          TypeSpecInfo : Spellings.Spelling;
          TypeQualInfo : yystype_ptr(Type_Qualifier);
        when Decln_List_Qualifier =>
          Decl_Spec_List : Decl_Spec_Lists.List; 
        when Init_Decl_List_Qualifier =>
          Init_Decl_List   : Init_Decl_Lists.List;
      end case;
    end record;

    Id_Type : Spellings.Spelling;
    Class_Name : Spellings.Spelling;
    Decl_Name : Spellings.Spelling;
    Qualified_Name : Spellings.Spelling;
    tempQualified_Name : Spellings.Spelling;
    Method_Name : Spellings.Spelling;
    Return_Type : Spellings.Spelling;
    Type_Specifier : Spellings.Spelling;
    Type_Name : Spellings.Spelling; 
    Param_Type : Spellings.Spelling;
    Param_Name : Spellings.Spelling;
  
    Type_Spec_Node : C_Misc.TypeSpec_Ptr;
    Param_Node : C_Misc.Param_Ptr;
    Param_List : C_Misc.Param_Ptr := null;
    TParam_List : Param_Lists.List;
    TArgs_List  : Arg_Lists.List;
    TMod_List   : Spelling_Lists.List;
    Storage_List : Spelling_Lists.List;
    ParamIter        : Param_Lists.ListIter;
    Classname_Stack : Classname_Stacks.Temp_Seq;
    Methodname_Stack : Methodname_Stacks.Temp_Seq;
    Interface_Set  : Interface_Sets.Set;
    Decl_List      : Decl_Spec_Lists.List;
    Init_List : Init_Decl_Lists.List;
    Init_List_Iter : Init_Decl_Lists.ListIter;
    Decl_Node_Ptr    :  Decl_Ptr; 

}


%%

primary_expression
	: IDENTIFIER_tk
	| CONSTANT_tk
	| STRING_LITERAL
	| '(' expression ')'
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' IDENTIFIER_tk
	| postfix_expression PTR_OP_tk IDENTIFIER_tk
	| postfix_expression INC_OP_tk
	| postfix_expression DEC_OP_tk
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

unary_expression
	: postfix_expression
	| INC_OP_tk unary_expression
	| DEC_OP_tk unary_expression
	| unary_operator cast_expression
	| SIZEOF_tk unary_expression
	| SIZEOF_tk '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP_tk additive_expression
	| shift_expression RIGHT_OP_tk additive_expression
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP_tk shift_expression
	| relational_expression GE_OP_tk shift_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP_tk relational_expression
	| equality_expression NE_OP_tk relational_expression
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP_tk inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP_tk logical_and_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| MUL_ASSIGN_tk
	| DIV_ASSIGN_tk
	| MOD_ASSIGN_tk
	| ADD_ASSIGN_tk
	| SUB_ASSIGN_tk
	| LEFT_ASSIGN_tk
	| RIGHT_ASSIGN_tk
	| AND_ASSIGN_tk
	| XOR_ASSIGN_tk
	| OR_ASSIGN_tk
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

CONSTANT_tk_expression
	: conditional_expression
	;

declaration
	: declaration_specifiers ';'   
	| declaration_specifiers init_declarator_list ';' 
        { 
          Decl_Node_Ptr := Init_Decl_Lists.Firstvalue($2.Init_Decl_List);
           if $1.StorageInfo /= null and then $1.StorageInfo.Is_Typedef then
             C_Grammar.Typename_Lists.Attach(Decl_Node_Ptr.DeclStr , Typename_List);
             print("Inserting typedef ", Decl_Node_Ptr.DeclStr);
           end if;
        }   
	;

declaration_specifiers
	: storage_class_specifier
        { 
          $$ := (Decln_Spec_Qualifier,
           Position =>  $1.Position,
           StorageInfo => new yystype'($1),
           TypeSpecInfo =>  Spellings.No_Spelling,
           TypeQualInfo => null 
          );
        }
	| storage_class_specifier declaration_specifiers
        {  $$ := $2; 
           $$.StorageInfo := new yystype'($1);
        }
	| type_specifier
        {  $$ := (Decln_Spec_Qualifier,
           Position =>  $1.Position,
           StorageInfo => null,
           TypeSpecInfo => $1.Str,
           TypeQualInfo => null 
          );
        }
	| type_specifier declaration_specifiers
        { $$ := $2;
          $$.TypeSpecInfo := $1.Str; 
        }
	| type_qualifier
        {  $$ := (Decln_Spec_Qualifier,
           Position =>  $1.Position,
           StorageInfo => null,
           TypeSpecInfo =>  Spellings.No_Spelling,
           TypeQualInfo => new yystype'($1)
          );
        }
	| type_qualifier declaration_specifiers
        { $$ := $2;
          $$.TypeQualInfo := new yystype'($1); 
        }
	;

init_declarator_list
	: init_declarator
        { Init_List := Init_Decl_Lists.Create;
          Init_Decl_Lists.Attach(new yystype'($1), Init_List);
          $$ := (Init_Decl_List_Qualifier, Position => $1.Position,
            Init_Decl_List => Init_List);
         }
	| init_declarator_list ',' init_declarator
        {
         $$ := $1;
         Init_Decl_Lists.Attach(new yystype'($3), $$.Init_Decl_List);
        }
	;

init_declarator
	: declarator
        {  $$ := (Decl_Qualifier,
            Position => $1.Position,
            DeclStr => $1.DeclStr,
            Has_Dimensions => False,
            Is_Pointer => False);
        }
	| declarator '=' initializer
        {  $$ := (Decl_Qualifier,
            Position => $1.Position,
            DeclStr => $1.DeclStr,
            Has_Dimensions => False,
            Is_Pointer => False);
        }
	;

storage_class_specifier
	: TYPEDEF_tk
          { $$ :=  (Storage_Qualifier,
            Position => $1.Position,
            Is_Typedef => True,
            Is_Extern => False,
            Is_Static => False,
            Is_Auto => False,
            Is_Register => False
            );    
          }
	| EXTERN_tk
          { $$ :=  (Storage_Qualifier,
            Position => $1.Position,
            Is_Extern => True,
            Is_Typedef => False,
            Is_Static => False,
            Is_Auto => False,
            Is_Register => False
          );    
          }
	| STATIC_tk
          { $$ :=  (Storage_Qualifier,
            Position => $1.Position,
            Is_Static => True,
            Is_Typedef => False,
            Is_Extern => False,
            Is_Auto => False,
            Is_Register => False
           );    
          }
	| AUTO_tk
          { $$ :=  (Storage_Qualifier,
            Position => $1.Position,
            Is_Auto => True,
            Is_Typedef => False,
            Is_Extern => False,
            Is_Static => False,
            Is_Register => False
           );    
          }
	| REGISTER_tk
          { $$ :=  (Storage_Qualifier,
            Position => $1.Position,
            Is_Register => True,
            Is_Typedef => False,
            Is_Extern => False,
            Is_Static => False,
            Is_Auto => False
           );    
          }
	;

type_specifier
	: VOID_tk
         { $$ := $1; }
	| CHAR_tk
          { $$ := $1;}
	| SHORT_tk
          { $$ := $1;}
	| INT_tk
          { $$ := $1;
          }
	| LONG_tk
          { $$ := $1;}
	| FLOAT_tk
          { $$ := $1;}
	| DOUBLE_tk
          { $$ := $1;}
	| SIGNED_tk
          { $$ := $1;}
	| UNSIGNED_tk
          { $$ := $1;}
	| struct_or_union_specifier
          { $$ := $1;}
	| enum_specifier
          { $$ := $1;}
	| TYPE_NAME_tk
         {  $$ := $1;
--            C_Grammar.Typename_Lists.Attach($1.Str, Typename_List);
--            print("Inserting typedef " , $1.Str);
         }
	;

struct_or_union_specifier
	: struct_or_union IDENTIFIER_tk '{' struct_declaration_list '}'
        { $$ := $1; }
	| struct_or_union '{' struct_declaration_list '}'
        { $$ := $1; }
	| struct_or_union IDENTIFIER_tk
        { $$ := $1; }
	;

struct_or_union
	: STRUCT_tk
        { $$ := $1;}
	| UNION_tk
        { $$ := $1;}
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| ':' CONSTANT_tk_expression
	| declarator ':' CONSTANT_tk_expression
	;

enum_specifier
	: ENUM_tk '{' enumerator_list '}'
         { $$ := $1; }
	| ENUM_tk IDENTIFIER_tk '{' enumerator_list '}'
         { $$ := $1; }
	| ENUM_tk IDENTIFIER_tk
         { $$ := $1; }
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: IDENTIFIER_tk
	| IDENTIFIER_tk '=' CONSTANT_tk_expression
	;

type_qualifier
	: CONST_tk
          { $$ :=  (Type_Qualifier,
            Position => $1.Position,
            Is_Const => True,
            Is_Volatile => False
            );    
          }
	| VOLATILE_tk
          { $$ :=  (Type_Qualifier,
            Position => $1.Position,
            Is_Volatile => True,
            Is_Const => False);    
          }
	;

declarator
	: pointer direct_declarator
        {  $$ := (Decl_Qualifier,
            Position => $1.Position,
            DeclStr => $2.DirectStr,
            Has_Dimensions => $2.Has_Array,
            Is_Pointer => True);
--           print("DeclaratorName", $1.Str);
        }
	| direct_declarator
        {  $$ := (Decl_Qualifier,
            Position => $1.Position,
            DeclStr => $1.DirectStr,
            Has_Dimensions => $1.Has_Array,
            Is_Pointer => False);
--           print("direct_declarator ", $1.DirectStr);
        }
	;

direct_declarator
	: IDENTIFIER_tk   
        {
--          print("Identifier  ", $1.Str);
--          $$ :=  $1; 
          $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.Str,
            Has_Array => False,
            Has_Pointer => False);
         }
	| '(' declarator ')'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $2.DeclStr,
            Has_Array => $2.Has_Dimensions,
            Has_Pointer => False);
        }
	| direct_declarator '[' CONSTANT_tk_expression ']'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.DirectStr,
            Has_Array => True,
            Has_Pointer => False);
        }
	| direct_declarator '[' ']'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.DirectStr,
            Has_Array => True,
            Has_Pointer => False);
        }
	| direct_declarator '(' parameter_type_list ')'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.DirectStr,
            Has_Array => False,
            Has_Pointer => False);
        }
	| direct_declarator '(' identifier_list ')'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.DirectStr,
            Has_Array => False,
            Has_Pointer => False);
        }
	| direct_declarator '(' ')'
        {  $$ := (Direct_Decl_Qualifier,
            Position => $1.Position,
            DirectStr => $1.DirectStr,
            Has_Array => False,
            Has_Pointer => False);
        }
	;

pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer
	| '*' type_qualifier_list pointer
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list
	| parameter_list ',' ELLIPSIS_tk
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: IDENTIFIER_tk
	| identifier_list ',' IDENTIFIER_tk
	;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' CONSTANT_tk_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' CONSTANT_tk_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: assignment_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER_tk ':' statement
	| CASE_tk CONSTANT_tk_expression ':' statement
	| DEFAULT_tk ':' statement
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	| '{' declaration_list '}'
	| '{' declaration_list statement_list '}'
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

statement_list
	: statement
	| statement_list statement
	;

expression_statement
	: ';'
	| expression ';'
	;

selection_statement
	: IF_tk '(' expression ')' statement
	| IF_tk '(' expression ')' statement ELSE_tk statement
	| SWITCH_tk '(' expression ')' statement
	;

iteration_statement
	: WHILE_tk '(' expression ')' statement
	| DO_tk statement WHILE_tk '(' expression ')' ';'
	| FOR_tk '(' expression_statement expression_statement ')' statement
	| FOR_tk '(' expression_statement expression_statement expression ')' statement
	;

jump_statement
	: GOTO_tk IDENTIFIER_tk ';'
	| CONTINUE_tk ';'
	| BREAK_tk ';'
	| RETURN_tk ';'
	| RETURN_tk expression ';'
	;

translation_unit
	: { Typename_List := C_Grammar.Typename_Lists.Create; }  external_declaration
	| translation_unit { Typename_List := C_Grammar.Typename_Lists.Create; }  external_declaration
	;

external_declaration
	:  function_definition
	|  declaration
	;

function_definition
	: declaration_specifiers declarator
        { 
--           print("function_definition Declarator_1 ", $2.Declstr);
           if $1.StorageInfo /= null and then $1.StorageInfo.Is_Typedef then
             C_Grammar.Typename_Lists.Attach($2.Declstr, Typename_List);
             print("Inserting typedef ", $2.Declstr);
           end if;
        }   declaration_list  compound_statement
	| declaration_specifiers declarator
        { 
--           print("function_definition Declarator_2 ", $2.Declstr);
           if $1.StorageInfo /= null and then $1.StorageInfo.Is_Typedef then
             C_Grammar.Typename_Lists.Attach($2.Declstr, Typename_List);
             print("Inserting typedef ", $2.Declstr);
           end if;
        }   compound_statement
	| declarator declaration_list compound_statement
	| declarator compound_statement
	;

%%

with C_Grammar_tokens, C_Lexer_io, C_Grammar_goto, C_Grammar_shift_reduce;
with C_Lexer, Ada.Text_IO;

use  C_Grammar_tokens, C_Lexer_io, C_Grammar_goto, C_Grammar_shift_reduce;
use  C_Lexer, Ada.Text_IO;

with Utils;
use Utils;
use Ada;

with Utils.Spellings;
use Utils.Spellings;
with Utils.Strings;
Use Utils.Strings;
with Utils.Messages;
use Utils.Messages;
with Utils.Sequences;
with Utils.Fast_Sets;
with C_Misc; use C_Misc;
with Utils.Output.Streams.Files;
use Utils.Output.Streams.Files;

package body C_Grammar is

    Name_Trace : Boolean := True;
--    Num_Annonymous_Classes : Integer := 0;

    procedure yyerror(s: in string := "syntax error") is
	error_col : constant Ada.Text_IO.positive_count := col;
    begin
	number_of_errors := number_of_errors + 1;
	put("<<< *** ");
	put_line(s);
	set_col(error_col);  -- Return to same column
    end yyerror;

    procedure print(s1: String ; s: Spelling)  is
    begin
	if Name_Trace then
	    put_line(s1 & ":  " & To_String(s));
	end if;
    end print;

    procedure print_string(s1: String )  is
    begin
	if Name_Trace then
	    put_line(s1 );
	end if;
    end print_string;

    procedure Output_String(str: String) is
    begin
        Utils.Output.Streams.Put(Listing, str);
    end Output_String;

    function Get_Annonymous_Class_Name return Spelling is
      classname : Spelling;
    begin
        Num_Annonymous_Classes := Num_Annonymous_Classes + 1;
        declare
          S : String := Integer'Image(Num_Annonymous_Classes);
          index : Integer := 0;
        begin
--            put_line("Annonymous class " & S);
            -- track nesting level for annonymous inner classes
            -- classname$nesting_level will be generated for
            -- annonymous inner class
            if Nested_Level > 0 then
              -- Decrement the num_annonymous_classes as it is the
              -- inner class within the same class
              Num_Annonymous_Classes := Num_Annonymous_Classes - 1;
              S := Integer'Image(Num_Annonymous_Classes);
              classname := Intern(S);      
              while index < Nested_Level loop
                classname := Intern( To_String(classname)  & "$"
                  & Integer'Image(1));
                index := index + 1;
              end loop;
              put_line("Nested Annonymous class " & To_String(classname));
              return classname ;
            end if;
            classname := Intern(S);
        end ;
        return classname; 
    end Get_Annonymous_Class_Name;

##%procedure_parse

end C_Grammar;

