
-- yacc-able Java 1 grammar
-- see notes at end
------------------------------------------------------------------

-- Single-character delimParamIters --
%token ',' ';' ':' '.'
%token '+' '-' '*' '/' '%'
%token '?'
%token '(' ')' '[' ']' '<' '>' '{' '}'
%token '|' '&' '^'
%token '=' '!' '~'
%token '@'

%token ABSTRACT_tk
%token ASSERT_tk
%token BOOLEAN_tk BREAK_tk BYTE_tk 
%token CASE_tk CATCH_tk CHAR_tk CLASS_tk CONTINUE_tk
%token DEFAULT_tk DO_tk DOUBLE_tk
%token ELSE_tk ENUM_tk EXTENDS_tk
%token FINAL_tk FINALLY_tk FLOAT_tk FOR_tk FUTURE_tk
%token GOTO_tk
%token IF_tk IMPLEMENTS_tk IMPORT_tk INSTANCEOF_tk INT_tk INTERFACE_tk
%token LONG_tk
%token NATIVE_tk NEW_tk JNULL_tk
%token OPERATOR_tk 
%token PACKAGE_tk PRIVATE_tk PROTECTED_tk PUBLIC_tk
%token RETURN_tk
%token SHORT_tk STATIC_tk SUPER_tk SWITCH_tk SYNCHRONIZED_tk
%token THIS_tk THROW_tk THROWS_tk TRANSIENT_tk TRY_tk
%token VOID_tk VOLATILE_tk
%token WHILE_tk
%token OP_INC_tk OP_DEC_tk
%token OP_SHL_tk OP_SHR_tk OP_SHRR_tk
%token OP_GE_tk OP_LE_tk OP_EQ_tk OP_NE_tk
%token OP_LAND_tk OP_LOR_tk
%token OP_DIM_tk
%token ASGN_MUL_tk ASGN_DIV_tk ASGN_MOD_tk ASGN_ADD_tk ASGN_SUB_tk
%token ASGN_SHL_tk ASGN_SHR_tk ASGN_SHRR_tk ASGN_AND_tk ASGN_XOR_tk ASGN_OR_tk
%token IDENTIFIER_tk LITERAL_tk BOOLLIT_tk

%start CompilationUnit

%with Std_Output;
%use Std_Output;
%with Utils.Messages;
%with Utils.Spellings;
%use Utils;
%use Utils.Spellings;
%with Utils.Sequences;
%with Utils.Fast_Sets;
%with Misc; 
%use Misc;
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
      Dim_Qualifier    
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

    package Arg_Lists is new Lists(ItemType => TypeArg_Ptr, Equal => "=");
    
    package Param_Lists is new Lists(ItemType => Misc.Param_Ptr, Equal => "=");
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

    type yystype(Kind : yystype_enum := Simple_Token) is record
      Position : Lex_Source_Position;
      case Kind is
	when Simple_Token =>
          Str : Spellings.Spelling;
        when Decl_Qualifier =>
          DeclStr : Spellings.Spelling;
          Has_Dimensions : Boolean := False;
        when Param_Qualifier =>
          ParamInfo : Misc.Param_Ptr;
        when Param_List_Qualifier =>
          ParamList : Param_Lists.List;
        when Type_Specifier_Qualifier =>
          TypeSpecInfo : Misc.TypeSpec_Ptr;
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
  
    Type_Spec_Node : Misc.TypeSpec_Ptr;
    Param_Node : Misc.Param_Ptr;
    Param_List : Misc.Param_Ptr := null;
    TParam_List : Param_Lists.List;
    TArgs_List  : Arg_Lists.List;
    TMod_List   : Spelling_Lists.List;
    ParamIter        : Param_Lists.ListIter;
    Classname_Stack : Classname_Stacks.Temp_Seq;
    Methodname_Stack : Methodname_Stacks.Temp_Seq;
    Interface_Set  : Interface_Sets.Set;

}


%%

TypeSpecifier
	: TypeName 
        { Type_Specifier := Type_Name; 
          Type_Spec_Node := Create_Type_Specifier(
           Name =>  $1.Str, Dims => 0, Has_Args => False);
          $$ := (Type_Specifier_Qualifier,
            Position => $1.Position,
            TypeSpecInfo => Type_Spec_Node,
            ArgsList => Arg_Lists.Create  );
         }
	| QualifiedName Dims
        { Type_Specifier := Type_Name; 
          Type_Spec_Node := Create_Type_Specifier(
           Name =>  $1.Str, Dims => $2.DimInfo,
               Has_Args => False);
          $$ := (Type_Specifier_Qualifier,
            Position => $1.Position,
            TypeSpecInfo => Type_Spec_Node,
            ArgsList => Arg_Lists.Create  );
         }
	| PrimitiveType Dims
        { Type_Specifier := Type_Name; 
          Type_Spec_Node := Create_Type_Specifier(
           Name =>  $1.Str, Dims => $2.DimInfo,
               Has_Args => False);
          $$ := (Type_Specifier_Qualifier,
            Position => $1.Position,
            TypeSpecInfo => Type_Spec_Node,
            ArgsList => Arg_Lists.Create  );
         }
	| TypeName TypeArguments Dims
        {  
          Type_Spec_Node := Create_Type_Specifier(
           Name =>  $1.Str, Dims => $3.DimInfo, Has_Args => True);
          $$ := (Type_Specifier_Qualifier,
            Position => $1.Position,
            TypeSpecInfo => Type_Spec_Node,
            ArgsList => $2.TypeArgsList);
         }
	| TypeName TypeArguments
        {  
          Type_Spec_Node := Create_Type_Specifier(
           Name =>  $1.Str, Dims => 0, Has_Args => True);
          $$ := (Type_Specifier_Qualifier,
            Position => $1.Position,
            TypeSpecInfo => Type_Spec_Node,
            ArgsList => $2.TypeArgsList);
         }
	;

TypeName
	: PrimitiveType
        { Type_Name := $1.Str;  
          $$ := $1; }
	| QualifiedName 
        { Type_Name := Qualified_Name;
          $$ := $1; }
	;

TypeArguments_opt
	:  {  
             $$ := (Type_Arg_List_Qualifier, Position => Lex_Null_Source_Position,
             TypeArgsList => Arg_Lists.Create);  
           }
	| TypeArguments
         { $$ := $1 ;}
	;

TypeArguments
	: '<' TypeArgumentList '>'
         { $$ := $2 ;}
	;

TypeArgumentList 
	: TypeArgument  
        { TArgs_List  := Arg_Lists.Create;
          Arg_Lists.Attach(new yystype'($1), TArgs_List);
          $$ := (Type_Arg_List_Qualifier, Position => $1.Position,
             TypeArgsList => TArgs_List);    
        }
	| TypeArgumentList ',' TypeArgument
        {
          $$ := $1;
          Arg_Lists.Attach(new yystype'($3), $$.TypeArgsList);
        }
	;

TypeArgument
	: TypeSpecifier  
        { $$ := (Type_Arg_Qualifier, Position => $1.Position,
		  ArgKind => Regular_Arg,
		  ArgInfo => new yystype'($1));
        }
	| '?'
        { $$ := (Type_Arg_Qualifier, Position => $1.Position,
		  ArgKind => Wildcard_Arg,
		  ArgInfo => Null);
        }
	| '?' EXTENDS_tk TypeSpecifier
        { $$ := (Type_Arg_Qualifier, Position => $1.Position,
		  ArgKind => Extends_Arg,
		  ArgInfo => new yystype'($3));
        }
	| '?' SUPER_tk TypeSpecifier
        { $$ := (Type_Arg_Qualifier, Position => $1.Position,
		  ArgKind => Super_Arg,
		  ArgInfo => new yystype'($3));
        }
	;

ClassNameList
        : QualifiedName 
        | ClassNameList ',' QualifiedName 
	;

PrimitiveType
	: BOOLEAN_tk  
         { $$ := $1 ;}
	| CHAR_tk
         { $$ := $1 ;}
	| BYTE_tk
         { $$ := $1 ;}
	| SHORT_tk
         { $$ := $1 ;}
	| INT_tk
         { $$ := $1 ;}
	| LONG_tk
         { $$ := $1 ;}
	| FLOAT_tk
         { $$ := $1 ;}
	| DOUBLE_tk
         { $$ := $1 ;}
	| VOID_tk
         { $$ := $1 ;}
	;

SemiColons
	: ';'
        | SemiColons ';'
        ;

CompilationUnit
	: ProgramFile
        { 
         Java_Lexer.Flush_All_Lex_Buffer(Listing);
         Interface_Sets.Remove_All(Interface_Set); 
--         Classname_Stacks.Free_Seq(Classname_Stack);
        }
        ;

ProgramFile
	: PackageStatement ImportStatements TypeDeclarations
	| PackageStatement ImportStatements
	| PackageStatement                  TypeDeclarations
	|                  ImportStatements TypeDeclarations
	| PackageStatement
	|                  ImportStatements
	|                                   TypeDeclarations
	;

PackageStatement
	: PACKAGE_tk QualifiedName SemiColons
	;

TypeDeclarations
	: TypeDeclarationOptSemi
	| TypeDeclarations TypeDeclarationOptSemi
	;

TypeDeclarationOptSemi
        : TypeDeclaration
        | TypeDeclaration SemiColons
        ;

ImportStatements
	: ImportStatement
	| ImportStatements ImportStatement
	;

ImportStatement
	: IMPORT_tk QualifiedName SemiColons
	| IMPORT_tk QualifiedName '.' '*' SemiColons
	;

QualifiedName
	: IDENTIFIER_tk
	{ Qualified_Name := $1.Str;
          $$ := $1;
        }
	| QualifiedName '.' IDENTIFIER_tk
	{ tempQualified_Name := Qualified_Name; 
           Qualified_Name :=  "." & $3.Str;
           Qualified_Name := tempQualified_Name & To_String(Qualified_Name);
           $$ := (Simple_Token,
             Position => $3.Position,
             Str => Qualified_Name);
        }
	;

TypeDeclaration
	: ClassHeader ClassBody
         {
           Classname_Stacks.Pop(Classname_Stack); 
         }
        | EnumDeclaration
        | AnnotationTypeDeclaration
	;

ClassBody
	: '{' FieldDeclarations '}'
	| '{' '}'
	;

ClassHeader
	: Modifiers_opt ClassWord IDENTIFIER_tk TypeParameters_opt 
	    Extends_opt Interfaces_opt
	{  Class_Name := $3.Str;
--           print("Parse Classname", Class_Name);
           Classname_Stacks.Push(Classname_Stack, Class_Name); 
           --  Have to include abstract classes also
           if Contains(To_String($2.Str), "interface") or 
             Contains(To_String($2.Str), "Interface") then
--               print("Interface", Class_Name);
               Interface_Sets.Include(Interface_Set,Class_Name);
           end if;
        }
	;

Modifiers_opt 
	: {
          $$ := (Mod_List_Qualifier, Position => Lex_Null_Source_Position,
             ModList => Spelling_Lists.Create);    }
	| Modifiers
        { $$ := $1; }
	;

Modifiers
	: Modifier
        { TMod_List  := Spelling_Lists.Create;
          Spelling_Lists.Attach($1.Str, TMod_List);
          $$ := (Mod_List_Qualifier, Position => $1.Position,
             ModList => TMod_List);    
        }
	| Modifiers Modifier
        { $$ := $1;
          Spelling_Lists.Attach($2.Str, $1.ModList);
        }
	;

Modifier
	: ABSTRACT_tk
         { $$ := $1 ;}
	| FINAL_tk
         { $$ := $1 ;}
	| PUBLIC_tk
         { $$ := $1 ;}
	| PROTECTED_tk
         { $$ := $1 ;}
	| PRIVATE_tk
         { $$ := $1 ;}
	| STATIC_tk
         { $$ := $1 ;}
	| TRANSIENT_tk
         { $$ := $1 ;}
	| VOLATILE_tk
         { $$ := $1 ;}
	| NATIVE_tk
         { $$ := $1 ;}
	| SYNCHRONIZED_tk
         { $$ := $1 ;}
	;

ClassWord
	: CLASS_tk
        { $$ := $1 ;}
	| INTERFACE_tk
        { $$ := $1 ;}
	;

Interfaces_opt
	:
	| Interfaces
	;

Interfaces
	: IMPLEMENTS_tk ClassNameList
	;

TypeParameters_opt
	:
	| TypeParameters
	;

TypeParameters
	: '<' TypeParameterList '>'
	;

TypeParameterList 
	: TypeParameter
	| TypeParameterList ',' TypeParameter
	;

TypeParameter
	: IDENTIFIER_tk  { Id_Type := $1.Str;
--                           print("TypeParameter" , Id_Type);
                          }
	| IDENTIFIER_tk EXTENDS_tk Bound
	;

Bound
	: TypeSpecifier
	| Bound '&' TypeSpecifier
	;


EnumDeclaration
	: Modifiers_opt ENUM_tk IDENTIFIER_tk Interfaces_opt EnumBody
	;

EnumBody
	: '{' EnumConstants_opt EnumBodyDeclarations_opt '}'
	;

Comma_opt
	:
	| ','
	;

EnumConstants_opt
	:
	| EnumConstants Comma_opt
	;

EnumConstants
	: EnumConstant
	| EnumConstants ',' EnumConstant
	;

EnumConstant
	: IDENTIFIER_tk Arguments_opt ClassBody_opt
	;

Arguments_opt
	:
	| Arguments
	;

ClassBody_opt
	:
	| ClassBody
	;

EnumBodyDeclarations_opt
	:
	| EnumBodyDeclarations
	;

EnumBodyDeclarations
	: ';' FieldDeclarations
	| ';'
	;

AnnotationTypeDeclaration
	: '@' INTERFACE_tk IDENTIFIER_tk AnnotationTypeBody
	;

AnnotationTypeBody
	: '{' AnnotationTypeElementDeclarations_opt '}'
	;

AnnotationTypeElementDeclarations_opt
	:
	| AnnotationTypeElementDeclarations
	;

AnnotationTypeElementDeclarations
	: AnnotationTypeElementDeclaration
        | AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration
	;

AnnotationTypeElementDeclaration
	: AnnotationTypeElementRest
	;

AnnotationTypeElementRest
	: Modifiers_opt TypeSpecifier IDENTIFIER_tk 
	     AnnotationMethodOrConstantRest ';'
        | TypeDeclaration
	;

AnnotationMethodOrConstantRest
	: AnnotationMethodRest
        | AnnotationConstantRest
	;

AnnotationMethodRest
	: '(' ')' DefaultValue_opt
	;

AnnotationConstantRest
	: VariableDeclarators
	;

DefaultValue_opt
	:
	| DefaultValue
	;

DefaultValue
	: DEFAULT_tk ElementValue
	;

Annotations
	: 
	| Annotations Annotation
	;

Annotation
	: '@' TypeName
	| '@' TypeName '(' ElementValue ')'
	| '@' TypeName '(' IDENTIFIER_tk '=' ElementValue ')'
	;

ElementValue
	: ConditionalExpression
        | Annotation
        | ElementValueArrayInitializer
	;

ElementValueArrayInitializer
	: '{' ElementValues Comma_opt '}'
	;

ElementValues
	:
	| ElementValues ',' ElementValue
	;

FieldDeclarations
	: FieldDeclarationOptSemi
        | FieldDeclarations FieldDeclarationOptSemi
	;

FieldDeclarationOptSemi
        : FieldDeclaration
        | FieldDeclaration SemiColons
        ;

FieldDeclaration
	: FieldVariableDeclaration ';'
	| MethodDeclaration
	| ConstructorDeclaration
	| TypeParameters MethodDeclaration
	| TypeParameters ConstructorDeclaration
	| StaticInitializer
        | NonStaticInitializer
        | TypeDeclaration
	;

FieldVariableDeclaration
	: Modifiers_opt TypeSpecifier VariableDeclarators
	;

VariableDeclarators
	: VariableDeclarator
	| VariableDeclarators ',' VariableDeclarator
	;

VariableDeclarator
	: DeclaratorName
	| DeclaratorName '=' VariableInitializer
	;

VariableInitializer
	: Expression
	| '{' '}'
        | '{' ArrayInitializers '}'
        ;

ArrayInitializers
	: VariableInitializer
	| ArrayInitializers ',' VariableInitializer
	| ArrayInitializers ','
	;

MethodDeclaration
	: Modifiers_opt TypeSpecifier MethodDeclarator Throws_opt MethodBody
        { Return_Type := $2.TypeSpecInfo.Type_Name;
--          print("ReturnType", Return_Type);
          $$ := (Method_Decln_Qualifier, Position => $1.Position,
            MethodMods => $1.ModList,
	    ReturnType  => new yystype'($2),
            MethodDeclr => new yystype'($3));
        }
	;

MethodDeclarator
	: DeclaratorName '(' { Java_Lexer.Flush_Lex_Buffer(Listing, $2.Position.Lex_Index + 1); Method_Name := $1.DeclStr; } ParameterList ')' 
        { --  Method_Name := $1.DeclStr;
--          print("Method_Name", Method_Name);
          $$ := (Method_Decl_Qualifier, Position => $1.Position,
            MethodName => $1.DeclStr,
            MethodParams => $4.ParamList,
            Method_Dimensions => False,
            Has_Params => True );
       
          ParamIter := Param_Lists.MakeListIter($4.ParamList);
          while Param_Lists.More(ParamIter) loop
              Param_Lists.Next(ParamIter, Param_Node);
--              print("Parameter", Param_Node.Param_Name);
          end loop;
        }
	| DeclaratorName '(' ')'
        { Method_Name :=  $1.DeclStr;
--          print("Method_Name", Method_Name);
          $$ := (Method_Decl_Qualifier, Position => $1.Position,
            MethodName => $1.DeclStr,
            MethodParams => Param_Lists.Create,
            Method_Dimensions => False,
            Has_Params => False );
        }
	| MethodDeclarator OP_DIM_tk
        { $$ := $1;
          $$.Method_Dimensions := True;
        }
	;

ParameterList
        : Parameter
        { TParam_List  := Param_Lists.Create;
          Param_Lists.Attach($1.ParamInfo, TParam_List);
          $$ := (Param_List_Qualifier, Position => $1.Position,
             ParamList => TParam_List);    
        }
	| ParameterList ',' {  Java_Lexer.Flush_Lex_Buffer(Listing, $2.Position.Lex_Index + 1); } Parameter
        {  $$ := $1;
          Param_Lists.Attach($4.ParamInfo, $1.ParamList);
        }
	;

Parameter
	: TypeSpecifier DeclaratorName
        { Param_Name := $2.DeclStr;
          Param_Type := $1.TypeSpecInfo.Type_Name;
          Param_Node := Create_Param(Param_Type, Param_Name, False);
   --       Misc.Insert_Param(Param_List,Param_Node);
          $$ := (Param_Qualifier,
            Position => $1.Position,
            ParamInfo => Param_Node );
--          print("Type " , Param_Type);
--          print("ParamName ", Param_Name);
          -- Ignore for Interfaces as they don't have any code
          if not Interface_Sets.Is_Present(
            Interface_Set, Classname_Stacks.Top(Classname_Stack)) then
              Output_String(Misc.Get_Arg_Type($1.Position.File_Name,
               Classname_Stacks.Top(Classname_Stack),
               Method_Name, Param_Name, $1.Position.MSP));
          end if;
          
        }
        | FINAL_tk TypeSpecifier DeclaratorName
        { Param_Name := $3.DeclStr;
--          print("ParamName", Param_Name);
          Param_Type := $2.TypeSpecInfo.Type_Name;
--          print("ParamType", Param_Type);
          Param_Node := Create_Param(Param_Type, Param_Name, True);
   --       Misc.Insert_Param(Param_List,Param_Node);
          $$ := (Param_Qualifier,
            Position => $1.Position,
            ParamInfo => Param_Node );
          -- Ignore for Interfaces as they don't have any code
          if not Interface_Sets.Is_Present(
            Interface_Set, Classname_Stacks.Top(Classname_Stack)) then
              Output_String(Misc.Get_Arg_Type($2.Position.File_Name,
               Classname_Stacks.Top(Classname_Stack),
               Method_Name, Param_Name, $2.Position.MSP));
          end if;
        }
	;

DeclaratorName
	: IDENTIFIER_tk
	{ Decl_Name := $1.Str;
          $$ := (Decl_Qualifier, 
            Position  => $1.Position,
            DeclStr => $1.Str,
            Has_Dimensions =>  False);
--           print("DeclaratorName" ,Decl_Name);
        }
        | DeclaratorName OP_DIM_tk

	{ Decl_Name := $1.DeclStr;
          $$ := (Decl_Qualifier, 
            Position  => $1.Position,
            DeclStr => $1.DeclStr,
            Has_Dimensions =>  True);
--           print("DeclaratorName" ,Decl_Name);
        }
        ;

Throws_opt
	:
	| Throws
	;

Throws
	: THROWS_tk ClassNameList
	;

MethodBody
	: Block
	| ';'
	;

ConstructorDeclaration
	: Modifiers_opt ConstructorDeclarator Throws_opt Block
	;

ConstructorDeclarator
	: IDENTIFIER_tk '(' { Java_Lexer.Flush_Lex_Buffer(Listing, $2.Position.Lex_Index+ 1); Method_Name := $1.Str; } ParameterList ')'
        {
          $$ := (Constr_Decl_Qualifier, Position => $1.Position,
            ConstrName => $1.Str,
            ConstrParams => $4.ParamList);
         }
	| IDENTIFIER_tk '(' ')'
        { Method_Name :=  $1.Str;
--          print("Method_Name", Method_Name);
          $$ := (Constr_Decl_Qualifier, Position => $1.Position,
            ConstrName => $1.Str,
            ConstrParams => Param_Lists.Create);
        }
	;

StaticInitializer
	: STATIC_tk Block
	;

NonStaticInitializer
        : Block
        ;

Extends_opt
	:
	| Extends
	;

Extends
	: EXTENDS_tk TypeName
	| Extends ',' TypeName
	;

Block
	: '{' LocalVariableDeclarationsAndStatements '}'
	| '{' '}'
        ;

LocalVariableDeclarationsAndStatements
	: LocalVariableDeclarationOrStatement
	| LocalVariableDeclarationsAndStatements LocalVariableDeclarationOrStatement
	;

LocalVariableDeclarationOrStatement
	: LocalVariableDeclarationStatement
	| Statement
	;

LocalVariableDeclarationStatement
	: TypeSpecifier VariableDeclarators ';'
        | FINAL_tk TypeSpecifier VariableDeclarators ';'
	;

Statement
	: EmptyStatement
	| AssertStatement
	| LabelStatement
	| ExpressionStatement ';'
        | SelectionStatement
        | IterationStatement
	| JumpStatement
	| GuardingStatement
	| Block
	;

EmptyStatement
	: ';'
        ;

AssertStatement
	: ASSERT_tk Expression ';'
	| ASSERT_tk Expression ':' Expression ';'
	;

LabelStatement
	: IDENTIFIER_tk ':'
        | CASE_tk ConstantExpression ':'
	| DEFAULT_tk ':'
        ;

ExpressionStatement
	: AdditiveExpression  -- Full expression causes ambiguity here
	| UnaryExpression AssignmentOperator AssignmentExpression
	;

SelectionStatement
	: IF_tk '(' Expression ')' Statement
        | IF_tk '(' Expression ')' Statement ELSE_tk Statement
        | SWITCH_tk '(' Expression ')' Block
        ;

IterationStatement
	: WHILE_tk '(' Expression ')' Statement
	| DO_tk Statement WHILE_tk '(' Expression ')' ';'
	| FOR_tk '(' ForInit ';' ForExpr ForIncr ')' Statement
	| FOR_tk '(' ForInit ';' ForExpr         ')' Statement
	| FOR_tk '(' ForInit ':' Expression         ')' Statement
	;

ForInit
	: ExpressionStatements 
	| TypeSpecifier VariableDeclarators
	|
	;

ForExpr
	: Expression ';'
	| ';'
	;

ForIncr
	: ExpressionStatements
	;

ExpressionStatements
	: ExpressionStatement
	| ExpressionStatements ',' ExpressionStatement
	;

JumpStatement
	: BREAK_tk IDENTIFIER_tk ';'
	| BREAK_tk            ';'
        | CONTINUE_tk IDENTIFIER_tk ';'
	| CONTINUE_tk            ';'
	| RETURN_tk Expression ';'
	| RETURN_tk            ';'
	| THROW_tk Expression ';'
	;

GuardingStatement
	: SYNCHRONIZED_tk '(' Expression ')' Statement
	| TRY_tk Block Finally
	| TRY_tk Block Catches
	| TRY_tk Block Catches Finally
	;

Catches
	: Catch
	| Catches Catch
	;

Catch
	: CatchHeader Block
	;

CatchHeader
	: CATCH_tk '(' TypeSpecifier IDENTIFIER_tk ')'
	| CATCH_tk '(' TypeSpecifier ')'
	;

Finally
	: FINALLY_tk Block
	;

PrimaryExpression
	: QualifiedName
	| NotJustName
	;

NotJustName
	: SpecialName
	| NewAllocationExpression
	| ComplexPrimary
	;

ComplexPrimary
	: '(' Expression ')'
	| ComplexPrimaryNoParenthesis
	;

ComplexPrimaryNoParenthesis
	: LITERAL_tk
	| BOOLLIT_tk
	| ArrayAccess
	| FieldAccess
	| MethodCall
	;

ArrayAccess
	: QualifiedName '[' Expression ']'
	| ComplexPrimary '[' Expression ']'
	;

FieldAccess
	: NotJustName '.' IDENTIFIER_tk
	| RealPostfixExpression '.' IDENTIFIER_tk
        | QualifiedName '.' THIS_tk
        | QualifiedName '.' CLASS_tk
        | QualifiedName Dims '.' CLASS_tk
        | PrimitiveType '.' CLASS_tk
        | PrimitiveType Dims '.' CLASS_tk
	;

MethodCall
	: MethodAccess Arguments
	;

Arguments
	: '(' ArgumentList ')'
	| '(' ')'
	;

MethodAccess
	: ComplexPrimaryNoParenthesis
	| SpecialName
	| QualifiedName
	;

SpecialName
	: THIS_tk
	| SUPER_tk
	| JNULL_tk
	;

ArgumentList
	: Expression
	| ArgumentList ',' Expression
	;

NewAllocationExpression
        : PlainNewAllocationExpression
        | QualifiedName '.' PlainNewAllocationExpression
        ;

PlainNewAllocationExpression
    	: ArrayAllocationExpression
    	| ClassAllocationExpression
    	| ArrayAllocationExpression '{' '}'
    	| ClassAllocationExpression '{' { Classname_Stacks.Push(Classname_Stack, Get_Annonymous_Class_Name); Nested_Level := Nested_Level + 1; }  '}' { Classname_Stacks.Pop(Classname_Stack);Nested_Level := Nested_Level - 1; }
    	| ArrayAllocationExpression '{' ArrayInitializers '}'
    	| ClassAllocationExpression '{' { Classname_Stacks.Push(Classname_Stack, Get_Annonymous_Class_Name); Nested_Level := Nested_Level + 1 ; }  FieldDeclarations { Classname_Stacks.Pop(Classname_Stack); Nested_Level := Nested_Level - 1;}  '}'
    	;

ClassAllocationExpression
	: NEW_tk TypeName TypeArguments_opt '(' ArgumentList ')' 
	| NEW_tk TypeName TypeArguments_opt '('              ')' 
        ;

ArrayAllocationExpression
	: NEW_tk TypeName TypeArguments_opt DimExprs Dims
	| NEW_tk TypeName TypeArguments_opt DimExprs
        | NEW_tk TypeName TypeArguments_opt Dims
	;

DimExprs
	: DimExpr
	| DimExprs DimExpr
	;

DimExpr
	: '[' Expression ']'
	;

Dims_opt
	: {
          $$ := (Dim_Qualifier,
            Position => Lex_Null_Source_Position,
            DimInfo => 0
            );
          }
	| Dims
	;

Dims
	: OP_DIM_tk
         {
          $$ := (Dim_Qualifier,
            Position => $1.Position,
            DimInfo => 1
            );
          }
	| Dims OP_DIM_tk
         { $$ := $1;
           $$.DimInfo := $$.DimInfo + 1;
         }
	;

PostfixExpression
	: PrimaryExpression
	| RealPostfixExpression
	;

RealPostfixExpression
	: PostfixExpression OP_INC_tk
	| PostfixExpression OP_DEC_tk
	;

UnaryExpression
	: OP_INC_tk UnaryExpression
	| OP_DEC_tk UnaryExpression
	| ArithmeticUnaryOperator CastExpression
	| LogicalUnaryExpression
	;

LogicalUnaryExpression
	: PostfixExpression
	| LogicalUnaryOperator UnaryExpression
	;

LogicalUnaryOperator
	: '~'
	| '!'
	;

ArithmeticUnaryOperator
	: '+'
	| '-'
	;

CastExpression
	: UnaryExpression
	| '(' PrimitiveTypeExpression ')' CastExpression
	| '(' ClassTypeExpression ')' CastExpression
	| '(' Expression ')' LogicalUnaryExpression
        | '(' ShiftExpression '<' ShiftExpression ',' 
		TypeParameterList '>' ')' CastExpression
        | '(' ShiftExpression '<' ShiftExpression '>' ')' CastExpression
	;

PrimitiveTypeExpression
	: PrimitiveType
        | PrimitiveType Dims
        ;

ClassTypeExpression
	: QualifiedName Dims
        ;

MultiplicativeExpression
	: CastExpression
	| MultiplicativeExpression '*' CastExpression
	| MultiplicativeExpression '/' CastExpression
	| MultiplicativeExpression '%' CastExpression
	;

AdditiveExpression
	: MultiplicativeExpression
        | AdditiveExpression '+' MultiplicativeExpression
	| AdditiveExpression '-' MultiplicativeExpression
        ;

ShiftExpression
	: AdditiveExpression
--         | ShiftExpression OP_SHL_tk AdditiveExpression
--         | ShiftExpression OP_SHR_tk AdditiveExpression
--         | ShiftExpression OP_SHRR_tk AdditiveExpression
        | ShiftExpression '<' '<' AdditiveExpression
        | ShiftExpression '>' '>' AdditiveExpression
        | ShiftExpression '>' '>' '>' AdditiveExpression
	;

RelationalExpression
	: ShiftExpression
        | ShiftExpression '<' ShiftExpression  -- To avoid ambiguity
	| ShiftExpression '>' ShiftExpression
	| ShiftExpression OP_LE_tk ShiftExpression
	| ShiftExpression OP_GE_tk ShiftExpression
	;

EqualityExpression
	: RelationalExpression
        | EqualityExpression OP_EQ_tk RelationalExpression
        | EqualityExpression OP_NE_tk RelationalExpression
	| EqualityExpression INSTANCEOF_tk TypeName Dims_opt
        ;

AndExpression
	: EqualityExpression
        | AndExpression '&' EqualityExpression
        ;

ExclusiveOrExpression
	: AndExpression
	| ExclusiveOrExpression '^' AndExpression
	;

InclusiveOrExpression
	: ExclusiveOrExpression
	| InclusiveOrExpression '|' ExclusiveOrExpression
	;

ConditionalAndExpression
	: InclusiveOrExpression
	| ConditionalAndExpression OP_LAND_tk InclusiveOrExpression
	;

ConditionalOrExpression
	: ConditionalAndExpression
	| ConditionalOrExpression OP_LOR_tk ConditionalAndExpression
	;

ConditionalExpression
	: ConditionalOrExpression
	| ConditionalOrExpression '?' Expression ':' ConditionalExpression
	;

AssignmentExpression
	: ConditionalExpression
	| UnaryExpression AssignmentOperator AssignmentExpression
	;

AssignmentOperator
	: '='
	| ASGN_MUL_tk
	| ASGN_DIV_tk
	| ASGN_MOD_tk
	| ASGN_ADD_tk
	| ASGN_SUB_tk
	| ASGN_SHL_tk
	| ASGN_SHR_tk
	| ASGN_SHRR_tk
	| ASGN_AND_tk
	| ASGN_XOR_tk
	| ASGN_OR_tk
	;

Expression
	: AssignmentExpression
        ;

ConstantExpression
	: ConditionalExpression
	;

%%

--------------------------------------------------------------------
-- Copyright (C)
--   1996, 1997, 1998 Dmitri Bronnikov, All rights reserved.
--
-- THIS GRAMMAR IS PROVIDED "AS IS" WITHOUT  ANY  EXPRESS  OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR
-- PURPOSE, OR NON-INFRINGMENT.
--
-- Bronikov@inreach.com
--
--------------------------------------------------------------------
--
-- VERSION 1.06 DATE 20 AUG 1998
--
--------------------------------------------------------------------
--
-- UPDATES
--
-- 1.06 Correction of Java 1.1 syntax
-- 1.05 Yet more Java 1.1
--      .
-- 1.04 More Java 1.1 features:
--      .this
--      .class
-- 1.03 Added Java 1.1 features:
--      inner classes,
--      anonymous classes,
--      non-static initializer blocks,
--      array initialization by new operator
-- 1.02 Corrected cast expression syntax
-- 1.01 All shift/reduce conflicts, except dangling else, resolved
--
--------------------------------------------------------------------
--
-- PARSING CONFLICTS RESOLVED
--
-- Some Shift/Reduce conflicts have been resolved at the expense of
-- the grammar defines a superset of the language. The following
-- actions have to be performed to complete program syntax checking:
--
-- 1) Check that modifiers applied to a class, interface, field,
--    or constructor are allowed in respectively a class, inteface,
--    field or constructor declaration. For example, a class
--    declaration should not allow other modifiers than abstract,
--    final and public.
--
-- 2) For an expression statement, check it is either increment, or
--    decrement, or assignment expression.
--
-- 3) Check that type expression in a cast operator indicates a type.
--    Some of the compilers that I have tested will allow simultaneous
--    use of identically named type and variable in the same scope
--    depending on context.
--
-- 4) Change lexical definition to change '[' optionally followed by
--    any number of white-space characters immediately followed by ']'
--    to OP_DIM token. I defined this token as [\[]{white_space}*[\]]
--    in the lexer.
--
--------------------------------------------------------------------
--
-- UNRESOLVED SHIFT/REDUCE CONFLICTS
--
-- Dangling else in if-then-else
--
--------------------------------------------------------------------


with Java_Parser_tokens, Java_Lexer_io, Java_Parser_goto, Java_Parser_shift_reduce;
with Java_Lexer, Ada.Text_IO;

use  Java_Parser_tokens, Java_Lexer_io, Java_Parser_goto, Java_Parser_shift_reduce;
use  Java_Lexer, Ada.Text_IO;

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
with Misc; use Misc;
with Utils.Output.Streams.Files;
use Utils.Output.Streams.Files;

package body Java_Parser is

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

end Java_Parser;

