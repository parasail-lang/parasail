-- This is a yacc-able parser for the entire ANSI C++ grammar with no unresolved conflicts. 
-- The parse is SYNTACTICALLY consistent and requires no template or type name assistance.
-- The grammar in the C++ standard notes that its grammar is a superset of the true
-- grammar requiring semantic constraints to resolve ambiguities. This grammar is a really big
-- superset unifying expressions and declarations, eliminating the type/non-type distinction,
-- and iterating to find a consistent solution to the template/arith,metoic < ambiguity.
-- As a result the grammar is much simpler, but requires the missing semantic constraints to be
-- performed in a subsequent semantic pass, which is of course where they belong. This grammar will
-- support conversion of C++ tokens into an Abstract Syntax Tree. A lot of further work is required to
-- make that tree useful. 
--
--  Author:         E.D.Willink             Ed.Willink@rrl.co.uk
--  Date:           19-Nov-1999
-- 
--
-- The lexer (and/or a preprocessor) is expected to identify the following
--
--  Punctuation:
-- 
%token '+' '-' '*' '/' '%' '^' '&' '|' '~' '!' '<'  '>' '=' ':' '[' ']' '{' '}' '(' ')'
%token '?' '.' ''' '"' '\' '@' '$' ';' ','
--
--  Punctuation sequences
-- 
%token ARROW ARROW_STAR DEC EQ GE INC LE LOG_AND LOG_OR NE SHL SHR
%token ASS_ADD ASS_AND ASS_DIV ASS_MOD ASS_MUL ASS_OR ASS_SHL ASS_SHR ASS_SUB ASS_XOR
%token DOT_STAR ELLIPSIS SCOPE
--
--  Reserved words
-- 
%token PRIVATE PROTECTED PUBLIC
%token BOOL CHAR DOUBLE FLOAT INT LONG SHORT SIGNED UNSIGNED VOID WCHAR_T
%token CLASS ENUM NAMESPACE STRUCT TYPENAME UNION
%token CONST VOLATILE
%token AUTO EXPLICIT EXPORT EXTERN FRIEND INLINE MUTABLE REGISTER STATIC TEMPLATE TYPEDEF USING VIRTUAL
%token ASM BREAK CASE CATCH CONST_CAST CONTINUE DEFAULT DELETE DO DYNAMIC_CAST
%token ELSE FALSE FOR GOTO IF NEW OPERATOR_tk REINTERPRET_CAST RETURN
%token SIZEOF STATIC_CAST SWITCH THIS THROW TRUE TRY TYPEID WHILE
--
--  Parametric values.
-- 
%token CharacterLiteral
%token FloatingLiteral
%token Identifier_tk
%token IntegerLiteral
%token NumberLiteral
%token StringLiteral
--
--  The lexer need not treat '0' as distinct from IntegerLiteral in the hope that pure-specifier can
--  be distinguished, It isn't. Semantic rescue from = constant-expression is necessary.
--
--  The lexer is not required to distinguish template or type names, although a slight simplification to the
--  grammar and elaboration of the action rules could make good use of template name information.
--
--  In return for not needing to use semantic information, the lexer must support back-tracking, which
--  is easily achieved by a simple linear buffer, a reference implementation of which may be found in the
--  accompanying CxxParsing.cxx. Back-tracking is used to support:
--
--  Binary search for a consistent parse of the template/arithmetic ambiguity.
--      start_search() initialises the search
--      advance_search() iterates the search
--      end_search() cleans up after a search
--      template_test() maintains context during a search
--
--  Lookahead to resolve the inheritance/anonymous bit-field similarity
--      mark() saves the starting context
--      unmark() pops it
--      rewind_colon() restores the context and forces the missing :
--
--  Lookahead to resolve type 1 function parameter ambiguities
--      mark_type1() potentially marks the starting position
--      mark() marks the pre { position
--      remark() rewinds to the starting position
--      unmark() pops the starting position
--
--  Note that lookaheads may nest. 
-- 

--
--  The parsing philosophy is unusual. The major ambiguities are resolved by creating a unified superset
--  grammar rather than non-overlapping subgrammars. Thus the grammar for parameter-declaration covers an
--  assignment-expression. Minor ambiguities whose resolution by supersetting would create more
--  ambiguities are resolved the normal way with partitioned subgrammars.
--  This eliminates the traditional expression/declaration and constructor/parenthesised declarator
--  ambiguities at the syntactic level. A subsequent semantic level has to sort the problems out.
--  The generality introduces four bogus ambiguities and defers the cast ambiguity for resolution
--  once semantic information is available.
--
--  The C++ grammar comprises 558 rules and uses 894 states in yacc, with 0 unresolved conflicts.
--  24 conflicts from 10 ambiguities are resolved by 8 %prec's, so that yacc and bison report 0 conflicts.
--
--  The ambiguities are:
--  1) dangling else resolved to inner-most if
--      1 conflict in 1 state on else
--  2) < as start-template or less-than
--      1 conflict in 2 states on <
--  3) a :: b :: c resolved to favour a::b::c rather than a::b ::c or a ::b::c
--      1 conflicts in 1 state for ::
--  4) pointer operators maximised at end of conversion id/new in preference to binary operators
--      2 conflicts in 4 states on * and &
--  5a) (a)@b resolved to favour binary a@b rather than cast unary (a)(@b)
--  5b) (a)(b) resolved to favour cast rather than call
--      8 conflicts in 1 state for the 8 prefix operators: 6 unaries and ( and [.
--  6) enum name { resolved to enum-specifier rather than function
--      1 conflict in 1 state on {
--  7) class name { resolved to class-specifier rather than function
--      1 conflict in 1 state on {
--  8) extern "C" resolved to linkage-specification rather than declaration
--      1 conflict in 1 state on StringLiteral
--  9) class X : forced to go through base-clause look-ahead
--      1 conflict in 1 state on :
--  10) id : forced to label_statement rather than constructor_head
--      0 conflicts - but causes a double state for 2)
--  of which
--      1 is a fundamental C conflict - always correctly resolved
--          can be removed - see the Java spec
--      2, 3, 4 are fundamental C++ conflicts
--          2 always consistently resolved by iteration
--          3 always correctly resolved
--          4 always correctly resolved
--      5 is a result of not using type information - deferred for semantic repair
--      6,7 are caused by parsing over-generous superset - always correctly resolved
--      8 is caused by parsing over-generous superset - always correctly resolved
--          can be removed at the expense of 7 rules and 5 states.
--      9 is a look-ahead trick - always correctly resolved
--          could be removed by marking one token sooner
--      10 is caused by parsing over-generous superset - always correctly resolved
--
--  The hard problem of distinguishing
--      class A { class B : C, D, E {           -- A::B privately inherits C, D and E
--      class A { class B : C, D, E ;           -- C is width of anon bit-field
--  is resolved by using a lookahead that assumes inheritance and rewinds for the bit-field.
--
--  The potential shift-reduce conflict on > is resolved by flattening part of the expression grammar
--  to know when the next > is template end or arithmetic >.
--
--  The grammar is SYNTACTICALLY context-free with respect to type. No semantic assistance is required
--  during syntactic analysis. However the cast ambiguity is deferred and must be recovered
--  after syntactic analysis of a statement has completed. 
--
--  The grammar is SYNTACTICALLY context-free with respect to template-names. This is achieved by
--  organising a binary search over all possible template/arithmetic ambiguities with respect to
--  the enclosing statement. This is potentially exponentially inefficient but well-behaved in practice.
--  Approximately 1% of statements trigger a search and approximately 1% of those are misparsed,
--  requiring the semantic analysis to check and correct once template information is available.
--  1.5 parse attempts are required on average per ambiguous statement.
--
--  The grammar supports type I function declarations at severe impediment to efficiency. A lookahead
--  has to be performed after almost every non-statement close parenthesis. A one-line plus corollary
--  change to postfix_expression is commented and strongly recommended to make this grammar as
--  efficient as the rather large number of reduction levels permits.
--
--  Error recovery occurs mostly at the statement/declaration level. Recovery also occurs at
--  the list-element level where this poses no hazard to statement/declaration level recovery. 
--  Note that since error propagation interacts with the lookaheads for template iteration or
--  type 1 function arguments, introduction of finer grained error recovery may repair a false
--  parse and so cause a misparse.
--
--  The following syntactic analysis errors occur, but are correctable semantically:
--  (cast)unary-op expr         is parsed as (parenthesised)binary-op expr
--      The semantic test should look for a binary/call with a (type) as its left child.
--  (parenthesised)(arguments)  is parsed as (cast)(parenthesised)
--      The semantic test should look for a cast with a non-type as its left child.
--  template < and arithmetic < may be cross-parsed (unless semnatic help is provided)
--      approximately 0.01% are misparsed, and must be sorted out - not easy.
--
--  The syntactic analysis defers the following ambiguities for semantic resolution:
--  declaration/expression is parsed as a unified concept
--      Use type and context to complete the parse.
--  ~class-name                 is parsed as unary~ name
--      The semantic test should look for ~ with a type as its child.
--  delete[] expr               is parsed as delete []expr
--      The semantic test should look for delete with a [] cast of its child.
--  operator new/delete[]       are parsed as array of operator new/delete
--      The semantic test should look for array of operator new/delete
--      or activate the two extra commented rules in operator
--  template of an explicit_instantiation is buried deep in the tree
--      dig it out 
--  pure-specifier and constant-initializer are covered by assignment-expression
--      just another of the deferred declaration/expression ambiguities
--  sizeof and typeid don't distinguish type/value syntaxes
--      probably makes life polymorphically easier
-- 
%nonassoc SHIFT_THERE
%nonassoc SCOPE ELSE INC DEC '+' '-' '*' '&' '[' '{' '<' ':' StringLiteral
%nonassoc REDUCE_HERE_MOSTLY
%nonassoc '('
--%nonassoc REDUCE_HERE 

%start translation_unit
%%

--
--  The %prec resolves the 14.2-3 ambiguity:
--  Identifier '<' is forced to go through the is-it-a-template-name test
--  All names absorb TEMPLATE with the name, so that no template_test is performed for them.
--  This requires all potential declarations within an expression to perpetuate this policy
--  and thereby guarantee the ultimate coverage of explicit_instantiation.
--
--  The %prec also resolves a conflict in identifier : which is forced to be a shift of a label for
--  a labeled-statement rather than a reduction for the name of a bit-field or generalised constructor.
--  This is pretty dubious syntactically but correct for all semantic possibilities.
--  The shift is only activated when the ambiguity exists at the start of a statement. In this context
--  a bit-field declaration or constructor definition are not allowed.
-- 
identifier:                         Identifier_tk
;
id:                                 identifier                          -- _prec SHIFT_THERE       -- Force < through test 
    |                               identifier template_test '+' template_argument_list '>'
    |                               identifier template_test '-'                                -- requeued < follows 
    |                               template_id 
;
template_test:                      '<'             -- Queue '+' or '-' < as follow on 
        { template_test(); }
;
global_scope:                       SCOPE
    |                               TEMPLATE global_scope
;
id_scope:                           id SCOPE
--
--  A :: B :: C; is ambiguous How much is type and how much name ?
--  The %prec maximises the (type) length which is the 7.1-2 semantic constraint.
-- 
;
nested_id:                          id                                  -- _prec SHIFT_THERE       -- Maximise length 
    |                               id_scope nested_id
;
scoped_id:                          nested_id
    |                               global_scope nested_id

--
--  destructor_id has to be held back to avoid a conflict with a one's complement as per 5.3.1-9,
--  It gets put back only when scoped or in a declarator_id, which is only used as an explicit member name.
--  Declarations of an unscoped destructor are always parsed as a one's complement.
-- 
;
destructor_id:                      '~' id
    |                               TEMPLATE destructor_id
;
special_function_id:                conversion_function_id
    |                               operator_function_id
    |                               TEMPLATE special_function_id
;
nested_special_function_id:         special_function_id
    |                               id_scope destructor_id
    |                               id_scope nested_special_function_id
;
scoped_special_function_id:         nested_special_function_id
    |                               global_scope nested_special_function_id

-- declarator-id is all names in all scopes, except reserved words 
;
declarator_id:                      scoped_id
    |                               scoped_special_function_id
    |                               destructor_id

--  The standard defines pseudo-destructors in terms of type-name, which is class/enum/typedef, of which
--  class-name is covered by a normal destructor. pseudo-destructors are supposed to support ~int() in
--  templates, so the grammar here covers built-in names. Other names are covered by the lack of
--  identifier/type discrimination.
-- 
;
built_in_type_id:                   built_in_type_specifier
    |                               built_in_type_id built_in_type_specifier
;
pseudo_destructor_id:               built_in_type_id SCOPE '~' built_in_type_id
    |                               '~' built_in_type_id
    |                               TEMPLATE pseudo_destructor_id
;
nested_pseudo_destructor_id:        pseudo_destructor_id
    |                               id_scope nested_pseudo_destructor_id
;
scoped_pseudo_destructor_id:        nested_pseudo_destructor_id
    |                               global_scope scoped_pseudo_destructor_id

-----------------------------------------------------------------------------------------------------
-- A.2 Lexical conventions
-----------------------------------------------------------------------------------------------------
--
--  String concatenation is a phase 6, not phase 7 activity so does not really belong in the grammar.
--  However it may be convenient to have it here to make this grammar fully functional.
--  Unfortunately it introduces a conflict with the generalised parsing of extern "C" which
--  is correctly resolved to maximise the string length as the token source should do anyway.
-- 
;
string:                             StringLiteral
--string:                           StringLiteral                           -- _prec SHIFT_THERE 
--  |                               StringLiteral string  -- Perverse order avoids conflicts -- 
;
literal:                            IntegerLiteral
    |                               CharacterLiteral
    |                               FloatingLiteral
    |                               string
    |                               boolean_literal
;
boolean_literal:                    FALSE
    |                               TRUE

-----------------------------------------------------------------------------------------------------
-- A.3 Basic concepts
-----------------------------------------------------------------------------------------------------
;
translation_unit:                   declaration_seq.opt

-----------------------------------------------------------------------------------------------------
-- A.4 Expressions
-----------------------------------------------------------------------------------------------------
--  primary_expression covers an arbitrary sequence of all names with the exception of an unscoped destructor,
--  which is parsed as its unary expression which is the correct disambiguation (when ambiguous).
--  This eliminates the traditional A(B) meaning A B ambiguity, since we never have to tack an A onto
--  the front of something that might start with (. The name length got maximised ab initio. The downside
--  is that semantic interpretation must split the names up again.
--
--  Unification of the declaration and expression syntax means that unary and binary pointer declarator operators:
--      int * * name
--  are parsed as binary and unary arithmetic operators (int) * (*name). Since type information is not used
--  ambiguities resulting from a cast
--      (cast)*(value)
--  are resolved to favour the binary rather than the cast unary to ease AST clean-up.
--  The cast-call ambiguity must be resolved to the cast to ensure that (a)(b)c can be parsed.
--
--  The problem of the functional cast ambiguity
--      name(arg)
--  as call or declaration is avoided by maximising the name within the parsing kernel. So
--  primary_id_expression picks up 
--      extern long int const var = 5;
--  as an assignment to the syntax parsed as "extern long int const var". The presence of two names is
--  parsed so that "extern long into const" is distinguished from "var" considerably simplifying subsequent
--  semantic resolution.
--
--  The generalised name is a concatenation of potential type-names (scoped identifiers or built-in sequences)
--  plus optionally one of the special names such as an operator-function-id, conversion-function-id or
--  destructor as the final name. 
-- 
;
primary_expression:                 literal
    |                               THIS
    |                               suffix_decl_specified_ids
--  |                               SCOPE identifier                                            -- covered by suffix_decl_specified_ids 
--  |                               SCOPE operator_function_id                                  -- covered by suffix_decl_specified_ids 
--  |                               SCOPE qualified_id                                          -- covered by suffix_decl_specified_ids 
    |                               abstract_expression               -- _prec REDUCE_HERE_MOSTLY  -- Prefer binary to unary ops, cast to call 
--  |                               id_expression                                               -- covered by suffix_decl_specified_ids 
--
--  Abstract-expression covers the () and [] of abstract-declarators.
-- 
;
abstract_expression:                parenthesis_clause
    |                               '[' expression.opt ']'
    |                               TEMPLATE abstract_expression

--  Type I function parameters are ambiguous with respect to the generalised name, so we have to do a lookahead following
--  any function-like parentheses. This unfortunately hits normal code, so kill the -- lines and add the ++ lines for efficiency.
--  Supporting Type I code under the superset causes perhaps 25% of lookahead parsing. Sometimes complete class definitions
--  get traversed since they are valid generalised type I parameters!
-- 
;
type1_parameters:       ------
    parameter_declaration_list ';'
    |                   ------
    type1_parameters parameter_declaration_list ';'
;
mark_type1:                         -- empty 
                                             { mark_type1(); yyclearin; }
;
postfix_expression:                 primary_expression
--  |                   /++++++/    postfix_expression parenthesis_clause 
    |                   ------
    postfix_expression parenthesis_clause mark_type1 '-'
    |                   ------
    postfix_expression parenthesis_clause mark_type1 '+' type1_parameters mark '{' error 
                        ------
                    { yyerrok; yyclearin; remark_type1(); unmark(); unmark(); }
    |                   ------
    postfix_expression parenthesis_clause mark_type1 '+' type1_parameters mark error 
                        ------
                    { yyerrok; yyclearin; remark_type1(); unmark(); unmark(); }
    |                   ------
    postfix_expression parenthesis_clause mark_type1 '+' error
                        ------
                    { yyerrok; yyclearin; remark_type1(); unmark(); }
    |                               postfix_expression '[' expression.opt ']'
--  |                               destructor_id '[' expression.opt ']'                    -- not semantically valid 
--  |                               destructor_id parenthesis_clause                        -- omitted to resolve known ambiguity 
--  |                               simple_type_specifier '(' expression_list.opt ')'       -- simple_type_specifier is a primary_expression 
    |                               postfix_expression '.' declarator_id
--  |                               postfix_expression '.' TEMPLATE declarator_id           -- TEMPLATE absorbed into declarator_id. 
    |                               postfix_expression '.' scoped_pseudo_destructor_id
    |                               postfix_expression ARROW declarator_id
--  |                               postfix_expression ARROW TEMPLATE declarator_id         -- TEMPLATE absorbed into declarator_id. 
    |                               postfix_expression ARROW scoped_pseudo_destructor_id   
    |                               postfix_expression INC
    |                               postfix_expression DEC
    |                               DYNAMIC_CAST '<' type_id '>' '(' expression ')'
    |                               STATIC_CAST '<' type_id '>' '(' expression ')'
    |                               REINTERPRET_CAST '<' type_id '>' '(' expression ')'
    |                               CONST_CAST '<' type_id '>' '(' expression ')'
    |                               TYPEID parameters_clause
--  |                               TYPEID '(' expression ')'                               -- covered by parameters_clause 
--  |                               TYPEID '(' type_id ')'                                  -- covered by parameters_clause 
;
expression_list.opt:                -- empty 
    |                               expression_list
;
expression_list:                    assignment_expression
    |                               expression_list ',' assignment_expression

;
unary_expression:                   postfix_expression
    |                               INC cast_expression
    |                               DEC cast_expression
    |                               ptr_operator cast_expression
--  |                               '*' cast_expression                                     -- covered by ptr_operator 
--  |                               '&' cast_expression                                     -- covered by ptr_operator 
--  |                               decl_specifier_seq '*' cast_expression                  -- covered by binary operator 
--  |                               decl_specifier_seq '&' cast_expression                  -- covered by binary operator 
    |                               suffix_decl_specified_scope star_ptr_operator cast_expression   -- covers e.g int ::type::* const t = 4 

    |                               '+' cast_expression
    |                               '-' cast_expression
    |                               '!' cast_expression
    |                               '~' cast_expression
    |                               SIZEOF unary_expression
--  |                               SIZEOF '(' type_id ')'                                  -- covered by unary_expression 
    |                               new_expression
    |                               global_scope new_expression
    |                               delete_expression
    |                               global_scope delete_expression
--  |                               DELETE '[' ']' cast_expression       -- covered by DELETE cast_expression since cast_expression covers ... 
--  |                               SCOPE DELETE '[' ']' cast_expression //  ... abstract_expression cast_expression and so [] cast_expression 

;
delete_expression:                  DELETE cast_expression                                  -- also covers DELETE[] cast_expression 

;
new_expression:                     NEW new_type_id new_initializer.opt
    |                               NEW parameters_clause new_type_id new_initializer.opt
    |                               NEW parameters_clause
--  |                               NEW '(' type-id ')'                                     -- covered by parameters_clause 
    |                               NEW parameters_clause parameters_clause new_initializer.opt
--  |                               NEW '(' type-id ')' new_initializer                     -- covered by parameters_clause parameters_clause 
--  |                               NEW parameters_clause '(' type-id ')'                   -- covered by parameters_clause parameters_clause 
                                                                                -- ptr_operator_seq.opt production reused to save a %prec 
;
new_type_id:                        type_specifier ptr_operator_seq.opt
    |                               type_specifier new_declarator
    |                               type_specifier new_type_id
;
new_declarator:                     ptr_operator new_declarator
    |                               direct_new_declarator
;
direct_new_declarator:              '[' expression ']'
    |                               direct_new_declarator '[' constant_expression ']'
;
new_initializer.opt:                -- empty 
    |                               '(' expression_list.opt ')'

--  cast-expression is generalised to support a [] as well as a () prefix. This covers the omission of DELETE[] which when
--  followed by a parenthesised expression was ambiguous. It also covers the gcc indexed array initialisation for free.
-- 
;
cast_expression:                    unary_expression
    |                               abstract_expression cast_expression
--  |                               '(' type_id ')' cast_expression                             -- covered by abstract_expression 

;
pm_expression:                      cast_expression
    |                               pm_expression DOT_STAR cast_expression
    |                               pm_expression ARROW_STAR cast_expression
;
multiplicative_expression:          pm_expression
    |                               multiplicative_expression star_ptr_operator pm_expression
    |                               multiplicative_expression '/' pm_expression
    |                               multiplicative_expression '%' pm_expression
;
additive_expression:                multiplicative_expression
    |                               additive_expression '+' multiplicative_expression
    |                               additive_expression '-' multiplicative_expression
;
shift_expression:                   additive_expression
    |                               shift_expression SHL additive_expression
    |                               shift_expression SHR additive_expression
;
relational_expression:              shift_expression
    |                               relational_expression '<' shift_expression
    |                               relational_expression '>' shift_expression
    |                               relational_expression LE shift_expression
    |                               relational_expression GE shift_expression
;
equality_expression:                relational_expression
    |                               equality_expression EQ relational_expression
    |                               equality_expression NE relational_expression
;
and_expression:                     equality_expression
    |                               and_expression '&' equality_expression
;
exclusive_or_expression:            and_expression
    |                               exclusive_or_expression '^' and_expression
;
inclusive_or_expression:            exclusive_or_expression
    |                               inclusive_or_expression '|' exclusive_or_expression
;
logical_and_expression:             inclusive_or_expression
    |                               logical_and_expression LOG_AND inclusive_or_expression
;
logical_or_expression:              logical_and_expression
    |                               logical_or_expression LOG_OR logical_and_expression
;
conditional_expression:             logical_or_expression
    |                               logical_or_expression '?' expression ':' assignment_expression


--  assignment-expression is generalised to cover the simple assignment of a braced initializer in order to contribute to the
--  coverage of parameter-declaration and init-declaration.
-- 
;
assignment_expression:              conditional_expression
    |                               logical_or_expression assignment_operator assignment_expression
    |                               logical_or_expression '=' braced_initializer
    |                               throw_expression
;
assignment_operator:                '=' | ASS_ADD | ASS_AND | ASS_DIV | ASS_MOD | ASS_MUL | ASS_OR | ASS_SHL | ASS_SHR | ASS_SUB | ASS_XOR

--  expression is widely used and usually single-element, so the reductions are arranged so that a
--  single-element expression is returned as is. Multi-element expressions are parsed as a list that
--  may then behave polymorphically as an element or be compacted to an element. 
;
expression.opt:                     -- empty 
    |                               expression
;
expression:                         assignment_expression
    |                               expression_list ',' assignment_expression
;
constant_expression:                conditional_expression

--  The grammar is repeated for when the parser stack knows that the next > must end a template.
-- 
;
templated_relational_expression:    shift_expression
    |                               templated_relational_expression '<' shift_expression
    |                               templated_relational_expression LE shift_expression
    |                               templated_relational_expression GE shift_expression
;
templated_equality_expression:      templated_relational_expression
    |                               templated_equality_expression EQ templated_relational_expression
    |                               templated_equality_expression NE templated_relational_expression
;
templated_and_expression:           templated_equality_expression
    |                               templated_and_expression '&' templated_equality_expression
;
templated_exclusive_or_expression:  templated_and_expression
    |                               templated_exclusive_or_expression '^' templated_and_expression

;
templated_inclusive_or_expression:  templated_exclusive_or_expression
    |                               templated_inclusive_or_expression '|' templated_exclusive_or_expression

;
templated_logical_and_expression:   templated_inclusive_or_expression
    |                               templated_logical_and_expression LOG_AND templated_inclusive_or_expression

;
templated_logical_or_expression:    templated_logical_and_expression
    |                               templated_logical_or_expression LOG_OR templated_logical_and_expression

;
templated_conditional_expression:   templated_logical_or_expression
    |                               templated_logical_or_expression '?' templated_expression ':' templated_assignment_expression

;
templated_assignment_expression:    templated_conditional_expression
    |                               templated_logical_or_expression assignment_operator templated_assignment_expression

    |                               templated_throw_expression
;
templated_expression:               templated_assignment_expression
    |                               templated_expression_list ',' templated_assignment_expression

;
templated_expression_list:          templated_assignment_expression
    |                               templated_expression_list ',' templated_assignment_expression

-----------------------------------------------------------------------------------------------------
-- A.5 Statements
-----------------------------------------------------------------------------------------------------
--  Parsing statements is easy once simple_declaration has been generalised to cover expression_statement.
-- 
;
looping_statement:                  start_search looped_statement                               { end_search(); }
;
looped_statement:                   statement
    |                               advance_search '+' looped_statement
    |                               advance_search '-'
;
statement:                          control_statement
--  |                               expression_statement                                        -- covered by declaration_statement 
    |                               compound_statement
    |                               declaration_statement
    |                               try_block
;
control_statement:                  labeled_statement
    |                               selection_statement
    |                               iteration_statement
    |                               jump_statement
;
labeled_statement:                  identifier ':' looping_statement
    |                               CASE constant_expression ':' looping_statement
    |                               DEFAULT ':' looping_statement
--expression_statement:             expression.opt ';'                                          -- covered by declaration_statement 
;
compound_statement:                 '{' statement_seq.opt '}'
    |                               '{' statement_seq.opt looping_statement '#' bang error '}'  { UNBANG("Bad statement-seq."); }
;
statement_seq.opt:                  -- empty 
    |                               statement_seq.opt looping_statement
    |                               statement_seq.opt looping_statement '#' bang error ';'      { UNBANG("Bad statement."); }
--
--  The dangling else conflict is resolved to the innermost if.
-- 
;
selection_statement:                IF '(' condition ')' looping_statement    -- _prec SHIFT_THERE
    |                               IF '(' condition ')' looping_statement ELSE looping_statement
    |                               SWITCH '(' condition ')' looping_statement
;
condition.opt:                      -- empty 
    |                               condition
;
condition:                          parameter_declaration_list
--  |                               expression                                                  -- covered by parameter_declaration_list 
--  |                               type_specifier_seq declarator '=' assignment_expression     -- covered by parameter_declaration_list 
;
iteration_statement:                WHILE '(' condition ')' looping_statement
    |                               DO looping_statement WHILE '(' expression ')' ';'
    |                               FOR '(' for_init_statement condition.opt ';' expression.opt ')' looping_statement

;
for_init_statement:                 simple_declaration
--  |                               expression_statement                                        -- covered by simple_declaration 
;
jump_statement:                     BREAK ';'
    |                               CONTINUE ';'
    |                               RETURN expression.opt ';'
    |                               GOTO identifier ';'
;
declaration_statement:              block_declaration

-----------------------------------------------------------------------------------------------------
-- A.6 Declarations
-----------------------------------------------------------------------------------------------------
;
compound_declaration:               '{' nest declaration_seq.opt '}'                            { unnest(); }
    |                               '{' nest declaration_seq.opt util looping_declaration '#' bang error '}'
                                                                                                { unnest(); UNBANG("Bad declaration-seq."); }
;
declaration_seq.opt:                -- empty 
    |                               declaration_seq.opt util looping_declaration
    |                               declaration_seq.opt util looping_declaration '#' bang error ';' { UNBANG("Bad declaration."); }
;
looping_declaration:                start_search1 looped_declaration                            { end_search(); }
;
looped_declaration:                 declaration
    |                               advance_search '+' looped_declaration
    |                               advance_search '-'
;
declaration:                        block_declaration
    |                               function_definition
    |                               template_declaration
--  |                               explicit_instantiation                                      -- covered by relevant declarations 
    |                               explicit_specialization
    |                               specialised_declaration
;
specialised_declaration:            linkage_specification
    |                               namespace_definition
    |                               TEMPLATE specialised_declaration
;
block_declaration:                  simple_declaration
    |                               specialised_block_declaration
;
specialised_block_declaration:      asm_definition
    |                               namespace_alias_definition
    |                               using_declaration
    |                               using_directive
    |                               TEMPLATE specialised_block_declaration
;
simple_declaration:                 ';'
    |                               init_declaration ';'
    |                               init_declarations ';'
    |                               decl_specifier_prefix simple_declaration

--  A decl-specifier following a ptr_operator provokes a shift-reduce conflict for
--      * const name
--  which is resolved in favour of the pointer, and implemented by providing versions
--  of decl-specifier guaranteed not to start with a cv_qualifier.
--
--  decl-specifiers are implemented type-centrically. That is the semantic constraint
--  that there must be a type is exploited to impose structure, but actually eliminate
--  very little syntax. built-in types are multi-name and so need a different policy.
--
--  non-type decl-specifiers are bound to the left-most type in a decl-specifier-seq,
--  by parsing from the right and attaching suffixes to the right-hand type. Finally
--  residual prefixes attach to the left.                
-- 
;
suffix_built_in_decl_specifier.raw: built_in_type_specifier
    |                               suffix_built_in_decl_specifier.raw built_in_type_specifier
    |                               suffix_built_in_decl_specifier.raw decl_specifier_suffix
;
suffix_built_in_decl_specifier:     suffix_built_in_decl_specifier.raw
    |                               TEMPLATE suffix_built_in_decl_specifier
;
suffix_named_decl_specifier:        scoped_id
    |                               elaborate_type_specifier
    |                               suffix_named_decl_specifier decl_specifier_suffix
;
suffix_named_decl_specifier.bi:     suffix_named_decl_specifier
    |                               suffix_named_decl_specifier suffix_built_in_decl_specifier.raw
;
suffix_named_decl_specifiers:       suffix_named_decl_specifier.bi
    |                               suffix_named_decl_specifiers suffix_named_decl_specifier.bi
;
suffix_named_decl_specifiers.sf:    scoped_special_function_id          -- operators etc 
    |                               suffix_named_decl_specifiers
    |                               suffix_named_decl_specifiers scoped_special_function_id
;
suffix_decl_specified_ids:          suffix_built_in_decl_specifier
    |                               suffix_built_in_decl_specifier suffix_named_decl_specifiers.sf
    |                               suffix_named_decl_specifiers.sf
;
suffix_decl_specified_scope:        suffix_named_decl_specifiers SCOPE
    |                               suffix_built_in_decl_specifier suffix_named_decl_specifiers SCOPE
    |                               suffix_built_in_decl_specifier SCOPE

;
decl_specifier_affix:               storage_class_specifier
    |                               function_specifier
    |                               FRIEND                                                          
    |                               TYPEDEF
    |                               cv_qualifier

;
decl_specifier_suffix:              decl_specifier_affix

;
decl_specifier_prefix:              decl_specifier_affix
    |                               TEMPLATE decl_specifier_prefix

;
storage_class_specifier:            REGISTER | STATIC | MUTABLE
    |                               EXTERN                  -- _prec SHIFT_THERE                   -- Prefer linkage specification 
    |                               AUTO

;
function_specifier:                 EXPLICIT
    |                               INLINE
    |                               VIRTUAL

;
type_specifier:                     simple_type_specifier
    |                               elaborate_type_specifier
    |                               cv_qualifier

;
elaborate_type_specifier:           class_specifier
    |                               enum_specifier
    |                               elaborated_type_specifier
    |                               TEMPLATE elaborate_type_specifier
;
simple_type_specifier:              scoped_id
    |                               built_in_type_specifier
;
built_in_type_specifier:            CHAR | WCHAR_T | BOOL | SHORT | INT | LONG | SIGNED | UNSIGNED | FLOAT | DOUBLE | VOID

--
--  The over-general use of declaration_expression to cover decl-specifier-seq.opt declarator in a function-definition means that
--      class X { };
--  could be a function-definition or a class-specifier.
--      enum X { };
--  could be a function-definition or an enum-specifier.
--  The function-definition is not syntactically valid so resolving the false conflict in favour of the
--  elaborated_type_specifier is correct.
-- 
;
elaborated_type_specifier:          elaborated_class_specifier
    |                               elaborated_enum_specifier
    |                               TYPENAME scoped_id

;
elaborated_enum_specifier:          ENUM scoped_id               -- _prec SHIFT_THERE
;
enum_specifier:                     ENUM scoped_id enumerator_clause
    |                               ENUM enumerator_clause
;
enumerator_clause:                  '{' enumerator_list_ecarb
    |                               '{' enumerator_list enumerator_list_ecarb
    |                               '{' enumerator_list ',' enumerator_definition_ecarb
;
enumerator_list_ecarb:              '}'
    |                               bang error '}'                                              { UNBANG("Bad enumerator-list."); }
;
enumerator_definition_ecarb:        '}'
    |                               bang error '}'                                              { UNBANG("Bad enumerator-definition."); }
;
enumerator_definition_filler:       -- empty 
    |                               bang error ','                                              { UNBANG("Bad enumerator-definition."); }
;
enumerator_list_head:               enumerator_definition_filler
    |                               enumerator_list ',' enumerator_definition_filler
;
enumerator_list:                    enumerator_list_head enumerator_definition
;
enumerator_definition:              enumerator
    |                               enumerator '=' constant_expression
;
enumerator:                         identifier

;
namespace_definition:               NAMESPACE scoped_id compound_declaration
    |                               NAMESPACE compound_declaration
;
namespace_alias_definition:         NAMESPACE scoped_id '=' scoped_id ';'

;
using_declaration:                  USING declarator_id ';'
    |                               USING TYPENAME declarator_id ';'

;
using_directive:                    USING NAMESPACE scoped_id ';'
;
asm_definition:                     ASM '(' string ')' ';'
;
linkage_specification:              EXTERN string looping_declaration
    |                               EXTERN string compound_declaration

-----------------------------------------------------------------------------------------------------
-- A.7 Declarators
-----------------------------------------------------------------------------------------------------
--init-declarator is named init_declaration to reflect the embedded decl-specifier-seq.opt
;
init_declarations:                  assignment_expression ',' init_declaration
    |                               init_declarations ',' init_declaration
;
init_declaration:                   assignment_expression
--  |                               assignment_expression '=' initializer_clause                -- covered by assignment_expression 
--  |                               assignment_expression '(' expression_list ')'               -- covered by another set of call arguments 

--declarator:                                                                                   -- covered by assignment_expression 
--direct_declarator:                                                                            -- covered by postfix_expression 

;
star_ptr_operator:                  '*'
    |                               star_ptr_operator cv_qualifier
;
nested_ptr_operator:                star_ptr_operator
    |                               id_scope nested_ptr_operator
;
ptr_operator:                       '&'
    |                               nested_ptr_operator
    |                               global_scope nested_ptr_operator
;
ptr_operator_seq:                   ptr_operator
    |                               ptr_operator ptr_operator_seq
-- Independently coded to localise the shift-reduce conflict: sharing just needs another _prec 
;
ptr_operator_seq.opt:               -- empty 
                         -- _prec SHIFT_THERE       -- Maximise type length 
    |                               ptr_operator ptr_operator_seq.opt

;
cv_qualifier_seq.opt:               -- empty 
    |                               cv_qualifier_seq.opt cv_qualifier
;
cv_qualifier:                       CONST | VOLATILE

--type_id                                                                                       -- also covered by parameter declaration 
;
type_id:                            type_specifier abstract_declarator.opt
    |                               type_specifier type_id

--abstract_declarator:                                                                          -- also covered by parameter declaration 
;
abstract_declarator.opt:            -- empty 
    |                               ptr_operator abstract_declarator.opt
    |                               direct_abstract_declarator
;
direct_abstract_declarator.opt:     -- empty 
    |                               direct_abstract_declarator
;
direct_abstract_declarator:         direct_abstract_declarator.opt parenthesis_clause
    |                               direct_abstract_declarator.opt '[' ']'
    |                               direct_abstract_declarator.opt '[' constant_expression ']'
--  |                               '(' abstract_declarator ')'                                 -- covered by parenthesis_clause 

;
parenthesis_clause:                 parameters_clause cv_qualifier_seq.opt
    |                               parameters_clause cv_qualifier_seq.opt exception_specification
;
parameters_clause:                  '(' parameter_declaration_clause ')'
-- parameter_declaration_clause also covers init_declaration, type_id, declarator and abstract_declarator. 
;
parameter_declaration_clause:       -- empty 
    |                               parameter_declaration_list
    |                               parameter_declaration_list ELLIPSIS
;
parameter_declaration_list:         parameter_declaration
    |                               parameter_declaration_list ',' parameter_declaration

--
-- A typed abstract qualifier such as
--      Class * ...
-- looks like a multiply, so pointers are parsed as their binary operation equivalents that
-- ultimately terminate with a degenerate right hand term.
-- 
;
abstract_pointer_declaration:       ptr_operator_seq
    |                               multiplicative_expression star_ptr_operator ptr_operator_seq.opt
;
abstract_parameter_declaration:     abstract_pointer_declaration
    |                               and_expression '&'
    |                               and_expression '&' abstract_pointer_declaration
;
special_parameter_declaration:      abstract_parameter_declaration
    |                               abstract_parameter_declaration '=' assignment_expression
    |                               ELLIPSIS
;
parameter_declaration:              assignment_expression
    |                               special_parameter_declaration
    |                               decl_specifier_prefix parameter_declaration

--  The grammar is repeated for use within template <>
-- 
;
templated_parameter_declaration:    templated_assignment_expression
    |                               templated_abstract_declaration
    |                               templated_abstract_declaration '=' templated_assignment_expression

    |                               decl_specifier_prefix templated_parameter_declaration
;
templated_abstract_declaration:     abstract_pointer_declaration
    |                               templated_and_expression '&'
    |                               templated_and_expression '&' abstract_pointer_declaration

--  function_definition includes constructor, destructor, implicit int definitions too.
--  A local destructor is successfully parsed as a function-declaration but the ~ was treated as a unary operator.
--  constructor_head is the prefix ambiguity between a constructor and a member-init-list starting with a bit-field.
-- 
;
function_definition:                ctor_definition
    |                               func_definition
;
func_definition:                    assignment_expression function_try_block
    |                               assignment_expression function_body
    |                               decl_specifier_prefix func_definition
;
ctor_definition:                    constructor_head function_try_block
    |                               constructor_head function_body
    |                               decl_specifier_prefix ctor_definition
;
constructor_head:                   bit_field_init_declaration
    |                               constructor_head ',' assignment_expression
;
function_try_block:                 TRY function_block handler_seq
;
function_block:                     ctor_initializer.opt function_body
;
function_body:                      compound_statement

--
--  An = initializer looks like an extended assignment_expression.
--  An () initializer looks like a function call.
--  initializer is therefore flattened into its generalised customers.
--initializer:                      '=' initializer_clause                                      -- flattened into caller
--  |                               '(' expression_list ')'                                     -- flattened into caller 
;
initializer_clause:                 assignment_expression
    |                               braced_initializer
;
braced_initializer:                 '{' initializer_list '}'
    |                               '{' initializer_list ',' '}'
    |                               '{' '}'
    |                               '{' looping_initializer_clause '#' bang error '}'           { UNBANG("Bad initializer_clause."); }
    |                               '{' initializer_list ',' looping_initializer_clause '#' bang error '}'
                                                                                                { UNBANG("Bad initializer_clause."); }
;
initializer_list:                   looping_initializer_clause
    |                               initializer_list ',' looping_initializer_clause
;
looping_initializer_clause:         start_search looped_initializer_clause                      { end_search(); }
;
looped_initializer_clause:          initializer_clause
    |                               advance_search '+' looped_initializer_clause
    |                               advance_search '-'

-----------------------------------------------------------------------------------------------------
-- A.8 Classes
-----------------------------------------------------------------------------------------------------
--
--  An anonymous bit-field declaration may look very like inheritance:
--      const int B = 3;
--      class A : B ;
--  The two usages are too distant to try to create and enforce a common prefix so we have to resort to
--  a parser hack by backtracking. Inheritance is much the most likely so we mark the input stream context
--  and try to parse a base-clause. If we successfully reach a { the base-clause is ok and inheritance was
--  the correct choice so we unmark and continue. If we fail to find the { an error token causes back-tracking
--  to the alternative parse in elaborated_type_specifier which regenerates the : and declares unconditional success.
-- 
;
colon_mark:                         ':'                                                         { mark(); }
;
elaborated_class_specifier:         class_key scoped_id                    -- _prec SHIFT_THERE
    |                               class_key scoped_id colon_mark error                        { rewind_colon(); }
;
class_specifier_head:               class_key scoped_id colon_mark base_specifier_list '{'      { unmark(); }
    |                               class_key ':' base_specifier_list '{'
    |                               class_key scoped_id '{'
    |                               class_key '{'
;
class_key:                          CLASS | STRUCT | UNION
;
class_specifier:                    class_specifier_head member_specification.opt '}'
    |                               class_specifier_head member_specification.opt util looping_member_declaration '#' bang error '}'
                                            { UNBANG("Bad member_specification.opt."); }
;
member_specification.opt:           -- empty 
    |                               member_specification.opt util looping_member_declaration
    |                               member_specification.opt util looping_member_declaration '#' bang error ';'
                                                                                                { UNBANG("Bad member-declaration."); }
;
looping_member_declaration:         start_search looped_member_declaration                      { end_search(); }
;
looped_member_declaration:          member_declaration
    |                               advance_search '+' looped_member_declaration
    |                               advance_search '-'
;
member_declaration:                 accessibility_specifier
    |                               simple_member_declaration
    |                               function_definition
--  |                               function_definition ';'                                     -- trailing ; covered by null declaration 
--  |                               qualified_id ';'                                            -- covered by simple_member_declaration 
    |                               using_declaration
    |                               template_declaration

--  The generality of constructor names (there need be no parenthesised argument list) means that that
--          name : f(g), h(i)
--  could be the start of a constructor or the start of an anonymous bit-field. An ambiguity is avoided by
--  parsing the ctor-initializer of a function_definition as a bit-field.
-- 
;
simple_member_declaration:          ';'
    |                               assignment_expression ';'
    |                               constructor_head ';'
    |                               member_init_declarations ';'
    |                               decl_specifier_prefix simple_member_declaration
;
member_init_declarations:           assignment_expression ',' member_init_declaration
    |                               constructor_head ',' bit_field_init_declaration
    |                               member_init_declarations ',' member_init_declaration
;
member_init_declaration:            assignment_expression
--  |                               assignment_expression '=' initializer_clause                -- covered by assignment_expression 
--  |                               assignment_expression '(' expression_list ')'               -- covered by another set of call arguments 
    |                               bit_field_init_declaration
;
accessibility_specifier:            access_specifier ':'
;
bit_field_declaration:              assignment_expression ':' bit_field_width
    |                               ':' bit_field_width
;
bit_field_width:                    logical_or_expression
--  |                               logical_or_expression '?' expression ':' assignment_expression  -- has SR conflict w.r.t later = 
    |                               logical_or_expression '?' bit_field_width ':' bit_field_width
;
bit_field_init_declaration:         bit_field_declaration
    |                               bit_field_declaration '=' initializer_clause

-----------------------------------------------------------------------------------------------------
-- A.9 Derived classes
-----------------------------------------------------------------------------------------------------
--base_clause:                      ':' base_specifier_list                                     -- flattened 
;
base_specifier_list:                base_specifier
    |                               base_specifier_list ',' base_specifier
;
base_specifier:                     scoped_id
    |                               access_specifier base_specifier
    |                               VIRTUAL base_specifier
;
access_specifier:                   PRIVATE | PROTECTED | PUBLIC

-----------------------------------------------------------------------------------------------------
-- A.10 Special member functions
-----------------------------------------------------------------------------------------------------
;
conversion_function_id:             OPERATOR_tk conversion_type_id
;
conversion_type_id:                 type_specifier ptr_operator_seq.opt
    |                               type_specifier conversion_type_id
--
--  Ctor-initialisers can look like a bit field declaration, given the generalisation of names:
--      Class(Type) : m1(1), m2(2) { }
--      NonClass(bit_field) : int(2), second_variable, ...
--  The grammar below is used within a function_try_block or function_definition.
--  See simple_member_declaration for use in normal member function_definition.
-- 
;
ctor_initializer.opt:               -- empty 
    |                               ctor_initializer
;
ctor_initializer:                   ':' mem_initializer_list
    |                               ':' mem_initializer_list bang error                         { UNBANG("Bad ctor-initializer."); }
;
mem_initializer_list:               mem_initializer
    |                               mem_initializer_list_head mem_initializer
;
mem_initializer_list_head:          mem_initializer_list ','
    |                               mem_initializer_list bang error ','                         { UNBANG("Bad mem-initializer."); }
;
mem_initializer:                    mem_initializer_id '(' expression_list.opt ')'
;
mem_initializer_id:                 scoped_id

-----------------------------------------------------------------------------------------------------
-- A.11 Overloading
-----------------------------------------------------------------------------------------------------
;
operator_function_id:               OPERATOR_tk operator
--
--  It is not clear from the ANSI standard whether spaces are permitted in delete[]. If not then it can
--  be recognised and returned as DELETE_ARRAY by the lexer. Assuming spaces are permitted there is an
--  ambiguity created by the over generalised nature of expressions. operator new is a valid delarator-id
--  which we may have an undimensioned array of. Semantic rubbish, but syntactically valid. Since the
--  array form is covered by the declarator consideration we can exclude the operator here. The need
--  for a semantic rescue can be eliminated at the expense of a couple of shift-reduce conflicts by
--  removing the comments on the next four lines.
-- 
;
operator:             --++++
      NEW
    |                 --++++
      DELETE
--  |                 / ---- /      NEW                 -- _prec SHIFT_THERE
--  |                 / ---- /      DELETE              -- _prec SHIFT_THERE
--  |                 / ---- /      NEW '[' ']'                                                 -- Covered by array of OPERATOR NEW 
--  |                 / ---- /      DELETE '[' ']'                                              -- Covered by array of OPERATOR DELETE 
    |                               '+'
    |                               '-'
    |                               '*'
    |                               '/'
    |                               '%'
    |                               '^'
    |                               '&'
    |                               '|'
    |                               '~'
    |                               '!'
    |                               '='
    |                               '<'
    |                               '>'
    |                               ASS_ADD
    |                               ASS_SUB
    |                               ASS_MUL
    |                               ASS_DIV
    |                               ASS_MOD
    |                               ASS_XOR
    |                               ASS_AND
    |                               ASS_OR
    |                               SHL
    |                               SHR
    |                               ASS_SHR
    |                               ASS_SHL
    |                               EQ
    |                               NE
    |                               LE
    |                               GE
    |                               LOG_AND
    |                               LOG_OR
    |                               INC
    |                               DEC
    |                               ','
    |                               ARROW_STAR
    |                               ARROW
    |                               '(' ')'
    |                               '[' ']'

-----------------------------------------------------------------------------------------------------
-- A.12 Templates
-----------------------------------------------------------------------------------------------------
;
template_declaration:               template_parameter_clause declaration
    |                               EXPORT template_declaration
;
template_parameter_clause:          TEMPLATE '<' template_parameter_list '>'
;
template_parameter_list:            template_parameter
    |                               template_parameter_list ',' template_parameter
;
template_parameter:                 simple_type_parameter
    |                               simple_type_parameter '=' type_id
    |                               templated_type_parameter
    |                               templated_type_parameter '=' identifier
    |                               templated_parameter_declaration
    |                               bang error                                                  { UNBANG("Bad template-parameter."); }
;
simple_type_parameter:              CLASS
--  |                               CLASS identifier                                            -- covered by parameter_declaration 
    |                               TYPENAME
--  |                               TYPENAME identifier                                         -- covered by parameter_declaration 
;
templated_type_parameter:           template_parameter_clause CLASS
    |                               template_parameter_clause CLASS identifier
;
template_id:                        TEMPLATE identifier '<' template_argument_list '>'
    |                               TEMPLATE template_id
--
--  template-argument is evaluated using a templated...expression so that > resolves to end of template.
-- 
;
template_argument_list:             template_argument
    |                               template_argument_list ',' template_argument
;
template_argument:                  templated_parameter_declaration
--  |                               type_id                                                     -- covered by templated_parameter_declaration 
--  |                               template_name                                               -- covered by templated_parameter_declaration 
--  |                               error                                                       -- must allow template failure to re-search 

--
--  Generalised naming makes identifier a valid declaration, so TEMPLATE identifier is too.
--  The TEMPLATE prefix is therefore folded into all names, parenthesis_clause and decl_specifier_prefix.
-- 
--explicit_instantiation:           TEMPLATE declaration 
;
explicit_specialization:            TEMPLATE '<' '>' declaration

-----------------------------------------------------------------------------------------------------
-- A.13 Exception Handling
-----------------------------------------------------------------------------------------------------
;
try_block:                          TRY compound_statement handler_seq
--function_try_block:                                                                           -- moved near function_block 
;
handler_seq:                        handler
    |                               handler handler_seq
;
handler:                            CATCH '(' exception_declaration ')' compound_statement
;
exception_declaration:              parameter_declaration
--                                  ELLIPSIS                                                    -- covered by parameter_declaration 
;
throw_expression:                   THROW
    |                               THROW assignment_expression
;
templated_throw_expression:         THROW
    |                               THROW templated_assignment_expression
;
exception_specification:            THROW '(' ')'
    |                               THROW '(' type_id_list ')'
;
type_id_list:                       type_id
    |                               type_id_list ',' type_id

-----------------------------------------------------------------------------------------------------
-- Back-tracking and context support
-----------------------------------------------------------------------------------------------------
;
advance_search:                     error               { yyerrok; yyclearin; advance_search(); } -- Rewind and queue '+' or '-' '#' 
;
bang:                               -- empty 
         { BANG(); }   -- set flag to suppress "parse error" 
;
mark:                               -- empty 
         { mark(); }        -- Push lookahead and input token stream context onto a stack 
;
nest:                               -- empty 
         { nest(); }        -- Push a declaration nesting depth onto the parse stack 
;
start_search:                       -- empty 
         { start_search(false); }    -- Create/reset binary search context 
;
start_search1:                      -- empty 
         { start_search(true); }     -- Create/reset binary search context 
;
util:                               -- empty 
           -- Get current utility mode 
;
%%
