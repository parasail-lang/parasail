grammar ada202x;

        White_space  :  [ \t\r\n\u000C\u001A]+ -> skip
           ;

        Identifier : 
           Identifier_start (Identifier_start | Identifier_extend)*
          | Synchronization_kind
          ;

        fragment Identifier_start : 
             [A-Z]
           | [a-z]
//           | letter_titlecase
//           | letter_modifier
//           | letter_other
//           | number_letter
             ;

        fragment Identifier_extend : 
//             mark_non_spacing
//           | mark_spacing_combining
             Digit
           | '_'
           ;

        Numeric_literal : Decimal_literal | Based_literal
           ;

        fragment Decimal_literal : Numeral ('.' Numeral)? (Exponent)?
           ;

        fragment Numeral : Digit (('_')? Digit)*
           ;

        fragment Exponent : 'E' ('+')? Numeral | 'E' '-' Numeral
           ;

        fragment Digit : [0-9]
           ;

        fragment Based_literal : 
           Base '#' Based_numeral ('.' Based_numeral)? '#' (Exponent)?
           ;

        fragment Base : Numeral
             ;

        fragment Based_numeral : 
           Extended_digit (('_')? Extended_digit)*
             ;

        fragment Extended_digit : Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
             ;

        Character_literal : '\'' Graphic_character '\''
             ;

        fragment Graphic_character : Non_quotation_mark_graphic_character | '"' ;

        fragment Non_quotation_mark_graphic_character : [#-~] | ' ' | '!' |
          [\u0080-\uFFFD] ;

        String_literal : '"' (String_element)* '"'
             ;

        fragment String_element : '""' | Non_quotation_mark_graphic_character
             ;

        Comment : '--' (Non_end_of_line_character)* -> skip
             ;

        fragment Non_end_of_line_character : Graphic_character ;

        pragma : 
           'pragma' Identifier ('(' (pragma_argument_association
         ',')* pragma_argument_association ')')? ';'
             ;

        pragma_argument_association : 
             (pragma_argument_identifier '=>')? name
           | (pragma_argument_identifier '=>')? expression
           | pragma_argument_aspect_mark '=>'  name
           | pragma_argument_aspect_mark '=>'  expression
             ;

        pragma_argument_identifier : Identifier ;

        pragma_argument_aspect_mark : aspect_mark ;

        basic_declaration : 
             type_declaration           | subtype_declaration
           | object_declaration         | number_declaration
           | subprogram_declaration     | abstract_subprogram_declaration
           | null_procedure_declaration | expression_function_declaration
           | package_declaration        | renaming_declaration
           | exception_declaration      | generic_declaration
           | generic_instantiation
             ;

        defining_identifier : Identifier
             ;

        type_declaration :  full_type_declaration
           | incomplete_type_declaration
           | private_type_declaration
           | private_extension_declaration
             ;

        full_type_declaration : 
             'type' defining_identifier (known_discriminant_part
        )? 'is' type_definition
                (aspect_specification)? ';'
           | task_type_declaration
           | protected_type_declaration
             ;

        type_definition : 
             enumeration_type_definition   | integer_type_definition
           | real_type_definition          | array_type_definition
           | record_type_definition        | access_type_definition
           | derived_type_definition       | interface_type_definition
             ;

        subtype_declaration : 
           'subtype' defining_identifier 'is' subtype_indication
                (aspect_specification)? ';'
             ;

        subtype_indication :  (null_exclusion)? subtype_mark (constraint)?
             ;

        subtype_indication_with_constraint : (null_exclusion)? subtype_mark constraint ;

        subtype_mark : subtype_name
             ;

        subtype_name : name ;

        constraint : scalar_constraint | composite_constraint
             ;

        scalar_constraint : 
             range_constraint | digits_constraint | delta_constraint
             ;

        composite_constraint : 
             index_constraint | discriminant_constraint
             ;

        object_declaration : 
            defining_identifier_list
         ':'  ('aliased')? ('constant')? subtype_indication (':=' initial_expression)?
                (aspect_specification)? ';'
          | defining_identifier_list
         ':'  ('aliased')? ('constant')? access_definition (':=' initial_expression)?
                (aspect_specification)? ';'
          | defining_identifier_list
         ':'  ('aliased')? ('constant')? array_type_definition (':=' initial_expression)?
                (aspect_specification)? ';'
          | single_task_declaration
          | single_protected_declaration
             ;

        defining_identifier_list : 
          defining_identifier (',' defining_identifier)*
             ;

        number_declaration : 
             defining_identifier_list ':'  'constant' ':=' static_expression ';'
             ;

        static_initial_expression : initial_expression ;

        static_expression : expression ;        

        derived_type_definition : 
            ('abstract')? ('limited')? 'new' parent_subtype_indication
         (('and' interface_list)? record_extension_part)?
             ;

        parent_subtype_indication : subtype_indication ;

        range_constraint :  'range' range
            ;

        range :  range_attribute_reference
           | simple_expression '..' simple_expression
            ;

        range_with_constraint : simple_expression '..' simple_expression ;

        enumeration_type_definition : 
           '(' (enumeration_literal_specification
         ',')* enumeration_literal_specification ')'
            ;

        enumeration_literal_specification :  defining_identifier
         | defining_character_literal
            ;

        defining_character_literal : Character_literal
            ;

        integer_type_definition : signed_integer_type_definition
         | modular_type_definition
            ;

        signed_integer_type_definition : 'range' static_simple_expression
         '..' static_simple_expression
            ;

        modular_type_definition : 'mod' static_initial_expression
            ;

        real_type_definition : 
           floating_point_definition | fixed_point_definition
            ;

        floating_point_definition : 
          'digits' static_initial_expression (real_range_specification)?
            ;

        real_range_specification : 
          'range' static_simple_expression '..' static_simple_expression
            ;

        fixed_point_definition : ordinary_fixed_point_definition
         | decimal_fixed_point_definition
            ;

        ordinary_fixed_point_definition : 
           'delta' static_initial_expression  real_range_specification
            ;

        decimal_fixed_point_definition : 
           'delta' static_initial_expression 'digits' static_initial_expression
         (real_range_specification)?
            ;

        digits_constraint : 
           'digits' static_simple_expression (range_constraint)?
            ;

        array_type_definition : 
           unconstrained_array_definition | constrained_array_definition
            ;

        unconstrained_array_definition : 
           'array''(' index_subtype_definition (',' index_subtype_definition)*
         ')' 'of' component_definition
            ;

        index_subtype_definition : subtype_mark 'range' '<>'
            ;

        constrained_array_definition : 
           'array' '(' (discrete_subtype_definition
         ',')* discrete_subtype_definition ')' 'of' component_definition
            ;

        discrete_subtype_definition : discrete_subtype_indication | range_with_constraint
            ;

        discrete_subtype_indication : subtype_indication ;

        discrete_subtype_indication_with_constraint : subtype_indication_with_constraint ;

        component_definition : 
           ('aliased')? subtype_indication
         | ('aliased')? access_definition
            ;

        index_constraint :  '(' discrete_range (',' discrete_range)* ')'
            ;

        discrete_range : discrete_subtype_indication | range_with_constraint
            ;

        discrete_range_with_constraint : subtype_indication_with_constraint |
          range_with_constraint ;

        discriminant_part : unknown_discriminant_part
         | known_discriminant_part
            ;

        unknown_discriminant_part : '(' '<>' ')'
            ;

        known_discriminant_part : 
           '(' (discriminant_specification  ';')* discriminant_specification ')'
            ;

        discriminant_specification : 
           defining_identifier_list ':'  (null_exclusion)? subtype_mark
         (':=' default_expression)?
         | defining_identifier_list ':'  access_definition
         (':=' default_expression)?
            ;

        default_expression : initial_expression
            ;

        discriminant_constraint : 
           '(' (discriminant_association ',')* discriminant_association ')'
            ;

        discriminant_association : 
           ((discriminant_selector_name '|') discriminant_selector_name
         '=>')? expression
            ;

        discriminant_selector_name : selector_name ;

        record_type_definition : (('abstract')? 'tagged')? ('limited')? record_definition
            ;

        record_definition : 
            'record'
               component_list
            'end' 'record' (record_identifier)?
          | 'null' 'record'
            ;

        record_identifier : Identifier ;

        component_list : 
              (component_item)* component_item
           | (component_item)* variant_part
           |  'null' ';'
            ;

        component_item : component_declaration | aspect_clause
            ;

        component_declaration : 
           defining_identifier_list ':'  component_definition
         (':=' default_expression)?
                (aspect_specification)? ';'
            ;

        variant_part : 
           'case' discriminant_direct_name 'is'
              (variant)*
               variant
           'end' 'case' ';'
            ;

        discriminant_direct_name : direct_name ;

        variant : 
           'when' discrete_choice_list '=>'
              component_list
            ;

        discrete_choice_list : (discrete_choice '|')* discrete_choice
            ;

        discrete_choice : choice_expression | discrete_subtype_indication_with_constraint
         | range_with_constraint | 'others'
            ;

        record_extension_part : 'with' record_definition
            ;

        abstract_subprogram_declaration : 
            (overriding_indicator)?
            subprogram_specification 'is' 'abstract'
                (aspect_specification)? ';'
            ;

        interface_type_definition : 
            ('limited' | 'task' | 'protected' | 'synchronized')? 'interface' ('and' interface_list
        )?
            ;

        interface_list : interface_subtype_mark
         ('and' interface_subtype_mark)*
            ;

        interface_subtype_mark : subtype_mark ;

        access_type_definition : 
            (null_exclusion)? access_to_object_definition
          | (null_exclusion)? access_to_subprogram_definition
            ;

        access_to_object_definition : 
            'access' (general_access_modifier)? subtype_indication
            ;

        general_access_modifier : 'all' | 'constant'
            ;

        access_to_subprogram_definition : 
            'access' ('protected')? 'procedure' parameter_profile
          | 'access' ('protected')? 'function'  parameter_and_result_profile
            ;

        null_exclusion : 'not' 'null'
            ;

        access_definition : 
            (null_exclusion)? 'access' ('constant')? subtype_mark
          | (null_exclusion)? 'access' ('protected')? 'procedure' parameter_profile
          | (null_exclusion
        )? 'access' ('protected')? 'function' parameter_and_result_profile
            ;

        incomplete_type_declaration : 'type' defining_identifier
         (discriminant_part)? ('is' 'tagged')? ';'
            ;

        declarative_part : (declarative_item)*
            ;

        declarative_item : 
            basic_declarative_item | body
            ;

        basic_declarative_item : 
            basic_declaration | aspect_clause | use_clause
            ;

        body : proper_body | body_stub
            ;

        proper_body : 
            subprogram_body | package_body | task_body | protected_body
            ;

        unsuffixed_name :
             direct_name          | Character_literal    
           | target_name
            ;

        name : 
             unsuffixed_name (name_suffix)*
           | reduction_attribute_reference
           ;

        name_suffix :
             explicit_dereference_suffix
           | function_call_suffix
           | slice_suffix
           | selected_component_suffix
           | attribute_reference_suffix
           | qualified_expression_suffix
           ;
//           direct_name          | explicit_dereference
//         | indexed_component    
//         | slice
//         | selected_component   | attribute_reference
//         | type_conversion      
//         | function_call_with_parameters
//         | Character_literal    | qualified_expression
//         | generalized_reference | generalized_indexing
//         | target_name
//          ;

        direct_name : Identifier | operator_symbol
            ;

        prefix : name // | implicit_dereference
            ;

        explicit_dereference : name '.' 'all'
            ;

        explicit_dereference_suffix : '.' 'all'
            ;

        implicit_dereference : name
            ;

        indexed_component : prefix '(' expression (',' expression)* ')'
            ;

        slice : prefix '(' discrete_range_with_constraint ')'
            ;

        slice_suffix : '(' discrete_range_with_constraint ')'
            ;

        selected_component : prefix '.' selector_name
            ;

        selected_component_suffix : '.' selector_name
            ;

        selector_name : Identifier | Character_literal | operator_symbol
            ;

        attribute_reference : 
            prefix '\'' attribute_designator
//        | reduction_attribute_reference
            ;

        attribute_reference_suffix : 
            '\'' attribute_designator
            ;

        attribute_designator : 
            Identifier // ('(' static_expression ')')?
          | 'Access' | 'Delta' | 'Digits' | 'Mod' | 'Range' | 'Class'
            ;

        range_attribute_reference : prefix '\'' range_attribute_designator
            ;

        range_attribute_designator : 'Range' ('(' static_expression ')')?
            ;

        generalized_reference : reference_object_name
            ;

        reference_object_name : name ;

        generalized_indexing : indexable_container_object_prefix
         actual_parameter_part
            ;

        indexable_container_object_prefix : prefix ;

        aggregate : 
            record_aggregate | extension_aggregate | array_aggregate
          | delta_aggregate | container_aggregate
            ;

        record_aggregate : '(' record_component_association_list ')'
            ;

        record_component_association_list : 
            (record_component_association ',')* record_component_association
          | 'null' 'record'
            ;

        record_component_association : 
            (component_choice_list '=>')? expression
           | component_choice_list '=>' '<>'
            ;

        component_choice_list : 
             (component_selector_name '|')* component_selector_name
           | 'others'
            ;

        component_selector_name : selector_name ;

        extension_aggregate : 
            '(' ancestor_part 'with' record_component_association_list ')'
            ;

        ancestor_part : initial_expression //  | subtype_mark
            ;

        array_aggregate : 
            positional_array_aggregate | null_array_aggregate
         | named_array_aggregate
            ;

        positional_array_aggregate : 
            '(' (expression ',')* expression ',' expression ')'
          | '(' (expression ',')* expression ',' 'others' '=>' expression ')'
          | '(' (expression ',')* expression ',' 'others' '=>' '<>' ')'
          | '[' (expression ',')* expression (',' 'others' '=>' expression)? ']'
          | '[' (expression ',')* expression ',' 'others' '=>' '<>' ']'
            ;

        null_array_aggregate : '[' ']'
            ;

        named_array_aggregate : 
            '(' array_component_association_list ')'
          | '[' array_component_association_list ']'
            ;

        array_component_association_list : 
            (array_component_association ',')* array_component_association
            ;

        array_component_association : 
            discrete_choice_list '=>' expression
          | discrete_choice_list '=>' '<>'
          | iterated_component_association
            ;

        iterated_component_association : 
            'for' defining_identifier 'in' discrete_choice_list '=>' expression
          | 'for' iterator_specification '=>' expression
            ;

        delta_aggregate : record_delta_aggregate | array_delta_aggregate
            ;

        record_delta_aggregate : 
            '(' base_expression 'with' 'delta' record_component_association_list ')'
            ;

        base_expression : expression ;

        array_delta_aggregate : 
            '(' base_expression 'with' 'delta' array_component_association_list ')'
          | '[' base_expression 'with' 'delta' array_component_association_list
         ']'
            ;

        container_aggregate : 
            null_container_aggregate
          | positional_container_aggregate
          | named_container_aggregate
            ;

        null_container_aggregate : '[' ']'
            ;

        positional_container_aggregate : '[' (expression',') expression ']'
            ;

        named_container_aggregate : '[' container_element_association_list
         ']'
            ;

        container_element_association_list : 
            (container_element_association ',') container_element_association
            ;

        container_element_association : 
            key_choice_list '=>' expression
          | key_choice_list '=>' '<>'
          | iterated_element_association
            ;

        key_choice_list : (key_choice '|') key_choice
            ;

        key_choice : key_expression | discrete_range_with_constraint
            ;

        key_expression : expression ;

        iterated_element_association : 
            'for' loop_parameter_specification( 'use' key_expression
        )? '=>' expression
          | 'for' iterator_specification( 'use' key_expression)? '=>' expression
            ;

        initial_expression : 
             initial_relation ('and' initial_relation)*
           | initial_relation ('and' 'then' initial_relation)*
           | initial_relation ('or' initial_relation)*
           | initial_relation ('or' 'else' initial_relation)*
           | initial_relation ('xor' initial_relation)*
            ;

        expression : 
             relation ('and' relation)*
           | relation ('and' 'then' relation)*
           | relation ('or' relation)*
           | relation ('or' 'else' relation)*
           | relation ('xor' relation)*
            ;

        choice_expression : 
             choice_relation ('and' choice_relation)*
           | choice_relation ('or' choice_relation)*
           | choice_relation ('xor' choice_relation)*
           | choice_relation ('and' 'then' choice_relation)*
           | choice_relation ('or' 'else' choice_relation)*
            ;

        choice_relation : 
             simple_expression (relational_operator simple_expression)?
            ;

        initial_relation : 
             simple_expression (relational_operator simple_expression)?
           | tested_simple_expression ('not')? 'in' membership_choice_list
            ;

        relation : 
             simple_expression (relational_operator simple_expression)?
           | tested_simple_expression ('not')? 'in' membership_choice_list
           | raise_expression
            ;

        tested_simple_expression : simple_expression ;

        membership_choice_list : membership_choice ('|' membership_choice)*
            ;

        membership_choice : choice_simple_expression | range
//       | subtype_mark
            ;

        choice_simple_expression : simple_expression ;

        simple_expression : (unary_adding_operator)? term
         (binary_adding_operator term)*
            ;

        term : factor (multiplying_operator factor)*
            ;

        factor : primary ('**' primary)? | 'abs' primary | 'not' primary
            ;

        primary : 
            Numeric_literal | 'null' | String_literal | aggregate
          | name | allocator | '(' expression ')'
          | '(' conditional_expression ')' | '(' quantified_expression ')'
          | '(' declare_expression ')'
            ;

        logical_operator :                         'and' | 'or'  | 'xor'
            ;

        relational_operator :                     
         '='   | '/='  | '<'   | '<=' | '>' | '>='
            ;

        binary_adding_operator :                   '+'   | '-'   | '&'
            ;

        unary_adding_operator :                    '+'   | '-'
            ;

        multiplying_operator :                     '*'   | '/'   | 'mod' | 'rem'
            ;

        highest_precedence_operator :              '**'  | 'abs' | 'not'
            ;

        conditional_expression : if_expression | case_expression
            ;

        if_expression : 
           'if' condition 'then' dependent_expression
           ('elsif' condition 'then' dependent_expression)*
           ('else' dependent_expression)?
            ;

        dependent_expression : expression ;

        condition : boolean_expression
            ;

        boolean_expression : expression ;

        case_expression : 
            'case' selecting_expression 'is'
            case_expression_alternative (','
            case_expression_alternative)*
            ;

        selecting_expression : expression ;

        case_expression_alternative : 
            'when' discrete_choice_list '=>'
                dependent_expression
            ;

        quantified_expression : 'for' quantifier
         loop_parameter_specification '=>' predicate
          | 'for' quantifier iterator_specification '=>' predicate
            ;

        quantifier : 'all' | 'some'
            ;

        predicate : boolean_expression
            ;

        declare_expression : 
            'declare' (declare_item)*
            'begin' body_expression
            ;

        body_expression : expression ;

        declare_item : object_declaration | object_renaming_declaration
            ;

        reduction_attribute_reference : 
            value_sequence '\'' reduction_attribute_designator
//        | prefix '\'' reduction_attribute_designator
            ;

        value_sequence : 
            '[' ('parallel'('(' chunk_specification
         ')')?)? iterated_element_association ']'
            ;

        reduction_attribute_designator : reduction_identifier
        '(' reduction_specification ')'
            ;

        reduction_identifier : Identifier ;

        reduction_specification : reducer_name ',' initial_value_expression
        (',' combiner_name)?
            ;

        reducer_name : name ;

        initial_value_expression : expression ;

        combiner_name : name ;

        type_conversion : 
            subtype_mark'(' expression ')'
          | subtype_mark'(' name ')'
            ;

        qualified_expression : 
           subtype_mark '\'' '(' expression ')' | subtype_mark '\'' aggregate
            ;

        qualified_expression_suffix :
           '\'' '(' expression ')' | '\'' aggregate
           ;

        allocator : 
           'new' (subpool_specification)? subtype_indication
         | 'new' (subpool_specification)? qualified_expression
            ;

        subpool_specification : '(' subpool_handle_name ')'
            ;

        subpool_handle_name : name ;

        sequence_of_statements : statement (statement)* (label)*
            ;

        statement : 
           (label)* simple_statement | (label)* compound_statement
            ;

        simple_statement : null_statement
           | assignment_statement            | exit_statement
           | goto_statement                  | procedure_call_statement
           | simple_return_statement         | entry_call_statement
           | requeue_statement               | delay_statement
           | abort_statement                 | raise_statement
           | code_statement                  | pragma
            ;

        compound_statement : 
             if_statement                    | case_statement
           | loop_statement                  | block_statement
           | extended_return_statement
           | parallel_block_statement
           | accept_statement                | select_statement
            ;

        null_statement : 'null' ';'
            ;

        label : '<<' label_statement_identifier '>>'
            ;

        label_statement_identifier : statement_identifier ;

        statement_identifier : direct_name
            ;

        assignment_statement : 
           variable_name ':=' expression ';'
            ;

        variable_name : name ;

        target_name : '@'
            ;

        if_statement : 
            'if' condition 'then'
              sequence_of_statements
           ('elsif' condition 'then'
              sequence_of_statements)*
           ('else'
              sequence_of_statements)?
            'end' 'if' ';'
            ;

        case_statement : 
           'case' selecting_expression 'is'
               case_statement_alternative
              (case_statement_alternative)*
           'end' 'case' ';'
            ;

        case_statement_alternative : 
           'when' discrete_choice_list '=>'
              sequence_of_statements
            ;

        loop_statement : 
           (loop_statement_identifier':' )?
              (iteration_scheme)? 'loop'
                 sequence_of_statements
               'end' 'loop' (loop_identifier)? ';'
            ;

        loop_statement_identifier : statement_identifier ;

        loop_identifier : Identifier ;

        iteration_scheme : 'while' condition
           | 'for' loop_parameter_specification
           | 'for' iterator_specification
           | 'for' procedural_iterator
           | 'parallel' ('(' chunk_specification ')')?
             'for' loop_parameter_specification
           | 'parallel' ('(' chunk_specification ')')?
             'for' iterator_specification
            ;

        chunk_specification : 
             integer_simple_expression
           | defining_identifier 'in' discrete_subtype_definition
            ;

        integer_simple_expression : simple_expression ;

        loop_parameter_specification : 
           defining_identifier 'in' ('reverse')? discrete_subtype_definition
             (iterator_filter)?
            ;

        iterator_filter : 'when' condition
            ;

        iterator_specification : 
            defining_identifier (':'  loop_parameter_subtype_indication
        )? 'in' ('reverse')? iterator_name
              (iterator_filter)?
          | defining_identifier (':'  loop_parameter_subtype_indication
        )? 'of' ('reverse')? iterable_name
              (iterator_filter)?
            ;

        iterator_name : name ;

        iterable_name : name ;

        loop_parameter_subtype_indication : subtype_indication
         | access_definition
            ;

        procedural_iterator : 
             iterator_parameter_specification 'of' iterator_procedure_call
               (iterator_filter)?
            ;

        iterator_parameter_specification : 
             formal_part
           | '(' defining_identifier(',' defining_identifier)* ')'
            ;

        iterator_procedure_call : 
             procedure_name
           | procedure_prefix iterator_actual_parameter_part
            ;

        procedure_name : name ;

        procedure_prefix : prefix ;

        iterator_actual_parameter_part : 
             '(' iterator_parameter_association
         (',' iterator_parameter_association)* ')'
            ;

        iterator_parameter_association : 
             parameter_association
           | parameter_association_with_box
            ;

        parameter_association_with_box : 
           ( formal_parameter_selector_name '=>' )? '<>'
            ;

        formal_parameter_selector_name : selector_name ;

        block_statement : 
           (block_statement_identifier':' )?
               ('declare'
                    declarative_part)?
                'begin'
                    handled_sequence_of_statements
                'end' (block_identifier)? ';'
            ;

        block_statement_identifier : statement_identifier ;

        block_identifier : Identifier ;

        parallel_block_statement : 
            'parallel' 'do'
               handled_sequence_of_statements
            'and'
               handled_sequence_of_statements
           ('and'
               handled_sequence_of_statements)*
            'end' 'do' ';'
            ;

        exit_statement : 
           'exit' (loop_name)? ('when' condition)? ';'
            ;

        loop_name : name ;

        goto_statement : 'goto' label_name ';'
            ;

        label_name : name ;

        subprogram_declaration : 
            (overriding_indicator)?
            subprogram_specification
                (aspect_specification)? ';'
            ;

        subprogram_specification : 
            procedure_specification
          | function_specification
            ;

        procedure_specification : 'procedure' defining_program_unit_name
         parameter_profile
            ;

        function_specification : 'function' defining_designator
         parameter_and_result_profile
            ;

        designator : (parent_unit_name '.' )? Identifier | operator_symbol
            ;

        defining_designator : defining_program_unit_name
         | defining_operator_symbol
            ;

//      defining_program_unit_name : (parent_unit_name

        defining_program_unit_name : parent_unit_name
//       '.' )? defining_identifier
            ;

        operator_symbol : String_literal
            ;

        defining_operator_symbol : operator_symbol
            ;

        parameter_profile : (formal_part)?
            ;

        parameter_and_result_profile : 
            (formal_part)? 'return' (null_exclusion)? subtype_mark
          | (formal_part)? 'return' access_definition
            ;

        formal_part : 
           '(' parameter_specification ( ';' parameter_specification)* ')'
            ;

        parameter_specification : 
            defining_identifier_list ':'  ('aliased')? param_mode (null_exclusion
        )? subtype_mark (':=' default_expression)?
          | defining_identifier_list ':'  access_definition
         (':=' default_expression)?
            ;

        param_mode : ('in')? | 'in' 'out' | 'out'
            ;

        global_aspect_definition : 
            primitive_global_aspect_definition
            ;

        primitive_global_aspect_definition : 
            'null'
          | 'Unspecified'
          | global_mode global_designator
          | '(' global_aspect_element ( ',' global_aspect_element)* ')'
            ;

        global_aspect_element :
            global_mode global_set 
            ;

        global_mode : basic_global_mode
          | extended_global_mode
            ;

        basic_global_mode : 'in' | 'in' 'out' | 'out'
            ;

        global_set : 
            global_designator (',' global_designator)*
            ;

        global_designator : global_group_designator | global_name
            ;

        global_group_designator : 'all' | 'synchronized'
            ;

        global_name : 
            object_name
          | package_name
            ;

        object_name : name ;

        package_name : name ;

        extended_global_mode :
            'overriding' basic_global_mode
            ;
        
        formal_parameter_designator :
            formal_group_designator
          | formal_parameter_name
            ;
        
        formal_parameter_set :
            formal_group_designator
          | formal_parameter_name (',' formal_parameter_name )*
            ;
          
        formal_group_designator : 'null' | 'all'
            ;
          
        formal_parameter_name :
           formal_subtype_mark
         | formal_subprogram_name
         | formal_access_to_subprogram_object_name
            ;

        formal_subtype_mark : name ;

        formal_subprogram_name : name ;

        formal_access_to_subprogram_object_name : name ;
         
        dispatching_operation_set :
          dispatching_operation_specifier (',' dispatching_operation_specifier )*
            ;
          
        dispatching_operation_specifier :
          dispatching_operation_name '(' object_name ')' 
            ;
 
        dispatching_operation_name : name ;

        subprogram_body : 
            (overriding_indicator)?
            subprogram_specification
               (aspect_specification)? 'is'
               declarative_part
            'begin'
                handled_sequence_of_statements
            'end' (designator)? ';'
            ;

        procedure_call_statement : 
            procedure_name ';'
          | procedure_prefix actual_parameter_part ';'
            ;

        function_call : 
            function_name
          | function_prefix actual_parameter_part
            ;

        function_call_with_parameters :
            function_prefix actual_parameter_part
            ;

        function_call_suffix : actual_parameter_part ;

        function_name : name ;

        function_prefix : prefix ;

        actual_parameter_part : 
            '(' parameter_association (',' parameter_association)* ')'
            ;

        parameter_association : 
           (formal_parameter_selector_name '=>')? explicit_actual_parameter
            ;

        explicit_actual_parameter : expression | variable_name
            ;

        simple_return_statement : 'return' (expression)? ';'
            ;

        extended_return_object_declaration : 
            defining_identifier
         ':'  ('aliased')?('constant')? return_subtype_indication (':=' expression)?
            ;

        extended_return_statement : 
            'return' extended_return_object_declaration ('do'
                handled_sequence_of_statements
            'end' 'return')? ';'
            ;

        return_subtype_indication : subtype_indication
         | access_definition
            ;

        null_procedure_declaration : 
           (overriding_indicator)?
           procedure_specification 'is' 'null'
               (aspect_specification)? ';'
            ;

        expression_function_declaration : 
           (overriding_indicator)?
           function_specification 'is'
               '(' expression ')'
               (aspect_specification)? ';'
         | (overriding_indicator)?
           function_specification 'is'
               aggregate
               (aspect_specification)? ';'
            ;

        package_declaration : package_specification ';'
            ;

        package_specification : 
            'package' defining_program_unit_name
                (aspect_specification)? 'is'
              (basic_declarative_item)*
           ('private'
              (basic_declarative_item)*)?
            'end' ((parent_unit_name '.')?Identifier)?
            ;

        package_body : 
            'package' 'body' defining_program_unit_name
                (aspect_specification)? 'is'
               declarative_part
           ('begin'
                handled_sequence_of_statements)?
            'end' ((parent_unit_name '.')?Identifier)? ';'
            ;

        private_type_declaration : 
           'type' defining_identifier (discriminant_part
        )? 'is' (('abstract')? 'tagged')? ('limited')? 'private'
              (aspect_specification)? ';'
            ;

        private_extension_declaration : 
           'type' defining_identifier (discriminant_part)? 'is'
             ('abstract')? ('limited' | 'synchronized')? 'new' ancestor_subtype_indication
             ('and' interface_list)? 'with' 'private'
               (aspect_specification)? ';'
            ;

        ancestor_subtype_indication : subtype_indication ;

        overriding_indicator : ('not')? 'overriding'
            ;

        use_clause : use_package_clause | use_type_clause
            ;

        use_package_clause : 'use' package_name (',' package_name)* ';'
            ;

        use_type_clause : 'use' ('all')? 'type' subtype_mark (',' subtype_mark)* ';'
            ;

        renaming_declaration : 
              object_renaming_declaration
            | exception_renaming_declaration
            | package_renaming_declaration
            | subprogram_renaming_declaration
            | generic_renaming_declaration
            ;

        object_renaming_declaration : 
            defining_identifier (':'  (null_exclusion)? subtype_mark
        )? 'renames' object_name
                (aspect_specification)? ';'
          | defining_identifier ':'  access_definition 'renames' object_name
                (aspect_specification)? ';'
            ;

        exception_renaming_declaration : defining_identifier
         ':'  'exception' 'renames' exception_name
           (aspect_specification)? ';'
            ;

        exception_name : name ;

        package_renaming_declaration : 'package' defining_program_unit_name
         'renames' package_name
           (aspect_specification)? ';'
            ;

        subprogram_renaming_declaration : 
            (overriding_indicator)?
            subprogram_specification 'renames' callable_entity_name
                (aspect_specification)? ';'
            ;

        callable_entity_name : name ;

        generic_renaming_declaration : 
            'generic' 'package'       defining_program_unit_name
         'renames' generic_package_name
                (aspect_specification)? ';'
          | 'generic' 'procedure'     defining_program_unit_name
         'renames' generic_procedure_name
                (aspect_specification)? ';'
          | 'generic' 'function'      defining_program_unit_name
         'renames' generic_function_name
                (aspect_specification)? ';'
            ;

        generic_package_name : name ;

        generic_procedure_name : name ;

        generic_function_name : name ;

        task_type_declaration : 
           'task' 'type' defining_identifier (known_discriminant_part)?
                (aspect_specification)? ('is'
             ('new' interface_list 'with')?
             task_definition)? ';'
            ;

        single_task_declaration : 
           'task' defining_identifier 
                (aspect_specification)?('is'
             ('new' interface_list 'with')?
             task_definition)? ';'
            ;

        task_definition : 
             (task_item)*
          ( 'private'
             (task_item)*)?
          'end' (task_identifier)?
            ;

        task_identifier : Identifier ;

        task_item : entry_declaration | aspect_clause
            ;

        task_body : 
           'task' 'body' defining_identifier
                (aspect_specification)? 'is'
             declarative_part
           'begin'
             handled_sequence_of_statements
           'end' (task_identifier)? ';'
            ;

        protected_type_declaration : 
          'protected' 'type' defining_identifier (known_discriminant_part)?
                (aspect_specification)? 'is'
             ('new' interface_list 'with')?
             protected_definition ';'
            ;

        single_protected_declaration : 
          'protected' defining_identifier
                (aspect_specification)? 'is'
             ('new' interface_list 'with')?
             protected_definition ';'
            ;

        protected_definition : 
            ( protected_operation_declaration )*
        ( 'private'
            ( protected_element_declaration )* )?
          'end' (protected_identifier)?
            ;

        protected_identifier : Identifier ;

        protected_operation_declaration : subprogram_declaration
             | entry_declaration
             | aspect_clause
            ;

        protected_element_declaration : protected_operation_declaration
             | component_declaration
            ;

        protected_body : 
          'protected' 'body' defining_identifier
                (aspect_specification)? 'is'
           ( protected_operation_item )*
          'end' (protected_identifier)? ';'
            ;

        protected_operation_item : subprogram_declaration
             | subprogram_body
             | null_procedure_declaration
             | expression_function_declaration
             | entry_body
             | aspect_clause
            ;

        Synchronization_kind : 'By_Entry' | 'By_Protected_Procedure' | 'Optional'
            ;

        entry_declaration : 
           (overriding_indicator)?
           'entry' defining_identifier ('(' discrete_subtype_definition
         ')')? parameter_profile
              (aspect_specification)? ';'
            ;

        accept_statement : 
           'accept' entry_direct_name ('(' entry_index ')')? parameter_profile ('do'
             handled_sequence_of_statements
           'end' (entry_identifier)?)? ';'
            ;

        entry_direct_name : direct_name ;

        entry_index : expression
            ;

        entry_body : 
            'entry' defining_identifier entry_body_formal_part
               (aspect_specification)?
            entry_barrier 'is'
               declarative_part
            'begin'
               handled_sequence_of_statements
            'end' (entry_identifier)? ';'
            ;

        entry_identifier : Identifier ;

        entry_body_formal_part : ('(' entry_index_specification
         ')')? parameter_profile
            ;

        entry_barrier : 'when' condition
            ;

        entry_index_specification : 'for' defining_identifier
         'in' discrete_subtype_definition
            ;

        entry_call_statement : entry_name (actual_parameter_part)? ';'
            ;

        entry_name : name ;

        requeue_statement : 'requeue' procedure_or_entry_name ('with' 'abort')? ';'
            ;

        procedure_or_entry_name : name ;

        delay_statement : delay_until_statement
         | delay_relative_statement
            ;

        delay_until_statement : 'delay' 'until' delay_expression ';'
            ;

        delay_expression : expression ;

        delay_relative_statement : 'delay' delay_expression ';'
            ;

        select_statement : 
           selective_accept
          | timed_entry_call
          | conditional_entry_call
          | asynchronous_select
            ;

        selective_accept : 
          'select'
           (guard)?
             select_alternative
        ( 'or'
           (guard)?
             select_alternative )*
        ( 'else'
           sequence_of_statements )?
          'end' 'select' ';'
            ;

        guard : 'when' condition '=>'
            ;

        select_alternative : 
           accept_alternative
          | delay_alternative
          | terminate_alternative
            ;

        accept_alternative : 
          accept_statement (sequence_of_statements)?
            ;

        delay_alternative : 
          delay_statement (sequence_of_statements)?
            ;

        terminate_alternative : 'terminate' ';'
            ;

        timed_entry_call : 
          'select'
           entry_call_alternative
          'or'
           delay_alternative
          'end' 'select' ';'
            ;

        entry_call_alternative : 
          procedure_or_entry_call (sequence_of_statements)?
            ;

        procedure_or_entry_call : 
          procedure_call_statement | entry_call_statement
            ;

        conditional_entry_call : 
          'select'
           entry_call_alternative
          'else'
           sequence_of_statements
          'end' 'select' ';'
            ;

        asynchronous_select : 
          'select'
           triggering_alternative
          'then' 'abort'
           abortable_part
          'end' 'select' ';'
            ;

        triggering_alternative : triggering_statement
         (sequence_of_statements)?
            ;

        triggering_statement : procedure_or_entry_call | delay_statement
            ;

        abortable_part : sequence_of_statements
            ;

        abort_statement : 'abort' task_name (',' task_name)* ';'
            ;

        task_name : name ;

        compilation : (compilation_unit)* EOF
            ;

        compilation_unit : 
            context_clause library_item
          | context_clause subunit
            ;

        library_item : ('private')? library_unit_declaration
          | library_unit_body
          | ('private')? library_unit_renaming_declaration
            ;

        library_unit_declaration : 
             subprogram_declaration   | package_declaration
           | generic_declaration      | generic_instantiation
           | pragma
            ;

        library_unit_renaming_declaration : 
           package_renaming_declaration
         | generic_renaming_declaration
         | subprogram_renaming_declaration
            ;

        library_unit_body : subprogram_body | package_body
            ;

        parent_unit_name : name
            ;

        context_clause : (context_item)*
            ;

        context_item : with_clause | use_clause | pragma
            ;

        with_clause : limited_with_clause | nonlimited_with_clause
            ;

        limited_with_clause : 'limited' ('private')? 'with' library_unit_name
         (',' library_unit_name)* ';'
            ;

        library_unit_name : name ;

        nonlimited_with_clause : ('private')? 'with' library_unit_name
         (',' library_unit_name)* ';'
            ;

        body_stub : subprogram_body_stub | package_body_stub
         | task_body_stub | protected_body_stub
            ;

        subprogram_body_stub : 
           (overriding_indicator)?
           subprogram_specification 'is' 'separate'
              (aspect_specification)? ';'
            ;

        package_body_stub : 
           'package' 'body' defining_identifier 'is' 'separate'
              (aspect_specification)? ';'
            ;

        task_body_stub : 
           'task' 'body' defining_identifier 'is' 'separate'
              (aspect_specification)? ';'
            ;

        protected_body_stub : 
           'protected' 'body' defining_identifier 'is' 'separate'
              (aspect_specification)? ';'
            ;

        subunit : 'separate' '(' parent_unit_name ')' proper_body
            ;

        exception_declaration : defining_identifier_list ':'  'exception'
           (aspect_specification)? ';'
            ;

        handled_sequence_of_statements : 
             sequence_of_statements
          ('exception'
             exception_handler
            (exception_handler)*)?
            ;

        exception_handler : 
          'when' (choice_parameter_specification':' )? exception_choice
         ('|' exception_choice)* '=>'
             sequence_of_statements
            ;

        choice_parameter_specification : defining_identifier
            ;

        exception_choice : exception_name | 'others'
            ;

        raise_statement : 'raise' ';'
              | 'raise' exception_name ('with' string_expression)? ';'
            ;

        string_expression : expression ;

        raise_expression : 'raise' exception_name
         ('with' string_simple_expression)?
            ;

        string_simple_expression : simple_expression ;

        generic_declaration : generic_subprogram_declaration
         | generic_package_declaration
            ;

        generic_subprogram_declaration : 
             generic_formal_part  subprogram_specification
                (aspect_specification)? ';'
            ;

        generic_package_declaration : 
             generic_formal_part  package_specification ';'
            ;

        generic_formal_part : 'generic' (generic_formal_parameter_declaration
         | use_clause | pragma)*
            ;

        generic_formal_parameter_declaration : 
              formal_object_declaration
            | formal_type_declaration
            | formal_subprogram_declaration
            | formal_package_declaration
            ;

        generic_instantiation : 
             'package' defining_program_unit_name 'is'
                 'new' generic_package_name (generic_actual_part)?
                    (aspect_specification)? ';'
           | (overriding_indicator)?
             'procedure' defining_program_unit_name 'is'
                 'new' generic_procedure_name (generic_actual_part)?
                    (aspect_specification)? ';'
           | (overriding_indicator)?
             'function' defining_designator 'is'
                 'new' generic_function_name (generic_actual_part)?
                    (aspect_specification)? ';'
            ;

        generic_actual_part : 
           '(' generic_association (',' generic_association)* ')'
            ;

        generic_association : 
           (generic_formal_parameter_selector_name
         '=>')? explicit_generic_actual_parameter
            ;

        generic_formal_parameter_selector_name : selector_name ;

        explicit_generic_actual_parameter : expression 
//         | variable_name
//         | subprogram_name | entry_name | subtype_mark
//         | package_instance_name
            ;

        subprogram_name : name ;

        package_instance_name : name ;

        formal_object_declaration : 
            defining_identifier_list ':'  param_mode (null_exclusion)? subtype_mark
         (':=' default_expression)?
                (aspect_specification)? ';'
          |  defining_identifier_list ':'  param_mode access_definition
         (':=' default_expression)?
                (aspect_specification)? ';'
            ;

        formal_type_declaration : 
              formal_complete_type_declaration
            | formal_incomplete_type_declaration
            ;

        formal_complete_type_declaration : 
            'type' defining_identifier(discriminant_part
        )? 'is' formal_type_definition
                (aspect_specification)? ';'
            ;

        formal_incomplete_type_declaration : 
            'type' defining_identifier(discriminant_part)? ('is' 'tagged')? ';'
            ;

        formal_type_definition : 
              formal_private_type_definition
            | formal_derived_type_definition
            | formal_discrete_type_definition
            | formal_signed_integer_type_definition
            | formal_modular_type_definition
            | formal_floating_point_definition
            | formal_ordinary_fixed_point_definition
            | formal_decimal_fixed_point_definition
            | formal_array_type_definition
            | formal_access_type_definition
            | formal_interface_type_definition
          ;

        formal_private_type_definition : (('abstract')? 'tagged')? ('limited')? 'private'
          ;

        formal_derived_type_definition : 
             ('abstract')? ('limited' | 'synchronized')? 'new' subtype_mark
         (('and' interface_list)? 'with' 'private')?
          ;

        formal_discrete_type_definition : '(' '<>' ')'
          ;

        formal_signed_integer_type_definition : 'range' '<>'
          ;

        formal_modular_type_definition : 'mod' '<>'
          ;

        formal_floating_point_definition : 'digits' '<>'
          ;

        formal_ordinary_fixed_point_definition : 'delta' '<>'
          ;

        formal_decimal_fixed_point_definition : 'delta' '<>' 'digits' '<>'
          ;

        formal_array_type_definition : array_type_definition
          ;

        formal_access_type_definition : access_type_definition
          ;

        formal_interface_type_definition : interface_type_definition
          ;

        formal_subprogram_declaration : formal_concrete_subprogram_declaration
            | formal_abstract_subprogram_declaration
          ;

        formal_concrete_subprogram_declaration : 
             'with' subprogram_specification ('is' subprogram_default)?
                (aspect_specification)? ';'
          ;

        formal_abstract_subprogram_declaration : 
             'with' subprogram_specification 'is' 'abstract' (subprogram_default)?
                (aspect_specification)? ';'
          ;

        subprogram_default : default_name | '<>' | 'null'
          ;

        default_name : name
          ;

        formal_package_declaration : 
            'with' 'package' defining_identifier 'is' 'new' generic_package_name
          formal_package_actual_part
                (aspect_specification)? ';'
          ;

        formal_package_actual_part : 
            '(' ('others' '=>')? '<>' ')'
          | (generic_actual_part)?
          | '(' formal_package_association (',' formal_package_association
        )* (',' 'others' '=>' '<>')? ')'
          ;

        formal_package_association : 
            generic_association
          | generic_formal_parameter_selector_name '=>' '<>'
          ;

        aspect_clause : attribute_definition_clause
              | enumeration_representation_clause
              | record_representation_clause
              | at_clause
              | pragma
          ;

        local_name : direct_name
              | direct_name '\'' attribute_designator
              | library_unit_name
          ;

        aspect_specification : 
           'with' aspect_mark ('=>' aspect_definition)? (','
                   aspect_mark ('=>' aspect_definition)? )*
          ;

        aspect_mark : aspect_identifier ('\'' 'Class')?
          ;

        aspect_identifier : Identifier ;

        aspect_definition : 
            name | expression | Identifier
          | aggregate | global_aspect_definition
          ;

        attribute_definition_clause : 
              'for' local_name '\'' attribute_designator 'use' expression ';'
            | 'for' local_name '\'' attribute_designator 'use' name ';'
            ;

        enumeration_representation_clause : 
            'for' first_subtype_local_name 'use' enumeration_aggregate ';'
           ;

        first_subtype_local_name : local_name ;

        enumeration_aggregate : array_aggregate
           ;

        record_representation_clause : 
            'for' first_subtype_local_name 'use'
               'record' (mod_clause)?
                  (component_clause)*
               'end' 'record' (local_name)? ';'
           ;

        component_clause : 
            component_local_name 'at' position 'range' first_bit '..' last_bit ';'
           ;

        component_local_name : local_name ;

        position : static_expression
           ;

        first_bit : static_simple_expression
           ;

        static_simple_expression : simple_expression ;

        last_bit : static_simple_expression
           ;

        code_statement : qualified_expression ';'
           ;

        storage_pool_indicator : storage_pool_name | 'null' // | 'Standard'
           ;

        storage_pool_name : name ;

        restriction : restriction_identifier
            | restriction_parameter_identifier
         '=>' restriction_parameter_argument
           ;

        restriction_identifier : Identifier ;

        restriction_parameter_identifier : Identifier ;

        restriction_parameter_argument : name | expression
           ;

        delta_constraint : 'delta' static_simple_expression
         (range_constraint)?
           ;

        at_clause : 'for' direct_name 'use' 'at' expression ';'
           ;

        mod_clause : 'at' 'mod' static_expression ';'
           ;
