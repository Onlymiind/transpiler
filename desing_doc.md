
Legend:
  <name> - non-terminal
  '' - used for escaping of special symbols
  stuff? - optional
  stuff+ - repetition
  () - grouping
  | - or
  NON_EMPTY_LIST(stuff, sep = ,) - stuff | (stuff sep NON_EMPTY_LIST(stuff))
  LIST(stuff, sep = ,) - NON_EMPTY_LIST(stuff, sep) | empty
Type declaration:

  Syntax: type <identifier> <type_definition> //note: semicolon must follow, unless type definition is a struct
  <type_definition> = <tuple_definition> | <union_definition> | <struct_definition> | <function_type_definition> | <identifier> | <modified_type>
  <type_definition> = (<type_definition>, <type_definition_list>) | empty //note: trailing comma not allowed
  <tuple_definition> = tuple '<' LIST(<type_definition>) '>'
  <union_definition> = union '<' LIST(<type_definition>) '>'
  <struct_definition> = struct { LIST(<field_declaration>, ;) }
  <function_type_definition> = func'('LIST(<func_param>)')' <type_definition>?
  <modified_type> = (* | '?')+<type_definition>
  <field_declaration> = <identifier> : <type_definition> (= <expression>)?;
  <func_param> = (<identifier> : <type_definition>) | <type_definition>

Variable declarations:
  Syntax: var <identifier> : <type_definition> (= <expression>)?;

Function declarations:
  Syntax: func <identifier>'('LIST(<func_param>)')' <type_definition>? ; | <block>

Statements:
  <block> = {LIST(<statement>)}
  <statement> = <expression> | <return_smt> | <if_smt> | <assignment> | <loop> | <variable_declaration> ; //';' does not follow blocks!

  <return_smt> = return <expression>?
  <assignment> = <identifier> '=' <expression>
  <if_smt> = if <expression> <block> <otherwise>
  <otherwise> = (elif <expression> <block> <otherwise>) | (else <block>) | empty
  <loop> = for (<assignment>;<expression>;<assignment>) | <expression> | empty <block>

Expressions:
  <expression> = <unary_expression> | <binary_expression> | <primary_expression>
  <unary_expression> = <unary_op> <expression>
  <binary_expression> = <unary_expression> | <primary_expression> <binary_op> <expression>
  <primary_expression> = <identifier> | <literal> | <type_cast> | <function_call>
  <type_cast> = <identifier>'('<expression>')' //TODO: support casting to any type
  <function_call> = <identifier>'('LIST(<expression>)')'
