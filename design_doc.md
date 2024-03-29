## Syntax legend
```
  <name> - non-terminal
  '' - used for escaping of special symbols
  <name>? - optional
  <name>+ - repetition
  () - grouping
  | - or
  NON_EMPTY_LIST(<item>, sep = ,) - <item> | (<item> sep NON_EMPTY_LIST(<item>))
  LIST(<item>, sep = ,) - NON_EMPTY_LIST(<item>, sep) | empty
```

### Unary operators
```
- ! * &
```

### Binary operators
Precedence:
```
6: & * / %
5: | + -
4: < > <= >= != ==
3: &&
2: ||
1: =
```

### Expression syntax

```
<expression> = <binary_expression> | <unary_expression> | <primary_expression>
<primary_expression> = ('('<expression>')') | <identifier> | <function_call> | <literal> | <index_expression>
<function_call> = <identifier>'('LIST(<expression>)')'
<literal> = true | false | <integer_constant> | <floating_point_constant>
<unary_expression> = <unary_op>?+ <primary_expression>
<binary_expression> = <expression> <binary_op> <expression>
<index_expression> = <primary_expression> '[' <expression> ']'
```

### Statement syntax
```
<statement> = <return_statement> | <expression_statement> | <loop> | <branch> | <var_decl>
<expression_statement> = <expression> ;
<return_statement> = return <expression>? ;
<branch> = if <expression> <block> <else_branch>?
<else_branch> = (else <block>) | (else <branch>)
<loop> = for (<var_decl> | <expression>)? ( ; <expression>)? ( ; <expression>)? <block>
<var_decl> = var <identifier> (<type>) | (<type> = <expression>) | ( = <expression>) ;
<block> = '{' LIST(<statement>) '}'
```

## Function syntax
```
<function_decl> = func <identifier>'('LIST(((<name> <type>) | <type>))')' <type>? ; | <block>
```

## Type syntax
```
<type> = u64 | bool | f64 | (* <type>) | ( '[' <expression> ']' <type>)
```

## Notes

- Only variable and function declarations are allowed in global scope
- All variables are zero-initialized if no initial value was provided
- Attempting to dereference null directly is compile-time error
- Attempting to dereference null pointer terminates the program with an error message
