```
program = {import}, {declaration}
```

```
declaration = type_declaration | function_declaration | variable_declaration
```

```
type_declaration = "type", identifier, generic_params, type_name | type_definition
```

```
type_definition = 
              ("tuple", generic_params)  
            | ("union", generic_params)  
            | ("enum", "<", type_name, ">", enum_definition)  
            | ("struct", struct_definition)  
            | ("interface", interface_definition)
```

```
generic_params = "<", {type_name} ,">"
```

```
type_name = identifier
```

```
struct_definition = "{", {identifier, ":", type_name, ["=", expression], ";"}, "}"
```

```
interface_definition = "{", {function_declaration} ,"}"
```

```
function_declaration = "func", identifier, "(", {parameter}, ")", [return_type], ";" | function_definition
```

```
parameter = [identifier, ":"], type_name 
```

```
return_type = type_name | type_definition
```