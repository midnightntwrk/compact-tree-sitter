; Comments
(comment) @comment

; Keywords
"pragma" @keyword
"import" @keyword
"export" @keyword
"module" @keyword
"ledger" @keyword
"circuit" @keyword
"witness" @keyword
"contract" @keyword
"struct" @keyword
"enum" @keyword
"constructor" @keyword.function
(pure) @keyword.modifier
(sealed) @keyword.modifier
"prefix" @keyword.modifier
"return" @keyword.return
"if" @keyword.conditional
"else" @keyword.conditional
"for" @keyword.repeat
"of" @keyword.repeat
"assert" @keyword
"const" @keyword
"default" @keyword
"map" @keyword
"fold" @keyword
"disclose" @keyword
"pad" @keyword

; Operators
"=" @operator
"+=" @operator
"-=" @operator
(equals) @operator
(not_equals) @operator
">" @operator
"<" @operator
(greater_than_or_equal) @operator
(less_than_or_equal) @operator
(not) @operator
(and) @operator
(or) @operator
"+" @operator
"-" @operator
"*" @operator
":" @operator
"." @operator
"as" @operator
"=>" @operator
"?" @operator
".." @operator
"..." @operator

; Identifiers
(id) @variable
(module_name) @module
(function_name) @function
(struct_name) @type
(enum_name) @type
(contract_name) @type
(tvar_name) @type.parameter

; Types
(tref (id) @type)
"Boolean" @type.builtin
"Field" @type.builtin
"Uint" @type.builtin
"Bytes" @type.builtin
"Opaque" @type.builtin
"Vector" @type.builtin

; Literals
(nat) @number
(str) @string
(file) @string
(version) @constant
"true" @boolean
"false" @boolean

; Punctuation
";" @punctuation.delimiter
"," @punctuation.delimiter
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"<" @punctuation.bracket
">" @punctuation.bracket

; Special Constructs
(pragma (id) @property)
(import_name (id) @namespace)
(gargs (_) @type.parameter)
(struct_arg (id) @field)
(pattern_struct_elt (id) @field)
(term (expr "." (id) @function.call (expr) @parameter))

; Function Definitions and Calls
(cdefn (function_name) @function)
(edecl (function_name) @function)
(wdecl (function_name) @function)
(ecdecl_circuit (id) @function)
(fun (id) @function)
