; Functions

(call_expression
  function: [
    (identifier) @function
    (field_expression
      field: (field_identifier) @function)
    (qualified_identifier
      name: [
        (identifier) @function
        (qualified_identifier
          (identifier) @function)
      ])
  ])

(template_function
  name: (identifier) @function)

(template_method
  name: (field_identifier) @function)

(template_function
  name: (identifier) @function)

(function_declarator
  type: (identifier) @type)

(function_declarator
  declarator: [
    (identifier) @function
    (field_identifier) @function
    (qualified_identifier
      name: [
        (identifier) @function
        (qualified_identifier
          (identifier) @function)
      ])
  ])

(destructor_name
  (identifier) @function)

; Punctuation

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

(template_parameter_list
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

(template_argument_list
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

"::" @punctuation.delimiter
":" @punctuation.delimiter
"." @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter

"=" @operator
"+" @operator
"-" @operator
"!" @operator
"*" @operator
"&" @operator
"&&" @operator
"~" @operator
"->" @operator

(assignment_expression
  operator: _ @operator)
(binary_expression
  operator: _ @operator)
(update_expression
  operator: _ @operator)

(operator_name
  "operator"
  _ @operator)

; Types

((namespace_identifier) @type
 (#match? @type "^[A-Z]"))

(auto) @type
(primitive_type) @type
(type_identifier) @type
"long" @type

(parameter_declaration
  declarator: [
    (identifier) @variable.parameter
    (pointer_declarator
      (identifier) @variable.parameter)
    (reference_declarator
      (identifier) @variable.parameter)
  ])

; Modules

(namespace_identifier) @module
(qualified_identifier
  scope: (namespace_identifier) @module)

; Constants

(this) @variable.builtin
(null "nullptr" @constant)
(true) @constant.builtin
(false) @constant.builtin

; Keywords

"catch" @keyword
"class" @keyword
"co_await" @keyword
"co_return" @keyword
"co_yield" @keyword
"const" @keyword
"constexpr" @keyword
"constinit" @keyword
"consteval" @keyword
"default" @keyword
"delete" @keyword
"do" @keyword
"explicit" @keyword
"final" @keyword
"for" @keyword
"friend" @keyword
"if" @keyword
"else" @keyword
"mutable" @keyword
"namespace" @keyword
"noexcept" @keyword
"new" @keyword
"operator" @keyword
"override" @keyword
"private" @keyword
"protected" @keyword
"public" @keyword
"struct" @keyword
"template" @keyword
"throw" @keyword
"try" @keyword
"typename" @keyword
"using" @keyword
"concept" @keyword
"requires" @keyword
"virtual" @keyword
"while" @keyword

; Strings

(raw_string_literal) @string
(string_literal) @string

; Meta

"#include" @attribute
(preproc_directive) @attribute

(system_lib_string) @string.path
