; Identifiers

(primitive_type) @type.builtin
(field_identifier) @property

; Identifier conventions

; Assume all-caps names are constants
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]+$"))

; Assume uppercase names are enum constructors
((identifier) @constructor
 (#match? @constructor "^[A-Z]"))

; Assume all qualified names in struct patterns are enum constructors. (They're
; either that, or struct names; highlighting both as constructors seems to be
; the less glaring choice of error, visually.)
(struct_pattern
  type: (scoped_type_identifier
    name: (type_identifier) @constructor))

; Modules

(mod_item 
  name: (identifier) @module) 
(scoped_identifier
  path: (identifier) @module
  (#match? @module "^[a-z\\d_]+$"))
(scoped_type_identifier
  path: (identifier) @module
  (#match? @module "^[a-z\\d_]+$"))
(scoped_use_list 
  path: (identifier) @module
  (#match? @module "^[a-z\\d_]+$"))

; This will often conflict with function identifiers, but those override it
(scoped_identifier
  name: (identifier) @module
  (#match? @module "^[a-z\\d_]+$"))

; I don't think this expression makes any sense, but I'll assume it's a module
(use_declaration
  argument: (identifier) @module)
 
; Functions

(call_expression
  function: [
    (identifier) @function
    (field_expression
      field: (field_identifier) @function.method)
    (scoped_identifier
      "::"
      name: (identifier) @function)
    ])

(generic_function
  function: [
    (identifier) @function
    (scoped_identifier
      name: (identifier) @function)
    (field_expression
      field: (field_identifier) @function.method)
  ])

(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)

(function_item (identifier) @function)
(function_signature_item (identifier) @function)

; I assume that any final names in paths are functions, though that may not
; always be the case
(use_declaration
  argument: ((scoped_identifier
    name: (identifier) @function
    (#match? @function "^[a-z\\d_]+$"))))

; Comments

(line_comment) @comment
(block_comment) @comment

(line_comment (doc_comment)) @comment.documentation
(block_comment (doc_comment)) @comment.documentation

; Punctuation

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

(type_arguments
  "<" @punctuation.bracket
  ">" @punctuation.bracket)
(type_parameters
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

"\$" @punctuation.bracket

"::" @punctuation.delimiter
":" @punctuation.delimiter
"." @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter

(lifetime (identifier) @label)

"as" @keyword
"async" @keyword
"await" @keyword
"break" @keyword
"const" @keyword
"continue" @keyword
"default" @keyword
"dyn" @keyword
"else" @keyword
"enum" @keyword
"extern" @keyword
"fn" @keyword
"for" @keyword
"if" @keyword
"impl" @keyword
"in" @keyword
"let" @keyword
"loop" @keyword
"macro_rules!" @keyword
"match" @keyword
"mod" @keyword
"move" @keyword
"pub" @keyword
"ref" @keyword
"return" @keyword
"static" @keyword
"struct" @keyword
"trait" @keyword
"type" @keyword
"union" @keyword
"unsafe" @keyword
"use" @keyword
"where" @keyword
"while" @keyword
"yield" @keyword
(crate) @keyword
(mutable_specifier) @keyword
(use_list (self) @keyword)
(scoped_use_list (self) @keyword)
(scoped_identifier (self) @keyword)
(super) @keyword

(self) @variable.builtin

(char_literal) @string
(string_literal) @string
(raw_string_literal) @string

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(float_literal) @constant.builtin

(escape_sequence) @escape

(attribute_item) @attribute
(inner_attribute_item) @attribute

"+" @operator
"-" @operator
"&" @operator
"*" @operator
"=" @operator
"'" @operator
"|" @operator
"->" @operator
"=>" @operator
".." @operator
"..=" @operator

(binary_expression
  operator: _ @operator)
(compound_assignment_expr
  operator: _ @operator)
(unary_expression
  "!" @operator)

; Parameter redefinitions

(parameter 
  (identifier) @variable.parameter)
(metavariable) @variable.parameter

; Type definitions

(scoped_identifier
  (identifier) @type
  (#match? @type "^[A-Z]"))

(scoped_use_list 
  (identifier) @type
  (#match? @type "^[A-Z]"))

(type_identifier) @type
(fragment_specifier 
  _ @type)

; Trait definitions

(trait_item
  name: (type_identifier) @interface)

(impl_item
  trait: [
    (type_identifier) @interface
    (scoped_type_identifier 
      name: (type_identifier) @interface)
    (generic_type
      type: [
        (type_identifier) @interface
        (scoped_type_identifier
          name: (type_identifier) @interface)
      ])
  ])
(abstract_type
  trait: [
    (type_identifier) @interface
    (scoped_type_identifier
      name: (type_identifier) @interface)
    (generic_type
      type: [
        (type_identifier) @interface
        (scoped_type_identifier
          name: (type_identifier) @interface)
      ])
  ])
(dynamic_type
  trait: [
    (type_identifier) @interface
    (scoped_type_identifier
      name: (type_identifier) @interface)
    (generic_type
      type: [
        (type_identifier) @interface
        (scoped_type_identifier
          name: (type_identifier) @interface)
      ])
  ])
(trait_bounds
  [
    (type_identifier) @interface
    (scoped_type_identifier
      name: (type_identifier) @interface)
    (generic_type
      type: [
        (type_identifier) @interface
        (scoped_type_identifier
          name: (type_identifier) @interface)
      ])
  ])
