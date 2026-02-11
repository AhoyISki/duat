; Simple tokens ;;;;
(terminal) @string

(special_sequence) @string.special

(integer) @number

(comment) @comment @spell

; Identifiers ;;;;
; Allow different highlighting for specific casings
((identifier) @type
  (#match? @type "^%u"))

((identifier) @string.special.symbol
  (#match? @string.special.symbol "^%l"))

((identifier) @constant
  (#match? @constant "^%u[%u%d_]+$"))

; Punctuation ;;;;
[
  ";"
  ","
] @punctuation.delimiter

[
  "|"
  "*"
  "-"
] @operator

"=" @keyword.operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket
