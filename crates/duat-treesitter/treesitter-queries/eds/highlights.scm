"=" @punctuation.delimiter

[
  "["
  "]"
] @punctuation.bracket

((section_name) @variable.builtin
  (#match? @variable.builtin
    "\\s^(FileInfo|DeviceInfo|DummyUsage|MandatoryObjects|OptionalObjects)$"))

((section_name) @variable.builtin
  (#match? @variable.builtin "^1"))

(section
  (section_name) @_name
  (#match? @_name "\\s^Comments$")) @comment

(section
  (section_name) @_name
  (statement
    (key) @_key) @string
  (#match? @_key "\\s^ParameterName$")
  (#not-match? @_name "\\s^Comments$"))

(section
  (section_name) @_name
  (statement
    (key) @_key) @type
  (#match? @_key "\\s^(ObjectType|DataType|AccessType)$")
  (#not-match? @_name "\\s^Comments$"))

(section
  (section_name) @_name
  (statement
    (key) @_key) @attribute
  (#match? @_key "\\s^PDOMapping$")
  (#not-match? @_name "\\s^Comments$"))

(section
  (section_name) @_name
  (statement
    (key) @_key) @number
  (#match? @_key "\\s^(DefaultValue|LowLimit|HighLimit|SubNumber)$")
  (#not-match? @_name "\\s^Comments$"))
