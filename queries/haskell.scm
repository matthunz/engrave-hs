(char) @constant.character
(integer) @constant.numeric.integer
(string) @string

; ---------- Keywords ----------

[
  "where"
  "let"
  "in"
  "class"
  "instance"
  "pattern"
  "data"
  "newtype"
  "family"
  "type"
  "as"
  "hiding"
  "deriving"
  "via"
  "stock"
  "anyclass"
  "do"
  "mdo"
  "rec"
  "infix"
  "infixl"
  "infixr"
] @keyword

[
  (operator)
  (constructor_operator)
  (all_names)
  (wildcard)
  "."
  ".."
  "="
  "|"
  "::"
  "=>"
  "->"
  "<-"
  "\\"
  "`"
  "@"
] @operator

; ---------- Punctuation ----------

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  ";"
] @punctuation.delimiter

; ---------- Functions and variables ----------

(decl
  [
   name: (variable) @function
   names: (binding_list (variable) @function)
  ])

(decl/bind
  name: (variable) @variable)
