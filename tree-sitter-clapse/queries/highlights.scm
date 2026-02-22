(comment) @comment

(identifier) @variable
(capitalized_identifier) @constructor
(integer) @constant.numeric.integer
(string) @string

(function_declaration
  name: (identifier) @function)

(function_declaration
  argument: (identifier) @variable.parameter)

(guarded_clause
  condition: (guard_condition_text) @variable.parameter)

(function_signature
  name: (identifier) @function
  signature: (signature_text) @type)

(data_declaration
  type_name: (capitalized_identifier) @type)

(data_declaration
  type_parameter: (identifier) @type)

(data_declaration
  constructor_type: (type_expr_text) @type)

(data_declaration
  constructor_name: (capitalized_identifier) @constructor)

(data_declaration
  field_name: (identifier) @variable.parameter)

(class_declaration
  name: (identifier) @type)

(class_declaration
  kind: (class_kind) @type.builtin)

(law_declaration
  class_name: (identifier) @type)

(law_declaration
  name: (identifier) @property)

(instance_declaration
  name: (identifier) @type)

(instance_declaration
  class_name: (identifier) @type)

(instance_binding
  method_name: (identifier) @property)

(instance_binding
  target_name: (identifier) @function)

(module_declaration
  name: (module_name) @namespace)

(import_declaration
  module: (module_name) @namespace)

(export_declaration
  name: (identifier) @function)

(export_declaration
  name: (capitalized_identifier) @constructor)

(operator_declaration
  assoc: (operator_assoc) @keyword)

(operator_declaration
  precedence: (integer) @constant.numeric.integer)

(operator_declaration
  target: (identifier) @function)

(operator_declaration
  operator: (operator_token
    (identifier) @operator))

(operator_declaration
  operator: (operator_token
    (operator_symbol) @operator))

(operator_symbol) @operator
(backtick_operator) @operator

(lambda_expression
  parameter: (identifier) @variable.parameter)

(let_value_binding
  name: (identifier) @variable)

(let_function_binding
  name: (identifier) @function)

(let_function_binding
  argument: (identifier) @variable.parameter)

(let_pattern_binding
  constructor: (capitalized_identifier) @constructor)

(let_pattern_binding
  field: (identifier) @variable.parameter)

(list_expression
  "[" @punctuation.bracket
  "]" @punctuation.bracket)

(application_expression
  function: (identifier) @function.call)

(application_expression
  function: (capitalized_identifier) @constructor)

(infix_expression
  operator: (infix_operator) @operator)

(list_expression
  "," @punctuation.bracket)

((identifier) @function.builtin
  (#match? @function.builtin "^(add|sub|mul|div|append|empty|fmap|pure|bind|ap|compose|id|slice_len|slice_get_u8|slice_set_u8|collection_empty|collection_extend)$"))

[
  "class"
  "law"
  "instance"
  "data"
  "infix"
  "infixl"
  "infixr"
  "module"
  "import"
  "export"
  "case"
  "of"
  "let"
] @keyword

(in_keyword) @keyword

[
  "\\"
  "|"
  "->"
  "="
  ":"
] @operator

[
  "("
  ")"
] @punctuation.bracket

(case_pattern_atom
  (integer) @constant.numeric.integer)

(case_pattern_atom
  (capitalized_identifier) @constructor)

(case_pattern_atom
  (identifier) @variable.parameter)

(wildcard) @variable.builtin
