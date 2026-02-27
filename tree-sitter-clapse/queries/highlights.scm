(comment) @comment

(string) @string
(integer) @constant.numeric.integer

(module_declaration
  name: (module_name) @namespace)

(import_declaration
  module: (module_name) @namespace)

(export_declaration
  name: (identifier) @function)

(export_declaration
  name: (capitalized_identifier) @constructor)

(function_declaration
  name: (identifier) @function)

(function_declaration
  argument: (binder_name
    (identifier) @variable.parameter))

(function_declaration
  argument: (binder_name
    (wildcard) @variable.parameter))

(function_signature
  name: (identifier) @function
  signature: (signature_text) @type)

(attributed_function_declaration
  (function_attribute
    name: (identifier) @attribute))

(function_attribute
  "#" @punctuation.bracket
  "[" @punctuation.bracket
  "]" @punctuation.bracket)

(function_attribute
  value: (integer) @constant.numeric.integer)

(function_attribute
  value: (string) @string)

(function_attribute
  value: (identifier) @variable)

(type_declaration
  type_name: (capitalized_identifier) @type.definition)

(type_declaration
  type_parameter: (identifier) @type.parameter)

(type_declaration
  (type_alias_rhs
    (type_union) @type))

(type_declaration
  (type_alias_rhs
    (type_record) @type))

(type_record
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

(type_field
  name: (identifier) @property)

(type_field
  type: (type_expression) @type)

(data_declaration
  type_name: (capitalized_identifier) @type.definition)

(data_declaration
  type_parameter: (identifier) @type.parameter)

(data_declaration
  constructor_name: (capitalized_identifier) @constructor)

(data_declaration
  constructor_type: (type_expr_text) @type)

(data_declaration
  field_name: (identifier) @variable.parameter)

(primitive_declaration
  type_name: (identifier) @type.definition)

(primitive_declaration
  type_parameter: (identifier) @type.parameter)

(primitive_declaration
  constructor_name: (identifier) @constant.builtin)

(primitive_declaration
  primitive_backing: (primitive_backing_text) @constant.numeric.integer
  (#match? @constant.numeric.integer "^[ \t]*-?[0-9]+[ \t]*$"))

(primitive_declaration
  primitive_backing: (primitive_backing_text) @string
  (#match? @string "^[ \t]*\".*\"[ \t]*$"))

(primitive_declaration
  primitive_backing: (primitive_backing_text) @type
  (#not-match? @type "^[ \t]*-?[0-9]+[ \t]*$")
  (#not-match? @type "^[ \t]*\".*\"[ \t]*$"))

(class_declaration
  name: (identifier) @type.definition)

(class_declaration
  name: (capitalized_identifier) @type.definition)

(class_declaration
  kind: (class_kind) @type.builtin)

(class_method_signature
  method_name: (identifier) @function.method @function)

(class_method_signature
  method_name: (operator_symbol) @function.method @function)

(class_method_signature
  signature: (signature_text) @type)

(class_method_binding
  method_name: (identifier) @function.method @function)

(class_method_binding
  method_name: (operator_symbol) @function.method @function)

(class_method_binding
  argument: (binder_name
    (identifier) @variable.parameter))

(class_method_binding
  argument: (binder_name
    (wildcard) @variable.parameter))

(class_method_binding
  target_name: (identifier) @function)

(law_declaration
  class_name: (identifier) @type)

(law_declaration
  class_name: (capitalized_identifier) @type)

(law_declaration
  name: (identifier) @property)

(instance_declaration
  name: (identifier) @type)

(instance_declaration
  name: (capitalized_identifier) @type)

(instance_declaration
  class_name: (identifier) @type)

(instance_declaration
  class_name: (capitalized_identifier) @type)

(instance_declaration
  name: (identifier) @type)

(instance_declaration
  name: (capitalized_identifier) @type)

(instance_binding
  method_name: (identifier) @function.method @function)

(instance_binding
  method_name: (operator_symbol) @function.method @function)

(instance_binding
  argument: (binder_name
    (identifier) @variable.parameter))

(instance_binding
  argument: (binder_name
    (wildcard) @variable.parameter))

(instance_binding
  target_name: (identifier) @function)

(operator_declaration
  assoc: (operator_assoc) @keyword)

(operator_declaration
  precedence: (integer) @constant.numeric.integer)

(operator_declaration
  operator: (operator_token
    (identifier) @keyword.operator))

(operator_declaration
  operator: (operator_token
    (operator_symbol) @keyword.operator))

(application_expression
  function: (identifier) @function)

(application_expression
  function: (capitalized_identifier) @constructor)

(infix_expression
  operator: (infix_operator) @keyword.operator)

(lambda_expression
  parameter: (binder_name
    (identifier) @variable.parameter))

(lambda_expression
  parameter: (binder_name
    (wildcard) @variable.parameter))

(let_value_binding
  name: (identifier) @variable)

(let_function_binding
  name: (identifier) @function)

(let_function_binding
  argument: (binder_name
    (identifier) @variable.parameter))

(let_function_binding
  argument: (binder_name
    (wildcard) @variable.parameter))

(let_pattern_binding
  constructor: (capitalized_identifier) @constructor)

(let_pattern_binding
  field: (identifier) @variable.parameter)

(case_pattern_atom
  (integer) @constant.numeric.integer)

(case_pattern_atom
  (string) @string)

(case_pattern_atom
  (capitalized_identifier) @constructor)

(case_pattern_atom
  (identifier) @variable.parameter)

(case_pattern_atom
  (identifier) @constant.builtin.boolean
  (#match? @constant.builtin.boolean "^(true|false)$"))

(case_pattern_atom
  (wildcard) @variable.parameter)

(wildcard) @variable.parameter

(backtick_operator) @keyword.operator
(operator_symbol) @keyword.operator
(guard_equals) @keyword.operator

(list_expression
  "[" @punctuation.bracket
  "]" @punctuation.bracket)

(record_expression
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

(record_field
  name: (identifier) @property)

(record_update_fields
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

(record_update_field
  name: (identifier) @property)

(field_projection_expression
  "." @punctuation.bracket)

(field_projection_expression
  field: (identifier) @property)

[
  "("
  ")"
] @punctuation.bracket

[
  "\\"
  "|"
  "->"
  "."
  "="
  ":"
] @keyword.operator

[
  "module"
  "import"
  "export"
  "class"
  "law"
  "instance"
  "where"
  "data"
  "primitive"
  "type"
  "infix"
  "infixl"
  "infixr"
  "case"
  "of"
  "let"
] @keyword

(in_keyword) @keyword

((identifier) @constant.builtin.boolean
  (#match? @constant.builtin.boolean "^(true|false)$"))

((identifier) @function.builtin
  (#match? @function.builtin "^(add|sub|mul|div|slice_len|slice_get_u8|slice_set_u8|collection_empty|collection_extend|slice_eq_u8|str_eq)$"))

((identifier) @keyword
  (#eq? @keyword "otherwise"))
