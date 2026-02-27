(function_declaration
  name: (identifier) @name) @definition.function

(function_signature
  name: (identifier) @name) @definition.function

(class_declaration
  name: (identifier) @name) @definition.type

(class_method_signature
  method_name: (identifier) @name) @definition.function

(class_method_signature
  method_name: (operator_symbol) @name) @definition.function

(class_method_binding
  method_name: (identifier) @name) @definition.function

(class_method_binding
  method_name: (operator_symbol) @name) @definition.function

(data_declaration
  type_name: (capitalized_identifier) @name) @definition.type

(data_declaration
  type_parameter: (identifier) @name) @reference.type

(data_declaration
  constructor_name: (capitalized_identifier) @name) @definition.class

(primitive_declaration
  type_name: (identifier) @name) @definition.type

(primitive_declaration
  type_parameter: (identifier) @name) @reference.type

(primitive_declaration
  constructor_name: (identifier) @name) @definition.constant

(law_declaration
  name: (identifier) @name) @definition.constant

(instance_declaration
  name: (identifier) @name) @definition.type

(instance_declaration
  class_name: (identifier) @name) @reference.type

(instance_declaration
  class_name: (capitalized_identifier) @name) @reference.type

(instance_binding
  target_name: (identifier) @name) @reference.call

(application_expression
  function: (identifier) @name) @reference.call

(case_pattern_atom
  (identifier) @name) @reference.call
