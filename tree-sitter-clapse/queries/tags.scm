(function_declaration
  name: (identifier) @name) @definition.function

(function_signature
  name: (identifier) @name) @definition.function

(class_declaration
  name: (identifier) @name) @definition.type

(data_declaration
  type_name: (capitalized_identifier) @name) @definition.type

(data_declaration
  type_parameter: (identifier) @name) @reference.type

(data_declaration
  constructor_name: (capitalized_identifier) @name) @definition.class

(law_declaration
  name: (identifier) @name) @definition.constant

(instance_declaration
  name: (identifier) @name) @definition.type

(instance_declaration
  class_name: (identifier) @name) @reference.type

(instance_binding
  target_name: (identifier) @name) @reference.call

(application_expression
  function: (identifier) @name) @reference.call

(operator_declaration
  target: (identifier) @name) @reference.call

(case_pattern_atom
  (identifier) @name) @reference.call
