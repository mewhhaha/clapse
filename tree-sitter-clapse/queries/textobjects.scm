(function_declaration
  body: (expression) @function.inside) @function.around

(lambda_expression
  body: (expression) @function.inside) @function.around

(law_declaration
  lhs: (expression) @function.inside) @function.around

(class_declaration) @class.around

(instance_declaration) @class.around

(operator_declaration) @class.around

(function_declaration
  argument: (binder_name
    (identifier) @parameter.inside @parameter.around))

(function_declaration
  argument: (binder_name
    (wildcard) @parameter.inside @parameter.around))

(lambda_expression
  parameter: (binder_name
    (identifier) @parameter.inside @parameter.around))

(lambda_expression
  parameter: (binder_name
    (wildcard) @parameter.inside @parameter.around))

(instance_binding
  method_name: (identifier) @parameter.inside @parameter.around)

(comment) @comment.inside
(comment)+ @comment.around
