const PREC = {
  LAMBDA: 1,
  INFIX: 2,
  APPLICATION: 3,
};

module.exports = grammar({
  name: "clapse",

  extras: ($) => [$.comment],

  word: ($) => $.identifier,

  conflicts: ($) => [
    [$.instance_declaration, $.instance_binding],
  ],

  rules: {
    source_file: ($) =>
      repeat(
        choice(
          $.declaration,
          $._newline,
          $._ws1,
        ),
      ),

    declaration: ($) =>
      choice(
        $.module_declaration,
        $.import_declaration,
        $.export_declaration,
        $.type_declaration,
        $.class_declaration,
        $.law_declaration,
        $.instance_declaration,
        $.operator_declaration,
        $.data_declaration,
        $.attributed_function_declaration,
        $.function_declaration,
        $.function_signature,
      ),

    attributed_function_declaration: ($) =>
      seq(
        repeat1(
          seq(
            field("attribute", $.function_attribute),
            choice($._newline, seq($._ws1, $._newline)),
          ),
        ),
        $.function_declaration,
      ),

    function_attribute: ($) =>
      seq(
        "#",
        "[",
        field("name", $.identifier),
        optional(
          seq(
            $._ws1,
            field("value", choice($.integer, $.string, $.identifier)),
          ),
        ),
        "]",
      ),

    operator_declaration: ($) =>
      seq(
        field("assoc", $.operator_assoc),
        $._ws1,
        field("precedence", $.integer),
        $._ws1,
        field("operator", $.operator_token),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("target", $.identifier),
      ),

    function_declaration: ($) =>
      prec.right(
        seq(
          field("name", $.identifier),
          repeat(seq($._ws1, field("argument", $.binder_name))),
          choice(
            seq(
              optional($._ws1),
              "=",
              optional(choice($._ws1, seq($._newline, optional($._ws1)))),
              field("body", $.expression),
            ),
            seq(
              $._ws1,
              field("guard", $.guarded_clause),
              repeat(
                choice(
                  seq(
                    optional($._ws1),
                    ";",
                    optional($._ws1),
                    field("guard", $.guarded_clause),
                  ),
                  seq(
                    $._newline_indent,
                    field("guard", $.guarded_clause),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),

    guarded_clause: ($) =>
      seq(
        "|",
        optional($._ws1),
        field("condition", $.expression),
        alias($._guard_eq_sep, $.guard_equals),
        field("result", $.expression),
      ),

    function_signature: ($) =>
      prec.right(
        2,
        seq(
          field("name", $.identifier),
          optional($._ws1),
          ":",
          optional($._ws1),
          field("signature", $.signature_text),
        ),
      ),

    signature_text: () => token(/[^\n]+/),

    data_declaration: ($) =>
      prec.right(
        seq(
          $._kw_data,
          $._ws1,
          field("type_name", choice($.capitalized_identifier, $.identifier)),
          repeat(seq($._ws1, field("type_parameter", $.identifier))),
          $._ws1,
          "=",
          $._ws1,
          choice(
            prec.dynamic(
              4,
              seq(
                field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                optional($._ws1),
                ":",
                optional($._ws1),
                field("constructor_type", $.type_expr_text),
              ),
            ),
            prec.dynamic(
              3,
              seq(
                field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                optional($._ws1),
                "<",
                optional($._ws1),
                field("literal_backing", $.type_union_literal),
                optional($._ws1),
                ">",
              ),
            ),
            prec.dynamic(
              2,
              seq(
                field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                repeat1(seq($._ws1, field("field_name", $.identifier))),
              ),
            ),
            prec.dynamic(
              -1,
              seq(
                field("constructor_name", choice($.capitalized_identifier, $.identifier)),
              ),
            ),
          ),
          repeat(
            prec.right(
              seq(
                optional($._ws1),
              "|",
              optional($._ws1),
              choice(
                prec.dynamic(
                  4,
                  seq(
                    field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                    optional($._ws1),
                    ":",
                    optional($._ws1),
                    field("constructor_type", $.type_expr_text),
                  ),
                ),
                prec.dynamic(
                  3,
                  seq(
                    field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                    optional($._ws1),
                    "<",
                    optional($._ws1),
                    field("literal_backing", $.type_union_literal),
                    optional($._ws1),
                    ">",
                  ),
                ),
                prec.dynamic(
                  2,
                  seq(
                    field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                    repeat1(seq($._ws1, field("field_name", $.identifier))),
                  ),
                ),
                prec.dynamic(
                  -1,
                  seq(
                    field("constructor_name", choice($.capitalized_identifier, $.identifier)),
                  ),
                ),
              ),
            ),
          ),
          ),
        ),
      ),

    type_declaration: ($) =>
      seq(
        $._kw_type,
        $._ws1,
        field("type_name", $.capitalized_identifier),
        $._ws1,
        "=",
        $._ws1,
        $.type_union,
      ),

    type_union: ($) =>
      seq(
        "<",
        optional($._ws1),
        field("member", $.type_union_member),
        repeat(
          seq(
            optional($._ws1),
            "|",
            optional($._ws1),
            field("member", $.type_union_member),
          ),
        ),
        optional($._ws1),
        ">",
      ),

    type_union_member: ($) =>
        choice(
        $.type_union_literal,
        seq(
          field("constructor_name", $.identifier),
          optional($._ws1),
          "<",
          optional($._ws1),
          field("literal", $.type_union_literal),
          optional($._ws1),
          ">",
        ),
      ),

    type_union_literal: ($) =>
      choice(
        $.integer,
        $.string,
      ),

    module_declaration: ($) =>
      seq($._kw_module, $._ws1, field("name", $.module_name)),

    import_declaration: ($) =>
      seq($._kw_import, $._ws1, field("module", $.module_name)),

    export_declaration: ($) =>
      seq(
        $._kw_export,
        $._ws1,
        field("name", $._export_name),
        repeat(
          seq(
            ",",
            optional($._ws1),
            field("name", $._export_name),
          ),
        ),
      ),

    class_declaration: ($) =>
      prec.right(
        seq(
          $._kw_class,
          $._ws1,
          field("name", choice($.identifier, $.capitalized_identifier)),
          repeat(seq($._ws1, field("type_parameter", $.identifier))),
          choice(
            seq(
              $._ws1,
              ":",
              $._ws1,
              field("kind", $.class_kind),
            ),
            seq(
              $._ws1,
              $._kw_where,
              choice(
                seq(
                  $._ws1,
                  field("method", $.class_method_entry),
                  repeat(
                    seq(
                      $._class_method_separator,
                      field("method", $.class_method_entry),
                    ),
                  ),
                ),
                seq(
                  $._newline_indent,
                  field("method", $.class_method_entry),
                  repeat(
                    seq(
                      $._newline_indent,
                      field("method", $.class_method_entry),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),

    class_method_entry: ($) => choice($.class_method_signature, $.class_method_binding),

    class_method_signature: ($) =>
      seq(
        field("method_name", choice($.identifier, $.operator_symbol)),
        optional($._ws1),
        ":",
        optional($._ws1),
        field("signature", $.signature_text),
      ),

    class_method_binding: ($) =>
      seq(
        field("method_name", choice($.identifier, $.operator_symbol)),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("target_name", $.identifier),
      ),

    instance_declaration: ($) =>
      prec.right(
        seq(
          $._kw_instance,
          $._ws1,
          field("name", choice($.identifier, $.capitalized_identifier)),
          $._ws1,
          ":",
          $._ws1,
          field("class_name", choice($.identifier, $.capitalized_identifier)),
          repeat(
            seq(
              $._ws1,
              field("class_type_argument", choice($.identifier, $.capitalized_identifier)),
            ),
          ),
          choice(
            repeat1(seq($._ws1, field("binding", $.instance_binding))),
            seq(
              $._ws1,
              $._kw_where,
              choice(
                seq(
                  $._ws1,
                  field("binding", $.instance_binding),
                  repeat(
                    seq(
                      $._instance_binding_separator,
                      field("binding", $.instance_binding),
                    ),
                  ),
                ),
                seq(
                  $._newline_indent,
                  field("binding", $.instance_binding),
                  repeat(
                    seq(
                      $._newline_indent,
                      field("binding", $.instance_binding),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),

    instance_binding: ($) =>
      seq(
        field("method_name", choice($.identifier, $.operator_symbol)),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("target_name", $.identifier),
      ),

    _class_method_separator: ($) =>
      choice(
        seq(
          optional($._ws1),
          ";",
          optional(choice($._ws1, seq($._newline, optional($._ws1)))),
        ),
        $._newline_indent,
      ),

    _instance_binding_separator: ($) =>
      choice(
        seq(
          optional($._ws1),
          ";",
          optional(choice($._ws1, seq($._newline, optional($._ws1)))),
        ),
        $._newline_indent,
      ),

    law_declaration: ($) =>
      seq(
        $._kw_law,
        $._ws1,
        field("class_name", choice($.identifier, $.capitalized_identifier)),
        $._ws1,
        field("name", $.identifier),
        $._ws1,
        "=",
        optional($._ws1),
        field("lhs", $.expression),
        $._law_arrow,
        optional($._ws1),
        field("rhs", $.expression),
      ),

    expression: ($) =>
      choice(
        $.lambda_expression,
        $.let_expression,
        $.infix_expression,
        $.application_expression,
        $.case_expression,
        $._atom,
      ),

    let_expression: ($) =>
      prec.right(
        4,
        seq(
          $._kw_let,
          $._ws1,
          field("binding", $.let_binding),
          repeat(
            seq(
              $._let_binding_separator,
              field("binding", $.let_binding),
            ),
          ),
          alias($._let_in_sep, $.in_keyword),
          field("body", $.expression),
        ),
      ),

    _let_binding_separator: ($) =>
      choice(
        seq(
          optional($._ws1),
          ";",
          optional(choice($._ws1, seq($._newline, optional($._ws1)))),
        ),
        $._newline_indent,
      ),

    let_binding: ($) =>
      choice(
        $.let_value_binding,
        $.let_function_binding,
        $.let_pattern_binding,
      ),

    let_value_binding: ($) =>
      seq(
        field("name", $.identifier),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("value", $.expression),
      ),

    let_function_binding: ($) =>
      seq(
        field("name", $.identifier),
        repeat1(seq($._ws1, field("argument", $.binder_name))),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("value", $.expression),
      ),

    let_pattern_binding: ($) =>
      seq(
        field("constructor", $.capitalized_identifier),
        repeat1(seq($._ws1, field("field", $.identifier))),
        optional($._ws1),
        "=",
        optional($._ws1),
        field("value", $.expression),
      ),

    case_expression: ($) =>
      prec.right(
        4,
        seq(
          $._kw_case,
          $._ws1,
          field("scrutinee", $.case_scrutinee_atom),
          repeat(seq($._ws1, field("scrutinee", $.case_scrutinee_atom))),
          $._ws1,
          $._kw_of,
          choice(
            seq(
              $._ws1,
              field("branch", $.case_branch),
              optional($.case_branch_tail_inline),
            ),
            seq(
              $._newline_indent,
              field("branch", $.case_branch),
              optional($.case_branch_tail_multiline),
            ),
          ),
        ),
      ),

    case_branch_tail_inline: ($) =>
      prec.right(
        seq(
          optional($._ws1),
          ";",
          optional($._ws1),
          field("branch", $.case_branch),
          optional($.case_branch_tail_inline),
        ),
      ),

    case_branch_tail_multiline: ($) =>
      prec.right(
        seq(
          $._newline_indent,
          field("branch", $.case_branch),
          optional($.case_branch_tail_multiline),
        ),
      ),

    case_scrutinee_atom: ($) =>
      choice(
        $.capitalized_identifier,
        $.identifier,
        $.integer,
        $.string,
        $.parenthesized_expression,
        $.list_expression,
      ),

    case_branch: ($) =>
      seq(
        field("pattern", $.case_pattern_atom),
        $._ws1,
        repeat(seq(field("pattern", $.case_pattern_atom), $._ws1)),
        "->",
        optional(choice($._ws1, $._newline_indent)),
        field("result", $.expression),
      ),

    case_pattern_atom: ($) =>
      choice(
        $.capitalized_identifier,
        $.identifier,
        $.wildcard,
        $.integer,
        $.string,
      ),

    infix_expression: ($) =>
      prec.left(
        PREC.INFIX,
        seq(
          field("left", choice($.infix_expression, $.application_expression, $._atom)),
          $._ws1,
          field("operator", $.infix_operator),
          $._ws1,
          field("right", $._atom),
        ),
      ),

    lambda_expression: ($) =>
      prec.right(
        PREC.LAMBDA,
        seq(
          "\\",
          optional($._ws1),
          field("parameter", $.binder_name),
          repeat(seq($._ws1, field("parameter", $.binder_name))),
          optional($._ws1),
          "->",
          optional($._ws1),
          field("body", $.expression),
        ),
      ),

    binder_name: ($) => choice($.identifier, $.wildcard),

    application_expression: ($) =>
      prec.left(
        PREC.APPLICATION,
        seq(
          field("function", choice($.application_expression, $.infix_expression, $._atom)),
          $._ws1,
          field("argument", $._atom),
        ),
      ),

    _atom: ($) =>
      choice(
        $.capitalized_identifier,
        $.identifier,
        $.integer,
        $.string,
        $.parenthesized_expression,
        $.list_expression,
      ),

    type_expression: ($) =>
      prec.right(
        seq(
          field("from", $.type_application),
          optional(
            seq(
              $._ws1,
              "->",
              optional($._ws1),
              field("to", $.type_expression),
            ),
          ),
        ),
      ),

    signature_type: ($) =>
      choice(
        seq(field("constraints", $.constraints), optional($._ws1), "=>", optional($._ws1), field("type", $.type_expression)),
        $.type_expression,
      ),

    type_expr_text: () => token(/[^\n|]+/),

    type_application: ($) =>
      prec.left(
        1,
        seq(
          field("type", $.type_atom),
          repeat(seq($._ws1, field("type", $.type_atom))),
        ),
      ),

    function_type: ($) =>
      prec.right(
        1,
        seq(
          field("from", $.type_application),
          $._ws1,
          "->",
          optional($._ws1),
          field("to", $.signature_type),
        ),
      ),

    type_atom: ($) =>
      prec(
        1,
        choice($.capitalized_identifier, $.identifier, $.parenthesized_type, $.list_type),
      ),

    parenthesized_type: ($) =>
      seq("(", optional($._ws1), $.type_expression, optional($._ws1), ")"),

    list_type: ($) =>
      choice(
        seq(
          "[",
          "]",
        ),
        seq(
          "[",
          optional($._ws1),
          field("type", $.type_expression),
          repeat(
            seq(
              optional($._ws1),
              ",",
              optional($._ws1),
              field("type", $.type_expression),
            ),
          ),
          optional($._ws1),
          "]",
        ),
      ),

    constraints: ($) =>
      prec.left(
        1,
        seq(
          $.type_constraint,
          repeat(
            seq(
              optional($._ws1),
              ",",
              optional($._ws1),
              $.type_constraint,
            ),
          ),
        ),
      ),

    type_constraint: ($) =>
      choice(
        $.named_type_constraint,
        $.constraint_expr,
      ),

    named_type_constraint: ($) =>
      seq(
        "(",
        optional($._ws1),
        field("witness", $.identifier),
        optional($._ws1),
        ":",
        optional($._ws1),
        field("constraint", $.constraint_expr),
        optional($._ws1),
        ")",
      ),

    constraint_expr: ($) =>
      prec.left(
        2,
        seq(
          $.identifier,
          repeat1(seq($._ws1, $.identifier)),
        ),
      ),

    list_expression: ($) =>
      choice(
        seq(
          "[",
          optional($._ws1),
          "]",
        ),
        seq(
          "[",
          optional($._ws1),
          field("element", $.expression),
          repeat(
            seq(
              optional($._ws1),
              ",",
              optional($._ws1),
              field("element", $.expression),
            ),
          ),
          optional($._ws1),
          "]",
        ),
      ),

    parenthesized_expression: ($) =>
      seq("(", optional($._ws1), $.expression, optional($._ws1), ")"),

    operator_assoc: ($) => choice($._kw_infixl, $._kw_infixr, $._kw_infix),

    operator_token: ($) => choice($.identifier, $.operator_symbol),

    infix_operator: ($) => choice($.operator_symbol, $.backtick_operator),

    operator_symbol: () =>
      token(
        prec(
          -1,
          choice(
            /[!#$%&*+./<>?@\\^|~:][!#$%&*+./<=>?@\\^|~:-]*/,
            /=[!#$%&*+./<=?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*/,
            /-[!#$%&*+./<=?@\\^|~:-][!#$%&*+./<=>?@\\^|~:-]*/,
            /-/,
          ),
        ),
      ),

    backtick_operator: () => token(seq("`", /[a-z_][a-z0-9_']*/, "`")),

    capitalized_identifier: () => /[A-Z][A-Za-z0-9_']*/,
    module_name: () => /[A-Za-z][A-Za-z0-9_']*(\.[A-Za-z][A-Za-z0-9_']*)*/,

    _export_name: ($) => choice($.identifier, $.capitalized_identifier),

    class_kind: () =>
      choice("add", "sub", "mul", "div", "monoid", "functor", "applicative", "monad"),

    _ws1: () => /[ \t\r\f]+/,
    _law_arrow: () => token(seq(/[ \t\r\f]+/, "=>")),

    _newline: () => /\n/,
    _newline_indent: () => token(seq(/\n/, /[ \t\r\f]+/)),

    _kw_data: () => token(prec(1, "data")),
    _kw_class: () => token(prec(1, "class")),
    _kw_law: () => token(prec(1, "law")),
    _kw_instance: () => token(prec(1, "instance")),
    _kw_where: () => token(prec(1, "where")),
    _kw_module: () => token(prec(1, "module")),
    _kw_import: () => token(prec(1, "import")),
    _kw_export: () => token(prec(1, "export")),
    _kw_type: () => token(prec(1, "type")),
    _kw_let: () => token(prec(1, "let")),
    _kw_in: () => token(prec(1, "in")),
    _let_in_sep: () =>
      token(
        prec(
          2,
          seq(
            choice(
              /[ \t\r\f]+/,
              seq(/\n/, /[ \t\r\f]+/),
            ),
            "in",
            choice(
              /[ \t\r\f]+/,
              seq(/\n/, /[ \t\r\f]*/),
            ),
          ),
        ),
      ),
    _guard_eq_sep: () =>
      token(
        seq(
          /[ \t\r\f]*/,
          "=",
          choice(
            /[ \t\r\f]+/,
            seq(/\n/, /[ \t\r\f]*/),
          ),
        ),
      ),
    _kw_case: () => token(prec(1, "case")),
    _kw_of: () => token(prec(1, "of")),
    _kw_infixl: () => token(prec(1, "infixl")),
    _kw_infixr: () => token(prec(1, "infixr")),
    _kw_infix: () => token(prec(1, "infix")),

    wildcard: () => token(prec(1, "_")),
    identifier: () =>
      token(
        prec(
          -2,
          choice(
            /[a-z][a-z0-9_']*/,
            /_[a-z0-9_'][a-z0-9_']*/,
          ),
        ),
      ),
    integer: () => /[0-9]+/,

    string: () =>
      token(seq('"', repeat(choice(/[^"\\\n]/, /\\["\\ntr]/)), '"')),

    comment: () => token(seq("--", /.*/)),
  },
});
