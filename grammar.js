/**
 * @file A zero-knowledge smart contract programming language
 * @author Lucas Rosa <lucas.rosa@shielded.io>
 * @license Apache
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "compact",

  conflicts: ($) => [
    [$.pattern, $.arg],
    [$.fun, $.term, $.tref],
    [$.fun, $.pattern, $.term],
    [$.pattern, $.term],
    [$.fun, $.term],
    [$.fun, $.pattern],
  ],

  extras: ($) => [
    /[\s\n]+/,
    $.comment, // Add comments as extras
  ],

  rules: {
    // Compact (program)
    //
    // program → pelt … pelt eof
    source_file: ($) => repeat($._pelt),

    // Comment rule
    comment: ($) => token(seq("//", /[^\n]*/)),

    // Program-element (pelt)
    //
    // pelt → pragma
    //  	  →	incld
    //  	  →	mdefn
    //  	  →	idecl
    //  	  →	xdecl
    //  	  →	ldecl
    //  	  →	lconstructor
    //  	  →	cdefn
    //  	  →	edecl
    //  	  →	wdecl
    //  	  →	ecdecl
    //  	  →	struct
    //  	  →	enumdef
    _pelt: ($) =>
      choice(
        $.pragma,
        $.incld,
        $.mdefn,
        $.idecl,
        $.xdecl,
        $.ldecl,
        $.lconstructor,
        $.cdefn,
        $.edecl,
        $.wdecl,
        $.ecdecl,
        $.struct,
        $.enumdef,
      ),

    // Pragma rules
    //
    // pragma →	pragma id version-expr ;
    pragma: ($) => seq("pragma", $.id, $._version_expr, ";"),

    // Version-expression (version-expr)
    //
    // version-expr →	version-expr || version-expr0
    //  	          →	version-expr0
    _version_expr: ($) =>
      prec.left(
        1,
        choice(seq($._version_expr, $.or, $._version_expr0), $._version_expr0),
      ),

    // Version-expression0 (version-expr0)
    //
    // version-expr0 → version-expr0 && version-term
    //  	           → version-term
    _version_expr0: ($) =>
      prec.left(
        2,
        choice(seq($._version_expr0, $.and, $._version_term), $._version_term),
      ),

    // Version-Term (version-term)
    //
    // version-term →	version-atom
    //  	          →	! version-term
    //  	          →	< version-atom
    //  	          →	<= version-atom
    //  	          →	>= version-atom
    //  	          →	> version-atom
    //  	          →	( version-expr )
    _version_term: ($) =>
      choice(
        $._version_atom,
        seq($._version_op, $._version_atom),
        seq("(", $._version_expr, ")"),
      ),

    // Version-atom (version-atom)
    //
    // version-atom →	nat
    //  	          →	version
    _version_atom: ($) => choice($.nat, $.version),

    _version_op: ($) =>
      choice(
        $.not,
        $.greater_than,
        $.less_than,
        $.greater_than_or_equal,
        $.less_than_or_equal,
      ),

    // Include (incld)
    //
    // incld → include file ;
    incld: ($) => seq("include", $.file, ";"),

    // Module-definition (mdefn)
    //
    // mdefn → export^opt module module-name gparams^opt { pelt … pelt }
    mdefn: ($) =>
      seq(
        optional($.export),
        "module",
        $.module_name,
        optional($.gparams),
        "{",
        repeat($._pelt),
        "}",
      ),

    // Generic-parameter-list (gparams)
    //
    // gparams → < generic-param , … , generic-param >
    gparams: ($) => seq("<", commaSep1($.generic_param), ">"),

    // Generic-parameter (generic-param)
    //
    // generic-param → # tvar-name
    //               → tvar-name
    generic_param: ($) => choice(seq("#", $.tvar_name), $.tvar_name),

    // Import-declaration (idecl)
    //
    // idecl → import import-name gargs^opt prefix^opt ;
    idecl: ($) =>
      seq("import", $.import_name, optional($.gargs), optional($.prefix), ";"),

    // Import-name (import-name)
    //
    // import-name → id
    //             → file
    import_name: ($) => choice($.id, $.file),

    // Generic-argument-list (gargs)
    //
    // gargs → < garg , … , garg >
    gargs: ($) => seq("<", commaSep1($.garg), ">"),

    // Import-prefix (prefix)
    //
    // prefix → prefix id
    prefix: ($) => seq("prefix", $.id),

    // Export-modifier (export)
    //
    // export → export
    export: ($) => "export",

    // Sealed-modifier (sealed)
    //
    // sealed → sealed
    sealed: ($) => "sealed",

    // Pure-modifier (pure)
    //
    // pure → pure
    pure: ($) => "pure",

    // Export-declaration (xdecl)
    //
    // xdecl → export { id , … , id } ;^opt
    xdecl: ($) => seq("export", "{", commaSep1($.id), "}", optional(";")),

    // Ledger-declaration (ldecl)
    //
    // ldecl → export^opt sealed^opt ledger id : type ;
    ldecl: ($) =>
      seq(
        optional($.export),
        optional($.sealed),
        "ledger",
        $.id,
        ":",
        $.type,
        ";",
      ),

    // Constructor (lconstructor)
    //
    // lconstructor → constructor ( parg , … , parg ) block ;opt
    lconstructor: ($) =>
      seq("constructor", "(", commaSep($.parg), ")", $.block, optional(";")),

    // Circuit-definition (cdefn)
    //
    // cdefn → export^opt pure^opt circuit function-name gparams^opt ( parg , … , parg ) : type block
    cdefn: ($) =>
      seq(
        optional($.export),
        optional($.pure),
        "circuit",
        $.function_name,
        optional($.gparams),
        "(",
        commaSep($.parg),
        ")",
        ":",
        $.type,
        $.block,
      ),

    // External-declaration (edecl)
    //
    // edecl → export^opt circuit id gparams^opt ( arg , … , arg ) : type ;
    edecl: ($) =>
      seq(
        optional($.export),
        "circuit",
        $.function_name,
        optional($.gparams),
        "(",
        commaSep($.arg),
        ")",
        ":",
        $.type,
        ";",
      ),

    // Witness-declaration (wdecl)
    //
    // wdecl → export^opt witness id gparams^opt ( arg , … , arg ) : type ;
    wdecl: ($) =>
      seq(
        optional($.export),
        "witness",
        $.function_name,
        optional($.gparams),
        "(",
        commaSep($.arg),
        ")",
        ":",
        $.type,
        ";",
      ),

    // External-contract-declaration (ecdecl)
    //
    // ecdecl	→ export^opt contract contract-name { ecdecl-circuit … ecdecl-circuit } ;^opt
    ecdecl: ($) =>
      seq(
        optional($.export),
        "contract",
        $.contract_name,
        "{",
        repeat($.ecdecl_circuit),
        "}",
        optional(";"),
      ),

    // External-contract-circuit (ecdecl-circuit)
    //
    // ecdecl-circuit →	 pure^opt circuit id ( arg , … , arg ) : type ;
    ecdecl_circuit: ($) =>
      seq(
        optional($.pure),
        "circuit",
        $.id,
        "(",
        commaSep($.arg),
        ")",
        ":",
        $.type,
        ";",
      ),

    // Structure-definition (struct)
    //
    // struct →	export^opt struct struct-name gparams^opt { arg ; … ; arg ;^opt } ;^opt
    //        →	export^opt struct struct-name gparams^opt { arg , … , arg  ,^opt } ;^opt
    struct: ($) =>
      seq(
        optional($.export),
        "struct",
        $.struct_name,
        optional($.gparams),
        choice($._struct_body_semicolon, $._struct_body_comma),
        optional(";"),
      ),

    // { arg ; … ; arg ;^opt }
    _struct_body_semicolon: ($) =>
      seq("{", repeat(seq($.arg, ";")), optional(";"), "}"),

    // { arg , … , arg ,^opt }
    _struct_body_comma: ($) =>
      prec(1, seq("{", repeat(seq($.arg, ",")), optional(","), "}")),

    // Enum-definition (enumdef)
    //
    // enumdef → export^opt enum enum-name { id , …¹ , id ,^opt } ;^opt
    enumdef: ($) =>
      seq(
        optional($.export),
        "enum",
        $.enum_name,
        "{",
        commaSep1($.id),
        optional(","),
        "}",
        optional(";"),
      ),

    // Argument (arg)
    //
    // arg → id : type
    arg: ($) => seq($.id, ":", $.type),

    // Pattern-argument (parg)
    //
    // parg → pattern : type
    parg: ($) => seq($.pattern, ":", $.type),

    // Type (type)
    //
    // type →	tref
    //      →	Boolean
    //      →	Field
    //      →	Uint < tsize >
    //      →	Uint < tsize .. tsize >
    //      →	Bytes < tsize >
    //      →	Opaque < str >
    //      →	Vector < tsize , type >
    //      →	[ type , … , type ]
    type: ($) =>
      choice(
        $.tref,
        "Boolean",
        "Field",
        seq("Uint", "<", $.tsize, ">"),
        seq("Uint", "<", $.tsize, "..", $.tsize, ">"),
        seq("Bytes", "<", $.tsize, ">"),
        seq("Opaque", "<", $.str, ">"),
        seq("Vector", "<", $.tsize, ",", $.type, ">"),
        seq("[", commaSep($.type), "]"),
      ),

    // Type-reference (tref)
    //
    // tref → id gargs^opt
    tref: ($) => prec.left(seq($.id, optional($.gargs))),

    // Type-size (tsize)
    //
    // tsize → nat
    //       → id
    tsize: ($) => choice($.nat, $.id),

    // Generic-argument (garg)
    //
    // garg → nat
    //      → type
    garg: ($) => choice($.nat, $.type),

    // Block (block)
    //
    // block → { stmt … stmt }
    block: ($) => seq("{", repeat($.stmt), "}"),

    // Statement (stmt)
    //
    // stmt → expr = expr ;
    //      → expr += expr ;
    //      → expr -= expr ;
    //      → expr-seq ;
    //      → return expr-seq ;
    //      → return ;
    //      → if ( expr-seq ) stmt else stmt
    //      → if ( expr-seq ) stmt
    //      → for ( const id of nat .. nat ) stmt
    //      → for ( const id of expr-seq ) stmt
    //      → assert expr str ;
    //      → const pattern = expr ;
    //      → const pattern : type = expr ;
    //      → block
    stmt: ($) =>
      prec.right(
        choice(
          seq($.expr, "=", $.expr, ";"),
          seq($.expr, "+=", $.expr, ";"),
          seq($.expr, "-=", $.expr, ";"),
          seq($.expr_seq, ";"),
          seq("return", $.expr_seq, ";"),
          seq("return", ";"),
          seq("if", "(", $.expr_seq, ")", $.stmt, "else", $.stmt),
          seq("if", "(", $.expr_seq, ")", $.stmt),
          seq("for", "(", "const", $.id, "of", $.nat, "..", $.nat, ")", $.stmt),
          seq("for", "(", "const", $.id, "of", $.expr_seq, ")", $.stmt),
          seq("assert", $.expr, optional($.str), ";"),
          seq("const", $.pattern, "=", $.expr, ";"),
          seq("const", $.pattern, ":", $.type, "=", $.expr, ";"),
          $.block,
        ),
      ),

    // Pattern (pattern)
    //
    // pattern → id
    //        → [ pattern-tuple-elt , … , pattern-tuple-elt ]
    //        → { pattern-struct-elt , … , pattern-struct-elt }
    pattern: ($) =>
      choice(
        $.id,
        seq("[", optional(commaSep($.pattern_tuple_elt)), "]"),
        seq("{", commaSep1($.pattern_struct_elt), "}"),
      ),

    // Pattern-tuple-element (pattern-tuple-elt)
    //
    // pattern-tuple-elt → (empty)
    //                   → pattern
    pattern_tuple_elt: ($) => $.pattern,

    // Pattern-struct-element (pattern-struct-elt)
    //
    // pattern-struct-elt → id
    //                    → id : pattern
    pattern_struct_elt: ($) => choice($.id, seq($.id, ":", $.pattern)),

    // Expression-sequence (expr-seq)
    //
    // expr-seq → expr
    //         → expr , …¹ , expr , expr
    expr_seq: ($) =>
      choice($.expr, seq($.expr, repeat1(seq(",", $.expr)), ",", $.expr)),

    // Expression (expr)
    //
    // expr → expr0 ? expr : expr
    //      → expr0
    expr: ($) => choice(seq($._expr0, "?", $.expr, ":", $.expr), $._expr0),

    // Expression0 (expr0)
    //
    // expr0 → expr0 || expr1
    //       → expr1
    _expr0: ($) => prec.left(choice(seq($._expr0, $.or, $._expr1), $._expr1)),

    // Expression1 (expr1)
    //
    // expr1 → expr1 && expr2
    //       → expr2
    _expr1: ($) => prec.left(choice(seq($._expr1, $.and, $._expr2), $._expr2)),

    // Expression2 (expr2)
    //
    // expr2 → expr2 == expr3
    //       → expr2 != expr3
    //       → expr3
    _expr2: ($) =>
      prec.left(
        choice(
          seq($._expr2, $.equals, $._expr3),
          seq($._expr2, $.not_equals, $._expr3),
          $._expr3,
        ),
      ),

    // Expression3 (expr3)
    //
    // expr3 → expr4 < expr4
    //       → expr4 <= expr4
    //       → expr4 >= expr4
    //       → expr4 > expr4
    //       → expr4
    _expr3: ($) =>
      prec.left(
        choice(
          seq($._expr4, $.less_than, $._expr4),
          seq($._expr4, $.less_than_or_equal, $._expr4),
          seq($._expr4, $.greater_than_or_equal, $._expr4),
          seq($._expr4, $.greater_than, $._expr4),
          $._expr4,
        ),
      ),

    // Expression4 (expr4)
    //
    // expr4 → expr4 as type
    //       → expr5
    _expr4: ($) => prec.left(4, choice(seq($._expr4, "as", $.type), $._expr5)),

    // Expression5 (expr5)
    //
    // expr5 → expr5 + expr6
    //       → expr5 - expr6
    //       → expr6
    _expr5: ($) =>
      prec.left(
        choice(
          seq($._expr5, "+", $._expr6),
          seq($._expr5, "-", $._expr6),
          $._expr6,
        ),
      ),

    // Expression6 (expr6)
    //
    // expr6 → expr6 * expr7
    //       → expr7
    _expr6: ($) => prec.left(choice(seq($._expr6, "*", $._expr7), $._expr7)),

    // Expression7 (expr7)
    //
    // expr7 → ! expr7
    //       → expr8
    _expr7: ($) => choice(seq($.not, $._expr7), $._expr8),

    // Expression8 (expr8)
    //
    // expr8 → expr8 [ nat ]
    //       → expr8 . id
    //       → expr8 . id ( expr , … , expr )
    //       → term
    _expr8: ($) =>
      prec.left(
        choice(
          seq($._expr8, "[", $.nat, "]"),
          seq($._expr8, ".", $.id),
          seq($._expr8, ".", $.id, "(", commaSep1($.expr), ")"),
          $.term,
        ),
      ),

    // Term (term)
    //
    // term → lit
    //      → default < type >
    //      → map ( fun , expr , …¹ , expr )
    //      → fold ( fun , expr , expr , …¹ , expr )
    //      → fun ( expr , … , expr )
    //      → disclose ( expr )
    //      → tref { struct-arg , … , struct-arg }
    //      → [ expr , … , expr ,opt ]
    //      → id
    //      → ( expr-seq )
    term: ($) =>
      choice(
        $.lit, // Literal (true, false, nat, str, pad)
        seq("default", "<", $.type, ">"), // Default value with type
        seq("map", "(", $.fun, ",", commaSep1($.expr), ")"), // Map function with one or more expressions
        seq("fold", "(", $.fun, ",", $.expr, ",", commaSep1($.expr), ")"), // Fold with initial value and list
        seq($.fun, "(", commaSep($.expr), ")"), // Function call with zero or more expressions
        seq("disclose", "(", $.expr, ")"), // Disclose with single expression
        seq($.tref, "{", commaSep($.struct_arg), "}"), // Struct literal with type reference
        seq("[", commaSep($.expr), "]"), // Array literal with optional trailing comma
        $.id, // Identifier
        seq("(", $.expr_seq, ")"), // Parenthesized expression sequence
      ),

    // Literal (lit)
    //
    // lit → true
    //     → false
    //     → nat
    //     → str
    //     → pad ( nat , str )
    lit: ($) =>
      choice(
        "true",
        "false",
        $.nat,
        $.str,
        seq("pad", "(", $.nat, ",", $.str, ")"),
      ),

    // Structure-argument (struct-arg)
    //
    // struct-arg → expr
    //            → id : expr
    //            → ... expr
    struct_arg: ($) =>
      choice($.expr, seq($.id, ":", $.expr), seq("...", $.expr)),

    // Function (fun)
    //
    // fun → id gargs^opt
    //     → ( pattern-or-parg , … , pattern-or-parg ) return-type-decl^opt => block
    //     → ( pattern-or-parg , … , pattern-or-parg ) return-type-decl^opt => expr
    //     → ( fun )
    fun: ($) =>
      choice(
        seq($.id, optional($.gargs)),
        seq(
          "(",
          commaSep($._pattern_or_parg),
          ")",
          optional($._return_type_decl),
          "=>",
          choice($.block, $.expr),
        ),
        seq("(", $.fun, ")"),
      ),

    // Return-type-declaration (return-type-decl)
    //
    // return-type-decl → : type
    _return_type_decl: ($) => seq(":", $.type),

    // Pattern-or-pattern-argument (pattern-or-parg)
    //
    // pattern-or-parg → pattern
    //                 → parg
    _pattern_or_parg: ($) => choice($.pattern, $.parg),

    // OPS
    equals: ($) => "==",
    not_equals: ($) => "!=",
    greater_than: ($) => ">",
    less_than: ($) => "<",
    greater_than_or_equal: ($) => ">=",
    less_than_or_equal: ($) => "<=",
    not: ($) => "!",
    and: ($) => "&&",
    or: ($) => "||",

    // LITERALS

    // identifier (id, module-name, function-name, struct-name, enum-name, contract-name, tvar-name)
    //
    // identifiers have the same syntax as Typescript identifiers
    id: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    module_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    function_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    struct_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    enum_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    contract_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,
    tvar_name: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // field-literal (nat)
    //
    // a field literal is 0 or a natural number formed from a sequence
    // of digits starting with 1-9, e.g. 723, whose value does not exceed
    // the maximum field value
    nat: ($) => choice("0", /[1-9][0-9]*/),

    // string-literal (str, file)
    //
    // The basic string-literal rule that matches TypeScript strings
    str: ($) => token(/"[^"]*"/),
    file: ($) => token(/"[^"]*"/),

    // version-literal (version)
    //
    // a version literal takes the form nat or nat.nat or nat.nat.nat,
    // e.g., 1.2 or 1.2.3, representing major, minor, and bugfix versions
    version: ($) => /[0-9]+(\.[0-9]+){0,2}/,
  },
});

/**
 * Creates a rule for one or more comma-separated occurrences of another rule
 * @param {Rule} rule - The rule to be repeated
 * @returns {SeqRule} A rule that matches one or more occurrences of the input rule separated by commas
 */
function commaSep1(rule) {
  return seq(
    rule,
    repeat(seq(",", rule)),
    optional(","), // Optional trailing comma
  );
}

/**
 * Creates a rule for zero or more comma-separated occurrences of another rule
 * @param {Rule} rule - The rule to be repeated
 * @returns {ChoiceRule} A rule that matches one or more occurrences of the input rule separated by commas
 */
function commaSep(rule) {
  return optional(seq(rule, repeat(seq(",", rule)), optional(",")));
}
