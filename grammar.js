// This file is part of compact-tree-sitter.
// Copyright (C) 2025 Midnight Foundation
// SPDX-License-Identifier: Apache-2.0
// Licensed under the Apache License, Version 2.0 (the "License");
// You may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
    [$.pattern, $.array_literal],
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
    pragma: ($) => seq(
      "pragma",
      field("id", $.id),
      $._version_expr,
      ";"
    ),

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
        field("version_expression", seq($._version_op, $._version_atom)),
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
    incld: ($) => seq("include", field("file", $.file), ";"),

    // Module-definition (mdefn)
    //
    // mdefn → export^opt module module-name gparams^opt { pelt … pelt }
    mdefn: ($) =>
      seq(
        optional(field("export", $.export)),
        "module",
        field("name", $.module_name),
        optional(field("gparams", $.gparams)),
        "{",
        repeat(field("module_element", $._pelt)),
        "}",
      ),

    // Generic-parameter-list (gparams)
    //
    // gparams → < generic-param , … , generic-param >
    gparams: ($) => seq("<", commaSep1(field("gparam", $.generic_param)), ">"),

    // Generic-parameter (generic-param)
    //
    // generic-param → # tvar-name
    //               → tvar-name
    generic_param: ($) => choice(seq("#", $.tvar_name), $.tvar_name),

    // Import-declaration (idecl)
    //
    // idecl → import import-selection^opt import-name gargs^opt import-prefix^opt ;
    idecl: ($) =>
      seq(
        "import",
        optional(field("selection", $.import_selection)),
        field("id", $.import_name),
        optional(field("gargs", $.gargs)),
        optional(field("prefix", $.prefix)),
        ";"
      ),

    // Import-selection (import-selection)
    //
    // import-selection → { import-element , ... , import-element ,? } from
    import_selection: ($) => seq(
      "{",
      commaSep1(field("element", $.import_element)),
      "}",
      "from"
    ),

    // Import-element (import-element)
    //
    // import-element → id
    //                → id as id
    import_element: ($) => choice(
      field("id", $.id),
      seq(field("id", $.id), "as", field("alias", $.id))
    ),

    // Import-name (import-name)
    //
    // import-name → id
    //             → file
    import_name: ($) => choice($.id, $.file),

    // Generic-argument-list (gargs)
    //
    // gargs → < garg , … , garg >
    gargs: ($) => seq("<", commaSep1(field("garg", $.garg)), ">"),

    // Import-prefix (prefix)
    //
    // prefix → prefix id
    prefix: ($) => seq("prefix", field("id", $.id)),

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
    xdecl: ($) => seq("export", "{", commaSep1(field("id", $.id)), "}", optional(";")),

    // Ledger-declaration (ldecl)
    //
    // ldecl → export^opt sealed^opt ledger id : type ;
    ldecl: ($) =>
      seq(
        optional(field("export", $.export)),
        optional(field("sealed", $.sealed)),
        "ledger",
        field("name", $.id),
        ":",
        field("type", $.type),
        ";",
      ),

    // Constructor (lconstructor)
    //
    // lconstructor → constructor ( parg , … , parg ) block ;opt
    lconstructor: ($) =>
      seq("constructor", "(", commaSep(field("parg", $.parg)), ")", field("body", $.block), optional(";")),

    // Circuit-definition (cdefn)
    //
    // cdefn → export^opt pure^opt circuit function-name gparams^opt ( parg , … , parg ) : type block
    cdefn: ($) =>
      seq(
        optional(field("export", $.export)),
        optional(field("pure", $.pure)),
        "circuit",
        field("id", $.function_name),
        optional(field("gparams", $.gparams)),
        "(",
        commaSep(field("parg", $.parg)),
        ")",
        ":",
        field("type", $.type),
        field("body", $.block),
      ),

    // External-declaration (edecl)
    //
    // edecl → export^opt circuit id gparams^opt ( arg , … , arg ) : type ;
    edecl: ($) =>
      seq(
        optional(field("export", $.export)),
        "circuit",
        field("id", $.function_name),
        optional(field("gparams", $.gparams)),
        "(",
        field("args", commaSep(field("arg", $.arg))),
        ")",
        ":",
        field("type", $.type),
        ";",
      ),

    // Witness-declaration (wdecl)
    //
    // wdecl → export^opt witness id gparams^opt ( arg , … , arg ) : type ;
    wdecl: ($) =>
      seq(
        optional(field("export", $.export)),
        "witness",
        field("id", $.function_name),
        optional(field("gparams", $.gparams)),
        "(",
        field("args", commaSep(field("arg", $.arg))),
        ")",
        ":",
        field("type", $.type),
        ";",
      ),

    // External-contract-declaration (ecdecl)
    //
    // ecdecl	→ export^opt contract contract-name { ecdecl-circuit … ecdecl-circuit } ;^opt
    ecdecl: ($) =>
      seq(
        optional(field("export", $.export)),
        "contract",
        field("name", $.contract_name),
        "{",
        repeat(field("contract_circuit", $.ecdecl_circuit)),
        "}",
        optional(";"),
      ),

    // External-contract-circuit (ecdecl-circuit)
    //
    // ecdecl-circuit →	 pure^opt circuit id ( arg , … , arg ) : type ;
    ecdecl_circuit: ($) =>
      seq(
        optional(field("pure", $.pure)),
        "circuit",
        field("id", $.id),
        "(",
        commaSep(field("arg", $.arg)),
        ")",
        ":",
        field("type", $.type),
        ";",
      ),

    // Structure-definition (struct)
    //
    // struct →	export^opt struct struct-name gparams^opt { arg ; … ; arg ;^opt } ;^opt
    //        →	export^opt struct struct-name gparams^opt { arg , … , arg  ,^opt } ;^opt
    struct: ($) =>
      seq(
        optional(field("export", $.export)),
        "struct",
        field("name", $.struct_name),
        optional(field("gparams", $.gparams)),
        choice($._struct_body_semicolon, $._struct_body_comma),
        optional(";"),
      ),

    // { arg ; … ; arg ;^opt }
    _struct_body_semicolon: ($) =>
      seq("{", field("arg", $.arg), repeat(seq(";", field("arg", $.arg))), optional(";"), "}"),

    // { arg , … , arg ,^opt }
    _struct_body_comma: ($) =>
      prec(1, seq("{", commaSep1(field("arg", $.arg)), "}")),

    // Enum-definition (enumdef)
    //
    // enumdef → export^opt enum enum-name { id , …¹ , id ,^opt } ;^opt
    enumdef: ($) =>
      seq(
        optional(field("export", $.export)),
        "enum",
        field("name", $.enum_name),
        "{",
        commaSep1(field("id", $.id)),
        optional(","),
        "}",
        optional(";"),
      ),

    // Argument (arg)
    //
    // arg → id : type
    arg: ($) => seq(field("id", $.id), ":", field("type", $.type)),

    // Pattern-argument (parg)
    //
    // parg → pattern : type
    parg: ($) => seq(field("pattern", $.pattern), ":", field("type", $.type)),

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
        $.uint_type,
        $.bytes_type,
        $.opaque_type,
        $.vector_type,
        seq("[", commaSep($.type), "]"),
      ),
    
    uint_type: ($) => seq("Uint", "<", field("tsize", $.tsize), optional(seq("..", field("tsize", $.tsize))), ">"),
    bytes_type : ($) => seq("Bytes", "<", field("tsize", $.tsize), ">"),
    opaque_type: ($) => seq("Opaque", "<", $.str, ">"),
    vector_type: ($) => seq("Vector", "<", field("tsize", $.tsize), ",", field("type", $.type), ">"),

    // Type-reference (tref)
    //
    // tref → id gargs^opt
    tref: ($) => prec.left(seq(field("id", $.id), optional(field("gargs", $.gargs)))),

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
    block: ($) => seq("{", repeat(field("stmt", $.stmt)), "}"),

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
          $.assign_stmt,
          $.expression_sequence_stmt,
          $.return_stmt,
          $.if_stmt,
          $.for_stmt,
          $.assert_stmt,
          $.const_stmt,
          $.block,
        ),
      ),
    
      assert_stmt: ($) => seq("assert", "(", field("condition", $.expr), ",", field("message", $.str), ")", ";"),
      return_stmt: ($) => seq("return", optional(field("value", $.expr_seq)), ";"),
      if_stmt: ($) => prec.right(seq("if", "(", field("condition", $.expr_seq), ")", field("then_branch", $.stmt), optional(seq("else", field("else_branch", $.stmt))))),
      for_stmt: ($) => seq("for", "(", "const", field("counter", $.id), "of", 
          choice(
            seq(field("range_start", $.nat), "..", field("range_end", $.nat)),
            field("limit", $.expr_seq),
          ),
          ")", field("body", $.stmt)),
      expression_sequence_stmt: ($) => seq($.expr_seq, ";"),
      assign_stmt: ($) => seq(field("target", $.expr), field("operator", choice("=", "-=", "+=")), field("value", $.expr), ";"),
      const_stmt: ($) => seq("const", field("pattern", $.pattern), optional(seq(":", field("type", $.type))), "=", field("value", $.expr), ";"),

    // Pattern (pattern)
    //
    // pattern → id
    //        → [ pattern-tuple-elt , … , pattern-tuple-elt ]
    //        → { pattern-struct-elt , … , pattern-struct-elt }
    pattern: ($) =>
      choice(
        field("id", $.id),
        field("pattern_tuple", seq("[", optional(commaSep(field("pattern_tuple_elt", $.pattern_tuple_elt))), "]")),
        field("pattern_struct", seq("{", commaSep1(field("pattern_struct_elt", $.pattern_struct_elt)), "}")),
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
    pattern_struct_elt: ($) => choice(field("id", $.id), seq(field("id", $.id), ":", field("pattern", $.pattern))),

    // Expression-sequence (expr-seq)
    //
    // expr-seq → expr
    //         → expr , …¹ , expr , expr
    expr_seq: ($) =>
      choice(field("expr", $.expr), seq(field("expr", $.expr), repeat1(seq(",", field("expr", $.expr))), ",", field("expr", $.expr))),

    // Expression (expr)
    //
    // expr → expr0 ? expr : expr
    //      → expr0
    conditional_expr: ($) =>
      seq(
        field("condition", $._expr0),
        "?",
        field("then_branch", $.expr),
        ":",
        field("else_branch", $.expr)
      ),

    expr: ($) => choice($.conditional_expr, $._expr0),

    // Expression0 (expr0)
    //
    // expr0 → expr0 || expr1
    //       → expr1
    or_expr: ($) => seq(field("left", $._expr0), $.or, field("right", $._expr1)),

    _expr0: ($) => prec.left(choice($.or_expr, $._expr1)),

    // Expression1 (expr1)
    //
    // expr1 → expr1 && expr2
    //       → expr2
    and_expr: ($) => seq(field("left", $._expr1), $.and, field("right", $._expr2)),
    _expr1: ($) => prec.left(choice($.and_expr, $._expr2)),

    // Expression2 (expr2)
    //
    // expr2 → expr2 == expr3
    //       → expr2 != expr3
    //       → expr3
    comparison_expr : ($) => seq(field("left", $._expr2), field("operator", choice($.equals, $.not_equals)), field("right", $._expr3)),
    _expr2: ($) =>
      prec.left(
        choice(
          $.comparison_expr,
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
    rel_comparison_expr: ($) => seq(field("left", $._expr3), field("operator", choice($.less_than, $.less_than_or_equal, $.greater_than_or_equal, $.greater_than)), field("right", $._expr4)),
    _expr3: ($) =>
      prec.left(
        choice(
          $.rel_comparison_expr,
          $._expr4,
        ),
      ),

    // Expression4 (expr4)
    //
    // expr4 → expr4 as type
    //       → expr5
    cast_expr : ($) => seq(field("expr", $._expr4), "as", field("type", $.type)),
    _expr4: ($) => prec.left(4, choice($.cast_expr, $._expr5)),

    // Expression5 (expr5)
    //
    // expr5 → expr5 + expr6
    //       → expr5 - expr6
    //       → expr6
    bin_sum_expr : ($) => seq(field("left", $._expr5), field("operator", choice("+", "-")), field("right", $._expr6)),
    _expr5: ($) =>
      prec.left(
        choice(
          $.bin_sum_expr,
          $._expr6,
        ),
      ),

    // Expression6 (expr6)
    //
    // expr6 → expr6 * expr7
    //       → expr7
    bin_mul_expr : ($) => prec.left(seq(field("left", $._expr6), field("operator", "*"), field("right", $._expr7))),
    _expr6: ($) => choice($.bin_mul_expr, $._expr7),

    // Expression7 (expr7)
    //
    // expr7 → ! expr7
    //       → expr8
    not_expr: ($) => seq($.not, field("expr", $._expr7)),
  
    _expr7: ($) => choice($.not_expr, $._expr8),

    // Expression8 (expr8)
    //
    // expr8 → expr8 [ expr ]
    //       → expr8 . id
    //       → expr8 . id ( expr , … , expr )
    //       → term
    index_access_expr: ($) => prec.left(seq(field("base", $._expr8), "[", field("index", $.expr), "]")),
    member_access_expr: ($) => prec.left(seq(field("base", $._expr8), ".", field("member", $.id), optional(field("arguments", seq("(", commaSep(field("expr", $.expr)), ")"))))),
    _expr8: ($) =>
      prec.left(
        choice(
          $.index_access_expr,
          $.member_access_expr,
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
    //      → Bytes [ expr , … , expr ,opt ]
    //      → id
    //      → ( expr-seq )
    term: ($) =>
      choice(
        $.lit, // Literal (true, false, nat, str, pad)
        $.default_term, // Default value with type
        $.map_term, // Map function with one or more expressions
        $.fold_term, // Fold with initial value and list
        $.function_call_term, // Function call with zero or more expressions
        $.disclose_term, // Disclose with single expression
        $.struct_term, // Struct literal with type reference
        $.array_literal, // Array literal with optional trailing comma
        $.bytes_literal, // Bytes literal
        $.slice_term, // Slice expression
        $.id, // Identifier
        $.expr_seq_term, // Parenthesized expression sequence
      ),

      default_term: ($) => seq("default", "<", field("type", $.type), ">"),
      map_term: ($) => seq("map", "(", field("fun", $.fun), ",", commaSep1(field("expr", $.expr)), ")"),
      fold_term: ($) => seq("fold", "(", field("fun", $.fun), ",", field("init_value", $.expr), ",", commaSep1(field("expr", $.expr)), ")"),
      struct_term: ($) => seq(field("tref", $.tref), "{", commaSep($.struct_arg), "}"),
      function_call_term: ($) => seq(field("fun", $.fun), "(", commaSep(field("expr", $.expr)), ")"),
      disclose_term : ($) => seq("disclose", "(", field("expr", $.expr), ")"),
      array_literal: ($) => seq("[", commaSep(field("element", choice($.expr, $.spread_element))), "]"),
      bytes_literal: ($) => seq("Bytes", "[", commaSep(field("element", choice($.expr, $.spread_element))), "]"),
      spread_element: ($) => seq("...", field("expr", $.expr)),
      slice_term: ($) => seq("slice", "<", field("size", $.tsize), ">", "(", field("value", $.expr), ",", field("index", $.expr), ")"),
      expr_seq_term: ($) => seq("(", $.expr_seq, ")"),

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
        $.pad,
      ),

      pad : ($) => seq("pad", "(", field("nat", $.nat), ",", field("str", $.str), ")"),

    // Structure-argument (struct-arg)
    //
    // struct-arg → expr
    //            → id : expr
    //            → ... expr
    struct_arg: ($) =>
      choice(
        $.expr,
        $.struct_named_filed_initializer,
        $.struct_update_field
    ),

    struct_named_filed_initializer : ($) => seq(field("id", $.id), ":", field("expr", $.expr)),
    struct_update_field : ($) => seq("...", field("expr", $.expr)),

    // Function (fun)
    //
    // fun → id gargs^opt
    //     → ( pattern-or-parg , … , pattern-or-parg ) return-type-decl^opt => block
    //     → ( pattern-or-parg , … , pattern-or-parg ) return-type-decl^opt => expr
    //     → ( fun )
    fun: ($) =>
      choice(
        seq(field("id", $.id), optional(field("gargs", $.gargs))),
        seq(
          "(",
          commaSep($._pattern_or_parg),
          ")",
          optional(field("return", $._return_type_decl)),
          "=>",
          choice(field("block", $.block), field("expr", $.expr)),
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
    _pattern_or_parg: ($) => choice(field("pattern", $.pattern), field("parg", $.parg)),

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
    // Also supports hexadecimal (0x/0X), octal (0o/0O), and binary (0b/0B) literals
    nat: ($) => choice(
      "0",
      /[1-9][0-9]*/,        // decimal
      /0[xX][0-9a-fA-F]+/,  // hexadecimal
      /0[oO][0-7]+/,        // octal
      /0[bB][01]+/          // binary
    ),

    // string-literal (str, file)
    //
    // The basic string-literal rule that matches TypeScript strings
    str: ($) => token(choice(/"[^"]*"/, /'[^']*'/)),
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
