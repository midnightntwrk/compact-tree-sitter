# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Block comments (`/* ... */`)
- `$`-prefixed identifiers matching the Compact lexer spec
- Single-quoted strings for `Opaque<'string'>` syntax
- String escape sequences (`\"`, `\'`, `\\`, `\n`, `\t`, `\r`, `\0`, `\uXXXX`, `\xXX`)
- Hex (`0xFF`), octal (`0o77`), binary (`0b1010`) number literals
- Assignments as expressions (right-associative `assign_expr`)
- Selective imports (`import { foo, bar as baz } from Module;`)
- Type aliases (`type U32 = Uint<32>;`, `new type Amount = Uint<64>;`)
- `nominal` field on `type_alias_declaration` for `new` keyword
- Bytes literals (`Bytes[0xFF, 0x00]`) with spread (`Bytes[...a, ...b]`)
- Array literal as named node (`array_literal`) with spread support
- `slice_term` built-in expression
- `assert_term` expression (function-call syntax)
- Multi-binding `const` with `cbinding` rule
- External contract body supports both `;` and `,` separators
- For-loop range bounds accept `tsize` (identifiers for generic params)
- Index access accepts expressions (not just `nat`)
- 159 corpus tests covering all language features
- Syntax highlighting queries for all new keywords and constructs

### Changed

- `struct_named_filed_initializer` renamed to `struct_named_field_initializer` (typo fix)
- Struct body semicolons now use delimiter style (trailing semicolon optional)
- `wdecl` argument wrapping aligned with other declaration rules
- `<`/`>` highlight captures use named nodes to avoid bracket conflict
- Member access highlighted as `@property` instead of `@function.call`

### Removed

- `edecl` (external circuit declarations, deprecated in Compact 0.28)
- `assign_stmt` (replaced by `assign_expr` inside `expression_sequence_stmt`)
- `assert_stmt` (replaced by `assert_term`)
