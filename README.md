# tree-sitter-compact

A [tree-sitter](https://tree-sitter.github.io/) grammar for the [Compact programming language](https://docs.midnight.network/develop/reference/compact/lang-ref), a zero-knowledge smart contract language for the [Midnight](https://midnight.network/) blockchain.

Based on the [formal grammar](https://docs.midnight.network/develop/reference/compact/compact-grammar). Targets **Compact v0.30.0**.

## Bindings

| Language | Path |
|----------|------|
| C | `bindings/c/` |
| Go | `bindings/go/` |
| Node.js | `bindings/node/` |
| Python | `bindings/python/` |
| Rust | `bindings/rust/` |
| Swift | `bindings/swift/` |

## Installation

```bash
# Node.js
npm install tree-sitter-compact

# Rust
cargo add tree-sitter-compact
```

## Development

Requires [tree-sitter-cli](https://tree-sitter.github.io/tree-sitter/creating-parsers/tool-overview.html).

```bash
npm install              # install dependencies
npm run generate         # regenerate parser from grammar.js
npm test                 # run corpus tests
npm start                # open interactive playground
```

### Rust

```bash
cargo test               # run Rust binding tests
```

## Project structure

```
grammar.js               # grammar definition (source of truth)
queries/highlights.scm   # syntax highlighting queries
src/parser.c             # generated parser (do not edit)
src/grammar.json         # generated grammar JSON (do not edit)
src/node-types.json      # generated node types (do not edit)
test/corpus/             # tree-sitter corpus tests
bindings/                # language bindings
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### CLA Assistant

The Midnight Foundation appreciates contributions, and like many other open source projects asks contributors to sign a Contributor License Agreement before accepting contributions. We use [CLA assistant](https://github.com/cla-assistant/cla-assistant) to streamline the CLA signing process, enabling contributors to sign our CLAs directly within a GitHub pull request.

## License

[Apache-2.0](LICENSE)
