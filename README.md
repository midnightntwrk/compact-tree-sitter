# tree-sitter-compact

A [tree-sitter](https://tree-sitter.github.io/) grammar for the [Compact programming language](https://docs.midnight.network/develop/reference/compact/lang-ref), a zero-knowledge smart contract language for the [Midnight](https://midnight.network/) blockchain.

Based on the [formal grammar](https://docs.midnight.network/develop/reference/compact/compact-grammar). Targets **Compact v0.30.0**.

## Editor integration

### Neovim

Add the parser to your [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.compact = {
  install_info = {
    url = "https://github.com/midnightntwrk/compact-tree-sitter",
    files = { "src/parser.c" },
    branch = "main",
  },
  filetype = "compact",
}
```

Then run `:TSInstall compact`.

Copy `queries/highlights.scm` to your Neovim runtime queries directory for syntax highlighting.

### Helix / Zed

Both editors support tree-sitter grammars natively. Point the grammar source to this repository in the editor's `languages.toml` or equivalent config.

## Usage

### Node.js

```javascript
const Parser = require("tree-sitter");
const Compact = require("tree-sitter-compact");

const parser = new Parser();
parser.setLanguage(Compact);

const tree = parser.parse('ledger balance: Field;');
console.log(tree.rootNode.toString());
// (source_file (ldecl (id) (type)))
```

### Rust

```rust
use tree_sitter::Parser;

let mut parser = Parser::new();
parser
    .set_language(&tree_sitter_compact::LANGUAGE.into())
    .expect("failed to load Compact grammar");

let tree = parser
    .parse("ledger balance: Field;", None)
    .expect("failed to parse");
println!("{}", tree.root_node().to_sexp());
```

## Installation

```bash
# Node.js
npm install @midnight-ntwrk/compact-tree-sitter

# Rust (add to Cargo.toml)
# tree-sitter-compact = { git = "https://github.com/midnightntwrk/compact-tree-sitter" }
```

Bindings are also available for C, Go, Python, and Swift under `bindings/`.

## Development

Requires [tree-sitter-cli](https://tree-sitter.github.io/tree-sitter/creating-parsers/tool-overview.html).

```bash
npm install              # install dependencies
npm run generate         # regenerate parser from grammar.js
npm test                 # run corpus tests (159 tests)
npm run parse -- file.compact  # parse a file and print the AST
npm start                # open interactive playground
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
bindings/                # language bindings (C, Go, Node, Python, Rust, Swift)
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### CLA Assistant

The Midnight Foundation appreciates contributions, and like many other open source projects asks contributors to sign a Contributor License Agreement before accepting contributions. We use [CLA assistant](https://github.com/cla-assistant/cla-assistant) to streamline the CLA signing process, enabling contributors to sign our CLAs directly within a GitHub pull request.

## License

[Apache-2.0](LICENSE)
