# RizzoMemory

MSc thesis project implementing **Rizzo**, a programming language based on the work of [Rizzo by Patrick Bahr](https://bahr.io/pubs/entries/rizzo.html), with a focus on memory management and safety. The project includes:

- A compiler that translates Rizzo source code to C, leveraging reference counting for memory management.
- A C runtime library that provides memory management primitives, safety checks and the Rizzo advance and update semantics.
- A VS Code extension that integrates the Rizzo compiler and language server for an enhanced development experience with diagnostics, hover information, and code navigation (Rough implementation; not the focus of the MSc thesis).

## Prerequisites

- **OCaml** (4.14+) and **opam** — for building the compiler
- **Dune** — OCaml build system (usually installed via opam)
- **GCC** or **Clang** — for compiling generated C code
- **Node.js** and **npm** — for the VS Code extension (optional)

## Development

- Build: `opam exec -- dune build`
- Run compiler on one or more source files: `opam exec -- dune exec rizzoc ./examples/first.rizz`

By default the compiler is quiet and only emits `output.c`. To inspect the intermediate ASTs while compiling, pass `--print-ast` to `rizzoc`.

## Full flow: compile and run a `.rizz` file

The full pipeline compiles one or more Rizzo source files to C and then runs them:

```bash
# 1. Compile .rizz → output.c
opam exec -- dune exec rizzoc <file.rizz> [more-files.rizz ...]

# 2. Compile output.c with the C runtime headers
gcc -I./src/runtime output.c -o output

# 3. Run
./output
```

Or use the convenience npm script from the repo root (after step 1):

```bash
npm run rizzo
```

> **Windows**: The extension automatically handles Windows-specific details (`.exe` extension, `-m64` flag). Both GCC and Clang are supported.

### Implicit stdlib

File-based compilation and editor analysis now prepend a compiler-owned stdlib before user files are typechecked. The default stdlib lives in [src/stdlib](src/stdlib) in the repository and installs next to the compiler under `lib/rizzoc/stdlib`. That means user `.rizz` files can use combinators such as `map`, `zip`, `scan`, and `filter_map` without copy-pasting their definitions first.

You can replace the default stdlib with `--overwrite-stdpath <path>`, where `<path>` can point to either a single `.rizz` file or a stdlib directory. You can also prepend extra files or directories with repeated `-I <path>` flags.

Multiple input files are compiled left-to-right after that implicit stdlib and any `-I` includes. Later files can refer to earlier files, but the reverse order is not supported yet.

### Running from VS Code

Open any `.rizz` file and click the **▶ Run** button in the editor title bar, or run the command **Rizzo: Run Current File** from the Command Palette.

The extension will:
1. Save the file
2. Compile it with `rizzoc` (auto-detected from local build or `opam exec -- dune exec rizzoc --`)
3. Compile the generated `output.c` with `gcc` (or the C compiler set in `rizzoLsp.compiler.cc`)
4. Run the resulting binary – all in an integrated terminal

**Configuration** (`settings.json`):

| Setting | Default | Description |
|---|---|---|
| `rizzoLsp.compiler.command` | *(auto)* | Path to `rizzoc` binary. Leave empty to auto-detect. |
| `rizzoLsp.compiler.cc` | `gcc` | C compiler command (`gcc` or `clang`). |

## LSP and VS Code extension

To run the LSP server, use the command:

- LSP server: `opam exec -- dune exec rizzolsp`

### VS Code extension development

The VS Code extension is located in the `vscode-rizzo-lsp` folder.
It can easily be installed with

```bash
cd vscode-rizzo-lsp && npm install && cd ..
npm run ext:install
```
This installs the dependencies, builds the project and then installs the extension locally.

> To compile the bundle with no VSIX: `npm run compile`

### Debugging the extension

For development you might want to run the extension in a debug session, which allows you to set breakpoints and inspect variables in the LSP server code. To do this, you can run the debug configuration `Run Rizzo Extension (repo root)` from the repository root. This will launch a new VS Code window with the extension loaded. You can then open any `.rizz` file to see diagnostics, symbols, definitions, hover information, and completion suggestions provided by the LSP server.

- Run debug config: `Run Rizzo Extension (repo root)`.
- Then open any `.rizz` file to get diagnostics, symbols, definition, hover, and completions (`Ctrl+Space` and typing-triggered suggestions).

### Notes

- The extension contributes language configuration through `package.json`, so `*.rizz` files are automatically recognized.
- For true "just open folder and it works" without running a debug session, run the task `rizzo: install local extension` once (requires `code` CLI in PATH), then reopen VS Code.
- The C runtime headers (`src/runtime/*.h`) are bundled into the VSIX automatically by the `vscode:prepublish` step via `npm run copy:runtime`.

### LSP Commands

The extension provides several commands accessible via the Command Palette (`Ctrl+Shift+P`):

| Command | Description |
|---------|-------------|
| `Rizzo: Run Current File` | Compile and run the active `.rizz` file |
| `Rizzo: Check LSP Health` | Show LSP server status and configuration |
| `Rizzo: Restart LSP Server` | Restart the language server |

A status bar item shows the current LSP state (starting/running/stopped).

## Testing

Run the test suite with:

```bash
npm run test
# or
opam exec -- dune test
```

Tests cover parsing, type checking, transformations, and the language service.

## Documentation

- **Thesis document**: See `pdf/` for the LaTeX thesis document
- **Development notes**: See `notes/` for design decisions and progress
- **Syntax reference**: See `notes/syntax.md` for language syntax overview
