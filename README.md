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

## Installation

### Linux and macOS

Run the install script to download and install the latest version:

```bash
curl -fsSL https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.sh | bash
```

To install a specific version, pass the `--version` flag:

```bash
curl -fsSL https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.sh | bash -s -- --version v0.1.2
```

**Security Note**: You can download and inspect the script before running it:

```sh
curl -fsSL https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.sh -o install.sh
# Review the script
cat install.sh
# Run it
bash install.sh
```

By default the installer:

- installs versioned toolchains under `${XDG_DATA_HOME:-$HOME/.local/share}/rizzo/toolchains`
- updates `${XDG_DATA_HOME:-$HOME/.local/share}/rizzo/current`
- links `rizzoc` and `rizzolsp` into `$HOME/.local/bin`

You can override the managed install root with `RIZZO_HOME` and the symlink directory with `RIZZO_BIN_DIR`.

### Windows

Run the install script to download and install the latest version:

```powershell
irm https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.ps1 | iex
```

To install a specific version, use the scriptblock form so PowerShell can bind `-Version`:

```powershell
& ([scriptblock]::Create((irm https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.ps1))) -Version 0.2.1
```

By default the installer:

- installs versioned toolchains under `$env:LOCALAPPDATA\Rizzo\toolchains`
- updates `$env:LOCALAPPDATA\Rizzo\current`
- creates `rizzoc.cmd` and `rizzolsp.cmd` under `$env:LOCALAPPDATA\Programs\Rizzo\bin`
- adds that bin directory to the user `Path`

You can override the managed install root with `RIZZO_HOME`, the launcher directory with `RIZZO_BIN_DIR`, and skip the `Path` update with `-NoPathUpdate`.

If you want to inspect the installer first, download `install.ps1` and run it locally instead of piping it to `iex`.

## Development

- Build: `opam exec -- dune build`
- Run compiler on one or more source files: `opam exec -- dune exec rizzoc ./examples/first.rizz`

By default the compiler is quiet and only emits `output.c`. To inspect the intermediate ASTs while compiling, pass `--print-ast` to `rizzoc`.

## How to Compile and Run Rizzo Programs

The compiler translates Rizzo source files (`.rizz`) to C (`output.c`), and triggers a C compile with either `gcc` or `clang` to produce an executable.

All you need to do is invoke the compiler on your Rizzo source file(s), and it will handle the rest:

```bash
rizzoc [more-files.rizz ...] <entrypoint.rizz>
```

For help run:

```bash
rizzoc --help
```

### Implicit stdlib

File-based compilation and editor analysis now prepend a compiler-owned stdlib before user files are typechecked. The default stdlib lives in [src/stdlib](src/stdlib) in the repository and installs next to the compiler under `lib/rizzoc/stdlib`. That means user `.rizz` files can use combinators such as `map`, `zip`, `scan`, and `filter_map` without copy-pasting their definitions first.

You can replace the default stdlib with `--overwrite-stdpath <path>`, where `<path>` can point to either a single `.rizz` file or a stdlib directory. You can also prepend extra files or directories with repeated `-I <path>` flags.

Multiple input files are compiled left-to-right after that implicit stdlib and any `-I` includes. Later files can refer to earlier files, but the reverse order is not supported yet.

### Running from VS Code

Open any `.rizz` file and click the **▶ Run** button in the editor title bar, or run the command **Rizzo: Run Current File** from the Command Palette.

The extension will:

1. Save the file
2. Compile it with `rizzoc`
3. Run the resulting binary in an integrated terminal

For `Rizzo: Run Current File`, the extension resolves `rizzoc` in this order:

1. `rizzoLsp.compiler.command`, if set
2. A local workspace build at `_build/default/src/bin/main(.exe)`
3. `rizzoc` on `PATH`

**Configuration** (`settings.json`):

| Setting | Default | Description |
| --- | --- | --- |
| `rizzoLsp.compiler.command` | *(auto)* | Path to `rizzoc` binary. Leave empty to auto-detect. |
| `rizzoLsp.server.command` | *(auto)* | Path to `rizzolsp`. Leave empty to auto-detect. |
| `rizzoLsp.server.args` | `[]` | Extra arguments passed when `rizzoLsp.server.command` is set explicitly. |
| `rizzoLsp.server.workspaceFolder` | `[]` | Optional workspace roots to use when resolving local builds during development. |

## LSP and VS Code extension

The Extension can be installed from the GitHub releases page, or built locally from the `vscode-rizzo-lsp` folder.

The extension starts `rizzolsp` automatically when you open a `.rizz` file. It resolves the server in this order:

1. `rizzoLsp.server.command`, if set
2. A local workspace build at `_build/default/src/bin/rizzolsp(.exe)`
3. `rizzolsp` on `PATH`

If you are developing inside this repository and want the extension to use the repo build, run `opam exec -- dune build` from the repository root first.

You can also run the server manually for development or debugging purposes.

To run the LSP server, use the command:

- LSP server: `opam exec -- dune exec rizzolsp`

### VS Code extension development

The VS Code extension is located in the `vscode-rizzo-lsp` folder.
It can easily be installed with

```bash
npm run ext:install:deps
npm run ext:install
npm run ext:install -- insiders
```

This installs the dependencies, builds the project and then installs the extension locally.

### Debugging the extension

For development you might want to run the extension in a debug session, which allows you to set breakpoints and inspect variables in the LSP server code. To do this, you can run the debug configuration `Run Rizzo Extension (repo root)` from the repository root. This will launch a new VS Code window with the extension loaded. You can then open any `.rizz` file to see diagnostics, symbols, definitions, hover information, and completion suggestions provided by the LSP server.

- Run debug config: `Run Rizzo Extension (repo root)`.
- Then open any `.rizz` file to get diagnostics, symbols, definition, hover, and completions (`Ctrl+Space` and typing-triggered suggestions).

### Notes

- The extension contributes language configuration through `package.json`, so `*.rizz` files are automatically recognized.
- For true "just open folder and it works" without running a debug session, run the task `rizzo: install local extension` once (requires `code` CLI in PATH), then reopen VS Code.
- `Rizzo: Check LSP Health` shows which server command and working directory the extension is currently using.

### LSP Commands

The extension provides several commands accessible via the Command Palette (`Ctrl+Shift+P`):

| Command | Description |
| --- | --- |
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
