# RizzoMemory

MSc thesis

## Development

- Build: `opam exec -- dune build`
- Run compiler on a source file: `opam exec -- dune exec rizzoc ./examples/first.rizz`

## Full flow: compile and run a `.rizz` file

The full pipeline compiles a Rizzo source file to C and then runs it:

```
# 1. Compile .rizz → output.c
opam exec -- dune exec rizzoc <file.rizz>

# 2. Compile output.c with the C runtime headers
gcc -I./src/runtime output.c -o output

# 3. Run
./output
```

Or use the convenience npm script from the repo root (after step 1):

```
npm run rizzo
```

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

```
cd vscode-rizzo-lsp && npm install && cd ..
npm run ext:install
```
This installs the dependencies, builds the project and then installs the extension locally.

> To compile the bundle with no VSIX: `npm run compile`

### Debugging the extension

For development you might want to run the extension in a debug session, which allows you to set breakpoints and inspect variables in the LSP server code. To do this, you can run the debug configuration `Run Rizzo Extension (repo root)` from the repository root. This will launch a new VS Code window with the extension loaded. You can then open any `.rizz` file to see diagnostics, symbols, definitions, and hover information provided by the LSP server.

- Run debug config: `Run Rizzo Extension (repo root)`.
- Then open any `.rizz` file to get diagnostics, symbols, definition, and hover.

### Notes

- Workspace settings in `.vscode/settings.json` already map `*.rizz` to the `rizzo` language and preconfigure server command/args.
- For true "just open folder and it works" without running a debug session, run the task `rizzo: install local extension` once (requires `code` CLI in PATH), then reopen VS Code.
- The C runtime headers (`src/runtime/*.h`) are bundled into the VSIX automatically by the `vscode:prepublish` step via `npm run copy:runtime`.
