# RizzoMemory

MSc thesis

## Development

- Build: `opam exec -- dune build`
- Run compiler on a source file: `opam exec -- dune exec rizzoc ./examples/first.rizz`

## LSP and VS Code extension

To run the LSP server, use the command:

- LSP server: `opam exec -- dune exec rizzolsp`

### VS Code extension development

The VS Code extension is located in the `vscode-rizz-lsp` folder.
It can easily be installed with

```
cd vscode-rizz-lsp && npm install && cd ..
npm run ext:install
```
This installs the dependencies, builds the project and then installs the extension locally.

> To compile the bundle with no VSIX: `npm run compile`

### Debugging the extension

For development you might want to run the extension in a debug session, which allows you to set breakpoints and inspect variables in the LSP server code. To do this, you can run the debug configuration `Run Rizz Extension (repo root)` from the repository root. This will launch a new VS Code window with the extension loaded. You can then open any `.rizz` file to see diagnostics, symbols, definitions, and hover information provided by the LSP server.

- Run debug config: `Run Rizz Extension (repo root)`.
- Then open any `.rizz` file to get diagnostics, symbols, definition, and hover.

### Notes

- Workspace settings in `.vscode/settings.json` already map `*.rizz` to the `rizz` language and preconfigure server command/args.
- For true "just open folder and it works" without running a debug session, run the task `rizz: install local extension` once (requires `code` CLI in PATH), then reopen VS Code.
