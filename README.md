# RizzoMemory

MSc thesis

## Development

- Build: `opam exec -- dune build`
- Run compiler on a source file: `opam exec -- dune exec rizzoc ./examples/first.rizz`
- Start language server (stdio): `opam exec -- dune exec rizzolsp`

## VS Code extension (local testing)

- Folder: `vscode-rizz-lsp`
- Install deps: `cd vscode-rizz-lsp && npm install`
- Compile: `npm run compile`

### From repository root (recommended)

- Open this repository folder in VS Code.
- Run debug config: `Run Rizz Extension (repo root)`.
- Then open any `.rizz` file to get diagnostics, symbols, definition, and hover.

### Notes

- Workspace settings in `.vscode/settings.json` already map `*.rizz` to the `rizz` language and preconfigure server command/args.
- For true "just open folder and it works" without running a debug session, run the task `rizz: install local extension` once (requires `code` CLI in PATH), then reopen VS Code.
