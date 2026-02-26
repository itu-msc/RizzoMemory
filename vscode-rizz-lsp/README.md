# Rizzo Language Support (Local)

This VS Code extension starts the local `rizzolsp` language server for `.rizz` files.

## Development

- Install deps: `npm install`
- Compile: `npm run compile`
- Press `F5` in this extension folder to launch an Extension Development Host.

## Requirements

- `opam` available in PATH
- Workspace opened at the RizzoMemory repository root
- Buildable server command: `opam exec -- dune exec rizzolsp`

## Commands

- `Rizzo: Check LSP Health`
- `Rizzo: Restart LSP Server`

## Settings

- `rizzoLsp.server.command`
- `rizzoLsp.server.args`
