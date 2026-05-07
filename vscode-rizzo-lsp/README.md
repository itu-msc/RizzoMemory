# Rizzo Language Support (Local)

This VS Code extension starts the local `rizzolsp` language server for `.rizz` files.

The extension resolves the language server in this order:

1. `rizzoLsp.server.command`, if set
2. A local workspace build at `_build/default/src/bin/rizzolsp(.exe)`
3. `rizzolsp` on `PATH`

`Rizzo: Run Current File` resolves `rizzoc` the same way, using `rizzoLsp.compiler.command`, then a local workspace build, then `rizzoc` on `PATH`.

## Features

- Diagnostics and parser or type checker errors
- Document symbols
- Go to definition
- Hover information
- Semantic tokens
- Code completion for in-scope names (manual `Ctrl+Space` and typing-triggered suggestions)

## Development

- Install deps: `npm install`
- Compile: `npm run compile`
- Press `F5` in this extension folder to launch an Extension Development Host.

## Requirements

- Either `rizzoLsp.server.command` set explicitly, a workspace containing `_build/default/src/bin/rizzolsp(.exe)`, or `rizzolsp` available on `PATH`
- For `Rizzo: Run Current File`, either `rizzoLsp.compiler.command` set explicitly, a workspace containing `_build/default/src/bin/main(.exe)`, or `rizzoc` available on `PATH`
- If you want the extension to pick up the repository build automatically, open the RizzoMemory repository root as the workspace

## Commands

- `Rizzo: Run Current File`
- `Rizzo: Check LSP Health`
- `Rizzo: Restart LSP Server`

## Settings

- `rizzoLsp.server.command`: explicit path to `rizzolsp`
- `rizzoLsp.server.args`: arguments used with `rizzoLsp.server.command`
- `rizzoLsp.server.workspaceFolder`: optional workspace folders used to resolve local builds
- `rizzoLsp.compiler.command`: explicit path to `rizzoc`
