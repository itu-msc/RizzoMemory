import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Executable,
    State
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let statusBarItem: vscode.StatusBarItem | undefined;
let currentState: State = State.Stopped;

function stateLabel(state: State): string {
    switch (state) {
        case State.Starting:
            return "starting";
        case State.Running:
            return "running";
        case State.Stopped:
        default:
            return "stopped";
    }
}

function updateStatusBar(): void {
    if (!statusBarItem) {
        return;
    }

    if (currentState === State.Running) {
        statusBarItem.text = "$(check) Rizz LSP";
        statusBarItem.tooltip = "Rizz LSP is running";
        statusBarItem.backgroundColor = undefined;
    } else if (currentState === State.Starting) {
        statusBarItem.text = "$(sync~spin) Rizz LSP";
        statusBarItem.tooltip = "Rizz LSP is starting";
        statusBarItem.backgroundColor = undefined;
    } else {
        statusBarItem.text = "$(warning) Rizz LSP";
        statusBarItem.tooltip = "Rizz LSP is stopped";
        statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
    }
}

async function restartLanguageServer(): Promise<void> {
    if (!client) {
        await vscode.window.showWarningMessage("Rizz LSP is not running.");
        return;
    }

    const shouldStop = currentState !== State.Stopped;
    currentState = State.Starting;
    updateStatusBar();

    try {
        if (shouldStop) {
            await client.stop();
        }
        await client.start();
        currentState = State.Running;
        updateStatusBar();
        await vscode.window.showInformationMessage("Rizz LSP server restarted.");
    } catch (error) {
        currentState = State.Stopped;
        updateStatusBar();
        const message = error instanceof Error ? error.message : String(error);
        await vscode.window.showErrorMessage(`Failed to restart Rizz LSP server: ${message}`);
    }
}

/**
 * Resolves the path to the C runtime headers bundled with the extension.
 * When running from source (dev mode) falls back to ../../src/runtime.
 */
function getRuntimePath(context: vscode.ExtensionContext): string {
    const bundled = path.join(context.extensionPath, "runtime");
    if (fs.existsSync(bundled)) {
        return bundled;
    }
    return path.join(context.extensionPath, "..", "src", "runtime");
}

/**
 * Resolves the rizzoc compiler command and arguments.
 * Priority:
 *   1. User setting rizzLsp.compiler.command (if non-empty)
 *   2. Local dune build: <workspace>/_build/default/src/bin/main.exe
 *   3. Fallback: opam exec -- dune exec rizzoc --
 */
function getRizzocCommand(
    workspaceFolder: string | undefined
): { command: string; args: string[] } {
    const config = vscode.workspace.getConfiguration("rizzLsp");
    const userCommand = config.get<string>("compiler.command", "").trim();
    if (userCommand.length > 0) {
        return { command: userCommand, args: [] };
    }

    if (workspaceFolder) {
        const localBuild = path.join(
            workspaceFolder,
            "_build",
            "default",
            "src",
            "bin",
            "main.exe"
        );
        if (fs.existsSync(localBuild)) {
            return { command: localBuild, args: [] };
        }
    }

    return { command: "opam", args: ["exec", "--", "dune", "exec", "rizzoc", "--"] };
}

/**
 * Compiles the current .rizz file and runs the result in an integrated terminal.
 * Steps:
 *   1. rizzoc <file>          → output.c  (in the workspace directory)
 *   2. <cc> -I<runtime> output.c -o output
 *   3. ./output  (or .\output.exe on Windows)
 */
async function runCurrentFile(context: vscode.ExtensionContext): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        await vscode.window.showErrorMessage("Rizz: No active editor.");
        return;
    }

    const doc = editor.document;
    if (doc.languageId !== "rizz") {
        await vscode.window.showErrorMessage("Rizz: Active file is not a .rizz file.");
        return;
    }

    // Save before compiling.
    await doc.save();

    const filePath = doc.uri.fsPath;
    const workspaceFolder =
        vscode.workspace.getWorkspaceFolder(doc.uri)?.uri.fsPath ??
        path.dirname(filePath);

    const rizzoc = getRizzocCommand(workspaceFolder);
    const runtimePath = getRuntimePath(context);
    const config = vscode.workspace.getConfiguration("rizzLsp");

    // Build the rizzoc invocation.
    const quotedCommand = `"${rizzoc.command}"`;
    const quotedArgs = rizzoc.args.join(" ");
    const quotedFile = `"${filePath}"`;
    const rizzocCmd =
        quotedArgs.length > 0
            ? `${quotedCommand} ${quotedArgs} ${quotedFile}`
            : `${quotedCommand} ${quotedFile}`;

    // Validate the C compiler setting to prevent shell injection.
    const rawCc = config.get<string>("compiler.cc", "gcc").trim() || "gcc";
    if (!/^[a-zA-Z0-9\-_./ \\:]+$/.test(rawCc)) {
        await vscode.window.showErrorMessage(
            `Rizz: Invalid rizzLsp.compiler.cc value: "${rawCc}". ` +
            `Only alphanumeric characters, spaces, and path separators are allowed.`
        );
        return;
    }
    const cc = rawCc;

    const ccCmd = `${cc} -I"${runtimePath}" output.c -o output`;
    const runCmd = process.platform === "win32" ? `.\\output.exe` : `./output`;

    const terminal = vscode.window.createTerminal({
        name: "Rizz Run",
        cwd: workspaceFolder
    });
    terminal.show(true);
    terminal.sendText(`${rizzocCmd} && ${ccCmd} && ${runCmd}`);
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const config = vscode.workspace.getConfiguration("rizzLsp");
    let command = config.get<string>("server.command", "opam");
    let args = config.get<string[]>("server.args", ["exec", "--", "dune", "exec", "rizzolsp"]);

    const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const fallbackCwd = path.dirname(context.extensionPath);
    const serverRoot = workspaceFolder ?? fallbackCwd;
    const builtServerPath = path.join(serverRoot, "_build", "default", "src", "bin", "rizzolsp.exe");

    if (fs.existsSync(builtServerPath)) {
        command = builtServerPath;
        args = [];
    }

    const serverExecutable: Executable = {
        command,
        args,
        options: {
            cwd: serverRoot
        }
    };

    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    statusBarItem.command = "rizzLsp.checkHealth";
    currentState = State.Starting;
    updateStatusBar();
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);

    const serverOptions: ServerOptions = {
        run: serverExecutable,
        debug: serverExecutable
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: "rizz" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.rizz")
        }
    };

    client = new LanguageClient(
        "rizzLanguageServer",
        "Rizz Language Server",
        serverOptions,
        clientOptions
    );

    context.subscriptions.push(
        client.onDidChangeState((event) => {
            currentState = event.newState;
            updateStatusBar();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("rizzLsp.checkHealth", async () => {
            const state = stateLabel(currentState);
            const workspace = workspaceFolder ?? "<none>";
            const cwd = serverExecutable.options?.cwd ?? "<none>";
            const launch = [command, ...args].join(" ");
            const message = [
                `State: ${state}`,
                `Workspace: ${workspace}`,
                `Server cwd: ${cwd}`,
                `Launch: ${launch}`
            ].join("\n");

            if (currentState === State.Running) {
                await vscode.window.showInformationMessage(`Rizz LSP health OK\n${message}`);
            } else {
                await vscode.window.showWarningMessage(`Rizz LSP health warning\n${message}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("rizzLsp.restartServer", async () => {
            await restartLanguageServer();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("rizzLsp.runFile", async () => {
            await runCurrentFile(context);
        })
    );

    await client.start();
    currentState = State.Running;
    updateStatusBar();
    context.subscriptions.push({
        dispose: () => {
            if (client) {
                void client.stop();
                client = undefined;
            }
            currentState = State.Stopped;
            updateStatusBar();
        }
    });
}

export async function deactivate(): Promise<void> {
    if (!client) {
        currentState = State.Stopped;
        updateStatusBar();
        return;
    }
    await client.stop();
    client = undefined;
    currentState = State.Stopped;
    updateStatusBar();
}
