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
        documentSelector: [{ scheme: "file", language: "rizz" }],
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
