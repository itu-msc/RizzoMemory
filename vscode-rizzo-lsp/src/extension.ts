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

async function getValidWorkspaceFolder(config: vscode.WorkspaceConfiguration) {
    let workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const customWorkspaceFolders = config.get<string[]>("server.workspaceFolder", []).map(folder => folder.trim());
    if (customWorkspaceFolders.length > 0) {
        const validFolders = customWorkspaceFolders.filter(folder => fs.existsSync(folder) && fs.statSync(folder).isDirectory());
        if (validFolders.length > 0) {
            workspaceFolder = validFolders[0];
        } else {
            await vscode.window.showWarningMessage(
                "Rizzo LSP: No valid workspace folders found in configuration. " +
                "Falling back to default workspace folder or extension directory."
            );
        }
    }
    return workspaceFolder;
}

function getOrCreateRunTerminal(workspaceFolder: string): vscode.Terminal {
    const activeTerminal = vscode.window.activeTerminal;
    if (activeTerminal) {
        return activeTerminal;
    }

    const existingTerminal = vscode.window.terminals.at(-1);
    if (existingTerminal) {
        return existingTerminal;
    }

    return vscode.window.createTerminal({
        name: "Rizzo Run",
        cwd: workspaceFolder
    });
}

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
        statusBarItem.text = "$(check) Rizzo LSP";
        statusBarItem.tooltip = "Rizzo LSP is running";
        statusBarItem.backgroundColor = undefined;
    } else if (currentState === State.Starting) {
        statusBarItem.text = "$(sync~spin) Rizzo LSP";
        statusBarItem.tooltip = "Rizzo LSP is starting";
        statusBarItem.backgroundColor = undefined;
    } else {
        statusBarItem.text = "$(warning) Rizzo LSP";
        statusBarItem.tooltip = "Rizzo LSP is stopped";
        statusBarItem.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
    }
}

async function restartLanguageServer(): Promise<void> {
    if (!client) {
        await vscode.window.showWarningMessage("Rizzo LSP is not running.");
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
        await vscode.window.showInformationMessage("Rizzo LSP server restarted.");
    } catch (error) {
        currentState = State.Stopped;
        updateStatusBar();
        const message = error instanceof Error ? error.message : String(error);
        await vscode.window.showErrorMessage(`Failed to restart Rizzo LSP server: ${message}`);
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
 *   1. User setting rizzoLsp.compiler.command (if non-empty)
 *   2. Local dune build: <workspace>/_build/default/src/bin/main.exe
 *   3. Fallback: opam exec -- dune exec rizzoc --
 */
function getRizzocCommand(
    workspaceFolder: string | undefined
): { command: string; args: string[] } {
    const config = vscode.workspace.getConfiguration("rizzoLsp");
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
        await vscode.window.showErrorMessage("Rizzo: No active editor.");
        return;
    }

    const doc = editor.document;
    if (doc.languageId !== "rizzo") {
        await vscode.window.showErrorMessage("Rizzo: Active file is not a .rizz file.");
        return;
    }

    // Save before compiling.
    await doc.save();

    const filePath = doc.uri.fsPath;

    const config = vscode.workspace.getConfiguration("rizzoLsp");
    const workspaceFolder = await getValidWorkspaceFolder(vscode.workspace.getConfiguration("rizzoLsp"));

    const rizzoc = getRizzocCommand(workspaceFolder);
    const runtimePath = getRuntimePath(context);
    const isWindows = process.platform === "win32";

    // Build the rizzoc invocation.
    const commandPrefix = isWindows ? "& " : "";
    const quotedCommand = `${commandPrefix}"${rizzoc.command}"`;
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
            `Rizzo: Invalid rizzoLsp.compiler.cc value: "${rawCc}". ` +
            `Only alphanumeric characters, spaces, and path separators are allowed.`
        );
        return;
    }
    const cc = rawCc;

    const outputName = isWindows ? "output.exe" : "output";
    const windowsArchFlag = isWindows ? " -m64" : "";
    const ccCmd = `${cc}${windowsArchFlag} -I"${runtimePath}" output.c -o ${outputName}`;
    const runCmd = isWindows ? `.\\${outputName}` : `./${outputName}`;

    const terminal = getOrCreateRunTerminal(workspaceFolder ?? "");
    terminal.show(true);

    const pwdVar = isWindows ? "$PREV_PWD = $PWD" : "PREV_PWD=$(pwd)";
    //TODO: Dont change dirs if the user use the opam installed rizzoc instead of a local build.
    //TODO: Use the c library files from the local workspace if not using the opam rizzoc installation.
    terminal.sendText(`${pwdVar}; cd "${workspaceFolder}" && ${rizzocCmd} && ${ccCmd} && echo "" && ${runCmd}; cd $PREV_PWD`);
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    const config = vscode.workspace.getConfiguration("rizzoLsp");
    let command = config.get<string>("server.command", "opam");
    let args = config.get<string[]>("server.args", ["exec", "--", "dune", "exec", "rizzolsp"]);

    let workspaceFolder = await getValidWorkspaceFolder(config);
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
    statusBarItem.command = "rizzoLsp.checkHealth";
    currentState = State.Starting;
    updateStatusBar();
    statusBarItem.show();
    context.subscriptions.push(statusBarItem);

    const serverOptions: ServerOptions = {
        run: serverExecutable,
        debug: serverExecutable
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: "rizzo" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.rizz")
        }
    };

    client = new LanguageClient(
        "rizzoLanguageServer",
        "Rizzo Language Server",
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
        vscode.commands.registerCommand("rizzoLsp.checkHealth", async () => {
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
                await vscode.window.showInformationMessage(`Rizzo LSP health OK\n${message}`, { modal: true });
            } else {
                await vscode.window.showWarningMessage(`Rizzo LSP health warning\n${message}`);
            }
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("rizzoLsp.restartServer", async () => {
            await restartLanguageServer();
        })
    );

    context.subscriptions.push(
        vscode.commands.registerCommand("rizzoLsp.runFile", async () => {
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
