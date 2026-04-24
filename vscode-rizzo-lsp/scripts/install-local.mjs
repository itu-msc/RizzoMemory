import { spawnSync } from "node:child_process";
import { readdirSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const extensionRoot = path.resolve(__dirname, "..");
const outDir = path.join(extensionRoot, "out");
const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const installTarget = parseInstallTarget(args);

function findNewestVsix(directory) {
  const candidates = readdirSync(directory)
    .filter((entry) => entry.endsWith(".vsix"))
    .map((entry) => {
      const filePath = path.join(directory, entry);
      return {
        filePath,
        entry,
        mtimeMs: statSync(filePath).mtimeMs,
      };
    })
    .sort((left, right) => right.mtimeMs - left.mtimeMs || left.entry.localeCompare(right.entry));

  return candidates[0]?.filePath;
}

function parseInstallTarget(argv) {
  const targetArg = argv.find((arg) => !arg.startsWith("-"));

  if (!targetArg) {
    return "stable";
  }

  if (["insiders", "insider", "code-insiders"].includes(targetArg)) {
    return "insiders";
  }

  if (["stable", "vscode", "code"].includes(targetArg)) {
    return "stable";
  }

  throw new Error(`Unknown VS Code target \"${targetArg}\". Use \"insiders\" or omit the target for stable VS Code.`);
}

function resolveCodeCli(target) {
  const candidates = getCodeCliCandidates(target);

  for (const candidate of candidates) {
    const result = spawnCodeCli(candidate, ["--version"], "ignore");
    if (!result.error && result.status === 0) {
      return candidate;
    }
  }

  throw new Error(`Could not find a ${target === "insiders" ? "VS Code Insiders" : "VS Code"} CLI. Expected ${formatCandidateList(candidates)}.`);
}

function getCodeCliCandidates(target) {
  if (process.platform !== "win32") {
    return target === "insiders"
      ? ["code-insiders"]
      : ["code"];
  }

  const stableCandidates = [
    "code.cmd",
    "code",
    getWindowsUserCodeCliPath("Microsoft VS Code", "code.cmd"),
    getWindowsSystemCodeCliPath("ProgramFiles", "Microsoft VS Code", "code.cmd"),
    getWindowsSystemCodeCliPath("ProgramFiles(x86)", "Microsoft VS Code", "code.cmd"),
  ];
  const insidersCandidates = [
    "code-insiders.cmd",
    "code-insiders",
    getWindowsUserCodeCliPath("Microsoft VS Code Insiders", "code-insiders.cmd"),
    getWindowsSystemCodeCliPath("ProgramFiles", "Microsoft VS Code Insiders", "code-insiders.cmd"),
    getWindowsSystemCodeCliPath("ProgramFiles(x86)", "Microsoft VS Code Insiders", "code-insiders.cmd"),
  ];

  return (target === "insiders" ? insidersCandidates : stableCandidates).filter(Boolean);
}

function getWindowsUserCodeCliPath(appFolderName, executableName) {
  const baseDir = process.env.LOCALAPPDATA;
  if (!baseDir) {
    return null;
  }

  return path.join(baseDir, "Programs", appFolderName, "bin", executableName);
}

function getWindowsSystemCodeCliPath(envVar, appFolderName, executableName) {
  const baseDir = process.env[envVar];
  if (!baseDir) {
    return null;
  }

  return path.join(baseDir, appFolderName, "bin", executableName);
}

function formatCandidateList(candidates) {
  return candidates.join(", ");
}

function spawnCodeCli(command, cliArgs, stdio) {
  return spawnSync(command, cliArgs, {
    stdio,
    shell: process.platform === "win32",
  });
}

const vsixPath = findNewestVsix(outDir);

if (!vsixPath) {
  throw new Error(`No VSIX found in ${outDir}. Run npm run package:vsix first.`);
}

const codeCli = resolveCodeCli(installTarget);

if (dryRun) {
  console.log(JSON.stringify({
    target: installTarget,
    codeCli,
    vsixPath,
  }, null, 2));
  process.exit(0);
}

const installResult = spawnCodeCli(codeCli, ["--install-extension", vsixPath, "--force"], "inherit");

if (installResult.error) {
  throw installResult.error;
}

if (installResult.status !== 0) {
  process.exit(installResult.status ?? 1);
}