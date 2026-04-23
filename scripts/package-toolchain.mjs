import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const repoRoot = path.resolve(__dirname, "..");

function parseArgs(argv) {
  const args = {};
  for (let index = 0; index < argv.length; index += 1) {
    const token = argv[index];
    if (!token.startsWith("--")) {
      throw new Error(`Unexpected argument: ${token}`);
    }
    const key = token.slice(2);
    const value = argv[index + 1];
    if (!value || value.startsWith("--")) {
      throw new Error(`Missing value for --${key}`);
    }
    args[key] = value;
    index += 1;
  }
  return args;
}

function normalizePlatform(value) {
  switch (value) {
    case "win32":
    case "windows":
      return "windows";
    case "darwin":
    case "macos":
      return "darwin";
    case "linux":
      return "linux";
    default:
      throw new Error(`Unsupported platform: ${value}`);
  }
}

function normalizeArch(value) {
  switch (value) {
    case "x64":
    case "amd64":
    case "x86_64":
      return "x86_64";
    case "arm64":
    case "aarch64":
      return "arm64";
    default:
      throw new Error(`Unsupported architecture: ${value}`);
  }
}

function ensureExists(targetPath, description) {
  if (!existsSync(targetPath)) {
    throw new Error(`Missing ${description}: ${targetPath}`);
  }
}

function ensureDir(targetPath) {
  mkdirSync(targetPath, { recursive: true });
}

function copyMatchingFiles(sourceDir, targetDir, predicate) {
  ensureExists(sourceDir, "source directory");
  ensureDir(targetDir);
  for (const entry of readdirSync(sourceDir)) {
    const sourcePath = path.join(sourceDir, entry);
    if (!statSync(sourcePath).isFile() || !predicate(entry)) {
      continue;
    }
    copyFileSync(sourcePath, path.join(targetDir, entry));
  }
}

function run(command, args) {
  const result = spawnSync(command, args, {
    cwd: repoRoot,
    stdio: "inherit",
  });
  if (result.status !== 0) {
    throw new Error(`${command} exited with status ${result.status ?? "unknown"}`);
  }
}

function createArchive({ platform, stageDir, archivePath }) {
  rmSync(archivePath, { force: true });
  if (platform === "windows") {
    const archivePathWindows = archivePath.replace(/'/g, "''");
    const stageWildcard = `${stageDir.replace(/'/g, "''")}\\*`;
    run("pwsh", [
      "-NoLogo",
      "-NoProfile",
      "-Command",
      `Compress-Archive -Path '${stageWildcard}' -DestinationPath '${archivePathWindows}' -Force`,
    ]);
    return;
  }

  run("tar", ["-czf", archivePath, "-C", path.dirname(stageDir), path.basename(stageDir)]);
}

function sha256(targetPath) {
  const hash = createHash("sha256");
  hash.update(readFileSync(targetPath));
  return hash.digest("hex");
}

function updateChecksumsFile(checksumsPath, archiveFileName, digest) {
  const nextEntry = `${digest}  ${archiveFileName}`;
  const existingEntries = existsSync(checksumsPath)
    ? readFileSync(checksumsPath, "utf8")
        .split(/\r?\n/)
        .map((line) => line.trim())
        .filter((line) => line.length > 0)
    : [];

  const mergedEntries = existingEntries
    .filter((line) => !line.endsWith(`  ${archiveFileName}`))
    .concat(nextEntry)
    .sort((left, right) => left.localeCompare(right));

  writeFileSync(checksumsPath, `${mergedEntries.join("\n")}\n`);
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  const platform = normalizePlatform(args.platform ?? process.platform);
  const arch = normalizeArch(args.arch ?? process.arch);
  const version = args.version ?? process.env.RIZZOC_VERSION ?? "dev";
  const distDir = path.resolve(repoRoot, args["output-dir"] ?? "dist");
  const compilerSource = path.resolve(repoRoot, args.compiler ?? path.join("_build", "default", "src", "bin", "main.exe"));
  const lspSource = path.resolve(repoRoot, args.lsp ?? path.join("_build", "default", "src", "bin", "rizzolsp.exe"));
  const runtimeSource = path.join(repoRoot, "src", "runtime");
  const stdlibSource = path.join(repoRoot, "src", "stdlib");

  ensureExists(compilerSource, "compiler executable");
  ensureExists(lspSource, "language server executable");

  const baseName = `rizzo-toolchain_${platform}_${arch}`;
  const archiveExtension = platform === "windows" ? ".zip" : ".tar.gz";
  const stageDir = path.join(distDir, baseName);
  const archivePath = path.join(distDir, `${baseName}${archiveExtension}`);
  const checksumsPath = path.join(distDir, "checksums.txt");
  const targetCompilerName = platform === "windows" ? "rizzoc.exe" : "rizzoc";
  const targetLspName = platform === "windows" ? "rizzolsp.exe" : "rizzolsp";

  rmSync(stageDir, { recursive: true, force: true });
  ensureDir(path.join(stageDir, "bin"));
  ensureDir(path.join(stageDir, "lib", "rizzoc"));

  copyFileSync(compilerSource, path.join(stageDir, "bin", targetCompilerName));
  copyFileSync(lspSource, path.join(stageDir, "bin", targetLspName));
  copyMatchingFiles(runtimeSource, path.join(stageDir, "lib", "rizzoc", "runtime"), (entry) => entry.endsWith(".h"));
  copyMatchingFiles(stdlibSource, path.join(stageDir, "lib", "rizzoc", "stdlib"), (entry) => entry === "manifest.txt" || entry.endsWith(".rizz"));

  writeFileSync(path.join(stageDir, "VERSION"), `${version}\n`);

  createArchive({ platform, stageDir, archivePath });
  updateChecksumsFile(checksumsPath, path.basename(archivePath), sha256(archivePath));

  console.log(`Created ${archivePath}`);
  console.log(`Updated ${checksumsPath}`);
}

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exit(1);
}