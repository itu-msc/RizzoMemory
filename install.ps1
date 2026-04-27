param(
    [string]$Version = $env:RIZZO_VERSION,
    [switch]$Force,
    [switch]$NoPathUpdate,
    [string]$RizzoHome = $(if ($env:RIZZO_HOME) { $env:RIZZO_HOME } else { Join-Path $env:LOCALAPPDATA "Rizzo" }),
    [string]$RizzoBinDir = $(if ($env:RIZZO_BIN_DIR) { $env:RIZZO_BIN_DIR } else { Join-Path $env:LOCALAPPDATA "Programs\Rizzo\bin" }),
    [string]$RepoOwner = $(if ($env:RIZZO_REPO_OWNER) { $env:RIZZO_REPO_OWNER } else { "itu-msc" }),
    [string]$RepoName = $(if ($env:RIZZO_REPO_NAME) { $env:RIZZO_REPO_NAME } else { "RizzoMemory" }),
    [string]$ReleaseBaseUrl = $env:RIZZO_RELEASE_BASE_URL,
    [switch]$Help
)

$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

function Write-Info {
    param([string]$Message)
    Write-Host $Message -ForegroundColor Green
}

function Write-Warn {
    param([string]$Message)
    Write-Host $Message -ForegroundColor Yellow
}

function Show-Usage {
    @"
Usage: install.ps1 [-Version <tag>] [-Force] [-NoPathUpdate]

Installs the Rizzo toolchain into a managed user directory and creates
launcher scripts for rizzoc and rizzolsp.

Options:
  -Version <tag>      Install a specific tag such as v0.1.2
  -Force              Reinstall even if the target version already exists
  -NoPathUpdate       Do not add the bin directory to the user PATH
  -Help               Show this help text

Environment overrides:
  RIZZO_VERSION             Same as -Version
  RIZZO_HOME                Managed toolchain root
  RIZZO_BIN_DIR             Directory for rizzoc/rizzolsp launchers
  RIZZO_REPO_OWNER          GitHub owner (default: itu-msc)
  RIZZO_REPO_NAME           GitHub repo (default: RizzoMemory)
  RIZZO_RELEASE_BASE_URL    Exact base URL containing checksums.txt and the archive

Examples:
  powershell -ExecutionPolicy Bypass -File .\install.ps1
  powershell -ExecutionPolicy Bypass -File .\install.ps1 -Version v0.1.2
"@
}

function Resolve-Arch {
    $arch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString().ToLowerInvariant()
    switch ($arch) {
        "x64" { return "x86_64" }
        "arm64" { return "arm64" }
        default { throw "Unsupported architecture: $arch" }
    }
}

function Resolve-Version {
    if (-not [string]::IsNullOrWhiteSpace($Version)) {
        return Normalize-ReleaseTag -Value $Version
    }

    Write-Info "Fetching latest Rizzo release information..."
    $release = Invoke-RestMethod -Uri "https://api.github.com/repos/$RepoOwner/$RepoName/releases/latest"
    if ([string]::IsNullOrWhiteSpace($release.tag_name)) {
        throw "Failed to parse release tag from GitHub response"
    }

    return Normalize-ReleaseTag -Value ([string]$release.tag_name)
}

function Normalize-ReleaseTag {
    param([string]$Value)

    $trimmed = $Value.Trim()
    if ([string]::IsNullOrWhiteSpace($trimmed)) {
        throw "Release version cannot be empty"
    }

    if ($trimmed -match '^v') {
        return $trimmed
    }

    return "v$trimmed"
}

function Format-VersionForDisplay {
    param([string]$ReleaseTag)

    if ($ReleaseTag.StartsWith("v")) {
        return $ReleaseTag.Substring(1)
    }

    return $ReleaseTag
}

function Resolve-ReleaseBaseUrl {
    param([string]$ResolvedVersion)

    if (-not [string]::IsNullOrWhiteSpace($ReleaseBaseUrl)) {
        return $ReleaseBaseUrl.TrimEnd("/")
    }

    return "https://github.com/$RepoOwner/$RepoName/releases/download/$ResolvedVersion"
}

function Get-ChecksumEntry {
    param(
        [string]$ChecksumsPath,
        [string]$ArchiveName
    )

    foreach ($line in Get-Content -LiteralPath $ChecksumsPath) {
        if ($line -match '^([0-9A-Fa-f]+)\s+(.+)$') {
            if ($Matches[2].Trim() -eq $ArchiveName) {
                return $Matches[1].ToLowerInvariant()
            }
        }
    }

    throw "No checksum entry found for $ArchiveName"
}

function Verify-Checksum {
    param(
        [string]$ArchivePath,
        [string]$ChecksumsPath,
        [string]$ArchiveName
    )

    $expected = Get-ChecksumEntry -ChecksumsPath $ChecksumsPath -ArchiveName $ArchiveName
    $actual = (Get-FileHash -LiteralPath $ArchivePath -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($expected -ne $actual) {
        throw "Checksum mismatch for $ArchiveName"
    }
}

function Resolve-ArchiveRoot {
    param(
        [string]$ExpandedRoot,
        [string]$Arch
    )

    $namedRoot = Join-Path $ExpandedRoot "rizzo-toolchain_windows_$Arch"
    if (Test-Path -LiteralPath $namedRoot) {
        return $namedRoot
    }

    if (Test-Path -LiteralPath (Join-Path $ExpandedRoot "bin")) {
        return $ExpandedRoot
    }

    $childDirectories = Get-ChildItem -LiteralPath $ExpandedRoot -Directory
    if ($childDirectories.Count -eq 1) {
        $singleRoot = $childDirectories[0].FullName
        if (Test-Path -LiteralPath (Join-Path $singleRoot "bin")) {
            return $singleRoot
        }
    }

    throw "Archive did not contain an expected toolchain layout under $ExpandedRoot"
}

function New-CurrentJunction {
    param(
        [string]$CurrentPath,
        [string]$TargetPath
    )

    if (Test-Path -LiteralPath $CurrentPath) {
        Remove-Item -LiteralPath $CurrentPath -Recurse -Force
    }

    $mklinkOutput = cmd /c mklink /J "$CurrentPath" "$TargetPath"
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to create junction at $CurrentPath`n$mklinkOutput"
    }
}

function Write-Launcher {
    param(
        [string]$LauncherPath,
        [string]$TargetPath
    )

    $content = "@echo off`r`n`"$TargetPath`" %*`r`n"
    Set-Content -LiteralPath $LauncherPath -Value $content -Encoding ASCII
}

function Ensure-PathEntry {
    param([string]$BinPath)

    $normalizedBinPath = [System.IO.Path]::GetFullPath($BinPath).TrimEnd('\\')
    $userPath = [Environment]::GetEnvironmentVariable("Path", "User")
    $entries = @()

    if (-not [string]::IsNullOrWhiteSpace($userPath)) {
        $entries = $userPath.Split(";", [System.StringSplitOptions]::RemoveEmptyEntries)
    }

    foreach ($entry in $entries) {
        if ([System.IO.Path]::GetFullPath($entry).TrimEnd('\\') -ieq $normalizedBinPath) {
            return $true
        }
    }

    if ($NoPathUpdate) {
        return $false
    }

    $nextEntries = @($entries + $normalizedBinPath)
    [Environment]::SetEnvironmentVariable("Path", ($nextEntries -join ";"), "User")
    if ([string]::IsNullOrWhiteSpace($env:Path)) {
        $env:Path = $normalizedBinPath
    }
    else {
        $env:Path = "$env:Path;$normalizedBinPath"
    }

    return $true
}

if ($Help) {
    Show-Usage
    exit 0
}

$resolvedVersion = Resolve-Version
$displayVersion = Format-VersionForDisplay -ReleaseTag $resolvedVersion
$arch = Resolve-Arch
$archiveName = "rizzo-toolchain_windows_${arch}.zip"
$baseUrl = Resolve-ReleaseBaseUrl -ResolvedVersion $resolvedVersion

Write-Info "Detected platform: windows/$arch"
Write-Info "Installing Rizzo version: $displayVersion"

$tempRoot = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString("N"))
$archivePath = Join-Path $tempRoot $archiveName
$checksumsPath = Join-Path $tempRoot "checksums.txt"
$expandedRoot = Join-Path $tempRoot "expanded"

try {
    New-Item -ItemType Directory -Path $tempRoot -Force | Out-Null
    New-Item -ItemType Directory -Path $expandedRoot -Force | Out-Null

    Write-Info "Downloading $archiveName..."
    Invoke-WebRequest -Uri "$baseUrl/$archiveName" -OutFile $archivePath

    Write-Info "Downloading checksum manifest..."
    Invoke-WebRequest -Uri "$baseUrl/checksums.txt" -OutFile $checksumsPath

    Write-Info "Verifying archive checksum..."
    Verify-Checksum -ArchivePath $archivePath -ChecksumsPath $checksumsPath -ArchiveName $archiveName

    $installRoot = Join-Path $RizzoHome "toolchains"
    $targetDir = Join-Path $installRoot $resolvedVersion
    $currentDir = Join-Path $RizzoHome "current"

    New-Item -ItemType Directory -Path $installRoot -Force | Out-Null
    New-Item -ItemType Directory -Path $RizzoBinDir -Force | Out-Null

    if ((Test-Path -LiteralPath $targetDir) -and -not $Force) {
        Write-Warn "Version already installed: $targetDir"
    }
    else {
        if (Test-Path -LiteralPath $targetDir) {
            Remove-Item -LiteralPath $targetDir -Recurse -Force
        }

        Write-Info "Extracting toolchain into $targetDir..."
        Expand-Archive -LiteralPath $archivePath -DestinationPath $expandedRoot -Force
        $archiveRoot = Resolve-ArchiveRoot -ExpandedRoot $expandedRoot -Arch $arch

        New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
        Copy-Item -Path (Join-Path $archiveRoot "*") -Destination $targetDir -Recurse -Force
    }

    New-CurrentJunction -CurrentPath $currentDir -TargetPath $targetDir

    $currentBinDir = Join-Path $currentDir "bin"
    Write-Launcher -LauncherPath (Join-Path $RizzoBinDir "rizzoc.cmd") -TargetPath (Join-Path $currentBinDir "rizzoc.exe")
    Write-Launcher -LauncherPath (Join-Path $RizzoBinDir "rizzolsp.cmd") -TargetPath (Join-Path $currentBinDir "rizzolsp.exe")

    $pathUpdated = Ensure-PathEntry -BinPath $RizzoBinDir

    Write-Info "Installed toolchain: $targetDir"
    Write-Info "Updated current junction: $currentDir"
    Write-Info "Created launchers: $(Join-Path $RizzoBinDir 'rizzoc.cmd') and $(Join-Path $RizzoBinDir 'rizzolsp.cmd')"

    if ($pathUpdated) {
        Write-Info "Installation complete. New terminals can run: rizzoc --version"
    }
    else {
        Write-Warn "Installation complete, but $RizzoBinDir is not on the user PATH."
        Write-Host "Add it manually, or rerun without -NoPathUpdate."
    }
}
finally {
    if (Test-Path -LiteralPath $tempRoot) {
        Remove-Item -LiteralPath $tempRoot -Recurse -Force
    }
}