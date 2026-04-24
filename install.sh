#!/usr/bin/env bash
set -euo pipefail

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

REPO_OWNER="${RIZZO_REPO_OWNER:-itu-msc}"
REPO_NAME="${RIZZO_REPO_NAME:-RizzoMemory}"
DEFAULT_RIZZO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/rizzo"
RIZZO_HOME="${RIZZO_HOME:-$DEFAULT_RIZZO_HOME}"
RIZZO_BIN_DIR="${RIZZO_BIN_DIR:-$HOME/.local/bin}"
TMP_DIR=""

VERSION="${RIZZO_VERSION:-}"
FORCE=0

usage() {
  cat <<EOF
Usage: install.sh [--version <tag>] [--force]

Installs the Rizzo toolchain into a managed user directory and links
the active tools into ~/.local/bin by default.

Options:
  --version <tag>   Install a specific tag such as v0.1.2
  --force           Reinstall even if the target version already exists
  --help            Show this help text

Environment overrides:
  RIZZO_VERSION             Same as --version
  RIZZO_HOME                Managed toolchain root
  RIZZO_BIN_DIR             Directory for rizzoc/rizzolsp symlinks
  RIZZO_REPO_OWNER          GitHub owner (default: itu-msc)
  RIZZO_REPO_NAME           GitHub repo (default: RizzoMemory)
  RIZZO_RELEASE_BASE_URL    Exact base URL containing checksums.txt and the archive

Examples:
  curl -fsSL https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.sh | bash
  curl -fsSL https://raw.githubusercontent.com/itu-msc/RizzoMemory/main/install.sh | bash -s -- --version v0.1.2
EOF
}

log() {
  printf '%b\n' "$1"
}

fail() {
  log "${RED}Error:${NC} $1"
  exit 1
}

cleanup() {
  if [ -n "${TMP_DIR:-}" ] && [ -d "$TMP_DIR" ]; then
    rm -rf "$TMP_DIR"
  fi
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || fail "Missing required command: $1"
}

parse_args() {
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --version)
        [ "$#" -ge 2 ] || fail "Missing value for --version"
        VERSION="$2"
        shift 2
        ;;
      --force)
        FORCE=1
        shift
        ;;
      --help|-h)
        usage
        exit 0
        ;;
      *)
        fail "Unknown argument: $1"
        ;;
    esac
  done
}

detect_os() {
  case "$(uname -s | tr '[:upper:]' '[:lower:]')" in
    linux*) printf 'linux' ;;
    darwin*) printf 'darwin' ;;
    *) fail "Unsupported operating system: $(uname -s)" ;;
  esac
}

detect_arch() {
  case "$(uname -m)" in
    x86_64|amd64) printf 'x86_64' ;;
    arm64|aarch64) printf 'arm64' ;;
    *) fail "Unsupported architecture: $(uname -m)" ;;
  esac
}

extract_tag_name() {
  sed -n 's/.*"tag_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' | head -n 1
}

resolve_version() {
  if [ -n "$VERSION" ]; then
    printf '%s' "$VERSION"
    return
  fi

  log "Fetching latest Rizzo release information..." >&2
  local latest_release
  latest_release="$(curl -fsSL --max-time 20 "https://api.github.com/repos/$REPO_OWNER/$REPO_NAME/releases/latest")" \
    || fail "Failed to fetch latest release information from GitHub"

  VERSION="$(printf '%s' "$latest_release" | extract_tag_name)"
  [ -n "$VERSION" ] || fail "Failed to parse release tag from GitHub response"
  printf '%s' "$VERSION"
}

release_base_url() {
  if [ -n "${RIZZO_RELEASE_BASE_URL:-}" ]; then
    printf '%s' "$RIZZO_RELEASE_BASE_URL"
  else
    printf 'https://github.com/%s/%s/releases/download/%s' "$REPO_OWNER" "$REPO_NAME" "$1"
  fi
}

sha256_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
    return
  fi

  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
    return
  fi

  if command -v openssl >/dev/null 2>&1; then
    openssl dgst -sha256 "$1" | awk '{print $NF}'
    return
  fi

  fail "No SHA-256 tool found; install sha256sum, shasum, or openssl"
}

verify_checksum() {
  local archive_path="$1"
  local checksums_path="$2"
  local archive_name="$3"
  local expected
  expected="$(awk -v target="$archive_name" '$2 == target { print $1 }' "$checksums_path")"
  [ -n "$expected" ] || fail "No checksum entry found for $archive_name"

  local actual
  actual="$(sha256_file "$archive_path")"
  [ "$expected" = "$actual" ] || fail "Checksum mismatch for $archive_name"
}

main() {
  parse_args "$@"

  require_command curl
  require_command tar
  require_command mkdir
  require_command ln
  require_command rm
  require_command cp

  local os
  local arch
  local version
  local archive_name
  local base_url
  os="$(detect_os)"
  arch="$(detect_arch)"
  version="$(resolve_version)"
  archive_name="rizzo-toolchain_${os}_${arch}.tar.gz"
  base_url="$(release_base_url "$version")"

  log "Detected platform: ${GREEN}${os}/${arch}${NC}"
  log "Installing Rizzo version: ${GREEN}${version}${NC}"

  TMP_DIR="$(mktemp -d)"
  trap cleanup EXIT

  local archive_path="$TMP_DIR/$archive_name"
  local checksums_path="$TMP_DIR/checksums.txt"

  log "Downloading ${archive_name}..."
  curl -fsSL --max-time 120 "$base_url/$archive_name" -o "$archive_path" \
    || fail "Failed to download $base_url/$archive_name"

  log "Downloading checksum manifest..."
  curl -fsSL --max-time 60 "$base_url/checksums.txt" -o "$checksums_path" \
    || fail "Failed to download $base_url/checksums.txt"

  log "Verifying archive checksum..."
  verify_checksum "$archive_path" "$checksums_path" "$archive_name"

  local install_root="$RIZZO_HOME/toolchains"
  local target_dir="$install_root/$version"
  local extracted_root="$TMP_DIR/rizzo-toolchain_${os}_${arch}"
  local current_link="$RIZZO_HOME/current"

  mkdir -p "$install_root"
  mkdir -p "$RIZZO_BIN_DIR"

  if [ -d "$target_dir" ] && [ "$FORCE" -ne 1 ]; then
    log "${YELLOW}Version already installed:${NC} $target_dir"
  else
    rm -rf "$target_dir"
    log "Extracting toolchain into $target_dir..."
    tar -xzf "$archive_path" -C "$TMP_DIR"
    [ -d "$extracted_root" ] || fail "Archive did not contain expected directory $extracted_root"
    mkdir -p "$target_dir"
    cp -R "$extracted_root/." "$target_dir/"
  fi

  ln -sfn "$target_dir" "$current_link"
  ln -sfn "$current_link/bin/rizzoc" "$RIZZO_BIN_DIR/rizzoc"
  ln -sfn "$current_link/bin/rizzolsp" "$RIZZO_BIN_DIR/rizzolsp"

  log "${GREEN}Installed toolchain:${NC} $target_dir"
  log "${GREEN}Updated current symlink:${NC} $current_link"
  log "${GREEN}Linked executables:${NC} $RIZZO_BIN_DIR/rizzoc and $RIZZO_BIN_DIR/rizzolsp"

  case ":$PATH:" in
    *":$RIZZO_BIN_DIR:"*)
      log "${GREEN}Installation complete.${NC} You can now run: rizzoc --version"
      ;;
    *)
      log "${YELLOW}Installation complete, but $RIZZO_BIN_DIR is not on PATH.${NC}"
      log "Add this to your shell profile: export PATH=\"$RIZZO_BIN_DIR:\$PATH\""
      ;;
  esac
}

main "$@"