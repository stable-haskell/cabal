#!/usr/bin/env bash
# Bootstrap cabal-install (exe:cabal) using MicroHs (mhs 0.16.12.0).
#
# Usage:
#   bootstrap/mhs/build-cabal-mhs.sh [toolchain|build|test|all]
#
# Environment variables:
#   MHS_PREFIX     Installation directory for MicroHs + packages (default: $HOME/.mcabal)
#   MHS_SRC_DIR    Checkout directory for augustss/MicroHs (default: _build/mhs-src)
#   MHS_CACHE_DIR  Directory for staged .mhscache checkpoints (default: _build/mhs-cache)
#   ARTIFACT_DIR   Output directory for the linked cabal binary (default: _build/artifacts)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"

MHS_COMMIT="9e1cd08"
MHS_VERSION="0.16.12.0"
MHS_PREFIX="${MHS_PREFIX:-$HOME/.mcabal}"
MHS_SRC_DIR="${MHS_SRC_DIR:-$REPO_ROOT/_build/mhs-src}"
MHS_CACHE_DIR="${MHS_CACHE_DIR:-$REPO_ROOT/_build/mhs-cache}"
MHS_AUTOGEN_DIR="${MHS_AUTOGEN_DIR:-$REPO_ROOT/_build/mhs-autogen}"
MHS_WORK_DIR="${MHS_WORK_DIR:-$REPO_ROOT/_build/mhs-work}"
ARTIFACT_DIR="${ARTIFACT_DIR:-$REPO_ROOT/_build/artifacts}"

MHS_BIN="$MHS_PREFIX/bin/mhs"
MCABAL_BIN="$MHS_PREFIX/bin/mcabal"
MHS_PKG_DIR="$MHS_PREFIX/mhs-$MHS_VERSION/packages"
TOOLCHAIN_STAMP="$MHS_PKG_DIR/.cabal-mhs-toolchain-v2.stamp"

export PATH="$MHS_PREFIX/bin:/usr/bin:/bin:$HOME/.mcabal/bin:$PATH"

build_toolchain() {
  if [ -x "$MHS_BIN" ] && [ -f "$TOOLCHAIN_STAMP" ]; then
    echo "=== [toolchain] Reusing cached MicroHs $MHS_VERSION toolchain in $MHS_PREFIX ==="
    "$MHS_BIN" --version
  else
    echo "=== [toolchain] Building patched MicroHs $MHS_VERSION ($MHS_COMMIT) ==="
    rm -rf "$MHS_SRC_DIR"
    mkdir -p "$(dirname "$MHS_SRC_DIR")"
    git clone https://github.com/augustss/MicroHs.git "$MHS_SRC_DIR"
    git -C "$MHS_SRC_DIR" checkout "$MHS_COMMIT"
    git -C "$MHS_SRC_DIR" apply "$REPO_ROOT/bootstrap/mhs/microhs-full.patch"

    # 1. Build and install stage-0 mhs, cpphs, mcabal, runtime, and base from upstream C sources
    make -C "$MHS_SRC_DIR" MCABAL="$MHS_PREFIX" install

    # 2. Self-compile the patched MicroHs compiler (src/*/*.hs) into C using stage-0 bin/mhs
    echo "=== [toolchain] Self-compiling patched MicroHs.Main ==="
    (
      cd "$MHS_SRC_DIR"
      ./bin/mhs -z -i -imhs -isrc -ilib MicroHs.Main -ogenerated/mhs-patched.c
      cc -Wall -O3 -DHEAP_CELLS=350000000 -DSTACK_SIZE=32000000 \
        -Isrc/runtime -Isrc/runtime/unix \
        src/runtime/main.c src/runtime/eval.c generated/mhs-patched.c \
        -lm -o "$MHS_BIN"
    )
    "$MHS_BIN" --version

    # 3. Rebuild base-0.16.12.0.pkg uncompressed with patched mhs
    echo "=== [toolchain] Building uncompressed base-$MHS_VERSION.pkg ==="
    (
      cd "$MHS_SRC_DIR/lib"
      CABALDIR="$MHS_PREFIX" "$MCABAL_BIN" install
    )

    # 4. Fetch Stackage snapshot and install standard pure-Haskell dependencies via mcabal
    echo "=== [toolchain] Installing standard dependencies via mcabal ==="
    mkdir -p "$MHS_WORK_DIR"
    (
      cd "$MHS_WORK_DIR"
      CABALDIR="$MHS_PREFIX" "$MCABAL_BIN" update
      for pkg in containers parsec filepath time async random; do
        echo "--- Installing $pkg ---"
        CABALDIR="$MHS_PREFIX" "$MCABAL_BIN" -r install "$pkg"
      done
    )
    touch "$TOOLCHAIN_STAMP"
  fi

  # 5. Build and install mhs-compat-shims-0.1.0.0.pkg from mhs-compat-shims/
  echo "=== [toolchain] Building and installing mhs-compat-shims-0.1.0.0.pkg ==="
  mkdir -p "$MHS_WORK_DIR"
  local mhs_dir="$MHS_PREFIX/mhs-$MHS_VERSION"
  rm -f "$MHS_PKG_DIR"/pretty-mhs-*.pkg \
        "$MHS_PKG_DIR"/uri-mhs-*.pkg \
        "$MHS_PKG_DIR"/binary-mhs-*.pkg \
        "$MHS_PKG_DIR"/text-encoding-mhs-*.pkg
  "$MHS_BIN" +RTS -H350M -K32M -RTS -Pmhs-compat-shims-0.1.0.0 \
    -i"$REPO_ROOT/mhs-compat-shims/src" \
    -I"$REPO_ROOT/mhs-compat-shims/cbits" \
    Data.Binary Data.Binary.Get Data.Binary.Put \
    Text.PrettyPrint \
    Network.URI \
    Data.Text.Encoding.Error Data.Text.Internal.Encoding.Utf8 Data.Text.Lazy.Encoding \
    Distribution.Compat.MhsInstances \
    Distribution.Compat.Internal.MhsPosix \
    Distribution.Compat.Internal.MhsProcess \
    Distribution.Client.Compat.Base16 \
    -o "$MHS_WORK_DIR/mhs-compat-shims-0.1.0.0.pkg"
  "$MHS_BIN" -Q "$MHS_WORK_DIR/mhs-compat-shims-0.1.0.0.pkg" "$mhs_dir"

  ls -lh "$MHS_PKG_DIR"/*.pkg
}

generate_autogen() {
  mkdir -p "$MHS_AUTOGEN_DIR"
  cat > "$MHS_AUTOGEN_DIR/Paths_Cabal.hs" <<'EOF'
module Paths_Cabal (version, getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir) where
import Data.Version (Version(..))
version :: Version
version = Version [3,19,0,0] []
getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = return "/private/tmp/mhs-lane-c/bin"
getLibDir = return "/private/tmp/mhs-lane-c/lib"
getDynLibDir = return "/private/tmp/mhs-lane-c/lib"
getDataDir = return "/private/tmp/mhs-lane-c/data"
getLibexecDir = return "/private/tmp/mhs-lane-c/libexec"
getSysconfDir = return "/private/tmp/mhs-lane-c/etc"
EOF

  cat > "$MHS_AUTOGEN_DIR/Paths_cabal_install.hs" <<'EOF'
module Paths_cabal_install (version, getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir) where
import Data.Version (Version(..))
version :: Version
version = Version [3,19,0,0] []
getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = return "/private/tmp/mhs-lane-e/bin"
getLibDir = return "/private/tmp/mhs-lane-e/lib"
getDynLibDir = return "/private/tmp/mhs-lane-e/lib"
getDataDir = return "/private/tmp/mhs-lane-e/data"
getLibexecDir = return "/private/tmp/mhs-lane-e/libexec"
getSysconfDir = return "/private/tmp/mhs-lane-e/etc"
EOF
}

build_cabal_mhs() {
  generate_autogen
  mkdir -p "$MHS_CACHE_DIR" "$MHS_WORK_DIR" "$ARTIFACT_DIR"

  local active_cache="$MHS_CACHE_DIR/active.mhscache"
  local last_good="$MHS_CACHE_DIR/last-good.mhscache"

  local flags=(
    -v
    "-C$active_cache"
    -iCabal-syntax/src -iCabal/src -ihooks-exe/cli -ihooks-exe/src
    -icabal-install-solver/src -icabal-install-solver/src-assertion
    -icabal-install/src -icabal-install/main
    "-i$MHS_AUTOGEN_DIR"
    -ICabal-syntax/src -ICabal/src -Icabal-install/src
    -Imhs-compat-shims/cbits
    -DBOOTSTRAPPED_CABAL=1
  )

  local flags_ro=(
    -v
    "-CR$active_cache"
    -iCabal-syntax/src -iCabal/src -ihooks-exe/cli -ihooks-exe/src
    -icabal-install-solver/src -icabal-install-solver/src-assertion
    -icabal-install/src -icabal-install/main
    "-i$MHS_AUTOGEN_DIR"
    -ICabal-syntax/src -ICabal/src -Icabal-install/src
    -Imhs-compat-shims/cbits
    -DBOOTSTRAPPED_CABAL=1
  )

  step_mhs() {
    local name="$1"
    local mod="$2"
    local done_stamp="$MHS_CACHE_DIR/${name}.done"
    if [ -f "$done_stamp" ] && [ -s "$last_good" ]; then
      echo "=== [$name] Reusing completed checkpoint ($mod) ==="
      return 0
    fi
    echo "=== [$name] Compiling $mod ($(date '+%H:%M:%S')) ==="
    if [ -s "$last_good" ]; then
      cp "$last_good" "$active_cache"
    else
      rm -f "$active_cache"
    fi
    "$MHS_BIN" +RTS -H350M -K32M -RTS "${flags[@]}" -c -o "$MHS_WORK_DIR/${name}.c" "$mod"
    if [ -s "$active_cache" ]; then
      mv "$active_cache" "$last_good"
      touch "$done_stamp"
      rm -f "$MHS_WORK_DIR/${name}.c"
      ls -lh "$last_good"
    else
      echo "ERROR: cache empty after $name ($mod)"
      exit 1
    fi
  }

  # Stage 1: Cabal-syntax (127 modules)
  step_mhs stage1             Distribution.PackageDescription.Parsec

  # Stage 2: Cabal + hooks-exe (167 modules)
  step_mhs stage2a            Distribution.Simple.Configure
  step_mhs stage2c            Distribution.Simple
  step_mhs stage2d            Distribution.Simple.SetupHooks.HooksMain

  # Stage 3: cabal-install-solver (53 modules)
  step_mhs stage3             Distribution.Solver.Modular

  # Stage 4: cabal-install (181 modules)
  step_mhs stage4_ciflags     Distribution.Client.CmdInstall.ClientInstallFlags
  step_mhs stage4_pflags      Distribution.Client.ProjectFlags
  step_mhs stage4_pkghash     Distribution.Client.PackageHash
  step_mhs stage4_inlib       Distribution.Client.InLibrary
  step_mhs stage4_spol        Distribution.Client.ProjectPlanning.SetupPolicy
  step_mhs stage4_pplan       Distribution.Client.ProjectPlanning
  step_mhs stage4_cmain       Distribution.Client.Main

  echo "=== [link] Linking $ARTIFACT_DIR/cabal ($(date '+%H:%M:%S')) ==="
  if [ ! -s "$active_cache" ] && [ -s "$last_good" ]; then
    cp "$last_good" "$active_cache"
  fi

  local link_opts=(-optl -lz)
  if [ "$(uname -s)" = "Darwin" ]; then
    link_opts+=(-optl -Wl,-stack_size,0x10000000)
  else
    link_opts+=(-optl -lcrypto)
  fi

  "$MHS_BIN" +RTS -H350M -K32M -RTS "${flags_ro[@]}" \
    mhs-compat-shims/cbits/mhs_posix.c \
    mhs-compat-shims/cbits/mhs_spawn.c \
    mhs-compat-shims/cbits/mhs_handle.c \
    mhs-compat-shims/cbits/mhs_sha256.c \
    "${link_opts[@]}" \
    -o "$ARTIFACT_DIR/cabal" \
    Main

  rm -f "$active_cache"
  ls -lh "$ARTIFACT_DIR/cabal"
}

run_smoke_tests() {
  local cabal_bin="$ARTIFACT_DIR/cabal"
  echo "=== [test] Smoke-testing $cabal_bin ==="
  "$cabal_bin" --version
  "$cabal_bin" --numeric-version
  "$cabal_bin" build --help | grep -E "build-compiler|with-compiler"
  (cd "$REPO_ROOT/Cabal-syntax" && "$cabal_bin" check)

  echo "=== [test] Compiling and running Cabal/tests/ProcessCaptureTests.hs with mhs ==="
  mkdir -p "$MHS_WORK_DIR"
  local active_cache="$MHS_CACHE_DIR/active.mhscache"
  local last_good="$MHS_CACHE_DIR/last-good.mhscache"
  if [ -s "$last_good" ]; then
    cp "$last_good" "$active_cache"
  fi
  "$MHS_BIN" +RTS -H350M -K32M -RTS \
    "-CR$active_cache" \
    -iCabal-syntax/src -iCabal/src -iCabal/tests \
    -ICabal-syntax/src -ICabal/src -Imhs-compat-shims/cbits \
    mhs-compat-shims/cbits/mhs_posix.c \
    mhs-compat-shims/cbits/mhs_spawn.c \
    mhs-compat-shims/cbits/mhs_handle.c \
    -o "$MHS_WORK_DIR/ProcessCaptureTests" \
    Cabal/tests/ProcessCaptureTests.hs
  rm -f "$active_cache"
  "$MHS_WORK_DIR/ProcessCaptureTests"
}

cmd="${1:-all}"
case "$cmd" in
  toolchain)
    build_toolchain
    ;;
  build)
    build_cabal_mhs
    ;;
  test)
    run_smoke_tests
    ;;
  all)
    build_toolchain
    build_cabal_mhs
    run_smoke_tests
    ;;
  *)
    echo "Usage: $0 [toolchain|build|test|all]" >&2
    exit 1
    ;;
esac
