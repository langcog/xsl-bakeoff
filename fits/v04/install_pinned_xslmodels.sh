#!/usr/bin/env bash
# Install a PINNED XSLmodels commit into a private library for the round-2
# (v0.4) refit, so (a) the fits record exactly which package version made
# them, and (b) installing doesn't touch the system-wide XSLmodels that other
# running R jobs may have loaded (replacing a lazy-load DB under a live
# session can corrupt it).
#
# Usage (from xsl-bakeoff/):  bash fits/v04/install_pinned_xslmodels.sh [commit]
# Then run fit scripts with:  R_LIBS=fits/v04/Rlib Rscript fits/v04/refit.R ...
# (R_LIBS, not .libPaths(), so DEoptim's PSOCK workers inherit it too.)
set -euo pipefail
COMMIT="${1:-d0e2230}"
PKG_REPO="../XSLmodels"
LIB="fits/v04/Rlib"
SRC="$(mktemp -d)/XSLmodels"

mkdir -p "$LIB" "$SRC"
git -C "$PKG_REPO" archive "$COMMIT" | tar -x -C "$SRC"
R CMD INSTALL --no-test-load --library="$LIB" "$SRC"
git -C "$PKG_REPO" rev-parse "$COMMIT" > fits/v04/XSLMODELS_COMMIT
echo "Installed XSLmodels @ $(cat fits/v04/XSLMODELS_COMMIT) into $LIB"
