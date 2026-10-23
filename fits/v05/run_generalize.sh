#!/usr/bin/env bash
# v05 generalization: every model except tilles with the v05 pin, then tilles
# with its corrected model (see fits/v05/generalize.R). Resumable.
# Usage (from xsl-bakeoff/):  bash fits/v05/run_generalize.sh [model ...]
set -euo pipefail
cd "$(dirname "$0")/../.."
R_LIBS="$(pwd)/fits/v05/Rlib"        Rscript fits/v05/generalize.R "$@"
if [ $# -eq 0 ]; then
  R_LIBS="$(pwd)/fits/v05/tilles/Rlib" Rscript fits/v05/generalize.R
fi
