#!/usr/bin/env bash
# v05 generalization: every model with the library that scores it (see
# fits/v05/extras/common.R's model_lib()): the v05 pin, then tilles with its
# corrected model, then the fixed rescorla-wagner and the error-driven
# redesigns (fits/v05/rwkf/). See fits/v05/generalize.R. Resumable.
# Usage (from xsl-bakeoff/):  bash fits/v05/run_generalize.sh
set -euo pipefail
cd "$(dirname "$0")/../.."
R_LIBS="$(pwd)/fits/v05/Rlib"        Rscript fits/v05/generalize.R
R_LIBS="$(pwd)/fits/v05/tilles/Rlib" Rscript fits/v05/generalize.R
R_LIBS="$(pwd)/fits/v05/rwkf/Rlib"   Rscript fits/v05/generalize.R
