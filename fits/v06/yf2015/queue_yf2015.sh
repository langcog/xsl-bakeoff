#!/usr/bin/env bash
# Trial-level Yurovsky & Frank (2015) model (XSLmodels PR #46):
#   1. install that XSLmodels into fits/v06/yf2015/Rlib
#   2. group fit + 5-fold CV of the YF models            (refit_yf2015.R)
#   3. generalization to yurovsky_frank2015              (generalize_yf.R)
#   4. every model scored on yurovsky_frank2015          (score_yf_all.R)
# Stops at the first failure. Log: fits/v06/yf2015/queue.log. Resumable.
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v06/yf2015/queue_yf2015.sh > /dev/null 2>&1 &
set -euo pipefail
cd "$(dirname "$0")/../../.."
D=fits/v06/yf2015
LOG=$D/queue.log
YF_LIB="$(pwd)/$D/Rlib"
COMMIT="${XSLMODELS_COMMIT:-49d9d61}"
N_WORKERS="${N_WORKERS:-10}"
say() { echo "$(date '+%F %T') $*" >> "$LOG"; }
run() { local name="$1"; shift; say "start: $name"; if "$@" >> "$D/${name}.log" 2>&1; then say "done: $name"; else say "FAILED: $name (see $D/${name}.log)"; exit 1; fi; }
caffeinate -i -w $$ &

say "queue start (N_WORKERS=$N_WORKERS)"
if [ ! -f "$D/XSLMODELS_COMMIT" ]; then
  SRC="$(mktemp -d)/XSLmodels"; mkdir -p "$SRC" "$D/Rlib"
  git -C ../XSLmodels archive "$COMMIT" | tar -x -C "$SRC"
  R CMD INSTALL --no-test-load --library="$D/Rlib" "$SRC" >> "$D/install.log" 2>&1
  git -C ../XSLmodels rev-parse "$COMMIT" > "$D/XSLMODELS_COMMIT"
  say "installed XSLmodels $(cat $D/XSLMODELS_COMMIT)"
fi
run refit      env R_LIBS="$YF_LIB" N_WORKERS="$N_WORKERS" Rscript $D/refit_yf2015.R
run generalize env R_LIBS="$YF_LIB" Rscript $D/generalize_yf.R
run score      env R_LIBS="$YF_LIB" N_CORES=4 Rscript $D/score_yf_all.R
say "queue finished"
