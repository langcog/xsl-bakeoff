#!/usr/bin/env bash
# Run the priming (learning-trajectory) model comparison: see priming.R.
# Usage (from xsl-bakeoff/):  nohup bash fits/v05/priming/run_priming.sh [N_WORKERS] &
# Resumable: every phase caches per model; just run it again.
set -u
cd "$(dirname "$0")/../../.."
D=fits/v05/priming
LOG=$D/priming.log
N=${1:-10}
caffeinate -i -w $$ &

# XSLmodels master (multi-block support) in a private library, for the fit phase
if [ ! -f "$D/XSLMODELS_COMMIT" ]; then
  COMMIT=$(git -C ../XSLmodels rev-parse master)
  SRC="$(mktemp -d)/XSLmodels"; mkdir -p "$SRC" "$D/Rlib"
  git -C ../XSLmodels archive "$COMMIT" | tar -x -C "$SRC"
  R CMD INSTALL --no-test-load --library="$D/Rlib" "$SRC" >> "$LOG" 2>&1 || exit 1
  echo "$COMMIT" > "$D/XSLMODELS_COMMIT"
fi
[ -f "$D/kachergis_priming.rds" ] ||
  R_LIBS="$(pwd)/$D/Rlib" Rscript -e "saveRDS(XSLmodels::kachergis_priming, '$D/kachergis_priming.rds')"

echo "$(date '+%F %T') start: N_WORKERS=$N, fit library XSLmodels $(cut -c1-7 $D/XSLMODELS_COMMIT)" >> "$LOG"
R_LIBS="$(pwd)/fits/v05/Rlib"        Rscript $D/priming.R generalize        >> "$LOG" 2>&1
R_LIBS="$(pwd)/fits/v05/tilles/Rlib" Rscript $D/priming.R generalize tilles >> "$LOG" 2>&1
R_LIBS="$(pwd)/$D/Rlib"              Rscript $D/priming.R summary           >> "$LOG" 2>&1
echo "$(date '+%F %T') generalize phase exited" >> "$LOG"
R_LIBS="$(pwd)/$D/Rlib" N_WORKERS=$N Rscript $D/priming.R fit               >> "$LOG" 2>&1
echo "$(date '+%F %T') fit phase exited ($?)" >> "$LOG"
R_LIBS="$(pwd)/$D/Rlib"              Rscript $D/priming.R summary           >> "$LOG" 2>&1
echo "$(date '+%F %T') done" >> "$LOG"
