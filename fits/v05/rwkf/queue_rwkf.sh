#!/usr/bin/env bash
# Fixed rescorla-wagner and the error-driven redesigns (specs.R's RWKF_MODELS)
# against XSLmodels master after PRs #41-#42, then their downstream analyses
# (2026-11-05):
#   1. install that XSLmodels into fits/v05/rwkf/Rlib
#   2. group fit + 5-fold CV, merged into fits/v05/adults/      (refit_rwkf.R)
#   3. noise ceiling's model table                               (v05 library)
#   4. choice-level results, then the full tables                (rwkf library)
#   5. generalization to held-out data                           (rwkf library)
#   6. kachergis_priming: generalization, direct fits, summary   (rwkf library)
# Parameter recovery for these models is not included.
# Stops at the first failure. Log: fits/v05/rwkf/queue.log
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v05/rwkf/queue_rwkf.sh > /dev/null 2>&1 &
set -euo pipefail
cd "$(dirname "$0")/../../.."
D=fits/v05/rwkf
LOG=$D/queue.log
V05_LIB="$(pwd)/fits/v05/Rlib"
RWKF_LIB="$(pwd)/$D/Rlib"
COMMIT="${XSLMODELS_COMMIT:-081015c}"
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
run refit         env R_LIBS="$RWKF_LIB" N_WORKERS="$N_WORKERS" Rscript $D/refit_rwkf.R
run noise_ceiling env R_LIBS="$V05_LIB" Rscript fits/v05/extras/noise_ceiling.R
run choice_level  env R_LIBS="$RWKF_LIB" N_CORES=4 Rscript fits/v05/extras/choice_level.R \
  rescorla-wagner rescorla-wagner_softmax rescorla-wagner_discriminative kalman_filter_full baseline_softmax
run generalize    env R_LIBS="$RWKF_LIB" Rscript fits/v05/generalize.R

# priming: the cached rescorla-wagner fit used the buggy model; archive it
P=fits/v05/priming
mkdir -p "$P/fits/superseded_buggy_rw"
[ -e "$P/fits/rescorla-wagner.rds" ] && mv "$P/fits/rescorla-wagner.rds" "$P/fits/superseded_buggy_rw/"
run priming_generalize env R_LIBS="$RWKF_LIB" Rscript $P/priming.R generalize
run priming_fit        env R_LIBS="$RWKF_LIB" N_WORKERS="$N_WORKERS" Rscript $P/priming.R fit
run priming_summary    env R_LIBS="$RWKF_LIB" Rscript $P/priming.R summary
say "queue finished"
