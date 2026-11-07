#!/usr/bin/env bash
# Tilles refit against XSLmodels b44b9fa, then its downstream analyses (2026-10-19):
#   1. group fit + 5-fold CV, merged into fits/v05/adults/   (refit_tilles.R)
#   2. noise ceiling's model table                            (main v05 library)
#   3. tilles' choice-level results, then the full tables     (tilles library)
#   4. tilles' parameter recovery (old draws archived first)  (tilles library)
# Stops at the first failure. Log: fits/v05/tilles/queue.log
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v05/tilles/queue_tilles.sh > /dev/null 2>&1 &
set -euo pipefail
cd "$(dirname "$0")/../../.."
LOG=fits/v05/tilles/queue.log
V05_LIB="$(pwd)/fits/v05/Rlib"
TIL_LIB="$(pwd)/fits/v05/tilles/Rlib"
N_WORKERS="${N_WORKERS:-8}"
say() { echo "$(date '+%F %T') $*" >> "$LOG"; }
run() { local name="$1"; shift; say "start: $name"; if "$@" >> "fits/v05/tilles/${name}.log" 2>&1; then say "done: $name"; else say "FAILED: $name (see fits/v05/tilles/${name}.log)"; exit 1; fi; }

say "queue start (N_WORKERS=$N_WORKERS)"
run refit        env R_LIBS="$TIL_LIB" N_WORKERS="$N_WORKERS" caffeinate -i Rscript fits/v05/tilles/refit_tilles.R
run noise_ceiling env R_LIBS="$V05_LIB" Rscript fits/v05/extras/noise_ceiling.R
run choice_level env R_LIBS="$TIL_LIB" N_CORES=2 Rscript fits/v05/extras/choice_level.R tilles

# archive the old-model tilles parameter-recovery draws, so they're regenerated
OLD=fits/v05/extras/recovery/superseded_tilles_2026-10-19
mkdir -p "$OLD/jobs" "$OLD/synth"
for f in fits/v05/extras/recovery/jobs/param__tilles__*.rds;  do [ -e "$f" ] && mv "$f" "$OLD/jobs/";  done
for f in fits/v05/extras/recovery/synth/param__tilles__*.rds; do [ -e "$f" ] && mv "$f" "$OLD/synth/"; done
say "archived old tilles param-recovery files to $OLD"
run recovery_param env R_LIBS="$TIL_LIB" PARAM_ONLY=tilles N_CORES="$N_WORKERS" caffeinate -i Rscript fits/v05/extras/recovery.R param
say "queue finished"
