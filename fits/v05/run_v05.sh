#!/usr/bin/env bash
# Run the v05 refit: group phase, then 5-fold CV, on all 47 xsl_datasets
# conditions (FIT_SET=adults), against XSLmodels pinned in fits/v05/Rlib.
# Usage (from xsl-bakeoff/):  nohup bash fits/v05/run_v05.sh [N_WORKERS] &
# Resumable: both phases checkpoint (per model / per fold); just run it again.
set -u
cd "$(dirname "$0")/../.."
LOG=fits/v05/refit_queue.log
N=${1:-6}
mkdir -p fits/v05/adults
caffeinate -i -w $$ &
export R_LIBS="$(pwd)/fits/v05/Rlib" N_WORKERS=$N FIT_SET=adults
echo "$(date '+%F %T') v05 start: N_WORKERS=$N, XSLmodels $(cat fits/v05/XSLMODELS_COMMIT | cut -c1-7) (47 conditions)" >> "$LOG"
Rscript fits/v05/refit.R group >> fits/v05/adults/refit_group.log 2>&1
echo "$(date '+%F %T') [adults] group phase exited ($?)" >> "$LOG"
Rscript fits/v05/refit.R cv >> fits/v05/adults/refit_cv.log 2>&1
echo "$(date '+%F %T') [adults] cv phase exited ($?)" >> "$LOG"
