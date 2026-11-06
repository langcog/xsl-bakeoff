#!/usr/bin/env bash
# Parameter and model recovery for the fixed rescorla-wagner and the
# error-driven redesigns (specs.R's RWKF_MODELS), under fits/v05/rwkf/Rlib:
# fits/v05/extras/recovery.R with REC_SET=rwkf, output in
# fits/v05/extras/recovery/rwkf/. Resumable. Log: fits/v05/rwkf/recovery.log
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v05/rwkf/queue_recovery.sh [N_CORES] > /dev/null 2>&1 &
set -u
cd "$(dirname "$0")/../../.."
export R_LIBS="$(pwd)/fits/v05/rwkf/Rlib" REC_SET=rwkf N_CORES=${1:-10}
LOG=fits/v05/rwkf/recovery.log
caffeinate -i -w $$ &
for phase in param model; do
  echo "$(date '+%F %T') recovery $phase start (N_CORES=$N_CORES)" >> "$LOG"
  Rscript fits/v05/extras/recovery.R $phase >> "$LOG" 2>&1
  echo "$(date '+%F %T') recovery $phase exited ($?)" >> "$LOG"
done
