#!/usr/bin/env bash
# Replaces queue_recovery.sh's remaining steps (2026-10-15): wait for the
# running recovery.R param process (PID given) to exit, rerun the param fits
# whose draws changed with the draw_par() fix (recovery.R param only does
# fits that have no saved job; the superseded ones were moved to
# recovery/superseded_2026-10-15/), then run the model phase.
# Usage (from xsl-bakeoff/): nohup bash fits/v05/extras/queue_recovery_rerun.sh PID [N_CORES] &
set -u
cd "$(dirname "$0")/../../.."
PID=$1; N=${2:-4}
LOG=fits/v05/extras/recovery_queue.log
echo "$(date '+%F %T') rerun queue: waiting for recovery param (PID $PID) to exit, then param rerun + model (N_CORES=$N)" >> "$LOG"
while kill -0 "$PID" 2>/dev/null; do sleep 120; done
export R_LIBS="$(pwd)/fits/v05/Rlib" N_CORES=$N
for phase in param model; do
  echo "$(date '+%F %T') recovery $phase start" >> "$LOG"
  caffeinate -i Rscript fits/v05/extras/recovery.R $phase >> "$LOG" 2>&1
  echo "$(date '+%F %T') recovery $phase exited ($?)" >> "$LOG"
done
