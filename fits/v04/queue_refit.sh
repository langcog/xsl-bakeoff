#!/usr/bin/env bash
# Queue the round-2 refit behind already-running jobs, then run the group
# phase followed by the CV phase.
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v04/queue_refit.sh <pid> [pid ...] &
#   Waits for every listed PID to exit, then sizes the DEoptim worker pool
#   from the cores actually free at that moment (clamped to 4..10) and runs
#   fits/v04/refit.R group, then cv, for each set in $FIT_SETS (default
#   "adults children"). Logs: fits/v04/refit_queue.log, fits/v04/<set>/refit_{group,cv}.log.
#   Resumable: if interrupted, just run it again (with no PIDs).
#
# caffeinate -i keeps the Mac from IDLE-sleeping while this script runs
# (scoped to this process; closing the lid still sleeps).
set -u
cd "$(dirname "$0")/../.."
LOG=fits/v04/refit_queue.log

for p in "$@"; do
  echo "$(date '+%F %T') waiting for PID $p" >> "$LOG"
  while kill -0 "$p" 2>/dev/null; do sleep 60; done
done
# grace period: let the previous job's parallel workers finish shutting down
# (the 2026-10-03 failure started 20 s after the job it queued behind ended)
[ $# -gt 0 ] && sleep 120

ncpu=$(sysctl -n hw.ncpu)
load=$(sysctl -n vm.loadavg | awk '{print int($3 + 0.5)}')   # 5-min load avg ("{ 1m 5m 15m }")
free=$(( ncpu - load ))
N=$(( free < 4 ? 4 : (free > 10 ? 10 : free) ))
echo "$(date '+%F %T') starting refit: N_WORKERS=$N (ncpu=$ncpu, 5-min load=$load)" >> "$LOG"

caffeinate -i -w $$ &
export R_LIBS="$(pwd)/fits/v04/Rlib" N_WORKERS=$N   # absolute: workers must not depend on their cwd
# Which fit sets to run, in order (see refit.R's FIT_SET). Default: the
# adult/child split decided 2026-10-07 -- the 56-condition "all" run is done.
# Logs go next to each set's outputs (fits/v04/<set>/refit_{group,cv}.log).
for set in ${FIT_SETS:-adults children}; do
  dir=fits/v04; [ "$set" != all ] && dir="fits/v04/$set"; mkdir -p "$dir"
  FIT_SET=$set Rscript fits/v04/refit.R group >> "$dir/refit_group.log" 2>&1
  echo "$(date '+%F %T') [$set] group phase exited ($?)" >> "$LOG"
  FIT_SET=$set Rscript fits/v04/refit.R cv    >> "$dir/refit_cv.log"    2>&1
  echo "$(date '+%F %T') [$set] cv phase exited ($?)" >> "$LOG"
done
