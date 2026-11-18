#!/usr/bin/env bash
# Score every v05 model on yurovsky_frank2015 now (score_yf_all.R), then wait
# for queue_yf2015.sh (PID $1) to finish the YF refit and score yf2015 and
# yf2015_me too (cached models are skipped). Log: fits/v06/yf2015/score_queue.log
#
# Usage (from xsl-bakeoff/):  nohup bash fits/v06/yf2015/score_queue.sh <queue pid> > /dev/null 2>&1 &
set -uo pipefail
cd "$(dirname "$0")/../../.."
D=fits/v06/yf2015
LOG=$D/score_queue.log
LIB="$(pwd)/$D/Rlib"
say() { echo "$(date '+%F %T') $*" >> "$LOG"; }
caffeinate -i -w $$ &
say "scoring v05 models"
R_LIBS="$LIB" N_CORES="${N_CORES:-4}" Rscript $D/score_yf_all.R >> $D/score.log 2>&1; say "v05 models: exit $?"
if [ -n "${1:-}" ]; then
  while kill -0 "$1" 2>/dev/null; do sleep 60; done
  say "queue_yf2015.sh finished; scoring the YF models"
fi
R_LIBS="$LIB" N_CORES="${N_CORES:-4}" Rscript $D/score_yf_all.R >> $D/score.log 2>&1; say "all models: exit $?"
