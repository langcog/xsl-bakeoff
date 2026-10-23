#!/usr/bin/env bash
# Wait for the v05 group phase to finish (run_v05.sh logs "group phase
# exited"), check all 21 group fits exist at the pinned commit, then run
# recovery.R param and model on N_CORES cores (alongside the CV phase's
# workers). Usage (from xsl-bakeoff/): nohup bash fits/v05/extras/queue_recovery.sh [N_CORES] &
set -u
cd "$(dirname "$0")/../../.."
N=${1:-4}
LOG=fits/v05/extras/recovery_queue.log
QLOG=fits/v05/refit_queue.log
START=$(wc -l < "$QLOG")
echo "$(date '+%F %T') waiting for the group phase (N_CORES=$N)" >> "$LOG"
until tail -n +"$((START + 1))" "$QLOG" | grep -q "group phase exited"; do sleep 300; done
export R_LIBS="$(pwd)/fits/v05/Rlib" N_CORES=$N
if ! Rscript -e 'suppressMessages(library(XSLmodels)); load("fits/v05/adults/group_fits.Rdata"); source("fits/v05/specs.R");
  stopifnot(setequal(names(group_fits), RUN_ORDER),
            meta$xslmodels_commit == trimws(readLines("fits/v05/XSLMODELS_COMMIT")))' >> "$LOG" 2>&1; then
  echo "$(date '+%F %T') group fits incomplete or wrong commit; recovery NOT started" >> "$LOG"; exit 1
fi
for phase in param model; do
  echo "$(date '+%F %T') recovery $phase start" >> "$LOG"
  caffeinate -i Rscript fits/v05/extras/recovery.R $phase >> "$LOG" 2>&1
  echo "$(date '+%F %T') recovery $phase exited ($?)" >> "$LOG"
done
