# Stage 3, part 1: each model's behaviour across its parameter space.
#
# For every model, a Latin-hypercube sample of parameter vectors within its
# fitting bounds (fits/v05/specs.R, fits/v06/yf2015/specs_yf2015.R), plus its
# group fit as point 0. At each point, the model's predictions on
#   - all 924 xsl_datasets test items (raw accuracy, as xsl_run() scores it)
#   - the 64 Yurovsky & Frank (2015) cells (raw p_target for the 48 Exp 1-2
#     cells, then Exp 3's 8 target and 8 competitor cells), scored as in
#     score_yf_all.R, without a lapse (stage3_analyse.R adds one)
# yf2015 variants run in mean-field mode on xsl_datasets (as fit) and sampling
# mode on YF (as scored), as in stages 1-2.
#
# Sampling: a parameter is sampled on a log scale when its bounds are
# positive, span two or more orders of magnitude, and its upper bound exceeds
# 1 (scale parameters: fazly's beta, Kalman variances, softmax phi, uncfam's
# X and B, ...); otherwise uniformly. Probabilities and rates in [0, 1] are
# sampled uniformly.
#
# Stochastic models are simulated with N_SIM learners per point (default 50).
# For them, points 0-10 are run a second time with independent seeds, to
# estimate how far a cloud point is from itself by simulation noise alone.
#
# Points: N_DET (default 500) per model deterministic on both datasets,
# N_STO (default 200) for models stochastic on xsl_datasets, N_YF (default
# 300) for the yf2015 variants (stochastic on YF only).
#
# Output: fits/v06/clusters/stage3_clouds/<model>__<chunk>.rds, one per chunk
# of CHUNK points (skipped if present, so an interrupted run resumes).
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib N_CORES=12 Rscript fits/v06/clusters/stage3_sweep.R [model ...]

suppressPackageStartupMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
})
YF <- "fits/v06/yf2015"
OUT <- Sys.getenv("OUT", "fits/v06/clusters/stage3_clouds")
if (!startsWith(find.package("XSLmodels"), normalizePath(file.path(YF, "Rlib")))) {
  stop("run with R_LIBS=$(pwd)/fits/v06/yf2015/Rlib (found ", find.package("XSLmodels"), ")")
}
dir.create(OUT, showWarnings = FALSE)
source("fits/v05/specs.R")
source(file.path(YF, "specs_yf2015.R"))
source(file.path(YF, "yf_scoring.R"))
N_SIM <- as.integer(Sys.getenv("N_SIM", "50"))
N_DET <- as.integer(Sys.getenv("N_DET", "500"))
N_STO <- as.integer(Sys.getenv("N_STO", "200"))
N_YF <- as.integer(Sys.getenv("N_YF", "300"))
CHUNK <- 25L
N_REP <- 10L
N_CORES <- as.integer(Sys.getenv("N_CORES", "12"))
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

specs <- c(specs_v04, specs_yf2015)
g <- new.env(); load("fits/v05/adults/group_fits.Rdata", g)
load(file.path(YF, "yf2015_fits.Rdata"))  # prog
group_par <- function(m) {
  if (m %in% names(g$group_fits)) unname(g$group_fits[[m]]$optim$bestmem)
  else unname(prog[[m]]$group$best$optim$bestmem)
}
models <- c(names(specs_v04), names(specs_yf2015))
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) { stopifnot(all(args %in% models)); models <- args }

n_points <- function(m) {
  if (length(specs[[m]]$lower) == 0) return(0L)  # baseline: the group fit only
  if (specs[[m]]$stochastic) N_STO else if (m %in% YF_MODELS) N_YF else N_DET
}
log_scale <- function(lo, up) lo > 0 & up / lo >= 100 & up > 1

# Latin hypercube in the unit cube, mapped to the bounds; row 1 is the group fit
sample_points <- function(m) {
  lo <- unname(specs[[m]]$lower); up <- unname(specs[[m]]$upper); n <- n_points(m)
  P <- matrix(group_par(m), nrow = 1)
  if (n == 0) return(P)
  set.seed(match(m, names(specs)))
  U <- sapply(seq_along(lo), \(j) (sample(n) - runif(n)) / n)
  if (is.null(dim(U))) U <- matrix(U, ncol = length(lo))
  X <- sapply(seq_along(lo), \(j) {
    if (log_scale(lo[j], up[j])) exp(log(lo[j]) + U[, j] * (log(up[j]) - log(lo[j])))
    else lo[j] + U[, j] * (up[j] - lo[j])
  })
  if (is.null(dim(X))) X <- matrix(X, ncol = length(lo))
  rbind(P, X)
}

xsl_vec <- function(mod, seed) {
  set.seed(seed)
  ctl <- if (mod$stochastic) xslControl(n_sim = N_SIM) else xslControl()
  unlist(map(xsl_run(mod, xsl_datasets, control = ctl)$fits, \(f) unname(f$perf)))
}
yf_vec <- function(mod, seed_offset) {
  if (identical(mod$name, "yf2015")) { mod$params$sampling <- TRUE; mod$stochastic <- TRUE }
  p <- score_model(mod, n_sim = N_SIM, seed_offset = seed_offset)
  c(p$e12$p_target, p$e3$p_target, p$e3$p_competitor)
}
run_point <- function(m, par, i, replicate = FALSE) {
  mod <- specs[[m]]$constructor(par)
  off <- if (replicate) 7e6 else 0
  seed <- 1e5 * match(m, names(specs)) + i + off
  x <- tryCatch(xsl_vec(mod, seed), error = \(e) rep(NA_real_, 924))
  y <- tryCatch(yf_vec(mod, seed + 1e4), error = \(e) rep(NA_real_, 64))
  list(xsl = x, yf = y)
}

run_chunk <- function(job) {
  path <- file.path(OUT, sprintf("%s__%03d.rds", job$model, job$chunk))
  if (file.exists(path)) return(invisible(NULL))
  t0 <- Sys.time()
  P <- job$P; m <- job$model
  stoch_xsl <- specs[[m]]$stochastic
  stoch_yf <- stoch_xsl || m %in% YF_MODELS
  res <- map(seq_len(nrow(P)), \(r) {
    i <- job$idx[r]
    main <- run_point(m, P[r, ], i)
    rep <- if ((stoch_xsl || stoch_yf) && i <= N_REP) run_point(m, P[r, ], i, replicate = TRUE) else NULL
    list(point = i, par = P[r, ], xsl = main$xsl, yf = main$yf, rep_xsl = rep$xsl, rep_yf = rep$yf)
  })
  saveRDS(list(model = m, chunk = job$chunk, stoch_xsl = stoch_xsl, stoch_yf = stoch_yf,
               n_sim = N_SIM, points = res), path)
  stamp(sprintf("%s chunk %d: %d points, %.1f min", m, job$chunk, nrow(P),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

jobs <- list()
for (m in models) {
  P <- sample_points(m)
  idx <- seq_len(nrow(P)) - 1L  # point 0 = group fit
  for (ch in split(seq_len(nrow(P)), ceiling(seq_len(nrow(P)) / CHUNK))) {
    jobs[[length(jobs) + 1]] <- list(model = m, chunk = ceiling(ch[1] / CHUNK), P = P[ch, , drop = FALSE], idx = idx[ch],
                                     cost = if (specs[[m]]$stochastic) 3 else if (m %in% YF_MODELS) 2 else 1)
  }
}
jobs <- jobs[order(-map_dbl(jobs, "cost"))]  # slowest first, for load balance
stamp(sprintf("%d models, %d chunks (%d points), %d cores", length(models), length(jobs),
              sum(map_int(jobs, \(j) nrow(j$P))), N_CORES))
res <- parallel::mclapply(jobs, \(j) tryCatch(run_chunk(j), error = \(e) paste(j$model, j$chunk, conditionMessage(e))),
                          mc.cores = N_CORES, mc.preschedule = FALSE)
walk(res, \(r) if (is.character(r)) stamp("FAILED ", r))
stamp("done")
