# Generalization of every v05 model to Yurovsky & Frank (2015)
# (yurovsky_frank2015): each model's group-fit parameters (fit to the 47
# xsl_datasets conditions) predict the three experiments, with NO refitting.
#
# The paradigm is response-contingent: a word's test shows the object the
# participant chose on its exposure trial (Same), one they didn't (Switch), or
# one they didn't with a new word (New Label). For a model, we take its
# exposure-trial response to be its own choice among that trial's objects,
# from its state right after the trial, by its own choice rule (Luce, ch_dec,
# softmax phi, or remember-or-guess), and condition on it, per simulated
# learner:
#   Same      = sum_o c(o) P(o | word, {o} + novel)
#   Switch    = sum_o c(o) mean_{o' != o} P(o' | word, {o'} + novel)
#   New Label = the same as Switch, with a novel word
# where c(o) is the exposure response distribution and P(. | word, alts) the
# choice at test, from the state just before the test trial. A model whose
# state after exposure is symmetric over the objects (most associative
# models, on a novel word with novel objects) therefore predicts Same =
# Switch.
#
# Training sequences follow the experiments' fixed designs (one per
# condition; the 4 practice trials are dropped):
#   Experiments 1-2: 8 words, each an exposure trial (N objects) and a test
#     trial `interval` trials later, at the positions in the data (identical
#     for every participant in an interval condition). Each test trial is
#     also a training trial, showing exposure object 1 and N - 1 novel ones
#     (which object a participant saw depended on their choice; this only
#     affects later words). Exp. 2's New Label test trials are trained as
#     their word's own trial, too.
#   Experiment 3: 4 blocks of Ex-A (word A, N objects), Ex-B (word B, N
#     objects), T-A (A with one of A's objects, one of B's, and N - 2 novel);
#     the paper puts one Ex-B between each Ex-A and T-A but doesn't give the
#     block order, so blocks are sequential.
#
# Models: every model in fits/v05/adults/group_fits.Rdata (specs.R), plus
# yf2015 and yf2015_me (specs_yf2015.R) once refit_yf2015.R has their group
# fits. All are scored with XSLmodels at fits/v06/yf2015/XSLMODELS_COMMIT,
# which for these models matches the libraries they were fit with (checked:
# `sse_check` re-runs each group fit on xsl_datasets). yf2015 models are fit
# in mean-field mode, whose state after an exposure is symmetric, so here
# they run in sampling mode (their own account of the paradigm, in which the
# response is the sampled hypothesis, is yf2015_paradigm(); see
# generalize_yf.R).
#
# Results are cached per model in fits/v06/yf2015/yf_scores/<model>.rds
# (skipped next time unless its parameters change), and the summary is
# rebuilt from every cached model: fits/v06/yf2015/yf_scores_summary.rds.
#
# Usage (from xsl-bakeoff/):
#   R_LIBS=$(pwd)/fits/v06/yf2015/Rlib N_CORES=4 Rscript fits/v06/yf2015/score_yf_all.R [model ...]
#   ... score_yf_all.R --validate   # check the scorer against yf2015_paradigm()

suppressMessages({
  library(XSLmodels)
  library(purrr)
  library(dplyr)
  library(here)
})
DIR <- here("fits/v06/yf2015")
if (!startsWith(find.package("XSLmodels"), file.path(DIR, "Rlib"))) {
  stop("run with R_LIBS=$(pwd)/fits/v06/yf2015/Rlib (found ", find.package("XSLmodels"), ")")
}
source(here("fits/v05/specs.R"))
source(file.path(DIR, "specs_yf2015.R"))
N_SIM   <- as.integer(Sys.getenv("N_SIM", "200"))
N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))
OUT <- file.path(DIR, "yf_scores")
dir.create(OUT, showWarnings = FALSE)
stamp <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)

## ---- designs --------------------------------------------------------------------------
e12_data <- yurovsky_frank2015$exp12
e3_data <- yurovsky_frank2015$exp3
# test positions per interval condition, without the 4 practice trials
test_pos <- e12_data |> distinct(interval, item, trial) |> arrange(interval, item)
stopifnot(nrow(test_pos) == 4 * 8)

design12 <- function(N, interval) {
  tp <- filter(test_pos, interval == !!interval)$trial - 4
  ep <- tp - interval
  stopifnot(setequal(c(tp, ep), 1:16))
  E <- map(1:8, \(k) (k - 1) * (2 * N - 1) + seq_len(N))           # exposure objects
  Tn <- map(1:8, \(k) (k - 1) * (2 * N - 1) + N + seq_len(N - 1))  # test-trial novel objects
  words <- vector("list", 16); objects <- vector("list", 16)
  for (k in 1:8) {
    words[[ep[k]]] <- k; objects[[ep[k]]] <- E[[k]]
    words[[tp[k]]] <- k; objects[[tp[k]]] <- c(E[[k]][1], Tn[[k]])
  }
  list(train = list(words = words, objects = objects), n_words = 8, n_objects = 8 * (2 * N - 1),
       items = tibble(k = 1:8, t_exp = ep, t_test = tp), E = E, Tn = Tn)
}

design3 <- function(N) {
  # block k: Ex-A (word k), Ex-B (word 4 + k), T-A
  per <- 3 * N - 2
  EA <- map(1:4, \(k) (k - 1) * per + seq_len(N))
  EB <- map(1:4, \(k) (k - 1) * per + N + seq_len(N))
  Tn <- map(1:4, \(k) (k - 1) * per + 2 * N + seq_len(N - 2))
  words <- list(); objects <- list()
  for (k in 1:4) {
    words <- c(words, list(k, 4 + k, k))
    objects <- c(objects, list(EA[[k]], EB[[k]], c(EA[[k]][1], EB[[k]][1], Tn[[k]])))
  }
  list(train = list(words = words, objects = objects), n_words = 8, n_objects = 4 * per,
       items = tibble(k = 1:4, t_exa = 3 * (1:4) - 2, t_exb = 3 * (1:4) - 1, t_test = 3 * (1:4)),
       EA = EA, EB = EB, Tn = Tn)
}

## ---- running a model ------------------------------------------------------------------
# Matrix after training trial t, padded to every word (plus a novel word,
# row n_words + 1) and object in the design, by label.
pad <- function(m, n_words, n_objects) {
  out <- matrix(0, n_words + 1, n_objects)
  if (!is.null(m) && length(m)) out[as.integer(rownames(m)), as.integer(colnames(m))] <- m
  out
}
fit_train <- function(mod, train, keep_traj = FALSE) {
  pos <- XSLmodels:::positional_train(train)
  s <- mod$model(params = mod$params, data = pos$train, control = xslControl(keep_traj = keep_traj))
  XSLmodels:::relabel_fit(s, pos$voc, pos$ref)
}
# A function t -> padded matrix after trial t, for one simulated learner.
# Deterministic models: a run on the first t trials (a learner hasn't heard
# later words yet). Stochastic models: one run, its per-trial trajectory
# (which must end at the run's own final matrix).
learner_states <- function(mod, design, seed) {
  if (!mod$stochastic) {
    cache <- list()
    return(function(t) {
      key <- as.character(t)
      if (is.null(cache[[key]])) {
        tr <- map(design$train, \(x) x[seq_len(t)])
        cache[[key]] <<- pad(fit_train(mod, tr)$matrix, design$n_words, design$n_objects)
      }
      cache[[key]]
    })
  }
  set.seed(seed)
  s <- fit_train(mod, design$train, keep_traj = TRUE)
  n <- length(design$train$words)
  if (length(s$traj) != n ||
      !isTRUE(all.equal(unname(s$traj[[n]]), unname(s$matrix), tolerance = 1e-6, check.attributes = FALSE))) {
    stop("model '", mod$name, "' has no per-trial trajectory matching its matrix; can't score it per learner")
  }
  traj <- map(s$traj, \(m) {
    if (is.null(dimnames(m))) dimnames(m) <- dimnames(s$matrix)
    pad(m, design$n_words, design$n_objects)
  })
  function(t) traj[[t]]
}
# The model's choice distribution over alternatives, by its own choice rule,
# as xsl_run() scores it.
choose <- function(M, params) {
  if (!is.null(params[["phi"]])) M <- softmax_weights(M, params[["phi"]])
  ca <- XSLmodels:::choice_args(params)
  function(w, alts) predict_referent(M, w, alts, d = ca$d, rule = ca$rule, mu = ca$mu)
}

## ---- scoring one learner --------------------------------------------------------------
score12_learner <- function(mod, design, st) {
  nw <- design$n_words + 1
  map(seq_len(nrow(design$items)), \(i) {
    it <- design$items[i, ]; k <- it$k; E <- design$E[[k]]; Tn <- design$Tn[[k]]; N <- length(E)
    c_o <- choose(st(it$t_exp), mod$params)(k, E)
    ch <- choose(st(it$t_test - 1), mod$params)
    q <- map_dbl(E, \(o) ch(k, c(o, Tn))[1])
    nl <- map_dbl(E, \(o) ch(nw, c(o, Tn))[1])
    other <- \(v) map_dbl(seq_len(N), \(j) mean(v[-j]))
    c(Same = sum(c_o * q), Switch = sum(c_o * other(q)), `New Label` = sum(c_o * other(nl)))
  }) |> reduce(`+`) / nrow(design$items)
}
score3_learner <- function(mod, design, st) {
  types <- expand.grid(target = c("Same", "Switch"), competitor = c("Same", "Switch"), stringsAsFactors = FALSE)
  map(seq_len(nrow(design$items)), \(i) {
    it <- design$items[i, ]; k <- it$k
    EA <- design$EA[[k]]; EB <- design$EB[[k]]; Tn <- design$Tn[[k]]; N <- length(EA)
    cA <- choose(st(it$t_exa), mod$params)(k, EA)
    cB <- choose(st(it$t_exb), mod$params)(4 + k, EB)
    ch <- choose(st(it$t_test - 1), mod$params)
    # p[a, b, ]: P(target), P(competitor) with A's object a and B's object b
    p <- array(0, c(N, N, 2))
    for (a in 1:N) for (b in 1:N) p[a, b, ] <- ch(k, c(EA[a], EB[b], Tn))[1:2]
    # weight over (a, b): Same = the chosen one, Switch = uniform over the others
    wt <- \(cc, type) if (type == "Same") diag(N) * cc else (1 - diag(N)) * cc / (N - 1)
    out <- map(seq_len(nrow(types)), \(j) {
      wa <- colSums(wt(cA, types$target[j]))   # P(target object = a)
      wb <- colSums(wt(cB, types$competitor[j]))
      c(sum(outer(wa, wb) * p[, , 1]), sum(outer(wa, wb) * p[, , 2]))
    })
    do.call(rbind, out)
  }) |> reduce(`+`) / nrow(design$items)
}

score_model <- function(mod, n_sim = N_SIM) {
  sims <- if (mod$stochastic) n_sim else 1
  e12 <- expand.grid(n_referents = c(2, 3, 4, 8), interval = c(1, 2, 4, 8)) |>
    pmap(\(n_referents, interval) {
      d <- design12(n_referents, interval)
      v <- map(seq_len(sims), \(s) score12_learner(mod, d, learner_states(mod, d, 1000 * interval + 10 * n_referents + s))) |>
        reduce(`+`) / sims
      tibble(n_referents, interval, trial_type = names(v), p_target = unname(v))
    }) |> list_rbind()
  types <- expand.grid(target = c("Same", "Switch"), competitor = c("Same", "Switch"), stringsAsFactors = FALSE)
  e3 <- map(c(2, 4), \(n_referents) {
    d <- design3(n_referents)
    v <- map(seq_len(sims), \(s) score3_learner(mod, d, learner_states(mod, d, 50000 + 10 * n_referents + s))) |>
      reduce(`+`) / sims
    tibble(n_referents, trial_type = paste(types$target, "vs.", types$competitor),
           p_target = v[, 1], p_competitor = v[, 2])
  }) |> list_rbind()
  list(e12 = e12, e3 = e3)
}

## ---- fit to the data ------------------------------------------------------------------
obs12 <- e12_data |> mutate(trial_type = as.character(trial_type)) |>
  group_by(experiment, n_referents, interval, trial_type) |>
  summarise(k = sum(chose_repeated), n = n(), .groups = "drop")
obs3 <- e3_data |> mutate(trial_type = as.character(trial_type)) |>
  group_by(n_referents, trial_type) |>
  summarise(n_t = sum(correct), n_c = sum(familiar), n = n(), .groups = "drop") |>
  mutate(n_o = n - n_t - n_c)
clamp <- \(p) pmin(pmax(p, 1e-9), 1 - 1e-9)
# Lapse: with probability eps a response is a uniform guess among the test
# trial's n_referents objects. Luce-choice models give a never-seen object
# exactly 0 strength, so without it they choose the old object with
# certainty on every Same and Switch trial.
with_lapse <- function(pred, eps) {
  pred$e12$p_target <- (1 - eps) * pred$e12$p_target + eps / pred$e12$n_referents
  pred$e3$p_target <- (1 - eps) * pred$e3$p_target + eps / pred$e3$n_referents
  pred$e3$p_competitor <- (1 - eps) * pred$e3$p_competitor + eps / pred$e3$n_referents
  pred
}
# per-condition binomial (Exp 1-2) and multinomial (Exp 3) log-likelihoods
ll12 <- function(pred) {
  x <- left_join(obs12, pred$e12, by = c("n_referents", "interval", "trial_type"))
  x$ll <- dbinom(x$k, x$n, clamp(x$p_target), log = TRUE)
  x
}
ll3 <- function(pred) {
  y <- left_join(obs3, pred$e3, by = c("n_referents", "trial_type"))
  sum(y$n_t * log(clamp(y$p_target)) + y$n_c * log(clamp(y$p_competitor)) +
        ifelse(y$n_o > 0, y$n_o * log(clamp(1 - y$p_target - y$p_competitor)), 0))
}
# The lapse that maximizes the likelihood of Experiment 1 (Same and Switch);
# Experiments 2 and 3 are then held out.
fit_lapse <- function(pred) {
  optimize(\(eps) -sum(filter(ll12(with_lapse(pred, eps)), experiment == 1)$ll), c(0, 1))$minimum
}
metrics <- function(pred, eps = 0) {
  pred <- with_lapse(pred, eps)
  x <- ll12(pred)
  y <- left_join(obs3, pred$e3, by = c("n_referents", "trial_type"))
  e1 <- x$experiment == 1
  r2 <- \(o, p) if (sd(p) > 0) cor(o, p)^2 else NA_real_
  r <- \(o, p) if (sd(p) > 0) cor(o, p) else NA_real_
  same <- filter(pred$e12, trial_type == "Same")$p_target
  switch <- filter(pred$e12, trial_type == "Switch")$p_target
  tibble(lapse = eps,
         e1_loglik = sum(x$ll[e1]), e2_loglik = sum(x$ll[!e1]), e3_loglik = ll3(pred),
         e1_r2 = r2(x$k[e1] / x$n[e1], x$p_target[e1]),
         e12_r2 = r2(x$k / x$n, x$p_target),
         same_minus_switch = mean(same - switch),
         e3_r_target = r(y$n_t / y$n, y$p_target),
         e3_r_competitor = r(y$n_c / y$n, y$p_competitor))
}

## ---- validation ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if ("--validate" %in% args) {
  # sigma = 1, gamma >= 1: the trial-level model's state after an exposure puts
  # all its strength on its hypothesis, so its response is its hypothesis and
  # this scorer must equal yf2015_paradigm() exactly, learner by learner
  for (mu in c(0, 2)) {
    mod <- yf2015(sigma = 1, lambda = .3, gamma = 1.2, mu = mu, sampling = TRUE)
    got <- score_model(mod, n_sim = 3)
    want12 <- yf2015_paradigm(mod, as.data.frame(got$e12))
    want3 <- yf2015_paradigm(mod, as.data.frame(got$e3))
    stamp(sprintf("validate mu = %g: Exp 1-2 max diff %.2g, Exp 3 max diff %.2g / %.2g", mu,
                  max(abs(got$e12$p_target - want12$p_target)),
                  max(abs(got$e3$p_target - want3$p_target)), max(abs(got$e3$p_competitor - want3$p_competitor))))
  }
  # a symmetric associative model predicts Same = Switch, and chance for New Label
  got <- score_model(baseline())
  stamp(sprintf("validate baseline: max |Same - Switch| %.2g, max |New Label - 1/N| %.2g",
                max(abs(filter(got$e12, trial_type == "Same")$p_target - filter(got$e12, trial_type == "Switch")$p_target)),
                max(abs(filter(got$e12, trial_type == "New Label")$p_target -
                          1 / filter(got$e12, trial_type == "New Label")$n_referents))))
  quit(save = "no")
}

## ---- all models -----------------------------------------------------------------------
g <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = g)
jobs <- imap(g$group_fits, \(f, m) list(model = m, par = unname(f$optim$bestmem), spec = specs_v04[[m]],
                                         in_sample = f$optim$bestval))
yf_file <- file.path(DIR, "yf2015_fits.Rdata")
if (file.exists(yf_file)) {
  load(yf_file)  # prog
  for (m in names(Filter(\(x) !is.null(x$group), prog))) {
    jobs[[m]] <- list(model = m, par = unname(prog[[m]]$group$best$optim$bestmem), spec = specs_yf2015[[m]],
                      in_sample = prog[[m]]$group$best$optim$bestval)
  }
}
models <- if (length(setdiff(args, "--validate"))) setdiff(args, "--validate") else names(jobs)
stopifnot(all(models %in% names(jobs)))

run_job <- function(j) {
  path <- file.path(OUT, paste0(j$model, ".rds"))
  if (file.exists(path) && identical(readRDS(path)$par, j$par)) return(invisible(NULL))
  t0 <- Sys.time()
  mod <- j$spec$constructor(j$par)
  # yf2015 models: sampling mode (see the header)
  if (identical(mod$name, "yf2015")) { mod$params$sampling <- TRUE; mod$stochastic <- TRUE }
  # check that this library reproduces the model's group fit on xsl_datasets
  set.seed(1)
  sse <- xsl_run(j$spec$constructor(j$par), xsl_datasets,
                 control = if (j$spec$stochastic) xslControl(n_sim = 200) else xslControl())$sse
  pred <- score_model(mod)
  saveRDS(list(model = j$model, par = j$par, pred = pred, sse_check = sse, in_sample = j$in_sample,
               stochastic = mod$stochastic, n_sim = if (mod$stochastic) N_SIM else 1,
               minutes = as.numeric(difftime(Sys.time(), t0, units = "mins"))), path)
  stamp(sprintf("%s: done (%.1f min; SSE %.4f vs fit %.4f)", j$model,
                as.numeric(difftime(Sys.time(), t0, units = "mins")), sse, j$in_sample))
}
res <- parallel::mclapply(jobs[models], \(j) tryCatch(run_job(j), error = \(e) conditionMessage(e)),
                          mc.cores = N_CORES)
walk2(models, res, \(m, r) if (is.character(r)) stamp("FAILED ", m, ": ", r))

## ---- summary --------------------------------------------------------------------------
# Two rows per model: its raw predictions (lapse 0), and with a lapse fit to
# Experiment 1. Ranked by the held-out likelihood (Experiments 2 and 3) with
# the lapse.
done <- map(list.files(OUT, "\\.rds$", full.names = TRUE), readRDS)
summary_tab <- map(done, \(x) {
  eps <- fit_lapse(x$pred)
  bind_cols(tibble(model = x$model, sse_fit = x$in_sample, sse_check = x$sse_check),
            bind_rows(metrics(x$pred, 0), metrics(x$pred, eps)))
}) |> list_rbind() |>
  mutate(heldout_loglik = e2_loglik + e3_loglik) |>
  group_by(model) |> mutate(rank_key = heldout_loglik[lapse > 0 | n() == 1][1]) |> ungroup() |>
  arrange(desc(rank_key), lapse) |> select(-rank_key)
saveRDS(summary_tab, file.path(DIR, "yf_scores_summary.rds"))
print(as.data.frame(filter(summary_tab, lapse > 0)), digits = 3)
