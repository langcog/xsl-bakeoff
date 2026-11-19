# Scoring models on Yurovsky & Frank (2015)'s response-contingent paradigm:
# the experiments' training designs, per-learner scoring of Same / Switch /
# New Label (Exps 1-2) and target / competitor choices (Exp 3), the lapse,
# and the likelihoods. score_model(mod, n_sim) returns a model's predictions.
# See score_yf_all.R's header for how the paradigm is scored. Sourced by
# score_yf_all.R and fits/v06/clusters/stage3_sweep.R; needs XSLmodels,
# purrr and dplyr attached.

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

# seed_offset shifts every simulated learner's seed (independent replicates).
score_model <- function(mod, n_sim = 200, seed_offset = 0) {
  sims <- if (mod$stochastic) n_sim else 1
  e12 <- expand.grid(n_referents = c(2, 3, 4, 8), interval = c(1, 2, 4, 8)) |>
    pmap(\(n_referents, interval) {
      d <- design12(n_referents, interval)
      v <- map(seq_len(sims), \(s) score12_learner(mod, d, learner_states(mod, d, seed_offset + 1000 * interval + 10 * n_referents + s))) |>
        reduce(`+`) / sims
      tibble(n_referents, interval, trial_type = names(v), p_target = unname(v))
    }) |> list_rbind()
  types <- expand.grid(target = c("Same", "Switch"), competitor = c("Same", "Switch"), stringsAsFactors = FALSE)
  e3 <- map(c(2, 4), \(n_referents) {
    d <- design3(n_referents)
    v <- map(seq_len(sims), \(s) score3_learner(mod, d, learner_states(mod, d, seed_offset + 50000 + 10 * n_referents + s))) |>
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

