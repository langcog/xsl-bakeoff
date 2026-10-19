# Choice-level evaluation of the v05 fits: how well does each model predict
# WHICH object people chose on each test item, not just how often they were
# right? Uses xsl_datasets' response_matrix (counts of each object chosen per
# test item).
#
# Two parameter regimes, as in Table 1:
#  - cv:    out of fold. For each of the 5 CV folds, the model is run at the
#           parameters fit to the other four folds and scored only on that
#           fold's held-out conditions; every condition is scored exactly once.
#           The primary analysis. (baseline has no parameters, so its CV
#           predictions are its group predictions; fazlyt is group-only.)
#  - group: at the parameters fit to all 47 conditions.
# Parameters were fit by n_subj-weighted SSE on accuracy, so nothing in the
# fits saw which foil people chose.
#
# Each model's choice distribution over an item's alternatives is taken
# exactly as xsl_run() scores accuracy (choice_items() in common.R). Scores:
#  - ll_total: multinomial log-likelihood per response, with a lapse rate eps
#    (uniform guessing among the alternatives) fit per model, which keeps the
#    likelihood finite when a model gives a chosen object p = 0. eps ranges
#    over [0, 1]; eps near 1 means the model's predictions for that part are
#    no better than uniform guessing (share ~ 0%).
#  - ll_acc:   the correct/incorrect part only (Bernoulli), with its own lapse.
#    Close to what the SSE fit optimized.
#  - ll_foil:  given an error, which foil was chosen (the model's distribution
#    over the foils, renormalized, with its own lapse), per error.
#  - top_foil: share of human errors that went to the model's most likely foil.
# The lapse rates (one per part) are fit to the scored data in both regimes:
# one extra parameter shared by every model.
# Log-likelihoods are also expressed as the share of the distance from chance
# (or, for accuracy, from the baseline) to the leave-one-out noise ceiling
# (noise_ceiling.rds).
#
# Results are cached per model and regime in choice_level/<regime>/
# (recomputed whenever the parameters change), then combined into
# choice_level.rds and choice_level.md.
#
# Usage (from xsl-bakeoff/, after noise_ceiling.R):
#   R_LIBS=fits/v05/Rlib N_CORES=4 Rscript fits/v05/extras/choice_level.R [model ...]

source(here::here("fits/v05/extras/common.R"))
suppressMessages(library(parallel))

N_CORES <- as.integer(Sys.getenv("N_CORES", "4"))
N_SIM   <- as.integer(Sys.getenv("N_SIM", "200"))
cache_dir <- file.path(EXTRAS_DIR, "choice_level")
for (r in c("group", "cv")) dir.create(file.path(cache_dir, r), recursive = TRUE, showWarnings = FALSE)

pars <- load_group_pars()
e <- new.env(); load(here("fits/v05/adults/cv_fits.Rdata"), envir = e)
cv_fits <- Filter(\(x) !is.null(x$testdf), e$cv_fits)   # models with all 5 folds
# out-of-fold parameter sets: per model, a list of (par, test_conditions)
cv_pars <- map(cv_fits, \(x) map(x$folds, \(f) list(par = unname(f$par), test = f$test_conditions)))
cv_pars$baseline <- list(list(par = numeric(0), test = map_chr(xsl_datasets, "label")))

args <- commandArgs(trailingOnly = TRUE)
models <- if (length(args)) args else names(pars)
stopifnot(all(models %in% names(pars)))
ceiling <- readRDS(file.path(EXTRAS_DIR, "noise_ceiling.rds"))$choice_ceiling
labels <- map_chr(xsl_datasets, "label")

# choice items for one model at one parameter set, on a subset of conditions
items_at <- function(m, par, conds, seed) {
  set.seed(seed)
  res <- run_model(m, par, data = xsl_datasets[labels %in% conds], n_sim = N_SIM)
  ch_dec <- if (m == "Bayesian_decay") par[3] else NULL
  map(res$fits, \(f) choice_items(f, ch_dec)) |> list_rbind() |> filter(!excluded)
}

score_items <- function(it) {
  ci <- map2_int(it$alts, it$correct, \(a, c) match(c, a))
  k  <- lengths(it$alts)
  pc <- map2_dbl(it$p, ci, \(p, j) p[j])
  nc <- map2_dbl(it$human, ci, \(h, j) h[j])
  n  <- map_dbl(it$human, sum)
  foil_h <- map2(it$human, ci, \(h, j) h[-j])
  foil_p <- map2(it$p, ci, \(p, j) { q <- p[-j]; if (sum(q) > 0) q / sum(q) else rep(1 / length(q), length(q)) })

  ll_total <- \(eps) sum(map2_dbl(it$p, it$human, \(p, h) sum(h * log(lapse(p, eps)))))
  ll_acc   <- \(eps) { q <- (1 - eps) * pc + eps / k; sum(nc * log(q) + (n - nc) * log(1 - q)) }
  ll_foil  <- \(eps) sum(map2_dbl(foil_p, foil_h, \(q, h) sum(h * log(lapse(q, eps)))))
  best <- \(f) optimize(\(e) -f(e), c(1e-6, 1 - 1e-6))
  bt <- best(ll_total); ba <- best(ll_acc); bf <- best(ll_foil)

  top_hits <- sum(map2_dbl(foil_p, foil_h, \(q, h) { top <- which(q == max(q)); sum(h[top]) / length(top) }))
  list(n_items = nrow(it), n_resp = sum(n), n_err = sum(n - nc),
       ll_total = -bt$objective, eps_total = bt$minimum,
       ll_acc = -ba$objective, eps_acc = ba$minimum,
       ll_foil = -bf$objective, eps_foil = bf$minimum,
       top_foil = top_hits / sum(n - nc),
       top_foil_chance = sum(map2_dbl(foil_h, k, \(h, k) sum(h) / (k - 1))) / sum(n - nc),
       items = it |> select(label, item) |> mutate(p_correct = pc, n = n, n_correct = nc))
}

score_model <- function(m, regime) {
  key <- if (regime == "group") pars[[m]] else cv_pars[[m]]
  f <- file.path(cache_dir, regime, paste0(m, ".rds"))
  if (file.exists(f)) {
    old <- readRDS(f)
    if (isTRUE(all.equal(old$key, key))) return(old)
  }
  it <- if (regime == "group") {
    items_at(m, pars[[m]], labels, 2026)
  } else {
    imap(cv_pars[[m]], \(fold, i) items_at(m, fold$par, fold$test, 2026 + i)) |> list_rbind()
  }
  stopifnot(nrow(distinct(it, label, item)) == nrow(it))   # each item scored once
  out <- c(list(model = m, regime = regime, key = key), score_items(it))
  saveRDS(out, f)
  out
}

jobs <- bind_rows(tibble(model = models, regime = "group"),
                  tibble(model = intersect(models, names(cv_pars)), regime = "cv"))
stamp("choice-level scoring: ", nrow(jobs), " model x regime jobs on ", N_CORES, " cores")
scores <- mclapply(seq_len(nrow(jobs)), \(j) tryCatch(score_model(jobs$model[j], jobs$regime[j]), error = \(e) e),
                   mc.cores = N_CORES, mc.preschedule = FALSE)
failed <- which(map_lgl(scores, \(s) inherits(s, "error")))
if (length(failed)) stop("failed: ", paste(jobs$model[failed], jobs$regime[failed],
                                           map_chr(scores[failed], conditionMessage), sep = ": ", collapse = "; "))

## ---- combine ---------------------------------------------------------------
ch_all  <- filter(ceiling, part == "all responses")
ch_acc  <- filter(ceiling, part == "correct vs error")
ch_foil <- filter(ceiling, part == "errors only (which foil)")
share <- \(ll, ch) (ll - ch$ll_chance_per_resp) / (ch$ll_ceiling_per_resp - ch$ll_chance_per_resp)

# accuracy-level SSE for the same predictions (Table 1's metric)
wsse <- \(df) df |> filter(!is.na(HumanPerf)) |> group_by(Model, condnum) |>
  summarise(s = sum((ModelPerf - HumanPerf)^2), n = first(Nsubj), .groups = "drop") |>
  group_by(Model) |> summarise(sse = sum(s * n) / sum(n), .groups = "drop")
g <- new.env(); load(here("fits/v05/adults/group_fits.Rdata"), envir = g)
sse_group <- wsse(g$gfd)
sse_cv <- wsse(list_rbind(map(cv_fits, "testdf"))) |>
  bind_rows(filter(sse_group, Model == "baseline"))

make_table <- function(regime) {
  sc <- keep(scores, \(s) !inherits(s, "error") && s$regime == regime)
  map(sc, \(s) tibble(
    Model = s$model, k = length(pars[[s$model]]),
    ll_total = s$ll_total / s$n_resp, ll_acc = s$ll_acc / s$n_resp, ll_foil = s$ll_foil / s$n_err,
    eps_total = s$eps_total, eps_foil = s$eps_foil,
    top_foil = s$top_foil, top_foil_chance = s$top_foil_chance)) |> list_rbind() |>
    mutate(share_acc = share(ll_acc, ch_acc), share_foil = share(ll_foil, ch_foil),
           # chance is a weak reference for accuracy (even the baseline covers
           # ~80%), so also measure from the zero-parameter co-occurrence baseline
           acc_beyond_baseline = (ll_acc - ll_acc[Model == "baseline"]) /
             (ch_acc$ll_ceiling_per_resp - ll_acc[Model == "baseline"])) |>
    left_join(if (regime == "cv") sse_cv else sse_group, by = "Model") |>
    arrange(desc(ll_total))
}
tabs <- list(cv = make_table("cv"), group = make_table("group"))
rhos <- map(tabs, \(t) c(choice = cor(t$sse, -t$ll_total, method = "spearman"),
                         foil = cor(t$sse, -t$ll_foil, method = "spearman")))

saveRDS(list(tables = tabs, scores = scores, ceiling = ceiling, rho_sse_vs_choice = rhos,
             excluded = EXCLUDE_CHOICE, n_sim = N_SIM, generated = format(Sys.time())),
        file.path(EXTRAS_DIR, "choice_level.rds"))

f3 <- \(x) sprintf("%.3f", x); pct <- \(x) sprintf("%.0f%%", 100 * x)
show <- \(t, sse_name) knitr::kable(t |> transmute(
  Model, k, !!sse_name := f3(sse), `ll total` = f3(ll_total),
  `ll acc` = f3(ll_acc), `share acc` = pct(share_acc), `acc beyond baseline` = pct(acc_beyond_baseline),
  `ll foil` = f3(ll_foil), `share foil` = pct(share_foil),
  `top-foil hits` = pct(top_foil), `(chance)` = pct(top_foil_chance),
  `foil lapse` = sprintf("%.2f", eps_foil)), format = "pipe")
rho_line <- \(r) paste0("Spearman rho between the SSE ranking and the choice-level ranking: ", sprintf("%.2f", r["choice"]),
                        " (all responses), ", sprintf("%.2f", r["foil"]), " (foil choice only).")
md <- c(
  "# Choice-level evaluation (v05 fits)", "",
  paste0("Generated ", format(Sys.time()), " by `fits/v05/extras/choice_level.R` (n_sim = ", N_SIM,
         " for stochastic models). Log-likelihoods are per response (ll_total, ll_acc) or per error (ll_foil); ",
         "lapse rates fit per model and per part. 'share' = fraction of the way from chance to the leave-one-out ",
         "noise ceiling for that part (above 100% = better than predicting from the other participants' choices); ",
         "'acc beyond baseline' measures from the co-occurrence baseline instead. All 924 items are scored ",
         "(no exclusions since XSLmodels PR #31)."), "",
  paste0("- Chance / LOO ceiling / saturated: all responses ", f3(ch_all$ll_chance_per_resp), " / ", f3(ch_all$ll_ceiling_per_resp),
         " / ", f3(ch_all$ll_saturated_per_resp), "; correct vs error ", f3(ch_acc$ll_chance_per_resp), " / ",
         f3(ch_acc$ll_ceiling_per_resp), " / ", f3(ch_acc$ll_saturated_per_resp), "; errors only ",
         f3(ch_foil$ll_chance_per_resp), " / ", f3(ch_foil$ll_ceiling_per_resp), " / ", f3(ch_foil$ll_saturated_per_resp), "."), "",
  "## Out of fold (5-fold CV parameters; primary)", "",
  paste0("Each condition scored once, at parameters fit without it. ", rho_line(rhos$cv), " (SSE = CV SSE.)"), "",
  show(tabs$cv, "CV SSE"), "",
  "## Group-fit parameters", "",
  paste0(rho_line(rhos$group), " (SSE = group SSE.)"), "",
  show(tabs$group, "group SSE"))
writeLines(md, file.path(EXTRAS_DIR, "choice_level.md"))
cat(md, sep = "\n")
