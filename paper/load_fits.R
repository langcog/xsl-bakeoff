# Shared fit loader for journal-paper.Rmd and appendix.Rmd, so both documents
# always read the same fits.
#
# Prefers the round-2 refit (fits/v04/, XSLmodels 0.4.0, 56 conditions, 19
# models + baseline). Until that exists -- or for any model it doesn't have
# yet -- falls back to the round-1 fits (XSLmodels 0.3.0, 53 conditions),
# and sets FITS_PRELIMINARY so the documents can say so visibly. Round-1
# uncfam-family numbers are known to be stale under 0.4.0 (see
# fits/check_package_drift.R), so a fallback render is for drafting only.
#
# Defines: gfd, group_fits, cv_group_fits, cvg, FITS_SOURCE, FITS_PRELIMINARY

suppressMessages(library(here))

.v04_group <- here("fits/v04/group_fits.Rdata")
.v04_cv    <- here("fits/v04/cv_fits.Rdata")

.load_round1_group <- function() {
  e1 <- new.env(); load(here("fits/group_fits_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/group_fits_new_models.Rdata"),       envir = e2)
  list(gfd = dplyr::bind_rows(e1$gfd_all, e2$gfd_new),
       fits = c(e1$group_fits_all, e2$group_fits_new))
}
.load_round1_cv <- function() {
  e1 <- new.env(); load(here("fits/cv_all_experiments.Rdata"), envir = e1)
  e2 <- new.env(); load(here("fits/cv_new_models.Rdata"),       envir = e2)
  c(e1$cv_all, e2$cv_new)
}

if (file.exists(.v04_group)) {
  local({ e <- new.env(); load(.v04_group, envir = e)
          gfd <<- e$gfd; group_fits <<- e$group_fits })
  .group_src <- "round 2 (XSLmodels 0.4.0)"
} else {
  local({ r1 <- .load_round1_group(); gfd <<- r1$gfd; group_fits <<- r1$fits })
  .group_src <- "round 1 (XSLmodels 0.3.0) -- PRELIMINARY"
}

if (file.exists(.v04_cv)) {
  local({ e <- new.env(); load(.v04_cv, envir = e)
          # only models with all 5 folds done carry $testdf
          cv_group_fits <<- Filter(\(x) !is.null(x$testdf), e$cv_fits) })
  .cv_src <- "round 2 (XSLmodels 0.4.0)"
} else {
  cv_group_fits <- .load_round1_cv()
  .cv_src <- "round 1 (XSLmodels 0.3.0) -- PRELIMINARY"
}
cvg <- purrr::list_rbind(lapply(cv_group_fits, `[[`, "testdf"))

FITS_SOURCE <- c(group = .group_src, cv = .cv_src)
FITS_PRELIMINARY <- any(grepl("PRELIMINARY", FITS_SOURCE))
if (FITS_PRELIMINARY) {
  warning("Using round-1 (0.3.0) fits for: ",
          paste(names(FITS_SOURCE)[grepl("PRELIMINARY", FITS_SOURCE)], collapse = ", "),
          ". Numbers are provisional until fits/v04/ refit completes.", call. = FALSE)
}
