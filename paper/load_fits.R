# Shared fit loader for journal-paper.Rmd and appendix.Rmd, so both documents
# always read the same fits.
#
# Prefers the adult-only round-2 refit (fits/v04/adults/, XSLmodels 0.4.0, 53
# adult conditions -- children are fit separately, fits/v04/children/). Until
# that exists, uses the 56-condition round-2 fits (fits/v04/, which include the
# 3 Suanda et al. 2014 child conditions) flagged PRELIMINARY; then round 1.
# Original note: prefers the round-2 refit (fits/v04/, XSLmodels 0.4.0, 56 conditions, 19
# models + baseline). Until that exists -- or for any model it doesn't have
# yet -- falls back to the round-1 fits (XSLmodels 0.3.0, 53 conditions),
# and sets FITS_PRELIMINARY so the documents can say so visibly. Round-1
# uncfam-family numbers are known to be stale under 0.4.0 (see
# fits/check_package_drift.R), so a fallback render is for drafting only.
#
# Defines: gfd, group_fits, cv_group_fits, cvg, FITS_SOURCE, FITS_PRELIMINARY

suppressMessages(library(here))

# adult-only refit if present (the intended final fits), else the 56-condition run
.adult_dir  <- here("fits/v04/adults")
.use_adults <- file.exists(file.path(.adult_dir, "group_fits.Rdata"))
.v04_dir    <- if (.use_adults) .adult_dir else here("fits/v04")
.v04_group  <- file.path(.v04_dir, "group_fits.Rdata")
.v04_cv     <- file.path(.v04_dir, "cv_fits.Rdata")
.v04_label  <- if (.use_adults) "round 2 (XSLmodels 0.4.0), adults only" else
  "round 2 (XSLmodels 0.4.0), 56 conditions incl. 3 child conditions -- PRELIMINARY until the adult-only refit"

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
  .group_src <- .v04_label
} else {
  local({ r1 <- .load_round1_group(); gfd <<- r1$gfd; group_fits <<- r1$fits })
  .group_src <- "round 1 (XSLmodels 0.3.0) -- PRELIMINARY"
}

if (file.exists(.v04_cv)) {
  local({ e <- new.env(); load(.v04_cv, envir = e)
          # only models with all 5 folds done carry $testdf
          cv_group_fits <<- Filter(\(x) !is.null(x$testdf), e$cv_fits) })
  .cv_src <- .v04_label
} else {
  cv_group_fits <- .load_round1_cv()
  .cv_src <- "round 1 (XSLmodels 0.3.0) -- PRELIMINARY"
}
cvg <- purrr::list_rbind(lapply(cv_group_fits, `[[`, "testdf"))

FITS_SOURCE <- c(group = .group_src, cv = .cv_src)
FITS_PRELIMINARY <- any(grepl("PRELIMINARY", FITS_SOURCE))
if (FITS_PRELIMINARY) {
  warning("Provisional fits -- ", paste(names(FITS_SOURCE), FITS_SOURCE, sep = ": ", collapse = "; "),
          call. = FALSE)
}
