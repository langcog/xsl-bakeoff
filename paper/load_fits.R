# Shared fit loader for journal-paper.Rmd and appendix.Rmd, so both documents
# always read the same fits.
#
# Loads the v05 refit (fits/v05/adults/): all 47 xsl_datasets conditions
# (adults; Suanda 2014 and Koehne 2013 are held-out generalization sets, and
# there is no separate children fit), 19 models + baseline, group fit and
# 5-fold CV. Fit with XSLmodels e70c10c, except tilles, which was refit with
# its corrected model at b44b9fa (fits/v05/tilles/; see meta$tilles_refit).
# gfd's group SSE is re-simulated at the best parameters at fit time, so the
# documents never re-run a model. There are no fallbacks: earlier rounds
# (fits/v04/, round 1) fit different data and must not be mixed in.
#
# Defines: gfd, group_fits, cv_group_fits, cvg, FITS_META, FITS_SOURCE,
# FITS_PRELIMINARY (always FALSE; kept for the documents' draft banner)

suppressMessages(library(here))

.v05_dir   <- here("fits/v05/adults")
.v05_group <- file.path(.v05_dir, "group_fits.Rdata")
.v05_cv    <- file.path(.v05_dir, "cv_fits.Rdata")
if (!file.exists(.v05_group) || !file.exists(.v05_cv)) {
  stop("v05 fits not found in ", .v05_dir, " (expected group_fits.Rdata and cv_fits.Rdata)", call. = FALSE)
}

local({
  e <- new.env(); load(.v05_group, envir = e)
  gfd <<- e$gfd; group_fits <<- e$group_fits; .group_meta <<- e$meta
})
local({
  e <- new.env(); load(.v05_cv, envir = e)
  # only models with all 5 folds done carry $testdf
  cv_group_fits <<- Filter(\(x) !is.null(x$testdf), e$cv_fits)
  .n_cv_models <<- length(e$cv_fits)
})
stopifnot(.group_meta$n_conditions == 47, length(unique(gfd$condnum)) == 47,
          length(cv_group_fits) == .n_cv_models)
cvg <- purrr::list_rbind(lapply(cv_group_fits, `[[`, "testdf"))

FITS_META <- .group_meta
FITS_SOURCE <- c(
  group = sprintf("v05 (XSLmodels %s; tilles %s), 47 conditions",
                  substr(.group_meta$xslmodels_commit, 1, 7),
                  sub(".*XSLmodels ([0-9a-f]{7}).*", "\\1", .group_meta$tilles_refit)),
  cv = "v05, 5-fold CV over the 47 conditions"
)
FITS_PRELIMINARY <- FALSE
