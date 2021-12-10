# http://adv-r.had.co.nz/S3.html
# perhaps define custom S3 object for each model? 
# useful for wrapping up default upper/lower bounds for parameters, 
# as well as given parameters and any special associated functions

myEstimator <- function(formula, data) {
  result <- list(
    coefficients = 1:3,
    residuals = 1:3
  )
  
  class(result) <- "myEstimator"
  result
}

# define custom summary function
summary.myEstimator <- function(object, ...) {
  value <- paste0(
    "Coefficients: ", paste0(object$coefficients, collapse = ", "),
    "; Residuals: ", paste0(object$residuals, collapse = ", ")
  )
  
  value
}


model <- myEstimator(y ~ x1 + x2, data = df)
summary(model)