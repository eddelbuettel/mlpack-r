#' @title Linear Regression Prediction
#'
#' @description
#' Predictions from model.
#'
#' @param input_model Existing LinearRegression model to use
#'   (LinearRegression).
#' @param test Matrix containing X' (test regressors) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_predictions}{Matrix containing predicted responses (numeric
#'   row).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
linear_regression_predict <- function(input_model,
                                      test,
                                      verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("linear_regression_predict")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamLinearRegressionPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_predictions")

  # Call the program.
  linear_regression_predict_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output_predictions" = GetParamRow(p, "output_predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
