#' @title L2-regularized Logistic Regression Classification
#'
#' @description
#' An implementation of L2-regularized logistic regression for two-class
#' classification.  Uses a trained model to classify new points.
#'
#' @param input_model Existing model (parameters) (LogisticRegression).
#' @param test Matrix containing test dataset (numeric matrix).
#' @param decision_boundary Decision boundary for prediction; if the
#'   logistic function for a point is less than the boundary, the class is taken
#'   to be 0; otherwise, the class is 1.  Default value "0.5" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{predictions}{If test data is specified, this matrix is where the
#'   predictions for the test set will be saved (integer row).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
logistic_regression_classify <- function(input_model,
                                         test,
                                         decision_boundary = 0.5,
                                         verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("logistic_regression_classify")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamLogisticRegressionPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamDouble(p, "decision_boundary", decision_boundary)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "predictions")

  # Call the program.
  logistic_regression_classify_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamURow(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
