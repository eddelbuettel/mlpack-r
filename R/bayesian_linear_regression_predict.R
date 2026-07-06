#' @title BayesianLinearRegression Prediction
#'
#' @description
#' An implementation of the Bayesian linear regression prediction: Given a
#' pre-trained model and a test data set, it provides model predictions.
#'
#' @param input_model Trained BayesianLinearRegression model to use
#'   (BayesianLinearRegression).
#' @param test Matrix containing points to regress on (test points)
#'   (numeric matrix).
#' @param stddevs Return standard deviations along with predictions. 
#'   Default value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{predictions}{Matrix of predicted responses, with associated
#'   standard deviations if option selected (numeric matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
bayesian_linear_regression_predict <- function(input_model,
                                               test,
                                               stddevs = FALSE,
                                               verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("bayesian_linear_regression_predict")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamBayesianLinearRegressionPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "stddevs", stddevs)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "predictions")

  # Call the program.
  bayesian_linear_regression_predict_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamMat(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
