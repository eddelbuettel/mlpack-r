#' @title Softmax Regression Classification
#'
#' @description
#' An implementation of softmax regression for classification, which is a
#' multiclass generalization of logistic regression.  Given a pre-trained
#' softmax regression model, new points are classified.
#'
#' @param input_model Existing model (parameters) (SoftmaxRegression).
#' @param test Matrix containing test dataset (numeric matrix).
#' @param test_labels Matrix containing test labels (integer row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{probabilities}{Matrix to save class probabilities for test dataset
#'   into (numeric matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ prob <- predict(model, newdata=X_test, type="probabilities") }
softmax_regression_probabilities <- function(input_model,
                                             test,
                                             test_labels = NA,
                                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("softmax_regression_probabilities")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamSoftmaxRegressionPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "probabilities")

  # Call the program.
  softmax_regression_probabilities_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
