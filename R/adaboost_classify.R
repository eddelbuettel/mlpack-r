#' @title AdaBoost Prediction
#'
#' @description
#' Class predictions from model.
#'
#' @param input_model Input AdaBoost model (AdaBoostModel).
#' @param test Test dataset (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{predictions}{Predicted labels for the test set (integer row).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
adaboost_classify <- function(input_model,
                              test,
                              verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("adaboost_classify")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamAdaBoostModelPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "predictions")

  # Call the program.
  adaboost_classify_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamURow(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
