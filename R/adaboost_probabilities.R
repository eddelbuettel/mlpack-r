#' @title AdaBoost Probability Prediction
#'
#' @description
#' Class probabilities from model.
#'
#' @param input_model Input AdaBoost model (AdaBoostModel).
#' @param test Test dataset (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{probabilities}{Predicted class probabilities for each point in the
#'   test set (numeric matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ prob <- predict(model, newdata=X_test, type="probabilities") }
adaboost_probabilities <- function(input_model,
                                   test,
                                   verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("adaboost_probabilities")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamAdaBoostModelPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "probabilities")

  # Call the program.
  adaboost_probabilities_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
