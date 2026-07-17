#' @title Parametric Naive Bayes Classifier Probabilities
#'
#' @description
#' Class probabilities from a Naive Bayes Classifier model.
#'
#' @param input_model Input Naive Bayes model (NBCModel).
#' @param test A matrix containing the test set (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{probabilities}{The matrix in which the predicted probability of
#'   labels for the test set will be written (numeric matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ prob <- predict(model, newdata=X_test, type="probabilities") }
nbc_probabilities <- function(input_model,
                              test,
                              verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("nbc_probabilities")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamNBCModelPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "probabilities")

  # Call the program.
  nbc_probabilities_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
