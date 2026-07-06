#' @title Random Forests Probabilities
#'
#' @description
#' Class probabilities from random forest model.
#'
#' @param input_model Pre-trained random forest to use for classification
#'   (RandomForestModel).
#' @param test Test dataset to produce predictions for (numeric matrix).
#' @param test_labels Test dataset labels, if accuracy calculation is
#'   desired (integer row).
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
random_forest_probabilities <- function(input_model,
                                        test,
                                        test_labels = NA,
                                        verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("random_forest_probabilities")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamRandomForestModelPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "probabilities")

  # Call the program.
  random_forest_probabilities_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
