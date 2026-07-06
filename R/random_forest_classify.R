#' @title Random Forests Prediction
#'
#' @description
#' Class predictions from random forest model.
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
#' \item{predictions}{Predicted classes for each point in the test set
#'   (integer row).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
random_forest_classify <- function(input_model,
                                   test,
                                   test_labels = NA,
                                   verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("random_forest_classify")
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
  SetPassed(p, "predictions")

  # Call the program.
  random_forest_classify_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamURow(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
