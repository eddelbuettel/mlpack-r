#' @title Hoeffding trees Classification
#'
#' @description
#' Class predictions from Hoeffding trees model.
#'
#' @param input_model Input trained Hoeffding tree model
#'   (HoeffdingTreeModel).
#' @param test Testing dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param test_labels Labels of test data (integer row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{predictions}{Matrix to output label predictions for test data into
#'   (integer row).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
hoeffding_tree_classify <- function(input_model,
                                    test,
                                    test_labels = NA,
                                    verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hoeffding_tree_classify")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamHoeffdingTreeModelPtr(p, "input_model", input_model)

  test <- to_matrix_with_info(test)
  SetParamMatWithInfo(p, "test", test$info, test$data)

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "predictions")

  # Call the program.
  hoeffding_tree_classify_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamURow(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
