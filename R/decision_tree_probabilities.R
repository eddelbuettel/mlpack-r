#' @title Decision tree Prediction
#'
#' @description
#' Class predictions from train decision tree model.
#'
#' @param input_model Pre-trained decision tree, to be used with test
#'   points (DecisionTreeModel).
#' @param test Testing dataset (may contain categorical variables) (numeric
#'   matrix/data.frame with info).
#' @param test_labels Test point labels, if accuracy calculation is desired
#'   (integer row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{probabilities}{Class probabilities for each test point if
#'   probabilities has been selected (numeric matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ prob <- predict(model, newdata=X_test, type="probabilities") }
decision_tree_probabilities <- function(input_model,
                                        test,
                                        test_labels = NA,
                                        verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("decision_tree_probabilities")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamDecisionTreeModelPtr(p, "input_model", input_model)

  test <- to_matrix_with_info(test)
  SetParamMatWithInfo(p, "test", test$info, test$data)

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "probabilities")

  # Call the program.
  decision_tree_probabilities_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
