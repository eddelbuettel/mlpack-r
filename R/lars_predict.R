#' @title LARS Prediction
#'
#' @description
#' An implementation of Least Angle Regression (stagewise/lasso), also known as
#' LARS.  This program can use a pre-trained LARS/LASSO/Elastic Net model to
#' output regression predictions from a test set.
#'
#' @param input_model Trained LARS model to use (LARS).
#' @param test Matrix containing points to regress on (test points)
#'   (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{predictions}{Matrix containing predicted responses (numeric
#'   matrix).}
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # \dontrun{ pred <- predict(model, newdata=X_test) }
lars_predict <- function(input_model,
                         test,
                         verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("lars_predict")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamLARSPtr(p, "input_model", input_model)

  SetParamMat(p, "test", to_matrix(test), TRUE)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "predictions")

  # Call the program.
  lars_predict_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "predictions" = GetParamMat(p, "predictions")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
