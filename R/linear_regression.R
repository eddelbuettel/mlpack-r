#' @title Simple Linear Regression and Prediction
#'
#' @description
#' An implementation of simple linear regression and ridge regression using
#' ordinary least squares.  Given a dataset and responses, a model can be
#' trained and saved for later use, or a pre-trained model can be used to output
#' regression predictions for a test set.
#'
#' @param input_model Existing LinearRegression model to use
#'   (LinearRegression).
#' @param lambda Tikhonov regularization for ridge regression.  If 0, the
#'   method reduces to linear regression.  Default value "0" (numeric).
#' @param test Matrix containing X' (test regressors) (numeric matrix).
#' @param training Matrix containing training set X (regressors) (numeric
#'   matrix).
#' @param training_responses Optional vector containing y (responses). If
#'   not given, the responses are assumed to be the last row of the input file
#'   (numeric row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output LinearRegression model (LinearRegression).}
#' \item{output_predictions}{If --test_file is specified, this matrix is
#'   where the predicted responses will be saved (numeric row).}
#'
#' @details
#' An implementation of simple linear regression and simple ridge regression
#' using ordinary least squares. This solves the problem
#' 
#'   y = X * b + e
#' 
#' where X (specified by "training") and y (specified either as the last column
#' of the input matrix "training" or via the "training_responses" parameter) are
#' known and b is the desired variable.  If the covariance matrix (X'X) is not
#' invertible, or if the solution is overdetermined, then specify a Tikhonov
#' regularization constant (with "lambda") greater than 0, which will regularize
#' the covariance matrix to make it invertible.  The calculated b may be saved
#' with the "output_predictions" output parameter.
#' 
#' Optionally, the calculated value of b is used to predict the responses for
#' another matrix X' (specified by the "test" parameter):
#' 
#'    y' = X' * b
#' 
#' and the predicted responses y' may be saved with the "output_predictions"
#' output parameter.  This type of regression is related to least-angle
#' regression, which mlpack implements as the 'lars' program.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run a linear regression on the dataset "X" with responses
#' # "y", saving the trained model to "lr_model", the following command could be
#' # used:
#' # 
#' # \dontrun{
#' # output <- linear_regression(training=X, training_responses=y)
#' # lr_model <- output$output_model
#' # }
#' # 
#' # Then, to use "lr_model" to predict responses for a test set "X_test",
#' # saving the predictions to "X_test_responses", the following command could
#' # be used:
#' # 
#' # \dontrun{
#' # output <- linear_regression(input_model=lr_model, test=X_test)
#' # X_test_responses <- output$output_predictions
#' # }
linear_regression <- function(input_model = NA,
                              lambda = 0,
                              test = NA,
                              training = NA,
                              training_responses = NA,
                              verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("linear_regression")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input_model, NA)) {
    SetParamLinearRegressionPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamDouble(p, "lambda", lambda)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  if (!identical(training_responses, NA)) {
    SetParamRow(p, "training_responses", to_matrix(training_responses))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "output_predictions")

  # Call the program.
  linear_regression_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLinearRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LinearRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "output_predictions" = GetParamRow(p, "output_predictions")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_linear", "mlpack_model_binding", "list")

  return(out)
}
