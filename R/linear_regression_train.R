#' @title Simple Linear Regression
#'
#' @description
#' Train a linear regression model.
#'
#' @param training Matrix containing training set X (regressors) (numeric
#'   matrix).
#' @param lambda Tikhonov regularization for ridge regression.  If 0, the
#'   method reduces to linear regression.  Default value "0" (numeric).
#' @param training_responses Optional vector containing y (responses). If
#'   not given, the responses are assumed to be the last row of the input file
#'   (numeric row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output LinearRegression model (LinearRegression).}
#'
#' @details
#' An implementation of simple linear regression and simple ridge regression
#' using ordinary least squares. This solves the problem
#' 
#'   y = X * b + e
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples

#' # 
#' # #' # \dontrun{
#' # suppressMessages(library(mlpack)) # in case 'mlpack' is not yet loaded
#' # X <-
#' # as.matrix(read.csv("https://datasets.mlpack.org/admission_predict.csv",
#' # header=FALSE))
#' # y <-
#' # as.matrix(read.csv("https://datasets.mlpack.org/admission_predict.responses
#' # .csv", header=FALSE))
#' # pp <- preprocess_split(input=X, input_label=as.matrix(1:nrow(X)),
#' # test_ratio=0.2)
#' # X_train <- pp[["training"]]
#' # X_test <- pp[["test"]]
#' # # labels are indices to operate on both factors or numeric data
#' # y_train <- y[as.integer(pp[["training_labels"]]), 1]
#' # y_test <- y[as.integer(pp[["test_labels"]]), 1]
#' # 
#' # model <- linear_regression_train(training=X_train,
#' # training_responses=y_train)
#' #   }
linear_regression_train <- function(training,
                                    lambda = 0,
                                    training_responses = NA,
                                    verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("linear_regression_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamDouble(p, "lambda", lambda)

  if (!identical(training_responses, NA)) {
    SetParamRow(p, "training_responses", to_matrix(training_responses))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  linear_regression_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLinearRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LinearRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_linear_regression", "mlpack_model_binding", "list")

  return(out)
}
