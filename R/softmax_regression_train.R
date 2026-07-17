#' @title Softmax Regression
#'
#' @description
#' An implementation of softmax regression for classification, which is a
#' multiclass generalization of logistic regression.  Given labeled data, a
#' softmax regression model can be trained for future use of classification on
#' new points.
#'
#' @param labels A matrix containing labels (0 or 1) for the points in the
#'   training set (y). The labels must order as a row (integer row).
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param lambda L2-regularization constan.  Default value "0.0001"
#'   (numeric).
#' @param max_iterations Maximum number of iterations before termination. 
#'   Default value "400" (integer).
#' @param no_intercept Do not add the intercept term to the model.  Default
#'   value "FALSE" (logical).
#' @param number_of_classes Number of classes for classification; if
#'   unspecified (or 0), the number of classes found in the labels will be used.
#'    Default value "0" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{File to save trained softmax regression model to
#'   (SoftmaxRegression).}
#'
#' @details
#' Implementation of softmax regression, a generalization of logistic regression
#' to the multiclass case, with support for L2 regularization. 
#' 
#' Training a softmax regression model is done by giving a file of training
#' points with the "training" parameter and their corresponding labels with the
#' "labels" parameter. The number of classes can be manually specified with the
#' "number_of_classes" parameter, and the maximum number of iterations of the
#' L-BFGS optimizer can be specified with the "max_iterations" parameter.  The
#' L2 regularization constant can be specified with the "lambda" parameter and
#' if an intercept term is not desired in the model, the "no_intercept"
#' parameter can be specified.
#' 
#' 
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # 
#' # 
#' # suppressMessages(library(mlpack)) # in case 'mlpack' is not yet loaded
#' # X <- as.matrix(read.csv("http://datasets.mlpack.org/iris.csv",
#' # header=FALSE))
#' # y <- as.matrix(read.csv("http://datasets.mlpack.org/iris_labels.csv",
#' # header=FALSE))
#' # pp <- preprocess_split(input=X, input_label=y, test_ratio=0.2)
#' # X_train <- pp[["training"]]
#' # X_test <- pp[["test"]]
#' # y_train <- pp[["training_labels"]]
#' # y_test <- pp[["test_labels"]]
#' # 
#' # model <- softmax_regression_train(training=X_train, labels=y_train,
#' #   lambda=0.1)
#' #   
softmax_regression_train <- function(labels,
                                     training,
                                     lambda = 0.0001,
                                     max_iterations = 400,
                                     no_intercept = FALSE,
                                     number_of_classes = 0,
                                     verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("softmax_regression_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamURow(p, "labels", to_matrix(labels))

  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "no_intercept", no_intercept)

  SetParamInt(p, "number_of_classes", number_of_classes)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  softmax_regression_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamSoftmaxRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "SoftmaxRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_softmax_regression", "mlpack_model_binding", "list")

  return(out)
}
