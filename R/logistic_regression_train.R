#' @title L2-regularized Logistic Regression Training and Prediction
#'
#' @description
#' An implementation of L2-regularized logistic regression for two-class
#' classification.  Given labeled data, a model is trained and saved for future
#' use; or, a pre-trained model can be used to classify new points.
#'
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param batch_size Batch size for SGD.  Default value "64" (integer).
#' @param labels A matrix containing labels (0 or 1) for the points in the
#'   training set (y) (integer row).
#' @param lambda L2-regularization parameter for training.  Default value
#'   "0" (numeric).
#' @param max_iterations Maximum iterations for optimizer (0 indicates no
#'   limit).  Default value "10000" (integer).
#' @param optimizer Optimizer to use for training ('lbfgs' or 'sgd'). 
#'   Default value "lbfgs" (character).
#' @param print_training_accuracy If set, then the accuracy of the model on
#'   the training set will be printed (verbose must also be specified).  Default
#'   value "FALSE" (logical).
#' @param step_size Step size for SGD optimizer.  Default value "0.01"
#'   (numeric).
#' @param tolerance Convergence tolerance for optimizer.  Default value
#'   "1e-10" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained logistic regression model
#'   (LogisticRegression).}
#'
#' @details
#' An implementation of L2-regularized logistic regression using either the
#' L-BFGS optimizer or SGD (stochastic gradient descent).  This solves the
#' regression problem
#' 
#'   y = (1 / 1 + e^-(X * b)).
#' 
#' In this setting, y corresponds to class labels and X corresponds to data.
#' 
#' This implementation can train a logistic regression model given training data
#' (specified with the "training" parameter).  A trained logistic regression
#' model can then be used to perform classification on a test dataset (specified
#' with the "test" parameter).  Alternatively, classification probabilities can
#' be computed and saved with the "probabilities" parameter.
#' 
#' The training data, if specified, may have class labels as its last dimension.
#'  Alternately, the "labels" parameter may be used to specify a separate matrix
#' of labels.
#' 
#' When a model is being trained, there are many options.  L2 regularization (to
#' prevent overfitting) can be specified with the "lambda" option, and the
#' optimizer used to train the model can be specified with the "optimizer"
#' parameter.  Available options are 'sgd' (stochastic gradient descent) and
#' 'lbfgs' (the L-BFGS optimizer).  There are also various parameters for the
#' optimizer; the "max_iterations" parameter specifies the maximum number of
#' allowed iterations, and the "tolerance" parameter specifies the tolerance for
#' convergence.  For the SGD optimizer, the "step_size" parameter controls the
#' step size taken at each iteration by the optimizer.  The batch size for SGD
#' is controlled with the "batch_size" parameter. If the objective function for
#' your data is oscillating between Inf and 0, the step size is probably too
#' large.  There are more parameters for the optimizers, but the C++ interface
#' must be used to access these.
#' 
#' For SGD, an iteration refers to a single point. So to take a single pass over
#' the dataset with SGD, "max_iterations" should be set to the number of points
#' in the dataset.
#' 
#' This implementation of logistic regression does not support the general
#' multi-class case but instead only the two-class case.  Any labels must be
#' either 0 or 1.  For more classes, see the softmax regression implementation.
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
#' # model <- logistic_regression_train(training=X_train, labels=y_train,
#' #   lambda=0.1)
#' #   
logistic_regression_train <- function(training,
                                      batch_size = 64,
                                      labels = NA,
                                      lambda = 0,
                                      max_iterations = 10000,
                                      optimizer = "lbfgs",
                                      print_training_accuracy = FALSE,
                                      step_size = 0.01,
                                      tolerance = 1e-10,
                                      verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("logistic_regression_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamInt(p, "batch_size", batch_size)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamString(p, "optimizer", optimizer)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  SetParamDouble(p, "step_size", step_size)

  SetParamDouble(p, "tolerance", tolerance)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  logistic_regression_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLogisticRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LogisticRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_logistic_regression", "mlpack_model_binding", "list")

  return(out)
}
