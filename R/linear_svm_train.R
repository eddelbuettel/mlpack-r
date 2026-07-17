#' @title Linear SVM Training
#'
#' @description
#' An implementation of linear SVM for multiclass classification. Given labeled
#' data, a model is.
#'
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param delta Margin of difference between correct class and other
#'   classes.  Default value "1" (numeric).
#' @param epochs Maximum number of full epochs over dataset for psg. 
#'   Default value "50" (integer).
#' @param labels A matrix containing labels (0 or 1) for the points in the
#'   training set (y) (integer row).
#' @param lambda L2-regularization parameter for training.  Default value
#'   "0.0001" (numeric).
#' @param max_iterations Maximum iterations for optimizer (0 indicates no
#'   limit).  Default value "10000" (integer).
#' @param no_intercept Do not add the intercept term to the model.  Default
#'   value "FALSE" (logical).
#' @param num_classes Number of classes for classification; if unspecified
#'   (or 0), the number of classes found in the labels will be used.  Default
#'   value "0" (integer).
#' @param optimizer Optimizer to use for training ('lbfgs' or 'psgd'). 
#'   Default value "lbfgs" (character).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param shuffle Don't shuffle the order in which data points are visited
#'   for parallel SGD.  Default value "FALSE" (logical).
#' @param step_size Step size for parallel SGD optimizer.  Default value
#'   "0.01" (numeric).
#' @param tolerance Convergence tolerance for optimizer.  Default value
#'   "1e-10" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained linear svm model
#'   (LinearSVMModel).}
#'
#' @details
#' An implementation of linear SVMs that uses either L-BFGS or parallel SGD
#' (stochastic gradient descent) to train the model.
#' 
#' This implementation allows training a linear SVM model given training data
#' (specified with the "training" parameter).
#' 
#' The training data may have class labels as its last dimension. Alternately,
#' the "labels" parameter may be used to specify a separate vector of labels.
#' 
#' When a model is being trained, there are many options.  L2 regularization (to
#' prevent overfitting) can be specified with the "lambda" option, and the
#' number of classes can be manually specified with the "num_classes"and if an
#' intercept term is not desired in the model, the "no_intercept" parameter can
#' be specified.
#' 
#' Margin of difference between correct class and other classes can be specified
#' with the "delta" option.The optimizer used to train the model can be
#' specified with the "optimizer" parameter.  Available options are 'psgd'
#' (parallel stochastic gradient descent) and 'lbfgs' (the L-BFGS optimizer). 
#' There are also various parameters for the optimizer; the "max_iterations"
#' parameter specifies the maximum number of allowed iterations, and the
#' "tolerance" parameter specifies the tolerance for convergence.  For the
#' parallel SGD optimizer, the "step_size" parameter controls the step size
#' taken at each iteration by the optimizer and the maximum number of epochs
#' (specified with "epochs"). If the objective function for your data is
#' oscillating between Inf and 0, the step size is probably too large.  There
#' are more parameters for the optimizers, but the C++ interface must be used to
#' access these.
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
#' # model <- linear_svm_train(training=X_train, labels=y_train, lambda=0.1,
#' #   delta=1, num_classes=0)
#' #   
linear_svm_train <- function(training,
                             delta = 1,
                             epochs = 50,
                             labels = NA,
                             lambda = 0.0001,
                             max_iterations = 10000,
                             no_intercept = FALSE,
                             num_classes = 0,
                             optimizer = "lbfgs",
                             seed = 0,
                             shuffle = FALSE,
                             step_size = 0.01,
                             tolerance = 1e-10,
                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("linear_svm_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamDouble(p, "delta", delta)

  SetParamInt(p, "epochs", epochs)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "no_intercept", no_intercept)

  SetParamInt(p, "num_classes", num_classes)

  SetParamString(p, "optimizer", optimizer)

  SetParamInt(p, "seed", seed)

  SetParamBool(p, "shuffle", shuffle)

  SetParamDouble(p, "step_size", step_size)

  SetParamDouble(p, "tolerance", tolerance)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  linear_svm_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLinearSVMModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LinearSVMModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_linear_svm", "mlpack_model_binding", "list")

  return(out)
}
