#' @title Linear SVM is an L2-regularized support vector machine.
#'
#' @description
#' An implementation of linear SVM for multiclass classification. Given labeled
#' data, a model can be trained and saved for future use; or, a pre-trained
#' model can be used to classify new points.
#'
#' @param delta Margin of difference between correct class and other
#'   classes.  Default value "1" (numeric).
#' @param epochs Maximum number of full epochs over dataset for psg. 
#'   Default value "50" (integer).
#' @param input_model Existing model (parameters) (LinearSVMModel).
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
#' @param test Matrix containing test dataset (numeric matrix).
#' @param test_labels Matrix containing test labels (integer row).
#' @param tolerance Convergence tolerance for optimizer.  Default value
#'   "1e-10" (numeric).
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained linear svm model
#'   (LinearSVMModel).}
#' \item{predictions}{If test data is specified, this matrix is where the
#'   predictions for the test set will be saved (integer row).}
#' \item{probabilities}{If test data is specified, this matrix is where the
#'   class probabilities for the test set will be saved (numeric matrix).}
#'
#' @details
#' An implementation of linear SVMs that uses either L-BFGS or parallel SGD
#' (stochastic gradient descent) to train the model.
#' 
#' This program allows loading a linear SVM model (via the "input_model"
#' parameter) or training a linear SVM model given training data (specified with
#' the "training" parameter), or both those things at once.  In addition, this
#' program allows classification on a test dataset (specified with the "test"
#' parameter) and the classification results may be saved with the "predictions"
#' output parameter. The trained linear SVM model may be saved using the
#' "output_model" output parameter.
#' 
#' The training data, if specified, may have class labels as its last dimension.
#'  Alternately, the "labels" parameter may be used to specify a separate vector
#' of labels.
#' 
#' When a model is being trained, there are many options.  L2 regularization (to
#' prevent overfitting) can be specified with the "lambda" option, and the
#' number of classes can be manually specified with the "num_classes"and if an
#' intercept term is not desired in the model, the "no_intercept" parameter can
#' be specified.Margin of difference between correct class and other classes can
#' be specified with the "delta" option.The optimizer used to train the model
#' can be specified with the "optimizer" parameter.  Available options are
#' 'psgd' (parallel stochastic gradient descent) and 'lbfgs' (the L-BFGS
#' optimizer).  There are also various parameters for the optimizer; the
#' "max_iterations" parameter specifies the maximum number of allowed
#' iterations, and the "tolerance" parameter specifies the tolerance for
#' convergence.  For the parallel SGD optimizer, the "step_size" parameter
#' controls the step size taken at each iteration by the optimizer and the
#' maximum number of epochs (specified with "epochs"). If the objective function
#' for your data is oscillating between Inf and 0, the step size is probably too
#' large.  There are more parameters for the optimizers, but the C++ interface
#' must be used to access these.
#' 
#' Optionally, the model can be used to predict the labels for another matrix of
#' data points, if "test" is specified.  The "test" parameter can be specified
#' without the "training" parameter, so long as an existing linear SVM model is
#' given with the "input_model" parameter.  The output predictions from the
#' linear SVM model may be saved with the "predictions" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # As an example, to train a LinaerSVM on the data '"data"' with labels
#' # '"labels"' with L2 regularization of 0.1, saving the model to
#' # '"lsvm_model"', the following command may be used:
#' # 
#' # \dontrun{
#' # output <- linear_svm(training=data, labels=labels, lambda=0.1, delta=1,
#' #   num_classes=0)
#' # lsvm_model <- output$output_model
#' # }
#' # 
#' # Then, to use that model to predict classes for the dataset '"test"',
#' # storing the output predictions in '"predictions"', the following command
#' # may be used: 
#' # 
#' # \dontrun{
#' # output <- linear_svm(input_model=lsvm_model, test=test)
#' # predictions <- output$predictions
#' # }
linear_svm <- function(delta = 1,
                       epochs = 50,
                       input_model = NA,
                       labels = NA,
                       lambda = 0.0001,
                       max_iterations = 10000,
                       no_intercept = FALSE,
                       num_classes = 0,
                       optimizer = "lbfgs",
                       seed = 0,
                       shuffle = FALSE,
                       step_size = 0.01,
                       test = NA,
                       test_labels = NA,
                       tolerance = 1e-10,
                       training = NA,
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("linear_svm")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamDouble(p, "delta", delta)

  SetParamInt(p, "epochs", epochs)

  if (!identical(input_model, NA)) {
    SetParamLinearSVMModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

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

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamDouble(p, "tolerance", tolerance)

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  linear_svm_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLinearSVMModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LinearSVMModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_linear", "mlpack_model_binding", "list")

  return(out)
}
