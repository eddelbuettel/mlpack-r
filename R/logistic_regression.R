#' @title L2-regularized Logistic Regression and Prediction
#'
#' @description
#' An implementation of L2-regularized logistic regression for two-class
#' classification.  Given labeled data, a model can be trained and saved for
#' future use; or, a pre-trained model can be used to classify new points.
#'
#' @param batch_size Batch size for SGD.  Default value "64" (integer).
#' @param decision_boundary Decision boundary for prediction; if the
#'   logistic function for a point is less than the boundary, the class is taken
#'   to be 0; otherwise, the class is 1.  Default value "0.5" (numeric).
#' @param input_model Existing model (parameters) (LogisticRegression).
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
#' @param test Matrix containing test dataset (numeric matrix).
#' @param tolerance Convergence tolerance for optimizer.  Default value
#'   "1e-10" (numeric).
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained logistic regression model
#'   (LogisticRegression).}
#' \item{predictions}{If test data is specified, this matrix is where the
#'   predictions for the test set will be saved (integer row).}
#' \item{probabilities}{If test data is specified, this matrix is where the
#'   class probabilities for the test set will be saved (numeric matrix).}
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
#' This program allows loading a logistic regression model (via the
#' "input_model" parameter) or training a logistic regression model given
#' training data (specified with the "training" parameter), or both those things
#' at once.  In addition, this program allows classification on a test dataset
#' (specified with the "test" parameter) and the classification results may be
#' saved with the "predictions" output parameter. The trained logistic
#' regression model may be saved using the "output_model" output parameter.
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
#' Optionally, the model can be used to predict the responses for another matrix
#' of data points, if "test" is specified.  The "test" parameter can be
#' specified without the "training" parameter, so long as an existing logistic
#' regression model is given with the "input_model" parameter.  The output
#' predictions from the logistic regression model may be saved with the
#' "predictions" parameter.
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
#' # As an example, to train a logistic regression model on the data '"data"'
#' # with labels '"labels"' with L2 regularization of 0.1, saving the model to
#' # '"lr_model"', the following command may be used:
#' # 
#' # \dontrun{
#' # output <- logistic_regression(training=data, labels=labels, lambda=0.1,
#' #   print_training_accuracy=TRUE)
#' # lr_model <- output$output_model
#' # }
#' # 
#' # Then, to use that model to predict classes for the dataset '"test"',
#' # storing the output predictions in '"predictions"', the following command
#' # may be used: 
#' # 
#' # \dontrun{
#' # output <- logistic_regression(input_model=lr_model, test=test)
#' # predictions <- output$predictions
#' # }
logistic_regression <- function(batch_size = 64,
                                decision_boundary = 0.5,
                                input_model = NA,
                                labels = NA,
                                lambda = 0,
                                max_iterations = 10000,
                                optimizer = "lbfgs",
                                print_training_accuracy = FALSE,
                                step_size = 0.01,
                                test = NA,
                                tolerance = 1e-10,
                                training = NA,
                                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("logistic_regression")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "batch_size", batch_size)

  SetParamDouble(p, "decision_boundary", decision_boundary)

  if (!identical(input_model, NA)) {
    SetParamLogisticRegressionPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamString(p, "optimizer", optimizer)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  SetParamDouble(p, "step_size", step_size)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
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
  logistic_regression_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLogisticRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LogisticRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_logistic", "mlpack_model_binding", "list")

  return(out)
}
