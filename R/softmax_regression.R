#' @title Softmax Regression
#'
#' @description
#' An implementation of softmax regression for classification, which is a
#' multiclass generalization of logistic regression.  Given labeled data, a
#' softmax regression model can be trained and saved for future use, or, a
#' pre-trained softmax regression model can be used for classification of new
#' points.
#'
#' @param input_model File containing existing model (parameters)
#'   (SoftmaxRegression).
#' @param labels A matrix containing labels (0 or 1) for the points in the
#'   training set (y). The labels must order as a row (integer row).
#' @param lambda L2-regularization constan.  Default value "0.0001"
#'   (numeric).
#' @param max_iterations Maximum number of iterations before termination. 
#'   Default value "400" (integer).
#' @param no_intercept Do not add the intercept term to the model.  Default
#'   value "FALSE" (logical).
#' @param number_of_classes Number of classes for classification; if
#'   unspecified (or 0), the number of classes found in the labels will be used.
#'    Default value "0" (integer).
#' @param test Matrix containing test dataset (numeric matrix).
#' @param test_labels Matrix containing test labels (integer row).
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{File to save trained softmax regression model to
#'   (SoftmaxRegression).}
#' \item{predictions}{Matrix to save predictions for test dataset into
#'   (integer row).}
#' \item{probabilities}{Matrix to save class probabilities for test dataset
#'   into (numeric matrix).}
#'
#' @details
#' This program performs softmax regression, a generalization of logistic
#' regression to the multiclass case, and has support for L2 regularization. 
#' The program is able to train a model, load  an existing model, and give
#' predictions (and optionally their accuracy) for test data.
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
#' The trained model can be saved with the "output_model" output parameter. If
#' training is not desired, but only testing is, a model can be loaded with the
#' "input_model" parameter.  At the current time, a loaded model cannot be
#' trained further, so specifying both "input_model" and "training" is not
#' allowed.
#' 
#' The program is also able to evaluate a model on test data.  A test dataset
#' can be specified with the "test" parameter. Class predictions can be saved
#' with the "predictions" output parameter.  If labels are specified for the
#' test data with the "test_labels" parameter, then the program will print the
#' accuracy of the predictions on the given test set and its corresponding
#' labels.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a softmax regression model on the data "dataset" with
#' # labels "labels" with a maximum of 1000 iterations for training, saving the
#' # trained model to "sr_model", the following command can be used: 
#' # 
#' # \dontrun{
#' # output <- softmax_regression(training=dataset, labels=labels)
#' # sr_model <- output$output_model
#' # }
#' # 
#' # Then, to use "sr_model" to classify the test points in "test_points",
#' # saving the output predictions to "predictions", the following command can
#' # be used:
#' # 
#' # \dontrun{
#' # output <- softmax_regression(input_model=sr_model, test=test_points)
#' # predictions <- output$predictions
#' # }
softmax_regression <- function(input_model = NA,
                               labels = NA,
                               lambda = 0.0001,
                               max_iterations = 400,
                               no_intercept = FALSE,
                               number_of_classes = 0,
                               test = NA,
                               test_labels = NA,
                               training = NA,
                               verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("softmax_regression")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input_model, NA)) {
    SetParamSoftmaxRegressionPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "no_intercept", no_intercept)

  SetParamInt(p, "number_of_classes", number_of_classes)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  softmax_regression_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamSoftmaxRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "SoftmaxRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_softmax", "mlpack_model_binding", "list")

  return(out)
}
