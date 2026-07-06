#' @title AdaBoost
#'
#' @description
#' An implementation of the AdaBoost.MH (Adaptive Boosting) algorithm for
#' classification.  This can be used to train an AdaBoost model on labeled data
#' or use an existing AdaBoost model to predict the classes of new points.
#'
#' @param input_model Input AdaBoost model (AdaBoostModel).
#' @param iterations The maximum number of boosting iterations to be run (0
#'   will run until convergence..  Default value "1000" (integer).
#' @param labels Labels for the training set (integer row).
#' @param test Test dataset (numeric matrix).
#' @param tolerance The tolerance for change in values of the weighted
#'   error during training.  Default value "1e-10" (numeric).
#' @param training Dataset for training AdaBoost (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param weak_learner The type of weak learner to use: 'decision_stump',
#'   or 'perceptron'.  Default value "decision_stump" (character).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output trained AdaBoost model (AdaBoostModel).}
#' \item{predictions}{Predicted labels for the test set (integer row).}
#' \item{probabilities}{Predicted class probabilities for each point in the
#'   test set (numeric matrix).}
#'
#' @details
#' This program implements the AdaBoost (or Adaptive Boosting) algorithm. The
#' variant of AdaBoost implemented here is AdaBoost.MH. It uses a weak learner,
#' either decision stumps or perceptrons, and over many iterations, creates a
#' strong learner that is a weighted ensemble of weak learners. It runs these
#' iterations until a tolerance value is crossed for change in the value of the
#' weighted training error.
#' 
#' For more information about the algorithm, see the paper "Improved Boosting
#' Algorithms Using Confidence-Rated Predictions", by R.E. Schapire and Y.
#' Singer.
#' 
#' This program allows training of an AdaBoost model, and then application of
#' that model to a test dataset.  To train a model, a dataset must be passed
#' with the "training" option.  Labels can be given with the "labels" option; if
#' no labels are specified, the labels will be assumed to be the last column of
#' the input dataset.  Alternately, an AdaBoost model may be loaded with the
#' "input_model" option.
#' 
#' Once a model is trained or loaded, it may be used to provide class
#' predictions for a given test dataset.  A test dataset may be specified with
#' the "test" parameter.  The predicted classes for each point in the test
#' dataset are output to the "predictions" output parameter.  The AdaBoost model
#' itself is output to the "output_model" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run AdaBoost on an input dataset "data" with labels
#' # "labels"and perceptrons as the weak learner type, storing the trained model
#' # in "model", one could use the following command: 
#' # 
#' # \dontrun{
#' # output <- adaboost(training=data, labels=labels, weak_learner="perceptron")
#' # model <- output$output_model
#' # }
#' # 
#' # Similarly, an already-trained model in "model" can be used to provide class
#' # predictions from test data "test_data" and store the output in
#' # "predictions" with the following command: 
#' # 
#' # \dontrun{
#' # output <- adaboost(input_model=model, test=test_data)
#' # predictions <- output$predictions
#' # }
adaboost <- function(input_model = NA,
                     iterations = 1000,
                     labels = NA,
                     test = NA,
                     tolerance = 1e-10,
                     training = NA,
                     verbose = getOption("mlpack.verbose", FALSE),
                     weak_learner = "decision_stump") {
  # Create parameters and timers objects.
  p <- CreateParams("adaboost")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input_model, NA)) {
    SetParamAdaBoostModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "iterations", iterations)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  SetParamDouble(p, "tolerance", tolerance)

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  SetParamString(p, "weak_learner", weak_learner)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  adaboost_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamAdaBoostModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "AdaBoostModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_adaboost", "mlpack_model_binding", "list")

  return(out)
}
