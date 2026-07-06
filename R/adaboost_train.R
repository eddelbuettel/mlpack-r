#' @title AdaBoost
#'
#' @description
#' Training AdaBoost model.
#'
#' @param training Dataset for training AdaBoost (numeric matrix).
#' @param iterations The maximum number of boosting iterations to be run (0
#'   will run until convergence..  Default value "1000" (integer).
#' @param labels Labels for the training set (integer row).
#' @param tolerance The tolerance for change in values of the weighted
#'   error during training.  Default value "1e-10" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param weak_learner The type of weak learner to use: 'decision_stump',
#'   or 'perceptron'.  Default value "decision_stump" (character).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output trained AdaBoost model (AdaBoostModel).}
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
#' @author
#' mlpack developers
#'
#' @export
#' @examples

#' # 
#' # \dontrun{
#' # suppressMessages(library(mlpack)) # in case 'mlpack' is not yet loaded
#' # X <- as.matrix(read.csv("http://datasets.mlpack.org/iris.csv",
#' # header=FALSE))
#' # y <- as.matrix(read.csv("http://datasets.mlpack.org/iris_labels.csv",
#' # header=FALSE))
#' # pp <- preprocess_split(input=X, input_label=as.matrix(1:nrow(X)),
#' # test_ratio=0.2)
#' # X_train <- pp[["training"]]
#' # X_test <- pp[["test"]]
#' # # labels are indices to operate on both factors or numeric data
#' # y_train <- y[as.integer(pp[["training_labels"]]), 1]
#' # y_test <- y[as.integer(pp[["test_labels"]]), 1]
#' # 
#' # model <- adaboost_train(training=X_train, labels=y_train)
#' # }
adaboost_train <- function(training,
                           iterations = 1000,
                           labels = NA,
                           tolerance = 1e-10,
                           verbose = getOption("mlpack.verbose", FALSE),
                           weak_learner = "decision_stump") {
  # Create parameters and timers objects.
  p <- CreateParams("adaboost_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamInt(p, "iterations", iterations)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamDouble(p, "tolerance", tolerance)

  SetParamBool(p, "verbose", verbose)

  SetParamString(p, "weak_learner", weak_learner)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  adaboost_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamAdaBoostModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "AdaBoostModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_adaboost", "mlpack_model_binding", "list")

  return(out)
}
