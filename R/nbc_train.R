#' @title Parametric Naive Bayes Classifier training
#'
#' @description
#' An implementation of the Naive Bayes Classifier, used for classification.
#' Given labeled data, an NBC model is be trained for later use for
#' classification on new data.
#'
#' @param training A matrix containing the training set (numeric matrix).
#' @param incremental_variance The variance of each class will be
#'   calculated incrementally.  Default value "FALSE" (logical).
#' @param labels A vector containing labels for the training set (integer
#'   row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{File to save trained Naive Bayes model to
#'   (NBCModel).}
#'
#' @details
#' Implements the Naive Bayes classifier on the given labeled training set for
#' us of that trained model to classify the points in a given test set.
#' 
#' The training set is specified with the "training" parameter.  Labels may be
#' either the last row of the training set, or alternately the "labels"
#' parameter may be specified to pass a separate matrix of labels.
#' 
#' The "incremental_variance" parameter can be used to force the training to use
#' an incremental algorithm for calculating variance.  This is slower, but can
#' help avoid loss of precision in some cases.
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
#' # model <- nbc_train(training=X_train, labels=y_train)
#' # 
nbc_train <- function(training,
                      incremental_variance = FALSE,
                      labels = NA,
                      verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("nbc_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamBool(p, "incremental_variance", incremental_variance)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  nbc_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamNBCModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "NBCModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_nbc", "mlpack_model_binding", "list")

  return(out)
}
