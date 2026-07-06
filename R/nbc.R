#' @title Parametric Naive Bayes Classifier
#'
#' @description
#' An implementation of the Naive Bayes Classifier, used for classification.
#' Given labeled data, an NBC model can be trained and saved, or, a pre-trained
#' model can be used for classification.
#'
#' @param incremental_variance The variance of each class will be
#'   calculated incrementally.  Default value "FALSE" (logical).
#' @param input_model Input Naive Bayes model (NBCModel).
#' @param labels A file containing labels for the training set (integer
#'   row).
#' @param test A matrix containing the test set (numeric matrix).
#' @param training A matrix containing the training set (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{File to save trained Naive Bayes model to
#'   (NBCModel).}
#' \item{predictions}{The matrix in which the predicted labels for the test
#'   set will be written (integer row).}
#' \item{probabilities}{The matrix in which the predicted probability of
#'   labels for the test set will be written (numeric matrix).}
#'
#' @details
#' This program trains the Naive Bayes classifier on the given labeled training
#' set, or loads a model from the given model file, and then may use that
#' trained model to classify the points in a given test set.
#' 
#' The training set is specified with the "training" parameter.  Labels may be
#' either the last row of the training set, or alternately the "labels"
#' parameter may be specified to pass a separate matrix of labels.
#' 
#' If training is not desired, a pre-existing model may be loaded with the
#' "input_model" parameter.
#' 
#' 
#' 
#' The "incremental_variance" parameter can be used to force the training to use
#' an incremental algorithm for calculating variance.  This is slower, but can
#' help avoid loss of precision in some cases.
#' 
#' If classifying a test set is desired, the test set may be specified with the
#' "test" parameter, and the classifications may be saved with the
#' "predictions"predictions  parameter.  If saving the trained model is desired,
#' this may be done with the "output_model" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a Naive Bayes classifier on the dataset "data" with
#' # labels "labels" and save the model to "nbc_model", the following command
#' # may be used:
#' # 
#' # \dontrun{
#' # output <- nbc(training=data, labels=labels)
#' # nbc_model <- output$output_model
#' # }
#' # 
#' # Then, to use "nbc_model" to predict the classes of the dataset "test_set"
#' # and save the predicted classes to "predictions", the following command may
#' # be used:
#' # 
#' # \dontrun{
#' # output <- nbc(input_model=nbc_model, test=test_set)
#' # predictions <- output$predictions
#' # }
nbc <- function(incremental_variance = FALSE,
                input_model = NA,
                labels = NA,
                test = NA,
                training = NA,
                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("nbc")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamBool(p, "incremental_variance", incremental_variance)

  if (!identical(input_model, NA)) {
    SetParamNBCModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
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
  nbc_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamNBCModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "NBCModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_nbc", "mlpack_model_binding", "list")

  return(out)
}
