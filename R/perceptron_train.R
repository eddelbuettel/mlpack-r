#' @title Perceptron training
#'
#' @description
#' An implementation of a perceptron---a single level neural network---for
#' classification.  Given labeled data, a perceptron can be trained and later be
#' used for classification on new points.
#'
#' @param training A matrix containing the training set (numeric matrix).
#' @param labels A matrix containing labels for the training set (integer
#'   row).
#' @param max_iterations The maximum number of iterations the perceptron is
#'   to be ru.  Default value "1000" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained perceptron model
#'   (PerceptronModel).}
#'
#' @details
#' Implementation of a perceptron, which is a single level neural network. The
#' perceptron makes its predictions based on a linear predictor function
#' combining a set of weights with the feature vector.  The perceptron learning
#' rule is able to converge, given enough iterations (specified using the
#' "max_iterations" parameter), if the data supplied is linearly separable.  The
#' perceptron is parameterized by a matrix of weight vectors that denote the
#' numerical weights of the neural network.
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
#' # model <- perceptron_train(training=X_train, labels=y_train,
#' #   max_iterations=100)
#' #   
perceptron_train <- function(training,
                             labels = NA,
                             max_iterations = 1000,
                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("perceptron_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "training", to_matrix(training), TRUE)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  perceptron_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamPerceptronModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "PerceptronModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_perceptron", "mlpack_model_binding", "list")

  return(out)
}
