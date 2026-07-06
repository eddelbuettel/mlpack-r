#' @title Decision tree training
#'
#' @description
#' Training ID3-style decision tree model.
#'
#' @param training Training dataset (may contain categorical variables)
#'   (numeric matrix/data.frame with info).
#' @param labels Training labels (integer row).
#' @param maximum_depth Maximum depth of the tree (0 means no limit). 
#'   Default value "0" (integer).
#' @param minimum_gain_split Minimum gain for node splitting.  Default
#'   value "1e-07" (numeric).
#' @param minimum_leaf_size Minimum number of points in a leaf.  Default
#'   value "20" (integer).
#' @param print_training_accuracy Print the training accuracy.  Default
#'   value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param weights The weight of label (numeric matrix).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained decision tree
#'   (DecisionTreeModel).}
#'
#' @details
#' Train using a decision tree.  Given a dataset containing numeric or
#' categorical features, and associated labels for each point in the dataset,
#' this program can train a decision tree on that data.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range `[0,
#' num_classes - 1]`. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' The trained model is returned, and can then be used for prediction. The
#' "minimum_leaf_size" parameter specifies the minimum number of training points
#' that must fall into each leaf for it to be split.  The "minimum_gain_split"
#' parameter specifies the minimum gain that is needed for the node to split. 
#' The "maximum_depth" parameter specifies the maximum depth of the tree.  If
#' "print_training_accuracy" is specified, the training accuracy will be
#' printed.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples

#' # 
#' # #' # \dontrun{
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
#' # model <- decision_tree_train(training=X_train, labels=y_train,
#' #   minimum_leaf_size=20, minimum_gain_split=0.001)
#' #   }
decision_tree_train <- function(training,
                                labels = NA,
                                maximum_depth = 0,
                                minimum_gain_split = 1e-07,
                                minimum_leaf_size = 20,
                                print_training_accuracy = FALSE,
                                verbose = getOption("mlpack.verbose", FALSE),
                                weights = NA) {
  # Create parameters and timers objects.
  p <- CreateParams("decision_tree_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  training <- to_matrix_with_info(training)
  SetParamMatWithInfo(p, "training", training$info, training$data)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamInt(p, "maximum_depth", maximum_depth)

  SetParamDouble(p, "minimum_gain_split", minimum_gain_split)

  SetParamInt(p, "minimum_leaf_size", minimum_leaf_size)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  SetParamBool(p, "verbose", verbose)

  if (!identical(weights, NA)) {
    SetParamMat(p, "weights", to_matrix(weights), TRUE)
  }

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  decision_tree_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamDecisionTreeModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "DecisionTreeModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_decision_tree", "mlpack_model_binding", "list")

  return(out)
}
