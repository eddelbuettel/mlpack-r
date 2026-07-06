#' @title Random Forests train
#'
#' @description
#' An implementation of the standard random forest algorithm by Leo Breiman for
#' classification.  Given labeled data, a random forest is trained.
#'
#' @param labels Labels for training dataset (integer row).
#' @param training Training dataset (numeric matrix).
#' @param maximum_depth Maximum depth of the tree (0 means no limit). 
#'   Default value "0" (integer).
#' @param minimum_gain_split Minimum gain needed to make a split when
#'   building a tree.  Default value "0" (numeric).
#' @param minimum_leaf_size Minimum number of points in each leaf node. 
#'   Default value "1" (integer).
#' @param num_trees Number of trees in the random forest.  Default value
#'   "10" (integer).
#' @param print_training_accuracy If set, then the accuracy of the model on
#'   the training set will be predicted (verbose must also be specified). 
#'   Default value "FALSE" (logical).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param subspace_dim Dimensionality of random subspace to use for each
#'   split.  '0' will autoselect the square root of data dimensionality. 
#'   Default value "0" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Model to save trained random forest to
#'   (RandomForestModel).}
#'
#' @details
#' This program is an implementation of the standard random forest
#' classification algorithm by Leo Breiman.  A random forest is trained (and
#' returned for later use for subsequent use where predictions or class
#' probabilities for points may be generated.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range `[0,
#' num_classes - 1]`. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' The "minimum_leaf_size" parameter specifies the minimum number of training
#' points that must fall into each leaf for it to be split.  The "num_trees"
#' controls the number of trees in the random forest.  The "minimum_gain_split"
#' parameter controls the minimum required gain for a decision tree node to
#' split.  Larger values will force higher-confidence splits.  The
#' "maximum_depth" parameter specifies the maximum depth of the tree.  The
#' "subspace_dim" parameter is used to control the number of random dimensions
#' chosen for an individual node's split.  If "print_training_accuracy" is
#' specified, the calculated accuracy on the training set will be printed.
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
#' # model <- random_forest_train(training=X_train, labels=y_train,
#' #   minimum_leaf_size=20, num_trees=10, print_training_accuracy=TRUE)
#' #   }
random_forest_train <- function(labels,
                                training,
                                maximum_depth = 0,
                                minimum_gain_split = 0,
                                minimum_leaf_size = 1,
                                num_trees = 10,
                                print_training_accuracy = FALSE,
                                seed = 0,
                                subspace_dim = 0,
                                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("random_forest_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamURow(p, "labels", to_matrix(labels))

  SetParamMat(p, "training", to_matrix(training), TRUE)

  SetParamInt(p, "maximum_depth", maximum_depth)

  SetParamDouble(p, "minimum_gain_split", minimum_gain_split)

  SetParamInt(p, "minimum_leaf_size", minimum_leaf_size)

  SetParamInt(p, "num_trees", num_trees)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "subspace_dim", subspace_dim)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  random_forest_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamRandomForestModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "RandomForestModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_random_forest", "mlpack_model_binding", "list")

  return(out)
}
