#' @title Hoeffding trees training
#'
#' @description
#' An implementation of Hoeffding trees, a form of streaming decision tree for
#' classification.  Given labeled data a Hoeffding tree can be trained for later
#' use of predicting the classifications of new points.
#'
#' @param training Training dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param batch_mode If true, samples will be considered in batch instead
#'   of as a stream.  This generally results in better trees but at the cost of
#'   memory usage and runtime.  Default value "FALSE" (logical).
#' @param bins If the 'domingos' split strategy is used, this specifies the
#'   number of bins for each numeric split.  Default value "10" (integer).
#' @param confidence Confidence before splitting (between 0 and 1). 
#'   Default value "0.95" (numeric).
#' @param info_gain If set, information gain is used instead of Gini
#'   impurity for calculating Hoeffding bounds.  Default value "FALSE"
#'   (logical).
#' @param labels Labels for training dataset (integer row).
#' @param max_samples Maximum number of samples before splitting.  Default
#'   value "5000" (integer).
#' @param min_samples Minimum number of samples before splitting.  Default
#'   value "100" (integer).
#' @param numeric_split_strategy The splitting strategy to use for numeric
#'   features: 'domingos' or 'binary'.  Default value "binary" (character).
#' @param observations_before_binning If the 'domingos' split strategy is
#'   used, this specifies the number of samples observed before binning is
#'   performed.  Default value "100" (integer).
#' @param passes Number of passes to take over the dataset.  Default value
#'   "1" (integer).
#' @param test Testing dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param test_labels Labels of test data (integer row).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained Hoeffding tree model
#'   (HoeffdingTreeModel).}
#'
#' @details
#' Implements Hoeffding trees, a form of streaming decision tree suited best for
#' large (or streaming) datasets, supporting both categorical and numeric data. 
#' Given an input dataset, it is able to train the tree with numerous training
#' options, and return the model.
#' 
#' The training file and associated labels are specified with the "training" and
#' "labels" parameters, respectively. Optionally, if "labels" is not specified,
#' the labels are assumed to be the last dimension of the training dataset.
#' 
#' The training may be performed in batch mode (like a typical decision tree
#' algorithm) by specifying the "batch_mode" option, but this may not be the
#' best option for large datasets.
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
#' # model <- hoeffding_tree_train(training=X_train, labels=y_train)
#' # 
hoeffding_tree_train <- function(training,
                                 batch_mode = FALSE,
                                 bins = 10,
                                 confidence = 0.95,
                                 info_gain = FALSE,
                                 labels = NA,
                                 max_samples = 5000,
                                 min_samples = 100,
                                 numeric_split_strategy = "binary",
                                 observations_before_binning = 100,
                                 passes = 1,
                                 test = NA,
                                 test_labels = NA,
                                 verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hoeffding_tree_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  training <- to_matrix_with_info(training)
  SetParamMatWithInfo(p, "training", training$info, training$data)

  SetParamBool(p, "batch_mode", batch_mode)

  SetParamInt(p, "bins", bins)

  SetParamDouble(p, "confidence", confidence)

  SetParamBool(p, "info_gain", info_gain)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamInt(p, "max_samples", max_samples)

  SetParamInt(p, "min_samples", min_samples)

  SetParamString(p, "numeric_split_strategy", numeric_split_strategy)

  SetParamInt(p, "observations_before_binning", observations_before_binning)

  SetParamInt(p, "passes", passes)

  if (!identical(test, NA)) {
    test <- to_matrix_with_info(test)
    SetParamMatWithInfo(p, "test", test$info, test$data)
  }

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  hoeffding_tree_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamHoeffdingTreeModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "HoeffdingTreeModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hoeffding_tree", "mlpack_model_binding", "list")

  return(out)
}
