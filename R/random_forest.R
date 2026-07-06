#' @title Random Forests
#'
#' @description
#' An implementation of the standard random forest algorithm by Leo Breiman for
#' classification.  Given labeled data, a random forest can be trained and saved
#' for future use; or, a pre-trained random forest can be used for
#' classification.
#'
#' @param input_model Pre-trained random forest to use for classification
#'   (RandomForestModel).
#' @param labels Labels for training dataset (integer row).
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
#' @param test Test dataset to produce predictions for (numeric matrix).
#' @param test_labels Test dataset labels, if accuracy calculation is
#'   desired (integer row).
#' @param training Training dataset (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param warm_start If true and passed along with `training` and
#'   `input_model` then trains more trees on top of existing model.  Default
#'   value "FALSE" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Model to save trained random forest to
#'   (RandomForestModel).}
#' \item{predictions}{Predicted classes for each point in the test set
#'   (integer row).}
#' \item{probabilities}{Predicted class probabilities for each point in the
#'   test set (numeric matrix).}
#'
#' @details
#' This program is an implementation of the standard random forest
#' classification algorithm by Leo Breiman.  A random forest can be trained and
#' saved for later use, or a random forest may be loaded and predictions or
#' class probabilities for points may be generated.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range `[0,
#' num_classes - 1]`. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' When a model is trained, the "output_model" output parameter may be used to
#' save the trained model.  A model may be loaded for predictions with the
#' "input_model"parameter. The "input_model" parameter may not be specified when
#' the "training" parameter is specified.  The "minimum_leaf_size" parameter
#' specifies the minimum number of training points that must fall into each leaf
#' for it to be split.  The "num_trees" controls the number of trees in the
#' random forest.  The "minimum_gain_split" parameter controls the minimum
#' required gain for a decision tree node to split.  Larger values will force
#' higher-confidence splits.  The "maximum_depth" parameter specifies the
#' maximum depth of the tree.  The "subspace_dim" parameter is used to control
#' the number of random dimensions chosen for an individual node's split.  If
#' "print_training_accuracy" is specified, the calculated accuracy on the
#' training set will be printed.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' measures are desired for that test set, labels for the test points may be
#' specified with the "test_labels" parameter.  Predictions for each test point
#' may be saved via the "predictions"output parameter.  Class probabilities for
#' each prediction may be saved with the "probabilities" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a random forest with a minimum leaf size of 20 using
#' # 10 trees on the dataset contained in "data"with labels "labels", saving the
#' # output random forest to "rf_model" and printing the training error, one
#' # could call
#' # 
#' # \dontrun{
#' # output <- random_forest(training=data, labels=labels, minimum_leaf_size=20,
#' #   num_trees=10, print_training_accuracy=TRUE)
#' # rf_model <- output$output_model
#' # }
#' # 
#' # Then, to use that model to classify points in "test_set" and print the test
#' # error given the labels "test_labels" using that model, while saving the
#' # predictions for each point to "predictions", one could call 
#' # 
#' # \dontrun{
#' # output <- random_forest(input_model=rf_model, test=test_set,
#' #   test_labels=test_labels)
#' # predictions <- output$predictions
#' # }
random_forest <- function(input_model = NA,
                          labels = NA,
                          maximum_depth = 0,
                          minimum_gain_split = 0,
                          minimum_leaf_size = 1,
                          num_trees = 10,
                          print_training_accuracy = FALSE,
                          seed = 0,
                          subspace_dim = 0,
                          test = NA,
                          test_labels = NA,
                          training = NA,
                          verbose = getOption("mlpack.verbose", FALSE),
                          warm_start = FALSE) {
  # Create parameters and timers objects.
  p <- CreateParams("random_forest")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input_model, NA)) {
    SetParamRandomForestModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamInt(p, "maximum_depth", maximum_depth)

  SetParamDouble(p, "minimum_gain_split", minimum_gain_split)

  SetParamInt(p, "minimum_leaf_size", minimum_leaf_size)

  SetParamInt(p, "num_trees", num_trees)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "subspace_dim", subspace_dim)

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

  SetParamBool(p, "warm_start", warm_start)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  random_forest_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamRandomForestModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "RandomForestModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_random", "mlpack_model_binding", "list")

  return(out)
}
