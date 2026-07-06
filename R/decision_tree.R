#' @title Decision tree
#'
#' @description
#' An implementation of an ID3-style decision tree for classification, which
#' supports categorical data.  Given labeled data with numeric or categorical
#' features, a decision tree can be trained and saved; or, an existing decision
#' tree can be used for classification on new points.
#'
#' @param input_model Pre-trained decision tree, to be used with test
#'   points (DecisionTreeModel).
#' @param labels Training labels (integer row).
#' @param maximum_depth Maximum depth of the tree (0 means no limit). 
#'   Default value "0" (integer).
#' @param minimum_gain_split Minimum gain for node splitting.  Default
#'   value "1e-07" (numeric).
#' @param minimum_leaf_size Minimum number of points in a leaf.  Default
#'   value "20" (integer).
#' @param print_training_accuracy Print the training accuracy.  Default
#'   value "FALSE" (logical).
#' @param test Testing dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param test_labels Test point labels, if accuracy calculation is desired
#'   (integer row).
#' @param training Training dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param weights The weight of label (numeric matrix).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained decision tree
#'   (DecisionTreeModel).}
#' \item{predictions}{Class predictions for each test point (integer
#'   row).}
#' \item{probabilities}{Class probabilities for each test point (numeric
#'   matrix).}
#'
#' @details
#' Train and evaluate using a decision tree.  Given a dataset containing numeric
#' or categorical features, and associated labels for each point in the dataset,
#' this program can train a decision tree on that data.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range `[0,
#' num_classes - 1]`. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' When a model is trained, the "output_model" output parameter may be used to
#' save the trained model.  A model may be loaded for predictions with the
#' "input_model" parameter.  The "input_model" parameter may not be specified
#' when the "training" parameter is specified.  The "minimum_leaf_size"
#' parameter specifies the minimum number of training points that must fall into
#' each leaf for it to be split.  The "minimum_gain_split" parameter specifies
#' the minimum gain that is needed for the node to split.  The "maximum_depth"
#' parameter specifies the maximum depth of the tree.  If
#' "print_training_accuracy" is specified, the training accuracy will be
#' printed.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' numbers are desired for that test set, labels may be specified with the
#' "test_labels" parameter.  Predictions for each test point may be saved via
#' the "predictions" output parameter.  Class probabilities for each prediction
#' may be saved with the "probabilities" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a decision tree with a minimum leaf size of 20 on the
#' # dataset contained in "data" with labels "labels", saving the output model
#' # to "tree" and printing the training error, one could call
#' # 
#' # \dontrun{
#' # output <- decision_tree(training=data, labels=labels, minimum_leaf_size=20,
#' #   minimum_gain_split=0.001, print_training_accuracy=TRUE)
#' # tree <- output$output_model
#' # }
#' # 
#' # Then, to use that model to classify points in "test_set" and print the test
#' # error given the labels "test_labels" using that model, while saving the
#' # predictions for each point to "predictions", one could call 
#' # 
#' # \dontrun{
#' # output <- decision_tree(input_model=tree, test=test_set,
#' #   test_labels=test_labels)
#' # predictions <- output$predictions
#' # }
decision_tree <- function(input_model = NA,
                          labels = NA,
                          maximum_depth = 0,
                          minimum_gain_split = 1e-07,
                          minimum_leaf_size = 20,
                          print_training_accuracy = FALSE,
                          test = NA,
                          test_labels = NA,
                          training = NA,
                          verbose = getOption("mlpack.verbose", FALSE),
                          weights = NA) {
  # Create parameters and timers objects.
  p <- CreateParams("decision_tree")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input_model, NA)) {
    SetParamDecisionTreeModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamInt(p, "maximum_depth", maximum_depth)

  SetParamDouble(p, "minimum_gain_split", minimum_gain_split)

  SetParamInt(p, "minimum_leaf_size", minimum_leaf_size)

  SetParamBool(p, "print_training_accuracy", print_training_accuracy)

  if (!identical(test, NA)) {
    test <- to_matrix_with_info(test)
    SetParamMatWithInfo(p, "test", test$info, test$data)
  }

  if (!identical(test_labels, NA)) {
    SetParamURow(p, "test_labels", to_matrix(test_labels))
  }

  if (!identical(training, NA)) {
    training <- to_matrix_with_info(training)
    SetParamMatWithInfo(p, "training", training$info, training$data)
  }

  SetParamBool(p, "verbose", verbose)

  if (!identical(weights, NA)) {
    SetParamMat(p, "weights", to_matrix(weights), TRUE)
  }

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  decision_tree_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamDecisionTreeModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "DecisionTreeModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_decision", "mlpack_model_binding", "list")

  return(out)
}
