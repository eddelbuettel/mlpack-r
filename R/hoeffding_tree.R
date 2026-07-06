#' @title Hoeffding trees
#'
#' @description
#' An implementation of Hoeffding trees, a form of streaming decision tree for
#' classification.  Given labeled data, a Hoeffding tree can be trained and
#' saved for later use, or a pre-trained Hoeffding tree can be used for
#' predicting the classifications of new points.
#'
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
#' @param input_model Input trained Hoeffding tree model
#'   (HoeffdingTreeModel).
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
#' @param training Training dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained Hoeffding tree model
#'   (HoeffdingTreeModel).}
#' \item{predictions}{Matrix to output label predictions for test data into
#'   (integer row).}
#' \item{probabilities}{In addition to predicting labels, provide rediction
#'   probabilities in this matrix (numeric matrix).}
#'
#' @details
#' This program implements Hoeffding trees, a form of streaming decision tree
#' suited best for large (or streaming) datasets.  This program supports both
#' categorical and numeric data.  Given an input dataset, this program is able
#' to train the tree with numerous training options, and save the model to a
#' file.  The program is also able to use a trained model or a model from file
#' in order to predict classes for a given test set.
#' 
#' The training file and associated labels are specified with the "training" and
#' "labels" parameters, respectively. Optionally, if "labels" is not specified,
#' the labels are assumed to be the last dimension of the training dataset.
#' 
#' The training may be performed in batch mode (like a typical decision tree
#' algorithm) by specifying the "batch_mode" option, but this may not be the
#' best option for large datasets.
#' 
#' When a model is trained, it may be saved via the "output_model" output
#' parameter.  A model may be loaded from file for further training or testing
#' with the "input_model" parameter.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' statistics are desired for that test set, labels may be specified with the
#' "test_labels" parameter.  Predictions for each test point may be saved with
#' the "predictions" output parameter, and class probabilities for each
#' prediction may be saved with the "probabilities" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a Hoeffding tree with confidence 0.99 with data
#' # "dataset", saving the trained tree to "tree", the following command may be
#' # used:
#' # 
#' # \dontrun{
#' # output <- hoeffding_tree(training=dataset, confidence=0.99)
#' # tree <- output$output_model
#' # }
#' # 
#' # Then, this tree may be used to make predictions on the test set "test_set",
#' # saving the predictions into "predictions" and the class probabilities into
#' # "class_probs" with the following command: 
#' # 
#' # \dontrun{
#' # output <- hoeffding_tree(input_model=tree, test=test_set)
#' # predictions <- output$predictions
#' # class_probs <- output$probabilities
#' # }
hoeffding_tree <- function(batch_mode = FALSE,
                           bins = 10,
                           confidence = 0.95,
                           info_gain = FALSE,
                           input_model = NA,
                           labels = NA,
                           max_samples = 5000,
                           min_samples = 100,
                           numeric_split_strategy = "binary",
                           observations_before_binning = 100,
                           passes = 1,
                           test = NA,
                           test_labels = NA,
                           training = NA,
                           verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hoeffding_tree")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamBool(p, "batch_mode", batch_mode)

  SetParamInt(p, "bins", bins)

  SetParamDouble(p, "confidence", confidence)

  SetParamBool(p, "info_gain", info_gain)

  if (!identical(input_model, NA)) {
    SetParamHoeffdingTreeModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

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

  if (!identical(training, NA)) {
    training <- to_matrix_with_info(training)
    SetParamMatWithInfo(p, "training", training$info, training$data)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "probabilities")

  # Call the program.
  hoeffding_tree_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamHoeffdingTreeModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "HoeffdingTreeModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamURow(p, "predictions"),
      "probabilities" = GetParamMat(p, "probabilities")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hoeffding", "mlpack_model_binding", "list")

  return(out)
}
