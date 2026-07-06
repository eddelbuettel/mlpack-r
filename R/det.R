#' @title Density Estimation With Density Estimation Trees
#'
#' @description
#' An implementation of density estimation trees for the density estimation
#' task.  Density estimation trees can be trained or used to predict the density
#' at locations given by query points.
#'
#' @param folds The number of folds of cross-validation to perform for the
#'   estimation (0 is LOOCV.  Default value "10" (integer).
#' @param input_model Trained density estimation tree to load (DTree).
#' @param max_leaf_size The maximum size of a leaf in the unpruned, fully
#'   grown DET.  Default value "10" (integer).
#' @param min_leaf_size The minimum size of a leaf in the unpruned, fully
#'   grown DET.  Default value "5" (integer).
#' @param path_format The format of path printing: 'lr', 'id-lr', or
#'   'lr-id'.  Default value "lr" (character).
#' @param skip_pruning Whether to bypass the pruning process and output the
#'   unpruned tree only.  Default value "FALSE" (logical).
#' @param test A set of test points to estimate the density of (numeric
#'   matrix).
#' @param training The data set on which to build a density estimation tree
#'   (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output to save trained density estimation tree to
#'   (DTree).}
#' \item{tag_counters_file}{The file to output the number of points that
#'   went to each leaf.  Default value "" (character).}
#' \item{tag_file}{The file to output the tags (and possibly paths) for
#'   each sample in the test set.  Default value "" (character).}
#' \item{test_set_estimates}{The output estimates on the test set from the
#'   final optimally pruned tree (numeric matrix).}
#' \item{training_set_estimates}{The output density estimates on the
#'   training set from the final optimally pruned tree (numeric matrix).}
#' \item{vi}{The output variable importance values for each feature
#'   (numeric matrix).}
#'
#' @details
#' This program performs a number of functions related to Density Estimation
#' Trees.  The optimal Density Estimation Tree (DET) can be trained on a set of
#' data (specified by "training") using cross-validation (with number of folds
#' specified with the "folds" parameter).  This trained density estimation tree
#' may then be saved with the "output_model" output parameter.
#' 
#' The variable importances (that is, the feature importance values for each
#' dimension) may be saved with the "vi" output parameter, and the density
#' estimates for each training point may be saved with the
#' "training_set_estimates" output parameter.
#' 
#' Enabling path printing for each node outputs the path from the root node to a
#' leaf for each entry in the test set, or training set (if a test set is not
#' provided).  Strings like 'LRLRLR' (indicating that traversal went to the left
#' child, then the right child, then the left child, and so forth) will be
#' output. If 'lr-id' or 'id-lr' are given as the "path_format" parameter, then
#' the ID (tag) of every node along the path will be printed after or before the
#' L or R character indicating the direction of traversal, respectively.
#' 
#' This program also can provide density estimates for a set of test points,
#' specified in the "test" parameter.  The density estimation tree used for this
#' task will be the tree that was trained on the given training points, or a
#' tree given as the parameter "input_model".  The density estimates for the
#' test points may be saved using the "test_set_estimates" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
det <- function(folds = 10,
                input_model = NA,
                max_leaf_size = 10,
                min_leaf_size = 5,
                path_format = "lr",
                skip_pruning = FALSE,
                test = NA,
                training = NA,
                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("det")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "folds", folds)

  if (!identical(input_model, NA)) {
    SetParamDTreePtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "max_leaf_size", max_leaf_size)

  SetParamInt(p, "min_leaf_size", min_leaf_size)

  SetParamString(p, "path_format", path_format)

  SetParamBool(p, "skip_pruning", skip_pruning)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "tag_counters_file")
  SetPassed(p, "tag_file")
  SetPassed(p, "test_set_estimates")
  SetPassed(p, "training_set_estimates")
  SetPassed(p, "vi")

  # Call the program.
  det_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamDTreePtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "DTree"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "tag_counters_file" = GetParamString(p, "tag_counters_file"),
      "tag_file" = GetParamString(p, "tag_file"),
      "test_set_estimates" = GetParamMat(p, "test_set_estimates"),
      "training_set_estimates" = GetParamMat(p, "training_set_estimates"),
      "vi" = GetParamMat(p, "vi")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_det", "mlpack_model_binding", "list")

  return(out)
}
