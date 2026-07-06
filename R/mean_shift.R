#' @title Mean Shift Clustering
#'
#' @description
#' A fast implementation of mean-shift clustering using dual-tree range search. 
#' Given a dataset, this uses the mean shift algorithm to produce and return a
#' clustering of the data.
#'
#' @param input Input dataset to perform clustering on (numeric matrix).
#' @param force_convergence If specified, the mean shift algorithm will
#'   continue running regardless of max_iterations until the clusters converge. 
#'   Default value "FALSE" (logical).
#' @param in_place If specified, a column containing the learned cluster
#'   assignments will be added to the input dataset file.  In this case,
#'   --output_file is overridden.  (Do not use with Python..  Default value
#'   "FALSE" (logical).
#' @param labels_only If specified, only the output labels will be written
#'   to the file specified by --output_file.  Default value "FALSE" (logical).
#' @param max_iterations Maximum number of iterations before mean shift
#'   terminates.  Default value "1000" (integer).
#' @param radius If the distance between two centroids is less than the
#'   given radius, one will be removed.  A radius of 0 or less means an estimate
#'   will be calculated and used for the radius.  Default value "0" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{centroid}{If specified, the centroids of each cluster will be
#'   written to the given matrix (numeric matrix).}
#' \item{output}{Matrix to write output labels or labeled data to (numeric
#'   matrix).}
#'
#' @details
#' This program performs mean shift clustering on the given dataset, storing the
#' learned cluster assignments either as a column of labels in the input dataset
#' or separately.
#' 
#' The input dataset should be specified with the "input" parameter, and the
#' radius used for search can be specified with the "radius" parameter.  The
#' maximum number of iterations before algorithm termination is controlled with
#' the "max_iterations" parameter.
#' 
#' The output labels may be saved with the "output" output parameter and the
#' centroids of each cluster may be saved with the "centroid" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run mean shift clustering on the dataset "data" and store
#' # the centroids to "centroids", the following command may be used: 
#' # 
#' # \dontrun{
#' # output <- mean_shift(input=data)
#' # centroids <- output$centroid
#' # }
mean_shift <- function(input,
                       force_convergence = FALSE,
                       in_place = FALSE,
                       labels_only = FALSE,
                       max_iterations = 1000,
                       radius = 0,
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("mean_shift")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamBool(p, "force_convergence", force_convergence)

  SetParamBool(p, "in_place", in_place)

  SetParamBool(p, "labels_only", labels_only)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamDouble(p, "radius", radius)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "centroid")
  SetPassed(p, "output")

  # Call the program.
  mean_shift_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "centroid" = GetParamMat(p, "centroid"),
      "output" = GetParamMat(p, "output")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_mean", "mlpack_model_binding", "list")

  return(out)
}
