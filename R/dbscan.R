#' @title DBSCAN clustering
#'
#' @description
#' An implementation of DBSCAN clustering.  Given a dataset, this can compute
#' and return a clustering of that dataset.
#'
#' @param input Input dataset to cluster (numeric matrix).
#' @param epsilon Radius of each range search.  Default value "1"
#'   (numeric).
#' @param min_size Minimum number of points for a cluster.  Default value
#'   "5" (integer).
#' @param naive If set, brute-force range search (not tree-based) will be
#'   used.  Default value "FALSE" (logical).
#' @param selection_type If using point selection policy, the type of
#'   selection to use ('ordered', 'random').  Default value "ordered"
#'   (character).
#' @param single_mode If set, single-tree range search (not dual-tree) will
#'   be used.  Default value "FALSE" (logical).
#' @param tree_type If using single-tree or dual-tree search, the type of
#'   tree to use ('kd', 'r', 'r-star', 'x', 'hilbert-r', 'r-plus',
#'   'r-plus-plus', 'cover', 'ball').  Default value "kd" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{assignments}{Output matrix for assignments of each point (integer
#'   row).}
#' \item{centroids}{Matrix to save output centroids to (numeric matrix).}
#'
#' @details
#' This program implements the DBSCAN algorithm for clustering using accelerated
#' tree-based range search.  The type of tree that is used may be parameterized,
#' or brute-force range search may also be used.
#' 
#' The input dataset to be clustered may be specified with the "input"
#' parameter; the radius of each range search may be specified with the
#' "epsilon" parameters, and the minimum number of points in a cluster may be
#' specified with the "min_size" parameter.
#' 
#' The "assignments" and "centroids" output parameters may be used to save the
#' output of the clustering. "assignments" contains the cluster assignments of
#' each point, and "centroids" contains the centroids of each cluster.
#' 
#' The range search may be controlled with the "tree_type", "single_mode", and
#' "naive" parameters.  "tree_type" can control the type of tree used for range
#' search; this can take a variety of values: 'kd', 'r', 'r-star', 'x',
#' 'hilbert-r', 'r-plus', 'r-plus-plus', 'cover', 'ball'. The "single_mode"
#' parameter will force single-tree search (as opposed to the default dual-tree
#' search), and '"naive" will force brute-force range search.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # An example usage to run DBSCAN on the dataset in "input" with a radius of
#' # 0.5 and a minimum cluster size of 5 is given below:
#' # 
#' # \dontrun{
#' # dbscan(input=input, epsilon=0.5, min_size=5)
#' # }
dbscan <- function(input,
                   epsilon = 1,
                   min_size = 5,
                   naive = FALSE,
                   selection_type = "ordered",
                   single_mode = FALSE,
                   tree_type = "kd",
                   verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("dbscan")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamDouble(p, "epsilon", epsilon)

  SetParamInt(p, "min_size", min_size)

  SetParamBool(p, "naive", naive)

  SetParamString(p, "selection_type", selection_type)

  SetParamBool(p, "single_mode", single_mode)

  SetParamString(p, "tree_type", tree_type)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "assignments")
  SetPassed(p, "centroids")

  # Call the program.
  dbscan_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "assignments" = GetParamURow(p, "assignments"),
      "centroids" = GetParamMat(p, "centroids")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_dbscan", "mlpack_model_binding", "list")

  return(out)
}
