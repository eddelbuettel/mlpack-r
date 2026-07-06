#' @title Fast Euclidean Minimum Spanning Tree
#'
#' @description
#' An implementation of the Dual-Tree Boruvka algorithm for computing the
#' Euclidean minimum spanning tree of a set of input points.
#'
#' @param input Input data matrix (numeric matrix).
#' @param leaf_size Leaf size in the kd-tree.  One-element leaves give the
#'   empirically best performance, but at the cost of greater memory
#'   requirements.  Default value "1" (integer).
#' @param naive Compute the MST using O(n^2) naive algorithm.  Default
#'   value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Output data.  Stored as an edge list (numeric matrix).}
#'
#' @details
#' This program can compute the Euclidean minimum spanning tree of a set of
#' input points using the dual-tree Boruvka algorithm.
#' 
#' The set to calculate the minimum spanning tree of is specified with the
#' "input" parameter, and the output may be saved with the "output" output
#' parameter.
#' 
#' The "leaf_size" parameter controls the leaf size of the kd-tree that is used
#' to calculate the minimum spanning tree, and if the "naive" option is given,
#' then brute-force search is used (this is typically much slower in low
#' dimensions).  The leaf size does not affect the results, but it may have some
#' effect on the runtime of the algorithm.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the minimum spanning tree of the input dataset "data" can be
#' # calculated with a leaf size of 20 and stored as "spanning_tree" using the
#' # following command:
#' # 
#' # \dontrun{
#' # spanning_tree <- emst(input=data, leaf_size=20)
#' # }
#' # #' # 
#' # The output matrix is a three-dimensional matrix, where each row indicates
#' # an edge.  The first dimension corresponds to the lesser index of the edge;
#' # the second dimension corresponds to the greater index of the edge; and the
#' # third column corresponds to the distance between the two points.
emst <- function(input,
                 leaf_size = 1,
                 naive = FALSE,
                 verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("emst")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamInt(p, "leaf_size", leaf_size)

  SetParamBool(p, "naive", naive)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  emst_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_emst", "mlpack_model_binding", "list")

  return(out)
}
