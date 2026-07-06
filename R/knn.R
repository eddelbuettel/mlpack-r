#' @title k-Nearest-Neighbors Search
#'
#' @description
#' An implementation of k-nearest-neighbor search using single-tree and
#' dual-tree algorithms.  Given a set of reference points and query points, this
#' can find the k nearest neighbors in the reference set of each query point
#' using trees; trees that are built can be saved for future use.
#'
#' @param algorithm Type of neighbor search: 'naive', 'single_tree',
#'   'dual_tree', 'greedy'.  Default value "dual_tree" (character).
#' @param epsilon If specified, will do approximate nearest neighbor search
#'   with given relative error.  Default value "0" (numeric).
#' @param input_model Pre-trained kNN model (KNNModel).
#' @param k Number of nearest neighbors to find.  Default value "0"
#'   (integer).
#' @param leaf_size Leaf size for tree building (used for kd-trees, vp
#'   trees, random projection trees, UB trees, R trees, R* trees, X trees,
#'   Hilbert R trees, R+ trees, R++ trees, spill trees, and octrees).  Default
#'   value "20" (integer).
#' @param query Matrix containing query points (optional) (numeric
#'   matrix).
#' @param random_basis Before tree-building, project the data onto a random
#'   orthogonal basis.  Default value "FALSE" (logical).
#' @param reference Matrix containing the reference dataset (numeric
#'   matrix).
#' @param rho Balance threshold (only valid for spill trees).  Default
#'   value "0.7" (numeric).
#' @param seed Random seed (if 0, std::time(NULL) is used).  Default value
#'   "0" (integer).
#' @param tau Overlapping size (only valid for spill trees).  Default value
#'   "0" (numeric).
#' @param tree_type Type of tree to use: 'kd', 'vp', 'rp', 'max-rp', 'ub',
#'   'cover', 'r', 'r-star', 'x', 'ball', 'hilbert-r', 'r-plus', 'r-plus-plus',
#'   'spill', 'oct'.  Default value "kd" (character).
#' @param true_distances Matrix of true distances to compute the effective
#'   error (average relative error) (it is printed when -v is specified)
#'   (numeric matrix).
#' @param true_neighbors Matrix of true neighbors to compute the recall (it
#'   is printed when -v is specified) (integer matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{distances}{Matrix to output distances into (numeric matrix).}
#' \item{neighbors}{Matrix to output neighbors into (integer matrix).}
#' \item{output_model}{If specified, the kNN model will be output here
#'   (KNNModel).}
#'
#' @details
#' This program will calculate the k-nearest-neighbors of a set of points using
#' kd-trees or cover trees (cover tree support is experimental and may be slow).
#' You may specify a separate set of reference points and query points, or just
#' a reference set which will be used as both the reference and query set.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command will calculate the 5 nearest neighbors
#' # of each point in "input" and store the distances in "distances" and the
#' # neighbors in "neighbors": 
#' # 
#' # \dontrun{
#' # output <- knn(k=5, reference=input)
#' # neighbors <- output$neighbors
#' # distances <- output$distances
#' # }
#' # #' # 
#' # The output is organized such that row i and column j in the neighbors
#' # output matrix corresponds to the index of the point in the reference set
#' # which is the j'th nearest neighbor from the point in the query set with
#' # index i.  Row j and column i in the distances output matrix corresponds to
#' # the distance between those two points.
knn <- function(algorithm = "dual_tree",
                epsilon = 0,
                input_model = NA,
                k = 0,
                leaf_size = 20,
                query = NA,
                random_basis = FALSE,
                reference = NA,
                rho = 0.7,
                seed = 0,
                tau = 0,
                tree_type = "kd",
                true_distances = NA,
                true_neighbors = NA,
                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("knn")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamString(p, "algorithm", algorithm)

  SetParamDouble(p, "epsilon", epsilon)

  if (!identical(input_model, NA)) {
    SetParamKNNModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "k", k)

  SetParamInt(p, "leaf_size", leaf_size)

  if (!identical(query, NA)) {
    SetParamMat(p, "query", to_matrix(query), TRUE)
  }

  SetParamBool(p, "random_basis", random_basis)

  if (!identical(reference, NA)) {
    SetParamMat(p, "reference", to_matrix(reference), TRUE)
  }

  SetParamDouble(p, "rho", rho)

  SetParamInt(p, "seed", seed)

  SetParamDouble(p, "tau", tau)

  SetParamString(p, "tree_type", tree_type)

  if (!identical(true_distances, NA)) {
    SetParamMat(p, "true_distances", to_matrix(true_distances), TRUE)
  }

  if (!identical(true_neighbors, NA)) {
    SetParamUMat(p, "true_neighbors", to_matrix(true_neighbors))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "distances")
  SetPassed(p, "neighbors")
  SetPassed(p, "output_model")

  # Call the program.
  knn_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamKNNModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "KNNModel"

  # Extract the results in order.
  out <- list(
      "distances" = GetParamMat(p, "distances"),
      "neighbors" = GetParamUMat(p, "neighbors"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_knn", "mlpack_model_binding", "list")

  return(out)
}
