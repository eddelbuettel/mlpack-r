#' @title K-Approximate-Nearest-Neighbor Search with LSH
#'
#' @description
#' An implementation of approximate k-nearest-neighbor search with
#' locality-sensitive hashing (LSH).  Given a set of reference points and a set
#' of query points, this will compute the k approximate nearest neighbors of
#' each query point in the reference set; models can be saved for future use.
#'
#' @param bucket_size The size of a bucket in the second level hash. 
#'   Default value "500" (integer).
#' @param hash_width The hash width for the first-level hashing in the LSH
#'   preprocessing. By default, the LSH class automatically estimates a hash
#'   width for its use.  Default value "0" (numeric).
#' @param input_model Input LSH model (LSHSearch).
#' @param k Number of nearest neighbors to find.  Default value "0"
#'   (integer).
#' @param num_probes Number of additional probes for multiprobe LSH; if 0,
#'   traditional LSH is used.  Default value "0" (integer).
#' @param projections The number of hash functions for each tabl.  Default
#'   value "10" (integer).
#' @param query Matrix containing query points (optional) (numeric
#'   matrix).
#' @param reference Matrix containing the reference dataset (numeric
#'   matrix).
#' @param second_hash_size The size of the second level hash table. 
#'   Default value "99901" (integer).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param tables The number of hash tables to be used.  Default value "30"
#'   (integer).
#' @param true_neighbors Matrix of true neighbors to compute recall with
#'   (the recall is printed when -v is specified) (integer matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{distances}{Matrix to output distances into (numeric matrix).}
#' \item{neighbors}{Matrix to output neighbors into (integer matrix).}
#' \item{output_model}{Output for trained LSH model (LSHSearch).}
#'
#' @details
#' This program will calculate the k approximate-nearest-neighbors of a set of
#' points using locality-sensitive hashing. You may specify a separate set of
#' reference points and query points, or just a reference set which will be used
#' as both the reference and query set. 
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following will return 5 neighbors from the data for each
#' # point in "input" and store the distances in "distances" and the neighbors
#' # in "neighbors":
#' # 
#' # \dontrun{
#' # output <- lsh(k=5, reference=input)
#' # distances <- output$distances
#' # neighbors <- output$neighbors
#' # }
#' # #' # 
#' # The output is organized such that row i and column j in the neighbors
#' # output corresponds to the index of the point in the reference set which is
#' # the j'th nearest neighbor from the point in the query set with index i. 
#' # Row j and column i in the distances output file corresponds to the distance
#' # between those two points.
#' # 
#' # Because this is approximate-nearest-neighbors search, results may be
#' # different from run to run.  Thus, the "seed" parameter can be specified to
#' # set the random seed.
#' # 
#' # This program also has many other parameters to control its functionality;
#' # see the parameter-specific documentation for more information.
lsh <- function(bucket_size = 500,
                hash_width = 0,
                input_model = NA,
                k = 0,
                num_probes = 0,
                projections = 10,
                query = NA,
                reference = NA,
                second_hash_size = 99901,
                seed = 0,
                tables = 30,
                true_neighbors = NA,
                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("lsh")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "bucket_size", bucket_size)

  SetParamDouble(p, "hash_width", hash_width)

  if (!identical(input_model, NA)) {
    SetParamLSHSearchPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "k", k)

  SetParamInt(p, "num_probes", num_probes)

  SetParamInt(p, "projections", projections)

  if (!identical(query, NA)) {
    SetParamMat(p, "query", to_matrix(query), TRUE)
  }

  if (!identical(reference, NA)) {
    SetParamMat(p, "reference", to_matrix(reference), TRUE)
  }

  SetParamInt(p, "second_hash_size", second_hash_size)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "tables", tables)

  if (!identical(true_neighbors, NA)) {
    SetParamUMat(p, "true_neighbors", to_matrix(true_neighbors))
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "distances")
  SetPassed(p, "neighbors")
  SetPassed(p, "output_model")

  # Call the program.
  lsh_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLSHSearchPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LSHSearch"

  # Extract the results in order.
  out <- list(
      "distances" = GetParamMat(p, "distances"),
      "neighbors" = GetParamUMat(p, "neighbors"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_lsh", "mlpack_model_binding", "list")

  return(out)
}
