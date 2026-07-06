#' @title Approximate furthest neighbor search
#'
#' @description
#' An implementation of two strategies for furthest neighbor search.  This can
#' be used to compute the furthest neighbor of query point(s) from a set of
#' points; furthest neighbor models can be saved and reused with future query
#' point(s).
#'
#' @param algorithm Algorithm to use: 'ds' or 'qdafn'.  Default value "ds"
#'   (character).
#' @param calculate_error If set, calculate the average distance error for
#'   the first furthest neighbor only.  Default value "FALSE" (logical).
#' @param exact_distances Matrix containing exact distances to furthest
#'   neighbors; this can be used to avoid explicit calculation when
#'   --calculate_error is set (numeric matrix).
#' @param input_model File containing input model (ApproxKFNModel).
#' @param k Number of furthest neighbors to search for.  Default value "0"
#'   (integer).
#' @param num_projections Number of projections to use in each hash table. 
#'   Default value "5" (integer).
#' @param num_tables Number of hash tables to use.  Default value "5"
#'   (integer).
#' @param query Matrix containing query points (numeric matrix).
#' @param reference Matrix containing the reference dataset (numeric
#'   matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{distances}{Matrix to save furthest neighbor distances to (numeric
#'   matrix).}
#' \item{neighbors}{Matrix to save neighbor indices to (integer matrix).}
#' \item{output_model}{File to save output model to (ApproxKFNModel).}
#'
#' @details
#' This program implements two strategies for furthest neighbor search. These
#' strategies are:
#' 
#'  - The 'qdafn' algorithm from "Approximate Furthest Neighbor in High
#' Dimensions" by R. Pagh, F. Silvestri, J. Sivertsen, and M. Skala, in
#' Similarity Search and Applications 2015 (SISAP).
#'  - The 'DrusillaSelect' algorithm from "Fast approximate furthest neighbors
#' with data-dependent candidate selection", by R.R. Curtin and A.B. Gardner, in
#' Similarity Search and Applications 2016 (SISAP).
#' 
#' These two strategies give approximate results for the furthest neighbor
#' search problem and can be used as fast replacements for other furthest
#' neighbor techniques such as those found in the mlpack_kfn program.  Note that
#' typically, the 'ds' algorithm requires far fewer tables and projections than
#' the 'qdafn' algorithm.
#' 
#' Specify a reference set (set to search in) with "reference", specify a query
#' set with "query", and specify algorithm parameters with "num_tables" and
#' "num_projections" (or don't and defaults will be used).  The algorithm to be
#' used (either 'ds'---the default---or 'qdafn')  may be specified with
#' "algorithm".  Also specify the number of neighbors to search for with "k".
#' 
#' Note that for 'qdafn' in lower dimensions, "num_projections" may need to be
#' set to a high value in order to return results for each query point.
#' 
#' If no query set is specified, the reference set will be used as the query
#' set.  The "output_model" output parameter may be used to store the built
#' model, and an input model may be loaded instead of specifying a reference set
#' with the "input_model" option.
#' 
#' Results for each query point can be stored with the "neighbors" and
#' "distances" output parameters.  Each row of these output matrices holds the k
#' distances or neighbor indices for each query point.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to find the 5 approximate furthest neighbors with
#' # "reference_set" as the reference set and "query_set" as the query set using
#' # DrusillaSelect, storing the furthest neighbor indices to "neighbors" and
#' # the furthest neighbor distances to "distances", one could call
#' # 
#' # \dontrun{
#' # output <- approx_kfn(query=query_set, reference=reference_set, k=5,
#' #   algorithm="ds")
#' # neighbors <- output$neighbors
#' # distances <- output$distances
#' # }
#' # 
#' # and to perform approximate all-furthest-neighbors search with k=1 on the
#' # set "data" storing only the furthest neighbor distances to "distances", one
#' # could call
#' # 
#' # \dontrun{
#' # output <- approx_kfn(reference=reference_set, k=1)
#' # distances <- output$distances
#' # }
#' # 
#' # A trained model can be re-used.  If a model has been previously saved to
#' # "model", then we may find 3 approximate furthest neighbors on a query set
#' # "new_query_set" using that model and store the furthest neighbor indices
#' # into "neighbors" by calling
#' # 
#' # \dontrun{
#' # output <- approx_kfn(input_model=model, query=new_query_set, k=3)
#' # neighbors <- output$neighbors
#' # }
approx_kfn <- function(algorithm = "ds",
                       calculate_error = FALSE,
                       exact_distances = NA,
                       input_model = NA,
                       k = 0,
                       num_projections = 5,
                       num_tables = 5,
                       query = NA,
                       reference = NA,
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("approx_kfn")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamString(p, "algorithm", algorithm)

  SetParamBool(p, "calculate_error", calculate_error)

  if (!identical(exact_distances, NA)) {
    SetParamMat(p, "exact_distances", to_matrix(exact_distances), TRUE)
  }

  if (!identical(input_model, NA)) {
    SetParamApproxKFNModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "k", k)

  SetParamInt(p, "num_projections", num_projections)

  SetParamInt(p, "num_tables", num_tables)

  if (!identical(query, NA)) {
    SetParamMat(p, "query", to_matrix(query), TRUE)
  }

  if (!identical(reference, NA)) {
    SetParamMat(p, "reference", to_matrix(reference), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "distances")
  SetPassed(p, "neighbors")
  SetPassed(p, "output_model")

  # Call the program.
  approx_kfn_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamApproxKFNModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "ApproxKFNModel"

  # Extract the results in order.
  out <- list(
      "distances" = GetParamMat(p, "distances"),
      "neighbors" = GetParamUMat(p, "neighbors"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_approx", "mlpack_model_binding", "list")

  return(out)
}
