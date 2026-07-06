#' @title FastMKS (Fast Max-Kernel Search)
#'
#' @description
#' An implementation of the single-tree and dual-tree fast max-kernel search
#' (FastMKS) algorithm.  Given a set of reference points and a set of query
#' points, this can find the reference point with maximum kernel value for each
#' query point; trained models can be reused for future queries.
#'
#' @param bandwidth Bandwidth (for Gaussian, Epanechnikov, and triangular
#'   kernels).  Default value "1" (numeric).
#' @param base Base to use during cover tree construction.  Default value
#'   "2" (numeric).
#' @param degree Degree of polynomial kernel.  Default value "2"
#'   (numeric).
#' @param input_model Input FastMKS model to use (FastMKSModel).
#' @param k Number of maximum kernels to find.  Default value "0"
#'   (integer).
#' @param kernel Kernel type to use: 'linear', 'polynomial', 'cosine',
#'   'gaussian', 'epanechnikov', 'triangular', 'hyptan'.  Default value "linear"
#'   (character).
#' @param naive If true, O(n^2) naive mode is used for computation. 
#'   Default value "FALSE" (logical).
#' @param offset Offset of kernel (for polynomial and hyptan kernels). 
#'   Default value "0" (numeric).
#' @param query The query dataset (numeric matrix).
#' @param reference The reference dataset (numeric matrix).
#' @param scale Scale of kernel (for hyptan kernel).  Default value "1"
#'   (numeric).
#' @param single If true, single-tree search is used (as opposed to
#'   dual-tree search.  Default value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{indices}{Output matrix of indices (integer matrix).}
#' \item{kernels}{Output matrix of kernels (numeric matrix).}
#' \item{output_model}{Output for FastMKS model (FastMKSModel).}
#'
#' @details
#' This program will find the k maximum kernels of a set of points, using a
#' query set and a reference set (which can optionally be the same set). More
#' specifically, for each point in the query set, the k points in the reference
#' set with maximum kernel evaluations are found.  The kernel function used is
#' specified with the "kernel" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command will calculate, for each point in the
#' # query set "query", the five points in the reference set "reference" with
#' # maximum kernel evaluation using the linear kernel.  The kernel evaluations
#' # may be saved with the  "kernels" output parameter and the indices may be
#' # saved with the "indices" output parameter.
#' # 
#' # \dontrun{
#' # output <- fastmks(k=5, reference=reference, query=query, kernel="linear")
#' # indices <- output$indices
#' # kernels <- output$kernels
#' # }
#' # #' # 
#' # The output matrices are organized such that row i and column j in the
#' # indices matrix corresponds to the index of the point in the reference set
#' # that has j'th largest kernel evaluation with the point in the query set
#' # with index i.  Row i and column j in the kernels matrix corresponds to the
#' # kernel evaluation between those two points.
#' # 
#' # This program performs FastMKS using a cover tree.  The base used to build
#' # the cover tree can be specified with the "base" parameter.
fastmks <- function(bandwidth = 1,
                    base = 2,
                    degree = 2,
                    input_model = NA,
                    k = 0,
                    kernel = "linear",
                    naive = FALSE,
                    offset = 0,
                    query = NA,
                    reference = NA,
                    scale = 1,
                    single = FALSE,
                    verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("fastmks")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamDouble(p, "bandwidth", bandwidth)

  SetParamDouble(p, "base", base)

  SetParamDouble(p, "degree", degree)

  if (!identical(input_model, NA)) {
    SetParamFastMKSModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "k", k)

  SetParamString(p, "kernel", kernel)

  SetParamBool(p, "naive", naive)

  SetParamDouble(p, "offset", offset)

  if (!identical(query, NA)) {
    SetParamMat(p, "query", to_matrix(query), TRUE)
  }

  if (!identical(reference, NA)) {
    SetParamMat(p, "reference", to_matrix(reference), TRUE)
  }

  SetParamDouble(p, "scale", scale)

  SetParamBool(p, "single", single)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "indices")
  SetPassed(p, "kernels")
  SetPassed(p, "output_model")

  # Call the program.
  fastmks_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamFastMKSModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "FastMKSModel"

  # Extract the results in order.
  out <- list(
      "indices" = GetParamUMat(p, "indices"),
      "kernels" = GetParamMat(p, "kernels"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_fastmks", "mlpack_model_binding", "list")

  return(out)
}
