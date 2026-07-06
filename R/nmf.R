#' @title Non-negative Matrix Factorization
#'
#' @description
#' An implementation of non-negative matrix factorization.  This can be used to
#' decompose an input dataset into two low-rank non-negative components.
#'
#' @param input Input dataset to perform NMF on (numeric matrix).
#' @param rank Rank of the factorization (integer).
#' @param initial_h Initial H matrix (numeric matrix).
#' @param initial_w Initial W matrix (numeric matrix).
#' @param max_iterations Number of iterations before NMF terminates (0 runs
#'   until convergence.  Default value "10000" (integer).
#' @param min_residue The minimum root mean square residue allowed for each
#'   iteration, below which the program terminates.  Default value "1e-05"
#'   (numeric).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param update_rules Update rules for each iteration; ( multdist |
#'   multdiv | als ).  Default value "multdist" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{h}{Matrix to save the calculated H to (numeric matrix).}
#' \item{w}{Matrix to save the calculated W to (numeric matrix).}
#'
#' @details
#' This program performs non-negative matrix factorization on the given dataset,
#' storing the resulting decomposed matrices in the specified files.  For an
#' input dataset V, NMF decomposes V into two matrices W and H such that 
#' 
#' V = W * H
#' 
#' where all elements in W and H are non-negative.  If V is of size (n x m),
#' then W will be of size (n x r) and H will be of size (r x m), where r is the
#' rank of the factorization (specified by the "rank" parameter).
#' 
#' Optionally, the desired update rules for each NMF iteration can be chosen
#' from the following list:
#' 
#'  - multdist: multiplicative distance-based update rules (Lee and Seung 1999)
#'  - multdiv: multiplicative divergence-based update rules (Lee and Seung 1999)
#'  - als: alternating least squares update rules (Paatero and Tapper 1994)
#' 
#' The maximum number of iterations is specified with "max_iterations", and the
#' minimum residue required for algorithm termination is specified with the
#' "min_residue" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run NMF on the input matrix "V" using the 'multdist' update
#' # rules with a rank-10 decomposition and storing the decomposed matrices into
#' # "W" and "H", the following command could be used: 
#' # 
#' # \dontrun{
#' # output <- nmf(input=V, rank=10, update_rules="multdist")
#' # W <- output$w
#' # H <- output$h
#' # }
nmf <- function(input,
                rank,
                initial_h = NA,
                initial_w = NA,
                max_iterations = 10000,
                min_residue = 1e-05,
                seed = 0,
                update_rules = "multdist",
                verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("nmf")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamInt(p, "rank", rank)

  if (!identical(initial_h, NA)) {
    SetParamMat(p, "initial_h", to_matrix(initial_h), TRUE)
  }

  if (!identical(initial_w, NA)) {
    SetParamMat(p, "initial_w", to_matrix(initial_w), TRUE)
  }

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamDouble(p, "min_residue", min_residue)

  SetParamInt(p, "seed", seed)

  SetParamString(p, "update_rules", update_rules)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "h")
  SetPassed(p, "w")

  # Call the program.
  nmf_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "h" = GetParamMat(p, "h"),
      "w" = GetParamMat(p, "w")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_nmf", "mlpack_model_binding", "list")

  return(out)
}
