#' @title RADICAL
#'
#' @description
#' An implementation of RADICAL, a method for independent component analysis
#' (ICA).  Given a dataset, this can decompose the dataset into an unmixing
#' matrix and an independent component matrix; this can be useful for
#' preprocessing.
#'
#' @param input Input dataset for ICA (numeric matrix).
#' @param angles Number of angles to consider in brute-force search during
#'   Radical2D.  Default value "150" (integer).
#' @param noise_std_dev Standard deviation of Gaussian noise.  Default
#'   value "0.175" (numeric).
#' @param objective If set, an estimate of the final objective function is
#'   printed.  Default value "FALSE" (logical).
#' @param replicates Number of Gaussian-perturbed replicates to use (per
#'   point) in Radical2D.  Default value "30" (integer).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param sweeps Number of sweeps; each sweep calls Radical2D once for each
#'   pair of dimensions.  Default value "0" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_ic}{Matrix to save independent components to (numeric
#'   matrix).}
#' \item{output_unmixing}{Matrix to save unmixing matrix to (numeric
#'   matrix).}
#'
#' @details
#' An implementation of RADICAL, a method for independent component analysis
#' (ICA).  Assuming that we have an input matrix X, the goal is to find a square
#' unmixing matrix W such that Y = W * X and the dimensions of Y are independent
#' components.  If the algorithm is running particularly slowly, try reducing
#' the number of replicates.
#' 
#' The input matrix to perform ICA on should be specified with the "input"
#' parameter.  The output matrix Y may be saved with the "output_ic" output
#' parameter, and the output unmixing matrix W may be saved with the
#' "output_unmixing" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to perform ICA on the matrix "X" with 40 replicates, saving
#' # the independent components to "ic", the following command may be used: 
#' # 
#' # \dontrun{
#' # output <- radical(input=X, replicates=40)
#' # ic <- output$output_ic
#' # }
radical <- function(input,
                    angles = 150,
                    noise_std_dev = 0.175,
                    objective = FALSE,
                    replicates = 30,
                    seed = 0,
                    sweeps = 0,
                    verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("radical")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamInt(p, "angles", angles)

  SetParamDouble(p, "noise_std_dev", noise_std_dev)

  SetParamBool(p, "objective", objective)

  SetParamInt(p, "replicates", replicates)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "sweeps", sweeps)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_ic")
  SetPassed(p, "output_unmixing")

  # Call the program.
  radical_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output_ic" = GetParamMat(p, "output_ic"),
      "output_unmixing" = GetParamMat(p, "output_unmixing")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_radical", "mlpack_model_binding", "list")

  return(out)
}
