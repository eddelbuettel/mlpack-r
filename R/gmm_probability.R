#' @title GMM Probability Calculator
#'
#' @description
#' A probability calculator for GMMs.  Given a pre-trained GMM and a set of
#' points, this can compute the probability that each point is from the given
#' GMM.
#'
#' @param input Input matrix to calculate probabilities of (numeric
#'   matrix).
#' @param input_model Input GMM to use as model (GMM).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to store calculated probabilities in (numeric
#'   matrix).}
#'
#' @details
#' This program calculates the probability that given points came from a given
#' GMM (that is, P(X | gmm)).  The GMM is specified with the "input_model"
#' parameter, and the points are specified with the "input" parameter.  The
#' output probabilities may be saved via the "output" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, for example, to calculate the probabilities of each point in "points"
#' # coming from the pre-trained GMM "gmm", while storing those probabilities in
#' # "probs", the following command could be used:
#' # 
#' # \dontrun{
#' # probs <- gmm_probability(input_model=gmm, input=points)
#' # }
gmm_probability <- function(input,
                            input_model,
                            verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("gmm_probability")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamGMMPtr(p, "input_model", input_model)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  gmm_probability_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_gmm", "mlpack_model_binding", "list")

  return(out)
}
