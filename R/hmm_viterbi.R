#' @title Hidden Markov Model (HMM) Viterbi State Prediction
#'
#' @description
#' A utility for computing the most probable hidden state sequence for Hidden
#' Markov Models (HMMs).  Given a pre-trained HMM and an observed sequence, this
#' uses the Viterbi algorithm to compute and return the most probable hidden
#' state sequence.
#'
#' @param input Matrix containing observations (numeric matrix).
#' @param input_model Trained HMM to use (HMMModel).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{File to save predicted state sequence to (integer
#'   matrix).}
#'
#' @details
#' This utility takes an already-trained HMM, specified as "input_model", and
#' evaluates the most probable hidden state sequence of a given sequence of
#' observations (specified as '"input", using the Viterbi algorithm.  The
#' computed state sequence may be saved using the "output" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to predict the state sequence of the observations "obs" using
#' # the HMM "hmm", storing the predicted state sequence to "states", the
#' # following command could be used:
#' # 
#' # \dontrun{
#' # states <- hmm_viterbi(input=obs, input_model=hmm)
#' # }
hmm_viterbi <- function(input,
                        input_model,
                        verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hmm_viterbi")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamHMMModelPtr(p, "input_model", input_model)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  hmm_viterbi_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamUMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hmm", "mlpack_model_binding", "list")

  return(out)
}
