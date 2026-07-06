#' @title Hidden Markov Model (HMM) Sequence Generator
#'
#' @description
#' A utility to generate random sequences from a pre-trained Hidden Markov Model
#' (HMM).  The length of the desired sequence can be specified, and a random
#' sequence of observations is returned.
#'
#' @param length Length of sequence to generate (integer).
#' @param model Trained HMM to generate sequences with (HMMModel).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param start_state Starting state of sequence.  Default value "0"
#'   (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to save observation sequence to (numeric matrix).}
#' \item{state}{Matrix to save hidden state sequence to (integer matrix).}
#'
#' @details
#' This utility takes an already-trained HMM, specified as the "model"
#' parameter, and generates a random observation sequence and hidden state
#' sequence based on its parameters. The observation sequence may be saved with
#' the "output" output parameter, and the internal state  sequence may be saved
#' with the "state" output parameter.
#' 
#' The state to start the sequence in may be specified with the "start_state"
#' parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to generate a sequence of length 150 from the HMM "hmm" and
#' # save the observation sequence to "observations" and the hidden state
#' # sequence to "states", the following command may be used: 
#' # 
#' # \dontrun{
#' # output <- hmm_generate(model=hmm, length=150)
#' # observations <- output$output
#' # states <- output$state
#' # }
hmm_generate <- function(length,
                         model,
                         seed = 0,
                         start_state = 0,
                         verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hmm_generate")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "length", length)

  SetParamHMMModelPtr(p, "model", model)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "start_state", start_state)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")
  SetPassed(p, "state")

  # Call the program.
  hmm_generate_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output"),
      "state" = GetParamUMat(p, "state")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hmm", "mlpack_model_binding", "list")

  return(out)
}
