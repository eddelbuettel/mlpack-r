#' @title Hidden Markov Model (HMM) Sequence Log-Likelihood
#'
#' @description
#' A utility for computing the log-likelihood of a sequence for Hidden Markov
#' Models (HMMs).  Given a pre-trained HMM and an observation sequence, this
#' computes and returns the log-likelihood of that sequence being observed from
#' that HMM.
#'
#' @param input File containing observations (numeric matrix).
#' @param input_model File containing HMM (HMMModel).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{log_likelihood}{Log-likelihood of the sequence.  Default value "0"
#'   (numeric).}
#'
#' @details
#' This utility takes an already-trained HMM, specified with the "input_model"
#' parameter, and evaluates the log-likelihood of a sequence of observations,
#' given with the "input" parameter.  The computed log-likelihood is given as
#' output.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to compute the log-likelihood of the sequence "seq" with the
#' # pre-trained HMM "hmm", the following command may be used: 
#' # 
#' # \dontrun{
#' # hmm_loglik(input=seq, input_model=hmm)
#' # }
hmm_loglik <- function(input,
                       input_model,
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hmm_loglik")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamHMMModelPtr(p, "input_model", input_model)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "log_likelihood")

  # Call the program.
  hmm_loglik_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "log_likelihood" = GetParamDouble(p, "log_likelihood")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hmm", "mlpack_model_binding", "list")

  return(out)
}
