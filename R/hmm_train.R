#' @title Hidden Markov Model (HMM) Training
#'
#' @description
#' An implementation of training algorithms for Hidden Markov Models (HMMs).
#' Given labeled or unlabeled data, an HMM can be trained for further use with
#' other mlpack HMM tools.
#'
#' @param input_file File containing input observations (character).
#' @param batch If true, input_file (and if passed, labels_file) are
#'   expected to contain a list of files to use as input observation sequences
#'   (and label sequences).  Default value "FALSE" (logical).
#' @param gaussians Number of gaussians in each GMM (necessary when type is
#'   'gmm').  Default value "0" (integer).
#' @param input_model Pre-existing HMM model to initialize training with
#'   (HMMModel).
#' @param labels_file Optional file of hidden states, used for labeled
#'   training.  Default value "" (character).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param states Number of hidden states in HMM (necessary, unless
#'   model_file is specified).  Default value "0" (integer).
#' @param tolerance Tolerance of the Baum-Welch algorithm.  Default value
#'   "1e-05" (numeric).
#' @param type Type of HMM: discrete | gaussian | diag_gmm | gmm.  Default
#'   value "gaussian" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained HMM (HMMModel).}
#'
#' @details
#' This program allows a Hidden Markov Model to be trained on labeled or
#' unlabeled data.  It supports four types of HMMs: Discrete HMMs, Gaussian
#' HMMs, GMM HMMs, or Diagonal GMM HMMs
#' 
#' Either one input sequence can be specified (with "input_file"), or, a file
#' containing files in which input sequences can be found (when
#' "input_file"and"batch" are used together).  In addition, labels can be
#' provided in the file specified by "labels_file", and if "batch" is used, the
#' file given to "labels_file" should contain a list of files of labels
#' corresponding to the sequences in the file given to "input_file".
#' 
#' The HMM is trained with the Baum-Welch algorithm if no labels are provided. 
#' The tolerance of the Baum-Welch algorithm can be set with the
#' "tolerance"option.  By default, the transition matrix is randomly initialized
#' and the emission distributions are initialized to fit the extent of the data.
#' 
#' Optionally, a pre-created HMM model can be used as a guess for the transition
#' matrix and emission probabilities; this is specifiable with "output_model".
#'
#' @author
#' mlpack developers
#'
#' @export
hmm_train <- function(input_file,
                      batch = FALSE,
                      gaussians = 0,
                      input_model = NA,
                      labels_file = "",
                      seed = 0,
                      states = 0,
                      tolerance = 1e-05,
                      type = "gaussian",
                      verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("hmm_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamString(p, "input_file", input_file)

  SetParamBool(p, "batch", batch)

  SetParamInt(p, "gaussians", gaussians)

  if (!identical(input_model, NA)) {
    SetParamHMMModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamString(p, "labels_file", labels_file)

  SetParamInt(p, "seed", seed)

  SetParamInt(p, "states", states)

  SetParamDouble(p, "tolerance", tolerance)

  SetParamString(p, "type", type)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  hmm_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamHMMModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "HMMModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_hmm", "mlpack_model_binding", "list")

  return(out)
}
