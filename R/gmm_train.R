#' @title Gaussian Mixture Model (GMM) Training
#'
#' @description
#' An implementation of the EM algorithm for training Gaussian mixture models
#' (GMMs).  Given a dataset, this can train a GMM for future use with other
#' tools.
#'
#' @param gaussians Number of Gaussians in the GMM (integer).
#' @param input The training data on which the model will be fit (numeric
#'   matrix).
#' @param diagonal_covariance Force the covariance of the Gaussians to be
#'   diagonal.  This can accelerate training time significantly.  Default value
#'   "FALSE" (logical).
#' @param input_model Initial input GMM model to start training with
#'   (GMM).
#' @param kmeans_max_iterations Maximum number of iterations for the
#'   k-means algorithm (used to initialize EM).  Default value "1000"
#'   (integer).
#' @param max_iterations Maximum number of iterations of EM algorithm
#'   (passing 0 will run until convergence).  Default value "250" (integer).
#' @param no_force_positive Do not force the covariance matrices to be
#'   positive definite.  Default value "FALSE" (logical).
#' @param noise Variance of zero-mean Gaussian noise to add to data. 
#'   Default value "0" (numeric).
#' @param percentage If using --refined_start, specify the percentage of
#'   the dataset used for each sampling (should be between 0.0 and 1.0). 
#'   Default value "0.02" (numeric).
#' @param refined_start During the initialization, use refined initial
#'   positions for k-means clustering (Bradley and Fayyad, 1998).  Default value
#'   "FALSE" (logical).
#' @param samplings If using --refined_start, specify the number of
#'   samplings used for initial points.  Default value "100" (integer).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param tolerance Tolerance for convergence of EM.  Default value "1e-10"
#'   (numeric).
#' @param trials Number of trials to perform in training GMM.  Default
#'   value "1" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output for trained GMM model (GMM).}
#'
#' @details
#' This program takes a parametric estimate of a Gaussian mixture model (GMM)
#' using the EM algorithm to find the maximum likelihood estimate.  The model
#' may be saved and reused by other mlpack GMM tools.
#' 
#' The input data to train on must be specified with the "input" parameter, and
#' the number of Gaussians in the model must be specified with the "gaussians"
#' parameter.  Optionally, many trials with different random initializations may
#' be run, and the result with highest log-likelihood on the training data will
#' be taken.  The number of trials to run is specified with the "trials"
#' parameter.  By default, only one trial is run.
#' 
#' The tolerance for convergence and maximum number of iterations of the EM
#' algorithm are specified with the "tolerance" and "max_iterations" parameters,
#' respectively.  The GMM may be initialized for training with another model,
#' specified with the "input_model" parameter. Otherwise, the model is
#' initialized by running k-means on the data.  The k-means clustering
#' initialization can be controlled with the "kmeans_max_iterations",
#' "refined_start", "samplings", and "percentage" parameters.  If
#' "refined_start" is specified, then the Bradley-Fayyad refined start
#' initialization will be used.  This can often lead to better clustering
#' results.
#' 
#' The 'diagonal_covariance' flag will cause the learned covariances to be
#' diagonal matrices.  This significantly simplifies the model itself and causes
#' training to be faster, but restricts the ability to fit more complex GMMs.
#' 
#' If GMM training fails with an error indicating that a covariance matrix could
#' not be inverted, make sure that the "no_force_positive" parameter is not
#' specified.  Alternately, adding a small amount of Gaussian noise (using the
#' "noise" parameter) to the entire dataset may help prevent Gaussians with zero
#' variance in a particular dimension, which is usually the cause of
#' non-invertible covariance matrices.
#' 
#' The "no_force_positive" parameter, if set, will avoid the checks after each
#' iteration of the EM algorithm which ensure that the covariance matrices are
#' positive definite.  Specifying the flag can cause faster runtime, but may
#' also cause non-positive definite covariance matrices, which will cause the
#' program to crash.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # As an example, to train a 6-Gaussian GMM on the data in "data" with a
#' # maximum of 100 iterations of EM and 3 trials, saving the trained GMM to
#' # "gmm", the following command can be used:
#' # 
#' # \dontrun{
#' # gmm <- gmm_train(input=data, gaussians=6, trials=3)
#' # }
#' # 
#' # To re-train that GMM on another set of data "data2", the following command
#' # may be used: 
#' # 
#' # \dontrun{
#' # new_gmm <- gmm_train(input_model=gmm, input=data2, gaussians=6)
#' # }
gmm_train <- function(gaussians,
                      input,
                      diagonal_covariance = FALSE,
                      input_model = NA,
                      kmeans_max_iterations = 1000,
                      max_iterations = 250,
                      no_force_positive = FALSE,
                      noise = 0,
                      percentage = 0.02,
                      refined_start = FALSE,
                      samplings = 100,
                      seed = 0,
                      tolerance = 1e-10,
                      trials = 1,
                      verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("gmm_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "gaussians", gaussians)

  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamBool(p, "diagonal_covariance", diagonal_covariance)

  if (!identical(input_model, NA)) {
    SetParamGMMPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamInt(p, "kmeans_max_iterations", kmeans_max_iterations)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "no_force_positive", no_force_positive)

  SetParamDouble(p, "noise", noise)

  SetParamDouble(p, "percentage", percentage)

  SetParamBool(p, "refined_start", refined_start)

  SetParamInt(p, "samplings", samplings)

  SetParamInt(p, "seed", seed)

  SetParamDouble(p, "tolerance", tolerance)

  SetParamInt(p, "trials", trials)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  gmm_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamGMMPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "GMM"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_gmm", "mlpack_model_binding", "list")

  return(out)
}
