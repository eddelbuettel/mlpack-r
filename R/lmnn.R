#' @title Large Margin Nearest Neighbors (LMNN)
#'
#' @description
#' An implementation of Large Margin Nearest Neighbors (LMNN), a distance
#' learning technique.  Given a labeled dataset, this learns a transformation of
#' the data that improves k-nearest-neighbor performance; this can be useful as
#' a preprocessing step.
#'
#' @param input Input dataset to run LMNN on (numeric matrix).
#' @param batch_size Batch size for mini-batch SGD.  Default value "50"
#'   (integer).
#' @param center Perform mean-centering on the dataset. It is useful when
#'   the centroid of the data is far from the origin.  Default value "FALSE"
#'   (logical).
#' @param distance Initial distance matrix to be used as starting poin
#'   (numeric matrix).
#' @param k Number of target neighbors to use for each datapoint.  Default
#'   value "1" (integer).
#' @param labels Labels for input dataset (integer row).
#' @param linear_scan Don't shuffle the order in which data points are
#'   visited for SGD or mini-batch SGD.  Default value "FALSE" (logical).
#' @param max_iterations Maximum number of iterations for L-BFGS (0
#'   indicates no limit).  Default value "100000" (integer).
#' @param normalize Use a normalized starting point for optimization. Itis
#'   useful for when points are far apart, or when SGD is returning NaN. 
#'   Default value "FALSE" (logical).
#' @param optimizer Optimizer to use; 'amsgrad', 'bbsgd', 'sgd', or
#'   'lbfgs'.  Default value "amsgrad" (character).
#' @param passes Maximum number of full passes over dataset for AMSGrad,
#'   BB_SGD and SGD.  Default value "50" (integer).
#' @param print_accuracy Print accuracies on initial and transformed
#'   datase.  Default value "FALSE" (logical).
#' @param rank Rank of distance matrix to be optimized..  Default value "0"
#'   (integer).
#' @param regularization Regularization for LMNN objective function. 
#'   Default value "0.5" (numeric).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param step_size Step size for AMSGrad, BB_SGD and SGD (alpha).  Default
#'   value "0.01" (numeric).
#' @param tolerance Maximum tolerance for termination of AMSGrad, BB_SGD,
#'   SGD or L-BFGS.  Default value "1e-07" (numeric).
#' @param update_interval Number of iterations after which impostors need
#'   to be recalculated.  Default value "1" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{centered_data}{Output matrix for mean-centered dataset (numeric
#'   matrix).}
#' \item{output}{Output matrix for learned distance matrix (numeric
#'   matrix).}
#' \item{transformed_data}{Output matrix for transformed dataset (numeric
#'   matrix).}
#'
#' @details
#' This program implements Large Margin Nearest Neighbors, a distance learning
#' technique.  The method seeks to improve k-nearest-neighbor classification on
#' a dataset.  The method employes the strategy of reducing distance between
#' similar labeled data points (a.k.a target neighbors) and increasing distance
#' between differently labeled points (a.k.a impostors) using standard
#' optimization techniques over the gradient of the distance between data
#' points.
#' 
#' To work, this algorithm needs labeled data.  It can be given as the last row
#' of the input dataset (specified with "input"), or alternatively as a separate
#' matrix (specified with "labels").  Additionally, a starting point for
#' optimization (specified with "distance"can be given, having (r x d)
#' dimensionality.  Here r should satisfy 1 <= r <= d, Consequently a Low-Rank
#' matrix will be optimized. Alternatively, Low-Rank distance can be learned by
#' specifying the "rank"parameter (A Low-Rank matrix with uniformly distributed
#' values will be used as initial learning point). 
#' 
#' The program also requires number of targets neighbors to work with (
#' specified with "k"), A regularization parameter can also be passed, It acts
#' as a trade of between the pulling and pushing terms (specified with
#' "regularization"), In addition, this implementation of LMNN includes a
#' parameter to decide the interval after which impostors must be re-calculated
#' (specified with "update_interval").
#' 
#' Output can either be the learned distance matrix (specified with "output"),
#' or the transformed dataset  (specified with "transformed_data"), or both.
#' Additionally mean-centered dataset (specified with "centered_data") can be
#' accessed given mean-centering (specified with "center") is performed on the
#' dataset. Accuracy on initial dataset and final transformed dataset can be
#' printed by specifying the "print_accuracy"parameter. 
#' 
#' This implementation of LMNN uses AdaGrad, BigBatch_SGD, stochastic gradient
#' descent, mini-batch stochastic gradient descent, or the L_BFGS optimizer. 
#' 
#' AdaGrad, specified by the value 'adagrad' for the parameter "optimizer", uses
#' maximum of past squared gradients. It primarily on six parameters: the step
#' size (specified with "step_size"), the batch size (specified with
#' "batch_size"), the maximum number of passes (specified with "passes").
#' Inaddition, a normalized starting point can be used by specifying the
#' "normalize" parameter. 
#' 
#' BigBatch_SGD, specified by the value 'bbsgd' for the parameter "optimizer",
#' depends primarily on four parameters: the step size (specified with
#' "step_size"), the batch size (specified with "batch_size"), the maximum
#' number of passes (specified with "passes").  In addition, a normalized
#' starting point can be used by specifying the "normalize" parameter. 
#' 
#' Stochastic gradient descent, specified by the value 'sgd' for the parameter
#' "optimizer", depends primarily on three parameters: the step size (specified
#' with "step_size"), the batch size (specified with "batch_size"), and the
#' maximum number of passes (specified with "passes").  In addition, a
#' normalized starting point can be used by specifying the "normalize"
#' parameter. Furthermore, mean-centering can be performed on the dataset by
#' specifying the "center"parameter. 
#' 
#' The L-BFGS optimizer, specified by the value 'lbfgs' for the parameter
#' "optimizer", uses a back-tracking line search algorithm to minimize a
#' function.  The following parameters are used by L-BFGS: "max_iterations",
#' "tolerance"(the optimization is terminated when the gradient norm is below
#' this value).  For more details on the L-BFGS optimizer, consult either the
#' mlpack L-BFGS documentation (in lbfgs.hpp) or the vast set of published
#' literature on L-BFGS.  In addition, a normalized starting point can be used
#' by specifying the "normalize" parameter.
#' 
#' By default, the AMSGrad optimizer is used.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # Example - Let's say we want to learn distance on iris dataset with number
#' # of targets as 3 using BigBatch_SGD optimizer. A simple call for the same
#' # will look like: 
#' # 
#' # \dontrun{
#' # output <- lmnn(input=iris, labels=iris_labels, k=3, optimizer="bbsgd")
#' # output <- output$output
#' # }
#' # 
#' # Another program call making use of update interval & regularization
#' # parameter with dataset having labels as last column can be made as: 
#' # 
#' # \dontrun{
#' # output <- lmnn(input=letter_recognition, k=5, update_interval=10,
#' #   regularization=0.4)
#' # output <- output$output
#' # }
lmnn <- function(input,
                 batch_size = 50,
                 center = FALSE,
                 distance = NA,
                 k = 1,
                 labels = NA,
                 linear_scan = FALSE,
                 max_iterations = 100000,
                 normalize = FALSE,
                 optimizer = "amsgrad",
                 passes = 50,
                 print_accuracy = FALSE,
                 rank = 0,
                 regularization = 0.5,
                 seed = 0,
                 step_size = 0.01,
                 tolerance = 1e-07,
                 update_interval = 1,
                 verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("lmnn")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamInt(p, "batch_size", batch_size)

  SetParamBool(p, "center", center)

  if (!identical(distance, NA)) {
    SetParamMat(p, "distance", to_matrix(distance), TRUE)
  }

  SetParamInt(p, "k", k)

  if (!identical(labels, NA)) {
    SetParamURow(p, "labels", to_matrix(labels))
  }

  SetParamBool(p, "linear_scan", linear_scan)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "normalize", normalize)

  SetParamString(p, "optimizer", optimizer)

  SetParamInt(p, "passes", passes)

  SetParamBool(p, "print_accuracy", print_accuracy)

  SetParamInt(p, "rank", rank)

  SetParamDouble(p, "regularization", regularization)

  SetParamInt(p, "seed", seed)

  SetParamDouble(p, "step_size", step_size)

  SetParamDouble(p, "tolerance", tolerance)

  SetParamInt(p, "update_interval", update_interval)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "centered_data")
  SetPassed(p, "output")
  SetPassed(p, "transformed_data")

  # Call the program.
  lmnn_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "centered_data" = GetParamMat(p, "centered_data"),
      "output" = GetParamMat(p, "output"),
      "transformed_data" = GetParamMat(p, "transformed_data")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_lmnn", "mlpack_model_binding", "list")

  return(out)
}
