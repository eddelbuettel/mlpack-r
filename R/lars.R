#' @title LARS
#'
#' @description
#' An implementation of Least Angle Regression (Stagewise/laSso), also known as
#' LARS.  This can train a LARS/LASSO/Elastic Net model and use that model or a
#' pre-trained model to output regression predictions for a test set.
#'
#' @param input Matrix of covariates (X) (numeric matrix).
#' @param input_model Trained LARS model to use (LARS).
#' @param lambda1 Regularization parameter for l1-norm penalty.  Default
#'   value "0" (numeric).
#' @param lambda2 Regularization parameter for l2-norm penalty.  Default
#'   value "0" (numeric).
#' @param no_intercept Do not fit an intercept in the model.  Default value
#'   "FALSE" (logical).
#' @param no_normalize Do not normalize data to unit variance before
#'   modeling.  Default value "FALSE" (logical).
#' @param responses Matrix of responses/observations (y) (numeric matrix).
#' @param test Matrix containing points to regress on (test points)
#'   (numeric matrix).
#' @param use_cholesky Use Cholesky decomposition during computation rather
#'   than explicitly computing the full Gram matrix.  Default value "FALSE"
#'   (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output LARS model (LARS).}
#' \item{output_predictions}{If --test_file is specified, this file is
#'   where the predicted responses will be saved (numeric matrix).}
#'
#' @details
#' An implementation of LARS: Least Angle Regression (Stagewise/laSso).  This is
#' a stage-wise homotopy-based algorithm for L1-regularized linear regression
#' (LASSO) and L1+L2-regularized linear regression (Elastic Net).
#' 
#' This program is able to train a LARS/LASSO/Elastic Net model or load a model
#' from file, output regression predictions for a test set, and save the trained
#' model to a file.  The LARS algorithm is described in more detail below:
#' 
#' Let X be a matrix where each row is a point and each column is a dimension,
#' and let y be a vector of targets.
#' 
#' The Elastic Net problem is to solve
#' 
#'   min_beta 0.5 || X * beta - y ||_2^2 + lambda_1 ||beta||_1 +
#'       0.5 lambda_2 ||beta||_2^2
#' 
#' If lambda1 > 0 and lambda2 = 0, the problem is the LASSO.
#' If lambda1 > 0 and lambda2 > 0, the problem is the Elastic Net.
#' If lambda1 = 0 and lambda2 > 0, the problem is ridge regression.
#' If lambda1 = 0 and lambda2 = 0, the problem is unregularized linear
#' regression.
#' 
#' For efficiency reasons, it is not recommended to use this algorithm with
#' "lambda1" = 0.  In that case, use the 'linear_regression' program, which
#' implements both unregularized linear regression and ridge regression.
#' 
#' To train a LARS/LASSO/Elastic Net model, the "input" and "responses"
#' parameters must be given.  The "lambda1", "lambda2", and "use_cholesky"
#' parameters control the training options.  A trained model can be saved with
#' the "output_model".  If no training is desired at all, a model can be passed
#' via the "input_model" parameter.
#' 
#' The program can also provide predictions for test data using either the
#' trained model or the given input model.  Test points can be specified with
#' the "test" parameter.  Predicted responses to the test points can be saved
#' with the "output_predictions" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command trains a model on the data "data" and
#' # responses "responses" with lambda1 set to 0.4 and lambda2 set to 0 (so,
#' # LASSO is being solved), and then the model is saved to "lasso_model":
#' # 
#' # \dontrun{
#' # output <- lars(input=data, responses=responses, lambda1=0.4, lambda2=0)
#' # lasso_model <- output$output_model
#' # }
#' # 
#' # The following command uses the "lasso_model" to provide predicted responses
#' # for the data "test" and save those responses to "test_predictions": 
#' # 
#' # \dontrun{
#' # output <- lars(input_model=lasso_model, test=test)
#' # test_predictions <- output$output_predictions
#' # }
lars <- function(input = NA,
                 input_model = NA,
                 lambda1 = 0,
                 lambda2 = 0,
                 no_intercept = FALSE,
                 no_normalize = FALSE,
                 responses = NA,
                 test = NA,
                 use_cholesky = FALSE,
                 verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("lars")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  if (!identical(input, NA)) {
    SetParamMat(p, "input", to_matrix(input), FALSE)
  }

  if (!identical(input_model, NA)) {
    SetParamLARSPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamDouble(p, "lambda1", lambda1)

  SetParamDouble(p, "lambda2", lambda2)

  SetParamBool(p, "no_intercept", no_intercept)

  SetParamBool(p, "no_normalize", no_normalize)

  if (!identical(responses, NA)) {
    SetParamMat(p, "responses", to_matrix(responses), TRUE)
  }

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), FALSE)
  }

  SetParamBool(p, "use_cholesky", use_cholesky)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "output_predictions")

  # Call the program.
  lars_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLARSPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LARS"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "output_predictions" = GetParamMat(p, "output_predictions")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_lars", "mlpack_model_binding", "list")

  return(out)
}
