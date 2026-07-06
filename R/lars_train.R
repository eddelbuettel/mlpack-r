#' @title LARS Training
#'
#' @description
#' An implementation of Least Angle Regression (stagewise/lasso), also known as
#' LARS.  This can train a LARS/LASSO/Elastic Net model, and save the
#' pre-trained model for later use to output regression predictions from a test
#' set.
#'
#' @param input Matrix of covariates (X) (numeric matrix).
#' @param responses Row vector of responses/observations (y) (numeric
#'   row).
#' @param lambda1 Regularization parameter for l1-norm penalty.  Default
#'   value "0" (numeric).
#' @param lambda2 Regularization parameter for l2-norm penalty.  Default
#'   value "0" (numeric).
#' @param no_intercept Do not fit an intercept in the model.  Default value
#'   "FALSE" (logical).
#' @param no_normalize Do not normalize data to unit variance before
#'   modeling.  Default value "FALSE" (logical).
#' @param use_cholesky Use Cholesky decomposition during computation rather
#'   than explicitly computing the full Gram matrix.  Default value "FALSE"
#'   (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output LARS model (LARS).}
#'
#' @details
#' An implementation of LARS: Least Angle Regression (stagewise/lasso).  This is
#' a stage-wise homotopy-based algorithm for L1-regularized linear regression
#' (LASSO) and L1+L2-regularized linear regression (Elastic Net).
#' 
#' This program is able to train a LARS/LASSO/Elastic Net model or load a model
#' from a file, output regression predictions for a test set, and save the
#' trained model to a file.  The LARS algorithm is described in more detail
#' below:
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
#' 
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples

#' # 
#' # #' # \dontrun{
#' # suppressMessages(library(mlpack)) # in case 'mlpack' is not yet loaded
#' # X <- as.matrix(read.csv("http://datasets.mlpack.org/admission_predict.csv",
#' # header=FALSE))
#' # y <-
#' # as.matrix(read.csv("http://datasets.mlpack.org/admission_predict.responses.
#' # csv", header=FALSE))
#' # pp <- preprocess_split(input=X, input_label=as.matrix(1:nrow(X)),
#' # test_ratio=0.2)
#' # X_train <- pp[["training"]]
#' # X_test <- pp[["test"]]
#' # # labels are indices to operate on both factors or numeric data
#' # y_train <- y[as.integer(pp[["training_labels"]]), 1]
#' # y_test <- y[as.integer(pp[["test_labels"]]), 1]
#' # 
#' # model <- lars_train(input=X_train, responses=y_train, lambda1=1e-05,
#' #   lambda2=1e-06)
#' #   }
lars_train <- function(input,
                       responses,
                       lambda1 = 0,
                       lambda2 = 0,
                       no_intercept = FALSE,
                       no_normalize = FALSE,
                       use_cholesky = FALSE,
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("lars_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), FALSE)

  SetParamRow(p, "responses", to_matrix(responses))

  SetParamDouble(p, "lambda1", lambda1)

  SetParamDouble(p, "lambda2", lambda2)

  SetParamBool(p, "no_intercept", no_intercept)

  SetParamBool(p, "no_normalize", no_normalize)

  SetParamBool(p, "use_cholesky", use_cholesky)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  lars_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLARSPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LARS"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_lars", "mlpack_model_binding", "list")

  return(out)
}
