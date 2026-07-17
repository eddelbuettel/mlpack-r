#' @title BayesianLinearRegression Training
#'
#' @description
#' An implementation of the Bayesian linear regression training.
#'
#' @param input Matrix of covariates (X) (numeric matrix).
#' @param responses Matrix of responses/observations (y) (numeric row).
#' @param center Center the data and fit the intercept if enabled.  Default
#'   value "FALSE" (logical).
#' @param scale Scale each feature by their standard deviations if enabled.
#'    Default value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output BayesianLinearRegression model
#'   (BayesianLinearRegression).}
#'
#' @details
#' An implementation of the Bayesian linear regression.
#' This model is a probabilistic view and implementation of the linear
#' regression. The final solution is obtained by computing a posterior
#' distribution from gaussian likelihood and a zero mean gaussian isotropic 
#' prior distribution on the solution. 
#' Optimization is AUTOMATIC and does not require cross validation. The
#' optimization is performed by maximization of the evidence function.
#' Parameters are tuned during the maximization of the marginal likelihood. This
#' procedure includes the Ockham's razor that penalizes over complex solutions. 
#' 
#' To train a BayesianLinearRegression model, the "input" and "responses"
#' parameters must be given. The "center" and "scale" parameters control the
#' centering and the normalizing options. A trained model is returned.
#' 
#' 
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # 
#' # 
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
#' # model <- bayesian_linear_regression_train(input=X_train, responses=y_train,
#' #   center=1, scale=0)
#' #   
bayesian_linear_regression_train <- function(input,
                                             responses,
                                             center = FALSE,
                                             scale = FALSE,
                                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("bayesian_linear_regression_train")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamRow(p, "responses", to_matrix(responses))

  SetParamBool(p, "center", center)

  SetParamBool(p, "scale", scale)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")

  # Call the program.
  bayesian_linear_regression_train_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamBayesianLinearRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "BayesianLinearRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_bayesian_linear_regression", "mlpack_model_binding", "list")

  return(out)
}
