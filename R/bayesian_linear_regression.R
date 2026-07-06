#' @title BayesianLinearRegression
#'
#' @description
#' An implementation of the Bayesian linear regression.
#'
#' @param center Center the data and fit the intercept if enabled.  Default
#'   value "FALSE" (logical).
#' @param input Matrix of covariates (X) (numeric matrix).
#' @param input_model Trained BayesianLinearRegression model to use
#'   (BayesianLinearRegression).
#' @param responses Matrix of responses/observations (y) (numeric row).
#' @param scale Scale each feature by their standard deviations if enabled.
#'    Default value "FALSE" (logical).
#' @param test Matrix containing points to regress on (test points)
#'   (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output_model}{Output BayesianLinearRegression model
#'   (BayesianLinearRegression).}
#' \item{predictions}{If --test_file is specified, this file is where the
#'   predicted responses will be saved (numeric matrix).}
#' \item{stds}{If specified, this is where the standard deviations of the
#'   predictive distribution will be saved (numeric matrix).}
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
#' This program is able to train a Bayesian linear regression model or load a
#' model from file, output regression predictions for a test set, and save the
#' trained model to a file.
#' 
#' To train a BayesianLinearRegression model, the "input" and
#' "responses"parameters must be given. The "center"and "scale" parameters
#' control the centering and the normalizing options. A trained model can be
#' saved with the "output_model". If no training is desired at all, a model can
#' be passed via the "input_model" parameter.
#' 
#' The program can also provide predictions for test data using either the
#' trained model or the given input model.  Test points can be specified with
#' the "test" parameter.  Predicted responses to the test points can be saved
#' with the "predictions" output parameter. The corresponding standard deviation
#' can be save by precising the "stds" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command trains a model on the data "data" and
#' # responses "responses"with center set to true and scale set to false (so,
#' # Bayesian linear regression is being solved, and then the model is saved to
#' # "blr_model":
#' # 
#' # \dontrun{
#' # output <- bayesian_linear_regression(input=data, responses=responses,
#' #   center=1, scale=0)
#' # blr_model <- output$output_model
#' # }
#' # 
#' # The following command uses the "blr_model" to provide predicted  responses
#' # for the data "test" and save those  responses to "test_predictions": 
#' # 
#' # \dontrun{
#' # output <- bayesian_linear_regression(input_model=blr_model, test=test)
#' # test_predictions <- output$predictions
#' # }
#' # 
#' # Because the estimator computes a predictive distribution instead of a
#' # simple point estimate, the "stds" parameter allows one to save the
#' # prediction uncertainties: 
#' # 
#' # \dontrun{
#' # output <- bayesian_linear_regression(input_model=blr_model, test=test)
#' # test_predictions <- output$predictions
#' # stds <- output$stds
#' # }
bayesian_linear_regression <- function(center = FALSE,
                                       input = NA,
                                       input_model = NA,
                                       responses = NA,
                                       scale = FALSE,
                                       test = NA,
                                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("bayesian_linear_regression")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamBool(p, "center", center)

  if (!identical(input, NA)) {
    SetParamMat(p, "input", to_matrix(input), TRUE)
  }

  if (!identical(input_model, NA)) {
    SetParamBayesianLinearRegressionPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  if (!identical(responses, NA)) {
    SetParamRow(p, "responses", to_matrix(responses))
  }

  SetParamBool(p, "scale", scale)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output_model")
  SetPassed(p, "predictions")
  SetPassed(p, "stds")

  # Call the program.
  bayesian_linear_regression_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamBayesianLinearRegressionPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "BayesianLinearRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = GetParamMat(p, "predictions"),
      "stds" = GetParamMat(p, "stds")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_bayesian_linear", "mlpack_model_binding", "list")

  return(out)
}
