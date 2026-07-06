#' @title Scale Data
#'
#' @description
#' A utility to perform feature scaling on datasets using one of six techniques.
#'  Both scaling and inverse scaling are supported, and scalers can be saved and
#' then applied to other datasets.
#'
#' @param input Matrix containing data (numeric matrix).
#' @param epsilon regularization Parameter for pcawhitening, or
#'   zcawhitening, should be between -1 to 1.  Default value "1e-06" (numeric).
#' @param input_model Input Scaling model (ScalingModel).
#' @param inverse_scaling Inverse Scaling to get original datase.  Default
#'   value "FALSE" (logical).
#' @param max_value Ending value of range for min_max_scaler.  Default
#'   value "1" (integer).
#' @param min_value Starting value of range for min_max_scaler.  Default
#'   value "0" (integer).
#' @param scaler_method method to use for scaling, the default is
#'   standard_scaler.  Default value "standard_scaler" (character).
#' @param seed Random seed (0 for std::time(NULL)).  Default value "0"
#'   (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to save scaled data to (numeric matrix).}
#' \item{output_model}{Output scaling model (ScalingModel).}
#'
#' @details
#' This utility takes a dataset and performs feature scaling using one of the
#' six scaler methods namely: 'max_abs_scaler', 'mean_normalization',
#' 'min_max_scaler' ,'standard_scaler', 'pca_whitening' and 'zca_whitening'. The
#' function takes a matrix as "input" and a scaling method type which you can
#' specify using "scaler_method" parameter; the default is standard scaler, and
#' outputs a matrix with scaled feature.
#' 
#' The output scaled feature matrix may be saved with the "output" output
#' parameters.
#' 
#' The model to scale features can be saved using "output_model" and later can
#' be loaded back using"input_model".
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to scale the dataset "X" into "X_scaled"
#' # with  standard_scaler as scaler_method, we could run 
#' # 
#' # \dontrun{
#' # output <- preprocess_scale(input=X, scaler_method="standard_scaler")
#' # X_scaled <- output$output
#' # }
#' # 
#' # A simple example where we want to whiten the dataset "X" into "X_whitened"
#' # with  PCA as whitening_method and use 0.01 as regularization parameter, we
#' # could run 
#' # 
#' # \dontrun{
#' # output <- preprocess_scale(input=X, scaler_method="pca_whitening",
#' #   epsilon=0.01)
#' # X_scaled <- output$output
#' # }
#' # 
#' # You can also retransform the scaled dataset back using"inverse_scaling". An
#' # example to rescale : "X_scaled" into "X"using the saved model "input_model"
#' # is:
#' # 
#' # \dontrun{
#' # output <- preprocess_scale(input=X_scaled, inverse_scaling=TRUE,
#' #   input_model=saved)
#' # X <- output$output
#' # }
#' # 
#' # Another simple example where we want to scale the dataset "X" into
#' # "X_scaled" with  min_max_scaler as scaler method, where scaling range is 1
#' # to 3 instead of default 0 to 1. We could run 
#' # 
#' # \dontrun{
#' # output <- preprocess_scale(input=X, scaler_method="min_max_scaler",
#' #   min_value=1, max_value=3)
#' # X_scaled <- output$output
#' # }
preprocess_scale <- function(input,
                             epsilon = 1e-06,
                             input_model = NA,
                             inverse_scaling = FALSE,
                             max_value = 1,
                             min_value = 0,
                             scaler_method = "standard_scaler",
                             seed = 0,
                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("preprocess_scale")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamDouble(p, "epsilon", epsilon)

  if (!identical(input_model, NA)) {
    SetParamScalingModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamBool(p, "inverse_scaling", inverse_scaling)

  SetParamInt(p, "max_value", max_value)

  SetParamInt(p, "min_value", min_value)

  SetParamString(p, "scaler_method", scaler_method)

  SetParamInt(p, "seed", seed)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")
  SetPassed(p, "output_model")

  # Call the program.
  preprocess_scale_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamScalingModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "ScalingModel"

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output"),
      "output_model" = output_model
  )


  return(out)
}
