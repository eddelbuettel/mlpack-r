#' @title One Hot Encoding
#'
#' @description
#' A utility to do one-hot encoding on features of dataset.
#'
#' @param input Matrix containing data (numeric matrix/data.frame with
#'   info).
#' @param dimensions Index of dimensions that need to be one-hot encoded
#'   (if unspecified, all categorical dimensions are one-hot encoded) (integer
#'   vector).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to save one-hot encoded features data to (numeric
#'   matrix).}
#'
#' @details
#' This utility takes a dataset and a vector of indices and does one-hot
#' encoding of the respective features at those indices. Indices represent the
#' IDs of the dimensions to be one-hot encoded.
#' 
#' If no dimensions are specified with "dimensions", then all categorical-type
#' dimensions will be one-hot encoded. Otherwise, only the dimensions given in
#' "dimensions" will be one-hot encoded.
#' 
#' The output matrix with encoded features may be saved with the "output"
#' parameters.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to encode 1st and 3rd feature from
#' # dataset "X" into "X_output" would be
#' # 
#' # \dontrun{
#' # X_output <- preprocess_one_hot_encoding(input=X, dimensions=1,
#' # dimensions=3)
#' # }
preprocess_one_hot_encoding <- function(input,
                                        dimensions = NA,
                                        verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("preprocess_one_hot_encoding")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  input <- to_matrix_with_info(input)
  SetParamMatWithInfo(p, "input", input$info, input$data)

  if (!identical(dimensions, NA)) {
    SetParamVecInt(p, "dimensions", dimensions)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  preprocess_one_hot_encoding_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]


  return(out)
}
