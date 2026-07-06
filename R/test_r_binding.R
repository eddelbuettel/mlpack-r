#' @title R binding test
#'
#' @description
#' A simple program to test R binding functionality.
#'
#' @param double_in Input double, must be 4.0 (numeric).
#' @param int_in Input int, must be 12 (integer).
#' @param string_in Input string, must be 'hello' (character).
#' @param build_model If true, a model will be returned.  Default value
#'   "FALSE" (logical).
#' @param col_in Input column (numeric column).
#' @param flag1 Input flag, must be specified.  Default value "FALSE"
#'   (logical).
#' @param flag2 Input flag, must not be specified.  Default value "FALSE"
#'   (logical).
#' @param matrix_and_info_in Input matrix and info (numeric
#'   matrix/data.frame with info).
#' @param matrix_in Input matrix (numeric matrix).
#' @param model_in Input model (GaussianKernel).
#' @param row_in Input row (numeric row).
#' @param str_vector_in Input vector of strings (character vector).
#' @param tmatrix_in Input (transposed) matrix (numeric matrix).
#' @param ucol_in Input unsigned column (integer column).
#' @param umatrix_in Input unsigned matrix (integer matrix).
#' @param urow_in Input unsigned row (integer row).
#' @param vector_in Input vector of numbers (integer vector).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{col_out}{Output column. 2x input colum (numeric column).}
#' \item{double_out}{Output double, will be 5.0.  Default value "0"
#'   (numeric).}
#' \item{int_out}{Output int, will be 13.  Default value "0" (integer).}
#' \item{matrix_and_info_out}{Output matrix and info; all numeric elements
#'   multiplied by 3 (numeric matrix).}
#' \item{matrix_out}{Output matrix (numeric matrix).}
#' \item{model_bw_out}{The bandwidth of the model.  Default value "0"
#'   (numeric).}
#' \item{model_out}{Output model, with twice the bandwidth
#'   (GaussianKernel).}
#' \item{row_out}{Output row.  2x input row (numeric row).}
#' \item{str_vector_out}{Output string vector (character vector).}
#' \item{string_out}{Output string, will be 'hello2'.  Default value ""
#'   (character).}
#' \item{ucol_out}{Output unsigned column. 2x input column (integer
#'   column).}
#' \item{umatrix_out}{Output unsigned matrix (integer matrix).}
#' \item{urow_out}{Output unsigned row.  2x input row (integer row).}
#' \item{vector_out}{Output vector (integer vector).}
#'
#' @details
#' A simple program to test R binding functionality.  You can build mlpack with
#' the BUILD_TESTS option set to off, and this binding will no longer be built.
#'
#' @author
#' mlpack developers
#'
#' @export
test_r_binding <- function(double_in,
                           int_in,
                           string_in,
                           build_model = FALSE,
                           col_in = NA,
                           flag1 = FALSE,
                           flag2 = FALSE,
                           matrix_and_info_in = NA,
                           matrix_in = NA,
                           model_in = NA,
                           row_in = NA,
                           str_vector_in = NA,
                           tmatrix_in = NA,
                           ucol_in = NA,
                           umatrix_in = NA,
                           urow_in = NA,
                           vector_in = NA,
                           verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("test_R_binding")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamDouble(p, "double_in", double_in)

  SetParamInt(p, "int_in", int_in)

  SetParamString(p, "string_in", string_in)

  SetParamBool(p, "build_model", build_model)

  if (!identical(col_in, NA)) {
    SetParamCol(p, "col_in", to_matrix(col_in))
  }

  SetParamBool(p, "flag1", flag1)

  SetParamBool(p, "flag2", flag2)

  if (!identical(matrix_and_info_in, NA)) {
    matrix_and_info_in <- to_matrix_with_info(matrix_and_info_in)
    SetParamMatWithInfo(p, "matrix_and_info_in", matrix_and_info_in$info, matrix_and_info_in$data)
  }

  if (!identical(matrix_in, NA)) {
    SetParamMat(p, "matrix_in", to_matrix(matrix_in), TRUE)
  }

  if (!identical(model_in, NA)) {
    SetParamGaussianKernelPtr(p, "model_in", model_in)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, model_in)
  }

  if (!identical(row_in, NA)) {
    SetParamRow(p, "row_in", to_matrix(row_in))
  }

  if (!identical(str_vector_in, NA)) {
    SetParamVecString(p, "str_vector_in", str_vector_in)
  }

  if (!identical(tmatrix_in, NA)) {
    SetParamMat(p, "tmatrix_in", to_matrix(tmatrix_in), FALSE)
  }

  if (!identical(ucol_in, NA)) {
    SetParamUCol(p, "ucol_in", to_matrix(ucol_in))
  }

  if (!identical(umatrix_in, NA)) {
    SetParamUMat(p, "umatrix_in", to_matrix(umatrix_in))
  }

  if (!identical(urow_in, NA)) {
    SetParamURow(p, "urow_in", to_matrix(urow_in))
  }

  if (!identical(vector_in, NA)) {
    SetParamVecInt(p, "vector_in", vector_in)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "col_out")
  SetPassed(p, "double_out")
  SetPassed(p, "int_out")
  SetPassed(p, "matrix_and_info_out")
  SetPassed(p, "matrix_out")
  SetPassed(p, "model_bw_out")
  SetPassed(p, "model_out")
  SetPassed(p, "row_out")
  SetPassed(p, "str_vector_out")
  SetPassed(p, "string_out")
  SetPassed(p, "ucol_out")
  SetPassed(p, "umatrix_out")
  SetPassed(p, "urow_out")
  SetPassed(p, "vector_out")

  # Call the program.
  test_r_binding_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  model_out <- GetParamGaussianKernelPtr(p, "model_out", inputModels)
  attr(model_out, "type") <- "GaussianKernel"

  # Extract the results in order.
  out <- list(
      "col_out" = GetParamCol(p, "col_out"),
      "double_out" = GetParamDouble(p, "double_out"),
      "int_out" = GetParamInt(p, "int_out"),
      "matrix_and_info_out" = GetParamMat(p, "matrix_and_info_out"),
      "matrix_out" = GetParamMat(p, "matrix_out"),
      "model_bw_out" = GetParamDouble(p, "model_bw_out"),
      "model_out" = model_out,
      "row_out" = GetParamRow(p, "row_out"),
      "str_vector_out" = GetParamVecString(p, "str_vector_out"),
      "string_out" = GetParamString(p, "string_out"),
      "ucol_out" = GetParamUCol(p, "ucol_out"),
      "umatrix_out" = GetParamUMat(p, "umatrix_out"),
      "urow_out" = GetParamURow(p, "urow_out"),
      "vector_out" = GetParamVecInt(p, "vector_out")
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_test_R", "mlpack_model_binding", "list")

  return(out)
}
