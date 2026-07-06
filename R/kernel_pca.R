#' @title Kernel Principal Components Analysis
#'
#' @description
#' An implementation of Kernel Principal Components Analysis (KPCA).  This can
#' be used to perform nonlinear dimensionality reduction or preprocessing on a
#' given dataset.
#'
#' @param input Input dataset to perform KPCA on (numeric matrix).
#' @param kernel The kernel to use; see the above documentation for the
#'   list of usable kernels (character).
#' @param bandwidth Bandwidth, for 'gaussian' and 'laplacian' kernels. 
#'   Default value "1" (numeric).
#' @param center If set, the transformed data will be centered about the
#'   origin.  Default value "FALSE" (logical).
#' @param degree Degree of polynomial, for 'polynomial' kernel.  Default
#'   value "1" (numeric).
#' @param kernel_scale Scale, for 'hyptan' kernel.  Default value "1"
#'   (numeric).
#' @param new_dimensionality If not 0, reduce the dimensionality of the
#'   output dataset by ignoring the dimensions with the smallest eigenvalues. 
#'   Default value "0" (integer).
#' @param nystroem_method If set, the Nystroem method will be used. 
#'   Default value "FALSE" (logical).
#' @param offset Offset, for 'hyptan' and 'polynomial' kernels.  Default
#'   value "0" (numeric).
#' @param sampling Sampling scheme to use for the Nystroem method:
#'   'kmeans', 'random', 'ordered.  Default value "kmeans" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix to save modified dataset to (numeric matrix).}
#'
#' @details
#' This program performs Kernel Principal Components Analysis (KPCA) on the
#' specified dataset with the specified kernel.  This will transform the data
#' onto the kernel principal components, and optionally reduce the
#' dimensionality by ignoring the kernel principal components with the smallest
#' eigenvalues.
#' 
#' For the case where a linear kernel is used, this reduces to regular PCA.
#' 
#' The kernels that are supported are listed below:
#' 
#'  * 'linear': the standard linear dot product (same as normal PCA):
#'     `K(x, y) = x^T y`
#' 
#'  * 'gaussian': a Gaussian kernel; requires bandwidth:
#'     `K(x, y) = exp(-(|| x - y || ^ 2) / (2 * (bandwidth ^ 2)))`
#' 
#'  * 'polynomial': polynomial kernel; requires offset and degree:
#'     `K(x, y) = (x^T y + offset) ^ degree`
#' 
#'  * 'hyptan': hyperbolic tangent kernel; requires scale and offset:
#'     `K(x, y) = tanh(scale * (x^T y) + offset)`
#' 
#'  * 'laplacian': Laplacian kernel; requires bandwidth:
#'     `K(x, y) = exp(-(|| x - y ||) / bandwidth)`
#' 
#'  * 'epanechnikov': Epanechnikov kernel; requires bandwidth:
#'     `K(x, y) = max(0, 1 - || x - y ||^2 / bandwidth^2)`
#' 
#'  * 'cosine': cosine distance:
#'     `K(x, y) = 1 - (x^T y) / (|| x || * || y ||)`
#' 
#' The parameters for each of the kernels should be specified with the options
#' "bandwidth", "kernel_scale", "offset", or "degree" (or a combination of those
#' parameters).
#' 
#' Optionally, the Nystroem method ("Using the Nystroem method to speed up
#' kernel machines", 2001) can be used to calculate the kernel matrix by
#' specifying the "nystroem_method" parameter. This approach works by using a
#' subset of the data as basis to reconstruct the kernel matrix; to specify the
#' sampling scheme, the "sampling" parameter is used.  The sampling scheme for
#' the Nystroem method can be chosen from the following list: 'kmeans',
#' 'random', 'ordered'.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command will perform KPCA on the dataset "input"
#' # using the Gaussian kernel, and saving the transformed data to
#' # "transformed": 
#' # 
#' # \dontrun{
#' # transformed <- kernel_pca(input=input, kernel="gaussian")
#' # }
kernel_pca <- function(input,
                       kernel,
                       bandwidth = 1,
                       center = FALSE,
                       degree = 1,
                       kernel_scale = 1,
                       new_dimensionality = 0,
                       nystroem_method = FALSE,
                       offset = 0,
                       sampling = "kmeans",
                       verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("kernel_pca")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamString(p, "kernel", kernel)

  SetParamDouble(p, "bandwidth", bandwidth)

  SetParamBool(p, "center", center)

  SetParamDouble(p, "degree", degree)

  SetParamDouble(p, "kernel_scale", kernel_scale)

  SetParamInt(p, "new_dimensionality", new_dimensionality)

  SetParamBool(p, "nystroem_method", nystroem_method)

  SetParamDouble(p, "offset", offset)

  SetParamString(p, "sampling", sampling)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")

  # Call the program.
  kernel_pca_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = GetParamMat(p, "output")
  )

  # If output list is single element, flatten it.
  out <- out[[1]]

  # Add binding name as class to the output.
  class(out) <- c("mlpack_kernel", "mlpack_model_binding", "list")

  return(out)
}
