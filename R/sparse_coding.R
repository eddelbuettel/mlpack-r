#' @title Sparse Coding
#'
#' @description
#' An implementation of Sparse Coding with Dictionary Learning.  Given a
#' dataset, this will decompose the dataset into a sparse combination of a few
#' dictionary elements, where the dictionary is learned during computation; a
#' dictionary can be reused for future sparse coding of new points.
#'
#' @param atoms Number of atoms in the dictionary.  Default value "15"
#'   (integer).
#' @param initial_dictionary Optional initial dictionary matrix (numeric
#'   matrix).
#' @param input_model File containing input sparse coding model
#'   (SparseCoding).
#' @param lambda1 Sparse coding l1-norm regularization parameter.  Default
#'   value "0" (numeric).
#' @param lambda2 Sparse coding l2-norm regularization parameter.  Default
#'   value "0" (numeric).
#' @param max_iterations Maximum number of iterations for sparse coding (0
#'   indicates no limit).  Default value "0" (integer).
#' @param newton_tolerance Tolerance for convergence of Newton method. 
#'   Default value "1e-06" (numeric).
#' @param normalize If set, the input data matrix will be normalized before
#'   coding.  Default value "FALSE" (logical).
#' @param objective_tolerance Tolerance for convergence of the objective
#'   function.  Default value "0.01" (numeric).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param test Optional matrix to be encoded by trained model (numeric
#'   matrix).
#' @param training Matrix of training data (X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{codes}{Matrix to save the output sparse codes of the test matrix
#'   (--test_file) to (numeric matrix).}
#' \item{dictionary}{Matrix to save the output dictionary to (numeric
#'   matrix).}
#' \item{output_model}{File to save trained sparse coding model to
#'   (SparseCoding).}
#'
#' @details
#' An implementation of Sparse Coding with Dictionary Learning, which achieves
#' sparsity via an l1-norm regularizer on the codes (LASSO) or an (l1+l2)-norm
#' regularizer on the codes (the Elastic Net).  Given a dense data matrix X with
#' d dimensions and n points, sparse coding seeks to find a dense dictionary
#' matrix D with k atoms in d dimensions, and a sparse coding matrix Z with n
#' points in k dimensions.
#' 
#' The original data matrix X can then be reconstructed as Z * D.  Therefore,
#' this program finds a representation of each point in X as a sparse linear
#' combination of atoms in the dictionary D.
#' 
#' The sparse coding is found with an algorithm which alternates between a
#' dictionary step, which updates the dictionary D, and a sparse coding step,
#' which updates the sparse coding matrix.
#' 
#' Once a dictionary D is found, the sparse coding model may be used to encode
#' other matrices, and saved for future usage.
#' 
#' To run this program, either an input matrix or an already-saved sparse coding
#' model must be specified.  An input matrix may be specified with the
#' "training" option, along with the number of atoms in the dictionary
#' (specified with the "atoms" parameter).  It is also possible to specify an
#' initial dictionary for the optimization, with the "initial_dictionary"
#' parameter.  An input model may be specified with the "input_model"
#' parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # As an example, to build a sparse coding model on the dataset "data" using
#' # 200 atoms and an l1-regularization parameter of 0.1, saving the model into
#' # "model", use 
#' # 
#' # \dontrun{
#' # output <- sparse_coding(training=data, atoms=200, lambda1=0.1)
#' # model <- output$output_model
#' # }
#' # 
#' # Then, this model could be used to encode a new matrix, "otherdata", and
#' # save the output codes to "codes": 
#' # 
#' # \dontrun{
#' # output <- sparse_coding(input_model=model, test=otherdata)
#' # codes <- output$codes
#' # }
sparse_coding <- function(atoms = 15,
                          initial_dictionary = NA,
                          input_model = NA,
                          lambda1 = 0,
                          lambda2 = 0,
                          max_iterations = 0,
                          newton_tolerance = 1e-06,
                          normalize = FALSE,
                          objective_tolerance = 0.01,
                          seed = 0,
                          test = NA,
                          training = NA,
                          verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("sparse_coding")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamInt(p, "atoms", atoms)

  if (!identical(initial_dictionary, NA)) {
    SetParamMat(p, "initial_dictionary", to_matrix(initial_dictionary), TRUE)
  }

  if (!identical(input_model, NA)) {
    SetParamSparseCodingPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamDouble(p, "lambda1", lambda1)

  SetParamDouble(p, "lambda2", lambda2)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamDouble(p, "newton_tolerance", newton_tolerance)

  SetParamBool(p, "normalize", normalize)

  SetParamDouble(p, "objective_tolerance", objective_tolerance)

  SetParamInt(p, "seed", seed)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "codes")
  SetPassed(p, "dictionary")
  SetPassed(p, "output_model")

  # Call the program.
  sparse_coding_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamSparseCodingPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "SparseCoding"

  # Extract the results in order.
  out <- list(
      "codes" = GetParamMat(p, "codes"),
      "dictionary" = GetParamMat(p, "dictionary"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_sparse", "mlpack_model_binding", "list")

  return(out)
}
