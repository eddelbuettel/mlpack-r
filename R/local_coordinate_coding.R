#' @title Local Coordinate Coding
#'
#' @description
#' An implementation of Local Coordinate Coding (LCC), a data transformation
#' technique.  Given input data, this transforms each point to be expressed as a
#' linear combination of a few points in the dataset; once an LCC model is
#' trained, it can be used to transform points later also.
#'
#' @param atoms Number of atoms in the dictionary.  Default value "0"
#'   (integer).
#' @param initial_dictionary Optional initial dictionary (numeric matrix).
#' @param input_model Input LCC model (LocalCoordinateCoding).
#' @param lambda Weighted l1-norm regularization parameter.  Default value
#'   "0" (numeric).
#' @param max_iterations Maximum number of iterations for LCC (0 indicates
#'   no limit).  Default value "0" (integer).
#' @param normalize If set, the input data matrix will be normalized before
#'   coding.  Default value "FALSE" (logical).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param test Test points to encode (numeric matrix).
#' @param tolerance Tolerance for objective function.  Default value "0.01"
#'   (numeric).
#' @param training Matrix of training data (X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{codes}{Output codes matrix (numeric matrix).}
#' \item{dictionary}{Output dictionary matrix (numeric matrix).}
#' \item{output_model}{Output for trained LCC model
#'   (LocalCoordinateCoding).}
#'
#' @details
#' An implementation of Local Coordinate Coding (LCC), which codes data that
#' approximately lives on a manifold using a variation of l1-norm regularized
#' sparse coding.  Given a dense data matrix X with n points and d dimensions,
#' LCC seeks to find a dense dictionary matrix D with k atoms in d dimensions,
#' and a coding matrix Z with n points in k dimensions.  Because of the
#' regularization method used, the atoms in D should lie close to the manifold
#' on which the data points lie.
#' 
#' The original data matrix X can then be reconstructed as D * Z.  Therefore,
#' this program finds a representation of each point in X as a sparse linear
#' combination of atoms in the dictionary D.
#' 
#' The coding is found with an algorithm which alternates between a dictionary
#' step, which updates the dictionary D, and a coding step, which updates the
#' coding matrix Z.
#' 
#' To run this program, the input matrix X must be specified (with -i), along
#' with the number of atoms in the dictionary (-k).  An initial dictionary may
#' also be specified with the "initial_dictionary" parameter.  The l1-norm
#' regularization parameter is specified with the "lambda" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run LCC on the dataset "data" using 200 atoms and an
#' # l1-regularization parameter of 0.1, saving the dictionary "dictionary" and
#' # the codes into "codes", use
#' # 
#' # \dontrun{
#' # output <- local_coordinate_coding(training=data, atoms=200, lambda=0.1)
#' # dict <- output$dictionary
#' # codes <- output$codes
#' # }
#' # 
#' # The maximum number of iterations may be specified with the "max_iterations"
#' # parameter. Optionally, the input data matrix X can be normalized before
#' # coding with the "normalize" parameter.
#' # 
#' # An LCC model may be saved using the "output_model" output parameter.  Then,
#' # to encode new points from the dataset "points" with the previously saved
#' # model "lcc_model", saving the new codes to "new_codes", the following
#' # command can be used:
#' # 
#' # \dontrun{
#' # output <- local_coordinate_coding(input_model=lcc_model, test=points)
#' # new_codes <- output$codes
#' # }
local_coordinate_coding <- function(atoms = 0,
                                    initial_dictionary = NA,
                                    input_model = NA,
                                    lambda = 0,
                                    max_iterations = 0,
                                    normalize = FALSE,
                                    seed = 0,
                                    test = NA,
                                    tolerance = 0.01,
                                    training = NA,
                                    verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("local_coordinate_coding")
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
    SetParamLocalCoordinateCodingPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamDouble(p, "lambda", lambda)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamBool(p, "normalize", normalize)

  SetParamInt(p, "seed", seed)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  SetParamDouble(p, "tolerance", tolerance)

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "codes")
  SetPassed(p, "dictionary")
  SetPassed(p, "output_model")

  # Call the program.
  local_coordinate_coding_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamLocalCoordinateCodingPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "LocalCoordinateCoding"

  # Extract the results in order.
  out <- list(
      "codes" = GetParamMat(p, "codes"),
      "dictionary" = GetParamMat(p, "dictionary"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_local_coordinate", "mlpack_model_binding", "list")

  return(out)
}
