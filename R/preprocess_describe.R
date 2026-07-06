#' @title Descriptive Statistics
#'
#' @description
#' A utility for printing descriptive statistics about a dataset.  This prints a
#' number of details about a dataset in a tabular format.
#'
#' @param input Matrix containing data (numeric matrix).
#' @param dimension Dimension of the data. Use this to specify a dimensio. 
#'   Default value "0" (integer).
#' @param population If specified, the program will calculate statistics
#'   assuming the dataset is the population. By default, the program will assume
#'   the dataset as a sample.  Default value "FALSE" (logical).
#' @param precision Precision of the output statistics.  Default value "4"
#'   (integer).
#' @param row_major If specified, the program will calculate statistics
#'   across rows, not across columns.  (Remember that in mlpack, a column
#'   represents a point, so this option is generally not necessary..  Default
#'   value "FALSE" (logical).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#' @param width Width of the output table.  Default value "8" (integer).
#'
#'
#' @details
#' This utility takes a dataset and prints out the descriptive statistics of the
#' data. Descriptive statistics is the discipline of quantitatively describing
#' the main features of a collection of information, or the quantitative
#' description itself. The program does not modify the original file, but
#' instead prints out the statistics to the console. The printed result will
#' look like a table.
#' 
#' Optionally, width and precision of the output can be adjusted by a user using
#' the "width" and "precision" parameters. A user can also select a specific
#' dimension to analyze if there are too many dimensions. The "population"
#' parameter can be specified when the dataset should be considered as a
#' population.  Otherwise, the dataset will be considered as a sample.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to print out statistical facts about the
#' # dataset "X" using the default settings, we could run 
#' # 
#' # \dontrun{
#' # preprocess_describe(input=X, verbose=TRUE)
#' # }
#' # 
#' # If we want to customize the width to 10 and precision to 5 and consider the
#' # dataset as a population, we could run
#' # 
#' # \dontrun{
#' # preprocess_describe(input=X, width=10, precision=5, verbose=TRUE)
#' # }
preprocess_describe <- function(input,
                                dimension = 0,
                                population = FALSE,
                                precision = 4,
                                row_major = FALSE,
                                verbose = getOption("mlpack.verbose", FALSE),
                                width = 8) {
  # Create parameters and timers objects.
  p <- CreateParams("preprocess_describe")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  SetParamInt(p, "dimension", dimension)

  SetParamBool(p, "population", population)

  SetParamInt(p, "precision", precision)

  SetParamBool(p, "row_major", row_major)

  SetParamBool(p, "verbose", verbose)

  SetParamInt(p, "width", width)

  # Mark all output options as passed.

  # Call the program.
  preprocess_describe_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(

  )


  return(out)
}
