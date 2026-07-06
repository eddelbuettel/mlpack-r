#' @title Split Data
#'
#' @description
#' A utility to split data into a training and testing dataset.  This can also
#' split labels according to the same split.
#'
#' @param input Matrix containing data (numeric matrix).
#' @param input_labels Matrix containing labels (integer matrix).
#' @param no_shuffle Avoid shuffling the data before splitting.  Default
#'   value "FALSE" (logical).
#' @param seed Random seed (0 for std::time(NULL)).  Default value "0"
#'   (integer).
#' @param stratify_data Stratify the data according to label.  Default
#'   value "FALSE" (logical).
#' @param test_ratio Ratio of test set; if not set,the ratio defaults to
#'   0..  Default value "0.2" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{test}{Matrix to save test data to (numeric matrix).}
#' \item{test_labels}{Matrix to save test labels to (integer matrix).}
#' \item{training}{Matrix to save training data to (numeric matrix).}
#' \item{training_labels}{Matrix to save train labels to (integer
#'   matrix).}
#'
#' @details
#' This utility takes a dataset and optionally labels and splits them into a
#' training set and a test set. Before the split, the points in the dataset are
#' randomly reordered. The percentage of the dataset to be used as the test set
#' can be specified with the "test_ratio" parameter; the default is 0.2 (20%).
#' 
#' The output training and test matrices may be saved with the "training" and
#' "test" output parameters.
#' 
#' Optionally, labels can also be split along with the data by specifying the
#' "input_labels" parameter.  Splitting labels works the same way as splitting
#' the data. The output training and test labels may be saved with the
#' "training_labels" and "test_labels" output parameters, respectively.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to split the dataset "X" into "X_train"
#' # and "X_test" with 60% of the data in the training set and 40% of the
#' # dataset in the test set, we could run 
#' # 
#' # \dontrun{
#' # output <- preprocess_split(input=X, test_ratio=0.4)
#' # X_train <- output$training
#' # X_test <- output$test
#' # }
#' # 
#' # Also by default the dataset is shuffled and split; you can provide the
#' # "no_shuffle" option to avoid shuffling the data; an example to avoid
#' # shuffling of data is:
#' # 
#' # \dontrun{
#' # output <- preprocess_split(input=X, test_ratio=0.4, no_shuffle=TRUE)
#' # X_train <- output$training
#' # X_test <- output$test
#' # }
#' # 
#' # If we had a dataset "X" and associated labels "y", and we wanted to split
#' # these into "X_train", "y_train", "X_test", and "y_test", with 30% of the
#' # data in the test set, we could run
#' # 
#' # \dontrun{
#' # output <- preprocess_split(input=X, input_labels=y, test_ratio=0.3)
#' # X_train <- output$training
#' # y_train <- output$training_labels
#' # X_test <- output$test
#' # y_test <- output$test_labels
#' # }
#' # To maintain the ratio of each class in the train and test sets,
#' # the"stratify_data" option can be used.
#' # 
#' # \dontrun{
#' # output <- preprocess_split(input=X, test_ratio=0.4, stratify_data=TRUE)
#' # X_train <- output$training
#' # X_test <- output$test
#' # }
preprocess_split <- function(input,
                             input_labels = NA,
                             no_shuffle = FALSE,
                             seed = 0,
                             stratify_data = FALSE,
                             test_ratio = 0.2,
                             verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("preprocess_split")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamMat(p, "input", to_matrix(input), TRUE)

  if (!identical(input_labels, NA)) {
    SetParamUMat(p, "input_labels", to_matrix(input_labels))
  }

  SetParamBool(p, "no_shuffle", no_shuffle)

  SetParamInt(p, "seed", seed)

  SetParamBool(p, "stratify_data", stratify_data)

  SetParamDouble(p, "test_ratio", test_ratio)

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "test")
  SetPassed(p, "test_labels")
  SetPassed(p, "training")
  SetPassed(p, "training_labels")

  # Call the program.
  preprocess_split_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "test" = GetParamMat(p, "test"),
      "test_labels" = GetParamUMat(p, "test_labels"),
      "training" = GetParamMat(p, "training"),
      "training_labels" = GetParamUMat(p, "training_labels")
  )


  return(out)
}
