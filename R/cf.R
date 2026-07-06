#' @title Collaborative Filtering
#'
#' @description
#' An implementation of several collaborative filtering (CF) techniques for
#' recommender systems.  This can be used to train a new CF model, or use an
#' existing CF model to compute recommendations.
#'
#' @param algorithm Algorithm used for matrix factorization.  Default value
#'   "NMF" (character).
#' @param all_user_recommendations Generate recommendations for all users. 
#'   Default value "FALSE" (logical).
#' @param input_model Trained CF model to load (CFModel).
#' @param interpolation Algorithm used for weight interpolation.  Default
#'   value "average" (character).
#' @param iteration_only_termination Terminate only when the maximum number
#'   of iterations is reached.  Default value "FALSE" (logical).
#' @param max_iterations Maximum number of iterations. If set to zero,
#'   there is no limit on the number of iterations.  Default value "1000"
#'   (integer).
#' @param min_residue Residue required to terminate the factorization
#'   (lower values generally mean better fits).  Default value "1e-05"
#'   (numeric).
#' @param neighbor_search Algorithm used for neighbor search.  Default
#'   value "euclidean" (character).
#' @param neighborhood Size of the neighborhood of similar users to
#'   consider for each query user.  Default value "5" (integer).
#' @param normalization Normalization performed on the ratings.  Default
#'   value "none" (character).
#' @param query List of query users for which recommendations should be
#'   generated (integer matrix).
#' @param rank Rank of decomposed matrices (if 0, a heuristic is used to
#'   estimate the rank).  Default value "0" (integer).
#' @param recommendations Number of recommendations to generate for each
#'   query user.  Default value "5" (integer).
#' @param seed Set the random seed (0 uses std::time(NULL)).  Default value
#'   "0" (integer).
#' @param test Test set to calculate RMSE on (numeric matrix).
#' @param training Input dataset to perform CF on (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value
#'   "getOption("mlpack.verbose", FALSE)" (logical).
#'
#' @return A list with several components defining the class attributes:
#' \item{output}{Matrix that will store output recommendations (integer
#'   matrix).}
#' \item{output_model}{Output for trained CF model (CFModel).}
#'
#' @details
#' This program performs collaborative filtering (CF) on the given dataset.
#' Given a list of user, item and preferences (the "training" parameter), the
#' program will perform a matrix decomposition and then can perform a series of
#' actions related to collaborative filtering.  Alternately, the program can
#' load an existing saved CF model with the "input_model" parameter and then use
#' that model to provide recommendations or predict values.
#' 
#' The input matrix should be a 3-dimensional matrix of ratings, where the first
#' dimension is the user, the second dimension is the item, and the third
#' dimension is that user's rating of that item.  Both the users and items
#' should be numeric indices, not names. The indices are assumed to start from
#' 0.
#' 
#' A set of query users for which recommendations can be generated may be
#' specified with the "query" parameter; alternately, recommendations may be
#' generated for every user in the dataset by specifying the
#' "all_user_recommendations" parameter.  In addition, the number of
#' recommendations per user to generate can be specified with the
#' "recommendations" parameter, and the number of similar users (the size of the
#' neighborhood) to be considered when generating recommendations can be
#' specified with the "neighborhood" parameter.
#' 
#' For performing the matrix decomposition, the following optimization
#' algorithms can be specified via the "algorithm" parameter:
#' 
#'  - 'RegSVD' -- Regularized SVD using a SGD optimizer
#'  - 'NMF' -- Non-negative matrix factorization with alternating least squares
#' update rules
#'  - 'BatchSVD' -- SVD batch learning
#'  - 'SVDIncompleteIncremental' -- SVD incomplete incremental learning
#'  - 'SVDCompleteIncremental' -- SVD complete incremental learning
#'  - 'BiasSVD' -- Bias SVD using a SGD optimizer
#'  - 'SVDPP' -- SVD++ using a SGD optimizer
#'  - 'RandSVD' -- RandomizedSVD learning
#'  - 'QSVD' -- QuicSVD learning
#'  - 'BKSVD' -- Block Krylov SVD learning
#' 
#' 
#' The following neighbor search algorithms can be specified via the
#' "neighbor_search" parameter:
#' 
#'  - 'cosine'  -- Cosine Search Algorithm
#'  - 'euclidean'  -- Euclidean Search Algorithm
#'  - 'pearson'  -- Pearson Search Algorithm
#' 
#' 
#' The following weight interpolation algorithms can be specified via the
#' "interpolation" parameter:
#' 
#'  - 'average'  -- Average Interpolation Algorithm
#'  - 'regression'  -- Regression Interpolation Algorithm
#'  - 'similarity'  -- Similarity Interpolation Algorithm
#' 
#' 
#' The following ranking normalization algorithms can be specified via the
#' "normalization" parameter:
#' 
#'  - 'none'  -- No Normalization
#'  - 'item_mean'  -- Item Mean Normalization
#'  - 'overall_mean'  -- Overall Mean Normalization
#'  - 'user_mean'  -- User Mean Normalization
#'  - 'z_score'  -- Z-Score Normalization
#' 
#' A trained model may be saved to with the "output_model" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # To train a CF model on a dataset "training_set" using NMF for decomposition
#' # and saving the trained model to "model", one could call: 
#' # 
#' # \dontrun{
#' # output <- cf(training=training_set, algorithm="NMF")
#' # model <- output$output_model
#' # }
#' # 
#' # Then, to use this model to generate recommendations for the list of users
#' # in the query set "users", storing 5 recommendations in "recommendations",
#' # one could call 
#' # 
#' # \dontrun{
#' # output <- cf(input_model=model, query=users, recommendations=5)
#' # recommendations <- output$output
#' # }
cf <- function(algorithm = "NMF",
               all_user_recommendations = FALSE,
               input_model = NA,
               interpolation = "average",
               iteration_only_termination = FALSE,
               max_iterations = 1000,
               min_residue = 1e-05,
               neighbor_search = "euclidean",
               neighborhood = 5,
               normalization = "none",
               query = NA,
               rank = 0,
               recommendations = 5,
               seed = 0,
               test = NA,
               training = NA,
               verbose = getOption("mlpack.verbose", FALSE)) {
  # Create parameters and timers objects.
  p <- CreateParams("cf")
  t <- CreateTimers()
  # Initialize an empty list that will hold all input models the user gave us,
  # so that we don't accidentally create two XPtrs that point to thesame model.
  inputModels <- vector()

  # Process each input argument before calling the binding.
  SetParamString(p, "algorithm", algorithm)

  SetParamBool(p, "all_user_recommendations", all_user_recommendations)

  if (!identical(input_model, NA)) {
    SetParamCFModelPtr(p, "input_model", input_model)
    # Add to the list of input models we received.
    inputModels <- append(inputModels, input_model)
  }

  SetParamString(p, "interpolation", interpolation)

  SetParamBool(p, "iteration_only_termination", iteration_only_termination)

  SetParamInt(p, "max_iterations", max_iterations)

  SetParamDouble(p, "min_residue", min_residue)

  SetParamString(p, "neighbor_search", neighbor_search)

  SetParamInt(p, "neighborhood", neighborhood)

  SetParamString(p, "normalization", normalization)

  if (!identical(query, NA)) {
    SetParamUMat(p, "query", to_matrix(query))
  }

  SetParamInt(p, "rank", rank)

  SetParamInt(p, "recommendations", recommendations)

  SetParamInt(p, "seed", seed)

  if (!identical(test, NA)) {
    SetParamMat(p, "test", to_matrix(test), TRUE)
  }

  if (!identical(training, NA)) {
    SetParamMat(p, "training", to_matrix(training), TRUE)
  }

  SetParamBool(p, "verbose", verbose)

  # Mark all output options as passed.
  SetPassed(p, "output")
  SetPassed(p, "output_model")

  # Call the program.
  cf_call(p, t)

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- GetParamCFModelPtr(p, "output_model", inputModels)
  attr(output_model, "type") <- "CFModel"

  # Extract the results in order.
  out <- list(
      "output" = GetParamUMat(p, "output"),
      "output_model" = output_model
  )

  # Add binding name as class to the output.
  class(out) <- c("mlpack_cf", "mlpack_model_binding", "list")

  return(out)
}
