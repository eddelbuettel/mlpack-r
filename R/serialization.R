#' Serialize/Unserialize an mlpack model.
#'
#' @param model Input model pointer.
#' @param filename Input filename.
#' @export
#' @rdname mlpack-serialization
Serialize <- function(model, filename) {
  model_serialization_function <-
    switch(attributes(model)$type,
      "GaussianKernel" = SerializeGaussianKernelPtr,
      "ApproxKFNModel" = SerializeApproxKFNModelPtr,
      "CFModel" = SerializeCFModelPtr,
      "DTree" = SerializeDTreePtr,
      "FastMKSModel" = SerializeFastMKSModelPtr,
      "GMM" = SerializeGMMPtr,
      "HMMModel" = SerializeHMMModelPtr,
      "HoeffdingTreeModel" = SerializeHoeffdingTreeModelPtr,
      "KDEModel" = SerializeKDEModelPtr,
      "BayesianLinearRegression" = SerializeBayesianLinearRegressionPtr,
      "LARS" = SerializeLARSPtr,
      "LinearSVMModel" = SerializeLinearSVMModelPtr,
      "LocalCoordinateCoding" = SerializeLocalCoordinateCodingPtr,
      "LogisticRegression" = SerializeLogisticRegressionPtr,
      "LSHSearch" = SerializeLSHSearchPtr,
      "NBCModel" = SerializeNBCModelPtr,
      "KNNModel" = SerializeKNNModelPtr,
      "KFNModel" = SerializeKFNModelPtr,
      "PerceptronModel" = SerializePerceptronModelPtr,
      "ScalingModel" = SerializeScalingModelPtr,
      "RAModel" = SerializeRAModelPtr,
      "SoftmaxRegression" = SerializeSoftmaxRegressionPtr,
      "SparseCoding" = SerializeSparseCodingPtr,
      "RandomForestModel" = SerializeRandomForestModelPtr,
      "DecisionTreeModel" = SerializeDecisionTreeModelPtr,
      "AdaBoostModel" = SerializeAdaBoostModelPtr,
      "LinearRegression" = SerializeLinearRegressionPtr,
      stop("Requested model type is not currently supported.")
    )

  # Read in model
  con <- file(as.character(filename), "wb")
  serialize(model_serialization_function(model), con)
  close(con)
}


#' @return For Unserialize, Output model_ptr.
#' @export
#' @rdname mlpack-serialization
Unserialize <- function(filename) {
  con <- file(as.character(filename), "rb")
  model <- unserialize(con)

  model_unserialization_function <-
    switch(attributes(model)$type,
      "GaussianKernel" = DeserializeGaussianKernelPtr,
      "ApproxKFNModel" = DeserializeApproxKFNModelPtr,
      "CFModel" = DeserializeCFModelPtr,
      "DTree" = DeserializeDTreePtr,
      "FastMKSModel" = DeserializeFastMKSModelPtr,
      "GMM" = DeserializeGMMPtr,
      "HMMModel" = DeserializeHMMModelPtr,
      "HoeffdingTreeModel" = DeserializeHoeffdingTreeModelPtr,
      "KDEModel" = DeserializeKDEModelPtr,
      "BayesianLinearRegression" = DeserializeBayesianLinearRegressionPtr,
      "LARS" = DeserializeLARSPtr,
      "LinearSVMModel" = DeserializeLinearSVMModelPtr,
      "LocalCoordinateCoding" = DeserializeLocalCoordinateCodingPtr,
      "LogisticRegression" = DeserializeLogisticRegressionPtr,
      "LSHSearch" = DeserializeLSHSearchPtr,
      "NBCModel" = DeserializeNBCModelPtr,
      "KNNModel" = DeserializeKNNModelPtr,
      "KFNModel" = DeserializeKFNModelPtr,
      "PerceptronModel" = DeserializePerceptronModelPtr,
      "ScalingModel" = DeserializeScalingModelPtr,
      "RAModel" = DeserializeRAModelPtr,
      "SoftmaxRegression" = DeserializeSoftmaxRegressionPtr,
      "SparseCoding" = DeserializeSparseCodingPtr,
      "RandomForestModel" = DeserializeRandomForestModelPtr,
      "DecisionTreeModel" = DeserializeDecisionTreeModelPtr,
      "AdaBoostModel" = DeserializeAdaBoostModelPtr,
      "LinearRegression" = DeserializeLinearRegressionPtr,
      stop("Requested model type is not currently supported.")
    )

  model_ptr <- model_unserialization_function(model)
  close(con)
  return(model_ptr)
}
