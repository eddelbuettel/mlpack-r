#' @rdname decision_tree
#' @param object An instantiated model object for which prediction is desired
#' @param newdata A test data set
#' @param type The type of result desired
#' @param ... Additional optional arguments affecting the prediction
#' @export
predict.mlpack_decision_tree <- function(object, newdata, type=c("predictions", "probabilities"), ...) {
    if (missing(newdata)) {
        stop("Need 'newdata'")
    }
    type <- match.arg(type)
    if (type == "predictions") {
        res <- decision_tree_classify(input_model=object, newdata, ...)
    } else {   res <- decision_tree_probabilities(input_model=object, newdata, ...)
    }
    res
}
