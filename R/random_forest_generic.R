#' @rdname random_forest
#' @param object An instantiated model object for which prediction is desired
#' @param newdata A test data set
#' @param type The type of result desired
#' @param ... Additional optional arguments affecting the prediction
#' @export
predict.mlpack_random_forest <- function(object, newdata, type=c("predictions", "probabilities"), ...) {
    if (missing(newdata)) {
        stop("Need 'newdata'")
    }
    type <- match.arg(type)
    if (type == "predictions") {
        res <- random_forest_classify(input_model=object, newdata, ...)
    } else {   res <- random_forest_probabilities(input_model=object, newdata, ...)
    }
    res
}
