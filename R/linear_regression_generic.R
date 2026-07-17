#' @rdname linear_regression
#' @param object An instantiated model object for which prediction is desired
#' @param newdata A test data set
#' @param type The type of result desired
#' @param ... Additional optional arguments affecting the prediction
#' @export
predict.mlpack_linear_regression <- function(object, newdata, type=c("predictions"), ...) {
    if (missing(newdata)) {
        stop("Need 'newdata'")
    }
    type <- match.arg(type)
    if (type == "predictions") {
        res <- linear_regression_predict(input_model=object, newdata, ...)
    
    }
    res
}
