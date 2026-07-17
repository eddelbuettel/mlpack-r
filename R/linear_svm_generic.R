#' @rdname linear_svm
#' @param object An instantiated model object for which prediction is desired
#' @param newdata A test data set
#' @param type The type of result desired
#' @param ... Additional optional arguments affecting the prediction
#' @export
predict.mlpack_linear_svm <- function(object, newdata, type=c("predictions", "scores"), ...) {
    if (missing(newdata)) {
        stop("Need 'newdata'")
    }
    type <- match.arg(type)
    if (type == "predictions") {
        res <- linear_svm_classify(input_model=object, newdata, ...)
    } else {   res <- linear_svm_scores(input_model=object, newdata, ...)
    }
    res
}
