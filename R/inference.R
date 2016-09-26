#' Inference Errors
#'
#' Returns the specificity and sensitivity values for a given alpha and beta
#' value.
#'
#' Specificity is also known as the true negative rate, which is calculated as
#' the number of true negatives divided by the sum of true negatives and false
#' positives.
#'
#' Sensitivity is also known as the true positive rate, which is calculated as
#' the number of true positives divided by the sum of true positives and false
#' negatives.
#'
#' @param alpha Type I error or the false positive error
#' @param beta Type II error or the false negative error
#'
#' @return list containing the specificity and sensitivity
#'
#' @examples
#' inference(alpha = 0.05, beta = 0.2)
#' inference(alpha = 0.001, beta = 0.2)
#' inference(alpha = 0.05, beta = 0.5)
inference <- function(alpha = 0.05, beta = 0.2) {
    list(specificity = 1 - alpha, sensitivity = 1 - beta)
}