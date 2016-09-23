#' Positive Predictive Value
#'
#' Calculates positive predictive value given the effect size, power, and alpha
#' level.
#'
#' The positive predictive value is the fraction of true positives out of
#' positive labeled classes (i.e. true positive and false positive).
#'
#' @param effectSize the quantitative measure of the strength of a phenomenon
#'   in an experiment, between 0 and 1
#' @param power probability that the test correctly rejects the null hypothesis
#'   when the alternative hypothesis is true
#' @param alpha false positive value or Type I error
#'
#' @return positive predictive value
#' @export
#'
#' @examples
#' ppv(effectSize = 0.5, power = 0.2, alpha = 0.05)
#' ppv(effectSize = 0.5, power = 0.5, alpha = 0.05)
#' ppv(effectSize = 0.5, power = 0.8, alpha = 0.05)
ppv <- function(effectSize, power, alpha) {
    fp <- (1 - effectSize) * alpha
    tp <- effectSize * power

    tp / (tp + fp)
}