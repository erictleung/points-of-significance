#' Compare Reference
#'
#' Generates \code{n} values from a normal distribution with mean (\code{mean})
#' and standard deviation (\code{sd}).
#'
#' @param reference a reference population mean
#' @param n number of sampels
#' @param mean sample mean
#' @param sd sample standard deviation
#'
#' @return Returns a list of values.
#' @export
#'
#' @references Krzywinski, Martin, and Naomi Altman. "Points of significance:
#'   Comparing samples--part I." Nature Methods 11.3 (2014): 215-216.
#'
#' @examples
#' compare_reference()
compare_reference <- function(reference = 10, n = 5, mean = 11, sd = 0.84) {
    nums <- rnorm(n, mean, sd)
    variance <- var(nums) # Find variance
    sem <- sqrt(variance) / n # Standard error of the mean
    mean_diff <- mean(nums) - reference
    var_norm <- variance / n
    list(variance = variance, mean_diff = mean_diff, sem = sem, var_norm = var_norm)
}