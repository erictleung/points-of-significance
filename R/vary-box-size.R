#' Vary Box Plot Sizes
#'
#' Plots \code{plotNum} number of box plots with the same sample size \code{n}
#' sampled from a normal distribution.
#'
#' Box plots are very powerful visualization tools. However, you need sufficient
#' number of samples for them to be confident sources of information.
#'
#' Try changing the sample size of the plots drawn to understand the variability
#' in visualizing small datasets.
#'
#' @param n sample size
#' @param plotNum number of box plots
#'
#' @return Plot box plots of five samples from a normal distribution
#' @export
#'
#' @references Krzywinski, Martin, and Naomi Altman. "Points of significance:
#'   visualizing samples with box plots." Nature Methods 11.2 (2014): 119-120.
#'
#' @examples
#' vary_box_size()
vary_box_size <- function(n = 5, plotNum = 5) {
    if (plotNum < 0) {
        stop("The argument `plotNum` needs to be positive")
    }
    if (n < 0) {
        stop("The argument `n` needs to be positive")
    }

    # Make empty data frame
    mat <- data.frame(data = NULL, plot = NULL)

    # Create data to plot
    for (i in 1:plotNum) {
        vals <- rnorm(n)
        temp <- cbind(vals, plot = i)
        mat <- rbind(mat, temp)
    }

    boxplot(vals ~ plot, data = mat, col = "lightgray")
}