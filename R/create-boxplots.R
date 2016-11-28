#' Create Histogram and Box Plot
#'
#' @param n sample size for box plots
#' @param s1,s2 non-negative parameters of the Beta distribution
#'
#' @return histrogram, summary statistics of data, and box plot
#' @export
#'
#' @references Krzywinski, Martin, and Naomi Altman. "Points of significance:
#'   Visualizing samples with box plots." Nature Methods 11.2 (2014): 119-120.
#'
#' @examples
#' create_boxplots()
#' create_boxplots(n = 100)
#' create_boxplots(n = 100, s1 = 5, s2 = 2)
create_boxplots <- function(n = 1000, s1 = 2, s2 = 5) {
    par(mfrow = c(2, 1))

    # Create population distribution
    x <- rbeta(n, s1, s2)

    # Plot density distribution histogram
    plot(density(x), main = "")
    abline(v = median(x)) # Plot median as solid black line
    abline(v = mean(x), lty = 3) # Plot mean as dotted line

    # Summary of x
    print(summary(x))
    cat("IQR:", IQR(x), "\n")
    cat("SD:", sd(x), "\n")

    # Plot boxplot
    boxplot(x, horizontal = TRUE)

    par(mfrow = c(1, 1))
}