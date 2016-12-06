#' Generate Population Distribution
#'
#' Creates a density plot with annotations
#'
#' @param mu population mean
#' @param std population standard deviation
#' @param n sample size
#'
#' @return Creates density plot with annotations plus prints out mean and
#'   standard deviation
#' @export
#'
#' @references Krzywinski, Martin, and Naomi Altman. "Points of significance:
#'   Importance of being uncertain." Nature Methods 10.9 (2013): 809-810.
#'
#' @examples
#' population_dist()                # Mean = 0, Stdev = 1, N = 100
#' population_dist(mu = 2)          # Mean = 2, Stdev = 1, N = 100
#' population_dist(mu = 1, std = 3) # Mean = 1, Stdev = 3, N = 100
#' population_dist(std = 3, n = 10) # Mean = 0, Stdev = 3, N = 10
population_dist <- function(mu = 0,
                            std = 1,
                            n = 100) {
    # Generate points
    pts <- rnorm(n = n, mean = mu, sd = std)
    densityVals = density(pts)

    # Calculate basic statistics
    meanPts <- mean(pts)
    sdPts <- sd(pts)
    arrowLen <- 0.1
    sdLineHeight <- 0.1 * max(densityVals$y)

    # Start density plot
    plot(densityVals, main = "Population distribution")
    abline(v = meanPts, col = "firebrick1")

    # Add standard deviation boundaries
    arrows(
        x0 = meanPts,
        y0 = sdLineHeight,
        x1 = meanPts + sdPts,
        y1 = sdLineHeight,
        length = arrowLen,
        col = "firebrick1"
    )
    arrows(
        x0 = meanPts + sdPts,
        y0 = sdLineHeight,
        x1 = meanPts,
        y1 = sdLineHeight,
        length = arrowLen,
        col = "firebrick1"
    )

    # Add rectangle to bound one standard deviation on both sides
    rect(
        xleft = meanPts - sdPts,
        ybottom = 0,
        xright = meanPts + sdPts,
        ytop = max(densityVals$y),
        col = "rosybrown1",
        density = 10
    )

    # Add parameter annotations
    text(
        # Population mean
        x = meanPts - (sdPts / 3),
        y = max(densityVals$y) / 2,
        labels = expression(mu),
        cex = 1.5
    )
    text(
        # Population standard deviation
        x = meanPts + (sdPts / 2),
        y = 1.65 * sdLineHeight,
        labels = expression(sigma),
        cex = 1.5
    )

    # Print out mean and standard deviation
    cat("Mean: ", round(meanPts, 3), "\n")
    cat("Std:  ", round(sdPts, 3))
}

#' Generate Location Plot
#'
#' Creates three density plots with varying means. The means are highlighted
#' with a vertical line.
#'
#' @param mu mean
#' @param std standard deviation
#' @param n sample size
#' @param xmin x-axis minimum limit
#' @param xmax x-axis maximum limit
#'
#' @return Creates three density plots with different center locations
#' @export
#'
#' @references Krzywinski, Martin, and Naomi Altman. "Points of significance:
#'   Importance of being uncertain." Nature Methods 10.9 (2013): 809-810.
#'
#' @examples
#' location_plot()
location_plot <- function(mu = c(-5, 0, 5),
                          std = 1,
                          n = 100,
                          xmin = -10,
                          xmax = 10) {

    # Empty lists to store means and density values
    pts <- list()
    densityVals <- list()
    meanPts <- list()

    # Generate data
    for (i in seq(length(mu))) {
        pts[[i]] <- rnorm(n = n, mean = mu[i], sd = std)
        densityVals[[i]] <- density(pts[[i]])
        meanPts[[i]] <- mean(pts[[i]])
    }

    # Start density plot
    par(mfrow = c(3, 1))
    for (i in seq(length(mu))) {
        plot(densityVals[[i]], main = "", xlim = c(xmin, xmax))
        abline(v = meanPts[[i]], col = "firebrick1")
        # Population mean
        text(
            x = meanPts[[i]] - (sd(pts[[i]]) / 3),
            y = max(densityVals[[i]]$y) / 2,
            labels = expression(mu),
            cex = 1.5
        )
    }
    par(mfrow = c(1, 1))

    # Print out means of each plot
    for (i in seq(length(mu))) {
        cat("Mean: ", round(meanPts[[i]], 3), "\n")
    }
}