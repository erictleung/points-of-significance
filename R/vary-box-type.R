vary_box_type <- function(n = 10000) {
    par(mfrow = c(2, 1))

    # Uniform distribution
    xuni <- runif(n)
    plot(density(xuni), main = "Uniform Distribution")
    boxplot(xuni, horizontal = TRUE)

    # Normal distribution
    xnor <- rnorm(n)
    plot(density(xnor), main = "Normal Distribution")
    boxplot(xnor, horizontal = TRUE)

    # Skew right
    xhyp <- rhyper(n, 300, 100, 100)
    plot(density(xhyp), main = "Hypergenometric Distribution")
    boxplot(xhyp, horizontal = TRUE)

    # Skew strong right
    xpoi <- rpois(n, 4)
    hist(xpoi, main = "Poisson Distribution")
    boxplot(xpoi, horizontal = TRUE)

    par(mfrow = c(1, 1))
}