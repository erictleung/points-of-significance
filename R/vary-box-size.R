vary_box_size <- function(n = 5) {
    # Number of different plots
    num <- 5

    # Make empty data frame
    mat <- data.frame(data = NULL, plot = NULL)

    for (i in 1:num) {
        vals <- rnorm(n)
        temp <- cbind(vals, plot = i)
        mat <- rbind(mat, temp)
    }

    boxplot(vals ~ plot, data = mat, col = "lightgray")
}