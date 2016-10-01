plot_three_errors <- function(n = 10) {
    # Parameters for plots
    std <- 0.5
    sem_std <- std * sqrt(n) # Standard deviation to achieve stdev == 0.5
    ci_std <- sem_std / 1.96 # Standard deviation to achieve conf int == 0.5

    # Standard deviation error

    ## Simulate data
    mean0_sd05_vals <- rnorm(n, mean = 0, std)
    mean1_sd05_vals <- rnorm(n, mean = 1, std)
    mean0_df <- data.frame(mean = "zero", vals = mean0_sd05_vals)
    mean1_df <- data.frame(mean = "one", vals = mean1_sd05_vals)
    all_vals <- rbind(mean0_df, mean1_df)

    ## Plot boxplot
    boxplot(vals ~ mean, data = all_vals, horizontal = TRUE)

    ## Calculate means and standard deviations
    mn.t <- tapply(all_vals$vals, all_vals$mean, mean)
    sd.t <- tapply(all_vals$vals, all_vals$mean, sd)

    ## Overlay means and one standard deviation error
    yi <- 0.3 + seq(2)
    points(mn.t, yi, col = "orange", pch = 18)
    arrows(mn.t - sd.t, yi, mn.t + sd.t, yi,
           code = 3, col = "pink", angle = 75, length = 0.1)

    ## Output two sample t-test for two samples
    t.test(mean0_sd05_vals, mean1_sd05_vals, alternative = "two.sided")

    cat("Press [Enter] to continue")
    line <- readline()

    # Standard error of the mean (s.e.m.)


    # 95% confidence interval (C.I.)
}