sim_gene_expr <- function(n = 5, geneN = 100, d = 2) {
    geneExpress <- data.frame(gene = NULL, control = NULL,
                              treat = NULL, sample = NULL)
    for (i in 1:geneN) {
        control <- rnorm(n)
        treat <- rnorm(n, mean = d)

        geneExprVals <- cbind(control = control, treat = treat, sample = 1:n)
        experiment <- cbind(gene = i, geneExprVals)
        geneExpress <- rbind(geneExpress, experiment)
    }
}