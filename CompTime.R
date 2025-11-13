
# Exemple de profiling
n <- 10e4
profvis::profvis(pdfval <- mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE))

# Comparaison des deux versions de mvtpdf
library(microbenchmark)
n <- 1000
mb <- microbenchmark(dmvnorm = mvtnorm::dmvnorm(matrix(1.96, nrow = n, ncol = 2)),
                     mvnpdf = mvnpdf(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     mvnpdfsmart = mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
                     times=100L)
boxplot(mb, ylab = "log(t)")
ggplot2::autoplot(mb) + ggplot2::ggtitle("Temps de calculs")

