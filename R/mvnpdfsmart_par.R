
#' MVNPDFSMART Parallel
#' 
#' Version parallélisée de mvnpdfsmart.
#'
#' @param x a matrix, with n columns (the observations) and p rows.
#' @param mean a vector of means.
#' @param varcovM a variance-covariance matrix.
#' @param Log a logical parameter, with default value to TRUE.
#'
#' @returns Returns a list containing the matrix x, and a vector of length n of the multivariate normal distribution density values at those points (eventually in log-scale).
#' @export
#'
#' @importFrom mvtnorm dmvnorm
#'
#' @examples
#' mvnpdf(x = matrix(1.96), Log = FALSE)
#' dnorm(1.96)

mvnpdfsmart_par <- function(x, mean =  rep(0, nrow(x)),
                            varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))
  
  y <- future.apply::future_sapply(1:ncol(x), FUN = function(j) {
        - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
        0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]})
  
  if (!Log) {y <- exp(y)}
  
  res <- list(x = x, y = y)
  class(res) <- "mvnpdfsmart_par" # Important pour toutes les S3 méthodes (plot, etc.)
  return(res)
}

