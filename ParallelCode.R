
library(microbenchmark)
library(future)
library(future.apply)


# Exemple 1
log_seq <- function(x) {return(log(x))}
log_par <- function(x) {res <- future_sapply(1:length(x), FUN = function(i) {log(x[i])})return(res)}
mb <- microbenchmark(log_par(1:100), log_seq(1:100), times = 50)
ggplot2::autoplot(mb)


# Exemple 2
x <- matrix(rnorm(1e6), nrow = 10)
dim(x)

colmedian_apply <- function(x){
  return(apply(X = x, MARGIN = 2, FUN = median))}
system.time(colmedian_apply(x))

colmedian_for <- function(x){
  p <- ncol(x)
  ans <- numeric(p)
  for (i in 1:p) {ans[i] <- median(x[, i]) }
  return(ans)}
system.time(colmedian_for(x))

colmedian_par <- function(x){
  res <- future_sapply(1:ncol(x), FUN = function(i) {median(x[, i])})
  return(res)}
plan(multisession(workers = 7))
system.time(colmedian_par(x))

mb <- microbenchmark(colmedian_apply(x), 
                     colmedian_for(x),
                     colmedian_par(x), 
                     times = 10)
ggplot2::autoplot(mb)


# Exemple 3
plan(multisession(workers = 3))
n <- 10000
mb <- microbenchmark::microbenchmark(
  mvnpdfsmart = mypkgr::mvnpdfsmart(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
  mvnpdfsmart_par = mypkgr::mvnpdfsmart_par(x=matrix(1.96, nrow = 2, ncol = n), Log=FALSE),
  times = 20)
ggplot2::autoplot(mb)


# Exemple 4 
# library(pbapply) -> ajoute une bar et semble même un peu meilleur
