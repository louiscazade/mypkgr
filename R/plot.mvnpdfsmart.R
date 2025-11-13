#' Plot of the mvnpdfsmart function
#'
#' @param x an object of class \code{mvnpdfsmart} resulting from a call of the
#' \code{mnvpdf()} function.
#' @param ... graphical parameters passed to \code{plot()} function.
#'
#' @return Nothing is returned, only a plot is given.
#' @export
#'
#' @examples
#' pdfvalues <- mvnpdf(x=matrix(seq(-3, 3, by = 0.1), nrow = 1), Log=FALSE)
#' plot(pdfvalues)
plot.mvnpdfsmart <- function(x, ...) {
  plot(x$x, x$y, type = "l", ...)
}