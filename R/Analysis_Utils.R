#' int_f
#' 
#' Used to determine the difference of two normally distributed sets based on their count, mean and SD.
#' @param x
#' @param mu1
#' @param mu2
#' @param sd1
#' @param sd2
#' @keywords xsample, mirror, MCMC, Monte Carlo, Markov Chain
#' @examples in_f()
#' 
int_f <- function(x, mu1, mu2, sd1, sd2) {
    f1 <- dnorm(x, mean=mu1, sd=sd1)
    f2 <- dnorm(x, mean=mu2, sd=sd2)
    pmin(f1, f2)
}