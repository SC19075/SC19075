# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title Random walk Metropolis using Rcpp
#' @description Implement the random walk version of the Metropolis using Rcpp
#' @param sigma the variance
#' @param x0 the inial location
#' @param m the total steps
#' @return a list conclude x and k \code{n}
#' @export
rw_MetropolisL <- function(sigma, x0, m) {
    .Call('_SC19075_rw_MetropolisL', PACKAGE = 'SC19075', sigma, x0, m)
}

