#' @title Estimate Abundance
#'
#' @description Estimate abundance using the adjusted Peterson formula for a closed
#'  population (Chapman 1951).
#'
#' @param n1 a vector of the number of fish marked and released at the first capture occasion; M
#' @param n2 a vector of the number of fish captured at the second occasion; C
#' @param m2 a vector of the number of fish captured at the second occasion with a mark; R
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @export
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' adjPeterson(n1 = c(100, 1000), n2 = c(50, 500), m2 = c(25, 250))

est_abundance <- function(n1, n2, m2, method = c('adjusted Peterson'), alpha = 0.05){

  method = match.arg(method)

  #{if(any(n1 < m2))warning('at least one set of recaptures at time 2 is greater than marks released at time 1')}

  if(method == 'adjusted Peterson') {
    N = (((n1 + 1) * (n2 + 1))/(m2 + 1)) -1
    V = ((n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2))/((m2 + 1)^2 * (m2 + 2))
    SE = sqrt(V)
    l = N - qnorm(1-alpha/2)*SE
    u = N + qnorm(1-alpha/2)*SE
  }

  data.frame(N = N, SE = SE, lwr = l, upr = u)
  #rbind(setNames(c(N,SE,l,u), c('N_hat', 'SE', 'lwr', 'upr')))
}
