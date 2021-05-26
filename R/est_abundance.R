#' @title Estimate Abundance
#'
#' @description Estimate abundance using the adjusted Peterson formula for a closed
#'  population (Chapman 1951).
#'
#' @param n1 a vector of the number of fish marked and released at the first capture occasion; M
#' @param n2 a vector of the number of fish captured at the second occasion; C
#' @param m2 a vector of the number of fish captured at the second occasion with a mark; R
#' @param method M/R estimate type; "adjusted peterson',
#' @param CItype c('normal','binomial', 'poisson', 'hypergeometric')
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @export
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' adjPeterson(n1 = c(100, 1000), n2 = c(50, 500), m2 = c(25, 250))

est_abundance <- function(n1, n2, m2, method = c('adjusted Peterson'), CItype = c('normal','binomial', 'Poisson', 'hypergeometric'), alpha = 0.05){

  method = match.arg(method)
  CItype = match.arg(CItype)

  #{if(any(n1 < m2))warning('at least one set of recaptures at time 2 is greater than marks released at time 1')}

  #https://www.zoology.ubc.ca/~krebs/downloads/krebs_chapter_02_2017.pdf


  if(method == 'adjusted Peterson') {
    N = (((n1 + 1) * (n2 + 1))/(m2 + 1)) -1
    V = ((n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2))/((m2 + 1)^2 * (m2 + 2))
    SE = sqrt(V)
    #l = N - qnorm(1-alpha/2)*SE
    #u = N + qnorm(1-alpha/2)*SE
  }

  if(CItype == 'normal'){
    l = N - qnorm(1-alpha/2)*SE
    u = N + qnorm(1-alpha/2)*SE
  }

  if(CItype == 'Poisson'){
    l_r <- qpois(1-alpha/2, m2)
    u_r <- qpois(alpha/2, m2)

    l = (((n1 + 1) * (n2 + 1))/(l_r + 1)) -1
    u = (((n1 + 1) * (n2 + 1))/(u_r + 1)) -1
  }

  if(CItype == 'binomial'){
    l_r <- qbinom(1-alpha/2, n2, m2/n2)
    l_eff <- l_r/n2

    u_r <- qbinom(alpha/2, n2, m2/n2)
    u_eff <- u_r/n2

    l = n1/l_eff
    u = n1/u_eff
  }

  if(CItype == 'hypergeometric'){
    l_r <- qhyper(alpha/2, m = n1, n = N - n1, k = n2)
    l_eff <- l_r/n2

    u_r <- qhyper(1-alpha/2, m = n1, n = N - n1, k = n2)
    u_eff <- u_r/n2

    l = n1/l_eff
    u = n1/u_eff
  }

  data.frame(N = N, SE = SE, lwr = l, upr = u)
  #rbind(setNames(c(N,SE,l,u), c('N_hat', 'SE', 'lwr', 'upr')))
}
