#' @title Estimate a proporiton and associated uncertainty.
#'
#' @description \code{estimate_proportion} estimates a proporiton using the
#' maximum likelihood estimator.  Uncertainty is estimated by either the
#' Wald or Score methods.
#'
#' @param x,n An integer value of the count of successes (\code{x}) and trials (\code{n}).
#'
#' @param method 'wald', 'exact', 'score'. The default method is 'score'.
#'
#' @param alpha Type I error rate.  Default is set at 0.05 to produce 95%
#'   confidence intervals.
#'
#' @export
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' estimate_proportion(x =  10, n = 100, method = 'score', alpha = 0.05)
#'

est_proportion <- function(x, n, method=c("score", "wald"), alpha=.05){
#------------------------------------------------------------------------------
# Estimate Proportions
#------------------------------------------------------------------------------
# Calculates an estimated proportion using x = successes and n = trials.  The
# estimate is based on the maximum likelihood estimate of p using a binomial
# distribution.  Standard errors are provided using the variance esimate, and
# confidence intervals are provided with three methods; 1) normal approximation
# and asymptotic Wald's, 2) exact binomial ci's; Clopper-Pearson, and 3) score
# ci's developed by Wilson.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 5/27/2016
#------------------------------------------------------------------------------

  # Agresti 2002. Categorical Data Analysis. page 14
  # x = number of successes or sample size of targeted group; females
  # n = number of trials or total sample size; males + females
  # phat = MLE of proportion, proportion of female
  # method =   wald - normal approximation, asymptotic CI's, Wald's ?
  #             exact - binomial ci's, Clopper and Pearson 1934
  #             score - score ci's, addes observations, Wilson 1927 - DEFAULT
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  method = match.arg(method)

  {if(length(x) != length(n)) stop("x and n must be same length");
   if(any(abs(round(x)) != x) || any(abs(round(n)) != n)) stop("x and n must be positive whole numbers");
   #if(any(x < 0)) stop("x must be equal to or greater than 0");
   #f(any(n <= 0)) stop("n must be greater than 0");
   if(any(n < x)) stop("x must be less than or equal to n");
   if(alpha < 0 || alpha > 1) stop("alpha must be greater than 0 and less than 1")}

  p <- x / n
  Std.error <- sqrt((p*(1-p))/n)

  if(method == "wald"){
      CI.lower <- p - qnorm(1-alpha/2)*Std.error
      CI.upper <- p + qnorm(1-alpha/2)*Std.error
  }

  # if(method == "exact"){
  #
  #   if(x == 0){
  #     CI.lower <- 0
  #     CI.upper <- 1-((alpha/2)^(1/n))}
  #
  #   if(x == n){
  #     CI.lower <- ((alpha/2)^(1/n))
  #     CI.upper <- 1} else
  #
  #   CI.lower <- qbeta(alpha/2,x,n-x+1)
  #   CI.upper <- qbeta(1-alpha/2,x+1,n-x)
  #   }

  if(method == "score"){

    z <- qnorm(1-alpha/2)
    CI.lower <- ((x + z^2/2)/(n + z^2)) - (z/(n + z^2))*sqrt((p*(1-p)*n)+(z^2/4))
    CI.upper <- ((x + z^2/2)/(n + z^2)) + (z/(n + z^2))*sqrt((p*(1-p)*n)+(z^2/4))
  }

  output <- data.frame(p = p, SE = Std.error, lwr = CI.lower, upr = CI.upper)

  return(output)

} # close function

