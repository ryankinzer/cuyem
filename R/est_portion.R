#------------------------------------------------------------------------------
# Proportion Abundance using a proportion
#------------------------------------------------------------------------------
# Partions estimated abundance into a specific group by an estimated supplied
# proportion.  The companion function (should be combined) is
# "proportion_abundance".  This function does not require the raw data and
# uses the pre-supplied proportion estimate and std.error.  The function also
# provides SE estimates and confidence intervals for the resulting partioned
# abundance.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 08/17/2016
#------------------------------------------------------------------------------

#' @title Estimate the portion of a total.
#'
#' @description Portion a total number \(i.e. escapement or abundance\) into its parts using estimated proportions. \code{est_portion} will return the estimated portion and the associated precision of each estimate.  Currently the portions are calculated assuming independence of the proportions.  In the future this function should be calcuated using a multinomial distribtion.
#'
#' @param N total escapement or abundance
#' @param SE_N standard error of N
#' @param phat vector of probabilities
#' @param SE_phat vector of standard errors for probabilities
#' @param alpha  type I error rate.  Default is set at 0.05 to produce 95%
#'   confidence intervals.
#'
#' @export
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' Nhat = 1000
#' SE_N = 50
#' phat = c(.2,.5,.3)
#' SE_phat = c(.01,.03,.01)
#' est_portion(N, SE_N, phat, SE_phat)
#'
est_portion <- function(N, SE_N, phat, SE_phat, alpha=.05){

  {if(sum(phat) > 1) stop("the sum of phat must be less than or equal 1");
   if(length(phat) != length(SE_phat)) stop("phat and SE_phat must be same length");
   if(any(phat < 0 || phat > 1)) stop("phat must be a probability");
   if(alpha < 0 || alpha > 1) stop("alpha must be greater than 0 and less than 1")}

  N.hat.i <- N*phat
  SE.N.hat.i <- sqrt((N^2*SE_phat^2) + (phat^2*(SE_N^2)) + (SE_phat^2 * SE_N^2))
  CI.lower.N <- N.hat.i - qnorm(1-alpha/2)*SE.N.hat.i
  CI.upper.N <- N.hat.i + qnorm(1-alpha/2)*SE.N.hat.i

  output <- data.frame(N_hat = N.hat.i,
                 SE_N_hat = SE.N.hat.i,
                 lwr = CI.lower.N,
                 upr = CI.upper.N)

   return(output)

} # end function
