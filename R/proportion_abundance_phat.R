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

#' Title
#'
#' @param N
#' @param SE.N
#' @param phat
#' @param SE.phat
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
proportion_abundance_phat <- function(N,SE.N,phat,SE.phat, alpha=.05){

  # N = total escapement or abundance
  # SE.N = standard error of N
  # phat = estimated proportion
  # SE.phat = standard error of phat
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  N.hat.i <- N*phat
  SE.N.hat.i <- sqrt((N^2*SE.phat^2) + (phat^2*(SE.N^2)) + (SE.phat^2 * SE.N^2))
  CI.lower.N <- N.hat.i - qnorm(1-alpha/2)*SE.N.hat.i
  CI.upper.N <- N.hat.i + qnorm(1-alpha/2)*SE.N.hat.i

  output <- list(N_hat_i = N.hat.i,
                 SE_N_hat_i = SE.N.hat.i,
                 CI_lower_N = CI.lower.N,
                 CI_upper_N = CI.upper.N,
                 alpha = alpha)

   return(output)

} # end function
