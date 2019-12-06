#------------------------------------------------------------------------------
# Proportion Abundance
#------------------------------------------------------------------------------
# Partions estimated abundance into a specific group by an estimated
# proportion.  For example, estimates spawner abundance by first estimating
# pre-spawn mortality and then multiply with escapement.  The function also
# provides SE estimates and confidence intervals.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 08/17/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param N
#' @param SE.N
#' @param x
#' @param n
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
proportion_abundance <- function(N,SE.N,x,n, alpha=.05){

  # N = total escapement or abundance
  # SE.N = standard error of N
  # x = number of successes
  # n = number of trials
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  phat <- ifelse(n == 0, 1, estimate_proportion(x,n,print=FALSE)$phat)
  SE.phat <- ifelse(n == 0, 0, estimate_proportion(x,n,print=FALSE)$Std.error)


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
