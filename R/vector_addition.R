#------------------------------------------------------------------------------
# Error propagation
#------------------------------------------------------------------------------
# Multiplies or divides two variables together and calculates the standard
# error and confidence intervals.  Both variables can be random variables with an
# associated For example, estimates spawner abundance by first estimating
# pre-spawn mortality and then multiply with escapement.  The function also
# provides SE estimates and confidence intervals.

# Uses delta method for equations with random variable denominator.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 08/25/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param fx
#' @param type
#' @param X
#' @param SE.X
#' @param Y
#' @param SE.Y
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
vector_addition <- function(fx = "multiply",type= "both_random",X, SE.X, Y, SE.Y = NULL, alpha = 0.05){

  # X
  # SE.X
  # Y
  # SE.Y
  # alpha





  CI.lower <- z - qnorm(1-alpha/2)*z.se
  CI.upper <- z + qnorm(1-alpha/2)*z.se

  output <- list(z = z, z.se = z.se,
                 CI.lower = CI.lower, CI.upper = CI.upper)

  return(output)
}
