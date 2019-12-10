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
error_propagation <- function(fx = "multiply",type= "both_random",X, SE.X, Y, SE.Y = NULL, alpha = 0.05){

  if(fx == "multiply"){
      z <- X*Y
    if(type == "both_random"){
        z.se <- sqrt((X^2*SE.Y^2) + (Y^2*SE.X^2) + (SE.X^2*SE.Y^2)) }
    if(type == "Y.constant"){
        z.se <- sqrt(Y^2*SE.X^2)}
  }

  if(fx == "divide_Y"){
      z <- X/Y
    if(type == "both_random"){
      z.se <- sqrt(z^2*((SE.Y^2/Y^2)+(SE.X^2/X^2)))}
    if(type == "Y.constant"){
      z.se <- sqrt((1/Y)^2*SE.X^2)}
  }

  if(fx == "addition"){
    z <- X + Y
      if(type == "both_random"){
        z.se <- sqrt(SE.X^2 + SE.Y^2)
      }
      if(type == "Y.constant"){
        z.se <- SE.X
      }
  }

  if(fx == "subtraction"){
      z <- X - Y
      if(type == "both_random"){
        z.se <- sqrt(SE.X^2 + SE.Y^2)}
      if(type == "Y.constant"){
          z.se <- SE.X}
}

  # CI.lower <- z - qnorm(1-alpha/2)*z.se
  # CI.upper <- z + qnorm(1-alpha/2)*z.se
  #
  # output <- list(z = z, z.se = z.se,
  #                CI.lower = CI.lower, CI.upper = CI.upper)

  return(z.se)
}
