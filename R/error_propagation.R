#' @title Error Propagation
#'
#' @description Propagate uncertainty using variance properties for
#'   addition, subtraction, and multiplication and the first order Taylor
#'   expansion (i.e., delta method) for the division of two random variables.
#'
#' @param fx the function being applied; \code{c("addition", "subtraction",
#'   "product","division")}.
#'
#' @param type set both \code{x} and \code{y} as random variables (\code{type =
#'   "both_random"}) or define \code{y} as a constant (\code{type =
#'   "y_constant"}).
#'
#' @param x vector of random variables
#'
#' @param se.x standard error of \code{x}
#'
#' @param y vector of random variables
#'
#' @param se.y standard error of \code{y}
#'
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @export
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' x <- rnorm(10, 0, 1)
#' y <- rnorm(10, 5, 10)
#' se.x <- rep(2,10)
#' se.y <- rep(10,10)
#' error_propagation("division", "both_random", x, se.x, y, se.y)

error_propagation <- function(fx = c("addition", "subtraction", "product","division"), type= c("both_random", "y_constant"), x, se.x, y, se.y = NULL, alpha = 0.05){

  fx <- match.arg(fx)
  type <- match.arg(type)

  if(fx == "product"){
      z <- x*y
    if(type == "both_random"){
        z.se <- sqrt((x^2*se.y^2) + (y^2*se.x^2) + (se.x^2*se.y^2)) }
    if(type == "y_constant"){
        z.se <- sqrt(y^2*se.x^2)}
  }

  if(fx == "division"){
      z <- x/y
    if(type == "both_random"){
      z.se <- sqrt(z^2*((se.y^2/y^2)+(se.x^2/x^2)))}
    if(type == "y_constant"){
      z.se <- sqrt((1/y)^2*se.x^2)}
  }

  if(fx == "addition"){
    z <- x + y
      if(type == "both_random"){
        z.se <- sqrt(se.x^2 + se.y^2)
      }
      if(type == "y_constant"){
        z.se <- se.x
      }
  }

  if(fx == "subtraction"){
      z <- x - y
      if(type == "both_random"){
        z.se <- sqrt(se.x^2 + se.y^2)}
      if(type == "y_constant"){
          z.se <- se.x}
}

  # CI.lower <- z - qnorm(1-alpha/2)*z.se
  # CI.upper <- z + qnorm(1-alpha/2)*z.se
  #
  # output <- list(z = z, z.se = z.se,
  #                CI.lower = CI.lower, CI.upper = CI.upper)

  return(z.se)
}
