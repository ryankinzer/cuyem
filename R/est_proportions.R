#' @title Estimate Multiple Proportions
#'
#' @description Estimate multiple proportions with uncertainty assuming a
#' binomial distribution. In the future this function will likely be changed to
#' use a multinomial distribution.
#'
#' @param x a vector of quantities for each estimated group
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @return
#' @export
#'
#' @examples
#' est_proportions(x = c(10,20,70))
est_proportions <- function(x, alpha=.05){

    output <- est_proportion(x,rep(sum(x),length(x), alpha = alpha))

    return(output)

  }
