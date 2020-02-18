#------------------------------------------------------------------------------
# Calculates escapment by expanding carcasses with a carcass recovery rate
# calculated from sampling upstream of adult monitoring site.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/01/2016
#------------------------------------------------------------------------------

#' @title Carcass Expansion Using Recovery Rate
#'
#' @param n number of carcasses collected
#' @param g recovery rate of carcasses calculated from marks released at weir and recaptured carcasses collected
#' @param SE.g standard error of recovery rate
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals
#'
#' @author Ryan N. Kinzer
#' @export
#'
#' @examples
#' est_abundance_carcass(100, .5, .05)
est_abundance_carcass <- function(n, g, SE.g, groups=NULL, alpha = 0.05, print=TRUE){

  # n = vector of carcasses in each group l to be estimated
  # g = vector of carcass recovery probabilities for fish in each group l,
          # obtained from stratified upstream_abundance() estimates
  # SE.g = standard error of recovery rate probabilities
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  if(!is.null(groups)){Strata <- groups} else
    {Strata <- 1:length(n)}

    N_D.l <- n/g
    Var.N_D.l <- (n/g)^2 * SE.g^2
    Std.error.N_D.l <- sqrt(Var.N_D.l)
    CI.lower.N_D.l <- N_D.l - (1-alpha/2)*Std.error.N_D.l
    CI.upper.N_D.l <- N_D.l + (1-alpha/2)*Std.error.N_D.l

  df <- data.frame(Strata = Strata, n = n, g = g, N.hat = N_D.l,
                  Std.error = Std.error.N_D.l,
                  CI.lower = CI.lower.N_D.l, CI.upper = CI.upper.N_D.l)

    N_D <- sum(N_D.l)
    Std.error.N_D <- sqrt(sum(Var.N_D.l))
    CI.lower.N_D <- N_D - (1-alpha/2)*Std.error.N_D
    CI.upper.N_D <- N_D + (1-alpha/2)*Std.error.N_D


    output = data.frame(N.hat = N_D,
                  Std.error = Std.error.N_D,
                  CI.lower = CI.lower.N_D,
                  CI.upper = CI.upper.N_D,
                  Strata = df,
                  alpha=alpha)

  return(output)

  # print = automatically prints result to R console

} # ends function
