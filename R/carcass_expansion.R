#------------------------------------------------------------------------------
# Calculates escapment by expanding carcasses with a carcass recovery rate
# calculated from sampling upstream of adult monitoring site.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/01/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param n
#' @param g
#' @param SE.g
#' @param groups
#' @param alpha
#' @param print
#'
#' @return
#' @export
#'
#' @examples
carcass_expansion <- function(n, g, SE.g, groups=NULL, alpha = 0.05, print=TRUE){

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


  if(print){
    cat("Confidence intervals are", (1-alpha)*100,"% \n",
        "N.hat:", round(N_D,3), "\n",
        "Std. Error:", round(Std.error.N_D,3),"\n",
        "CI.lower:",round(CI.lower.N_D,3),"\n",
        "CI.upper:",round(CI.upper.N_D,3),"\n")
  }

    output = list(N.hat = N_D,Std.error = Std.error.N_D,
                  CI.lower = CI.lower.N_D, CI.upper = CI.upper.N_D,
                  Strata = df,
                  alpha=alpha)

  return(output)

  # print = automatically prints result to R console

} # ends function
