#' @title Fish per Redd Expansion
#' @description Calculates fish per redd expansion estimate for fish upstream of a monitoring site. The function then uses the expansion to create an estimate of the same type of fish downstream of the monitoring site. Depending on the group of fish in the expansion calculation, the resulting downstream estimate may not include all groups of fish downstream of the site. To include all groups of fish, set x and n parameters to calculate the proportion of the initial group downstream, and then expand the downstream estimate by the proportion.
#' @param R redds upstream associated with the estimate of fish, or fish located in
#         similar spatial location as the redds
#' @param N total abundance of the fish group in similar spatial locations as redds
#' @param SE_N standard error of abundance estimate
#' @param R_D redds downstream
#' @param x number of fish downstream that were in the same group as N
#' @param n total number of fish downstream that need to be expanded
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals
#'
#' @author Ryan N. Kinzer
#'
#' @export
#'
#' @examples
#' est_abundance_fpr(100, 200, 5, alpha = .05)

est_abundance_fpr <- function(R, N, SE_N, R_D = NULL, x = NULL, n = NULL, alpha = 0.05){

  NperR <- N/R
  Var.NperR <- (1/R^2) * SE_N^2
  Std.error.NperR <- sqrt(Var.NperR)

  CI.lower.NperR <- NperR - qnorm(1-alpha/2)*Std.error.NperR
  CI.upper.NperR <- NperR + qnorm(1-alpha/2)*Std.error.NperR

  if(is.null(R_D)){
    output <- data.frame(NperR = NperR, SE_NperR = Std.error.NperR,
                         lwr_NperR = CI.lower.NperR,
                         upr_NperR = CI.upper.NperR)
  } # ends is null

  if(!is.null(R_D)){

      if(is.null(x)) {
          N_D <- R_D*NperR
          Var.N_D <- (R_D/R)^2 * (SE_N)^2
          Std.error.N_D <- sqrt(Var.N_D)
          CI.lower.N_D <- N_D - qnorm(1-alpha/2)*Std.error.N_D
          CI.upper.N_D <- N_D + qnorm(1-alpha/2)*Std.error.N_D
        } # ends fish method

       if(!is.null(x)){

          N_D.m <- R_D*NperR
          Var.N_D.m <- (R_D/R)^2 * (SE_N)^2
          Std.error.N_D.m <- sqrt(Var.N_D.m)

          p_df <- est_proportion(x,n)
          N_D <- N_D.m / p_df$p

          Var.N_D <- ((1/(p_df$p)^2)*Var.N_D.m) + ((N_D.m^2/p_df$p^4)*p_df$SE^2)
          Std.error.N_D <- sqrt(Var.N_D)
          CI.lower.N_D <- N_D - qnorm(1-alpha/2)*Std.error.N_D
          CI.upper.N_D <- N_D + qnorm(1-alpha/2)*Std.error.N_D
      } # ends female or adult method

      output <- data.frame(NperR = NperR, SE_NperR = Std.error.NperR,
                           lwr_NperR = CI.lower.NperR,
                           upr_NperR = CI.upper.NperR,
                 N_D = N_D,
                 SE_N_D = Std.error.N_D,
                 lwr_N_D = CI.lower.N_D,
                 upr_N_D = CI.upper.N_D)


  } # ends !is.null


  return(output)

} # ends function
