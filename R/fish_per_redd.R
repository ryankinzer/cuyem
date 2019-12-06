#------------------------------------------------------------------------------
# Fish per Redd
#------------------------------------------------------------------------------
# Calculates fish per redd estimate upstream of adult monitoring site, or a
# female, adult per redd estimate.  Then, using the per redd estimate it
# expands redds downstream; fish (female, adult) per redd downstream
# abundance method.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 5/31/2016
#------------------------------------------------------------------------------

fish_per_redd <- function(R, N, SE.N, R_D = NULL, x =  NULL, n = NULL,
                          method="fish", alpha = 0.05, print=TRUE){


  # R = redds associated with estimate of fish, should be located in
  #         similar spatial location
  # N = total abundance of method group in similar spatial locations as redds
  # SE.N = standard error of abundance estimate
  # R_D = redds downstream of adult monitoring site
  # x = sample size of fish used for expansion (method group); used to
  #         calculate the proportion of the population this group represents
  # n = total sample size of x group + other groups
  # method = "fish", "female", "adult"
  #           fish - fish per redd expansion
  #           female - female per redd expansion method
  #           adult - adult per redd expansion method
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05

  NperR <- N/R
  Var.NperR <- (1/R^2) * SE.N^2
  Std.error.NperR <- sqrt(Var.NperR)

  CI.lower.NperR <- NperR - qnorm(1-alpha/2)*Std.error.NperR
  CI.upper.NperR <- NperR + qnorm(1-alpha/2)*Std.error.NperR


  if(is.null(R_D)){
  output <- list(Fish_per_Redd = NperR, Std.error.NperR = Std.error.NperR,
                 CI.lower.NperR = CI.lower.NperR,
                 CI.upper.NperR = CI.upper.NperR,
                 method = method, alpha = alpha)
  } # ends is null

  if(!is.null(R_D)){

      if(method=="fish") {
          N_D <- R_D*NperR
          Var.N_D <- (R_D/R)^2 * (SE.N)^2
          Std.error.N_D <- sqrt(Var.N_D)
          CI.lower.N_D <- N_D - qnorm(1-alpha/2)*Std.error.N_D
          CI.upper.N_D <- N_D + qnorm(1-alpha/2)*Std.error.N_D
        } # ends fish method

      if(method=="female" | method== "adult"){

          N_D.m <- R_D*NperR
          Var.N_D.m <- (R_D/R)^2 * (SE.N)^2
          Std.error.N_D.m <- sqrt(Var.N_D.m)

          p <- estimate_proportion(x,n,print=FALSE)
          N_D <- N_D.m / p$phat

          Var.N_D <- ((1/(p$phat)^2)*Var.N_D.m) + ((N_D.m^2/p$phat^4)*p$Std.error^2)
          Std.error.N_D <- sqrt(Var.N_D)
          CI.lower.N_D <- N_D - qnorm(1-alpha/2)*Std.error.N_D
          CI.upper.N_D <- N_D + qnorm(1-alpha/2)*Std.error.N_D
      } # ends female or adult method

      output <- list(Fish_per_Redd = NperR, Std.error.NperR = Std.error.NperR,
                 CI.lower.NperR = CI.lower.NperR,
                 CI.upper.NperR = CI.upper.NperR,
                 N_D = N_D, Std.error.N_D = Std.error.N_D,
                 CI.lower.N_D = CI.lower.N_D,
                 CI.upper.N_D = CI.upper.N_D,
                 method = method, alpha = alpha)

  } # ends !is.null

  if(print){
    cat("Confidence intervals are", (1-alpha)*100,"% \n",
        method,"per redd:", round(NperR,3), "\n",
        "Std. Error:", round(Std.error.NperR,3),"\n",
        "CI.lower:",round(CI.lower.NperR,3),"\n",
        "CI.upper:",round(CI.upper.NperR,3),"\n")
  }

  return(output)

  # print = automatically prints result to R console

} # ends function
