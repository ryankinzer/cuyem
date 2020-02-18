#' @title Redd Expansion Using Females and Prespawn Mortality
#'
#' @description Estimate abundance of fish using the Chasco et al. (2014) redd
#'  expansion. The calculation expands redds using female and prespawn
#'   mortality proportions.

#' @param R total redd number
#' @param p_F female proportion
#' @param SE_F standard error of female proportion
#' @param p_Psp prespawn mortality proportion
#' @param SE_Psp standard error of prespawne mortality
#' @param CI_type either a delta or bootstrap method for precision estimates
#' @param iterations bootstrap iterations
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals
#'
#' @author Ryan N. Kinzer
#'
#' @export
#'
#' @examples
#' est_abundance_sgs(100, .5, .02, .1, .01, alpha = .05)

est_abundance_sgs <- function(R, p_F, SE_F, p_Psp, SE_Psp,
                           CI_type= c("delta", "boot"), alpha = 0.05, iterations=1000){

  CI_type <- match.arg(CI_type)
  p_F = data.frame(p_F, SE_F)
  p_Psp = data.frame(p_Psp, SE_Psp)

  # Estimate spawner abundance
  S.hat <- R / p_F[,1]

  # Estimate escapement
  N.hat <- R / (p_F[,1] *(1-p_Psp[,1]))

  if(CI_type == "delta"){

    Var.S <- (R / p_F[,1]^2)^2 * p_F[,2]^2
    Std.error.S <- sqrt(Var.S)

    CI.lower.S <- S.hat - qnorm(1-alpha/2)*Std.error.S
    CI.upper.S <- S.hat + qnorm(1-alpha/2)*Std.error.S

    Var.N <- ((1/(1-p_Psp[,1])^2)*Var.S) +
      ((S.hat^2/(1-p_Psp[,1])^4)*p_Psp[,2]^2)
    Std.error.N <- sqrt(Var.N)

    CI.lower.N <- N.hat - qnorm(1-alpha/2)*Std.error.N
    CI.upper.N <- N.hat + qnorm(1-alpha/2)*Std.error.N

  }

  if(CI_type == "bootstrap"){
    N.boot <- NULL
    S.boot <- NULL

    for(i in 1:iterations){
        f <- rbinom(1,(females+males),p_F[,1])
        p <- rbinom(1,(prespawned+spawned),p_Psp[,1])

        f.boot <- f / (females+males)
        p.boot <- p / (prespawned+spawned)

        N.boot[i] <- R / (f.boot * (1 - p.boot))
        S.boot[i] <- R / f.boot
      } # i loop

    ci.N <- quantile(N.boot,c(alpha/2,1-alpha/2))
    Std.error.N <- sd(N.boot)
    CI.lower.N <- ci.N[1]
    CI.upper.N <- ci.N[2]

    ci.S <- quantile(S.boot,c(alpha/2,1-alpha/2))
    Std.error.S <- sd(S.boot)
    CI.lower.S <- ci.S[1]
    CI.upper.S <- ci.S[2]

  } # if boot

  output <- data.frame(
                 Nhat = N.hat,
                 SE_Nhat = Std.error.N,
                 lwr_N = CI.lower.N,
                 upr_N = CI.upper.N,
                 Shat = S.hat,
                 SE_Shat = Std.error.S,
                 lwr_S = CI.lower.S,
                 upr_S = CI.upper.S
                 )

  return(output)
}
