#------------------------------------------------------------------------------
# Redd Expansion - Chasco et al. 2014
#------------------------------------------------------------------------------
# Calculates escapment by expanding redds with female proportion and
# pre-spawn mortality estimates.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 5/27/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param R
#' @param n_f
#' @param n_s
#' @param n_p
#' @param n_fp
#' @param CI.type
#' @param alpha
#' @param iterations
#' @param print
#'
#' @return
#' @export
#'
#' @examples
redd_expansion <- function(R, n_f, n_s, n_p, n_fp,
                           CI.type="delta", alpha = 0.05, iterations=1000, print=TRUE){

  # Needs estimate_proportion function loaded to run!!!!!!!
  # R = redds to expand to fish, should be located in similar spatial location
  # n_f = number of females, used to estimate female proportion
  # n_s = known sex individuals, used to estimate female proportion
  # n_p = number of female prespawns, used to estimate pre-spawn mortality
  # n_fp = number of females with known spawn status,
  #        used to estimate pre-spawn mortality
  # CI.type = delta - approximate variance calculated with delta method
  #               and then uses the normal approx. and asymptotic ci's
  #           bootstrap - bootstrap ci's
  # alpha = precision level; e.g. 0.1, 0.05,... - DEFAULT = 0.05
  # interations = needed for boot CI_type
  # print = automatically prints result to R console

  # function to estimate female and prespawn proportions
  female = estimate_proportion(n_f,n_s,print=FALSE)
  prespawn = estimate_proportion(n_p,n_fp,print=FALSE)

  # Estimate spawner abundance
  S.hat <- R / female$phat


  # Estimate escapement
  N.hat <- R / (female$phat *(1-prespawn$phat))

  if(CI.type == "delta"){

    Var.S <- (R / female$phat^2)^2 * female$Std.error^2
    Std.error.S <- sqrt(Var.S)

    CI.lower.S <- S.hat - qnorm(1-alpha/2)*Std.error.S
    CI.upper.S <- S.hat + qnorm(1-alpha/2)*Std.error.S

    Var.N <- ((1/(1-prespawn$phat)^2)*Var.S) +
      ((S.hat^2/(1-prespawn$phat)^4)*prespawn$Std.error^2)
    Std.error.N <- sqrt(Var.N)

    CI.lower.N <- N.hat - qnorm(1-alpha/2)*Std.error.N
    CI.upper.N <- N.hat + qnorm(1-alpha/2)*Std.error.N

  } # if normal

  if(CI.type == "bootstrap"){
    N.boot <- NULL
    S.boot <- NULL

    for(i in 1:iterations){
        f <- rbinom(1,n_s,(n_f/n_s))
        p <- rbinom(1,n_fp,(n_p/n_fp))

        f.boot <- f / n_s
        p.boot <- p / n_fp

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

  Escapement <- c(N.hat,Std.error.N,CI.lower.N,CI.upper.N)
  Spawners <- c(S.hat,Std.error.S,CI.lower.S,CI.upper.S)

  df <- as.data.frame(rbind(Escapement,Spawners))
  names(df) <- c("Estimate","Std.error","CI.lower","CI.upper")

  if(print){
    cat("Confidence intervals are", (1-alpha)*100,"% calculated using",CI.type,"method. \n")
    print(round(df,3))
        }

  output <- list(N_hat = N.hat,
                 SE_N_hat = Std.error.N,
                 estimates = df,
                 female.p = female$phat, prespawn.p = prespawn$phat,
                 CI.type = CI.type, alpha = alpha)

  return(output)
}
