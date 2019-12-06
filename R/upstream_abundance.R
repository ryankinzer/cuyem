#------------------------------------------------------------------------------
# Lincoln-Peterson Mark-Recapture Estimate (Chapman 1951)
#------------------------------------------------------------------------------
# Calculates a mark recapture estimate for use above a picket weir.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 5/5/2016
#------------------------------------------------------------------------------
#' @title Estimate adults abundance upstream of a weir using mark-recapture data.
#'
#' @description \code{upstream_abundance} generates stratified adult abundance
#'   estimates from mark-recapture data collected at Chinook salmon and steelhead
#'   pickett weirs or during Chinook salmon spawning ground surveys. The function
#'   using the adjusted Lincoln-Peterson estimator to account for small sample
#'   bias.
#'
#' #@section Warning:
#'
#' @param C a vector of captures at the second capture occasion
#'
#' @param M a vector of marks applied at the first capture occasion
#'
#' @param R a vector of recaptures as the second capture occasion
#'
#' @param groups optional character vector of strata group names
#'
#' @param alpha Type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @param CI.type Confidence intervals can either be reported as asymptotic normal or bootstrapped intervals.
#'
#' @param iter The number of bootstrap iterations to be run.  Default is set at
#'   1000.
#' @param print Logical value. Default value is TRUE. Should results be printed to the console?
#'
#' @author Ryan N. Kinzer
#'
#' @returnA list of point and uncertainty estimates.
#'
#' @export
#'
#' @examples
#'
#' C <- c(100,200,300)
#' M <- c(50,100,150)
#' R <-  c(10,20,30)
#' s_name <- c('small', 'medium', 'large')
#' tmp <- upstream_abundanc(C,M,R,groups = s_name, CI.type = 'normal')
#'
upstream_abundance <- function(C,M,R,groups=NULL,alpha=0.05,CI.type=c("normal","boot"),iter=1000,print=TRUE){

  # need data
  stopifnot(!is.null(C))
  stopifnot(!is.null(M))
  stopifnot(!is.null(R))

  # pull out default CI.type
  CI.type = match.arg(CI.type)

  # C = a vector of captures at second capture occasion
  # M = a vector of marks applied at first capture occasion
  # R = a vector of recaptures as second capture occasion
  # alpha = precision level; e.g. 0.1, 0.05,...
  # CI_type = confidence interval type to calculate (only accept normal, boot)
      # normal = normal approximation with asymptotic ci's
      # #hypergeometric = exact hypergeometric confidence interval - not incorporated yet
      # #binomial = binomial approximation - not incorporated yet
      # #poisson = poisson approximation  - not incorporated yet
      # boot = bootstrapped confidence intervals, captures are binomial dist
                # and recaptures are from hypergeometric
  # interations = needed for boot CI_type
  # print = automatically prints result to R console


  df = data.frame(matrix(NA,length(C),9, dimnames=list(c(),c("Strata", "Capture",
           "Mark","Recapture","Recovery.Rate","N.hat","Std.error","CI.lower","CI.upper"))),
                  stringsAsFactors=F)

  if(!is.null(groups)){df$Strata <- groups} else
    {df$Strata <- 1:length(C)} # 1

  df$Capture <-C # 2
  df$Mark <- M # 3
  df$Recapture <- R # 4
  #df$Weir.Efficiency <- df$Recapture/df$Capture # 5
  df$Recovery.Rate <- df$Recapture/df$Mark # 6
  df$N.hat <- (((df$Mark + 1)*(df$Capture+1))/(df$Recapture + 1))-1 # 7


  Var <- ((df$Mark+1)*(df$Capture+1)*(df$Mark-df$Recapture)*(df$Capture-df$Recapture))/
          ((df$Recapture+1)*(df$Recapture+1)*(df$Recapture+2))

  if(CI.type == "normal") {
                        df$Std.error <- sqrt(Var) # 8
                        df$CI.lower <- df$N.hat - qnorm(alpha/2,lower.tail=FALSE)*df$Std.error # 9

                        #if(!is.na(df$CI.lower) < df$Mark) {df$CI.lower <- df$Mark}

                        df$CI.upper <- df$N.hat + qnorm(alpha/2,lower.tail=FALSE)*df$Std.error # 10
                        N.hat <- sum(df$N.hat)
                        Std.error <- sqrt(sum(Var))
                        CI.lower <- N.hat - qnorm(1-alpha/2)*Std.error
                        CI.upper <- N.hat + qnorm(1-alpha/2)*Std.error
                      }

  if(CI.type == "boot") {

   Nboot = matrix(NA,iter,length(C))

    for(i in 1:length(C)){

        Ctmp = df[i,2]
        Mtmp = df[i,3]
        Rtmp = df[i,4]
        g_l  = df[i,5]
        Ntmp = df[i,6]

        for(j in 1:iter){

          Cboot = rbinom(1,as.integer(Ntmp),g_l)
          Rboot = rhyper(1,m=Mtmp, n = Ntmp - Mtmp , k = Cboot)
          Nboot[j,i] = (((Mtmp+1)*(Cboot+1))/(Rboot+1))-1

          } # j

    df$Std.error[i] <- sd(Nboot[,i])
    ci = quantile(Nboot[,i],c(alpha/2,1-alpha/2))
    df$CI.lower[i] = ci[1]
    df$CI.upper[i] = ci[2]

      } #i

              N.hat <- sum(df$N.hat)
              N.tot <- rowSums(Nboot)
              Std.error <- sd(N.tot)
              ci <- quantile(N.tot,c(alpha/2,1-alpha/2))
              CI.lower <- ci[1]
              CI.upper <- ci[2]

  } # ends boot if statement

  Var.g <- (1/df$Mark)*(df$Capture/df$N.hat)*((df$N.hat-df$Capture)/df$N.hat)*((df$N.hat-df$Mark)/(df$N.hat-1))
  df$Std.error.g <- sqrt(Var.g)

  Var.w <- (df$Mark/df$Capture^2)*(df$Capture/df$N.hat)*((df$N.hat-df$Capture)/df$N.hat)*((df$N.hat-df$Mark)/(df$N.hat-1))
  df$Std.error.w <- sqrt(Var.w)

  if(print){
    cat("Confidence intervals are", (1-alpha)*100,"% using the",CI.type,"method \n",
              "N.hat = ",N.hat,"\n",
              "Std. Error = ",Std.error,"\n",
              "CI.Lower = ",CI.lower,"\n",
              "CI.Upper = ",CI.upper,"\n")}

  output = list(N.hat = N.hat,Std.error = Std.error,
                CI.lower = CI.lower, CI.upper = CI.upper, Strata = df,
                CI.type = CI.type, alpha=alpha)
 return(output)

} # function close
