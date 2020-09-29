#' @title Estimate juvenile emigrant abundance from M-R data.
#'
#' @description \code{emigrant_abundance} generates stratified juvenile abundance
#'   estimates from M-R data collected at rotary screw traps (RST).
#'   The function operates similar to a Gauss based program developed by
#'   Steinhorst et al. (2004). Point estimates are derived using the Bailey
#'   estimator and uncertainty is obtained by bootstrapping unMed capatures
#'   and Med Rs.
#'
#' @section Warning: Standard error and confidence intervals will not match GAUSS estimates
#'   perfectly because of bootstrapped sample distribution.
#'
#' @param data An R dataframe or "example.csv" file containing at least three fields of data; the
#'   count of unMed Cs (\code{C}), Med fish (\code{M}) released
#'   upstream to conduct trap efficiency trials, and Rd (\code{R}) trap
#'   efficiency fish. Field names are required to be; C, M, R.  All other fields are ignored.
#'
#' @param alpha Type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @param iter The number of bootstrap iterations to be run.  Default is set at
#'   1000.
#'
#' @param print Logical value. Default value is TRUE. Should results be printed to the console?
#'
#' @param save_file Logical value. Default value is FALSE.  Should the results be saved as a .csv file to the current working directory?
#'
#' @param file_name A character string with the desired name of the outputed .csv file.  The saved file name
#' will also have the month, day and year of creation appended to the file_name.
#'
#' @author Ryan N. Kinzer
#'
#' @examples
#' # A single strata
#' est1 <- emigrant_abundance(data.frame(C = 76, M = 68, R = 7), alpha = 0.05, iter = 1000, print = TRUE)
#' names(est1)
#' est1$Strata
#'
#' # Multiple strata
#' unM <- c(76, 128, 82, 61, 350, 74)
#' M <- c(68, 117, 70, 42, 282, 64)
#' recap <- c(7, 10, 9, 18, 48, 10)
#'
#' dat <- data.frame(C = unM, M = M, R = recap)
#' est_strata <- emigrant_abundance(data = dat, alpha = .05,iter = 1000, print=TRUE)
#' est_strata$Strata
#'
#' est_csv <- emigrant_abundance(data = "example.csv", alpha = .05,iter = 1000, print=TRUE)
#'
#' @return A list of point and uncertainty estimates by each strata.
#'
#' @export

emigrant_abundance <- function(data, alpha = 0.05, iter = 1000, print = TRUE, save_file = FALSE, file_name = NULL){

  if(is.character(data) == TRUE){
    input <- read.csv(file = data, header = TRUE, sep =',')
    } else {
    input <- data
    }

  C <- input$C
  M <- input$M
  R <- input$R

    df = data.frame(matrix(NA,length(C)+1,9, dimnames=list(c(),c("Strata", "C",
         "M","R","Efficiency","N.hat","Std.error","CI.lower","CI.upper"))),
         stringsAsFactors=F)
    df$Strata = c(1:length(C),"Total")
    df$C = c(C,sum(C))
    df$M = c(M,sum(M))
    df$R = c(R,sum(R))
    df$Efficiency = round(df$R/df$M,3)
    df$N.hat = round((df$C*(df$M + 1))/(df$R + 1))
    df[length(C)+1,6] = sum(df[1:length(C),6])
    Nboot = matrix(NA,iter,length(C))
    for(i in 1:(dim(df)[1]-1)){
      Ctmp = df[i,2]
      Mtmp = df[i,3]
      Rtmp = df[i,4]
      Eff  = df[i,5]
      Ntmp = df[i,6]
        for(j in 1:iter){
            Rboot = rbinom(1,Mtmp,Eff)
            Cboot = rbinom(1,Ntmp,Eff)
            Nboot[j,i] = (Cboot*(Mtmp+1))/(Rboot+1)
            } # j
      df[i,7] = sd(Nboot[,i])
      ci = quantile(Nboot[,i],c(alpha/2,1-alpha/2))
      df[i,8] = ci[1]
      df[i,9] = ci[2]
    } #i
    if (length(C) == 1) {Ntot = Nboot} else
      {Ntot = rowSums(Nboot[,1:length(C)])}
    df[length(C)+1,7] = sd(Ntot)
    ci_t = quantile(Ntot,c(alpha/2,1-alpha/2))
    df[length(C)+1,8] = ci_t[1]
    df[length(C)+1,9] = ci_t[2]

    if(save_file == TRUE){
      if(is.null(file_name)){
        name <- as.character(paste0("emigrant_abundance_",format(Sys.Date(), format="%m%d%Y"),".csv"))
          } else {
        name <- as.character(paste0(file_name,"_",format(Sys.Date(), format="%m%d%Y"),".csv"))
          }
        write.csv(df, file = name, row.names = FALSE)
      }

    if(print == TRUE){
      cat("Total N.hat = ",df[length(C)+1,6],"\n",
          "Std. Error = ",df[length(C)+1,7],"\n",
          "CI.Lower = ",df[length(C)+1,8],"\n",
          "CI.Upper = ",df[length(C)+1,9],"\n")
      }

    output = list(N.hat = df[length(C)+1,6],Std.error = df[length(C)+1,7],
                  CI.lower = df[length(C)+1,8], CI.upper = df[length(C)+1,9], Strata = df,
                  Nboot = Nboot, Ntot = Ntot)

    return(output)

    }
