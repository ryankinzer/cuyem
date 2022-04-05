#' @title Estimate juvenile emigrant abundance from M-R data.
#'
#' @description \code{emigrant_abundance} generates stratified juvenile abundance
#'   estimates from M-R data collected at rotary screw traps (RST).
#'   The function operates similar to a Gauss based program developed by
#'   Steinhorst et al. (2004). Point estimates are derived using the Bailey
#'   estimator and uncertainty is obtained by bootstrapping unmarked captures
#'   and marked recaptures.
#'
#' @section Warning: Standard error and confidence intervals will not match GAUSS estimates
#'   perfectly because of bootstrapped sample distribution.
#'
#' @param data An R dataframe containing at least three fields of data; the count of "unmark",  "mark", and "recap" fish captured in the trap. Each row of the #' data is treated as an independent strata. Grouping variables can also be included in the data (e.g., trap season or life-stage).
#'
#' @param unmark
#' @param mark
#' @param recap
#' @param alpha Type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#' @param iter The number of bootstrap iterations to be run.  Default is set at
#'   1000.
#' @author Ryan N. Kinzer
#'
#' @examples
#' u <- c(76, 128, 82, 61, 350, 74)
#' m <- c(68, 117, 70, 42, 282, 64)
#' r <- c(7, 10, 9, 18, 48, 10)
#'
#' dat <- data.frame('u' = u, 'm' = m, 'r' = r)
#' est_strata <- est_emigrants(data = dat, alpha = .05,iter = 1000, print=TRUE)
#' est_strata$Strata
#'
#' @return A list of point and uncertainty estimates by each strata.
#'
#' @export

est_emigrants <- function(data, unmark, mark, recap, alpha = 0.05, iter = 1000, boot_values = FALSE, print = FALSE){

  if(is.null(data)){stop("A dataset is necessary for the function to work.")}

  unmark <- ensym(unmark)
  mark <- ensym(mark)
  recap <- ensym(recap)

  C <- data %>% select(!!unmark) %>% pull()
  M <- data %>% select(!!mark) %>% pull()
  R <- data %>% select(!!recap) %>% pull()

    df = data.frame(matrix(NA,length(C)+1,9, dimnames=list(c(),c("strata", "unmark",
         "mark","recap","efficiency","Nhat","std_error","ci_upper","ci_lower"))),
         stringsAsFactors=F)
    df$strata = c(1:length(C),"Total")
    df$unmark = c(C,sum(C))
    df$mark = c(M,sum(M))
    df$recap = c(R,sum(R))
    df$efficiency = round(df$recap/df$mark,3)
    df$Nhat = round((df$unmark*(df$mark + 1))/(df$recap + 1))
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

    # if(save_file == TRUE){
    #   if(is.null(file_name)){
    #     name <- as.character(paste0("emigrant_abundance_",format(Sys.Date(), format="%m%d%Y"),".csv"))
    #       } else {
    #     name <- as.character(paste0(file_name,"_",format(Sys.Date(), format="%m%d%Y"),".csv"))
    #       }
    #     write.csv(df, file = name, row.names = FALSE)
    #   }

    tot_df <- data.frame('Nhat' = df[length(C)+1,6],
                         'std_error' = df[length(C)+1,7],
                         'lwr' = df[length(C)+1,8],
                         'upr' = df[length(C)+1,9])

    if(print == TRUE){
      print(tot_df)
    }

    output = list(Nhat = tot_df, strata = df[1:length(C),5:9])

    if(boot_values == TRUE){
      output = list(Nhat = tot_df, strata = df[1:length(C),5:9], Nboot = Nboot)
    }

    return(output)

    }
