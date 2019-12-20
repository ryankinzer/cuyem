#' @title Summarize Juvenile Abundance and Survival Metrics
#'
#' @description Download juvenile abundance and survival estimates from NPT's
#'   data repository, \url{https://cdms.nptfisheries.org}. Requires a valid
#'   username and password to access data.
#'
#' @param alpha Type I error rate. Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @param cdms_host URL for CDMS website. The default is the Nez Perce Tribe's
#'   data repository \url{https://cdms.nptfisheries.org}.
#'
#' @return \code{get_JuvenileEstimates()} returns a dataframe with all lifestage
#'   abundance, survival and smolt equivalent (smoltEq) estimates stored in
#'   \url{https://cdms.nptfisheries.org} for each juvenile screw trap, seining
#'   and hatchery release group.
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' cdmsR::cdmsLogin('your_username', 'your_password')
#' get_JuvenileEstimates()

get_JuvenileEstimates <- function(alpha = c('0.05', '0.10'),
                              cdms_url = 'https://cdms.nptfisheries.org'){

  alpha <- as.numeric(match.arg(alpha))

  if(!requireNamespace("cdmsR", quietly = TRUE)){
    stop("Package \"cdmsR\" is needed for this function to work.  Please install it.",
         call. = FALSE)
  }

  if("try-error" %in% class(try(cdmsR::getDatastores(cdms_host = cdms_url)))){
    stop('A CDMS login is required for this function to work. Please use \"cdmsLogin\" with your username and password first.', call. = FALSE)
  }

  a_raw <- cdmsR::getDatasetView(datastoreID = 85, cdms_host = cdms_url)

  s_raw <- cdmsR::getDatasetView(datastoreID = 86, cdms_host = cdms_url)

  a_df <- a_raw %>%
    filter(Lifestage != 'Total') %>%
    # select(LocationLabel, Species, Run, Origin, MigratoryYear, Lifestage, StartDate, EndDate, Abundance, SE_A = StdError, Lower95_A = Lower95, Upper95_A = Upper95, EffDt, Comments) %>%
  mutate(Lifestage = ifelse(Lifestage%in%c('YOY', 'Parr'),'YOY/Parr',Lifestage)) %>%
    group_by(LocationLabel, Species, Run, Origin, MigratoryYear, Lifestage) %>%
  summarise(StartDate = min(StartDate),
         EndDate = max(EndDate),
         Abundance = sum(Abundance),
         SE_A = sqrt(sum((StdError^2))),
         Lower95_A = Abundance - qnorm(1-alpha/2)*SE_A,
         Upper95_A = Abundance + qnorm(1-alpha/2)*SE_A,
         EffDt_A = max(EffDt),
         Comments_A = min(Comments)) %>%
    mutate(Lower95_A = ifelse(Lower95_A < 0, 0, Lower95_A))

  s_df <- s_raw %>%
    mutate(Lifestage = ifelse(Lifestage%in%c('YOY', 'Parr'),'YOY/Parr',Lifestage)) %>%
    select(LocationLabel, Species, Run, Origin, MigratoryYear, SampleType, TagSite, RearingVessel,
           ReleaseType, ReleaseGroup, AdClipped, Lifestage, SurvivalTo, Survival, SE_S = StdError, Lower95_S = Lower95, Upper95_S = Upper95, EffDt_S = EffDt, Comments_S = Comments) %>%
    mutate(Lower95_S = Survival - qnorm(1-alpha/2)*SE_S,
           Upper95_S = Survival + qnorm(1-alpha/2)*SE_S,
           Lower95_S = ifelse(Lower95_S < 0, 0, Lower95_S),
           Upper95_S = ifelse(Upper95_S > 1, 1, Upper95_S))

  df <- full_join(a_df, s_df, by = c("LocationLabel", "Species", "Run", "Origin", "MigratoryYear", "Lifestage")) %>%
  mutate(EffDt_A = lubridate::ymd_hms(EffDt_A),
         EffDt_S = lubridate::ymd_hms(EffDt_S),
         Comments = paste0('Abundance comments: ',Comments_A,'; ','Suvival comments: ',Comments_S)) %>%
    mutate(SmoltEq = round(Abundance * Survival,0),
           SE_SmEq = error_propagation(fx = 'product',type= "both_random", x = Abundance, se.x = SE_A,
                                        y = Survival, se.y = SE_S),
           Lower95_SmEq = SmoltEq - qnorm(1-alpha/2)*SE_SmEq,
           Lower95_SmEq = ifelse(Lower95_SmEq < 0, 0, Lower95_SmEq),
           Upper95_SmEq = SmoltEq + qnorm(1-alpha/2)*SE_SmEq,
           UpdDate = Sys.time(),
           Abundance_alpha = alpha,
           Survival_alpha = alpha,
           SmoltEq_alpha = alpha) %>%
    mutate_at(vars(contains('_SmEq')),round, digits =0) %>%
    rowwise() %>%
    mutate(LastUpdated = max(c(EffDt_A, EffDt_S), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-EffDt_A, -EffDt_S, -Comments_A, -Comments_S)

  return(df)
}
