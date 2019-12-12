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
#'
get_JuvenileEstimates <- function(alpha = c(0.05, 0.10),
                              cdms_url = 'https://cdms.nptfisheries.org'){

  alpha <- match.arg(alpha)

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
    #filter(Origin == 'Natural') %>%
    select(LocationLabel, Species, Run, Origin, MigratoryYear, Lifestage, StartDate, EndDate, Period, Abundance, SE_A = StdError, Lower95_A = Lower95, Upper95_A = Upper95, EffDt_A = EffDt, Comments_A = Comments)

  s_df <- s_raw %>%
    select(LocationLabel, Species, Run, Origin, MigratoryYear, SampleType, TagSite, RearingVessel,
           ReleaseType, ReleaseGroup, AdClipped, Lifestage, SurvivalTo, Survival, SE_S = StdError, Lower95_S = Lower95, Upper95_S = Upper95, EffDt_S = EffDt, Comments_S = Comments)

  #tmp_a <- anti_join(a_df, s_df)
  #tmp_s <- anti_join(s_df, a_df)

  df <- full_join(a_df, s_df, by = c("LocationLabel", "Species", "Run", "Origin", "MigratoryYear", "Lifestage")) %>%
  mutate(EffDt_A = lubridate::ymd_hms(EffDt_A),
         EffDt_S = lubridate::ymd_hms(EffDt_S),
         Comments = paste0('Abundance comments: ',Comments_A,'; ','Suvival comments: ',Comments_S))

  detail <- df %>%
    mutate(SmoltEq = round(Abundance * Survival,0),
           SE_SmEq = error_propagation(fx = 'product',type= "both_random", x = Abundance, se.x = SE_A,
                                        y = Survival, se.y = SE_S),
           Lower95_SmEq = SmoltEq - qnorm(1-alpha/2)*SE_SmEq,
           Upper95_SmEq = SmoltEq + qnorm(1-alpha/2)*SE_SmEq,
           UpdDate = Sys.time(),
           Abundance_alpha = alpha,
           Survival_alpha = alpha,
           SmoltEq_alpha = alpha) %>%
    mutate_at(vars(contains('_SmEq')),round, digits =2) %>%
    rowwise() %>%
    mutate(LastUpdated = max(c(EffDt_A, EffDt_S), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-EffDt_A, -EffDt_S, -Comments_A, -Comments_S)

  return(detail)
}
