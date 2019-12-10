#' @title Download juvenile abundance data from CDMS and format for Coordinated
#' Assessments Juvenile Detail DES. The function requires a CDMS login to
#' first gather data.
#'
#' @param dsId_abundance CDMS datasetId for RST abundance dataset.
#' @param dsID_survival CDMS datasetId for juvenile survival dataset.
#' @param alpha Type I error rate. Default is set at 0.05 to produce 95\%
#' confidence intervals.
#' @param cdms_host URL for CDMS website.
#'
#' @return
#' @export
#'
#' @examples
get_JuvenileDES <- function(output = c('detail', 'smoltEq'),
                              alpha = 0.05,
                              cdms_url = 'https://cdms.nptfisheries.org'){

  output <- match.arg(output)

  if(!requireNamespace("cdmsR", quietly = TRUE)){
    stop("Package \"cdmsR\" is needed for this function to work.  Please install it.",
         call. = FALSE)
  }

  if("try-error" %in% class(try(getDatastores(cdms_host = cdms_url)))){
    stop('A CDMS login is required for this function to work. Please use \"cdmsLogin\" with your username and password first.', call. = FALSE)
  }

  a_raw <- getDatasetView(datastoreID = 85, cdms_host = cdms_url)

  s_raw <- getDatasetView(datastoreID = 86, cdms_host = cdms_url)

  a_df <- a_raw %>%
    filter(Lifestage != 'Total') %>%
    filter(Origin == 'Natural') %>%
    select(LocationLabel, Species, Run, Origin, MigratoryYear, Lifestage, StartDate, EndDate, Period, Abundance, SE_A = StdError, Lower95_A = Lower95, Upper95_A = Upper95, EffDt_A = EffDt, Comments_A = Comments)

  s_df <- s_raw %>%
    filter(SampleType == 'Rotary Screw Trap') %>%
    filter(Origin == 'Natural') %>%
    filter(SurvivalTo == 'Lower Granite Dam') %>%
    select(LocationLabel, Species, Run, Origin, MigratoryYear, Lifestage, Survival, SE_S = StdError, Lower95_S = Lower95, Upper95_S = Upper95, EffDt_S = EffDt, Comments_S = Comments)

  #tmp_a <- anti_join(a_df, s_df)
  #tmp_s <- anti_join(s_df, a_df)

  df <- left_join(a_df, s_df, by = c("LocationLabel", "Species", "Run", "Origin", "MigratoryYear", "Lifestage")) %>%
  mutate(EffDt_A = lubridate::ymd_hms(EffDt_A),
         EffDt_S = lubridate::ymd_hms(EffDt_S),
         Comments = paste0('Abundance comments: ',Comments_A,'; ','Suvival comments: ',Comments_S))

  detail <- df %>%
    mutate(SmoltEq = Abundance * Survival,
           SE_SmEq = error_propagation(fx = 'multiply',type= "both_random", X = Abundance, SE.X = SE_A,
                                        Y = Survival, SE.Y = SE_S),
           Lower95_SmEq = SmoltEq - qnorm(1-alpha/2)*SE_SmEq,
           Upper95_SmEq = SmoltEq + qnorm(1-alpha/2)*SE_SmEq,
           UpdDate = Sys.time(),
           Abundance_alpha = alpha,
           Survival_alpha = alpha,
           SmoltEq_alpha = alpha) %>%
    rowwise() %>%
    mutate(LastUpdated = max(c(EffDt_A, EffDt_S), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-EffDt_A, -EffDt_S, -Comments_A, -Comments_S)


  if(output == 'detail'){
    ests <- detail
  } else {
    ests <- detail %>%
      filter(!is.na(Survival)) %>%
      group_by(LocationLabel, Species, Run, Origin, MigratoryYear) %>%
      summarise(SmoltEq = sum(SmoltEq),
                SE_SmEq = sqrt(sum(SE_SmEq^2)),
                Lower95_SmEq = SmoltEq - qnorm(1-alpha/2)*SE_SmEq,
                Upper95_SmEq = SmoltEq + qnorm(1-alpha/2)*SE_SmEq,
                Comments = paste0('Only includes lifestages: ', toString(Lifestage)),
                LastUpdated = max(LastUpdated),
                UpdDate = max(UpdDate),
                SmoltEq_alpha = alpha
      )

  }

  return(ests)
}
