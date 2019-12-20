#' @title Format juvenile out-migrant DES tables.
#'
#' @description Format juvenile abundance and survival data generated from
#'   \code{get_JuvenileEstimates} for the coordinated assessments juvenile
#'   outmigrant and juvenile outmigrant detail DES tables. The function requires
#'   an odbc connection from behind the NPT firewall to download CAX metadata
#'   tables from the CDMS database.  Additionally, if a dataframe is not
#'   supplied a connection to \url{https://cdms.nptfisheries.org} using
#'   \code{cdmsR::cdmsLogin()} is also required.
#'
#' @param df \code{lifestage} dataframe output from \code{get_JuvenileEstimates}.
#'
#' @param alpha Type I error rate. Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @param odbc_connection an open odbc channel to CDMS database
#'
#' @param cdms_host URL for CDMS website. The default is the Nez Perce Tribe's
#'   data repository \url{https://cdms.nptfisheries.org}.
#
#' @return a list with coordinated assessments juvenile outmigrant and juvenile outmigrant detail DES tables
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' cdmsR::cdmsLogin('your_username', 'your_password')
#' df <- get_JuvenileEstimates()
#' con <- RODBC::odbcConnect(dsn = 'data source', uid = 'your_username', pwd = 'your_password')
#' format_JuvenileDES(df, odbc_connection = con)

format_JuvenileDES <- function(df = NULL,
                               alpha = c('0.05', '0.10'),
                               odbc_connection,
                               cdms_url = 'https://cdms.nptfisheries.org'){

  alpha <- as.numeric(match.arg(alpha))
  con <- odbc_connection

  if(class(con) != 'RODBC'){
    stop('An \"odbc\" connection is not identified. A connection with the
    back-end CDMS database is needed for this function to work. Please
    provide a valid connection using package \"RODBC\".')
  }

  # get abundance and smolt data from CDMS
  if(is.null(df)){
    df <- get_JuvenileEstimates(alpha, cdms_url)
  }
  # filter for records specific to CAX
  tmp_df <- df %>%
    filter(Origin == 'Natural') %>%
    filter(!(SampleType %in% c('Dip-Net', 'Beach Seine'))) %>%
    filter(SurvivalTo %in% c(NA, 'Lower Granite Dam')) %>%
    mutate_all(as.character) %>%
    group_by(LocationLabel, Species, Run, Origin, MigratoryYear) %>%
    mutate(JuvenileOutmigrantsID = guid(uppercase = FALSE)) %>%
    rowwise() %>%
    mutate(ID = guid(uppercase = FALSE)) %>%
    ungroup()

  # get CAX metadata
  detail_meta <- RODBC::sqlFetch(con, 'CAX_JuvOutmigrantsDetail_meta') %>%
    mutate_all(as.character) %>%
    select(-ID, -JuvenileOutmigrantsID)

  smoltEq_meta <- RODBC::sqlFetch(con, 'CAX_JuvOutmigrants_meta') %>%
    mutate_all(as.character) %>%
    select(-ID)

  # format detail table

  detail_df <- tmp_df %>%
    mutate(Run = case_when( # align CDMS run with CAX run types
      Species == 'Chinook salmon' & LocationLabel == 'Lolo Creek RST' ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == 'Newsome Creek RST' ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == 'Johnson Creek RST' ~ 'Summer',
      Species == 'Chinook salmon' & LocationLabel == 'Lower Secesh River RST' ~ 'Summer',
      Species == 'Chinook salmon' & LocationLabel == 'Upper Secesh River RST' ~ 'Summer',
      Species == 'Chinook salmon' & LocationLabel == 'Lake Creek RST' ~ 'Summer',
      TRUE ~ Run)) %>%
    left_join(detail_meta, by = c('LocationLabel', 'Species' = 'CommonName', 'Run'))

  detail_des <- detail_df %>%
    select(ID,
           JuvenileOutmigrantsID,
           Location = LocationLabel,
           LocPTcode,
           LifeStage = Lifestage,
           TotalNatural = Abundance,
           TotalNaturalLowerLimit = Lower95_A,
           TotalNaturalUpperLimit = Upper95_A,
           TotalNaturalAlpha = Abundance_alpha,
           SurvivalRate = Survival,
           SurvivalRateLowerLimit = Lower95_S,
           SurvivalRateUpperLimit = Upper95_S,
           SurvivalRateAlpha = Survival_alpha,
           ContactAgency,
           Comments,
           JMXID,
           NullRecord,
           LastUpdated,
           MetricLocation,
           MeasureLocation,
           SubmitAgency,
           RefID,
           UpdDate,
           DataEntry,
           DataEntryNotes,
           CompilerRecordID,
           Publish)

# format smoltEq table
    smoltEq_des <- detail_df %>%
      mutate_at(vars(contains('Eq')), as.numeric) %>%
      group_by(LocationLabel, Species, Run, MigratoryYear, JuvenileOutmigrantsID) %>%
      summarise(TotalNatural = round(sum(SmoltEq)),
                SE_SmEq = sqrt(sum(SE_SmEq^2)),
                TotalNaturalLowerLimit = round(TotalNatural - qnorm(1-alpha/2)*SE_SmEq),
                TotalNaturalUpperLimit = round(TotalNatural + qnorm(1-alpha/2)*SE_SmEq),
                Comments = paste0('Includes lifestages: ', toString(Lifestage)),
                LastUpdated = max(LastUpdated),
                UpdDate = max(UpdDate),
                TotalNaturalAlpha = alpha) %>%
      mutate(TotalNaturalLowerLimit = ifelse(TotalNaturalLowerLimit < 0, 0, TotalNaturalLowerLimit)) %>%
      ungroup() %>%
    left_join(smoltEq_meta, by = c('LocationLabel', 'Species' = 'CommonName', 'Run')) %>%
      mutate(NullRecord = ifelse(is.na(TotalNatural),'Yes','No'),
             DataStatus = 'Draft') %>%
      select(ID = JuvenileOutmigrantsID,
             CommonName = Species,
             Run,
             RecoveryDomain,
             ESU_DPS,
             MajorPopGroup,
             PopID = POPID,
             CBFWApopName,
             CommonPopName,
             PopFit,
             PopFitNotes,
             SmoltEqLocation,
             SmoltEqLocPTcode,
             OutmigrationYear = MigratoryYear,
             ContactAgency,
             MethodNumber,
             BestValue,
             TotalNatural,
             TotalNaturalLowerLimit,
             TotalNaturalUpperLimit,
             TotalNaturalAlpha,
             Age0Prop,
             Age0PropLowerLimit,
             Age0PropUpperLimit,
             Age1Prop,
             Age1PropLowerLimit,
             Age1PropUpperLimit,
             Age2Prop,
             Age2PropLowerLimit,
             Age2PropUpperLimit,
             Age3Prop,
             Age3PropLowerLimit,
             Age3PropUpperLimit,
             Age4PlusProp,
             Age4PlusPropLowerLimit,
             Age4PlusPropUpperLimit,
             AgePropAlpha,
             ProtMethName,
             ProtMethURL,
             ProtMethDocumentation,
             MethodAdjustments,
             OtherDataSources,
             Comments,
             NullRecord,
             DataStatus,
             LastUpdated,
             IndicatorLocation,
             ContactPersonFirst,
             ContactPersonLast,
             ContactPhone,
             ContactEmail,
             MetaComments,
             SubmitAgency,
             RefID,
             UpdDate,
             DataEntry,
             DataEntryNotes,
             CompilerRecordID,
             Publish)

  return(list(detail_des, smoltEq_des))

}
