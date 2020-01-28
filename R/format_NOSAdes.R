#' @title Format Natural Origin Spawner Abundance DES tables.
#'
#' @description Format NOSA data generated from \code{get_NOSAestimates}
#'   for the coordinated assessments natural origin spawner abundance DES
#'   tables. The function requires an odbc connection from behind the NPT
#'   firewall to download CAX metadata tables from the CDMS database.
#'   Additionally, if a dataframe is not supplied a connection to
#'   \url{https://npt-cdms.nptfisheries.org} using
#'   \code{cdmsR::cdmsLogin()} is also required.
#'
#' @param df \code{excellent} dataframe output from \code{get_NOSAestimates}.
#'
#' @param alpha Type I error rate. Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @param odbc_connection an open odbc channel to CDMS database
#'
#' @param cdms_host URL for CDMS website. The default is the Nez Perce Tribe's
#'   data repository \url{https://npt-cdms.nptfisheries.org}.
#
#' @return coordinated assessments NOSA DES table
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' cdmsR::cdmsLogin('your_username', 'your_password')
#' df <- get_NOSAestimates()
#' con <- RODBC::odbcConnect(dsn = 'data source', uid = 'your_username', pwd = 'your_password')
#' format_NOSAdes(df, odbc_connection = con)

format_NOSAdes <- function(df = NULL,
                           alpha = c('0.05', '0.10'),
                           odbc_connection,
                           cdms_url = 'https://npt-cdms.nptfisheries.org'){

  alpha <- as.numeric(match.arg(alpha))
  con <- odbc_connection

  if(class(con) != 'RODBC'){
    stop('An \"odbc\" connection is not identified. A connection with the
    back-end CDMS database is needed for this function to work. Please
    provide a valid connection using package \"RODBC\".')
  }

  # get NOSA data from CDMS
  if(is.null(df)){
    df <- get_NOSAestimates(alpha, cdms_url)
  }

  # filter NOSA data for CAX-specific records
  tmp_df <- df %>%
    filter(Origin == 'Natural') %>%
    # filter()
    mutate_all(as.character) %>%
    group_by(LocationLabel, Species, Run, Origin, SpawningYear) %>%
    rowwise() %>%
    mutate(ID = guid(uppercase = FALSE)) %>%
    ungroup()


  # TESTING stuff
  # con <- RODBC::odbcConnect('CDMS_DEV', uid = 'guest', pwd = 'guest')

  # get CAX metadata
  nosa_meta <- RODBC::sqlFetch(con, 'CAX_NOSA_meta') %>%
    mutate_all(as.character) %>%
    select(-ID)


  # format NOSA DES table
  nosa_df <- tmp_df %>%
    mutate(Run = case_when( # align CDMS run with CAX run types
      Species == 'Chinook salmon' & LocationLabel %in% c("Big Sheep Creek: BSC",
                                                         "Imnaha River: Imnaha River Weir",
                                                         "Imnaha River: IR1",
                                                         "Imnaha River: IR2",
                                                         "Imnaha River: IR3",
                                                         "Imnaha River: IR4") ~ 'spring/summer',
      Species == 'Chinook salmon' & LocationLabel == "Lolo Creek: Lower Lolo Creek Weir" ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == "Clearwater River: Dworshak National Fish Hatchery Trap" ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == "Clearwater River: Nez Perce Tribal Hatchery Trap" ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == "Lochsa River: Clearwater Hatchery Powell Sattelite Trap" ~ 'unknown',
      Species == 'Chinook salmon' & LocationLabel == "Joseph Creek: Joseph Creek Weir" ~ 'unknown',
      TRUE ~ Run)) %>%
    left_join(nosa_meta, by = c('LocationLabel', 'Species' = 'CommonName', 'Run'))


  nosa_des <- nosa_df %>%
    mutate(NullRecord == case_when(
      is.na(NOSAIJ) | is.na(NOSAEJ) ~ 'Yes',
      TRUE ~ 'No')) %>%
    select(ID,
           CommonName,
           Run,
           RecoveryDomain,
           ESU_DPS,
           MajorPopGroup,
           PopID,
           CBFWApopName,
           CommonPopName,
           PopFit,
           PopFitNotes,
           WaterBody,
           SpawningYear,
           TRTmethod,
           ContactAgency,
           MethodNumber,
           BestValue,
           NOSADefinition,
           NOSAIJ,
           NOSAIJLowerLimit,
           NOSAIJUpperLimit,
           NOSAIJAlpha,
           NOSAEJ,
           NOSAEJLowerLimit,
           NOSAEJUpperLimit,
           NOSAEJAlpha,
           NOBroodStockRemoved,
           pHOSij,
           pHOSijLowerLimit,
           pHOSijUpperLimit,
           pHOSijAlpha,
           pHOSej,
           pHOSejLowerLimit,
           pHOSejUpperLimit,
           pHOSejAlpha,
           NOSJF,
           NOSJFLowerLimit,
           NOSJFUpperLimit,
           NOSJFAlpha,
           HOSJF,
           TSAIJ,
           TSAIJLowerLimit,
           TSAIJUpperLimit,
           TSAIJAlpha,
           TSAEJ,
           TSAEJLowerLimit,
           TSAEJUpperLimit,
           TSAEJAlpha,
           Age2Prop,
           Age2PropLowerLimit,
           Age2PropUpperLimit,
           Age3Prop,
           Age3PropLowerLimit,
           Age3PropUpperLimit,
           Age4Prop,
           Age4PropLowerLimit,
           Age4PropUpperLimit,
           Age5Prop,
           Age5PropLowerLimit,
           Age5PropUpperLimit,
           Age6Prop,
           Age6PropLowerLimit,
           Age6PropUpperLimit,
           Age7Prop,
           Age7PropLowerLimit,
           Age7PropUpperLimit,
           Age8Prop,
           Age8PropLowerLimit,
           Age8PropUpperLimit,
           Age9Prop,
           Age9PropLowerLimit,
           Age9PropUpperLimit,
           Age10Prop,
           Age10PropLowerLimit,
           Age10PropUpperLimit,
           Age11PlusProp,
           Age11PlusPropLowerLimit,
           Age11PlusPropUpperLimit,
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
           MetricLocation,
           MeasureLocation,
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

  return(nosa_des)

}
