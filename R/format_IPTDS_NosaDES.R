#' @title Format Natural Origin Spawner Abundance (NOSA) DES tables for IPTDS.
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

format_IPTDS_NosaDES <- function(df = NULL,
                           alpha = c('0.05', '0.10'),
                           odbc_connection,
                           cdms_url = 'https://npt-cdms.nptfisheries.org'){

  # TESTING stuff--------------------------------------------------------
  con <- RODBC::odbcConnect('CDMS_DEV', uid = 'guest', pwd = 'guest')
  alpha <- '0.05'
  source('./R/guid.R')
  library(tidyverse)
  library(purrr)

  alpha <- as.numeric(match.arg(alpha))
  con <- odbc_connection

  if(class(con) != 'RODBC'){
    stop('An \"odbc\" connection is not identified. A connection with the
    back-end CDMS database is needed for this function to work. Please
    provide a valid connection using package \"RODBC\".')
  }

  # get IPTDS data from CDMS ----
  base_df <- RODBC::sqlFetch(con, 'IPTDS_vw') %>%
    filter(TRT_POPID != 'GRLOS/GRMIN', # remove unwanted populations
            !grepl('_bb', SiteId)) %>% # remove black box estimates
    select(-ActivityDate, -DatasetId, -LocationId, -ActivityQAStatusId, -ActivityQAComments, -QAStatusId,
           -QAStatusName, -kus_pop, -kus_year, -kus_species)

  # if(is.null(df)){
  #   df <- get_NOSAestimates(alpha, cdms_url)
  # }

  # IPTDS: NOSAIJ / NOSAEJ / GUID creation ----
  NOSAIJ_df <- base_df %>%
    filter(Variable == 'Population Escapement') %>%
    mutate_all(as.character) %>%
    mutate(NOSADefinition = 'Escapement',
           NOSAIJAlpha = alpha) %>%
    group_by(ActivityId, TRT_POPID, Species, Run, SpawnYear) %>%
    rowwise() %>%
    mutate(ID = guid(uppercase = FALSE)) %>%
    ungroup() %>%
    # NOSAEJ - Excluding Jacks added here until we have data.
    mutate(NOSAEJ = as.character(NA),
           NOSAEJ = as.character(NA),
           NOSAEJLowerLimit = as.character(NA),
           NOSAEJUpperLimit = as.character(NA),
           NOSAEJAlpha = as.character(NA),
           NOBroodStockRemoved = as.character(NA)) %>%   # should this value be 0?
    select(ActivityId,
           ID,
           TRT_POPID,
           Species,
           Run,
           SpawnYear,
           NOSADefinition,
           NOSAIJ = Estimate,
           NOSAIJLowerLimit = LowerCI,
           NOSAIJUpperLimit = UpperCI,
           NOSAIJAlpha,
           NOSAEJ,
           NOSAEJLowerLimit,
           NOSAEJUpperLimit,
           NOSAEJAlpha,
           NOBroodStockRemoved,
           ValidEstimate) # for NullRecord field.

  # IPTDS: Age Proportions ----
  ageprops_df <- pivot_wider(data= base_df %>%
    filter(Variable == 'Age Proportion') %>%
      mutate(Age = paste0('Age', Age, 'Prop')) %>%
      select(SpawnYear, Species, Run, TRT_POPID, Age, Estimate, LowerCI, UpperCI),
    id_cols = c(SpawnYear, Species, Run, TRT_POPID),
    names_from = Age,
    names_sep = '',
    values_from = c(Estimate, LowerCI, UpperCI)) %>%
    mutate(AgePropAlpha = alpha) %>%
    mutate_all(as.character)

  names(ageprops_df) <- gsub('Estimate', '', names(ageprops_df))
  names(ageprops_df) <- if_else(grepl('LowerCI', names(ageprops_df))==TRUE, paste0(gsub('LowerCI', '', names(ageprops_df)), 'LowerLimit'), names(ageprops_df))
  names(ageprops_df) <- if_else(grepl('UpperCI', names(ageprops_df))==TRUE, paste0(gsub('UpperCI', '', names(ageprops_df)), 'UpperLimit'), names(ageprops_df))

    # add missing age proportion fields
  age_fields <- c('Age2Prop', 'Age2PropLowerLimit', 'Age2PropUpperLimit',
                  'Age3Prop', 'Age3PropLowerLimit', 'Age3PropUpperLimit',
                  'Age4Prop', 'Age4PropLowerLimit', 'Age4PropUpperLimit',
                  'Age5Prop', 'Age5PropLowerLimit', 'Age5PropUpperLimit',
                  'Age6Prop', 'Age6PropLowerLimit', 'Age6PropUpperLimit',
                  'Age7Prop', 'Age7PropLowerLimit', 'Age7PropUpperLimit',
                  'Age8Prop', 'Age8PropLowerLimit', 'Age8PropUpperLimit',
                  'Age9Prop', 'Age9PropLowerLimit', 'Age9PropUpperLimit',
                  'Age10Prop', 'Age10PropLowerLimit', 'Age10PropUpperLimit',
                  'Age11PlusProp', 'Age11PlusPropLowerLimit', 'Age11PlusPropUpperLimit')

  missing_age_fields <- setdiff(age_fields, names(ageprops_df)) # give me the fields not in ageprops_df (no data)

  for(i in 1:length(missing_age_fields)) {
    ageprops_df[paste(missing_age_fields[i])] <- as.character(NA)
  }

  rm(age_fields, missing_age_fields)

  # get NOSA metadata - CAX ----
  nosa_meta <- RODBC::sqlFetch(con, 'CAX_NOSA_meta') %>%
    mutate_all(as.character) %>%
    select(-ID)


  # IPTDS: NOSA DES table
  nosa_des <- NOSAIJ_df %>%
    left_join(ageprops_df, by = c('SpawnYear', 'Species', 'Run', 'TRT_POPID')) %>%
    #  mutate to be removed once DABOM is run again (this fixed the SF Salmon mainstem trtpop)
    mutate(TRT_POPID = if_else(TRT_POPID == 'SFMAI', 'SFSMA', TRT_POPID)) %>%
    mutate(Run = case_when( # align CDMS run with CAX run types
      Species == 'Steelhead' ~ 'summer',  # all steelhead -> summer
      TRT_POPID == 'CRLAP' ~ 'unknown',
      TRT_POPID == 'CRLOC' ~ 'unknown',
      TRT_POPID == 'CRLOL' ~ 'unknown',
      TRT_POPID == 'CRPOT' ~ 'unknown',
      TRT_POPID == 'GRCAT' ~ 'spring',
      TRT_POPID == 'GRLOO' ~ 'unknown',
      TRT_POPID == 'GRLOS' ~ 'spring',
      TRT_POPID == 'GRMIN' ~ 'spring',
      TRT_POPID == 'GRUMA' ~ 'spring',
      TRT_POPID == 'GRWEN' ~ 'spring',
      TRT_POPID == 'IRBSH' ~ 'spring',
      TRT_POPID == 'IRMAI' ~ 'spring/summer',
      TRT_POPID == 'MFBEA' ~ 'spring',
      TRT_POPID == 'MFBIG' ~ 'spring/summer',
      TRT_POPID == 'SCLAW' ~ 'unknown',
      TRT_POPID == 'SCUMA' ~ 'unknown',
      TRT_POPID == 'SEMEA' ~ 'unknown',
      TRT_POPID == 'SFEFS' ~ 'summer',
      TRT_POPID == 'SFMAI' ~ 'summer',    # This is also SF Salmon Mainstem wut hapnan.
      TRT_POPID == 'SFSEC' ~ 'summer',
      TRT_POPID == 'SFSMA' ~ 'summer',   # Didnt find this in the CAX_POPS table (South Fork Salmon Mainstem)
      TRT_POPID == 'SNASO' ~ 'spring',
      TRT_POPID == 'SNTUC' ~ 'spring',
      TRT_POPID == 'SREFS' ~ 'spring/summer',
      TRT_POPID == 'SRLEM' ~ 'spring',
      TRT_POPID == 'SRLMA' ~ 'spring/summer',
      TRT_POPID == 'SRLSR' ~ 'spring/summer',
      TRT_POPID == 'SRNFS' ~ 'spring',
      TRT_POPID == 'SRPAH' ~ 'summer',
      TRT_POPID == 'SRPAN' ~ 'unknown',
      TRT_POPID == 'SRUMA' ~ 'spring',
      TRT_POPID == 'SRVAL' ~ 'spring/summer',
      TRT_POPID == 'SRYFS' ~ 'spring',
      TRUE ~ Run)) %>%
    left_join(nosa_meta, by = c('TRT_POPID'='LocationLabel', 'Species'='CommonName', 'Run', 'NOSADefinition')) %>%
    mutate(NullRecord == case_when(
      ValidEstimate == 0 ~ 'Yes',  # 0=INVALID estimate, therefore should be a Null record.
      TRUE ~ 'No'),
      pHOSij = as.character(NA),
      pHOSijLowerLimit = as.character(NA),
      pHOSijUpperLimit = as.character(NA),
      pHOSijAlpha = as.character(NA),

      pHOSej = as.character(NA),
      pHOSejLowerLimit = as.character(NA),
      pHOSejUpperLimit = as.character(NA),
      pHOSejAlpha = as.character(NA),

      NOSJF = as.character(NA),
      NOSJFLowerLimit = as.character(NA),
      NOSJFUpperLimit = as.character(NA),
      NOSJFAlpha = as.character(NA),
      HOSJF = as.character(NA),

      TSAIJ = as.character(NA),
      TSAIJLowerLimit = as.character(NA),
      TSAIJUpperLimit = as.character(NA),
      TSAIJAlpha = as.character(NA),

      TSAEJ = as.character(NA),
      TSAEJLowerLimit = as.character(NA),
      TSAEJUpperLimit = as.character(NA),
      TSAEJAlpha = as.character(NA),
      Comments = as.character(NA),
      LastUpdated = as.character(NA),
      UpdDate = as.character(NA)) %>%
    select(ActivityId,
           ID,
           CommonName = Species,
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
           SpawningYear = SpawnYear,
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

