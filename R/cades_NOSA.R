#' @title Format NOSA Coordinated Assessments DES table
#'
#' @description Format NOSA data generated from
#'   \code{get_NOSAests} for the coordinated assessments NOSA DES table. The function requires a downloaded CAX metadata
#'   table from NPT servers.
#'
#' @param df dataframe output from \code{get_NOSAests}
#' @param nosa_meta CA DES NOSA metadata
#' @param IPTDS logical for the estimates being from the DABOM output
#' @param alpha  Type I error rate. Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @import dplyr
#' @return
#' @export

cades_NOSA <- function(nosa_data, nosa_meta, IPTDS = FALSE, alpha = c('0.05', '0.10')){

  alpha <- match.arg(alpha)

  if(is.null(nosa_data) || is.null(nosa_meta)){
    stop("NOSA data and the metadata is required.")
  }

  nosa_data <- nosa_data %>% ungroup()

  if(!IPTDS){

  tmp_df <- nosa_meta %>%
    filter(Method == 'Weir + Redd Expansion') %>% select(-Method) %>%
    inner_join(nosa_data %>% ungroup() %>% select(-Run),
              by = c('CommonName' = 'Species', 'CommonPopName' = 'TRT_POPID'))

  tmp_df <- tmp_df %>%
    mutate(NullRecord = ifelse(is.na(NOSAij), 'Yes', 'No'),
           #ShadowId = replicate(n(),guid(uppercase = FALSE)),
           Age2Prop = NA,
           Age2PropLowerLimit = NA,
           Age2PropUpperLimit = NA,
            Age7Prop =  NA,
            Age7PropLowerLimit =  NA,
            Age7PropUpperLimit =  NA,
            Age8Prop =  NA,
            Age8PropLowerLimit =  NA,
            Age8PropUpperLimit =  NA,
            Age9Prop =  NA,
            Age9PropLowerLimit =  NA,
            Age9PropUpperLimit =  NA,
            Age10Prop =  NA,
            Age10PropLowerLimit =  NA,
            Age10PropUpperLimit =  NA,
            Age11PlusProp =  NA,
            Age11PlusPropLowerLimit = NA,
            Age11PlusPropUpperLimit =  NA,
           MethodAdjustments = NA,
           UpdDate = Sys.time(),
            alpha = alpha
           )

  tmp_df <- tmp_df %>%
    select(#ShadowId,
           CommonName,
           Run = CAX_Run,
           RecoveryDomain,
           ESU_DPS,
           MajorPopGroup,
           PopID,
           CBFWApopName,
           CommonPopName,
           PopFit,
           PopFitNotes,
           EstimateType,
           EstimateTypeNotes,
           WaterBody,
           SpawningYear = SurveyYear,
           TRTmethod,
           ContactAgency,
           MethodNumber,
           BestValue,
           NOSAIJ = NOSAij,
           NOSAIJLowerLimit = NOSAij_lwr,
           NOSAIJUpperLimit = NOSAij_upr,
           NOSAIJAlpha = alpha,
           NOSAEJ = NOSAej,
           NOSAEJLowerLimit = NOSAej_lwr,
           NOSAEJUpperLimit = NOSAej_upr,
           NOSAEJAlpha = alpha,
           NOBroodStockRemoved,
           pHOSij,
           pHOSijLowerLimit = pHOSij_lwr,
           pHOSijUpperLimit = pHOSij_upr,
           pHOSijAlpha = alpha,
           pHOSej,
           pHOSejLowerLimit = pHOSej_lwr,
           pHOSejUpperLimit = pHOSej_upr,
           pHOSejAlpha = alpha,
           NOSJF,
           NOSJFLowerLimit = lwr_NOSJF,
           NOSJFUpperLimit = upr_NOSJF,
           NOSJFAlpha = alpha,
           HOSJF,
           TSAIJ = TSAij,
           TSAIJLowerLimit = TSAij_lwr,
           TSAIJUpperLimit = TSAij_upr,
           TSAIJAlpha = alpha,
           TSAEJ = TSAej,
           TSAEJLowerLimit = TSAej_lwr,
           TSAEJUpperLimit = TSAej_upr,
           TSAEJAlpha = alpha,
           Age2Prop,
           Age2PropLowerLimit,
           Age2PropUpperLimit,
           Age3Prop = p_3,
           Age3PropLowerLimit = lwr_3,
           Age3PropUpperLimit = upr_3,
           Age4Prop = p_4,
           Age4PropLowerLimit = lwr_4,
           Age4PropUpperLimit = upr_4,
           Age5Prop = p_5,
           Age5PropLowerLimit = lwr_5,
           Age5PropUpperLimit = upr_5,
           Age6Prop = p_6,
           Age6PropLowerLimit = lwr_6,
           Age6PropUpperLimit = upr_6,
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
           AgePropAlpha = alpha,
           ProtMethName,
           ProtMethURL,
           ProtMethDocumentation,
           MethodAdjustments,
           OtherDataSources,
           Comments,
           NullRecord,
           DataStatus,
           LastUpdated = EffDt,
           IndicatorLocation,
           MetricLocation,
           MeasureLocation,
           ContactPersonFirst,
           ContactPersonLast,
           ContactPhone,
           ContactEmail,
           MetaComments = Method,
           SubmitAgency,
           RefID,
           UpdDate,
           DataEntry,
           DataEntryNotes,
           CompilerRecordID,
           Publish
           )
  }


  if(IPTDS){

    tmp_df <- inner_join(nosa_data,
                     nosa_meta %>%
                       filter(Method == 'STADEM and DABOM'),
                     by = c('TRT' = 'CommonPopName'))

    tmp_df <- tmp_df %>%
      mutate(NullRecord = ifelse(is.na(N), 'Yes', 'No'),
             #ShadowId = replicate(n(), guid(uppercase = FALSE)),
             Age2Prop =  NA,
             Age2PropLowerLimit =  NA,
             Age2PropUpperLimit =  NA,
             Age3Prop =  NA,
             Age3PropLowerLimit =  NA,
             Age3PropUpperLimit =  NA,
             Age4Prop =  NA,
             Age4PropLowerLimit =  NA,
             Age4PropUpperLimit =  NA,
             Age5Prop =  NA,
             Age5PropLowerLimit =  NA,
             Age5PropUpperLimit =  NA,
             Age6Prop =  NA,
             Age6PropLowerLimit =  NA,
             Age6PropUpperLimit =  NA,
             Age7Prop =  NA,
             Age7PropLowerLimit =  NA,
             Age7PropUpperLimit =  NA,
             Age8Prop =  NA,
             Age8PropLowerLimit =  NA,
             Age8PropUpperLimit =  NA,
        Age9Prop =  NA,
        Age9PropLowerLimit =  NA,
        Age9PropUpperLimit =  NA,
        Age10Prop =  NA,
        Age10PropLowerLimit =  NA,
        Age10PropUpperLimit =  NA,
        Age11PlusProp =  NA,
        Age11PlusPropLowerLimit = NA,
        Age11PlusPropUpperLimit =  NA,
        NOSAEJ = NA,
        NOSAEJLowerLimit = NA,
        NOSAEJUpperLimit = NA,
        NOSAEJAlpha = NA,
        NOBroodStockRemoved = NA,
        pHOSij = NA,
        pHOSijLowerLimit = NA,
        pHOSijUpperLimit = NA,
        pHOSijAlpha = NA,
        pHOSej = NA,
        pHOSejLowerLimit = NA,
        pHOSejUpperLimit = NA,
        pHOSejAlpha = NA,
        NOSJF = NA,
        NOSJFLowerLimit = NA,
        NOSJFUpperLimit = NA,
        NOSJFAlpha = NA,
        HOSJF = NA,
        TSAIJ = NA,
        TSAIJLowerLimit = NA,
        TSAIJUpperLimit = NA,
        TSAIJAlpha = NA,
        TSAEJ = NA,
        TSAEJLowerLimit = NA,
        TSAEJUpperLimit = NA,
        TSAEJAlpha = NA,
        MethodAdjustments = NA,
        UpdDate = Sys.time(),
        alpha = alpha
      )


    tmp_df <- tmp_df %>%
      select(#ShadowId,
        CommonName,
        Run = CAX_Run,
        RecoveryDomain,
        ESU_DPS,
        MajorPopGroup,
        PopID,
        CBFWApopName,
        CommonPopName = TRT,
        PopFit,
        PopFitNotes,
        EstimateType,
        EstimateTypeNotes,
        WaterBody,
        SpawningYear = spawn_yr,
        TRTmethod,
        ContactAgency,
        MethodNumber,
        BestValue,
        NOSAIJ = N,
        NOSAIJLowerLimit = lowerCI,
        NOSAIJUpperLimit = upperCI,
        NOSAIJAlpha = alpha,
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
        AgePropAlpha = alpha,
        ProtMethName,
        ProtMethURL,
        ProtMethDocumentation,
        MethodAdjustments,
        OtherDataSources,
        Comments,
        NullRecord,
        DataStatus,
        LastUpdated = EffDt,
        IndicatorLocation,
        MetricLocation,
        MeasureLocation,
        ContactPersonFirst,
        ContactPersonLast,
        ContactPhone,
        ContactEmail,
        MetaComments = Method,
        SubmitAgency,
        RefID,
        UpdDate,
        DataEntry,
        DataEntryNotes,
        CompilerRecordID,
        Publish
      )

  }

  tmp_df <- tmp_df %>%
    mutate_at(vars(
           'NOSAIJ',
           'NOSAIJLowerLimit',
           'NOSAIJUpperLimit',
           'NOSAEJ',
           'NOSAEJLowerLimit',
           'NOSAEJUpperLimit',
           'TSAIJ',
           'TSAIJLowerLimit',
           'TSAIJUpperLimit',
           'TSAEJ',
           'TSAEJLowerLimit',
           'TSAEJUpperLimit'), round) %>%
    mutate_at(vars(
           'pHOSij',
           'pHOSijLowerLimit',
           'pHOSijUpperLimit',
           'pHOSej',
           'pHOSejLowerLimit',
           'pHOSejUpperLimit',
           'NOSJF',
           'NOSJFLowerLimit',
           'NOSJFUpperLimit',
           'HOSJF',
           'Age2Prop',
           'Age2PropLowerLimit',
           'Age2PropUpperLimit',
           'Age3Prop',
          'Age3PropLowerLimit',
           'Age3PropUpperLimit',
           'Age4Prop',
           'Age4PropLowerLimit',
           'Age4PropUpperLimit',
           'Age5Prop',
           'Age5PropLowerLimit',
           'Age5PropUpperLimit',
           'Age6Prop',
           'Age6PropLowerLimit',
           'Age6PropUpperLimit',
          'Age7Prop',
          'Age7PropLowerLimit',
          'Age7PropUpperLimit',
          'Age8Prop',
          'Age8PropLowerLimit',
          'Age8PropUpperLimit',
          'Age9Prop',
          'Age9PropLowerLimit',
          'Age9PropUpperLimit',
          'Age10Prop',
          'Age10PropLowerLimit',
          'Age10PropUpperLimit',
          'Age11PlusProp',
          'Age11PlusPropLowerLimit',
          'Age11PlusPropUpperLimit'), round, 3) %>%
    mutate_all(as.character()) %>%
    #mutate_at(vars(contains('Alpha')),
    #         ~ if_else(tmp_df$NullRecord == 'Yes' | is.na(tmp_df$NOSAIJLowerLimit),NA_character_,alpha)) %>%
    mutate(NOSAIJAlpha = if_else(NullRecord == 'Yes' | is.na(NOSAIJLowerLimit),NA_character_,alpha),
           NOSAEJAlpha = if_else(NullRecord == 'Yes' | is.na(NOSAEJLowerLimit),NA_character_,alpha),
           pHOSijAlpha = if_else(NullRecord == 'Yes' | is.na(pHOSijLowerLimit),NA_character_,alpha),
           pHOSejAlpha = if_else(NullRecord == 'Yes' | is.na(pHOSejLowerLimit),NA_character_,alpha),
           NOSJFAlpha = if_else(NullRecord == 'Yes' | is.na(NOSJFLowerLimit),NA_character_,alpha),
           TSAIJAlpha = if_else(NullRecord == 'Yes' | is.na(TSAIJLowerLimit),NA_character_,alpha),
           TSAEJAlpha = if_else(NullRecord == 'Yes' | is.na(TSAEJLowerLimit),NA_character_,alpha),
           AgePropAlpha = if_else(NullRecord == 'Yes' | is.na(Age4PropLowerLimit),NA_character_,alpha),
           ) %>%





    mutate(Comments = ifelse(NullRecord == 'Yes', 'Insuffient spawning ground data to complete estimates.', Comments))
    # mutate(NOSAIJAlpha = case_when(NullRecord == 'Yes' ~ NA_character_,
    #                             is.na(NOSAIJLowerLimit) ~ NA_character_,
    #                             NullRecord == 'No' ~ NOSAIJAlpha)
    #        )

  return(tmp_df)
}
