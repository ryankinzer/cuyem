#' @title Carcass Age Data
#' @description Add scale, age fin, CWT, PIT and VIE age data to carcasses. Scale and age fin data is stored in the CDMS age summary dataset and connected to carcasses using stream name, survey date and sample number. CWT, PIT and VIE age data is supplied by the user and connect through code numbers (CWT and PIT) and stream name and return year for VIE marks.
#' @param carcass_data cleaned CDMS carcass dataset
#' @param age_data cleaned CDMS age summary dataset
#' @param rmis_codes RMIS release coded wire codes
#' @param pit_codes PTAGIS tagging detail query
#' @param vie_marks VIE marks applied by release year and potential return year
#'
#' @author Ryan N. Kinzer
#' @return
#' @import
#' dplyr
#' lubridate
#' @export
#'
#' @examples
#' car_dat <- getDatasetView(datastoreID = 79)
#' car_dat <- clean_carcassData(car_dat)
#' age_dat <- getDatasetView(datastoreID = 80)
#' get_carcassAge(car_dat, age_dat)
get_carcassAge <- function(carcass_data, age_data = NULL, rmis_codes = NULL, pit_codes = NULL, vie_marks = NULL){

  {if(is.null(carcass_data))stop("carcass data must be supplied")}

  #----------------------------------------------------------------------------
  # Prep carcass data and create a unique id
  #----------------------------------------------------------------------------
  c_dat <-  carcass_data #clean_carcassData(carcass_data) # comment out when complete
  o_names <- names(c_dat)

  c_dat <- c_dat %>% # need sample_id_sgs
    mutate(a_SampleNumber = ifelse(SampleNumber == '-999', NA, SampleNumber),
           a_HistoricSampleNumber = str_trim(HistoricSampleNumber),
           a_HistoricSampleNumber = gsub('--','-',a_HistoricSampleNumber),
           a_HistoricSampleNumber = ifelse(a_HistoricSampleNumber == 'NA', NA, a_HistoricSampleNumber),
           a_HistoricSampleNumber = ifelse(nchar(a_HistoricSampleNumber) == 0, NA, a_HistoricSampleNumber)) %>%
    separate(a_HistoricSampleNumber, into = paste0('s',1:6), sep = '-', remove = FALSE) %>%
    mutate(h_id = case_when(
      !is.na(s6) ~ s6,
      !is.na(s5) ~ s5,
      !is.na(s4) ~ s4,
      !is.na(s3) ~ s3,
      !is.na(s2) ~ s2,
      !is.na(s1) ~ s1)) %>%
    mutate(c_id = str_pad(a_SampleNumber, 4, pad = '0'),
           h_id = str_pad(h_id, 4, pad = '0'),
           id = ifelse(is.na(a_HistoricSampleNumber), a_SampleNumber, a_HistoricSampleNumber),
           sample_id_sgs = paste0(StreamName, '-', SurveyDate, '-', c_id)) %>%
    select(sample_id_sgs, o_names)

  if(!is.null(age_data)){
  #------------------------------------------------------------------------------
  # Append Fins and Scale age data to all carcass records. - currently comes from
  # summary age dataset but should be routed from bio_samples or own scale repos.
  #------------------------------------------------------------------------------

  #age_data <- cdmsR::getDatasetView(datastoreID = 80) # age
  a_dat <- age_data

  a_dat <- a_dat %>%
    filter(AgeDetermination %in% c('Fin', 'Scale')) %>%
    filter(CollectionRepository == 'NPT-SGS') %>%
    filter(SampleNumber != -999 & SampleNumber != 'NA') %>%
    # select(CollectionRepository, SampleNumber, UniqueFishID, OtherId, OtherIdType, AnalysisId, StreamName, LocationLabel, Species, Run, Origin, TargetFish, Sex, CollectionDate, ForkLength, Lifestage, LifeHistory, PITCode, CWTCode, AgeDetermination, AgeOrigin:AgeingComment) %>%
    separate(SampleNumber, into = paste0('s',1:5), remove = FALSE) %>%
    mutate(o_id = case_when(
      !is.na(s5) ~ s5,
      !is.na(s4) ~ s4,
      !is.na(s3) ~ s3,
      !is.na(s2) ~ s2,
      !is.na(s1) ~ s1)) %>%
    mutate(CollectionDate = lubridate::date(CollectionDate),
           o_id = str_pad(o_id, 4, pad = '0'),
           sample_id_sgs = paste0(StreamName, '-', CollectionDate, '-',  o_id)) %>%
    select(sample_id_sgs, AgeDetermination, TotalAge) %>%
    mutate(TotalAge = ifelse(TotalAge == -99, NA, TotalAge),
           AgeDetermination = paste0(AgeDetermination,'_Age')) %>%
    pivot_wider(names_from = AgeDetermination, values_from = TotalAge)

  c_dat <- left_join(c_dat, a_dat, by = 'sample_id_sgs')
  #anti_join(a_dat, c_dat)
}

  if(!is.null(rmis_codes)){
  #------------------------------------------------------------------------------
  # Append RMIS CWT data for CWT age
  #------------------------------------------------------------------------------

  cwt_df <- rmis_codes %>%
    select(CWTCode = tag_code_or_release_id, brood_year, release_agency,
           release_location_name, hatchery_location_name,
           stock_location_name) %>%
    mutate(CWTCode = as.character(CWTCode))

  c_dat <- left_join(c_dat %>%
                         mutate(pad_CWTCode = str_pad(CWTCode, 6, pad = '0')),
                       cwt_df, by = c("pad_CWTCode" = "CWTCode")) %>%
    mutate(CWT_Age = SurveyYear - brood_year) %>%
    select(-brood_year, -pad_CWTCode)
}

  if(!is.null(vie_marks)){
  #------------------------------------------------------------------------------
  # Append JCAPE VIE data for VIE ages
  #------------------------------------------------------------------------------
  c_dat <- left_join(c_dat %>%
                         mutate(return_year = SurveyYear),
                       vie_marks, by = c('StreamName' = 'release_location','return_year', 'TagsVIE')) %>%
    select(-return_year)
}


if(!is.null(pit_codes)){
  #------------------------------------------------------------------------------
  # Append PTAGIS tagging data for PIT ages
  #------------------------------------------------------------------------------
  pit_dat <- pit_codes %>%
    filter(brood_year != '') %>%
    mutate(brood_year = as.numeric(brood_year)) %>%
    select(PITCode = tag, brood_year, pit_release_site = release_site)

  c_dat <- left_join(c_dat, pit_dat, by = 'PITCode') %>%
    mutate(PIT_Age = SurveyYear - brood_year) %>%
    select(-brood_year)
}

c_dat <- c_dat %>%
  mutate(BestAge = case_when(!is.na(CWT_Age) ~ as.character(CWT_Age),
                             !is.na(PIT_Age) ~ as.character(PIT_Age),
                             !is.na(VIE_Age) ~ as.character(VIE_Age),
                             !is.na(Fin_Age) ~ as.character(Fin_Age),
                             !is.na(Scale_Age) ~ as.character(Scale_Age)
  ))

  return(c_dat)

}
