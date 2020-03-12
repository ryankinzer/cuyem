#' @title Clean Carcass Data
#' @description Processes the raw CDMS carcass dataset and adds important fields for summaries (e.g., Survey Year, Reporting Group, Transect Name, Above Weir, Origin, Mark and Recapture etc.)
#' @param data raw CDMS carcass dataset from \code{cdmsR::getDatasetView(datastore = 79)} or from CDMS dataset export
#' @export
#' @import dplyr
#' @author Ryan N. Kinzer
#' @examples
#' car_dat <- cdmsR::getDatasetView(datastore = 79)
#' clean_carcassData(car_dat)
clean_carcassData <- function(data){
  {if(is.null(data))stop("carcass data must be supplied")}

  dat <- data %>%
    mutate(TransectName = str_split(LocationLabel, 'Transect - ', simplify = TRUE)[,2]) %>%
    left_join(transect_meta, by = c('Species', 'Run', 'StreamName', 'TransectName')) %>%
    mutate(SurveyDate = lubridate::date(lubridate::ymd_hms(SurveyDate)),
           SurveyYear = lubridate::year(SurveyDate),
           ForkLength = ifelse(ForkLength == -99, NA, ForkLength)) %>%
    mutate(Count = ifelse(is.na(Count) & !is.na(SampleNumber), 1,
                          ifelse(is.na(Count), 0, Count)))

  dat <- dat %>%
    mutate(Origin = case_when(AdiposeFinClipped == 'Yes' ~ 'Hatchery',
                              CWTScanned == 'Yes' ~ 'Hatchery',
                              grepl('RE|LE|Yes', TagsVIE) ~ 'Hatchery',
                              grepl('LV|RV', MarksVentralFin) ~ 'Hatchery',

                              AdiposeFinClipped == 'Unknown' ~ 'Unknown',
                              CWTScanned == 'Unknown' ~ 'Unknown',
                              TagsVIE == 'Unknown' ~ 'Unknown',
                              MarksVentralFin == 'Unknown' ~ 'Unknown',

                              grepl('No|NA', AdiposeFinClipped) &
                              grepl('No|NA', CWTScanned) &
                              !grepl('RE|LE|Yes|Unknown', TagsVIE) &
                              !grepl('LV|RV|Unknown', MarksVentralFin) ~ 'Natural',

                              TRUE ~ NA_character_)
            )

  dat <- dat %>%
    mutate(OpercleLeft = ifelse(OpercleLeft == 'LOPN', 'LON', OpercleLeft)) %>%
    mutate(OpercleRight = ifelse(OpercleRight == 'ROPN', 'RON', OpercleRight)) %>%
    mutate(Mark_Discernible = case_when(AboveWeir == 'Yes' & grepl('LOP|No', OpercleLeft) ~ TRUE,
                                        AboveWeir == 'Yes' & grepl('ROP|No', OpercleRight) ~ TRUE,
                                        AboveWeir == 'Yes' & grepl('OP|Lost|No', TagsPetersonDisk) ~ TRUE,
                                        AboveWeir == 'Yes' & str_detect(TagsPetersonDisk, '\\d') ~ TRUE,
                                        AboveWeir == 'Yes' & grepl('OP|Lost|Yes|No', TagsStaple) ~ TRUE,
                                   TRUE ~ FALSE)) %>%
    mutate(Recapture = case_when(AboveWeir == 'Yes' & grepl('LOP', OpercleLeft) ~ TRUE,
                                 AboveWeir == 'Yes' & grepl('ROP', OpercleRight) ~ TRUE,
                                 AboveWeir == 'Yes' & grepl('OP|Lost', TagsPetersonDisk) ~ TRUE,
                                 AboveWeir == 'Yes' & str_detect(TagsPetersonDisk, '\\d') ~ TRUE,
                                 AboveWeir == 'Yes' & grepl('OP|Lost|Yes', TagsStaple) ~ TRUE,
                                TRUE ~ FALSE)) %>%
    select(ESU_DPS:Run, ReportingGroup, StreamName, TribToName, LocationLabel, TransectName, SurveyYear, everything())

  return(dat)

  }
