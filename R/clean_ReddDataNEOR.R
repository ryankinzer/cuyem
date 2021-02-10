#' @title Clean North East Oregon Redd Data - from ODFW Access DB
#' @description Processes the raw ODFW Access DB carcass dataset and standardizes to join with clean_reddData(CDMS_dat)
#' @param data Data obtained from premade query in ODFW Access DB. !!Export data as text file, comma delim, headers included.!!
#' @param data Import text file with:  read.delim(file = 'path_to_file.txt', sep = ',', header = TRUE)
#' @export
#' @import dplyr, lubridate
#' @author Tyler T. Stright
#' @examples
#' clean_reddDataNEOR(redd_dat)
#'

clean_reddDataNEOR <- function(data){
  {if(is.null(data))stop("carcass data must be supplied")}

# NOTE: fields not captured from carcass query: "Subbasin"  'SiteID'

# clean
data_clean <- data %>%
  mutate(
    ESU_DPS = 'Snake River Spring/Summer-run Chinook Salmon ESU',
    MPG = 'Grande Ronde / Imnaha',
    POP_NAME = case_when(
      River %in% c('Bear Creek', 'Hurricane Creek', 'Lostine River', 'Parsnip Creek', 'Prairie Creek', 'Wallowa River') ~ 'Lostine River',
      River %in% c('Big Sheep Creek', 'Lick Creek', 'Little Sheep Creek') ~ 'Big Sheep Creek',
      River %in% c('Imnaha River', 'Spring Creek') ~ 'Imnaha River mainstem'
    ),
    TRT_POPID = case_when(
      River %in% c('Bear Creek', 'Hurricane Creek', 'Lostine River', 'Parsnip Creek', 'Prairie Creek', 'Wallowa River') ~ 'GRLOS',
      River %in% c('Big Sheep Creek', 'Lick Creek', 'Little Sheep Creek') ~ 'IRBSH',
      River %in% c('Imnaha River', 'Spring Creek') ~ 'IRMAI'
    ),
    Species = 'Chinook salmon',
    Run = 'Spring/summer',
    ReportingGroup = case_when(
      River %in% c('Lick Creek','Little Sheep Creek') ~ 'Big Sheep Creek',
      River == 'Wallowa River' ~ 'Grande Ronde River',
      River %in% c('Spring Creek', 'Big Sheep Creek') ~ 'Imnaha River',
      River == 'Imnaha River' ~ 'Snake River',
      River %in% c('Bear Creek', 'Hurricane Creek', 'Lostine River', 'Parsnip Creek', 'Prairie Creek') ~ 'Wallowa River'
    ),
    StreamName = River,
    TribToName = case_when(
      River == 'Imnaha River' ~ 'Snake River',
      River %in% c('Bear Creek', 'Lostine River', 'Hurricane Creek', 'Prairie Creek', 'Parsnip Creek') ~ 'Wallowa River',
      River == 'Wallowa River' ~ 'Grande Ronde River',
      River %in% c('Little Sheep Creek', 'Lick Creek') ~ 'Big Sheep Creek',
      River %in% c('Big Sheep Creek', 'Spring Creek') ~ 'Imnaha River'
    ),
    LocationLabel = Section,
    TransectName = NA_character_,
    SurveyDate = mdy(gsub(' 0:00:00', '', SurveyDate)),
    SurveyYear = year(SurveyDate),
    ActivityDate = paste0(SurveyDate, 'T00:00:00'), # redundant
    TargetSpecies = 'Chinook salmon',
    Pass = NA_integer_,
    StartSurvey = NA_character_,
    EndSurvey = NA_character_,
    StartTime = NA_character_,
    EndTime = NA_character_,
    Observers = NA_character_,
    SurveyMethod = 'Ground',
    GPSUnit = NA_character_,
    Datum = NA_character_,
    Weather = NA_character_,
    Visibility = NA_character_,
    SurveyComments = paste0('Site_Classification: ', Site_Classification, '; ', 'Survey_Type: ', Survey_Type, ';'), # metadata capture
    PreviousRedds = NA_integer_,
    NewRedds = NewRedds,
    FieldsheetLink = NA_character_,
    ReddSpecies = 'Chinook salmon',
    Count = NA_integer_,
    WPTName = as.character(WptAuto),
    Latitude = as.character(Latitude),
    Longitude = as.character(Longitude),
    WPTType = 'New Redd',  # They don't record prev redds, so all new?
    WPTComments = NA_character_,
    QAStatusId = NA_integer_,
    ActivityQAStatusId = NA_integer_,
    ActivityQAComments = NA_character_,
    DatasetId = NA_integer_,
    LocationId = NA_integer_, # SiteID can't be git into here as integer,  # is this correct?
    ActivityId = ReddID,  # this should be correct.
    QAStatusName = NA_character_,
    ProjectId = NA_integer_,
    EffDt = NA_character_,
    Year = Year,
    AboveWeir = case_when(
      is.na(AboveOrBelowWeir) ~ NA_character_,
      AboveOrBelowWeir == 'Above Weir' ~ 'Yes',
      AboveOrBelowWeir %in% c('Below Weir', 'BeforeWeir', 'No Weir', '') ~ 'No',
      AboveOrBelowWeir == 'Diversion' & Section %in% c('Foster Diversion', 'Miles Ditch','Sheep Ridge Ditch: Diversion to Fish Wheel') ~ 'Yes', # confirmed on map
      TRUE ~ 'Unknown'
    ),
    AbovePITArray = 'Yes',
    AboveRST = 'Yes'
  ) %>%
  select(
    ESU_DPS,
    MPG,
    POP_NAME,
    TRT_POPID,
    Species,
    Run,
    ReportingGroup,
    StreamName,
    TribToName,
    LocationLabel,
    TransectName,
    SurveyYear,
    SurveyDate,
    ActivityDate,
    TargetSpecies,
    Pass,
    StartSurvey,
    EndSurvey,
    StartTime,
    EndTime,
    Observers,
    SurveyMethod,
    GPSUnit,
    Datum,
    Weather,
    Visibility,
    SurveyComments,
    PreviousRedds,
    NewRedds,
    FieldsheetLink,
    ReddSpecies,
    Count,
    WPTName,
    Latitude,
    Longitude,
    WPTType,
    WPTComments,
    QAStatusId,
    ActivityQAStatusId,
    ActivityQAComments,
    DatasetId,
    LocationId,
    ActivityId,
    QAStatusName,
    ProjectId,
    EffDt,
    Year,
    AboveWeir,
    AbovePITArray,
    AboveRST
  )

return(data_clean)
}
