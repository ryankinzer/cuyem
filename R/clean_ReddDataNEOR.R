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

# NOTE: fields not captured from carcass query: "Subbasin"

# clean
data_clean <- data %>%
  mutate(
    ESU_DPS = 'Snake River Spring/Summer-run Chinook Salmon ESU',
    MPG = 'Grande Ronde / Imnaha',
    POP_NAME = case_when(
      River %in% c('Big Sheep Creek', 'Lick Creek', 'Little Sheep Creek') ~ 'Big Sheep Creek',
      River == 'Imnaha River' ~ 'Imnaha River mainstem',
      River %in% c('Bear Creek', 'Hurricane Creek', 'Lostine River', 'Parsnip Creek', 'Prairie Creek', 'Spring Creek', 'Wallowa River') ~ 'Lostine River',
      River == 'Minam River' ~ 'Minam River',
      River == 'Wenaha River' ~ 'Wenaha River'
    ),
    TRT_POPID = case_when(
      River %in% c('Bear Creek', 'Hurricane Creek', 'Lostine River', 'Parsnip Creek', 'Prairie Creek', 'Spring Creek', 'Wallowa River') ~ 'GRLOS',
      River == 'Minam River' ~ 'GRMIN',
      River == 'Wenaha River' ~ 'GRWEN',
      River %in% c('Big Sheep Creek', 'Lick Creek', 'Little Sheep Creek') ~ 'IRBSH',
      River == 'Imnaha River' ~ 'IRMAI'
    ),
    Species = 'Chinook salmon',
    Run = 'Spring/summer',
    ReportingGroup = case_when(  # in between tributary and population: transect/tributary/reporting group/population/mpg/esu
      River %in% c('Big Sheep Creek', 'Lick Creek','Little Sheep Creek') ~ 'Big Sheep Creek',
      River == 'Imnaha River' ~ 'Imnaha River',
      River == 'Lostine River' ~ 'Lostine River',
      River == 'Minam River' ~ 'Minam River',
      River %in% c('Bear Creek', 'Hurricane Creek', 'Parsnip Creek', 'Prairie Creek', 'Spring Creek', 'Wallowa River') ~ 'Wallowa River',
      River == 'Wenaha River' ~ 'Wenaha River'
    ),
    StreamName = River,
    TribToName = case_when(
      River %in% c('Little Sheep Creek', 'Lick Creek') ~ 'Big Sheep Creek',
      River %in% c('Wallowa River','Wenaha River') ~ 'Grande Ronde River',
      River == 'Big Sheep Creek' ~ 'Imnaha River',
      River == 'Imnaha River' ~ 'Snake River',
      River %in% c('Bear Creek', 'Lostine River', 'Hurricane Creek', 'Minam River', 'Prairie Creek', 'Parsnip Creek', 'Spring Creek') ~ 'Wallowa River'
    ),
    LocationLabel = Section,
    TransectName = SiteID,
    SurveyDate = mdy(gsub(' 0:00:00', '', SurveyDate)),
    SurveyYear = year(SurveyDate),
    ActivityDate = paste0(SurveyDate, 'T00:00:00'),
    TargetSpecies = 'Chinook salmon',
    Pass = NA_integer_,
    StartSurvey = NA_character_,
    EndSurvey = NA_character_,
    StartTime = Start_Time,
    EndTime = End_Time,
    Observers = Surveyors,
    SurveyMethod = 'Ground',
    GPSUnit = GPSnumber,
    Datum = NA_character_, # No record of this. WGS84?
    Weather = NA_character_,
    Visibility = Visibility,
    SurveyComments = paste0('Survey_Type: ', Survey_Type, '; ', Comments_SurveyEvent),
    PreviousRedds = NA_integer_,
    NewRedds = NewRedds,
    FieldsheetLink = NA_character_,
    ReddSpecies = 'Chinook salmon',
    Count = NA_integer_,
    WPTName = as.character(WptID_AutoNum),
    Latitude = as.character(Lat),
    Longitude = as.character(Long),
    WPTType = 'New Redd',
    WPTComments = Notes,
    QAStatusId = NA_integer_,
    ActivityQAStatusId = NA_integer_,
    ActivityQAComments = NA_character_,
    DatasetId = NA_integer_,
    LocationId = NA_integer_, # If needed we could apply a unique numeric identifier to the tbl_Sites for use here.
    ActivityId = SurveyID,  # tbl_SurveyEvents: SurveyID=ReddID
    QAStatusName = NA_character_,
    ProjectId = 11059, # GRSME: 11059
    EffDt = NA_character_,
    Year = Year,
    AboveWeir = case_when(
      is.na(AboveOrBelowWeir) | AboveOrBelowWeir == '' ~ NA_character_,
      AboveOrBelowWeir %in% c('Above Weir', 'Diversion') ~ 'Yes',
      AboveOrBelowWeir %in% c('Below Weir', 'BeforeWeir', 'No Weir', 'No weir', 'Now Weir') ~ 'No',
      TRUE ~ 'Unknown'
    ),
    AbovePITArray = 'Yes',  # WR2 = Wallowa River Site, Wenaha=Yes, Minam=Yes. Imnaha=Yes.
    AboveRST = case_when(
      River %in% c('Wenaha River','Wallowa River') ~ 'No',
      TribToName == 'Wallowa River' & !River %in% c('Minam River','Lostine River') ~ 'No',
      River == 'Lostine River' & SiteID %in% c('LOS8','LOS8.1','LOS8.2','LOSW','LOSTULLEY') ~ 'No',
      TRUE ~ 'Yes'
  )) %>%
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
