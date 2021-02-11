#' @title Clean North East Oregon Carcass Data - from ODFW Access DB
#' @description Processes the raw ODFW Access DB carcass dataset and standardizes to join with clean_carcassData(CDMS_dat)
#' @param data Data obtained from premade query in ODFW Access DB. !!Export data as text file, comma separated, headers included.!!
#' @param data Import text file with:  read.delim(file = 'path_to_file.txt', sep = ',', header = TRUE)
#' @export
#' @import dplyr, lubridate
#' @author Tyler T. Stright
#' @examples
#' clean_carcassData(car_dat)
#'

clean_carcassDataNEOR <- function(data){
  {if(is.null(data))stop("carcass data must be supplied")}

# NOTE: Fields not captured from carcass query: "Subbasin" "MEPSlength" "CWTAge" "BestAge" "PITage" "LengthAge" "AgeKey"
# "PIT2" "BestScaleAge" "Adult_or_Jack" "MarkRecapSizeCategory" "TagFile" "ExternalMarks" "Population"

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
    ActivityId = as.integer(SurveyID),
    DatasetId = NA_integer_,
    LocationId = NA_integer_,
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
    SurveyComments = paste0('Survey_Type: ', Survey_Type, ';'),
    SampleNumber = GeneticsNumber,  # not 100% accurate
    HistoricSampleNumber = NA_character_,
    CarcassSpecies = 'Chinook salmon',
    Sex = case_when(
      Sex == 'M' ~ 'Male',
      Sex == 'F' ~ 'Female',
      Sex == 'J' ~ 'Jack',
      Sex %in% c('Unk', 'UNK') ~ 'Unknown'
    ),
    ForkLength = ForkLength,
    SpawnedOut = case_when(
      PreSpawn == 'Spawned' ~ 'Yes',
      PreSpawn == 'PreSpawn' ~ 'No',
      PreSpawn %in% c('', 'NotValid', 'Unknown') ~ 'Unknown',
      TRUE ~ NA_character_
    ),
    PercentSpawned = as.integer(round(PercentSpawned, 0)),
    OpercleLeft = if_else(grepl('LOP', OperclePunchType, ignore.case = T),
                          str_extract(OperclePunchType, '\\d?\\s*LOP'), NA_character_),
    OpercleRight = if_else(grepl('ROP', OperclePunchType, ignore.case = T),
                           str_extract(OperclePunchType, '\\d?\\s*ROP'), NA_character_),
    PITScanned = case_when(
      PITscan == 'PIT tag present' ~ 'Yes',
      PITscan == 'No PIT tag' ~ 'No',
      PITscan == 'Not Scanned' ~ 'Unknown',
    ),
    PITCode = PIT1, # no data in PIT2
    AdiposeFinClipped = case_when(  # Assuming all Hatchery fish are ad-clipped
      grepl('ad', FinMark, ignore.case = T) ~ 'Yes',
      Origin %in% c('Hat', 'DS.Hat', 'HON') ~ 'Yes',
      Origin %in% c('Nat') | grepl('none', FinMark, ignore.case = T) ~ 'No',
      TRUE ~ 'Unknown'
    ),
    CWTScanned = NA_character_,  #"CWT Y.N." doesn't really provide enough clarity
    SnoutCollected = case_when(
      SnoutID %in% c('CWT not Present', 'CWT not present', 'No CWT', 'Not taken', 'Not taken', NA, 'None', 'CWT Unk', 'Deleted') ~ 'No',  # this may not be perfect
      TRUE ~ 'Yes'
    ),
    Fins = NA_character_,
    Scales = NA_character_,
    Otolith = NA_character_,
    Count = as.double(Count),
    CarcassComments = Comments,
    Latitude = NA_character_,
    Longitude = NA_character_,
    RadioTag = as.character(RadioTag),
    TransmitterType = NA_character_,
    Vendor = NA_character_,
    SerialNumber = NA_character_,
    Frequency = NA_character_,
    Channel = NA_character_,
    Code = NA_character_,
    TagsFloy = NA_character_,
    TagsVIE = NA_character_,
    TagsJaw = NA_character_,
    TagsStaple = NA_character_, # didn't see anything in the database
    TagsSpaghetti = NA_character_,
    MarksVentralFin = NA_character_,
    Notes = NA_character_,
    QAStatusId = NA_integer_,
    CWTCode = CWTcode,
    TagsPetersonDisk = NA_character_,   # didn't see anything in the database
    CarcassWPT = as.character(CarcassID),
    DNACollected = NA_character_,
    ActivityQAStatusId = NA_integer_,
    ActivityQAComments = NA_character_,
    FieldsheetLink = as.logical(NA),
    QAStatusName = NA_character_,
    EffDt = NA_character_,
    Year = as.integer(Year),
    AboveWeir = case_when(
      is.na(AboveOrBelowWeir) ~ NA_character_,
      AboveOrBelowWeir == 'Above Weir' ~ 'Yes',
      AboveOrBelowWeir %in% c('Below Weir', 'BeforeWeir', 'No Weir', '') ~ 'No',
      AboveOrBelowWeir == 'Diversion' & Section %in% c('Foster Diversion', 'Miles Ditch','Sheep Ridge Ditch: Diversion to Fish Wheel') ~ 'Yes', # confirmed on map
      TRUE ~ 'Unknown'
    ),
    AbovePITArray = 'Yes',
    AboveRST = 'Yes',
    Origin = Origin,
    Mark_Discernible = case_when(AboveWeir == 'Yes' & grepl('LOP|No', OpercleLeft) ~ TRUE,
                                 AboveWeir == 'Yes' & grepl('ROP|No', OpercleRight) ~ TRUE, # Source of potential problem for JCAPE - includes fish when opercle right is possible, when only we need to look at opercle left.
                                 AboveWeir == 'Yes' & grepl('OP|Lost|No', TagsPetersonDisk) ~ TRUE,
                                 AboveWeir == 'Yes' & str_detect(TagsPetersonDisk, '\\d') ~ TRUE,
                                 AboveWeir == 'Yes' & grepl('OP|Lost|Yes|No', TagsStaple) ~ TRUE,
                                 TRUE ~ FALSE),
    Recapture = case_when(AboveWeir == 'Yes' & grepl('LOP', OpercleLeft) ~ TRUE,
                          AboveWeir == 'Yes' & grepl('ROP', OpercleRight) ~ TRUE,
                          AboveWeir == 'Yes' & grepl('OP|Lost', TagsPetersonDisk) ~ TRUE,
                          AboveWeir == 'Yes' & str_detect(TagsPetersonDisk, '\\d') ~ TRUE,
                          AboveWeir == 'Yes' & grepl('OP|Lost|Yes', TagsStaple) ~ TRUE,
                          TRUE ~ FALSE)
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
    ActivityId,
    DatasetId,
    LocationId,
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
    SampleNumber,
    HistoricSampleNumber,
    CarcassSpecies,
    Sex,
    ForkLength,
    SpawnedOut,
    PercentSpawned,
    OpercleLeft,
    OpercleRight,
    PITScanned,
    PITCode,
    AdiposeFinClipped,
    CWTScanned,
    SnoutCollected,
    Fins,
    Scales,
    Otolith,
    Count,
    CarcassComments,
    Latitude,
    Longitude,
    RadioTag = RadioTag,
    TransmitterType,
    Vendor,
    SerialNumber,
    Frequency,
    Channel,
    Code,
    TagsFloy,
    TagsVIE,
    TagsJaw,
    TagsStaple,
    TagsSpaghetti,
    MarksVentralFin,
    Notes,
    QAStatusId,
    CWTCode,
    TagsPetersonDisk,
    CarcassWPT,
    DNACollected,
    ActivityQAStatusId,
    ActivityQAComments,
    FieldsheetLink,
    QAStatusName,
    EffDt,
    Year,
    AboveWeir,
    AbovePITArray,
    AboveRST,
    Origin,
    Mark_Discernible,
    Recapture
  )

return(data_clean)

}
