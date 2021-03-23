#' @title Clean North East Oregon Carcass Data - from ODFW Access DB
#' @description Processes the raw ODFW Access DB carcass dataset and standardizes to join with clean_carcassData(CDMS_dat)
#' @param data Data obtained from premade query in ODFW Access DB. !!Export data as text file, comma separated, headers included.!!
#' @param data Import text file with:  read.delim(file = 'path_to_file.txt', sep = ',', header = TRUE)
#' @export
#' @import dplyr lubridate
#' @author Tyler T. Stright
#' @examples
#' clean_carcass_Data(car_dat)
#'

clean_carcassData_NEOR <- function(data){
  {if(is.null(data))stop("carcass data must be supplied")}

# NOTE: Fields not captured from carcass query: "Subbasin" "MEPSlength" "CWTAge" "BestAge" "PITage" "LengthAge" "AgeKey"
# "PIT2" "BestScaleAge" "Adult_or_Jack" "MarkRecapSizeCategory" "TagFile" "ExternalMarks" "Population"

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
    SurveyDate = ymd(gsub('T00:00:00', '', SurveyDate)),
    SurveyYear = year(SurveyDate),
    ActivityDate = paste0(SurveyDate, 'T00:00:00'),
    ActivityId = as.integer(SurveyID),
    DatasetId = NA_integer_,
    LocationId = NA_integer_,
    TargetSpecies = 'S_CHN',
    Pass = NA_integer_,
    StartSurvey = NA_character_,
    EndSurvey = NA_character_,
    StartTime = Start_Time,
    EndTime = End_Time,
    Observers = Surveyors,
    SurveyMethod = 'Ground',
    GPSUnit = NA_character_, # No GPS for Carcasses.
    Datum = NA_character_,
    Weather = NA_character_,
    Visibility = Visibility,
    SurveyComments = paste0('Survey_Type: ', Survey_Type, '; ', Comments_SurveyEvent),
    SampleNumber = GeneticsNumber,
    HistoricSampleNumber = NA_character_,
    CarcassSpecies = 'S_CHN',
    Sex = case_when(
      Sex == 'M' ~ 'Male',
      Sex == 'F' ~ 'Female',
      Sex == 'J' ~ 'Jack',
      Sex %in% c('Unk', 'UNK') ~ 'Unknown'
    ),
    ForkLength = ForkLength,
    PercentSpawned = if_else(Sex == 'M', NA_integer_, as.integer(round(PercentSpawned, 0))),
    # SpawnedOut = case_when(   # First Go - remove if other logic is best
    #   PreSpawn == 'Spawned' ~ 'Yes',
    #   PreSpawn == 'PreSpawn' ~ 'No',
    #   PreSpawn %in% c('', 'NotValid', 'Unknown') ~ 'Unknown',
    #   TRUE ~ NA_character_
    # ),
    SpawnedOut = case_when(   # anti-PrespawnMort?
      PercentSpawned < 50  ~ 'No', # Indicates a Prespawn Mortality
      PercentSpawned >= 50 ~ 'Yes', # Successful Spawner
      TRUE ~ NA_character_
    ),
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
      grepl('unk', FinMark, ignore.case = T) | FinMark == '' ~ 'Unknown',
      TRUE ~ 'No'
    ),
    CWTScanned = if_else(`CWT(Y/N)` == 'Unk', 'Unknown', `CWT(Y/N)`),
    SnoutCollected = case_when(   # This may need to be revisited
      grepl('\\d{2}[[:alpha:]]{1}\\d{4}', SnoutID) ~ 'Yes',
      grepl('DB\\d{3}', SnoutID) ~ 'Yes',
      TRUE ~ 'No'
    ),
    Fins = NA_character_,
    Scales = NA_character_,
    Otolith = NA_character_,
    Count = as.double(Count),
    CarcassComments = Comments,
    Latitude = NA_character_, # no lat/long for carcasses
    Longitude = NA_character_,
    RadioTag = if_else(RadioTag == 1, 'Yes', 'No'),
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
    MarksVentralFin = NA_character_, # Extract LV/RV from FinMark  ****************
    Notes = NA_character_,
    QAStatusId = NA_integer_,
    CWTCode = CWTcode,
    TagsPetersonDisk = NA_character_,   # didn't see anything in the database
    CarcassWPT = as.character(CarcassID),
    DNACollected = NA_character_,  # won't affect analysis
    ActivityQAStatusId = NA_integer_,
    ActivityQAComments = NA_character_,
    FieldsheetLink = as.logical(NA),
    QAStatusName = NA_character_,
    EffDt = NA_character_,
    Year = as.integer(Year),
    AboveWeir = case_when(
      is.na(AboveOrBelowWeir) | AboveOrBelowWeir == '' ~ NA_character_,
      AboveOrBelowWeir %in% c('Above Weir', 'Diversion','Lostine Weir') ~ 'Yes',
      AboveOrBelowWeir %in% c('Below Weir', 'BeforeWeir', 'No Weir', 'No weir', 'Now Weir') ~ 'No',
      TRUE ~ 'Unknown'
    ),
    AbovePITArray = 'Yes',  # WR2 = Wallowa River Site, Wenaha=Yes, Minam=Yes. Imnaha=Yes.
    AboveRST = case_when(
      River %in% c('Wenaha River','Wallowa River') ~ 'No',
      TribToName == 'Wallowa River' & !River %in% c('Minam River','Lostine River') ~ 'No',
      River == 'Lostine River' & SiteID %in% c('LOS8','LOS8.1','LOS8.2','LOSW','LOSTULLEY') ~ 'No',
      TRUE ~ 'Yes'
    ),
    Origin = case_when(
      Origin %in% c('Nat') ~ 'Natural',
      # HON are non-clipped fish that weren't scanned in 2011. Assumed to be Naturals, but not positive b/c no CWT Scan.
      Origin %in% c('DS.Hat','Hat') ~ 'Hatchery',  # DS.Hat = hatchery determined by Discriminate Scale Analysis...
        # ...where hatchery was determined by the distance between the origin and first annulus..
      TRUE ~ 'Unknown'  # HON = Unknown
    ),
    Mark_Discernible = case_when(
      OPPunch %in% c('Yes','No','yes','no', 'NO')
      & MarkRecapSizeCategory %in% c('Adult','Jack','MiniJ') ~ TRUE,
      TRUE ~ FALSE),
    #OPPunch is the NEOR field for whether the passed upstream mark(s) are discernible on the carcass
    #MarkRecapSizeCategory is the NEOR bread-crumb trail of whether to include/exclude carcasses in MR analyses
    #we could use the MarkRecapSizeCategory criteria during cleaning or when stratifying data for input into cuyem functions
    #using MarkRecapSizeCategory later would be more straightforward, otherwise we are hiding info that doesn't have anything to do
    #with whether the mark is discernable, e.g., it was a radio-tagged carcass that was only found because of it's radio-tag
    #similarly, above/below weir probably doesn't need to be incorporated into this variable
    #but it all depends on how Kinzer wants to use the variable
    Recapture = case_when(
      Mark_Discernible == TRUE & OPPunch %in% c('Yes','yes') ~ TRUE,
      TRUE ~ FALSE),
    CWT_Age = ifelse(CWTAge>0,CWTAge, NA),
    VIE_Age = NA_integer_,
    PIT_Age = ifelse(PITage>0,PITage, NA),
    Fin_Age = NA_integer_,
    Scale_Age = ifelse(BestScaleAge>0,BestScaleAge, NA)
  ) %>%
  # filter(MarkRecapSizeCategory %in% c('Adult','Adult NA')) %>%  # This probably won't live here forever.
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
    Recapture,
    CWT_Age,
    VIE_Age,
    PIT_Age,
    Fin_Age,
    Scale_Age)

return(data_clean)

}
