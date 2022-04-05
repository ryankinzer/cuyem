#' @title clean_P4Data:
#'
#' @description Processes raw P4 data and adds important fields for summaries
#'
#' @param data raw CDMS P4 data from get_P4data()
#'
#' @export
#'
#' @import dplyr
#'
#' @author Tyler T. Stright
#'
#' @examples
#' p4_raw <- get_P4Data()
#' p4_clean <- clean_P4Data(p4_raw)

clean_P4Data <- function(data){
  {if(is.null(data))stop("P4 data must be supplied")}

  # snake_case field names
  x <- names(data)
  x1 <- str_replace_all(x, '([a-z])([A-Z])([a-z])', '\\1_\\2\\3')
  x2 <- str_replace_all(x1, '([a-z])([A-Z])([A-Z])', '\\1_\\2\\3')
  x3 <- str_replace_all(x2, '([A-Z])([A-Z])([a-z])', '\\1_\\2\\3')
  names(data) <- tolower(x3)

  # rename, fix datatypes ----
  data_clean <- data %>%
    rename(
      nfish = pdv1,
      trap_start_datetime = spdv1,
      trap_end_datetime = spdv2,
      hours_sampled = spdv3,
      staff_gauge_cm = spdv4,
      trap_rpm = spdv5,
      weather = spdv6,
      operational_condition = spdv7,
      staff_gauge_ft= spdv8) %>%


    # Do we need to modify other datetime fields?  This is acting strange currently (2/8/22)
    # separate(event_date, into = c('event_date', 'event_time'), sep='T') %>% # this is the only trustworthy datetime, filled and standardized by P4
    separate(event_date, into = c('event_date', 'event_time', 'gmt_offset'), sep = ' ') %>%
    mutate(event_time = gsub('.0000000', '', event_time)) %>%


    # Fixing datatypes
    mutate(event_date = lubridate::ymd(event_date),
           across(.cols = c(trap_start_datetime, trap_end_datetime), lubridate::mdy_hm),
           across(.cols = c(trap_rpm, staff_gauge_ft), as.double),
           across(.cols = c(nfish, staff_gauge_cm), as.integer)) %>%
    mutate(
      pit_tagged = if_else(pit_tag == '..........', FALSE, TRUE),
      mark_recap = case_when(
        # get marks
        event_type == 'Mark' & grepl('\\bTU\\b', text_comments, ignore.case = T) ~ 'mark',
        event_type %in% c('Mark', 'Tally') & grepl('\\bCL\\b|\\bCU\\b', conditional_comments, ignore.case = T) ~ 'mark',
        # get recaps
        grepl('\\bRE\\b', conditional_comments) & grepl('\\bRC\\b', text_comments) ~ 'recap',
        grepl('\\bRE\\b', conditional_comments) & grepl('\\bCL\\b|\\bCU\\b', conditional_comments) ~ 'recap',
        # exclude DS
        grepl('\\bRE\\b', conditional_comments) & grepl('\\bDS\\b', text_comments) ~ 'not used',
        # get unmarks
      TRUE ~ 'unmark'
        ),
      efficiency_mark = case_when(  # Bismark Brown? BBY? ********************
        # Tagged sent upstream
        event_type == 'Mark' & grepl('\\bTU\\b', text_comments, ignore.case = T) ~ TRUE,
        # Caudal Lower/Upper sent upstream
        event_type %in% c('Mark', 'Tally') & grepl('\\bCL\\b|\\bCU\\b', conditional_comments, ignore.case = T) ~ TRUE,
        TRUE ~ FALSE),
      efficiency_recap = case_when(
        event_type %in% c('Recapture', 'Recovery') & grepl('\\bRE\\b', conditional_comments) & grepl('\\bRC\\b', text_comments) ~ TRUE,
        event_type %in% c('Recapture', 'Recovery') & grepl('\\bRE\\b', conditional_comments) & grepl('\\bCL\\b|\\bCU\\b', conditional_comments) ~ TRUE,
        TRUE ~ FALSE),
      mortality = if_else(grepl('\\bM\\b', conditional_comments), TRUE, FALSE),
      origin = case_when(
        grepl('Hat.', srr_verbose)  ~ 'Hatchery',
        grepl('Wild', srr_verbose) ~ 'Natural',
        TRUE ~ NA_character_
      ),
      life_stage = case_when(
        # Pacific Lamprey
        srr_verbose == 'Pacific Lamprey' & grepl('\\bAM\\b', conditional_comments) ~ 'Ammocoete',
        srr_verbose == 'Pacific Lamprey' & grepl('\\bMP\\b', conditional_comments) ~ 'Macropthalmia',
        srr_verbose == 'Pacific Lamprey' & grepl('\\bAL\\b', conditional_comments) ~ 'Adult',
        # All Hatchery
        origin == 'Hatchery' ~ NA_character_, # probably shouldn't just assume 'Smolt'
        # Steelhead - may need to update this.
        grepl('Steelhead', srr_verbose) & lubridate::month(event_date) %in% c(1:6) ~ 'Winter/Spring',
        grepl('Steelhead', srr_verbose) & lubridate::month(event_date) %in% c(7:12) ~ 'Summer/Fall',
        # Spring/summer Chinook - this doesn't deal with Fall chinook properly.
        grepl('YOY', text_comments) ~ 'YOY',
        origin == 'Natural' & lubridate::month(event_date) %in% c(1:6) ~ 'Smolt',
        origin == 'Natural' & lubridate::month(event_date) %in% c(7:8) ~ 'Parr',
        origin == 'Natural' & lubridate::month(event_date) %in% c(9:12) ~ 'Presmolt',
        TRUE ~ NA_character_
      ),
      trap_season = case_when(
        lubridate::month(event_date) %in% c(1:6) ~ 'Spring',
        lubridate::month(event_date) %in% c(7:8) ~ 'Summer',
        lubridate::month(event_date) %in% c(9:12) ~ 'Fall',
      ),
      condition_factor = (weight/length^3)*100000,
      target = if_else(species_run_rear_type %in% c('11H', '11W', '12H', '12W', '32W', '32H', '25W', '25H', '7RW', 'A0W'), TRUE, FALSE), # 1=target, 0=incidental
      scientific_name = case_when(
        srr_verbose %in% c('Unknown', 'Unknown (fish not observed)', 'Other', 'No Fish Day') ~ NA_character_,
        grepl('Chinook', srr_verbose) ~ 'Oncorhynchus tshawytscha',
        grepl('Steelhead|Rainbow Trout', srr_verbose) ~ 'Oncorhynchus mykiss',
        grepl('Coho', srr_verbose) ~ 'Oncorhynchus kisutch',
        srr_verbose == 'American Shad' ~ 'Alosa sapidissima',
        srr_verbose == 'Bridgelip Sucker' ~ 'Catostomus columbianus',
        srr_verbose == 'Brook Trout' ~ 'Salvelinus fontinalis',
        srr_verbose == 'Bull Trout' ~ 'Salvelinus confluentus',
        srr_verbose == 'Bullhead Catfish' ~ 'Ameiurus sp.',  # 3 different in this group?
        srr_verbose == 'Channel Catfish' ~ 'Ictalurus punctatus',
        srr_verbose == 'Chiselmouth' ~ 'Acrocheilus alutaceus',
        srr_verbose == 'Dace species' ~ 'Leuciscus sp.',
        srr_verbose == 'Largescale Sucker' ~ 'Castostomus macrocheilus',
        srr_verbose == 'Longnose Dace' ~ 'Rhinichthys cataractae',
        srr_verbose == 'Mottled Sculpin' ~ 'Cottus bairdii',
        srr_verbose == 'Mountain Whitefish' ~ 'Prosopium williamsoni',
        srr_verbose == 'Northern Pikeminnow' ~ 'Ptychocheilus oregonensis',
        srr_verbose == 'Pacific Lamprey' ~ 'Entosphenus tridentatus',
        srr_verbose == 'Peamouth' ~ 'Mylocheilus caurinus',
        srr_verbose == 'Redside Shiner' ~ 'Richardsonius balteatus',
        srr_verbose == 'Sculpin species' ~ 'Cottus sp.',
        srr_verbose == 'Smallmouth Bass' ~ 'Micropterus dolomieu',
        srr_verbose == 'Speckled Dace' ~ 'Rhinichthys osculus',
        srr_verbose == 'Sucker species' ~ 'Catostomus sp.',
        srr_verbose %in% c('Wild Coastal Cutthroat', 'Wild Resident Cutthroat') ~ 'Oncorhynchus clarkii',
        srr_verbose == 'Wild Sockeye (unknown run)' ~ 'Oncorhynchus nerka',
        TRUE ~ NA_character_
      ),
      family = case_when(
        srr_verbose %in% c('Unknown', 'Unknown (fish not observed)', 'Other', 'No Fish Day') ~ NA_character_,
        grepl('Chinook', srr_verbose) ~ 'Salmonidae',
        grepl('Steelhead|Rainbow Trout', srr_verbose) ~ 'Salmonidae',
        grepl('Coho', srr_verbose) ~ 'Salmonidae',
        srr_verbose == 'American Shad' ~ 'Clupeidae',
        srr_verbose == 'Bridgelip Sucker' ~ 'Catostomidae',
        srr_verbose == 'Brook Trout' ~ 'Salmonidae',
        srr_verbose == 'Bull Trout' ~ 'Salmonidae',
        srr_verbose == 'Bullhead Catfish' ~ 'Ictaluridae',
        srr_verbose == 'Channel Catfish' ~ 'Ictaluridae',
        srr_verbose == 'Chiselmouth' ~ 'Leuciscidae',
        srr_verbose == 'Dace species' ~ 'Cyprinidae',
        srr_verbose == 'Largescale Sucker' ~ 'Catostomidae',
        srr_verbose == 'Longnose Dace' ~ 'Leuciscidae',
        srr_verbose == 'Mottled Sculpin' ~ 'Cottidae',
        srr_verbose == 'Mountain Whitefish' ~ 'Salmonidae',
        srr_verbose == 'Northern Pikeminnow' ~ 'Leuciscidae',
        srr_verbose == 'Pacific Lamprey' ~ 'Petromyzontidae',
        srr_verbose == 'Peamouth' ~ 'Cyprinidae',
        srr_verbose == 'Redside Shiner' ~ 'Cyprinidae',
        srr_verbose == 'Sculpin species' ~ 'Cottidae', # this might not be 100% correct.
        srr_verbose == 'Smallmouth Bass' ~ 'Centrarchidae',
        srr_verbose == 'Speckled Dace' ~ 'Leuciscidae',
        srr_verbose == 'Sucker species' ~ 'Catostomidae',
        srr_verbose %in% c('Wild Coastal Cutthroat', 'Wild Resident Cutthroat') ~ 'Salmonidae',
        srr_verbose == 'Wild Sockeye (unknown run)' ~ 'Salmonidae',
        TRUE ~ NA_character_
      ),
      streamname = case_when(
        event_site %in% c('JOHNSC', 'JOHTRP') ~ 'Johnson Creek',   # should we have trap site and streamname separate?
        event_site == 'SECTRP' ~ 'Secesh River',
        event_site == 'IMNTRP' ~ 'Imnaha River',
        event_site %in% c('CLWRSF', 'SFCTRP') ~ 'South Fork Clearwater River',
        event_site %in% c('LOLOC', 'LOLTRP') ~ 'Lolo Creek',
        event_site == 'NEWSOC' ~ 'Newsome Creek',
        TRUE ~ NA_character_  # skipped LSFTRP/MCCA/NA/NPTH
      ),
      emigrant_group = case_when(
        scientific_name == 'Oncorhynchus tshawytscha' & life_stage == 'Parr' ~ 'Natural Chinook Salmon Parr',
        scientific_name == 'Oncorhynchus tshawytscha' & life_stage == 'Presmolt' ~ 'Natural Chinook Salmon Presmolts',
        scientific_name == 'Oncorhynchus tshawytscha' & life_stage == 'Smolt' ~ 'Natural Chinook Salmon Smolts',
        scientific_name == 'Oncorhynchus tshawytscha' & origin == 'Hatchery' ~ 'Hatchery Chinook Salmon Smolts',
        scientific_name == 'Oncorhynchus mykiss' & origin == 'Natural' ~ 'Natural Steelhead Juveniles',
        scientific_name == 'Oncorhynchus mykiss' & origin == 'Hatchery' ~ 'Hatchery Steelhead Juveniles',
        TRUE ~ NA_character_)
      ) %>%

    # establish emigrant groups? - plot groups?

    # get common staff gauge
    mutate(staff_gauge = ifelse(!is.na(staff_gauge_ft), staff_gauge_ft, staff_gauge_cm * 2.54 * 12)) %>%

    select(event_site, streamname, event_date, event_time, gmt_offset, trap_start_datetime, trap_end_datetime,
           hours_sampled, operational_condition, session_note, origin, srr_verbose, species_run_rear_type,
           event_type, pit_tag, length, weight, condition_factor, life_stage, nfish, brood_year,
           conditional_comments, text_comments, tagger, pit_tagged, efficiency_mark,
           efficiency_recap, mortality, target, scientific_name, family, emigrant_group,
           everything())

 return(data_clean)
}
