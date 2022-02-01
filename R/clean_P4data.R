#' @title Clean P4 Data
#' @description Processes raw P4 data and adds important fields for summaries
#' @param data raw CDMS RST dataset from get_RSTdata()
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' rst_raw <- get_RSTdata()
#' rst_clean <- clean_RSTdata(rst_raw)
clean_P4data <- function(data){
  {if(is.null(data))stop("P4 data must be supplied")}

  # snake-ize field names
  names(data) <- gsub(' ','_',tolower(names(data)))

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
    # Do we need to modify other datetime fields?
    separate(eventdate, into = c('event_date', 'event_time'), sep='T') %>% # this is the only trustworthy datetime, filled and standardized by P4
    # Fixing datatypes
    mutate(event_date = lubridate::ymd(event_date),
           across(.cols = c(trap_start_datetime, trap_end_datetime), lubridate::mdy_hm),
           across(.cols = c(trap_rpm, staff_gauge_ft), as.double)) %>%
    mutate(
      pit_tag_issued = if_else(pittag == '..........', 0, 1),
      efficiency_mark = case_when(  # Bismark Brown? BBY? ********************
        # Tagged sent upstream
        grepl('[ ]{0,1}TU[ ]{0,1}', textcomments, ignore.case = T) ~ 1,
        # Caudal Lower/Upper sent upstream
        grepl('[ ]{0,1}CL[ ]{0,1}|[ ]{0,1}CU[ ]{0,1}', conditionalcomments, ignore.case = T) ~ 1,
        TRUE ~ 0),
      efficiency_recap = if_else(
        grepl('Recapture', eventtype) &  # this avoids including Mortality RE/RC
          grepl('[ ]{0,1}RE[ ]{0,1}', conditionalcomments) &
          grepl('[ ]{0,1}RC[ ]{0,1}', textcomments), 1, 0),
      mortality =
        if_else(grepl('[ ]{0,1}M[ ]{0,1}', conditionalcomments) &
                  grepl('[ ]{0,1}TRP[ ]{0,1}|[ ]{0,1}PRD[ ]{0,1}|[ ]{0,1}HND[ ]{0,1}|[ ]{0,1}DOA[ ]{0,1}|[ ]{0,1}TG[ ]{0,1}', textcomments), 1, 0),
      origin = case_when(
        grepl('Hat.', srrverbose)  ~ 'Hatchery',
        grepl('Wild', srrverbose) ~ 'Natural',
        TRUE ~ NA_character_
      ),
      lifestage = case_when(
        # Pacific Lamprey
        srrverbose == 'Pacific Lamprey' & grepl('[ ]{0,1}AM[ ]{0,1}', conditionalcomments) ~ 'Ammocoete',
        srrverbose == 'Pacific Lamprey' & grepl('[ ]{0,1}MP[ ]{0,1}', conditionalcomments) ~ 'Macropthalmia',
        srrverbose == 'Pacific Lamprey' & grepl('[ ]{0,1}AL[ ]{0,1}', conditionalcomments) ~ 'Adult',
        # All Hatchery
        origin == 'Hatchery' ~ NA_character_, # probably shouldn't just assume 'Smolt'
        # Steelhead - may need to update this.
        grepl('Steelhead', srrverbose) & lubridate::month(event_date) %in% c(1:6) ~ 'Winter/Spring',
        grepl('Steelhead', srrverbose) & lubridate::month(event_date) %in% c(7:12) ~ 'Summer/Fall',
        # Spring/summer Chinook - this doesn't deal with Fall chinook properly.
        grepl('YOY', textcomments) ~ 'YOY',
        origin == 'Natural' & lubridate::month(event_date) %in% c(1:6) ~ 'Smolt',
        origin == 'Natural' & lubridate::month(event_date) %in% c(7:9) ~ 'Parr',
        origin == 'Natural' & lubridate::month(event_date) %in% c(10:12) ~ 'Presmolt',
        TRUE ~ NA_character_
      ),
      condition_factor = (weight/length^3)*100000,
      target = if_else(speciesrunreartype %in% c('11H', '11W', '12H', '12W', '32W', '32H', '25W', '25H', '7RW', 'A0W'), 1, 0), # 1=target, 0=incidental
      scientific_name = case_when(
        srrverbose %in% c('Unknown', 'Unknown (fish not observed)', 'Other', 'No Fish Day') ~ NA_character_,
        grepl('Chinook', srrverbose) ~ 'Oncorhynchus tshawytscha',
        grepl('Steelhead|Rainbow Trout', srrverbose) ~ 'Oncorhynchus mykiss',
        grepl('Coho', srrverbose) ~ 'Oncorhynchus kisutch',
        srrverbose == 'American Shad' ~ 'Alosa sapidissima',
        srrverbose == 'Bridgelip Sucker' ~ 'Catostomus columbianus',
        srrverbose == 'Brook Trout' ~ 'Salvelinus fontinalis',
        srrverbose == 'Bull Trout' ~ 'Salvelinus confluentus',
        srrverbose == 'Bullhead Catfish' ~ 'Ameiurus sp.',  # 3 different in this group?
        srrverbose == 'Channel Catfish' ~ 'Ictalurus punctatus',
        srrverbose == 'Chiselmouth' ~ 'Acrocheilus alutaceus',
        srrverbose == 'Dace species' ~ 'Leuciscus sp.',
        srrverbose == 'Largescale Sucker' ~ 'Castostomus macrocheilus',
        srrverbose == 'Longnose Dace' ~ 'Rhinichthys cataractae',
        srrverbose == 'Mottled Sculpin' ~ 'Cottus bairdii',
        srrverbose == 'Mountain Whitefish' ~ 'Prosopium williamsoni',
        srrverbose == 'Northern Pikeminnow' ~ 'Ptychocheilus oregonensis',
        srrverbose == 'Pacific Lamprey' ~ 'Entosphenus tridentatus',
        srrverbose == 'Peamouth' ~ 'Mylocheilus caurinus',
        srrverbose == 'Redside Shiner' ~ 'Richardsonius balteatus',
        srrverbose == 'Sculpin species' ~ 'Cottus sp.',
        srrverbose == 'Smallmouth Bass' ~ 'Micropterus dolomieu',
        srrverbose == 'Speckled Dace' ~ 'Rhinichthys osculus',
        srrverbose == 'Sucker species' ~ 'Catostomus sp.',
        srrverbose %in% c('Wild Coastal Cutthroat', 'Wild Resident Cutthroat') ~ 'Oncorhynchus clarkii',
        srrverbose == 'Wild Sockeye (unknown run)' ~ 'Oncorhynchus nerka',
        TRUE ~ NA_character_
      ),
      family = case_when(
        srrverbose %in% c('Unknown', 'Unknown (fish not observed)', 'Other', 'No Fish Day') ~ NA_character_,
        grepl('Chinook', srrverbose) ~ 'Salmonidae',
        grepl('Steelhead|Rainbow Trout', srrverbose) ~ 'Salmonidae',
        grepl('Coho', srrverbose) ~ 'Salmonidae',
        srrverbose == 'American Shad' ~ 'Clupeidae',
        srrverbose == 'Bridgelip Sucker' ~ 'Catostomidae',
        srrverbose == 'Brook Trout' ~ 'Salmonidae',
        srrverbose == 'Bull Trout' ~ 'Salmonidae',
        srrverbose == 'Bullhead Catfish' ~ 'Ictaluridae',
        srrverbose == 'Channel Catfish' ~ 'Ictaluridae',
        srrverbose == 'Chiselmouth' ~ 'Leuciscidae',
        srrverbose == 'Dace species' ~ 'Cyprinidae',
        srrverbose == 'Largescale Sucker' ~ 'Catostomidae',
        srrverbose == 'Longnose Dace' ~ 'Leuciscidae',
        srrverbose == 'Mottled Sculpin' ~ 'Cottidae',
        srrverbose == 'Mountain Whitefish' ~ 'Salmonidae',
        srrverbose == 'Northern Pikeminnow' ~ 'Leuciscidae',
        srrverbose == 'Pacific Lamprey' ~ 'Petromyzontidae',
        srrverbose == 'Peamouth' ~ 'Cyprinidae',
        srrverbose == 'Redside Shiner' ~ 'Cyprinidae',
        srrverbose == 'Sculpin species' ~ 'Cottidae', # this might not be 100% correct.
        srrverbose == 'Smallmouth Bass' ~ 'Centrarchidae',
        srrverbose == 'Speckled Dace' ~ 'Leuciscidae',
        srrverbose == 'Sucker species' ~ 'Catostomidae',
        srrverbose %in% c('Wild Coastal Cutthroat', 'Wild Resident Cutthroat') ~ 'Salmonidae',
        srrverbose == 'Wild Sockeye (unknown run)' ~ 'Salmonidae',
        TRUE ~ NA_character_
      ),
      streamname = case_when(
        eventsite %in% c('JOHNSC', 'JOHTRP') ~ 'Johnson Creek',   # should we have trap site and streamname separate?
        eventsite == 'SECTRP' ~ 'Secesh River',
        eventsite == 'IMNTRP' ~ 'Imnaha River',
        eventsite %in% c('CLWRSF', 'SFCTRP') ~ 'South Fork Clearwater River',
        eventsite %in% c('LOLOC', 'LOLTRP') ~ 'Lolo Creek',
        eventsite == 'NEWSOC' ~ 'Newsome Creek',
        TRUE ~ NA_character_  # skipped LSFTRP/MCCA/NA/NPTH
      ),
      emigrant_group = case_when(
        scientific_name == 'Oncorhynchus tshawytscha' & lifestage == 'Parr' ~ 'Natural Chinook Salmon Parr',
        scientific_name == 'Oncorhynchus tshawytscha' & lifestage == 'Presmolt' ~ 'Natural Chinook Salmon Presmolts',
        scientific_name == 'Oncorhynchus tshawytscha' & lifestage == 'Smolt' ~ 'Natural Chinook Salmon Smolts',
        scientific_name == 'Oncorhynchus tshawytscha' & origin == 'Hatchery' ~ 'Hatchery Chinook Salmon Smolts',
        scientific_name == 'Oncorhynchus mykiss' & origin == 'Natural' ~ 'Natural Steelhead Juveniles',
        scientific_name == 'Oncorhynchus mykiss' & origin == 'Hatchery' ~ 'Hatchery Steelhead Juveniles',
        TRUE ~ NA_character_)
      ) %>%

    # establish emigrant groups? - plot groups?

    select(eventsite, streamname, event_date, event_time, trap_start_datetime, trap_end_datetime,
           hours_sampled, operational_condition, sessionnote, origin, srrverbose, speciesrunreartype,
           eventtype, pittag, length, weight, condition_factor, lifestage, nfish, broodyear,
           conditionalcomments, textcomments, tagger, pit_tag_issued, efficiency_mark,
           efficiency_recap, mortality, target, scientific_name, family, emigrant_group,
           everything())

 return(data_clean)
}
