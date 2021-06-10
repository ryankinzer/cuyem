#' @title Clean Rotary Screw Trapping Data
#' @description Processes raw P4 data and adds important fields for summaries
#' @param data raw CDMS RST dataset from \code{cdmsR::getDatasetView(datastore = 107)} or from CDMS dataset export
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' rst_dat <- getDatasetView(datastore = 107)
#' tmp <- clean_RSTdata(rst_dat)
clean_RSTdata <- function(data){
  {if(is.null(data))stop("RST data must be supplied")}

  # snake-ize field names
  names(data) <- gsub(' ','_',tolower(names(data)))

  # P4 SRR Verbose list for join ----
  p4_srr_verbose <- structure(list(speciesrunreartype = c("F0W", "ERU", "7RW", "I0W", "15U", "55U",
                                                    "85U", "25U", "13U", "53U", "C0W", "15H", "55H", "85H", "25H",
                                                    "13H", "53H", "30H", "3RH", "45H", "11H", "35H", "12H", "52H",
                                                    "42H", "32H", "B0H", "34H", "4RH", "G0W", "D0W", "30U", "90U",
                                                    "A0W", "3RU", "J0W", "45U", "11U", "35U", "12U", "52U", "42U",
                                                    "32U", "05U", "00U", "H0W", "K0W", "B0W", "15W", "55W", "85W",
                                                    "25W", "13W", "53W", "L0W", "30W", "65W", "3RW", "8RW", "4RW",
                                                    "45W", "11W", "35W", "12W", "52W", "42W", "32W", "34W", "BM",
                                                    "BL", "BH", "CP", "CM", "CR", "LD", "SD", "BK", "MA", "PM", "PE",
                                                    "SR", "SC", "RS", "SP", "SK", "BU", "LU", "MU", "SU", "TT", "WA",
                                                    "LW", "DA", "MS", "TS", "NU", "SS", "NFD"),
                             species = c("American Shad",
                                         "Brook Trout", "Bull Trout", "Channel Catfish", "Chinook (unknown run & r/t)",
                                         "Chum (unknown run & r/t)", "Coastal Cutthroat (unknown r/t)",
                                         "Coho (unknown r/t)", "Fall Chinook (unknown r/t)", "Fall Chum (unknown r/t)",
                                         "Green Sturgeon", "Hat. Chinook (unknown run)", "Hat. Chum (unknown run)",
                                         "Hat. Coastal Cutthroat", "Hat. Coho", "Hat. Fall Chinook", "Hat. Fall Chum",
                                         "Hat. O. mykiss (unknown migratory status)", "Hat. Rainbow Trout",
                                         "Hat. Sockeye (unknown run)", "Hat. Spring Chinook", "Hat. Steelhead (unknown run)",
                                         "Hat. Summer Chinook", "Hat. Summer Chum", "Hat. Summer Sockeye",
                                         "Hat. Summer Steelhead", "Hat. White Sturgeon", "Hat. Winter Steelhead",
                                         "Hatchery Resident Sockeye (Kokanee)", "Mountain Whitefish",
                                         "Northern Pikeminnow", "O. mykiss (unknown migratory/rearing status)",
                                         "Other", "Pacific Lamprey", "Rainbow Trout (unknown r/t)", "Smallmouth Bass",
                                         "Sockeye (unknown run & r/t)", "Spring Chinook (unknown r/t)",
                                         "Steelhead (unknown run & r/t)", "Summer Chinook (unknown r/t)",
                                         "Summer Chum (unknown r/t)", "Summer Sockeye (unknown r/t)",
                                         "Summer Steelhead (unknown r/t)", "Unknown", "Unknown (fish not observed)",
                                         "Walleye", "Western Brook Lamprey", "White Sturgeon", "Wild Chinook (unknown run)",
                                         "Wild Chum (unknown run)", "Wild Coastal Cutthroat", "Wild Coho",
                                         "Wild Fall Chinook", "Wild Fall Chum", "Wild Lamprey (species unknown)",
                                         "Wild O. mykiss (unknown migratory status)", "Wild Pink", "Wild Rainbow Trout",
                                         "Wild Resident Cutthroat", "Wild Resident Sockeye (Kokanee)",
                                         "Wild Sockeye (unknown run)", "Wild Spring Chinook", "Wild Steelhead (unknown run)",
                                         "Wild Summer Chinook", "Wild Summer Chum", "Wild Summer Sockeye",
                                         "Wild Summer Steelhead", "Wild Winter Steelhead", "Bass, Large Mouth",
                                         "Bluegill and Pumpkinseed", "Bullhead", "Carp", "Chiselmouth",
                                         "Crappie species", "Dace - Long Nose", "Dace - Speckled", "Killifish, Banded",
                                         "Madtom, Tadpole", "Peamouth", "Perch", "Sand Roller", "Sculpin species",
                                         "Shiner, Redside", "Siberian Prawn", "Stickleback", "Sucker - Bridgelip",
                                         "Sucker - Largescale", "Sucker - Mountain", "Sucker, Other",
                                         "Tench", "Warmouth", "Whitefish - Lake", "Dace species", "Sculpin - Mottled",
                                         "Sculpin - Torrent", "Sucker - Longnose", "Sucker species", "No Fish Day")),
                        row.names = c(NA, -98L), class = c("tbl_df", "tbl", "data.frame"))

  # rename, fix datatypes
  trap_df <- data %>%
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
    separate(locationlabel, into = c('streamname', 'trap'), sep=': ') %>%
    # Do we need to modify other datetime fields?
    separate(eventdate, into = c('event_date', 'event_time'), sep='T') %>% # this is the only trustworthy datetime, filled and standardized by P4
    mutate(event_date = ymd(event_date)) %>%
    left_join(p4_srr_verbose, by = 'speciesrunreartype') %>%
    mutate(
      pit_tag_issued = if_else(pittag == '..........', 0, 1),
      efficiency_mark = if_else(grepl('TU', textcomments), 1, 0),
      efficiency_recap = if_else(
        grepl('Recapture', eventtype) &&  # this avoids including Mortality RE/RC
          grepl('RE', conditionalcomments) &&
          grepl('RC', textcomments), 1, 0),
      mortality =
        if_else(grepl('M', conditionalcomments) &&
                  grepl('TRP|PRD|HND|DOA|TG', textcomments), 1, 0),
      origin = case_when(
        grepl('Hat.', species)  ~ 'Hatchery',
        grepl('Wild', species) ~ 'Natural',
        TRUE ~ NA_character_
      ),
      lifestage = case_when(
        # Pacific Lamprey
        species == 'Pacific Lamprey' & grepl('AM', conditionalcomments) ~ 'Ammocoete',
        species == 'Pacific Lamprey' & grepl('MP', conditionalcomments) ~ 'Macropthalmia',
        species == 'Pacific Lamprey' & grepl('AL', conditionalcomments) ~ 'Adult',
        # All Hatchery
        origin == 'Hatchery' ~ NA_character_, # probably shouldn't just assume 'Smolt'
        # Steelhead - may need to update this.
        grepl('Steelhead', species) & month(event_date %in% 1:6) ~ 'Winter/Spring',
        grepl('Steelhead', species) & month(event_date %in% 7:12) ~ 'Summer/Fall',
        # Spring/summer Chinook - this doesn't deal with Fall chinook properly.
        grepl('YOY', textcomments) ~ 'YOY',
        origin == 'Natural' & month(event_date) %in% c(1:6) ~ 'Smolt',
        origin == 'Natural' & month(event_date) %in% c(7:9) ~ 'Parr',
        origin == 'Natural' & month(event_date) %in% c(10:12) ~ 'Presmolt',
        TRUE ~ NA_character_
      ),
      condition_factor = (weight/length^3)*100000
      ) %>%
    select(trap, filename, event_date, event_time, trap_start_datetime, trap_end_datetime,
           hours_sampled, operational_condition, sessionnote, origin, species, speciesrunreartype,
           eventtype, pittag, length, weight, condition_factor, lifestage, nfish, broodyear,
           conditionalcomments, textcomments, tagger, pit_tag_issued, efficiency_mark,
           efficiency_recap, mortality,
           everything()) %>%
  select(-cdms_origin) # prevents confusion with new origin field.



 return(trap_df)
}
