#' @title qa_P4Data
#' @description Provides basic summaries and figures of P4 data to assist in data validation and the QA/QC process.
#' @param data cleaned P4 data from get_RSTData() %>% clean_RSTData() and filtered for a single RST
#' @param srr_code vector of species run rear codes, can be a single code or multiple, if left NULL the function will return list of table for each srr_code
#' @export
#' @import dplyr
#' @author Ryan N. Kinzer
#' @examples
#' p4_raw <- get_P4Data(EventSite = 'IMNTRP', MigrationYear = 2020)
#' p4_clean <- clean_P4Data(p4_raw)
#' qa_dat <- qa_P4Data(p4_clean)
#' names(qa_dat)
#' qa_dat[[1]]
#'
qa_P4Data <- function(data){

  {if(is.null(data))stop("RST data must be supplied")}

  spp_counts <- data %>%
    mutate(release_year = lubridate::year(release_date)) %>%
    group_by(capture_method, srr_verbose, release_site, brood_year, migration_year, release_year) %>%
    summarise(n_events = n_distinct(name),
              n_fish = sum(nfish)) %>%
    arrange(srr_verbose, release_site, brood_year)

  disp_sums <- data %>%
    group_by(srr_verbose, trap_season, event_type, conditional_comments, text_comments, mark_recap, mortality) %>%
    summarise(n_events = n_distinct(name),
              n_fish = sum(nfish)) %>%
    arrange(srr_verbose, trap_season)

  ops_dat <- data %>%
    select(event_date, trap_rpm, mark_temperature, staff_gauge) %>%
    distinct()

  temp_plot <- ops_dat %>%
    ggplot(aes(x = event_date, y = mark_temperature)) +
    geom_point()

  rpm_plot <- ops_dat %>%
    ggplot(aes(x = event_date, y = trap_rpm)) +
    geom_point()

  staff_plot <- ops_dat %>%
    ggplot(aes(x = event_date, y = staff_gauge)) +
    geom_point()

  # Validation
  # Check Operational_Condition
  operational_condition_check <- data %>%
    distinct(name, .keep_all=TRUE) %>%
    filter(!operational_condition %in% c('S1 – Complete overnight sample (<=24 hours)|S12 – Sampled more than 24 hours|S9 – Sub-sampled|S3 – No or incomplete sample due to environmental conditions|S5 – No or incomplete sample due to equipment failure, staffing, etc.')) %>%
    mutate(operational_condition_check = paste('Error:', operational_condition)) %>%
    select(name, ends_with('check'))

  # Hours_Sampled check
  hours_sampled_check <- data %>%
    distinct(name, .keep_all=TRUE) %>%
    mutate(hours_sampled_check = case_when(
      trap_end_datetime - trap_start_datetime != hours_sampled ~ 'Error: Check math.',
      grepl('00:00:00', trap_start_datetime) & grepl('00:00:00', trap_end_datetime) & hours_sampled != 0 ~ 'Warning: should this be 0?',  # attempt to capture non-operational days.
      TRUE ~ NA_character_)) %>%
    filter(grepl('Error|Warning', hours_sampled_check)) %>%
    select(name, ends_with('check'))

  # Check Trap RPM
  trap_rpm_check <- data %>%
    distinct(name, .keep_all=TRUE) %>%
    filter(is.na(trap_rpm)) %>%
    # RPM = 0 for non-operational days (S3, S5)
    mutate(trap_rpm_check = case_when(
      is.na(trap_rpm) ~ 'Error: missing RPM',
      hours_sampled ==0 & trap_rpm != 0 ~ 'Warning: Should RPM=0?')) %>%
    select(name, ends_with('check'))

  # Check NFDs
  nfd_check <- data %>%
    filter(species_run_rear_type == '00U' & nfish !=1| species_run_rear_type == 'NFD' & nfish != 0) %>%
    mutate(nfd_check = case_when(
      species_run_rear_type == '00U' & nfish !=1 ~ 'Error: 00U should be NFD',
      species_run_rear_type == 'NFD' & nfish != 0 ~ 'Error: NFD nfish should = 0',
      TRUE ~ NA_character_)) %>%
    distinct(name, .keep_all=T) %>%
    select(name, ends_with('check'))

  # NFD TRAP TIMES - check protocol

  # Check Mortalities
  mort_check <- data %>%
    select(name, conditional_comments, text_comments) %>%
    filter(grepl('\\bM\\b', conditional_comments, perl = TRUE) &
             !grepl('\\bTR\\b|\\bPR\\b|\\bHND\\b|\\bDOA\\b|\\bTG\\b', text_comments)) %>%
    mutate(mort_check = 'Error: Check mortality source') %>%
    select(name, ends_with('check'))

  # Check Weather
  weather_check <- data %>%
    distinct(name, .keep_all = TRUE) %>%
    filter(!weather %in% c('CLEAR','PARTLY OVERCAST','OVERCAST','RAIN','SNOW')) %>%
    mutate(weather_check = 'Error') %>%
    select(name, ends_with('check'))

  # Lamprey Lifestage Check
  lamprey_check <- data %>%
    select(name, species_run_rear_type, conditional_comments) %>%
    filter(species_run_rear_type == 'A0W' & !grepl('\\bAM\\b|\\bMP\\b|\\bAL\\b', conditional_comments)) %>%
    mutate(lamprey_check = 'Error: Lamprey Missing Lifestage') %>%
    select(name, ends_with('check'))

  # Validation Join
  validation_df <- operational_condition_check %>%
    full_join(hours_sampled_check, by = 'name') %>%
    full_join(trap_rpm_check, by = 'name') %>%
    full_join(nfd_check, by = 'name') %>%
    full_join(mort_check, by = 'name') %>%
    full_join(weather_check, by = 'name') %>%
    full_join(lamprey_check, by = 'name')


  #get marks
  mark_df <- data %>%
    filter(pit_tagged) %>% # assigned with the clean function
    filter(mark_recap == 'mark') %>% # assigned with the clean function
    select(mark_file = name, event_site, trap_season, mark_date = event_date, pit_tag)

  # get recaps
  recap_df <- data %>%
    filter(pit_tagged) %>%
    filter(mark_recap == 'recap') %>%
    select(recap_file = name, pit_tag, recap_date = event_date)

  orphan <- anti_join(recap_df, mark_df)

  return(list('spp_counts' = spp_counts,
              'disp_sums' = disp_sums,
              'validation' = validation_df,
              'orphan_tags' = orphan,
              'temp_plot' = temp_plot,
              'rpm_plot' = rpm_plot,
              'staff_plot' = staff_plot))
}
