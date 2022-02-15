#' @title Clean Fall Chinook Run Reconstruction (FCRR) Data
#' @description Processes raw FCRR data into a more manageable format.
#' @param data raw CDMS FCRR dataset from \code{cdmsR::getDatasetView(datastore = 100)} or from CDMS dataset export
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' fcrr_dat <- getDatasetView(datastore = 100)
#' fcrr_clean <- clean_FCRRData(fcrr_dat)
clean_FCRRData <- function(data){
  {if(is.null(data))stop("FCRR data must be supplied")}

  # remove 02_FCRR_standardize.R when this is complete.

  # Find all records containing only NA or 0.
  remove_tmp <- data %>%
    filter(
      # all 0
      female_escapement == 0 &
      male_escapement == 0 &
      jack_escapement == 0 &
      females_removed == 0 &
      males_removed == 0 &
      jacks_removed == 0 &
      females_released == 0 &
      males_released == 0 &
      jacks_released == 0 |
      # all NA
      is.na(female_escapement) &
      is.na(male_escapement) &
      is.na(jack_escapement) &
      is.na(females_removed) &
      is.na(males_removed) &
      is.na(jacks_removed) &
      is.na(females_released) &
      is.na(males_released) &
      is.na(jacks_released))

  # unique(remove_tmp$age) # these records are also mostly ones with bogus age values

  # Duplicate Check 1
  # dupes1 <- data %>%
  #   group_by(return_year, age, rearing, origin, cwt_code, cwt_origin, female_escapement, male_escapement, jack_escapement, females_removed,
  #            males_removed, jacks_removed, females_released, males_released, jacks_released) %>%
  #   tally() %>%
  #   filter(n != 1)

  # Remove the all-zero records w/ anti_join  (1806 records 4/12/21)
  data_trimmed <- anti_join(data, remove_tmp)

  # Duplicate Check 2 (this confirms the anti_join removed all dupes!)
  # dupes2 <- data_trimmed %>%
  #   group_by(return_year, age, rearing, origin, cwt_code, cwt_origin, female_escapement, male_escapement, jack_escapement, females_removed,
  #            males_removed, jacks_removed, females_released, males_released, jacks_released) %>%
  #   tally() %>%
  #   filter(n != 1)

  # rm(dupes1, dupes2, remove_tmp, data) # remove clutter

  # Standardize ----
  data_standardizing <- data_trimmed %>%
    filter(rearing != 'parr') %>%  # remove parr groups. Email: RE:fchnk data (Bill Young) 4/21/2021
    mutate(
      species = 'Chinook salmon',
      run = 'Fall',
      SpeciesRun = 'F_CHN',
      location = 'Lower Granite Dam',
      brood_year = if_else(brood_year==1900, NA_integer_, brood_year),
      age = if_else(age > 100, 'unknown', as.character(age)),
      rearing = case_when(
        rearing == 0 ~ 'unknown',  # agency wire = unknown = 0
        rearing == 'Sub/yrlg' ~ 'sub/yrlg',
        str_detect(cwt_origin, 'surrog') ~ 'sub-surrogate',
        TRUE ~ rearing),
      target = if_else(str_detect(origin, 'stray'), 0, 1), # if not a stray==0, it's a target==1.
      origin = case_when(
        origin %in% c('SR hatchery', 'stray hatchery', 'agency wire', '0') ~ 'hatchery',
        origin %in% c('wild', 'stray wild', 'SR wild') ~ 'wild',
        TRUE ~ origin),
      cwt_code = case_when(
        cwt_code %in% c(NA, 'wild', 'hatchery') ~ NA_character_,
        cwt_code == '09blank' ~ '090909', # AGENCY WIRE
        cwt_code == '63blank' ~ '636363',
        TRUE ~ cwt_code
      ),
      # Shuffle cwt_origin to comments
      cwt_origin = case_when(
        grepl('age', cwt_origin) ~ NA_character_,
        cwt_origin == 'surrogate' ~ NA_character_,
        TRUE ~ cwt_origin)
    ) %>%
  select(species, run, SpeciesRun, return_year, age, brood_year, rearing, origin, cwt_code, cwt_origin, target, everything())

  # Pivot standardized data ----
  data_clean <- data_standardizing %>%
    pivot_longer(cols = c('female_escapement', 'male_escapement', 'jack_escapement', 'females_removed', 'males_removed',
                          'jacks_removed', 'females_released', 'males_released', 'jacks_released'),
                 names_to = 'group',
                 values_to = 'estimate') %>%
    separate(group, into = c('grouping', 'measure'), sep='_') %>%
    mutate(grouping = tolower(gsub('s','',grouping))) %>%
    select(location, species, run, SpeciesRun, return_year, age, brood_year, origin, rearing, grouping, target,
           cwt_code, cwt_origin, measure, estimate, ActivityDate)

  return(data_clean)
}
