#' @title sum_RSTstrata:
#' @description Takes prepared RST data and creates a plot to help determine
#' strata through a visualization, returning the strata dates, and returns a
#' dataframe to be fed to gauss estimates.
#' @param data Prepared P4 RST data from get_RSTData() %>% clean_RSTData() and filtered for a single RST.
#' @param season Season desired for strata determination.
#' @param strata_dates Vector of dates (YYYY-MM-DD)input by user to establish
#' strata.  If NULL, function will automatically apply one-week strata.
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- get_P4Data(EventSite = 'IMNTRP', MigrationYear = 2020)
#' p4_clean <- clean_P4Data(p4_raw)
#' strata <- sum_RSTstrata(p4_clean, 'Spring', NULL)

sum_RSTstrata <- function(data, season, strata_dates = NULL) {

  # plotting function
  # source('./R/gg_strata.R')

  # must have data
  {if(is.null(data))stop("RST data must be supplied")}

  # establish months
  if(season == 'Spring') {season_months <- 1:6}
  if(season == 'Summer') {season_months <- 7:8}
  if(season == 'Fall') {season_months <- 9:12}

  # Get M/C/R for RST ----
  mcr_prep <- data %>%
    filter(month(event_date) %in% season_months) %>%
    filter(speciesrunreartype %in% c('11W', '12W', '32W')) %>%
    arrange(event_date) %>%
    group_by(event_date, trap_rpm, staff_gauge_cm, staff_gauge_ft, speciesrunreartype) %>%
    summarize(
      # Captures, all first observations of fish
      C = sum(nfish[eventtype %in% c('Mark', 'Tally')], na.rm = TRUE),
      # Marks of fish passed upstream for efficiency measurements
      M = sum(efficiency_mark, na.rm = TRUE),
      # Recaptures of efficiency marks
      R = sum(efficiency_recap, na.rm = TRUE)
    )

  # apply strata based on NULL or provided strata_dates
  if(is.null(strata_dates)) {
    mcr_daily <- mcr_prep %>%
      ungroup() %>%
      mutate(strata_start = ymd(cut.Date(event_date, breaks='week', start.on.monday = F))) %>%
      mutate(strata_n = group_indices_(., .dots='strata_start'))
  } else {
    mcr_daily <- mcr_prep %>%
      ungroup() %>%
      mutate(strata_start = ymd(cut.Date(event_date, breaks=ymd(strata_dates), start.on.monday = F))) %>%
      mutate(strata_n = group_indices_(., .dots='strata_start'))
  }

  # summarize strata
  mcr_strata <- mcr_daily %>%
    group_by(strata_start, strata_n) %>%
    summarize(
      C_12W = sum(C[speciesrunreartype %in% c('11W', '12W')], na.rm = T),
      M_12W = sum(M[speciesrunreartype %in% c('11W', '12W')], na.rm = T),
      R_12W = sum(R[speciesrunreartype %in% c('11W', '12W')], na.rm = T),
      C_32W = sum(C[speciesrunreartype == '32W'], na.rm = T),
      M_32W = sum(M[speciesrunreartype == '32W'], na.rm = T),
      R_32W = sum(R[speciesrunreartype == '32W'], na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      strata_end = case_when(
        !is.na(lead(strata_start)) ~ lead(strata_start) - days(1),
        is.na(lead(strata_start)) ~ strata_start + days(6),
        TRUE ~ as.Date(NA)),
      # strata_labeldate helps place labels in the middle of the strata
      strata_labeldate = strata_start + (strata_end-strata_start)/2) %>%
    filter(!is.na(strata_start))

  strata_vector <- unique(mcr_strata$strata_start)

  strata_plot <- gg_strata(mcr_daily, mcr_strata)

  # generate return for gauss estimates.
  gauss_return <- mcr_strata %>%
    arrange(strata_n) %>%
    select(C=C_12W, M=M_12W, R=R_12W)

  cat('This functions returns a list of objects:',
      '[[1]]: strata visualization' ,
      '[[2]]: strata dates vector',
      '[[3]]: dataframe for gauss estimates (12W)', sep='\n')

  return(list(strata_plot, strata_vector, gauss_return))
}
