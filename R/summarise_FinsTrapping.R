#' @title Summarise Fins Trapping Data
#'
#' @description summarizes and processes \url{https://www.finsnet.org/} trapping
#'   module data for quick dissemination and mark/recapture analysis
#'
#' @param df the data.frame output from \code{get_FinsTrapping}
#'
#' @return a list with two objects. 1) a summary data.frame to easily pivot
#'   table for counts of fish trapped and their disposition, 2) a data.frame
#'   with the number of fish marked and released upstream and any recaptured
#' @export
#'
#' @import dplyr
#'
#' @examples
#' con <- RODBC::odbcConnect(dsn = 'Fins', uid = 'guest', pwd = 'guest')
#' qry <- "SELECT distinct(Facility) FROM FINS_all_trapping"
#' f <- RODBC::sqlQuery(con, qry) %>% mutate_all(as.character) %>% pull(Facility)
#' npt_f <- f[grepl('NPT', f)]
#' df <- get_FinsTrapping(npt_f, odbc_connection = con)
#' sum_df <- summarise_FinsTrapping(df)

summarise_FinsTrapping <- function(df){

  if(is.null(df)){stop("A data frame of raw Fins trapping data must be specified.")}

  # create date/time fields, and mark-recapture fields

    df <- df %>%
    mutate(trapped_date = lubridate::mdy(trapped_date),
           trap_year = lubridate::year(trapped_date)) %>%
    mutate(applied_marks = gsub(' ', '', applied_marks)) %>%
    mutate(marked = case_when(
                      disposition == 'Released' &
                        grepl('Above|Upstream', release_site)  &
                        !is.na(applied_marks) ~ TRUE,
                      TRUE~FALSE),
           recap = as.logical(recap))

  # high level summary for tuesday call: count of species, run, origin, sex, living status, purpose, dispostion, release site

  sum_new <- df %>%
    filter(recap != TRUE) %>%
    group_by(trap_year, facility, trap, species, run, origin, sex, age_designation, living_status, disposition, purpose) %>%
    summarise(count = n())

  # captued and released (n1) / recaptured (n2) / marked recaptured (m2)
  n1 <- df %>%
    filter(recap != TRUE) %>%
    filter(marked) %>%
    filter(disposition == 'Released' & grepl('Above Weir', release_site)) %>%
    group_by(trap_year, facility, trap, species, run, origin, age_designation, age_criteria_start_length, age_criteria_end_length, applied_marks) %>%
    summarise(n1= n())

  n2 <- df %>%
    filter(recap == TRUE) %>%
    group_by(trap_year, facility, trap, species, run, origin, age_designation, age_criteria_start_length, age_criteria_end_length, existing_marks) %>%
    summarise(n2 = n())

  # tmp <- marked %>%
  #   filter(is.na(applied_marks))

  # tmp_df <- df %>%
  #   filter(recap %in% c('False', 'FALSE')) %>%
  #   filter(disposition == 'Released' & grepl('Above Weir', release_site)) %>%
  #   filter(grepl('Johnson', trap)) %>%
  #   filter(trap_year == 2017)

  # recapture <- df %>%
  #   filter(recap == TRUE) %>%
  #   group_by(trap_year, facility, trap, species, run, age_designation, age_criteria_start_length, age_criteria_end_length, existing_marks) %>%
  #   summarise(recaptured = n()) #%>%
  #   #rename(marks = existing_marks)
  #
  # mr_df <- left_join(marked, recapture,
  #                    by = c("trap_year", "facility", "trap", "species", "run", "age_designation", "age_criteria_start_length", "age_criteria_end_length"))

  return(list(sum_new, n1, n2))
}
