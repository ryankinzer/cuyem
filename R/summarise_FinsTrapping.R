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
                      TRUE~FALSE))

  # high level summary for tuesday call: count of species, run, origin, sex, living status, purpose, dispostion, release site

  sum_new <- df %>%
    filter(recap != TRUE) %>%
    group_by(trap_year, facility, trap, species, run, origin, sex, age_designation, living_status, disposition, purpose) %>%
    summarise(count = n())

  # graphics
  # tmp <- sum_new %>%
  #   filter(trap_year == '2019') %>%
  #   filter(species == 'Chinook') %>%
  #   filter(grepl('Hatchery', origin)) %>%
  #   group_by(facility, trap) %>%
  #   mutate(p = count/sum(count)) %>%
  #   ggplot(aes(x = '', y = p, colour = purpose, fill = purpose)) +
  #   geom_bar(stat = 'identity',  width = 1) +
  #   scale_fill_viridis_d() +
  #   scale_colour_viridis_d() +
  #   facet_wrap(~trap, labeller = label_wrap_gen(width = 15), ncol = 9) + # labeller = label_wrap_gen(width = 20),
  #   coord_polar("y", start = 0) +
  #   guides(fill = guide_legend(nrow = 4)) +
  #   labs(x = '',
  #        y = '',
  #        fill = '',
  #        colour = '',
  #        title = 'Purpose of Returning Hatchery Chinook Salmon to Snake River Basin Traps',
  #        caption = 'Supporting data was collected in 2019 and available at www.finsnet.org.') +
  #   theme(axis.text = element_blank(),
  #         axis.ticks = element_blank(),
  #         panel.grid  = element_blank(),
  #         panel.background = element_blank(),
  #         legend.position = 'bottom')
  #
  # ggsave('./purpose.png', tmp, height = 8.5, width = 11)

  # captued / released/ marked
  marked <- df %>%
    filter(recap != TRUE) %>%
    #filter(disposition == 'Released' & grepl('Above Weir', release_site)) %>%
    filter(marked) %>%
    group_by(trap_year, facility, trap, species, run, age_designation, age_criteria_start_length, age_criteria_end_length, applied_marks) %>%
    summarise(marked = n()) #%>%
    #rename(marks = applied_marks)

  # tmp <- marked %>%
  #   filter(is.na(applied_marks))

  # tmp_df <- df %>%
  #   filter(recap %in% c('False', 'FALSE')) %>%
  #   filter(disposition == 'Released' & grepl('Above Weir', release_site)) %>%
  #   filter(grepl('Johnson', trap)) %>%
  #   filter(trap_year == 2017)

  recapture <- df %>%
    filter(recap) %>%
    group_by(trap_year, facility, trap, species, run, age_designation, age_criteria_start_length, age_criteria_end_length, existing_marks) %>%
    summarise(recaptured = n()) #%>%
    #rename(marks = existing_marks)

  mr_df <- left_join(marked, recapture)

  return(list(sum_new, mr_df))
}
