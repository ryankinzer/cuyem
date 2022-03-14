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
    group_by(capture_method, srr_verbose, brood_year, migration_year, release_site) %>%
    tally(nfish) %>%
    arrange(brood_year, srr_verbose, desc(n))

  disp_sums <- data %>%
    group_by(srr_verbose, life_stage, event_type, conditional_comments, text_comments, mark_recap, mortality) %>%
    summarise(n = sum(nfish))

  bio_plot <- data %>%
      filter(srr_verbose != 'No Fish Day') %>%
      ggplot(aes(x = length, y = weight)) +
      geom_point() +
      facet_wrap(~srr_verbose, scale = 'free')

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

  return(list('spp_counts' = spp_counts,
              'disp_sums' = disp_sums,
              'bio_plot' = bio_plot,
              'temp_plot' = temp_plot,
              'rpm_plot' = rpm_plot,
              'staff_plot' = staff_plot))
}
