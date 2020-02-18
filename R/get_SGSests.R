#' @title Summarize SGS Data
#' @description Summarize and estimate performance measures for SGS data.
#' @param redd_data cleaned redd data
#' @param carcass_data cleaned carcass data with best age appended
#' @param species Chinook or steelhead
#' @param run Spring/summer, fall, summer
#' @param group variables to group the data
#' @author Ryan N. Kinzer
#' @return
#' @export
#' @import dplyr
#' @examples
#' get_SGSests(clean_redd, clean_car)
get_SGSests <- function(redd_data = NULL, carcass_data = NULL, alpha = 0.05, ...){

  {if(is.null(redd_data) || is.null(carcass_data)) stop('must supply both SGS datasets')}
  redd_dat <- redd_data
  car_dat <- carcass_data

  # count groups first
  r_sum <- sum_Redds(redd_dat, ...)




  cnt <- cnt_groups(.data, .summary_var = !!summary_var, ...)

  # get redd nums
  dat <- #redd_sum %>%
    redd_dat %>%
    select(MPG:SurveyDate, SurveyYear, Pass, AboveWeir, NewRedds) %>%
    distinct() %>%
#    filter(Species == 'Chinook salmon',
#           Run == 'Spring/summer') %>%
#    mutate(AboveWeir = case_when(AboveWeir == 'Yes' ~ 'U',
#                                 TRUE ~ 'D')) %>%
#    group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, AboveWeir) %>%
    group_by(grp) %>%
    summarise(n = sum(NewRedds, na.rm = TRUE)) #%>%
#  pivot_wider(names_from = AboveWeir, values_from = n, names_prefix = 'R_', values_fill = list(n = 0))

  return(dat)
}
