#' @title sum_Lamprey:
#'
#' @description Summarize P4 lamprey catch for BPA deliverable.
#'
#' @param data cleaned P4 data queried for a single calendar year.
#'
#' @author Tyler T. Stright
#'
#' @import dplyr lubridate
#'
#' @return
#'
#' @export
#'
#' @examples
#' data_raw <- get_P4Data(SRRcode = 'A0W', CalendarYear = 2021)
#' data_clean <- cuyem::clean_p4Data(data_raw)
#' lamprey_sum <- sum_Lamprey(data_clean

sum_Lamprey <- function(data) {

  {if(is.null(data)) stop('You must supply clean P4 data.')}

  year <- unique(lubridate::year(data$event_date))

  {if(length(year)>1) stop('Data can only contain a single calendar year.')}

  date_range <- tibble(
    event_date = seq(lubridate::ymd(paste0(year, '-01-01')),
                     lubridate::ymd(paste0(year, '-12-31')), by="day"))

  # summarize daily by site & lifestage
  data_spread <- data %>%
    filter(speciesrunreartype == 'A0W') %>%
    mutate(genetics_taken = if_else(is.na(geneticid), 'no', 'yes'),
           lifestage = if_else(is.na(lifestage), 'Unknown', lifestage)) %>%
    group_by(eventsite, event_date, speciesrunreartype, lifestage, genetics_taken) %>%
    summarize(catch = sum(nfish)) %>%
    spread(key = lifestage, value = catch, fill = 0)

  # insert lifestage columns if needed
  if(!'Ammocoete' %in% names(data_spread)) {data_spread$Ammocoete <- 0}
  if(!'Macropthalmia' %in% names(data_spread)) {data_spread$Macropthalmia <- 0}
  if(!'Adult' %in% names(data_spread)) {data_spread$Adult <- 0}
  if(!'Unknown' %in% names(data_spread)) {data_spread$Unknown <- 0}

  # complete all dates, they want zeros.
  data_complete <- data_spread %>%
    ungroup() %>%
    complete(event_date = date_range$event_date, nesting(eventsite),
             fill = list(speciesrunreartype = 'A0W', genetics_taken = 'no',
                         Ammocoete = 0, Macropthalmia = 0, Adult = 0, Unknown = 0)) %>%
    arrange(eventsite, event_date)

  return(data_complete)
}
