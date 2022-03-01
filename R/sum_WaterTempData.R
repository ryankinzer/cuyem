#' @title sum_WaterTempData:
#'
#' @description summarizes cleaned water temperature data
#'
#' @param data CDMS water temperature data from get_WaterTempData() %>% clean_WaterTempData()
#'
#' @return dataframe containing summarized water temperature metrics
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' wt_raw <- get_WaterTempData(date_begin = '2009-07-01', date_end = '2009-11-30', locationID = 1605)
#' wt_clean <- clean_WaterTempData(wt_raw)
#' wt_sum <- sum_WaterTempData(wt_clean)

sum_WaterTempData <- function(data){

  if(is.null(data)){stop("A data frame containing prepared water temperature data must be provided.")}

  # daily average by date/location/instrument name
  sum_df <- data %>%
    arrange(readingdatetime) %>%
    group_by(locationlabel, reading_date, instrument_name) %>%
    summarize(daily_mean = mean(watertemperature),
              daily_min = min(watertemperature),
              daily_max = max(watertemperature)) %>%
    ungroup() %>%
    mutate(`7dma` = (1/7)*(daily_mean +
                             dplyr::lag(daily_mean, n = 6) +
                             dplyr::lag(daily_mean, n = 5) +
                             dplyr::lag(daily_mean, n = 4) +
                             dplyr::lag(daily_mean, n = 3) +
                             dplyr::lag(daily_mean, n = 2) +
                             dplyr::lag(daily_mean, n = 1))) %>%
    mutate(`7dadm` = (1/7)*(daily_max +
                              dplyr::lag(daily_max, n = 6) +
                              dplyr::lag(daily_max, n = 5) +
                              dplyr::lag(daily_max, n = 4) +
                              dplyr::lag(daily_max, n = 3) +
                              dplyr::lag(daily_max, n = 2) +
                              dplyr::lag(daily_max, n = 1)))

  cat('7dma = 7 day moving average',
      '7dadm = 7 day average daily maximum', sep = '\n')

  return(sum_df)
}
