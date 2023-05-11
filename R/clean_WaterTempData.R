#' @title clean_WaterTempData:
#' @description Cleans raw CDMS water temperature data
#' @param data raw CDMS water temperature dataset from \code{cdmsR::get_WaterTempData(year=2020)}
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' wt_raw <- cdmsR::get_WaterTempData()
#' wt_clean <- clean_WaterTempData(wt_raw)
clean_WaterTempData <- function(data){
  {if(is.null(data))stop("Water temperature data must be supplied")}

  # clean field names
  names(data) <- gsub(' ', '_', tolower(names(data)))

  # fix data types
  clean_df <- data %>%
    mutate(across(c(readingdatetime, activitydate, start_date, end_date, createdate), \(x) gsub('T',' ', x)),
           across(c(readingdatetime, activitydate, start_date, end_date, createdate), lubridate::ymd_hms)) %>%
    separate(readingdatetime, into = c('reading_date', 'reading_time'), sep = ' ', remove = FALSE) %>%
    mutate(reading_date = ymd(reading_date)) %>%
    select(locationlabel, instrument_name = name, readingdatetime, reading_date, reading_time, watertemperature,
           downloaded_by, comments, starts_with('instantaneous_'), activityid,
           downloaddate = activitydate, start_date, end_date, locationid, datasetid, instrumentid, everything()) %>%
    # remove system fields
    select(-ends_with('effdt'))

  # date_range <- tibble(
  #   reading_date = seq(lubridate::ymd(min(clean_df$reading_date, na.rm=TRUE)),
  #                    lubridate::ymd(max(clean_df$reading_date, na.rm=TRUE)), by="day"))
  #
  # complete_df <- clean_df %>%
  #   complete(reading_date = date_range$reading_date, nesting(locationlabel))


  return(clean_df)
}
