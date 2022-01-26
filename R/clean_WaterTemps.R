#' @title Clean Water Temperature Data
#' @description Processes the raw CDMS water temperature dataset
#' @param data raw CDMS water temperature dataset from \code{cdmsR::getWaterTemps(start_date = 'mm-dd-yyyy', end_date = 'mm-dd-yyyy')}
#' @export
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' wt_dat <- cdmsR::getWaterTemps(datastore = ????)
#' clean_harvestData(harvest_dat)
clean_WaterTemps <- function(data){
  {if(is.null(data))stop("Water temperature data must be supplied")}  # I Think this is CREEL

  # testing
  load(file = './data/wt_raw.rda')
  data <- wt_raw

  # clean field names
  names(data) <- gsub(' ', '_', tolower(names(data)))

  # fix data types
  clean_df <- data %>%
    mutate(across(c(readingdatetime, activitydate, start_date, end_date, createdate), lubridate::ymd_hms),
           across(c(activitydate, start_date, end_date), lubridate::ymd)) %>%
    separate(readingdatetime, into = c('reading_date', 'reading_time'), sep = ' ', remove = FALSE) %>%
    mutate(reading_date = ymd(reading_date)) %>%
    select(locationlabel, instrument_name = name, readingdatetime, reading_date, reading_time, watertemperature,
           downloaded_by, comments, starts_with('instantaneous_'), activityid, downloaddate = activitydate, start_date, end_date, locationid, datasetid, instrumentid, everything()) %>%
    # remove system fields
    select(-ends_with('effdt'))


  return(clean_df)
}
