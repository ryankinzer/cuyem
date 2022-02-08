#' @title Clean Redd Data
#' @description Processes the raw CDMS redd dataset and adds important fields for summaries (e.g., Survey Year, Reporting Group, Transect Name, Above Weir, etc.)
#' @param data raw CDMS redd dataset from \code{cdmsR::getDatasetView(datastore = 78)} or from CDMS dataset export
#' @export
#' @import dplyr
#' @author Ryan N. Kinzer
#' @examples
#' redd_dat <- cdmsR::getDatasetView(datastore = 78)
#' clean_reddData(redd_dat)
clean_reddData <- function(data){
  {if(is.null(data))stop("redd data must be supplied")}

    redd_dat <- data %>%
      mutate(TransectName = str_split(LocationLabel, 'Transect - ', simplify = TRUE)[,2]) %>%
      left_join(transect_meta, by = c('Species', 'Run', 'StreamName', 'TransectName')) %>%
      mutate(#SurveyDate = lubridate::date(lubridate::ymd_hms(SurveyDate)),
             SurveyYear = lubridate::year(SurveyDate)) %>%
      select(ESU_DPS:Run, ReportingGroup, StreamName, TribToName, LocationLabel, TransectName, SurveyYear, everything())

    return(redd_dat)
}
