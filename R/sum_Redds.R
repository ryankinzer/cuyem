
#' @title Sum Redd Counts
#'
#' @param redd_data cleaned redd data
#' @param ... grouping variables
#'
#' @author Ryan N. Kinzer
#' @import dplyr
#' @return
#' @export
#'
#' @examples
#' redd_df <- cdmsR::getDatasetView(datastoreID = 78)
#' redd_dat <- clean_ReddData(redd_df)
#' sum_Redds(redd_dat, SurveyYear, StreamName, TransectName)
sum_Redds <- function(.redd_data, ...){

  {if(is.null(.redd_data)) stop('must supply redd dataset')}
  redd_dat <- .redd_data

  # get redd nums
  dat <- redd_dat %>%
    distinct(ActivityId, .keep_all = TRUE) %>%
    group_by(...) %>%
    summarise(Redds = sum(NewRedds, na.rm = FALSE))

  return(dat)
}
