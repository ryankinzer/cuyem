#' @title Estimate Group Proportions
#' @description Count and estimate proportions with precision for defined grouping variables.
#' @param .data dataset
#' @param .summary_var variable to count and estimate proportions
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#' @param ... grouping variables
#' @author Ryan N. Kinzer
#' @import dplyr
#' @return
#' @export
#' @examples
#' car_df <- cdmsR::getDatasetView(datastoreID = 79)
#' car_dat <- clean_ReddData(car_df)
#' est_group_p(car_dat, Sex, alpha = 0.05, SurveyYear, StreamName, Species, Run)
est_group_p <- function(.data, .summary_var, alpha = 0.05, ...){

  # quote field names
  summary_var <- enquo(.summary_var)

  # count groups first
  cnt <- cnt_groups(.data, .summary_var = !!summary_var, ...)

  p_df <- cnt %>%
    nest(cnt = c(!!summary_var, n)) %>%
    mutate(p_df = map(cnt, function(df) est_proportions(x = df$n, alpha = alpha))) %>%
    unnest(cols = c(cnt, p_df))

  return(p_df)
}
