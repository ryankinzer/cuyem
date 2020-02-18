#' @title Count Groups
#' @description Count records in a dataset within defined groups.
#' @param .data dataset
#' @param .summary_var variable to count
#' @param ... grouping variables
#' @author Ryan N. Kinzer
#' @return
#' @export
cnt_groups <- function(.data, .summary_var, ...){

  # quote count variable
  summary_var <- enquo(.summary_var)

  dat <- .data %>%
    group_by(...) %>%
    count(!!summary_var)
}
