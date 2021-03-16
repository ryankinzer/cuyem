#' @title Sum Groups by Count Field
#' @description Sum a count field for records in a dataset within defined groups.
#' @param .data dataset
#' @param .sum_var variable to sum
#' @param ... grouping variables
#' @author Ryan N. Kinzer
#' @return
#' @export
sum_groups <- function(.data, .summary_var, .cnt_var, ...){

  # quote count variable
  summary_var <- enquo(.summary_var)
  cnt_var <- enquo(.cnt_var)

  # n_name <- paste('n_', quo_name(summary_var), sep='')

  dat <- .data %>%
    group_by(..., !!summary_var) %>%
    summarise(n = sum(!!cnt_var, na.rm = TRUE)) %>%
    ungroup()

  # colnum <- ncol(dat)
  # colnames(dat)[colnum] <- n_name

  return(dat)
}
