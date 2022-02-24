#' @title gg_strata:
#'
#' @description Function inside 'sum_RSTstrata()', Takes summarized RST data, both
#' daily and by strata, and plots captures/marks/recaptures to help assess
#' accuracy of strata. Not intended for use outside of sum_RSTstrata().
#'
#' @param daily_df Summarized P4 RST data with daily C/M/R tallies and strata.
#' Produced by sum_RSTstrata().
#'
#' @param strata_df Daily data further summarized by strata. Produced by sum_RSTstrata().
#'
#' @param strata_dates Vector of dates (YYYY-MM-DD) input by user to establish
#' strata.  If NULL, function will automatically apply 7-day strata.
#'
#' @import dplyr
#'
#' @author Tyler T. Stright
#'
#' @examples

gg_strata <- function(daily_df, strata_df, species = c('Chinook', 'Steelhead')) {

  # determine staff gauge used (RSTs should only use one)
  if(length(unique(daily_df$staff_gauge_cm)) > 1) {
    gauge_used = 'staff_gauge_cm'
    gauge_label = 'Staff Gauge (cm)'}
  if(length(unique(daily_df$staff_gauge_ft)) > 1) {
    gauge_used = 'staff_gauge_ft'
    gauge_label = 'Staff Gauge (ft)'}

  # placements, colors, and other plot helpers
  scaleFactor <- round(max(daily_df[ ,'trap_rpm'], na.rm=TRUE)/max(daily_df[ ,paste(gauge_used)], na.rm=TRUE), 5)
  y_strata <- max(daily_df[ ,paste(gauge_used)], na.rm = TRUE)*1.13
  y_c <- max(daily_df[ ,paste(gauge_used)], na.rm = TRUE)*1.09
  y_m <- max(daily_df[ ,paste(gauge_used)], na.rm = TRUE)*1.05
  y_r <- max(daily_df[ ,paste(gauge_used)], na.rm = TRUE)*1.01

  annotate_dodge <- 1.015
  color_c <- 'darkgreen'
  color_m <- 'blue'
  color_r <- 'red'

  # g_strata <-
  ggplot(data = daily_df, aes(x=event_date)) +
    # flow
    geom_line(aes_string(y=gauge_used), color = 'blue') +
    # RPM
    geom_line(aes(y=trap_rpm/scaleFactor), color = 'black', linetype = 'solid') +
    # Y axis - double
    scale_y_continuous(name= paste0(gauge_label), breaks = scales::breaks_pretty(7), # gauge_label
                       sec.axis=sec_axis(~.*scaleFactor, name = expression("Trap RPM", breaks = scales::breaks_pretty(7)))) +
    # X axis
    scale_x_date(name = 'Date', breaks = strata_df$strata_start, labels = scales::label_date("%m/%d")) +
    # strata lines and labels
    geom_vline(xintercept = strata_df$strata_start, linetype = 'dotted') +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_strata, label = strata_n)) +
    # Strata & CMR text
    annotate(geom = 'text',
             label = c('S', 'C', 'M', 'R'),
             x = rep(min(daily_df$event_date-days(3)), 4),
             y = c(y_strata, y_c, y_m, y_r),
             color = c('black', color_c, color_m, color_r)) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_c, label = C), color = color_c) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_m, label = M), color = color_m) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_r, label = R), color = color_r) +
    # theme
    theme_bw() +
    theme(
      axis.title.y.left=element_text(color="blue", size = 13, family = 'serif'),
      axis.text.y.left=element_text(color="blue", size = 11, family = 'serif'),
      axis.title.y.right=element_text(color="black", size = 13, family = 'serif'),
      axis.text.y.right=element_text(color="black", size = 11, family = 'serif'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle(label = paste0(species, ' Strata Visualization'))
}
