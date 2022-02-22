#' @title Plotting function to help visualize strata for RST trap efficiencies
#' @description Function inside 'sum_RSTstrata()', Takes summarized RST data, both
#' daily and by strata, and plots captures/marks/recaptures to help assess
#' accuracy of strata.
#' @param daily_df Summarized P4 RST data with daily C/M/R tallies and strata.
#' @param strata_df Daily data further summarized by strata.
#' @param strata_dates Vector of dates (YYYY-MM-DD)input by user to establish
#' strata.  If NULL, function will automatically apply one-week strata.
#' @import dplyr
#' @author Tyler T. Stright
#' @examples
#' p4_raw <- get_P4Data(EventSite = 'IMNTRP', MigrationYear = 2020)
#' p4_clean <- clean_P4Data(p4_raw)
#' strata <- sum_RSTstrata(p4_clean, 'Spring', NULL)

gg_strata <- function(daily_df, strata_df) {

  # determine staff gauge used (RSTs should only use one)
  if(length(unique(daily_df$staff_gauge_cm)) > 1) {
    gauge_used = 'staff_gauge_cm'
    gauge_label = 'Staff Gauge (cm)'}
  if(length(unique(daily_df$staff_gauge_ft)) > 1) {
    gauge_used = 'staff_gauge_ft'
    gauge_label = 'Staff Gauge (ft)'}

  # placements, colors, and other plot helpers
  scaleFactor <- round(max(daily_df[ ,'trap_rpm'], na.rm=TRUE)/max(daily_df[ ,paste(gauge_used)], na.rm=TRUE), 5)
  y_strata <- max(daily_df[ ,paste(gauge_used)])*1.28
  y_c <- max(daily_df[ ,paste(gauge_used)])*1.23
  y_m <- max(daily_df[ ,paste(gauge_used)])*1.19
  y_r <- max(daily_df[ ,paste(gauge_used)])*1.15
  y_c2 <- max(daily_df[ ,paste(gauge_used)])*1.09
  y_m2 <- max(daily_df[ ,paste(gauge_used)])*1.05
  y_r2 <- max(daily_df[ ,paste(gauge_used)])*1.01

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
    # CMR labels - 12W / Strata
    annotate(geom = 'text',
             label = c('Strata', '12W: C', '12W: M', '12W: R'),
             x = rep(min(daily_df$event_date), 4),
             y = c(y_strata*annotate_dodge, y_c*annotate_dodge, y_m*annotate_dodge, y_r*annotate_dodge),
             color = c('black', color_c, color_m, color_r)) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_c, label = C_12W), color = color_c) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_m, label = M_12W), color = color_m) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_r, label = R_12W), color = color_r) +
    # CMR labels - 32W
    annotate(geom = 'text',
             label = c('32W: C', '32W: M', '32W: R'),
             x = rep(min(daily_df$event_date), 3),
             y = c(y_c2*annotate_dodge, y_m2*annotate_dodge, y_r2*annotate_dodge),
             color = c(color_c, color_m, color_r)) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_c2, label = C_32W), color = color_c) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_m2, label = M_32W), color = color_m) +
    geom_text(data = strata_df, aes(x = strata_labeldate, y = y_r2, label = R_32W), color = color_r) +
    # geom_text(mapping = aes(x = rep(ymd('2021-01-01'), 3), y = c(y_c, y_m, y_r), label = c('Captures', 'Marks', 'Recaptues'))) +
    theme_bw() +
    theme(
      axis.title.y.left=element_text(color="blue", size = 13, family = 'serif'),
      axis.text.y.left=element_text(color="blue", size = 11, family = 'serif'),
      axis.title.y.right=element_text(color="black", size = 13, family = 'serif'),
      axis.text.y.right=element_text(color="black", size = 11, family = 'serif'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
