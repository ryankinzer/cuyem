#' @title Estimate juvenile arrival timing at Lower Granite Dam.
#'
#' @description \code{arrival_LGR} generates arrival timing at Lower Granite
#'   Dam (LGR) for juvenile Chinook salmon and Steelhead marked at Nez Perce
#'   Tribe Rotary Scew Trap Sites for a single migration year. Using data
#'   queried from PTAGIS, dates are calculated for when 0%, 10%, 50%, 90%, and
#'   100% of PIT-tagged fish are first detected at LGR for the migratory year.
#'
#' @param ptagis_data File path to raw PTAGIS data (CSV) for the tagging year in question
#'  (July 1 - June 30). Data should at minimum contain the following fields:
#'   Species Name, Tag Code, Mark Site Name, Rear Type Code, Obs Date MMDDYYYY,
#'   Mark Date MMDDYYYY, and RST Mark Sites for observations at
#'   LGR juvenile (LGJ).
#'
#' @author Tyler Stright
#'
#' @examples arrival_LGR(ptagis_data = './data/RST LGR Arrival MY2019.csv')
#'
#' @import dplyr, lubridate, purrr, readr
#' @export
#' @return NULL

arrival_LGR <- function(ptagis_data) {

  roxygen2::roxygenize()

  raw_data <- readr::read_csv(ptagis_data)

  mark_sites <- sort(unique(raw_data$`Mark Site Name`))

  # Quantile Calculation ----
  nat_q <- map_df(.x= mark_sites,
                  .f= function(x){
                    # filter for first detections, natural origin
                    tmp <- raw_data %>%
                      filter(`Mark Site Name` == x,
                             `Rear Type Code` == 'W') %>%
                      arrange(`Obs Date MMDDYYYY`) %>%
                      distinct(`Tag Code`, .keep_all=TRUE)

                    # split by species
                    chn_tmp <- tmp %>% filter(`Species Name` == 'Chinook')
                    sth_tmp <- tmp %>% filter(`Species Name` == 'Steelhead')

                    # get quantile values
                    chn_qvals <- quantile(chn_tmp$`Obs Date MMDDYYYY`, probs = c(0, .1, .5, .9, 1), names = TRUE, type = 1)
                    sth_qvals <- quantile(sth_tmp$`Obs Date MMDDYYYY`, probs = c(0, .1, .5, .9, 1), names = TRUE, type = 1)

                    # Create and combine dataframes
                    Q_chn <- data.frame(probs= names(chn_qvals),
                                        date = mdy(chn_qvals),
                                        ReleaseSite = x,
                                        Origin = 'Natural',
                                        Species = 'Chinook')

                    Q_sth <- data.frame(probs= names(sth_qvals),
                                    date = mdy(sth_qvals),
                                    ReleaseSite = x,
                                    Origin = 'Natural',
                                    Species = 'Steelhead')

                    Q <- bind_rows(Q_chn, Q_sth) %>%
                      group_by(Species, ReleaseSite) %>%
                      spread(key= probs, value= date)

                  })

  hat_q <- map_df(.x= mark_sites,
                  .f= function(x){
                    # filter for first detections, natural origin
                    tmp <- raw_data %>%
                      filter(`Mark Site Name` == x,
                             `Rear Type Code` == 'H') %>%
                      arrange(`Obs Date MMDDYYYY`) %>%
                      distinct(`Tag Code`, .keep_all=TRUE)

                    # split by species
                    chn_tmp <- tmp %>% filter(`Species Name` == 'Chinook')
                    sth_tmp <- tmp %>% filter(`Species Name` == 'Steelhead')

                    # get quantile values
                    chn_qvals <- quantile(chn_tmp$`Obs Date MMDDYYYY`, probs = c(0, .1, .5, .9, 1), names = TRUE, type = 1)
                    sth_qvals <- quantile(sth_tmp$`Obs Date MMDDYYYY`, probs = c(0, .1, .5, .9, 1), names = TRUE, type = 1)

                    # Create and combine dataframes
                    Q_chn <- data.frame(probs= names(chn_qvals),
                                        date = mdy(chn_qvals),
                                        ReleaseSite = x,
                                        Origin = 'Hatchery',
                                        Species = 'Chinook')

                    Q_sth <- data.frame(probs= names(sth_qvals),
                                        date = mdy(sth_qvals),
                                        ReleaseSite = x,
                                        Origin = 'Hatchery',
                                        Species = 'Steelhead')

                    Q <- bind_rows(Q_chn, Q_sth) %>%
                      group_by(Species, ReleaseSite) %>%
                      spread(key= probs, value= date)

                  })

  # combine dataframes
  quantile_df <- bind_rows(nat_q, hat_q) %>%
    select(`Release Site` = ReleaseSite, Species, Origin, '0%', '10%', '50%', '90%', '100%')

  return(quantile_df)
}
