#' @title Mark-Recapture Estimates
#'
#' @description Summarize mark-recapture data from a FINS database trapping query and CDMS spawning ground survey carcass dataset. Steelhead mark-recapture summaries only require the trapping data while Chinook also needs carcass data.
#'
#' @param species Chinook or Steelhead
#' @param weir_data cleaned FINS weir/trapping query
#' @param carcass_data cleaned CDMS carcass dataset
#' @param alpha type I error rate.  Default is set at 0.05 to produce 95\%
#'   confidence intervals.
#'
#' @author Ryan N. Kinzer
#' @export
#' @import dplyr
#' @examples
#' get_WeirMR(weir_data)
sum_WeirMR <- function(weir_data, carcass_data = NULL, species = c('Chinook', 'Steelhead'), alpha = 0.05, ...){
  spp <- match.arg(species)
  w_df <- weir_data
  c_df <- carcass_data

  {if(spp == 'Chinook' && is.null(carcass_data)) stop("carcass data must be supplied for Chinook summaries")}

  if(spp == 'Steelhead'){

    n1 <- w_df %>%
      filter(target_species == species) %>%
      filter(species == 'Steelhead') %>%
      #filter(stream != 'Lostine River') %>%
      filter(marked) %>%
      group_by(...) %>%
      summarise(n1= n())# %>%
      #arrange(stream, trap_year)

    n2 <- w_df %>%
      filter(target_species == species) %>%
      filter(species == 'Steelhead') %>%
      #filter(stream != 'Lostine River') %>%
      filter(release_dwn) %>%
      group_by(...) %>%
      summarise(n2 = n())# %>%
      #arrange(stream, trap_year)

    m2 <- w_df %>%
      filter(target_species == species) %>%
      filter(species == 'Steelhead') %>%
      #filter(stream != 'Lostine River') %>%
      filter(recapped == TRUE) %>%
      group_by(...) %>%
      summarise(m2 = n())# %>%
      #arrange(stream, trap_year)

    mr_df <- left_join(n1,n2) %>% # by = c('trap_year', 'stream', 'species')) %>%
      left_join(m2) #, by = c('trap_year', 'stream', 'species'))
  }

  if(spp == 'Chinook'){

    n1 <- w_df %>%
      filter(target_species == species) %>%
      filter(species == 'Chinook') %>%
      filter(marked) %>%
      filter(final_location == 'Upstream') %>%
      group_by(...) %>%
      summarise(n1= n())# %>%
      #arrange(stream, trap_year)

    car_up <- c_df %>%
      filter(AboveWeir == 'Yes') %>%
      filter(CarcassSpecies == 'S_CHN') %>% # REMOVE NON-TARGET THIS WAY!!!!!!
      filter(Species == 'Chinook salmon' & Run == 'Spring/summer') %>%
      filter(ForkLength > 200 | is.na(ForkLength)) %>%
      filter(ReportingGroup %in% unique(n1$stream))

    n2 <- car_up %>%
      filter(Mark_Discernible) %>%
      group_by(...) %>%
      #summarise(n2 = n())
      summarise(n2 = sum(Count))

    m2 <- car_up %>%
      filter(Recapture) %>%
      group_by(...) %>%
      #summarise(m2 = n())
      summarise(m2 = sum(Count))

    mr_df <- left_join(n1,n2) %>% #, by = c('trap_year' = 'SurveyYear', 'stream' = 'ReportingGroup')) %>%
      left_join(m2) #, by = c('trap_year' = 'SurveyYear', 'stream' = 'ReportingGroup'))
  }
#
#   mr_df <- mr_df %>%
#     bind_cols(est_abundance(mr_df$n1, mr_df$n2, mr_df$m2, method = 'adjusted Peterson', alpha))
#
#   if(species == 'Chinook'){
#
#   didson <- data.frame(
#     trap_year = 2004:2018,
#     stream = as.character(rep('Secesh River', 15)),
#     n1 = rep(NA, 15),
#     n2 = rep(NA, 15),
#     m2 = rep(NA, 15),
#     species = as.character(rep('Chinook', 15,)),
#            N = c(914, 334, 223, 301, 901, 1139, 1155, 923, 909, 889, 1467, 391,
#                    536, 479, 555),
#           SE = c(98.712, 3.34, 7.582, 12.04, 33.337, 78.591, 24.255, 18.46,
#                    24.543, 29.337, 32.274, 28.152, 24.656, 10.538, 13.875),
#     stringsAsFactors = FALSE)
#
#   didson <- didson %>%
#     mutate(lwr = N - qnorm(1-alpha/2)*SE,
#            upr = N + qnorm(1-alpha/2)*SE)
#
#   mr_df <- bind_rows(mr_df, didson)
#   }

  return(mr_df)
}
