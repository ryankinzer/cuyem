#' @title Summarize SGS Data
#' @description Summarize and estimate performance measures for SGS data.
#' @param redd_data cleaned redd data
#' @param carcass_data cleaned carcass data
#' @param ... variables to group the data
#' @author Ryan N. Kinzer, Tyler T. Stright
#' @return
#' @export
#' @import dplyr
#' @examples
#' get_SGSests(clean_redd, clean_car)
get_SGSests <- function(redd_data = NULL, carcass_data = NULL, ...){

  {if(is.null(redd_data) || is.null(carcass_data)) stop('must supply both SGS datasets')}
  redd_dat <- redd_data
  car_dat <- carcass_data

  # Join vars
  vars <- gsub('~', '', map_chr(quos(...), deparse))

  # redd count
  r_sum <- sum_Redds(redd_dat, ...)

  # # carcass count
  c_sum <- cnt_groups(car_dat %>% filter(Count != 0), Count, ...) %>%
    rename(Carcasses = n) %>%
    select(-Count)

  # % hatchery origin spawners
  phos <- est_group_p(car_dat %>% filter(Origin %in% c('Natural', 'Hatchery')),
                      Origin, alpha = 0.05, ...) %>%
    filter(Origin == 'Hatchery') %>%
    select(-Origin, -n)

  names(phos)[(ncol(phos)-3):ncol(phos)] <- paste('phos_', names(phos)[(ncol(phos)-3):ncol(phos)], sep = '')

  # % female
  pfem <- est_group_p(car_dat %>% filter(Sex %in% c('Male', 'Female')),
                      Sex, alpha = 0.05, ...) %>%
    filter(Sex == 'Female') %>%
    select(-Sex, -n)

  names(pfem)[(ncol(pfem)-3):ncol(pfem)] <- paste('pfem_', names(pfem)[(ncol(pfem)-3):ncol(pfem)], sep = '')

  # prespawn mortality
  psm <- est_group_p(car_dat %>% filter(Sex == 'Female',
                                                 SpawnedOut %in% c('Yes','No')),
                              SpawnedOut, alpha = 0.05, ...) %>%
    filter(SpawnedOut == 'No') %>% # not spawned = prespawn mort.
    select(-SpawnedOut, -n)

  names(psm)[(ncol(psm)-3):ncol(psm)] <- paste('psm_', names(psm)[(ncol(psm)-3):ncol(psm)], sep = '')

  # join
  SGS_ests <- full_join(r_sum, c_sum, by = vars) %>%
    full_join(phos, by = vars) %>%
    full_join(pfem, by = vars) %>%
    full_join(psm, by = vars)

  return(SGS_ests)
}
