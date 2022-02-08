#' @title Estimate NOSA DES
#'
#' @param redd_data clean redd data
#' @param carcass_data clean carcass data with best age appended
#' @param mr_ests mark-recapture data
#'
#' @author Ryan N. Kinzer
#' @import dplyr
#' @return
#' @export
#'
#' @examples
#' get_NOSAests()
get_NOSAests <- function(redd_data, carcass_data, mr_ests, split_age = FALSE){

  r_df <- redd_data %>% filter(ReportingGroup != 'Meadow Creek') %>%
    mutate(r_EffDt = lubridate::ymd_hms(EffDt),
           r_EffDt = if_else(is.na(r_EffDt), lubridate::ymd_hms('19000101 00:00:00'), r_EffDt)) %>%
    ungroup() %>%
    select(-EffDt)

  c_df <- carcass_data %>% filter(ReportingGroup != 'Meadow Creek') %>%
    filter(CarcassSpecies == 'S_CHN') %>%
    mutate(c_EffDt = lubridate::ymd_hms(EffDt),
           c_EffDt = if_else(is.na(c_EffDt), lubridate::ymd_hms('19000101 00:00:00'), c_EffDt)) %>%
    ungroup() %>%
    select(-EffDt)

  mr_df <- mr_ests

  # get effective date

  eff_dt <- r_df %>%
    filter(Species == 'Chinook salmon',
           Run == 'Spring/summer') %>%
    group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear) %>%
    summarise(r_EffDt = max(r_EffDt, na.rm = TRUE)) %>%
    left_join(c_df %>%
                filter(Species == 'Chinook salmon',
                       Run == 'Spring/summer') %>%
                group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear) %>%
                summarise(c_EffDt = max(c_EffDt, na.rm = TRUE)),
              by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')) %>%
    rowwise() %>%
    mutate(EffDt = max(r_EffDt, c_EffDt, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-r_EffDt, -c_EffDt)

  # get redd nums
  ch_pop_df <-
    r_df %>%
    #select(MPG:SurveyDate, SurveyYear, Pass, AboveWeir, NewRedds) %>%
    #distinct() %>%
    filter(Species == 'Chinook salmon',
           Run == 'Spring/summer') %>%
    mutate(AboveWeir = case_when(AboveWeir == 'Yes' ~ 'U',
                                 TRUE ~ 'D')) %>%
    #group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, AboveWeir) %>%
    #summarise(n = sum(NewRedds, na.rm = TRUE)) %>%
    sum_Redds(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, AboveWeir) %>%
    pivot_wider(names_from = AboveWeir, values_from = Redds, names_prefix = 'R_', values_fill = list(Redds = 0))

  # get origin sums
  ch_pop_df <- ch_pop_df %>%
    left_join(
      c_df %>%
        filter(Species == 'Chinook salmon',
               Run == 'Spring/summer') %>%
        filter(Origin != 'Unknown') %>%
        #mutate(AboveWeir = case_when(AboveWeir == 'Yes' ~ 'U',
        #                             TRUE ~ 'D')) %>%
        group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, Origin) %>%#, AboveWeir) %>%
        summarise(n = n()) %>%
        #mutate(key = paste0(origin,'_',AboveWeir)) %>%
        #ungroup() %>%
        #select(-origin, -AboveWeir) %>%
        pivot_wider(names_from = Origin, values_from = n, names_prefix = 'n_', values_fill = list(n = 0)),
      by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')
    )

  # get sex sums
  ch_pop_df <- ch_pop_df %>%
    left_join(c_df %>%
                filter(Species == 'Chinook salmon',
                       Run == 'Spring/summer') %>%
                mutate(Sex = str_squish(Sex)) %>%
                filter(Sex != 'Unknown') %>%
                filter(Origin == 'Natural') %>%
                mutate(AboveWeir = case_when(AboveWeir == 'Yes' ~ 'U',
                                             TRUE ~ 'D')) %>%
                group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, Sex, AboveWeir) %>%
                summarise(n = n()) %>%
                mutate(key = paste0(Sex,'_',AboveWeir)) %>%
                ungroup() %>%
                select(-Sex, -AboveWeir) %>%
                pivot_wider(names_from = key, values_from = n, names_prefix = 'n_', values_fill = list(n = 0)),
              by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')
    )

  # get prespawn sums
  ch_pop_df <- ch_pop_df %>%
    left_join(c_df %>%
                filter(Species == 'Chinook salmon',
                       Run == 'Spring/summer') %>%
                mutate(Sex = str_squish(Sex)) %>%
                filter(Sex == 'Female') %>%
                filter(SpawnedOut %in% c('No', 'Yes')) %>%
                mutate(AboveWeir = case_when(AboveWeir == 'Yes' ~ 'U',
                                             TRUE ~ 'D')) %>%
                mutate(SpawnedOut = case_when(SpawnedOut == 'No' ~ 'Prespawn',
                                              SpawnedOut == 'Yes' ~ 'Spawned')) %>%
                group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, SpawnedOut, AboveWeir) %>%
                summarise(n = n()) %>%
                mutate(key = paste0(SpawnedOut,'_',AboveWeir)) %>%
                ungroup() %>%
                select(-SpawnedOut, -AboveWeir) %>%
                pivot_wider(names_from = key, values_from = n, names_prefix = 'n_', values_fill = list(n = 0)),
              by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')
    )

  ch_pop_df <- ch_pop_df %>%
    mutate(n_Female = n_Female_D + n_Female_U,
           n_Male = n_Male_D + n_Male_U,
           n_Prespawn = n_Prespawn_D + n_Prespawn_U,
           n_Spawned = n_Spawned_D + n_Spawned_U) %>%
    mutate_each(funs(replace(., which(is.na(.)), 0)))

  ch_pop_df <- ch_pop_df %>%
    bind_cols(est_proportion(ch_pop_df$n_Hatchery, (ch_pop_df$n_Hatchery + ch_pop_df$n_Natural)) %>%
                select_all(.funs = funs(paste0("pHOSij_",.)))) %>%
    bind_cols(est_proportion(ch_pop_df$n_Natural, (ch_pop_df$n_Hatchery + ch_pop_df$n_Natural)) %>%
                select_all(.funs = funs(paste0("pNOSij_",.)))) %>%
    rename(pHOSij = pHOSij_p, pNOSij = pNOSij_p) %>%
    bind_cols(est_proportion(ch_pop_df$n_Female, (ch_pop_df$n_Female + ch_pop_df$n_Male)) %>%
                select_all(.funs = funs(paste0("F_",.)))) %>%
    bind_cols(est_proportion(ch_pop_df$n_Prespawn, (ch_pop_df$n_Prespawn + ch_pop_df$n_Spawned)) %>%
                select_all(.funs = funs(paste0("Psp_",.))) %>%
                mutate_each(funs(replace(., which(is.na(.)), 0)))) %>%
    bind_cols(est_proportion(ch_pop_df$n_Female_D, (ch_pop_df$n_Female_D + ch_pop_df$n_Male_D)) %>%
                select_all(.funs = funs(paste0("F_D_",.)))) %>%
    bind_cols(est_proportion(ch_pop_df$n_Prespawn_D, (ch_pop_df$n_Prespawn_D + ch_pop_df$n_Spawned_D)) %>%
                select_all(.funs = funs(paste0("Psp_D_",.))))

  # append weir estimates
  weir_pop <- r_df %>% filter(Species == 'Chinook salmon',
                              Run == 'Spring/summer') %>%
    select(Species, POP_NAME, StreamName) %>% distinct()

  ch_pop_df <- ch_pop_df %>%
    left_join(
      mr_df %>%
        ungroup() %>%
        left_join(weir_pop, by = c('stream' = 'StreamName')) %>%
        mutate(WeirRemoval = Ponded + Disposed + Transferred) %>%
        mutate(Harvest = 0) %>%
        select(POP_NAME, Species, SurveyYear = trap_year, Weir = stream, WeirRemoval, Harvest, NOBroodStockRemoved, n1, n2, m2, N_U = N, SE_N_U = SE, lwr_N_U = lwr, upr_N_U = upr ),
      by = c('SurveyYear', 'POP_NAME', 'Species')) %>%
    mutate(tmp_f = case_when((n_Female_D + n_Male_D) <= 1 ~ F_p,
                             (F_D_SE/F_D_p) >= .5 ~ F_p,
                             TRUE ~ F_D_p),
           tmp_f_se = case_when((n_Female_D + n_Male_D) <= 1 ~ F_SE,
                                (F_D_SE/F_D_p) >= .5 ~ F_SE,
                                TRUE ~ F_D_SE))


  # Do we need weir efficiency, marking rate and carcass recovery rate?

  ch_pop_df <- ch_pop_df %>%
    bind_cols(est_abundance_sgs(ch_pop_df$R_D,
                                ch_pop_df$tmp_f,
                                ch_pop_df$tmp_f_se,
                                ch_pop_df$Psp_p,
                                ch_pop_df$Psp_SE
    ) %>%
      select(N_D = Nhat, SE_N_D = SE_Nhat, lwr_N_D = lwr_N, upr_N_D = upr_N)) %>%
    select(-tmp_f, -tmp_f_se)

  ch_pop_df <- ch_pop_df %>%
    mutate(WeirRemoval = ifelse(is.na(WeirRemoval), 0, WeirRemoval),
           Harvest = ifelse(is.na(Harvest), 0, Harvest),
           N_D = ifelse(is.nan(N_D), 0, N_D),
           SE_N_D = ifelse(is.nan(SE_N_D), 0, SE_N_D),
           lwr_N_D = ifelse(is.nan(lwr_N_D), 0, lwr_N_D),
           upr_N_D = ifelse(is.nan(upr_N_D), 0, upr_N_D)) %>%
    mutate(TribAbund = N_D + N_U,
           TribAbund_SE = sqrt(SE_N_D^2 + SE_N_U^2),
           Method = case_when(Weir == 'Secesh River' & !is.na(TribAbund) ~ 'DIDSON + Redd Expansion',
                              !is.na(TribAbund) ~ 'MR Weir + Redd Expansion'))
  # Could add fish per redd estimate!!!!

  ch_pop_df <- ch_pop_df %>%
    mutate(ReddExp = est_abundance_sgs(R_U + R_D,
                                       F_p,
                                       F_SE,
                                       Psp_p,
                                       Psp_SE)[,1],
           ReddExp_SE = est_abundance_sgs(R_U + R_D,
                                          F_p,
                                          F_SE,
                                          Psp_p,
                                          Psp_SE)[,2],
           Method = ifelse(is.na(Method) & !is.na(ReddExp), 'Redd Expansion', Method),
           TribAbund = ifelse(is.na(TribAbund), ReddExp, TribAbund),
           TribAbund_SE = ifelse(is.na(TribAbund_SE), ReddExp_SE, TribAbund_SE))

  ch_pop_df <- ch_pop_df %>%
    mutate(N = TribAbund + WeirRemoval + Harvest, # Trib Escapement
           N_SE = TribAbund_SE,
           TSAij = TribAbund * (1 - Psp_p), # Total Spawner Abundance
           TSAij_SE = error_propagation(fx = 'product', type = 'both_random',
                                        x = TribAbund,
                                        se.x = N_SE,
                                        y = (1-Psp_p),
                                        se.y = Psp_SE),
           TSAij_lwr = TSAij - 1.96*TSAij_SE,
           TSAij_upr = TSAij + 1.96*TSAij_SE,
           NOSAij = TSAij * pNOSij,
           NOSAij_SE = error_propagation(fx = 'product', type = 'both_random',
                                         x = TSAij,
                                         se.x = TSAij_SE,
                                         y = pNOSij,
                                         se.y = pNOSij_SE),
           NOSAij_lwr = NOSAij - 1.96*NOSAij_SE,
           NOSAij_upr = NOSAij + 1.96*NOSAij_SE,
           HOSAij = TSAij * pHOSij,
           HOSAij_SE = error_propagation(fx = 'product', type = 'both_random',
                                         x = TSAij,
                                         se.x = TSAij_SE,
                                         y = pHOSij,
                                         se.y = pHOSij_SE),
           HOSAij_lwr = HOSAij - 1.96*HOSAij_SE,
           HOSAij_upr = HOSAij + 1.96*HOSAij_SE
    )

if(split_age){
  ch_car_age <- c_df %>%
    filter(Species == 'Chinook salmon',
           Run == 'Spring/summer',
           Origin == 'Natural') %>%
    filter(ForkLength > 200) %>%
    filter(!is.na(BestAge)) %>%
    filter(BestAge != '-99') %>%
    group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, BestAge) %>%
    summarise(n = n()) %>%
    nest(n_age = c(BestAge, n)) %>%
    mutate(p_age = map(n_age, function(df) est_proportions(x = df$n, alpha = 0.05))) %>%
    unnest(cols = c(n_age, p_age)) %>%
    arrange(BestAge) %>%
    pivot_wider(names_from = BestAge, values_from = n:upr) %>%
    mutate_each(funs(replace(., which(is.na(.)), 0)))

  jack_frac_origin <- c_df %>%
    filter(Species == 'Chinook salmon',
           Run == 'Spring/summer',
           Origin != 'Unknown') %>%
    filter(ForkLength > 200) %>%
    filter(!is.na(BestAge)) %>%
    mutate(BestAge = ifelse(BestAge == '3', 'JF', 'AF'),
           Origin = ifelse(Origin == 'Natural', 'NOSJF','HOSJF')) %>%
    group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, Origin, BestAge) %>%
    summarise(n = n()) %>%
    nest(n_age = c(BestAge, n)) %>%
    mutate(p_age = map(n_age, function(df) est_proportions(x = df$n, alpha = 0.05))) %>%
    unnest(cols = c(n_age, p_age)) %>%
    filter(BestAge == 'JF') %>%
    select(-BestAge) %>%
    pivot_wider(names_from = Origin, values_from = n:upr) %>%
    mutate_each(funs(replace(., which(is.na(.)), 0))) %>%
    rename(HOSJF = p_HOSJF, NOSJF = p_NOSJF) %>%
    select(-n_HOSJF, -n_NOSJF)


  jack_frac_total <- c_df %>%
    filter(Species == 'Chinook salmon',
           Run == 'Spring/summer',
           Origin != 'Unknown') %>%
    filter(ForkLength > 200) %>%
    filter(!is.na(BestAge)) %>%
    mutate(BestAge = ifelse(BestAge == '3', 'JF', 'AF')) %>%
    group_by(MPG, POP_NAME, TRT_POPID, Species, Run, SurveyYear, BestAge) %>%
    summarise(n = n()) %>%
    nest(n_age = c(BestAge, n)) %>%
    mutate(p_age = map(n_age, function(df) est_proportions(x = df$n, alpha = 0.05))) %>%
    unnest(cols = c(n_age, p_age)) %>%
    filter(BestAge == 'JF') %>%
    select(MPG:SurveyYear, JF = p, JF_SE = SE)

  ch_car_age <- left_join(ch_car_age, jack_frac_total,
                          by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')) %>%
    left_join(jack_frac_origin,
              by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear')) %>%
  mutate_each(funs(replace(., which(is.na(.)), 0)))

  ch_pop_df <- left_join(ch_pop_df, ch_car_age,
                         by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear'))

  ch_pop_df <- ch_pop_df %>%
    mutate(NOSAej = NOSAij * (1-NOSJF),
           NOSAej_SE = error_propagation(fx = 'product', type = 'both_random',
                                         x = NOSAij,
                                         se.x = NOSAij_SE,
                                         y = (1-NOSJF),
                                         se.y = SE_NOSJF),
           NOSAej_lwr = NOSAej - 1.96*NOSAej_SE,
           NOSAej_upr = NOSAej + 1.96*NOSAej_SE,

           HOSAej = HOSAij * (1-HOSJF),
           HOSAej_SE = error_propagation(fx = 'product', type = 'both_random',
                                         x = HOSAij,
                                         se.x = HOSAij_SE,
                                         y = (1-HOSJF),
                                         se.y = SE_HOSJF),
           HOSAej_lwr = HOSAej - 1.96*HOSAej_SE,
           HOSAej_upr = HOSAej + 1.96*HOSAej_SE,

           TSAej = NOSAej + HOSAej,
           TSAej_SE = error_propagation(fx = 'addition', type = 'both_random',
                                        x = NOSAej,
                                        se.x = NOSAej_SE,
                                        y = HOSAej,
                                        se.y = HOSAej_SE),
           TSAej_lwr = TSAej - 1.96*TSAej_SE,
           TSAej_upr = TSAej + 1.96*TSAej_SE,

           pHOSej = HOSAej / TSAej,
           pHOSej_SE = error_propagation(fx = 'division', type = 'both_random',
                                         x = HOSAej,
                                         se.x = HOSAej_SE,
                                         y = TSAej,
                                         se.y = TSAej_SE),
           pHOSej_SE = ifelse(is.nan(pHOSej_SE),0,pHOSej_SE),
           pHOSej_lwr = pHOSej - 1.96*pHOSej_SE,
           pHOSej_upr = pHOSej + 1.96*pHOSej_SE,
           pHOSej_lwr = if_else(pHOSej_lwr < 0, 0, pHOSej_lwr))
}
  ch_pop_df <- ch_pop_df %>%
    mutate_each(funs(replace(., which(is.nan(.)), NA)))

  ch_pop_df <- right_join(eff_dt, ch_pop_df,
                          by = c('MPG', 'POP_NAME', 'TRT_POPID', 'Species', 'Run', 'SurveyYear'))

  return(ch_pop_df)
}
