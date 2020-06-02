#' @title Clean Weir Trapping Data
#' @description Processes the raw FINS trapping query and adds important fields for summaries (e.g., Survey Year, Reporting Group, Transect Name, Above Weir, Origin, Mark and Recapture etc.)
#' @param data raw CDMS redd dataset from \code{cdmsR::getDatasetView(datastore = 78)} or from CDMS dataset export
#' @export
#' @import dplyr
#' @author Ryan N. Kinzer
#' @examples
#' con <- RODBC::odbcConnect(dsn = 'Fins', uid = 'guest', pwd = 'guest')
#' qry <- "SELECT distinct(Facility) FROM FINS_all_trapping"
#' f <- RODBC::sqlQuery(con, qry) %>% mutate_all(as.character) %>% pull(Facility)
#' npt_f <- f[grepl('NPT', f)]
#' weir_dat <- get_FinsTrapping(npt_f, odbc_connection = con)
#' tmp <- clean_weirData(weir_dat)
clean_weirData <- function(data){
  {if(is.null(data))stop("weir data must be supplied")}

  # create date/time fields, and mark-recapture fields

  names(data) <- gsub(' ','_',tolower(names(data)))

  #data <- mutate_all(as.character)

  trap_df <- data %>%
    mutate(trapped_date = gsub('T\\d{2}:\\d{2}:\\d{2}', '', trapped_date),
           trapped_date = lubridate::ymd(trapped_date),
           trap_year = lubridate::year(trapped_date)) %>%
    mutate(weir = str_split(trap, ' - ', simplify =  TRUE)[,1]) %>%
    mutate(stream = str_replace(weir, ' Weir', ''),
           stream = str_replace(stream, 'Upper ', '')) %>%
    #mutate(applied_marks = gsub(' ', '', applied_marks)) %>%
    mutate(release_up = case_when(disposition == 'Released' &
                                    grepl('Above|Upstream', release_site) ~ TRUE,
                                  TRUE ~ FALSE)) %>%
    mutate(release_dwn = case_when(disposition == 'Released' &
                                     grepl('Below|Downstream', release_site) ~ TRUE,
                                   living_status %in% c('DOA', 'TrapMort') &
                                     grepl('Below|Downstream', moved_to) ~ TRUE, # added to catch downstream morts
                                   TRUE ~ FALSE)) %>%
    mutate(marked = case_when(release_up & grepl('OP', applied_marks) ~ TRUE,
                              release_up & grepl('OP', applied_tags) ~ TRUE,
                              TRUE~FALSE),
           recapped = case_when(release_dwn & grepl('OP', existing_marks) ~ TRUE,
                                release_dwn & grepl('OP', existing_tags) ~ TRUE,
                                TRUE ~ FALSE),
           recap = as.logical(recap)) %>%
    mutate(existing_tags = ifelse(is.na(existing_tags), 'None', existing_tags),
           tmp_id = 1:n(),
           pit = ifelse(!is.na(existing_pit), existing_pit, applied_pit),
           op = ifelse(str_detect(existing_tags, 'OP'), str_extract(existing_tags, 'OP \\d+'), str_extract(applied_tags, 'OP \\d+')),
           op = str_extract(op, '\\d+'),
           tmp_fish_id = ifelse(!is.na(pit), pit,
                            ifelse(!is.na(op), paste0(trap_year, '-',op), tmp_id))) %>%
    arrange(tmp_fish_id, trapped_date) %>%
    group_by(tmp_fish_id) %>%
    mutate(current_location = ifelse(grepl('Upstream|Above', release_site), 'Upstream',
                        ifelse(grepl('Downstream|Below', release_site), 'Downstream', NA)))

  trap_df <- trap_df %>%
    left_join(trap_df %>%
                group_by(tmp_fish_id) %>%
                slice(which.max(trapped_date)) %>%
                select(tmp_fish_id, final_location = current_location), by = 'tmp_fish_id') %>%
    select(trap_year, trapped_date, tmp_fish_id, current_location, final_location, everything()) %>%
    select(-tmp_id, -pit, -op)

  return(trap_df)

  }
