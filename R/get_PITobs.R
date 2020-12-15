#' @title DART PIT tag Observation Data
#'
#' @description Query DART's PIT tag observation detail records.
#'
#' @author Ryan N. Kinzer
#'
#' @param query_type Query PIT-tag observation data for either an observation site (default) or
#' a release site.
#' @param obs_site For use with observation site query. Currently only available for Lower Granite Dam (GRA, default) and Bonneville (B2A).
#' @param release_site For use with release site query. Use PTAGIS release codes.
#' @param species Species to query. for window counts. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param run
#' @param rear_type
#' @param start_date Start date with format mm/dd/yyyy
#' @param end_date End date with format mm/dd/yyyy
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import httr
#' @export
#' @return NULL
#' @examples
#'
#' queryPITtagAdult(spawn_yr = 2015)

get_PITobs = function(query_type = c('obs_site', 'release_site'),
                         obs_site = c('GRA','B2A'), # many others
                         release_site = NULL,
                         species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                         run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                         rear_type = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                         start_date = NULL,
                         end_date = NULL) {

  # pull out default params and check for problems
  query_type <- match.arg(query_type)

  stopifnot(!is.null(start_date)|!is.null(end_date))

  #stopifnot(!is.null(obs_year), obs_year >= 1989) #spawn_yr = NULL, #1988 to current for GRA and 1995 for B2A

  obs_site <- match.arg(obs_site, several.ok = FALSE)
  species <- match.arg(species, several.ok = FALSE)
  run <- match.arg(run, several.ok = FALSE)
  rear_type <- match.arg(rear_type, several.ok = FALSE)

  # set up default start and end days
  start_day <- substr(start_date,1,5)
  syear <- substr(start_date, 7,10)

  end_day <- substr(end_date,1,5)
  eyear <- substr(end_date, 7,10)

  {if(as.numeric(eyear) - as.numeric(syear) > 1) stop("year range must be less than 2 years")}

  if(as.numeric(eyear) - as.numeric(syear) == 0){
      span <- 'no'
    } else {
      span <- 'yes'
    }

  # match up site code with site name
  obs_code_df <- data.frame(site = c('GRA', 'B2A'),
                             code = c('GRA:Lower Granite Dam Adult Fishway (GRA) rkm 522.173',
                                      'B2A:Bonneville Dam Adult Fishways (B2A BO1 BO2 BO3 BO4 BWL) rkm 234'))

  obs_code = obs_code_df$code[match(obs_site, obs_code_df$site)]

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  spp_code = spp_code_df$code[match(species, spp_code_df$Species)]

  # match up run code with run name
  run_code_df = data.frame(Run = c('All', 'Spring', 'Summer', 'Fall', 'Winter', 'Unknown'),
                           code = c('Null',1:5))

  run_code = run_code_df$code[match(run, run_code_df$Run)]

  # match up rear type code with rear type name
  rear_code_df = data.frame(Rear = c('All','W+H', 'Wild', 'Hatchery', 'Unknown'),
                            code = c('Null', 'WH','W', 'H', 'U'))

  rear_code = rear_code_df$code[match(rear_type, rear_code_df$Rear)]

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/ryankinzer/cuyem')

  # build query for DART
  if(query_type == 'obs_site'){

    # compose url with query
    url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php'

    queryList = list(sc = 1,
                   queryName = 'pitadult_obs_de',
                   stage = NULL,
                   outputFormat = 'csv',
                   year = syear,
                   proj = obs_code,
                   species = spp_code,
                   run = run_code,
                   rear_type = rear_code,
                   span = span,
                   startdate = start_day,
                   enddate = end_day,
                   syear = syear,
                   eyear = eyear,
                   reltype = 'alpha',
                   relloc = NULL,
                   summary = 'no')
  }

  if(query_type == 'release_site'){

    # compose url with query
    url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_rel_de.php'

    queryList = list(sc = 1,
                     #queryName = 'pitadult_obs_de',
                     #stage = NULL,
                     outputFormat = 'csv',
                     year = syear,
                     rel_site = release_site,
                     species = spp_code,
                     run = run_code,
                     rear_type = rear_code,
                     span = span,
                     startdate = start_day,
                     enddate = end_day,
                     #reltype = 'alpha',
                     #relloc = NULL,
                     syear = syear,
                     eyear = eyear,
                     summary = 'no')
  }

  httr::modify_url(url_req, query = queryList)


  # May need to include something like the following if we span years or look for
  # steelhead spawn years.
  # if(grepl('Steelhead', species)) {
  #   queryList[['span']] = 'yes'
  #   queryList[['year']] = spawn_yr -1
  #   queryList = c(queryList,
  #                 list(syear = spawn_yr - 1,
  #                      eyear = spawn_yr))
  # }


  # send query to DART
  web_req = httr::GET(url_req, ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  parsed = httr::content(web_req,
                         'text') %>%
    read_delim(delim = ',',
               col_names = T)

  if(is.null(parsed)) {
    message(paste('DART returned no data for', species, 'in', spawn_yr, '\n'))
    stop
  }

  dat <- parsed[!is.na(parsed$`Tag ID`),] # need to remove empty rows with metadata and bottom header row
  dat <- dat[dat$`Tag ID` != 'Tag ID',]

  return(dat)
}
