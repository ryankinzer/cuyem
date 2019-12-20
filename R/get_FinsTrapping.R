#' @title Get Fins Trapping Data
#'
#' @description Query \url{https://www.finsnet.org/} trapping
#'   module data. Currently the function requires an odbc connection from behind
#'   the NPT firewall to download trapping data that has been copied from
#'   \url{https://www.finsnet.org/}. Data is updated monthly or as needed.  In
#'   the future the function will retrieve data from a
#'   \url{https://www.finsnet.org/} API.
#'
#' @param facility a vector of valid \url{https://www.finsnet.org/} hatchery
#'   facilities
#' @param odbc_connection an open odbc channel to CDMS database
#'
#' @return a data.frame containing raw \url{https://www.finsnet.org/} trapping module information
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' con <- RODBC::odbcConnect(dsn = 'Fins', uid = 'guest', pwd = 'guest')
#' qry <- "SELECT distinct(Facility) FROM FINS_all_trapping"
#' f <- RODBC::sqlQuery(con, qry) %>% mutate_all(as.character) %>% pull(Facility)
#' npt_f <- f[grepl('NPT', f)]
#' get_FinsTrapping(npt_f, odbc_connection = con)

get_FinsTrapping <- function(facility, odbc_connection){

  x <- facility
  con <- odbc_connection

  if(is.null(facility)){stop("A Fins facility must be specified.")}

  if(class(con) != 'RODBC'){
    stop('An \"odbc\" connection is not identified. A connection with the
    back-end CDMS database is needed for this function to work. Please
    provide a valid connection using package \"RODBC\".')
  }

  all_trap <- as.list(x) %>%
    rlang::set_names() %>%
    purrr::map_df(
           .f = function(x){
             qry <- paste0("SELECT * FROM FINS_all_trapping WHERE Facility = '", x[1],"'")

             # if(!is.null(species)){
             #   qry <- paste0(qry," AND Species = '", species, "'")
             # }

             # if(!is.null(year)){
             #   qry <- paste0(qry," AND year(Trapped Date) IN '", year, "'")
             # }

             RODBC::sqlQuery(con, qry) %>%
               mutate_all(as.character)
           })

  names(all_trap) <- tolower(gsub(' ','_',names(all_trap)))

  return(all_trap)
}
