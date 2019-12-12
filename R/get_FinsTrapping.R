
#
# con <- RODBC::odbcConnect(dsn = 'fins',
#                           uid = 'guest',
#                           pwd = 'guest')
#
# qry <- "SELECT distinct(Facility) FROM FINS_all_trapping"
#
# f <- RODBC::sqlQuery(con, qry) %>%
#   mutate_all(as.character)
#
# f <- f %>% pull(Facility)
#
# npt_f <- f[grepl('NPT', f)]



#' @title Summarize Fins Trapping Data
#'
#' @description Query and summarize \url{https://www.finsnet.org/} trapping
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
#' @return a data.frame containing raw or summarized
#'   \url{https://www.finsnet.org/} trapping module information
#'
#' @export
#'
#' @examples
#' con <- RODBC::odbcConnect(dsn = 'data source',
#'  uid = 'your_username', pwd = 'your_password')
#' get_FinsTrapping('NPT Hatchery', odbc_connection = con)

get_FinsTrapping <- function(facility, odbc_connection){

  f <- facility
  con <- odbc_connection

  if(is.null(facility)){stop("A Fins facility must be specified.")}

  if(class(con) != 'RODBC'){
    stop('An \"odbc\" connection is not identified. A connection with the
    back-end CDMS database is needed for this function to work. Please
    provide a valid connection using package \"RODBC\".')
  }

  all_trap <- as.list(f) %>%
    rlang::set_names() %>%
    map_df(.id = 'Facility',
           .f = function(x){
             qry <- paste0("SELECT * FROM FINS_all_trapping WHERE Facility = '", x[1],"'")

             RODBC::sqlQuery(con, qry) %>%
               mutate_all(as.character)
           })

  return(all_trap)
  }
