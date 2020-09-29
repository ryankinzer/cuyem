#' @title Download FINS Trapping Data
#'
#' @param apikey user defined API key provided by FINS development team
#' @param startDate query start date
#' @param endDate query end data
#'
#' @return
#' @import httr
#'
dnload_WeirData <- function(apikey, startDate, endDate){
  # assign user agent to the GitHub repo for this package
  #ua = httr::user_agent('https://github.com/ryankinzer/cuyem')
  ua = httr::user_agent('Nez Perce Tribe')

  # compose url with query
  url_req = 'https://www.finsnet.org/fins/ncg/Trapping/'

  # build query for DART
  queryList = list(apikey = 'cGsvN7QlsSj0Uiy2kOLECQ8UsNhdt2bj',#apikey,
                   startDate = '01/01/1990',#
                   endDate = '01/01/2020')#

  # send query to DART
  httr::modify_url(url_req, query = queryList)
  web_req = httr::GET(url_req, ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from FINS')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  parsed = httr::content(web_req,
                         'text')

  parsed = httr::content(web_req,
                         'parsed',
                         type = 'text/csv',
                         encoding = 'utf-8')

  #%>%
  df <- readr::read_delim(parsed, delim = ',',
               col_names = T)

  if(is.null(df)) {
    message(paste('FINS returned no data'))
    stop
  }

  # if(class(parsed)[1] == 'xml_document') {
  #   message(paste('XML document returned by FINS instead of data\n'))
  #   stop
  # }

  if (httr::status_code(web_req) != 200) {
    message(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
    stop
  }

  return(df)
}

