#------------------------------------------------------------------------------
# Summarize FINS Trapping Data
#------------------------------------------------------------------------------
# Summarizes FINS trapping data which has already been processed by
# "trap_data_processor" function.  The "trap_sums" function sums across
# individual fish data by unique/indentifiable groups; origin, sex, dispostion,
# facility, year, size, etc.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/30/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
trap_sums <- function(data){

  if("dplyr" %in% rownames(installed.packages()))
    {library(dplyr)} else
      {install.packages("dplyr")
        library(dplyr)}


  df <- data %>%
    group_by(Facility,
             Trap,
             Species,
             Run,
             Year,
             Living.Status,
             Origin,
             ADclip,
             Sex,
             Length_bin,
             Disposition,
             Purpose,
             Moved.To,
             Recap,
             Existing.CWT, # need to use, and later correct "Existing CWTValue" field records to include code
             #Existing.PIT,
             A.ROP,
             A.LOP)%>%
    summarise(n = sum(Count,na.rm=TRUE))

  return(df)
}

