#------------------------------------------------------------------------------
# Summarize Carcass Data
#------------------------------------------------------------------------------
# Summarizes Carcass data which has already been processed by
# "sgs_data_processor" and "carcass_value_added" functions to include transect information.
# The "markrecap_flatfile" function sums all captures and recpatures upstream
# of a weir for use in the mark/recapture tab of the performance measures
# flatfile.  Marks and recapture data can be pulled from carcasses (SGS dbase)
# for Chinook weirs or from FINS for Steelhead.  The function aggregates data
# by MPG, Population, Reporting_Group, Stream, and Year.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/30/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param carcass.data
#' @param species
#' @param run
#'
#' @return
#' @export
#'
#' @examples
markrecap_flatfile <- function(carcass.data,species="Chinook",run="Spring/Summer"){

  # carcass.data = processed SGS carcass data; individual records

  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  if("tidyr" %in% rownames(installed.packages()))
  {library(tidyr)} else
  {install.packages("tidyr")
    library(tidyr)}

  df <- carcass.data %>%
          filter(Species == species, Run == run, Above_Weir == "Yes") %>%
          group_by(Species, Run, MPG, Population, Reporting_Group,
                    Year, Origin, Marks_AD_Clip, Target, Sex, Length_bin,
                    Recap.A) %>%
          summarise(n_recap = n()) %>%
          ungroup() %>%
          spread(Recap.A,n_recap,fill=0) %>%
          rename(Unmark = No, Recap = Yes) %>%
          mutate(Captures = Unmark + Recap)

  return(df)

}
