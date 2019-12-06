#------------------------------------------------------------------------------
# Summarize Carcass Data
#------------------------------------------------------------------------------
# Summarizes Carcass data which has already been processed by
# "sgs_data_processor" and "carcass_value_added" functions and include transect information.
# The "carcass_sums" function sums across MPG, Population, Reporting_Group,
# Stream, Year, Transect_Type, and above or below weirs, PIT-array and RST.
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
carcass_sums <- function(carcass.data,species="Chinook",run="Spring/Summer"){

  # carcass.data = processed SGS carcass data; individual records

  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  carcass.df <- carcass.data %>%
                filter(Species == species, Run == run) %>%
                group_by(Species, Run, MPG, Population, Reporting_Group,
                          Stream, Year, Origin, Marks_AD_Clip, Target,
                          Recap.A, Length_bin,
                          Above_Weir, Sex, Spawned) %>%
        summarise(n_carcasses = n())

    return(carcass.df)

}
