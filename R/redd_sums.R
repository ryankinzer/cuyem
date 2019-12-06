#------------------------------------------------------------------------------
# Summarize Redd Data
#------------------------------------------------------------------------------
# Summarizes Redd data which has already been processed by
# "sgs_data_processor" function and include transect information.
# The "redd_sums" function sums across MPG, Population, Reporting_Group,
# Stream, Year, Transect_Type, and above or below weirs, PIT-array and RST.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/30/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param redd.data
#' @param species
#' @param run
#' @param probabilistic
#'
#' @return
#' @export
#'
#' @examples
redd_sums <- function(redd.data,species="Chinook",run="Spring/Summer",
                      probabilistic=FALSE){
  # redd.data = processed redd survey data (includes transect table info.)
  # species = specifiy species of interest; default = "Chinook"
  # run = specify run of interest; default = "Spring/Summer"

  if("dplyr" %in% rownames(installed.packages()))
    {library(dplyr)} else
      {install.packages("dplyr")
        library(dplyr)}

  if(probabilistic==FALSE){
      redd.df <- redd.data %>%
                  filter(Species == species, Run == run,
                  Transect_Type != "Probabilistic") %>%
                  group_by(Species, Run, MPG, Population, Reporting_Group,
                           Stream, Year, Transect_Type, Above_Weir,
                           Above_PITArray, Above_RST) %>%
                  summarise(n_redds = sum(NewRedds,na.rm=TRUE))
      } else

    {redd.df <- redd.data %>%
                  filter(Species == species, Run == run) %>%
                  group_by(Species, Run, MPG, Population, Reporting_Group,
                            Stream, Year, Transect_Type, Above_Weir,
                          Above_PITArray, Above_RST) %>%
                  summarise(n_redds = sum(NewRedds,na.rm=TRUE))
    }

  return(redd.df)
}
