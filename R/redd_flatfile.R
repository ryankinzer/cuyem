#------------------------------------------------------------------------------
# Summarize Redd Data into the flat file format
#------------------------------------------------------------------------------
# Summarizes Redd data which has already been processed by
# "sgs_data_processor" function to include transect information and the
# "redd_sums" function which sums across MPG, Population, Reporting_Group,
# Stream, Year, Transect_Type, and above or below weirs, PIT-array and RST.
# "redd_flatfile" sums total count, index, and above weir, PIT-array and RST
# and puts data into the wide format.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 06/30/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param redd.dat
#'
#' @return
#' @export
#'
#' @examples
redd_flatfile <- function(redd.dat){


  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  redd_total <- redd.dat %>%
    group_by(Species, Run, MPG, Population,
             Reporting_Group, Year) %>%
    summarise(R_T = sum(n_redds,na.rm=TRUE))

  redd_index <- redd.dat %>%
    filter(Transect_Type == "Index") %>%
    group_by(Species, Run, MPG, Population,
             Reporting_Group, Year) %>%
    summarise(R_I = sum(n_redds,na.rm=TRUE))

  redd_weir <- redd.dat %>%
    filter(Above_Weir == "Yes") %>%
    group_by(Species, Run, MPG, Population,
             Reporting_Group, Year) %>%
    summarise(R_W = sum(n_redds,na.rm=TRUE))

  redd_RST <- redd.dat %>%
    filter(Above_RST == "Yes") %>%
    group_by(Species, Run, MPG, Population,
             Reporting_Group, Year) %>%
    summarise(R_J = sum(n_redds,na.rm=TRUE))

  redd_PIT <- redd.dat %>%
    filter(Above_PITArray == "Yes") %>%
    group_by(Species, Run, MPG, Population,
             Reporting_Group, Year) %>%
    summarise(R_P = sum(n_redds,na.rm=TRUE))

  tbl_redd <- full_join(redd_total,redd_index) %>%
    full_join(redd_weir) %>%
    full_join(redd_RST) %>%
    full_join(redd_PIT)

  return(tbl_redd)

}
