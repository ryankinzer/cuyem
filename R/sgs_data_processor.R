#------------------------------------------------------------------------------
# Redd Data Processor
#------------------------------------------------------------------------------
# Function to merge SGS data (redd or carcass tables) with transect table.  It
# first splits survey records with two transects in the "Transect" field into
# separate fields.  The first is call "Transect" and the second is called
# "Transect.Other".  It then joins SGS data on unique "Stream" and "Transect"
# records.  Next the "DateSurveyed" field is formatted as a date and a "Year"
# field is created.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# 6/30/2016
#------------------------------------------------------------------------------


#' Title
#'
#' @param sgs.data
#' @param transect.tbl
#'
#' @return
#' @export
#'
#' @examples
sgs_data_processor <- function(sgs.data, transect.tbl){
  # sgs.data = data from NPT SGS flat file, will change to .admb source
  # transect.tbl = the final NPT transect table, , will change to .admb source
  # keep = vector of transect.tbl field names to join with sgs.data

  if("dplyr" %in% rownames(installed.packages())) # requires or installs dplyr
    {library(dplyr)} else
      {install.packages("dplyr")
        library(dplyr)}

  if("tidyr" %in% rownames(installed.packages())) # requires or installs dplyr
  {library(tidyr)} else
  {install.packages("tidyr")
    library(tidyr)}

# Strip first entry in Transect Field into a unique field, and combines others
  section <- c("",".Other")
  field_name <- paste0("Transect",section)

  sgs.df <- sgs.data %>%
    separate(Transect,field_name,",",extra="merge")

# Combine Transect table info with Redd detail data

  sgs.df <- transect_tbl %>%
    right_join(sgs.df) %>%
    mutate(DateSurveyed = as.Date(DateSurveyed,origin="1899-12-30"))

  return(sgs.df)

}
