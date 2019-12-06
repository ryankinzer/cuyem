#------------------------------------------------------------------------------
# Summarize Carcass Data into the flat file format
#------------------------------------------------------------------------------
# Requires the output of the "carcass_value_added" function; which has assigned
# Origin.A, Recap.A, Target.A and Length_bins to each individual fish.  The
# "carcass_age_flatfile" summarized the different age groups observed in
# carcasses using tags or boney structure.  "Best_age" is assigned based
# on a pre-determined hierarchy; CWT, VIE, PIT, Age Fin, Scale and DNA.
# Observed samples are then summed into the necessary strata matching the
# flatfile format and other outputted data.
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 07/06/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param carcass.dat
#' @param species
#' @param run
#'
#' @return
#' @export
#'
#' @examples
carcass_age_flatfile <- function(carcass.dat, species, run){

  # carcass.dat = data.frame that has been preprocessed and value added

  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  if("tidyr" %in% rownames(installed.packages()))
  {library(tidyr)} else
  {install.packages("tidyr")
    library(tidyr)}

  df <- carcass.dat %>%
    filter(Species == species, Run == run) %>%
    group_by(Species, Run, MPG, Population, Reporting_Group,
             Year, Origin, Marks_AD_Clip, Target, Sex, Length_bin,
             Best_Age.A) %>%
    summarise(n_a = n()) %>%
    ungroup() %>%
    spread(Best_Age.A,n_a,fill=0)

  return(df)

}
