#------------------------------------------------------------------------------
# Summarize Carcass Data
#------------------------------------------------------------------------------
# Summarizes Carcass data which has already been processed by
# "sgs_data_processor" function and include transect information.
# The "carcass_value_added" function adds three additional fields to the raw
# individual fish carcass data; "Origin", "Target", "Recap" and "Length_bin".
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 07/05/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param carcass.data
#' @param bin_length
#' @param recap_mark
#' @param target_mark
#'
#' @return
#' @export
#'
#' @examples
carcass_value_added <- function(carcass.data, bin_length, recap_mark, target_mark){

  # carcass.data = processed SGS carcass data; individual records

  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  carcass.df <- carcass.data %>%
    mutate(Length_bin = cut(as.numeric(FL),breaks=seq(0,200,by=bin_length)),
           Origin.A = ifelse(Marks_AD_Clip == "Yes","Hatchery",
                              ifelse(Tags_CWT == "Yes","Hatchery",
                                      ifelse(grepl("e",Tags_VIE), "Hatchery",
                                            ifelse(Marks_AD_Clip == "Unknown", "Unknown",
                                                   ifelse(Tags_CWT == "Unknown","Unknown","Natural"))))),
           #Target.A =
           Recap.A = ifelse(grepl(recap_mark,Marks_Opercle),"Yes",
                          ifelse(Tags_PetersonDisc == "Unknown","Unknown",
                                 ifelse(Tags_PetersonDisc == "No","No","Yes"))),
           Best_Age.A = ifelse(!is.null(CWT_Age), CWT_Age,
                                      ifelse(!is.null(PIT_Age), PIT_Age,
                                             ifelse(!is.null(VIE_Age), VIE_Age,
                                                    ifelse(!is.null(DNA_Age), DNA_Age,
                                                           ifelse(!is.null(AgeFin_Age), AgeFin_Age,
                                                                  ifelse(!is.null(Scale_Age), Scale_Age,"NA")))))))

  return(carcass.df)

}
