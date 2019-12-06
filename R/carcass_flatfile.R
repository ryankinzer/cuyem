#------------------------------------------------------------------------------
# Summarize Carcass Data into the flat file format
#------------------------------------------------------------------------------
# Requires the output of the "carcass_sums" function; which has already grouped
# and summed the fish into the necessary strata.  The "carcass_flatfile"
# function re-organizes the summary data to fit the flat file format
#------------------------------------------------------------------------------
# Ryan N. Kinzer
# Created 07/05/2016
#------------------------------------------------------------------------------

#' Title
#'
#' @param carcass.sum
#'
#' @return
#' @export
#'
#' @examples
carcass_flatfile <- function(carcass.sum){

  # carcass.sum = data.frame that has be preprocessed and summarized

  if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
  {install.packages("dplyr")
    library(dplyr)}

  if("tidyr" %in% rownames(installed.packages()))
  {library(tidyr)} else
  {install.packages("tidyr")
    library(tidyr)}

  carcass_origin <- carcass.sum %>%
                    filter(Run == "Spring/Summer",Above_Weir != "NA") %>% #Origin.A != "Unknown"
                    group_by(Species, Run, MPG, Population, Reporting_Group,
                            Year, Origin, Marks_AD_Clip, Target, Length_bin,
                            Above_Weir) %>%
                    summarise(n_O = sum(n_carcasses,na.rm=TRUE)) %>%
                    ungroup() %>%
                    spread(Above_Weir,n_O,fill=0) %>%
                    rename(n_U = Yes, n_D = No)

  carcass_female <- carcass.sum %>%
                    filter(Run == "Spring/Summer",Above_Weir != "NA", Sex == "Female") %>%
                    group_by(Species, Run, MPG, Population, Reporting_Group,
                    Year, Origin, Marks_AD_Clip, Target, Length_bin,
                    Above_Weir) %>%
                    summarise(n_f = sum(n_carcasses,na.rm=TRUE))%>%
                    ungroup() %>%
                    spread(Above_Weir,n_f,fill=0) %>%
                    rename(n_U_f = Yes, n_D_f = No)

  carcass_sex <- carcass.sum %>%
                  filter(Run == "Spring/Summer",Above_Weir != "NA", Sex != "Unknown") %>%
                  group_by(Species, Run, MPG, Population, Reporting_Group,
                            Year, Origin, Marks_AD_Clip, Target, Length_bin,
                            Above_Weir) %>%
                  summarise(n_s = sum(n_carcasses,na.rm=TRUE))%>%
                  ungroup() %>%
                  spread(Above_Weir,n_s,fill=0) %>%
                  rename(n_U_s = Yes, n_D_s = No)


  carcass_f_prespawn <- carcass.sum %>%
                  filter(Run == "Spring/Summer", Above_Weir != "NA",
                         Sex == "Female", Spawned == "No") %>%
                  group_by(Species, Run, MPG, Population, Reporting_Group,
                            Year, Origin, Marks_AD_Clip, Target, Length_bin,
                            Above_Weir) %>%
                  summarise(n_f_p = sum(n_carcasses,na.rm=TRUE))%>%
                  ungroup() %>%
                  spread(Above_Weir,n_f_p,fill=0) %>%
                  rename(n_U_p = Yes, n_D_p = No)

  carcass_prespawn <- carcass.sum %>%
                  filter(Run == "Spring/Summer", Above_Weir != "NA",
                          Sex == "Female", Spawned != "Unknown") %>%
                  group_by(Species, Run, MPG, Population, Reporting_Group,
                        Year, Origin, Marks_AD_Clip, Target, Length_bin,
                        Above_Weir) %>%
                  summarise(n_f_p = sum(n_carcasses,na.rm=TRUE))%>%
                  ungroup() %>%
                  spread(Above_Weir,n_f_p,fill=0) %>%
                  rename(n_U_f_p = Yes, n_D_f_p = No)

  df <- full_join(carcass_origin,carcass_female) %>%
          full_join(carcass_sex) %>%
            full_join(carcass_f_prespawn) %>%
              full_join(carcass_prespawn)

  return(df)

}
