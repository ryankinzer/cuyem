#------------------------------------------------------------------------------
# FINS Trapping Query Data Process Function
#------------------------------------------------------------------------------
# Process data downloaded from FINS website.  Downloaded data should include
# "everything" from the Trapping Query tool.
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
trap_data_processor <- function(data){

  # the function depends on the "dplyr" package
  # it first transforms the Trapped.Date column and builds a Year field
  # it then counts the number of characters in the Length column and
  # determines if Length entered are in "cm" or "mm", then coverts
  # all lengths to "cm" and creates lenght bins into 10cm groups
  # the "Existing.Marks" and "Applied.Marks" fields are then split into
  # ADclip, opercle punch and notch fields.

if("dplyr" %in% rownames(installed.packages()))
  {library(dplyr)} else
{install.packages("dplyr")
  library(dplyr)}

  df <- data %>%
    mutate(Date = as.POSIXct(Trapped.Date,format = "%m/%d/%Y"),
           Year = as.factor(substr(Date,1,4)),
           nchar = nchar(Length,keepNA=TRUE),
           Length_type = ifelse(nchar<=2,"cm",
                                ifelse(nchar>=4,"mm",
                                       ifelse(nchar==3 & as.numeric(Length)>=175,"mm","cm"))),
           Length_cm = ifelse(Length_type=="cm",as.numeric(Length),as.numeric(Length)/10),
           Length_bin = cut(as.numeric(Length_cm),breaks=seq(0,200,by=10)),
           ADclip = ifelse(grepl("AD",Existing.Marks),"Yes","No"),
           E.ROP = ifelse(grepl("ROP",Existing.Marks),"Yes","No"),
           E.LOP = ifelse(grepl("LOP",Existing.Marks),"Yes","No"),
           E.CP = ifelse(grepl("CP",Existing.Marks),"Yes","No"),
           E.RON = ifelse(grepl("RON",Existing.Marks),"Yes","No"),
           E.LON = ifelse(grepl("LON",Existing.Marks),"Yes","No"),
           A.ROP = ifelse(grepl("ROP",Applied.Marks),"Yes","No"),
           A.LOP = ifelse(grepl("LOP",Applied.Marks),"Yes","No"),
           A.CP = ifelse(grepl("CP",Applied.Marks),"Yes","No"),
           A.RON = ifelse(grepl("RON",Applied.Marks),"Yes","No"),
           A.LON = ifelse(grepl("LON",Applied.Marks),"Yes","No"))
  return(df)
}
