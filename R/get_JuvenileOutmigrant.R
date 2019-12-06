# Purpose: Download juvenile abundance data from CDMS and format for CA DES.
# Description: Requires a CDMS login and a DNS connection to internal server to gather CAX metadata.
# Depends: cdmsR, tidyverse, lubridate
# Author: Ryan N. Kinzer
# Created: 11/12/19

library(tidyverse)
library(cdmsR)
library(cuyem)

get_JuvenileOutmigrant <- function(){
  # datastore id = 85 for RST abundance
  df <- getDatasetView(datastoreID = 85, cdms_host = 'https://cdms.nptfisheries.org')

  df2 <- df %>%
    mutate(trapSeason = lubridate::interval(start = StartDate, end = EndDate) %/% days(1))

  all <- df2 %>% filter(Origin == 'Natural' & Lifestage != 'Total')

  ests <- all %>%
    group_by(TRT_POPID, LocationLabel, Species, Run, BroodYear, MigratoryYear) %>%
    summarise(Lifestages = toString(Lifestage),
              StartDate = min(StartDate),
              EndDate = max(EndDate),
              trapSeason = sum(trapSeason),
              Nhat = sum(Abundance, na.rm = TRUE),
              V = sum(StdError^2),
              StdError = sqrt(V),
              CV = StdError/Nhat,
              Lower95 = Nhat - 1.96*StdError,
              Upper95 = Nhat + 1.96*StdError,
              EffDt = max(EffDt))

  return(ests)
}


library(DBI)
# connect using available/authorized DNS
con <- dbConnect(odbc::odbc(),
                 "DFRM-SQL",
                 Database = 'kus',
                 UID = 'guest',
                 PWD = 'guest')

tmp <- dbReadTable(con, 'juvout_meta')
dbListTables(con)

# using windows authentication and local dev. server
# con <- dbConnect(odbc::odbc(),
#                  Driver = "SQL Server",
#                  Server = "localhost\\SQLEXPRESS",
#                  Database = "kus",
#                  Trusted_Connection = "True")

capop <- dbReadTable(con, 'cax_populations')
cades <- dbReadTable(con, 'juvout_meta')
trtpop <- dbReadTable(con, 'ICTRTpops')

ca_meta <- left_join(cades, capop, by = c('CAXPOP_Id' = 'ID')) %>%
  left_join(trtpop %>%
              select(ictrt_id, TRT_POPID, POP_NAME, MPG), by = c('ictrt_fk' = 'ictrt_id'))

ca_data <- left_join(ests, ca_meta, by = 'TRT_POPID') %>%
  mutate(SpeciesRun = paste0(Species,' - ',Run),
         CompilerRecordID = NULL,
         ContactAgency = 'Nez Perce Tribe',
         SubmitAgency = 'Nez Perce Tribe',
         ContactEmail = 'ryank@nezperce.org',
         ContactPersonFirst = 'Ryan',
         ContactPersonLast = 'Kinzer',
         ContactPhone = '208-634-5290 ext:3312',
         DataEntry = Sys.Date(),
         DataEntryNotes = 'queried from NPT CDMS',
         DataStatus = 'Draft',
         TotalNaturalAlpha = '0.05',
         UpdDate = Sys.Date(),
         `Activity Date` = ymd(paste0(MigratoryYear,'12','31')) + 1:n(),
         Location = 'CA DES',
         Publish = 'Yes'
         ) %>%
  select(CBFWApopName = POP_NAME,
            #Comments,
        CommonName = SpeciesRun,
        CommonPopName = POP_NAME,
        #CompilerRecordID,
            ContactAgency,
            ContactEmail,
            ContactPersonFirst,
            ContactPersonLast,
            ContactPhone,
            DataEntry,
            DataEntryNotes,
            DataStatus,
            ESU_DPS,
            IndicatorLocation = LocationLabel,
            LastUpdated = EffDt,
            MajorPopGroup = MPG,
          #  MetaComments,
          #  MethodAdjustments,
            MethodNumber,
          #  NullRecord,
          #  OtherDataSources,
            OutmigrationYear = MigratoryYear,
            PopFit,
            PopFitNotes,
            BestValue,
            PopID = POPID,
            ProtMethDocumentation,
            ProtMethName,
            ProtMethURL,
            Publish,
            RecoveryDomain = RECOVERYDOMAIN,
            #RefID,
            Run = NMFS_RUN,
            SmoltEqLocation,
            SmoltEqLocPTcode,
            SubmitAgency,
            TotalNatural = Nhat,
            TotalNaturalAlpha,
            TotalNaturalLowerLimit = Lower95,
            TotalNaturalUpperLimit = Upper95,
            UpdDate,
            `Activity Date`,
            Location)

write_csv(ca_data, path = '../ca_juv_out.csv')
