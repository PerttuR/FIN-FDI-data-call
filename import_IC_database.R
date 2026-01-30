# Sijoita ja pura .ZIP tiedoston sisältö, jossa IC data Henrikiltä kansioon: path_IC_ORIG
#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Common paths data call folders:

run.year = 2025

# Output folder
path_IC_ORIG <- paste0(getwd(), .Platform$file.sep, "orig/IC_database/", .Platform$file.sep, run.year)
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep, run.year)
path_der <- paste0(getwd(), .Platform$file.sep, "der/", run.year,"/")
path_orig <- paste0(getwd(), .Platform$file.sep, "orig/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)

# import IC_database csv data 2024 and before data
IC_SD <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch SD Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header
IC_SI <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch SI Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header
IC_HI <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch HI Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header

# left join by ID
IC_SD_SI <- left_join(IC_SD, IC_SI, by=c('ImportStratumId'='ImportStratumId'), suffix = c("", ".y"), keep = FALSE)
IC_SD_SI_HI <- left_join(IC_SD_SI, IC_HI, by=c('ImportHeaderId'='ImportHeaderId'), suffix = c("", ".z"), keep = FALSE)

unique(IC_SD_SI_HI$Deleted)
# Delete all rows where deleted = 1
IC_1 <- IC_SD_SI_HI %>% filter (Deleted != 1)

#select columns based on IC_DB names:
IC_2 <- IC_1 %>% select(Country,Year,SeasonType,Season,Fleet,AreaType,Area,DepthRange,Species,Stock,CatCat,RepCat,Sex,CanumType,AgeOrLength,PlusGroup,SampledCatch,NumSamplesLength,NumLengthMeasurements,NumSamplesAge,NumAgeMeasurement,UnitWeca,UnitCanum,UnitAgeOrLength,UnitMeanLength,Maturity,NumberLanded,MeanWeight,MeanLength,varNumLanded,varWgtLanded,varLgtLanded,DataToFrom,Usage,SamplesOrigin,QualityFlag,UnitCaton,CATON,OfficialLandings,UnitEffort,Effort,AreaQualifier)

# vector for column names:
ic_names <- c("Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","UnitEffort","Effort","AreaQualifier")
#rename columns:
colnames(IC_2) <- ic_names

#rename dataframe
IC_DB <- IC_2

#save to der folder
saveRDS(IC_DB, file = paste0(path_der,"IC_DB.rds"))


                                      