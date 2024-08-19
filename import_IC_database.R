# Sijoita ja pura .ZIP tiedoston sisältö, mikä saatu Nicolakselta path_IC kansioon

#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Paths & 2024 folder:
path_IC_ORIG <- paste0(getwd(), .Platform$file.sep, "orig/IC_database/")
path_der <- paste0(getwd(), .Platform$file.sep, "der/2024/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")

dir.create(path_out, showWarnings = FALSE)
dir.create(path_der, showWarnings = FALSE, recursive = TRUE)



sd_header <- c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")

# import IC_database csv data 2023 and before data
IC_SD <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch SD Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header
IC_SI <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch SI Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header
IC_HI <- read.csv2(paste0(path_IC_ORIG,.Platform$file.sep,"InterCatch HI Finland all years.csv"), sep = "," , na.strings = "", header = TRUE) #, col.names=sd_header

IC_SD_SI <- left_join(IC_SD, IC_SI, by=c('ImportStratumId'='ImportStratumId'), suffix = c("", ".y"), keep = FALSE)
IC_SD_SI_HI <- left_join(IC_SD_SI, IC_HI, by=c('ImportHeaderId'='ImportHeaderId'), suffix = c("", ".z"), keep = FALSE)

unique(IC_SD_SI_HI$Deleted)
IC_1 <- IC_SD_SI_HI %>% filter (Deleted != 1)
 
IC_2 <- IC_1 %>% select(Country,Year,Season,Fleet,Area,Species,AgeOrLength,SampledCatch, CATON, UnitCaton)
IC_2 <- IC_1 %>% select(Country,Year,SeasonType,Season,Fleet,AreaType,Area,DepthRange,Species,Stock,CatCat,RepCat,Sex,CanumType,AgeOrLength,PlusGroup,SampledCatch,NumSamplesLength,NumLengthMeasurements,NumSamplesAge,NumAgeMeasurement,UnitWeca,UnitCanum,UnitAgeOrLength,UnitMeanLength,Maturity,NumberLanded,MeanWeight,MeanLength,varNumLanded,varWgtLanded,varLgtLanded,DataToFrom,Usage,SamplesOrigin,QualityFlag,UnitCaton,CATON,OfficialLandings,UnitEffort,Effort,AreaQualifier)

ic_names <- c("Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","UnitEffort","Effort","AreaQualifier")
ic_names2 <- c("Country","Year","Season","Fleet","FishingArea","Species","AgeLength","SampledCatch","CATON","UnitCATON")
colnames(IC_2) <- ic_names

IC_DB <- IC_2

saveRDS(IC_DB, file = paste0(path_der,"IC_DB.rds"))


                                      