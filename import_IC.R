# Sijoita ja pura .ZIP tiedoston sisältö, mikä saatu Nicolakselta path_IC kansioon

#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Paths & 2024 folder:
path_IC <- paste0(getwd(), .Platform$file.sep, "orig/IC/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")


sd_header <- c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")
# import IC csv data from 2023
IC_HER_30_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 30 2023.csv"), sep = "," , na.strings = "", header = FALSE, col.names=sd_header)
IC_HER_31_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 31 2023.csv"), sep = "," , na.strings = "", header = FALSE, col.names=sd_header)
IC_HER_32_south_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 25-27 28.2 29 and 32 2023.csv"), sep = "," , na.strings = "", header = FALSE, col.names=sd_header)
IC_SPR_22_32_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN SPR 22-32 2023.csv"), sep = "," , na.strings = "", header = FALSE, col.names=sd_header)

# combine rows
IC_2023 <- rbind(IC_HER_30_2023,IC_HER_31_2023,IC_HER_32_south_2023,IC_SPR_22_32_2023)

#Filter by SD result rows and filter DeptRange and Stock out:
IC_2023_SD <- IC_2023 %>% filter(RecordType == "SD") %>% select(-RecordType)

#Filter by SI result rows:
IC_2023_SI <- IC_2023 %>% filter(RecordType == "SI")
si_header <- c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral")
colnames(IC_2023_SI) <- si_header
IC_2023_SI <- IC_2023_SI[,1:length(si_header)]
IC_2023_SI <- IC_2023_SI %>% select(-RecordType)



#Filter by HI result rows:
IC_2023_HI <- IC_2023 %>% filter(RecordType == "HI")
hi_header <- c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier")
colnames(IC_2023_HI) <- hi_header 
IC_2023_HI <- IC_2023_HI[,1:length(hi_header)]
IC_2023_HI <- IC_2023_HI %>% select(-RecordType)

#Left join unitCATOn and CATON values from SI record to SD record
IC_2023_SD_SI <- left_join(IC_2023_SD, IC_2023_SI)


                                      