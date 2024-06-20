#- Clear workspace
rm(list=ls())

# needed libraries .. testi
library(dplyr)
library(openxlsx)
library(icesVocab)
library(openxlsx)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------


# Paths & 2024 folder:
path_IC <- paste0(getwd(), .Platform$file.sep, "orig/IC/")
path_rproject <- getwd() # folder where the r project is (and the source file db.R!)
# folder where the output is saved
path_out <- paste0(getwd(), .Platform$file.sep,"results", .Platform$file.sep,"2024")

# import table A
IC_HER_30_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 30 2023.csv"), sep = "," , na.strings = "", header = FALSE)
IC_HER_31_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 31 2023.csv"), sep = "," , na.strings = "", header = FALSE)
IC_HER_32_south_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN HER 25-27 28.2 29 and 32 2023.csv"), sep = "," , na.strings = "", header = FALSE)
IC_SPR_22_32_2023 <- read.csv2(paste0(path_IC,.Platform$file.sep,"FIN SPR 22-32 2023.csv"), sep = "," , na.strings = "", header = FALSE)

# combine by row
IC_2023a <- rbind(IC_HER_30_2023,IC_HER_31_2023)
IC_2023b <- rbind(IC_HER_32_south_2023,IC_SPR_22_32_2023)

IC_2023a2 <- IC_2023a %>% filter(V1 == "SD")
IC_2023b2 <- IC_2023b %>% filter(V1 == "SD")
