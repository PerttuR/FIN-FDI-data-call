#-------------------------------------------------------------------------------
#
# Script to process raw fisheries data to lengthclass data
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: Project särkikala
#-------------------------------------------------------------------------------


#install.packages("RPostgreSQL")
#install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(tidyverse)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------

# Perttu:
path_data <- "C:/perttu/särkikala/LEENA_PUURA/2013" # folder where raw data is
path_out <- "C:/perttu/särkikala/LEENA_PUURA/2013/out" # folder for results

setwd(path_data)

# import raw
raw_data <- read.csv2("2013_raw.csv", sep = ";" )

#lenths to mm
raw_data$Pituus.cm <- gsub(",",".",raw_data$Pituus)
raw_data<- raw_data %>% mutate(Pituus.mm=as.numeric(Pituus.cm)*10) 

#set 0 length to NA
raw_data$Pituus.mm<- raw_data$Pituus.mm %>% na_if(0) 

#Check min and max
min(raw_data$Pituus.mm, na.rm = TRUE)
max(raw_data$Pituus.mm, na.rm = TRUE)

# aggregate data into 1 cm length classes based on min & max ranges
raw_data$pituusluokka[raw_data$Pituus.mm >= 100 & raw_data$Pituus.mm < 110] <- 100
raw_data$pituusluokka[raw_data$Pituus.mm >= 110 & raw_data$Pituus.mm < 120] <- 110
raw_data$pituusluokka[raw_data$Pituus.mm >= 120 & raw_data$Pituus.mm < 130] <- 120
raw_data$pituusluokka[raw_data$Pituus.mm >= 130 & raw_data$Pituus.mm < 140] <- 130
raw_data$pituusluokka[raw_data$Pituus.mm >= 140 & raw_data$Pituus.mm < 150] <- 140
raw_data$pituusluokka[raw_data$Pituus.mm >= 150 & raw_data$Pituus.mm < 160] <- 150
raw_data$pituusluokka[raw_data$Pituus.mm >= 160 & raw_data$Pituus.mm < 170] <- 160
raw_data$pituusluokka[raw_data$Pituus.mm >= 170 & raw_data$Pituus.mm < 180] <- 170
raw_data$pituusluokka[raw_data$Pituus.mm >= 180 & raw_data$Pituus.mm < 190] <- 180
raw_data$pituusluokka[raw_data$Pituus.mm >= 190 & raw_data$Pituus.mm < 200] <- 190
raw_data$pituusluokka[raw_data$Pituus.mm >= 200 & raw_data$Pituus.mm < 210] <- 200
raw_data$pituusluokka[raw_data$Pituus.mm >= 210 & raw_data$Pituus.mm < 220] <- 210
raw_data$pituusluokka[raw_data$Pituus.mm >= 220 & raw_data$Pituus.mm < 230] <- 220
raw_data$pituusluokka[raw_data$Pituus.mm >= 230 & raw_data$Pituus.mm < 240] <- 230
raw_data$pituusluokka[raw_data$Pituus.mm >= 240 & raw_data$Pituus.mm < 250] <- 240
raw_data$pituusluokka[raw_data$Pituus.mm >= 250 & raw_data$Pituus.mm < 260] <- 250
raw_data$pituusluokka[raw_data$Pituus.mm >= 260 & raw_data$Pituus.mm < 270] <- 260
raw_data$pituusluokka[raw_data$Pituus.mm >= 270 & raw_data$Pituus.mm < 280] <- 270
raw_data$pituusluokka[raw_data$Pituus.mm >= 280 & raw_data$Pituus.mm < 290] <- 280
raw_data$pituusluokka[raw_data$Pituus.mm >= 290 & raw_data$Pituus.mm < 300] <- 290
raw_data$pituusluokka[raw_data$Pituus.mm >= 300 & raw_data$Pituus.mm < 310] <- 300
raw_data$pituusluokka[raw_data$Pituus.mm >= 310 & raw_data$Pituus.mm < 320] <- 310
raw_data$pituusluokka[raw_data$Pituus.mm >= 320 & raw_data$Pituus.mm < 330] <- 320
raw_data$pituusluokka[raw_data$Pituus.mm >= 330 & raw_data$Pituus.mm < 340] <- 330
raw_data$pituusluokka[raw_data$Pituus.mm >= 340 & raw_data$Pituus.mm < 350] <- 340
raw_data$pituusluokka[raw_data$Pituus.mm >= 350 & raw_data$Pituus.mm < 360] <- 350
raw_data$pituusluokka[raw_data$Pituus.mm >= 360 & raw_data$Pituus.mm < 370] <- 360
raw_data$pituusluokka[raw_data$Pituus.mm >= 370 & raw_data$Pituus.mm < 380] <- 370
raw_data$pituusluokka[raw_data$Pituus.mm >= 380 & raw_data$Pituus.mm < 390] <- 380
raw_data$pituusluokka[raw_data$Pituus.mm >= 390 & raw_data$Pituus.mm < 400] <- 390
raw_data$pituusluokka[raw_data$Pituus.mm >= 400 & raw_data$Pituus.mm < 410] <- 400
raw_data$pituusluokka[raw_data$Pituus.mm >= 410 & raw_data$Pituus.mm < 420] <- 410
raw_data$pituusluokka[raw_data$Pituus.mm >= 420 & raw_data$Pituus.mm < 430] <- 420
raw_data$pituusluokka[raw_data$Pituus.mm >= 430 & raw_data$Pituus.mm < 440] <- 430
raw_data$pituusluokka[raw_data$Pituus.mm >= 440 & raw_data$Pituus.mm < 450] <- 440
raw_data$pituusluokka[raw_data$Pituus.mm >= 450 & raw_data$Pituus.mm < 460] <- 450
raw_data$pituusluokka[raw_data$Pituus.mm >= 460 & raw_data$Pituus.mm < 470] <- 460
raw_data$pituusluokka[raw_data$Pituus.mm >= 470 & raw_data$Pituus.mm < 480] <- 470
raw_data$pituusluokka[raw_data$Pituus.mm >= 480 & raw_data$Pituus.mm < 490] <- 480
raw_data$pituusluokka[raw_data$Pituus.mm >= 490 & raw_data$Pituus.mm < 500] <- 490
raw_data$pituusluokka[raw_data$Pituus.mm >= 500 & raw_data$Pituus.mm < 510] <- 500
raw_data$pituusluokka[raw_data$Pituus.mm >= 510 & raw_data$Pituus.mm < 520] <- 510
raw_data$pituusluokka[raw_data$Pituus.mm >= 520 & raw_data$Pituus.mm < 530] <- 520
raw_data$pituusluokka[raw_data$Pituus.mm >= 530 & raw_data$Pituus.mm < 540] <- 530
raw_data$pituusluokka[raw_data$Pituus.mm >= 540 & raw_data$Pituus.mm < 550] <- 540
raw_data$pituusluokka[raw_data$Pituus.mm >= 550 & raw_data$Pituus.mm < 560] <- 550
raw_data$pituusluokka[raw_data$Pituus.mm >= 560 & raw_data$Pituus.mm < 570] <- 560
raw_data$pituusluokka[raw_data$Pituus.mm >= 570 & raw_data$Pituus.mm < 580] <- 570
raw_data$pituusluokka[raw_data$Pituus.mm >= 580 & raw_data$Pituus.mm < 590] <- 580
raw_data$pituusluokka[raw_data$Pituus.mm >= 590 & raw_data$Pituus.mm < 600] <- 590

#Check min and max
min(raw_data$pituusluokka, na.rm = TRUE)
max(raw_data$pituusluokka, na.rm = TRUE)

#delete rows with NAs in pituusluokka
nrow(raw_data)
raw_data <- raw_data %>% drop_na(pituusluokka)
nrow(raw_data)

# aggregate data to the same level as landings data (by length CLASS)
lc_data <- raw_data %>% group_by(Näytenumero, Laji, pituusluokka) %>% summarise(pituusluokan_kpl_maara = n(), aliotoksenpaino = sum(Paino.g))

lc_data <- lc_data %>% arrange(Näytenumero, Laji, pituusluokka)
lc_data$mittausvali <- 10
lc_data <- lc_data %>% select(Näytenumero, Laji, pituusluokka, mittausvali, pituusluokan_kpl_maara, aliotoksenpaino)
# select species

species <- lc_data %>% group_by(Näytenumero, Laji) %>% summarise(otoksen_paino = sum(aliotoksenpaino))
species$painokokosaaliista <- ""
species$Saalisluokka <-"LANDING"
species$Kayttotarkoitus <- "Ruokakalaa"
species$otoksen_paino <- species$otoksen_paino/1000
species <- species %>% select(Näytenumero, Saalisluokka, Laji, Kayttotarkoitus, painokokosaaliista, otoksen_paino)


# set working directory to save data
setwd(path_out)
write.csv(lc_data, "2013_särkikalaprojektin_pituusluokat.csv", row.names = F)
write.csv(species, "2013_särkikalaprojektin_lajijakauma.csv", row.names = F)
