#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE B
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A fron stat DEP (Pirkko)
#-------------------------------------------------------------------------------


#install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(RPostgreSQL)


#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_salmon <- "C:/2018/FDI/work/data/orig/" # folder where the salmon data is (stecf.csv)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu :
path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
path_salmon <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where the salmon data is (stecf.csv)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results" # folder where the output is saved


#-------------------------------------------------------------------------------
#                       1. add metiers from db                       
#-------------------------------------------------------------------------------

setwd(path_rproject)

source("db.R")

metiers <- read.dbTable("suomu","metier")

#-------------------------------------------------------------------------------
# choose upper (WP&AR) hierarcy gears/metiers only (DEL national sub metiers)

wp_gears <- c("OTM", "PTM", "FYK", "GNS")
metiers <- filter(metiers, gear_code %in% wp_gears) 


years <- c(2015, 2016, 2017)

table_b <- expand.grid(years,metiers$level6)


table_b <- table_b %>% rename(YEAR = Var1, SAMPLE_FRAME = Var2)
table_b <- table_b[order(table_b$YEAR),]

COUNTRY <- "FIN"
REFUSAL_RATE <- "NK"

table_b$COUNTRY <- COUNTRY
table_b$REFUSAL_RATE <- REFUSAL_RATE

table_b <- table_b %>% select(COUNTRY, YEAR, SAMPLE_FRAME, REFUSAL_RATE)

# set working directory to save table B and table
setwd(path_out)
write.csv(table_b, "FIN_TABLE_B_REFUSAL_RATE.csv", row.names = F)

