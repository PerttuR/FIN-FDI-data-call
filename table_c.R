#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE C
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)

# set working directory to read files from
#perttu´s addresses:
# setwd("~/R/FIN-FDI-data-call")
setwd("C:/perttu/eu-tike/STECF/FIN-FDI-data-call")
#mira´s wd:
#setwd("C:/2018/FDI/work/data/orig/")


#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------

# import table A
table_A <- read.csv2("FIN_TABLE_A_CATCH.csv", sep = "," )
#-------------------------------------------------------------------------------

# sum totwghtlandg and unwanted_catch BY year, domain_discards and species from TABLE A
table_A_sum <- table_A %>% group_by(country, year, domain_discards, species) %>% summarise(totwghtlandg = sum(as.numeric(as.character(totwghtlandg))), unwanted_catch = sum(as.numeric(as.character(unwanted_catch))))

# rounding the number to three digits precision
table_A_sum$totwghtlandg <- round(table_A_sum$totwghtlandg, digits = 3)
table_A_sum$unwanted_catch <- round(table_A_sum$unwanted_catch, digits = 3)

#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#                       2. aggregate AGE DATA for merging                       
#-------------------------------------------------------------------------------

source("db.R")

agedata <- read.dbTable("suomu","report_individual")
