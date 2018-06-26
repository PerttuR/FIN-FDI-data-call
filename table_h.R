#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call - TABLE H
#
# Coded: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for futher prepare FDI data table H from partial Table H from statistical DEP (Pirkko)
#-------------------------------------------------------------------------------


# install.packages("RPostgreSQL")
# install.packages("dplyr")


#- Clear workspace
rm(list=ls())

# needed libraries
library(dplyr)
library(vmstools)
library(magrittr)

#-------------------------------------------------------------------------------
#                   0. set working directories to match folder paths                      
#-------------------------------------------------------------------------------
# Mira:
path_tablea <- "C:/2018/FDI/work/data/orig/" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/2018/FDI/work/prog/FIN-FDI-data-call/" # folder where the r project is (and the source file db.R!)
path_out <- "C:/2018/FDI/work/data/der/" # folder where the output is saved

# Perttu:
path_tablea <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/orig" # folder where TABLE A is (FIN_TABLE_A_CATCH.csv)
path_rproject <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call" # folder where the r project is (and the source file db.R!)
path_out <- "C:/perttu/eu-tike/STECF/FIN-FDI-data-call/results" # folder where the output is saved

#-------------------------------------------------------------------------------
#                       1. aggregate TABLE A for merging                       
#-------------------------------------------------------------------------------
setwd(path_tablea)

# import table A
table_H <- read.csv2("FIN_TABLE_H_LANDINGS.csv", sep = "," )
#-------------------------------------------------------------------------------

setwd(path_rproject)
source("spatial.R")


midpoints <- latlon(table_H$rectangle,midpoint=TRUE)


table_H <- tibble::rowid_to_column(table_H, "ID")
midpoints <- tibble::rowid_to_column(midpoints, "ID")

table_H <- full_join(table_H, midpoints,copy = TRUE)

table_H <- table_H %>% rename(RECTANGLE_LAT = SI_LATI, RECTANGLE_LON = SI_LONG)


names(table_H) %<>% toupper

#table_H <- table_H %>% select()
# to be continued
