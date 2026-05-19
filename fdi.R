#-------------------------------------------------------------------------------
#
# Script to process FIN- commercial data for STECF FDI data call
#
# By: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
# Code by: Perttu Rantanen, Mira Sustar, Petri Sarvamaa
# Contact: icttuki@valtori :9 :) ) hou hou hou
#
# Date: JUN-2018
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

#--------------------READ ME----------------------------------------------------
# The following script is for preparing FDI data tables from Table A fron stat DEP (Pirkko)
#-------------------------------------------------------------------------------


#- Clear workspace
rm(list=ls())


#WD juureen

# Luetaan Pirkon A-taulu sisään(Import Table A):

table_A<-read.csv2("FIN_TABLE_A_CATCH.csv", sep="," )

library(dplyr)

domain_discards_key =table_A %>% distinct(domain_discards)
domain_landings_key =table_A %>% distinct(domain_landings)

count(domain_discards_key)
count(domain_landings_key)

source("db.R")

spec <- read.dbTable("suomu","species")
lengthdata <- read.dbTable("suomu","report_lengthclassrecords")
