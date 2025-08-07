# testing variables in SAS logbook data
# some variables have to be renamed and some can be removed

library(tidyverse)
library(skimr)
library(waldo)

## metier data ####
for (i in 2013:2015){
  
  tmp <- read_sas(paste0("orig/pvkarvo",i,"_metier.sas7bdat")) |>
    mutate(KALASTUSVUOSI = i)
  
  assign(paste0("metier_",i), tmp)
  
} 

# change all column names to upper case

names(metier_2013) <- toupper(names(metier_2013))
names(metier_2014) <- toupper(names(metier_2014))
names(metier_2015) <- toupper(names(metier_2015))

# remove column labels
var_label(metier_2013) <- NULL
var_label(metier_2014) <- NULL
var_label(metier_2015) <- NULL

# identical(metier_2013$TE, metier_2013$TE_PAL)

skim(metier_2013)
skim(metier_2014)
skim(metier_2015)

compare(metier_2013$TE, metier_2013$TE_PAL)
grep("LENGTH", names(metier_2013), value=TRUE); unique(sort(metier_2013$LENGTH)) # -> can we derive VESSEL_LENGTH???
grep("LENGTH", names(metier_2014), value=TRUE); unique(sort(metier_2014$VESSEL_LENGTH))
grep("LENGTH", names(metier_2015), value=TRUE); unique(sort(metier_2015$VESSEL_LENGTH)); unique(sort(metier_2015$LENGTH))
# compare(metier_2015$VESSEL_LENGTH, metier_2015$LENGTH, max_diffs=Inf)

grep("ASTUNNUS", names(metier_2013), value=TRUE); metier_2013$ASIAKASTUNNUS # set to ASTUNNUS
grep("ASTUNNUS", names(metier_2014), value=TRUE); metier_2014$ASTUNNUS
grep("ASTUNNUS", names(metier_2015), value=TRUE); metier_2015$ASTUNNUS

unique(sort(metier_2013$"_NAME_")); unique(sort(metier_2013$"_LABEL_"))
compare(metier_2013$"_NAME_", metier_2013$"_LABEL_") # are the same
compare(metier_2015$"_NAME_", metier_2015$"_LABEL_") # are the same -> only keep label

unique(sort(metier_2013$NIMI)) # -> set to OMISTAJA
unique(sort(metier_2014$OMISTAJA))
unique(sort(metier_2015$OMISTAJA))

unique(sort(metier_2014$ALUE))
unique(sort(metier_2015$ALUE))

grep("KUNTA", names(metier_2013), value=TRUE);
grep("KUNTA", names(metier_2014), value=TRUE);
grep("KUNTA", names(metier_2015), value=TRUE);

compare(metier_2014$KUNTA, metier_2014$KUNTA_KNRO, max_diffs = Inf)
compare(metier_2014$KUNTA, metier_2014$PURKU_KUNTA, max_diffs = Inf) 
compare(metier_2015$KUNTA, metier_2015$SATAMAKUNTA, max_diffs = Inf) 

grep("SATAMA", names(metier_2013), value=TRUE);
grep("SATAMA", names(metier_2014), value=TRUE);
grep("SATAMA", names(metier_2015), value=TRUE);

unique(sort(metier_2014$SATAMA_NIMI))  # set to SATAMA
unique(sort(metier_2015$SATAMA))

# testing variables in SAS shore data
compare(shore_2014$"_LABEL_", shore_2013$"_NAME_", max_diffs = Inf) # remove _NAME_
compare(shore_2015$"_LABEL_", shore_2015$"_NAME_", max_diffs = Inf)