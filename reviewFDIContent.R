



#-------------------------------------------------------------------------------
#
# Script to review FIN - commercial data for STECF FDI data call
#
# Coded: Antti Sykkö
#
# Date: June 2023
# Updated: 
#
# Client: LUKE EU-DCF project
#-------------------------------------------------------------------------------

# ... function for plotting
source("plotTimeSeriesByGroup.R")

# ... path for tables folder 
orig <- paste0(getwd(), .Platform$file.sep, "orig/") 

# ... read tables 
table_A <- read.csv2(paste0(orig,.Platform$file.sep,"A_table_2013_2022.csv"), sep = "," , na.strings = "")
table_J <- read.csv2(paste0(orig,"J_table_2013_2022.csv"), sep = "," ,na.strings="")
table_H <- read.csv2(paste0(orig,"H_table_2013_2022.csv"), sep = "," ,na.strings="")
table_G <- read.csv2(paste0(orig,.Platform$file.sep,"G_table_2013_2022.csv"), sep = "," , na.strings = "")
table_I <- read.csv2(paste0(orig,"I_table_2013_2022.csv"), sep = "," ,na.strings="")



# ... add columns for tot counts 
table_J$TOTAL <- "TOTALS"
table_H$TOTAL <- "TOTALS"
table_G$TOTAL <- "TOTALS"
table_A$TOTAL <- "TOTALS"
table_I$TOTAL <- "TOTALS"


# A ... convert to num 
table_A$TOTWGHTLANDG_NUM <- ifelse(table_A$TOTWGHTLANDG=="NK", 0 ,as.numeric(table_A$TOTWGHTLANDG))
table_A$DISCARDS_NUM <- as.numeric(table_A$DISCARDS)

# H ... convert to num 
table_H$TOTWGHTLANDG_NUM <- ifelse(table_H$TOTWGHTLANDG=="NK", 0 ,as.numeric(table_H$TOTWGHTLANDG))


# G ... convert to num (warnings OK)
varList <- c("TOTKWDAYSATSEA", "TOTGTDAYSATSEA", "TOTKWFISHDAYS", "TOTGTFISHDAYS","HRSEA", "KWHRSEA", "GTHRSEA")

suppressWarnings(
for (var in varList) {
  table_G[, paste0(var, "_NUM")] <- ifelse(table_G[, var] == "NK", 0, as.numeric(table_G[, var]))
}
)


#-------------------------------------------------------------------------------




#------------------------------ TABLE A ----------------------------------------
# 
#-------------------------------------------------------------------------------



# ... first compare total catch sums of A and H 
par(mfrow=c(1,2))
plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_A, "TOTWGHTLANDG_NUM", "TOTAL", fun_ = sum)

# ... catch
plotTimeSeriesByGroup(table_A, "TOTWGHTLANDG_NUM", "GEAR_TYPE", fun_ = sum)
plotTimeSeriesByGroup(table_A, "TOTWGHTLANDG_NUM", "QUARTER" , fun_ = sum)
plotTimeSeriesByGroup(table_A, "TOTWGHTLANDG_NUM", "METIER" , fun_ = sum)
plotTimeSeriesByGroup(table_A, "TOTWGHTLANDG_NUM", "TARGET_ASSEMBLAGE" , fun_ = sum)
plotTimeSeriesByGroup(table_A[which(table_A$TARGET_ASSEMBLAGE != "SPF"),], 
                      "TOTWGHTLANDG_NUM", "TARGET_ASSEMBLAGE" , fun_ = sum)

# ... Discards 
plotTimeSeriesByGroup(table_A, "DISCARDS_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_A, "DISCARDS_NUM", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_A, "DISCARDS_NUM", "SUB_REGION", fun_ = sum)
plotTimeSeriesByGroup(table_A, "DISCARDS_NUM", "SPECIES", fun_ = sum)

# ... aux func 
plotDiscardsBySpecie <- function(specie) {
  par(mfrow=c(1,1))
  plotTimeSeriesByGroup(table_A[table_A$SPECIES == specie,], "DISCARDS_NUM", "SPECIES", fun_ = sum)
}


plotDiscardsBySpecie("SAL")



#------------------------------ TABLE J ----------------------------------------
# 
#-------------------------------------------------------------------------------


                            # ... TOTVES ... # 


plotTimeSeriesByGroup(table_J, "TOTVES", "TOTAL", fun_ = sum)

plotTimeSeriesByGroup(table_J, "TOTVES", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH != "PG"),], "TOTVES", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH == "PG"),], "TOTVES", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J, "TOTVES", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[table_J$VESSEL_LENGTH %in% c("NK", "VL0010"),], "TOTVES", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[!(table_J$VESSEL_LENGTH %in% c("NK", "VL0010")),], "TOTVES", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_J, "TOTVES", "PRINCIPAL_SUB_REGION", fun_ = sum)

#-------------------------------------------------------------------------------

                          # ... TOTTRIPS ... # 


plotTimeSeriesByGroup(table_J, "TOTTRIPS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_J, "TOTTRIPS", "TOTAL", fun_ = max)

plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH != "PG"),], "TOTTRIPS", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH == "PG"),], "TOTTRIPS", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J, "TOTTRIPS", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[table_J$VESSEL_LENGTH %in% c("NK", "VL0010"),], "TOTTRIPS", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[!(table_J$VESSEL_LENGTH %in% c("NK", "VL0010")),], "TOTTRIPS", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_J, "TOTTRIPS", "PRINCIPAL_SUB_REGION", fun_ = sum)



#-------------------------------------------------------------------------------

                              # ... MAXSEADAYS ... # 


plotTimeSeriesByGroup(table_J, "MAXSEADAYS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_J, "MAXSEADAYS", "TOTAL", fun_ = max)

plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH != "PG"),], "MAXSEADAYS", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH == "PG"),], "MAXSEADAYS", "FISHING_TECH", fun_ = sum)
plotTimeSeriesByGroup(table_J, "MAXSEADAYS", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[table_J$VESSEL_LENGTH %in% c("NK", "VL0010"),], "MAXSEADAYS", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[!(table_J$VESSEL_LENGTH %in% c("NK", "VL0010")),], "MAXSEADAYS", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_J, "MAXSEADAYS", "PRINCIPAL_SUB_REGION", fun_ = sum)


#-------------------------------------------------------------------------------

                                # ... AVGAGE ... # 

# ... convert to num -> NOTE: warning is OK here 
table_J$AVGAGE_NUM <- ifelse(table_J$AVGAGE=="NK", 0 ,as.numeric(table_J$AVGAGE))


plotTimeSeriesByGroup(table_J, "AVGAGE_NUM", "TOTAL", fun_ = mean)
plotTimeSeriesByGroup(table_J, "AVGAGE_NUM", "TOTAL", fun_ = max)

plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH != "PG"),], "AVGAGE_NUM", "FISHING_TECH", fun_ = mean)
plotTimeSeriesByGroup(table_J[which(table_J$FISHING_TECH == "PG"),], "AVGAGE_NUM", "FISHING_TECH", fun_ = mean)
plotTimeSeriesByGroup(table_J, "AVGAGE_NUM", "VESSEL_LENGTH", fun_ = sum)
plotTimeSeriesByGroup(table_J[table_J$VESSEL_LENGTH %in% c("NK", "VL0010"),], "AVGAGE_NUM", "VESSEL_LENGTH", fun_ = mean)
plotTimeSeriesByGroup(table_J[!(table_J$VESSEL_LENGTH %in% c("NK", "VL0010")),], "AVGAGE_NUM", "VESSEL_LENGTH", fun_ = mean)

plotTimeSeriesByGroup(table_J, "AVGAGE_NUM", "PRINCIPAL_SUB_REGION", fun_ = mean)




#------------------------------ TABLE H ----------------------------------------
# 
#-------------------------------------------------------------------------------


                            # ... TOTWGHTLANDG_NUM ... # 


plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "TOTAL", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "QUARTER", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "METIER", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "SUB_REGION", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$MESH_SIZE_RANGE %in% c("NA","16D32")),], 
                      "TOTWGHTLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)


plotTimeSeriesByGroup(table_H[(table_H$MESH_SIZE_RANGE %in% c("NA", "32D90","16D32")),], 
                      "TOTWGHTLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "SPECIES", fun_ = sum)
plotTimeSeriesByGroup(table_H[(table_H$SPECIES %in% c("HER", "SPR")),], "TOTWGHTLANDG_NUM", "SPECIES", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$SPECIES %in% c("HER", "SPR")),], "TOTWGHTLANDG_NUM", "SPECIES", fun_ = sum)

plotTimeSeriesByGroup(table_H[(table_H$SPECIES %in% c("SAL")),], "TOTWGHTLANDG_NUM", "SPECIES", fun_ = sum)


plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "GEAR_TYPE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$TARGET_ASSEMBLAGE %in% c("SPF")),], "TOTWGHTLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTWGHTLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)

#-------------------------------------------------------------------------------


                                # ... TOTVALLANDG ... # 

# ... convert to num 
table_H$TOTVALLANDG_NUM <- ifelse(table_H$TOTVALLANDG=="NK", 0 ,as.numeric(table_H$TOTVALLANDG))


plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "TOTAL", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "QUARTER", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "METIER", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "SUB_REGION", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$MESH_SIZE_RANGE %in% c("NA","16D32")),], 
                      "TOTVALLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)


plotTimeSeriesByGroup(table_H[(table_H$MESH_SIZE_RANGE %in% c("NA", "32D90","16D32")),], 
                      "TOTVALLANDG_NUM", "MESH_SIZE_RANGE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "SPECIES", fun_ = sum)
plotTimeSeriesByGroup(table_H[(table_H$SPECIES %in% c("HER", "SPR")),], "TOTVALLANDG_NUM", "SPECIES", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$SPECIES %in% c("HER", "SPR")),], "TOTVALLANDG_NUM", "SPECIES", fun_ = sum)

plotTimeSeriesByGroup(table_H[(table_H$SPECIES %in% c("SAL")),], "TOTVALLANDG_NUM", "SPECIES", fun_ = sum)


plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "GEAR_TYPE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)
plotTimeSeriesByGroup(table_H[!(table_H$TARGET_ASSEMBLAGE %in% c("SPF")),], "TOTVALLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)

plotTimeSeriesByGroup(table_H, "TOTVALLANDG_NUM", "TARGET_ASSEMBLAGE", fun_ = sum)



#------------------------------ TABLE G ----------------------------------------
# 
#-------------------------------------------------------------------------------


                          # ... TOTFISHDAYS ... #


plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "TOTAL", fun_ = sum)

plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "VESSEL_LENGTH", fun_ = sum)

plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "QUARTER", fun_ = sum)

plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "METIER", fun_ = sum)

plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "SUB_REGION", fun_ = sum)

plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "MESH_SIZE_RANGE", fun_ = sum)
plotTimeSeriesByGroup(table_G[!(table_G$MESH_SIZE_RANGE %in% c("NA","16D32")),], 
                      "TOTFISHDAYS", "MESH_SIZE_RANGE", fun_ = sum)


plotTimeSeriesByGroup(table_G[(table_G$MESH_SIZE_RANGE %in% c("NA", "32D90","16D32")),], 
                      "TOTFISHDAYS", "MESH_SIZE_RANGE", fun_ = sum)

par(mfrow=c(3,3))
plotTimeSeriesByGroup(table_G, "TOTSEADAYS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTKWDAYSATSEA_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTGTDAYSATSEA_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTKWFISHDAYS_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTGTFISHDAYS_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "HRSEA_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "KWHRSEA_NUM", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "GTHRSEA_NUM", "TOTAL", fun_ = sum)



#------------------------------ TABLE I ----------------------------------------
# 
#-------------------------------------------------------------------------------


# ... compare to table G 
par(mfrow=c(3,2))
plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_I, "TOTFISHDAYS", "TOTAL", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "QUARTER", fun_ = sum)
plotTimeSeriesByGroup(table_I, "TOTFISHDAYS", "QUARTER", fun_ = sum)
plotTimeSeriesByGroup(table_G, "TOTFISHDAYS", "GEAR_TYPE", fun_ = sum)
plotTimeSeriesByGroup(table_I, "TOTFISHDAYS", "GEAR_TYPE", fun_ = sum)












