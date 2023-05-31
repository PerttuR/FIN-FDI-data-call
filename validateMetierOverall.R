
#-------------------------------------------------------------------------------
#
# Function for METIER validation
#
# Coded: Antti Sykkö
#
# Creation Date: MAY-2023
#
# Params: 
#     - D = a dataset containing column METIER or metier
#     - codelist = a metier codelist given by icesVocab::getCodeList("Metier6_FishingActivity", date = NULL)
#
# Output: Prints invalid metier codes to console
#
# Example:
# ... import codelist from IcesVocab 
#clist <- getCodeList("Metier6_FishingActivity", date = NULL)
#
# .. validate metier in data df
#validateMetierOverall(df, clist)

#-------------------------------------------------------------------------------


validateMetierOverall <- function(D, codelist) {
  
  
  invalidMetiers <- toString(unique(
    D[which(
      !(D$METIER %in% codelist$Key) 
      & D$METIER != "NK"),]$METIER))
  
   
  
  dataName <- deparse(substitute(D))
  invalidMetiersString <- toString(sort(invalidMetiers))
  
  invalidMetiersString <- gsub(",", "\n ", invalidMetiersString)
  
  invalidMetiersString <- paste0("  ", invalidMetiersString)
  
  # ... check what metiers are wrong type
  if (!(invalidMetiers == "")) {
    message("Invalid Metiers in data ", dataName, ": ")
    cat(invalidMetiersString)
  } else {
    message("All metiers are valid in data ", dataName, ".")
  }
  
  
}



