

#-------------------------------------------------------------------------------
#
# Function for plotting FDI time series 
#
# Coded: Antti Sykkö
#
# Creation Date: JUN-2023
#
# Client: LUKE EU-DCF project
#
# Params: 
#   - D         : a data frame (FDI table)
#   - COL       : target column
#   - GROUPCOL  : grouping column
#   - fun_      : aggregation function (sum, min, max, ...)
#-------------------------------------------------------------------------------

plotTimeSeriesByGroup <- function(D, COL, GROUPCOL, fun_) {
  # Calculate the sum of the specified COL grouped by the GROUPCOL
  aggregated_data <- aggregate(D[[COL]], by = list(D[[GROUPCOL]], D[["YEAR"]]), FUN = fun_)
  colnames(aggregated_data) <- c(GROUPCOL, "YEAR", paste("Sum of", COL))
  
  # .. empty grid for plotting
  plot(NULL, xlim = range(aggregated_data$YEAR), ylim = range(aggregated_data[, paste("Sum of", COL)]), 
       xlab = "Year", ylab = paste("Sum of", COL), main = paste0(COL, " by ", GROUPCOL))
  
  grid()
  
  unique_groups <- unique(aggregated_data[[GROUPCOL]])
  
  # .. plot time series with separate lines for each group
  for (group in unique_groups) {
    group_data <- subset(aggregated_data, aggregated_data[[GROUPCOL]] == group)
    lines(group_data$YEAR, group_data[, paste("Sum of", COL)], type = "l", col = match(group, unique_groups))
  }
  
  # ... legend for the groups
  legend("topright", legend = unique_groups, col = 1:length(unique_groups), lty = 1, cex = 0.8)
  
  # ... add labels for x-axis 
  axis(1, at = unique(aggregated_data$YEAR), labels = as.integer(unique(aggregated_data$YEAR)))
  
  
  
}
