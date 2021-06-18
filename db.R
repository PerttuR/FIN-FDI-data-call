#' Reads a table from  data base.
#' @param schema = name of the schema, must consist of A-z, -, _, 0-9
#' @param table = name of a table in the schema, must consist of A-z, -, _, 0-9
#' @param where = where condition, e.g. "age BETWEEN 0 AND 25"
#' @return the table as a data.frame

library(RPostgreSQL);

read.dbTable <- function(schema, table, where=NA) {
  where = ifelse(is.na(where),"",paste0(" WHERE ",where))
  set_utf8 = function(x){
    # Declare UTF-8 encoding on all character strings:
    for(i in 1:ncol(x)){
      if(is.character(x[, i])) {
        Encoding(x[, i]) <- "UTF-8"        
      }
    }
    # Same on column names:
    for(name in colnames(x)){
      Encoding(name) <- "UTF-8"
    }
    return(x)
    
    # TODO: would this be much faster?
    # Encoding(x[is.charachter(x),]) <- "UTF-8" 
    # Encoding(Encoding(colnames(x)) <- "UTF-8")
    
    
  }
  #Some validation
  if(length(c(table)) != 1 || length(c(schema)) != 1) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", table)) {
    return(NULL);
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", schema)) {
    return (NULL);
  }
  #End validation
  tmp <- new.env();
  source("db_params.R", local=tmp);
  drv <- dbDriver("PostgreSQL");
  con <- dbConnect(drv, dbname = tmp$dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password);
  rm(tmp);
  data <- set_utf8(dbGetQuery(con, paste("SELECT * from ",schema,".",table,where,sep="")))
  dbDisconnect(con);
  rm(con);
  return(data);
}

