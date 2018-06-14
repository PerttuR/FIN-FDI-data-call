library(RPostgreSQL);

read.dbTable <- function(schema, table) {
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
  data <- dbGetQuery(con, paste("SELECT * from ",schema,".",table, sep=""));
  dbDisconnect(con);
  rm(con);
  return(data);
}

