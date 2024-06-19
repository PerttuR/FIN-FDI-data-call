library(RPostgres)

set_utf8 <- function(x){
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
}

validate.params <- function(schema, table) {
  if(length(c(table)) != 1 || length(c(schema)) != 1) {
    stop("missing schema or table name");
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", table)) {
    stop(paste("bad table name", table));
  }
  if(!grepl("^[-[:digit:][:alpha:]_]+$", schema)) {
    stop(paste("bad schema name", schema));
  }
}

get.params <- function(dbname, env) {
  param_name <- paste0("db_params_", dbname, ".R")
  if(!is.null(dbname) && file.exists(param_name)) {
    source(param_name, local = env)
  } else {
    source("db_params.R", local = env)
  }
}

cache.write <- function(name, data) {
  cache_folder <- "DATA-CACHE"
  if (!dir.exists(cache_folder)) {
    dir.create(cache_folder)
  }
  file_path <- file.path(cache_folder, paste0(name, ".rds"))
  saveRDS(data, file_path)
}

cache.read <- function(name) {
  cache_folder <- "DATA-CACHE"
  file_path <- file.path(cache_folder, paste0(name, ".rds"))
  if (file.exists(file_path)) {
    data <- readRDS(file_path)
    message("Cache does contain data with name '", name, "'")
    return(data)
  } else {
    message("Cache does not contain data with name '", name, "'. Returning NA.")
    return(NA)
  }
}

cache.name <- function(vars) {
  pasted <- paste(vars, collapse = ";")
  return(paste(charToRaw(pasted), collapse=""))
}

read.dbTable <- function(schema, table, where = NA, dbname = NULL) {
  validate.params(schema, table)
  where <- ifelse(is.na(where),"",paste0(" WHERE ", where))

  cname <- cache.name(c(schema, table, where))
  candidate <- cache.read(cname)
  if (!all(is.na(candidate))) {
    return(candidate)
  }

  tmp <- new.env()
  get.params(dbname, tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp)
  data <- set_utf8(dbGetQuery(con, paste0('SELECT * from "',schema,'".',table,where)))
  dbDisconnect(con)
  rm(con)
  cache.write(cname, data)
  return(data)
}

query.dbTable <- function(schema, table, dbname = NULL, query ) {
  validate.params(schema, table)
  tmp <- new.env()
  get.params(dbname, tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp)
  data <- set_utf8(dbGetQuery(con, query))
  dbDisconnect(con)
  rm(con)
  return(data)
}

write.dbTable <- function(schema, table, data, dbname = NULL, overwrite = FALSE) {
  validate.params(schema, table)
  tmp <- new.env()
  get.params(dbname, tmp)
  drv <- RPostgres::Postgres()
  resolved_dbname <- ifelse(is.null(dbname), tmp$dbname, dbname)
  con <- dbConnect(drv, dbname = resolved_dbname,
                   host = tmp$host, tmp$port,
                   user = tmp$user, password = tmp$password)
  rm(tmp)
  dbExecute(con, "set DATESTYLE TO ISO")
  dbExecute(con, "set role write_kake_siirto")
  dbWriteTable(con, Id(schema=schema, table=table), data, overwrite = overwrite)
  dbDisconnect(con)
  rm(con)
  return(data)
}
