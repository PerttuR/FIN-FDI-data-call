library(mongolite);

tmp <- new.env()
source("mongo_params.R", local=tmp)

read.mongoCollection <- function(collection) {
  return(mongo(collection=collection, db=tmp$db, url=tmp$connection_string))
}

read.mongoCollectionToDataframe <- function(collection) {
  data <- read.mongoCollection(collection)
  data <- data$find(fields = "{}")
  data <- rename_with(data, ~paste0(collection, .), .cols=c("_id"))
  return(data)
}