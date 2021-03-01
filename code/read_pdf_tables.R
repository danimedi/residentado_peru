library(here)
library(tabulizer)
library(readr)
library(stringr)

# obtain files to extract tables from
pdfs <- list.files(here("data/raw"), pattern = "[.]pdf$", full.names = TRUE)

# loop to extract the tables and write csv files
sapply(pdfs, function(pdf) {
  dat <- extract_tables(pdf, output = "matrix")
  
  # find the way to join the tables for all the pdfs
  
  
  # file <- paste0(gsub("[.]pdf$", "", basename(pdf)), ".csv")
  # file <- here("data/clean", file)
  # write_csv(dat, file)
})
