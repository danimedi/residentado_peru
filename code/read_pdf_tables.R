
# packages ----------------------------------------------------------------

library(here) # to deal with the paths of the project
library(tabulizer) # to read the tables of PDF files 
library(readr) # to write CSV files faster

# example to test ---------------------------------------------------------

mtrx <- extract_tables("data/raw/practice_sample/example.pdf", 
                       output = "matrix", encoding = "UTF-8")
# obtain the names removing the false empty values in the first row of the
# first page
i <- sapply(mtrx[[1]][1,], function(x) nchar(x) > 0)
col_names <- mtrx[[1]][1,i]
# remove the last (false) empty columns to create `table1` without the names
# of the first row
i <- seq_len(ncol(mtrx[[2]]))
table1 <- mtrx[[1]][-1,i]
# join everything
mtrx1 <- do.call(rbind, mtrx[-1])
mtrx1 <- rbind(table1, mtrx1)
# name columns and save data frame as CSV
colnames(mtrx1) <- col_names
mtrx1 <- as.data.frame(mtrx1)
write_csv(mtrx1, "data/raw/practice_sample/example.csv")

# real loop ---------------------------------------------------------------

# obtain files to extract tables from
pdfs <- list.files(here("data/raw"), pattern = "[.]pdf$", full.names = TRUE)

# loop to extract the tables and write csv files
# (save the results in `res` to check errors)
res <- sapply(pdfs, function(pdf) {
  
  # obtain the name of the output file
  file <- paste0(gsub("[.]pdf$", "", basename(pdf)), ".csv")
  file <- here("data/clean", file)
  
  # check if it exists to avoid running something more than once
  if (!basename(file) %in% list.files(here("data/clean"))) {
    
    tryCatch({
      print(paste("working with", basename(pdf), "..."))
      
      # extract the tables from PDF file
      dat <- extract_tables(pdf, output = "matrix", encoding = "UTF-8")
      
      # check that there is no problems with the first page (same number of columns)
      # otherwise, fix the first page first, and then join it with the rest
      if (ncol(dat[[1]]) == ncol(dat[[2]])) {
        # bind the rows
        dat <- do.call(rbind, dat)
        # name the columns from the first row and transform it to a data frame
        colnames(dat) <- dat[1,]
        dat <- dat[-1,]
        dat <- as.data.frame(dat)
      } else {
        # remove empty values of the first page and save the names from the first
        # row in `col_names`, `table1` contains the data of the "clean" table/matrix
        # of the first page (by removing the last empty columns)
        i <- sapply(dat[[1]][1,], function(x) nchar(x) > 0)
        col_names <- dat[[1]][1, i]
        i <- seq_len(ncol(dat[[2]]))
        table1 <- dat[[1]][-1, i]
        # join everything
        dat <- do.call(rbind, dat[-1])
        dat <- rbind(table1, dat)
        # name columns
        colnames(dat) <- col_names
        dat <- as.data.frame(dat)
      }
      
      # save data as CSV
      write_csv(dat, file)
      
    }, error = function(e) e, warning = function(w) w)
  }
  
})

# check the missing transformed files
raws <- list.files(here("data/raw"), pattern = "[.]pdf$")
raws <- gsub("[.]pdf$", "", raws)
cleans <- list.files(here("data/clean"), pattern = "[.]csv$")
cleans <- gsub("[.]csv$", "", cleans)
raws[!raws %in% cleans]

