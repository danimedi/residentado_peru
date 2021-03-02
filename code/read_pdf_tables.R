
# packages ----------------------------------------------------------------

library(here)
library(tabulizer)
library(readr)
library(stringr)

# example to test ---------------------------------------------------------

mtrx <- extract_tables("data/raw/practice_sample/example.pdf", 
                       output = "matrix", encoding = "UTF-8")
mtrx1 <- do.call(rbind, mtrx)
colnames(mtrx1) <- mtrx1[1,]
mtrx1 <- mtrx1[-1,]
mtrx1 <- as.data.frame(mtrx1)
write_csv(mtrx1, "data/raw/practice_sample/example.csv")


# real loop ---------------------------------------------------------------

# obtain files to extract tables from
pdfs <- list.files(here("data/raw"), pattern = "[.]pdf$", full.names = TRUE)

# loop to extract the tables and write csv files
res <- sapply(pdfs, function(pdf) {
  tryCatch({
    # extract the tables from a pdf file and join the tables into a single one
    dat <- extract_tables(pdf, output = "matrix", encoding = "UTF-8")
    dat <- do.call(rbind, dat)
    
    # name the columns from the first row and transform it to a data frame
    colnames(dat) <- dat[1,]
    dat <- dat[-1,]
    dat <- as.data.frame(dat)
    
    # save data as CSV file with the same name
    file <- paste0(gsub("[.]pdf$", "", basename(pdf)), ".csv")
    file <- here("data/clean", file)
    write_csv(dat, file)
  }, error = function(e) e)
})

# check results to find errors
res


