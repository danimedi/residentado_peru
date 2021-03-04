#' Save tables from raw PDF data as CSV files
#'
#' @param path_input_pdf relative path in the project to the folder where the PDF files are located
#' @param path_output_csv relative path in the project to the folder where the CSV files will be saved
#'
#' @return
#' @export
#'
#' @examples

read_pdf_tables <- function(path_input_pdf = "data/raw", path_output_csv = "data/clean/identified_data") {
  
  # obtain files to extract tables from
  pdfs <- list.files(here::here(path_input_pdf), pattern = "[.]pdf$", full.names = TRUE)
  
  # loop to extract the tables and write csv files
  # (save the results in `res` to check errors)
  res <- sapply(pdfs, function(pdf) {
    
    # obtain the name of the output file
    file <- paste0(gsub("[.]pdf$", "", basename(pdf)), ".csv")
    file <- here::here(path_output_csv, file)
    
    # check if it exists to avoid running something more than once
    if (!basename(file) %in% list.files(here::here(path_output_csv))) {
      
      tryCatch({
        print(paste("working with", basename(pdf), "..."))
        
        # extract the tables from PDF file
        dat <- tabulizer::extract_tables(pdf, output = "matrix", encoding = "UTF-8")
        
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
        readr::write_csv(dat, file)
        
      }, error = function(e) e, warning = function(w) w)
    }
    
  })
  
  # check the missing transformed files
  raws <- list.files(here::here(path_input_pdf), pattern = "[.]pdf$")
  raws <- gsub("[.]pdf$", "", raws)
  cleans <- list.files(here::here(path_output_csv), pattern = "[.]csv$")
  cleans <- gsub("[.]csv$", "", cleans)
  paste("there are", raws[!raws %in% cleans], "missing files in the output folder")
  
}
