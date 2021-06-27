#' Read multiple CSV files and combine them specifying the name of columns
#'
#' The name of the column is a **regular expression** and if it matches the name of
#' a column in the CSV file, that column is selected and returned in the final data set.
#' The year of the CSV file is stored in the first column of the data set.
#'
#' @param files vector of paths to the CSV files to read
#' @param col_names **regular expression** to select the column or columns
#'
#' @return Tibble with the year of the data in the first column and the selected columns
read_csv_by_cols <- function(files, col_names) {
  columns <- unlist(col_names)
  list <- lapply(
    files,
    function(x) {
      # obtain the year from the file name
      year <- basename(x) %>% substring(1, 4)
      # read the data sets but only with the columns for year and specialty
      readr::read_csv(x) %>%
        dplyr::mutate(year = year) %>%
        dplyr::select(year, dplyr::matches(col_names, perl = TRUE, ignore.case = TRUE))
    }
  )
  # collapse the list into a tibble / data frame
  do.call(dplyr::bind_rows, list)
}
