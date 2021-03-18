#' Combine data sets from different years into one big data set with all the information
#'
#' @param files vector of strings with the paths of the files to the CSV files
#' @param years vector with the years of the files, they have to be the same length as 
#'
#' @return
#' @export
#'
#' @examples
#' files <- list.files("data/clean/identified", full.names = TRUE, pattern = "postulantes")
#' years <- 2013:2020
#' unify_data_sets(files, years)
#' 
unify_data_sets <- function(files, years) {
  # read the files in a list as characters
  dat <- lapply(seq_along(files), function(i) {
    dplyr::mutate(readr::read_csv(files[[i]], col_types = readr::cols(.default = col_character())), 
           year = years[[i]])
  })
  # bind the data sets by rows
  dplyr::bind_rows(dat)
}
