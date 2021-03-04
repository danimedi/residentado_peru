library(targets)
sapply(list.files("code", full.names = TRUE, recursive = TRUE), source)
tar_option_set(packages = "here", "tabulizer", "readr", "dplyr", "magrittr")

list(
  tar_target(
    read_pdf_tables, 
    read_pdf_tables(
      path_input_pdf = "data/raw", 
      path_output_csv = "data/clean/identified"
    )
  ),
  tar_target(
    write_especialidad_aplicantes,
    condense_different_years(
      input_path = "data/clean/identified", 
      output_csv_path = "data/clean/identified/especialidad_aplicantes.csv", 
      cols_regex = "Especialidad"
    )
  )
)
