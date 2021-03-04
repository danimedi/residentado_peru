library(targets)
sapply(list.files("code", full.names = TRUE), source)
tar_option_set(packages = "here", "tabulizer", "readr")

list(
  tar_target(read_pdf_tables, read_pdf_tables())
)
