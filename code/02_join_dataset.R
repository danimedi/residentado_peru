library(readr)
library(dplyr)
library(stringr)
library(here)

ingresantes <- read_csv(here("data", "clean", "ingresantes.csv"),
                        col_types = cols(`No Doc.` = col_integer()))
resultados <- read_csv(here("data", "clean", "resultados.csv"),
                       col_types = cols(`No Doc.` = col_integer()))

dat <- left_join(
  resultados, select(ingresantes, c(`No Doc.`, Sede)), 
  by = c("No Doc.")
) %>% select(-c(No, Codigo, CMP, `No Doc.`, Obs.))
  
write_csv(dat, here("data", "clean", "joined_dataset.csv"))
