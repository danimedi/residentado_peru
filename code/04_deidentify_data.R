library(here)
library(readr)

dat <- read_csv(here("data/clean/final_data.csv"))

# exclude first names and last names
i <- !colnames(dat) %in% c("Apellido.Paterno", "Apellido.Materno", "Nombres")
dat <- dat[,i]

# randomize the rows
i <- sample(seq_len(nrow(dat)))
dat <- dat[i,]

write_csv(dat, here("data/clean/deidentified_data.csv"))
