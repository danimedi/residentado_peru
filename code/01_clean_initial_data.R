library(tabulizer)
library(here)

ingresantes <- extract_tables(here("data", "raw", "Relacion de ingresantes al cierre del proceso de admision.pdf"),
                              output = "matrix")
col_names <- ingresantes[[1]][1,-c(9,11)]
ingresantes[[1]] <- ingresantes[[1]][-1,-c(11,12)]
ingresantes_mtrx <- do.call(rbind, ingresantes)
ingresantes_dat <- as.data.frame(ingresantes_mtrx)
names(ingresantes_dat) <- col_names
write.csv(ingresantes_dat, here("data", "clean", "ingresantes.csv"), row.names = FALSE)

resultados <- extract_tables(here("data", "raw", "Resultados del examen 2019.pdf"),
                             output = "matrix")
col_names <- resultados[[1]][1,-c(11,13)]
resultados[[1]] <- resultados[[1]][-1,-c(23,22)]
resultados_mtrx <- do.call(rbind, resultados)
resultados_dat <- as.data.frame(resultados_mtrx)
names(resultados_dat) <- col_names
write.csv(resultados_dat, here("data", "clean", "resultados.csv"), row.names = FALSE)
