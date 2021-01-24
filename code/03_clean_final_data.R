library(here)

dat <- as.matrix(read.csv(here("data", "clean", "joined_dataset.csv")))

#remove NAs
dat <- apply(dat, 2, function(x) ifelse(x == "<NA>", NA, x))

# remove "pts" words
dat <- apply(dat, 2, function(x) gsub(x, pattern = " (pts[.]|pts)", replacement = ""))

dat <- as.data.frame(dat)

# columns to convert to numeric type
x <- c("Serum", "SNCDS", "X1er.Niv.", "X5to.Sup.", "Prom.Pre", "ENAM", "Examen", "Factor.A.", "ota.Fina")

# detect problems in the conversion to numeric type
errors <- vector("list")
k <- 1
apply(dat[,x], 2, function(col) {
  new <- as.numeric(col)
  i <- !(is.na(col) == is.na(new))
  errors[[colnames(dat[,x])[k]]] <<- col[i]
  k <<- k + 1
})

dat[,"Serum"] <- ifelse(dat[,"Serum"] == "a8.000", "8.000", dat[,"Serum"])

# convert to numeric type
dat[,x] <- apply(dat[,x], 2, as.numeric)

# change names
colnames(dat) <- ifelse(colnames(dat) == "ota.Fina", "Nota.Final", colnames(dat))

write.csv(dat, here("data", "clean", "final_data.csv"), row.names = FALSE)
