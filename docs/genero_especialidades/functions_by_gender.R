# FUNCTIONS FOR THE DATA OF CONAREME (POSTULATIONS)

# give more popular:
more_pop <- function(column, top){
      nd <- dat[ , column ]
      -(sort(-table(nd))[1:top])
      }

# My merge, neccessary for join data for the timelines
# merge() only works with 2 data frames, there are some universities (of entrances)
# that are NOT present in all years, se merge will inlcude NAs

MyMerge <- function(x, y){
  df <- merge(x, y, by= "row.names", all=T)
  rownames(df)  <- df$Row.names
  df$Row.names <- NULL
  return(df)
}


timeline_by_spec_enters <- function(column, specialty, top = 5, years = 2016:2020, ylim = NULL) {
  
  #first of all, select the "plot" (what to show in the plot, can be more than one):
  for(i in specialty){
            newdat <- dat[ dat$especialidad_subespecialidad_ingresantes == i, ]
            
            ls <- list()
            for(y in years){
              # each year
              dat_year <- newdat[newdat$year == y, ]
              # only with the entrances, and select the column that want to be evaluated:
              dat_y_ent <- dat_year[dat_year$ingreso == 1, column]
              ls[[which(years == y)]] <- as.matrix(table(dat_y_ent))
            }
            
            tab <- data.frame(Reduce(MyMerge, ls))
            names(tab) <- years
            
            # now, select only top, OF THE ENTERS:
            dat_tot_ent <- dat[dat$ingreso == 1, ]
            total_tab <- table(dat_tot_ent[, column])
            idx <- order(-total_tab)[1:top]
            top_names_idx <- names( total_tab[idx] )
            new_tab <- tab[top_names_idx, ]
            # now we have a matrix were rows are variables and cols the years
            # plot
            par(mar=c(5.1, 4.1, 4.1, 2.1))
            matplot(t(new_tab), type = "l", lty = "solid", xaxt = "n", ylim = ylim, main = i)
            axis(1, at=seq_along(years), labels=colnames(new_tab))
            legend("bottomright", inset=c(0,1), xpd=T, horiz=T,
                   legend = rownames(new_tab), col = seq_len(nrow(new_tab)), pch = 16)
            }
}

##
give_timeline <- function(column, top = 6, years = 2013:2020, ylim) {
  
  ls <- list()
  for(y in years){
    # first each year, all, not only entrances
    dat_year <- dat[dat$year == y, ]
    # select the column that want to be evaluated:
    dat_y_col <- dat_year[ , column]
    ls[[which(years == y)]] <- as.matrix(table(dat_y_col))
  }
  
  tab <- data.frame(Reduce(MyMerge, ls))
  names(tab) <- years
  
  # now, select only top of ALL:
  total_tab <- table(dat[, column])
  idx <- order(-total_tab)[1:top]
  top_names_idx <- names( total_tab[idx] )
  new_tab <- tab[top_names_idx, ]
  # now we have a matrix were rows are variables and cols the years
  
  # plot
  par(mar=c(5.1, 4.1, 4.1, 2.1))
  matplot(t(new_tab), type = "l", lty = "solid", xaxt = "n", ylim = ylim)
  axis(1, at=seq_along(years), labels=colnames(new_tab))
  legend("bottomright", inset=c(0,1), xpd=T, horiz=T,
         legend = rownames(new_tab), col = seq_len(nrow(new_tab)), pch = 16)
  
  # now we are going to add the entrance lines:
  
  ls2 <- list()
  for(y in years){
    # first each year
    dat_year <- dat[dat$year == y, ]
    # only with the entrances, and select the column that want to be evaluated:
    dat_y_ent <- dat_year[dat_year$ingreso == 1, column]
    ls2[[which(years == y)]] <- as.matrix(table(dat_y_ent))
  }
  
  tab2 <- data.frame(Reduce(MyMerge, ls2))
  names(tab2) <- years
  
  # now, select only the variables of the PREVIOUS SELECTION:
  new_tab2 <- tab2[top_names_idx, ]
  # now we have a matrix were rows are variables and cols the years
  
  # plot
  matplot(t(new_tab2), type = "l", lty = 3, xaxt = "n", ylim = ylim, add = T, lwd = 2)
}

timelines_by <- function(column = "gender", plots = "especialidad_subespecialidad_ingresantes", top_column = 2, top_by = 5, years = 2016:2020, ylim = NULL) {
  
  # select only top, of enters:
  tot_ent <- dat[dat$ingreso == 1, ]
  total_tab <- table(tot_ent[, plots])
  idx_by <- order(-total_tab)[1:top_by]
  idx2 <- names(sort(-total_tab)[1:top_by]) # given by names
  
  for(i in idx2 ){
    
    dat2 <- tot_ent[ tot_ent[, plots] == i, ]
    
    ls <- list()
    for(y in years){
      # first each year
      dat_year <- dat2[dat2$year == y, ]
      # only with the entrances, and select the column that want to be evaluated:
      dat_y_ent <- dat_year[ , column]
      ls[[which(years == y)]] <- as.matrix(table(dat_y_ent))
    }
    
    tab <- data.frame(Reduce(MyMerge, ls))
    names(tab) <- years
    
    # now, select only top, OF THE ENTERS:
    dat_tot_ent <- dat[dat$ingreso == 1, ]
    total_tab <- table(dat_tot_ent[, column])
    idx <- order(-total_tab)[1:top_column]
    top_names_idx <- names( total_tab[idx] )
    new_tab <- tab[top_names_idx, ]
    # now we have a matrix were rows are variables and cols the years
    # plot
    par(mar=c(5.1, 4.1, 4.1, 2.1))
    matplot(t(new_tab), type = "l", lty = "solid", xaxt = "n", ylim = ylim, main = i)
    axis(1, at=seq_along(years), labels=colnames(new_tab))
    legend("topleft", xpd=T, horiz=T,
           legend = rownames(new_tab), col = seq_len(nrow(new_tab)),
           pch = 16, cex = 0.5, inset=c(0,-0.2))
  }
}


timelines_by_table <- function(column = "gender", plots = "especialidad_subespecialidad_ingresantes", top_column = 2, top_by = 5, years = 2016:2020) {
  
  # select only top, of enters:
  tot_ent <- dat[dat$ingreso == 1, ]
  total_tab <- table(tot_ent[, plots])
  idx_by <- order(-total_tab)[1:top_by]
  idx2 <- names(sort(-total_tab)[1:top_by]) # given by names
  
  complete_list <- vector(mode = "list", length = length(idx2))
  
  for(i in idx2 ){
    
    dat2 <- tot_ent[ tot_ent[, plots] == i, ]
    
    ls <- list()
    for(y in years){
      # first each year
      dat_year <- dat2[dat2$year == y, ]
      # only with the entrances, and select the column that want to be evaluated:
      dat_y_ent <- dat_year[ , column]
      ls[[which(years == y)]] <- as.matrix(table(dat_y_ent))
    }
    
    tab <- data.frame(Reduce(MyMerge, ls))
    names(tab) <- years
    
    # now, select only top, OF THE ENTERS:
    dat_tot_ent <- dat[dat$ingreso == 1, ]
    total_tab <- table(dat_tot_ent[, column])
    idx <- order(-total_tab)[1:top_column]
    top_names_idx <- names( total_tab[idx] )
    new_tab <- tab[top_names_idx, ]

    complete_list[[which(idx2 == i)]] <- new_tab
  }
  names(complete_list) <- idx2
  complete_list
}

###



give_bars <- function(column, top = 20, years = 2016:2020, adj = 17, ylim = NULL) {
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    
    # postulations
    tab_sp_post <- table(dat_year[, column])
    idx <- (order( -tab_sp_post ))[1:top]
    
    # entrances
    only_ent <- dat_year[dat_year$ingreso == 1, column]
    tab_sp_ent <- table(only_ent)
    
    # index of the enters in the basis of postulations
    idx2 <- names(tab_sp_post[idx]) # note that this index is given by the names (not position as idx)
    
    # plot
    par(mar=c(12,3,2,0))
    barplot(tab_sp_post[idx], las = 2, cex.names = 0.6,
            col = "indianred1", ylim = ylim,
            main = i)
    bb <- barplot(tab_sp_ent[idx2], las = 2, cex.names = 0.6,
                  col = "royalblue1", ylim = ylim,
                  add = T)
    text(x = bb, y = tab_sp_post[idx]+adj, labels = tab_sp_post[idx], cex=.8, col = "red", font = 2) # adding the numbers
    text(x = bb, y = tab_sp_ent[idx2]+adj, labels = tab_sp_ent[idx2], cex=.8, col = "blue", font = 2)
    text(x = bb, y = tab_sp_post[idx]+2.4*adj, labels = format( round(tab_sp_ent[idx2]/tab_sp_post[idx], 2) , nsmall = 2), cex=.6, font = 2)}
}


###

give_sep_bars <- function(column, by = "gender", top = 20, years = 2016:2020, adj = 12){
  # index of most popular of years
  tab_column <- table(dat[ dat$year %in% years, column])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    
    # postulations
    tab_column_post_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    matrix_post <- t(tab_column_post_by[idx,]) # matrix for the postulations
    
    # entrances
    only_ent <- dat_year[dat_year$ingreso == 1, ]
    # now, tables by:
    tab_column_ent_by <- table(only_ent[, c(column,by)])
    # and select the most popular:
    matrix_ent <- t(tab_column_ent_by[idx,]) # matrix for the entrances
    
    # plot, by using the matrices
    par(mar=c(12,3,2,0))
    barplot(matrix_post, beside = T, las = 2, cex.names = 0.6,
            col = "indianred1",
            main = i)
    bb <- barplot(matrix_ent, beside = T, las = 2, cex.names = 0.6,
                  col = "royalblue1",
                  add = T)
    text(x = bb, y = matrix_post+adj, labels = matrix_post, cex=.55, col = "red", font = 2) # adding the numbers
    text(x = bb, y = matrix_ent+adj, labels = matrix_ent, cex=.55, col = "blue", font = 2)
    text(x = bb, y = matrix_post+2*adj, labels = format( round(matrix_ent/matrix_post, 2) , nsmall = 2),
         cex=.5, font = 2)
    legend("topright", legend = rownames(matrix_post), title = "order") }
}

ent_post_spec_by_gender <- function(top = 20, years = 2016:2020){
  # index of most popular of all years selected (total), ordered by postulation popularity
  tab_column <- table(dat[ dat$year %in% years, "especialidad_subespecialidad_postulantes"])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  years_g <- paste(rep(years, each = 2), c("f", "m"), sep ="" )
  mt_post <- matrix( dimnames = list(idx, years_g) , nrow = length(idx), ncol = length(years_g)) # empty list of postulations
  mt_ent <- matrix(dimnames = list(idx, years_g), nrow = length(idx), ncol = length(years_g)) # empty list of enters
  
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    
    # postulations
    tab_column_post_by <- table(dat_year[, c("especialidad_subespecialidad_postulantes","gender")])      
    # and select the most popular of all times:
    matrix_post <- tab_column_post_by[idx,] # matrix for the postulations
    
    # entrances
    only_ent <- dat_year[dat_year$ingreso == 1, ]
    # now, tables by:
    tab_column_ent_by <- table(only_ent[, c("especialidad_subespecialidad_ingresantes","gender")])
    # and select the most popular:
    matrix_ent <- tab_column_ent_by[idx,] # matrix for the entrances
    
    mt_post[ , c( which(years == i)*2-1, which(years == i)*2 ) ] <- matrix_post
    mt_ent[ , c( which(years == i)*2-1, which(years == i)*2 ) ] <- matrix_ent
  }
  ls <- list(mt_post, mt_ent)
  names(ls) <- c("Postulations", "Enters")
  ls
}

###

give_sep_bars_enters <- function(column, by = "gender", top = 20, years = 2016:2020, adj = 12, col = c("goldenrod2", "deepskyblue3")){
  # index of most popular of all times, of ENTERS
  tab_column <- table(dat[dat$ingreso == 1, column])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    
    # work all with entrances
    tab_column_ent_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    # idx may contain things not present in all years:
    not_missed <- idx %in% rownames(tab_column_ent_by)
    new_idx <- idx[ not_missed ]
    
    matrix_ent <- tab_column_ent_by[new_idx,] # matrix for the postulations
    
    add_mt <- matrix(0, nrow = length(idx[ !not_missed ]), ncol = 2)
    row.names(add_mt) <- idx[ !not_missed ]
    matrix_ent <- rbind(matrix_ent, add_mt)
    matrix_ent[is.na(matrix_ent)] <- 0 # and NAs as 0s
    
    # plot, by using the matrices
    par(mar=c(12,3,2,0))
    bb <- barplot(t(matrix_ent), beside = T, las = 2, cex.names = 0.6,
                  col = col,
                  main = i)
    text(x = bb, y = t(matrix_ent)+adj, labels = t(matrix_ent), cex=.55, font = 2)
    legend("topright", legend = colnames(matrix_ent), col = col, pch = 16) }
}

give_sep_bars_enters_one_year <- function(column, by = "gender", top = 20, year = 2020, adj = 12, col = c("goldenrod2", "deepskyblue3")){
    
    dat_year <- dat[dat$year == year, ]
  
    # index of most popular of ONE YEAR, of ENTERS
    tab_column <- table(dat_year[dat_year$ingreso == 1, column])
    idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
    
    # work all with entrances
    tab_column_ent_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    # idx may contain things not present in all years:
    not_missed <- idx %in% rownames(tab_column_ent_by)
    new_idx <- idx[ not_missed ]
    
    matrix_ent <- tab_column_ent_by[new_idx,] # matrix for the postulations
    
    add_mt <- matrix(0, nrow = length(idx[ !not_missed ]), ncol = 2)
    row.names(add_mt) <- idx[ !not_missed ]
    matrix_ent <- rbind(matrix_ent, add_mt)
    matrix_ent[is.na(matrix_ent)] <- 0 # and NAs as 0s
    
    # plot, by using the matrices
    par(mar=c(12,3,2,0))
    bb <- barplot(t(matrix_ent), beside = T, las = 2, cex.names = 0.6,
                  col = col,
                  main = year)
    text(x = bb, y = t(matrix_ent)+adj, labels = t(matrix_ent), cex=.55, font = 2)
    legend("topright", legend = colnames(matrix_ent), col = col, pch = 16)
}

give_sep_bars_enters_table <- function(column, by = "gender", top = 20, years = 2016:2020){
  # index of most popular of all times, of ENTERS
  tab_column <- table(dat[dat$ingreso == 1, column])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  ls_mat <- vector("list", length(years))
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    
    # work all with entrances
    tab_column_ent_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    # idx may contain things not present in all years:
    not_missed <- idx %in% rownames(tab_column_ent_by)
    new_idx <- idx[ not_missed ]
    
    matrix_ent <- tab_column_ent_by[new_idx,] # matrix for the postulations
    
    add_mt <- matrix(0, nrow = length(idx[ !not_missed ]), ncol = 2)
    row.names(add_mt) <- idx[ !not_missed ]
    matrix_ent <- rbind(matrix_ent, add_mt)
    matrix_ent[is.na(matrix_ent)] <- 0 # and NAs as 0s
    ls_mat[[which(years == i)]] <- matrix_ent
  }
  
  ls_mat
}

sep_bars_enters_sum_years <- function(column, by = "gender", top = 20, years = 2016:2020, adj = 12, col = c("goldenrod2", "deepskyblue3")){
  # index of most popular of all times, of ENTERS
  tab_column <- table(dat[dat$ingreso == 1, column])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  ls_mat <- vector("list", length(years))
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    # work all with entrances
    tab_column_ent_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    # idx may contain things not present in all years:
    not_missed <- idx %in% rownames(tab_column_ent_by)
    new_idx <- idx[ not_missed ]
    
    matrix_ent <- tab_column_ent_by[new_idx,] # matrix for the postulations
    
    add_mt <- matrix(0, nrow = length(idx[ !not_missed ]), ncol = 2)
    row.names(add_mt) <- idx[ !not_missed ]
    matrix_ent <- rbind(matrix_ent, add_mt)
    matrix_ent[is.na(matrix_ent)] <- 0 # and NAs as 0s
    
    ls_mat[[which(years == i)]] <- matrix_ent
  }
  
  sum_matrix <- t( Reduce('+', ls_mat) )
  
  # plot, by using the matrices
  par(mar=c(12,3,2,0))
  bb <- barplot(sum_matrix, beside = T, las = 2, cex.names = 0.6,
                col = col)
  text(x = bb, y = sum_matrix+adj, labels = sum_matrix, cex=.55, font = 2)
  legend("topright", legend = rownames(sum_matrix), col = col, pch = 16)
}

sep_bars_enters_sum_years_table <- function(column, by = "gender", top = 20, years = 2016:2020){
  # index of most popular of all times, of ENTERS
  tab_column <- table(dat[dat$ingreso == 1, column])
  idx <- names( (sort( -tab_column ))[1:top] ) # index given by names
  
  ls_mat <- vector("list", length(years))
  for(i in years){
    dat_year <- dat[dat$year == i, ]
    # work all with entrances
    tab_column_ent_by <- table(dat_year[, c(column,by)])      
    # and select the most popular of all times:
    # idx may contain things not present in all years:
    not_missed <- idx %in% rownames(tab_column_ent_by)
    new_idx <- idx[ not_missed ]
    
    matrix_ent <- tab_column_ent_by[new_idx,] # matrix for the postulations
    
    add_mt <- matrix(0, nrow = length(idx[ !not_missed ]), ncol = 2)
    row.names(add_mt) <- idx[ !not_missed ]
    matrix_ent <- rbind(matrix_ent, add_mt)
    matrix_ent[is.na(matrix_ent)] <- 0 # and NAs as 0s
    
    ls_mat[[which(years == i)]] <- matrix_ent
  }
  
  sum_matrix <- t( Reduce('+', ls_mat) )
  t(sum_matrix)
}


# boxplots for grades:

boxplot_by <- function(num_column = "nota_final", by = "gender", year = 2019:2020, ylim = c(0,100)){
  # I do not want to work with "num_column" of an empty "by": (NAs excluded)
  NA_rows <- is.na( dat[ , by] )
  new_dat <- dat[ !NA_rows, ]
  # set the year:
  for(y in year) {
    dat_year <- new_dat[ new_dat$year == y, ]
    # matrix for plot:
    d <- dat_year[ , c(by, num_column)]
    d[,1] <- as.factor(d[,1])
    # plot:
    boxplot(d[,2]~d[,1], main = y, xlab = by, ylab = num_column, ylim = ylim)
  }
}
