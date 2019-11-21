truncated_svd <- function(.matrix, original_data = NULL) {
  matrix_svd <- rbindlist(
    lapply(1:ncol(.matrix), function(i) {
      z <- suppressWarnings(suppressMessages(RSpectra::svds(.matrix, i)))
      s <- diag(z$d[1:i], i, i)
      rec <- as.data.table(tcrossprod(z$u %*% s, z$v))
      rec[, comp := as.double(i)][]
    })
  )
  
  if(!is.null(original_data)) {
    mss_cols <- original_data[, !colnames(original_data) %in% colnames(.matrix), with = FALSE]
    matrix_svd <- cbind(matrix_svd, mss_cols)
  }
  
  matrix_svd
}

calculate_svd_errors <- function(train, test) {
  test <- melt(test, variable.name = "store", measure.vars = patterns("^V"))
  setDT(test)[, store := as.factor(substring(store, 2))][]
  
  combine_names <- names(train)[names(train) %in% names(test)]
  test[train, on = combine_names, weekly_sales := weekly_sales]
  test[is.na(weekly_sales), weekly_sales := 0]
  test[, error := weekly_sales - value]
  test[, 
       .(sd_error = sd(error),
         sd_sales = sd(weekly_sales),
         pct_error = sd(error)/sd(weekly_sales),
         var_exp = 1 - sd(error)/sd(weekly_sales)),
       .(comp)]
}