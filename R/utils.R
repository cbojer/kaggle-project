quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

get_correlations <- function(.data, target) {
  correlation <- quiet(cor(.data, method = "spearman"))
  if(all(is.na(correlation))) return(NULL)
  as.data.table(correlation, keep.rownames = T)[rn != target, .(variable = rn, cor = get(target))]
}

makeMatrixWithNAZero <- function(.data) {
  if(any(is.na(.data))) .data[is.na(.data)] <- 0
  as.matrix(.data)
}

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

tsfeats <- function(x, ...) {
  feats <- suppressWarnings(tsfeatures::tsfeatures(x, ...))
  idx <- grep("^seasonal_period|^nperiods", names(feats))
  as.data.table(feats[, -idx])
}
