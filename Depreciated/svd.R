truncated_svd <- function(.matrix, parallel = TRUE, ...) {
  # Which looping function should be used?
  FUN <- ifelse(parallel, future_map, lapply)
  
  # Calculate Full SVD
  n <- ncol(.matrix)
  z <- svd(.matrix, n, n)
  
  # Calculate Truncated SVD
  svdDT <- FUN(1:n, function(i) {
    s <- diag(z$d[1:i], i, i)
    rec <- as.data.table(tcrossprod(z$u[, 1:i] %*% s, z$v[, 1:i]))
    rec[, comb := i]
  })
  
  # Return Truncated SVD
  svdDT <- rbindlist(svdDT)
  
  if(!is.null(attr(.matrix, "index"))) svdDT <- cbind(attr(.matrix, "index"), svdDT)
  
  svdDT
}

calculate_svd <- function(.data, commonality, group, index, target, fill_target = 0, na.rm = T, parallel = TRUE, ...) {
  .new_data <- .data[, .(data = list(.SD)), c(commonality)]
  .new_data[, mat := lapply(data, cast_matrix, group = group, index = index, target = target, fill_target = fill_target)]
  
  if (parallel) {
    if(nrow(.new_data) < length(unique(.data[, get(group)]))) {
      innerp <- TRUE
      FUN <- lapply
    } else {
      innerp <- FALSE
      FUN <- future_map
    }
  } else {
    innerp <- FALSE
    FUN <- lapply
  }
  
  .new_data[, svd := FUN(mat, possibly(truncated_svd, NA), parallel = innerp)]
  
  if(na.rm) {
    .new_data[, check := sapply(svd, is.data.table)]
    .new_data <- .new_data[check != FALSE, ]
    .new_data <- .new_data[, check := NULL]
  }
  
  .new_data[, ex_var := map2(data, svd, calculate_svd_errors, group = c(group), index = c(index), target = c(target))]
  svd_result <- .new_data[, rbindlist(ex_var), c(commonality)]
  
  list("data" = .new_data,
       "svd_errors" = svd_result)
}

calculate_svd_errors <- function(.data, .svd, group, index, target) {
  # Index SVD by combinations and pull all unique combinations
  setkey(.svd, comb)
  combinations <- unique(.svd$comb)
  
  # Index Original data by group and index
  setindexv(.data, c(group, index))
  
  # Calculate Errors
  errors <- lapply(combinations, function(i) {
    # Melt SVD by comb
    errors <- melt(.svd[comb == i], variable.name = c(group), measure.vars = patterns("^V"))
    # Fix Names
    setDT(errors)[, c(group) := as.factor(substring(get(group), 2))][]
    # Index Errors by group and index and Join Original Data
    setindexv(errors, c(group, index))
    errors[.data, on = c(group, index), c(target) := get(target)]
    errors[is.na(get(target)), c(target) := 0]
    # Calculate and Summarize Errors
    errors[, error := get(target) - value]
    errors[, 
           .(sd_error = sd(error),
             sd_sales = sd(get(target)),
             pct_error = sd(error)/sd(get(target)),
             var_exp = 1 - sd(error)/sd(get(target))),
           .(comb)]
  })
  
  # Row bind Result
  rbindlist(errors)
}

cast_matrix <- function(.data, group, index, target, fill_target = 0) {
  spread_formula <- as.formula(paste(c(index, group), collapse = "~"))
  .new_data <- dcast(.data, spread_formula, value.var = target, fill = fill_target)
  .matrix <- as.matrix(.new_data[, -1])
  attr(.matrix, "index") <- .new_data[, 1]
  .matrix
}