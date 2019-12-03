
# Parallized Vector Apply -------------------------------------------------

parVec <- function(cl = NULL, x, FUN, ...) {
  if (is.null(cl)) {
    cl <- parallel::makeCluster(parallel::detectCores())
    on.exit(parallel::stopCluster(cl))
  }
  n <- length(cl)
  s <- lapply(parallel::splitIndices(length(x), ncl = n), function(idx) x[idx])
  r <- parallel::parLapply(cl, s, FUN, ...)
  do.call("c", r)
}

# Get Load ID -------------------------------------------------------------

get_id <- function(path) {
  s <- unlist(strsplit(path, "/"))
  s[length(s)-1]
}

# Object Size formatted ---------------------------------------------------

obj.size <- function(x, unit = "Kb") {
  if(is.atomic(x) && is.character(x)) x <- get(x)
  object.size(x) %>% format(units = unit)
}

# Sums --------------------------------------------------------------------

sumNA <- function(x) sum(is.na(x))
sumZERO <- function(x) sum(x == 0)
sumZEROPCT <- function(x) sum(x == 0) / length(x)

# Exclude Colnames --------------------------------------------------------

colnames_excluded <- function(.data, exclude) {
  colnames(.data)[!colnames(.data) %in% exclude]
}

# Quiet -------------------------------------------------------------------

quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

suppressor <- function(...) {
  suppressWarnings(suppressMessages(...))
}

# Get Correlations --------------------------------------------------------

get_correlations <- function(.data, target) {
  require(data.table, quietly = TRUE)
  correlation <- suppressWarnings(cor(.data, method = "spearman"))
  if(all(is.na(correlation))) return(NULL)
  data.table::as.data.table(correlation, keep.rownames = T)[rn != target, .(variable = rn, cor = get(target))]
}

# makeMatrixWithNAZero ----------------------------------------------------

makeMatrixWithNAZero <- function(.data) {
  if(any(is.na(.data))) .data[is.na(.data)] <- 0
  as.matrix(.data)
}

# getmode -----------------------------------------------------------------

getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# Time Series Features ----------------------------------------------------

make_ts <- function(x, frequency, target, index) {
  y <- x[, get(target)]
  if(any(y == 0)) y <- y + 0.0000000001
  suppressor(forecast::msts(y, seasonal.periods = frequency))
}

tsfeats <- function(x, ...) {
  feats <- suppressor(tsfeatures::tsfeatures(x, ...))
  idx <- grep("^seasonal_period|^nperiods", names(feats))
  as.data.table(feats[, -idx])
}

ts_features <- function(.data, target, group, index, frequency, parallel = TRUE, ...) {
  FUN <- ifelse(parallel, furrr::future_map, base::lapply)
  if(!is.data.table(.data)) setDT(.data)
  tsib <- .data[, .(data = list(.SD)), c(group)]
  tsib[, ts := lapply(data, make_ts, frequency = frequency, target = target, index = index)]
  tsib[, feats := FUN(ts, possibly(tsfeats, NA), ...)]
  tsib[, check := sapply(feats, is.logical)]
  feats <- tsib[check == FALSE, unlist(feats, recursive = F), c(group)]
  feats
}


# KHS Time Series Features ------------------------------------------------

get_khs_feats <- function(x, fill_na = NULL) {
  if(any(x == 0)) x <- x + 0.00001
  if(any(x < 0)) x[x < 0] <- 0.00001
  freq <- stats::frequency(tsfeatures:::scalets(x))
  ent <- tsfeatures::entropy(tsfeatures:::scalets(x))
  lambda <- forecast::BoxCox.lambda(x, lower = 0, upper = 1, method = "loglik")
  y <- forecast::BoxCox(x, lambda)
  stl_feats <- try(tsfeatures::stl_features(y, s.window = "periodic", robust = TRUE))
  if(any(!c("trend", "seasonal_strength", "e_acf1") %in% names(stl_feats))) {
    message("Trend, Season, or ACF1 is Missing")
    message(paste("Available Cols Are:\n", paste0(names(stl_feats), collapse = ", ")))
  }
  if(is.element("try-error", class(stl_feats))) {
    stl_feats <- c("trend" = NA_real_, "seasonal_strength" = NA_real_, "e_acf1" = NA_real_)
  }
  out <- data.table::data.table(Frequency = as.double(freq),
                                Entropy = as.double(ent),
                                Trend = as.double(stl_feats[["trend"]]),
                                Season = as.double(stl_feats[["seasonal_strength"]]),
                                ACF1 = as.double(stl_feats[["e_acf1"]]),
                                Lambda = as.double(lambda),
                                Period = as.factor(freq))
  if(!is.null(fill_na)) {
    data.table::setnafill(out, fill = fill_na, cols = c("Frequency", "Entropy", "Trend", "Season", "ACF1", "Lambda"))
  }
  out
}

khs_ts_features <- function(.data, target, group, index, frequency, parallel = TRUE, fill_na = NULL) {
  if(!is.data.table(.data)) setDT(.data)
  tsib <- .data[, .(data = list(.SD)), c(group)]
  tsib[, ts := lapply(data, make_ts, frequency = frequency, target = target, index = index)]
  if(parallel) {
    cl <- parallel::makeCluster(parallel::detectCores())
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(cl, "get_khs_feats")
    tsib[, feats := pbapply::pblapply(cl = cl, X = ts, FUN = get_khs_feats, fill_na = fill_na)]
  } else {
    tsib[, feats := lapply(ts, get_khs_feats, fill_na = fill_na)]
  }
  tsib[, rbindlist(feats)]
}

prplot <- function(feats) {
  pcs <- prcomp(select(feats, -c(Period, Frequency)), scale=TRUE)$x
  pcs %>%
    as_tibble() %>%
    bind_cols(Period=feats$Period) %>%
    ggplot(aes(x=PC1, y=PC2)) +
    geom_point(aes(col=Period))
}

# Mean Fill ---------------------------------------------------------------

mean_fill <- function(x) {
  nafill(x, fill = mean(x, na.rm = T))
}

# Time Series Characteristics ---------------------------------------------

ts_summary <- function(.data, target, group, frequency) {
  .new_data <- setDT(.data)[,
                            .(N = as.double(.N),
                              "Count Non Zero" = as.double(sum(get(target) > 0)),
                              "Pct. Non Zero" = as.double(sum(get(target) > 0)/.N),
                              "Count Seasons" = as.double(frequency/.N)),
                            c(group)] %>%
    melt(id.vars = group) 
  
  .new_data_plot <- ggplot(.new_data, aes(value)) + 
    geom_histogram() + facet_wrap(~variable, scales = "free")
  
  list("data" = .new_data,
       "plot" = .new_data_plot)
}

# Get Principal Components ------------------------------------------------

get_pcs <- function(.data, group, ...) {
  target_names <- names(.data)[!names(.data) %in% group]
  .new_data <- .data[, target_names, with = F]
  .new_data[, lapply(.SD, mean_fill)] %>%
    prcomp(...)
}
