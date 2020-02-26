
<<<<<<< HEAD
# Get Information from ZIP file -------------------------------------------

get_zip_info <- function(path) {
  file_info <- system(paste0("unzip -l ",  path), intern = T)
  n <- length(file_info)
  read.table(text = file_info[-c(1,3, n-1, n)], header = TRUE, stringsAsFactors = FALSE)
}

=======
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
# Get Load ID -------------------------------------------------------------

get_id <- function(path) {
  s <- unlist(strsplit(path, "/"))
  s[length(s)-1]
}

# Trim Leading & Trailing Zero's ------------------------------------------

<<<<<<< HEAD
trimlt <- function(.data, target = NULL, trim = c("both", "leading", "trailing")) {
  if(inherits(.data, "data.frame") && !is.null(target)) {
    x <- as.data.frame(.data)[, eval(substitute(target))]
    n <- nrow(.data)
    non_zero <- which(x > 0)
    switch (match.arg(trim),
            both = .data[non_zero[1]:non_zero[length(non_zero)], ],
            leading = .data[non_zero[1]:n, ],
            trailing = .data[1:non_zero[length(non_zero)], ])
    return(.data[non_zero[1]:non_zero[length(non_zero)], ])
  } else if(is.vector(.data) && is.numeric(.data)) {
    x <- .data
    n <- length(.data)
    non_zero <- which(x > 0)
    switch (match.arg(trim),
            both = x[non_zero[1]:non_zero[length(non_zero)]],
            leading = x[non_zero[1]:n],
            trailing = x[1:non_zero[length(non_zero)]])
=======
trimlt <- function(.data, target = NULL) {
  if(inherits(.data, "data.frame") && !is.null(target)) {
    x <- as.data.frame(.data)[, eval(substitute(target))]
    non_zero <- which(x > 0)
    return(.data[non_zero[1]:non_zero[length(non_zero)], ])
  } else if(is.vector(.data) && is.numeric(.data)) {
    x <- .data
    non_zero <- which(x > 0)
    return(x[non_zero[1]:non_zero[length(non_zero)]])
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
  }
}

# Get sequence from min to max of x ---------------------------------------

minmax_sequence <- function(x, interval = 1) {
  seq(min(x), max(x), interval)
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

suppress_wm <- function(...) {
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

# KHS Time Series Features ------------------------------------------------

calculate_khs_feats <- function(x, use_dw_frequency = TRUE) {
  # Required Packages
  require(forecast, quietly = TRUE)
  require(feasts, quietly = TRUE)
  require(data.table, quietly = TRUE)
  
  # Assert that 'x' is ts
  if(!is.ts(x)) {
    stop("x must be a time series, class(x) == 'ts'")
  }
  
  # Calculate length of time series
  N <- length(x)
  
  # Adjust time series to correct frequency
  if(isTRUE(use_dw_frequency)) {
    freq <- frequency(x)
    timeseries <- x
  } else {
    freq <- frequency(x)
    if(frequency(x) == 7 | frequency(x) == 52) {
      timeseries <- ts(x, frequency = 1)
      freq <- 1
    } else {
      timeseries <- x
    }
  }
  
  # Calculate Features
<<<<<<< HEAD
  # Zero values result in error, hence add '0.000001'
  lambda <- try(forecast::BoxCox.lambda(timeseries + 0.000001, lower = 0, upper = 1, method = "loglik")) 
=======
  lambda <- try(forecast::BoxCox.lambda(timeseries + 0.000001, lower = 0, upper = 1, method = "loglik")) # Zero values result in error, hence add '0.000001'
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
  stl_feats <- try(feasts::feat_stl(timeseries, .period = freq))
  acf_feats <- try(feasts::feat_acf(timeseries, .period = freq))
  spectral_feats <- try(feasts::feat_spectral(timeseries))
  
<<<<<<< HEAD
=======
  
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
  # Pull Features of Interest
  if(!is.element("try-error", class(lambda))) {
    lambda <- lambda
  } else {
    lambda <- NA_real_
  }
  if(!is.element("try-error", class(stl_feats))) {
    trend <- stl_feats[grep("^trend_strength", names(stl_feats))]
    season <- stl_feats[grep("^seasonal_strength", names(stl_feats))]
  } else {
    trend <- NA_real_
    season <- NA_real_
  }
  if(!is.element("try-error", class(acf_feats))) {
    acf1 <- acf_feats[grep("^acf1", names(acf_feats))][1]
  } else {
    acf1 <- NA_real_
  }
  if(!is.element("try-error", class(spectral_feats))) {
    entropy <- spectral_feats[grep("^spectral_entropy", names(spectral_feats))]
  } else {
    entropy <- NA_real_
  }
  
  # If frequency == 1, season estimation will fail, hence we set this to 0.
  if(freq == 1) {
    season <- 0
  }
  
  # Assemble Result
  data.table::data.table(
    "N" = N,
    "Frequency" = freq,
    "Entropy" = entropy,
    "Trend" = trend,
    "Season" = season,
    "ACF1" = acf1,
    "Lambda" = lambda,
    "Period" = as.factor(freq)
  )
  
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


# Get SBC Classification --------------------------------------------------

sbc_classifier <- function(x) {
  N <- length(x)
  tmp <- x[!is.na(x)]
  nzd <- which(tmp != 0)
  z <- tmp[nzd]
  ADI = mean(c(nzd[1], diff(nzd)))
  CV2 = (sd(z) / mean(z))^2
  
  if(anyNA(c(ADI, CV2))) {
    return(list("N" = N, "ADI" = NA_real_, "CV2" = NA_real_, "Class" = NA_character_))
  } else if(ADI <= 1.32 && CV2 <= 0.49) {
    return(list("N" = N,"ADI" = ADI, "CV2" = CV2, "Class" = "Smooth"))
  } else if(ADI > 1.32 && CV2 <= 0.49) {
    return(list("N" = N,"ADI" = ADI, "CV2" = CV2, "Class" = "Intermittent"))
  } else if(ADI <= 1.32 && CV2 > 0.49) {
    return(list("N" = N,"ADI" = ADI, "CV2" = CV2, "Class" = "Erratic"))
  } else {
    return(list("N" = N,"ADI" = ADI, "CV2" = CV2, "Class" = "Lumpy"))
  }
}

# Get Principal Components ------------------------------------------------

get_pcs <- function(.data, group, ...) {
  target_names <- names(.data)[!names(.data) %in% group]
  .new_data <- .data[, target_names, with = F]
  .new_data[, lapply(.SD, mean_fill)] %>%
    prcomp(...)
}

prplot <- function(feats) {
  pcs <- prcomp(select(feats, -c(Period, Frequency)), scale=TRUE)$x
  pcs %>%
    as_tibble() %>%
    bind_cols(Period=feats$Period) %>%
    ggplot(aes(x=PC1, y=PC2)) +
    geom_point(aes(col=Period))
}
