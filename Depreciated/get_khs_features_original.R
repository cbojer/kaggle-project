
# Kang Hyndman Smith Features with GAM Trend Estimation -------------------

get_khs_feats_original <- function(x, fill_na = NULL) {
  if(any(x == 0)) x <- x + 0.00001
  if(any(x < 0)) x[x < 0] <- 0.00001
  freq <- stats::frequency(tsfeatures:::scalets(x))
  ent <- tsfeatures::entropy(tsfeatures:::scalets(x))
  lambda <- forecast::BoxCox.lambda(x, lower = 0, upper = 1, method = "loglik")
  y <- forecast::BoxCox(x, lambda)
  stl_feats <- try(cust.stl_features(y, s.window = "periodic", robust = TRUE))
  name_check <- !c("trend", "seasonal_strength", "e_acf1") %in% names(stl_feats)
  if(any(name_check)) {
    missing_cols <- c("trend", "seasonal_strength", "e_acf1")[name_check]
    for(col in missing_cols) {
      stl_feats[col] <- NA
    }
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

# Reimplementation of STL Decomposition -----------------------------------

cust.stl_features <- function (x, ...) 
{
  if ("msts" %in% class(x)) {
    msts <- attributes(x)$msts
    nperiods <- length(msts)
  }
  else if ("ts" %in% class(x)) {
    msts <- frequency(x)
    nperiods <- msts > 1
    season <- 0
  }
  else {
    msts <- 1
    nperiods <- 0L
    season <- 0
  }
  if (NCOL(x) > 1) {
    stop("x must be a univariate time series.")
  }
  trend <- linearity <- curvature <- season <- spike <- peak <- trough <- acfremainder <- NA
  stlfit <- cust.mstl(x, ...)
  trend0 <- stlfit[, "Trend"]
  remainder <- stlfit[, "Remainder"]
  seasonal <- stlfit[, grep("Season", colnames(stlfit)), drop = FALSE]
  tsp(x) <- tsp(trend0)
  detrend <- x - trend0
  deseason <- forecast::seasadj(stlfit)
  fits <- x - remainder
  n <- length(x)
  varx <- var(x, na.rm = TRUE)
  vare <- var(remainder, na.rm = TRUE)
  vardetrend <- var(detrend, na.rm = TRUE)
  vardeseason <- var(deseason, na.rm = TRUE)
  nseas <- NCOL(seasonal)
  if (vardeseason/varx < 1e-10) {
    trend <- 0
  }
  else {
    trend <- max(0, min(1, 1 - vare/vardeseason))
  }
  if (nseas > 0) {
    season <- numeric(nseas)
    for (i in seq(nseas)) season[i] <- max(0, min(1, 1 - 
                                                    vare/var(remainder + seasonal[, i], na.rm = TRUE)))
    peak <- trough <- numeric(nseas)
    for (i in seq(nseas)) {
      startx <- start(x)[2L] - 1L
      pk <- (startx + which.max(seasonal[, i]))%%msts[i]
      th <- (startx + which.min(seasonal[, i]))%%msts[i]
      peak[i] <- ifelse(pk == 0, msts[i], pk)
      trough[i] <- ifelse(th == 0, msts[i], th)
    }
  }
  d <- (remainder - mean(remainder, na.rm = TRUE))^2
  varloo <- (vare * (n - 1) - d)/(n - 2)
  spike <- var(varloo, na.rm = TRUE)
  tren.coef <- coef(lm(trend0 ~ poly(seq(n), degree = 2L)))[2L:3L]
  linearity <- tren.coef[1L]
  curvature <- tren.coef[2L]
  acfremainder <- unname(acf_features(remainder))
  output <- c(nperiods = nperiods, seasonal_period = msts, 
              trend = trend, spike = spike, linearity = unname(linearity), 
              curvature = unname(curvature), e_acf1 = acfremainder[1L], 
              e_acf10 = acfremainder[2L])
  if (nseas > 0) {
    output <- c(output, seasonal_strength = season, peak = peak, 
                trough = trough)
  }
  return(output)
}


# Reimplementation of STL with GAM Trend Estimation -----------------------

cust.mstl <- function (x, lambda = NULL, iterate = 2, s.window = 13, ...) 
{
  n <- length(x)
  if ("msts" %in% class(x)) {
    msts <- attributes(x)$msts
    if (any(msts >= n/2)) {
      warning("Dropping seasonal components with fewer than two full periods.")
      msts <- msts[msts < n/2]
      x <- forecast::msts(x, seasonal.periods = msts)
    }
    msts <- sort(msts, decreasing = FALSE)
  }
  else if ("ts" %in% class(x)) {
    msts <- frequency(x)
    iterate <- 1L
  }
  else {
    x <- as.ts(x)
    msts <- 1L
  }
  if (!is.null(dim(x))) {
    if (NCOL(x) == 1L) 
      x <- x[, 1]
  }
  if (anyNA(x)) {
    x <- na.interp(x, lambda = lambda)
  }
  if (!is.null(lambda)) {
    x <- forecast::BoxCox(x, lambda = lambda)
    lambda <- attr(x, "lambda")
  }
  tt <- seq_len(n)
  if (msts[1L] > 1) {
    seas <- as.list(rep(0, length(msts)))
    deseas <- x
    if (length(s.window) == 1L) {
      s.window <- rep(s.window, length(msts))
    }
    iterate <- pmax(1L, iterate)
    for (j in seq_len(iterate)) {
      for (i in seq_along(msts)) {
        deseas <- deseas + seas[[i]]
        fit <- stl(ts(deseas, frequency = msts[i]), 
                   s.window = s.window[i], ...)
        seas[[i]] <- msts(seasonal(fit), seasonal.periods = msts)
        attributes(seas[[i]]) <- attributes(x)
        deseas <- deseas - seas[[i]]
      }
    }
    trend <- msts(trendcycle(fit), seasonal.periods = msts)
  }
  else {
    msts <- NULL
    deseas <- x
    trend <- ts(mgcv::gam(x~seq_along(x))$fitted.values)
  }
  attributes(trend) <- attributes(x)
  remainder <- deseas - trend
  output <- cbind(x, trend)
  if (!is.null(msts)) {
    for (i in seq_along(msts)) output <- cbind(output, seas[[i]])
  }
  output <- cbind(output, remainder)
  colnames(output)[1L:2L] <- c("Data", "Trend")
  if (!is.null(msts)) {
    colnames(output)[2L + seq_along(msts)] <- paste0("Seasonal", 
                                                     round(msts, 2))
  }
  colnames(output)[NCOL(output)] <- "Remainder"
  if (length(msts) > 1) {
    attr(output, "seasonal.periods") <- msts
    return(structure(output, seasonal.periods = msts, class = c("mstl", 
                                                                "mts", "msts", "ts")))
  }
  return(structure(output, class = c("mstl", "mts", "ts")))
}