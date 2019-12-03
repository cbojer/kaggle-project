prepdata <- function(.data, target, date, group, fill_target = 0, fill_dates = TRUE) {
  .data <- .data %>% janitor::clean_names()
  group_idx <- group %in% names(.data)
  interval <- NULL
  
  if(date %in% names(.data)) {
    setnames(.data, date, "date")
    .data[, c(date) := anytime::anydate(date)]
    
    if(fill_dates) {
      freq <- getmode(diff(.data$date))
      if (freq < 7) interval <- "1 day"
      else if (freq == 7) interval <- "1 week"
      else if (any(freq %in% c(28, 29, 30, 31))) interval <- "1 month"
      else if (any(freq %in% c(364, 365, 366))) interval <- "1 year"
      
      dates <- .data[, .(date = seq(min(date), max(date), interval)), c(group[group_idx])]
      .data <- .data[dates, on = c(group[group_idx], "date")]
    }
    
    setnames(.data, "date", c(date))
  }
  
  if(any(group_idx)) {
    .data[, c(group[group_idx]) := lapply(.SD, as.factor), .SDcols = c(group[group_idx])]
  }
  
  if(target %in% names(.data)) {
    .data[is.na(get(target)), c(target) := fill_target]
  }
  
  .data
}