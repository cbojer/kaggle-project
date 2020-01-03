prepdata <- function(.data, group, target, index, fill_target = 0, fill_dates = TRUE) {
  .data <- copy(.data) %>% janitor::clean_names()
  self <- list("group" = group, "target" = target, "index" = index)
  group_idx <- group %in% names(.data)
  
  if(index %in% names(.data) && is.Date(.data[, ..index])) {
    .data[, c(index) := anytime::anydate(get(index))]
    
    if(fill_dates) {
      freq <- getmode(diff(.data[, ..index]))
      self$frequency <- freq
      if (freq < 7) interval <- "1 day"
      else if (freq == 7) interval <- "1 week"
      else if (any(freq %in% c(28, 29, 30, 31))) interval <- "1 month"
      else if (any(freq %in% c(364, 365, 366))) interval <- "1 year"
      self$interval <- interval
      indices <- .data[, .(seq(min(get(index)), max(get(index)), interval)), c(group[group_idx])]
      setnames(indices, "V1", c(index))
      .data <- .data[indices, on = c(group[group_idx], index)]
    }
  }
  
  if(any(group_idx)) {
    .data[, c(group[group_idx]) := lapply(.SD, as.factor), .SDcols = c(group[group_idx])]
  }
  
  if(target %in% names(.data)) {
    .data[is.na(get(target)), c(target) := fill_target]
  }
  
  attr(.data, "self") <- self
  
  .data
}


