
# Common Analysis Data Structure ------------------------------------------

as_dtts <- function(.data, group, index, target, type = c("target", "features", "both")) {
  setDT(.data)
  self <- list("group" = group, "target" = target, "index" = index)
  attr(.data, "self") <- self
  target_cols <- c(group, index, target)
  feature_cols <- colnames(.data)[!colnames(.data) %in% target_cols]
  targetDT <- .data[, target_cols, with = F]
  featureDT <- .data[, .SD, .SDcols = !target]
  type <- match.arg(type)
  switch(type,
         target = return(targetDT),
         features = return(featureDT),
         both = return(list("target" = targetDT, "feature" = featureDT)))
}
