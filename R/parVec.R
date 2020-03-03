# Parallized Vector Apply -------------------------------------------------

parVec <- function(cl = NULL, x, FUN, ...) {
  require(parallel, quietly = T)
  if (is.null(cl)) {
    ncl <- getOption("mc.cores", max(parallel::detectCores(), 1, na.rm=TRUE))
    cl <- parallel::makeCluster(ncl)
    on.exit(parallel::stopCluster(cl))
  } else {
    ncl <- length(cl)
  }
  
  s <- lapply(parallel::splitIndices(length(x), ncl = ncl), function(idx) x[idx])
  r <- parallel::parLapply(cl, s, FUN, ...)
  do.call("c", r)
}