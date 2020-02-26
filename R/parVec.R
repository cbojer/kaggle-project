# Parallized Vector Apply -------------------------------------------------

parVec <- function(cl = NULL, x, FUN, ...) {
  require(parallel, quietly = T)
  if (is.null(cl)) {
<<<<<<< HEAD
    ncl <- getOption("mc.cores", max(parallel::detectCores(), 1, na.rm=TRUE))
=======
    ncl <- getOption("mc.cores", max(parllel::detectCores(), 1, na.rm=TRUE))
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
    cl <- parallel::makeCluster(ncl)
    on.exit(parallel::stopCluster(cl))
  } else {
    ncl <- length(cl)
  }
  
  s <- lapply(parallel::splitIndices(length(x), ncl = ncl), function(idx) x[idx])
  r <- parallel::parLapply(cl, s, FUN, ...)
  do.call("c", r)
}