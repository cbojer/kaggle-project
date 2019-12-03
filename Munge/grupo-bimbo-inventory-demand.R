# Bimbo

bimbo_data <- readRDS(".../Data/grupo-bimbo-inventory-demand/combined.Rds")

bi_raw_train <- bimbo_data$train %>% janitor::clean_names()
group = c("cliente_id", "producto_id")
index = "semana"
target = "demanda_uni_equil"
dt_train <- as_dtts(bi_raw_train, group, index, target)
rm(bimbo_data, bi_raw_train)
gc()

setkeyv(dt_train, group)
setindexv(dt_train, index)


prepdata <- function(.data, target, date, group, fill_target = 0, fill_dates = TRUE) {
  .data <- .data %>% janitor::clean_names()
  group_idx <- group %in% names(.data)

  if (date %in% names(.data)) {
    setnames(.data, date, "date")
    .data[, c(date) := anytime::anydate(date)]

    if (fill_dates) {
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

  if (any(group_idx)) {
    .data[, c(group[group_idx]) := lapply(.SD, as.factor), .SDcols = c(group[group_idx])]
  }

  if (target %in% names(.data)) {
    .data[is.na(get(target)), c(target) := fill_target]
  }
  .data
}





help(swi)
rm(bimbo_data, data, feature_cols, featureDT, targetDT, .data, t, client);
gc()

.data <- bi_train

setindex(train, cliente_id, producto_id)
setkeyv(bi_train, group)
train[, .(.N), .(cliente_id, producto_id)]
train <- as_dtts(bi_train, group, index, target)
client <- bi_train[, unique(cliente_id)]
t <- bi_train[cliente_id %in% client[1:10000]]
t <- bi_train[, .(list(demanda_uni_equil)), group, verbose = T]
t
tt <- bi_train[cliente_id %in% client[1:100], .(data)]
object.size(bi_nest)
object.size(t)
setkeyv(targetDT, group)
DTnest <- targetDT[, .(data = list(.SD)), c(self$group)]
targetDT
featuresDT <- targetDT[, (target) := NULL]
plan(sequential)
self;
writeLines("------------------");
b

object.size(train) %>% format(units = "Mb")
object.size(bi_train) %>% format(units = "Mb")
nn <- train[, .(data = list(.SD))]



df <- data.table(a = 1:5)
self <- list()
self$group <- group
self$target <- target
self$index <- index
self$add_self <- function() attr(df, "self") <<- self
self$add_self()
attributes(df)


