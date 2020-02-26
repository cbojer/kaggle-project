# Walmart

walmart_data <- readRDS("../Data/walmart-recruiting-store-sales-forecasting/combined.Rds")

walmart_data <- future_map(walmart_data, prepdata, target = "weekly_sales", date = "date", group = c("store", "dept", "type"))
wm_train <- walmart_data$train
wm_test<- walmart_data$test
feats <- walmart_data$features
stores <- walmart_data$stores

setkeyv(dt_train, group)
setindexv(dt_train, index)


## Investigate Time Series Characteristics

ts_summary(wm_train, target = "weekly_sales", group = c("store", "dept"), frequency = 365.25/7)$plot


## Investigate Time Series Features

tictoc::tic("Collect Time Series Features")
wm_feats <- ts_features(wm_train, 
                        target = "weekly_sales", 
                        group = c("store", "dept"), 
                        date_col = "date",
                        frequency = c(365.25/7, 365.25), 
                        scale = F,
                        features = c("stl_features", "entropy"),
                        parallel = TRUE)
tictoc::toc()




pcs <- get_pcs(wm_feats,
               group = c("store", "dept"),
               scale = TRUE)

autoplot(pcs, loadings = TRUE, loadings.label = TRUE)



wm_feats[, .(store, dept, seasonal_strength, trend, entropy)] %>%
  melt(id.vars = c("store", "dept")) %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~variable)


## Investigate Additional Features

# Stores
unique(wm_train[, .(store, dept)]) %>%
  .[stores, on = "store"] %>%
  .[, .(n_distinct = uniqueN(store), .N), .(dept, type)]



# Additional Features
train_w_var <- feats[wm_train[, !c("is_holiday")], on = c("store", "date")]
train_w_var[is.na(train_w_var)] <- 0
feat_cols <- names(train_w_var[, !c("store", "dept", "date")])

tictoc::tic("Feature Correlation")
cor_df <- train_w_var[, .(data = list(.SD)), .(store, dept), .SDcols = feat_cols] %>%
  .[, cor := future_map(data, get_correlations, target = "weekly_sales")]
tictoc::toc()


cor_df[, unlist(cor, recursive = F), .(store, dept)] %>%
  .[, .(cor, mean_cor = mean(abs(cor), na.rm = T)), variable] %>%
  ggplot(aes(x = cor)) + geom_histogram() + geom_vline(aes(xintercept = mean_cor)) + facet_grid(~variable)



overall_cor <- get_correlations(train_w_var[, !c("store", "dept", "date")], target = "weekly_sales") %>%
  .[order(-abs(cor))]

overall_cor


## Investigate Seasonality

wm_train[dept %in% 1:10, .(doy = yday(date), year = year(date), weekly_sales, store, dept)] %>%
  ggplot(aes(x = doy, y = weekly_sales, group = store)) + 
  geom_line(alpha = .5) + 
  facet_grid(year~dept)


## Single-value Decomposition

tictoc::tic("Single Value Decomposition")
wm_svd <- calculate_svd(wm_train, nest_group = "dept", spread_group = "store", date_col = "date", target = "weekly_sales")
tictoc::toc()


wm_svd$svd_errors %>%
  ggplot(aes(x = comp, y = var_exp, group = dept)) + 
  geom_line() + 
  scale_y_continuous(limits = seq(0, 1)) + 
  facet_wrap(~dept)

