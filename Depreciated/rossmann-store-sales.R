
# Rossmann ----------------------------------------------------------------


# Initialize Series -------------------------------------------------------

rossmann_data <- readRDS("../Data/rossmann-store-sales/combined.Rds")
rossmann_data <- future_map(rossmann_data, prepdata, target = "sales", group = "store", date = "date", fill_target = 0, fill_dates = TRUE)

target <- "sales"
group <- "store"
index <- "date"
commonality <- "store_type"

rs_train <- as_dtts(rossmann_data$train, group = group, target = target, index = index, "train")
rs_train_feats <- as_dtts(rossmann_data$train, group = group, target = target, index = index, "features")
rs_store <- rossmann_data$store
rs_test <- rossmann_data$test

# Fix NA's caused by filling dates
rs_train[is.na(rs_train)] <- 0
rs_train_feats[is.na(rs_train_feats)] <- 0

# Set Key and Index
setkeyv(rs_train, group)
setindexv(rs_train, index)

# Remove raw data
rm(rossmann_data) ; gc()

# Time Series Summary -----------------------------------------------------

ts_summary(rs_train, target = target, group = group, frequency = 365.25)$plot


# Collect Time Series Features --------------------------------------------

tictoc::tic("Collect Time Series Features")
rs_feats <- ts_features(rs_train, 
                        target = target, 
                        group = group, 
                        date_col = index,
                        frequency = c(365.25), 
                        scale = F,
                        features = c("stl_features", "entropy"))
tictoc::toc()


# Plot Principal Components -----------------------------------------------

pcs <- get_pcs(rs_feats, group, scale = T)

autoplot(pcs, loadings = TRUE, loadings.label = TRUE)

# Plot Trend, Season, and Entropy -----------------------------------------

rs_feats[, .(store, seasonal_strength, trend, entropy)] %>%
  melt(id.vars = group) %>%
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~variable)


# Investigate Detailed Correlation of Explicit Features -------------------

train_w_var <- rs_train_feats[rs_train, on = c(group, index)]
train_w_var[, state_holiday := as.integer(as.factor(state_holiday))]
feat_cols <- colnames_excluded(train_w_var, exclude = c(group, index))

tictoc::tic("Feature Correlation")
cor_df <- train_w_var[, .(data = list(.SD)), group, .SDcols = feat_cols] %>%
  .[, cor := future_map(data, get_correlations, target = target)]
tictoc::toc()

cor_df[, rbindlist(cor), group] %>%
  .[, .(cor, mean_cor = mean(abs(cor), na.rm = T)), variable] %>%
  ggplot(aes(x = cor)) + geom_histogram() + geom_vline(aes(xintercept = mean_cor)) + facet_grid(~variable)


# Investigate Overall Correlation of Explicit Features --------------------

overall_cor <- get_correlations(train_w_var[, !c(group, index), with = F], target = target) %>%
  .[order(-abs(cor))]

overall_cor


# Inspect Annual Seasonality of X Groups ----------------------------------

rs_train[store %in% 1:10, .(doy = yday(date), year = year(date), sales, store)] %>%
  ggplot(aes(x = doy, y = sales, group = store)) + 
  geom_line(alpha = .5) + 
  facet_grid(year~store)


# Inspect Commonality by Single Value Decomposition -----------------------

rs_train[rs_store, on = group, c(commonality) := get(commonality)]

tictoc::tic("Single Value Decomposition")
rs_svd <- calculate_svd(rs_train, 
                        commonality = commonality,
                        group = group,
                        index = index,
                        target = target,
                        fill_target = 0,
                        na.rm = FALSE,
                        parallel = TRUE)
tictoc::toc()

rs_svd$svd_errors %>%
  ggplot(aes(x = comb, y = var_exp, group = get(commonality))) + 
  geom_line() + 
  facet_wrap(~get(commonality))
