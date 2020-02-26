# Capture and Prepare training data ---------------------------------------

if(!dir.exists(temporary_kaggle_training_save_path)) {
  dir.create(temporary_kaggle_training_save_path)
} 

if(!length(list.files(temporary_kaggle_training_save_path)) != nrow(data_info) &&
   !all(sub(".rds", "", list.files(temporary_kaggle_training_save_path), ignore.case = T) %in% data_info$id)) {
  
  # favorita-grocery-sales-forecasting
  idx <- 1
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    original_train <- raw_data$train %>% janitor::clean_names()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    rm(raw_data, original_train) ; gc()
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  
  # recruit-restaurant-visitor-forecasting
  idx <- 2
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    original_train <- raw_data$air_visit_data %>% janitor::clean_names()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    rm(raw_data, original_train) ; gc()
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  # rossmann-store-sales
  idx <- 3
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    original_train <- raw_data$train %>% janitor::clean_names()
    original_test <- raw_data$test %>% janitor::clean_names()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    rm(raw_data, original_train) ; gc()
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  # walmart-recruiting-store-sales-forecasting
  idx <- 4
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    original_train <- raw_data$train %>% janitor::clean_names()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    rm(raw_data, original_train) ; gc()
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  # walmart-storm-weather-competition
  idx <- 5
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    original_train <- raw_data$train %>% janitor::clean_names()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    rm(raw_data, original_train) ; gc()
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  # web-traffic-time-series-forecasting
  idx <- 6
  path <- data_info$data_dir[[idx]]
  save_path <- paste0(temporary_kaggle_training_save_path, get_id(path), ".RDS")
  prior_train_capture_check <- file.exists(save_path)
  if(!prior_train_capture_check) {
    raw_data <- readRDS(path)
    train1 <- melt(raw_data$train_1, id.vars = "Page", variable.name = "date", variable.factor = F, na.rm = TRUE, value.name = "views", verbose = T)
    train2 <- melt(raw_data$train_2, id.vars = "Page", variable.name = "date", variable.factor = F, na.rm = TRUE, value.name = "views", verbose = T)
    original_train <- rbind(train1, train2) %>% janitor::clean_names()
    rm(train1, train2, raw_data) ; gc()
    train <- as_dtts(original_train, group = data_info$group[[idx]], target = data_info$target[[idx]], index = data_info$index[[idx]], "train")
    rm(original_train) ; gc()
    train[, views := as.integer(views)]
    zero_check <- train[, .(lapply(.SD, sum)), by = eval(data_info$group[[idx]]), .SDcols = data_info$target[[idx]]][V1 != 0, -"V1"]
    train <- train[zero_check, on = data_info$group[[idx]]]
    train[, page := as.factor(page)]
    train[, c(attr(train, "self")$index) := parVec(x = get(attr(train, "self")$index), FUN = anytime::anydate)]
    saveRDS(train, save_path)
    rm(idx, path, save_path, train, zero_check) ; gc()
  }
  
  ## CLEAN UP
  rm(idx, path, save_path, prior_train_capture_check)
  
}

# Fill Missing Index Sequence (If Any) ------------------------------------

if(!dir.exists(processed_kaggle_training_save_path)) {
  dir.create(processed_kaggle_training_save_path)
}

if(length(list.files(processed_kaggle_training_save_path)) != nrow(data_info)) {
  
  for(idx in 1:nrow(data_info)) {
    print(sprintf("Filling Index in %s", data_info$id[[idx]]))
    
    # Pull Information
    path <- data_info$train_dir[[idx]]
    target <- data_info$target[[idx]]
    group <- data_info$group[[idx]]
    index <- data_info$index[[idx]]
    interval <- ifelse(grepl("week", target), "1 week", "1 day")
    
    # Load & prepare data
    train <- readRDS(path)
    
    # Fill Index
    index_full <- train[, .(seq(min(get(index)), max(get(index)), interval)), by = eval(group)]
    setnames(index_full, "V1", eval(index))
    train <- train[index_full, on = c(eval(group), eval(index))]
    train[is.na(get(target)), eval(target) := 0]
    
    # Save Filled DT
    save_path <- sub(tail(unlist(strsplit(temporary_kaggle_training_save_path, "/")), 1),
                     tail(unlist(strsplit(processed_kaggle_training_save_path, "/")), 1), 
                     path)
    
    saveRDS(train, save_path)
    
    rm(train, index_full, save_path) ; gc()
  }
  
  ## CLEAN UP
  rm(path, target, group, index, interval)
}