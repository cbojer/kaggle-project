
# Load Packages -----------------------------------------------------------

library(data.table)
library(qs)
library(pbapply)
library(magrittr)
library(parallel)
library(forecast)
library(feasts)

# Enable Progress Bar -----------------------------------------------------

pboptions(type = "timer")

# Save variables from Global Environment ----------------------------------

variables_to_keep <- ls()

# Extract KHS Features from Kaggle Data -----------------------------------

for(idx in 1:nrow(data_info)) {
  # Pull explanatory information from data_info
  id <- data_info$id[[idx]]
  path <- processed_data_paths[grep(id, processed_data_paths)]
  train_data <- data_info$train_data[[idx]]
  test_data <- data_info$test_data[[idx]]
  group <- data_info$group[[idx]]
  index <- data_info$index[[idx]]
  target <- data_info$target[[idx]]
  ts_frequency <- data_info$ts_frequency[[idx]]
  
  # Establish save paths
  if(adjusted_weekly_daily_frequency == TRUE) {
    # Check directory existence
    if(!dir.exists(paste0(khs_feat_save_path, "adjusted/"))) {
      dir.create(paste0(khs_feat_save_path, "adjusted/"), recursive = TRUE)
    }
    # Define save path
    save_path <- paste0(khs_feat_save_path, "adjusted/", id, ".qs")
  } else {
    # Check directory existence
    if(!dir.exists(paste0(khs_feat_save_path, "original/"))) {
      dir.create(paste0(khs_feat_save_path, "original/"), recursive = TRUE)
    }
    # Define save path
    save_path <- paste0(khs_feat_save_path, "original/", id, ".qs")
  }
  
  
  # Initiate Retry's
  retry_result <- vector("list", khs_number_of_retry)
  
  # Load data
  writeLines(sprintf("\nLoad data from %s", path))  
  train <- qread(path, nthreads = getDTthreads())
  
  # Set key and ensure ordering
  setkeyv(train, c(group, index))
  
  
  # Extract KHS features
  writeLines(sprintf("\nExtracting KHS features from %s", id))
  tictoc::tic()
  cl <- parallel::makeCluster(parallel::detectCores() - 1L)
  parallel::clusterExport(cl, "calculate_khs_feats")
  khs_feats <- train[, 
                     j = .(data = list(ts(data = .SD[, eval(str2lang(target))],
                                          frequency = ts_frequency))),
                     by = c(group)] %>%
    .[, rbindlist(pbapply::pblapply(data, calculate_khs_feats, use_dw_frequency = adjusted_weekly_daily_frequency, cl = cl))]
  parallel::stopCluster(cl)
  tictoc::toc()
  
  # Add Group ID to Features and Training Data
  train[, grp := .GRP, c(group)]
  khs_feats[, grp := .I]
  
  # Filter training data based on complete and incomplete features
  complete_feats <- khs_feats[complete.cases(khs_feats), -c("grp")]
  incomplete_feats <- khs_feats[!complete.cases(khs_feats), .(grp)]
  
  if(nrow(incomplete_feats) != 0) {
    cl <- parallel::makeCluster(parallel::detectCores() - 1L)
    # Subset training data by incomplete feature groups
    retry_data <- train[incomplete_feats, on = "grp"]
    
    for(retry_i in 1:khs_number_of_retry) {
      writeLines(sprintf("\n%d groups failed. Running try %d of %d", nrow(retry_data), retry_i, khs_number_of_retry))
      
      # Establish retry save paths
      if(adjusted_weekly_daily_frequency == TRUE) {
        # Check directory existence
        if(!dir.exists(paste0(khs_retry_save_path, "adjusted/"))) {
          dir.create(paste0(khs_retry_save_path, "adjusted/"))
        }
        # Define save path
        retry_save_path <- paste0(khs_retry_save_path, "adjusted/", id, ".qs")
      } else {
        # Check directory existence
        if(!dir.exists(paste0(khs_retry_save_path, "original/"))) {
          dir.create(paste0(khs_retry_save_path, "original/"))
        }
        # Define save path
        retry_save_path <- paste0(khs_retry_save_path, "original/", id, ".qs")
      }
      
      # Save Retry Data to Cache
      qsave(retry_data, paste0(retry_save_path, id, "_retry", retry_i, ".qs"), nthreads = getDTthreads())
      
      # Early stopping...
      if(nrow(retry_data) == 0) {
        writeLines(sprintf("\n\nStopped early on iteration %d", retry_i))
        break()
      }
      
      # Save number of rows in retry data for print
      retry_data_nrow <- nrow(retry_data)
      
      # Retry KHS features sith frequency 1
      retry <- retry_data[, 
                          j = .(data = list(ts(data = .SD[, eval(str2lang(target))],
                                               frequency = 1))),
                          by = c(group)] %>%
        .[, rbindlist(pbapply::pblapply(data, calculate_khs_feats, use_dw_frequency = adjusted_weekly_daily_frequency, cl = cl))]
      
      # Establish connection between retry and retry_data
      retry[, grp := .I]
      retry_data[, grp := .GRP,  c(group)]
      
      # Filter Retry data that is still incomplete
      complete_retry <- retry[complete.cases(retry), !c("grp")]
      incomplete_retry <- retry[!complete.cases(retry)]
      
      # Subset retry_data and save result
      retry_data <- retry_data[incomplete_retry[, .(grp)], on = "grp"]
      retry_result[[retry_i]] <- complete_retry
      
      writeLines(sprintf("\tReduced retry data from %d rows to %d rows", retry_data_nrow, nrow(retry_data)))
      
      # If there is still incomplete features in last retry attach these to result
      if(retry_i == khs_number_of_retry && nrow(retry_data) != 0) {
        retry_result[[retry_i]] <- incomplete_retry[, -c("grp")]
      }
      
    }
    
    retry_result <- rbindlist(retry_result)
    complete_feats <- rbind(complete_feats, retry_result)
    
    parallel::stopCluster(cl)
  } 
  
  # Save result
  writeLines(sprintf("\nSaving Result from %s", id))
  qsave(cbind(id = id, complete_feats), save_path, nthreads = getDTthreads())
  
  # Cleanup
  rm(train, khs_feats) ; gc()
}

# Extract KHS Features from M Competitions --------------------------------

# Load data
M3 <- qread(processed_data_paths[grep("M3DT", processed_data_paths)], nthreads = getDTthreads())
M4 <- qread(processed_data_paths[grep("M4DT", processed_data_paths)], nthreads = getDTthreads())

# Extract KHS Features
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, "calculate_khs_feats")

writeLines(sprintf("\nExtracting KHS features from M3"))
tictoc::tic()
M3_feats <- M3[, .(id = "M3", rbindlist(pbapply::pblapply(train, calculate_khs_feats, use_dw_frequency = adjusted_weekly_daily_frequency, cl = cl)))]
tictoc::toc()

tictoc::tic()
writeLines(sprintf("\nExtracting KHS features from M4"))
M4_feats <- M4[, .(id = "M4", rbindlist(pbapply::pblapply(train, calculate_khs_feats, use_dw_frequency = adjusted_weekly_daily_frequency, cl = cl)))]
tictoc::toc()

stopCluster(cl)

if(adjusted_weekly_daily_frequency == TRUE) {
  save_path <- paste0(khs_feat_save_path, "adjusted/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
} else {
  save_path <- paste0(khs_feat_save_path, "original/")
  if(!dir.exists(save_path)) {
    dir.create(save_path)
  }
}

# Save results
qsave(M3_feats, paste0(save_path, "M3.qs"), nthreads = getDTthreads())
qsave(M4_feats, paste0(save_path, "M4.qs"), nthreads = getDTthreads())

# Remove variables created in script --------------------------------------

rm(list = ls()[!ls() %in% variables_to_keep]) ; invisible(gc())
