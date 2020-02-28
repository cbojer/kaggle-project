
# Load Packages -----------------------------------------------------------

# Check if M Competition data pacakages are installed, otherwise install them 

if(!"Mcomp" %in% installed.packages()) {
  install.packages("Mcomp")
} 

if(!"M4comp2018" %in% installed.packages()) {
  install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",
                   repos=NULL)
}

library(data.table)
library(qs)
library(janitor)
library(ggplot2)
library(gridExtra)
library(Mcomp)
library(M4comp2018)

# Capture and Prepare training data ---------------------------------------

if(!dir.exists(processed_training_save_path)) {
  dir.create(processed_training_save_path)
} 

# Initiate preprocessing loop
for(idx in 1:nrow(data_info)) {
  
  # Pull explanatory information from data_info
  id <- data_info$id[[idx]]
  path <- data_info$restructured_data_path[[idx]]
  train_data <- data_info$train_data[[idx]]
  test_data <- data_info$test_data[[idx]]
  group <- data_info$group[[idx]]
  index <- data_info$index[[idx]]
  target <- data_info$target[[idx]]
  ts_frequency <- data_info$ts_frequency[[idx]]
  interval <- ifelse(grepl("week", target), "1 week", "1 day")
  save_path <- paste0(processed_training_save_path, id, ".qs")
  
  # Check if data has already been processed
  
  writeLines(sprintf("(%d) Initiating preprocessing with: %s", idx,  id))
  
  # Read raw data
  writeLines(sprintf("\t Reading in raw data"))
  raw_data <- qread(file = path, nthreads = getDTthreads())
  
  # Pull Test & Train
  writeLines(sprintf("\t Pulling Test & Train"))
  if(length(train_data) > 1) {
    
    writeLines(sprintf("\t\t Train contains more than 1 file, merging %d files", length(train_data)))
    
    # Row-bind Train Sets
    train <- rbindlist(lapply(raw_data[train_data], function(dt) {
      
      # Locate Group, Index & Target Columns
      group_check <- grep(c(group), names(dt), ignore.case = T)
      index_check <- grep(c(index), names(dt), ignore.case = T)
      target_check <- grep(c(target), names(dt), ignore.case = T)
      
      # Gather Located Columns
      ids <- names(dt)[c(group_check, index_check, target_check)]
      
      # Check If All: Group, Index & Target are Present
      # If they are, then subset columns
      # If not, melt based on present columns
      if(length(ids) == length(c(group, index, target))) {
        tmp <- dt[, ..ids]
        setnames(tmp, ids, c(group, index, target))
      } else {
        tmp <- suppress_wm(melt(dt, id.vars = as.character(ids), variable.factor = FALSE, na.rm = TRUE))
        setnames(tmp, c(ids, "variable", "value"), c(group, index, target))
      }
      
      tmp
    }))
    
    invisible(gc())
  } else {
    train <- raw_data[[train_data]] %>% 
      janitor::clean_names()
  }
  
  # Process Test Data
  if(!is.na(test_data)) {
    # Pull Test
    test <- raw_data[[test_data]] %>% 
      janitor::clean_names()
    
    # Null/Remove columns not in Group, Index, or Target to save memory
    # Also set key and reorder data according to Group
    redundant_cols <- colnames(test)[!colnames(test) %in% c(group, index, target)]
    if(length(redundant_cols) == 0) {
      setkeyv(test, c(group, index))
    } else {
      test[, (redundant_cols) := NULL]
      setkeyv(test, c(group, index))
    }
    
    # Convert Test Index to Date
    writeLines(sprintf("\t Convert Test Index to 'Date' format"))
    test[, c(index) := as.IDate(eval(str2lang(index)), format = "%Y-%m-%d")]
    
    # Remove original Test from memory
    rm(redundant_cols)
    invisible(gc())
  }
  
  # Remove Raw Data from Memory
  rm(raw_data)
  invisible(gc())
  
  # Null/Remove columns not in Group, Index, or Target to save memory
  # Also set key and reorder data according to Group
  writeLines(sprintf("\t Pull Group, Index and Target from Train and set Key == Group"))
  redundant_cols <- colnames(train)[!colnames(train) %in% c(group, index, target)]
  if(length(redundant_cols) == 0) {
    setkeyv(train, c(group, index))
  } else {
    train[, (redundant_cols) := NULL]
    setkeyv(train, c(group, index))
  }
  
  rm(redundant_cols)
  invisible(gc())
  
  # Set all NA or Negative values to zero
  writeLines(sprintf("\t Set NA or Negative values to zero in Train"))
  train[train[[target]] < 0, c(target) := 0]
  train[is.na(train[[target]]), c(target) := 0]
  
  # Remove Group with all zero Target
  writeLines(sprintf("\t Remove Group with all zero Target"))
  non_zero_groups <- train[, sum(eval(str2lang(target))), c(group)][V1 != 0, ..group]
  train <- train[non_zero_groups, on = c(group)]
  rm(non_zero_groups)
  invisible(gc())
  
  # Convert Train Index to Date
  writeLines(sprintf("\t Convert Train Index to 'Date' format"))
  train[, c(index) := as.IDate(eval(str2lang(index)), format = "%Y-%m-%d")]
  invisible(gc())
  
  # Fill Missing Dates in Index
  writeLines(sprintf("\t Fill in missing dates in Train Index"))
  filled_index <- train[, lapply(.SD, minmax_sequence, interval = c(interval)), by = c(group), .SDcols = c(index)]
  train <- train[filled_index, on = c(group, index)]
  setnafill(train, fill = 0, cols = target)
  
  rm(filled_index)
  invisible(gc())
  
  # If test is available, filter Train based on Groups & Index in Test 
  if(!is.na(test_data)) {
    writeLines(sprintf("\t Filtering Train by Group & Index in Test"))
    
    # Subset Train by Group in Test
    common_groups <- fintersect(unique(test[, ..group]), unique(train[, ..group]))
    train <- train[common_groups, on = c(group)]
    rm(common_groups)
    
    # Locate all distinct Group and Index in Test
    unique_test_index <- unique(test[, .SD, by = c(group), .SDcols = index])
    
    # Stormy Weather competition require special treatment since Test occur sporatically throughout Train.
    if(!grepl("storm", id)) {
      # Locate Test Group and Index 
      unique_train_index <- fsetdiff(train[, .SD, by = c(group), .SDcols = index], unique_test_index)
      train <- train[unique_train_index, on = c(group, index)]
      
      rm(unique_train_index, unique_test_index)
      invisible(gc())
    } else {
      writeLines(sprintf("\t\t Processing Stormy Weather"))
      
      # NA all Test observations in Train
      train[unique_test_index, c(target) := NA, on = c(group, index)]
      
      # Impute Test observations in Train
      imputation_col <- paste0(target, "_imputed")
      
      train[, 
            c(imputation_col) := lapply(.SD, function(x) {
              imputation <- imputation_method(ts(x, frequency = ts_frequency))
              eval(parse(text = paste0("as.", class(x), "(imputation)")))
            }),
            by = c(group),
            .SDcols = c(target)]
      
      # NA Train observations from imputation column
      train[!is.na(train[[target]]), c(imputation_col) := NA]
      
      # Plot imputations
      writeLines(sprintf("\t\t Making & saving imputation plot"))
      imputation_plots <- train[, 
                                j = .(plots = list(ggplot(melt(.SD, id.vars = c(index), measure_vars = c(target, imputation_col))) + 
                                                     geom_line(aes(eval(parse(text = index)), value, col = variable), na.rm = T))), 
                                by = c(group), 
                                .SDcols = c(index, target, imputation_col)]
      
      ggsave(paste0("./Figures/", id, "_imputation.pdf"), gridExtra::marrangeGrob(imputation_plots$plots, ncol = 1, nrow = 3, top = ""))
      
      # Exchange Target column with imputed Target
      train[is.na(train[[target]]), c(target) := .SD, .SDcols = c(imputation_col)]
      train[, c(imputation_col) := NULL]
      
      # Clean Up
      rm(imputation_col, imputation_plots, unique_test_index)
      invisible(gc())
    }
  }
  
  # Trim leading & trailing zero's from Train by each Group
  writeLines(sprintf("\t Removing leading zero's from Train"))
  train <- train[, trimlt(.SD, target, trim = "leading"), by = c(group), .SDcols = c(target, index)]
  
  # Save Processed Train
  writeLines(sprintf("\t Save processed Train data"))
  qsave(train, save_path, nthreads = getDTthreads())
  
  # Final Clean Up
  if(!is.na(test_data)) rm(test)
  rm(id, path, train_data, test_data, group, index, target, interval, save_path, train)
  invisible(gc())
}

# Preprocess M* Competition Data ------------------------------------------


# Load M Competition Data -------------------------------------------------

M3 <- Mcomp::M3
data(M4, package = "M4comp2018")


# Repackage M competition data in tidy data.table format ------------------

M3DT <- rbindlist(lapply(M3, function(x) {
  data.table(
    sn = x$sn,
    st = x$st,
    n = x$n,
    h = x$h,
    period = x$period,
    type = x$type,
    description = x$description,
    train = list(x$x),
    test = list(x$xx)
  )
}))

M4DT <- rbindlist(lapply(M4, function(x) {
  data.table(
    st = x$st,
    n = x$n,
    h = x$h,
    period = x$period,
    type = x$type,
    train = list(x$x),
    test = list(x$xx)
  )
}))


# Adjust Frequency of Daily & Weekly series in M4 -------------------------

M4DT[period == "Daily", train := lapply(train, ts, frequency = 7)]
M4DT[period == "Weekly", train := lapply(train, ts, frequency = 52)]

# Save repackaged M competition data --------------------------------------

qsave(M3DT, paste0(processed_training_save_path, "M3DT.qs"), nthreads = getDTthreads())
qsave(M4DT, paste0(processed_training_save_path, "M4DT.qs"), nthreads = getDTthreads())