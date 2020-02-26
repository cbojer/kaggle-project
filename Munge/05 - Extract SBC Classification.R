
# Load Packages -----------------------------------------------------------

library(data.table)

# Save variables from Global Environment ----------------------------------

variables_to_keep <- ls()

# Extract SBC Classification from Kaggle Data -----------------------------

for(idx in 1:nrow(data_info)) {
  
  id <- data_info$id[[idx]]
  path <- processed_data_paths[grep(id, processed_data_paths)]
  target <- data_info$target[[idx]]
  group <- data_info$group[[idx]]
  index <- data_info$index[[idx]]
  
  writeLines(sprintf("\nExtracting SBC Classification from %s", id))
  
  # Load & prepare data
  train <- readRDS(path)
  
  writeLines(sprintf("\tPerforming calculations for %d groups", uniqueN(train, by = group)))
  
  tictoc::tic()
  sbc_class <- train[, sbc_classifier(eval(str2lang(target))), group]
  tictoc::toc()
  
  sbc_class[, c(group) := NULL]
  
  saveRDS(cbind(id = id, sbc_class), paste0(sbc_save_path, id, ".RDS"))
  
  rm(train, sbc_class) ; gc()
}


# Extract SBC Classification from M Competion Data ------------------------

writeLines(sprintf("\nExtracting SBC Classification from M3"))
M3 <- readRDS(processed_data_paths[grep("M3", processed_data_paths)])

writeLines(sprintf("\tPerforming calculations for %d groups", nrow(M3)))
tictoc::tic()
M3_sbc <- cbind(id = "M3", M3[, rbindlist(lapply(train, sbc_classifier))])
tictoc::toc()

saveRDS(M3_sbc, paste0(sbc_save_path, "M3.RDS"))

writeLines(sprintf("\nExtracting SBC Classification from M4"))
M4 <- readRDS(processed_data_paths[grep("M4", processed_data_paths)])

writeLines(sprintf("\tPerforming calculations for %d groups", nrow(M4)))
tictoc::tic()
M4_sbc <- cbind(id = "M4", M4[, rbindlist(lapply(train, sbc_classifier))])
tictoc::toc()

saveRDS(M4_sbc, paste0(sbc_save_path, "M4.RDS"))

# Remove variables created in script --------------------------------------

rm(list = ls()[!ls() %in% variables_to_keep]) ; invisible(gc())
