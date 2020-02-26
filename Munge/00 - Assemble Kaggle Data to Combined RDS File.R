
# Save variables from Global Environment ----------------------------------

variables_to_keep <- ls()

# Gather data for each Kaggle Competition to "Combined.rds" ---------------

if(!all(grepl("combined.rds", list.files(original_kaggle_data_path, recursive = T), ignore.case = T))) {
  
  bulk_list_assembly(original_kaggle_data_path, recursive = TRUE, delete_files = FALSE)
  
}


# Remove variables created in script --------------------------------------

rm(list = ls()[!ls() %in% variables_to_keep]) ; invisible(gc())
