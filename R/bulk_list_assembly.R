
# Assemble all files in a folder and save as .RDS list --------------------

bulk_list_assembly <- function(readpath, writepath, recursive = TRUE, delete_originals = FALSE) {
  require(data.table, quietly = TRUE)
  
  if(recursive) {
    folders <- list.dirs(path, full.names = TRUE)
    folders <- sub(paste0(path, "/"),"",folders[-1]) # Remove first that contain data_path
  } else {
    folders <- path
  }
  
  output <- vector("list", length(folders))
  x <- "key.csv.zip"
  for(i in seq_along(folders)) {
    files <- list.files(paste0(path, "/", folders[i]), pattern = ".csv$", full.names = F)
    file_names <- sub(".csv", "", files)
    load_paths <- paste0(path, "/", folders[i], "/", files)
    save_path <- paste0(path, "/", folders[i], "/combined.Rds")
    
    if(length(files) > 0) {
      cat(sprintf("\n\nReading %d files from folder %d of %d: %s", length(files), i, length(folders), folders[i]))
      
      d <- lapply(seq_along(load_paths), function(i) {
        cat(sprintf("\n\tloading... %s", file_names[i]))
        data.table::fread(load_paths[i])
      })
      
      names(d) <- file_names
      
      cat(sprintf("\nSaving combined.Rds to '%s'", save_path))
      saveRDS(d, file = save_path)
    }
    
    
    if(delete_originals) {
      for(i in seq_along(load_paths)) {
        file.remove(load_paths[i])
      }
    }
    
    output[[i]] <- list("folder" = folders[i],
                        "file_names" = files,
                        "load_paths" = load_paths,
                        "save_path" = save_path)
  }
  
  output
}
