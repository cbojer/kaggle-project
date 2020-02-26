
# Assemble all files in a folder and save as .RDS list --------------------

<<<<<<< HEAD
bulk_list_assembly <- function(readpath, writepath, recursive = TRUE, delete_originals = FALSE) {
=======
bulk_list_assembly <- function(path, recursive = TRUE, delete_originals = FALSE) {
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
  require(data.table, quietly = TRUE)
  
  if(recursive) {
    folders <- list.dirs(path, full.names = TRUE)
    folders <- sub(paste0(path, "/"),"",folders[-1]) # Remove first that contain data_path
  } else {
    folders <- path
  }
  
  output <- vector("list", length(folders))
<<<<<<< HEAD
  x <- "key.csv.zip"
=======
  
>>>>>>> 5bc19f9f58fc3bb7a14131defc399cc59a2b9d2b
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
