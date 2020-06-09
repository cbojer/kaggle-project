

# Load Packages -----------------------------------------------------------

library(data.table)
library(qs)

# Get paths, names, and info ----------------------------------------------

file_paths <- data_info$raw_data_dir
file_names <- paste0(data_info$id, ".zip")
zip_info <- sapply(file_paths, get_zip_info, simplify = FALSE)
zip_info <- lapply(zip_info, function(x) if(any("Name" %in% names(x))) x[grepl("zip|7z|csv", x$Name), ]) 

# Initiate Assembly Loop --------------------------------------------------

for (i in seq_along(file_paths)) {
  writeLines(sprintf("\n (%d: %s) Initiating assembly of data\n", i, file_names[i]))
  
  # Temporarily extract '.zip' data files -----------------------------------
  
  writeLines(sprintf("\n (%d: %s) Extracting data from: %s\n", i, file_names[i], file_paths[i]))
  system(paste0("7z e -y ", file_paths[i], " -oCache/tmp"))
  
  # Read extracted data -----------------------------------------------------
  
  writeLines(sprintf("\n (%d: %s) Reading data from: \n", i, file_names[i]))
  file_data <- sapply(zip_info[[i]]$Name, function(x) {
    if(grepl("storm", file_names[i]) && grepl("test", x)) {
      print(x)
      .data <- tryCatch(data.table::fread(cmd = paste0("7z x -so ", "Cache/tmp/", x, " -p'Work4WalmarT'"), fill = T),
                        error = function(e) NULL, warning = function(w) NULL)
    } else {
      if (endsWith(x, ".csv")) {
        print(x)
        .data <- tryCatch(data.table::fread(paste0("Cache/tmp/", x)),
                          error = function(e) NULL, warning = function(w) NULL)
      } else if (endsWith(x, ".zip") || endsWith(x, ".7z")) {
        print(x)
        .data <- tryCatch(data.table::fread(cmd = paste0("7z x -so ", "Cache/tmp/", x), fill = T),
                          error = function(e) NULL, warning = function(w) NULL)
        
        
      } else {
        print(paste("Failed to read:", x))
        return(NULL)
      }
      if (!inherits(.data, "data.table")) NULL else .data
    }
  })
  
  # Correct names
  names(file_data) <- sapply(zip_info[[i]]$Name, function(x) strsplit(x, "\\.")[[1]][1])
  
  # Save extracted data -----------------------------------------------------
  
  # Construct save path
  save_path <- paste0(restructured_write_path, data_info$id[[i]], ".qs")
  
  writeLines(sprintf("\n (%d: %s) Save combined data set in: %s\n", i, file_names[i], save_path))
  qsave(x = file_data, file = save_path, nthreads = getDTthreads())
  
  # Remove temporary files --------------------------------------------------
  
  cat(sprintf("\n (%d: file_names[i])Removing temporary data", i, file_names[i]))
  invisible(sapply(list.files("./Cache/tmp/", full.names = TRUE), file.remove))
  
}
