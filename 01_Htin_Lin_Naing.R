if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}


# define directory paths
dir_path <- getwd() # Get current working directory
dir <- file.path(dir_path, "Assignment1")

# check the directory exists and create
if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

# define zip file path and download URL
zip_file <- file.path(dir, "basin_timeseries.zip")
link <- "https://gdex.ucar.edu/dataset/camels/file/basin_timeseries_v1p2_modelOutput_daymet.zip"

# download zip file only if it does not exist
if (!file.exists(zip_file)) {
  options(timeout = 6000)  # to pass timeout error
  download.file(link, zip_file, mode = "wb", method = "curl")
}

# extract zip content
extracted_files <- list.files(dir, recursive = TRUE, full.names = TRUE)

if (length(extracted_files) == 1) { #assuming there is downloaded zip file
  unzip(zip_file, exdir = dir)
  extracted_files <- list.files(dir, recursive = TRUE, full.names = TRUE) # update the file list
} else {
  warning("No file found")
}

# select "model_output" files
model_output_files <- grep("model_output", extracted_files, value = TRUE, ignore.case = TRUE)

if (length(model_output_files) == 0) {
  warning("No model output files found.")
} else {
  message(paste(length(model_output_files), "model output files found."))
}

# function to read .txt files
read_file <- function(file) {
  tryCatch({
    fread(file)
  }, error = function(e) {
    warning(paste("Error reading:", file, "-", e$message))
    return(NULL)
  })
}


# read files
if (length(model_output_files) > 0) {
  model_data_list <- lapply(model_output_files, read_file)
  
  # remove NULL values
  model_data_list <- Filter(Negate(is.null), model_data_list)
  
  if (length(model_data_list) > 0) {
    model_data <- rbindlist(model_data_list, use.names = TRUE, fill = TRUE)
    
   # save to CSV
    output_file <- file.path(dir, "combined_model_output.csv")
    fwrite(model_data, output_file)
    
    message("Model output saved to: ", output_file)
  } else {
    warning("No valid data to save.")
  }
} else {
warning("No model output files found.")
}

