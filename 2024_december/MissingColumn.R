library(arrow)

# List all files
files <- list.files(path = getwd(), recursive = TRUE, full.names = TRUE, pattern = ".parquet")

# Function for missing column
missing_column <- lapply(files, function(file) {
  data <- read_parquet(file)
  
  # Check if 'town' is missing
  if (!"town" %in% colnames(data)) {
    return(basename(file))  # Return file name if "town" is missing
  } else {
    return(NULL)  # Return NULL if 'town' is present
  }
})

# Filter out NULL values to get the list of files without 'town'
missing_files <- Filter(Negate(is.null), missing_column)

# Print the result
if (length(missing_files) > 0) {
  cat("Files missing 'town' column:\n")
  print(missing_files)
} else {
  cat("All files contain the expected column.\n")
}
