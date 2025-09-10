library(arrow)
library(purrr)

# List of all parquet files
files_to_check <- list.files(path = getwd(), pattern = ".parquet$", full.names = TRUE)

# Function to get column names from a parquet file
get_column_names <- function(file) {
  # Read the schema (column names) of the parquet file
  schema <- read_parquet(file, as_data_frame = FALSE)$schema
  schema$names
}

# Get column names for each file
column_names_list <- map(files_to_check, get_column_names)

# Find the intersection of column names across all files
same_columns <- reduce(column_names_list, intersect)

# Print common columns
cat("Common columns across all parquet files:\n")
print(same_columns)

# View columns in each file for comparison
names(column_names_list) <- basename(files_to_check)
column_names_list
