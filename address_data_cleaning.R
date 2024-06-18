# R script to clean messy address data 

# If you haven't installed the packages yet, that's the first step 
# install.packages(c('readr', 'dplyr', 'tidyverse'))

# Load packages used in the script
library(readr) # readr is package for reading in dataframes from delimited files likes csvs
library(dplyr) # dplyr is a package for dataframe manipulation 
library(stringr) # stringr is a package to manipulate strings
#library (tidyverse) # instead of reading in each of these packages separately, you can also load the tidyverse
# tidyverse is a collection of packages for data science 

# Load the address data, which is hosted on Github, and save it as a dataframe called addresses
addresses <- read_csv("https://raw.githubusercontent.com/CTOpenData/r-user-group/main/demo_address_data.csv")

# Standardize the town names 
# Bring in the ctnamecleaner file from Github 
ct_name_cleaner <- read_csv("https://raw.githubusercontent.com/CT-Data-Collaborative/ctnamecleaner/master/ctnamecleaner.csv")

# Join the addresses dataframe with the ct_name_cleaner dataframe and bring in realname column
# & clean the zipcodes
addresses_cleaned <- addresses %>% 
  # Make town names uppercase using the mutate function
  mutate(Town = toupper(Town)) %>% 
  # Join with ctnamecleaner on town name
  left_join(ct_name_cleaner, by = c("Town" = "name")) %>% 
  # Rename columns
  rename(
    Town_Messy = Town,
    Town_Standardized = realname
  ) %>% 
  # Keep just the first 5 characters of the "Zip" column
  mutate(Zip_clean = str_sub(Zip, 1, 5)) %>% 
  # Reorder the columns
  select(
    `Street address`, 
    Town_Messy, 
    Town_Standardized, 
    Zip, 
    Zip_clean,
    County, 
    State
  )


