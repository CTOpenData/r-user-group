# R script to clean messy address data 

# If you haven't installed the packages yet, that's the first step 
# install.packages(c('readr', 'dplyr', 'tidyverse'))
# pak::pkg_install("tidyverse")

# Load packages used in the script
library(readr) # readr is package for reading in dataframes from delimited files likes csvs
library(dplyr) # dplyr is a package for dataframe manipulation 
library(stringr) # stringr is a package to manipulate strings
# library (tidyverse) # instead of reading in each of these packages separately, you can also load the tidyverse
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

## Okay now we have "corrected addresses" lets
## put them on a map and show the county and correct
## town name.

# pak::pkg_install("tidygeocoder")
# pak::pkg_install("sf")
# pak::pkg_install("ggrepel")

ct_towns <- sf::st_read("shapefiles/cb_2017_09_cousub_500k.shp")
ct_counties <- sf::st_read("shapefiles/countyct_37800_0000_1990_s100_CENSUS_1_shp_wgs84.shp")

addresses_cleaned %>% 
  mutate(plot_address = paste0(`Street address`, ", ", Town_Standardized, ", ", State),
         Town_Standardized = str_to_title(Town_Standardized)) %>% 
  tidygeocoder::geocode(address = plot_address, 
                        # method = "osm") %>% 
                        method = "bing") %>% 
                        # method = "census") %>% 
  ggplot() +
  geom_sf(data = ct_towns, fill = "white") +
  geom_sf(data = ct_counties, fill = NA, colour = "darkblue", linewidth = 1)  +
  # coord_sf(default_crs = sf::st_crs(4326), label_axes = "----") +
  # borders("county", regions = "connecticut") +
  geom_point(aes(x = long, y = lat, color = County)) +
  ggrepel::geom_label_repel(aes(x = long, y = lat, label = Town_Standardized),
                            size = 2.5,
                            min.segment.length = .25) +
  labs(
    x = NULL, y = NULL
  ) +
  ggtitle("Our Cleaned Addresses with County and Town boundaries") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

ggplot(ct_counties) +
  geom_sf()
geom_sf(aes(fill = ALAND))

