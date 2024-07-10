# R script to clean messy address data
# the original script is address_data_cleaning.R
# After cleaning we'll use R/RStudio to display the addresses
# on a map in both static (ggplot2) and interactive (leaflet) formats


# If you haven't installed the packages yet, that's the first step
# install.packages("pak")
# pak::pkg_install("tidyverse")

# Very opinionated but I always suggest loading the entire tidyverse first
# the tidyverse is a collection of packages for data science
library(tidyverse) # instead of reading in each of these packages separately
library(here) # here is a package for easy file referencing

# Load the address data, which is hosted on Github, and save it as a tibble/dataframe called addresses
addresses <-
  # TODO fix the permalink when this is merged
  # read_csv("https://raw.githubusercontent.com/CTOpenData/r-user-group/main/demo_address_data.csv")
  read_csv(
    here("geos/data/demo_address_data.csv")
  )

# Standardize the town names
# Bring in the ctnamecleaner file from Github
ct_name_cleaner <-
  read_csv("https://raw.githubusercontent.com/CT-Data-Collaborative/ctnamecleaner/master/ctnamecleaner.csv")

# Someone in the chat, half in jest, suggested they had more names
# they'd like to clean.  The "cleanest" way to do so is to modify
# the csv file in github, but it also possible to add it "on the fly"
# let's add another common New Britain case

ct_name_cleaner <-
  ct_name_cleaner %>%
  add_row(name = "NB", realname = "NEW BRITAIN") %>%
  arrange(name)

# Join the addresses dataframe with the ct_name_cleaner dataframe
# and bring in realname column & clean the zipcodes
addresses_cleaned <-
  addresses %>%
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


ct_towns <-
  sf::st_read(here("geos/shapefiles/cb_2017_09_cousub_500k.shp"))

ct_counties <-
  sf::st_read(here("geos/shapefiles/countyct_37800_0000_1990_s100_CENSUS_1_shp_wgs84.shp"))

addresses_cleaned %>%
  mutate(
    plot_address = paste0(`Street address`, ", ", Town_Standardized, ", ", State),
    Town_Standardized = str_to_title(Town_Standardized)
  ) %>%
  tidygeocoder::geocode(
    address = plot_address,
    # method = "osm") %>%
    # method = "geocodio") %>%
    # method = "census") %>%
    method = "bing"
  ) %>%
  ggplot() +
  geom_sf(data = ct_towns, fill = "white") +
  geom_sf(data = ct_counties, fill = NA, colour = "darkblue", linewidth = 1) +
  geom_point(aes(x = long, y = lat, color = County)) +
  ggrepel::geom_label_repel(aes(x = long, y = lat, label = Town_Standardized),
    size = 2.5,
    min.segment.length = .25
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Our Cleaned Addresses with County and Town boundaries",
    caption = "Geocoded with Bing geocoding service"
  ) +
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

# pak::pkg_install("janitor")

adds_to_plot <-
  addresses_cleaned %>%
  mutate(
    Town_Standardized = str_to_title(Town_Standardized),
    `Street address` = str_to_title(`Street address`),
    plot_address = paste0(`Street address`, ", ", Town_Standardized, ", ", State)
  ) %>%
  tidygeocoder::geocode(
    address = plot_address,
    # method = "osm",
    method = "geocodio",
    # method = "bing",
    # method = "census",
    custom_query = list(fields = "acs-economics"),
    full_results = TRUE,
    verbose = TRUE
  ) %>%
  janitor::clean_names() %>%
  mutate(
    hhincome = scales::dollar(fields_acs_economics_median_household_income_total_value),
    special_label = paste0(
      formatted_address,
      "<br>",
      address_components_county,
      "<br>",
      "2021 Census Tract Code = ",
      fields_census_2021_tract_code,
      "<br>",
      "2021 ACS Median Household Income = ",
      hhincome
    )
  )

# pak::pkg_install("leaflet")

m <-
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::setView(
    lng = -72.74587984553425,
    lat = 41.60828560557468,
    zoom = 8.8
  ) %>%
  leaflet::addMarkers(
    data = adds_to_plot,
    label = adds_to_plot$plot_address,
    popup = adds_to_plot$special_label
  ) %>%
  leaflet::addPolygons(
    data = town_esri_data,
    fill = NA,
    color = "red",
    weight = 2
  ) %>%
  leaflet::addPolygons(
    data = county_esri_data,
    fill = NA,
    color = "blue",
    weight = 2
  )


m
