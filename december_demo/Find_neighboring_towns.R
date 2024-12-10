# This script will create lists of neighboring towns in Connecticut.
# Author: Sarah Hurley
  
# Packages
library(tidyverse)
library(sf)
library(arcgis)

# Install packages
# install.packages("tidyverse")
# install.packages("sf")
# remotes::install_github("r-arcgis/arcgis", dependencies = TRUE)

# ============================================================================

# Read in CT Municipalities layer (ArcGIS REST URL)
town_url <- "https://services1.arcgis.com/FCaUeJ5SOVtImake/arcgis/rest/services/CT_Municipalities/FeatureServer/0"
towns <- arc_open(town_url) %>% arc_select()

plot(towns["Municipality"])


# Create a tiny buffer around each towns geometry so we can capture the neighbors
towns2 <- st_buffer(towns, dist = 0.001)

# Get a list of lists of the overlapping neighbors
neighbors <- st_intersects(towns2,
                           towns2)


# A function returning data frame of neighbors of a given town
nbr_pairs <- function(idx)  {
  data.frame(Town = rep(towns$Municipality[idx], length(neighbors[[idx]])), 
             Neighbor_town = towns$Municipality[neighbors[[idx]]])
}


# Apply the function to list of indices (the neighbors list)
pairs_of_names <- purrr::map_dfr(seq_along(neighbors),
                                 nbr_pairs)


grouped_list <- pairs_of_names %>% 
  mutate(same = if_else(Town == Neighbor_town, TRUE, FALSE)) %>% 
  filter(same == FALSE) %>% 
  select(-same) %>% 
  group_by(Town) %>%
  mutate(Neighbor_index = row_number()) %>%
  ungroup() 



## Create 3 new data sets from the grouped list

# 1. Number of neighboring towns for each town
summary_towns <- grouped_list %>% 
  group_by(Town) %>% 
  summarise(Count = n())

# 2. Wide data set: columns are # of neighboring towns
wide_list <- grouped_list %>%
  pivot_wider(names_from = Neighbor_index, 
              values_from = Neighbor_town, 
              names_prefix = "Neighbor_") %>% 
  left_join(summary_towns, by = "Town") %>%
  select(Town, Count, everything())

# 3. Long data set: two columns, one for town and another for a neighboring town
long_list <- grouped_list %>% 
  select(Town, Neighbor_town)




# write_csv(wide_list, "CT_Adjacent_Towns_WideFormat.csv")
# write_csv(long_list, "CT_Adjacent_Towns_LongList.csv")





### Resources ###
# 1. Connecticut Neighboring Towns data set
## https://data.ct.gov/Local-Government/Connecticut-Neighboring-Towns/3u6x-c464/about_data

# 2. Wide vs. Long data
## "Getting wide and long with data" by Stephen Skalicky: https://tinyurl.com/musuvthk

# 3. R-ArcGIS Packages
## https://github.com/R-ArcGIS/arcgislayers



