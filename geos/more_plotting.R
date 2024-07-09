# taken from here
# https://walker-data.com/tidycensus/articles/spatial-data.html

library(tidyverse)
options(tigris_use_cache = TRUE)

nutmeg <- 
  tidycensus::get_acs(
    state = "CT",
    geography = "tract",
    variables = "B19013_001", # median household income from the 2016-2020 ACS
    geometry = TRUE,
    year = 2020
  )

head(nutmeg)
View(nutmeg)
nutmeg %>% 
  filter(is.na(estimate))

nutmeg %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma", 
                       na.value = "white") 

nutmeg2 <- 
  tidycensus::get_decennial(
    state = "CT",
    # geography = "tract",
    # geography = "congressional district",
    geography = "county subdivision",
    geometry = TRUE, 
    variables = "P13_001N", # median age
    sumfile = "dhc",
    year = 2020
  )

nutmeg2 %>% 
  filter(!str_starts(NAME, "County subdivisions not defined")) %>%
  separate(
    col = NAME, 
    sep = ",", 
    into = c("town", "county", "state"),
    remove = FALSE
  ) %>% 
  mutate(
    across(
      .cols = c(town, county, state), 
      str_squish
    )
  ) %>% 
  mutate(
    town = str_remove(town, " town$"),
    county = str_remove(county, " County$")
  ) %>% 
  mutate(median_age = cut_number(value, n = 5)) %>% 
  mutate(median_age2 = cut_width(value, width = 10, center = 45)) %>% 
  mutate(town_center = sf::st_centroid(geometry)) %>% 
  # slice_max(order_by = value, n = 5) %>% 
  ggplot() +
  geom_sf(aes(fill = median_age2), color = "black") +
  geom_point(aes(x = -72.2633871, y = 41.8034199),
             colour = "green", 
             size = 3, 
             show.legend = FALSE) +
  geom_sf_text(aes(label = town), size = 2) +
  scale_fill_viridis_d(option = "inferno", 
                       begin = .4, 
                       end = 1) +
  theme_void()

# https://github.com/r-spatial/sf/issues/2359
nutmeg2 %>% 
  filter(!str_starts(NAME, "County subdivisions not defined")) %>%
  # separate_wider_delim(
  #   cols = NAME,
  #   delim = ",",
  #   names = c("town", "county", "state"),
  #   cols_remove = FALSE
  # ) %>%
  # mutate(
  #   across(
  #     .cols = c(town, county, state), 
  #     str_squish
  #   )
  # ) %>% 
  # mutate(
  #   town = str_remove(town, " town$"),
  #   county = str_remove(county, " County$")
  # ) %>% 
  mutate(median_age = cut_number(value, n = 5)) %>% 
  mutate(median_age2 = cut_width(value, width = 10, center = 45)) %>% 
  # slice_max(order_by = value, n = 5) %>% 
  ggplot(aes(fill = median_age2)) +
  geom_sf(color = NA) +
  geom_point(aes(x = -72.2633871, y = 41.8034199),
             colour = "green", 
             size = 3, 
             show.legend = FALSE) +
  scale_fill_viridis_d(option = "magma") +
  theme_void()


