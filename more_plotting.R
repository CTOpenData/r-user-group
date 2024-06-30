options(tigris_use_cache = TRUE)

nutmeg <- 
  tidycensus::get_acs(
    state = "CT",
    geography = "tract",
    variables = "B19013_001", #median household income from the 2016-2020 ACS
    geometry = TRUE,
    year = 2020
  )

head(nutmeg)

nutmeg %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "magma") 

wtf2 <- 
  tidycensus::get_decennial(
    state = "CT",
    geography = "tract",
    # geography = "congressional district",
    # geography = "county subdivision",
    geometry = TRUE, 
    variables = "P13_001N", # median age
    sumfile = "dhc",
    year = 2020
  )

wtf2 %>% 
  # filter(!is.na(value)) %>%
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

wtf2 %>% mutate(median_age = cut_number(value, n = 5))
