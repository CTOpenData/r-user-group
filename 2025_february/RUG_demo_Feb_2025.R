library(tidyverse)
# tigris::shift_geometry()
options(tigris_use_cache = TRUE)

state_shapes <-
  tidycensus::get_acs(
    geography = "state",
    variables = "B01003_001",
    geometry = TRUE,
    year = 2020
  ) %>%
  select(-GEOID, -variable, -moe) %>%
  rename(
    "State" = NAME,
    "Population" = estimate
  ) %>%
  mutate(
    Population = scales::comma(Population)
  )

mapview::mapview(state_shapes)
mapview::mapview(state_shapes, zcol = "Population")
mapview::mapview(state_shapes, zcol = "State")

# We all forget how big Alaska is or how fas away Hawaii is
# You can opt to disregard geographic reality or
# filter our things you don't want on the map

shift_state_shapes <-
  tigris::shift_geometry(state_shapes) %>%
  filter(State != "Puerto Rico")
mapview::mapview(shift_state_shapes, zcol = "Population")

## As noted this is all public data no secrets here
your_data <-
  readxl::read_excel("Snap_stagger.xlsx") %>%
  mutate(`Number of staggered days` = factor(`Number of staggered days`,
                                             ordered = TRUE)
  ) %>%
  mutate(`Number of staggered days` = fct_relevel(`Number of staggered days`,
                                                  "1 day", "1-3 days",
                                                  "4-7 days", "8-10 days",
                                                  "11-15 days", "16-28 days")
  ) %>%
  mutate(
    Persons_receiving = scales::comma(Persons_receiving),
    Households_receiving = scales::comma(Households_receiving)
  )

By_State <-
  left_join(
    shift_state_shapes,
    your_data,
    by = join_by(State)
  )

### Sigh Arizona why do you have to be different
mapview::mapview(By_State, zcol = "Number of staggered days")

### Unleash the cool tricks Kraken!
mapview::mapview(By_State, zcol = "Number of staggered days",
                 popup = leafpop::popupTable(
                   x = By_State,
                   zcol = c(
                     "State",
                     "Population",
                     "Number of staggered days",
                     "Persons_receiving",
                     "Households_receiving"
                   ),
                   feature.id = FALSE,
                   row.numbers = FALSE
                 ))


# mapview::mapview(By_State %>%
#                    filter(!State %in% c("Alaska", "Hawaii") ),
#                  zcol = "Number of staggered days", legend = FALSE)

cool_map <-
  mapview::mapview(By_State, zcol = "Number of staggered days",
                   popup = leafpop::popupTable(
                     x = By_State,
                     zcol = c(
                       "State",
                       "Population",
                       "Number of staggered days",
                       "Persons_receiving",
                       "Households_receiving"
                     ),
                     feature.id = FALSE,
                     row.numbers = FALSE
                   ))

mapview::mapshot2(cool_map, "Rug_demo_Feb2025.html")

