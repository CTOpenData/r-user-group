# Mapping Rates in CT towns
# pak::pkg_install("tmap")

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(sf)
library(mapview)
library(tmap)


# Read the Towns and shapes
load(file = "CT_Towns_2023_pops_shapes.RData")


#Read the data file
demo_map_rates <-
  read_csv("demo_map_rates.csv")

#Join data file with the town geometry
demo_map_plot <-
  left_join(
    CT_towns,
    demo_map_rates,
    by = join_by(NAME == town)
  )

demo_map_plot <-
  demo_map_plot %>%
  mutate(
    elig_rate_gp = fct_reorder(
      elig_rate_gp,
      pct_eligible,
      .na_rm = FALSE
    ),
    cpct = scales::percent(pct_eligible,
                           accuracy = 1)
  )

#Plot the eligibility rate group in a static map
ggplot(demo_map_plot, aes(fill = elig_rate_gp)) +
  geom_sf(show.legend = TRUE) +
  theme_void()


# Format the map to make it a little prettier
ggplot(
  data = demo_map_plot,
  aes()
) +
  geom_sf(
    aes(
      fill = elig_rate_gp),
    color = "white") +
  scale_fill_manual(
    values = c("<25%" = "#0070C0", "26%-50%" = "#0B3040", ">75%" = "#00B0F0", "51%-75%" = "#5ec962",
               na.value = "lightgray"),
    name = "Eligibility Rate Group",
    # labels = c("<25%", "26%-50%", ">75%", "51%-75%")
  ) +
  theme_void() +
  theme(legend.position = "bottom")

my_colors <- c("#0070C0", "#0B3040", "#00B0F0", "#5ec962")

#For a popup table upon mouse click
mapview::mapview(demo_map_plot,
                 zcol = "elig_rate_gp",
                 col.regions = my_colors,
                 layer.name = ("Demo Program Eligibility by Town"),
                 legend = TRUE,
                 label = demo_map_plot$NAME,
                 popup = leafpop::popupTable(
                   x = demo_map_plot,
                   zcol = c(
                     "NAME",
                     "cPopulation",
                     "cpct",
                     "Participants",
                     "Households"
                   ),
                   feature.id = FALSE,
                   row.numbers = FALSE
                 )
)

#For a popup table upon mouse click
cool_map <-
  mapview::mapview(demo_map_plot,
                   zcol = "elig_rate_gp",
                   col.regions = my_colors,
                   layer.name = ("Demo Program Eligibility by Town"),
                   legend = TRUE,
                   label = demo_map_plot$NAME,
                   popup = leafpop::popupTable(
                     x = demo_map_plot,
                     zcol = c(
                       "NAME",
                       "cPopulation",
                       "cpct",
                       "Participants",
                       "Households"
                     ),
                     feature.id = FALSE,
                     row.numbers = FALSE
                   )
  )



mapview::mapshot2(cool_map, "Demo Rates for RUG.html")
