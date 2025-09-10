# Load packages
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)

# Use Alex Senetcky's gist to pull EV data
devtools::source_gist(
  id = "https://gist.github.com/asenetcky/52bf3fa10a2dff08f62da96e2347e018",
  sha1 = "5b27e785462239543a6cda235382ebec7c381471"
)

ev <- pull_odp(resource = "y7ky-5wcz")

# Filter for Hartford only 
ct_ev <- ev |> 
  filter(primarycustomerstate == "CT", 
         primarycustomercity == "HARTFORD") |> 
  group_by(
    vehiclemake, 
    registration_date_start, 
    registration_date_expiration
  ) |>
  mutate(
    registration_date_start = as.Date(registration_date_start),
    registration_date_expiration = as.Date(registration_date_expiration),
  ) |> 
  select(vehiclemake, registration_date_expiration, registration_date_start)

all_dates <- data.frame(
  day = seq(
    min(
      ct_ev$registration_date_start
    ), 
    Sys.Date(), # today
    by = "day"
  )
)

ev_all_dates <- ct_ev |> 
  full_join(
    all_dates, 
    by = join_by(
      registration_date_start < day, 
      registration_date_expiration > day
    )
  )

vehiclemake_by_date <- ev_all_dates |>
  group_by(vehiclemake, day) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  ungroup()

include_vehiclemake <- vehiclemake_by_date |>
  filter(day == Sys.Date()) |>
  arrange(desc(count)) |>
  pull(vehiclemake) |>
  head()

dif_ordering <- vehiclemake_by_date |>
  filter(vehiclemake %in% include_vehiclemake) |>
  group_by(day) |> 
  mutate(order = row_number(count) * 1.0) |>
  ungroup()

colors <- c('Tesla' = '#212121', 
            'BMW'='#00Bac9', 
            'Toyota' = '#FFFFFF', 
            'Hyundai' = '#00287A', 
            'Chevrolet' = '#D1AD57', 
            'KIA' = '#BB162B')


anim <- ggplot(dif_ordering, aes(x = order, group = vehiclemake)) +
  geom_tile(
    aes(
      y = count/2,
      height = count,
      fill = vehiclemake), 
    width = 0.9, 
    color = "black",
  ) +
  theme_minimal() +
  labs(
    title = "Tesla Is Dominating Hartford's EV Scene",
    subtitle = "Date Visualized: {closest_state}", 
    y = 'Currently Registered Vehicles',
    x = '') +  
  transition_states(day,
                    transition_length = 6, 
                    state_length = 2) + 
  geom_text(aes(y = count, label = count), vjust = -0.5) +
  # text in x-axis (requires clip = "off" in coord_cartesian)
  geom_text(aes(y = 0, label = vehiclemake), vjust = 2) +
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = colors) + 
  ease_aes('cubic-in-out') 

anim_save("ev_data.gif", anim)