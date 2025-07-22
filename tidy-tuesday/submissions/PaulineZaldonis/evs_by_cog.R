# -------------------------------
# Load required packages
# -------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(ggplot2)
library(geofacet)
library(extrafont)  # For custom fonts
library(scales)
library(stringr)
library(lubridate)
# font_import()

# -------------------------------
# Load and clean town names
# -------------------------------
ct_name_cleaner <- read_csv("https://raw.githubusercontent.com/CT-Data-Collaborative/ctnamecleaner/master/ctnamecleaner.csv")

# Add missing towns
new_towns <- data.frame(
  name = c("TACONIC", "INDIAN NECK", "GREENS FARMS", "WINCHESTR CENTER", "SOUTH BRITAIN", "GROTON LONG PT."),
  realname = c("SIMSBURY", "BRANFORD", "WESTPORT", "WINCHESTER", "SOUTHBURY", "GROTON")
)

ct_name_cleaner <- bind_rows(ct_name_cleaner, new_towns)

# -------------------------------
# Load EV registration data
# -------------------------------
ev_registrations <- read_csv("https://data.ct.gov/resource/y7ky-5wcz.csv?$limit=1000000000")

ev_reg_ct <- ev_registrations %>%
  filter(primarycustomerstate == "CT") %>%
  mutate(primarycustomercity = str_to_upper(primarycustomercity)) %>%
  left_join(ct_name_cleaner, by = c("primarycustomercity" = "name"))

# -------------------------------
#  Download and prep town boundaries
# -------------------------------
ct_towns <- st_read("https://data.ct.gov/resource/5c5g-mddb.geojson") %>%
  mutate(realname = str_to_upper(municipality))

# -------------------------------
# Join EV data with boundaries
# -------------------------------
ev_reg_ct <- ev_reg_ct %>%
  left_join(ct_towns)

# -------------------------------
# Summarize registrations by month & COG
# -------------------------------
ev_reg_ct_summary <- ev_reg_ct %>%
  mutate(month = as.Date(floor_date(registration_date_start, "month"))) %>%
  group_by(councilofgovernment, month) %>%
  summarize(count = n(), .groups = "drop")

# -------------------------------
# Define CT planning region layout for geo_facet
# -------------------------------
mygrid <- data.frame(
  code = c("NWCOG", "CRCOG", "NECCOG", "NVCOG", "SCRCOG", "RIVERCOG", "SECCOG", "WESTCOG", "MCCOG"),
  name = c("Northwest Hills", "Capital Region", "Northeastern CT", "Naugatuck Valley",
           "South Central Regional", "Lower CT River Valley", "Southeastern CT",
           "Western CT", "CT Metropolitan"),
  row = c(1, 1, 1, 2, 2, 2, 2, 3, 3),
  col = c(2, 3, 5, 2, 3, 4, 5, 1, 2),
  stringsAsFactors = FALSE
)

# -------------------------------
# Plot EV registrations
# -------------------------------

ev_plot <- ggplot(ev_reg_ct_summary, aes(x = month, y = count)) +
  geom_line(color = "#1F64E5", size = 0.95) +
  facet_geo(~ councilofgovernment, grid = mygrid, move_axes = TRUE) +
  scale_x_date(
    labels = function(x) paste0("'", format(x, "%y")),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme_minimal(base_family = "Poppins") +
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold", color = "#FFFFFF",
      margin = margin(t = 5, b = 5), family = "Poppins"
    ),
    strip.background = element_rect(fill = "#1F64E5", color = NA),
    panel.spacing = unit(0.3, "lines"),
    axis.text.x = element_text(size = 8, hjust = 1, color = "gray30"),
    axis.text.y = element_text(size = 8, color = "gray30"),
    axis.ticks.y = element_line(color = "gray90"),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "gray30"),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "#f3f6fe", color = NA),
    plot.caption = element_text(hjust = 0, size = 10)
  ) +
  labs(
    title = "Electric Vehicle Registrations by Planning Region 🚗⚡️",
    subtitle = "Western CT and Capitol Region have the largest number of EV registrations amongst the CT Councils of Government.",
    x = "Registration Start Date",
    y = "# Electric Vehicle Registrations",
    caption = "Source: Electric Vehicle Registration Data, CT DMV"
  )

# ggsave("ev_plot.png", ev_plot, width = 10000, height = 5000, units = "px", dpi = 800, bg = "white")

