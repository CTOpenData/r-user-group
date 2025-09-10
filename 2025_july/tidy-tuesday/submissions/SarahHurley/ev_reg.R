library(tidyverse)
library(showtext)

## Read in the dataset
data <- read_csv("https://data.ct.gov/resource/y7ky-5wcz.csv?$limit=1000000000")

# Get table of all available colors built into R
r_colors <- as.data.frame(colors()) %>% 
  rename(color = 'colors()') %>% 
  mutate(r_color = color)

# Make table of car colors, and match to an R color if available
colors <- as.data.frame(unique(data$primarycolor)) %>% 
  rename(color = 'unique(data$primarycolor)') %>% 
  mutate(color = tolower(color)) %>% 
  left_join(r_colors, by = "color") %>% 
  mutate(r_color = case_when( # Choose some colors manually if they didn't have an exact match
    color == "silver" ~ "dimgray",
    color == "teal" ~ "turquoise4",
    color == "amethyst" ~ "slateblue1",
    color == "cream" ~ "wheat1",
    color == "wood" ~ "burlywood4",
    color == "white" ~ "ivory1",
    color == "green" ~ "darkolivegreen3",
    TRUE ~ color
  ))

# Manipulate the dataset so we can plot it
df <- data %>%
  mutate(year = year(as.Date(registration_date_start)),
         primarycolor = tolower(primarycolor)) %>%
  filter(!is.na(year), !is.na(primarycolor)) %>%
  count(year, primarycolor) %>% 
  left_join(colors, by = c("primarycolor" = "color")) %>% 
  filter(primarycolor != "unknown") %>% 
  arrange(n)

# Create base plot
base_plot <- df %>%
  group_by(year) %>%
  mutate(primarycolor = fct_reorder(primarycolor, desc(n))) %>%
  ungroup() %>%
  ggplot(aes(x = primarycolor, y = n, fill = primarycolor)) +
  geom_col() +
  scale_fill_manual(values = setNames(df$r_color, df$primarycolor)) +
  facet_wrap(~ year, scales = "free_x")

# Get Poppins font
font_add_google("Poppins", "poppins")
showtext_auto()

# Format the chart
base_plot +
  labs(y = "Number of Vehicles", title = "Registered Electric Vehicles by Primary Color") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Primary Color of Vehicle")) +
  theme(
    text = element_text(family = "poppins", size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major = element_line(colour = "#A8A8A8", linetype = "dotted"),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15),
    strip.background = element_rect(fill = "#d8d8d8", color = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5, size = 20)
  )


# Export to my computer
# ggsave("C:/Users/hurleysa/Documents/R/TidyTuesday/R_Users_Group/plot.png", width = 7, height = 7, dpi = 300)

