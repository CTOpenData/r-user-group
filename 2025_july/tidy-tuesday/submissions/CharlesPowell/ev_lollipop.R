### R User Group #TidyTuesday Challenge
### Chuck Powell
### July 8, 2025

### https://r-graph-gallery.com/lollipop-plot.html

### Concept:  Using one visual with a minimum of "ink" and text notation tell the
### data story of how EV Registrations have changed from 2022 to 2024 using the
### CT Open Data Portal and compare it to National data
### The viewer should be able to quickly intuit which Manufacturers (Makers)
### have increased or decreased in CT "market share" and simultaneously compare
### with best approximation I could quickly find for National data.
### Hopefully you can see Tesla lost the most ground and it was already below
### national.  Stellantis is the polar opposite, biggest gains over that time
### period, and it was already way over national numbers

### Data is filtered to just passenger and SUV models

library(tidyverse)
# source("helpers.R")

### A couple of custom functions optional but makes like easier
summarise_columns <- function(dfname, ...) {
  checkmate::assert(
    checkmate::checkClass(dfname, "tbl_lazy"),
    checkmate::checkClass(dfname, "data.frame")
  )

  grps <- enquos(...)
  # print(as.character(grps))

  if ("tbl_lazy" %in% class(dfname)) {
    summary_df <-
      dfname %>%
      count(!!!grps, name = "Count") %>%
      collect()
  } else {
    summary_df <-
      dfname %>%
      group_by(!!!grps, .drop = FALSE) %>%
      summarise(Count = n())
  }

  summary_df %>%
    mutate(Pct = Count / sum(Count) * 100) %>%
    ungroup() %>%
    arrange(!!!grps[1:length(grps) - 1], desc(Pct))
}

save_as_png <- function(
    x,
    filename,
    width = 1000,
    height = 500,
    pointsize = 14,
    res = 100
) {
  png(filename = filename,
      width = width,
      height = height,
      pointsize = pointsize,
      res = res,
      antialias = "cleartype")
  print(x)
  dev.off()
}

### I'm not going to rehash what Alex demoed last time.  I've
### downloaded the data locally as a csv file.

ev_regs <-
  read_csv("Electric_Vehicle_Registration_Data_20250514.csv") %>%
  filter(`Vehicle Type` %in% c("SUV", "Passenger"))

ev_regs %>%
  group_by(`Vehicle Make`) %>%
  count()

### Didn't want to spend a lot of time shopping for data
### found this quickly on the web

ev_by_maker <-
  read_tsv("ev_by_maker.tsv", col_names = FALSE) %>%
  select(Maker = X1, pct_market = X14)

ev_by_maker

### Sigh a little data cleaning to get things to line up
### this is just the CT Data manipulated a bit ;)

try_this <-
  ev_regs %>%
  mutate(Maker =
           case_when(
             `Vehicle Make` == "Tesla" ~ "Tesla",
             `Vehicle Make` == "Hyundai" | `Vehicle Make` == "Genesis" | `Vehicle Make` == "KIA" ~ "Hyundai Motor Group (incl. Kia)",
             `Vehicle Make` == "Chevrolet" | `Vehicle Make` == "GMC"  | `Vehicle Make` == "Cadillac" ~ "General Motors",
             `Vehicle Make` == "Volvo" | `Vehicle Make` == "Polestar" ~ "Volvo/Polestar",
             `Vehicle Make` == "Acura" | `Vehicle Make` == "Honda" ~ "Honda Motor Co",
             `Vehicle Make` == "Audi" | `Vehicle Make` == "Volkswagen" ~ "Volkswagen Group",
             `Vehicle Make` == "BMW" | `Vehicle Make` == "Mini"  ~ "BMW",
             `Vehicle Make` == "Toyota" | `Vehicle Make` == "Lexus"  ~ "Toyota Motor N.A.",
             `Vehicle Make` == "Rivian"  ~ "Rivian",
             `Vehicle Make` == "Jeep" | `Vehicle Make` == "Chrysler" | `Vehicle Make` == "Alfa Romeo" | `Vehicle Make` == "Dodge" ~ "Stellantis",
             `Vehicle Make` == "Ford" | `Vehicle Make` == "Lincoln" ~ "Ford",
             .default = "All Others"
           ),
         Year = factor(year(mdy(`Registration Start Date`)))
  ) %>%
  summarise_columns(Year, Maker) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(Pct, Count),
    names_prefix = "Year_"
  ) %>%
  arrange(desc(Pct_Year_2024)) %>%
  mutate(
    Maker = factor(Maker, ordered = TRUE),
    Maker = fct_reorder(.f = Maker,
                        .x = Pct_Year_2024
    ),
    NetChange = if_else(
      Pct_Year_2024 < Pct_Year_2022,
      "#E55C30FF",
      "#414487FF"
    )
  )

### Yeah this is a good start point!
ggplot(try_this) +
  geom_segment(aes(x = Maker,
                   xend = Maker,
                   y = Pct_Year_2022,
                   yend = Pct_Year_2024,
                   color = NetChange),
               linewidth = 3.5,
               lineend = "round"
               ) +
  geom_point(aes(x = Maker, y = Pct_Year_2022),
             color = "#7AD151FF",
             size = 4,
             alpha = .1,
             shape = 16) +
  geom_point(aes(x = Maker, y = Pct_Year_2024 ),
             color = "#414487FF",
             size = 4,
             alpha = .1,
             shape = 17) +
  scale_colour_identity() +
  coord_flip() +
  theme_bw() +
  theme(
    # legend.position = "none",
    panel.border = element_blank()
  ) +
  xlab("") +
  ylab("Percent of Total Registrations")

### Okay let's join the national data to the CT data
ggg <-
  left_join(
    try_this,
    ev_by_maker,
    by = join_by(Maker)
  ) %>%
  mutate(
    Maker = factor(Maker, ordered = TRUE),
    Maker = fct_reorder(.f = Maker,
                        .x = Pct_Year_2024)
  )

### Season to taste
final_product <-
  ggplot(ggg) +
  geom_segment(aes(x = Maker,
                   xend = Maker,
                   y = Pct_Year_2022,
                   yend = Pct_Year_2024,
                   color = NetChange),
               linewidth = 3,
               lineend = "round",
               arrow = arrow(length = unit(0.1, "inches"))
  ) +
  geom_point(aes(x = Maker, y = Pct_Year_2022),
             size = 4,
             alpha = .6,
             shape = 3) +
  geom_point(aes(x = Maker, y = Pct_Year_2024 ),
             size = 4,
             alpha = .6,
             shape = 3) +
  geom_point(aes(x = Maker, y = pct_market,
                 shape = 17 ),
             color = "black",
             size = 4) +
  scale_colour_identity(
    guide = "legend",
    labels = c("Increased CT Market Share", "Decreased CT Market Share")
  ) +
  scale_shape_identity(
    guide = "legend",
    labels = c("2024 National % Market Share")
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size = rel(1.1), face = "bold"),
    panel.grid.major.x = element_line(colour = "black"),
    panel.grid.minor.x = element_line(colour = "grey50")
  ) +
  xlab("") +
  ylab("Percent of Total CT Registrations") +
  labs(
    title = "Comparing CT EV Registrations between 2022 and 2024 and against National Data",
    subtitle = "Filtered to select manufacturers for Passenger and SUV models",
    caption = "Data from: CT Open Data Portal & https://caredge.com/guides/electric-vehicle-market-share-and-sales"
  )

final_product

### More or less optimized to cut and paste into powerpoint
save_as_png(
  final_product,
  "final_product.png",
  width = 1600,
  height = 800
  )

