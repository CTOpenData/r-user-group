# pak::pkg_install("arrow")

library(arrow)
library(dplyr)
pums_person <- open_dataset("./data/person")
pums_person
pums_person %>% dim_desc()

open_dataset("./data/person") |>
  # filter(location == "wa") |>
  group_by(year, location) |>
  summarize(
    avg_commute = sum(JWMNP * PWGTP, na.rm = TRUE) / sum(PWGTP)
  ) |>
  arrange(desc(year)) |>
  collect() %>%
  print(n = Inf)


open_dataset("./data/person") |>
  # filter(location == "wa") |>
  group_by(year, location) |>
  summarize(
    avg_commute = sum(JWMNP * PWGTP, na.rm = TRUE) / sum(PWGTP)
  ) |>
  arrange(desc(year)) |>
  collect() %>%
  pivot_wider(names_from = location, values_from = avg_commute)

