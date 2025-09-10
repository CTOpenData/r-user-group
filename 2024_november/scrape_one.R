
### Nov 12th
## Loosely derived from here
## https://crimebythenumbers.com/scrape-table.html

# pak::pkg_install("pdftools")
library(tidyverse)
library(pdftools)

## Here's one of the original files
## https://www.medicaid.gov/state-overviews/downloads/magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf
## R is capable of grabbing them from the web on the fly but for
## now I made a local copy

list.files(pattern = "^magi-app-process-time-snapshot-rpt-.*pdf")

magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-jan-mar-2024.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-oct-dec-2023.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-jul-sep-2023.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-apr-jun-2023.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-january-march-2023.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-oct-dec-2022.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-jul-sep-2022.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-apr-jun-2022.pdf")
# magi_apps <- pdf_text("magi-app-process-time-snapshot-rpt-jan-mar-2022.pdf")


magi_apps


what_year <- "2024"
# what_year <- "2023"
# what_year <- "2022"

magi_apps[12:24]
# magi_apps[10:22] # different for cy22_q2
# magi_apps[8:18] # different for cy22_q1

relevant_pages <- magi_apps[12:24]
# relevant_pages <- magi_apps[10:22] # different for cy22_q2
# relevant_pages <- magi_apps[8:18] # different for cy22_q1

glimpse(relevant_pages)

relevant_pages

relevant_pages <- strsplit(relevant_pages, "\n")

relevant_pages

relevant_pagest <- as_tibble_col(unlist(relevant_pages))

glimpse(relevant_pagest)
relevant_pagest
View(relevant_pagest)

relevant_pagest_done <-
  relevant_pagest %>%
  mutate(value = str_trim(value)) %>%
  filter(
    str_starts(
      value,
      paste0(state.name, collapse = "|")
    ) |
      str_starts(
        value,
        paste0(month.name, collapse = "|")
      ) |
      str_starts(
        value,
        "National Totals"
      )  |
      str_starts(
        value,
        "District of Columbia"
      )
  ) %>%
  mutate(
    value = str_replace(value,
                        "National Totals |National Totals1|National Totalsi",
                        "National Totals  "),
    value = str_replace_all(value,
                            pattern = " {2,}",
                            replacement = "|")
  )  %>%
  separate_wider_delim(value,
                       # delim = " //{2,//}",
                       delim = "|",
                       names = c("State", "Month", "PCT_lt_24hr",
                                 "PCT1_to7_days", "PCT8_to_30_days",
                                 "PCT31_to_45_days", "PCT_gt_45_days"),
                       too_few = "align_end",
                       # too_many = "debug",
                       cols_remove = FALSE) %>%
  filter(!is.na(Month)) %>%
  group_by(stategrp = rep(row_number(), length.out = n(), each = 3)) %>%
  fill(State, .direction = 'updown') %>%
  mutate(across(
    .cols = starts_with("PCT"),
    ~ as.integer(str_remove(.x, "%"))
  )) %>%
  ungroup() %>%
  mutate(State = replace_na(State, "District of Columbia")) %>%
  mutate(Month = str_replace(
    Month,
    "(January|February|March|April|May|June|July|August|September|October|November|December).*",
    "\\1")
  )

relevant_pagest_done
View(relevant_pagest_done)

all_pages_done <-
  relevant_pagest_done %>%
  select(State:PCT_gt_45_days) %>%
  mutate(RealMonth = my(paste(Month, what_year))) %>%
  arrange(State, RealMonth, PCT_lt_24hr) %>%
  group_by(RealMonth) %>%
  mutate(rank = rank(-PCT_lt_24hr)) %>%
  ungroup()


#   %>%
# print(n = Inf)

writexl::write_xlsx(all_pages_done, "sample_table_scrape.xlsx")

all_pages_done %>%
  pivot_longer(PCT_lt_24hr:PCT_gt_45_days,
               names_to = "timeliness",
               values_to = "pct") %>%
  filter(timeliness == "PCT_lt_24hr") %>%
  ggplot(aes(x = RealMonth,
             y = pct,
             color = State,
             group = State)) +
  geom_line(linewidth = 2) +
  labs(
    x = "Month",
    y = "%",
    title = "% MAGI Applications Processed in < 24 hours by State",
    caption = "Source: https://www.medicaid.gov/state-overviews/downloads/magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf"
  ) +
  theme_bw()

