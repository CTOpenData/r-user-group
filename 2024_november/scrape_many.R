
### Oct 28th
## Loosely derived from here
## https://crimebythenumbers.com/scrape-table.html

# pak::pkg_install("pdftools")
library(tidyverse)
library(pdftools)

## Here's one of the original files
## https://www.medicaid.gov/state-overviews/downloads/magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf
## R is capable of grabbing them from the web on the fly but for
## now I made a local copy

### in this case the "pattern" is starts with some fixed text and ends in pdf

list.files(pattern = "^magi-app-process-time-snapshot-rpt-.*pdf")

pdf_file_name <- list.files(pattern = "^magi-app-process-time-snapshot-rpt-.*pdf")


### old school for loop through the vector file by file
### for each of the files that match our pattern we will make a tibble
### with the cleaned results.  We'll store each of the tibbles
### in a list then bind them all together at the end

# So create an empty list to append to ...
tibble_list <- list()

# started_loop <- now()
for(f in 1:length(pdf_file_name)) {

  # loops can feel endless so inform us about progress
  print(paste("Processing ", f, pdf_file_name[f]))

  # grab the year because there are differences by year
  what_year <-
    str_split_i(
      pdf_file_name[[f]],
      "\\-|\\.",
      i = 9)
  #print(what_year)

  # use pdftools to read the text
  magi_apps <- pdf_text(pdf_file_name[f])

  # for most of the files the tables run from page 12 to 24
  # but for two of them nope!
  if (str_detect(
    pdf_file_name[f],
    "jan-mar-2022.pdf"
  )
  ) {
    relevant_pages <- magi_apps[8:18] # different for cy22_q1
  } else if (str_detect(
    pdf_file_name[f],
    "apr-jun-2022.pdf"
  )
  ) {
    relevant_pages <- magi_apps[10:22] # different for cy22_q2
  } else {
    relevant_pages <- magi_apps[12:24]
  }

  # load em up split them up make em a tibble
  relevant_pages <- strsplit(relevant_pages, "\n")
  relevant_pagest <- as_tibble_col(unlist(relevant_pages))

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

  all_pages_done <-
    relevant_pagest_done %>%
    select(State:PCT_gt_45_days) %>%
    mutate(RealMonth = my(paste(Month, what_year))) %>%
    arrange(State, RealMonth, PCT_lt_24hr) %>%
    group_by(RealMonth) %>%
    mutate(rank = rank(-PCT_lt_24hr)) %>%
    ungroup()

  ### q3 in CY22
  if(str_detect(
    pdf_file_name[f],
    "jul-sep-2022.pdf")) {
    all_pages_done <-
      all_pages_done  %>%
      filter(RealMonth <= "2022-09-01" & RealMonth >= "2022-07-01")
    print("Bad Dates")
  }

  ### q1 in CY22
  if(str_detect(
    pdf_file_name[f],
    "jan-mar-2022.pdf")) {
    all_pages_done <-
      all_pages_done  %>%
      filter(RealMonth <= "2022-03-01" & RealMonth >= "2022-01-01")
    print("Bad Dates")
  }

  tibble_list[[f]] <- all_pages_done

}

whole_thing <- bind_rows(tibble_list)

overall_rankings <-
  whole_thing %>%
  group_by(State) %>%
  summarise(overallavgtime = mean(PCT_lt_24hr, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(overallavgtime)) %>%
  mutate(overall_rank = rank(-overallavgtime))

all_reports <- left_join(whole_thing, overall_rankings)

writexl::write_xlsx(all_reports, "all_cms_magi_data.xlsx")

all_reports %>%
  pivot_longer(PCT_lt_24hr:PCT_gt_45_days,
               names_to = "timeliness",
               values_to = "pct") %>%
  filter(timeliness == "PCT_lt_24hr" & rank <= 12) %>%
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

all_reports %>%
  filter(RealMonth >= "2023-01-01") %>%
  pivot_longer(PCT_lt_24hr:PCT_gt_45_days,
               names_to = "timeliness",
               values_to = "pct") %>%
  filter(timeliness == "PCT_lt_24hr") %>%
  filter( overall_rank <= 5 | State == "National Totals") %>%
  ggplot(aes(x = RealMonth,
             y = pct,
             color = State,
             group = State,
             label = pct)) +
  geom_line(linewidth = 2) +
  # geom_label(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  scale_x_date(date_labels="%b %Y",date_breaks = "1 month") +
  labs(
    x = "Month",
    y = "%",
    title = "% MAGI Applications Processed in < 24 hours by State",
    subtitle = "Displaying only the Top 5 States",
    caption = "Source: https://www.medicaid.gov/state-overviews/medicaid-modified-adjusted-gross-income-childrens-health-insurance-program-application-processing-time-report/index.html"
  ) +
  theme_bw() +
  theme(
        panel.grid.minor.x = element_blank()
        ) +
  scale_color_viridis_d(option = "inferno") +
  scale_colour_manual(values = c("#1170aa","#a3cce9", "#1170aa", "#1170aa", "#1170aa", "#57606c",
                                 "#1170aa", "#1170aa", "#1170aa", "#1170aa", "#1170aa"
                                 )
                      )

### fini

unique(all_reports$overall_rank)

# url <- "https://www.medicaid.gov/state-overviews/downloads/magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf"
#
# file_name <- "magi-app-process-time-snapshot-rpt-apr-jun-2024.pdf"
# file_path <- "C:\\Users\\Powellch\\Desktop\\RProjects\\emk"
#
# download.file(url = url, destfile = paste0(file_path, file_name, sep = ""))
#


