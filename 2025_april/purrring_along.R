### R Users Group Demo April 8, 2025
### You'll need these packages

# pak::pkg_install("tidyverse")
# pak::pkg_install("writexl")
# pak::pkg_install("readxl")


library(tidyverse)
library(writexl)

### Time permitting show the converse operation
# dir.create("demodata")
# diamonds %>%
#   group_by(cut) %>%
#   group_walk(~ write_xlsx(.x, paste0("demodata/cut_", .y$cut, ".xlsx")))

### Assume the files we want are in one known directory
### Here called demodata
files_we_want <-
  list.files(
    path = "demodata",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE,
    pattern = ".*cut_.*xlsx"
  )

files_we_want

files_we_want[[3]]
files_we_want[["demodata/cut_Good.xlsx"]]

names(files_we_want) <- files_we_want

files_we_want[[3]]
files_we_want[["demodata/cut_Good.xlsx"]]

readxl::read_excel(
  path = files_we_want[[3]]
)

just_one_cut <-
  readxl::read_excel(
    path = files_we_want[[3]]
  )


list_of_tibbles <-
  files_we_want %>%
  imap(
    ~ readxl::read_excel(
      path = .x
    )
  )

list_of_tibbles

alltogether <-
  bind_rows(
    list_of_tibbles,
    .id = "file_name"
  )

alltogether

alltogether <-
  alltogether %>%
  mutate(
    cut = str_remove_all(file_name, "^demodata/cut_"),
    cut = str_remove_all(cut, "\\.xlsx$"),
    cut = fct(cut,
              levels(c("Fair", "Good", "Very Good", "Premium", "Ideal"))
    )
  ) %>%
  select(-file_name) %>%
  relocate(cut, .before = color) %>%
  arrange(price)

alltogether



