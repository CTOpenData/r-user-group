# Janitor Package  Demo

# Some links for references
# https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html
# https://www.r-bloggers.com/2024/08/why-every-data-scientist-needs-the-janitor-package/
# https://www.r-bloggers.com/2024/05/easy-data-cleaning-with-the-janitor-package/


# Install if needed
# install.packages("janitor")
# install.packages("dplyr")

library(janitor)
library(dplyr)

#### 1. Create messy sample dataset ####

set.seed(123)

raw_data <- data.frame(
  "Employee ID" = c(101, 102, 103, 103, 104, 105, 105),
  "First Name" = c("Alice", "Bob", "Charlie", "Charlie", "Dana", "Eli", "Eli"),
  "Last Name" = c("Smith", "Jones", "Brown", "Brown", "White", "Black", "Black"),
  "Start Date" = c(44561, 43831, 44200, 44200, 45000, 44900, 44900),
  "Salary ($)" = c(70000, 80000, 75000, 75000, 82000, 60000, 60000),
  check.names = FALSE
)

raw_data

#### 2. clean_names() ####

cleaned_data <- raw_data %>%
  clean_names()

cleaned_data

#### 3. Excel date conversion ####

cleaned_data <- cleaned_data %>%
  mutate(start_date = excel_numeric_to_date(start_date))

cleaned_data

#### 4. get_dupes() ####

dupes <- cleaned_data %>%
  get_dupes(employee_id)

dupes

#### 5. remove_empty() ####

messy_data <- cleaned_data
messy_data$empty_column <- NA
messy_data <- rbind(messy_data, rep(NA, ncol(messy_data)))

messy_data

cleaned_empty <- messy_data %>%
  remove_empty(c("rows", "cols"))

cleaned_empty

#### 6. tabyl() + adorn ####

name_summary <- cleaned_data %>%
  tabyl(first_name) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

name_summary

#### 7. row_to_names() ####

messy_headers <- data.frame(
  X1 = c("Employee Report", "", "ID", 101, 102, 103),
  X2 = c("Generated: 2024", "", "First Name", "Alice", "Bob", "Charlie"),
  X3 = c("", "", "Last Name", "Smith", "Jones", "Brown"),
  X4 = c("", "", "Start Date", 44561, 43831, 44200),
  X5 = c("", "", "Salary", 70000, 80000, 75000),
  stringsAsFactors = FALSE
)

messy_headers

fixed_headers <- messy_headers %>%
  row_to_names(row_number = 3) %>%
  slice(-(1:2))

fixed_headers

#### 8. remove_constant() ####

constant_demo <- cleaned_data %>%
  mutate(constant_col = "same_value")

constant_demo

no_constants <- constant_demo %>%
  remove_constant()

no_constants

