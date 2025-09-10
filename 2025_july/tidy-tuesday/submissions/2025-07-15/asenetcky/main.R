# EV Registrations

## Env
### Using `renv` but trying to make as `renv` agnostic as possible
library(dplyr) # for data wrangling
library(glue) # for string literals
library(httr2) # for http requests
library(ggplot2) # for data visualizations
library(nanoparquet) # low dependancy parquet files
library(fs) # file manipulations
library(lubridate) # date manipulations
library(forcats) # for factors
library(purrr) # for functional programming

## Data Pull
domain <- "https://data.ct.gov/"
resource <- "resource/y7ky-5wcz.json"

### too lazy to loop through - just going to grab > 70k (should be enough)
### API uses LIMITS and OFFSETS
limit <- 70000L
limit_string <- glue::glue("$limit={limit}")

### combine it all
endpoint <- glue::glue(
    "{domain}{resource}?{limit_string}"
)

### GET request
req <- request(endpoint)
req |> req_dry_run()

### GET response
path <- path_wd("data", "ev-registrations", ext = "parquet")

if (!file_exists(path)) {
    resp <- req_perform(req)

    ### convert resp to data we can use
    data <-
        resp |>
        resp_body_json() |>
        bind_rows() |>
        ### enforcing column types from ODP doco
        mutate(
            across(
                .cols = c(
                    id,
                    vehicleweight,
                    vehicleyear,
                    vehicledeclaredgrossweight,
                    vehiclerecordedgvwr,
                ),
                .fns = as.numeric
            ),
            across(
                .cols = c(
                    registration_date_start,
                    registration_date_expiration
                ),
                .fns = as_date
            )
        )

    ### parquet this for later
    write_parquet(data, path)
}

data <- read_parquet(path)

## Data Exploration

### take a look
glimpse(data)

char_data <-
    data |>
    select(where(is.character))

### sussing out good factor candidates
maybe_factors <-
    map(
        char_data,
        \(var) {
            count(char_data, {{ var }}, sort = TRUE)
        }
    ) |>
    keep(\(df) nrow(df) < 100) |>
    names()

data <-
    data |>
    mutate(
        across(
            .cols = all_of(maybe_factors),
            .fns = \(col) {
                col |>
                    forcats::as_factor() |>
                    forcats::fct_infreq()
            }
        )
    )

data |>
    select(all_of(maybe_factors)) |>
    summary()

## Visualizations

### add year, quarter and factor lump
agg_by_year_quarter_type <-
    data |>
    mutate(
        year = year(registration_date_start),
        quarter = quarter(registration_date_start),
        vehicletype = forcats::fct_lump_prop(
            vehicletype,
            prop = .01
        )
    ) |>
    group_by(year, quarter, vehicletype) |>
    count(name = "type_count") |>
    ungroup() |>
    mutate(log_type = log(type_count))

### column chart
fig_agg_by_year_quarter_type <-
    agg_by_year_quarter_type |>
    ggplot() +
    geom_col(
        aes(
            quarter,
            log_type,
            fill = vehicletype
        ),
        position = position_dodge(),
        col = "black"
    ) +
    scale_fill_viridis_d() +
    theme_classic() +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
    ) +
    labs(
        x = "",
        y = "",
        fill = ""
    )

fig_agg_by_year_quarter_type +
    facet_grid(cols = vars(year)) +
    labs(title = "Natural Log of Vehicle Type Count by Quarter by Year")


### output
figure <-
    fig_agg_by_year_quarter_type +
    facet_grid(cols = vars(year)) +
    labs(title = "Natural Log of Vehicle Type Count by Quarter by Year")


ggsave(
    filename = fs::path_wd(
        "agg_type_year_quarter",
        ext = "png"
    ),
    plot = figure
)
