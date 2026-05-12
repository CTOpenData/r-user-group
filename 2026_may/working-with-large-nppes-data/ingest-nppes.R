# Download the NPPES Data and stream CT to a parquet with duckdb

# Constants ------------------------------------------------------------

NPPES_LISTING_URL <- "https://download.cms.gov/nppes/NPI_Files.html"
NOW <- lubridate::now()


# resources ------------------------------------------------------------

# rvest package doco: section 'Web scraping 101' https://rvest.tidyverse.org/articles/rvest.html#attributes-1

# Download -------------------------------------------------------------

#' Find the Monthly Download URL for NPPES
#'
#' @param listing_url Website for NPPES download.
#'
#' @returns A url string invisibly.
fetch_monthly_url <- function(listing_url = NPPES_LISTING_URL) {
  page <- rvest::read_html(listing_url)

  url <-
    page |>
    rvest::html_elements("a") |> # anchors
    rvest::html_attr("href") |> # urls
    stringr::str_subset("NPPES_Data_Dissemination.*V2\\.zip") |> # look for the zipped download (should be a monthly and a weekly)
    stringr::str_subset("Weekly", negate = TRUE) |> # ignore the weekly
    head(1)

  if (length(url) == 0) {
    rlang::abort("Could not find monthly NPPES file URL.")
  }

  if (!stringr::str_starts(url, "http")) {
    url <- glue::glue("https://download.cms.gov/nppes/{fs::path_file(url)}")
  }

  cli::cli_alert_success(glue::glue("Found: {url}"))
  url
}

#' Download the the zip file at the NPPES url
#'
#' @param url A string of the url with the zip file to download.
#' @param dest_dir A string of the destination folder for the download.
#'
#' @returns An invisible string of the path to the compressed NPPES download.
download_nppes_zip <- function(url, dest_dir) {
  zip_path <-
    fs::path(dest_dir, fs::path_file(url))

  cli::cli_alert_info("Downloading (~1 GB, this will take a while)...")

  httr2::request(url) |>
    httr2::req_progress() |>
    httr2::req_perform(path = zip_path)

  cli::cli_alert_success(glue::glue("Saved: {zip_path}"))
  invisible(zip_path)
}

#' Extract the Compressed NPPES Data
#'
#' @param zip_path A string of the path to the compressed nppes data.
#' @param dest_dir A string of the path for extracted data destination.
#'
#' @returns A stringr invisibly of the file path to the extracted NPPES data csv.
extract_nppes_zip <- function(zip_path, dest_dir) {
  cli::cli_alert_info("Extracting archive...")
  utils::unzip(zip_path, exdir = dest_dir)

  csv_path <-
    fs::dir_ls(dest_dir, regexp = "npidata_pfile.*\\.csv$") |>
    stringr::str_subset("FileHeader", negate = TRUE) |>
    head(1)

  if (length(csv_path) == 0) {
    stop("Could not locate main NPI CSV after extraction.")
  }

  message(glue::glue("Located: {fs::path_file(csv_path)}"))
  invisible(csv_path)
}

# Streaming to Parquet -------------------------------------------------

#' Stream the NPPES State Data into a Parquet file
#'
#' @description
#' At time of the writing the NPPES API is limited to quick lookups
#' and the file download is the only wait to get all data for a location.
#' However, the data is for the entire United States and it is a massive
#' csv that cannot be loaded fully into memory for most standard DPH machines.
#' `duckdb` is used to stream smaller portions of the csv into a parquet file
#' with tight restrictions of how much memory it can use. This will take a
#' little bit longer, but ensure most machines can run this process. The
#' parquet artifact will be much smaller, and CT only and have a big boost
#' to performance downstream.
#'
#'
#' @param csv_path A string of the file path to the extracted NPPES data csv.
#' @param dest_dir A string of the file path for the parquet data.
#' @param state A string of the state abbreviation - defaults to 'CT' in `main()`.
#'
#' @returns An invisible string
stream_csv_to_state_parquet <-
  function(csv_path, dest_dir, state) {
    state_dir <- fs::path(
      dest_dir,
      glue::glue("nppes_{stringr::str_to_lower(state)}")
    )

    fs::dir_create(state_dir)

    out_path <-
      fs::path(
        state_dir,
        glue::glue("nppes-{lubridate::today()}.parquet")
      )

    cli::cli_alert_info(glue::glue(
      "Streaming via DuckDB → {fs::path_file(out_path)}..."
    ))

    con <- duckdb::dbConnect(duckdb::duckdb(
      config = list(threads = '24', memory_limit = "4GB"),
      dbdir = ":memory:"
    ))

    on.exit(duckdb::dbDisconnect(con, shutdown = TRUE))
    sql <- glue::glue(
      "
    COPY (
      SELECT *
      FROM read_csv(
        '{csv_path}',
        header      = true,
        all_varchar = true,
        buffer_size = 1000000
      )
      WHERE \"Provider Business Practice Location Address State Name\" = '{state}'
        OR \"Provider Business Mailing Address State Name\"           = '{state}'
    )
    TO '{out_path}' (FORMAT parquet)
  "
    )

    DBI::dbExecute(con, sql)

    n <-
      DBI::dbGetQuery(
        con,
        glue::glue("SELECT COUNT(*) AS n FROM '{out_path}'")
      ) |>
      purrr::pluck('n')

    cli::cli_alert_success(
      glue::glue(
        "{format(n, big.mark = ',')} providers written for {state}"
      )
    )
    invisible(out_path)
  }


# Entry point ----------------------------------------------------------

#' Entry Point into NPPES Data Ingestion
main <-
  function(
    dest_dir = "data/nppes",
    state = "CT",
    clean_source = FALSE
  ) {
    cli::cli_alert_info("Starting pipeline {NOW}")
    fs::dir_create(dest_dir, recurse = TRUE)

    url <- fetch_monthly_url()
    zip_path <- download_nppes_zip(url, dest_dir)
    csv_path <- extract_nppes_zip(zip_path, dest_dir)

    out_path <- stream_csv_to_state_parquet(csv_path, dest_dir, state)

    if (clean_source) {
      fs::file_delete(zip_path)
      fs::file_delete(csv_path)
      cli::cli_alert_success("Source files removed.")
    }

    FIN <- lubridate::now()
    cli::cli_alert_success(
      "Pipeline complete - Duration: {prettyunits::pretty_dt(FIN-NOW)}"
    )
    invisible(dest_dir)
  }

main()
