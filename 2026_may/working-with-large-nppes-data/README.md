# NPPES Data Ingest: Handling Large CSVs in R

## What is CMS and NPPES?

The **Centers for Medicare & Medicaid Services (CMS)** is a federal agency within the United States Department of Health and Human Services. One of their key responsibilities is maintaining the **National Plan and Provider Enumeration System (NPPES)**.

NPPES assigns unique identifiers, known as **National Provider Identifiers (NPIs)**, to health care providers and organizations. This database is the "gold standard" for identifying who is providing care in the U.S. health system. You can learn more about [CMS Data Dissemination here](https://www.cms.gov/medicare/regulations-guidance/administrative-simplification/data-dissemination).

## The Problem: "Out of Memory" and API Limits

For most beginner to intermediate R users, the `tidyverse` is the go-to for data manipulation. However, the `tidyverse` generally expects your data to fit into your computer's RAM. 

> When dealing with an 11GB file on a machine with 8GB or 16GB of RAM, you will hit a wall.

### Why not just use the API?
CMS provides an [NPI Registry Search API](https://npiregistry.cms.hhs.gov/search), but it is **not feasible for bulk data work**. The API is designed for quick, individual lookups (e.g., "What is Dr. Smith's NPI?"). If you need to analyze every provider in a specific state or specialty, the API's rate limits and results-per-query caps make it impossible to use. 

This leaves us with one option: downloading the massive "Full Replacement File".

Even if you have enough RAM, the NPPES file is **sparse**—it has hundreds of columns, many of which are empty for most providers. Reading all that "nothing" is a waste of time and resources.

## The Solution: DuckDB + Parquet

This project sidesteps the memory limit by using two powerful tools:

1.  **[DuckDB](https://duckdb.org/):** An analytical database that can "stream" data. Instead of loading the whole CSV into R, DuckDB reads it in chunks, filters for the data we actually want (e.g., just Connecticut providers), and writes it directly to a new file.
2.  **[Parquet](https://parquet.apache.org/):** A columnar storage format. Unlike a CSV, Parquet is compressed and highly efficient. A multi-gigabyte CSV can often be reduced to a few hundred megabytes as a Parquet file, while keeping all the data types intact.

## Getting Started

### 1. Setup the Environment
This project uses `renv` to manage R packages. This ensures you have the exact versions of `duckdb`, `httr2`, and other dependencies needed.

```r
# Open R and run:
renv::restore()
```

```r
# and select #1
It looks like you've called renv::restore() in a project that hasn't been activated yet.
How would you like to proceed? 

1: Activate the project and use the project library.
2: Do not activate the project and use the current library paths.
3: Cancel and resolve the situation another way.

```

### 2. Run the Ingest Pipeline
The main logic is contained in `ingest-nppes.R`. You can run the entire process (download, extract, and convert) by sourcing `ingest-nppes.R`.

```r
source("ingest-nppes.R", echo=FALSE)
```

## How It Works

The script follows a "Research -> Strategy -> Execution" lifecycle:

1.  **Scrape:** It visits the CMS website to find the latest monthly download link.
2.  **Download:** It uses `httr2` to download the ~1GB zip file.
3.  **Extract:** It unzips the file to locate the main `npidata_pfile_xxx.csv`.
4.  **Stream (The Magic):** It uses a SQL query within DuckDB to read the CSV from disk and write it to Parquet. **R never actually "holds" the full dataset in memory.**

```sql
COPY (
  SELECT * FROM read_csv('path/to/huge.csv')
  WHERE State = 'CT'
) TO 'output.parquet' (FORMAT parquet);
```

## Why this is better for you

-   **Speed:** DuckDB is incredibly fast at reading CSVs.
-   **Stability:** Your R session won't crash.
-   **Efficiency:** The resulting `.parquet` file is much smaller and ready for high-performance analysis using the `arrow` or `duckdb` packages.

## Next Steps: Analyzing the Data

Once you have your `.parquet` file, you can analyze it without loading the whole thing:

```r
library(arrow)
library(dplyr)

nppes_data <- open_dataset("data/nppes/nppes_ct/")

nppes_data |>
  filter(`Provider Business Practice Location Address City Name` == "HARTFORD") |>
  collect() # Only now does the filtered data enter your RAM
```

---
*Data Source: [CMS NPPES Data Dissemination](https://download.cms.gov/nppes/NPI_Files.html)*
