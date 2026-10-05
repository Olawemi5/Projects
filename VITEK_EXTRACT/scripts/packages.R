#### Package checks ####

required_packages <- c(
  "shiny", "pdftools", "dplyr", "tidyr", "stringr", "purrr", "tibble", "readr",
  "janitor", "lubridate", "REDCapR", "assertthat", "validate", "logger",
  "glue", "fs", "here", "config", "yaml", "openxlsx", "rmarkdown",
  "progress", "bslib", "DT"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install these R packages before running the app or pipeline: ",
    paste(missing_packages, collapse = ", ")
  )
}


#### Loading Packages ####

# PDF extraction
library(pdftools)

# Data wrangling
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(readr)
library(janitor)
library(lubridate)

# Optional table extraction if some PDFs contain table structures
#library(tabulapdf)

# REDCap integration
library(REDCapR)

# Validation and quality checks
library(assertthat)
library(validate)

# Logging and workflow messages
library(logger)
library(glue)

# File and folder management
library(fs)
library(here)

# Optional configuration management
library(config)
library(yaml)

# Optional output/reporting
library(openxlsx)
library(rmarkdown)

# Optional package for progress display during processing
library(progress)
library(bslib)
library(DT)
