# ==================================================
# functions/read_pdf_manifest.R
# Read PDF manifest from docs/files_to_process.txt
# ==================================================

read_pdf_manifest <- function(
    manifest_file = here::here("docs", "files_to_process.txt"),
    data_raw_dir = here::here("data_raw"),
    quiet = FALSE
) {
  resolved_data_raw_dir <- data_raw_dir

  if (!dir.exists(resolved_data_raw_dir)) {
    fallback_data_raw_dir <- "data_raw"
    if (dir.exists(fallback_data_raw_dir)) {
      resolved_data_raw_dir <- fallback_data_raw_dir
    } else {
      stop("data_raw directory not found: ", data_raw_dir)
    }
  }

  if (!file.exists(manifest_file)) {
    if (!quiet) {
      message("Manifest file not found. Returning an empty manifest: ", manifest_file)
    }

    return(tibble::tibble(
      source_file = character(),
      file_path = character(),
      file_exists = logical()
    ))
  }

  manifest_ext <- tolower(tools::file_ext(manifest_file))

  if (manifest_ext == "csv") {
    manifest_df <- readr::read_csv(
      manifest_file,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )

    if (!"file_path" %in% names(manifest_df)) {
      return(tibble::tibble(
        source_file = character(),
        file_path = character(),
        file_exists = logical()
      ))
    }

    if (!"source_file" %in% names(manifest_df)) {
      manifest_df$source_file <- basename(manifest_df$file_path)
    }

    manifest_df$file_exists <- file.exists(manifest_df$file_path)

    if (!quiet) {
      message("CSV manifest loaded successfully.")
      message("Records found: ", nrow(manifest_df))
    }

    return(manifest_df)
  }

  file_list <- readLines(manifest_file, warn = FALSE)
  file_list <- trimws(file_list)
  file_list <- file_list[file_list != ""]
  file_list <- file_list[!grepl("^#", file_list)]
  file_list <- unique(file_list)
  file_list <- sort(file_list)

  manifest_df <- data.frame(
    source_file = file_list,
    file_path = file.path(resolved_data_raw_dir, file_list),
    stringsAsFactors = FALSE
  )

  manifest_df$file_exists <- file.exists(manifest_df$file_path)

  if (!quiet) {
    message("Text manifest loaded successfully.")
    message("Files listed: ", nrow(manifest_df))
    message("Files found in data_raw: ", sum(manifest_df$file_exists))
    message("Files missing: ", sum(!manifest_df$file_exists))
  }

  manifest_df
}
