load_append_redcap_mapping <- function(
    mapping_file = here::here("docs", "redcap_append_variable_mapping.csv"),
    parsed_columns = NULL,
    quiet = FALSE,
    keep_unmapped = TRUE
) {
  if (!file.exists(mapping_file)) {
    stop("Append uploader mapping file not found: ", mapping_file)
  }

  mapping_df <- readr::read_csv(
    mapping_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- c(
    "parser_version",
    "uploader_version",
    "source_name",
    "target_redcap_name",
    "update_mode",
    "key_field",
    "notes"
  )

  missing_cols <- setdiff(required_cols, names(mapping_df))
  if (length(missing_cols) > 0) {
    stop(
      "Append uploader mapping is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  mapping_df <- mapping_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ {
          value <- as.character(.x)
          value[is.na(value)] <- ""
          trimws(value)
        }
      )
    ) |>
    dplyr::filter(.data$source_name != "")

  if (!isTRUE(keep_unmapped)) {
    mapping_df <- mapping_df |>
      dplyr::filter(.data$target_redcap_name != "")
  }

  if (!is.null(parsed_columns)) {
    mapping_df <- mapping_df |>
      dplyr::mutate(source_present = .data$source_name %in% parsed_columns)
  } else if (!"source_present" %in% names(mapping_df)) {
    mapping_df <- mapping_df |>
      dplyr::mutate(source_present = NA)
  }

  if (!quiet) {
    message("Append uploader mapping loaded successfully.")
    message("Rows available: ", nrow(mapping_df))
  }

  mapping_df
}
