prepare_append_trial <- function(redcap_df,
                                 mapping_file = here::here("docs", "redcap_append_variable_mapping.csv"),
                                 config_file = here::here("config", "redcap_config.yml"),
                                 redcap_uri = NULL,
                                 keyring_service = NULL,
                                 keyring_username = NULL,
                                 output_dir = here::here("output"),
                                 trial_prefix = "append_trial",
                                 quiet = FALSE) {
  if (is.null(redcap_df) || !is.data.frame(redcap_df)) {
    stop("redcap_df must be a non-null data frame.")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")

  if (!exists("load_append_redcap_mapping", mode = "function")) {
    stop("load_append_redcap_mapping() is not available.")
  }

  if ((is.null(redcap_uri) || is.null(keyring_service) || is.null(keyring_username)) &&
      !exists("load_redcap_config", mode = "function")) {
    stop("load_redcap_config() is not available. Source functions/load_redcap_config.R first.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(redcap_uri) || redcap_uri == "" ||
      is.null(keyring_service) || keyring_service == "" ||
      is.null(keyring_username) || keyring_username == "") {
    cfg <- load_redcap_config(config_file = config_file)

    redcap_uri <- if (is.null(redcap_uri) || redcap_uri == "") cfg$redcap_uri else redcap_uri
    keyring_service <- if (is.null(keyring_service) || keyring_service == "") cfg$keyring_service else keyring_service
    keyring_username <- if (is.null(keyring_username) || keyring_username == "") cfg$keyring_username else keyring_username
  }

  append_mapping <- load_append_redcap_mapping(
    mapping_file = mapping_file,
    parsed_columns = names(redcap_df),
    quiet = quiet
  )

  normalized_upload <- redcap_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  mapped_rows <- append_mapping |>
    dplyr::mutate(
      source_present = .data$source_name %in% names(normalized_upload),
      key_field = tolower(.data$key_field) %in% c("true", "t", "1", "yes", "y"),
      has_target = .data$target_redcap_name != ""
    )

  key_rows <- mapped_rows |>
    dplyr::filter(.data$key_field)

  mapped_key_rows <- key_rows |>
    dplyr::filter(.data$has_target)

  duplicate_targets <- mapped_rows |>
    dplyr::filter(.data$has_target) |>
    dplyr::count(.data$target_redcap_name, name = "n_sources") |>
    dplyr::filter(.data$n_sources > 1)

  missing_source_rows <- mapped_rows |>
    dplyr::filter(.data$has_target, !.data$source_present)

  missing_key_source_rows <- key_rows |>
    dplyr::filter(!.data$source_present)

  empty_key_target_rows <- key_rows |>
    dplyr::filter(!.data$has_target)

  blocking_issues <- tibble::tibble(
    issue_type = character(),
    severity = character(),
    detail = character()
  )

  warnings_tbl <- tibble::tibble(
    issue_type = character(),
    severity = character(),
    detail = character()
  )

  add_issue <- function(tbl, issue_type, severity, detail) {
    dplyr::bind_rows(
      tbl,
      tibble::tibble(
        issue_type = issue_type,
        severity = severity,
        detail = detail
      )
    )
  }

  if (nrow(mapped_rows) == 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "empty_mapping",
      "blocking",
      "Append mapping file contains no source rows."
    )
  }

  if (!file.exists(mapping_file)) {
    blocking_issues <- add_issue(
      blocking_issues,
      "mapping_file_missing",
      "blocking",
      paste("Append mapping file not found:", mapping_file)
    )
  }

  if (nrow(mapped_rows |> dplyr::filter(.data$has_target)) == 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "no_target_mappings",
      "blocking",
      "No target_redcap_name values are populated in the append mapping."
    )
  }

  if (nrow(key_rows) == 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "no_key_field",
      "blocking",
      "No key_field rows were defined in the append mapping."
    )
  }

  if (nrow(mapped_key_rows) == 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "no_mapped_key_field",
      "blocking",
      "No key_field row has a populated target_redcap_name."
    )
  }

  if (nrow(empty_key_target_rows) > 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "empty_key_target",
      "blocking",
      paste(
        "Key fields without target_redcap_name:",
        paste(empty_key_target_rows$source_name, collapse = ", ")
      )
    )
  }

  if (nrow(missing_key_source_rows) > 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "missing_key_source_in_upload",
      "blocking",
      paste(
        "Key source fields missing from upload-ready data:",
        paste(missing_key_source_rows$source_name, collapse = ", ")
      )
    )
  }

  if (nrow(duplicate_targets) > 0) {
    blocking_issues <- add_issue(
      blocking_issues,
      "duplicate_target_redcap_name",
      "blocking",
      paste(
        "Duplicate mapped target_redcap_name values:",
        paste(duplicate_targets$target_redcap_name, collapse = ", ")
      )
    )
  }

  if (nrow(missing_source_rows) > 0) {
    warnings_tbl <- add_issue(
      warnings_tbl,
      "missing_source_in_upload",
      "warning",
      paste(
        "Mapped source fields not present in upload-ready data:",
        paste(missing_source_rows$source_name, collapse = ", ")
      )
    )
  }

  processed_at_file <- format(Sys.time(), "%Y%m%d_%H%M%S")

  summary_tbl <- tibble::tibble(
    timestamp = as.character(Sys.time()),
    trial_ready = nrow(blocking_issues) == 0,
    n_input_rows = nrow(normalized_upload),
    n_input_columns = ncol(normalized_upload),
    n_mapping_rows = nrow(mapped_rows),
    n_mapped_rows = nrow(mapped_rows |> dplyr::filter(.data$has_target)),
    n_key_rows = nrow(key_rows),
    n_mapped_key_rows = nrow(mapped_key_rows),
    n_duplicate_targets = nrow(duplicate_targets),
    n_missing_sources = nrow(missing_source_rows),
    redcap_uri = redcap_uri,
    keyring_service = keyring_service,
    keyring_username = keyring_username,
    mapping_file = mapping_file
  )

  summary_file <- file.path(
    output_dir,
    paste0(trial_prefix, "_summary_", processed_at_file, ".csv")
  )
  blocking_file <- file.path(
    output_dir,
    paste0(trial_prefix, "_blocking_issues_", processed_at_file, ".csv")
  )
  warnings_file <- file.path(
    output_dir,
    paste0(trial_prefix, "_warnings_", processed_at_file, ".csv")
  )
  mapped_fields_file <- file.path(
    output_dir,
    paste0(trial_prefix, "_mapped_fields_", processed_at_file, ".csv")
  )
  staging_file <- file.path(
    output_dir,
    paste0(trial_prefix, "_upload_ready_", processed_at_file, ".csv")
  )

  readr::write_csv(summary_tbl, summary_file)
  readr::write_csv(blocking_issues, blocking_file)
  readr::write_csv(warnings_tbl, warnings_file)
  readr::write_csv(mapped_rows, mapped_fields_file)
  readr::write_csv(normalized_upload, staging_file)

  if (!quiet) {
    if (summary_tbl$trial_ready[[1]]) {
      message("Append trial setup is ready.")
    } else {
      message("Append trial setup is blocked. Review generated issue files.")
    }
  }

  list(
    trial_ready = summary_tbl$trial_ready[[1]],
    status = if (summary_tbl$trial_ready[[1]]) "Append trial setup ready" else "Append trial setup blocked",
    summary = summary_tbl,
    blocking_issues = blocking_issues,
    warnings = warnings_tbl,
    mapped_fields = mapped_rows,
    config_used = list(
      redcap_uri = redcap_uri,
      keyring_service = keyring_service,
      keyring_username = keyring_username
    ),
    summary_file = summary_file,
    blocking_issues_file = blocking_file,
    warnings_file = warnings_file,
    mapped_fields_file = mapped_fields_file,
    staging_file = staging_file
  )
}
