run_shiny_pipeline <- function(uploaded_paths,
                               uploaded_names = basename(uploaded_paths),
                               parser_version = "v1.0.0") {
  settings <- list(
    parser_version = parser_version,
    mapping_file = file.path("docs", "redcap_variable_mapping.csv"),
    upload_required_fields = c("patient_id"),
    upload_key_fields = c("patient_id"),
    marker_tests = c("esbl", "cefoxitin_screen", "inducible_clindamycin_resistance"),
    batch_size_upload = 100
  )

  run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_output_dir <- file.path("output", "runs", paste0("shiny_run_", run_timestamp))
  dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run_output_dir, "review"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run_output_dir, "processed"), recursive = TRUE, showWarnings = FALSE)

  audit_log_file <- file.path(run_output_dir, paste0("audit_", run_timestamp, ".csv"))
  initialize_audit_log(audit_log_file)

  input_manifest <- tibble::tibble(
    upload_index = seq_along(uploaded_paths),
    source_file = uploaded_names,
    source_path = normalizePath(uploaded_paths, winslash = "/", mustWork = FALSE)
  )
  readr::write_csv(input_manifest, file.path(run_output_dir, "input_files_manifest.csv"))

  append_audit_log(audit_log_file, "pipeline_start", "success", sprintf("Processing %s uploaded file(s).", length(uploaded_paths)))

  parsed_rows <- lapply(seq_along(uploaded_paths), function(i) {
    tryCatch({
      pdf_obj <- read_vitek_pdf_full(
        file_path = uploaded_paths[[i]],
        output_dir = file.path(run_output_dir, "processed"),
        output_file = paste0(tools::file_path_sans_ext(uploaded_names[[i]]), "_raw_text.txt"),
        quiet = TRUE
      )

      parsed <- parse_ast_chart_report_full(pdf_obj, quiet = TRUE)
      parsed$ast_wide |>
        dplyr::mutate(
          source_file = uploaded_names[[i]],
          processed_at = as.character(Sys.time()),
          parser_version = settings$parser_version
        ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    }, error = function(e) {
      failing_file <- uploaded_names[[i]]
      failing_path <- normalizePath(uploaded_paths[[i]], winslash = "/", mustWork = FALSE)
      failure_message <- sprintf(
        "Failed while parsing file %s (%s): %s",
        failing_file,
        failing_path,
        conditionMessage(e)
      )
      append_audit_log(audit_log_file, "parse_ast_chart_report_full", "error", failure_message)
      stop(failure_message, call. = FALSE)
    })
  })

  parsed_wide <- dplyr::bind_rows(parsed_rows)
  append_audit_log(audit_log_file, "parse_ast_chart_report_full", "success", sprintf("Parsed %s row(s).", nrow(parsed_wide)))

  readr::write_csv(parsed_wide, file.path(run_output_dir, "parsed_wide.csv"))

  parsed_wide_clean <- clean_data(parsed_wide, quiet = TRUE)
  append_audit_log(audit_log_file, "clean_data", "success", sprintf("Collapsed parsed rows from %s to %s row(s).", nrow(parsed_wide), nrow(parsed_wide_clean)))
  readr::write_csv(parsed_wide_clean, file.path(run_output_dir, "parsed_wide_clean.csv"))

  redcap_ready <- map_redcap(
    parsed_df = parsed_wide_clean,
    mapping_file = settings$mapping_file,
    required_redcap_fields = settings$upload_required_fields,
    default_fill = "",
    quiet = TRUE
  ) |>
    dplyr::mutate(redcap_event_name = "microbiology") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  append_audit_log(audit_log_file, "map_redcap", "success", sprintf("Mapped %s row(s).", nrow(redcap_ready)))

  # Avoid a second isolate-collapse pass after mapping. At this stage we only
  # want the normalization/cleanup behavior before validation.
  redcap_ready_clean <- clean_data(
    redcap_ready,
    quiet = TRUE,
    collapse_patient_isolates = FALSE
  )

  validation_result <- validate_redcap_data(
    redcap_df = redcap_ready_clean,
    required_fields = settings$upload_required_fields,
    key_fields = settings$upload_key_fields,
    marker_tests = settings$marker_tests,
    parser_version = settings$parser_version,
    quiet = TRUE
  )

  append_audit_log(audit_log_file, "validate_redcap_data", validation_result$overall_status, sprintf("Pass rows: %s. Flagged rows: %s.", nrow(validation_result$upload_ready), nrow(validation_result$flagged_rows)))

  flagged_rows <- validation_result$flagged_rows
  review_file <- NA_character_
  if (nrow(flagged_rows) > 0) {
    review_file <- file.path(run_output_dir, "review", paste0("flagged_rows_", run_timestamp, ".csv"))
    readr::write_csv(flagged_rows, review_file)
  }

  upload_ready <- validation_result$upload_ready |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  readr::write_csv(upload_ready, file.path(run_output_dir, "upload_ready.csv"))

  dir.create(file.path("output", "cache"), recursive = TRUE, showWarnings = FALSE)
  manifest_file <- file.path("output", "cache", "pdf_manifest.csv")
  manifest_existing <- read_pdf_manifest(
    manifest_file = manifest_file,
    data_raw_dir = "data_raw",
    quiet = TRUE
  )
  manifest_new <- tibble::tibble(
    file_path = normalizePath(uploaded_paths, winslash = "/", mustWork = FALSE),
    file_name = uploaded_names,
    processed = "TRUE"
  )
  manifest_all <- dplyr::bind_rows(manifest_existing, manifest_new) |>
    dplyr::distinct(.data$file_path, .keep_all = TRUE)
  readr::write_csv(manifest_all, manifest_file)

  append_audit_log(audit_log_file, "pipeline_complete", "success", "Shiny pipeline completed.")

  list(
    input_files = uploaded_names,
    parsed_wide = parsed_wide,
    parsed_wide_clean = parsed_wide_clean,
    redcap_ready = redcap_ready_clean,
    validation_result = validation_result,
    upload_ready = upload_ready,
    flagged_rows = flagged_rows,
    review_file = review_file,
    audit_log_file = audit_log_file,
    run_output_dir = run_output_dir,
    settings = settings
  )
}
