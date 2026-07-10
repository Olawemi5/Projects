# functions/validate_redcap_data.R

validate_redcap_data <- function(redcap_df,
                                 required_fields = c("patient_id"),
                                 key_fields = c("patient_id"),
                                 marker_tests = c(
                                   "esbl",
                                   "cefoxitin_screen",
                                   "inducible_clindamycin_resistance"
                                 ),
                                 parser_version = "v1.0.0",
                                 default_fill = "",
                                 quiet = FALSE) {
  
  # --------------------------------------------------
  # checks
  # --------------------------------------------------
  if (is.null(redcap_df) || !is.data.frame(redcap_df)) {
    stop("redcap_df must be a non-null data frame.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  # --------------------------------------------------
  # local copy
  # --------------------------------------------------
  df <- redcap_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # --------------------------------------------------
  # helper values and functions
  # --------------------------------------------------
  empty_values <- c("", "(Empty)", "(EMPTY)", "empty")
  
  is_empty_value <- function(x) {
    is.na(x) | x %in% empty_values
  }
  
  is_not_empty_value <- function(x) {
    !is_empty_value(x)
  }
  
  # --------------------------------------------------
  # add workflow metadata if missing
  # --------------------------------------------------
  if (!"parser_version" %in% names(df)) {
    df$parser_version <- parser_version
  }
  
  if (!"processed_at" %in% names(df)) {
    df$processed_at <- as.character(Sys.time())
  }
  
  if (!"validation_status" %in% names(df)) {
    df$validation_status <- "Not checked"
  }
  
  # replace NA with default_fill
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.x), default_fill, .x)
      )
    )
  
  # --------------------------------------------------
  # required field checks
  # --------------------------------------------------
  missing_required_columns <- setdiff(required_fields, names(df))
  
  if (length(missing_required_columns) > 0) {
    stop(
      "These required REDCap fields are missing from the dataset: ",
      paste(missing_required_columns, collapse = ", ")
    )
  }
  
  required_field_issues <- df |>
    dplyr::mutate(row_id__ = dplyr::row_number()) |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(required_fields),
        ~ .x %in% empty_values
      )
    ) |>
    dplyr::mutate(issue_type = "Missing required REDCap field")
  
  # --------------------------------------------------
  # duplicate key check
  # --------------------------------------------------
  missing_key_columns <- setdiff(key_fields, names(df))
  
  if (length(missing_key_columns) > 0) {
    stop(
      "These duplicate-check key fields are missing from the dataset: ",
      paste(missing_key_columns, collapse = ", ")
    )
  }
  
  duplicate_key_issues <- df |>
    dplyr::mutate(row_id__ = dplyr::row_number()) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_fields))) |>
    dplyr::mutate(key_n__ = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(key_n__ > 1) |>
    dplyr::mutate(issue_type = "Duplicate REDCap key")
  
  # --------------------------------------------------
  # classify antibiotic result columns
  # --------------------------------------------------
  mic_cols <- names(df)[stringr::str_detect(names(df), "_mic$")]
  
  antibiotic_bases <- mic_cols |>
    stringr::str_replace("_mic$", "") |>
    unique()
  
  # --------------------------------------------------
  # build long validation table for antibiotic fields
  # --------------------------------------------------
  antibiotic_validation_rows <- lapply(antibiotic_bases, function(abx) {
    
    mic_col <- paste0(abx, "_mic")
    raw_col <- paste0(abx, "_interpretation_raw")
    int_col <- paste0(abx, "_interpretation")
    
    if (!all(c(mic_col, raw_col, int_col) %in% names(df))) {
      return(NULL)
    }
    
    tibble::tibble(
      row_id__ = seq_len(nrow(df)),
      antibiotic = abx,
      mic = df[[mic_col]],
      interpretation_raw = df[[raw_col]],
      interpretation = df[[int_col]]
    )
  })
  
  antibiotic_validation_df <- dplyr::bind_rows(antibiotic_validation_rows)
  
  if (nrow(antibiotic_validation_df) == 0) {
    antibiotic_validation_df <- tibble::tibble(
      row_id__ = integer(),
      antibiotic = character(),
      mic = character(),
      interpretation_raw = character(),
      interpretation = character()
    )
  }
  
  antibiotic_validation_df <- antibiotic_validation_df |>
    dplyr::mutate(
      test_class = dplyr::if_else(
        antibiotic %in% marker_tests,
        "marker_test",
        "mic_test"
      )
    )
  
  # --------------------------------------------------
  # accepted values
  # --------------------------------------------------
  valid_mic_raw_values <- c(
    "R", "I", "S",
    "r", "i", "s",
    "empty"
  )
  valid_mic_note_values <- c(
    "Standard", "AES modified", "User modified",
    "standard", "aes_modified", "user_modified",
    "empty"
  )
  
  valid_marker_mic_values <- c("POS", "NEG", "empty")
  valid_marker_raw_values <- c("+", "-", "POS", "NEG", "empty")
  valid_marker_note_values <- c("Standard", "standard", "empty")
  
  # --------------------------------------------------
  # validation rules for MIC-based tests
  # --------------------------------------------------
  mic_test_issues <- antibiotic_validation_df |>
    dplyr::filter(test_class == "mic_test") |>
    dplyr::filter(
      # flag only when MIC is empty but interpretation is present
      # do NOT flag when MIC is present and interpretation is empty,
      # because empty interpretation is now acceptable in REDCap
      is_empty_value(mic) & is_not_empty_value(interpretation_raw)
    ) |>
    dplyr::mutate(issue_type = "MIC/interpretation mismatch in MIC-based test")
  
  invalid_interp_mic_tests <- antibiotic_validation_df |>
    dplyr::filter(test_class == "mic_test") |>
    dplyr::filter(
      is_not_empty_value(interpretation_raw) &
        !interpretation_raw %in% valid_mic_raw_values
    ) |>
    dplyr::mutate(issue_type = "Invalid interpretation_raw for MIC-based test")
  
  invalid_interp_note_mic_tests <- antibiotic_validation_df |>
    dplyr::filter(test_class == "mic_test") |>
    dplyr::filter(
      is_not_empty_value(interpretation) &
        !interpretation %in% valid_mic_note_values
    ) |>
    dplyr::mutate(issue_type = "Invalid interpretation note for MIC-based test")
  
  # --------------------------------------------------
  # validation rules for marker tests
  # --------------------------------------------------
  marker_test_issues <- antibiotic_validation_df |>
    dplyr::filter(test_class == "marker_test") |>
    dplyr::filter(
      is_not_empty_value(mic) &
        !mic %in% valid_marker_mic_values
    ) |>
    dplyr::mutate(issue_type = "Invalid marker-test mic value")
  
  marker_test_interp_issues <- antibiotic_validation_df |>
    dplyr::filter(test_class == "marker_test") |>
    dplyr::filter(
      is_not_empty_value(interpretation_raw) &
        !interpretation_raw %in% valid_marker_raw_values
    ) |>
    dplyr::mutate(issue_type = "Invalid marker-test interpretation_raw value")
  
  marker_test_note_issues <- antibiotic_validation_df |>
    dplyr::filter(test_class == "marker_test") |>
    dplyr::filter(
      is_not_empty_value(interpretation) &
        !interpretation %in% valid_marker_note_values
    ) |>
    dplyr::mutate(issue_type = "Invalid marker-test interpretation note")
  
  # --------------------------------------------------
  # empty row check
  # --------------------------------------------------
  nontechnical_cols <- setdiff(
    names(df),
    c("parser_version", "processed_at", "validation_status")
  )
  
  empty_row_issues <- df |>
    dplyr::mutate(row_id__ = dplyr::row_number()) |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(nontechnical_cols),
        ~ .x %in% empty_values
      )
    ) |>
    dplyr::mutate(issue_type = "Completely empty row")
  
  # --------------------------------------------------
  # combine issues
  # --------------------------------------------------
  issue_tables <- list(
    required_field_issues,
    duplicate_key_issues,
    mic_test_issues,
    invalid_interp_mic_tests,
    invalid_interp_note_mic_tests,
    marker_test_issues,
    marker_test_interp_issues,
    marker_test_note_issues,
    empty_row_issues
  )
  
  flagged_issues <- dplyr::bind_rows(issue_tables) |>
    dplyr::select(
      dplyr::any_of(c(
        "row_id__",
        "issue_type",
        "antibiotic",
        "mic",
        "interpretation_raw",
        "interpretation",
        "test_class"
      )),
      dplyr::everything()
    ) |>
    dplyr::arrange(row_id__, issue_type)
  
  flagged_row_ids <- unique(flagged_issues$row_id__)
  
  # --------------------------------------------------
  # assign validation status
  # --------------------------------------------------
  df <- df |>
    dplyr::mutate(row_id__ = dplyr::row_number()) |>
    dplyr::mutate(
      validation_status = dplyr::if_else(
        row_id__ %in% flagged_row_ids,
        "Flagged",
        "Pass"
      )
    )
  
  # --------------------------------------------------
  # create flagged rows dataset
  # --------------------------------------------------
  flagged_rows <- df |>
    dplyr::filter(row_id__ %in% flagged_row_ids) |>
    dplyr::select(-row_id__)
  
  # --------------------------------------------------
  # summary table
  # --------------------------------------------------
  summary_tbl <- tibble::tibble(
    metric = c(
      "n_rows_input",
      "n_rows_pass",
      "n_rows_flagged",
      "n_issue_records",
      "n_missing_required_field_issues",
      "n_duplicate_key_issues",
      "n_mic_test_issues",
      "n_invalid_interp_mic_tests",
      "n_invalid_interp_note_mic_tests",
      "n_marker_test_issues",
      "n_marker_test_interp_issues",
      "n_marker_test_note_issues",
      "n_empty_row_issues"
    ),
    value = c(
      nrow(df),
      sum(df$validation_status == "Pass"),
      sum(df$validation_status == "Flagged"),
      nrow(flagged_issues),
      nrow(required_field_issues),
      nrow(duplicate_key_issues),
      nrow(mic_test_issues),
      nrow(invalid_interp_mic_tests),
      nrow(invalid_interp_note_mic_tests),
      nrow(marker_test_issues),
      nrow(marker_test_interp_issues),
      nrow(marker_test_note_issues),
      nrow(empty_row_issues)
    )
  )
  
  # --------------------------------------------------
  # upload-ready dataset
  # --------------------------------------------------
  upload_ready <- df |>
    dplyr::filter(validation_status == "Pass") |>
    dplyr::select(-row_id__)
  
  overall_status <- if (nrow(flagged_rows) == 0) "Pass" else "Flagged"
  
  if (!quiet) {
    message("Validation completed.")
    message("Rows input: ", nrow(df))
    message("Rows passed: ", sum(df$validation_status == "Pass"))
    message("Rows flagged: ", sum(df$validation_status == "Flagged"))
    message("Overall status: ", overall_status)
  }
  
  return(list(
    overall_status = overall_status,
    upload_ready = upload_ready,
    validated_data = df |> dplyr::select(-row_id__),
    flagged_rows = flagged_rows,
    flagged_issues = flagged_issues,
    summary = summary_tbl
  ))
}