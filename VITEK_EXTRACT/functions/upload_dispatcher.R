upload_redcap_dispatcher <- function(uploader_version = "v1.0.0",
                                     redcap_df,
                                     config_file = here::here("config", "redcap_config.yml"),
                                     mapping_file = here::here("docs", "redcap_append_variable_mapping.csv"),
                                     redcap_uri = NULL,
                                     keyring_service = NULL,
                                     keyring_username = NULL,
                                     required_fields = c(
                                       "record_id",
                                       "redcap_event_name",
                                       "redcap_repeat_instrument",
                                       "redcap_repeat_instance"
                                     ),
                                     key_fields = c(
                                       "record_id",
                                       "redcap_event_name",
                                       "redcap_repeat_instrument",
                                       "redcap_repeat_instance"
                                     ),
                                     marker_tests = c(
                                       "esbl",
                                       "cefoxitin_screen",
                                       "inducible_clindamycin_resistance"
                                     ),
                                     parser_version = "v1.0.0",
                                     default_fill = "",
                                     dry_run = FALSE,
                                     output_dir = here::here("output"),
                                     batch_size = 100,
                                     save_staging = TRUE,
                                     quiet = FALSE) {
  uploader_fn <- get_uploader_function(uploader_version)

  uploader_fn(
    redcap_df = redcap_df,
    config_file = config_file,
    mapping_file = mapping_file,
    redcap_uri = redcap_uri,
    keyring_service = keyring_service,
    keyring_username = keyring_username,
    required_fields = required_fields,
    key_fields = key_fields,
    marker_tests = marker_tests,
    parser_version = parser_version,
    default_fill = default_fill,
    dry_run = dry_run,
    output_dir = output_dir,
    batch_size = batch_size,
    save_staging = save_staging,
    quiet = quiet
  )
}

get_uploader_function <- function(uploader_version) {
  switch(
    uploader_version,
    "v1.0.0" = upload_redcap_v1_0_0,
    "v1.1.0" = upload_redcap_v1_1_0_append,
    stop("Unsupported uploader version: ", uploader_version)
  )
}

upload_redcap_v1_0_0 <- function(redcap_df,
                                 config_file = here::here("config", "redcap_config.yml"),
                                 mapping_file = NULL,
                                 redcap_uri = NULL,
                                 keyring_service = NULL,
                                 keyring_username = NULL,
                                 required_fields = c(
                                   "record_id",
                                   "redcap_event_name",
                                   "redcap_repeat_instrument",
                                   "redcap_repeat_instance"
                                 ),
                                 key_fields = c(
                                   "record_id",
                                   "redcap_event_name",
                                   "redcap_repeat_instrument",
                                   "redcap_repeat_instance"
                                 ),
                                 marker_tests = c(
                                   "esbl",
                                   "cefoxitin_screen",
                                   "inducible_clindamycin_resistance"
                                 ),
                                 parser_version = "v1.0.0",
                                 default_fill = "",
                                 dry_run = FALSE,
                                 output_dir = here::here("output"),
                                 batch_size = 100,
                                 save_staging = TRUE,
                                 quiet = FALSE) {
  upload_redcap(
    redcap_df = redcap_df,
    config_file = config_file,
    redcap_uri = redcap_uri,
    keyring_service = keyring_service,
    keyring_username = keyring_username,
    required_fields = required_fields,
    key_fields = key_fields,
    marker_tests = marker_tests,
    parser_version = parser_version,
    default_fill = default_fill,
    dry_run = dry_run,
    output_dir = output_dir,
    batch_size = batch_size,
    save_staging = save_staging,
    quiet = quiet
  )
}

to_logical_flag <- function(x) {
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

is_append_empty_value <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

merge_append_payload_duplicates <- function(payload,
                                            key_target_fields,
                                            quiet = FALSE) {
  if (is.null(payload) || !is.data.frame(payload) || nrow(payload) == 0) {
    return(list(
      payload = payload,
      duplicate_summary = tibble::tibble(),
      conflict_details = tibble::tibble(),
      merged_row_count = 0L
    ))
  }

  key_target_fields <- intersect(key_target_fields, names(payload))
  if (length(key_target_fields) == 0) {
    stop("No key target fields are available in the append payload.")
  }

  payload_chr <- payload |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(.append_row_id__ = seq_len(dplyr::n()))

  payload_key <- apply(
    payload_chr[, key_target_fields, drop = FALSE],
    1,
    function(row) paste(trimws(as.character(row)), collapse = "||")
  )

  payload_chr$.append_group_key__ <- payload_key

  duplicate_summary <- payload_chr |>
    dplyr::count(.data$.append_group_key__, name = "n_rows") |>
    dplyr::filter(.data$n_rows > 1)

  if (nrow(duplicate_summary) == 0) {
    return(list(
      payload = payload,
      duplicate_summary = duplicate_summary,
      conflict_details = tibble::tibble(),
      merged_row_count = 0L
    ))
  }

  payload_groups <- split(payload_chr, payload_chr$.append_group_key__)
  merged_rows <- vector("list", length(payload_groups))
  conflict_details <- vector("list", length(payload_groups))

  group_names <- names(payload_groups)

  for (i in seq_along(payload_groups)) {
    group_df <- payload_groups[[i]]
    merged_row <- group_df[1, , drop = FALSE]

    group_conflicts <- list()

    for (col_name in names(payload)) {
      values <- as.character(group_df[[col_name]])
      values[is.na(values)] <- ""
      values <- trimws(values)
      non_empty_unique <- unique(values[values != ""])

      if (length(non_empty_unique) <= 1) {
        merged_row[[col_name]] <- if (length(non_empty_unique) == 0) {
          NA_character_
        } else {
          non_empty_unique[[1]]
        }
      } else {
        group_conflicts[[length(group_conflicts) + 1]] <- tibble::tibble(
          append_group_key = group_names[[i]],
          target_redcap_name = col_name,
          conflicting_values = paste(non_empty_unique, collapse = " | "),
          source_row_ids = paste(group_df$.append_row_id__, collapse = ",")
        )
      }
    }

    merged_rows[[i]] <- merged_row
    conflict_details[[i]] <- if (length(group_conflicts) > 0) {
      dplyr::bind_rows(group_conflicts)
    } else {
      tibble::tibble()
    }
  }

  conflict_details <- dplyr::bind_rows(conflict_details)

  if (nrow(conflict_details) > 0) {
    return(list(
      payload = payload,
      duplicate_summary = duplicate_summary,
      conflict_details = conflict_details,
      merged_row_count = 0L
    ))
  }

  merged_payload <- dplyr::bind_rows(merged_rows) |>
    dplyr::select(-dplyr::any_of(c(".append_row_id__", ".append_group_key__")))

  merged_row_count <- nrow(payload) - nrow(merged_payload)

  if (!quiet) {
    message(
      "Merged ",
      merged_row_count,
      " duplicate append payload row(s) across ",
      nrow(duplicate_summary),
      " final REDCap key group(s)."
    )
  }

  list(
    payload = merged_payload,
    duplicate_summary = duplicate_summary,
    conflict_details = conflict_details,
    merged_row_count = merged_row_count
  )
}

build_append_redcap_payload <- function(redcap_df,
                                        append_mapping,
                                        quiet = FALSE) {
  organism_lookup_source <- function(source_name) {
    suffix <- stringr::str_match(source_name, "(_[0-9]+)$")[, 2]
    if (is.na(suffix) || !nzchar(suffix)) {
      return("identified_organism")
    }

    paste0("identified_organism", suffix)
  }

  parse_redcap_choices <- function(choice_string) {
    choice_string <- trimws(as.character(choice_string))
    if (is.na(choice_string) || !nzchar(choice_string)) {
      return(tibble::tibble(code = character(), label = character(), normalized_label = character()))
    }

    parts <- unlist(strsplit(choice_string, "\\|", perl = TRUE))
    parts <- trimws(parts)
    parsed <- lapply(parts, function(part) {
      pieces <- strsplit(part, ",", fixed = TRUE)[[1]]
      code <- trimws(pieces[[1]])
      label <- if (length(pieces) > 1) trimws(paste(pieces[-1], collapse = ",")) else code
      tibble::tibble(
        code = code,
        label = label,
        normalized_label = normalize_mapping_terms(label)
      )
    })

    dplyr::bind_rows(parsed)
  }

  normalize_choice_label <- function(value) {
    value <- normalize_mapping_terms(value)
    value <- stringr::str_replace(value, "^\\d+\\s*", "")
    value <- stringr::str_replace(value, "^[a-z]\\s*", "")
    trimws(value)
  }

  normalize_append_choice_value <- function(value) {
    normalized <- normalize_mapping_terms(value)

    dplyr::case_when(
      normalized %in% c("s") ~ "susceptible",
      normalized %in% c("i") ~ "intermediate",
      normalized %in% c("r") ~ "resistant",
      normalized %in% c("pos", "positive", "+") ~ "positive",
      normalized %in% c("neg", "negative", "-") ~ "negative",
      normalized %in% c("yes", "y", "true") ~ "yes",
      normalized %in% c("no", "n", "false") ~ "no",
      TRUE ~ normalized
    )
  }

  match_choice_code <- function(candidate_value, choices_tbl) {
    if (is.null(candidate_value) || is.na(candidate_value) || !nzchar(trimws(candidate_value))) {
      return(list(code = NA_character_, converted = FALSE, valid = FALSE))
    }

    candidate_value <- as.character(candidate_value)
    normalized_value <- normalize_append_choice_value(candidate_value)
    normalized_value_alt <- normalize_choice_label(candidate_value)

    if (candidate_value %in% choices_tbl$code) {
      return(list(code = candidate_value, converted = FALSE, valid = TRUE))
    }

    exact_match <- choices_tbl |>
      dplyr::filter(
        .data$normalized_label %in% c(normalized_value, normalized_value_alt)
      ) |>
      dplyr::slice_head(n = 1)

    if (nrow(exact_match) == 1) {
      return(list(code = exact_match$code[[1]], converted = TRUE, valid = TRUE))
    }

    stripped_match <- choices_tbl |>
      dplyr::mutate(
        normalized_label_stripped = normalize_choice_label(.data$label)
      ) |>
      dplyr::filter(
        .data$normalized_label_stripped %in% c(normalized_value, normalized_value_alt)
      ) |>
      dplyr::slice_head(n = 1)

    if (nrow(stripped_match) == 1) {
      return(list(code = stripped_match$code[[1]], converted = TRUE, valid = TRUE))
    }

    distances <- utils::adist(normalized_value_alt, choices_tbl$normalized_label)
    stripped_distances <- utils::adist(
      normalized_value_alt,
      normalize_choice_label(choices_tbl$label)
    )
    best_idx <- which.min(pmin(distances, stripped_distances))
    best_distance <- min(distances[[best_idx]], stripped_distances[[best_idx]])

    if (length(best_idx) == 1 && !is.na(best_distance) && best_distance <= 3) {
      return(list(code = choices_tbl$code[[best_idx]], converted = TRUE, valid = TRUE))
    }

    list(code = NA_character_, converted = FALSE, valid = FALSE)
  }

  coerce_append_value <- function(value,
                                  source_name,
                                  target_name,
                                  row_data = NULL,
                                  field_type = "",
                                  field_choices = "") {
    value <- as.character(value)
    value[is.na(value)] <- ""

    if (source_name == "patient_id") {
      digits_only <- gsub("[^0-9]", "", value)
      value <- ifelse(
        nchar(digits_only) >= 6,
        substr(digits_only, 1, 6),
        sub("_([0-9]+)$", "", value)
      )
    }

    if (!nzchar(value)) {
      return(list(value = NA_character_, converted = FALSE, valid = TRUE))
    }

    field_type <- tolower(trimws(as.character(field_type)))
    categorical_types <- c("dropdown", "radio", "checkbox", "yesno", "truefalse")

    if (!field_type %in% categorical_types) {
      return(list(value = value, converted = FALSE, valid = TRUE))
    }

    choices_tbl <- parse_redcap_choices(field_choices)
    if (nrow(choices_tbl) == 0) {
      return(list(value = NA_character_, converted = FALSE, valid = FALSE))
    }

    direct_match <- match_choice_code(value, choices_tbl)
    if (isTRUE(direct_match$valid)) {
      return(list(value = direct_match$code, converted = direct_match$converted, valid = TRUE))
    }

    base_source_name <- sub("_[0-9]+$", "", source_name)
    if (identical(base_source_name, "isolate") && !is.null(row_data)) {
      organism_source <- organism_lookup_source(source_name)
      organism_value <- row_data[[organism_source]]
      organism_match <- match_choice_code(organism_value, choices_tbl)
      if (isTRUE(organism_match$valid)) {
        return(list(value = organism_match$code, converted = TRUE, valid = TRUE))
      }
    }

    return(list(value = NA_character_, converted = FALSE, valid = FALSE))
  }

  if (is.null(redcap_df) || !is.data.frame(redcap_df)) {
    stop("redcap_df must be a non-null data frame.")
  }

  if (is.null(append_mapping) || !is.data.frame(append_mapping)) {
    stop("append_mapping must be a non-null data frame.")
  }

  normalized_df <- redcap_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  mapped_rows <- append_mapping |>
    dplyr::mutate(
      key_field = to_logical_flag(.data$key_field),
      has_target = .data$target_redcap_name != "",
      source_present = .data$source_name %in% names(normalized_df)
    ) |>
    dplyr::filter(.data$has_target, .data$source_present)

  if (nrow(mapped_rows) == 0) {
    stop("No mapped append fields are available in the upload-ready dataset.")
  }

  duplicate_targets <- mapped_rows |>
    dplyr::count(.data$target_redcap_name, name = "n_sources") |>
    dplyr::filter(.data$n_sources > 1)

  if (nrow(duplicate_targets) > 0) {
    stop(
      "Duplicate target_redcap_name values remain in the append mapping: ",
      paste(duplicate_targets$target_redcap_name, collapse = ", ")
    )
  }

  payload <- normalized_df[, integer(0), drop = FALSE]
  conversion_log <- tibble::tibble(
    source_name = character(),
    target_redcap_name = character(),
    original_value = character(),
    converted_value = character(),
    conversion_status = character()
  )
  for (i in seq_len(nrow(mapped_rows))) {
    source_name <- mapped_rows$source_name[[i]]
    target_name <- mapped_rows$target_redcap_name[[i]]
    field_type <- if ("target_field_type" %in% names(mapped_rows)) mapped_rows$target_field_type[[i]] else ""
    field_choices <- if ("target_field_choices" %in% names(mapped_rows)) mapped_rows$target_field_choices[[i]] else ""

    converted_values <- vapply(seq_len(nrow(normalized_df)), function(j) {
      row_slice <- as.list(normalized_df[j, , drop = FALSE])
      coerce_append_value(
        value = normalized_df[[source_name]][[j]],
        source_name = source_name,
        target_name = target_name,
        row_data = row_slice,
        field_type = field_type,
        field_choices = field_choices
      )$value
    }, character(1))

    status_values <- vapply(seq_len(nrow(normalized_df)), function(j) {
      row_slice <- as.list(normalized_df[j, , drop = FALSE])
      result <- coerce_append_value(
        value = normalized_df[[source_name]][[j]],
        source_name = source_name,
        target_name = target_name,
        row_data = row_slice,
        field_type = field_type,
        field_choices = field_choices
      )
      if (isTRUE(result$valid) && isTRUE(result$converted)) {
        "converted"
      } else if (isTRUE(result$valid)) {
        "passed_through"
      } else {
        "dropped_invalid_choice"
      }
    }, character(1))

    payload[[target_name]] <- converted_values

    conversion_log <- dplyr::bind_rows(
      conversion_log,
      tibble::tibble(
        source_name = source_name,
        target_redcap_name = target_name,
        original_value = as.character(normalized_df[[source_name]]),
        converted_value = as.character(converted_values),
        conversion_status = status_values
      )
    )
  }

  key_target_fields <- mapped_rows |>
    dplyr::filter(.data$key_field) |>
    dplyr::pull(.data$target_redcap_name) |>
    unique()

  update_target_fields <- setdiff(names(payload), key_target_fields)

  empty_values <- c("", "(Empty)", "(EMPTY)", "empty")

  if (length(update_target_fields) > 0) {
    payload <- payload |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(update_target_fields),
          ~ {
            value <- as.character(.x)
            value[trimws(value) %in% empty_values] <- NA_character_
            value
          }
        )
      )
  }

  dropped_missing_key <- tibble::tibble()
  if (length(key_target_fields) > 0) {
    missing_key_mask <- apply(
      payload[, key_target_fields, drop = FALSE],
      1,
      function(row) any(is.na(row) | trimws(row) == "")
    )

    if (any(missing_key_mask)) {
      dropped_missing_key <- payload[missing_key_mask, , drop = FALSE]
      payload <- payload[!missing_key_mask, , drop = FALSE]
    }
  }

  if (length(update_target_fields) > 0 && nrow(payload) > 0) {
    no_update_mask <- apply(
      payload[, update_target_fields, drop = FALSE],
      1,
      function(row) all(is.na(row) | trimws(as.character(row)) == "")
    )
  } else {
    no_update_mask <- rep(FALSE, nrow(payload))
  }

  dropped_no_updates <- tibble::tibble()
  if (any(no_update_mask)) {
    dropped_no_updates <- payload[no_update_mask, , drop = FALSE]
    payload <- payload[!no_update_mask, , drop = FALSE]
  }

  if (!quiet) {
    message("Append payload prepared successfully.")
    message("Append rows ready: ", nrow(payload))
  }

  list(
    payload = payload,
    mapped_rows = mapped_rows,
    key_target_fields = key_target_fields,
    update_target_fields = update_target_fields,
    dropped_missing_key = dropped_missing_key,
    dropped_no_updates = dropped_no_updates,
    conversion_log = conversion_log
  )
}

upload_redcap_v1_1_0_append <- function(redcap_df,
                                        config_file = here::here("config", "redcap_config.yml"),
                                        mapping_file = here::here("docs", "redcap_append_variable_mapping.csv"),
                                        redcap_uri = NULL,
                                        keyring_service = NULL,
                                        keyring_username = NULL,
                                        required_fields = c(
                                          "record_id",
                                          "redcap_event_name",
                                          "redcap_repeat_instrument",
                                          "redcap_repeat_instance"
                                        ),
                                        key_fields = c(
                                          "record_id",
                                          "redcap_event_name",
                                          "redcap_repeat_instrument",
                                          "redcap_repeat_instance"
                                        ),
                                        marker_tests = c(
                                          "esbl",
                                          "cefoxitin_screen",
                                          "inducible_clindamycin_resistance"
                                        ),
                                        parser_version = "v1.0.0",
                                        default_fill = "",
                                        dry_run = FALSE,
                                        output_dir = here::here("output"),
                                        batch_size = 100,
                                        save_staging = TRUE,
                                        quiet = FALSE) {
  validation_result <- validate_redcap_data(
    redcap_df = redcap_df,
    required_fields = required_fields,
    key_fields = key_fields,
    marker_tests = marker_tests,
    parser_version = parser_version,
    default_fill = default_fill,
    quiet = quiet
  )

  upload_ready <- validation_result$upload_ready |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  append_mapping <- load_append_redcap_mapping(
    mapping_file = mapping_file,
    parsed_columns = names(upload_ready),
    quiet = quiet
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  processed_at_file <- format(Sys.time(), "%Y%m%d_%H%M%S")

  staging_csv <- file.path(
    output_dir,
    paste0("staging_upload_ready_append_", processed_at_file, ".csv")
  )

  if (isTRUE(save_staging)) {
    readr::write_csv(upload_ready, staging_csv)
  }

  mapping_preview_file <- file.path(
    output_dir,
    paste0("append_mapping_preview_", processed_at_file, ".csv")
  )
  readr::write_csv(append_mapping, mapping_preview_file)

  trial_setup <- prepare_append_trial(
    redcap_df = upload_ready,
    mapping_file = mapping_file,
    config_file = config_file,
    redcap_uri = redcap_uri,
    keyring_service = keyring_service,
    keyring_username = keyring_username,
    output_dir = output_dir,
    trial_prefix = "append_trial",
    quiet = quiet
  )

  if (!isTRUE(trial_setup$trial_ready)) {
    return(list(
      status = trial_setup$status,
      uploader_version = "v1.1.0",
      dry_run = dry_run,
      config_used = trial_setup$config_used,
      validation_result = validation_result,
      append_mapping = append_mapping,
      append_mapping_file = mapping_file,
      append_mapping_preview_file = mapping_preview_file,
      trial_setup = trial_setup,
      upload_ready = upload_ready,
      upload_log = NULL,
      upload_log_file = NA_character_,
      successful_rows = tibble::tibble(),
      failed_rows = tibble::tibble(),
      failed_rows_file = NA_character_,
      raw_results = list(),
      staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
      staging_rds = NA_character_,
      trial_summary_file = trial_setup$summary_file,
      trial_blocking_issues_file = trial_setup$blocking_issues_file,
      trial_warnings_file = trial_setup$warnings_file,
      trial_mapped_fields_file = trial_setup$mapped_fields_file,
      append_payload = tibble::tibble(),
      append_payload_file = NA_character_,
      notes = paste(
        "Append setup is blocked.",
        "Review the generated trial issue files before retrying the append upload."
      )
    ))
  }

  payload_result <- build_append_redcap_payload(
    redcap_df = upload_ready,
    append_mapping = append_mapping,
    quiet = quiet
  )

  duplicate_merge_result <- merge_append_payload_duplicates(
    payload = payload_result$payload,
    key_target_fields = payload_result$key_target_fields,
    quiet = quiet
  )

  payload_result$payload <- duplicate_merge_result$payload

  append_payload_file <- file.path(
    output_dir,
    paste0("append_payload_", processed_at_file, ".csv")
  )
  conversion_log_file <- file.path(
    output_dir,
    paste0("append_value_conversion_log_", processed_at_file, ".csv")
  )
  dropped_missing_key_file <- file.path(
    output_dir,
    paste0("append_dropped_missing_key_", processed_at_file, ".csv")
  )
  dropped_no_updates_file <- file.path(
    output_dir,
    paste0("append_dropped_no_updates_", processed_at_file, ".csv")
  )
  duplicate_merge_summary_file <- file.path(
    output_dir,
    paste0("append_duplicate_merge_summary_", processed_at_file, ".csv")
  )
  duplicate_merge_conflicts_file <- file.path(
    output_dir,
    paste0("append_duplicate_merge_conflicts_", processed_at_file, ".csv")
  )

  readr::write_csv(payload_result$payload, append_payload_file)
  readr::write_csv(payload_result$dropped_missing_key, dropped_missing_key_file)
  readr::write_csv(payload_result$dropped_no_updates, dropped_no_updates_file)
  readr::write_csv(payload_result$conversion_log, conversion_log_file)
  readr::write_csv(duplicate_merge_result$duplicate_summary, duplicate_merge_summary_file)
  readr::write_csv(duplicate_merge_result$conflict_details, duplicate_merge_conflicts_file)

  duplicate_merge_message <- if (duplicate_merge_result$merged_row_count > 0) {
    paste0(
      duplicate_merge_result$merged_row_count,
      " duplicate append row(s) were merged by final REDCap key before upload."
    )
  } else {
    ""
  }

  duplicate_key_groups_identified <- nrow(duplicate_merge_result$duplicate_summary)
  duplicate_payload_rows_identified <- if (duplicate_key_groups_identified > 0) {
    sum(duplicate_merge_result$duplicate_summary$n_rows)
  } else {
    0L
  }
  final_append_rows_ready <- nrow(payload_result$payload)

  if (nrow(duplicate_merge_result$conflict_details) > 0) {
    return(list(
      status = "Append payload conflicts detected",
      uploader_version = "v1.1.0",
      dry_run = dry_run,
      config_used = trial_setup$config_used,
      validation_result = validation_result,
      append_mapping = append_mapping,
      append_mapping_file = mapping_file,
      append_mapping_preview_file = mapping_preview_file,
      trial_setup = trial_setup,
      upload_ready = upload_ready,
      upload_log = NULL,
      upload_log_file = NA_character_,
      successful_rows = tibble::tibble(),
      failed_rows = tibble::tibble(),
      failed_rows_file = NA_character_,
      raw_results = list(),
      staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
      staging_rds = NA_character_,
      trial_summary_file = trial_setup$summary_file,
      trial_blocking_issues_file = trial_setup$blocking_issues_file,
      trial_warnings_file = trial_setup$warnings_file,
      trial_mapped_fields_file = trial_setup$mapped_fields_file,
      append_payload = payload_result$payload,
      append_payload_file = append_payload_file,
      conversion_log_file = conversion_log_file,
      dropped_missing_key_file = dropped_missing_key_file,
      dropped_no_updates_file = dropped_no_updates_file,
      duplicate_merge_summary_file = duplicate_merge_summary_file,
      duplicate_merge_conflicts_file = duplicate_merge_conflicts_file,
      duplicate_key_groups_identified = duplicate_key_groups_identified,
      duplicate_payload_rows_identified = duplicate_payload_rows_identified,
      duplicate_rows_merged = duplicate_merge_result$merged_row_count,
      final_append_rows_ready = final_append_rows_ready,
      final_rows_uploaded = 0L,
      warning_message = paste(
        "Append payload contains duplicate final REDCap keys with conflicting mapped values.",
        "Review",
        basename(duplicate_merge_conflicts_file),
        "before retrying."
      ),
      notes = "Append upload was stopped because duplicate final REDCap keys could not be merged safely."
    ))
  }

  dropped_no_updates_message <- if (nrow(payload_result$dropped_no_updates) > 0) {
    paste0(
      nrow(payload_result$dropped_no_updates),
      " row(s) were excluded because no valid REDCap-mappable update values remained after conversion. ",
      "Review ",
      basename(dropped_no_updates_file),
      " for details."
    )
  } else {
    ""
  }

  if (nrow(payload_result$payload) == 0) {
    return(list(
      status = "No append-ready rows",
      uploader_version = "v1.1.0",
      dry_run = dry_run,
      config_used = trial_setup$config_used,
      validation_result = validation_result,
      append_mapping = append_mapping,
      append_mapping_file = mapping_file,
      append_mapping_preview_file = mapping_preview_file,
      trial_setup = trial_setup,
      upload_ready = upload_ready,
      upload_log = NULL,
      upload_log_file = NA_character_,
      successful_rows = tibble::tibble(),
      failed_rows = tibble::tibble(),
      failed_rows_file = NA_character_,
      raw_results = list(),
      staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
      staging_rds = NA_character_,
      trial_summary_file = trial_setup$summary_file,
      trial_blocking_issues_file = trial_setup$blocking_issues_file,
      trial_warnings_file = trial_setup$warnings_file,
      trial_mapped_fields_file = trial_setup$mapped_fields_file,
      append_payload = payload_result$payload,
      append_payload_file = append_payload_file,
      conversion_log_file = conversion_log_file,
      dropped_missing_key_file = dropped_missing_key_file,
      dropped_no_updates_file = dropped_no_updates_file,
      duplicate_merge_summary_file = duplicate_merge_summary_file,
      duplicate_merge_conflicts_file = duplicate_merge_conflicts_file,
      duplicate_key_groups_identified = duplicate_key_groups_identified,
      duplicate_payload_rows_identified = duplicate_payload_rows_identified,
      duplicate_rows_merged = duplicate_merge_result$merged_row_count,
      final_append_rows_ready = final_append_rows_ready,
      final_rows_uploaded = 0L,
      warning_message = dropped_no_updates_message,
      notes = paste(
        "Append payload generation completed, but no rows remained after key and update-value filtering.",
        dropped_no_updates_message,
        duplicate_merge_message
      )
    ))
  }

  if (isTRUE(dry_run)) {
    return(list(
      status = "Append dry run completed",
      uploader_version = "v1.1.0",
      dry_run = TRUE,
      config_used = trial_setup$config_used,
      validation_result = validation_result,
      append_mapping = append_mapping,
      append_mapping_file = mapping_file,
      append_mapping_preview_file = mapping_preview_file,
      trial_setup = trial_setup,
      upload_ready = upload_ready,
      upload_log = NULL,
      upload_log_file = NA_character_,
      successful_rows = payload_result$payload,
      failed_rows = tibble::tibble(),
      failed_rows_file = NA_character_,
      raw_results = list(),
      staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
      staging_rds = NA_character_,
      trial_summary_file = trial_setup$summary_file,
      trial_blocking_issues_file = trial_setup$blocking_issues_file,
      trial_warnings_file = trial_setup$warnings_file,
      trial_mapped_fields_file = trial_setup$mapped_fields_file,
      append_payload = payload_result$payload,
      append_payload_file = append_payload_file,
      conversion_log_file = conversion_log_file,
      dropped_missing_key_file = dropped_missing_key_file,
      dropped_no_updates_file = dropped_no_updates_file,
      duplicate_merge_summary_file = duplicate_merge_summary_file,
      duplicate_merge_conflicts_file = duplicate_merge_conflicts_file,
      duplicate_key_groups_identified = duplicate_key_groups_identified,
      duplicate_payload_rows_identified = duplicate_payload_rows_identified,
      duplicate_rows_merged = duplicate_merge_result$merged_row_count,
      final_append_rows_ready = final_append_rows_ready,
      final_rows_uploaded = final_append_rows_ready,
      warning_message = paste(c(dropped_no_updates_message, duplicate_merge_message)[nzchar(c(dropped_no_updates_message, duplicate_merge_message))], collapse = " "),
      notes = paste(
        "Append dry run completed.",
        "Mapped payload was prepared with overwrite_with_blanks = FALSE semantics for safe append behavior.",
        dropped_no_updates_message,
        duplicate_merge_message
      )
    ))
  }

  if (!requireNamespace("REDCapR", quietly = TRUE)) stop("Package 'REDCapR' is required.")
  if (!requireNamespace("keyring", quietly = TRUE)) stop("Package 'keyring' is required.")

  resolved_uri <- trial_setup$config_used$redcap_uri
  resolved_service <- trial_setup$config_used$keyring_service
  resolved_username <- trial_setup$config_used$keyring_username

  api_token <- tryCatch(
    keyring::key_get(
      service = resolved_service,
      username = resolved_username
    ),
    error = function(e) {
      stop(
        "Unable to retrieve REDCap API token from keyring for append upload. Details: ",
        e$message
      )
    }
  )

  n_total <- nrow(payload_result$payload)
  batch_index <- ceiling(seq_len(n_total) / batch_size)
  upload_batches <- split(payload_result$payload, batch_index)

  upload_log <- tibble::tibble(
    batch_id = integer(),
    n_rows_attempted = integer(),
    n_rows_uploaded = integer(),
    success = logical(),
    elapsed_seconds = numeric(),
    error_message = character()
  )

  successful_batches <- list()
  failed_batches <- list()
  raw_results <- list()

  for (i in seq_along(upload_batches)) {
    batch_df <- upload_batches[[i]] |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    t0 <- Sys.time()

    res <- tryCatch(
      REDCapR::redcap_write(
        ds_to_write = batch_df,
        redcap_uri = resolved_uri,
        token = api_token,
        batch_size = batch_size,
        continue_on_error = FALSE,
        overwrite_with_blanks = FALSE,
        verbose = !quiet
      ),
      error = function(e) e
    )

    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    if (inherits(res, "error")) {
      upload_log <- dplyr::bind_rows(
        upload_log,
        tibble::tibble(
          batch_id = i,
          n_rows_attempted = nrow(batch_df),
          n_rows_uploaded = 0,
          success = FALSE,
          elapsed_seconds = elapsed,
          error_message = as.character(res$message)
        )
      )
      failed_batches[[paste0("batch_", i)]] <- batch_df
      raw_results[[paste0("batch_", i)]] <- list(status = "error", result = res)
    } else {
      uploaded_n <- if ("records_affected_count" %in% names(res)) {
        res$records_affected_count
      } else if ("affected_records" %in% names(res)) {
        res$affected_records
      } else if ("count" %in% names(res)) {
        res$count
      } else {
        nrow(batch_df)
      }

      upload_log <- dplyr::bind_rows(
        upload_log,
        tibble::tibble(
          batch_id = i,
          n_rows_attempted = nrow(batch_df),
          n_rows_uploaded = uploaded_n,
          success = TRUE,
          elapsed_seconds = elapsed,
          error_message = ""
        )
      )
      successful_batches[[paste0("batch_", i)]] <- batch_df
      raw_results[[paste0("batch_", i)]] <- list(status = "success", result = res)
    }
  }

  upload_log_file <- file.path(
    output_dir,
    paste0("redcap_append_upload_log_", processed_at_file, ".csv")
  )
  readr::write_csv(upload_log, upload_log_file)

  if (length(failed_batches) > 0) {
    failed_rows <- dplyr::bind_rows(failed_batches)
    failed_rows_file <- file.path(
      output_dir,
      paste0("redcap_append_failed_rows_", processed_at_file, ".csv")
    )
    readr::write_csv(failed_rows, failed_rows_file)
  } else {
    failed_rows <- tibble::tibble()
    failed_rows_file <- NA_character_
  }

  if (length(successful_batches) > 0) {
    successful_rows <- dplyr::bind_rows(successful_batches)
  } else {
    successful_rows <- tibble::tibble()
  }

  final_rows_uploaded <- if (nrow(upload_log) > 0) {
    sum(upload_log$n_rows_uploaded, na.rm = TRUE)
  } else {
    0L
  }

  status <- dplyr::case_when(
    nrow(upload_log) == 0 ~ "No append upload attempted",
    all(upload_log$success) ~ "Append upload completed successfully",
    any(upload_log$success) ~ "Append upload partially completed",
    TRUE ~ "Append upload failed"
  )

  list(
    status = status,
    uploader_version = "v1.1.0",
    dry_run = dry_run,
    config_used = trial_setup$config_used,
    validation_result = validation_result,
    append_mapping = append_mapping,
    append_mapping_file = mapping_file,
    append_mapping_preview_file = mapping_preview_file,
    trial_setup = trial_setup,
    upload_ready = upload_ready,
    upload_log = upload_log,
    upload_log_file = upload_log_file,
    successful_rows = successful_rows,
    failed_rows = failed_rows,
    failed_rows_file = failed_rows_file,
    raw_results = raw_results,
    staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
    staging_rds = NA_character_,
    trial_summary_file = trial_setup$summary_file,
    trial_blocking_issues_file = trial_setup$blocking_issues_file,
    trial_warnings_file = trial_setup$warnings_file,
    trial_mapped_fields_file = trial_setup$mapped_fields_file,
    append_payload = payload_result$payload,
    append_payload_file = append_payload_file,
    conversion_log_file = conversion_log_file,
    dropped_missing_key_file = dropped_missing_key_file,
    dropped_no_updates_file = dropped_no_updates_file,
    duplicate_merge_summary_file = duplicate_merge_summary_file,
    duplicate_merge_conflicts_file = duplicate_merge_conflicts_file,
    duplicate_key_groups_identified = duplicate_key_groups_identified,
    duplicate_payload_rows_identified = duplicate_payload_rows_identified,
    duplicate_rows_merged = duplicate_merge_result$merged_row_count,
    final_append_rows_ready = final_append_rows_ready,
    final_rows_uploaded = final_rows_uploaded,
    warning_message = paste(c(dropped_no_updates_message, duplicate_merge_message)[nzchar(c(dropped_no_updates_message, duplicate_merge_message))], collapse = " "),
    notes = paste(
      "Append uploader v1.1.0 wrote only mapped target fields with overwrite_with_blanks = FALSE.",
      "This preserves unrelated REDCap variables while updating mapped values.",
      dropped_no_updates_message,
      duplicate_merge_message
    )
  )
}
