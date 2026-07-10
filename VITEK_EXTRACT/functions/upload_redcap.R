# functions/upload_redcap.R

upload_redcap <- function(redcap_df,
                          config_file = here::here("config", "redcap_config.yml"),
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
  normalize_standard_upload_patient_id <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""

    suffix <- stringr::str_extract(x, "_[^_]+$")
    suffix[is.na(suffix)] <- ""

    id_root <- stringr::str_replace(x, "_[^_]+$", "")
    digits_only <- gsub("[^0-9]", "", id_root)
    normalized_base <- ifelse(
      digits_only == "",
      "",
      ifelse(nchar(digits_only) > 6, substr(digits_only, 1, 6), digits_only)
    )

    tibble::tibble(
      original_patient_id = x,
      normalized_base = normalized_base,
      existing_suffix = suffix
    )
  }

  format_standard_upload_patient_ids <- function(df) {
    if (!"patient_id" %in% names(df) || nrow(df) == 0) {
      return(df)
    }

    id_parts <- normalize_standard_upload_patient_id(df$patient_id)

    duplicate_counts <- table(id_parts$normalized_base[id_parts$normalized_base != ""])
    duplicate_keys <- names(duplicate_counts)[duplicate_counts > 1]

    duplicate_mask <- id_parts$normalized_base %in% duplicate_keys & id_parts$normalized_base != ""
    duplicate_index <- ave(
      seq_len(nrow(id_parts)),
      interaction(id_parts$normalized_base, drop = TRUE),
      FUN = seq_along
    )

    resolved_suffix <- ifelse(
      duplicate_mask & nzchar(id_parts$existing_suffix),
      id_parts$existing_suffix,
      ifelse(duplicate_mask, paste0("_", duplicate_index), "")
    )

    formatted_id <- ifelse(
      nzchar(id_parts$normalized_base),
      paste0(id_parts$normalized_base, resolved_suffix),
      df$patient_id
    )

    df$patient_id <- formatted_id
    df
  }
  
  # --------------------------------------------------
  # checks
  # --------------------------------------------------
  if (is.null(redcap_df) || !is.data.frame(redcap_df)) {
    stop("redcap_df must be a non-null data frame.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Package 'readr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("REDCapR", quietly = TRUE)) stop("Package 'REDCapR' is required.")
  if (!requireNamespace("keyring", quietly = TRUE)) stop("Package 'keyring' is required.")
  
  if (!exists("validate_redcap_data", mode = "function")) {
    stop("validate_redcap_data() is not available. Source functions/validate_redcap_data.R first.")
  }
  
  if ((is.null(redcap_uri) || is.null(keyring_service) || is.null(keyring_username)) &&
      !exists("load_redcap_config", mode = "function")) {
    stop("load_redcap_config() is not available. Source functions/load_redcap_config.R first.")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  processed_at <- as.character(Sys.time())
  processed_at_file <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # --------------------------------------------------
  # load config if needed
  # --------------------------------------------------
  if (is.null(redcap_uri) || is.null(keyring_service) || is.null(keyring_username)) {
    cfg <- load_redcap_config(config_file = config_file)
    
    redcap_uri <- cfg$redcap_uri
    keyring_service <- cfg$keyring_service
    keyring_username <- cfg$keyring_username
  }
  
  if (is.null(redcap_uri) || redcap_uri == "") {
    stop("REDCap API URL is missing.")
  }
  
  if (is.null(keyring_service) || keyring_service == "") {
    stop("keyring_service is missing.")
  }
  
  if (is.null(keyring_username) || keyring_username == "") {
    stop("keyring_username is missing.")
  }
  
  # --------------------------------------------------
  # validate data first
  # --------------------------------------------------
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

  upload_ready <- format_standard_upload_patient_ids(upload_ready)
  
  # --------------------------------------------------
  # save validation outputs
  # --------------------------------------------------
  validation_summary_file <- file.path(
    output_dir, paste0("upload_validation_summary_", processed_at_file, ".csv")
  )
  
  flagged_issues_file <- file.path(
    output_dir, paste0("upload_flagged_issues_", processed_at_file, ".csv")
  )
  
  flagged_rows_file <- file.path(
    output_dir, paste0("upload_flagged_rows_", processed_at_file, ".csv")
  )
  
  readr::write_csv(validation_result$summary, validation_summary_file)
  readr::write_csv(validation_result$flagged_issues, flagged_issues_file)
  readr::write_csv(validation_result$flagged_rows, flagged_rows_file)
  
  if (nrow(upload_ready) == 0) {
    if (!quiet) {
      message("No rows available for upload after validation.")
    }
    
    return(list(
      status = "No upload-ready rows",
      dry_run = dry_run,
      config_used = list(
        redcap_uri = redcap_uri,
        keyring_service = keyring_service,
        keyring_username = keyring_username
      ),
      validation_result = validation_result,
      upload_result = NULL,
      upload_log = NULL,
      upload_ready = upload_ready
    ))
  }
  
  # --------------------------------------------------
  # save staging files
  # --------------------------------------------------
  staging_csv <- file.path(
    output_dir, paste0("staging_upload_ready_", processed_at_file, ".csv")
  )
  
  staging_rds <- file.path(
    output_dir, paste0("staging_upload_ready_", processed_at_file, ".rds")
  )
  
  if (isTRUE(save_staging)) {
    readr::write_csv(upload_ready, staging_csv)
    saveRDS(upload_ready, staging_rds)
  }
  
  # --------------------------------------------------
  # dry run
  # --------------------------------------------------
  if (isTRUE(dry_run)) {
    if (!quiet) {
      message("Dry run completed. No data uploaded to REDCap.")
      message("Rows ready for upload: ", nrow(upload_ready))
      if (isTRUE(save_staging)) {
        message("Staging CSV saved to: ", staging_csv)
      }
    }
    
    return(list(
      status = "Dry run completed",
      dry_run = TRUE,
      config_used = list(
        redcap_uri = redcap_uri,
        keyring_service = keyring_service,
        keyring_username = keyring_username
      ),
      validation_result = validation_result,
      upload_result = NULL,
      upload_log = NULL,
      upload_ready = upload_ready,
      staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
      staging_rds = if (isTRUE(save_staging)) staging_rds else NA_character_,
      validation_summary_file = validation_summary_file,
      flagged_issues_file = flagged_issues_file,
      flagged_rows_file = flagged_rows_file
    ))
  }
  
  # --------------------------------------------------
  # retrieve API token from keyring
  # --------------------------------------------------
  api_token <- tryCatch(
    keyring::key_get(
      service = keyring_service,
      username = keyring_username
    ),
    error = function(e) {
      stop(
        "Unable to retrieve REDCap API token from keyring. ",
        "Check your setup and run scripts/setup_keyring.R if necessary. Details: ",
        e$message
      )
    }
  )
  
  # --------------------------------------------------
  # split into batches
  # --------------------------------------------------
  n_total <- nrow(upload_ready)
  batch_index <- ceiling(seq_len(n_total) / batch_size)
  upload_batches <- split(upload_ready, batch_index)
  
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
  
  # --------------------------------------------------
  # upload each batch
  # --------------------------------------------------
  for (i in seq_along(upload_batches)) {
    
    batch_df <- upload_batches[[i]] |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    
    t0 <- Sys.time()
    
    res <- tryCatch(
      REDCapR::redcap_write(
        ds_to_write = batch_df,
        redcap_uri = redcap_uri,
        token = api_token,
        batch_size = batch_size,
        continue_on_error = FALSE,
        overwrite_with_blanks = TRUE,
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
      
      if (!quiet) {
        message("Batch ", i, " failed: ", res$message)
      }
      
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
      
      if (!quiet) {
        message("Batch ", i, " uploaded successfully.")
      }
    }
  }
  
  # --------------------------------------------------
  # save upload logs
  # --------------------------------------------------
  upload_log_file <- file.path(
    output_dir, paste0("redcap_upload_log_", processed_at_file, ".csv")
  )
  readr::write_csv(upload_log, upload_log_file)
  
  if (length(failed_batches) > 0) {
    failed_rows <- dplyr::bind_rows(failed_batches)
    failed_rows_file <- file.path(
      output_dir, paste0("redcap_failed_rows_", processed_at_file, ".csv")
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
  
  status <- dplyr::case_when(
    nrow(upload_log) == 0 ~ "No upload attempted",
    all(upload_log$success) ~ "Upload completed successfully",
    any(upload_log$success) ~ "Upload partially completed",
    TRUE ~ "Upload failed"
  )
  
  if (!quiet) {
    message(status)
    message("Upload log saved to: ", upload_log_file)
    if (!is.na(failed_rows_file)) {
      message("Failed rows saved to: ", failed_rows_file)
    }
  }
  
  return(list(
    status = status,
    dry_run = FALSE,
    config_used = list(
      redcap_uri = redcap_uri,
      keyring_service = keyring_service,
      keyring_username = keyring_username
    ),
    validation_result = validation_result,
    upload_ready = upload_ready,
    upload_log = upload_log,
    upload_log_file = upload_log_file,
    successful_rows = successful_rows,
    failed_rows = failed_rows,
    failed_rows_file = failed_rows_file,
    raw_results = raw_results,
    staging_csv = if (isTRUE(save_staging)) staging_csv else NA_character_,
    staging_rds = if (isTRUE(save_staging)) staging_rds else NA_character_,
    validation_summary_file = validation_summary_file,
    flagged_issues_file = flagged_issues_file,
    flagged_rows_file = flagged_rows_file
  ))
}
