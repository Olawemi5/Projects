# ==================================================
# functions/get_unprocessed_files.R
# Filter manifest to only files not yet successfully processed
# ==================================================

get_unprocessed_files <- function(
    manifest_df,
    audit_log_file = here::here("output", "vitek_pipeline_audit_log.csv"),
    success_status = c("Dry run completed", "Upload completed successfully"),
    quiet = FALSE
) {
  
  if (is.null(manifest_df) || !is.data.frame(manifest_df)) {
    stop("manifest_df must be a non-null data frame.")
  }
  
  required_cols <- c("source_file", "file_path", "file_exists")
  missing_cols <- setdiff(required_cols, names(manifest_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "manifest_df is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  manifest_df <- manifest_df |>
    dplyr::filter(file_exists)
  
  if (nrow(manifest_df) == 0) {
    manifest_df$already_processed <- logical(0)
    
    if (!quiet) {
      message("No files in the manifest were found inside data_raw.")
    }
    
    return(manifest_df)
  }
  
  if (!file.exists(audit_log_file)) {
    manifest_df$already_processed <- rep(FALSE, nrow(manifest_df))
    
    if (!quiet) {
      message("Audit log not found. All existing files are treated as unprocessed.")
      message("Files to process: ", nrow(manifest_df))
    }
    
    return(manifest_df)
  }
  
  audit_log <- readr::read_csv(
    audit_log_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  
  if (!"source_file" %in% names(audit_log)) {
    stop("Audit log does not contain a 'source_file' column: ", audit_log_file)
  }
  
  # only count truly successful runs as processed
  if ("upload_status" %in% names(audit_log)) {
    processed_files <- audit_log |>
      dplyr::filter(upload_status %in% success_status) |>
      dplyr::pull(source_file) |>
      unique()
  } else {
    processed_files <- character(0)
  }
  
  manifest_df$already_processed <- manifest_df$source_file %in% processed_files
  
  unprocessed_df <- manifest_df |>
    dplyr::filter(!already_processed)
  
  if (!quiet) {
    message("Audit log loaded successfully.")
    message("Total existing files in manifest: ", nrow(manifest_df))
    message("Already successfully processed: ", sum(manifest_df$already_processed))
    message("Remaining to process: ", nrow(unprocessed_df))
  }
  
  return(unprocessed_df)
}