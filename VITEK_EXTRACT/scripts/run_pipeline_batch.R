# ==================================================
# scripts/run_pipeline_batch.R
# Batch processing of VITEK PDF files
# ==================================================

# --------------------------------------------------
# Load packages and functions
# --------------------------------------------------
source(here::here("scripts", "packages.R"))

source(here::here("functions", "read_pdf_manifest.R"))
source(here::here("functions", "get_unprocessed_files.R"))

source(here::here("functions", "read_vitek_pdf_full.R"))
source(here::here("functions", "parse_ast_chart_report_full.R"))
source(here::here("functions", "map_redcap.R"))
source(here::here("functions", "validate_redcap_data.R"))
source(here::here("functions", "clean_data.R"))
source(here::here("functions", "load_redcap_config.R"))
source(here::here("functions", "upload_redcap.R"))

# --------------------------------------------------
# Settings
# --------------------------------------------------
parser_version <- "v1.0.0"

manifest_file <- here::here("docs", "files_to_process.txt")

# keep this as the main persistent audit log
audit_log_file <- here::here("output", "vitek_pipeline_audit_log.csv")

# keep master parsed outputs in the main output folder
master_output_file <- here::here("output", "vitek_ast_master.csv")
master_output_rds  <- here::here("output", "vitek_ast_master.rds")

mapping_file <- here::here("docs", "redcap_variable_mapping.csv")
config_file  <- here::here("config", "redcap_config.yml")

required_redcap_fields <- c("patient_id")
key_fields <- c("patient_id")

marker_tests <- c(
  "esbl",
  "cefoxitin_screen",
  "inducible_clindamycin_resistance"
)

dry_run_upload <- FALSE
batch_size_upload <- 100

# --------------------------------------------------
# Create timestamped run output folder
# --------------------------------------------------
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

run_output_dir <- file.path(
  here::here("output"),
  paste0("batch_run_", run_timestamp)
)

dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)

message("Run output directory created:")
message(run_output_dir)

# --------------------------------------------------
# Helper: append audit row
# --------------------------------------------------
append_audit_row <- function(audit_row, audit_log_file) {
  
  audit_row <- audit_row |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  if (file.exists(audit_log_file)) {
    
    existing_log <- readr::read_csv(
      audit_log_file,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
    
    missing_in_existing <- setdiff(names(audit_row), names(existing_log))
    if (length(missing_in_existing) > 0) {
      existing_log[missing_in_existing] <- ""
    }
    
    missing_in_new <- setdiff(names(existing_log), names(audit_row))
    if (length(missing_in_new) > 0) {
      audit_row[missing_in_new] <- ""
    }
    
    audit_row <- audit_row[, names(existing_log), drop = FALSE]
    combined_log <- dplyr::bind_rows(existing_log, audit_row)
    
  } else {
    combined_log <- audit_row
  }
  
  readr::write_csv(combined_log, audit_log_file)
}

# --------------------------------------------------
# Step 1: Read manifest and get unprocessed files
# --------------------------------------------------
manifest_df <- read_pdf_manifest(
  manifest_file = manifest_file,
  quiet = FALSE
)

files_to_run <- get_unprocessed_files(
  manifest_df = manifest_df,
  audit_log_file = audit_log_file,
  quiet = FALSE
)

if (nrow(files_to_run) == 0) {
  message("No new files to process.")
} else {
  
  message("Starting batch processing...")
  
  for (i in seq_len(nrow(files_to_run))) {
    
    current_file <- files_to_run$file_path[i]
    current_name <- files_to_run$source_file[i]
    processed_at <- as.character(Sys.time())
    timestamp_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    message("--------------------------------------------------")
    message("Processing file ", i, " of ", nrow(files_to_run), ": ", current_name)
    
    tryCatch({
      
      # ==============================================
      # Step 2: Read PDF
      # ==============================================
      pdf_obj <- read_vitek_pdf_full(current_file)
      
      # ==============================================
      # Step 3: Parse PDF
      # ==============================================
      parsed_full <- parse_ast_chart_report_full(pdf_obj)
      
      parsed_wide <- parsed_full$ast_wide |>
        dplyr::mutate(
          processed_at = processed_at,
          parser_version = parser_version
        ) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      
      # ==============================================
      # Step 4: Update master parsed dataset
      # ==============================================
      if (file.exists(master_output_file)) {
        
        existing <- readr::read_csv(
          master_output_file,
          show_col_types = FALSE,
          col_types = readr::cols(.default = readr::col_character())
        ) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
        
        missing_in_existing <- setdiff(names(parsed_wide), names(existing))
        if (length(missing_in_existing) > 0) {
          existing[missing_in_existing] <- "(Empty)"
        }
        
        missing_in_new <- setdiff(names(existing), names(parsed_wide))
        if (length(missing_in_new) > 0) {
          parsed_wide[missing_in_new] <- "(Empty)"
        }
        
        parsed_wide <- parsed_wide[, names(existing), drop = FALSE]
        existing <- existing[, names(existing), drop = FALSE]
        
        combined <- dplyr::bind_rows(parsed_wide, existing) |>
          dplyr::distinct(source_file, isolate, .keep_all = TRUE)
        
      } else {
        combined <- parsed_wide
      }
      
      readr::write_csv(combined, master_output_file)
      saveRDS(combined, master_output_rds)
      
      # ==============================================
      # Step 5: Map to REDCap
      # ==============================================
      redcap_ready <- map_redcap(
        parsed_df = parsed_wide,
        mapping_file = mapping_file,
        required_redcap_fields = required_redcap_fields,
        default_fill = ""
      )
      
      # ==============================================
      # Step 6: Populate REDCap technical fields
      # ==============================================
      redcap_ready <- redcap_ready |>
        dplyr::mutate(
          redcap_event_name = "microbiology"
        ) |>
        dplyr::select(-dplyr::any_of(c(
          "record_id",
          "redcap_repeat_instrument",
          "redcap_repeat_instance",
          "parser_version",
          "processed_at",
          "validation_status"
        ))) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      
      # Optional quick diagnostic
      message("patient_id preview:")
      print(redcap_ready[, intersect("patient_id", names(redcap_ready)), drop = FALSE])
      
      # ==============================================
      # Step 7: Validate
      # ==============================================
      validation_result <- validate_redcap_data(
        redcap_df = redcap_ready,
        required_fields = required_redcap_fields,
        key_fields = key_fields,
        marker_tests = marker_tests,
        parser_version = parser_version,
        quiet = FALSE
      )
      
      # Save validation outputs for each file
      readr::write_csv(
        validation_result$summary,
        file.path(
          run_output_dir,
          paste0(tools::file_path_sans_ext(current_name), "_validation_summary_", timestamp_tag, ".csv")
        )
      )
      
      readr::write_csv(
        validation_result$flagged_issues,
        file.path(
          run_output_dir,
          paste0(tools::file_path_sans_ext(current_name), "_flagged_issues_", timestamp_tag, ".csv")
        )
      )
      
      readr::write_csv(
        validation_result$flagged_rows,
        file.path(
          run_output_dir,
          paste0(tools::file_path_sans_ext(current_name), "_flagged_rows_", timestamp_tag, ".csv")
        )
      )
      
      # Console diagnostics
      print(validation_result$summary)
      
      if (nrow(validation_result$flagged_issues) > 0) {
        message("Flagged issues for: ", current_name)
        print(
          validation_result$flagged_issues |>
            dplyr::select(
              dplyr::any_of(c(
                "row_id__",
                "issue_type",
                "antibiotic",
                "mic",
                "interpretation_raw",
                "interpretation",
                "test_class"
              ))
            )
        )
      }
      
      if (nrow(validation_result$flagged_rows) > 0) {
        message("Flagged row preview for: ", current_name)
        cols_to_show <- intersect(
          c("patient_id", "validation_status"),
          names(validation_result$flagged_rows)
        )
        print(validation_result$flagged_rows[, cols_to_show, drop = FALSE])
      }
      
      # ==============================================
      # Step 8: Stop upload if validation failed
      # ==============================================
      if (nrow(validation_result$upload_ready) == 0) {
        
        upload_result <- list(status = "No upload-ready rows")
        
        audit_row <- tibble::tibble(
          processed_at = processed_at,
          source_file = current_name,
          source_path = normalizePath(current_file, winslash = "/", mustWork = FALSE),
          parser_version = parser_version,
          ast_card_type = if ("ast_card_type" %in% names(parsed_wide)) parsed_wide$ast_card_type[1] else "",
          patient_id = if ("patient_id" %in% names(parsed_wide)) parsed_wide$patient_id[1] else "",
          isolate = if ("isolate" %in% names(parsed_wide)) parsed_wide$isolate[1] else "",
          validation_status = validation_result$overall_status,
          upload_status = upload_result$status,
          n_rows_upload_ready = 0,
          n_rows_flagged = nrow(validation_result$flagged_rows),
          n_issue_records = nrow(validation_result$flagged_issues),
          dry_run_upload = as.character(dry_run_upload)
        )
        
        append_audit_row(audit_row, audit_log_file)
        
        message("Completed with validation failure: ", current_name)
        message("Validation status: ", validation_result$overall_status)
        message("Upload status: ", upload_result$status)
        
      } else {
        
        # ============================================
        # Step 9: Clean upload-ready data for REDCap
        # ============================================
        upload_df <- clean_data(validation_result$upload_ready, quiet = FALSE) |>
          dplyr::select(-dplyr::any_of(c(
            "record_id",
            "redcap_repeat_instrument",
            "redcap_repeat_instance",
            "parser_version",
            "processed_at",
            "validation_status"
          ))) |>
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
        
        # Save upload-ready extract
        readr::write_csv(
          upload_df,
          file.path(
            run_output_dir,
            paste0(tools::file_path_sans_ext(current_name), "_upload_ready_", timestamp_tag, ".csv")
          )
        )
        
        # ============================================
        # Step 10: Upload
        # ============================================
        upload_result <- upload_redcap(
          redcap_df = upload_df,
          config_file = config_file,
          required_fields = required_redcap_fields,
          key_fields = key_fields,
          marker_tests = marker_tests,
          parser_version = parser_version,
          dry_run = dry_run_upload,
          output_dir = run_output_dir,
          batch_size = batch_size_upload,
          quiet = FALSE
        )
        
        # ============================================
        # Step 11: Write audit log
        # ============================================
        audit_row <- tibble::tibble(
          processed_at = processed_at,
          source_file = current_name,
          source_path = normalizePath(current_file, winslash = "/", mustWork = FALSE),
          parser_version = parser_version,
          ast_card_type = if ("ast_card_type" %in% names(parsed_wide)) as.character(parsed_wide$ast_card_type[1]) else "",
          patient_id = if ("patient_id" %in% names(parsed_wide)) as.character(parsed_wide$patient_id[1]) else "",
          isolate = if ("isolate" %in% names(parsed_wide)) as.character(parsed_wide$isolate[1]) else "",
          validation_status = as.character(validation_result$overall_status),
          upload_status = as.character(upload_result$status),
          n_rows_upload_ready = as.character(nrow(validation_result$upload_ready)),
          n_rows_flagged = as.character(nrow(validation_result$flagged_rows)),
          n_issue_records = as.character(nrow(validation_result$flagged_issues)),
          dry_run_upload = as.character(dry_run_upload),
          upload_log_file = if (!is.null(upload_result$upload_log_file)) as.character(upload_result$upload_log_file) else "",
          error_message = if (!is.null(upload_result$failed_rows) && nrow(upload_result$failed_rows) > 0) {
            "Upload completed with failed rows"
          } else {
            ""
          }
        )
        
        append_audit_row(audit_row, audit_log_file)
        
        message("Completed successfully: ", current_name)
        message("Validation status: ", validation_result$overall_status)
        message("Upload status: ", upload_result$status)
      }
      
    }, error = function(e) {
      
      error_row <- tibble::tibble(
        processed_at = as.character(Sys.time()),
        source_file = current_name,
        source_path = normalizePath(current_file, winslash = "/", mustWork = FALSE),
        parser_version = parser_version,
        ast_card_type = if (exists("parsed_wide") && "ast_card_type" %in% names(parsed_wide)) as.character(parsed_wide$ast_card_type[1]) else "",
        patient_id = if (exists("parsed_wide") && "patient_id" %in% names(parsed_wide)) as.character(parsed_wide$patient_id[1]) else "",
        isolate = if (exists("parsed_wide") && "isolate" %in% names(parsed_wide)) as.character(parsed_wide$isolate[1]) else "",
        validation_status = "Error",
        upload_status = "Not reached",
        n_rows_upload_ready = "0",
        n_rows_flagged = "0",
        n_issue_records = "0",
        dry_run_upload = as.character(dry_run_upload),
        upload_log_file = "",
        error_message = as.character(e$message)
      )
      
      append_audit_row(error_row, audit_log_file)
      
      message("Error processing ", current_name, ": ", e$message)
    })
  }
  
  message("Batch processing completed.")
}

if (exists("combined")) {
  View(combined)
}