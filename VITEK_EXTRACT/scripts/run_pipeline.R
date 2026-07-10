# ==================================================
# scripts/run_pipeline.R
# VITEK PDF -> Parse -> Save Master -> Map -> Validate -> Clean -> Upload
# ==================================================

# --------------------------------------------------
# Load packages and functions
# --------------------------------------------------
source(here::here("scripts", "packages.R"))

source(here::here("functions", "read_vitek_pdf_full.R"))
source(here::here("functions", "parse_ast_chart_report_full.R"))
source(here::here("functions", "map_redcap.R"))
source(here::here("functions", "validate_redcap_data.R"))
source(here::here("functions", "clean_data.R"))
source(here::here("functions", "load_redcap_config.R"))
source(here::here("functions", "upload_redcap.R"))

# --------------------------------------------------
# User settings
# --------------------------------------------------
input_file <- here::here("data_raw", "example_report_01.pdf")

# keep these in main output folder
output_file <- here::here("output", "vitek_ast_master.csv")
output_rds  <- here::here("output", "vitek_ast_master.rds")

parser_version <- "v1.0.0"

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
  paste0("single_run_", run_timestamp)
)

dir.create(run_output_dir, recursive = TRUE, showWarnings = FALSE)

message("Run output directory created:")
message(run_output_dir)

# --------------------------------------------------
# Step 1: Read PDF
# --------------------------------------------------
pdf_obj <- read_vitek_pdf_full(input_file)

# --------------------------------------------------
# Step 2: Parse PDF
# --------------------------------------------------
parsed_full <- parse_ast_chart_report_full(pdf_obj)

parsed_wide <- parsed_full$ast_wide |>
  dplyr::mutate(
    processed_at = as.character(Sys.time()),
    parser_version = parser_version
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# --------------------------------------------------
# Step 3: Update master parsed dataset
# --------------------------------------------------
if (file.exists(output_file)) {
  
  existing <- readr::read_csv(
    output_file,
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
  existing    <- existing[, names(existing), drop = FALSE]
  
  combined <- dplyr::bind_rows(parsed_wide, existing) |>
    dplyr::distinct(source_file, isolate, .keep_all = TRUE)
  
} else {
  combined <- parsed_wide
}

readr::write_csv(combined, output_file)
saveRDS(combined, output_rds)

message("Updated master file saved to: ", output_file)
message("Current number of parsed records: ", nrow(combined))

# --------------------------------------------------
# Step 4: Map to REDCap structure
# --------------------------------------------------
redcap_ready <- map_redcap(
  parsed_df = parsed_wide,
  mapping_file = here::here("docs", "redcap_variable_mapping.csv"),
  required_redcap_fields = required_redcap_fields,
  default_fill = ""
)

# --------------------------------------------------
# Step 5: Populate REDCap technical fields
# --------------------------------------------------
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

message("patient_id preview:")
print(redcap_ready[, intersect("patient_id", names(redcap_ready)), drop = FALSE])

# --------------------------------------------------
# Step 6: Validate upload readiness
# --------------------------------------------------
validation_result <- validate_redcap_data(
  redcap_df = redcap_ready,
  required_fields = required_redcap_fields,
  key_fields = key_fields,
  marker_tests = marker_tests,
  parser_version = parser_version,
  quiet = FALSE
)

message("Validation status: ", validation_result$overall_status)

# Save validation outputs
input_stub <- tools::file_path_sans_ext(basename(input_file))

readr::write_csv(
  validation_result$summary,
  file.path(run_output_dir, paste0(input_stub, "_validation_summary_", run_timestamp, ".csv"))
)

readr::write_csv(
  validation_result$flagged_issues,
  file.path(run_output_dir, paste0(input_stub, "_flagged_issues_", run_timestamp, ".csv"))
)

readr::write_csv(
  validation_result$flagged_rows,
  file.path(run_output_dir, paste0(input_stub, "_flagged_rows_", run_timestamp, ".csv"))
)

# Optional review
View(validation_result$summary)
View(validation_result$flagged_issues)
View(validation_result$upload_ready)

# --------------------------------------------------
# Step 7: Stop if no upload-ready rows
# --------------------------------------------------
if (nrow(validation_result$upload_ready) == 0) {
  
  message("No upload-ready rows after validation.")
  
} else {
  
  # ------------------------------------------------
  # Step 8: Clean upload-ready data for REDCap
  # ------------------------------------------------
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
    file.path(run_output_dir, paste0(input_stub, "_upload_ready_", run_timestamp, ".csv"))
  )
  
  # ------------------------------------------------
  # Step 9: Upload to REDCap
  # ------------------------------------------------
  upload_result <- upload_redcap(
    redcap_df = upload_df,
    config_file = here::here("config", "redcap_config.yml"),
    required_fields = required_redcap_fields,
    key_fields = key_fields,
    marker_tests = marker_tests,
    parser_version = parser_version,
    dry_run = dry_run_upload,
    output_dir = run_output_dir,
    batch_size = batch_size_upload,
    quiet = FALSE
  )
  
  message("Upload status: ", upload_result$status)
  
  # Optional review
  upload_result$upload_log
}
