# ==================================================
# scripts/update_manifest.R
# Build / refresh docs/files_to_process.txt
# from PDFs in data_raw
# ==================================================

source(here::here("scripts", "packages.R"))

# --------------------------------------------------
# settings
# --------------------------------------------------
data_raw_dir  <- here::here("data_raw")
docs_dir      <- here::here("docs")
manifest_file <- file.path(docs_dir, "files_to_process.txt")

# --------------------------------------------------
# create docs folder if missing
# --------------------------------------------------
if (!dir.exists(docs_dir)) {
  dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
}

# --------------------------------------------------
# list all PDFs in data_raw
# --------------------------------------------------
current_files <- list.files(
  path = data_raw_dir,
  pattern = "\\.pdf$",
  ignore.case = TRUE,
  full.names = FALSE
)

# clean and sort
current_files <- trimws(current_files)
current_files <- current_files[current_files != ""]
current_files <- unique(current_files)
current_files <- sort(current_files)

# --------------------------------------------------
# write manifest fresh
# --------------------------------------------------
writeLines(current_files, manifest_file)

# --------------------------------------------------
# messages
# --------------------------------------------------
message("Manifest update completed.")
message("Manifest file: ", manifest_file)
message("Number of PDF files listed: ", length(current_files))

if (length(current_files) > 0) {
  print(current_files)
} else {
  message("No PDF files found in data_raw.")
}