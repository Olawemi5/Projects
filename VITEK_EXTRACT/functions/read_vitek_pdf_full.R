# functions/read_vitek_pdf_full.R

read_vitek_pdf_full <- function(file_path,
                                save_raw_text = TRUE,
                                output_dir = here::here("data_processed"),
                                output_file = NULL,
                                quiet = FALSE) {
  
  # -----------------------------
  # package checks
  # -----------------------------
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required but not installed.")
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required but not installed.")
  }
  
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("Package 'fs' is required but not installed.")
  }
  
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }
  
  # -----------------------------
  # checks
  # -----------------------------
  if (missing(file_path) || is.null(file_path) || file_path == "") {
    stop("Please provide a valid PDF file path.")
  }
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  if (tolower(tools::file_ext(file_path)) != "pdf") {
    stop("Input file must be a PDF: ", basename(file_path))
  }
  
  # -----------------------------
  # helpers
  # -----------------------------
  clean_value <- function(x) {
    x |>
      stringr::str_replace_all("\r", "\n") |>
      stringr::str_replace_all("[\u00A0]", " ") |>
      stringr::str_replace_all("[ \t]+", " ") |>
      stringr::str_trim()
  }
  
  extract_first_match <- function(text, pattern) {
    m <- stringr::str_match(text, pattern)
    if (nrow(m) == 0 || ncol(m) < 2) return(NA_character_)
    clean_value(m[, 2])
  }
  
  extract_all_matches <- function(text, pattern) {
    m <- stringr::str_match_all(text, pattern)[[1]]
    if (nrow(m) == 0 || ncol(m) < 2) return(character(0))
    unique(clean_value(m[, 2]))
  }
  
  # -----------------------------
  # read PDF
  # -----------------------------
  page_text <- tryCatch(
    pdftools::pdf_text(file_path),
    error = function(e) {
      stop("Unable to read PDF file. Details: ", e$message)
    }
  )
  
  if (length(page_text) == 0) {
    stop("No text could be extracted from the PDF.")
  }
  
  page_text <- vapply(page_text, clean_value, FUN.VALUE = character(1))
  full_text <- paste(page_text, collapse = "\n\n")
  
  # -----------------------------
  # basic metadata for reading stage
  # -----------------------------
  file_info <- fs::file_info(file_path)
  
  biomerieux_customer <- extract_first_match(
    full_text,
    "^(.*?)\\s+bioM[ée]rieux Customer:"
  )
  
  report_title <- extract_first_match(
    full_text,
    "bioM[ée]rieux Customer:\\s*([^\\n]+)"
  )
  
  patient_name <- extract_first_match(
    full_text,
    "Patient Name:\\s*(.*?)\\s+Patient ID:"
  )
  
  patient_id <- extract_first_match(
    full_text,
    "Patient ID:\\s*([^\\n]+)"
  )
  
  isolate <- extract_first_match(
    full_text,
    "Isolate:\\s*([^\\n]+?)\\s+Bench:"
  )
  if (is.na(isolate) || !nzchar(isolate)) {
    isolate <- extract_first_match(
      full_text,
      "Isolate Number\\s*:\\s*([^\\n]+)"
    )
  }
  
  bench <- extract_first_match(
    full_text,
    "Bench:\\s*([^\\n]+)"
  )
  
  selected_organism <- extract_first_match(
    full_text,
    "Selected Organism\\s*:\\s*([^\\n]+)"
  )
  
  bionumber <- extract_first_match(
    full_text,
    "Bionumber:\\s*([A-Za-z0-9]+)"
  )
  
  installed_version <- extract_first_match(
    full_text,
    "Installed VITEK 2 Systems Version:\\s*([^\\n]+)"
  )
  
  # all card type entries in the file
  card_types <- extract_all_matches(
    full_text,
    "Card Type:\\s*([^\\s]+)"
  )
  
  # main ID card family
  id_card_type <- extract_first_match(
    full_text,
    "Card Type:\\s*(GN|GP|YST)\\s+Bar Code:"
  )
  
  # AST card family
  ast_card_type <- extract_first_match(
    full_text,
    "Card Type:\\s*(AST-[A-Z0-9]+)\\s+Bar Code:"
  )
  
  report_version <- extract_first_match(
    full_text,
    "Report Version:\\s*([^\\n]+)"
  )
  
  printed_by <- extract_first_match(
    full_text,
    "Printed by:\\s*([^\\n]+?)\\s+Last Updated:"
  )
  
  last_updated <- extract_first_match(
    full_text,
    "Last Updated:\\s*(.*?)\\s+By:"
  )
  
  updated_by <- extract_first_match(
    full_text,
    "Last Updated:\\s*.*?\\s+By:\\s*(.*?)\\s+Report Version:"
  )
  
  # -----------------------------
  # page-level data
  # -----------------------------
  page_df <- tibble::tibble(
    page_number = seq_along(page_text),
    page_text = unname(page_text)
  )
  
  # -----------------------------
  # summary metadata
  # -----------------------------
  metadata_df <- tibble::tibble(
    source_file = basename(file_path),
    file_path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
    file_size = unname(file_info$size),
    n_pages = length(page_text),
    biomerieux_customer = biomerieux_customer,
    report_title = report_title,
    patient_name = patient_name,
    patient_id = patient_id,
    isolate = isolate,
    bench = bench,
    selected_organism = selected_organism,
    bionumber = bionumber,
    installed_vitek_version = installed_version,
    id_card_type = id_card_type,
    ast_card_type = ast_card_type,
    all_card_types = paste(card_types, collapse = " | "),
    printed_by = printed_by,
    last_updated = last_updated,
    updated_by = updated_by,
    report_version = report_version
  )
  
  # -----------------------------
  # save raw extracted text
  # -----------------------------
  raw_text_output_path <- NA_character_
  
  if (isTRUE(save_raw_text)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    if (is.null(output_file)) {
      base_name <- tools::file_path_sans_ext(basename(file_path))
      output_file <- paste0(base_name, "_raw_text.txt")
    }
    
    raw_text_output_path <- file.path(output_dir, output_file)
    
    tryCatch(
      writeLines(page_text, con = raw_text_output_path, useBytes = TRUE),
      error = function(e) {
        warning("PDF was read, but raw text could not be saved: ", e$message)
      }
    )
  }
  
  # -----------------------------
  # console messages
  # -----------------------------
  if (!quiet) {
    message("Full VITEK report read successfully: ", basename(file_path))
    message("Pages extracted: ", length(page_text))
    message("ID card type detected: ", metadata_df$id_card_type[1] %||% NA_character_)
    message("AST card type detected: ", metadata_df$ast_card_type[1] %||% NA_character_)
    
    if (!is.na(raw_text_output_path)) {
      message("Raw text saved to: ", normalizePath(raw_text_output_path, winslash = "/", mustWork = FALSE))
    }
  }
  
  # -----------------------------
  # return
  # -----------------------------
  list(
    metadata = metadata_df,
    page_data = page_df,
    page_text = page_text,
    full_text = full_text,
    raw_text_output_path = raw_text_output_path
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
