# functions/read_vitek_pdf.R

read_vitek_pdf <- function(file_path,
                           save_raw_text = TRUE,
                           output_dir = here::here("data_processed"),
                           output_file = NULL,
                           quiet = FALSE) {
  
  # ---- checks ----
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required but not installed.")
  }
  
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required but not installed.")
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required but not installed.")
  }
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  file_ext <- tolower(tools::file_ext(file_path))
  if (file_ext != "pdf") {
    stop("Input file must be a PDF. Supplied file: ", basename(file_path))
  }
  
  # ---- read pdf ----
  pdf_text_pages <- tryCatch(
    pdftools::pdf_text(file_path),
    error = function(e) {
      stop("Unable to read PDF file. Details: ", e$message)
    }
  )
  
  if (length(pdf_text_pages) == 0) {
    stop("No text could be extracted from the PDF. !!Confirm Content!!")
  }
  
  # ---- clean page text lightly ----
  pdf_text_pages <- pdf_text_pages |>
    stringr::str_replace_all("\r", "\n") |>
    stringr::str_replace_all("[\u00A0]", " ")   # replace non-breaking spaces
  
  pdf_text_full <- paste(pdf_text_pages, collapse = "\n\n")
  
  # ---- metadata ----
  file_info <- fs::file_info(file_path)
  
  result <- list(
    file_name = basename(file_path),
    file_path = normalizePath(file_path, winslash = "/", mustWork = FALSE),
    n_pages = length(pdf_text_pages),
    file_size = unname(file_info$size),
    page_text = pdf_text_pages,
    full_text = pdf_text_full
  )
  
  # ---- optionally save raw extracted text ----
  if (isTRUE(save_raw_text)) {
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    if (is.null(output_file)) {
      base_name <- tools::file_path_sans_ext(basename(file_path))
      output_file <- paste0(base_name, "_raw_text.txt")
    }
    
    output_path <- file.path(output_dir, output_file)
    
    tryCatch(
      writeLines(pdf_text_pages, con = output_path, useBytes = TRUE),
      error = function(e) {
        warning("PDF text was read successfully, but raw text could not be saved. Details: ", e$message)
      }
    )
    
    result$raw_text_output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  }
  
  # ---- optional message ----
  if (!quiet) {
    message("PDF successfully read: ", basename(file_path))
    message("Pages extracted: ", length(pdf_text_pages))
    
    if (!is.null(result$raw_text_output_path)) {
      message("Raw text saved to: ", result$raw_text_output_path)
    }
  }
  
  return(result)
}