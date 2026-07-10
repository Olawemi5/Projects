# functions/parse_ast_chart_report.R

parse_ast_chart_report <- function(pdf_object, quiet = FALSE) {
  
  if (is.null(pdf_object)) {
    stop("pdf_object is NULL.")
  }
  
  if (!is.list(pdf_object) || !("full_text" %in% names(pdf_object))) {
    stop("pdf_object must be the output from read_vitek_pdf(), containing 'full_text'.")
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")
  if (!requireNamespace("janitor", quietly = TRUE)) stop("Package 'janitor' is required.")
  
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
  }
  
  clean_value <- function(x) {
    x |>
      stringr::str_replace_all("\\s+", " ") |>
      stringr::str_trim()
  }
  
  empty_if_na <- function(x) {
    x <- clean_value(x)
    ifelse(is.na(x) | x == "", "(Empty)", x)
  }
  
  extract_first_match <- function(text, pattern) {
    m <- stringr::str_match(text, pattern)
    if (nrow(m) == 0 || ncol(m) < 2) return(NA_character_)
    clean_value(m[, 2])
  }
  
  txt <- pdf_object$full_text |>
    stringr::str_replace_all("\r", "\n") |>
    stringr::str_replace_all("[\u00A0]", " ") |>
    stringr::str_replace_all("[ \t]+", " ")
  
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- clean_value(lines)
  lines <- lines[!is.na(lines)]
  
  if (length(lines) == 0) {
    stop("No readable lines found in PDF text.")
  }
  
  # -------------------------
  # Metadata
  # -------------------------
  biomerieux_customer <- extract_first_match(
    txt,
    "^(.*?)\\s+bioM[ée]rieux Customer:"
  )
  
  report_printed_datetime <- extract_first_match(
    txt,
    "Printed\\s+(.+?WAT)"
  )
  
  patient_name <- extract_first_match(
    txt,
    "Patient Name:\\s*(.*?)\\s+Patient ID:"
  )
  
  patient_id <- extract_first_match(
    txt,
    "Patient ID:\\s*([^\\n]+)"
  )
  
  physician <- extract_first_match(
    txt,
    "Physician:\\s*([^\\n]*)"
  )
  
  location <- extract_first_match(
    txt,
    "Location:\\s*(.*?)\\s+Physician:"
  )
  
  lab_id <- extract_first_match(
    txt,
    "Lab ID:\\s*([^\\s]+)"
  )
  
  isolate_number <- extract_first_match(
    txt,
    "Isolate Number:\\s*([^\\n]+)"
  )
  
  selected_organism <- extract_first_match(
    txt,
    "Selected Organism\\s*:\\s*([^\\n]+)"
  )
  
  if (is.na(selected_organism) || selected_organism == "") {
    selected_organism <- extract_first_match(
      txt,
      "Selected Organism\\s+([^\\n]+)"
    )
  }
  
  identified_organism <- extract_first_match(
    txt,
    "\\d+%\\s+Probability\\s+([^\\n]+)"
  )
  
  organism_probability_pct <- extract_first_match(
    txt,
    "(\\d+)%\\s+Probability"
  )
  
  bionumber <- extract_first_match(
    txt,
    "Bionumber:\\s*([A-Za-z0-9]+)"
  )
  
  id_analysis_time <- extract_first_match(
    txt,
    "Identification Information\\s+Analysis Time:\\s*([0-9.]+\\s*hours)"
  )
  
  id_status <- extract_first_match(
    txt,
    "Identification Information\\s+Analysis Time:\\s*[0-9.]+\\s*hours\\s+Status:\\s*([^\\n]+)"
  )
  
  id_analysis_messages <- extract_first_match(
    txt,
    "ID Analysis Messages\\s*(.*?)\\s*Susceptibility Information"
  )
  
  ast_analysis_time <- extract_first_match(
    txt,
    "Susceptibility Information\\s+Analysis Time:\\s*([0-9.]+\\s*hours)"
  )
  
  ast_status <- extract_first_match(
    txt,
    "Susceptibility Information\\s+Analysis Time:\\s*[0-9.]+\\s*hours\\s+Status:\\s*([^\\n]+)"
  )
  
  aes_findings_confidence <- extract_first_match(
    txt,
    "AES Findings\\s+Confidence:\\s*([^\\n]+)"
  )
  
  comments <- extract_first_match(
    txt,
    "Comments:\\s*(.*?)\\s*Identification Information"
  )
  
  meta_df <- tibble::tibble(
    source_file = pdf_object$file_name %||% NA_character_,
    biomerieux_customer = empty_if_na(biomerieux_customer),
    report_printed_datetime = empty_if_na(report_printed_datetime),
    patient_name = empty_if_na(patient_name),
    patient_id = empty_if_na(patient_id),
    physician = empty_if_na(physician),
    location = empty_if_na(location),
    lab_id = empty_if_na(lab_id),
    isolate_number = suppressWarnings(as.integer(clean_value(isolate_number))),
    selected_organism = empty_if_na(selected_organism),
    identified_organism = empty_if_na(identified_organism),
    organism_probability_pct = suppressWarnings(as.numeric(clean_value(organism_probability_pct))),
    bionumber = empty_if_na(bionumber),
    id_analysis_time = empty_if_na(id_analysis_time),
    id_status = empty_if_na(id_status),
    ast_analysis_time = empty_if_na(ast_analysis_time),
    ast_status = empty_if_na(ast_status),
    aes_findings_confidence = empty_if_na(aes_findings_confidence),
    id_analysis_messages = empty_if_na(id_analysis_messages),
    comments = empty_if_na(comments)
  )
  
  # -------------------------
  # AST block
  # -------------------------
  start_idx <- which(lines == "Antimicrobial MIC Interpretation Antimicrobial MIC Interpretation")
  end_idx   <- which(lines == "*= AES modified **= User modified")
  
  if (length(start_idx) == 0) stop("Could not locate AST block start.")
  if (length(end_idx) == 0) stop("Could not locate AST block end.")
  
  ast_lines <- lines[(start_idx[1] + 1):(end_idx[1] - 1)]
  ast_lines <- ast_lines[ast_lines != ""]
  
  if (length(ast_lines) == 0) {
    stop("AST block found, but no AST result lines were extracted.")
  }
  
  ast_block <- paste(ast_lines, collapse = " ")
  ast_block <- clean_value(ast_block)
  ast_block <- stringr::str_replace(ast_block, "\\*\\= AES modified.*$", "")
  ast_block <- clean_value(ast_block)
  
  # -------------------------
  # Known antibiotics
  # -------------------------
  antibiotic_dict <- c(
    "ESBL",
    "Ertapenem",
    "Ampicillin",
    "Ampicillin/Sulbactam",
    "Piperacillin",
    "Cefazolin",
    "Cefoxitin",
    "Ceftazidime",
    "Ceftriaxone",
    "Cefepime",
    "Meropenem",
    "Amikacin",
    "Gentamicin",
    "Tobramycin",
    "Ciprofloxacin",
    "Levofloxacin",
    "Nitrofurantoin",
    "Trimethoprim/Sulfamethoxazole"
  )
  
  antibiotic_regex <- antibiotic_dict |>
    stringr::str_replace_all("([./()])", "\\\\\\1")
  
  antibiotic_regex <- antibiotic_regex[order(nchar(antibiotic_regex), decreasing = TRUE)]
  antibiotic_pattern <- paste(antibiotic_regex, collapse = "|")
  
  entry_pattern <- paste0(
    "(", antibiotic_pattern, ")",
    "\\s+",
    "(<=\\s*\\d+(?:\\.\\d+)?\\*?|>=\\s*\\d+(?:\\.\\d+)?\\*?|<\\s*\\d+(?:\\.\\d+)?\\*?|>\\s*\\d+(?:\\.\\d+)?\\*?|=\\s*\\d+(?:\\.\\d+)?\\*?|\\d+(?:\\.\\d+)?\\*?)",
    "\\s+",
    "(\\*\\*[RIS]|\\*[RIS]|[RIS])"
  )
  
  matches <- stringr::str_match_all(ast_block, entry_pattern)[[1]]
  
  if (nrow(matches) == 0) {
    stop("No AST entries could be parsed from the AST block.")
  }
  
  ast_long <- tibble::tibble(
    antibiotic = clean_value(matches[, 2]),
    mic = clean_value(matches[, 3]),
    interpretation_token = clean_value(matches[, 4])
  ) |>
    dplyr::mutate(
      interpretation_raw = dplyr::case_when(
        stringr::str_detect(interpretation_token, "R$") ~ "R",
        stringr::str_detect(interpretation_token, "I$") ~ "I",
        stringr::str_detect(interpretation_token, "S$") ~ "S",
        TRUE ~ NA_character_
      ),
      interpretation = dplyr::case_when(
        stringr::str_detect(interpretation_token, "^\\*\\*") ~ "User modified",
        stringr::str_detect(interpretation_token, "^\\*") ~ "AES modified",
        TRUE ~ "Standard"
      )
    ) |>
    dplyr::select(antibiotic, mic, interpretation_raw, interpretation) |>
    dplyr::filter(!is.na(antibiotic), antibiotic != "")
  
  # -------------------------
  # Add metadata to long
  # -------------------------
  ast_long <- ast_long |>
    dplyr::mutate(
      source_file = meta_df$source_file[1],
      biomerieux_customer = meta_df$biomerieux_customer[1],
      report_printed_datetime = meta_df$report_printed_datetime[1],
      patient_name = meta_df$patient_name[1],
      patient_id = meta_df$patient_id[1],
      physician = meta_df$physician[1],
      location = meta_df$location[1],
      lab_id = meta_df$lab_id[1],
      isolate_number = meta_df$isolate_number[1],
      selected_organism = meta_df$selected_organism[1],
      identified_organism = meta_df$identified_organism[1],
      organism_probability_pct = meta_df$organism_probability_pct[1],
      bionumber = meta_df$bionumber[1],
      id_analysis_time = meta_df$id_analysis_time[1],
      id_status = meta_df$id_status[1],
      ast_analysis_time = meta_df$ast_analysis_time[1],
      ast_status = meta_df$ast_status[1],
      aes_findings_confidence = meta_df$aes_findings_confidence[1],
      id_analysis_messages = meta_df$id_analysis_messages[1],
      comments = meta_df$comments[1]
    )
  
  # -------------------------
  # Wide format with grouped antibiotic columns
  # -------------------------
  id_cols <- c(
    "source_file",
    "biomerieux_customer",
    "report_printed_datetime",
    "patient_name",
    "patient_id",
    "physician",
    "location",
    "lab_id",
    "isolate_number",
    "selected_organism",
    "identified_organism",
    "organism_probability_pct",
    "bionumber",
    "id_analysis_time",
    "id_status",
    "ast_analysis_time",
    "ast_status",
    "aes_findings_confidence",
    "id_analysis_messages",
    "comments"
  )
  
  ast_wide <- ast_long |>
    dplyr::select(
      dplyr::all_of(id_cols),
      antibiotic,
      mic,
      interpretation_raw,
      interpretation
    ) |>
    tidyr::pivot_wider(
      names_from = antibiotic,
      values_from = c(mic, interpretation_raw, interpretation),
      names_glue = "{antibiotic}_{.value}"
    ) |>
    janitor::clean_names()
  
  # Reorder antibiotic columns to appear as:
  # antibiotic_mic, antibiotic_interpretation_raw, antibiotic_interpretation
  antibiotic_clean <- antibiotic_dict |>
    janitor::make_clean_names()
  
  ordered_ast_cols <- unlist(lapply(antibiotic_clean, function(abx) {
    c(
      paste0(abx, "_mic"),
      paste0(abx, "_interpretation_raw"),
      paste0(abx, "_interpretation")
    )
  }))
  
  ordered_ast_cols <- ordered_ast_cols[ordered_ast_cols %in% names(ast_wide)]
  
  final_cols <- c(
    "source_file",
    "biomerieux_customer",
    "report_printed_datetime",
    "patient_name",
    "patient_id",
    "physician",
    "location",
    "lab_id",
    "isolate_number",
    "selected_organism",
    "identified_organism",
    "organism_probability_pct",
    "bionumber",
    "id_analysis_time",
    "id_status",
    "ast_analysis_time",
    "ast_status",
    "aes_findings_confidence",
    ordered_ast_cols,
    "id_analysis_messages",
    "comments"
  )
  
  final_cols <- final_cols[final_cols %in% names(ast_wide)]
  ast_wide <- ast_wide |>
    dplyr::select(dplyr::all_of(final_cols))
  
  if (!quiet) {
    message("AST Chart Report parsed successfully.")
    message("AST entries parsed: ", nrow(ast_long))
    message("Wide dataset rows: ", nrow(ast_wide))
  }
  
  list(
    metadata = meta_df,
    ast_long = ast_long,
    ast_wide = ast_wide,
    raw_ast_lines = ast_lines,
    raw_ast_block = ast_block
  )
}