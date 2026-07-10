# functions/parse_ast_chart_report_full.R

parse_ast_chart_report_full <- function(pdf_object, quiet = FALSE) {
  
  if (is.null(pdf_object)) {
    stop("pdf_object is NULL.")
  }
  
  if (!is.list(pdf_object) || !all(c("full_text", "page_text") %in% names(pdf_object))) {
    stop("pdf_object must be the output from read_vitek_pdf_full().")
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
      stringr::str_replace_all("\r", "\n") |>
      stringr::str_replace_all("[\u00A0]", " ") |>
      stringr::str_replace_all("[ \t]+", " ") |>
      stringr::str_replace_all("\\n{2,}", "\n") |>
      stringr::str_trim()
  }
  
  empty_if_na <- function(x) {
    x <- clean_value(x)
    ifelse(is.na(x) | x == "", "(Empty)", x)
  }
  
  extract_first_match <- function(text, pattern) {
    m <- stringr::str_match(text, stringr::regex(pattern, dotall = TRUE))
    if (nrow(m) == 0 || ncol(m) < 2) return(NA_character_)
    clean_value(m[, 2])
  }
  
  extract_block <- function(text, start_pattern, end_pattern) {
    pattern <- stringr::regex(
      paste0(start_pattern, "(.*?)", end_pattern),
      dotall = TRUE
    )
    m <- stringr::str_match(text, pattern)
    if (nrow(m) == 0 || ncol(m) < 2) return(NA_character_)
    clean_value(m[, 2])
  }
  
  escape_regex <- function(x) {
    stringr::str_replace_all(x, "([.|()\\[\\]{}+?^$\\\\/])", "\\\\\\1")
  }

  infer_ast_card_type <- function(text, antibiotic_dicts) {
    if (is.na(text) || !nzchar(text)) {
      return(list(
        ast_card_type = NA_character_,
        matched_antibiotics = character(),
        match_score = 0L
      ))
    }

    scores <- vapply(names(antibiotic_dicts), function(card_name) {
      antibiotics <- antibiotic_dicts[[card_name]]
      patterns <- vapply(antibiotics, function(abx) {
        paste0("(?<!\\S)", escape_regex(abx), "(?!\\S)")
      }, FUN.VALUE = character(1))
      sum(vapply(patterns, function(pattern) {
        stringr::str_detect(text, stringr::regex(pattern))
      }, FUN.VALUE = logical(1)))
    }, FUN.VALUE = integer(1))

    max_score <- max(scores)
    if (max_score <= 0L) {
      return(list(
        ast_card_type = NA_character_,
        matched_antibiotics = character(),
        match_score = 0L
      ))
    }

    winning_cards <- names(scores)[scores == max_score]
    if (length(winning_cards) != 1L) {
      return(list(
        ast_card_type = NA_character_,
        matched_antibiotics = character(),
        match_score = max_score
      ))
    }

    winning_card <- winning_cards[[1]]
    matched_antibiotics <- antibiotic_dicts[[winning_card]][vapply(
      antibiotic_dicts[[winning_card]],
      function(abx) {
        stringr::str_detect(
          text,
          stringr::regex(paste0("(?<!\\S)", escape_regex(abx), "(?!\\S)"))
        )
      },
      FUN.VALUE = logical(1)
    )]

    list(
      ast_card_type = winning_card,
      matched_antibiotics = matched_antibiotics,
      match_score = max_score
    )
  }
  
  # --------------------------------------------------
  # source text
  # --------------------------------------------------
  full_text <- clean_value(pdf_object$full_text)
  page_text <- vapply(pdf_object$page_text, clean_value, FUN.VALUE = character(1))
  
  if (length(page_text) == 0) {
    stop("No page text found in pdf_object.")
  }
  
  page1_text <- page_text[1]
  
  # --------------------------------------------------
  # normalize known wrapped/interleaved text
  # --------------------------------------------------
  normalize_report_text <- function(x) {
    x |>
      # keep long GP names intact
      stringr::str_replace_all(
        "Gentamicin High Level\\s*\\(synergy\\)",
        "Gentamicin High Level (synergy)"
      ) |>
      stringr::str_replace_all(
        "Streptomycin High Level\\s*\\(synergy\\)",
        "Streptomycin High Level (synergy)"
      ) |>
      stringr::str_replace_all(
        "Inducible Clindamycin\\s+Resistance",
        "Inducible Clindamycin Resistance"
      ) |>
      stringr::str_replace_all(
        "Cefoxitin\\s+Screen",
        "Cefoxitin Screen"
      ) |>
      # repair Trimethoprim/Sulfamethoxazole broken across lines
      stringr::str_replace_all(
        "Trimethoprim/\\s+Sulfamethoxazole",
        "Trimethoprim/Sulfamethoxazole"
      ) |>
      stringr::str_replace_all(
        "Trimethoprim/\\s*(<=|>=|<|>|=)?\\s*(\\d+(?:\\.\\d+)?)\\s*(\\*\\*[RIS]|\\*[RIS]|[RIS])\\s+Sulfamethoxazole",
        "Trimethoprim/Sulfamethoxazole \\1 \\2 \\3"
      ) |>
      # repair GP interleaving around inducible clindamycin resistance + trimethoprim
      stringr::str_replace_all(
        "Inducible Clindamycin\\s+(POS|NEG)\\s*([+-])\\s+Trimethoprim/\\s*(<=|>=|<|>|=)?\\s*(\\d+(?:\\.\\d+)?)\\s*(\\*\\*[RIS]|\\*[RIS]|[RIS])\\s+Resistance\\s+Sulfamethoxazole",
        "Inducible Clindamycin Resistance \\1 \\2 Trimethoprim/Sulfamethoxazole \\3 \\4 \\5"
      ) |>
      clean_value()
  }
  
  page1_text <- normalize_report_text(page1_text)
  full_text  <- normalize_report_text(full_text)
  
  # --------------------------------------------------
  # metadata extraction
  # --------------------------------------------------
  source_file <- pdf_object$metadata$source_file[1] %||% NA_character_
  biomerieux_customer <- pdf_object$metadata$biomerieux_customer[1] %||% NA_character_
  
  report_title <- extract_first_match(full_text, "bioM[ée]rieux Customer:\\s*([^\\n]+)")
  system_number <- extract_first_match(full_text, "System #:\\s*([^\\n]+?)\\s+Printed by:")
  printed_by <- extract_first_match(full_text, "Printed by:\\s*([^\\n]+?)\\s+Last Updated:")
  last_updated <- extract_first_match(full_text, "Last Updated:\\s*(.*?)\\s+By:")
  updated_by <- extract_first_match(full_text, "Last Updated:\\s*.*?\\s+By:\\s*(.*?)\\s+Report Version:")
  report_version <- extract_first_match(full_text, "Report Version:\\s*([^\\n]+)")
  patient_name <- extract_first_match(full_text, "Patient Name:\\s*(.*?)\\s+Patient ID:")
  patient_id <- extract_first_match(full_text, "Patient ID:\\s*([^\\n]+)")
  lab_id <- extract_first_match(full_text, "Lab ID:\\s*(.*?)\\s+(?:Isolate Number:|Last Updated:|$)")
  if (is.na(lab_id) || !nzchar(lab_id)) {
    lab_id <- extract_first_match(full_text, "Lab ID:\\s*([^\\n]+)")
  }
  isolate <- extract_first_match(full_text, "Isolate:\\s*([^\\n]+?)\\s+Bench:")
  if (is.na(isolate) || !nzchar(isolate)) {
    isolate <- extract_first_match(full_text, "Isolate Number\\s*:\\s*([^\\n]+)")
  }
  isolate_status <- extract_first_match(full_text, "Isolate:\\s*[^\\n]+\\(([^\\)]+)\\)\\s+Bench:")
  bench <- extract_first_match(full_text, "Bench:\\s*([^\\n]+)")
  
  id_card_type <- extract_first_match(full_text, "Card Type:\\s*(GN|GP|YST)\\s+Bar Code:")
  id_card_barcode <- extract_first_match(full_text, "Card Type:\\s*(?:GN|GP|YST)\\s+Bar Code:\\s*([^\\s]+)")
  id_testing_instrument <- extract_first_match(full_text, "Card Type:\\s*(?:GN|GP|YST)\\s+Bar Code:\\s*[^\\s]+\\s+Testing Instrument:\\s*([^\\n]+)")
  
  ast_card_type <- extract_first_match(full_text, "Card Type:\\s*(AST-[A-Z0-9]+)\\s+Bar Code:")
  ast_card_type_detected <- ast_card_type
  ast_card_barcode <- extract_first_match(full_text, "Card Type:\\s*AST-[A-Z0-9]+\\s+Bar Code:\\s*([^\\s]+)")
  ast_testing_instrument <- extract_first_match(full_text, "Card Type:\\s*AST-[A-Z0-9]+\\s+Bar Code:\\s*[^\\s]+\\s+Testing Instrument:\\s*([^\\n]+)")
  
  setup_technologist <- extract_first_match(full_text, "Setup Technologist:\\s*([^\\n]+)")
  bionumber <- extract_first_match(full_text, "Bionumber:\\s*([A-Za-z0-9]+)")
  selected_organism <- extract_first_match(full_text, "Organism Quantity:\\s*Selected Organism\\s*:\\s*([^\\n]+)")
  if (is.na(selected_organism) || !nzchar(selected_organism)) {
    selected_organism <- extract_first_match(full_text, "Selected Organism\\s*:\\s*([^\\n]+)")
  }
  installed_vitek_version <- extract_first_match(full_text, "Installed VITEK 2 Systems Version:\\s*([^\\n]+)")
  mic_interpretation_guideline <- extract_first_match(full_text, "MIC Interpretation Guideline:\\s*(.*?)\\s+Therapeutic Interpretation Guideline:")
  therapeutic_interpretation_guideline <- extract_first_match(full_text, "Therapeutic Interpretation Guideline:\\s*([^\\n]+)")
  aes_parameter_set_name <- extract_first_match(full_text, "AES Parameter Set Name:\\s*(.*?)\\s+AES Parameter Last Modified:")
  aes_parameter_last_modified <- extract_first_match(full_text, "AES Parameter Last Modified:\\s*([^\\n]+)")
  
  comments <- extract_block(page1_text, "Comments:\\s*", "McFarland:")
  comments <- empty_if_na(comments)
  
  mcfarland <- extract_first_match(page1_text, "McFarland:\\s*([^\\n]+)")
  
  # Identification section
  id_card <- extract_first_match(page1_text, "Identification\\s+Card:\\s*([^\\s]+)")
  id_lot_number <- extract_first_match(page1_text, "Identification\\s+Card:\\s*[^\\s]+\\s+Lot Number:\\s*([^\\s]+)")
  id_expires <- extract_first_match(page1_text, "Identification\\s+Card:\\s*[^\\s]+\\s+Lot Number:\\s*[^\\s]+\\s+Expires:\\s*([^\\n]+)")
  id_status <- extract_first_match(page1_text, "Identification\\s+Card:.*?Information\\s+Status:\\s*([^\\s]+)")
  id_analysis_time <- extract_first_match(page1_text, "Identification\\s+Card:.*?Information\\s+Status:\\s*[^\\s]+\\s+Analysis Time:\\s*([0-9.]+\\s*hours)")
  id_completed <- extract_first_match(page1_text, "Identification\\s+Card:.*?Completed:\\s*([^\\n]+)")
  if (is.na(id_status) || !nzchar(id_status)) {
    id_status <- extract_first_match(
      page1_text,
      "Identification\\s+Information\\s+Analysis Time:\\s*[0-9.]+\\s*hours\\s+Status:\\s*([^\\n]+)"
    )
  }
  if (is.na(id_analysis_time) || !nzchar(id_analysis_time)) {
    id_analysis_time <- extract_first_match(
      page1_text,
      "Identification\\s+Information\\s+Analysis Time:\\s*([0-9.]+\\s*hours)"
    )
  }
  
  organism_origin <- extract_first_match(page1_text, "Organism Origin\\s+([^\\n]+)")
  organism_probability_pct <- extract_first_match(page1_text, "(\\d+)%\\s+Probability")
  identified_organism <- extract_first_match(page1_text, "\\d+%\\s+Probability\\s+([^\\n]+)")
  id_confidence <- extract_first_match(page1_text, "Bionumber:\\s*[A-Za-z0-9]+\\s+Confidence:\\s*([^\\n]+)")
  
  analysis_organisms_tests_to_separate <- extract_block(
    page1_text,
    "Analysis Organisms and Tests to Separate:\\s*",
    "Analysis Messages:"
  )
  
  id_analysis_messages <- extract_block(
    page1_text,
    "Analysis Messages:\\s*",
    "Contraindicating Typical Biopattern\\(s\\)"
  )
  
  contraindicating_typical_biopatterns <- extract_block(
    page1_text,
    "Contraindicating Typical Biopattern\\(s\\)\\s*",
    "McFarland:"
  )
  
  # Susceptibility section
  susceptibility_card <- extract_first_match(page1_text, "Susceptibility\\s+Card:\\s*([^\\s]+)")
  susceptibility_lot_number <- extract_first_match(page1_text, "Susceptibility\\s+Card:\\s*[^\\s]+\\s+Lot Number:\\s*([^\\s]+)")
  susceptibility_expires <- extract_first_match(page1_text, "Susceptibility\\s+Card:\\s*[^\\s]+\\s+Lot Number:\\s*[^\\s]+\\s+Expires:\\s*([^\\n]+)")
  susceptibility_status <- extract_first_match(page1_text, "Susceptibility\\s+Card:.*?Information\\s+Status:\\s*([^\\s]+)")
  susceptibility_analysis_time <- extract_first_match(page1_text, "Susceptibility\\s+Card:.*?Information\\s+Status:\\s*[^\\s]+\\s+Analysis Time:\\s*([0-9.]+\\s*hours)")
  susceptibility_completed <- extract_first_match(page1_text, "Susceptibility\\s+Card:.*?Completed:\\s*([^\\n]+)")
  if (is.na(susceptibility_status) || !nzchar(susceptibility_status)) {
    susceptibility_status <- extract_first_match(
      page1_text,
      "Susceptibility\\s+Information\\s+Analysis Time:\\s*[0-9.]+\\s*hours\\s+Status:\\s*([^\\n]+)"
    )
  }
  if (is.na(susceptibility_analysis_time) || !nzchar(susceptibility_analysis_time)) {
    susceptibility_analysis_time <- extract_first_match(
      page1_text,
      "Susceptibility\\s+Information\\s+Analysis Time:\\s*([0-9.]+\\s*hours)"
    )
  }
  
  aes_last_modified <- extract_first_match(full_text, "AES Findings:\\s*Last Modified:\\s*(.*?)\\s+Parameter Set:")
  aes_parameter_set <- extract_first_match(full_text, "Parameter Set:\\s*([^\\n]+)")
  aes_confidence_level <- extract_first_match(full_text, "Confidence Level:\\s*([^\\n]+)")
  
  # --------------------------------------------------
  # card-specific panels only
  # --------------------------------------------------
  antibiotic_dicts <- list(
    "AST-GN75" = c(
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
    ),
    "AST-GP75" = c(
      "Cefoxitin Screen",
      "Clindamycin",
      "Ampicillin",
      "Linezolid",
      "Oxacillin",
      "Daptomycin",
      "Gentamicin High Level (synergy)",
      "Vancomycin",
      "Streptomycin High Level (synergy)",
      "Doxycycline",
      "Gentamicin",
      "Tetracycline",
      "Ciprofloxacin",
      "Tigecycline",
      "Levofloxacin",
      "Nitrofurantoin",
      "Moxifloxacin",
      "Rifampicin",
      "Inducible Clindamycin Resistance",
      "Trimethoprim/Sulfamethoxazole",
      "Erythromycin"
    ),
    "AST-YS08" = c(
      "Fluconazole",
      "Micafungin",
      "Voriconazole",
      "Amphotericin B",
      "Caspofungin",
      "Flucytosine"
    )
  )
  
  ast_block <- extract_block(
    page1_text,
    "Antimicrobial MIC Interpretation Antimicrobial MIC Interpretation\\s*",
    "(\\*= AES modified \\*\\*= User modified|AES Findings:?|Installed VITEK 2 Systems Version:)"
  )
  
  if (is.na(ast_block) || ast_block == "") {
    ast_block <- extract_block(
      full_text,
      "Antimicrobial MIC Interpretation Antimicrobial MIC Interpretation\\s*",
      "(\\*= AES modified \\*\\*= User modified|AES Findings:?|Installed VITEK 2 Systems Version:)"
    )
  }
  
  has_ast_block <- !(is.na(ast_block) || ast_block == "")
  antibiotic_dict <- character()
  parsed_rows <- list()
  ast_long <- tibble::tibble(
    antibiotic = character(),
    mic = character(),
    interpretation_raw = character(),
    interpretation = character()
  )

  if (has_ast_block) {
    ast_block <- normalize_report_text(clean_value(ast_block))

    if (is.na(ast_card_type) || !nzchar(ast_card_type)) {
      inferred_card <- infer_ast_card_type(ast_block, antibiotic_dicts)
      if (is.na(inferred_card$ast_card_type) || !nzchar(inferred_card$ast_card_type)) {
        inferred_card <- infer_ast_card_type(full_text, antibiotic_dicts)
      }
      ast_card_type <- inferred_card$ast_card_type
    }

    antibiotic_dict <- antibiotic_dicts[[ast_card_type]]

    if (is.null(antibiotic_dict)) {
      stop("Unsupported or unrecognized AST card type: ", ast_card_type)
    }

    # --------------------------------------------------
    # special extraction first
    # --------------------------------------------------
    ast_block_general <- ast_block

    if (ast_card_type == "AST-GN75") {
    esbl_match <- stringr::str_match(ast_block_general, "\\bESBL\\s+(POS|NEG)\\s*([+-])")
    if (nrow(esbl_match) > 0 && !all(is.na(esbl_match))) {
      parsed_rows[["ESBL"]] <- tibble::tibble(
        antibiotic = "ESBL",
        mic = esbl_match[, 2],
        interpretation_raw = esbl_match[, 3],
        interpretation = "Standard"
      )
      ast_block_general <- stringr::str_replace(
        ast_block_general,
        "\\bESBL\\s+(POS|NEG)\\s*([+-])",
        " "
      )
    }
    }
    
    if (ast_card_type == "AST-GP75") {
    cefoxitin_screen_match <- stringr::str_match(
      ast_block_general,
      "\\bCefoxitin Screen\\s+(POS|NEG)\\s*([+-])"
    )
    if (nrow(cefoxitin_screen_match) > 0 && !all(is.na(cefoxitin_screen_match))) {
      parsed_rows[["Cefoxitin Screen"]] <- tibble::tibble(
        antibiotic = "Cefoxitin Screen",
        mic = cefoxitin_screen_match[, 2],
        interpretation_raw = cefoxitin_screen_match[, 3],
        interpretation = "Standard"
      )
      ast_block_general <- stringr::str_replace(
        ast_block_general,
        "\\bCefoxitin Screen\\s+(POS|NEG)\\s*([+-])",
        " "
      )
    }

    gentamicin_synergy_match <- stringr::str_match(
      ast_block_general,
      "\\bGentamicin High Level\\s+(SYN-[A-Z]+)\\s+(\\*\\*[RIS]|\\*[RIS]|[RIS])\\b"
    )
    if (nrow(gentamicin_synergy_match) > 0 && !all(is.na(gentamicin_synergy_match))) {
      parsed_rows[["Gentamicin High Level (synergy)"]] <- tibble::tibble(
        antibiotic = "Gentamicin High Level (synergy)",
        mic = gentamicin_synergy_match[, 2],
        interpretation_raw = dplyr::case_when(
          gentamicin_synergy_match[, 3] %in% c("R", "*R", "**R") ~ "R",
          gentamicin_synergy_match[, 3] %in% c("I", "*I", "**I") ~ "I",
          gentamicin_synergy_match[, 3] %in% c("S", "*S", "**S") ~ "S",
          TRUE ~ "(Empty)"
        ),
        interpretation = dplyr::case_when(
          stringr::str_detect(gentamicin_synergy_match[, 3], "^\\*\\*") ~ "User modified",
          stringr::str_detect(gentamicin_synergy_match[, 3], "^\\*") ~ "AES modified",
          gentamicin_synergy_match[, 3] %in% c("R", "I", "S") ~ "Standard",
          TRUE ~ "(Empty)"
        )
      )
      ast_block_general <- stringr::str_replace(
        ast_block_general,
        "\\bGentamicin High Level\\s+(SYN-[A-Z]+)\\s+(\\*\\*[RIS]|\\*[RIS]|[RIS])\\b",
        " "
      )
    }

    streptomycin_synergy_match <- stringr::str_match(
      ast_block_general,
      "\\bStreptomycin High Level\\s+(SYN-[A-Z]+)\\s+(\\*\\*[RIS]|\\*[RIS]|[RIS])\\b"
    )
    if (nrow(streptomycin_synergy_match) > 0 && !all(is.na(streptomycin_synergy_match))) {
      parsed_rows[["Streptomycin High Level (synergy)"]] <- tibble::tibble(
        antibiotic = "Streptomycin High Level (synergy)",
        mic = streptomycin_synergy_match[, 2],
        interpretation_raw = dplyr::case_when(
          streptomycin_synergy_match[, 3] %in% c("R", "*R", "**R") ~ "R",
          streptomycin_synergy_match[, 3] %in% c("I", "*I", "**I") ~ "I",
          streptomycin_synergy_match[, 3] %in% c("S", "*S", "**S") ~ "S",
          TRUE ~ "(Empty)"
        ),
        interpretation = dplyr::case_when(
          stringr::str_detect(streptomycin_synergy_match[, 3], "^\\*\\*") ~ "User modified",
          stringr::str_detect(streptomycin_synergy_match[, 3], "^\\*") ~ "AES modified",
          streptomycin_synergy_match[, 3] %in% c("R", "I", "S") ~ "Standard",
          TRUE ~ "(Empty)"
        )
      )
      ast_block_general <- stringr::str_replace(
        ast_block_general,
        "\\bStreptomycin High Level\\s+(SYN-[A-Z]+)\\s+(\\*\\*[RIS]|\\*[RIS]|[RIS])\\b",
        " "
      )
    }
    
    inducible_match <- stringr::str_match(
      ast_block_general,
      "\\bInducible Clindamycin Resistance\\s+(POS|NEG)\\s*([+-])"
    )
    if (nrow(inducible_match) > 0 && !all(is.na(inducible_match))) {
      parsed_rows[["Inducible Clindamycin Resistance"]] <- tibble::tibble(
        antibiotic = "Inducible Clindamycin Resistance",
        mic = inducible_match[, 2],
        interpretation_raw = inducible_match[, 3],
        interpretation = "Standard"
      )
      ast_block_general <- stringr::str_replace(
        ast_block_general,
        "\\bInducible Clindamycin Resistance\\s+(POS|NEG)\\s*([+-])",
        " "
      )
    }
    }
    
    ast_block_general <- clean_value(ast_block_general)
    
    # --------------------------------------------------
    # standard MIC-based parsing
    # --------------------------------------------------
    mic_based_antibiotics <- setdiff(
      antibiotic_dict,
      c("ESBL", "Cefoxitin Screen", "Inducible Clindamycin Resistance")
    )
    
    abx_regex <- mic_based_antibiotics |>
      vapply(escape_regex, FUN.VALUE = character(1))
    
    abx_regex <- abx_regex[order(nchar(abx_regex), decreasing = TRUE)]
    abx_pattern <- paste(abx_regex, collapse = "|")
    
    locs <- stringr::str_locate_all(
      ast_block_general,
      paste0("(?<!\\S)(", abx_pattern, ")(?!\\S)")
    )[[1]]
    
    abx_matches <- stringr::str_match_all(
      ast_block_general,
      paste0("(?<!\\S)(", abx_pattern, ")(?!\\S)")
    )[[1]]
    
    if (nrow(locs) > 0) {
      abx_names <- clean_value(abx_matches[, 2])
      
      parse_result_chunk <- function(chunk_text) {
        chunk_text <- clean_value(chunk_text)
        chunk_text <- normalize_report_text(chunk_text)
        
        mic_pattern <- "(SYN-[A-Z]+|<=\\s*\\d+(?:\\.\\d+)?\\*?|>=\\s*\\d+(?:\\.\\d+)?\\*?|<\\s*\\d+(?:\\.\\d+)?\\*?|>\\s*\\d+(?:\\.\\d+)?\\*?|=\\s*\\d+(?:\\.\\d+)?\\*?|\\d+(?:\\.\\d+)?\\*?)"
        
        mic_val <- extract_first_match(chunk_text, paste0("^", mic_pattern))
        if (is.na(mic_val)) {
          mic_val <- extract_first_match(chunk_text, mic_pattern)
        }
        
        remainder <- chunk_text
        if (!is.na(mic_val) && mic_val != "") {
          remainder <- stringr::str_replace(remainder, paste0("^", escape_regex(mic_val), "\\s*"), "")
          remainder <- stringr::str_replace(remainder, escape_regex(mic_val), "")
          remainder <- clean_value(remainder)
        }
        
        interp_token <- NA_character_
        
        # Only accept interpretation token if it is the immediate leading token
        # after MIC has been removed
        if (!is.na(mic_val) && mic_val != "") {
          if (stringr::str_detect(remainder, "^\\*\\*[RIS]\\b|^\\*[RIS]\\b|^[RIS]\\b")) {
            interp_token <- extract_first_match(remainder, "^(\\*\\*[RIS]|\\*[RIS]|[RIS])\\b")
          } else if (stringr::str_detect(remainder, "^(POS|NEG)(\\s*[+-])?\\b")) {
            interp_token <- extract_first_match(remainder, "^((?:POS|NEG)(?:\\s*[+-])?)\\b")
          } else {
            interp_token <- NA_character_
          }
        } else {
          interp_token <- NA_character_
        }
        
        interpretation_raw <- dplyr::case_when(
          is.na(interp_token) | interp_token == "" ~ "(Empty)",
          interp_token %in% c("R", "*R", "**R") ~ "R",
          interp_token %in% c("I", "*I", "**I") ~ "I",
          interp_token %in% c("S", "*S", "**S") ~ "S",
          stringr::str_detect(interp_token, "^POS") ~ "POS",
          stringr::str_detect(interp_token, "^NEG") ~ "NEG",
          TRUE ~ "(Empty)"
        )
        
        interpretation <- dplyr::case_when(
          is.na(interp_token) | interp_token == "" ~ "(Empty)",
          stringr::str_detect(interp_token, "^\\*\\*") ~ "User modified",
          stringr::str_detect(interp_token, "^\\*") ~ "AES modified",
          interp_token %in% c("R", "I", "S", "POS", "NEG", "POS +", "NEG -") ~ "Standard",
          TRUE ~ "(Empty)"
        )
        
        mic_val <- ifelse(is.na(mic_val) | mic_val == "", "(Empty)", clean_value(mic_val))
        
        tibble::tibble(
          mic = mic_val,
          interpretation_raw = interpretation_raw,
          interpretation = interpretation
        )
      }
      
      for (i in seq_len(nrow(locs))) {
        abx <- abx_names[i]
        start_pos <- locs[i, 1]
        end_pos <- if (i < nrow(locs)) locs[i + 1, 1] - 1 else nchar(ast_block_general)
        
        chunk <- substr(ast_block_general, start_pos, end_pos)
        chunk <- stringr::str_replace(chunk, paste0("^", escape_regex(abx), "\\s*"), "")
        chunk <- clean_value(chunk)
        chunk <- normalize_report_text(chunk)
        
        parsed_chunk <- parse_result_chunk(chunk)
        
        parsed_rows[[abx]] <- tibble::tibble(
          antibiotic = abx,
          mic = parsed_chunk$mic,
          interpretation_raw = parsed_chunk$interpretation_raw,
          interpretation = parsed_chunk$interpretation
        )
      }
    }
    
    ast_long <- dplyr::bind_rows(parsed_rows)
    
    ast_long <- tibble::tibble(antibiotic = antibiotic_dict) |>
      dplyr::left_join(ast_long, by = "antibiotic") |>
      dplyr::group_by(antibiotic) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        mic = dplyr::if_else(is.na(mic) | mic == "", "(Empty)", mic),
        interpretation_raw = dplyr::if_else(is.na(interpretation_raw) | interpretation_raw == "", "(Empty)", interpretation_raw),
        interpretation = dplyr::if_else(is.na(interpretation) | interpretation == "", "(Empty)", interpretation)
      )
  } else {
    ast_block <- NA_character_
  }
  
  # --------------------------------------------------
  # metadata dataframe
  # --------------------------------------------------
  meta_df <- tibble::tibble(
    source_file = empty_if_na(source_file),
    biomerieux_customer = empty_if_na(biomerieux_customer),
    report_title = empty_if_na(report_title),
    system_number = empty_if_na(system_number),
    printed_by = empty_if_na(printed_by),
    last_updated = empty_if_na(last_updated),
    updated_by = empty_if_na(updated_by),
    report_version = empty_if_na(report_version),
    patient_name = empty_if_na(patient_name),
    patient_id = empty_if_na(patient_id),
    lab_id = empty_if_na(lab_id),
    isolate = empty_if_na(isolate),
    isolate_status = empty_if_na(isolate_status),
    bench = empty_if_na(bench),
    id_card_type = empty_if_na(id_card_type),
    id_card_barcode = empty_if_na(id_card_barcode),
    id_testing_instrument = empty_if_na(id_testing_instrument),
    ast_card_type = empty_if_na(ast_card_type),
    ast_card_barcode = empty_if_na(ast_card_barcode),
    ast_testing_instrument = empty_if_na(ast_testing_instrument),
    setup_technologist = empty_if_na(setup_technologist),
    bionumber = empty_if_na(bionumber),
    selected_organism = empty_if_na(selected_organism),
    installed_vitek_version = empty_if_na(installed_vitek_version),
    mic_interpretation_guideline = empty_if_na(mic_interpretation_guideline),
    therapeutic_interpretation_guideline = empty_if_na(therapeutic_interpretation_guideline),
    aes_parameter_set_name = empty_if_na(aes_parameter_set_name),
    aes_parameter_last_modified = empty_if_na(aes_parameter_last_modified),
    comments = empty_if_na(comments),
    mcfarland = empty_if_na(mcfarland),
    id_card = empty_if_na(id_card),
    id_lot_number = empty_if_na(id_lot_number),
    id_expires = empty_if_na(id_expires),
    id_status = empty_if_na(id_status),
    id_analysis_time = empty_if_na(id_analysis_time),
    id_completed = empty_if_na(id_completed),
    organism_origin = empty_if_na(organism_origin),
    identified_organism = empty_if_na(identified_organism),
    organism_probability_pct = suppressWarnings(as.numeric(organism_probability_pct)),
    id_confidence = empty_if_na(id_confidence),
    analysis_organisms_tests_to_separate = empty_if_na(analysis_organisms_tests_to_separate),
    contraindicating_typical_biopatterns = empty_if_na(contraindicating_typical_biopatterns),
    susceptibility_card = empty_if_na(susceptibility_card),
    susceptibility_lot_number = empty_if_na(susceptibility_lot_number),
    susceptibility_expires = empty_if_na(susceptibility_expires),
    susceptibility_status = empty_if_na(susceptibility_status),
    susceptibility_analysis_time = empty_if_na(susceptibility_analysis_time),
    susceptibility_completed = empty_if_na(susceptibility_completed),
    aes_last_modified = empty_if_na(aes_last_modified),
    aes_parameter_set = empty_if_na(aes_parameter_set),
    aes_confidence_level = empty_if_na(aes_confidence_level),
    id_analysis_messages = empty_if_na(id_analysis_messages)
  )
  
  ast_long <- ast_long |>
    dplyr::mutate(
      source_file = meta_df$source_file[1],
      biomerieux_customer = meta_df$biomerieux_customer[1],
      report_title = meta_df$report_title[1],
      system_number = meta_df$system_number[1],
      printed_by = meta_df$printed_by[1],
      last_updated = meta_df$last_updated[1],
      updated_by = meta_df$updated_by[1],
      report_version = meta_df$report_version[1],
      patient_name = meta_df$patient_name[1],
      patient_id = meta_df$patient_id[1],
      lab_id = meta_df$lab_id[1],
      isolate = meta_df$isolate[1],
      isolate_status = meta_df$isolate_status[1],
      bench = meta_df$bench[1],
      id_card_type = meta_df$id_card_type[1],
      id_card_barcode = meta_df$id_card_barcode[1],
      id_testing_instrument = meta_df$id_testing_instrument[1],
      ast_card_type = meta_df$ast_card_type[1],
      ast_card_barcode = meta_df$ast_card_barcode[1],
      ast_testing_instrument = meta_df$ast_testing_instrument[1],
      setup_technologist = meta_df$setup_technologist[1],
      bionumber = meta_df$bionumber[1],
      selected_organism = meta_df$selected_organism[1],
      installed_vitek_version = meta_df$installed_vitek_version[1],
      mic_interpretation_guideline = meta_df$mic_interpretation_guideline[1],
      therapeutic_interpretation_guideline = meta_df$therapeutic_interpretation_guideline[1],
      aes_parameter_set_name = meta_df$aes_parameter_set_name[1],
      aes_parameter_last_modified = meta_df$aes_parameter_last_modified[1],
      comments = meta_df$comments[1],
      mcfarland = meta_df$mcfarland[1],
      id_card = meta_df$id_card[1],
      id_lot_number = meta_df$id_lot_number[1],
      id_expires = meta_df$id_expires[1],
      id_status = meta_df$id_status[1],
      id_analysis_time = meta_df$id_analysis_time[1],
      id_completed = meta_df$id_completed[1],
      organism_origin = meta_df$organism_origin[1],
      identified_organism = meta_df$identified_organism[1],
      organism_probability_pct = meta_df$organism_probability_pct[1],
      id_confidence = meta_df$id_confidence[1],
      analysis_organisms_tests_to_separate = meta_df$analysis_organisms_tests_to_separate[1],
      contraindicating_typical_biopatterns = meta_df$contraindicating_typical_biopatterns[1],
      susceptibility_card = meta_df$susceptibility_card[1],
      susceptibility_lot_number = meta_df$susceptibility_lot_number[1],
      susceptibility_expires = meta_df$susceptibility_expires[1],
      susceptibility_status = meta_df$susceptibility_status[1],
      susceptibility_analysis_time = meta_df$susceptibility_analysis_time[1],
      susceptibility_completed = meta_df$susceptibility_completed[1],
      aes_last_modified = meta_df$aes_last_modified[1],
      aes_parameter_set = meta_df$aes_parameter_set[1],
      aes_confidence_level = meta_df$aes_confidence_level[1],
      id_analysis_messages = meta_df$id_analysis_messages[1]
    )
  
  id_cols <- c(
    "source_file",
    "biomerieux_customer",
    "report_title",
    "system_number",
    "printed_by",
    "last_updated",
    "updated_by",
    "report_version",
    "patient_name",
    "patient_id",
    "lab_id",
    "isolate",
    "isolate_status",
    "bench",
    "id_card_type",
    "id_card_barcode",
    "id_testing_instrument",
    "ast_card_type",
    "ast_card_barcode",
    "ast_testing_instrument",
    "setup_technologist",
    "bionumber",
    "selected_organism",
    "installed_vitek_version",
    "mic_interpretation_guideline",
    "therapeutic_interpretation_guideline",
    "aes_parameter_set_name",
    "aes_parameter_last_modified",
    "mcfarland",
    "id_card",
    "id_lot_number",
    "id_expires",
    "id_status",
    "id_analysis_time",
    "id_completed",
    "organism_origin",
    "identified_organism",
    "organism_probability_pct",
    "id_confidence",
    "analysis_organisms_tests_to_separate",
    "contraindicating_typical_biopatterns",
    "susceptibility_card",
    "susceptibility_lot_number",
    "susceptibility_expires",
    "susceptibility_status",
    "susceptibility_analysis_time",
    "susceptibility_completed",
    "aes_last_modified",
    "aes_parameter_set",
    "aes_confidence_level"
  )
  
  if (nrow(ast_long) > 0) {
    ast_wide <- ast_long |>
      dplyr::select(
        dplyr::all_of(id_cols),
        antibiotic,
        mic,
        interpretation_raw,
        interpretation,
        id_analysis_messages,
        comments
      ) |>
      tidyr::pivot_wider(
        names_from = antibiotic,
        values_from = c(mic, interpretation_raw, interpretation),
        names_glue = "{antibiotic}_{.value}"
      ) |>
      janitor::clean_names()

    antibiotic_clean <- janitor::make_clean_names(unique(antibiotic_dict))

    ordered_ast_cols <- unlist(lapply(antibiotic_clean, function(abx) {
      c(
        paste0(abx, "_mic"),
        paste0(abx, "_interpretation_raw"),
        paste0(abx, "_interpretation")
      )
    }))

    ordered_ast_cols <- ordered_ast_cols[ordered_ast_cols %in% names(ast_wide)]

    final_cols <- c(id_cols, ordered_ast_cols, "id_analysis_messages", "comments")
    final_cols <- janitor::make_clean_names(final_cols)
    final_cols <- final_cols[final_cols %in% names(ast_wide)]

    ast_wide <- ast_wide |>
      dplyr::select(dplyr::all_of(final_cols))
  } else {
    ast_wide <- meta_df |>
      dplyr::select(dplyr::all_of(id_cols), id_analysis_messages, comments)
  }
  
  if (!quiet) {
    if (has_ast_block) {
      message("Full AST report parsed successfully.")
      message(
        if (is.na(ast_card_type_detected) || !nzchar(ast_card_type_detected)) {
          "Inferred AST card type: "
        } else {
          "Detected AST card type: "
        },
        ast_card_type
      )
      message("Panel antimicrobials expected: ", length(unique(antibiotic_dict)))
      message("AST entries returned: ", nrow(ast_long))
      message("Wide dataset rows: ", nrow(ast_wide))
    } else {
      message("Metadata-only chart parsed successfully. No susceptibility result block was found.")
      message("Wide dataset rows: ", nrow(ast_wide))
    }
  }
  
  list(
    metadata = meta_df,
    ast_long = ast_long,
    ast_wide = ast_wide,
    raw_ast_block = ast_block,
    antibiotic_panel = unique(antibiotic_dict)
  )
}
