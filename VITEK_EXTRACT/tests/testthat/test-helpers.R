testthat::test_that("audit log initializes and appends", {
  tmp <- tempfile(fileext = ".csv")
  initialize_audit_log(tmp)
  append_audit_log(tmp, "stage_a", "success", "details")
  logged <- read_audit_log(tmp)

  testthat::expect_true(file.exists(tmp))
  testthat::expect_equal(nrow(logged), 1)
  testthat::expect_equal(logged$stage[[1]], "stage_a")
})

testthat::test_that("check_redcap_keyring_status reports missing setup cleanly", {
  tmp_config <- tempfile(fileext = ".yml")

  status <- check_redcap_keyring_status(
    config_file = tmp_config,
    key_get_fn = function(service, username) "token"
  )

  testthat::expect_false(status$configured)
  testthat::expect_false(status$config_exists)
  testthat::expect_match(status$message, "setup is required", ignore.case = TRUE)
})

testthat::test_that("save_redcap_keyring_setup writes config and validates local credential", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_config <- file.path(tmp_dir, "redcap_config.yml")
  saved_token <- NULL

  result <- save_redcap_keyring_setup(
    redcap_uri = "https://redcap.example.org/api/",
    keyring_service = "redcap_api",
    keyring_username = "analyst",
    api_token = "super-secret-token",
    config_file = tmp_config,
    key_set_with_value_fn = function(service, username, password) {
      saved_token <<- list(
        service = service,
        username = username,
        password = password
      )
      invisible(TRUE)
    }
  )

  status <- check_redcap_keyring_status(
    config_file = tmp_config,
    key_get_fn = function(service, username) {
      if (
        identical(service, saved_token$service) &&
          identical(username, saved_token$username)
      ) {
        saved_token$password
      } else {
        ""
      }
    }
  )

  cfg <- yaml::read_yaml(tmp_config)

  testthat::expect_true(file.exists(tmp_config))
  testthat::expect_equal(cfg$redcap_uri, "https://redcap.example.org/api/")
  testthat::expect_equal(cfg$keyring_service, "redcap_api")
  testthat::expect_equal(cfg$keyring_username, "analyst")
  testthat::expect_equal(saved_token$password, "super-secret-token")
  testthat::expect_true(status$configured)
  testthat::expect_true(status$credential_exists)
  testthat::expect_match(status$message, "configured locally", ignore.case = TRUE)
  testthat::expect_equal(result$config$keyring_username, "analyst")
})

testthat::test_that("clear_redcap_keyring_setup removes saved config and local credential", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_config <- file.path(tmp_dir, "redcap_config.yml")
  deleted <- NULL

  yaml::write_yaml(
    list(
      redcap_uri = "https://redcap.example.org/api/",
      keyring_service = "redcap_api",
      keyring_username = "analyst"
    ),
    tmp_config
  )

  status <- clear_redcap_keyring_setup(
    config_file = tmp_config,
    key_delete_fn = function(service, username) {
      deleted <<- c(service, username)
      invisible(TRUE)
    }
  )

  testthat::expect_false(file.exists(tmp_config))
  testthat::expect_equal(deleted, c("redcap_api", "analyst"))
  testthat::expect_false(status$config_exists)
  testthat::expect_false(status$configured)
  testthat::expect_match(status$message, "setup is required", ignore.case = TRUE)
})

testthat::test_that("parse_github_repo_url handles tree subdirectories", {
  parsed <- parse_github_repo_url("https://github.com/Olawemi5/DS/tree/main/data_raw")

  testthat::expect_equal(parsed$owner, "Olawemi5")
  testthat::expect_equal(parsed$repo, "DS")
  testthat::expect_equal(parsed$branch, "main")
  testthat::expect_equal(parsed$subdir, file.path("data_raw"))
})

testthat::test_that("parse_github_repo_url keeps nested tree subdirectories as one path", {
  parsed <- parse_github_repo_url("https://github.com/Olawemi5/DS/tree/main/Coding/Python/Calculator")

  testthat::expect_equal(parsed$subdir, "Coding/Python/Calculator")
})

testthat::test_that("parse_github_repo_url accepts common pasted github formats", {
  parsed_plain <- parse_github_repo_url("github.com/Olawemi5/DS/tree/main/Coding")
  parsed_www <- parse_github_repo_url("www.github.com/Olawemi5/DS?tab=readme")
  parsed_quoted <- parse_github_repo_url("\"https://github.com/Olawemi5/DS/tree/main/Coding/\"")

  testthat::expect_equal(parsed_plain$owner, "Olawemi5")
  testthat::expect_equal(parsed_plain$repo, "DS")
  testthat::expect_equal(parsed_plain$subdir, "Coding")

  testthat::expect_equal(parsed_www$owner, "Olawemi5")
  testthat::expect_equal(parsed_www$repo, "DS")
  testthat::expect_equal(parsed_www$branch, "main")

  testthat::expect_equal(parsed_quoted$owner, "Olawemi5")
  testthat::expect_equal(parsed_quoted$repo, "DS")
  testthat::expect_equal(parsed_quoted$subdir, "Coding")
})

testthat::test_that("github_candidate_dirs keeps explicit subdir scoped to that subtree", {
  dirs <- github_candidate_dirs(file.path("nested", "pdfs"))

  testthat::expect_equal(dirs, file.path("nested", "pdfs"))
})

testthat::test_that("github_candidate_dirs falls back to data_raw then root when no subdir is given", {
  dirs <- github_candidate_dirs("")

  testthat::expect_equal(dirs, c("data_raw", ""))
})

testthat::test_that("normalize_repo_path standardizes github subdirectory paths", {
  testthat::expect_equal(
    normalize_repo_path("\\Coding\\Python\\Calculator\\"),
    "Coding/Python/Calculator"
  )
})

testthat::test_that("extract_pdf_files_from_zip pulls only pdf files from nested zip paths", {
  tmp_dir <- withr::local_tempdir()
  zip_dir <- file.path(tmp_dir, "zip_src")
  dir.create(file.path(zip_dir, "nested"), recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(zip_dir, "nested", "calc_manual.pdf")
  txt_path <- file.path(zip_dir, "nested", "notes.txt")

  writeLines("fake pdf content", pdf_path)
  writeLines("not a pdf", txt_path)

  zip_file <- file.path(tmp_dir, "calculator_bundle.zip")
  compress_status <- system2(
    "powershell",
    c(
      "-NoProfile",
      "-Command",
      sprintf(
        "Compress-Archive -LiteralPath '%s','%s' -DestinationPath '%s' -Force",
        pdf_path,
        txt_path,
        zip_file
      )
    )
  )

  testthat::expect_equal(compress_status, 0)
  testthat::expect_true(file.exists(zip_file))

  dest_dir <- file.path(tmp_dir, "dest")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  extracted <- extract_pdf_files_from_zip(zip_file, dest_dir)

  testthat::expect_length(extracted, 1)
  testthat::expect_true(file.exists(extracted))
  testthat::expect_match(basename(extracted), "calc_manual[.]pdf$")
})

testthat::test_that("clean_data keeps duplicate same-organism reports as separate rows", {
  input_df <- data.frame(
    patient_id = c("P001", "P001"),
    identified_organism = c("Staph aureus", "Staph aureus"),
    isolate = c("1", "2"),
    cefoxitin_screen_mic = c("POS", "NEG"),
    cefoxitin_screen_interpretation_raw = c("+", "-"),
    cefoxitin_screen_interpretation = c("Standard", "Standard"),
    isolate_2 = c("", ""),
    identified_organism_2 = c("", ""),
    cefoxitin_screen_mic_2 = c("", ""),
    cefoxitin_screen_interpretation_raw_2 = c("", ""),
    cefoxitin_screen_interpretation_2 = c("", ""),
    comments = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$identified_organism[[1]], "Staph aureus")
  testthat::expect_equal(result$identified_organism[[2]], "Staph aureus")
  testthat::expect_equal(result$identified_organism_2[[1]], "")
  testthat::expect_equal(result$identified_organism_2[[2]], "")
})

testthat::test_that("clean_data fills _2 for a second distinct organism", {
  input_df <- data.frame(
    patient_id = c("P001", "P001"),
    identified_organism = c("Staph aureus", "E. coli"),
    isolate = c("1", "2"),
    cefoxitin_screen_mic = c("POS", "NEG"),
    cefoxitin_screen_interpretation_raw = c("+", "-"),
    cefoxitin_screen_interpretation = c("Standard", "Standard"),
    isolate_2 = c("", ""),
    identified_organism_2 = c("", ""),
    cefoxitin_screen_mic_2 = c("", ""),
    cefoxitin_screen_interpretation_raw_2 = c("", ""),
    cefoxitin_screen_interpretation_2 = c("", ""),
    comments = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$identified_organism[[1]], "Staph aureus")
  testthat::expect_equal(result$identified_organism_2[[1]], "E. coli")
  testthat::expect_equal(result$cefoxitin_screen_mic_2[[1]], "NEG")
})

testthat::test_that("parse_ast_chart_report_full infers AST-GP75 when card type header is missing", {
  stripped_chart_text <- paste(
    "IFAIN ABUJA",
    "bioMérieux Customer: Microbiology Chart Report Printed March 14, 2025 8:45:19 AM WAT",
    "Patient Name: camra, prosp Patient ID: 17-0879-01",
    "Lab ID: 17087901 Isolate Number: 1",
    "Organism Quantity:",
    "Selected Organism : Enterococcus faecalis",
    "Comments:",
    "Identification Information Analysis Time: 5.83 hours Status: Final",
    "97% Probability Enterococcus faecalis",
    "Selected Organism",
    "Bionumber: 116002765773671",
    "ID Analysis Messages",
    "Susceptibility Information Analysis Time: 11.20 hours Status: Final",
    "Antimicrobial MIC Interpretation Antimicrobial MIC Interpretation",
    "Cefoxitin Screen Clindamycin",
    "Ampicillin <= 2 S Linezolid 2 S",
    "Oxacillin Daptomycin 4 I",
    "Gentamicin High Level SYN-S S Vancomycin 1 S",
    "(synergy)",
    "Streptomycin High Level SYN-S S Doxycycline >= 16 R",
    "(synergy)",
    "Gentamicin Tetracycline >= 16 R",
    "Ciprofloxacin <= 0.5 S Tigecycline <= 0.12 S",
    "Levofloxacin 1 S Nitrofurantoin <= 16 S",
    "Moxifloxacin Rifampicin",
    "Inducible Clindamycin Trimethoprim/",
    "Resistance Sulfamethoxazole",
    "Erythromycin >= 8 R",
    "AES Findings",
    "Confidence: Consistent",
    sep = "\n"
  )

  pdf_object <- list(
    metadata = tibble::tibble(
      source_file = "stripped_chart.pdf",
      biomerieux_customer = "IFAIN ABUJA"
    ),
    full_text = stripped_chart_text,
    page_text = c(stripped_chart_text)
  )

  parsed <- parse_ast_chart_report_full(pdf_object, quiet = TRUE)

  testthat::expect_equal(parsed$metadata$ast_card_type[[1]], "AST-GP75")
  testthat::expect_equal(parsed$ast_wide$ast_card_type[[1]], "AST-GP75")
  testthat::expect_equal(parsed$metadata$selected_organism[[1]], "Enterococcus faecalis")
  testthat::expect_equal(parsed$metadata$identified_organism[[1]], "Enterococcus faecalis")
  testthat::expect_equal(parsed$metadata$organism_probability_pct[[1]], 97)
  testthat::expect_equal(parsed$metadata$bionumber[[1]], "116002765773671")
  testthat::expect_equal(parsed$metadata$id_analysis_time[[1]], "5.83 hours")
  testthat::expect_equal(parsed$metadata$id_status[[1]], "Final")
  testthat::expect_equal(parsed$metadata$susceptibility_analysis_time[[1]], "11.20 hours")
  testthat::expect_equal(parsed$metadata$susceptibility_status[[1]], "Final")
  testthat::expect_equal(parsed$ast_wide$gentamicin_high_level_synergy_mic[[1]], "SYN-S")
  testthat::expect_equal(parsed$ast_wide$gentamicin_high_level_synergy_interpretation_raw[[1]], "S")
  testthat::expect_equal(parsed$ast_wide$streptomycin_high_level_synergy_mic[[1]], "SYN-S")
  testthat::expect_equal(parsed$ast_wide$streptomycin_high_level_synergy_interpretation_raw[[1]], "S")
  testthat::expect_equal(parsed$ast_wide$linezolid_mic[[1]], "2")
  testthat::expect_equal(parsed$ast_wide$linezolid_interpretation_raw[[1]], "S")
  testthat::expect_equal(parsed$ast_wide$erythromycin_interpretation_raw[[1]], "R")
})

testthat::test_that("parse_ast_chart_report_full returns metadata-only output when AST block is absent", {
  id_only_chart_text <- paste(
    "IFAIN ABUJA",
    "bioMérieux Customer: Microbiology Chart Report Printed September 26, 2025 9:54:32 AM WAT",
    "Patient Name: CAMRA, PROS Patient ID: 141454-00",
    "Location: V4400 Physician:",
    "Lab ID: 14145400 Isolate Number: 1",
    "Organism Quantity:",
    "Selected Organism : Trichosporon inkin",
    "Source: Collected:",
    "Comments:",
    "Identification Information Analysis Time: 17.80 hours Status: Final",
    "90% Probability Trichosporon inkin",
    "Selected Organism",
    "Bionumber: 6104756205727571",
    "ID Analysis Messages",
    sep = "\n"
  )

  pdf_object <- list(
    metadata = tibble::tibble(
      source_file = "id_only_chart.pdf",
      biomerieux_customer = "IFAIN ABUJA"
    ),
    full_text = id_only_chart_text,
    page_text = c(id_only_chart_text)
  )

  parsed <- parse_ast_chart_report_full(pdf_object, quiet = TRUE)

  testthat::expect_equal(nrow(parsed$ast_wide), 1)
  testthat::expect_equal(nrow(parsed$ast_long), 0)
  testthat::expect_equal(parsed$ast_wide$patient_id[[1]], "141454-00")
  testthat::expect_equal(parsed$ast_wide$isolate[[1]], "1")
  testthat::expect_equal(parsed$ast_wide$selected_organism[[1]], "Trichosporon inkin")
  testthat::expect_equal(parsed$ast_wide$identified_organism[[1]], "Trichosporon inkin")
  testthat::expect_equal(parsed$ast_wide$organism_probability_pct[[1]], 90)
  testthat::expect_equal(parsed$ast_wide$bionumber[[1]], "6104756205727571")
  testthat::expect_equal(parsed$ast_wide$id_analysis_time[[1]], "17.80 hours")
  testthat::expect_equal(parsed$ast_wide$id_status[[1]], "Final")
  testthat::expect_equal(parsed$ast_wide$ast_card_type[[1]], "(Empty)")
})

testthat::test_that("parse_ast_chart_report_full extracts lab_id from chart reports", {
  chart_text <- paste(
    "IFAIN ABUJA",
    "bioMérieux Customer: Microbiology Chart Report Printed June 11, 2024 9:08:10 AM WAT",
    "Patient Name: CAMRA, PROSPECTIVE Patient ID: 14-0289-00R",
    "Location: VP0118 Physician:",
    "Lab ID: 14028900R Isolate Number: 1",
    "Last Updated: Jun 10, 2024 10:57 WAT",
    "Organism Quantity:",
    "Selected Organism : Pseudomonas aeruginosa",
    "Source: Collected:",
    "Comments:",
    "Identification Information Analysis Time: 5.82 hours Status: Final",
    "95% Probability Pseudomonas aeruginosa",
    "Selected Organism",
    "Bionumber: 0003553103500272",
    "ID Analysis Messages",
    "Susceptibility Information Analysis Time: 12.67 hours Status: Final",
    "Antimicrobial MIC Interpretation Antimicrobial MIC Interpretation",
    "ESBL Ertapenem",
    "Ampicillin Meropenem 8 I",
    "Ampicillin/Sulbactam Amikacin <= 2 S",
    "Piperacillin >= 128 R Gentamicin 4 S",
    "Cefazolin >= 64 R Tobramycin <= 1 S",
    "Cefoxitin Ciprofloxacin 0.5 S",
    "Ceftazidime >= 64 R Levofloxacin 2 S",
    "Ceftriaxone Nitrofurantoin",
    "Cefepime 16 I Trimethoprim/ Sulfamethoxazole",
    "AES Findings",
    "Confidence: Consistent",
    sep = "\n"
  )

  pdf_object <- list(
    metadata = tibble::tibble(
      source_file = "lab_id_chart.pdf",
      biomerieux_customer = "IFAIN ABUJA"
    ),
    full_text = chart_text,
    page_text = c(chart_text)
  )

  parsed <- parse_ast_chart_report_full(pdf_object, quiet = TRUE)

  testthat::expect_equal(parsed$ast_wide$lab_id[[1]], "14028900R")
  testthat::expect_equal(parsed$metadata$lab_id[[1]], "14028900R")
})

testthat::test_that("clean_data fills _3 for a third distinct organism", {
  input_df <- data.frame(
    patient_id = c("P001", "P001", "P001"),
    identified_organism = c("Staph aureus", "E. coli", "Klebsiella"),
    isolate = c("1", "2", "3"),
    isolate_2 = c("", "", ""),
    isolate_3 = c("", "", ""),
    identified_organism_2 = c("", "", ""),
    identified_organism_3 = c("", "", ""),
    comments = c("", "", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$identified_organism[[1]], "Staph aureus")
  testthat::expect_equal(result$identified_organism_2[[1]], "E. coli")
  testthat::expect_equal(result$identified_organism_3[[1]], "Klebsiella")
})

testthat::test_that("clean_data groups same normalized patient-family IDs into isolate slots", {
  input_df <- data.frame(
    patient_id = c("1100234", "110023"),
    identified_organism = c("E. coli", "Salmonella Typhi"),
    isolate = c("iso_a", "iso_b"),
    isolate_2 = c("", ""),
    identified_organism_2 = c("", ""),
    comments = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$identified_organism[[1]], "E. coli")
  testthat::expect_equal(result$identified_organism_2[[1]], "Salmonella Typhi")
})

testthat::test_that("clean_data keeps same-organism duplicates across normalized patient-family IDs as separate rows", {
  input_df <- data.frame(
    patient_id = c("1100234", "110023"),
    identified_organism = c("E. coli", "E. coli"),
    isolate = c("iso_a", "iso_b"),
    isolate_2 = c("", ""),
    identified_organism_2 = c("", ""),
    comments = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$identified_organism[[1]], "E. coli")
  testthat::expect_equal(result$identified_organism[[2]], "E. coli")
  testthat::expect_equal(result$identified_organism_2[[1]], "")
})

testthat::test_that("clean_data collapses distinct organisms but retains extra same-organism duplicates separately", {
  input_df <- data.frame(
    patient_id = c("1100234", "110023", "110023"),
    identified_organism = c("E. coli", "E. coli", "Salmonella Typhi"),
    isolate = c("iso_a", "iso_b", "iso_c"),
    isolate_2 = c("", "", ""),
    identified_organism_2 = c("", "", ""),
    comments = c("", "", ""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$identified_organism[[1]], "E. coli")
  testthat::expect_equal(result$identified_organism_2[[1]], "Salmonella Typhi")
  testthat::expect_equal(result$identified_organism[[2]], "E. coli")
})

testthat::test_that("clean_data derives numbered participant_id from report_version", {
  input_df <- data.frame(
    patient_id = c("P001"),
    report_version = c("2 of 3"),
    identified_organism = c("Staph aureus"),
    isolate = c("1"),
    comments = c(""),
    stringsAsFactors = FALSE
  )

  result <- clean_data(input_df, quiet = TRUE, collapse_patient_isolates = FALSE)

  testthat::expect_equal(result$patient_id[[1]], "P001_2")
})

testthat::test_that("load_append_redcap_mapping keeps unmapped rows by default", {
  tmp_mapping <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("patient_id", ""),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary key", "")
    ),
    tmp_mapping
  )

  result <- load_append_redcap_mapping(tmp_mapping, quiet = TRUE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$target_redcap_name[[2]], "")
})

testthat::test_that("suggest_redcap_target_fields finds exact field-name matches", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("patient_identifier", "identified_organism"),
    redcap_field_label = c("Patient Identifier", "Identified Organism")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("identified_organism"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  top_match <- result[result$match_rank == 1, ]

  testthat::expect_equal(top_match$redcap_field_name[[1]], "identified_organism")
  testthat::expect_equal(top_match$match_method[[1]], "exact_field_name")
})

testthat::test_that("suggest_redcap_target_fields handles empty field labels safely", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("patient_id", "organism_name"),
    redcap_field_label = c("", "Identified Organism")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("identified_organism"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$match_rank[[1]], 1)
})

testthat::test_that("suggest_redcap_target_fields excludes descriptive field types", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("identified_organism", "organism_name"),
    redcap_field_label = c("Identified Organism", "Organism Name"),
    field_type = c("descriptive", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("identified_organism"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_false(any(result$redcap_field_name == "identified_organism"))
  testthat::expect_equal(result$redcap_field_name[[1]], "organism_name")
})

testthat::test_that("suggest_redcap_target_fields prefers S/I/R targets for interpretation_raw fields", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_16o1_ai", "crf11_16o1_am"),
    redcap_field_label = c(
      "S/I/R Code: Organism 1 Ampicillin",
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 1 Ampicillin"
    ),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("ampicillin_interpretation_raw"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_16o1_ai")
})

testthat::test_that("suggest_redcap_target_fields prefers MIC targets for mic fields", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_16o1_ai", "crf11_16o1_am"),
    redcap_field_label = c(
      "S/I/R Code: Organism 1 Ampicillin",
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 1 Ampicillin"
    ),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("ampicillin_mic"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_16o1_am")
})

testthat::test_that("suggest_redcap_target_fields treats participant and record identifiers as patient_id synonyms", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("participant_id", "organism_name", "record_id"),
    redcap_field_label = c("Participant ID", "Organism Name", "Record ID"),
    field_type = c("text", "text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("patient_id"),
    dictionary_df = dictionary_df,
    top_n = 3,
    quiet = TRUE
  )

  testthat::expect_true(result$redcap_field_name[[1]] %in% c("participant_id", "record_id"))
  testthat::expect_true(any(result$redcap_field_name %in% c("participant_id", "record_id")))
})

testthat::test_that("suggest_redcap_target_fields recognizes participant number labels for patient_id", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("study_code", "participant_number"),
    redcap_field_label = c("Study Code", "Participant Number"),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("patient_id"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "participant_number")
})

testthat::test_that("suggest_redcap_target_fields prefers early dictionary rows for patient identifiers", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c(
      "participant_id",
      paste0("filler_", seq_len(10)),
      "record_id"
    ),
    redcap_field_label = c(
      "Participant ID",
      paste("Filler Field", seq_len(10)),
      "Record ID"
    ),
    field_type = rep("text", 12)
  )

  result <- suggest_redcap_target_fields(
    source_names = c("patient_id"),
    dictionary_df = dictionary_df,
    top_n = 3,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "participant_id")
})

testthat::test_that("suggest_redcap_target_fields marks strong patient_id synonym matches as high confidence", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("participant_id", "organism_name"),
    redcap_field_label = c("Participant ID", "Organism Name"),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("patient_id"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "participant_id")
  testthat::expect_equal(result$match_status[[1]], "high_confidence")
  testthat::expect_true(result$match_score[[1]] > 90)
})

testthat::test_that("suggest_redcap_target_fields prefers second-isolate targets for _2 fields", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_11b", "crf11_12b"),
    redcap_field_label = c("Organism 1 ID", "Organism 2 ID"),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("isolate_2"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_12b")
  testthat::expect_equal(result$match_method[[1]], "isolate_slot_semantics")
})

testthat::test_that("suggest_redcap_target_fields separates clindamycin from inducible clindamycin by label", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_15o1_ci", "crf11_15o1_ti"),
    redcap_field_label = c(
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 1 Clindamycin",
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 1 Inducible Clindamycin resistance"
    ),
    field_type = c("text", "text")
  )

  clindamycin_result <- suggest_redcap_target_fields(
    source_names = c("clindamycin_mic"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  inducible_result <- suggest_redcap_target_fields(
    source_names = c("inducible_clindamycin_resistance_mic"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(clindamycin_result$redcap_field_name[[1]], "crf11_15o1_ci")
  testthat::expect_equal(inducible_result$redcap_field_name[[1]], "crf11_15o1_ti")
})

testthat::test_that("suggest_redcap_target_fields separates clindamycin interpretation from inducible clindamycin interpretation by label", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_15o1_cii", "crf11_15o1_tii"),
    redcap_field_label = c(
      "S/I/R Code: Organism 1 Clindamycin",
      "S/I/R Code: Organism 1 Inducible Clindamycin resistance"
    ),
    field_type = c("text", "text")
  )

  clindamycin_result <- suggest_redcap_target_fields(
    source_names = c("clindamycin_interpretation_raw"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  inducible_result <- suggest_redcap_target_fields(
    source_names = c("inducible_clindamycin_resistance_interpretation_raw"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(clindamycin_result$redcap_field_name[[1]], "crf11_15o1_cii")
  testthat::expect_equal(inducible_result$redcap_field_name[[1]], "crf11_15o1_tii")
})

testthat::test_that("suggest_redcap_target_fields does not map plain clindamycin interpretation to inducible targets when both exist", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_16o1_jii", "crf11_15o1_tii"),
    redcap_field_label = c(
      "16jii S/I/R Code :Organism 1- Clindamycin",
      "15sii S/I/R Code: Organism 1- Inducible Clindamycin resistance"
    ),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("clindamycin_interpretation_raw"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_16o1_jii")
})

testthat::test_that("suggest_redcap_target_fields keeps inducible clindamycin slots aligned for _2 fields", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_15o2_ci", "crf11_15o2_ti", "crf11_15o2_cii", "crf11_15o2_tii"),
    redcap_field_label = c(
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 2 Clindamycin",
      "Minimum Inhibitory Concentration (MIC ug/ml): Organism 2 Inducible Clindamycin resistance",
      "S/I/R Code: Organism 2 Clindamycin",
      "S/I/R Code: Organism 2 Inducible Clindamycin resistance"
    ),
    field_type = c("text", "text", "text", "text")
  )

  mic_result <- suggest_redcap_target_fields(
    source_names = c("inducible_clindamycin_resistance_mic_2"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  interpretation_result <- suggest_redcap_target_fields(
    source_names = c("inducible_clindamycin_resistance_interpretation_raw_2"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(mic_result$redcap_field_name[[1]], "crf11_15o2_ti")
  testthat::expect_equal(interpretation_result$redcap_field_name[[1]], "crf11_15o2_tii")
})

testthat::test_that("suggest_redcap_target_fields recognizes d test labels for inducible clindamycin interpretation", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_15o3_cii", "crf11_15o3_tii"),
    redcap_field_label = c(
      "S/I/R Code: Organism 3 Clindamycin",
      "S/I/R Code: Organism 3 D-test / Inducible Clindamycin"
    ),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("inducible_clindamycin_resistance_interpretation_raw_3"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_15o3_tii")
})

testthat::test_that("suggest_redcap_target_fields prefers true comment fields over participant identifiers", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("participant_id", "crf11a_21_v2b"),
    redcap_field_label = c("Participants ID", "Comments:"),
    field_type = c("text", "notes")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("comments"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11a_21_v2b")
  testthat::expect_false(any(result$redcap_field_name[result$match_rank == 1] == "participant_id"))
})

testthat::test_that("suggest_redcap_target_fields keeps erythromycin interpretation aligned to erythromycin labels", {
  dictionary_df <- tibble::tibble(
    redcap_field_name = c("crf11_16o3_sii_v2", "crf11_15o3_aii"),
    redcap_field_label = c(
      "16sii S/I/R Code :Organism 3- Erythromycin",
      "15aii S/I/R Code: Organism 3- ESBL"
    ),
    field_type = c("text", "text")
  )

  result <- suggest_redcap_target_fields(
    source_names = c("erythromycin_interpretation_raw_3"),
    dictionary_df = dictionary_df,
    top_n = 2,
    quiet = TRUE
  )

  testthat::expect_equal(result$redcap_field_name[[1]], "crf11_16o3_sii_v2")
})

testthat::test_that("update_append_mapping_targets fills blank target_redcap_name values", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("patient_id", ""),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary key", "")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("patient_id", "organism_name"),
      `Field Label` = c("Patient ID", "Identified Organism"),
      `Field Type` = c("text", "text"),
      `Form Name` = c("microbiology", "microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "identified_organism", ]

  testthat::expect_true(file.exists(tmp_output))
  testthat::expect_equal(updated_row$target_redcap_name[[1]], "organism_name")
})

testthat::test_that("update_append_mapping_targets leaves low-confidence interpretation note fields unmapped", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("ampicillin_interpretation"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c("")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("crf11_16o1_ai"),
      `Field Label` = c("S/I/R Code: Organism 1 Ampicillin"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "ampicillin_interpretation", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_low_confidence")
})

testthat::test_that("update_append_mapping_targets leaves low-confidence suffixed interpretation note fields unmapped", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("ampicillin_interpretation_2"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c("")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("crf11_16o2_ai"),
      `Field Label` = c("S/I/R Code: Organism 2 Ampicillin"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "ampicillin_interpretation_2", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(updated_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_low_confidence")
})

testthat::test_that("update_append_mapping_targets keeps high-confidence interpretation note matches", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("ampicillin_interpretation"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c("")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("ampicillin_interpretation"),
      `Field Label` = c("Ampicillin Interpretation"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "ampicillin_interpretation", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "ampicillin_interpretation")
  testthat::expect_false(updated_row$match_status[[1]] == "left_unmapped_low_confidence")
})

testthat::test_that("update_append_mapping_targets leaves parser-only metadata unmapped by default", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("source_file"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c(""),
      source_present = c("FALSE")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("source_document"),
      `Field Label` = c("Source File"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "source_file", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(updated_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets still leaves parser metadata unmapped even when source_present is true", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("source_file"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c(""),
      source_present = c("TRUE")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("source_document"),
      `Field Label` = c("Source File"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "source_file", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets clears parser metadata mappings even if prefilled", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("source_file"),
      target_redcap_name = c("manual_source_document"),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c(""),
      source_present = c("FALSE")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("source_document"),
      `Field Label` = c("Source File"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    overwrite_existing = TRUE,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "source_file", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets leaves id_analysis_messages unmapped", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("id_analysis_messages"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c("")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("analysis_message_field"),
      `Field Label` = c("ID Analysis Messages"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "id_analysis_messages", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(updated_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets leaves selected low-confidence technical fields unmapped", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("updated_by", "id_card_type", "ast_card_type", "identified_by"),
      target_redcap_name = c("", "", "", ""),
      update_mode = c("update", "update", "update", "update"),
      key_field = c("FALSE", "FALSE", "FALSE", "FALSE"),
      notes = c("", "", "", "")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("participant_id", "crf11_direct_gram_stain"),
      `Field Label` = c("Participants ID", "Description of any organism by Direct Gram stain"),
      `Field Type` = c("text", "text"),
      `Form Name` = c("microbiology", "microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  testthat::expect_true(all(result$updated_mapping$target_redcap_name == ""))
  testthat::expect_true(all(is.na(result$updated_mapping$suggested_target_redcap_name)))
  testthat::expect_true(all(result$updated_mapping$match_status == "left_unmapped_low_confidence"))
})

testthat::test_that("update_append_mapping_targets leaves suffixed metadata fields unmapped", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("source_file_2"),
      target_redcap_name = c(""),
      update_mode = c("update"),
      key_field = c("FALSE"),
      notes = c("")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("source_document_2"),
      `Field Label` = c("Source File Organism 2"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  updated_row <- result$updated_mapping[result$updated_mapping$source_name == "source_file_2", ]

  testthat::expect_equal(updated_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(updated_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(updated_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets reserves the patient_id target for patient_id only", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "source_file"),
      target_redcap_name = c("", ""),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("", ""),
      source_present = c("TRUE", "TRUE")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("participant_id"),
      `Field Label` = c("Participant ID Source File"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  patient_row <- result$updated_mapping[result$updated_mapping$source_name == "patient_id", ]
  source_file_row <- result$updated_mapping[result$updated_mapping$source_name == "source_file", ]

  testthat::expect_equal(patient_row$target_redcap_name[[1]], "participant_id")
  testthat::expect_equal(source_file_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(source_file_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(source_file_row$match_status[[1]], "left_unmapped_parser_metadata")
})

testthat::test_that("update_append_mapping_targets clears a prefilled duplicate of the patient_id target", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("", "participant_id"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("", "")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("participant_id", "organism_name"),
      `Field Label` = c("Participant ID", "Identified Organism"),
      `Field Type` = c("text", "text"),
      `Form Name` = c("microbiology", "microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    quiet = TRUE
  )

  patient_row <- result$updated_mapping[result$updated_mapping$source_name == "patient_id", ]
  organism_row <- result$updated_mapping[result$updated_mapping$source_name == "identified_organism", ]

  testthat::expect_equal(patient_row$target_redcap_name[[1]], "participant_id")
  testthat::expect_equal(organism_row$target_redcap_name[[1]], "")
  testthat::expect_true(is.na(organism_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(organism_row$match_status[[1]], "reserved_for_patient_id")
})

testthat::test_that("update_append_mapping_targets does not let other variables suggest the patient_id target", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_dictionary <- tempfile(fileext = ".csv")
  tmp_output <- tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "comments"),
      target_redcap_name = c("", ""),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("", "")
    ),
    tmp_mapping
  )

  readr::write_csv(
    tibble::tibble(
      `Variable / Field Name` = c("participant_id"),
      `Field Label` = c("Participant ID Comments"),
      `Field Type` = c("text"),
      `Form Name` = c("microbiology")
    ),
    tmp_dictionary
  )

  result <- update_append_mapping_targets(
    append_mapping_file = tmp_mapping,
    dictionary_file = tmp_dictionary,
    output_file = tmp_output,
    overwrite_existing = FALSE,
    quiet = TRUE
  )

  patient_row <- result$updated_mapping[result$updated_mapping$source_name == "patient_id", ]
  comments_row <- result$updated_mapping[result$updated_mapping$source_name == "comments", ]

  testthat::expect_equal(patient_row$target_redcap_name[[1]], "participant_id")
  testthat::expect_true(is.na(comments_row$suggested_target_redcap_name[[1]]))
  testthat::expect_equal(comments_row$target_redcap_name[[1]], "")
  testthat::expect_false(identical(comments_row$target_redcap_name[[1]], "participant_id"))
})

testthat::test_that("prepare_append_trial is ready when mapped key field and update fields are available", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("participant_id", "organism_name"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- prepare_append_trial(
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    mapping_file = tmp_mapping,
    config_file = tmp_config,
    output_dir = tmp_output,
    quiet = TRUE
  )

  testthat::expect_true(result$trial_ready)
  testthat::expect_equal(result$status, "Append trial setup ready")
  testthat::expect_equal(nrow(result$blocking_issues), 0)
  testthat::expect_true(file.exists(result$summary_file))
  testthat::expect_true(file.exists(result$mapped_fields_file))
})

testthat::test_that("prepare_append_trial blocks when no mapped key field is available", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("", "organism_name"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- prepare_append_trial(
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    mapping_file = tmp_mapping,
    config_file = tmp_config,
    output_dir = tmp_output,
    quiet = TRUE
  )

  testthat::expect_false(result$trial_ready)
  testthat::expect_true(any(result$blocking_issues$issue_type == "no_mapped_key_field"))
  testthat::expect_true(any(result$blocking_issues$issue_type == "empty_key_target"))
})

testthat::test_that("build_append_redcap_payload maps source fields to append target fields", {
  append_mapping <- tibble::tibble(
    parser_version = "v1.0.0",
    uploader_version = "v1.1.0",
    source_name = c("patient_id", "identified_organism", "ampicillin_mic"),
    target_redcap_name = c("participant_id", "organism_name", "ampicillin_result"),
    update_mode = c("key", "update", "update"),
    key_field = c("TRUE", "FALSE", "FALSE"),
    notes = c("Primary lookup field", "", ""),
    source_present = c("TRUE", "TRUE", "TRUE")
  )

  result <- build_append_redcap_payload(
    redcap_df = data.frame(
      patient_id = c("P001", "P002"),
      identified_organism = c("E. coli", "Klebsiella"),
      ampicillin_mic = c("S", ""),
      stringsAsFactors = FALSE
    ),
    append_mapping = append_mapping,
    quiet = TRUE
  )

  testthat::expect_equal(names(result$payload), c("participant_id", "organism_name", "ampicillin_result"))
  testthat::expect_equal(result$payload$participant_id[[1]], "P001")
  testthat::expect_equal(result$payload$organism_name[[2]], "Klebsiella")
  testthat::expect_true(is.na(result$payload$ampicillin_result[[2]]))
})

testthat::test_that("build_append_redcap_payload strips derived suffix from patient_id keys", {
  append_mapping <- tibble::tibble(
    parser_version = "v1.0.0",
    uploader_version = "v1.1.0",
    source_name = c("patient_id"),
    target_redcap_name = c("participant_id"),
    update_mode = c("key"),
    key_field = c("TRUE"),
    notes = c("Primary lookup field"),
    source_present = c("TRUE"),
    target_field_type = c("text"),
    target_field_choices = c("")
  )

  result <- build_append_redcap_payload(
    redcap_df = data.frame(
      patient_id = c("11-0884-00-2_1", "11-0884-00-2_2"),
      stringsAsFactors = FALSE
    ),
    append_mapping = append_mapping,
    quiet = TRUE
  )

  testthat::expect_equal(result$payload$participant_id[[1]], "110884")
  testthat::expect_equal(result$payload$participant_id[[2]], "110884")
})

testthat::test_that("build_append_redcap_payload reduces patient_id keys to the first six digits for append", {
  append_mapping <- tibble::tibble(
    parser_version = "v1.0.0",
    uploader_version = "v1.1.0",
    source_name = c("patient_id"),
    target_redcap_name = c("participant_id"),
    update_mode = c("key"),
    key_field = c("TRUE"),
    notes = c("Primary lookup field"),
    source_present = c("TRUE"),
    target_field_type = c("text"),
    target_field_choices = c("")
  )

  result <- build_append_redcap_payload(
    redcap_df = data.frame(
      patient_id = c("11-23-2-3-5-RK"),
      stringsAsFactors = FALSE
    ),
    append_mapping = append_mapping,
    quiet = TRUE
  )

  testthat::expect_equal(result$payload$participant_id[[1]], "112323")
})

testthat::test_that("merge_append_payload_duplicates merges duplicate final append keys safely", {
  payload <- data.frame(
    participant_id = c("121266", "121266", "110884"),
    organism_name = c("E. coli", "E. coli", "Klebsiella"),
    ampicillin_result = c("1", "1", "2"),
    stringsAsFactors = FALSE
  )

  result <- merge_append_payload_duplicates(
    payload = payload,
    key_target_fields = "participant_id",
    quiet = TRUE
  )

  testthat::expect_equal(nrow(result$payload), 2)
  testthat::expect_equal(sum(result$payload$participant_id == "121266", na.rm = TRUE), 1)
  testthat::expect_equal(result$merged_row_count, 1)
  testthat::expect_equal(nrow(result$conflict_details), 0)
})

testthat::test_that("merge_append_payload_duplicates reports conflicts when duplicate keys disagree", {
  payload <- data.frame(
    participant_id = c("121266", "121266"),
    organism_name = c("E. coli", "Salmonella"),
    stringsAsFactors = FALSE
  )

  result <- merge_append_payload_duplicates(
    payload = payload,
    key_target_fields = "participant_id",
    quiet = TRUE
  )

  testthat::expect_equal(nrow(result$payload), 2)
  testthat::expect_true(nrow(result$conflict_details) > 0)
  testthat::expect_equal(result$conflict_details$target_redcap_name[[1]], "organism_name")
})

testthat::test_that("build_append_redcap_payload converts SIR values into REDCap dropdown codes", {
  append_mapping <- tibble::tibble(
    parser_version = "v1.0.0",
    uploader_version = "v1.1.0",
    source_name = c("linezolid_interpretation_raw"),
    target_redcap_name = c("crf11_16o1_kii"),
    update_mode = c("update"),
    key_field = c("FALSE"),
    notes = c(""),
    source_present = c("TRUE"),
    target_field_type = c("dropdown"),
    target_field_choices = c("1, Susceptible | 2, Intermediate | 3, Resistant")
  )

  result <- build_append_redcap_payload(
    redcap_df = data.frame(
      linezolid_interpretation_raw = c("S", "R", "I"),
      stringsAsFactors = FALSE
    ),
    append_mapping = append_mapping,
    quiet = TRUE
  )

  testthat::expect_equal(result$payload$crf11_16o1_kii[[1]], "1")
  testthat::expect_equal(result$payload$crf11_16o1_kii[[2]], "3")
  testthat::expect_equal(result$payload$crf11_16o1_kii[[3]], "2")
})

testthat::test_that("build_append_redcap_payload maps isolate fields to REDCap organism codes using identified organism labels", {
  append_mapping <- tibble::tibble(
    parser_version = "v1.0.0",
    uploader_version = "v1.1.0",
    source_name = c("isolate", "isolate_2"),
    target_redcap_name = c("crf11_10a_ai", "crf11_10b_ai"),
    update_mode = c("update", "update"),
    key_field = c("FALSE", "FALSE"),
    notes = c("", ""),
    source_present = c("TRUE", "TRUE"),
    target_field_type = c("dropdown", "dropdown"),
    target_field_choices = c(
      "1219, 1219-Budvicia aquatica | 0, 0-Escherichia coli",
      "1219, 1219-Budvicia aquatica | 0, 0-Escherichia coli"
    )
  )

  result <- build_append_redcap_payload(
    redcap_df = data.frame(
      isolate = c("SYNTHETIC-1 (Qualified)"),
      identified_organism = c("Budvicia aquatica"),
      isolate_2 = c("SYNTHETIC-2 (Qualified)"),
      identified_organism_2 = c("Escherichia coli"),
      stringsAsFactors = FALSE
    ),
    append_mapping = append_mapping,
    quiet = TRUE
  )

  testthat::expect_equal(result$payload$crf11_10a_ai[[1]], "1219")
  testthat::expect_equal(result$payload$crf11_10b_ai[[1]], "0")
})

testthat::test_that("upload_redcap_v1_1_0_append returns append payload artifacts in dry run mode", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("participant_id", "organism_name"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap_v1_1_0_append(
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    mapping_file = tmp_mapping,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "Append dry run completed")
  testthat::expect_true(result$trial_setup$trial_ready)
  testthat::expect_true(file.exists(result$trial_summary_file))
  testthat::expect_true(file.exists(result$append_mapping_preview_file))
  testthat::expect_true(file.exists(result$append_payload_file))
  testthat::expect_equal(names(result$append_payload), c("participant_id", "organism_name"))
})

testthat::test_that("upload_redcap_v1_1_0_append merges duplicate normalized patient keys before dry run output", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("participant_id", "organism_name"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap_v1_1_0_append(
    redcap_df = data.frame(
      patient_id = c("12-1266-00_1", "12-1266-00_2", "11-0884-00-2_1"),
      identified_organism = c("E. coli", "E. coli", "Klebsiella"),
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    mapping_file = tmp_mapping,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "Append dry run completed")
  testthat::expect_equal(nrow(result$append_payload), 2)
  testthat::expect_equal(sum(result$append_payload$participant_id == "121266", na.rm = TRUE), 1)
  testthat::expect_equal(result$duplicate_key_groups_identified, 1)
  testthat::expect_equal(result$duplicate_payload_rows_identified, 2)
  testthat::expect_equal(result$duplicate_rows_merged, 1)
  testthat::expect_equal(result$final_append_rows_ready, 2)
  testthat::expect_equal(result$final_rows_uploaded, 2)
  testthat::expect_true(file.exists(result$duplicate_merge_summary_file))
})

testthat::test_that("upload_redcap_v1_1_0_append reports rows excluded for having no valid update values", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "isolate"),
      target_redcap_name = c("participant_id", "crf11_10a_ai"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      target_field_type = c("text", "dropdown"),
      target_field_choices = c("", "1219, 1219-Budvicia aquatica"),
      target_text_validation_type = c("", ""),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap_v1_1_0_append(
    redcap_df = data.frame(
      patient_id = "SYNTHETIC-001_1",
      isolate = "SYNTHETIC-1 (To be reviewed)",
      identified_organism = "Unidentified Organism",
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    mapping_file = tmp_mapping,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "No append-ready rows")
  testthat::expect_match(result$warning_message, "excluded because no valid REDCap-mappable update values remained after conversion", fixed = TRUE)
  testthat::expect_true(file.exists(result$dropped_no_updates_file))
})

testthat::test_that("upload_redcap returns a true dry-run success result", {
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap(
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "Dry run completed")
  testthat::expect_true(isTRUE(result$dry_run))
  testthat::expect_true(file.exists(result$staging_csv))
  testthat::expect_true(file.exists(result$validation_summary_file))
})

testthat::test_that("run_shiny_pipeline reports the failing file when parsing stops", {
  tmp_project <- withr::local_tempdir()
  old_read <- read_vitek_pdf_full
  old_parse <- parse_ast_chart_report_full

  withr::defer(assign("read_vitek_pdf_full", old_read, envir = .GlobalEnv))
  withr::defer(assign("parse_ast_chart_report_full", old_parse, envir = .GlobalEnv))

  assign(
    "read_vitek_pdf_full",
    function(file_path, output_dir, output_file, quiet = TRUE) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines("raw text", file.path(output_dir, output_file))
      list(
        metadata = tibble::tibble(source_file = basename(file_path)),
        page_text = c("placeholder"),
        full_text = "placeholder"
      )
    },
    envir = .GlobalEnv
  )

  assign(
    "parse_ast_chart_report_full",
    function(pdf_object, quiet = TRUE) {
      stop("Could not extract susceptibility result block from full report.")
    },
    envir = .GlobalEnv
  )

  bad_pdf <- file.path(tmp_project, "bad_layout.pdf")
  file.create(bad_pdf)

  withr::local_dir(tmp_project)

  testthat::expect_error(
    run_shiny_pipeline(
      uploaded_paths = bad_pdf,
      uploaded_names = basename(bad_pdf)
    ),
    regexp = "Failed while parsing file bad_layout[.]pdf"
  )

  run_dirs <- list.dirs(file.path("output", "runs"), recursive = FALSE, full.names = TRUE)
  testthat::expect_true(length(run_dirs) >= 1)

  latest_run <- run_dirs[[which.max(file.info(run_dirs)$mtime)]]
  manifest_file <- file.path(latest_run, "input_files_manifest.csv")
  audit_files <- list.files(latest_run, pattern = "^audit_.*[.]csv$", full.names = TRUE)

  testthat::expect_true(file.exists(manifest_file))
  testthat::expect_true(length(audit_files) == 1)

  audit_log <- readr::read_csv(audit_files[[1]], show_col_types = FALSE)
  testthat::expect_true(any(audit_log$status == "error"))
  testthat::expect_true(any(grepl("bad_layout[.]pdf", audit_log$details)))
})

testthat::test_that("run_shiny_pipeline maps merged second-organism slots into upload-ready output", {
  tmp_project <- withr::local_tempdir()
  old_read <- read_vitek_pdf_full
  old_parse <- parse_ast_chart_report_full

  withr::defer(assign("read_vitek_pdf_full", old_read, envir = .GlobalEnv))
  withr::defer(assign("parse_ast_chart_report_full", old_parse, envir = .GlobalEnv))

  dir.create(file.path(tmp_project, "docs"), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(
    tibble::tibble(
      source_name = c(
        "patient_id",
        "report_version",
        "identified_organism",
        "identified_organism_2",
        "isolate",
        "isolate_2"
      ),
      redcap_name = c(
        "patient_id",
        "report_version",
        "identified_organism",
        "identified_organism_2",
        "isolate",
        "isolate_2"
      )
    ),
    file.path(tmp_project, "docs", "redcap_variable_mapping.csv")
  )

  assign(
    "read_vitek_pdf_full",
    function(file_path, output_dir, output_file, quiet = TRUE) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      writeLines("raw text", file.path(output_dir, output_file))
      list(
        metadata = tibble::tibble(source_file = basename(file_path)),
        page_text = c("placeholder"),
        full_text = "placeholder"
      )
    },
    envir = .GlobalEnv
  )

  parse_counter <- 0L
  assign(
    "parse_ast_chart_report_full",
    function(pdf_object, quiet = TRUE) {
      parse_counter <<- parse_counter + 1L
      rows <- list(
        tibble::tibble(
          patient_id = "14-1138-01",
          report_version = "1 of 2",
          identified_organism = "Escherichia coli",
          isolate = "1",
          comments = ""
        ),
        tibble::tibble(
          patient_id = "14-1138-01",
          report_version = "2 of 2",
          identified_organism = "Enterobacter cloacae complex",
          isolate = "2",
          comments = ""
        )
      )
      list(ast_wide = rows[[parse_counter]])
    },
    envir = .GlobalEnv
  )

  bad_pdf_1 <- file.path(tmp_project, "one.pdf")
  bad_pdf_2 <- file.path(tmp_project, "two.pdf")
  file.create(bad_pdf_1)
  file.create(bad_pdf_2)
  dir.create(file.path(tmp_project, "data_raw"), recursive = TRUE, showWarnings = FALSE)

  withr::local_dir(tmp_project)

  result <- run_shiny_pipeline(
    uploaded_paths = c(bad_pdf_1, bad_pdf_2),
    uploaded_names = c("one.pdf", "two.pdf")
  )

  testthat::expect_equal(nrow(result$parsed_wide), 2)
  testthat::expect_equal(nrow(result$parsed_wide_clean), 1)
  testthat::expect_equal(nrow(result$upload_ready), 1)
  testthat::expect_equal(result$upload_ready$identified_organism[[1]], "Escherichia coli")
  testthat::expect_equal(result$upload_ready$identified_organism_2[[1]], "Enterobacter cloacae complex")
  testthat::expect_equal(result$upload_ready$isolate[[1]], "1")
  testthat::expect_equal(result$upload_ready$isolate_2[[1]], "2")
})

testthat::test_that("upload_redcap normalizes patient_id and removes unnecessary suffixes in dry run mode", {
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap(
    redcap_df = data.frame(
      patient_id = c("11-22-33_1", "112-3-24_1", "112-3-24_2"),
      identified_organism = c("Org A", "Org B", "Org C"),
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$upload_ready$patient_id[[1]], "112233")
  testthat::expect_equal(result$upload_ready$patient_id[[2]], "112324_1")
  testthat::expect_equal(result$upload_ready$patient_id[[3]], "112324_2")
})

testthat::test_that("upload_redcap keeps the full digit string when it is shorter than six digits", {
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap(
    redcap_df = data.frame(
      patient_id = c("140-23_3"),
      identified_organism = c("Org A"),
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    save_staging = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$upload_ready$patient_id[[1]], "14023")
})

testthat::test_that("upload_redcap_dispatcher routes v1.0.0 successfully in dry run mode", {
  tmp_config <- tempfile(fileext = ".yml")
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id"),
      target_redcap_name = c("participant_id"),
      update_mode = c("key"),
      key_field = c("TRUE"),
      notes = c("Primary lookup field")
    ),
    tmp_mapping
  )

  result <- upload_redcap_dispatcher(
    uploader_version = "v1.0.0",
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    mapping_file = tmp_mapping,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "Dry run completed")
  testthat::expect_true(isTRUE(result$dry_run))
})

testthat::test_that("upload_redcap_dispatcher routes v1.1.0 successfully in dry run mode", {
  tmp_mapping <- tempfile(fileext = ".csv")
  tmp_config <- tempfile(fileext = ".yml")
  tmp_output <- tempfile()
  dir.create(tmp_output, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    tibble::tibble(
      parser_version = "v1.0.0",
      uploader_version = "v1.1.0",
      source_name = c("patient_id", "identified_organism"),
      target_redcap_name = c("participant_id", "organism_name"),
      update_mode = c("key", "update"),
      key_field = c("TRUE", "FALSE"),
      notes = c("Primary lookup field", "")
    ),
    tmp_mapping
  )

  writeLines(
    c(
      "redcap_uri: https://example.org/api/",
      "keyring_service: redcap-service",
      "keyring_username: analyst"
    ),
    tmp_config
  )

  result <- upload_redcap_dispatcher(
    uploader_version = "v1.1.0",
    redcap_df = data.frame(
      patient_id = "P001",
      identified_organism = "E. coli",
      stringsAsFactors = FALSE
    ),
    config_file = tmp_config,
    mapping_file = tmp_mapping,
    required_fields = character(),
    key_fields = "patient_id",
    marker_tests = character(),
    output_dir = tmp_output,
    dry_run = TRUE,
    quiet = TRUE
  )

  testthat::expect_equal(result$status, "Append dry run completed")
  testthat::expect_true(isTRUE(result$dry_run))
  testthat::expect_true(file.exists(result$append_payload_file))
})
