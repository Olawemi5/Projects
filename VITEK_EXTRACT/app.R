source(file.path("scripts", "packages.R"))
source(file.path("functions", "load_redcap_config.R"))
source(file.path("functions", "keyring_setup_helpers.R"))
source(file.path("functions", "read_pdf_manifest.R"))
source(file.path("functions", "get_unprocessed_files.R"))
source(file.path("functions", "read_vitek_pdf_full.R"))
source(file.path("functions", "parse_ast_chart_report_full.R"))
source(file.path("functions", "map_redcap.R"))
source(file.path("functions", "validate_redcap_data.R"))
source(file.path("functions", "clean_data.R"))
source(file.path("functions", "upload_redcap.R"))
source(file.path("functions", "prepare_append_trial.R"))
source(file.path("functions", "upload_dispatcher.R"))
source(file.path("functions", "load_append_redcap_mapping.R"))
source(file.path("functions", "normalize_mapping_terms.R"))
source(file.path("functions", "read_redcap_dictionary.R"))
source(file.path("functions", "suggest_redcap_target_fields.R"))
source(file.path("functions", "update_append_mapping_targets.R"))
source(file.path("functions", "audit_logger.R"))
source(file.path("functions", "run_shiny_pipeline.R"))
source(file.path("functions", "import_github_data_raw.R"))

dir.create("data_raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "templates"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "runs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "cache"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("docs", "saved_mappings"), recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
dir.create("review", recursive = TRUE, showWarnings = FALSE)

initial_redcap_cfg <- tryCatch(
  load_redcap_config(redcap_config_file()),
  error = function(e) {
    list(
      redcap_uri = "",
      keyring_service = "",
      keyring_username = ""
    )
  }
)

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#ebe6dd",
  fg = "#1a2620",
  primary = "#2a5240",
  secondary = "#6b5344",
  success = "#357a5c",
  warning = "#a67c32",
  danger = "#8b3a2e",
  base_font = bslib::font_google("DM Sans"),
  heading_font = bslib::font_google("Syne"),
  code_font = bslib::font_google("IBM Plex Mono")
)

metric_card <- function(title, value_output, accent_class) {
  shiny::div(
    class = paste("metric-card", accent_class),
    shiny::div(class = "metric-label", title),
    shiny::div(class = "metric-value", shiny::textOutput(value_output, inline = TRUE))
  )
}

timestamp_slug <- function(time = Sys.time()) {
  format(time, "%Y%m%d_%H%M%S")
}

safe_input_value <- function(value, default = "") {
  if (is.null(value) || length(value) == 0 || is.na(value)) default else value
}

append_upload_audit_summary <- function(upload_result) {
  if (is.null(upload_result)) {
    return(NULL)
  }

  list(
    duplicate_key_groups_identified = safe_input_value(upload_result$duplicate_key_groups_identified, 0L),
    duplicate_payload_rows_identified = safe_input_value(upload_result$duplicate_payload_rows_identified, 0L),
    duplicate_rows_merged = safe_input_value(upload_result$duplicate_rows_merged, 0L),
    final_append_rows_ready = safe_input_value(upload_result$final_append_rows_ready, 0L),
    final_rows_uploaded = safe_input_value(upload_result$final_rows_uploaded, 0L)
  )
}

is_append_upload_result <- function(upload_result) {
  !is.null(upload_result) &&
    !is.null(upload_result$uploader_version) &&
    identical(as.character(upload_result$uploader_version), "v1.1.0")
}

sanitize_mapping_name <- function(value, default = "accepted_mapping") {
  cleaned <- trimws(as.character(value))
  if (!nzchar(cleaned)) {
    cleaned <- default
  }

  cleaned <- gsub("[^A-Za-z0-9_-]+", "_", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)

  if (!nzchar(cleaned)) {
    cleaned <- default
  }

  cleaned
}

app_templates_dir <- function(...) {
  file.path("output", "templates", ...)
}

saved_mappings_dir <- function(...) {
  file.path("docs", "saved_mappings", ...)
}

app_runs_dir <- function(...) {
  file.path("output", "runs", ...)
}

app_cache_dir <- function(...) {
  file.path("output", "cache", ...)
}

app_audit_log_file <- function() {
  app_cache_dir("app_actions_audit.csv")
}

list_saved_mapping_files <- function() {
  search_dirs <- c(saved_mappings_dir(), app_templates_dir())
  existing_dirs <- search_dirs[dir.exists(search_dirs)]
  if (length(existing_dirs) == 0) {
    return(character())
  }

  files <- unlist(lapply(existing_dirs, function(dir_path) {
    list.files(
      dir_path,
      pattern = "^redcap_append_variable_mapping_.*\\.csv$",
      full.names = TRUE
    )
  }), use.names = FALSE)

  files <- unique(files)
  if (length(files) == 0) {
    return(character())
  }

  files[order(file.info(files)$mtime, decreasing = TRUE)]
}

saved_mapping_label <- function(path) {
  file_name <- basename(path)
  timestamp_match <- stringr::str_match(file_name, "_([0-9]{8}_[0-9]{6})[.]csv$")
  timestamp_raw <- timestamp_match[, 2]
  timestamp_text <- if (!is.na(timestamp_raw) && nzchar(timestamp_raw)) {
    format(as.POSIXct(timestamp_raw, format = "%Y%m%d_%H%M%S"), "%Y-%m-%d %H:%M:%S")
  } else {
    format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S")
  }

  base_label <- sub("^redcap_append_variable_mapping_", "", file_name)
  base_label <- sub("_[0-9]{8}_[0-9]{6}[.]csv$", "", base_label)
  base_label <- gsub("_", " ", base_label)
  base_label <- tools::toTitleCase(tolower(base_label))

  paste0(base_label, " (", timestamp_text, ")")
}

package_summary_lines <- function() {
  c(
    "VITEK AST Extraction and REDCap Upload",
    "",
    "Executive summary",
    "This package is an R and Shiny application for turning VITEK antimicrobial susceptibility testing PDF reports into structured, reviewable, and REDCap-ready microbiology data. It supports an end-to-end workflow from PDF intake through extraction, parsing, cleaning, validation, audit logging, and upload.",
    "",
    "What the application does",
    "The app can import VITEK PDFs from local uploads or a GitHub repository, extract raw report text, parse organism identification and AST results, reshape those results into structured datasets, validate records, separate review cases, and prepare upload-ready REDCap payloads.",
    "",
    "Uploader options",
    "Uploader v1.0.0 is designed for REDCap projects that already follow the parser-ready structure expected by this package. It uses the existing parser schema directly and is the simpler route when destination field names and layout are already aligned with the package output.",
    "Uploader v1.1.0 is designed for append-safe REDCap workflows where the destination project has its own naming conventions and existing variables. It supports REDCap dictionary-assisted mapping, saved mapping files, append trials, and mapped partial updates so that only selected destination fields are written while unrelated REDCap fields are preserved.",
    "",
    "Outputs and audit trail",
    "Each run creates structured outputs that help with traceability and review. Parsed files, upload-ready datasets, review files, append payloads, upload logs, and audit logs are saved into timestamped output folders so each processing run can be traced independently. Reusable generated mapping files are saved separately for later reuse.",
    "",
    "What is currently in development",
    "Parser v1.1.0 is still in development. The goal of this next parser version is to expand coverage so the package can recognize and structure a broader set of antibiotics and related AST content than parser v1.0.0 currently handles.",
    "",
    "Thank you",
    "Thank you for using this application. Please do not hesitate to reach out with suggestions to further improve the package, or if you would like to contribute, support, or collaborate. The contact options are available in the Contact Me menu inside the app.",
    "",
    "BioParseR.com",
    "https://BioParseR.com"
  )
}

datatable_export_buttons <- function(filename_base) {
  timestamped_filename <- paste0(
    filename_base,
    "_",
    timestamp_slug()
  )
  export_modifier <- list(
    columns = ":visible",
    modifier = list(
      page = "all",
      search = "applied",
      order = "applied"
    )
  )

  list(
    list(extend = "copy", title = NULL, exportOptions = export_modifier),
    list(extend = "csv", filename = timestamped_filename, title = NULL, exportOptions = export_modifier),
    list(extend = "excel", filename = timestamped_filename, title = NULL, exportOptions = export_modifier)
  )
}

ui <- bslib::page_sidebar(
  title = shiny::div(
    class = "app-header-bar",
    shiny::div(
      class = "app-header-left",
      shiny::uiOutput("back_to_landing_ui"),
      shiny::div(
        class = "app-title-wrap",
        shiny::div(class = "app-kicker", "Microbiology Pipeline"),
        shiny::div(class = "app-title", "VITEK AST Extraction and REDCap Upload")
      )
    ),
    shiny::uiOutput("header_controls_ui")
  ),
  shiny::uiOutput("layout_mode_ui"),
  theme = app_theme,
  sidebar = bslib::sidebar(
    width = 360,
    open = "desktop",
    shiny::conditionalPanel(
      condition = "output.app_stage == 'workflow'",
      shiny::tags$div(
      class = "sidebar-section",
      shiny::tags$h4("Pipeline Input"),
      shiny::fileInput("pdf_files", "Upload VITEK PDF reports", accept = ".pdf", multiple = TRUE),
      shiny::actionButton("toggle_github_upload", "GitHub Upload", class = "btn-outline-dark"),
      shiny::conditionalPanel(
        condition = "input.toggle_github_upload % 2 == 1",
        shiny::div(
          class = "toggle-panel",
          shiny::textInput("github_repo_url", "GitHub repository URL", placeholder = "https://github.com/owner/repo"),
          shiny::actionButton("import_github", "Import PDFs From GitHub", class = "btn-outline-dark"),
          shiny::uiOutput("github_import_inline_status_ui")
        )
      ),
      shiny::div(
        class = "button-stack",
        shiny::actionButton("run_pipeline", "Run Pipeline", class = "btn-primary btn-lg")
      )
    )),
    shiny::conditionalPanel(
      condition = "output.app_stage == 'workflow' && output.mapping_enabled == 'true'",
      shiny::tags$div(
      class = "sidebar-section",
      bslib::accordion(
        id = "redcap_mapping_tools_accordion",
        open = FALSE,
        bslib::accordion_panel(
          title = "REDCap Mapping And Tools",
          shiny::div(
            class = "subtool-toggle-row",
            shiny::actionButton("show_redcap_mapping", "REDCap Mapping", class = "btn-outline-primary"),
            shiny::actionButton("show_mapped_upload", "Mapped Upload", class = "btn-outline-primary")
          ),
          shiny::uiOutput("saved_mapping_picker_ui"),
          shiny::div(
            class = "button-stack",
            shiny::downloadButton("download_dictionary", "Download Data Dictionary Template", class = "btn btn-outline-primary"),
            shiny::downloadButton("download_append_mapping", "Download Append Mapping Template", class = "btn btn-outline-primary")
          ),
          shiny::conditionalPanel(
            condition = "input.show_redcap_mapping % 2 == 1",
            shiny::div(
              class = "toggle-panel",
              shiny::fileInput(
                "redcap_dictionary_file",
                "Upload REDCap Data Dictionary",
                accept = c(".csv")
              ),
              shiny::actionButton("suggest_append_mapping", "Suggest Append Mapping Targets", class = "btn-outline-primary")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.show_mapped_upload % 2 == 1",
            shiny::div(
              class = "toggle-panel",
              shiny::fileInput(
                "mapped_append_mapping_file",
                "Upload Completed Append Mapping CSV",
                accept = c(".csv")
              ),
              shiny::tags$div(
                class = "workflow-step",
                "Upload a completed append mapping file to use it directly for REDCap upload without running the matcher."
              )
            )
          )
        )
      )
    )),
    shiny::conditionalPanel(
      condition = "output.app_stage == 'workflow'",
      shiny::tags$div(
      class = "sidebar-section",
      bslib::accordion(
        id = "redcap_upload_accordion",
        open = FALSE,
        bslib::accordion_panel(
          title = "REDCap Upload",
          shiny::checkboxInput("dry_run_upload", "Dry run REDCap upload", value = TRUE),
          shiny::div(
            class = "button-stack",
            shiny::conditionalPanel(
              condition = "output.mapping_enabled == 'false'",
              shiny::actionButton("run_upload", "Upload Validated Records", class = "btn-outline-secondary")
            ),
            shiny::conditionalPanel(
              condition = "output.mapping_enabled == 'true'",
              shiny::actionButton("run_append_upload", "Append To Existing REDCap", class = "btn-outline-success")
            )
          ),
          shiny::tags$div(
            class = "workflow-step",
            "Run the pipeline first, then use this panel to upload only the validated rows."
          ),
          shiny::conditionalPanel(
            condition = "output.mapping_enabled == 'true'",
            shiny::tags$div(
              class = "workflow-step",
              "The append button uses uploader v1.1.0 to prepare the mapped payload and append validated rows to an existing REDCap project."
            )
          )
        )
      )
    )),
    shiny::conditionalPanel(
      condition = "output.app_stage == 'workflow'",
      shiny::tags$div(
      class = "sidebar-section workflow-steps-panel",
      shiny::uiOutput("workflow_steps_ui")
    )),
    shiny::conditionalPanel(
      condition = "output.app_stage == 'workflow'",
      shiny::tags$div(
      class = "sidebar-section",
      shiny::tags$h4("Current Status"),
      shiny::actionButton("change_uploader_version", "Change Uploader Version", class = "btn-outline-dark"),
      shiny::verbatimTextOutput("status_text")
    ))
  ),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* VSUFT-inspired: forest greens, processed wood, geometric frames, subtle growth rings */
      .bslib-page-title { border-bottom: 2px solid #2a5240; padding-bottom: 0.65rem; margin-bottom: 0.5rem; }
      .app-title-wrap { line-height: 1.1; }
      .app-header-bar { display: flex; align-items: flex-start; justify-content: space-between; gap: 20px; width: 100%; overflow: visible; }
      .app-header-left { display: flex; align-items: flex-start; gap: 12px; }
      .header-controls { display: flex; align-items: flex-start; gap: 10px; flex-wrap: wrap; overflow: visible; position: relative; z-index: 20; }
      .back-arrow-btn { border: none; background: transparent; color: #1a2620; font-size: 1.4rem; line-height: 1; padding: 4px 8px; margin-top: 2px; border-radius: 4px; }
      .back-arrow-btn:hover { background: rgba(42,82,64,0.10); color: #2a5240; }
      .app-kicker { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.18em; color: #6b5344; font-weight: 600; }
      .app-title { font-family: 'Syne', sans-serif; font-size: 1.55rem; font-weight: 700; color: #1a2620; letter-spacing: -0.02em; }
      .parser-header-control { min-width: 120px; display: flex; flex-direction: column; align-items: flex-end; }
      .parser-label { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.12em; color: #6b5344; margin-bottom: 4px; font-weight: 600; }
      .parser-header-control .form-group { margin-bottom: 0; }
      .header-refresh-control { min-width: 170px; display: flex; flex-direction: column; align-items: flex-end; position: relative; overflow: visible; }
      .header-refresh-control .accordion { width: 170px; position: relative; overflow: visible; }
      .header-refresh-control .accordion-item,
      .header-refresh-control .accordion-header,
      .header-refresh-control .accordion-collapse { overflow: visible; }
      .header-refresh-control .accordion-button { padding: 0.48rem 0.85rem; font-size: 0.72rem; letter-spacing: 0.1em; text-transform: uppercase; color: #1a2620; align-items: flex-start; font-weight: 600; border-radius: 4px; }
      .header-refresh-control .accordion-button:not(.collapsed) { box-shadow: none; }
      .header-refresh-control .accordion-collapse { position: absolute; top: calc(100% + 6px); right: 0; width: min(320px, calc(100vw - 32px)); z-index: 1200; }
      .header-refresh-control .accordion-body { padding: 0.9rem; min-width: 0; background: rgba(252,250,246,0.98); border: 2px solid #2a5240; border-radius: 4px; box-shadow: 0 18px 40px rgba(26,38,32,0.12); }
      .header-refresh-control .button-stack .btn { margin-top: 8px; }
      .header-refresh-control .workflow-step { margin-bottom: 0; font-size: 0.88rem; }
      .sidebar-section { background: rgba(252,250,246,0.92); border: 2px solid #2a5240; border-radius: 4px; padding: 16px; margin-bottom: 16px; box-shadow: 0 4px 0 rgba(42,82,64,0.08); }
      .sidebar-section .btn-outline-dark { width: 100%; border-radius: 4px; margin-top: 6px; margin-bottom: 10px; }
      .sidebar-section .btn-outline-primary { width: 100%; border-radius: 4px; margin-top: 10px; }
      .button-stack .btn { width: 100%; margin-top: 10px; border-radius: 4px; font-weight: 600; letter-spacing: 0.04em; }
      .toggle-panel { margin-top: 12px; padding-top: 10px; border-top: 1px solid rgba(107,83,68,0.25); }
      .inline-import-status { margin-top: 8px; padding-top: 6px; border-top: 1px solid rgba(107,83,68,0.22); font-size: 0.86rem; color: #4a5c52; line-height: 1.35; }
      .subtool-toggle-row { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 8px; }
      .subtool-toggle-row .btn { border-radius: 4px; }
      .workflow-steps-title { font-family: 'Syne', sans-serif; font-size: 1rem; margin-bottom: 10px; color: #1a2620; font-weight: 700; }
      .workflow-step { padding: 7px 10px; margin-bottom: 6px; border-radius: 2px; background: rgba(235,230,221,0.9); border-left: 4px solid #2a5240; font-size: 0.88rem; line-height: 1.35; }
      .metric-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 16px; margin-bottom: 20px; }
      .metric-card { border-radius: 4px; padding: 18px; color: #fcfaf6; box-shadow: 0 8px 24px rgba(26,38,32,0.14); border: 1px solid rgba(252,250,246,0.15); position: relative; overflow: hidden; }
      .metric-card::after { content: ''; position: absolute; inset: -50%; background: repeating-radial-gradient(circle at 30% 40%, transparent 0, transparent 14px, rgba(255,255,255,0.04) 14px, rgba(255,255,255,0.04) 15px); pointer-events: none; opacity: 0.85; }
      .metric-card.teal { background: linear-gradient(145deg, #234d3a 0%, #357a5c 100%); }
      .metric-card.sage { background: linear-gradient(145deg, #5c4a3a 0%, #8b7355 100%); }
      .metric-card.amber { background: linear-gradient(145deg, #8a6a2e 0%, #c4a35a 100%); }
      .metric-card.terracotta { background: linear-gradient(145deg, #6e382e 0%, #9a5545 100%); }
      .metric-label { font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.12em; opacity: 0.92; font-weight: 600; position: relative; z-index: 1; }
      .metric-value { font-family: 'Syne', sans-serif; font-size: 2rem; font-weight: 700; margin-top: 8px; position: relative; z-index: 1; }
      .section-card { background: rgba(252,250,246,0.95); border: 2px solid #2a5240; border-radius: 4px; padding: 18px; margin-bottom: 18px; box-shadow: 0 6px 28px rgba(26,38,32,0.07); }
      .section-title { font-family: 'Syne', sans-serif; font-size: 1.12rem; font-weight: 700; color: #1a2620; margin-bottom: 8px; letter-spacing: -0.02em; }
      .section-subtitle { color: #4a5c52; margin-bottom: 14px; }
      .status-banner { background: linear-gradient(180deg, #f2ede4 0%, #e5ddd2 100%); border: 2px solid #6b5344; border-radius: 4px; margin-bottom: 18px; overflow: hidden; max-height: 56px; transition: max-height 0.25s ease, box-shadow 0.25s ease; cursor: pointer; }
      .status-banner:hover, .status-banner:focus-within { max-height: 320px; box-shadow: 0 12px 32px rgba(26,38,32,0.10); }
      .status-banner-head { display: flex; align-items: center; justify-content: space-between; gap: 14px; padding: 14px 16px; font-weight: 700; color: #1a2620; font-family: 'Syne', sans-serif; }
      .status-banner-hint { font-size: 0.82rem; font-weight: 500; color: #6b5344; letter-spacing: 0.06em; text-transform: uppercase; }
      .status-banner-body { padding: 0 16px 16px 16px; }
      .status-line { margin-bottom: 4px; font-size: 0.98rem; }
      .bslib-sidebar-layout > .main { padding-top: 20px; position: relative; }
      .bslib-sidebar-layout > .main::before { content: ''; position: absolute; top: 0; right: 0; width: min(420px, 45vw); height: min(420px, 50vh); pointer-events: none; opacity: 0.07; background: repeating-radial-gradient(circle at 70% 0%, transparent 0, transparent 18px, #2a5240 18px, #2a5240 19px); border-radius: 50%; transform: translate(15%, -25%); }
      .table-action-bar { display: flex; justify-content: flex-end; align-items: center; gap: 10px; flex-wrap: wrap; margin-bottom: 10px; }
      .table-action-bar .btn { border-radius: 4px; }
      .table-action-bar .form-group { margin-bottom: 0; min-width: 240px; }
      .saved-mapping-picker { margin: 12px -10px 10px 0; overflow: visible; }
      .saved-mapping-picker .form-group { margin-bottom: 0; width: calc(100% + 10px); }
      .saved-mapping-picker .form-control,
      .saved-mapping-picker .selectize-control { width: 100%; }
      .saved-mapping-picker .selectize-input,
      .saved-mapping-picker select.form-control { min-height: 54px; font-size: 0.98rem; line-height: 1.45; white-space: normal; border-radius: 4px; }
      .saved-mapping-actions { display: flex; flex-direction: column; align-items: flex-start; gap: 8px; overflow: visible; }
      .saved-mapping-use-btn { width: auto !important; min-width: 136px; margin-top: 0 !important; padding: 0.42rem 0.9rem; font-size: 0.88rem; border-radius: 4px; }
      .landing-shell { min-height: calc(100vh - 140px); display: flex; align-items: center; justify-content: center; width: 100%; position: relative; }
      .landing-shell::before { content: ''; position: fixed; inset: 0; pointer-events: none; z-index: 0; background: radial-gradient(ellipse 90% 70% at 0% 30%, rgba(42,82,64,0.09) 0%, transparent 55%), radial-gradient(ellipse 80% 60% at 100% 70%, rgba(139,115,85,0.10) 0%, transparent 50%); }
      .landing-card { max-width: 900px; width: min(100%, 900px); min-height: 640px; margin: 0 auto; position: relative; z-index: 1; background: rgba(252,250,246,0.94); border: 2px solid #2a5240; border-radius: 4px; padding: 36px 40px; box-shadow: 0 24px 48px rgba(26,38,32,0.10), 6px 0 0 #6b5344 inset; overflow: hidden; }
      .landing-card::before { content: ''; position: absolute; top: -60px; right: -40px; width: 240px; height: 240px; pointer-events: none; border-radius: 50%; background: repeating-radial-gradient(circle at center, transparent 0, transparent 10px, rgba(42,82,64,0.11) 10px, rgba(42,82,64,0.11) 11px); }
      .landing-kicker { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.2em; color: #6b5344; margin-bottom: 10px; font-weight: 600; }
      .landing-title { font-family: 'Syne', sans-serif; font-size: 2.05rem; font-weight: 700; color: #1a2620; margin-bottom: 12px; letter-spacing: -0.03em; line-height: 1.15; }
      .landing-subtitle { font-size: 1.05rem; color: #3d4a42; margin-bottom: 20px; max-width: 560px; line-height: 1.5; }
      .landing-note { margin-top: 10px; margin-bottom: 16px; padding: 12px 14px; border-radius: 2px; background: rgba(235,230,221,0.95); border-left: 4px solid #2a5240; color: #1a2620; }
      .landing-about-trigger { position: absolute; right: -18px; bottom: 18px; width: 44px; height: 44px; border-radius: 4px; display: inline-flex; align-items: center; justify-content: center; padding: 0; font-size: 1.05rem; box-shadow: 0 10px 24px rgba(26,38,32,0.12); border: 2px solid #2a5240; }
      .landing-about-trigger:hover { transform: translateY(-2px); background: #2a5240 !important; color: #fcfaf6 !important; }
      .landing-contact-trigger { position: absolute; right: -18px; bottom: 72px; width: 44px; height: 44px; border-radius: 4px; display: inline-flex; align-items: center; justify-content: center; padding: 0; font-size: 1rem; box-shadow: 0 10px 24px rgba(26,38,32,0.12); border: 2px solid #6b5344; }
      .landing-contact-trigger:hover { transform: translateY(-2px); }
      .contact-option-list { display: grid; gap: 10px; margin-top: 8px; }
      .contact-option-item { padding: 10px 12px; border-radius: 2px; background: rgba(235,230,221,0.95); border-left: 4px solid #2a5240; color: #1a2620; }
      .contact-option-head { display: flex; align-items: center; gap: 8px; margin-bottom: 2px; }
      .contact-option-icon { width: 1.15rem; text-align: center; font-size: 1rem; color: #2a5240; }
      .contact-option-label { font-weight: 700; color: #1a2620; font-family: 'Syne', sans-serif; }
      .contact-option-muted { color: #5a6a62; }
      .card { border-radius: 4px; }
      .nav-tabs .nav-link { border-radius: 2px 2px 0 0; font-weight: 600; letter-spacing: 0.03em; }
    "))
  ),
  shiny::conditionalPanel(
    condition = "output.app_stage == 'landing'",
    shiny::div(
      class = "landing-shell",
      shiny::div(
        class = "landing-card",
        shiny::div(class = "landing-kicker", "Package"),
        shiny::div(class = "landing-title", "VITEK AST Extraction and REDCap Upload"),
        shiny::uiOutput("landing_keyring_status_ui"),
        shiny::div(
          class = "button-stack",
          shiny::actionButton("open_keyring_setup", "Set Up REDCap Keyring", class = "btn-outline-success")
        ),
        shiny::div(
          class = "landing-subtitle",
          "Choose the uploader version to enter the matching workflow for this REDCap destination."
        ),
        shiny::selectInput(
          "landing_uploader_version",
          "Uploader Version",
          selected = "v1.0.0",
          choices = c(
            "v1.0.0" = "v1.0.0",
            "v1.1.0" = "v1.1.0"
          ),
          width = "280px"
        ),
        shiny::uiOutput("landing_uploader_message"),
        shiny::actionButton("enter_workflow", "Continue", class = "btn-primary btn-lg"),
        shiny::actionButton("open_contact_developer", HTML("&#9742;"), class = "btn btn-outline-secondary landing-contact-trigger", title = "Contact developer"),
        shiny::actionButton("open_package_summary", HTML("&#128161;"), class = "btn btn-outline-secondary landing-about-trigger", title = "About this package")
      )
    )
  ),
  shiny::conditionalPanel(
    condition = "output.app_stage == 'workflow'",
    shiny::uiOutput("workflow_main_ui")
  )
)

server <- function(input, output, session) {
  app_stage <- shiny::reactiveVal("landing")
  selected_uploader_version <- shiny::reactiveVal("v1.0.0")
  pipeline_state <- shiny::reactiveVal(NULL)
  upload_state <- shiny::reactiveVal(NULL)
  github_import_state <- shiny::reactiveVal(NULL)
  redcap_config_state <- shiny::reactiveVal(initial_redcap_cfg)
  keyring_status_state <- shiny::reactiveVal(
    check_redcap_keyring_status(config_file = redcap_config_file())
  )
  append_mapping_state <- shiny::reactiveVal(NULL)
  active_append_mapping_file <- shiny::reactiveVal(file.path("docs", "redcap_append_variable_mapping.csv"))

  output$app_stage <- shiny::renderText(app_stage())
  output$mapping_enabled <- shiny::renderText(if (identical(selected_uploader_version(), "v1.1.0")) "true" else "false")
  shiny::outputOptions(output, "app_stage", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "mapping_enabled", suspendWhenHidden = FALSE)

  output$saved_mapping_picker_ui <- shiny::renderUI({
    saved_files <- list_saved_mapping_files()

    if (length(saved_files) == 0) {
      return(
        shiny::div(
          class = "saved-mapping-picker workflow-step",
          "No saved mappings yet. Accepted mappings are stored permanently, while temporary uploaded mappings appear here for the current workspace."
        )
      )
    }

    choice_labels <- vapply(saved_files, saved_mapping_label, character(1))

    shiny::div(
      class = "saved-mapping-picker",
      shiny::div(
        class = "saved-mapping-actions",
        shiny::selectInput(
          "saved_mapping_choice",
          "Saved Generated Mapping",
          choices = stats::setNames(saved_files, choice_labels),
          selected = if (active_append_mapping_file() %in% saved_files) active_append_mapping_file() else saved_files[[1]]
        ),
        shiny::actionButton("use_saved_mapping", "Use Saved Mapping", class = "btn-outline-dark saved-mapping-use-btn")
      )
    )
  })

  output$layout_mode_ui <- shiny::renderUI({
    if (identical(app_stage(), "landing")) {
      shiny::tags$style(shiny::HTML("
        .bslib-sidebar-layout { grid-template-columns: minmax(0, 1fr) !important; }
        .bslib-sidebar-layout > .sidebar { display: none !important; }
        .bslib-sidebar-layout > .main { grid-column: 1 / -1 !important; width: 100% !important; margin: 0 auto !important; }
        .collapse-toggle { display: none !important; }
      "))
    } else {
      shiny::tags$style(shiny::HTML("
        .bslib-sidebar-layout { grid-template-columns: 360px minmax(0, 1fr) !important; }
        .bslib-sidebar-layout > .sidebar { display: block !important; }
        .bslib-sidebar-layout > .main { grid-column: auto !important; width: auto !important; margin: 0 !important; }
        .collapse-toggle { display: inline-flex !important; }
      "))
    }
  })

  output$back_to_landing_ui <- shiny::renderUI({
    if (!identical(app_stage(), "workflow")) {
      return(NULL)
    }

    shiny::actionButton(
      "back_to_landing",
      label = "\u2190",
      class = "back-arrow-btn",
      title = "Back to landing page"
    )
  })

  output$header_controls_ui <- shiny::renderUI({
    if (identical(app_stage(), "landing")) {
      return(shiny::div())
    }

    shiny::div(
      class = "header-controls",
      shiny::div(
        class = "parser-header-control",
        shiny::div(class = "parser-label", "Parser Version"),
        shiny::selectInput(
          "parser_version",
          label = NULL,
          selected = "v1.0.0",
          choices = c(
            "v1.0.0" = "v1.0.0",
            "v1.1.0 (in development)" = "v1.1.0_dev"
          ),
          width = "120px"
        )
      ),
      shiny::div(
        class = "parser-header-control",
        shiny::div(class = "parser-label", "Uploader Version"),
        shiny::selectInput(
          "workflow_uploader_version",
          label = NULL,
          selected = selected_uploader_version(),
          choices = c(
            "v1.0.0" = "v1.0.0",
            "v1.1.0" = "v1.1.0"
          ),
          width = "120px"
        )
      ),
      shiny::div(
        class = "header-refresh-control",
        bslib::accordion(
          id = "refreshing_app_header_accordion",
          open = FALSE,
          bslib::accordion_panel(
            title = "Refreshing App",
            shiny::div(
              class = "button-stack",
              shiny::actionButton("clear_manifest", "Clear PDF Manifest", class = "btn-outline-warning"),
              shiny::actionButton("clear_output", "Clear Output Folder", class = "btn-outline-danger"),
              shiny::actionButton("clear_cache", "Clear Cache", class = "btn-outline-secondary")
            ),
            shiny::tags$div(
              class = "workflow-step",
              "Use these actions to reset manifests, generated outputs, and temporary in-app state."
            )
          )
        )
      )
    )
  })

  output$landing_uploader_message <- shiny::renderUI({
    if (identical(safe_input_value(input$landing_uploader_version, "v1.0.0"), "v1.1.0")) {
      shiny::div(
        class = "landing-note",
        "Uploader v1.1.0 enables the REDCap append-mapping workflow for projects where parser variables must be mapped into an existing REDCap structure."
      )
    } else {
      shiny::div(
        class = "landing-note",
        "Uploader v1.0.0 uses the parser-ready REDCap structure directly and does not expose the REDCap append-mapping workflow."
      )
    }
  })

  output$landing_keyring_status_ui <- shiny::renderUI({
    keyring_status <- keyring_status_state()

    shiny::div(
      class = "landing-note",
      shiny::tags$strong("Local REDCap keyring status: "),
      keyring_status$message
    )
  })

  output$github_import_inline_status_ui <- shiny::renderUI({
    import_result <- github_import_state()
    if (is.null(import_result)) {
      return(NULL)
    }

    shiny::div(
      class = "inline-import-status",
      sprintf(
        "%s record%s imported and ready to run.",
        import_result$imported_n,
        if (identical(import_result$imported_n, 1L)) "" else "s"
      )
    )
  })

  output$workflow_steps_ui <- shiny::renderUI({
    workflow_steps <- if (identical(selected_uploader_version(), "v1.1.0")) {
      c(
        "1. Import VITEK PDF reports from local files or GitHub.",
        "2. Parse and structure AST results.",
        "3. Validate records and separate flagged rows.",
        "4. Load a REDCap dictionary and generate append mapping suggestions.",
        "5. Review, save, or load the active append mapping.",
        "6. Prepare the append trial and append-safe payload.",
        "7. Append validated rows into the existing REDCap project."
      )
    } else {
      c(
        "1. Import VITEK PDF reports from local files or GitHub.",
        "2. Parse and structure AST results.",
        "3. Normalize patient-family IDs and isolate rows for upload.",
        "4. Validate records and separate flagged rows.",
        "5. Review upload-ready data and review-queue rows.",
        "6. Upload validated rows directly into REDCap."
      )
    }

    shiny::tagList(
      shiny::tags$h4(
        class = "workflow-steps-title",
        paste("Workflow", paste0("(", selected_uploader_version(), ")"))
      ),
      lapply(workflow_steps, function(step_text) {
        shiny::tags$div(class = "workflow-step", step_text)
      })
    )
  })

  shiny::observeEvent(input$open_package_summary, {
    shiny::showModal(
      shiny::modalDialog(
        title = "About This Package",
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::downloadButton("download_package_summary_pdf", "Download PDF", class = "btn btn-outline-primary btn-sm"),
          shiny::modalButton("Close")
        ),
        shiny::tags$p(
          "A compact Shiny workflow for importing VITEK PDF reports, extracting AST results, validating them, and uploading into REDCap through either direct or append-safe uploader paths."
        )
      )
    )
  })

  shiny::observeEvent(input$open_contact_developer, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Contact Developer",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::div(
          class = "contact-option-list",
          shiny::div(
            class = "contact-option-item",
            shiny::div(
              class = "contact-option-head",
              shiny::span(class = "contact-option-icon", HTML("&#9993;")),
              shiny::div(class = "contact-option-label", "Mail")
            ),
            shiny::tags$a(
              href = "mailto:adewemimocharles@gmail.com",
              "adewemimocharles@gmail.com"
            )
          ),
          shiny::div(
            class = "contact-option-item",
            shiny::div(
              class = "contact-option-head",
              shiny::span(class = "contact-option-icon", HTML("&#128247;")),
              shiny::div(class = "contact-option-label", "Instagram")
            ),
            shiny::div(class = "contact-option-muted", "In progress")
          ),
          shiny::div(
            class = "contact-option-item",
            shiny::div(
              class = "contact-option-head",
              shiny::span(class = "contact-option-icon", "X"),
              shiny::div(class = "contact-option-label", "X")
            ),
            shiny::div(class = "contact-option-muted", "In progress")
          ),
          shiny::div(
            class = "contact-option-item",
            shiny::div(
              class = "contact-option-head",
              shiny::span(class = "contact-option-icon", HTML("&#9742;")),
              shiny::div(class = "contact-option-label", "Phone")
            ),
            shiny::div(class = "contact-option-muted", "In progress")
          )
        )
      )
    )
  })

  perform_upload_action <- function(mode = c("standard", "append")) {
    mode <- match.arg(mode)

    shiny::req(pipeline_state())
    current_cfg <- redcap_config_state()
    redcap_config_state(current_cfg)

    progress_message <- if (identical(mode, "append")) {
      "Preparing append upload"
    } else {
      "Uploading records"
    }

    upload_result <- shiny::withProgress(message = progress_message, value = 0, {
      shiny::incProgress(0.3, detail = "Validating upload-ready rows")
      result <- upload_redcap_dispatcher(
        uploader_version = selected_uploader_version(),
        redcap_df = pipeline_state()$upload_ready,
        config_file = redcap_config_file(),
        mapping_file = active_append_mapping_file(),
        redcap_uri = current_cfg$redcap_uri,
        keyring_service = current_cfg$keyring_service,
        keyring_username = current_cfg$keyring_username,
        required_fields = pipeline_state()$settings$upload_required_fields,
        key_fields = pipeline_state()$settings$upload_key_fields,
        marker_tests = pipeline_state()$settings$marker_tests,
        parser_version = pipeline_state()$settings$parser_version,
        dry_run = input$dry_run_upload,
        output_dir = pipeline_state()$run_output_dir,
        batch_size = pipeline_state()$settings$batch_size_upload,
        quiet = FALSE
      )
      shiny::incProgress(0.7, detail = "Writing audit log")
      result
    })

    audit_stage <- if (identical(mode, "append")) "append_redcap" else "upload_redcap"
    audit_detail <- if (identical(mode, "append")) {
      append_summary <- append_upload_audit_summary(upload_result)
      sprintf(
        paste(
          "Append action invoked for %s upload-ready row(s).",
          "Duplicate final-key groups identified: %s.",
          "Duplicate payload rows identified: %s.",
          "Rows merged away: %s.",
          "Final append rows ready: %s.",
          "Final rows uploaded: %s."
        ),
        nrow(pipeline_state()$upload_ready),
        append_summary$duplicate_key_groups_identified,
        append_summary$duplicate_payload_rows_identified,
        append_summary$duplicate_rows_merged,
        append_summary$final_append_rows_ready,
        append_summary$final_rows_uploaded
      )
    } else {
      sprintf("Upload invoked for %s row(s).", nrow(pipeline_state()$upload_ready))
    }

    append_audit_log(
      log_file = pipeline_state()$audit_log_file,
      stage = audit_stage,
      status = upload_result$status,
      details = audit_detail
    )

    upload_state(upload_result)

    if (identical(mode, "append") && !is.null(upload_result$trial_setup)) {
      append_message <- paste("Append action finished with status:", upload_result$status)
      if (isTRUE(upload_result$trial_setup$trial_ready)) {
        shiny::showNotification(
          append_message,
          type = "message"
        )
      } else {
        shiny::showNotification(
          append_message,
          type = "warning"
        )
      }

      if (!is.null(upload_result$warning_message) && nzchar(upload_result$warning_message)) {
        shiny::showNotification(
          upload_result$warning_message,
          type = "warning",
          duration = NULL
        )
      }
    } else {
      shiny::showNotification(
        paste("Upload finished with status:", upload_result$status),
        type = if (grepl("success|completed|ready", tolower(upload_result$status))) "message" else "warning"
      )
    }
  }

  output$workflow_main_ui <- shiny::renderUI({
    nav_items <- list(
      bslib::nav_panel(
        "Preview",
        shiny::div(
          class = "section-card",
          shiny::div(class = "section-title", "Structured Dataset Preview"),
          shiny::div(class = "section-subtitle", "Parsed records ready for mapping and validation."),
          DT::DTOutput("preview_table")
        )
      ),
      bslib::nav_panel(
        "Validation Summary",
        shiny::div(
          class = "section-card",
          shiny::div(class = "section-title", "Validation Metrics"),
          shiny::div(class = "section-subtitle", "High-level readiness checks for REDCap upload."),
          DT::DTOutput("validation_table")
        )
      ),
      bslib::nav_panel(
        "Review Queue",
        shiny::div(
          class = "section-card",
          shiny::div(class = "section-title", "Flagged Records"),
          shiny::div(class = "section-subtitle", "Rows that need manual review before upload."),
          DT::DTOutput("review_table")
        )
      ),
      bslib::nav_panel(
        "Audit Log",
        shiny::div(
          class = "section-card",
          shiny::div(class = "section-title", "Audit Trail"),
          shiny::div(class = "section-subtitle", "Pipeline and upload events for the current run."),
          DT::DTOutput("audit_table")
        )
      )
    )

    if (identical(selected_uploader_version(), "v1.1.0")) {
      nav_items[[length(nav_items) + 1]] <- bslib::nav_panel(
        "Append Mapping",
        shiny::div(
          class = "section-card",
          shiny::div(class = "section-title", "Append Mapping Suggestions"),
          shiny::div(
            class = "section-subtitle",
            "Upload a REDCap data dictionary in the configuration panel to suggest target fields for the fixed source_name list."
          ),
          shiny::div(
            class = "table-action-bar",
            shiny::textInput("accepted_mapping_name", NULL, value = "", placeholder = "Save accepted mapping as"),
            shiny::actionButton("accept_suggested_mapping", "Accept Suggested Mapping", class = "btn-outline-success")
          ),
          DT::DTOutput("append_mapping_table")
        )
      )
    }

    shiny::tagList(
      bslib::layout_column_wrap(
        width = 1/4,
        class = "metric-grid",
        metric_card("Files Uploaded", "files_metric", "teal"),
        metric_card("Parsed Rows", "parsed_metric", "sage"),
        metric_card("Flagged Rows", "flagged_metric", "amber"),
        metric_card("Upload-Ready", "ready_metric", "terracotta")
      ),
      shiny::div(
        class = "status-banner",
        shiny::uiOutput("status_banner")
      ),
      do.call(bslib::navset_card_tab, c(list(full_screen = TRUE), nav_items))
    )
  })

  shiny::observeEvent(input$enter_workflow, {
    selected_uploader_version(safe_input_value(input$landing_uploader_version, "v1.0.0"))
    app_stage("workflow")
    pipeline_state(NULL)
    upload_state(NULL)
    github_import_state(NULL)
    append_mapping_state(NULL)
    active_append_mapping_file(file.path("docs", "redcap_append_variable_mapping.csv"))
  })

  show_keyring_setup_modal <- function() {
    keyring_status <- keyring_status_state()
    current_cfg <- redcap_config_state()

    shiny::showModal(
      shiny::modalDialog(
        title = "Set Up REDCap Keyring",
        shiny::tags$p(
          "This stores your REDCap API token securely in your computer's local keyring and saves the non-secret REDCap configuration for this app."
        ),
        shiny::tags$p(
          "The setup stays on this computer until you remove or replace the saved keyring credential."
        ),
        shiny::div(
          class = "workflow-step",
          keyring_status$message
        ),
        shiny::textInput(
          "landing_redcap_uri",
          "REDCap API URL",
          value = safe_input_value(current_cfg$redcap_uri, keyring_status$redcap_uri)
        ),
        shiny::textInput(
          "landing_keyring_service",
          "Keyring Service",
          value = safe_input_value(current_cfg$keyring_service, if (nzchar(keyring_status$keyring_service)) keyring_status$keyring_service else "redcap_api")
        ),
        shiny::textInput(
          "landing_keyring_username",
          "Keyring Username",
          value = safe_input_value(current_cfg$keyring_username, keyring_status$keyring_username)
        ),
        shiny::passwordInput(
          "landing_api_token",
          "REDCap API Token",
          value = ""
        ),
        shiny::tags$div(
          class = "workflow-step",
          "Use Save And Test to store the token in the local keyring, save the REDCap configuration file, and confirm the credential can be read back on this computer. Use Save And Exit if you want to store the setup now and continue without the extra test step."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton("clear_existing_keyring_setup", "Clear Existing Setup", class = "btn-outline-danger"),
          shiny::actionButton("test_existing_keyring_setup", "Test Existing Setup", class = "btn-outline-dark"),
          shiny::actionButton("save_and_exit_keyring_setup", "Save And Exit", class = "btn-outline-success"),
          shiny::actionButton("save_and_test_keyring_setup", "Save And Test Keyring", class = "btn-success")
        ),
        easyClose = TRUE
      )
    )
  }

  shiny::observeEvent(input$open_keyring_setup, {
    show_keyring_setup_modal()
  })

  shiny::observeEvent(input$test_existing_keyring_setup, {
    keyring_status <- check_redcap_keyring_status(config_file = redcap_config_file())
    keyring_status_state(keyring_status)

    if (isTRUE(keyring_status$configured)) {
      current_cfg <- list(
        redcap_uri = keyring_status$redcap_uri,
        keyring_service = keyring_status$keyring_service,
        keyring_username = keyring_status$keyring_username
      )
      redcap_config_state(current_cfg)
      shiny::updateTextInput(session, "redcap_uri", value = current_cfg$redcap_uri)
      shiny::updateTextInput(session, "keyring_service", value = current_cfg$keyring_service)
      shiny::updateTextInput(session, "keyring_username", value = current_cfg$keyring_username)
      shiny::showNotification("Local REDCap keyring setup is ready on this computer.", type = "message")
    } else {
      shiny::showNotification(keyring_status$message, type = "warning")
    }

    show_keyring_setup_modal()
  })

  shiny::observeEvent(input$save_and_test_keyring_setup, {
    setup_result <- tryCatch(
      save_redcap_keyring_setup(
        redcap_uri = input$landing_redcap_uri,
        keyring_service = input$landing_keyring_service,
        keyring_username = input$landing_keyring_username,
        api_token = input$landing_api_token,
        config_file = redcap_config_file()
      ),
      error = function(e) e
    )

    if (inherits(setup_result, "error")) {
      shiny::showNotification(setup_result$message, type = "error")
      return(invisible(NULL))
    }

    redcap_config_state(setup_result$config)
    keyring_status_state(setup_result$status)

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "setup_redcap_keyring",
      status = "success",
      details = sprintf(
        "REDCap keyring setup saved locally for %s (%s).",
        setup_result$config$keyring_username,
        setup_result$config$keyring_service
      )
    )

    shiny::removeModal()
    shiny::showNotification(
      "REDCap keyring setup was saved and tested successfully for this computer.",
      type = "message"
    )
  })

  shiny::observeEvent(input$save_and_exit_keyring_setup, {
    setup_result <- tryCatch(
      save_redcap_keyring_setup(
        redcap_uri = input$landing_redcap_uri,
        keyring_service = input$landing_keyring_service,
        keyring_username = input$landing_keyring_username,
        api_token = input$landing_api_token,
        config_file = redcap_config_file()
      ),
      error = function(e) e
    )

    if (inherits(setup_result, "error")) {
      shiny::showNotification(setup_result$message, type = "error")
      return(invisible(NULL))
    }

    redcap_config_state(setup_result$config)
    keyring_status_state(check_redcap_keyring_status(config_file = redcap_config_file()))

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "save_redcap_keyring_setup",
      status = "success",
      details = sprintf(
        "REDCap keyring setup was saved locally for %s (%s) without running the extra verification step.",
        setup_result$config$keyring_username,
        setup_result$config$keyring_service
      )
    )

    shiny::removeModal()
    shiny::showNotification(
      "REDCap keyring setup was saved for this computer.",
      type = "message"
    )
  })

  shiny::observeEvent(input$clear_existing_keyring_setup, {
    cleared_status <- clear_redcap_keyring_setup(config_file = redcap_config_file())

    keyring_status_state(cleared_status)
    redcap_config_state(list(
      redcap_uri = "",
      keyring_service = "",
      keyring_username = ""
    ))
    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "clear_redcap_keyring_setup",
      status = "success",
      details = "Existing REDCap keyring setup was cleared from this computer."
    )

    shiny::showNotification(
      "Existing REDCap keyring setup was cleared from this computer.",
      type = "message"
    )

    show_keyring_setup_modal()
  })

  shiny::observeEvent(input$workflow_uploader_version, {
    shiny::req(identical(app_stage(), "workflow"))
    new_version <- safe_input_value(input$workflow_uploader_version, selected_uploader_version())

    if (identical(new_version, selected_uploader_version())) {
      return(invisible(NULL))
    }

    selected_uploader_version(new_version)
    upload_state(NULL)
    append_mapping_state(NULL)
    active_append_mapping_file(file.path("docs", "redcap_append_variable_mapping.csv"))

    shiny::showNotification(
      sprintf("Uploader switched to %s.", new_version),
      type = "message"
    )
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$change_uploader_version, {
    app_stage("landing")
    pipeline_state(NULL)
    upload_state(NULL)
    github_import_state(NULL)
    append_mapping_state(NULL)
    active_append_mapping_file(file.path("docs", "redcap_append_variable_mapping.csv"))
  })

  shiny::observeEvent(input$back_to_landing, {
    app_stage("landing")
    pipeline_state(NULL)
    upload_state(NULL)
    github_import_state(NULL)
    append_mapping_state(NULL)
    active_append_mapping_file(file.path("docs", "redcap_append_variable_mapping.csv"))
  })

  shiny::observeEvent(input$parser_version, {
    if (identical(input$parser_version, "v1.1.0_dev")) {
      shiny::updateSelectInput(session, "parser_version", selected = "v1.0.0")
      shiny::showNotification("Parser v1.1.0 is still in development and cannot be selected yet.", type = "warning")
    }
  }, ignoreInit = TRUE)

  clear_directory_contents <- function(dir_path, exclude = character()) {
    if (!dir.exists(dir_path)) {
      return(invisible(NULL))
    }

    targets <- list.files(dir_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(exclude) > 0) {
      targets <- targets[!basename(targets) %in% exclude]
    }

    if (length(targets) > 0) {
      unlink(targets, recursive = TRUE, force = TRUE)
    }

    invisible(NULL)
  }

  shiny::observeEvent(input$run_pipeline, {
    uploaded_paths <- character()
    uploaded_names <- character()

    if (!is.null(input$pdf_files)) {
      uploaded_paths <- input$pdf_files$datapath
      uploaded_names <- input$pdf_files$name
    }

    if (!is.null(github_import_state())) {
      uploaded_paths <- c(uploaded_paths, github_import_state()$imported_files)
      uploaded_names <- c(uploaded_names, basename(github_import_state()$imported_files))
    }

    shiny::req(length(uploaded_paths) > 0)

    result <- tryCatch(
      shiny::withProgress(message = "Running pipeline", value = 0, {
        shiny::incProgress(0.2, detail = "Reading uploaded files")
        result <- run_shiny_pipeline(
          uploaded_paths = uploaded_paths,
          uploaded_names = uploaded_names,
          parser_version = input$parser_version
        )
        shiny::incProgress(0.8, detail = "Preparing outputs")
        result
      }),
      error = function(e) {
        append_audit_log(
          log_file = app_audit_log_file(),
          stage = "run_pipeline",
          status = "error",
          details = conditionMessage(e)
        )
        shiny::showNotification(
          conditionMessage(e),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )

    if (is.null(result)) {
      return(invisible(NULL))
    }

    pipeline_state(result)
    upload_state(NULL)

    if (nrow(result$flagged_rows) > 0) {
      review_hint <- if (!is.null(result$review_file) && !is.na(result$review_file) && nzchar(result$review_file)) {
        paste0(" Review file: ", result$review_file)
      } else {
        ""
      }
      shiny::showNotification(
        sprintf(
          "Pipeline completed with %s upload-ready row(s) and %s flagged row(s). Check the Review Queue tab.%s",
          nrow(result$upload_ready),
          nrow(result$flagged_rows),
          review_hint
        ),
        type = "warning",
        duration = NULL
      )
    } else {
      shiny::showNotification("Pipeline completed successfully.", type = "message")
    }
  })

  shiny::observeEvent(input$import_github, {
    shiny::req(input$github_repo_url)

    import_result <- shiny::withProgress(message = "Importing from GitHub", value = 0, {
      shiny::incProgress(0.3, detail = "Downloading repository archive")
      result <- import_github_data_raw(
        repo_url = input$github_repo_url,
        dest_dir = "data_raw",
        quiet = TRUE
      )
      shiny::incProgress(0.7, detail = "Copying PDF files into data_raw")
      result
    })

    github_import_state(import_result)

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "import_github_data_raw",
      status = "success",
      details = sprintf(
        "Imported %s PDF file(s) from %s/%s on branch %s using %s.",
        import_result$imported_n,
        import_result$owner,
        import_result$repo,
        import_result$branch,
        import_result$method
      )
    )

    shiny::showNotification(
      sprintf(
        "Imported %s PDF file(s) from GitHub into data_raw using %s.",
        import_result$imported_n,
        import_result$method
      ),
      type = "message"
    )
  })

  shiny::observeEvent(input$suggest_append_mapping, {
    shiny::req(input$redcap_dictionary_file)

    suggestion_result <- shiny::withProgress(message = "Generating append mapping suggestions", value = 0, {
      shiny::incProgress(0.3, detail = "Reading REDCap data dictionary")
      result <- update_append_mapping_targets(
        append_mapping_file = active_append_mapping_file(),
        dictionary_file = input$redcap_dictionary_file$datapath,
        output_file = app_templates_dir(paste0("redcap_append_variable_mapping_suggested_", timestamp_slug(), ".csv")),
        overwrite_existing = FALSE,
        quiet = TRUE
      )
      shiny::incProgress(0.8, detail = "Preparing mapping preview")
      result
    })

    append_mapping_state(suggestion_result)

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "suggest_append_mapping",
      status = "success",
      details = sprintf(
        "Generated append mapping suggestions for %s source field(s) using %s.",
        nrow(suggestion_result$updated_mapping),
        input$redcap_dictionary_file$name
      )
    )

    shiny::showNotification("Append mapping suggestions generated.", type = "message")
  })

  shiny::observeEvent(input$mapped_append_mapping_file, {
    shiny::req(input$mapped_append_mapping_file)

    uploaded_mapping <- load_append_redcap_mapping(
      mapping_file = input$mapped_append_mapping_file$datapath,
      quiet = TRUE
    )

    saved_mapping_path <- app_templates_dir(
      paste0("redcap_append_variable_mapping_uploaded_", timestamp_slug(), ".csv")
    )
    file.copy(input$mapped_append_mapping_file$datapath, saved_mapping_path, overwrite = TRUE)

    active_append_mapping_file(saved_mapping_path)
    append_mapping_state(list(
      updated_mapping = uploaded_mapping,
      suggestions = NULL,
      output_file = saved_mapping_path,
      source = "uploaded_mapping"
    ))

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "upload_append_mapping_file",
      status = "success",
      details = sprintf(
        "Uploaded mapped append mapping file %s.",
        basename(saved_mapping_path)
      )
    )

    shiny::showNotification("Mapped append mapping uploaded and set as the active mapping.", type = "message")
  })

  shiny::observeEvent(input$use_saved_mapping, {
    shiny::req(input$saved_mapping_choice)

    selected_mapping <- input$saved_mapping_choice
    shiny::req(file.exists(selected_mapping))

    loaded_mapping <- load_append_redcap_mapping(
      mapping_file = selected_mapping,
      quiet = TRUE
    )

    active_append_mapping_file(selected_mapping)
    append_mapping_state(list(
      updated_mapping = loaded_mapping,
      suggestions = NULL,
      output_file = selected_mapping,
      source = "saved_mapping"
    ))

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "use_saved_mapping",
      status = "success",
      details = sprintf(
        "Activated saved generated mapping file %s.",
        basename(selected_mapping)
      )
    )

    shiny::showNotification("Saved generated mapping loaded as the active mapping.", type = "message")
  })

  shiny::observeEvent(input$accept_suggested_mapping, {
    shiny::req(append_mapping_state())
    shiny::req(!is.null(append_mapping_state()$updated_mapping))
    if (!("suggested_target_redcap_name" %in% names(append_mapping_state()$updated_mapping))) {
      shiny::showNotification("Generate mapping suggestions first before accepting them.", type = "warning")
      return(invisible(NULL))
    }

    accepted_mapping <- append_mapping_state()$updated_mapping |>
      dplyr::mutate(
        target_redcap_name = dplyr::coalesce(
          dplyr::na_if(.data$suggested_target_redcap_name, ""),
          .data$target_redcap_name
        )
      )

    accepted_mapping_name <- sanitize_mapping_name(
      input$accepted_mapping_name,
      default = "accepted_mapping"
    )

    accepted_mapping_path <- saved_mappings_dir(
      paste0("redcap_append_variable_mapping_", accepted_mapping_name, "_", timestamp_slug(), ".csv")
    )
    readr::write_csv(accepted_mapping, accepted_mapping_path)

    active_append_mapping_file(accepted_mapping_path)
    append_mapping_state(list(
      updated_mapping = accepted_mapping,
      suggestions = append_mapping_state()$suggestions,
      output_file = accepted_mapping_path,
      source = "accepted_mapping"
    ))

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "accept_suggested_mapping",
      status = "success",
      details = sprintf(
        "Accepted suggested append mapping into persistent saved mapping %s.",
        basename(accepted_mapping_path)
      )
    )

    shiny::updateTextInput(session, "accepted_mapping_name", value = "")
    shiny::showNotification("Suggested mapping accepted and set as the active mapping for upload.", type = "message")
  })

  shiny::observeEvent(input$run_upload, {
    if (identical(selected_uploader_version(), "v1.1.0")) {
      perform_upload_action("append")
    } else {
      perform_upload_action("standard")
    }
  })

  shiny::observeEvent(input$run_append_upload, {
    shiny::req(identical(selected_uploader_version(), "v1.1.0"))
    perform_upload_action("append")
  })

  shiny::observeEvent(input$clear_manifest, {
    manifest_file <- app_cache_dir("pdf_manifest.csv")

    readr::write_csv(
      tibble::tibble(
        file_path = character(),
        file_name = character(),
        processed = logical()
      ),
      manifest_file
    )

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "clear_manifest",
      status = "success",
      details = "pdf_manifest.csv was cleared from the Shiny dashboard."
    )

    shiny::showNotification("PDF manifest cleared.", type = "message")
  })

  shiny::observeEvent(input$clear_output, {
    clear_directory_contents(app_runs_dir())

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "clear_output",
      status = "success",
      details = "Run output folders were cleared from the Shiny dashboard."
    )

    shiny::showNotification("Run output folders cleared.", type = "message")
  })

  shiny::observeEvent(input$clear_cache, {
    clear_directory_contents(app_cache_dir())
    clear_directory_contents(app_runs_dir())
    clear_directory_contents(app_templates_dir())
    if (dir.exists("data_raw")) {
      pdf_files <- list.files(
        "data_raw",
        pattern = "[.]pdf$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )
      if (length(pdf_files) > 0) {
        file.remove(pdf_files)
      }
    }
    pipeline_state(NULL)
    upload_state(NULL)
    github_import_state(NULL)
    append_mapping_state(NULL)
    active_append_mapping_file(file.path("docs", "redcap_append_variable_mapping.csv"))

    append_audit_log(
      log_file = app_audit_log_file(),
      stage = "clear_cache",
      status = "success",
      details = "Temporary app state, cache, generated run folders, temporary output mappings, and PDF files in data_raw were cleared from the Shiny dashboard. Persistent saved mappings in docs/saved_mappings were preserved."
    )

    shiny::showNotification("App cache, generated runs, temporary output mappings, data_raw PDF files, and in-session state cleared. Persistent saved mappings were preserved.", type = "message")
  })

output$download_dictionary <- shiny::downloadHandler(
  filename = function() {
    paste0("Extract_DataDictionary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
    content = function(file) {
      src <- here::here("docs", "Extract_DataDictionary.csv")
      if (!file.exists(src)) {
        stop("Data dictionary template not found in docs/Extract_DataDictionary.csv")
      }
      file.copy(src, file, overwrite = TRUE)
    }
  )

output$download_append_mapping <- shiny::downloadHandler(
  filename = function() {
    paste0("redcap_append_variable_mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  },
    content = function(file) {
      src <- here::here("docs", "redcap_append_variable_mapping.csv")
      if (!file.exists(src)) {
        stop("Append mapping template not found in docs/redcap_append_variable_mapping.csv")
      }
      file.copy(src, file, overwrite = TRUE)
    }
  )

output$download_package_summary_pdf <- shiny::downloadHandler(
  filename = function() {
    paste0("VITEK_EXTRACT_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
  },
  content = function(file) {
    grDevices::pdf(file, width = 8.27, height = 11.69)
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
      graphics::par(old_par)
      grDevices::dev.off()
    }, add = TRUE)

    init_summary_page <- function() {
      graphics::par(mar = c(0.6, 0.6, 0.6, 0.6))
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
    }

    draw_wrapped_text <- function(text, x, y, width = 78, cex = 1, font = 1, col = "#1a2620", line_height = 0.034) {
      wrapped <- strwrap(text, width = width)
      if (length(wrapped) == 0) {
        return(y)
      }
      for (line in wrapped) {
        graphics::text(x = x, y = y, labels = line, adj = c(0, 1), cex = cex, font = font, col = col, family = "sans")
        y <- y - line_height
      }
      y
    }

    ensure_page_space <- function(y, needed = 0.08, reset_y = 0.95) {
      if (y <= needed) {
        init_summary_page()
        return(reset_y)
      }
      y
    }

    sections <- list(
      list(
        heading = "Executive Summary",
        body = "This package is an R and Shiny application for turning VITEK antimicrobial susceptibility testing PDF reports into structured, reviewable, and REDCap-ready microbiology data. It supports an end-to-end workflow from PDF intake through extraction, parsing, cleaning, validation, audit logging, and upload."
      ),
      list(
        heading = "What The Application Does",
        body = "The app can import VITEK PDFs from local uploads or a GitHub repository, extract raw report text, parse organism identification and AST results, reshape those results into structured datasets, validate records, separate review cases, and prepare upload-ready REDCap payloads."
      ),
      list(
        heading = "Uploader Options",
        body = c(
          "Uploader v1.0.0 is designed for REDCap projects that already follow the parser-ready structure expected by this package. It uses the existing parser schema directly and is the simpler route when destination field names and layout are already aligned with the package output.",
          "Uploader v1.1.0 is designed for append-safe REDCap workflows where the destination project has its own naming conventions and existing variables. It supports REDCap dictionary-assisted mapping, saved mapping files, append trials, and mapped partial updates so that only selected destination fields are written while unrelated REDCap fields are preserved."
        )
      ),
      list(
        heading = "Outputs And Audit Trail",
        body = "Each run creates structured outputs that help with traceability and review. Parsed files, upload-ready datasets, review files, append payloads, upload logs, and audit logs are saved into timestamped output folders so each processing run can be traced independently. Reusable generated mapping files are saved separately for later reuse."
      ),
      list(
        heading = "In Development",
        body = "Parser v1.1.0 is still in development. The goal of this next parser version is to expand coverage so the package can recognize and structure a broader set of antibiotics and related AST content than parser v1.0.0 currently handles."
      ),
      list(
        heading = "Thank You",
        body = "Thank you for using this application. Please do not hesitate to reach out with suggestions to further improve the package, or if you would like to contribute, support, or collaborate. The contact options are available in the Contact Me menu inside the app."
      )
    )

    init_summary_page()

    y <- 0.965
    graphics::text(x = 0.05, y = y, labels = "VITEK AST Extraction and REDCap Upload", adj = c(0, 1), cex = 1.55, font = 2, col = "#1a2620", family = "sans")
    y <- y - 0.045
    graphics::text(x = 0.05, y = y, labels = "Microbiology pipeline package overview", adj = c(0, 1), cex = 0.95, font = 1, col = "#6b5344", family = "sans")
    y <- y - 0.04

    for (section in sections) {
      y <- ensure_page_space(y, needed = 0.12)
      y <- y - 0.01
      graphics::text(x = 0.05, y = y, labels = section$heading, adj = c(0, 1), cex = 1.05, font = 2, col = "#2a5240", family = "sans")
      y <- y - 0.028
      for (paragraph in section$body) {
        paragraph_lines <- strwrap(paragraph, width = 88)
        needed_height <- max(0.08, length(paragraph_lines) * 0.029 + 0.03)
        y <- ensure_page_space(y, needed = needed_height)
        y <- draw_wrapped_text(paragraph, x = 0.05, y = y, width = 88, cex = 0.92, font = 1, col = "#1a2620", line_height = 0.029)
        y <- y - 0.016
      }
      y <- y - 0.004
    }

    y <- ensure_page_space(y, needed = 0.10)
    y <- y - 0.005
    graphics::segments(x0 = 0.05, y0 = y, x1 = 0.95, y1 = y, col = "#c9b8a4", lwd = 1)
    y <- y - 0.026
    graphics::text(x = 0.05, y = y, labels = "BioParseR.com", adj = c(0, 1), cex = 0.98, font = 2, col = "#2a5240", family = "sans")
    y <- y - 0.025
    graphics::text(x = 0.05, y = y, labels = "https://BioParseR.com", adj = c(0, 1), cex = 0.88, font = 1, col = "#2a5c93", family = "sans")
  }
)

  output$status_text <- shiny::renderText({
    pipeline <- pipeline_state()
    upload <- upload_state()
    github_import <- github_import_state()
    current_cfg <- redcap_config_state()

    if (is.null(pipeline)) {
      lines <- "Upload one or more PDF reports, import PDFs from GitHub, then click Run Pipeline."
      if (!is.null(github_import)) {
        lines <- c(
          lines,
          sprintf(
            "Latest GitHub import: %s file(s) from %s/%s via %s",
            github_import$imported_n,
            github_import$owner,
            github_import$repo,
            github_import$method
          )
        )
      }
      return(paste(lines, collapse = "\n"))
    }

    status_lines <- c(
      sprintf("Files selected: %s", length(pipeline$input_files)),
      sprintf("Parser version: %s", pipeline$settings$parser_version),
      sprintf("Uploader version: %s", selected_uploader_version()),
      sprintf("Active mapping file: %s", basename(active_append_mapping_file())),
      sprintf("Parsed rows: %s", nrow(pipeline$parsed_wide)),
      sprintf("Upload-ready rows: %s", nrow(pipeline$upload_ready)),
      sprintf("Flagged rows: %s", nrow(pipeline$flagged_rows)),
      sprintf("Run output: %s", pipeline$run_output_dir)
    )

    if (!is.null(pipeline$review_file) && !is.na(pipeline$review_file) && nzchar(pipeline$review_file)) {
      status_lines <- c(status_lines, sprintf("Review file: %s", pipeline$review_file))
    }

    if (!is.null(upload)) {
      status_lines <- c(status_lines, sprintf("Upload status: %s", upload$status))
      if (is_append_upload_result(upload)) {
        append_summary <- append_upload_audit_summary(upload)
        status_lines <- c(
          status_lines,
          sprintf("Duplicate final-key groups identified: %s", append_summary$duplicate_key_groups_identified),
          sprintf("Duplicate payload rows identified: %s", append_summary$duplicate_payload_rows_identified),
          sprintf("Rows merged away before append write: %s", append_summary$duplicate_rows_merged),
          sprintf("Final append rows ready: %s", append_summary$final_append_rows_ready),
          sprintf("Final rows uploaded: %s", append_summary$final_rows_uploaded)
        )
      }
      if (!is.null(upload$warning_message) && nzchar(upload$warning_message)) {
        status_lines <- c(status_lines, sprintf("Upload warning: %s", upload$warning_message))
      }
    }

    if (!is.null(github_import)) {
      status_lines <- c(
        status_lines,
        sprintf(
          "Latest GitHub import: %s file(s) from %s/%s via %s",
          github_import$imported_n,
          github_import$owner,
          github_import$repo,
          github_import$method
        )
      )
    }

    status_lines <- c(
      status_lines,
      sprintf("REDCap target: %s", if (nzchar(current_cfg$redcap_uri)) current_cfg$redcap_uri else "Not set")
    )

    paste(status_lines, collapse = "\n")
  })

  output$files_metric <- shiny::renderText({
    pipeline <- pipeline_state()
    if (is.null(pipeline)) "0" else as.character(length(pipeline$input_files))
  })

  output$parsed_metric <- shiny::renderText({
    pipeline <- pipeline_state()
    if (is.null(pipeline)) "0" else as.character(nrow(pipeline$parsed_wide))
  })

  output$flagged_metric <- shiny::renderText({
    pipeline <- pipeline_state()
    if (is.null(pipeline)) "0" else as.character(nrow(pipeline$flagged_rows))
  })

  output$ready_metric <- shiny::renderText({
    pipeline <- pipeline_state()
    if (is.null(pipeline)) "0" else as.character(nrow(pipeline$upload_ready))
  })

  output$status_banner <- shiny::renderUI({
    pipeline <- pipeline_state()
    upload <- upload_state()
    github_import <- github_import_state()

    if (is.null(pipeline)) {
      body_ui <- shiny::tagList(
        shiny::div(class = "status-line", shiny::strong("Ready to start")),
        shiny::div(class = "status-line", "Upload one or more PDF reports, or import PDFs from GitHub, then run the pipeline."),
        shiny::div(class = "status-line", shiny::strong("Active parser version:"), paste(" ", input$parser_version)),
        shiny::div(class = "status-line", shiny::strong("Active uploader version:"), paste(" ", selected_uploader_version()))
      )

      if (!is.null(github_import)) {
        body_ui <- shiny::tagList(
          body_ui,
          shiny::div(
            class = "status-line",
            shiny::strong("Latest GitHub import:"),
            paste(" ", github_import$imported_n, "file(s) from", paste0(github_import$owner, "/", github_import$repo), "via", github_import$method)
          )
        )
      }

      return(shiny::tagList(
        shiny::div(
          class = "status-banner-head",
          shiny::span("Run details"),
          shiny::span(class = "status-banner-hint", "Hover to expand")
        ),
        shiny::div(class = "status-banner-body", body_ui)
      ))
    }

    shiny::tagList(
      shiny::div(
        class = "status-banner-head",
        shiny::span(sprintf("Run details: %s", pipeline$run_output_dir)),
        shiny::span(class = "status-banner-hint", "Hover to expand")
      ),
      shiny::div(
        class = "status-banner-body",
        shiny::div(class = "status-line", shiny::strong("Run output directory:"), paste(" ", pipeline$run_output_dir)),
        shiny::div(class = "status-line", shiny::strong("Audit log:"), paste(" ", pipeline$audit_log_file)),
        shiny::div(class = "status-line", shiny::strong("Parser version:"), paste(" ", pipeline$settings$parser_version)),
        shiny::div(class = "status-line", shiny::strong("Uploader version:"), paste(" ", selected_uploader_version())),
        shiny::div(class = "status-line", shiny::strong("Active mapping file:"), paste(" ", basename(active_append_mapping_file()))),
        if (!is.null(pipeline$review_file) && !is.na(pipeline$review_file) && nzchar(pipeline$review_file)) {
          shiny::div(class = "status-line", shiny::strong("Review file:"), paste(" ", pipeline$review_file))
        },
        shiny::div(class = "status-line", shiny::strong("REDCap target:"), paste(" ", if (nzchar(redcap_config_state()$redcap_uri)) redcap_config_state()$redcap_uri else "Not set")),
        shiny::div(class = "status-line", shiny::strong("Upload status:"), paste(" ", if (is.null(upload)) "Not started" else upload$status)),
        if (is_append_upload_result(upload)) {
          append_summary <- append_upload_audit_summary(upload)
          shiny::tagList(
            shiny::div(class = "status-line", shiny::strong("Duplicate final-key groups identified:"), paste(" ", append_summary$duplicate_key_groups_identified)),
            shiny::div(class = "status-line", shiny::strong("Duplicate payload rows identified:"), paste(" ", append_summary$duplicate_payload_rows_identified)),
            shiny::div(class = "status-line", shiny::strong("Rows merged away before append write:"), paste(" ", append_summary$duplicate_rows_merged)),
            shiny::div(class = "status-line", shiny::strong("Final append rows ready:"), paste(" ", append_summary$final_append_rows_ready)),
            shiny::div(class = "status-line", shiny::strong("Final rows uploaded:"), paste(" ", append_summary$final_rows_uploaded))
          )
        },
        if (!is.null(upload) && !is.null(upload$warning_message) && nzchar(upload$warning_message)) {
          shiny::div(class = "status-line", shiny::strong("Upload warning:"), paste(" ", upload$warning_message))
        },
        if (!is.null(github_import)) {
          shiny::div(
            class = "status-line",
            shiny::strong("Latest GitHub import:"),
            paste(" ", github_import$imported_n, "file(s) from", paste0(github_import$owner, "/", github_import$repo), "via", github_import$method)
          )
        }
      )
    )
  })

  output$preview_table <- DT::renderDT({
    pipeline <- pipeline_state()
    shiny::req(pipeline)
    DT::datatable(
      pipeline$parsed_wide,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = datatable_export_buttons("vitek_parsed_preview")
      )
    )
  })

  output$validation_table <- DT::renderDT({
    pipeline <- pipeline_state()
    shiny::req(pipeline)
    DT::datatable(
      pipeline$validation_result$summary,
      rownames = FALSE,
      options = list(dom = "tip", pageLength = 15)
    )
  })

  output$review_table <- DT::renderDT({
    pipeline <- pipeline_state()
    shiny::req(pipeline)
    DT::datatable(
      pipeline$flagged_rows,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = datatable_export_buttons("vitek_review_queue")
      )
    )
  })

  output$audit_table <- DT::renderDT({
    pipeline <- pipeline_state()
    shiny::req(pipeline)
    DT::datatable(
      read_audit_log(pipeline$audit_log_file),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$append_mapping_table <- DT::renderDT({
    mapping_result <- append_mapping_state()

    if (is.null(mapping_result)) {
        current_mapping <- load_append_redcap_mapping(
        mapping_file = active_append_mapping_file(),
        quiet = TRUE
      )

      return(DT::datatable(
        current_mapping,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      ))
    }

    DT::datatable(
      mapping_result$updated_mapping,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = datatable_export_buttons("redcap_append_mapping_suggestions")
      )
    )
  }, server = FALSE)
}

shiny::shinyApp(ui, server)
