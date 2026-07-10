# Project Context

## Project Purpose

This project is an R-based Shiny application for the end-to-end VITEK AST workflow:

- upload or import VITEK PDF reports
- extract raw text from reports
- parse organism identification and AST results
- transform parsed results into structured wide-format data
- map parser output into REDCap-compatible fields
- validate and clean records
- separate records needing review
- upload validated records to REDCap
- generate audit logs and run artifacts

The project is intentionally R-first and reuses the user's existing R pipeline logic instead of rewriting it in another language.

## Folder Structure

```text
VITEK_EXTRACT/
|-- app.R
|-- PROJECT_CONTEXT.md
|-- README.md
|-- config/
|   `-- redcap_config.yml
|-- data_raw/
|-- docs/
|   |-- Extract_DataDictionary.csv
|   |-- files_to_process.txt
|   |-- redcap_variable_mapping.csv
|   |-- redcap_append_variable_mapping.csv
|   `-- saved_mappings/
|-- functions/
|   |-- audit_logger.R
|   |-- clean_data.R
|   |-- get_unprocessed_files.R
|   |-- import_github_data_raw.R
|   |-- keyring_setup_helpers.R
|   |-- load_append_redcap_mapping.R
|   |-- load_redcap_config.R
|   |-- map_redcap.R
|   |-- normalize_mapping_terms.R
|   |-- parse_ast_chart_report_full.R
|   |-- parse_ast_chart_report_part.R
|   |-- prepare_append_trial.R
|   |-- read_pdf_manifest.R
|   |-- read_redcap_dictionary.R
|   |-- read_vitek_pdf.R
|   |-- read_vitek_pdf_full.R
|   |-- run_shiny_pipeline.R
|   |-- suggest_redcap_target_fields.R
|   |-- update_append_mapping_targets.R
|   |-- upload_dispatcher.R
|   |-- upload_redcap.R
|   `-- validate_redcap_data.R
|-- logs/
|-- output/
|   |-- cache/
|   |-- runs/
|   `-- templates/
|-- review/
|-- scripts/
|   |-- packages.R
|   |-- run_pipeline.R
|   `-- run_pipeline_batch.R
`-- tests/
    |-- testthat.R
    `-- testthat/
```

## Generated Artifact Conventions

- `docs/` holds reusable base templates and reference files.
- `docs/saved_mappings/` holds persistent named append mappings saved intentionally by the user from the app.
- `output/templates/` holds temporary generated mapping files created from the app:
  - suggested mappings
  - temporary accepted mappings
  - uploaded completed mappings copied into the project
- `output/runs/<timestamp>/` holds per-run artifacts:
  - parsed data
  - upload-ready data
  - append payloads
  - append preflight outputs
  - per-run audit logs
  - per-run review exports
  - raw text extraction outputs for that run
- `output/cache/` holds temporary app-level artifacts:
  - PDF manifest
  - app action audit log
- `data_raw/` is the app-local PDF staging folder used for uploaded and GitHub-imported reports.

## Coding Standards

- Keep the project R-first and reuse existing R functions whenever possible.
- Prefer project-relative paths for Shiny-facing workflows instead of relying on `here::here()` when root detection may vary.
- Keep parsing, mapping, cleaning, validation, and upload logic separated into focused functions.
- Preserve existing pipeline behavior unless a requested change clearly requires behavior updates.
- Add or update `testthat` coverage for behavior changes, especially around:
  - cleaning and reshaping
  - mapping suggestions
  - append payload generation
  - uploader routing
  - GitHub import behavior
- Keep app-specific orchestration in `app.R`; move reusable logic into `functions/`.
- Timestamp generated artifacts for traceability.

## Important Decisions

- The app is currently a Shiny project, not yet a formal installed R package.
- The app opens on a landing page with uploader-version selection before entering the main workflow.
- The landing page is the home for REDCap keyring setup and setup-status checking.

### Parser Versioning

- Parser version is shown in the workflow header.
- `v1.0.0` is active.
- `v1.1.0` is visible as in-development and not selectable yet.

### Uploader Versioning

- Uploader version is selectable on the landing page and in the workflow header.
- `v1.0.0` uses the standard uploader path.
- `v1.1.0` enables append-mapping features and uses the append uploader path.
- The app can switch between `v1.0.0` and `v1.1.0` during a session.

### Shiny Workflow Design

- GitHub import is hidden behind a toggle button instead of always showing the URL field.
- The landing page contains:
  - uploader-version selection
  - local REDCap keyring status
  - REDCap setup/test/clear actions
  - a package-summary help action that downloads a multi-page PDF overview of the package
  - a contact menu for support, suggestions, and collaboration
- The workflow page no longer contains a separate REDCap setup sidebar panel.
- REDCap mapping/tools and REDCap upload remain separate sidebar panels.
- A saved generated mapping dropdown appears in the REDCap mapping/tools area when generated mappings exist.
- Accepted suggested mappings can be named at save time.
- Named saved mappings are persistent and stored outside cache-cleared output.
- A back arrow in the header returns the user to the landing page.
- A hover-expand status banner is used instead of a permanently open status panel.
- The header refresh panel is styled as a floating dropdown so it does not push page content downward.
- The landing card is intentionally larger, with help/contact floating actions anchored on the outer edge so they do not overlap the uploader description.

### REDCap Local Setup

- REDCap token storage is local-machine keyring based.
- The landing page offers:
  - `Set Up REDCap Keyring`
  - `Save And Test Keyring`
  - `Save And Exit`
  - `Test Existing Setup`
  - `Clear Existing Setup`
- Non-secret REDCap config is stored in `config/redcap_config.yml`.
- The token itself is stored in the local machine keyring and persists on that computer until removed or replaced.
- If the credential is removed but the YAML config remains, the app reports that setup must be completed again for that computer.

### Cleaning and Wide-Format Logic

- Multi-isolate handling is patient-family aware and wide-format.
- A normalized patient-family key is derived from `patient_id` by extracting digits and keeping up to the first 6 digits.
- Distinct organisms within the same normalized patient family are collapsed into:
  - base fields
  - `_2`
  - `_3`
- Repeated same-organism reports are retained as separate upload rows instead of being collapsed away.
- If a family contains both distinct-organism and repeated same-organism reports:
  - distinct organisms are widened into base, `_2`, `_3`
  - extra same-organism repeats are preserved as additional rows
- Overflow beyond three distinct organisms is flagged in comments and validation status.

### Standard Uploader Behavior (`v1.0.0`)

- The standard uploader uses the parser-ready REDCap structure directly.
- Before upload, `patient_id` is normalized for REDCap-facing output by:
  - removing non-digit characters from the base portion
  - keeping the first 6 digits when more than 6 exist
  - keeping the full number when fewer than 6 digits exist
- Suffixes are retained only when needed to preserve uniqueness among final upload rows.

### Append Mapping Logic

- `source_name` is fixed.
- `target_redcap_name` is the field being suggested or filled.
- REDCap dictionary matching uses:
  - field names
  - field labels
  - interpretation and MIC semantics
  - isolate slot semantics
  - patient ID semantics
  - substantive token matching
  - antibiotic-family heuristics such as clindamycin vs inducible clindamycin
- Parser-only metadata fields are not auto-mapped.
- Comment-style fields are strongly penalized from matching patient identifier fields.
- The REDCap target chosen for `patient_id` is reserved so it cannot be suggested for any other source field.

### Append Uploader Behavior (`v1.1.0`)

- Append preflight runs before live append.
- Append uses accepted or uploaded append mapping files.
- Append writes only mapped fields and uses `overwrite_with_blanks = FALSE` to preserve unrelated REDCap fields.
- `patient_id` key normalization for append is uploader-specific:
  - digits are extracted
  - the first 6 digits are used when available
- Isolate dropdown targets can be populated using organism-label-to-code matching based on the corresponding `identified_organism`, `identified_organism_2`, or `identified_organism_3`.
- Categorical REDCap fields can be coerced from parser values such as:
  - `S`, `I`, `R`
  - `POS`, `NEG`
  - organism labels matched to coded REDCap choices
- Rows with no valid REDCap-mappable update values after safe conversion are dropped before write.
- The app surfaces a user-facing warning when rows are dropped for having no valid update values left.

### Active Mapping State

- The app uses a reactive `active_append_mapping_file` to decide which append mapping file is currently active.
- Default active mapping is `docs/redcap_append_variable_mapping.csv`.
- Uploaded, accepted, or manually selected saved generated mappings can become the active file for the current session.
- Persistent named mappings are stored in `docs/saved_mappings/`.
- Temporary generated and uploaded mapping files are stored in `output/templates/`.
- Persistent accepted mappings saved intentionally by the user are stored in `docs/saved_mappings/` and appear in the saved-mapping dropdown on later runs.

### GitHub Import Behavior

- GitHub import saves PDFs into the app-local `data_raw/` folder.
- If a GitHub link points to a specific subtree, import is limited to that subtree and its descendants only.
- The importer prefers downloading only PDFs and can also extract PDFs from ZIP files found inside the chosen subtree.
- If selective PDF import fails, it can fall back to ZIP-based import with timeout and retry logic.
- The app surfaces which import method was used, such as `github_api_pdf_only` or `github_zip_pdf_only`.
- After successful import, the app shows a thin status line indicating how many records were imported and are ready to run.

### Cache and Cleanup

- `Clear Output Folder` clears generated run folders under `output/runs/`.
- `Clear Cache` clears:
  - `output/cache/`
  - `output/runs/`
  - `output/templates/`
  - PDF files under `data_raw/`
  - in-session Shiny state
- Persistent named mappings in `docs/saved_mappings/` are not deleted by cache clearing.
- Reusable base templates in `docs/` are not deleted by cache clearing.

### Package Summary PDF

- The landing page includes a downloadable package-summary PDF.
- The summary is designed as a multi-page overview, not a single clipped page.
- It includes:
  - an executive summary
  - uploader distinctions
  - output and audit-trail explanation
  - parser v1.1.0 development notes
  - collaboration and contact language
  - a BioParseR.com link at the end

## Required Packages

Core packages referenced in the project include:

- `shiny`
- `bslib`
- `DT`
- `pdftools`
- `dplyr`
- `tidyr`
- `stringr`
- `purrr`
- `tibble`
- `readr`
- `janitor`
- `lubridate`
- `REDCapR`
- `assertthat`
- `validate`
- `logger`
- `glue`
- `fs`
- `here`
- `config`
- `yaml`
- `openxlsx`
- `rmarkdown`
- `progress`
- `keyring`
- `testthat`

## Naming Conventions

- Functions use `snake_case`.
- Versioned logic uses explicit routing or version labels:
  - parser version via UI selection
  - uploader version via `upload_redcap_dispatcher()`
- REDCap append mapping files use:
  - `source_name`
  - `target_redcap_name`
- Isolate expansion fields use:
  - base field for first isolate
  - `_2` for second isolate
  - `_3` for third isolate
- Generated mapping filenames are timestamped.
- Persistent named mappings are saved under `docs/saved_mappings/`.
- Temporary generated mapping files are saved under `output/templates/`.
- Run artifact folders are timestamped under `output/runs/`.
- Download filenames are timestamped for traceability.

## Current Verification State

- Test suite currently passes.
- Latest confirmed status:
  - `176` tests passed
  - `0` failures
  - `0` warnings
- `app.R` parses successfully.
