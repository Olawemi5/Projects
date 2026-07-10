# VITEK AST Extraction and REDCap Upload

This project is an R-based Shiny application that wraps an existing VITEK AST extraction and REDCap upload pipeline. It keeps the workflow R-first, modular, and reproducible while adding a user-facing interface for running the process end to end.

## What the app does

The app supports this workflow:

1. upload one or more VITEK PDF reports
2. optionally import PDFs from a GitHub repository
3. extract and parse organism identification and AST results
4. transform parsed data into REDCap-ready wide-format records
5. validate and clean records
6. separate rows needing manual review
7. upload validated records to REDCap
8. generate run artifacts, audit logs, and review outputs

## App flow

The app opens on a landing page before the main workflow.

On the landing page you can:

- choose the uploader version
- review local REDCap keyring status
- set up REDCap keyring storage on that computer
- test an existing setup
- clear an existing setup if the local credential should be replaced
- download a package-summary PDF that explains the app, uploader options, outputs, auditing, and current development status
- open the contact/help actions from the landing card

After that, the main workflow page handles parsing, mapping, review, and upload.

## Uploader versions

### `v1.0.0`

- uses the standard uploader path
- is intended for cases where the parser-ready REDCap structure already matches the destination project
- does not expose the append-mapping workflow
- normalizes outbound `patient_id` values before upload by removing separators, using up to the first 6 digits, and keeping suffixes only when still needed for uniqueness

### `v1.1.0`

- enables append-safe upload into an existing REDCap project
- uses a REDCap dictionary-guided mapping workflow
- supports accepted or uploaded append mapping files
- writes only mapped fields with append-safe behavior
- surfaces warnings when rows are excluded because no valid REDCap-mappable update values remain after conversion

## REDCap setup

REDCap setup is now handled from the landing page, not from the workflow sidebar.

The app stores:

- non-secret REDCap configuration in `config/redcap_config.yml`
- the REDCap API token in the local machine keyring

That setup stays on the local machine until the saved credential is removed or replaced.

## Project layout

```text
VITEK_EXTRACT/
|-- .here
|-- app.R
|-- README.md
|-- LICENSE
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
|-- logs/
|-- output/
|   |-- cache/
|   |-- runs/
|   `-- templates/
|-- review/
|-- scripts/
`-- tests/
```

## Output structure

Generated files are organized as follows:

- `docs/`
  - reusable base templates and reference files
- `docs/saved_mappings/`
  - persistent named mappings saved intentionally by the user
- `output/templates/`
  - temporary append mapping files generated from the app
  - suggested mappings
  - temporary accepted mappings
  - uploaded completed mappings copied into the project
- `output/runs/<timestamp>/`
  - per-run parsed data
  - upload-ready data
  - append payloads
  - append preflight outputs
  - per-run audit logs
  - review files
  - raw text extraction outputs for that run
- `output/cache/`
  - PDF manifest
  - app action audit log
- `data_raw/`
  - local PDF staging folder for uploaded and GitHub-imported reports

## Mapping workflow for `v1.1.0`

The append workflow uses a fixed `source_name` list and suggests `target_redcap_name` values from a REDCap data dictionary.

In the app you can:

1. upload a REDCap data dictionary
2. generate append mapping suggestions
3. review the suggestions in the `Append Mapping` tab
4. accept and name the mapping
5. reuse previously saved mappings from the saved mapping dropdown
6. upload a completed append mapping CSV directly if you already have one

Persistent named mappings are saved in `docs/saved_mappings/`. Temporary generated and uploaded mapping files remain in `output/templates/`.

## Package summary PDF

The landing page includes a downloadable PDF overview of the package.

That PDF now includes:

- an executive summary of what the package is for
- a clearer explanation of the app workflow
- a distinction between `Uploader v1.0.0` and `Uploader v1.1.0`
- explanation of outputs, audit logs, and run artifacts
- a note on what is still in development, including `Parser v1.1.0`
- a closing collaboration note and `BioParseR.com`

The PDF renderer is paginated, so longer content is allowed to flow across multiple pages instead of being cut off on one page.

## Cleaning and isolate handling

The cleaning logic reshapes multiple reports into patient-family-aware wide records.

- a normalized patient-family key is derived from `patient_id`
- the first distinct organism uses base fields
- second and third distinct organisms use `_2` and `_3`
- repeated same-organism reports are retained as separate upload rows
- if a family contains both distinct organisms and same-organism repeats, the distinct organisms are widened while the extra repeats are still preserved separately
- append payload building can convert isolate dropdown targets by matching the corresponding organism label to the REDCap coded choice list

## GitHub import

GitHub import saves directly into the app-local `data_raw/` folder.

- if a GitHub link points to a specific subtree, import is limited to that subtree and its descendants
- the importer prefers downloading only PDFs
- it can also extract PDFs from ZIP files found inside the chosen subtree
- if selective import fails, it can fall back to ZIP-based import with timeout and retry logic
- the app shows which import method was used
- after a successful import, the UI shows a thin confirmation line such as `8 records imported and ready to run.`

## Running the app locally

Open R from the project root and run:

```r
shiny::runApp()
```

You can also use the included launchers on Windows:

```text
run_app.bat
run_app.ps1
run_tests.bat
run_tests.ps1
```

## Running tests

From the project root:

```r
source("tests/testthat.R")
```

Or from the command line with Rscript:

```text
Rscript tests/testthat.R
```

## Scripted pipeline runs

Single-file example:

```text
Rscript scripts/run_pipeline.R data_raw/your_report.pdf
```

Batch processing example:

```text
Rscript scripts/run_pipeline_batch.R
```

## Configuration

Main configuration files:

- `config/redcap_config.yml`
  - REDCap API URL
  - keyring service
  - keyring username
- `docs/redcap_variable_mapping.csv`
  - base parser-to-REDCap mapping
- `docs/redcap_append_variable_mapping.csv`
  - append uploader mapping template

The repository contains neutral example REDCap settings only. Configure your own API URL, keyring username, and API token from the landing page before attempting an upload. API tokens are stored in the local operating-system keyring and must never be committed to the repository.

## Privacy and data safety

VITEK reports can contain patient and laboratory identifiers. Do not commit real reports, extracted text, upload payloads, review files, or run logs. The repository ignores generated content under `data_raw/`, `data_processed/`, `output/`, `logs/`, and `review/`; only placeholder files are tracked. Use synthetic or properly de-identified reports for demonstrations and software testing.

## Cache and cleanup

Inside the app:

- `Clear PDF Manifest`
  - resets the cached PDF manifest
- `Clear Output Folder`
  - removes generated run folders under `output/runs/`
- `Clear Cache`
  - clears:
    - `output/cache/`
    - `output/runs/`
    - `output/templates/`
    - PDF files in `data_raw/`
    - in-session app state

The following are preserved by cache clearing:

- reusable base templates in `docs/`
- persistent named mappings in `docs/saved_mappings/`

## Current verification state

Latest verified checks:

- `235` tests passed
- `0` failures
- `0` warnings
- `app.R` parses successfully
- the Shiny application object constructs successfully from the repository subfolder

## License

VITEK-EXTRACT is free software distributed under the [GNU General Public License v3.0](LICENSE). You may use, study, modify, and redistribute it under the terms of that license. There are no additional restrictions on use by non-academic users.
