# How to Use Outputs and Audit Logs

Each application run creates a timestamped directory under `output/runs/`. Keep the complete run directory together when retaining, reviewing, or securely deleting a run.

## Run directory map

```text
output/
|-- cache/
|   |-- pdf_manifest.csv
|   `-- app_action_audit_log.csv
|-- templates/
|   `-- temporary mapping files
`-- runs/
    `-- shiny_run_YYYYMMDD_HHMMSS/
        |-- input_files_manifest.csv
        |-- parsed_wide.csv
        |-- parsed_wide_clean.csv
        |-- upload_ready.csv
        |-- audit_YYYYMMDD_HHMMSS.csv
        |-- processed/
        |   `-- *_raw_text.txt
        `-- review/
            `-- flagged_rows_*.csv
```

Append and upload actions can create additional payload, trial, preflight, and upload-log artifacts in the same run directory.

## Understand each core artifact

| Artifact | Purpose | Sensitivity |
|---|---|---|
| `input_files_manifest.csv` | Connects each run entry to its source file | May contain filenames and local paths |
| `*_raw_text.txt` | Text extracted from the PDF | May contain all identifiers visible in the report |
| `parsed_wide.csv` | Parser output before isolate cleaning | Usually sensitive |
| `parsed_wide_clean.csv` | Cleaned/collapsed structured records | Usually sensitive |
| `upload_ready.csv` | Rows that passed structural validation | Usually sensitive and upload-capable |
| `flagged_rows_*.csv` | Records requiring investigation | Usually sensitive |
| `audit_*.csv` | Pipeline stage, status, and detail messages | May contain filenames, paths, or operational details |
| append/upload artifacts | Proposed or transmitted REDCap fields | Usually sensitive |

## Reconstruct a run

1. Identify the timestamped run directory.
2. Read `input_files_manifest.csv` to confirm the source batch.
3. Review the audit log in chronological order.
4. Compare `parsed_wide.csv` with `parsed_wide_clean.csv` to understand cleaning and isolate handling.
5. Compare the validated data with `upload_ready.csv` and the review file.
6. Inspect dry-run or upload artifacts to determine what was proposed or sent.
7. Record the Git commit, parser version, uploader version, active mapping, REDCap dictionary version, R version, and operator approval outside the generated files if local policy requires them.

## Use the cache controls safely

### Clear PDF Manifest

Resets the cached list of processed PDFs. It does not undo an upload and should not be used to conceal duplicate processing.

### Clear Output Folder

Removes generated run folders. Export or retain required audit material before using it.

### Clear Cache

Clears temporary app state, generated run folders, output templates, cached metadata, and PDF files staged in `data_raw/`. Persistent base templates and deliberately saved mappings are preserved.

> [!WARNING]
> Clearing local output does not reverse records already sent to REDCap. Any correction in REDCap must follow the project's authorized correction procedure.

## Retention procedure

1. Follow the study or institution's retention schedule.
2. Restrict run directories to authorized users.
3. Store a checksum or immutable copy when required for regulated or publication-related work.
4. Do not move sensitive run artifacts into the public Git repository.
5. When retention expires, use an approved secure-deletion process.

## Files intentionally excluded from Git

The repository `.gitignore` excludes generated content under:

- `data_raw/`
- `data_processed/`
- `output/`
- `logs/`
- `review/`
- `docs/saved_mappings/`

Do not rely on `.gitignore` as the only privacy control. Check `git status` before every commit and never force-add sensitive artifacts.

## Next step

For problems, read [Troubleshooting](09-troubleshooting.md). For operational controls, read [Privacy and operational safety](10-privacy-and-safety.md).
