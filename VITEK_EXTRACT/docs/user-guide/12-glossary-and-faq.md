# Glossary and Frequently Asked Questions

## Glossary

### AES

Advanced Expert System information represented in VITEK reports. The parser preserves whether an interpretation is standard, AES modified, or user modified when the report provides that marker.

### Append mapping

A reviewed relationship between a parser source field and an existing REDCap target field, used by uploader `v1.1.0`.

### AST

Antimicrobial susceptibility testing.

### Audit log

A chronological record of pipeline or upload stages, statuses, and operational details for a run.

### Data dictionary

A REDCap CSV describing project variables, forms, field types, choices, validation, and related metadata.

### Dry run

A preparation/check step that builds and validates what would be uploaded without intentionally performing the live REDCap write.

### ETL

Extract-transform-load: obtain information from a source, restructure/clean it, then load it into a destination system.

### Key field

The field used to identify the REDCap record that should be created or updated. The supplied workflows use `patient_id` by default.

### Keyring

The operating system's protected credential store. VITEK-EXTRACT uses it for the REDCap API token.

### Long AST data

A representation with one row per antimicrobial/result and columns for antimicrobial, MIC, raw interpretation, and interpretation note.

### MIC

Minimum inhibitory concentration. The application retains MIC values as text so operators such as `<=` and `>=` can be preserved.

### Parser

The software component that recognizes report labels and result patterns in extracted text and converts them into named structured fields.

### REDCap

Research Electronic Data Capture, the destination data platform used by this workflow.

### Review queue

Rows that did not pass one or more structural checks and require investigation before upload.

### Structural validation

Checks for required fields, duplicate keys, allowed value patterns, and consistency. It is not the same as clinical or scientific verification.

### Wide AST data

A report-level record where each antimicrobial has fields such as `<antibiotic>_mic`, `<antibiotic>_interpretation_raw`, and `<antibiotic>_interpretation`.

## Frequently asked questions

### Is VITEK-EXTRACT a general PDF reader?

No. It is a rule-based ETL application for supported VITEK 2 COMPACT report structures. It does not reliably parse arbitrary PDFs.

### Which AST cards are currently represented in the parser?

The current parser code contains card-specific dictionaries for `AST-GN75`, `AST-GP75`, and `AST-YS08`.

### Can it read scanned PDFs?

Not by itself. `pdftools` extracts embedded text. Image-only scans need an approved OCR process and separate verification.

### Does a `Pass` result mean the AST value is correct?

No. It means the row passed configured structural checks. Compare extracted values with source reports and perform local validation.

### Does the software standardize data to SNOMED CT or LOINC?

No standards-based terminology mapping is implemented in the current public code. The software performs local field/value normalization and project-specific REDCap mapping.

### Which uploader should I choose?

Use `v1.0.0` for a destination aligned with the parser-ready structure. Use `v1.1.0` when mapping into an existing REDCap project with different variable names. See [Choose an uploader workflow](03-choose-a-workflow.md).

### Is parser v1.1.0 ready?

The interface identifies parser `v1.1.0` as in development. Use the supported parser version unless a tested release states otherwise.

### Where is the API token stored?

In the computer's operating-system keyring. The YAML configuration stores only the API URL, service name, and keyring username.

### Can I move a configured copy to another computer?

The files can be moved, but the token remains in the original computer's keyring. Configure the credential separately on the new approved computer.

### Why can the number of parsed rows differ from the number of PDFs?

Reports may contain multiple organisms or versions, and cleaning rules can collapse distinct organisms into field slots while retaining repeated same-organism reports as separate rows.

### Can I edit `upload_ready.csv` and upload it?

Do not use manual edits to bypass validation. Correct the input, mapping, or code and rerun so the audit trail remains reproducible.

### Can I publish example reports in the repository?

Only synthetic or properly de-identified reports that are approved for public release. Check filenames, metadata, raw text, and screenshots as well as visible page content.

### Is external use restricted to academic institutions?

No additional non-academic restriction is imposed. The software is distributed under the GNU General Public License v3.0.

### How should I cite the software?

Cite the public repository together with a fixed release/tag and commit identifier used in the work. Add the manuscript citation when it becomes available.

### Where should I start when something fails?

Preserve the error, inspect the run audit log, and use [Troubleshooting](09-troubleshooting.md).

Return to the [guide home](README.md).
