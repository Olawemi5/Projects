# How to Troubleshoot VITEK-EXTRACT

Start with the visible symptom, then inspect the current run's audit log. Preserve the original error message before clearing cache or rerunning the application.

## Fast diagnostic sequence

1. Record the exact error and time.
2. Record the operating system, R version, Git commit, parser version, and uploader version.
3. Identify whether the failure occurred during startup, import, parsing, mapping, validation, dry run, or live upload.
4. Inspect the latest `output/runs/shiny_run_*` audit log.
5. Reproduce the problem with the smallest synthetic or de-identified example.
6. Run the automated tests.
7. Do not attach sensitive reports or outputs to a public issue.

## Symptom guide

| Symptom | Likely cause | What to do |
|---|---|---|
| `Rscript` is not recognized | R is not installed or its `bin` directory is not on `PATH` | Run through RStudio, use the full `Rscript.exe` path, or add the correct R `bin` directory to `PATH` |
| A required package is missing | Runtime packages were not installed for this R library | Run `Rscript scripts/packages.R`, then install the packages named in the message |
| The app starts but the browser does not open | Browser launch was blocked or disabled | Open the local URL printed after `Listening on` |
| PDF produces no text | The report is image-only, damaged, encrypted, or uses an unsupported encoding | Confirm text can be selected; obtain a text PDF or run an approved OCR workflow and validate it separately |
| Unsupported or unrecognized AST card | Card heading is absent and the antimicrobial panel cannot be inferred, or the card is unsupported | Confirm the report card; document the failure; add/test a parser rule before production use |
| Parsing fails for one file | Layout differs, expected labels are absent, or extracted text is malformed | Inspect its raw extracted text and parser error; isolate it from supported files; do not silently omit it |
| Parsed row count differs from file count | Multiple isolates/report versions or cleaning rules changed row structure | Inspect `parsed_wide.csv`, `parsed_wide_clean.csv`, and source reports |
| Many rows are flagged as duplicate keys | `patient_id` is not unique for the intended record model | Stop; review record/event/repeat-instance design with the REDCap administrator |
| MIC/interpretation mismatch | An interpretation was parsed without a MIC, or a field was misparsed/mismapped | Compare the source PDF, raw text, parsed columns, and mapping |
| Mapping suggestion is blank | No sufficiently compatible REDCap field was found | Map manually only after verifying meaning, or add an appropriate destination field through REDCap governance |
| Mapping suggestion looks plausible but wrong | Name-based similarity does not guarantee semantic equivalence | Reject/correct it; verify type, choices, units, organism slot, and key behavior |
| Append row is excluded | No valid mapped update value remains after conversion | Inspect mapping, coded-choice conversion, and blank-value handling |
| Keyring credential is missing | The project was moved to another computer, the service/username changed, or the token was removed | Re-enter the token through **Set Up REDCap Keyring** on that computer |
| REDCap rejects the request | URL/token/permissions are wrong, fields are invalid, or values violate project rules | Keep the payload and response securely; confirm configuration, dictionary, rights, field names, choices, and event design |
| Test run shows warnings but no failures | Windows locale transliteration or package-build differences may emit warnings | Read each warning; do not report `0 warnings`; distinguish environmental warnings from failed expectations |

## Fix a startup problem

1. Confirm you are in the `VITEK_EXTRACT` directory.
2. Run:

   ```powershell
   Rscript scripts\packages.R
   ```

3. Start directly for a clearer console trace:

   ```powershell
   Rscript -e "shiny::runApp('.')"
   ```

4. Read the first error rather than the final cascade of messages.
5. Confirm that `config/redcap_config.yml` is valid YAML.
6. Run `Rscript tests\testthat.R`.

## Diagnose a parser failure

1. Find the run directory named in the current session.
2. Open the corresponding file in `processed/`.
3. Confirm that labels such as patient ID, selected organism, card type, and the AST heading appear in the extracted text.
4. Confirm the card is `AST-GN75`, `AST-GP75`, or `AST-YS08`.
5. Compare spacing, line breaks, antimicrobial names, and interpretation markers with the parser rules.
6. Create a synthetic regression case before changing parser logic.
7. Run all tests after the change.

## Diagnose an upload problem

1. Keep the app open and preserve the run folder.
2. Determine whether **Dry run REDCap upload** was enabled.
3. Check whether REDCap received any records before retrying.
4. Inspect the key field and event values first.
5. Compare payload field names with the current REDCap data dictionary.
6. Check coded choices and required fields.
7. Ask the REDCap administrator to confirm the token's rights.
8. Retry with one synthetic/test record before a larger batch.

## Report an issue safely

Include:

- a concise title and expected behavior;
- exact reproducible steps;
- operating system and R version;
- Git commit;
- parser and uploader versions;
- relevant package versions;
- sanitized audit/error text;
- a synthetic report that reproduces the issue, if possible;
- confirmation that the example contains no protected or identifying information.

Exclude:

- API tokens;
- real VITEK reports;
- patient names or identifiers;
- institutional internal URLs;
- raw text or payloads containing laboratory identifiers;
- screenshots with identifiable data.

Use the public repository's issue tracker only for information approved for public release.

## Next step

Review [Privacy and operational safety](10-privacy-and-safety.md), especially before sharing diagnostic material.
