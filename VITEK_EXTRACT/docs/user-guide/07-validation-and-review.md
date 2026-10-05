# How to Review Validation Results

Validation separates records that meet the application's structural rules from records requiring manual review. It does not establish clinical correctness or prove that the parser extracted every value accurately.

## Understand the four main views

### Preview

Displays the structured records produced by parsing, cleaning, and mapping. Use it to compare source reports with the data that would proceed to upload.

### Validation Summary

Reports counts such as input rows, passing rows, flagged rows, and issue categories.

### Review Queue

Shows rows stopped by validation rules. These rows are not part of `upload_ready.csv` until the underlying issue is corrected and the pipeline is rerun.

### Audit Log

Records pipeline and upload events for the current run. Use it to establish what happened and where a failure occurred.

## Checks performed by the current validator

- Required REDCap fields, currently including `patient_id`.
- Duplicate configured keys, currently based on `patient_id`.
- MIC/interpretation mismatch when an interpretation exists but the MIC is empty.
- Unexpected susceptibility interpretation values.
- Unexpected interpretation-note values.
- Marker-test values for ESBL, cefoxitin screen, and inducible clindamycin resistance.
- Completely empty rows.

The validator recognizes values such as `S`, `I`, `R`, `POS`, `NEG`, `standard`, `aes_modified`, and `user_modified` according to the relevant field type.

## Review a flagged row

1. Record the run timestamp and source filename.
2. Open the original PDF in an approved viewer.
3. Locate the patient/report identifier, organism, card type, and affected AST value.
4. Compare the PDF with **Preview**, the issue description, and the raw extracted text in the run folder.
5. Classify the cause:
   - source report issue;
   - unsupported layout/card;
   - text extraction failure;
   - parser rule failure;
   - mapping error;
   - duplicate/key-design issue;
   - legitimate value not covered by validation rules.
6. Correct the source configuration, mapping, or code as appropriate.
7. Rerun the original batch or an approved subset.
8. Confirm that the issue is resolved without creating a new discrepancy.
9. Document any excluded record and the reason.

> [!WARNING]
> Do not edit `upload_ready.csv` manually to bypass the review queue. Manual edits break traceability and can conceal a systematic parsing problem.

## Scientific verification

For a new laboratory, report layout, card, or release:

1. Select a representative validation sample.
2. Have qualified personnel create a reference transcription independently of VITEK-EXTRACT.
3. Compare patient/report fields, organism, antimicrobial, MIC, interpretation, and marker tests field by field.
4. Record false additions, false omissions, and incorrect values.
5. Investigate disagreements.
6. Approve the workflow only after predefined acceptance criteria are met.

Automated unit tests verify code behavior against test cases. They are not a substitute for local analytical validation with representative reports.

## Spot-checking a routine run

Local policy should define the minimum review rate. A useful risk-based sample includes:

- at least one report of each card type in the batch;
- reports with modified interpretations;
- reports with marker tests;
- multiple-organism and repeated-report cases;
- minimum and maximum MIC patterns;
- any report whose layout or software version differs from the common pattern;
- every flagged or parser-failure case.

## Release criteria

A run is ready for upload only when:

- the expected files and rows are accounted for;
- no unresolved parser failure remains hidden outside the review count;
- review-queue decisions are documented;
- representative values match the source reports;
- the active REDCap mapping is approved;
- the dry-run payload matches expectation;
- the operator is authorized to perform the transfer.

## Next step

Continue to [Use outputs and audit logs](08-outputs-and-audit.md).
