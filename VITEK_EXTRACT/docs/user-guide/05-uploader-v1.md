# How to Use Uploader v1.0.0

Uploader `v1.0.0` is the direct route for a REDCap project whose variables and layout already match the parser-ready structure.

## Before you begin

- Confirm that `docs/redcap_variable_mapping.csv` matches the destination project.
- Confirm the destination record identifier and `patient_id` rules.
- Confirm whether the event name `microbiology` is valid for the project.
- Test the complete procedure in a non-production project.
- Complete the review described in [Review validation results](07-validation-and-review.md).

## Inspect the base mapping

Open `docs/redcap_variable_mapping.csv`. It has two required columns:

| Column | Purpose |
|---|---|
| `source_name` | Field produced by the parser/cleaner |
| `redcap_name` | Destination REDCap variable name |

The mapping function keeps only usable mapped source columns, renames them, converts values to character data, adds required fields when configured, and replaces missing values with the configured default.

> [!CAUTION]
> Matching field names do not guarantee matching meaning. Confirm field type, coded choices, units, identifier status, and event/instrument placement in REDCap.

## Run the direct workflow

1. On the landing page, choose uploader `v1.0.0`.
2. Select **Continue**.
3. Upload or import the intended PDF batch.
4. Select **Run Pipeline**.
5. Review all tabs and compare a sample against the source reports.
6. Expand **REDCap Upload**.
7. Leave **Dry run REDCap upload** selected.
8. Select **Upload Validated Records**.
9. Inspect the dry-run result, including row count, key values, event value, and fields.
10. Save the dry-run artifacts with the run documentation.

## Authorize a live upload

Proceed only when the pre-upload checklist is complete and local policy permits the transfer.

1. Confirm the REDCap URL and project token again.
2. Confirm that only validated, authorized rows are present.
3. Clear **Dry run REDCap upload**.
4. Select **Upload Validated Records** once.
5. Wait for the result. Do not click repeatedly while an upload is in progress.
6. Review the upload status and audit log.
7. In REDCap, verify record count and representative field values.
8. Reconcile any rejected or missing rows before another upload.

## Patient identifier normalization

The direct uploader normalizes outbound `patient_id` values by removing separators, using up to the first six digits, and retaining suffixes when needed for uniqueness. This behavior is project-specific and must be validated against the destination key design.

Do not use direct upload if this normalization could join different patients, overwrite an existing record, or violate the project's identifier rules.

## Safe retry procedure

1. Identify whether the failed attempt reached REDCap or stopped locally.
2. Inspect the audit and upload logs.
3. Check the affected records in REDCap.
4. Determine whether a retry would create, update, or duplicate records.
5. Correct the cause.
6. Repeat a dry run.
7. Perform the smallest necessary live retry.

## Completion checklist

- [ ] Dry-run payload reviewed.
- [ ] Live upload authorized.
- [ ] Upload status recorded.
- [ ] REDCap record count reconciled.
- [ ] Representative values checked against source reports.
- [ ] Flagged and failed records accounted for.
- [ ] Audit artifacts retained according to policy.

## Next step

Read [Use outputs and audit logs](08-outputs-and-audit.md).
