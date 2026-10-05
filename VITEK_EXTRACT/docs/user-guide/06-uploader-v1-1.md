# How to Use Uploader v1.1.0

Uploader `v1.1.0` supports append-safe, dictionary-assisted mapping into an existing REDCap project. It is appropriate when parser variables and destination variables do not already share the same names and structure.

## Before you begin

Obtain the current REDCap data dictionary and confirm the project's key field, events, coded choices, and update policy. A stale dictionary can create a mapping that looks reasonable but is wrong.

> [!IMPORTANT]
> Mapping suggestions are candidates, not approvals. A person who understands both the parser output and the REDCap project must review every target field.

## Open the mapping workflow

1. Choose uploader `v1.1.0` on the landing page.
2. Select **Continue**.
3. Confirm that the header shows uploader `v1.1.0`.
4. Confirm that the **Append Mapping** tab and **REDCap Mapping And Tools** panel are available.

![Uploader v1.1.0 workspace](images/workflow-v1.1.png)

## Process the reports

1. Upload or import the PDF batch.
2. Select **Run Pipeline**.
3. Review **Preview**, **Validation Summary**, **Review Queue**, and **Audit Log** before mapping.

## Load a REDCap data dictionary

1. Expand **REDCap Mapping And Tools**.
2. Use **Download Data Dictionary Template** if you need to inspect the expected CSV format.
3. Export the current data dictionary from the destination REDCap project.
4. Upload that dictionary in the mapping panel.
5. Confirm that it belongs to the intended project and environment.

The repository includes `docs/Extract_DataDictionary.csv` as an example/template. Do not assume that it matches another REDCap project.

## Generate and review suggestions

1. Select **Suggest Append Mapping Targets**.
2. Open the **Append Mapping** tab.
3. For each `source_name`, inspect the proposed destination variable.
4. Verify field meaning, type, choices, units, organism/isolate slot, and required status.
5. Verify that `patient_id` or the configured key identifies exactly the intended existing record.
6. Leave a source field unmapped when no safe destination exists.
7. Correct mappings before accepting them.

### High-risk mapping examples

- A patient identifier mapped to a specimen identifier.
- A raw interpretation mapped to an interpretation-note field.
- An organism label mapped to a coded field without converting it to an allowed choice.
- Base-organism values mapped to `_2` or `_3` isolate fields.
- MIC text mapped to a numeric field that cannot retain `<=` or `>=`.
- A blank source value configured to overwrite a populated destination value.

## Save or load a mapping

### Save reviewed suggestions

1. Enter a short, descriptive mapping name.
2. Select **Accept Suggested Mapping** only after the review is complete.
3. The application saves the accepted mapping under `docs/saved_mappings/` and makes it active.

### Load a saved mapping

1. Choose it from **Saved Generated Mapping**.
2. Select **Use Saved Mapping**.
3. Confirm that the mapping was created for the current REDCap dictionary version.

### Upload a completed mapping CSV

Use **Upload Completed Append Mapping CSV** when an approved mapping has been prepared separately. The file must follow the append mapping structure, including source, target, update mode, and key information.

Do not reuse a mapping after a REDCap dictionary change without revalidation.

## Prepare the append upload

The append workflow builds mapped partial updates. It can convert organism dropdown targets by matching the organism label to the destination REDCap coded choices. Rows with no valid mappable update values can be excluded and reported as warnings.

1. Review the active mapping.
2. Confirm the final key fields.
3. Confirm that unrelated destination fields will not be written.
4. Expand **REDCap Upload**.
5. Keep **Dry run REDCap upload** selected.
6. Select **Append To Existing REDCap**.
7. Inspect duplicate-key groups, duplicate payload rows, merged rows, excluded rows, and final rows ready.

## Perform a live append

1. Reconcile every dry-run warning.
2. Confirm that all target records exist or that the intended create/update behavior is permitted.
3. Clear **Dry run REDCap upload**.
4. Select **Append To Existing REDCap** once.
5. Review the final rows uploaded and the audit log.
6. Verify representative records in REDCap, including records with multiple organisms.

## Mapping approval checklist

- [ ] Current data dictionary used.
- [ ] Key field verified.
- [ ] Every mapped field reviewed by meaning, not name alone.
- [ ] Coded choices verified.
- [ ] Isolate suffixes verified.
- [ ] MIC field types preserve operators.
- [ ] Empty-value behavior understood.
- [ ] Dry-run exclusions and duplicates reconciled.
- [ ] Mapping name/version documented.
- [ ] Live append authorized.

## Next step

Read [Use outputs and audit logs](08-outputs-and-audit.md).
