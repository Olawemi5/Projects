# How to Operate VITEK-EXTRACT Safely

VITEK reports and generated artifacts can contain patient identifiers, laboratory identifiers, antimicrobial results, local paths, usernames, and REDCap project information. Treat the complete workflow as sensitive data processing.

## Use the minimum necessary data

1. Process only fields required for the approved purpose.
2. Use synthetic data for demonstrations, screenshots, training, and public testing.
3. Use properly de-identified data for development when synthetic data is insufficient and policy permits it.
4. Keep production and demonstration environments separate.

## Protect each stage

| Stage | Primary risk | Control |
|---|---|---|
| Source PDF | Direct identifiers and clinical/laboratory information | Approved storage, least-privilege access, encrypted device |
| Raw extracted text | Replicates most PDF content in searchable text | Protect like the source PDF; do not email or commit it |
| Parsed and review CSVs | Concentrated structured identifiers and AST results | Restrict access and retention; avoid spreadsheet cloud sync unless approved |
| Mapping files | Can reveal project design and control what is updated | Version and approve mappings; keep sensitive project-specific mappings private |
| API token | Provides programmatic project access | Store only in the operating-system keyring; revoke on suspected exposure |
| Upload payload/log | Shows what was proposed or transmitted | Retain securely for audit; redact before support sharing |

## Public repository rules

Before every commit:

```powershell
git status --short
git diff --cached
```

Confirm that no file from `data_raw`, `data_processed`, `output`, `logs`, `review`, or a private mapping directory has been staged. Do not use `git add -f` on generated data.

If sensitive material is committed, deleting it in a later commit is not sufficient because it remains in history. Stop sharing the repository and follow the organization's incident and Git-history-remediation procedure.

## REDCap upload safety

- Use project-specific, least-privilege tokens.
- Test in a non-production project.
- Keep dry run enabled until the proposed changes are understood.
- Confirm project URL, key, event, and mapping at the point of upload.
- Upload only passing and authorized rows.
- Reconcile REDCap results immediately after upload.
- Never assume clearing local outputs reverses remote changes.

## Clinical and scientific limitations

- Structural `Pass` status does not mean a result is clinically verified.
- The software does not determine treatment or patient care.
- The current parser is rule based and limited to supported report structures.
- The current workflow does not provide standards-based terminology harmonization.
- Parser changes require regression testing and local validation.
- Human review remains necessary for new layouts, difficult cases, and flagged records.

## Screenshot procedure

1. Use synthetic or de-identified reports.
2. Check the entire screen, including filenames, browser bars, local paths, usernames, notifications, and background windows.
3. Check modal dialogs and download lists.
4. Crop only after confirming the source image is safe.
5. Have a second person review publication screenshots when practical.

## End-of-run procedure

1. Reconcile the run and upload results.
2. Move required audit material to approved storage.
3. Close the application and browser session.
4. Clear temporary staged PDFs and outputs only after retention requirements are met.
5. Do not remove saved mappings needed for reproducibility without archiving their approved version.
6. Lock or sign out of the workstation.

## Incident response

If a token, report, or generated artifact may have been exposed:

1. Stop the transfer or publication.
2. Preserve necessary evidence without spreading the data further.
3. Revoke the affected REDCap token.
4. Notify the authorized data-protection, information-security, study, and REDCap contacts.
5. Follow institutional incident procedures.
6. Do not attempt to conceal the event by deleting logs.

## Next step

For reproducible technical operation, read [Developer and command-line guide](11-developer-guide.md).
