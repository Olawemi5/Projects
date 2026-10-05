# VITEK-EXTRACT Preflight Checklist

Use one copy for each production batch. Record evidence in the approved study or laboratory system rather than committing completed checklists containing identifiers to Git.

## Environment

- [ ] Approved computer and storage location.
- [ ] Intended Git commit/release recorded.
- [ ] Automated tests reviewed for this release.
- [ ] Correct parser and uploader versions selected.

## Source batch

- [ ] Operator is authorized to process the reports.
- [ ] Batch belongs to one intended study/project and destination.
- [ ] Expected file count recorded.
- [ ] PDFs contain extractable text.
- [ ] Card/layout support confirmed.
- [ ] No public repository contains identifiable reports.

## REDCap

- [ ] Correct API endpoint confirmed.
- [ ] Correct project-specific token confirmed.
- [ ] Import/update permission confirmed.
- [ ] Current data dictionary obtained.
- [ ] Record key and event/repeat design confirmed.
- [ ] Backup/export or correction procedure available.

## Review

- [ ] File and parsed-row counts reconciled.
- [ ] Representative reports compared field by field.
- [ ] Every flagged row investigated.
- [ ] Every parser failure accounted for.
- [ ] Mapping reviewed by meaning and type.
- [ ] Coded choices and isolate slots checked.

## Upload

- [ ] Dry run completed.
- [ ] Proposed keys, fields, values, and row count reviewed.
- [ ] Duplicates, exclusions, and warnings reconciled.
- [ ] Live upload authorized.
- [ ] REDCap results reconciled after upload.
- [ ] Audit artifacts retained securely.
