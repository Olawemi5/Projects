# How to Install VITEK-EXTRACT

This procedure prepares a local computer to run the R/Shiny application. Windows is the most direct route because the repository includes PowerShell and batch launchers. The R commands also work on macOS and Linux when the required system libraries are available.

## What you need

- A current R 4.x installation.
- A modern web browser.
- Permission to install R packages.
- Git, if you want to clone and update the repository. Git is optional when downloading a ZIP file.
- Network access during package installation.
- REDCap API access only if you intend to upload records.

The code does not enforce a minimum R version. Use a currently supported R release and record the exact version used for reproducible work.

## Method 1: Clone with Git

1. Open PowerShell.
2. Choose a parent directory that is approved for your work.
3. Run:

   ```powershell
   git clone https://github.com/Olawemi5/Projects.git
   cd Projects\VITEK_EXTRACT
   ```

4. Confirm that `app.R`, `README.md`, `functions`, `scripts`, and `tests` are present.

### Update an existing clone

From the `Projects` directory, run:

```powershell
git pull --ff-only
```

> [!TIP]
> Record the commit used for a production run with `git rev-parse HEAD`. This makes the analysis and manuscript reproducible.

## Method 2: Download a ZIP

1. Open the [public repository](https://github.com/Olawemi5/Projects/tree/main/VITEK_EXTRACT).
2. Select **Code**, then **Download ZIP**.
3. Extract the archive to an approved local folder.
4. Open the extracted `VITEK_EXTRACT` directory.

ZIP downloads are easy to start with, but they do not provide a convenient update history. Use Git for maintained installations.

## Install the R packages

1. Open R or RStudio.
2. Run the following once:

   ```r
   install.packages(c(
     "shiny", "pdftools", "dplyr", "tidyr", "stringr", "purrr",
     "tibble", "readr", "janitor", "lubridate", "REDCapR",
     "assertthat", "validate", "logger", "glue", "fs", "here",
     "config", "yaml", "openxlsx", "rmarkdown", "progress",
     "bslib", "DT", "testthat", "keyring"
   ))
   ```

3. From the project directory, verify the runtime packages:

   ```powershell
   Rscript scripts\packages.R
   ```

4. If R reports a missing package, install that package and repeat the check.

> [!NOTE]
> `pdftools` depends on Poppler. On Windows and macOS, its R package installation commonly supplies what is needed. Linux users may need the distribution's Poppler development package before installing `pdftools`.

## Start the application

### Windows PowerShell

```powershell
.\run_app.ps1
```

### Windows Command Prompt

```bat
run_app.bat
```

### R or RStudio

Set the working directory to `VITEK_EXTRACT`, then run:

```r
shiny::runApp()
```

The application prints a local address similar to `http://127.0.0.1:xxxx`. Keep the R process running while using the application.

## Verify the installation

Run the automated test suite:

```powershell
.\run_tests.ps1
```

or:

```powershell
Rscript tests\testthat.R
```

At the time this guide was prepared, the suite contained 235 passing tests. On some Windows locale configurations, parser tests can emit transliteration warnings for non-ASCII characters even when all tests pass. Treat failures and warnings separately, and retain the complete test output with release records.

## Installation checklist

- [ ] R starts without an error.
- [ ] All packages in `scripts/packages.R` are installed.
- [ ] The Shiny landing page opens.
- [ ] The automated tests complete without failures.
- [ ] The working directory is the project root.
- [ ] The project is stored only on an approved computer or drive.
- [ ] Real VITEK reports have not been committed to Git.

## Next step

Continue to [Configure REDCap and the local keyring](02-redcap-setup.md).
