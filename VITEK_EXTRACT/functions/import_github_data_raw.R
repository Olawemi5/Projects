import_github_data_raw <- function(repo_url,
                                   dest_dir = here::here("data_raw"),
                                   quiet = FALSE) {
  if (is.null(repo_url) || !nzchar(trimws(repo_url))) {
    stop("Provide a GitHub repository URL.")
  }

  parsed <- parse_github_repo_url(repo_url)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  candidate_dirs <- github_candidate_dirs(parsed$subdir)

  selective_result <- tryCatch(
    download_github_pdfs_via_api(
      owner = parsed$owner,
      repo = parsed$repo,
      branch = parsed$branch,
      candidate_dirs = candidate_dirs,
      dest_dir = dest_dir,
      quiet = quiet
    ),
    error = function(e) {
      if (!quiet) {
        message("Selective GitHub PDF download failed: ", e$message)
        message("Falling back to repository ZIP download.")
      }
      NULL
    }
  )

  if (!is.null(selective_result) && selective_result$imported_n > 0) {
    return(c(
      list(method = "github_api_pdf_only"),
      selective_result
    ))
  }

  zip_result <- download_github_pdfs_via_zip(
    owner = parsed$owner,
    repo = parsed$repo,
    branch = parsed$branch,
    candidate_dirs = candidate_dirs,
    dest_dir = dest_dir,
    quiet = quiet
  )

  c(
    list(method = "github_zip_pdf_only"),
    zip_result
  )
}

normalize_repo_path <- function(path = "") {
  path <- path %||% ""
  path <- gsub("\\\\", "/", as.character(path))
  path <- gsub("^/+", "", path)
  path <- gsub("/+$", "", path)
  path
}

unique_dest_file <- function(dest_dir, filename) {
  dest_file <- file.path(dest_dir, filename)
  if (!file.exists(dest_file)) {
    return(dest_file)
  }

  stem <- tools::file_path_sans_ext(filename)
  ext <- tools::file_ext(filename)
  counter <- 1L

  repeat {
    candidate_name <- sprintf(
      "%s_%s%s",
      stem,
      counter,
      if (nzchar(ext)) paste0(".", ext) else ""
    )
    candidate <- file.path(dest_dir, candidate_name)
    if (!file.exists(candidate)) {
      return(candidate)
    }
    counter <- counter + 1L
  }
}

extract_pdf_files_from_zip <- function(zip_file, dest_dir) {
  zip_listing <- utils::unzip(zip_file, list = TRUE)
  if (nrow(zip_listing) == 0 || !"Name" %in% names(zip_listing)) {
    return(character())
  }

  pdf_entries <- zip_listing$Name[grepl("[.]pdf$", zip_listing$Name, ignore.case = TRUE)]
  if (length(pdf_entries) == 0) {
    return(character())
  }

  tmp_extract_dir <- tempfile("github_zip_inner_")
  dir.create(tmp_extract_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_file, files = pdf_entries, exdir = tmp_extract_dir)

  extracted_paths <- file.path(tmp_extract_dir, pdf_entries)
  extracted_paths <- extracted_paths[file.exists(extracted_paths)]
  if (length(extracted_paths) == 0) {
    return(character())
  }

  copied_files <- character()
  for (path in extracted_paths) {
    dest_file <- unique_dest_file(dest_dir, basename(path))
    if (file.copy(path, dest_file, overwrite = FALSE)) {
      copied_files <- c(copied_files, dest_file)
    }
  }

  copied_files
}

parse_github_repo_url <- function(repo_url) {
  cleaned <- trimws(repo_url)
  cleaned <- gsub("^[\"']|[\"']$", "", cleaned)
  cleaned <- sub("[?#].*$", "", cleaned)
  cleaned <- sub("^www\\.", "", cleaned, ignore.case = TRUE)
  cleaned <- sub("^github\\.com/", "https://github.com/", cleaned, ignore.case = TRUE)
  cleaned <- sub("[.]git$", "", cleaned)
  cleaned <- sub("/+$", "", cleaned)

  if (!grepl("^https?://github\\.com/", cleaned, ignore.case = TRUE)) {
    stop("Only GitHub repository URLs are supported right now.")
  }

  path_part <- sub("^https?://github\\.com/", "", cleaned, ignore.case = TRUE)
  segments <- strsplit(path_part, "/", fixed = TRUE)[[1]]

  if (length(segments) < 2) {
    stop("GitHub URL must include both owner and repository name.")
  }

  owner <- segments[[1]]
  repo <- segments[[2]]
  branch <- "main"
  subdir <- ""

  if (length(segments) >= 4 && identical(segments[[3]], "tree")) {
    branch <- segments[[4]]
    if (length(segments) > 4) {
      subdir <- normalize_repo_path(paste(segments[5:length(segments)], collapse = "/"))
    }
  }

  list(owner = owner, repo = repo, branch = branch, subdir = subdir)
}

github_candidate_dirs <- function(subdir = "") {
  subdir <- normalize_repo_path(subdir %||% "")
  subdir <- trimws(as.character(subdir))
  subdir <- subdir[!is.na(subdir) & nzchar(subdir)]

  if (length(subdir) > 0) {
    return(unique(subdir))
  }

  c("data_raw", "")
}

github_download_with_retry <- function(url,
                                       destfile,
                                       quiet = FALSE,
                                       timeout_sec = 300,
                                       retries = 3,
                                       methods = c("libcurl", "wininet", "auto")) {
  methods <- unique(methods)
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(timeout_sec, old_timeout %||% timeout_sec))

  last_error <- NULL

  for (method in methods) {
    for (attempt in seq_len(retries)) {
      result <- tryCatch(
        {
          utils::download.file(
            url = url,
            destfile = destfile,
            mode = "wb",
            quiet = quiet,
            method = method
          )
          TRUE
        },
        warning = function(w) {
          last_error <<- conditionMessage(w)
          FALSE
        },
        error = function(e) {
          last_error <<- conditionMessage(e)
          FALSE
        }
      )

      if (isTRUE(result) && file.exists(destfile) && file.info(destfile)$size > 0) {
        return(invisible(destfile))
      }
    }
  }

  stop("Download failed for ", url, ". Last error: ", last_error)
}

github_api_get_listing <- function(owner,
                                   repo,
                                   path = "",
                                   branch = "main",
                                   quiet = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for selective GitHub PDF download.")
  }

  encoded_path <- if (nzchar(path)) utils::URLencode(path, reserved = TRUE) else ""
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s?ref=%s",
    owner,
    repo,
    encoded_path,
    utils::URLencode(branch, reserved = TRUE)
  )

  tmp_json <- tempfile(fileext = ".json")
  github_download_with_retry(
    url = api_url,
    destfile = tmp_json,
    quiet = quiet
  )

  parsed <- jsonlite::fromJSON(tmp_json, simplifyDataFrame = TRUE)

  if (is.data.frame(parsed)) {
    return(tibble::as_tibble(parsed))
  }

  if (is.list(parsed) && identical(parsed$type, "file")) {
    return(tibble::as_tibble(parsed))
  }

  tibble::tibble()
}

collect_github_pdf_entries <- function(owner,
                                       repo,
                                       branch,
                                       start_dir = "",
                                       quiet = FALSE) {
  queue <- start_dir
  seen <- character()
  matched_entries <- list()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current %in% seen) {
      next
    }
    seen <- c(seen, current)

    listing <- github_api_get_listing(
      owner = owner,
      repo = repo,
      path = current,
      branch = branch,
      quiet = quiet
    )

    if (nrow(listing) == 0) {
      next
    }

    if (!all(c("type", "path", "name") %in% names(listing))) {
      next
    }

    dir_rows <- listing |> dplyr::filter(.data$type == "dir")
    if (nrow(dir_rows) > 0) {
      queue <- c(queue, dir_rows$path)
    }

    file_rows <- listing |>
      dplyr::filter(
        .data$type == "file",
        grepl("([.]pdf|[.]zip)$", .data$name, ignore.case = TRUE)
      )

    if (nrow(file_rows) > 0) {
      matched_entries[[length(matched_entries) + 1]] <- file_rows
    }
  }

  if (length(matched_entries) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(matched_entries) |>
    dplyr::distinct(.data$path, .keep_all = TRUE)
}

download_github_pdfs_via_api <- function(owner,
                                         repo,
                                         branch,
                                         candidate_dirs,
                                         dest_dir,
                                         quiet = FALSE) {
  matched_entries <- tibble::tibble()
  source_dir_used <- NA_character_

  for (candidate in candidate_dirs) {
    candidate_entries <- tryCatch(
      collect_github_pdf_entries(
        owner = owner,
        repo = repo,
        branch = branch,
        start_dir = candidate,
        quiet = quiet
      ),
      error = function(e) {
        if (!quiet) {
          message("GitHub API listing failed for '", candidate, "': ", e$message)
        }
        tibble::tibble()
      }
    )

    if (nrow(candidate_entries) > 0) {
      matched_entries <- candidate_entries
      source_dir_used <- if (nzchar(candidate)) candidate else "/"
      break
    }
  }

  if (nrow(matched_entries) == 0) {
    stop("No PDF files or ZIP files containing PDFs were found through the GitHub contents API.")
  }

  if (!"download_url" %in% names(matched_entries)) {
    stop("GitHub API response did not include download URLs for matched files.")
  }

  imported_files <- character()
  for (i in seq_len(nrow(matched_entries))) {
    download_url <- matched_entries$download_url[[i]]
    if (is.na(download_url) || !nzchar(download_url)) {
      next
    }

    file_name <- matched_entries$name[[i]]
    tmp_file <- tempfile(fileext = paste0(".", tools::file_ext(file_name)))
    github_download_with_retry(
      url = download_url,
      destfile = tmp_file,
      quiet = quiet,
      timeout_sec = 30,
      retries = 1,
      methods = c("libcurl", "auto")
    )

    if (grepl("[.]zip$", file_name, ignore.case = TRUE)) {
      imported_files <- c(imported_files, extract_pdf_files_from_zip(tmp_file, dest_dir))
    } else if (grepl("[.]pdf$", file_name, ignore.case = TRUE)) {
      dest_file <- unique_dest_file(dest_dir, basename(file_name))
      if (file.copy(tmp_file, dest_file, overwrite = FALSE)) {
        imported_files <- c(imported_files, dest_file)
      }
    }
  }

  if (length(imported_files) == 0) {
    stop("GitHub API located matching files, but no PDF files were downloaded successfully.")
  }

  list(
    repo_url = sprintf("https://github.com/%s/%s", owner, repo),
    owner = owner,
    repo = repo,
    branch = branch,
    source_dir = source_dir_used,
    imported_n = length(imported_files),
    imported_files = imported_files
  )
}

download_github_pdfs_via_zip <- function(owner,
                                         repo,
                                         branch,
                                         candidate_dirs,
                                         dest_dir,
                                         quiet = FALSE) {
  zip_url <- sprintf(
    "https://codeload.github.com/%s/%s/zip/refs/heads/%s",
    owner,
    repo,
    utils::URLencode(branch, reserved = TRUE)
  )

  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile("github_repo_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  github_download_with_retry(
    url = zip_url,
    destfile = tmp_zip,
    quiet = quiet,
    timeout_sec = 180,
    retries = 2,
    methods = c("libcurl", "wininet", "auto")
  )
  utils::unzip(tmp_zip, exdir = tmp_dir)

  extracted_root <- list.dirs(tmp_dir, full.names = TRUE, recursive = FALSE)
  if (length(extracted_root) == 0) {
    stop("GitHub archive was downloaded, but no files were extracted.")
  }

  repo_root <- extracted_root[[1]]
  zip_candidate_dirs <- unique(vapply(candidate_dirs, function(candidate) {
    if (nzchar(candidate)) {
      file.path(repo_root, candidate)
    } else {
      repo_root
    }
  }, character(1)))
  zip_candidate_dirs <- zip_candidate_dirs[dir.exists(zip_candidate_dirs)]

  pdf_files <- character()
  zip_files <- character()
  source_dir_used <- NA_character_

  for (candidate in zip_candidate_dirs) {
    pdfs <- list.files(
      candidate,
      pattern = "[.]pdf$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    zips <- list.files(
      candidate,
      pattern = "[.]zip$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )

    if (length(pdfs) > 0 || length(zips) > 0) {
      pdf_files <- pdfs
      zip_files <- zips
      source_dir_used <- candidate
      break
    }
  }

  if (length(pdf_files) == 0 && length(zip_files) == 0) {
    stop("No PDF files or ZIP files containing PDFs were found in the repository archive.")
  }

  imported_files <- character()

  if (length(pdf_files) > 0) {
    for (pdf_file in pdf_files) {
      dest_file <- unique_dest_file(dest_dir, basename(pdf_file))
      if (file.copy(from = pdf_file, to = dest_file, overwrite = FALSE)) {
        imported_files <- c(imported_files, dest_file)
      }
    }
  }

  if (length(zip_files) > 0) {
    for (zip_file in zip_files) {
      imported_files <- c(imported_files, extract_pdf_files_from_zip(zip_file, dest_dir))
    }
  }

  list(
    repo_url = sprintf("https://github.com/%s/%s", owner, repo),
    owner = owner,
    repo = repo,
    branch = branch,
    source_dir = source_dir_used,
    imported_n = length(imported_files),
    imported_files = imported_files
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
