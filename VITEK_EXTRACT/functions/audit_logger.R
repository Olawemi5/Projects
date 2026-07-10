initialize_audit_log <- function(log_file) {
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(log_file)) {
    utils::write.csv(
      data.frame(
        timestamp = character(),
        stage = character(),
        status = character(),
        details = character(),
        stringsAsFactors = FALSE
      ),
      log_file,
      row.names = FALSE
    )
  }

  invisible(log_file)
}

append_audit_log <- function(log_file, stage, status, details) {
  initialize_audit_log(log_file)

  entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stage = stage,
    status = status,
    details = details,
    stringsAsFactors = FALSE
  )

  existing <- read_audit_log(log_file)
  updated <- rbind(existing, entry)
  utils::write.csv(updated, log_file, row.names = FALSE)
  invisible(entry)
}

read_audit_log <- function(log_file) {
  if (!file.exists(log_file)) {
    return(data.frame())
  }

  utils::read.csv(log_file, stringsAsFactors = FALSE)
}
