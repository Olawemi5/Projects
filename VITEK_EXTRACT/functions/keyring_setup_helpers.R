redcap_config_file <- function() {
  file.path("config", "redcap_config.yml")
}

write_yaml_with_final_newline <- function(x, file) {
  yaml::write_yaml(x, file)

  existing_lines <- readLines(file, warn = FALSE)
  writeLines(existing_lines, file, useBytes = TRUE)

  invisible(file)
}

check_redcap_keyring_status <- function(
    config_file = redcap_config_file(),
    key_get_fn = NULL
) {
  if (is.null(key_get_fn)) {
    key_get_fn <- function(service, username) {
      keyring::key_get(service = service, username = username)
    }
  }

  status <- list(
    configured = FALSE,
    config_exists = FALSE,
    credential_exists = FALSE,
    redcap_uri = "",
    keyring_service = "",
    keyring_username = "",
    message = "Keyring setup is required on this computer."
  )

  if (!file.exists(config_file)) {
    return(status)
  }

  status$config_exists <- TRUE

  cfg <- tryCatch(
    suppressWarnings(yaml::read_yaml(config_file)),
    error = function(e) NULL
  )

  required_fields <- c("redcap_uri", "keyring_service", "keyring_username")

  if (is.null(cfg) || length(setdiff(required_fields, names(cfg))) > 0) {
    status$message <- "Saved REDCap configuration is incomplete. Please run keyring setup again."
    return(status)
  }

  status$redcap_uri <- trimws(as.character(cfg$redcap_uri %||% ""))
  status$keyring_service <- trimws(as.character(cfg$keyring_service %||% ""))
  status$keyring_username <- trimws(as.character(cfg$keyring_username %||% ""))

  if (!requireNamespace("keyring", quietly = TRUE)) {
    status$message <- "Package 'keyring' is not installed yet. Install it to complete local credential setup."
    return(status)
  }

  token <- tryCatch(
    key_get_fn(
      service = status$keyring_service,
      username = status$keyring_username
    ),
    error = function(e) NULL
  )

  if (!is.null(token) && nzchar(trimws(as.character(token)))) {
    status$configured <- TRUE
    status$credential_exists <- TRUE
    status$message <- sprintf(
      "Keyring is configured locally for %s (%s). This setup stays on this computer until it is reconfigured or removed.",
      status$keyring_username,
      status$keyring_service
    )
  } else {
    status$message <- sprintf(
      "Saved REDCap configuration was found for %s, but the local keyring credential is missing. Set it up again for this computer or enter a new REDCap configuration.",
      status$keyring_username
    )
  }

  status
}

save_redcap_keyring_setup <- function(
    redcap_uri,
    keyring_service,
    keyring_username,
    api_token,
    config_file = redcap_config_file(),
    key_set_with_value_fn = NULL
) {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("Package 'keyring' is required for local credential setup.")
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for local credential setup.")
  }

  if (is.null(key_set_with_value_fn)) {
    key_set_with_value_fn <- function(service, username, password) {
      keyring::key_set_with_value(
        service = service,
        username = username,
        password = password
      )
    }
  }

  redcap_uri <- trimws(as.character(redcap_uri %||% ""))
  keyring_service <- trimws(as.character(keyring_service %||% ""))
  keyring_username <- trimws(as.character(keyring_username %||% ""))
  api_token <- trimws(as.character(api_token %||% ""))

  if (!nzchar(redcap_uri)) {
    stop("REDCap API URL cannot be empty.")
  }

  if (!nzchar(keyring_service)) {
    stop("Keyring service cannot be empty.")
  }

  if (!nzchar(keyring_username)) {
    stop("Keyring username cannot be empty.")
  }

  if (!nzchar(api_token)) {
    stop("REDCap API token cannot be empty.")
  }

  config_dir <- dirname(config_file)
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  key_set_with_value_fn(
    service = keyring_service,
    username = keyring_username,
    password = api_token
  )

  cfg <- list(
    redcap_uri = redcap_uri,
    keyring_service = keyring_service,
    keyring_username = keyring_username
  )

  write_yaml_with_final_newline(cfg, config_file)

  list(
    config_file = config_file,
    config = cfg,
    status = check_redcap_keyring_status(config_file = config_file)
  )
}

clear_redcap_keyring_setup <- function(
    config_file = redcap_config_file(),
    key_delete_fn = NULL
) {
  if (is.null(key_delete_fn)) {
    key_delete_fn <- function(service, username) {
      keyring::key_delete(service = service, username = username)
    }
  }

  cfg <- NULL
  if (file.exists(config_file)) {
    cfg <- tryCatch(
      yaml::read_yaml(config_file),
      error = function(e) NULL
    )
  }

  service_name <- trimws(as.character(cfg$keyring_service %||% ""))
  username_label <- trimws(as.character(cfg$keyring_username %||% ""))

  if (
    nzchar(service_name) &&
      nzchar(username_label) &&
      requireNamespace("keyring", quietly = TRUE)
  ) {
    tryCatch(
      key_delete_fn(service = service_name, username = username_label),
      error = function(e) NULL
    )
  }

  if (file.exists(config_file)) {
    file.remove(config_file)
  }

  check_redcap_keyring_status(config_file = config_file)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
