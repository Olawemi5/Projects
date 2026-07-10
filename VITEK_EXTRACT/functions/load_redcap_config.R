load_redcap_config <- function(
    config_file = here::here("config", "redcap_config.yml")
) {
  
  if (!file.exists(config_file)) {
    stop(
      "REDCap config file not found. Please run scripts/setup_keyring.R first."
    )
  }
  
  cfg <- suppressWarnings(yaml::read_yaml(config_file))
  
  required_fields <- c(
    "redcap_uri",
    "keyring_service",
    "keyring_username"
  )
  
  missing_fields <- setdiff(required_fields, names(cfg))
  
  if (length(missing_fields) > 0) {
    stop(
      "Missing required settings in config file: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  return(cfg)
}
