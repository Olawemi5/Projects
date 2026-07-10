# ==================================================
# scripts/setup_keyring.R
# One-time REDCap credential setup
# ==================================================

source(here::here("scripts", "packages.R"))

if (!requireNamespace("keyring", quietly = TRUE)) {
  stop("Package 'keyring' is required. Please install it first.")
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required. Please install it first.")
}

cat("\n")
cat("=============================================\n")
cat(" REDCap Credential Setup\n")
cat("=============================================\n\n")

cat("You will need:\n")
cat("1. Your REDCap API URL\n")
cat("   Example: https://redcap.myinstitution.org/api/\n\n")

cat("2. Your REDCap API token\n")
cat("   Obtain this from the REDCap project under:\n")
cat("   Applications -> API -> Generate API Token\n\n")

# --------------------------------------------------
# ask for REDCap URL
# --------------------------------------------------
redcap_uri <- readline(
  prompt = "Enter your REDCap API URL: "
)

while (redcap_uri == "") {
  redcap_uri <- readline(
    prompt = "REDCap API URL cannot be empty. Please enter it: "
  )
}

# --------------------------------------------------
# ask for username label
# --------------------------------------------------
username_label <- readline(
  prompt = "Enter a label for this credential [default: ifain_vitek]: "
)

if (username_label == "") {
  username_label <- "ifain_vitek"
}

service_name <- "redcap_api"

cat("\n")
cat("You will now be prompted to securely enter your REDCap API token.\n")
cat("The token will NOT be stored in the script.\n\n")

# --------------------------------------------------
# save token in keyring
# --------------------------------------------------
keyring::key_set(
  service = service_name,
  username = username_label
)

# --------------------------------------------------
# save non-secret config locally
# --------------------------------------------------
config_dir <- here::here("config")

if (!dir.exists(config_dir)) {
  dir.create(config_dir, recursive = TRUE)
}

config_file <- file.path(config_dir, "redcap_config.yml")

config_list <- list(
  redcap_uri = redcap_uri,
  keyring_service = service_name,
  keyring_username = username_label
)

yaml::write_yaml(config_list, config_file)

cat("\n")
cat("=============================================\n")
cat(" Setup completed successfully\n")
cat("=============================================\n\n")

cat("Configuration saved to:\n")
cat(config_file, "\n\n")

cat("Stored keyring credential:\n")
cat("Service : ", service_name, "\n", sep = "")
cat("Username: ", username_label, "\n\n", sep = "")

cat("You can now use these settings in upload_redcap().\n")