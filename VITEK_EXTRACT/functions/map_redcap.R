# functions/map_redcap.R

map_redcap <- function(parsed_df,
                       mapping_file = here::here("docs", "redcap_variable_mapping.csv"),
                       required_redcap_fields = NULL,
                       default_fill = "",
                       quiet = FALSE) {
  
  if (is.null(parsed_df) || !is.data.frame(parsed_df)) {
    stop("parsed_df must be a non-null data frame.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required.")
  }
  
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' is required.")
  }
  
  resolved_mapping_file <- mapping_file

  if (!file.exists(resolved_mapping_file)) {
    fallback_candidates <- unique(c(
      file.path("docs", basename(mapping_file)),
      basename(mapping_file)
    ))

    existing_fallback <- fallback_candidates[file.exists(fallback_candidates)]

    if (length(existing_fallback) > 0) {
      resolved_mapping_file <- existing_fallback[[1]]
    } else {
      stop("Mapping file not found: ", mapping_file)
    }
  }
  
  # --------------------------------------------------
  # read mapping file
  # --------------------------------------------------
  mapping_df <- readr::read_csv(
    resolved_mapping_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  
  required_map_cols <- c("source_name", "redcap_name")
  missing_map_cols <- setdiff(required_map_cols, names(mapping_df))
  
  if (length(missing_map_cols) > 0) {
    stop(
      "Mapping file must contain these columns: ",
      paste(required_map_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_map_cols, collapse = ", ")
    )
  }
  
  mapping_df <- mapping_df |>
    dplyr::mutate(
      source_name = stringr::str_trim(source_name),
      redcap_name = stringr::str_trim(redcap_name)
    ) |>
    dplyr::filter(
      !is.na(source_name), source_name != "",
      !is.na(redcap_name), redcap_name != ""
    )
  
  # --------------------------------------------------
  # identify mapped columns that exist in parsed data
  # --------------------------------------------------
  usable_mapping <- mapping_df |>
    dplyr::filter(source_name %in% names(parsed_df))
  
  missing_source_cols <- setdiff(mapping_df$source_name, names(parsed_df))
  
  if (!quiet && length(missing_source_cols) > 0) {
    message(
      "These source columns were listed in the mapping file but not found in parsed_df: ",
      paste(missing_source_cols, collapse = ", ")
    )
  }
  
  # --------------------------------------------------
  # keep only mapped source columns
  # --------------------------------------------------
  redcap_df <- parsed_df |>
    dplyr::select(dplyr::all_of(usable_mapping$source_name))
  
  # --------------------------------------------------
  # rename to REDCap names
  # --------------------------------------------------
  rename_vector <- stats::setNames(
    usable_mapping$redcap_name,
    usable_mapping$source_name
  )
  
  redcap_df <- redcap_df |>
    dplyr::rename(!!!rename_vector)
  
  # --------------------------------------------------
  # ensure all columns are character
  # --------------------------------------------------
  redcap_df <- redcap_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # --------------------------------------------------
  # add required REDCap fields if supplied
  # --------------------------------------------------
  if (!is.null(required_redcap_fields)) {
    missing_required <- setdiff(required_redcap_fields, names(redcap_df))
    
    if (length(missing_required) > 0) {
      for (fld in missing_required) {
        redcap_df[[fld]] <- default_fill
      }
    }
    
    redcap_df <- redcap_df |>
      dplyr::select(dplyr::all_of(required_redcap_fields), dplyr::everything())
  }
  
  # --------------------------------------------------
  # replace NA with default_fill
  # --------------------------------------------------
  redcap_df <- redcap_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.x), default_fill, .x)
      )
    )
  
  if (!quiet) {
    message("REDCap mapping completed successfully.")
    message("Rows mapped: ", nrow(redcap_df))
    message("Columns mapped: ", ncol(redcap_df))
  }
  
  return(redcap_df)
}
