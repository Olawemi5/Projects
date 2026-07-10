read_redcap_dictionary <- function(
    dictionary_file,
    quiet = FALSE
) {
  get_optional_column <- function(df, column_name) {
    if (column_name %in% names(df)) {
      return(df[[column_name]])
    }

    rep("", nrow(df))
  }

  if (missing(dictionary_file) || is.null(dictionary_file) || !nzchar(dictionary_file)) {
    stop("Provide a REDCap data dictionary file path.")
  }

  if (!file.exists(dictionary_file)) {
    stop("REDCap data dictionary file not found: ", dictionary_file)
  }

  dict_df <- readr::read_csv(
    dictionary_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- c("Variable / Field Name", "Field Label")
  missing_cols <- setdiff(required_cols, names(dict_df))

  if (length(missing_cols) > 0) {
    stop(
      "REDCap data dictionary is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  dict_df <- dict_df |>
    dplyr::transmute(
      redcap_field_name = .data$`Variable / Field Name`,
      redcap_field_label = .data$`Field Label`,
      field_type = dplyr::coalesce(get_optional_column(dict_df, "Field Type"), ""),
      form_name = dplyr::coalesce(get_optional_column(dict_df, "Form Name"), ""),
      field_choices = dplyr::coalesce(get_optional_column(dict_df, "Choices, Calculations, OR Slider Labels"), ""),
      text_validation_type = dplyr::coalesce(get_optional_column(dict_df, "Text Validation Type OR Show Slider Number"), "")
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ trimws(.x))
    ) |>
    dplyr::filter(.data$redcap_field_name != "")

  if (!quiet) {
    message("REDCap dictionary loaded successfully.")
    message("Fields available: ", nrow(dict_df))
  }

  dict_df
}
