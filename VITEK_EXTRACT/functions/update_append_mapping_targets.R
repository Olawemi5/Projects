update_append_mapping_targets <- function(
    append_mapping_file = here::here("docs", "redcap_append_variable_mapping.csv"),
    dictionary_file,
    output_file = here::here("docs", "redcap_append_variable_mapping_suggested.csv"),
    overwrite_existing = FALSE,
    quiet = FALSE
) {
  get_base_source_name <- function(source_name) {
    stringr::str_replace(source_name, "_[0-9]+$", "")
  }

  is_note_interpretation_field <- function(source_name) {
    stringr::str_detect(source_name, "_interpretation(_[0-9]+)?$") &
      !stringr::str_detect(source_name, "_interpretation_raw(_[0-9]+)?$")
  }

  is_low_confidence_skip_field <- function(source_name) {
    low_confidence_skip_bases <- c(
      "identified_organism",
      "updated_by",
      "id_card_type",
      "ast_card_type",
      "ast_testing_instrument",
      "identified_by"
    )

    get_base_source_name(source_name) %in% low_confidence_skip_bases
  }

  should_leave_unmapped_note_interpretation <- function(source_name,
                                                        match_score,
                                                        candidate_is_interpretation,
                                                        candidate_mentions_standard_note) {
    is_note_interpretation_field(source_name) & (
      is.na(match_score) |
        match_score <= 50 |
        !(isTRUE(candidate_is_interpretation) | isTRUE(candidate_mentions_standard_note))
    )
  }

  should_leave_unmapped_low_confidence <- function(source_name, match_score) {
    is_low_confidence_skip_field(source_name) & !is.na(match_score) & match_score <= 50
  }

  is_true_flag <- function(x) {
    value <- tolower(trimws(as.character(x)))
    value %in% c("true", "t", "1", "yes", "y")
  }

  is_parser_only_metadata_field <- function(source_name) {
    parser_only_metadata_bases <- c(
      "source_file",
      "biomerieux_customer",
      "report_title",
      "system_number",
      "printed_by",
      "last_updated",
      "report_version",
      "isolate_status",
      "patient_name",
      "bench",
      "id_card_barcode",
      "id_testing_instrument",
      "ast_testing_instrument",
      "setup_technologist",
      "bionumber",
      "selected_organism",
      "installed_vitek_version",
      "mic_interpretation_guideline",
      "therapeutic_interpretation_guideline",
      "aes_parameter_set_name",
      "aes_parameter_last_modified",
      "mcfarland",
      "id_card",
      "id_lot_number",
      "id_expires",
      "id_status",
      "id_analysis_time",
      "id_completed",
      "organism_origin",
      "organism_probability_pct",
      "id_confidence",
      "analysis_organisms_tests_to_separate",
      "contraindicating_typical_biopatterns",
      "susceptibility_card",
      "susceptibility_lot_number",
      "susceptibility_expires",
      "susceptibility_status",
      "susceptibility_analysis_time",
      "susceptibility_completed",
      "aes_last_modified",
      "aes_parameter_set",
      "aes_confidence_level",
      "id_analysis_messages"
    )

    get_base_source_name(source_name) %in% parser_only_metadata_bases
  }

  append_mapping <- load_append_redcap_mapping(
    mapping_file = append_mapping_file,
    quiet = quiet
  )

  dictionary_df <- read_redcap_dictionary(
    dictionary_file = dictionary_file,
    quiet = quiet
  )

  source_names <- unique(append_mapping$source_name)
  suggestions <- suggest_redcap_target_fields(
    source_names = source_names,
    dictionary_df = dictionary_df,
    top_n = 3,
    quiet = quiet
  )

  top_suggestions <- suggestions |>
    dplyr::filter(.data$match_rank == 1) |>
    dplyr::rename(
      suggested_target_redcap_name = redcap_field_name,
      suggested_target_redcap_label = redcap_field_label,
      target_field_type = field_type,
      target_field_choices = field_choices,
      target_text_validation_type = text_validation_type
    ) |>
    dplyr::mutate(
      suggested_target_redcap_name = dplyr::case_when(
        should_leave_unmapped_note_interpretation(
          .data$source_name,
          .data$match_score,
          .data$candidate_is_interpretation,
          .data$candidate_mentions_standard_note
        ) ~ NA_character_,
        should_leave_unmapped_low_confidence(.data$source_name, .data$match_score) ~ NA_character_,
        TRUE ~ .data$suggested_target_redcap_name
      ),
      suggested_target_redcap_label = dplyr::case_when(
        should_leave_unmapped_note_interpretation(
          .data$source_name,
          .data$match_score,
          .data$candidate_is_interpretation,
          .data$candidate_mentions_standard_note
        ) ~ NA_character_,
        should_leave_unmapped_low_confidence(.data$source_name, .data$match_score) ~ NA_character_,
        TRUE ~ .data$suggested_target_redcap_label
      ),
      match_status = dplyr::case_when(
        should_leave_unmapped_note_interpretation(
          .data$source_name,
          .data$match_score,
          .data$candidate_is_interpretation,
          .data$candidate_mentions_standard_note
        ) ~ "left_unmapped_low_confidence",
        should_leave_unmapped_low_confidence(.data$source_name, .data$match_score) ~ "left_unmapped_low_confidence",
        TRUE ~ .data$match_status
      )
    ) |>
    dplyr::select(
      source_name,
      suggested_target_redcap_name,
      suggested_target_redcap_label,
      target_field_type,
      target_field_choices,
      target_text_validation_type,
      match_score,
      match_method,
      match_status,
      candidate_is_interpretation,
      candidate_mentions_standard_note
    )

  updated_mapping <- append_mapping |>
    dplyr::left_join(top_suggestions, by = "source_name") |>
    dplyr::mutate(
      existing_target_redcap_name = .data$target_redcap_name,
      suggested_target_redcap_name = dplyr::case_when(
        is_parser_only_metadata_field(.data$source_name) ~ NA_character_,
        TRUE ~ .data$suggested_target_redcap_name
      ),
      suggested_target_redcap_label = dplyr::case_when(
        is_parser_only_metadata_field(.data$source_name) ~ NA_character_,
        TRUE ~ .data$suggested_target_redcap_label
      ),
      target_redcap_name = dplyr::case_when(
        is_parser_only_metadata_field(.data$source_name) ~ "",
        should_leave_unmapped_note_interpretation(
          .data$source_name,
          .data$match_score,
          .data$candidate_is_interpretation,
          .data$candidate_mentions_standard_note
        ) ~ "",
        should_leave_unmapped_low_confidence(.data$source_name, .data$match_score) ~ "",
        overwrite_existing ~ dplyr::coalesce(.data$suggested_target_redcap_name, .data$target_redcap_name),
        is.na(.data$target_redcap_name) | .data$target_redcap_name == "" ~ dplyr::coalesce(.data$suggested_target_redcap_name, .data$target_redcap_name),
        TRUE ~ .data$target_redcap_name
      ),
      match_status = dplyr::case_when(
        is_parser_only_metadata_field(.data$source_name) ~ "left_unmapped_parser_metadata",
        should_leave_unmapped_note_interpretation(
          .data$source_name,
          .data$match_score,
          .data$candidate_is_interpretation,
          .data$candidate_mentions_standard_note
        ) ~ "left_unmapped_low_confidence",
        should_leave_unmapped_low_confidence(.data$source_name, .data$match_score) ~ "left_unmapped_low_confidence",
        TRUE ~ .data$match_status
      )
    )

  patient_id_target <- updated_mapping |>
    dplyr::filter(.data$source_name == "patient_id") |>
    dplyr::transmute(
      reserved_target = dplyr::coalesce(
        dplyr::na_if(.data$target_redcap_name, ""),
        dplyr::na_if(.data$suggested_target_redcap_name, "")
      )
    ) |>
    dplyr::pull(.data$reserved_target)

  patient_id_target <- patient_id_target[!is.na(patient_id_target) & patient_id_target != ""]
  patient_id_target <- unique(patient_id_target)

  if (length(patient_id_target) > 0) {
    reserved_target <- patient_id_target[[1]]

    updated_mapping <- updated_mapping |>
      dplyr::mutate(
        suggested_target_redcap_name = dplyr::case_when(
          .data$source_name != "patient_id" &
            (.data$suggested_target_redcap_name == reserved_target | .data$existing_target_redcap_name == reserved_target) ~ NA_character_,
          TRUE ~ .data$suggested_target_redcap_name
        ),
        suggested_target_redcap_label = dplyr::case_when(
          .data$source_name != "patient_id" &
            (.data$suggested_target_redcap_name == reserved_target | .data$existing_target_redcap_name == reserved_target) ~ NA_character_,
          TRUE ~ .data$suggested_target_redcap_label
        ),
        target_redcap_name = dplyr::case_when(
          .data$source_name != "patient_id" & .data$target_redcap_name == reserved_target ~ "",
          TRUE ~ .data$target_redcap_name
        ),
        match_status = dplyr::case_when(
          .data$source_name != "patient_id" &
            (.data$suggested_target_redcap_name == reserved_target | .data$existing_target_redcap_name == reserved_target) ~ "reserved_for_patient_id",
          TRUE ~ .data$match_status
        )
      )
  }

  updated_mapping <- updated_mapping |>
    dplyr::select(-dplyr::any_of("existing_target_redcap_name"))

  readr::write_csv(updated_mapping, output_file)

  list(
    updated_mapping = updated_mapping,
    suggestions = suggestions,
    output_file = output_file
  )
}
