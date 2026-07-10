suggest_redcap_target_fields <- function(source_names,
                                         dictionary_df,
                                         top_n = 3,
                                         quiet = FALSE) {
  extract_slot_number <- function(x) {
    suffix <- stringr::str_match(x, "_([0-9]+)$")[, 2]
    slot <- suppressWarnings(as.integer(suffix))
    slot[is.na(slot)] <- 1L
    slot
  }

  extract_substantive_tokens <- function(x) {
    generic_tokens <- c(
      "interpretation", "raw", "mic", "patient", "id", "identifier",
      "participant", "record", "field", "code", "sir", "source", "name",
      "value", "result", "results", "organism", "isolate", "number", "no",
      "study", "subject", "minimum", "inhibitory", "concentration",
      "susceptible", "intermediate", "resistant", "susceptibility",
      "standard", "user", "modified", "note", "notes"
    )

    tokens <- unique(unlist(tokenize_mapping_terms(x)))
    tokens <- tokens[nzchar(tokens)]
    tokens[!tokens %in% generic_tokens]
  }

  build_field_semantics <- function(field_name, field_label) {
    combined <- normalize_mapping_terms(paste(field_name, field_label))
    patient_id_patterns <- c(
      "\\bpatient id\\b",
      "\\bpatient identifier\\b",
      "\\bparticipant id\\b",
      "\\bparticipant identifier\\b",
      "\\bparticipant number\\b",
      "\\bparticipant no\\b",
      "\\bparticipant record\\b",
      "\\brecord id\\b",
      "\\brecord identifier\\b",
      "\\bstudy id\\b",
      "\\bstudy identifier\\b",
      "\\bsubject id\\b",
      "\\bsubject identifier\\b",
      "\\bperson id\\b",
      "\\bperson identifier\\b",
      "\\bunique id\\b",
      "\\bunique identifier\\b"
    )
    patient_id_regex <- paste(patient_id_patterns, collapse = "|")
    slot_number_from_name <- extract_slot_number(field_name)
    slot_number_from_label <- suppressWarnings(as.integer(
      stringr::str_match(tolower(field_label), "\\b(?:organism|isolate)\\s+([0-9]+)\\b")[, 2]
    ))
    slot_number <- ifelse(
      !is.na(slot_number_from_label),
      slot_number_from_label,
      slot_number_from_name
    )
    slot_number[is.na(slot_number)] <- 1L

    list(
      slot_number = slot_number,
      is_isolate_identifier = normalize_mapping_terms(field_name) == "isolate" |
        stringr::str_detect(normalize_mapping_terms(field_name), "^isolate [0-9]+$") |
        stringr::str_detect(combined, "\\b(?:organism|isolate)\\s+[0-9]+\\s+id\\b"),
      is_interpretation_raw = stringr::str_detect(field_name, "_interpretation_raw(_[0-9]+)?$"),
      is_interpretation = stringr::str_detect(field_name, "_interpretation(_[0-9]+)?$") &&
        !stringr::str_detect(field_name, "_interpretation_raw(_[0-9]+)?$"),
      is_mic = stringr::str_detect(field_name, "_mic(_[0-9]+)?$"),
      mentions_clindamycin = stringr::str_detect(
        combined,
        "\\bclindamycin\\b|\\bclinda\\b"
      ),
      mentions_inducible_clindamycin = stringr::str_detect(
        combined,
        "\\binducible\\b.*\\bclindamycin\\b|\\binducible\\b.*\\bclinda\\b|\\bclindamycin\\b.*\\binducible\\b|\\bclinda\\b.*\\binducible\\b"
      ) |
        stringr::str_detect(
          combined,
          "\\binducible clindamycin resistance\\b|\\binducible clinda resistance\\b|\\bd test\\b|\\bdzone\\b|\\bd zone\\b"
        ),
      mentions_plain_clindamycin_only = stringr::str_detect(
        combined,
        "\\bclindamycin\\b|\\bclinda\\b"
      ) &
        !(
          stringr::str_detect(
            combined,
            "\\binducible\\b.*\\bclindamycin\\b|\\binducible\\b.*\\bclinda\\b|\\bclindamycin\\b.*\\binducible\\b|\\bclinda\\b.*\\binducible\\b"
          ) |
            stringr::str_detect(
              combined,
              "\\binducible clindamycin resistance\\b|\\binducible clinda resistance\\b|\\bd test\\b|\\bdzone\\b|\\bd zone\\b"
            )
        ),
      is_patient_identifier = stringr::str_detect(combined, patient_id_regex),
      mentions_sir = stringr::str_detect(
        combined,
        "\\b(s\\s*i\\s*r|sir|susceptible|intermediate|resistant|susceptibility|interpretation code|s i r code)\\b"
      ),
      mentions_standard_note = stringr::str_detect(
        combined,
        "\\b(standard|aes modified|user modified|modified by aes|modified by user)\\b"
      ),
      is_comment_field = stringr::str_detect(
        combined,
        "\\bcomment\\b|\\bcomments\\b|\\bremark\\b|\\bremarks\\b|\\bnote\\b|\\bnotes\\b"
      )
    )
  }

  if (is.null(source_names) || length(source_names) == 0) {
    stop("Provide at least one source_name.")
  }

  if (is.null(dictionary_df) || !is.data.frame(dictionary_df)) {
    stop("dictionary_df must be a non-null data frame.")
  }

  required_cols <- c("redcap_field_name", "redcap_field_label")
  missing_cols <- setdiff(required_cols, names(dictionary_df))
  if (length(missing_cols) > 0) {
    stop(
      "dictionary_df is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  optional_cols <- c("field_type", "field_choices", "text_validation_type")
  for (col_name in optional_cols) {
    if (!col_name %in% names(dictionary_df)) {
      dictionary_df[[col_name]] <- ""
    }
  }

  if ("field_type" %in% names(dictionary_df)) {
    dictionary_df <- dictionary_df |>
      dplyr::filter(tolower(trimws(.data$field_type)) != "descriptive")
  }

  if (nrow(dictionary_df) == 0) {
    stop("No eligible REDCap fields found for matching after excluding descriptive fields.")
  }

  dictionary_scored <- dictionary_df |>
    dplyr::mutate(
      dictionary_row_index = dplyr::row_number(),
      normalized_field_name = normalize_mapping_terms(.data$redcap_field_name),
      normalized_field_label = normalize_mapping_terms(.data$redcap_field_label),
      name_tokens = purrr::map(.data$redcap_field_name, function(value) {
        tokens <- unique(unlist(tokenize_mapping_terms(value)))
        tokens[nzchar(tokens)]
      }),
      label_tokens = purrr::map(.data$redcap_field_label, function(value) {
        tokens <- unique(unlist(tokenize_mapping_terms(value)))
        tokens[nzchar(tokens)]
      }),
      substantive_name_tokens = purrr::map(.data$redcap_field_name, extract_substantive_tokens),
      substantive_label_tokens = purrr::map(.data$redcap_field_label, extract_substantive_tokens),
      semantics = purrr::map2(.data$redcap_field_name, .data$redcap_field_label, build_field_semantics),
      candidate_is_patient_identifier = purrr::map_lgl(.data$semantics, "is_patient_identifier"),
      candidate_is_isolate_identifier = purrr::map_lgl(.data$semantics, "is_isolate_identifier"),
      candidate_is_interpretation_raw = purrr::map_lgl(.data$semantics, "is_interpretation_raw"),
      candidate_is_interpretation = purrr::map_lgl(.data$semantics, "is_interpretation"),
      candidate_is_mic = purrr::map_lgl(.data$semantics, "is_mic"),
      candidate_mentions_clindamycin = purrr::map_lgl(.data$semantics, "mentions_clindamycin"),
      candidate_mentions_inducible_clindamycin = purrr::map_lgl(.data$semantics, "mentions_inducible_clindamycin"),
      candidate_mentions_plain_clindamycin_only = purrr::map_lgl(.data$semantics, "mentions_plain_clindamycin_only"),
      candidate_mentions_sir = purrr::map_lgl(.data$semantics, "mentions_sir"),
      candidate_mentions_standard_note = purrr::map_lgl(.data$semantics, "mentions_standard_note"),
      candidate_is_comment_field = purrr::map_lgl(.data$semantics, "is_comment_field")
    )

  score_one_source <- function(source_name) {
    source_norm <- normalize_mapping_terms(source_name)
    source_tokens <- unique(unlist(tokenize_mapping_terms(source_name)))
    source_tokens <- source_tokens[nzchar(source_tokens)]
    source_substantive_tokens <- extract_substantive_tokens(source_name)
    source_semantics <- build_field_semantics(source_name, source_name)

    scored <- dictionary_scored |>
      dplyr::mutate(
        name_exact = as.integer(.data$normalized_field_name == source_norm),
        label_exact = as.integer(.data$normalized_field_label == source_norm),
        name_distance = utils::adist(source_norm, .data$normalized_field_name)[1, 1],
        label_distance = utils::adist(source_norm, .data$normalized_field_label)[1, 1],
        token_overlap_name = purrr::map_int(
          .data$name_tokens,
          ~ length(intersect(source_tokens, .x))
        ),
        token_overlap_label = purrr::map_int(
          .data$label_tokens,
          ~ length(intersect(source_tokens, .x))
        ),
        substantive_overlap_name = purrr::map_int(
          .data$substantive_name_tokens,
          ~ length(intersect(source_substantive_tokens, .x))
        ),
        substantive_overlap_label = purrr::map_int(
          .data$substantive_label_tokens,
          ~ length(intersect(source_substantive_tokens, .x))
        ),
        positional_bonus = dplyr::case_when(
          isTRUE(source_semantics$is_patient_identifier) &
            .data$candidate_is_patient_identifier &
            .data$dictionary_row_index <= 10 ~ 45,
          isTRUE(source_semantics$is_patient_identifier) &
            .data$candidate_is_patient_identifier &
            .data$dictionary_row_index <= 20 ~ 20,
          TRUE ~ 0
        ),
        slot_bonus = dplyr::case_when(
          isTRUE(source_semantics$is_isolate_identifier) &
            .data$candidate_is_isolate_identifier &
            purrr::map_int(.data$semantics, "slot_number") == source_semantics$slot_number ~ 95,
          isTRUE(source_semantics$is_isolate_identifier) &
            .data$candidate_is_isolate_identifier &
            purrr::map_int(.data$semantics, "slot_number") != source_semantics$slot_number ~ -55,
          source_semantics$slot_number > 1L &
            purrr::map_int(.data$semantics, "slot_number") == source_semantics$slot_number ~ 70,
          source_semantics$slot_number > 1L &
            purrr::map_int(.data$semantics, "slot_number") == 1L ~ -40,
          source_semantics$slot_number == 1L &
            purrr::map_int(.data$semantics, "slot_number") > 1L ~ -20,
          TRUE ~ 0
        ),
        semantics_bonus = dplyr::case_when(
          isTRUE(source_semantics$is_patient_identifier) & .data$candidate_is_patient_identifier ~ 140,
          isTRUE(source_semantics$is_patient_identifier) &
            stringr::str_detect(.data$normalized_field_name, "\\b(id|identifier|number|record)\\b") ~ 35,
          isTRUE(source_semantics$is_patient_identifier) ~ -30,
          isTRUE(source_semantics$is_interpretation_raw) & .data$candidate_is_interpretation_raw ~ 110,
          isTRUE(source_semantics$is_interpretation_raw) & .data$candidate_mentions_sir ~ 80,
          isTRUE(source_semantics$is_interpretation_raw) & .data$candidate_is_mic ~ -140,
          isTRUE(source_semantics$is_interpretation_raw) ~ -35,
          isTRUE(source_semantics$is_interpretation) & .data$candidate_is_interpretation ~ 80,
          isTRUE(source_semantics$is_interpretation) & .data$candidate_mentions_sir ~ 60,
          isTRUE(source_semantics$is_interpretation) & .data$candidate_mentions_standard_note ~ 45,
          isTRUE(source_semantics$is_interpretation) & .data$candidate_is_mic ~ -120,
          isTRUE(source_semantics$is_interpretation) ~ -20,
          isTRUE(source_semantics$is_mic) & .data$candidate_is_mic ~ 100,
          isTRUE(source_semantics$is_mic) & (.data$candidate_is_interpretation_raw | .data$candidate_is_interpretation) ~ -120,
          isTRUE(source_semantics$is_comment_field) & .data$candidate_is_comment_field ~ 150,
          isTRUE(source_semantics$is_comment_field) & .data$candidate_is_patient_identifier ~ -220,
          isTRUE(source_semantics$is_comment_field) ~ -35,
          TRUE ~ 0
        ),
        substantive_bonus = dplyr::case_when(
          length(source_substantive_tokens) == 0 ~ 0,
          substantive_overlap_label > 0 ~ substantive_overlap_label * 55,
          substantive_overlap_name > 0 ~ substantive_overlap_name * 45,
          (isTRUE(source_semantics$is_interpretation_raw) || isTRUE(source_semantics$is_mic)) &
            length(source_substantive_tokens) > 0 ~ -80,
          TRUE ~ 0
        ),
        clindamycin_bonus = dplyr::case_when(
          isTRUE(source_semantics$mentions_inducible_clindamycin) &
            .data$candidate_mentions_inducible_clindamycin &
            isTRUE(source_semantics$is_interpretation_raw) &
            .data$candidate_mentions_sir ~ 200,
          isTRUE(source_semantics$mentions_inducible_clindamycin) &
            .data$candidate_mentions_inducible_clindamycin &
            isTRUE(source_semantics$is_mic) &
            .data$candidate_is_mic ~ 200,
          isTRUE(source_semantics$mentions_inducible_clindamycin) &
            .data$candidate_mentions_inducible_clindamycin ~ 130,
          isTRUE(source_semantics$mentions_inducible_clindamycin) &
            .data$candidate_mentions_plain_clindamycin_only ~ -220,
          isTRUE(source_semantics$mentions_inducible_clindamycin) &
            .data$candidate_mentions_clindamycin &
            !.data$candidate_mentions_inducible_clindamycin ~ -160,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_plain_clindamycin_only) &
            .data$candidate_mentions_plain_clindamycin_only &
            isTRUE(source_semantics$is_interpretation_raw) &
            .data$candidate_mentions_sir ~ 220,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_plain_clindamycin_only) &
            .data$candidate_mentions_plain_clindamycin_only &
            isTRUE(source_semantics$is_mic) &
            .data$candidate_is_mic ~ 220,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_plain_clindamycin_only) &
            .data$candidate_mentions_plain_clindamycin_only ~ 140,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_plain_clindamycin_only) &
            .data$candidate_mentions_inducible_clindamycin ~ -240,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_clindamycin) &
            .data$candidate_mentions_clindamycin &
            !.data$candidate_mentions_inducible_clindamycin ~ 70,
          !isTRUE(source_semantics$mentions_inducible_clindamycin) &
            isTRUE(source_semantics$mentions_clindamycin) &
            .data$candidate_mentions_inducible_clindamycin ~ -180,
          TRUE ~ 0
        ),
        match_score = (name_exact * 100) +
          (label_exact * 95) +
          (token_overlap_name * 10) +
          (token_overlap_label * 7) -
          pmin(name_distance, 50) -
          (pmin(label_distance, 50) / 2) +
          positional_bonus +
          slot_bonus +
          semantics_bonus +
          substantive_bonus +
          clindamycin_bonus,
        match_method = dplyr::case_when(
          slot_bonus >= 60 ~ "isolate_slot_semantics",
          positional_bonus > 0 & .data$candidate_is_patient_identifier ~ "patient_identifier_top_of_dictionary",
          semantics_bonus >= 120 & .data$candidate_is_patient_identifier ~ "patient_identifier_semantics",
          name_exact == 1 ~ "exact_field_name",
          label_exact == 1 ~ "exact_field_label",
          semantics_bonus >= 120 & .data$candidate_is_comment_field ~ "comment_field_semantics",
          substantive_bonus >= 45 ~ "substantive_token_overlap",
          semantics_bonus >= 80 & .data$candidate_mentions_sir ~ "susceptibility_semantics",
          semantics_bonus >= 80 & .data$candidate_is_interpretation_raw ~ "interpretation_semantics",
          semantics_bonus >= 70 & .data$candidate_is_mic ~ "mic_semantics",
          token_overlap_name > 0 ~ "token_overlap_field_name",
          token_overlap_label > 0 ~ "token_overlap_field_label",
          TRUE ~ "distance_only"
        )
      ) |>
      dplyr::arrange(dplyr::desc(.data$match_score), .data$name_distance, .data$label_distance) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::mutate(
        source_name = source_name,
        match_rank = dplyr::row_number(),
        match_status = dplyr::case_when(
          .data$match_rank == 1 &
            isTRUE(source_semantics$is_patient_identifier) &
            .data$match_score > 90 ~ "high_confidence",
          .data$match_rank == 1 & .data$match_method %in% c("exact_field_name", "exact_field_label") ~ "high_confidence",
          .data$match_rank == 1 & .data$match_score >= 20 ~ "needs_review",
          TRUE ~ "low_confidence"
        )
      ) |>
      dplyr::select(
        source_name,
        redcap_field_name,
        redcap_field_label,
        field_type,
        field_choices,
        text_validation_type,
        match_score,
        match_method,
        match_status,
        candidate_is_interpretation,
        candidate_mentions_standard_note,
        match_rank
      )

    scored
  }

  suggestions <- dplyr::bind_rows(lapply(source_names, score_one_source))

  if (!quiet) {
    message("Field suggestions generated for ", length(source_names), " source fields.")
  }

  suggestions
}
