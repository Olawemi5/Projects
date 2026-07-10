# functions/clean_data.R

clean_data <- function(redcap_df,
                       quiet = FALSE,
                       collapse_patient_isolates = TRUE) {
  
  if (is.null(redcap_df) || !is.data.frame(redcap_df)) {
    stop("redcap_df must be a non-null data frame.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  
  df <- redcap_df |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.character)
    )

  empty_values <- c("", "(Empty)", "(EMPTY)", "empty", NA_character_)
  is_empty_value <- function(x) {
    is.na(x) | x %in% empty_values
  }

  normalize_empty_values <- function(x) {
    x[is_empty_value(x)] <- ""
    x
  }

  pick_first_non_empty <- function(x) {
    x <- as.character(x)
    x <- x[!is_empty_value(x)]
    if (length(x) == 0) "" else x[[1]]
  }

  normalize_patient_family_id <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringr::str_replace(x, "_[^_]+$", "")
    digits_only <- gsub("[^0-9]", "", x)

    dplyr::if_else(
      digits_only == "",
      "",
      ifelse(nchar(digits_only) > 6, substr(digits_only, 1, 6), digits_only)
    )
  }

  get_isolate_slot <- function(col_name) {
    if (stringr::str_detect(col_name, "_[0-9]+$")) {
      as.integer(stringr::str_extract(col_name, "[0-9]+$"))
    } else {
      1L
    }
  }

  get_base_name <- function(col_name) {
    stringr::str_replace(col_name, "_[0-9]+$", "")
  }

  build_slot_name <- function(base_name, slot) {
    if (slot <= 1) base_name else paste0(base_name, "_", slot)
  }

  # --------------------------------------------------
  # derive participant identifier from patient_id +
  # report_version so grouping follows the numbered ID
  # convention already used in the earlier workflow.
  # example:
  # patient_id = 11-00-224
  # report_version = 2 of 3
  # new patient_id = 11-00-224_2
  # --------------------------------------------------
  if (all(c("patient_id", "report_version") %in% names(df))) {
    report_version_first <- stringr::str_extract(df$report_version, "^\\d+")

    df <- df |>
      dplyr::mutate(
        patient_id = dplyr::if_else(
          !is.na(.data$patient_id) & .data$patient_id != "" &
            !is.na(report_version_first) & report_version_first != "",
          paste0(.data$patient_id, "_", report_version_first),
          .data$patient_id
        )
      )
  }

  # --------------------------------------------------
  # clean antibiotic result fields
  # --------------------------------------------------
  
  interpretation_raw_cols <- names(df)[
    stringr::str_detect(names(df), "_interpretation_raw$")
  ]
  
  interpretation_cols <- names(df)[
    stringr::str_detect(names(df), "_interpretation$") &
      !stringr::str_detect(names(df), "_interpretation_raw$")
  ]
  
  cols_to_clean <- unique(c(
    interpretation_raw_cols,
    interpretation_cols
  ))
  
  if (length(cols_to_clean) > 0) {
    df <- df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols_to_clean),
          ~ dplyr::case_when(
            .x %in% c("(Empty)", "(EMPTY)") ~ "empty",
            .x == "Standard" ~ "standard",
            .x == "User modified" ~ "user_modified",
            .x == "AES modified" ~ "aes_modified",
            .x == "+" ~ "POS",
            .x == "-" ~ "NEG",
            TRUE ~ .x
          )
        )
      )
  }

  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), normalize_empty_values))

  # --------------------------------------------------
  # collapse multiple isolate rows per patient into
  # one wide REDCap-ready row using distinct organisms.
  # --------------------------------------------------
  if (isTRUE(collapse_patient_isolates) &&
      all(c("patient_id", "identified_organism") %in% names(df))) {
    repeatable_cols <- names(df)[
      !stringr::str_detect(names(df), "_[23]$")
    ]

    expected_suffix_cols <- unique(unlist(lapply(repeatable_cols, function(col_name) {
      c(build_slot_name(col_name, 2L), build_slot_name(col_name, 3L))
    })))

    missing_suffix_cols <- setdiff(expected_suffix_cols, names(df))
    if (length(missing_suffix_cols) > 0) {
      for (col_name in missing_suffix_cols) {
        df[[col_name]] <- ""
      }
    }

    suffix_target_cols <- names(df)[
      stringr::str_detect(names(df), "_[23]$")
    ]

    family_id <- normalize_patient_family_id(df$patient_id)

    patient_group_id <- ifelse(
      !is_empty_value(family_id),
      family_id,
      ifelse(
        is_empty_value(df$patient_id),
        paste0("__ROW_", seq_len(nrow(df))),
        df$patient_id
      )
    )

    patient_groups <- split(df, patient_group_id)

    collapsed_rows <- lapply(patient_groups, function(patient_df) {
      patient_df <- patient_df |>
        dplyr::mutate(
          organism_key__ = dplyr::if_else(
            is_empty_value(.data$identified_organism),
            "__EMPTY_ORGANISM__",
            .data$identified_organism
          )
        )

      distinct_organisms <- unique(patient_df$organism_key__)
      distinct_organisms <- distinct_organisms[distinct_organisms != "__EMPTY_ORGANISM__"]

      # Keep same-organism repeated reports as separate upload rows. Only
      # families containing multiple distinct organisms are collapsed into
      # base/_2/_3 isolate slots.
      if (length(distinct_organisms) <= 1L) {
        return(
          patient_df |>
            dplyr::select(-dplyr::any_of("organism_key__"))
        )
      }

      template <- patient_df[1, , drop = FALSE]
      template[] <- lapply(template, function(col) rep("", length(col)))

      shared_cols <- setdiff(names(df), unique(c(repeatable_cols, suffix_target_cols)))
      if (length(shared_cols) > 0) {
        for (col in shared_cols) {
          template[[col]] <- pick_first_non_empty(patient_df[[col]])
        }
      }

      if ("patient_id" %in% names(template)) {
        template$patient_id <- pick_first_non_empty(patient_df$patient_id)
      }

      used_slots <- 0L
      for (organism in distinct_organisms) {
        organism_rows <- patient_df[patient_df$organism_key__ == organism, , drop = FALSE]
        used_slots <- used_slots + 1L

        if (used_slots > 3L) {
          break
        }

        for (base_col in repeatable_cols) {
          target_col <- build_slot_name(base_col, used_slots)
          if (!target_col %in% names(template)) {
            next
          }

          template[[target_col]] <- pick_first_non_empty(organism_rows[[base_col]])
        }
      }

      overflow_flag <- length(distinct_organisms) > 3L
      same_organism_duplicate_flag <- nrow(patient_df) > length(distinct_organisms)

      if ("comments" %in% names(template)) {
        comment_bits <- c()
        existing_comment <- pick_first_non_empty(patient_df$comments)
        if (nzchar(existing_comment)) {
          comment_bits <- c(comment_bits, existing_comment)
        }
        if (same_organism_duplicate_flag) {
          comment_bits <- c(comment_bits, "Repeated same-organism reports were suppressed from _2/_3 isolate slots.")
        }
        if (overflow_flag) {
          comment_bits <- c(comment_bits, "More than three distinct organisms were found for this patient; extra organisms were not mapped.")
        }
        template$comments <- paste(unique(comment_bits[nzchar(comment_bits)]), collapse = " | ")
      }

      if ("validation_status" %in% names(template) && overflow_flag) {
        template$validation_status <- "Flagged"
      }

      if ("processed_at" %in% names(template)) {
        template$processed_at <- pick_first_non_empty(patient_df$processed_at)
      }

      if ("parser_version" %in% names(template)) {
        template$parser_version <- pick_first_non_empty(patient_df$parser_version)
      }

      template <- template |>
        dplyr::select(-dplyr::any_of("organism_key__"))

      extra_same_organism_rows <- patient_df |>
        dplyr::group_by(.data$organism_key__) |>
        dplyr::slice(-1) |>
        dplyr::ungroup() |>
        dplyr::filter(.data$organism_key__ != "__EMPTY_ORGANISM__") |>
        dplyr::mutate(
          comments = dplyr::case_when(
            is_empty_value(.data$comments) ~ "Repeated same-organism report retained as a separate upload row.",
            TRUE ~ paste(.data$comments, "Repeated same-organism report retained as a separate upload row.", sep = " | ")
          )
        ) |>
        dplyr::select(-dplyr::any_of("organism_key__"))

      dplyr::bind_rows(template, extra_same_organism_rows)
    })

    df <- dplyr::bind_rows(collapsed_rows)
  }
  
  if (!quiet) {
    message("clean_data() completed.")
    message("Columns cleaned: ", length(cols_to_clean))
    if ("patient_id" %in% names(df)) {
      message("Rows after patient-level isolate collapse: ", nrow(df))
    }
  }
  
  return(df)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
