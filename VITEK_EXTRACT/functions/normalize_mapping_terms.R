normalize_mapping_terms <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  x |>
    tolower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

tokenize_mapping_terms <- function(x) {
  normalized <- normalize_mapping_terms(x)
  strsplit(normalized, " ", fixed = TRUE)
}
