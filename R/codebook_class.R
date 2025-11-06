## not currently used, will probably remove

as_codebook <- function(x, 
                        name = name, 
                        label = label,
                        value_labels = value_labels,
                        type = NULL,
                        missing = NULL,
                        .keep = tidyselect::everything(),
                        .drop = NULL,
                        .lookups = NULL, 
                        .types = NULL,
                        .user_missing = NULL,
                        .miss_propagate = NULL,
                        .n_obs = NULL) {
  x_class <- class(x)
  if (!("data.frame" %in% x_class)) {
    cli::cli_abort(c(
      "!" = "{.code x} must be a data frame or data frame extension.",
      "i" = "{.code x} is of class {x_class}."
    ))
  }
  if (!missing(.drop)) {
    if (!missing(.keep)) {
      cli::cli_abort(c(
        "x" = "Only one of {.code .keep} and {.code .drop} should be specified."
      ))
    }
    .keep <- rlang::quo(!{{ .drop }})
  }
  class(x) <- c("li_codebook", x_class)
  x |>
    dplyr::select(
      name = {{ name }}, type = {{ type }}, label = {{ label }}, 
      value_labels = {{ value_labels }}, missing = {{ missing }},
      {{ .keep }}
    ) |> 
    set_attrs(
      lookups = .lookups, types = .types, user_missing = .user_missing, 
      miss_propagate = .miss_propagate, n_obs = .n_obs
    )
}
codebook <- function(name,
                      label = NA_character_,
                      value_labels = NA_character_,
                      ..., 
                      type = NULL,
                      missing = NULL,
                      .lookups = NULL, 
                      .types = NULL,
                      .user_missing = NULL, 
                      .miss_propagate = NULL,
                      .n_obs = NULL) {
  tibble::tibble(
      name = name, type = type, label = label, value_labels = value_labels, 
      missing = missing, ...
    ) |>
    as_codebook(
      .lookups = .lookups, .types = .types, .user_missing = .user_missing, 
      .miss_propagate = .miss_propagate, .n_obs = .n_obs
    )
}
is_codebook <- function(x) "li_codebook" %in% class(x)

cb_attributes <- c("lookups", "types", "user_missing", "miss_propagate")

bind_attrs <- function(which, objs) {
  attrs <- lapply(objs, attr, which)
  out <- unlist(attrs, recursive = FALSE)
  dups <- duplicated(names(out))
  if (any(dups)) {
    cli::cli_abort(c(
      "!" = "Values for attribute {.code {which}} are not unique across codebooks.",
      "i" = "Duplicated values: {toString(names(out)[dups])}"
    ))
  }
  out
}

cb_bind_rows <- function(..., .id = NULL) {
  out <- dplyr::bind_rows(..., .id = .id)
  dots <- list(...)
  if (length(dots) == 1 && rlang::is_bare_list(dots[[1]])) {
      dots <- dots[[1]]
  }
  cbs <- purrr::keep(dots, is_codebook)
  attr_out <- lapply(setNames(nm = cb_attributes), bind_attrs, objs = cbs)
  set_attrs(out, !!!attr_out)
}
