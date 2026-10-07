# Labelled Data Integration
#
# Interoperability helpers converting haven and labelled data frame columns
# with value labels into standard R factors while preserving variable labels.

#########
# HAVEN AND LABELLED COERCION
# Data frame sweep and factor conversion honoring SPSS/Stata value mappings.

#' Coerce labelled columns in data frame into standard factors
#' @keywords internal
#' @noRd
.as_simtab_data <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  changed <- FALSE
  out <- data
  for (nm in names(out)) {
    converted <- .as_simtab_factor(out[[nm]])
    if (!identical(converted, out[[nm]])) {
      out[[nm]] <- converted
      changed <- TRUE
    }
  }

  if (isTRUE(changed)) out else data
}

#' Convert single labelled vector into factor using attached value labels
#' @keywords internal
#' @noRd
.as_simtab_factor <- function(x) {
  if (!inherits(x, c("haven_labelled", "labelled", "labelled_spss"))) {
    return(x)
  }

  value_labels <- attr(x, "labels", exact = TRUE)
  if (is.null(value_labels) || is.null(names(value_labels)) ||
      any(!nzchar(names(value_labels)))) {
    return(x)
  }

  var_label <- attr(x, "label", exact = TRUE)
  raw <- unclass(x)
  raw_key <- as.character(raw)
  label_key <- stats::setNames(names(value_labels), as.character(unname(value_labels)))

  values <- raw_key
  matched <- raw_key %in% names(label_key)
  values[matched] <- unname(label_key[raw_key[matched]])
  values[is.na(raw)] <- NA_character_

  ordered_labels <- names(value_labels)[order(unname(value_labels), na.last = TRUE)]
  extras <- setdiff(unique(values[!is.na(values)]), ordered_labels)
  out <- factor(values, levels = c(ordered_labels, extras))
  if (!is.null(var_label)) {
    attr(out, "label") <- var_label
  }
  out
}
