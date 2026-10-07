# Constructors and validators for the "quic" and "quic_metrics" S3 classes.
#
# Both classes share the same shape:
#   data   - a tibble
#   params - a named list of the settings used to build the object, which is
#            carried forward so downstream functions don't need them re-supplied.

new_quic <- function(data, params) {
  stopifnot(is.data.frame(data), is.list(params))
  structure(list(data = data, params = params), class = "quic")
}

validate_quic <- function(x) {
  required <- c("well", "sample", "dilution", "data")
  missing_cols <- setdiff(required, names(x$data))
  if (length(missing_cols) > 0) {
    stop("A quic object is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.list(x$data$data)) {
    stop("The `data` column of a quic object must be a list-column of time series.", call. = FALSE)
  }
  if (!x$params$plate %in% c(96, 384)) {
    stop("`plate` must be either 96 or 384.", call. = FALSE)
  }
  x
}

new_quic_metrics <- function(data, params) {
  stopifnot(is.data.frame(data), is.list(params))
  structure(list(data = data, params = params), class = "quic_metrics")
}

validate_quic_metrics <- function(x) {
  missing_cols <- setdiff(x$params$by, names(x$data))
  if (length(missing_cols) > 0) {
    stop("A quic_metrics object is missing grouping column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  x
}
