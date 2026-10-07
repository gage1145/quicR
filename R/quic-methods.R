# S3 methods for the "quic" and "quic_metrics" classes.

# Small formatting helpers ------------------------------------------------

n_distinct_present <- function(x) length(unique(x[!is.na(x)]))

yes_no <- function(x) if (isTRUE(as.logical(x))) "yes" else "no"


# Coercion ----------------------------------------------------------------

#' Convert quicR objects to data frames
#'
#' Flattens a "quic" object into the long-format data frame (one row per well
#' per read), or extracts the metrics table from a "quic_metrics" object.
#'
#' @param x An object of class "quic" or "quic_metrics".
#' @param row.names,optional Ignored; included for compatibility with the generic.
#' @param ... Ignored.
#'
#' @return A data frame (or tibble, for `as_tibble()`).
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' x <- get_quic(file)
#' head(as.data.frame(x))
#'
#' @name quic-coercion
NULL

#' @rdname quic-coercion
#' @export
as_tibble.quic <- function(x, ...) {
  unnest(x$data, "data")
}

#' @rdname quic-coercion
#' @export
as.data.frame.quic <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(as_tibble(x))
}

#' @rdname quic-coercion
#' @export
as_tibble.quic_metrics <- function(x, ...) {
  as_tibble(x$data)
}

#' @rdname quic-coercion
#' @export
as.data.frame.quic_metrics <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$data)
}


# Printing ----------------------------------------------------------------

#' Print quicR objects
#'
#' @param x An object of class "quic", "quic_metrics", or one of their summaries.
#' @param n Integer; the number of rows of data to show.
#' @param ... Passed on to the tibble print method.
#'
#' @return `x`, invisibly.
#'
#' @name quic-print
NULL

#' @rdname quic-print
#' @export
print.quic <- function(x, n = 6, ...) {
  times <- unlist(lapply(x$data$data, `[[`, "time"))
  reads <- vapply(x$data$data, nrow, integer(1))

  cat("<quic> ", x$params$plate, "-well plate\n", sep = "")
  cat(
    nrow(x$data), " wells | ",
    n_distinct_present(x$data$sample), " samples | ",
    n_distinct_present(x$data$dilution), " dilutions | ",
    max(reads), " reads over ", format(min(times, na.rm = TRUE)), "-", format(max(times, na.rm = TRUE)), " h\n",
    sep = ""
  )
  cat(
    "Normalized to read ", x$params$norm_point,
    " | smoothed: ", yes_no(x$params$smooth),
    " | zeroed: ", yes_no(x$params$zero), "\n\n",
    sep = ""
  )
  print(x$data, n = n, ...)
  invisible(x)
}

#' @rdname quic-print
#' @export
print.quic_metrics <- function(x, n = 6, ...) {
  cat("<quic_metrics> ", x$params$plate, "-well plate\n", sep = "")
  cat(
    nrow(x$data), " groups by ", paste(x$params$by, collapse = ", "),
    " | threshold: ", x$params$threshold, "\n",
    sep = ""
  )
  if ("crossed" %in% names(x$data)) {
    cat("Crossed threshold: ", sum(x$data$crossed, na.rm = TRUE), " / ", nrow(x$data), "\n", sep = "")
  }
  cat("\n")
  print(x$data, n = n, ...)
  invisible(x)
}


# Summaries ---------------------------------------------------------------

#' Summarize quicR objects
#'
#' `summary()` on a "quic" object counts the wells and reads per sample and
#' dilution. On a "quic_metrics" object it averages each metric per sample and
#' dilution and reports how many wells crossed the threshold.
#'
#' @param object An object of class "quic" or "quic_metrics".
#' @param ... Ignored.
#'
#' @return An object of class "summary.quic" or "summary.quic_metrics": a list
#'   with a `table` tibble and the `params` of the summarized object.
#'
#' @import dplyr
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' x <- get_quic(file)
#' summary(x)
#' summary(calculate_metrics(x, threshold = 3))
#'
#' @name quic-summary
NULL

#' @rdname quic-summary
#' @export
summary.quic <- function(object, ...) {
  table <- object$data %>%
    mutate(reads = vapply(.data$data, nrow, integer(1))) %>%
    summarize(
      wells = n(),
      reads = max(.data$reads),
      .by = all_of(c("sample", "dilution"))
    )

  structure(
    list(table = table, params = object$params, n_wells = nrow(object$data)),
    class = "summary.quic"
  )
}

#' @rdname quic-print
#' @export
print.summary.quic <- function(x, ...) {
  cat("<quic summary> ", x$n_wells, " wells on a ", x$params$plate, "-well plate\n\n", sep = "")
  print(x$table, n = Inf, ...)
  invisible(x)
}

#' @rdname quic-summary
#' @export
summary.quic_metrics <- function(object, ...) {
  metrics <- intersect(c("mpr", "qr", "ms", "auc", "ttt", "raf"), names(object$data))
  groups <- intersect(c("sample", "dilution"), object$params$by)

  table <- object$data %>%
    summarize(
      wells = n(),
      crossed = sum(.data$crossed, na.rm = TRUE),
      across(all_of(metrics), ~ mean(.x, na.rm = TRUE)),
      .by = all_of(groups)
    )

  structure(
    list(table = table, params = object$params, metrics = metrics),
    class = "summary.quic_metrics"
  )
}

#' @rdname quic-print
#' @export
print.summary.quic_metrics <- function(x, ...) {
  cat(
    "<quic_metrics summary> threshold: ", x$params$threshold,
    " | ", sum(x$table$crossed), " / ", sum(x$table$wells), " wells crossed\n",
    "Metric columns are group means.\n\n",
    sep = ""
  )
  print(x$table, n = Inf, ...)
  invisible(x)
}


# Plotting ----------------------------------------------------------------

#' Plot quicR objects
#'
#' `autoplot()` returns a ggplot: a [plate_view()] for "quic" objects and a
#' [plot_metrics()] boxplot for "quic_metrics" objects. `plot()` draws that
#' same plot and returns it invisibly.
#'
#' @param object,x An object of class "quic" or "quic_metrics".
#' @param y Ignored; included for compatibility with the generic.
#' @param ... Passed on to [plate_view()] or [plot_metrics()].
#'
#' @return A ggplot object (invisibly, for `plot()`).
#'
#' @importFrom ggplot2 autoplot
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test2.xlsx",
#'   package = "quicR"
#' )
#' x <- get_quic(file)
#' plot(x, plot_deriv = FALSE)
#' plot(calculate_metrics(x, threshold = 3))
#'
#' @name quic-plot
NULL

#' @rdname quic-plot
#' @export
autoplot.quic <- function(object, ...) {
  plate_view(object, ...)
}

#' @rdname quic-plot
#' @export
autoplot.quic_metrics <- function(object, ...) {
  plot_metrics(object, ...)
}

#' @rdname quic-plot
#' @export
plot.quic <- function(x, y, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}

#' @rdname quic-plot
#' @export
plot.quic_metrics <- function(x, y, ...) {
  p <- autoplot(x, ...)
  print(p)
  invisible(p)
}
