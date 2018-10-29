#' @author richard.kunze
get_filters <- function(input, context) {
  els <- grep(paste0("^filter_", context), names(input), value = TRUE)
  vals <- lapply(els, function(x) input[[x]])
  names(vals) <- gsub(paste0("^filter_", context, "_"), "", els)
  vals
}

#' @title text filter for data.frames
#'
#' @description text_filter filters a given data frame with the given filter
#' values. Names of the given values vector should be the indices of the
#' corresponding data frame columns. All filters are case-ignoring.
#'
#' @param df data frame to filter
#' @param values character array with the filter values, should have length of
#' data or being named
#'
#' @return text_filter: filtered data frame
#' @author richard.kunze
#' @export
text_filter <- function(df, values) {
  if (all(values == "")) return(df)
  values <- unlist(values)
  if (!is.null(names(values))) {
    values <- values[match(names(df), names(values))]
  } else {
    values[values == ""] <- NA
  }
  if (all(is.na(values))) return(df)
  return(text_filter_rec(df, values, seq(nrow(df))))
}

#' @author richard.kunze
text_filter_rec <- function(df, values, valid, depth = 1L) {
  if (depth > length(values)) return(df[valid, , drop = FALSE])
  if (!is.na(values[depth])) {
    index <- tryCatch(
      grep(values[depth], df[valid, depth], ignore.case = TRUE),
      error = function(e) !grepl("", df[valid, depth], fixed = TRUE)
    ) # catch invalid regex
    valid <- valid[index]
  }
  return(text_filter_rec(df, values, valid, depth + 1L))
}

#' @title numeric range filter for data.frames
#'
#' @description range_filter filters a given data frame with the given filter
#' values. Names of the given ranges vector should be the indices of the
#' corresponding data frame columns.
#'
#' @param df data frame to filter
#' @param ranges numeric (or convertable) vector with the filter ranges,
#' should have length of data or being named
#'
#' @return range_filter: filtered data frame
#' @author richard.kunze
#' @export
range_filter <- function(df, ranges) {
  if (is.atomic(ranges) && length(ranges) == 2L) ranges <- list(ranges)
  newRanges <- lapply(ranges, function(x) {
    vals <- suppressWarnings(as.numeric(x))
    if (length(x) == 2 && all(!is.na(vals))) vals
    else NA
  })
  if (!is.null(names(ranges))) {
    newRanges <- lapply(names(df), function(x) {
      if (x %in% names(ranges)) newRanges[[x]]
      else NA
    })
  }
  return(range_filter_rec(df, newRanges, seq(nrow(df))))
}

#' @author richard.kunze
range_filter_rec <- function(df, ranges, valid, depth = 1L) {
  if (depth > length(ranges) || length(valid) == 0) return(df[valid,])
  if (all(!is.na(ranges[depth]))) {
    tmp <- suppressWarnings(as.numeric(df[valid, depth]))
    valid <- valid[!is.na(tmp) & tmp >= ranges[[depth]][1L] & tmp <= ranges[[depth]][2L]]
  }
  return(range_filter_rec(df, ranges, valid, depth + 1L))
}
