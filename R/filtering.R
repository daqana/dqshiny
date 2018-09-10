#' Get filter values from a given context
#'
#' @description get_filters receives the values from all filters within the given context and returns them in a list.
#'
#' @param input shiny input environment
#'
#' @return getFilter: list of filter values
#' @export
#' @rdname filter_row
get_filters <- function(input, context) {
  els <- grep(paste0("^filter_", context), names(input), value = TRUE)
  vals <- lapply(els, function(x) input[[x]])
  names(vals) <- gsub(paste0("^filter_", context, "_"), "", els)
  vals
}

#' Filters a data frame of text values
#'
#' @description text_filter filters a given data frame with the given filter values. Names of the given values vector should be
#' the indices of the corresponding data frame columns. All filters are case-ignoring.
#'
#' @param df data frame to filter
#' @param values character array with the filter values, should have length of data or being named
#'
#' @return text_filter: filtered data frame
#' @export
#' @rdname filter_row
text_filter <- function(df, values) {
  if (all(values == "")) return(df)
  newValues <- values
  if (!is.null(names(values))) {
    newValues <- sapply(names(df), function(x) {
      if (x %in% names(values)) values[x]
      else ""
    }, USE.NAMES = F)
  }
  return(text_filter_rec(df, newValues, seq(nrow(df))))
}

#' @author richard.kunze
text_filter_rec <- function(df, values, valid, depth = 1) {
  if (depth > length(values)) return(df[valid,])
  if (values[depth] != "") valid <- valid[grepl(values[depth], df[valid, depth], ignore.case = T)]
  return(text_filter_rec(df, values, valid, depth + 1))
}

#' Filters a data frame of text values
#'
#' @description range_filter filters a given data frame with the given filter values. Names of the given ranges vector should be
#' the indices of the corresponding data frame columns.
#'
#' @param ranges numeric (or numeric makeable) vector with the filter values, should have length of data or being named
#'
#' @return range_filter: filtered data frame
#' @export
#' @rdname filter_row
range_filter <- function(df, ranges) {
  if (is.atomic(ranges) && length(ranges) == 2) ranges <- list(ranges)
  newRanges <- lapply(ranges, function(x) {
    if (length(x) == 2 && suppressWarnings(all(!is.na(vals <- as.numeric(x))))) vals
    else NA
  })
  if (!is.null(names(ranges))) {
    newRanges <- lapply(names(df), function(x) {
      if (x %in% names(ranges)) ranges[[x]]
      else NA
    })
  }
  return(range_filter_rec(df, newRanges, seq(nrow(df))))
}

#' @author richard.kunze
range_filter_rec <- function(df, ranges, valid, depth = 1) {
  if (depth > length(ranges) || length(valid) == 0) return(df[valid,])
  if (all(!is.na(ranges[depth]))) {
    tmp <- suppressWarnings(as.numeric(df[valid, depth]))
    valid <- valid[!is.na(tmp) & tmp >= ranges[[depth]][1] & tmp <= ranges[[depth]][2]]
  }
  return(range_filter_rec(df, ranges, valid, depth + 1))
}
