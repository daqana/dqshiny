#' rhandsontable renderer
#'
#' @description dq_hot_date_renderer: Renderer to show rhandsontable dates in
#' proper formatting.
#'
#' @return character containing js renderer
#' @author richard.kunze
#' @export
#' @examples df <- data.frame(empty = rep(c("value", ""), 5),
#'   html = paste0("<div style='background:#ff",sprintf("%x",25*1:10),"ff'>&nbsp;</div>"),
#'   date = seq(from = Sys.Date(), by = "days", length.out = 10),
#'   stringsAsFactors = FALSE)
#'
#' hot <- rhandsontable::rhandsontable(df, rowHeaders = NULL)
#' hot <- rhandsontable::hot_col(hot, 1, renderer = dq_hot_empty_renderer())
#' hot <- rhandsontable::hot_col(hot, 2, renderer = dq_hot_html_renderer())
#' hot <- rhandsontable::hot_col(hot, 3, renderer = dq_hot_date_renderer())
#' hot
dq_hot_date_renderer <- function() {
  "function (instance, td, row, col, prop, value, cellProperties) {
     if (value !== null && typeof value !== 'undefined') {
       var val = value;
       value = val.substr(6) + '-' + val.substr(0, 2) + '-' + val.substr(3, 2);
     }
     Handsontable.renderers.NumericRenderer.apply(this, arguments);
  }"
}

#' @description dq_hot_empty_renderer: Renderer to highlight empty cells in
#' rhandsontable.
#' @param renderer rhandsontable base renderer to be adjusted, can be one of
#' ("Autocomplete", "Base", "Checkbox", "Date", "Dropdown", "Html", "Numeric",
#' "Password", "Text", "Time")
#' @export
#' @rdname dq_hot_date_renderer
dq_hot_empty_renderer <- function(renderer = "Autocomplete") {
  if (length(renderer) != 1L || !(renderer %in% c(
    "Autocomplete", "Base", "Checkbox", "Date", "Dropdown",
    "Html", "Numeric", "Password", "Text", "Time"))) {
    renderer <- "Autocomplete"
  }
  paste0("function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.", renderer, "Renderer.apply(this, arguments);
    if (value.toString() == ' ' || value.toString() == '') {
      td.style.backgroundColor = 'pink';
    }
  }")
}

#' @description dq_hot_html_renderer: Renderer to replace missing "html"
#' rhandsontable renderer.
#' @export
#' @rdname dq_hot_date_renderer
dq_hot_html_renderer <- function() {
  "function (instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.HtmlRenderer.apply(this, arguments);
   }"
}

#' @description dq_hot_selectize_renderer: Renderer to properly display multiple
#' selectize options.
#' @export
#' @rdname dq_hot_date_renderer
dq_hot_selectize_renderer <- function() {
  "function (instance, td, row, col, prop, value, cellProperties) {
    var settings = cellProperties.selectizeOptions;
    if (value && settings && (settings.maxItems === null || settings.maxItems > 1)) {
      var arr = value.toString().split(',');
      value = arr.map(x => '<span class=\"selectize_item\">' + x + '</span>').join('');
    }
    Handsontable.renderers.HtmlRenderer.apply(this, arguments);
  }"
}
