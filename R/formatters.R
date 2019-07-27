#' pagebreak
#'
#' A function that includes a pagebreak in rmarkdown,
#' in the correct format depending on the chosen output
#' of the document (pdf or html)
#'
#' To include in a markdown document use:
#' `r pagebreak()`
#'
#' @family formatters
#' @export
#'
pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\newpage")
  else
    return('<div style="page-break-before: always;" />')
}
