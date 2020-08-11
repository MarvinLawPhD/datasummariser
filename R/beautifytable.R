#' beautifytable
#'
#' This function beautifies tables for Rmarkdown pdf outputs
#'
#' @param .data The dataframe or table which is being beautified
#' @param caption The caption for the table
#' @param size The font size. Default size is 7.
#' @param width The width of the first column of the table. Default size is 3 inches.
#'
#' @return Output in Rmarkdown following pdf knit
#' @export
#'
#' @examples
#' irisflowers <- iris %>%
#'   group_by(Species) %>%
#'   summarise(n = n())
#' beautifytable(irisflowers, "Flower Count")
beautifytable <- function(.data, caption, size = 7, width = "3in") {
  .data %>%
    kable(booktabs = T, linesep = "", caption = caption) %>%
    row_spec(c((1:(nrow(.data) / 2)) * 2), color = "black", background = "#d3d3d3") %>%
    column_spec(column = 1, width = width) %>%
    kable_styling(font_size = size, latex_options = "HOLD_position")
}
