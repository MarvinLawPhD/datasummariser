#' Threshold Check
#'
#'This function tests whether there are unexpected values for a variable in a dataframe.
#'
#' @param .data dataframe which contains variable of interest
#' @param variable variable which is being tested for thresholds
#' @param vector vector containing expected values
#'
#' @return
#' If there are rows with values outside of the expected values, a dataframe with these rows will appear.
#' @export
#'
#' @examples
#' thresholdcheck(iris, Species, c("setosa"))
#' thresholdcheck(mtcars, cyl, 1:5)
#'
#'@seealso \code{\link{mass_ttest}}
#'
thresholdcheck <- function(.data, variable, vector) {

  variable <- enquo(variable)
  dat <- .data %>%
    select(!!variable) %>%
    filter(!(!!variable %in% vector))

  if(dim(dat)[1] != 0) {
    message(paste0("Warning: There appears to be ", dat %>% nrow(), " row/s outside of the threshold range:"))
    d <- dat %>% group_by(!!variable) %>% summarise(n = n(), .groups = "keep")
    return(d)
  } else message("No rows outside of the threshold range.")
}

