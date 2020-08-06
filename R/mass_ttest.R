#' Mass T-Test
#'
#' Runs t-test results between two groups for multiple variables
#'
#' @param .data dataframe that we're working with.
#' @param x grouping variable with two levels
#' @param y all numerical variables being compared by x
#' @param ... all parameters from t.test from the psych package
#' @return A tidy table of t-test results with Cohen's D on all variables in \code{varlist} between two groups in \code{variable}
#' @export
#' @examples
#' mass_ttest(mtcars, vs, c("cyl", "mpg"))
#' mass_ttest(mtcars, vs, c("cyl", "mpg"), var.equal = TRUE)
#'
#' @seealso \code{\link{thresholdcheck}}
mass_ttest <- function(.data, x, y, ...){

  x <- enquo(x)
  .y <- syms(y)

  d <- .data %>% mutate(!!x := as_factor(!!x))
  a <- d %>% filter(as.numeric(!!x) == 1) %>% select(!!!.y)
  b <- d %>% filter(as.numeric(!!x) == 2) %>% select(!!!.y)
  df <- tibble()

  for (i in 1:length(.y)){

    dd <- t.test(a[i], b[i], ...) %>%
      tidy()
    dd$cohens <- cohensD(a[i] %>% unlist(), b[i] %>% unlist())
    dd$variable <- colnames(a[i])

    df <- df %>%
      rbind(dd)
  }
  df <- df %>%
    select(variable, everything())
  return(df)
}

