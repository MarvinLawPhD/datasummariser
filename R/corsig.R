#' corsig
#'
#' This function follows stats::cor results to provide correlation results and
#' significance based on p-values in a table
#'
#' @param .data a dataframe or matrix with numeric values
#' @param decimals the number of decimals that are shown in the correlations
#' @param method methods used based on stats::cor methods
#' @param numbered whether to number the row and column names to condense results
#' @return a dataframe with correlations and significance based on p-values
#' @export
#'
#' @examples
#' irisnum <- iris %>%
#'   select_if(is.numeric)
#' corsig(irisnum, decimals = 3, method = "kendall", numbered = T)
#' corsig(irisnum, decimals = 2, method = "pearson", numbered = F)
#'
#'
#'
corsig <- function(.data, decimals = 2, method = "pearson", numbered = T) {
  d <- .data %>%
    cor(method = method, use = "pairwise.complete.obs") %>%
    round(decimals)

  dd <- d %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    gather(key = key, val = val, -rowname)

  vec <- c()
  for (i in 1:nrow(dd)) {
    vec <- c(vec, stats::cor.test(.data[, dd$rowname[i]], .data[, dd$key[i]])$p.value)
  }

  dd$pvalue <- vec

  cort <- dd %>%
    mutate(
      sign = ifelse(pvalue < .001, "***",
                    ifelse(pvalue < .01, "**",
                           ifelse(pvalue < .05, "*",
                                  ""
                           )
                    )
      ),
      val = paste0(str_replace(format(val, nsmall = 2), "0.", "."), sign)
    ) %>%
    select(-pvalue, -sign) %>%
    spread(key, val) %>%
    mutate(rowname = factor(rowname, levels = rownames(d))) %>%
    arrange(rowname) %>%
    select(rowname, rownames(d))

    colnames(.data) %in% cort$rowname

  cort[upper.tri(cort, diag = FALSE)] <- ""
  diag(cort[-1]) <- NA

  if (numbered) {
    rownames(cort) <- c(paste0(1:nrow(cort), ". ", cort$rowname))
    cort <- cort %>% select(-rowname)
    colnames(cort) <- 1:(ncol(cort))
  } else {
    rownames(cort) <- cort$rowname
    cort <- cort %>% select(-rowname)
  }


  message("Correlations produced using ", method, ".")
  return(cort)
}
