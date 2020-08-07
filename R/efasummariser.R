#' efasummariser
#'
#' This function produces a dataframe with loadings and communalities for EFA produced using the psych package.
#' It can handle multiple EFA results from different samples, binding them together.
#' This function should be used either with a single EFA or with EFAs on the same variables but with different samples.
#'
#' @param threshold The threshold cutoff for showing factor loadings.
#' @param ... Dataframe of EFA loadings and communalities from all input EFAs
#'
#' @return A dataframe of EFA loadings and communalities with loadings lower than the threshold hidden
#' @export
#'
#' @examples
#' setosa <- psych::fa(iris %>%
#'                       filter(Species == "setosa") %>%
#'                       select_if(is.numeric),
#'                     fm = "gls")
#'
#' versicolor <- psych::fa(iris %>%
#'                           filter(Species == "versicolor") %>%
#'                           select_if(is.numeric),
#'                         fm = "gls")
#'
#' efa_loadings(threshold = 0.3, setosa, versicolor)
#'
efasummariser <- function(threshold = 0, ...) {

  b <- sapply(enquos(...), quo_name) %>% as.vector()
  a <- list(...)
  d <- data.frame(row.names = 1:nrow(a[[1]]$loadings))

  for (i in 1:length(a)){
    dd <- a[[i]]$loadings %>%
      unclass() %>% as.data.frame() %>% rownames_to_column() %>%
      left_join(a[[i]]$communality %>% as.data.frame()%>% rownames_to_column(), by = "rowname")

    ddc <- colnames(dd)

    dd <- dd %>%
      gather(key = key, val = val, matches("RC|PC|MR|ULS|WLS|PA|ML|MC|GLS")) %>%
      mutate(val = ifelse(abs(val)<threshold, "", format(round(val, 2), nsmall = 2)))%>%
      spread(key, val) %>%
      select(all_of(ddc))

    colnames(dd) <- c(b[i], paste0(colnames(dd)[2:(ncol(dd)-1)], "_", b[i]), paste0("h2_", b[i]))

    dd <- dd %>%
      mutate_at(vars(matches("h2")), list( ~format(round(., 2), nsmall = 2))) %>%
      mutate_if(is.numeric, as.character) %>%
      mutate_all(list(~str_replace(., "0.", ".")))

    d <- d %>% cbind(dd)
  }

  return(d)
}


