#' Extract parameter estimates as a dataframe
#'
#' @param nma multinma model
#'
#' @return
#' @export
#'
#' @examples

extract_nma_estimates <- function(nma) {
  nma %>%
    summary() %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    mutate(
      par = str_extract(parameter, "\\w+"),
      par_index = parameter %>%
        str_remove("^\\w+\\[") %>%
        str_remove("\\]")
    ) %>%
    select(par, par_index, everything()) %>%
    mutate(
      ci_lb = x2_5_percent,
      ci_ub = x97_5_percent
    ) %>%
    select(par, par_index, mean, sd, ci_lb, ci_ub)

}
