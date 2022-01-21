#' Calculate smd accounting for arms
#'
#' Using David's code.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

smd_calc <- function(df){
  df %>%
  mutate(
    intervention = fct_relevel(intervention, "placebo",
                               "cbt",
                               "cbt and milnacipran"),
    class = toupper(class)
  ) %>%
    arrange(outcome, timepoint, study, intervention) %>%
    group_by(outcome, timepoint, study) %>%
    mutate(
      arm = 1:n(),
      mi = sum(n) - n(),
      cmi = exp(lgamma(mi / 2) - log(sqrt(mi / 2)) - lgamma((mi - 1) /
                                                              2)),
      sdpool = sqrt(weighted.mean(sd ^ 2, n - 1)),
      smd = if_else(arm == 1, NA_real_, (mean - first(mean)) / sdpool * cmi),
      se_smd = if_else(arm == 1,
                       se / sdpool * cmi,
                       sqrt((n + first(
                         n
                       )) / (n * first(
                         n
                       )) + smd ^ 2 / (2 * (
                         n + first(n)
                       ))))
    ) %>%
    select(study, intervention, smd, se_smd, n, everything())
}
