#' Wrangle pairwise
#'
#' @param dat A filtered dataset
#'
#' @return

pw_wide <- function(pw_dat) {
  m_type <- pw_dat$model_type %>% unique()

  pw_dat %>%
    pivot_wider(
      names_from = intervention,
      names_glue = "{.value}_{intervention}",
      values_from = ifelse(model_type == "lor", c(r, n), c(mean, sd, n)) 
    )
}