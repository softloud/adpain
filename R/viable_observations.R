#' Function to extract viable arms
#'
#' Takes set of observations and returns only those with at least two observations
#' per study.
#'
#' @param hpp_df A subset of `w_obs`.
#'
#' @export

viable_observations <- function(hpp_df) {
  viable <-
  hpp_df %>%
    group_by(study) %>%
    mutate(
      int_count = n_distinct(intervention)
    ) %>%
    select(int_count, everything()) %>%
    arrange(int_count, study) %>%
    filter(int_count > 1)  %>%
    ungroup()

  n_studies <- viable %>% pull(study) %>% unique() %>% length()

  if (n_studies > 1) {viable} else {tibble()}
}
