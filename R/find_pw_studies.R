#' Find studies for pairwise comparison
#'
#' Filters dataset from input data (with study and intervention relabelled) to
#' studies that have arms for each of the two given comparators.
#'
#' @param dat Cleaned nma dat with study and intervention renamed
#' @param g1 First comparator for meta-analysis: int
#' @param g2 Second comparator for meta-analysis: placebo
#'
#' @return Vector of study ids.
#' @export
#'
#' @examples

find_pw_studies <- function(g1, g2, dat) {
  dat %>%
    filter(
      intervention %in% c(g1, g2)
    ) %>%
    select(study, intervention) %>%
    distinct() %>%
    pivot_wider(
      names_from = intervention,
      values_from = intervention
    ) %>%
    drop_na() %>%
    pull(study)

}

#' Get pw data
#'
#' Using [find_pw_studies], identify the number of studies.
#'
#' @section Dev notes
#'
#' Will need to remove timepoint if we combine change scores.
#'
#' @param dat Input model data [mod_dat].
#' @param outcome Outcome pain_sub, pain_mod, etc.
#' @param type Type of comparator: ad, etc.
#' @param timepoint Type of timepoint: post_int, change_score, etc.
#' @param g2 Placebo or other ref comparator.
#' @param g1 Intervention of comparison.
#'
#' @export
#' @return Wide dataset with placebo labelled as control.

get_pw_dat <- function(dat, outcome, timepoint, g1, g2) {

  this_studies <- find_pw_studies(g1, g2, dat)

  this_dat <-
    dat %>%
    filter(outcome == .env$outcome,
           timepoint == .env$timepoint,
           intervention %in% c(g1, g2),
           study %in% this_studies
           )

  this_dat %>%
    filter(intervention == g2) %>%
    rename_with(~ glue("{.x}_control"), everything()) %>%
    rename(study = study_control) %>%
    full_join(this_dat %>% filter(intervention == g1))


}
