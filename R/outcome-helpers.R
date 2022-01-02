#' Get direction of improvement
#'
#' @param o outcome
#'
#' @export

outcome_dir <- function(o) {
  outcome_key %>%
    filter(outcome == o) %>%
    pull(direction_of_improvement)
}



#' Get model type
#'
#' @inheritParams outcome_dir
#'
#' @return lor or smd
#' @export

outcome_mod <- function(o) {
  outcome_key %>%
    filter(outcome == o) %>%
    pull(model_type)
}


#' Title
#'
#' @param o dichotomous outcome
#'
#' @return
#' @export
#'
#' @examples
#' outcome_label("adverse")
outcome_label <- function(o) {
  outcome_key %>%
    filter(outcome == o) %>%
    pull(outcome_label)
}

#' Title
#'
#' @param o
#'
#' @return
#' @export
#'
#' @examples
outcome_acr <- function(o) {
  sof_acr %>%
    filter(outcome == o) %>%
    pull(acr)
}

#' Title
#'
#' @param t
#'
#' @return
#' @export
#'
#' @examples

timepoint_label <- function(t) {
  timepoint_key %>%
    filter(timepoint == t) %>%
    pull(timepoint_label)
}

#' Title
#'
#' @param t
#'
#' @return
#' @export
#'
#' @examples

type_label <- function(t) {
  type_key %>%
  filter(type == t) %>%
    pull(type_label)
}
