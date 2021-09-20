withr::with_dir(here::here(), {
  tar_load(m_key)
  tar_load(pw_results)
})

# make plots function

mod_plots <- function(target, index) {

  this_key <-
    m_key %>%
    filter(.data$index == .env$index,
           .data$target == .env$target)

  net_plot_path <-
  this_key %>%
    pull(netpath)

  glue("<img src=\"{net_plot_path}\" style = 'width:100%;' />") %>%
    cat()

  # forest plot
  forest_plot_path <-
    this_key %>%
    pull(forestpath)

  glue("<img src=\"{forest_plot_path}\" style = 'width:100%;' />") %>%
    cat()

  pw_results %>%
    filter(.data$index == .env$index,
           .data$target == .env$target)
}


pw_tab <- function(target, index) {

  this_key <-
    m_key %>%
    filter(.data$index == .env$index,
           .data$target == .env$target)

  # pairwise
  this_key %>%
    select(outcome, timepoint, type, condition) %>%
    left_join(pw_results)

  }


# make timepoints

maketimepoints <- function(specified_type, specified_key) {
    specified_key %>%
      filter(type == specified_type) %>%
      select(index, target, timepoint) %>%
    pmap(
      .f = function(index, target, timepoint) {
        cat("\n\n####",  timepoint, "\n\n")

        mod_plots(target, index)

        cat("\n\n")

      }
    )
}

# make chapter function

makechapter <- function(this_outcome) {
  outcome_key <- m_key %>% filter(outcome == this_outcome)

  outcome_text <- outcome_key %>%
    pull(label) %>% unique()

  cat("## NMA: ", outcome_text, "\n\n")

  nma_key <- outcome_key %>%
    filter(target == "m_o_tt")

  nma_types <- nma_key  %>%
    pull(type) %>% unique()

  nma_types %>%
    map(function(this_type) {
      cat("### Type: ", this_type, "\n\n")

      maketimepoints(this_type, nma_key)

    })


# conditions --------------------------------------------------------------

conditions <-
    outcome_key %>%
    filter(!is.na(condition)) %>%
    pull(condition) %>% unique()

  conditions %>%
    map(function(this_condition) {
      cat("## Condition: ", this_condition, "\n\n")

      condition_key <-
        outcome_key %>%
        filter(condition == this_condition)


      condition_types <-
        condition_key %>%
        pull(type) %>% unique()

      condition_types %>%
        map(function(this_condition_type) {
          cat("### Type: ", this_condition_type, "\n\n")
          maketimepoints(this_condition_type, condition_key)

        })

    })

}

makechapter_notype <- function(outcome) {
  cat("## NMA: ",
      #outcome,
      "\n\n")

  cat("Network meta-analysis results across all conditions, classes, and doses.\n")

  # output analyses for all condition subgroups
  m_key %>%
    filter(outcome == outcome,
           target == "m_o_tt") %>%
    select(index, target, type, timepoint) %>%
    pmap(
      .f = function(index, target, type, timepoint) {
        cat("\n\n### ", type, "at", timepoint, "\n\n")

        mod_plots(target, index)

        cat("\n\n")

      }
    )


  cat("\n\n## Conditions\n\n")

  # output analyses for all condition subgroups
  m_key %>%
    filter(outcome == outcome,
           str_detect(target, "m_con")) %>%
    select(index, target, type, timepoint, condition) %>%
    pmap(
      .f = function(index,
                    target,
                    type,
                    timepoint,
                    condition) {
        cat("\n\n### ", type, "at", timepoint, "for", condition, "\n\n")

        mod_plots(target, index)

        cat("\n\n")

      }
    )

}

system("cp -rf images ../docs/")
