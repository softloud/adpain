withr::with_dir(here::here(), {
  tar_load(m_key)
  tar_load(w_obs_outcome)
  tar_load(obs_excluded)
  tar_load(obs_dat)

  source("R/hpp_themes.R")
})

# make plots function

mod_plots <- function(target, index) {
  net_plot_path <-
    m_key %>%
    filter(.data$index == .env$index,
           .data$target == .env$target) %>%
    pull(netpath)

  glue("<img src=\"{net_plot_path}\" style = 'width:100%;' />") %>%
    cat()

  # # forest plot
  # forest_plot_path <-
  #   m_key %>%
  #   filter(.data$index == .env$index,
  #          .data$target == .env$target
  #   ) %>%
  #   pull(forestpath)
  #
  # glue("<img src=\"{forest_plot_path}\" style = 'width:100%;' />") %>%
  #   cat()

  # pairwise
}





# make timepoints

maketimepoints <- function(type) {

}

# make chapter function

makechapter <- function(outcome) {
  # consider removing nma from heading once other information goes in
  cat("## Outcome NMA")

  cat("## Subgroup NMA: condition")
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
