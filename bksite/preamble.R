withr::with_dir(here::here(), {
  tar_load(m_model_key)
  tar_load(plot_write_key)
  
})

# make plots function

mod_plots <- function(plot_index) {
  # network
  # m_net_plot[[plot_index]] %>% print()
  net_plot_path <-
    plot_write_key %>%
    filter(.data$plot_index == .env$plot_index) %>%
    pull(netpath)
  
  glue("<img src=\"{net_plot_path}\" style = 'width:100%;' />") %>%
    cat()
  
  # forest plot
  forest_plot_path <-
    plot_write_key %>%
    filter(.data$plot_index == .env$plot_index) %>%
    pull(forestpath)
  
  glue("<img src=\"{forest_plot_path}\" style = 'width:100%;' />") %>%
    cat()
  
  # pairwise
}


# make chapter function

makechapter <- function(params = list(outcome = "pain_int")) {
  cat("## NMA: all in for", params$outcome, "\n\n")
  
  cat("Network meta-analysis results across all conditions, classes, and doses.\n")
  
  # output analysis for all in
  m_model_key %>%
    filter(outcome == params$outcome,
           condition == "all") %>%
    pull(plot_index) %>%
    mod_plots()
  
  
  cat("\n\n## Conditions\n\n")
  
  # output analyses for all condition subgroups
  m_model_key %>%
    filter(outcome == params$outcome,
           condition != "all") %>%
    select(condition, plot_index) %>%
    pmap(
      .f = function(condition, plot_index) {
        cat("\n\n### ", condition, "\n\n")
        
        mod_plots(plot_index)
        
        cat("\n\n")
        
      }
    )
  
}

system("cp -rf images ../docs/")
