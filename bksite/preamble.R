withr::with_dir(here::here(), {
  tar_load(m_key)
  tar_load(w_obs_outcome)
  tar_load(obs_excluded)
  tar_load(obs_dat)
  tar_load(pw_results)
  source("R/hpp_themes.R")
  source("R/msg_mine.R")
})

library(knitr)

conflicted::conflict_prefer("filter", "dplyr")


# the hard way ------------------------------------------------------------

analysis <- function(subgroup,
                     outcome,
                     type,
                     timepoint,
                     condition = NULL) {
  pw_dat <-
    pw_results %>%
    filter(
      .data$outcome == .env$outcome,
      .data$subgroup == .env$subgroup,
      .data$type == .env$type,
      .data$timepoint == .env$timepoint
    )

  pw_forest <-
    if (is.null(condition)) {
      pw_dat
    } else {
      pw_dat %>%
        filter(.data$condition == .env$condition)
    }

  pw_forest_summary <-
    pw_forest %>%
    select(outcome_label,
           type_label,
           timepoint_label,
           n_studies,
           int_1,
           int_2) %>%
    mutate(across(everything(), str_to_sentence))



  pw_forest_files <-
    pw_forest %>%
    pull(pw_forest_file)

  list(pw_forest_summary = pw_forest_summary,
       pw_forest_files = pw_forest_files)
}



# procedurally generated --------------------------------------------------


analysis_output <- function(m_key_row) {
  cat("##### Pairwise comparisons \n\n")

  pw_files <-
    pw_results %>%
    select(-type) %>%
    inner_join(m_key_row) %>%
    pull(pw_forest_file)

  msg_mine(glue("Number of pw_files {length(pw_files)}"))

  msg_mine(glue("{pw_files}"))

  pw_files %>%
    map(function(file) {
      "<img src=\"{file}\" />" %>% cat()
    })

  cat("\n\n")

}




# make chapter function

makechapter <- function(this_outcome) {

  outcome_dulox <- if (this_outcome == "duloxetine") {
    pw_results %>% filter(str_detect(comp, "duloxetine"))
  } else {
    pw_results %>%
      filter(outcome == this_outcome)  }

  outcome_pw <-
    outcome_dulox %>%
    mutate(across(everything(), str_to_sentence)) %>%
    mutate(class = if_else(
      class == "All classes",
      class,
      toupper(class)
    ))

  conditions <-
    outcome_pw %>% pull(condition) %>% unique()

  conditions %>%
    map(function(cond){

      # Condition heading
      glue("## Condition: {str_to_sentence(cond)} \n\n") %>% cat()

      # Condition levels
      outcome_pw %>%
        filter(condition == cond) %>%
        select(condition, type_label, timepoint_label, class, dose) %>%
        distinct() %>%
        select(
          this_condition = condition,
          this_type = type_label,
          this_timepoint = timepoint_label,
          this_class = class,
          this_dose = dose
        ) %>%
        pmap(function(this_condition, this_type, this_timepoint,
                      this_class, this_dose) {
          glue("### {this_type} | {this_timepoint} | {this_class} | {this_dose} \n\n") %>% cat()

          outcome_pw %>%
            filter(condition == this_condition,
                   type_label == this_type,
                   timepoint_label == this_timepoint,
                   class == this_class,
                   dose == this_dose
                   ) %>%
            pull(pw_forest_file) %>%
            tolower() %>%
            map(~glue("<img src=\"{.x}\" style = 'width:100%;' />") %>%
                  cat())

          cat("\n\n")

        })

      cat("\n\n")
    })

}

makechapter_test <- function(outcome) {
  cat("## testing image output")

  print(here::here())

  test_img <- "images/pw-forest/subgroup_con-adverse-ad-post_int-placebo_amitriptyline.png"

  glue("<img src=\"{test_img}\" style = 'width:100%;' />") %>%
    cat()

}




makechapter_fermata <- function(outcome) {
  # consider removing nma from heading once other information goes in
  cat("## Outcome ", outcome, " data and analysis \n\n")

  subsections <-
    m_key %>%
    count(type_label, timepoint_label)

  subsections %>%
    pull(type_label) %>%
    unique() %>%
    map(function(type) {
      cat("### Type ", type, "\n\n")

      subsections %>%
        filter(type_label == type) %>%
        pull(timepoint_label) %>%
        map(function(timepoint_label) {
          cat("#### ", timepoint_label, type, "\n\n")

          this_m_key_row <-
            tibble(
              outcome = outcome,
              target = "m_type",
              timepoint_label = timepoint_label,
              type_label = type
            ) %>%
            left_join(m_key) %>%
            select(target, outcome, type, timepoint)

          msg_mine(glue(
            "Number of rows in this_m_key_row: {nrow(this_m_key_row)}"
          ))

          # analysis_output(this_m_key_row)
        })

    })


}

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


#  copy files to docs -----------------------------------------------------


system("cp -rf images ../docs/")
